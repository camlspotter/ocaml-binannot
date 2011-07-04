(***********************************************************************)
(*                                                                     *)
(*                            OcamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2011 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Utils

module Name = struct
  type t = string

  let create name = function
    | -1 -> Printf.sprintf "%s__G" name
    | -2 -> Printf.sprintf "%s__X" name (* CR jfuruse: a dirty hack *)
    | n when n >= 0 -> Printf.sprintf "%s__%d" name n
    | _ -> assert false
  ;;

  let parse s =
    try
      let pos = String.rindex s '_' in
      if pos = 0 then raise Not_found;
      if s.[pos-1] <> '_' then raise Not_found;
      let n = String.sub s 0 (pos-1) in
      let id =
        match String.sub s (pos+1) (String.length s - pos - 1) with
        | "G" -> -1
        | s -> int_of_string s
      in
      n, id
    with
    | _ -> raise Not_found
end

module Ident = struct
  (* extend the original module *)
  include Ident

  (* extend the original Ident module *)
    
  let name id =
    let binding_time = binding_time id in
    Name.create (name id) binding_time
	
  module Ident_internal : sig
    type t
    val unsafe_create_with_stamp : ?flags: int -> string -> int -> Ident.t
  end= struct
    (* Stamp is untouchable outside of ident.ml. A dirty
       workaround *)
    type t = { stamp: int; name: string; mutable flags: int }
    let to_ident (id : t) = (Obj.magic id : Ident.t)
      
    (* It is dangerous operation! *)        
      let unsafe_create_with_stamp ?(flags=0) name stamp =
        to_ident { stamp = stamp; name = name; flags = flags }
  end
      
  let unsafe_create_with_stamp = Ident_internal.unsafe_create_with_stamp

  let parse s =
    let s, pos = Name.parse s in
    let id = unsafe_create_with_stamp s pos in
    (* CR jfuruse: actually 0 is global and should be printed as 'G'
       Current 'G' means -1 *)
    if pos = 0 then make_global id;
    id

  let format ppf p = Format.pp_print_string ppf (name p)
end

module Path = struct
  (* extend the original module *)
  include Path

  let rec name = function
    | Pident id -> Ident.name id
    | Pdot(p, s, pos) -> Name.create (name p ^ "." ^ s) pos
    | Papply(p1, p2) -> name p1 ^ "(" ^ name p2 ^ ")"
	  
  let rec local = function
    | Pident id -> not (Ident.global id)
    | Pdot (p, _, _) -> local p
    | Papply(p1, _p2) -> local p1 (* ? *) 

  let parse s =
    let rec to_path = function
      | Longident.Lident s -> Pident (Ident.parse s)
      | Longident.Ldot (lid, s) ->
          let s, pos = Name.parse s in
          let path = to_path lid in
          Pdot (path, s, pos)
      | Longident.Lapply (lid1, lid2) ->
          let path1 = to_path lid1 in
          let path2 = to_path lid2 in
	  Papply(path1, path2)
    in
    to_path (Longident.parse s)

  let format ppf p = Format.pp_print_string ppf (name p)
end

module TypeFix : sig

  val type_expr : Types.type_expr -> Types.type_expr
    (** put pos and stamps to type_expr *)

  val module_type : Types.module_type -> Types.module_type

end = struct
  let ident id = Ident.create_persistent (Ident.name id)
    
  let rec path = function
      | Path.Pident id -> Path.Pident (ident id)
      | Path.Pdot (t, name, pos) -> Path.Pdot (path t, Name.create name pos, pos)
      | Path.Papply (t1, t2) -> Path.Papply (path t1, path t2)
	  
  module TypeTable = Hashtbl.Make(Types.TypeOps)
  open Types
  open Btype
  
  let type_expr =
    let cache = TypeTable.create 107 in
    let rec f t =
      let t = repr t in
      try TypeTable.find cache t with Not_found ->
	(* We need to create the cache entry first *)
	let t_dest = { t with desc = Tnil (* to be replaced *) } in
	TypeTable.add cache t t_dest;
	let default t =
	  let desc = Btype.copy_type_desc f t.desc in
          match desc with
          | Tconstr (p, args, abbrev_memo_ref) ->
              (* abbrev_memo_ref is ignored *)
              Tconstr (path p, args, abbrev_memo_ref)
          | Tobject (_, { contents = None } ) -> desc
          | Tobject (t, { contents = Some (p, ts) }) ->
              Tobject (t, ref (Some (path p, ts)))
	  | Tpackage (p, strs, tys) -> Tpackage (path p, strs, tys) (* CR: strs should be renamed too ? *)
          | Tvar | Tarrow _ | Ttuple _ | Tfield _ | Tnil 
	  | Tsubst _ | Tvariant _ 
          | Tlink _ | Tpoly _ | Tunivar -> desc
        in
        (* Exception: printer of optional argument is bit special.
           We cannot rename the option type *)
	let desc = 
  	  match t.desc with
  	  | Tarrow(l, ty1, ty2, comm) when Btype.is_optional l ->
              begin match (repr ty1).desc with
              | Tconstr(path, [ty], abbrev) when Path.same path Predef.path_option ->
                  (* we do not copy abbrev but it is ok *)
		  Tarrow (l, 
  	  		  { ty1 with desc = Tconstr(path, [f ty], abbrev) },
  	  		  f ty2,
  	  		  comm)
              | _ -> (* not option ? *) default t
              end
  	  | Tvariant row_desc -> (* we cannot use copy_type_desc *)
              (* not sure ... *)
  	      Tvariant (Btype.copy_row f true row_desc true 
  			  (f row_desc.row_more))
  	  | _ -> default t
	in
	t_dest.desc <- desc;
	t_dest
    in
    f
  ;;

  let value_description vdesc = 
    { vdesc with val_type = type_expr vdesc.val_type }

  let type_declaration tdecl = 
    { tdecl with type_params = List.map type_expr tdecl.type_params;
      type_manifest = Option.map ~f:type_expr tdecl.type_manifest }

  let exception_declaration = List.map type_expr

  let rec class_type = function
    | Cty_constr (p, tys, clty) ->
        Cty_constr (path p, List.map type_expr tys, class_type clty)
    | Cty_signature clsig -> Cty_signature (class_signature clsig)
    | Cty_fun (l, ty, clty) -> Cty_fun (l, type_expr ty, class_type clty)

  and class_signature clsig = 
    { clsig with cty_self = type_expr clsig.cty_self;
      cty_vars = 
	Vars.map (fun (f1,f2,ty) -> (f1,f2, type_expr ty)) clsig.cty_vars;
      cty_inher = 
        List.map (fun (p, tys) -> path p, List.map type_expr tys)
          clsig.cty_inher }

  let class_declaration cldecl = 
    { cldecl with cty_params = List.map type_expr cldecl.cty_params;
      cty_type = class_type cldecl.cty_type;
      cty_path = path cldecl.cty_path;
      cty_new = Option.map cldecl.cty_new ~f:type_expr }

  let cltype_declaration _ = assert false

  let rec module_type = function
    | Mty_ident p -> Mty_ident (path p)
    | Mty_signature sg -> Mty_signature (signature sg)
    | Mty_functor (id, mty, mty') ->
        Mty_functor (ident id, module_type mty, module_type mty')

  and signature sg = List.map signature_item sg

  and signature_item = function
    | Sig_value (id, vdesc) -> Sig_value (ident id, value_description vdesc)
    | Sig_type (id, tdecl, rec_status) -> 
        Sig_type (ident id, type_declaration tdecl, rec_status)
    | Sig_exception (id, edecl) ->
        Sig_exception (ident id, exception_declaration edecl)
    | Sig_module (id, mty, rec_status) ->
        Sig_module (ident id, module_type mty, rec_status)
    | Sig_modtype (id, mty_decl) -> 
        Sig_modtype (ident id, modtype_declaration mty_decl)
    | Sig_class (id, cldecl, rec_status) ->
        Sig_class (ident id, class_declaration cldecl, rec_status)
    | Sig_class_type (id, cltdecl, rec_status) ->
        Sig_class_type (ident id, cltype_declaration cltdecl, rec_status)

  and modtype_declaration = function
    | Modtype_abstract -> Modtype_abstract
    | Modtype_manifest mty -> Modtype_manifest (module_type mty)
end

module Printtyp = struct
  include Printtyp

  let make_type ppf f ?(with_pos=false) ty =
    let ty = if with_pos then TypeFix.type_expr ty else ty in
    f ppf ty

  let type_expr ?with_pos ppf = make_type ppf type_expr ?with_pos
  let type_sch ?with_pos ppf = make_type ppf type_sch ?with_pos
  let type_scheme ?with_pos ppf = make_type ppf type_scheme ?with_pos
  let modtype ?(with_pos=false) ppf mty = 
    let mty = if with_pos then TypeFix.module_type mty else mty in
    modtype ppf mty
end
