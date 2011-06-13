(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2011 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* CR jfuruse: now this module becomes very big after merging spot and spotapi.
   Need to be refactored *)

open Utils

module Ident0 = Ident

module Name = struct
  type t = string

  let create name = function
    | -1 -> Printf.sprintf "%s__G" name
    | -2 -> Printf.sprintf "%s__X" name (* a dirty hack *)
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
end

module Path0 = Path

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

module Position = struct
  open Lexing

  type t = { line_column : (int * int) option; 
             bytes : int option }

  let of_lexing_position pos =
    { line_column = Some (pos.pos_lnum, pos.pos_cnum - pos.pos_bol);
      bytes = Some pos.pos_cnum }

  let compare p1 p2 = match p1, p2 with
    | { bytes = Some b1; _ }, { bytes = Some b2; _ } -> compare b1 b2
    | { line_column = Some (l1,c1); _ }, { line_column = Some (l2,c2); _ } ->
	begin match compare l1 l2 with
	| 0 -> compare c1 c2
	| n -> n
	end
    | _ -> assert false

  let to_string p = match p.line_column, p.bytes with
    | Some (l,c), Some b -> Printf.sprintf "l%dc%db%d" l c b
    | Some (l,c), None -> Printf.sprintf "l%dc%d" l c
    | None, Some b -> Printf.sprintf "b%d" b
    | None, None -> assert false

  let none = { line_column = None; bytes = None }

  exception Parse_failure of string

  let parse s =
    (* token : [a-z][0-9]+ *)
    let len = String.length s in 
    let rec get_number ~num pos =
      if pos >= len then num, pos
      else 
	match s.[pos] with
	| '0'..'9' -> 
	    get_number   
	      ~num: (num * 10 + int_of_char s.[pos] - int_of_char '0')
	      (pos + 1)
	| _ -> num, pos
    in
    let rec get_tokens pos =
      if pos >= len then []
      else
	match s.[pos] with
	| 'a'..'z' -> 
	    let k = s.[pos] in
	    let pos = pos + 1 in
	    let num, pos' = get_number ~num:0 pos in
	    if pos = pos' then 
	      raise (Parse_failure (Printf.sprintf "pos token has no number: '%c'" 
				       k));
	    (k, num) :: get_tokens pos'
        | '0'..'9' ->
            (* Good Ol' Syntax *)
            begin try ['b', int_of_string s] with _ ->
              raise (Parse_failure
                        (Printf.sprintf "failed to parse %S as a byte position" s))
            end
	| _ -> 
	    raise (Parse_failure (Printf.sprintf "illegal pos token head '%c'" 
				     s.[pos]))
    in
    let tokens = get_tokens 0 in
    match tokens with
    | ['l', line; 'c', column] -> { line_column = Some (line, column); 
				    bytes = None }
    | ['b', bytes] -> { line_column = None; bytes = Some bytes }
    | _ -> raise (Parse_failure "illegal pos token combination")

  let next = function
    | { bytes = Some b; _ } -> { bytes = Some (b + 1); line_column = None }
    | { line_column = Some (l,c); bytes = None; } ->
        { line_column = Some (l, c+1); bytes = None }
    | _ -> assert false

  let is_complete = function
    | { line_column = Some _; bytes = Some _ } -> true
    | _ -> false
      
  (* it drops one byte at the end, but who cares? *)        
  let complete mlpath t = match t with
    | { line_column = Some _; bytes = Some _ } -> 
        t (* already complete *)
    | { line_column = Some (line, column); bytes = None } ->
        let ic = open_in_bin mlpath in
        let rec iter cur_line pos =
          ignore (input_line ic);
          let cur_line = cur_line + 1 in
          if cur_line = line then begin
            close_in ic;
            { line_column = Some (line, column); bytes = Some (pos + column) }
          end else iter cur_line (pos_in ic)
        in
        iter 0 0

    | { line_column = None; bytes = Some bytes } -> 
        let ic = open_in_bin mlpath in
        let rec iter lines remain =
          let pos = pos_in ic in
          let new_remain = bytes - pos in
          if new_remain < 0 then begin (* run over *)
            close_in ic;
            { line_column = Some (lines, remain); bytes = Some bytes }
          end else begin
            ignore (input_line ic);
            iter (lines+1) new_remain
          end
        in
        iter 0 bytes
          
    | { line_column = None; bytes = None } -> assert false

end

module Region = struct
  type t = { 
    start : Position.t;
    end_ : Position.t
  }

  let to_string t =
    Printf.sprintf "%s:%s"
      (Position.to_string t.start)
      (Position.to_string t.end_)

  let of_parsing l =
    let start = Position.of_lexing_position l.Location.loc_start in
    let end_ = Position.of_lexing_position l.Location.loc_end in
    match Position.compare start end_ with
    | -1 | 0 -> { start = start; end_ = end_ }
    | _ -> { start = end_; end_ = start }

  let compare l1 l2 = 
    if Position.compare l1.start l2.start = 0 
       && Position.compare l2.end_ l1.end_ = 0 then `Same
    else if Position.compare l1.start l2.start <= 0 
         && Position.compare l2.end_ l1.end_ <= 0 then `Includes
    else if Position.compare l2.start l1.start <= 0 
         && Position.compare l1.end_ l2.end_ <= 0 then `Included
    else if Position.compare l1.end_ l2.start <= 0 then `Left
    else if Position.compare l2.end_ l1.start <= 0 then `Right
    else `Overwrap

(*
  let position_prev pos = { pos with pos_cnum = pos.pos_cnum - 1 }
  let position_next pos = { pos with pos_cnum = pos.pos_cnum + 1 }
*)

  let split l1 ~by:l2 =
    if compare l1 l2 = `Overwrap then
      if Position.compare l1.start l2.start < 0 then
	Some ({ l1 with end_ = (* position_prev *) l2.start },
	      { l1 with start = l2.start })
      else if Position.compare l2.start l1.start < 0 then
        Some ({ l1 with end_ = l2.end_ },
	      { l1 with start = (* position_next *) l2.end_ })
      else assert false
    else None

  open Position

  let point_by_byte pos =
    { start = { line_column = None;
 		bytes = Some pos };
      end_ = { line_column = None;
               bytes = Some (pos + 1)} }

  let point pos = { start = pos; end_ = Position.next pos }

  let none = { start = Position.none;
	       end_ = Position.none }

  let length_in_bytes t =
    let bytes = function
      | { Position.bytes = Some bytes; _ } -> bytes
      | _ -> raise Not_found
    in
    bytes t.end_ - bytes t.start

  let is_complete t = 
    Position.is_complete t.start && Position.is_complete t.end_

  let complete mlpath t =
    { start = Position.complete mlpath t.start;
      end_ = Position.complete mlpath t.end_ }

  let substring mlpath t =
    let t = complete mlpath t in
    let ic = open_in_bin mlpath in
    match t.start.Position.bytes, t.end_.Position.bytes with
    | Some start, Some end_ ->
	seek_in ic start;
	let s = String.create (end_ - start) in
	really_input ic s 0 (end_ - start);
	t, s
    | _ -> assert false
    
end

module Regioned = struct
  type 'a t = { region: Region.t; value: 'a }  

  let compare { region = r1; _ } { region = r2; _ } = Region.compare r1 r2

  let split { region = r1; value = v } ~by:{ region = r2; _ } = 
    Option.map (Region.split r1 ~by: r2) ~f:(fun (r11, r12) -> 
      { region = r11; value = v },
      { region = r12; value = v }) 

  let format f ppf { region = r; value = v } =
    Format.fprintf ppf "@[<2>%s: @[%a@]@]" 
      (Region.to_string r) 
      f v
end

module Location_bound = struct
  (* Some objects lack their location positions in typedtree,
     and Location_bound.upperbound tries to provide better approximation for them
  *)
  open Location
  let upperbound loc by = { loc with loc_end = by.loc_start }
end

module Kind = struct
  type t = 
    | Value  (** regular value *)
    | Special_value (** primitives and others *) 
    | Type 
    | Exception 
    | Module 
    | Module_type 
    | Class 
    | Class_type

  let to_string = function
    | Value -> "v"
    | Special_value -> "sv"
    | Type -> "t"
    | Exception -> "e" 
    | Module -> "m"
    | Module_type -> "mt"
    | Class -> "c"
    | Class_type -> "ct"

  (* for messages *)
  let name = function
    | Value -> "value"
    | Special_value -> "special_value"
    | Type -> "type"
    | Exception -> "exception" 
    | Module -> "module"
    | Module_type -> "module_type"
    | Class -> "class"
    | Class_type -> "class_type"

  (* used for query interface *)        
  let from_string = function
    | "v" | "value" -> Value
    | "sv" | "special_value" -> Special_value
    | "t" | "type" -> Type
    | "e" | "exception" -> Exception
    | "m" | "module" -> Module
    | "mt" | "module_type" -> Module_type
    | "c" | "class" -> Class
    | "ct" | "class_type" -> Class_type
    | _ -> raise Not_found
end

module Abstraction = struct
  (* module definition abstraction *)

  (* CR jfuruse: types may be incompatible between compiler versions *)
  type module_expr = 
    | Mod_ident of Path.t (* module M = N *)
    | Mod_packed of string (* full path *)
        (* -pack overrides load paths: ocamlc -pack dir1/dir2/dir3/x.cmo *)
    | Mod_structure of structure (* module M = struct ... end *)
    | Mod_functor of Ident.t * Types.module_type * module_expr (* module M(I:S) = *)
    | Mod_apply of module_expr * module_expr (* module M = N(O) *)
    | Mod_constraint of module_expr * Types.module_type
    | Mod_unpack of Types.module_type
    | Mod_abstract (* used for Tmodtype_abstract *)

  (* structure abstraction : name - defloc asoc list *)
  and structure = structure_item list

  (* modtype must be identified from module, since they can have the
     same name *) 

  and structure_item = 
    | Str_value of Ident.t
    | Str_value_alias of Ident.t * Path.t
    | Str_type of Ident.t
    | Str_exception of Ident.t
    | Str_module of Ident.t * module_expr
    | Str_modtype of Ident.t * module_expr
    | Str_class of Ident.t
    | Str_cltype of Ident.t
    | Str_include of module_expr * (Kind.t * Ident.t) list

  module Module_expr = struct
    (* cache key is Typedtree.module_expr *)
    module M = struct
      type t = Typedtree.module_expr
      let equal m1 m2 = m1 == m2
      let hash_source m = m.Typedtree.mod_loc
      let hash m = Hashtbl.hash (hash_source m)
    end
    include M
    module Table = Hashtbl.Make(M)
  end

  module Structure_item = struct
    (* cache key is Abstraction.structure_item, not Typedtree.structure_item *)
    module M = struct
      type t = structure_item
      let equal s1 s2 =
	match s1, s2 with
	| Str_value id1, Str_value id2 
	| Str_type id1, Str_type id2 
	| Str_exception id1, Str_exception id2
	| Str_class id1, Str_class id2
	| Str_cltype id1, Str_cltype id2 -> id1 = id2
        | Str_value_alias (id1, p1), Str_value_alias (id2, p2) ->
            id1 = id2 && p1 = p2
	| Str_module (id1, mexp1) , Str_module (id2, mexp2) ->
	    id1 = id2 && Module_expr.equal mexp1 mexp2
	| Str_modtype (id1, mty1), Str_modtype (id2, mty2) ->
            id1 = id2 && Module_expr.equal mty1 mty2
	| Str_include (mexp1, kids1), Str_include (mexp2, kids2) ->
	    Module_expr.equal mexp1 mexp2 && kids1 = kids2
	| (Str_value _ | Str_value_alias _ | Str_type _ | Str_exception _ | Str_modtype _ 
	  | Str_class _ | Str_cltype _ | Str_module _ | Str_include _),
	  (Str_value _ | Str_value_alias _ | Str_type _ | Str_exception _ | Str_modtype _ 
	  | Str_class _ | Str_cltype _ | Str_module _ | Str_include _) -> false

      let hash = Hashtbl.hash
    end
    include M
    module Table = Hashtbl.Make(M)
  end

  open Types

  let cache_module_expr = Module_expr.Table.create 31
  let cache_structure_item = Structure_item.Table.create 31

  let included_sig_identifier_table = Hashtbl.create 31

(* CR jfuruse: TODO 
  let kident_of_sigitem = function
    | Tsig_value (id, _) -> Kind.Value, id
    | Tsig_exception (id, _) -> Kind.Exception, id
    | Tsig_module (id, _) ->  Kind.Module, id
    | Tsig_class (id, _) -> assert false
    | Tsig_type (id, _, _) -> Kind.Type, id
    | Tsig_modtype (id, _) -> Kind.Module_type, id
    | Tsig_cltype (id, _, _) -> Kind.Class_type, id
*)

  (* CR jfuruse: this is called many times (new_include2.ml *)    
  let kident_of_sigitemX exported_value_ids included_mexp = 
    let open Typedtree in
    let mty = Mtype.scrape included_mexp.mod_env included_mexp.mod_type in
    match mty with
    | Mty_functor _ -> assert false (* Including a functor?! *)
    | Mty_ident _ -> assert false (* Including an abstract module?! *)
    | Mty_signature sg ->
        let internal_value_ids = Typemod.bound_value_identifiers sg in
        let value_id_table = List.combine internal_value_ids exported_value_ids in
        let kident_of_sigitem = function
          | Sig_value (id, {val_kind = Val_reg; _}) -> Kind.Value, id
          | Sig_value (id, _) -> Kind.Special_value, id
          | Sig_exception (id, _) -> Kind.Exception, id
          | Sig_module (id, _, _) ->  Kind.Module, id
          | Sig_class (id, _, _) -> Kind.Class, id

          | Sig_type (id, _, _) -> Kind.Type, id
          | Sig_modtype (id, _) -> Kind.Module_type, id
          | Sig_class_type (id, _, _) -> Kind.Class_type, id
        in
        let kids = List.map kident_of_sigitem sg in
        (* Fixing internal ids to exported ids.
           Non value ids are replaced by id with -2 *)
        let fixed = List.map (fun (k, id) ->
          (k, 
           try List.assoc id value_id_table with Not_found -> 
             Ident.unsafe_create_with_stamp (Ident0.name id) (-2) (* magic number *))) 
          kids
        in
(*
        prerr_endline "fixing kids";
        Format.eprintf "exported: @[%a@]@."
          (Format.list ", " (fun ppf id -> Format.fprintf ppf "%s" (Ident.name id))) exported_value_ids;
        Format.eprintf "sig: @[%a@]@."
          (Format.list ", " (fun ppf (k,id) -> 
            Format.fprintf ppf "%s:%s" (Kind.to_string k) (Ident.name id))) kids;
        Format.eprintf "fixed: @[%a@]@."
          (Format.list ", " (fun ppf (k,id) -> 
            Format.fprintf ppf "%s:%s" (Kind.to_string k) (Ident.name id))) fixed;
*)
        fixed
          
  let rec module_expr mexp =
    try
      match Module_expr.Table.find cache_module_expr mexp with
      | None ->
          (* When a module definition finds itself in itself.
             Impossible to happen, so far. *)
          assert false
      | Some v -> v
    with
    | Not_found ->
	Module_expr.Table.replace cache_module_expr mexp None;
	let res = module_expr_sub mexp in
	Module_expr.Table.replace cache_module_expr mexp (Some res);
        res

  and module_expr_sub mexp = 
    let open Typedtree in
    match mexp.mod_desc with
    | Tmod_ident p -> Mod_ident p
    | Tmod_structure { str_items = str_items; str_type = _str_sig (* CR jfuruse: TODO *) } ->
	(* This may recompute abstractions of structure_items.
	   It sounds inefficient but not so much actually, since
	   module_expr is nicely cached. *)
	Mod_structure (List.flatten (List.map structure_item str_items))
    | Tmod_functor (id, mty, mexp) ->
        let mty = Mtype.scrape mexp.mod_env mty.mty_type in (* CR jfuruse: now loc is available! *)
	Mod_functor(id, mty, module_expr mexp)
    | Tmod_apply (mexp1, mexp2, _mcoercion) -> (* CR jfuruse ? *)
	Mod_apply (module_expr mexp1, module_expr mexp2)
    | Tmod_constraint (mexp, mty, _mtyconst, (* CR jfuruse: TODO *) _mcoercion) ->
	Mod_constraint (module_expr mexp, mty)
    | Tmod_unpack (_expr, mty) -> Mod_unpack mty

  and structure_item sitem = 
    (* it may recompute the same thing, but it is cheap *)
    let sitems = structure_item_sub sitem in
    (* make the same result (==) *)
    let equalize sitem =
      try
	Structure_item.Table.find cache_structure_item sitem
      with
      | Not_found -> 
	  Structure_item.Table.replace cache_structure_item sitem sitem;
	  sitem
    in
    List.map equalize sitems
    
  and structure_item_sub sitem = 
    (* CR jfuruse: now sitem.str_loc is available! *)
    let open Typedtree in
    match sitem.str_desc with
    | Tstr_eval _ -> []
    | Tstr_value (_, pat_exps) ->
	List.map (fun id -> Str_value id) (let_bound_idents pat_exps)
    | Tstr_primitive (id, _vdesc) -> 
	[Str_value id]
    | Tstr_type (id_descs) -> 
	List.map (fun (id, _) -> Str_type id) id_descs 
    | Tstr_exception (id ,_) ->
	[Str_exception id]
    | Tstr_exn_rebind (id, _path) -> (* CR jfuruse: path? *)
	[Str_exception id]
    | Tstr_module (id, mexp) ->
	[Str_module (id, module_expr mexp)]
    | Tstr_recmodule (idmexps) ->
	List.map (fun (id, _mty, (* CR jfuruse: todo *) mexp) ->
	  Str_module (id, module_expr mexp)) idmexps
    | Tstr_modtype (id, mty) ->
	[Str_modtype (id, module_type mty.mty_type (* CR jfuruse: todo *))]
    | Tstr_open _ -> []
(* CR jfuruse: todo
    | Tstr_class classdescs ->
	List.map (fun (class_decl, _names, _vflag) -> Str_class id) classdescs
*)
    | Tstr_class _ -> [] 
    | Tstr_class_type iddecls ->
	List.map (fun (id, _) -> Str_cltype id) iddecls
    | Tstr_include (mexp, exported_ids) ->
        let kids = kident_of_sigitemX exported_ids mexp in
        [Str_include (module_expr mexp, kids)]

  (* CR jfuruse: caching like module_expr_sub *)
  and module_type : Types.module_type -> _ = function
    | Mty_ident p -> Mod_ident p
    | Mty_signature sg -> 
	Mod_structure (List.filter_map signature_item sg)
    | Mty_functor (id, mty1, mty2) ->
        (* CR jfuruse: need to scrape ? but how ? *)
        Mod_functor(id, mty1, module_type mty2)

  and signature_item (sitem : Types.signature_item) = 
    try
      match sitem with
      | Sig_value (id, _)
      | Sig_type (id, _, _)
      | Sig_exception (id, _)
      | Sig_module (id, _ , _)
      | Sig_modtype (id, _)
      | Sig_class (id, _, _)
      | Sig_class_type (id, _, _) ->
	  (* Sigitem might be defined by include, but it is not recorded
	     in signature. We here try to recover it. *)
	  (* CR jfuruse: included modules may listed more than once *)
	  let sitem, recorded = Hashtbl.find included_sig_identifier_table id in
          if !recorded then None
          else begin
            recorded := true;
            Some sitem
          end
    with
    | Not_found ->  Some (signature_item_sub sitem)
	
  and signature_item_sub = function
    | Sig_value (id, _) -> Str_value id
    | Sig_type (id, _, _) -> Str_type id
    | Sig_exception (id, _) -> Str_exception id
    | Sig_module (id, mty , _) -> Str_module (id, module_type mty)
    | Sig_modtype (id, mty_decl) -> (* todo *) Str_modtype (id, modtype_declaration mty_decl)
    | Sig_class (id, _, _) -> Str_class id
    | Sig_class_type (id, _, _) -> Str_cltype id

  and modtype_declaration = function
    | Modtype_abstract -> Mod_abstract
    | Modtype_manifest mty -> module_type mty

  let ident_of_structure_item : structure_item -> (Kind.t * Ident.t) option = function
    | Str_value id -> Some (Kind.Value, id)
    | Str_value_alias (id, _) -> Some (Kind.Value, id)
    | Str_type id -> Some (Kind.Type, id)
    | Str_exception id -> Some (Kind.Exception, id) 
    | Str_module (id, _) -> Some (Kind.Module, id)
    | Str_modtype (id, _) -> Some (Kind.Module_type, id)
    | Str_class id -> Some (Kind.Class, id)
    | Str_cltype id -> Some (Kind.Class_type, id)
    | Str_include _ -> None

  open Format
  
  let rec format_module_expr ppf =
    let open Typedtree in
    function
    | Mod_ident p -> fprintf ppf "%s" (Path.name p)
    | Mod_packed s -> fprintf ppf "packed(%s)" s
    | Mod_structure str -> format_structure ppf str
    | Mod_functor (id, mty, mexp) ->
        fprintf ppf "@[<4>\\(%s : %a) ->@ %a@]" 
	  (Ident.name id)
          (Printtyp.modtype ~with_pos:true) mty
          format_module_expr mexp
    | Mod_apply (mexp1, mexp2) ->
        fprintf ppf "%a(%a)"
          format_module_expr mexp1
          format_module_expr mexp2 
    | Mod_constraint (mexp, mty) ->
        fprintf ppf "@[%a@ :@ @[%a@]@]"
          format_module_expr mexp
          (Printtyp.modtype ~with_pos:true) mty
    | Mod_abstract -> fprintf ppf "<abst>"
    | Mod_unpack mty -> 
        fprintf ppf "@[unpack@ : @[%a@]@]"
          (Printtyp.modtype ~with_pos:true) mty

  and format_structure ppf items = 
    fprintf ppf "{ @[<v>%a@] }"
      (list "; " format_structure_item) items
      
  and format_structure_item ppf = function
    | Str_value id -> fprintf ppf "val %s" (Ident.name id)
    | Str_value_alias (id, path) -> fprintf ppf "val %s = %s" (Ident.name id) (Path.name path)
    | Str_type id -> fprintf ppf "type %s" (Ident.name id)
    | Str_exception id -> fprintf ppf "exception %s" (Ident.name id)
    | Str_module (id, mexp) -> 
        fprintf ppf "@[<v4>module %s = %a@]" 
          (Ident.name id) 
          format_module_expr mexp
    | Str_modtype (id, mexp) ->
        fprintf ppf "@[<v4>module type %s =@ %a@]" 
          (Ident.name id)
          format_module_expr mexp
    | Str_class id -> fprintf ppf "class %s" (Ident.name id)
    | Str_cltype id -> fprintf ppf "class type %s" (Ident.name id)
    | Str_include (mexp, kidents) ->
        fprintf ppf "@[include %a@ : [ @[%a@] ]@]"
          format_module_expr mexp
          (list "; " (fun ppf (k,id) -> 
            fprintf ppf "%s %s" (String.capitalize (Kind.name k)) (Ident.name id))) 
          kidents
end

let protect name f v =
  try f v with e ->
    Format.eprintf "Error: %s: %s@." name (Printexc.to_string e)
    
module Annot = struct
  type t =
    | Type of Types.type_expr (* sub-expression's type *)
    | Str of Abstraction.structure_item 
    | Use of Kind.t * Path.t
    | Module of Abstraction.module_expr
    | Functor_parameter of Ident.t
    | Non_expansive of bool
    | Mod_type of Types.module_type

  let equal t1 t2 =
    match t1, t2 with
    | Type t1, Type t2 -> t1 == t2
    | Mod_type mty1, Mod_type mty2 -> mty1 == mty2
    | Str sitem1, Str sitem2 -> Abstraction.Structure_item.equal sitem1 sitem2
    | Module mexp1, Module mexp2 -> mexp1 == mexp2
    | Use (k1,p1), Use (k2,p2) -> k1 = k2 && p1 = p2
    | Non_expansive b1, Non_expansive b2 -> b1 = b2
    | Functor_parameter id1, Functor_parameter id2 -> id1 = id2
    | (Type _ | Str _ | Module _ | Functor_parameter _ | Use _ | Non_expansive _ 
          | Mod_type _),
      (Type _ | Str _ | Module _ | Functor_parameter _ | Use _ | Non_expansive _
          | Mod_type _) -> false 

  (* Recorded Location-Annot table. One location may have more than one binding! *)
  let recorded = (Hashtbl.create 1023 : (Location.t, t) Hashtbl.t)

  let recorded_top = ref None

  let clear () = Hashtbl.clear recorded

  type location_property = Wellformed | Flipped | Over_files | Illformed

  let check_location loc = 
    if loc.Location.loc_start == Lexing.dummy_pos || loc.Location.loc_end == Lexing.dummy_pos then Illformed
    else if loc.Location.loc_start = Lexing.dummy_pos || loc.Location.loc_end = Lexing.dummy_pos then Illformed
    else 
      (* If the file name is different between the start and the end, we cannot tell the wellformedness. *)
      if loc.Location.loc_start.Lexing.pos_fname <> loc.Location.loc_end.Lexing.pos_fname then Over_files
      else
        (* P4 creates some flipped locations where loc_start > loc_end *)
        match compare loc.Location.loc_start.Lexing.pos_cnum loc.Location.loc_end.Lexing.pos_cnum 
        with
        | -1 | 0 -> Wellformed
        | _ -> Flipped

  open Typedtree

  let record loc t = 
    let really_record () = Hashtbl.add recorded loc t in
    match check_location loc with
    | Wellformed -> really_record ()
    | Flipped -> 
        if not loc.Location.loc_ghost then Format.eprintf "%aWarning: Flipped location.@." Location.print loc; 
        really_record ()
    | Illformed -> 
        if not loc.Location.loc_ghost then Format.eprintf "%aWarning: Ill-formed location.@." Location.print loc
    | Over_files -> ()

  let record_constr_type_use loc ty =
    let path_of_constr_type t =
      let t = Ctype.repr t in 
      match (Ctype.repr t).Types.desc with
      | Types.Tconstr (p, _, _) -> Some p
      | _ ->
          Format.eprintf "Error: Spot.Annot.record_constr_type_use: not a constructor type: %a@." 
            (Printtyp.type_expr ~with_pos:false) ty;
          None
    in
    match path_of_constr_type ty with
    | Some path -> record loc (Use (Kind.Type, path))
    | None -> ()

  let record_module_expr_def loc id modl =
    protect "Spot.Annot.record_module_expr_def" (fun () ->
      record loc (Str (Abstraction.Str_module 
	                  (id, 
	                  (Abstraction.module_expr modl)))))
      ()
    
  let record_module_type_def id mty =
    protect "Spot.Annot.record_module_type_def" (fun () ->
      record mty.mty_loc 
        (Str (Abstraction.Str_modtype
                (id, Abstraction.module_type mty.mty_type))))
      ()

  let record_include_sig mty (* sg *) =
    protect "Spot.Annot.record_include_sig" (fun () ->
(* CR jfuruse: TODO
      let kids = (* CR jfuruse: copy of structure_item_sub *) 
	List.map Abstraction.kident_of_sigitem sg
      in
*)
      let kids = [] in
      let sitem = Abstraction.Str_include (Abstraction.module_type mty.mty_type, kids)
      in 
      (* ocaml signature simply forgets the fact that kids are
	 included. We memorize them here. *)
      List.iter (fun (_,id) ->
	Hashtbl.add
          Abstraction.included_sig_identifier_table
	  id (sitem, ref false (* never recorded in the parent sig yet *))) kids;
      record mty.mty_loc (Str sitem))
      ()

  let record_include modl exported_ids =
    protect "Spot.Annot.record_include" (fun () ->
      List.iter (fun sitem -> record modl.mod_loc (Str sitem))
        (Abstraction.structure_item
           { str_desc = Typedtree.Tstr_include (modl, exported_ids);
             str_loc = modl.mod_loc }))
      ()

  module IteratorArgument = struct
    include DefaultIteratorArgument

    let enter_pattern pattern = 
      let loc = pattern.pat_loc in
      record loc (Type pattern.pat_type);
      match pattern.pat_desc with
      | Tpat_var id -> record loc (Str (Abstraction.Str_value id))
      | Tpat_alias (_, TPat_constraint _) -> ()
      | Tpat_alias (_, TPat_alias id) -> record loc (Str (Abstraction.Str_value id))
      | Tpat_alias (_, TPat_type _path) -> assert false (* CR jfuruse: todo *)
      | Tpat_construct (_path, constr, _pats) ->
          begin match constr.Types.cstr_tag with
          | Types.Cstr_exception p ->
              record loc (Use (Kind.Exception, p))
          | _ -> record_constr_type_use loc constr.Types.cstr_res
          end
      | Tpat_record (_, _) -> record_constr_type_use loc pattern.pat_type
      | Tpat_any
      | Tpat_constant _
      | Tpat_tuple _
      | Tpat_variant _
      | Tpat_array _
      | Tpat_or _
      | Tpat_lazy _ -> ()

    let enter_expression exp =
      let loc = exp.exp_loc in
      record loc (Type exp.exp_type);
      match exp.exp_desc with
      | Texp_ident (path, vdesc) -> 
          (* Val_self's path is modified and we must recover it *)  
          (* This is a bizarre way, but works... *) 
          begin match vdesc.Types.val_kind with
          | Types.Val_self _ ->
              let env_summary = Env.summary exp.exp_env in
              let rec find = function
                | Env.Env_empty -> assert false
                | Env.Env_value (_, id, vdesc') when vdesc == vdesc' -> id
                | Env.Env_value (sum, _, _)
                | Env.Env_type (sum, _, _) 
                | Env.Env_exception (sum, _, _)
                | Env.Env_module (sum, _, _)
                | Env.Env_modtype (sum, _, _)
                | Env.Env_class (sum, _, _)
                | Env.Env_cltype (sum, _, _)
                | Env.Env_open (sum, _) -> find sum
              in
              record loc (Use (Kind.Value, Path.Pident (find env_summary)))
          | _ -> record loc (Use (Kind.Value, path))
          end
      | Texp_constant _ -> ()
      | Texp_let _ | Texp_function _ -> () (* done at bindings *)
      | Texp_apply _ -> ()
      | Texp_match _ -> ()
      | Texp_try _ -> ()
      | Texp_tuple _ -> ()
      | Texp_construct (_path, constr, _args) -> (* CR jfuruse: we can use path *)
          begin match constr.Types.cstr_tag with
          | Types.Cstr_exception p ->
              record loc (Use (Kind.Exception, p))
          | _ -> record_constr_type_use loc constr.Types.cstr_res
          end
      | Texp_variant _ -> ()
      | Texp_record _ -> record_constr_type_use loc exp.exp_type
      | Texp_field (_exp, _path, _label) -> (* CR jfuruse: we can use path *)
          record_constr_type_use loc exp.exp_type          
      | Texp_setfield (exp1, _path , _label, _exp2) -> (* CR jfuruse: we can use path *)
          record_constr_type_use loc exp1.exp_type          
      | Texp_array _ -> ()
      | Texp_ifthenelse _ -> ()
      | Texp_sequence _ -> ()
      | Texp_while _ -> ()
      | Texp_for (id, _exp1, _exp2, _dir, _exp3) ->
          record loc (Str (Abstraction.Str_value id) )
      | Texp_when _ -> ()
      | Texp_send _ -> ()
      | Texp_new (path, _) -> record loc (Use (Kind.Class, path))
      | Texp_instvar (_, path) -> record loc (Use (Kind.Value, path))
      | Texp_setinstvar (_, path, _) -> record loc (Use (Kind.Value, path))
      | Texp_override (_, _list) -> () 
      | Texp_letmodule (id, mexpr, _exp) -> record_module_expr_def mexpr.mod_loc id mexpr
      | Texp_assert _ -> ()
      | Texp_assertfalse -> ()
      | Texp_lazy _ -> ()
      | Texp_object (_, _) -> ()
      | Texp_pack _ -> ()
      | Texp_poly _ -> ()
      | Texp_open (path, exp) ->
          (* workaround to get better location *)
          let loc = Location_bound.upperbound loc exp.exp_loc in
          record loc (Use (Kind.Module, path))
      | Texp_newtype (_s, _exp) ->
          (* CR jfuruse: string is not sufficient! *)
          (* CR jfuruse: Texp_newtype is never used! *)
          assert false
          (* Spot.Annot.record loc (Spot.Annot.Str (Spot.Abstraction.Str_type id)); *)
      | Texp_constraint _ -> ()

    let enter_signature_item item = 
      let loc = item.sig_loc in
      match item.sig_desc with
        | Tsig_value (id, v) ->
            record loc (Str (Abstraction.Str_value id));
            record loc (Type v.val_desc.ctyp_type)
        | Tsig_type list ->
            List.iter (fun (id, decl) ->
	      record decl.typ_loc (Str (Abstraction.Str_type id))
            ) list
        | Tsig_exception (id, _decl) -> record loc (Str (Abstraction.Str_exception id))
        | Tsig_module (id, mtype) -> record_module_type_def id mtype
        | Tsig_recmodule list ->
            List.iter (fun (id, mtype) -> record_module_type_def id mtype) list
        | Tsig_modtype (id, Tmodtype_abstract) ->
	    record loc (Str (Abstraction.Str_modtype (id, Abstraction.Mod_abstract)))
        | Tsig_modtype (id, Tmodtype_manifest mty) -> record_module_type_def id mty
        | Tsig_open path -> record loc (Use (Kind.Module, path))
        | Tsig_include mty -> record_include_sig mty
        | Tsig_class _list -> () (* CR jfuruse *)
        | Tsig_class_type _list -> () (* CR jfuruse *)

    let enter_structure_item item =
      let loc = item.str_loc in
      match item.str_desc with
      | Tstr_eval _ -> ()
      | Tstr_value _ -> ()
      | Tstr_primitive (id, _v) -> record loc (Str (Abstraction.Str_value id))
      | Tstr_type list ->
          List.iter (fun (id, decl) ->
            record decl.typ_loc (Str (Abstraction.Str_type id))) list
      | Tstr_exception (id, _decl) -> record loc (Str (Abstraction.Str_exception id))
      | Tstr_exn_rebind (id, _p) -> record loc (Str (Abstraction.Str_exception id))
      | Tstr_module (id, mexpr) -> record_module_expr_def loc id mexpr
      | Tstr_recmodule list ->
	  List.iter (fun (id, _mtype, mexpr) ->
	    record_module_expr_def mexpr.mod_loc id mexpr) list
      | Tstr_modtype (id, mtype) -> record_module_type_def id mtype
      | Tstr_open path -> record loc (Use (Kind.Module, path))
      | Tstr_class _list -> () (* CR jfuruse *)
      | Tstr_class_type _list -> () (* CR jfuruse *)
      | Tstr_include (mexpr, exported_ids) -> record_include mexpr exported_ids

    let enter_core_type ct =
      match ct.ctyp_desc with
      | Ttyp_any -> ()
      | Ttyp_var _s -> () (* CR jfuruse: we will be able to work on poly things *) 
      | Ttyp_arrow _ -> ()
      | Ttyp_tuple _ -> ()
      | Ttyp_constr (path, _list) -> record ct.ctyp_loc (Use (Kind.Type, path))
      | Ttyp_object _list -> ()
      | Ttyp_class _ -> ()
      | Ttyp_alias (_ct, _s) -> () (* CR jfuruse: we will be able to work on poly things *) 
      | Ttyp_variant _ -> ()
      | Ttyp_poly _ -> ()
      | Ttyp_package _pack -> ()

    let enter_module_expr mexpr =
      match mexpr.mod_desc with
      | Tmod_ident p -> record mexpr.mod_loc (Use (Kind.Module, p))
      | Tmod_structure _ -> ()
      | Tmod_functor (id, mtype, _mexpr) ->
          (* CR jfuruse: id should have its position  *) 
          record mtype.mty_loc (Functor_parameter id);
      | Tmod_apply _ -> ()
      | Tmod_constraint _ -> ()
      | Tmod_unpack _ -> ()

    let enter_module_type mty = 
      let loc = mty.mty_loc in
      match mty.mty_desc with
      | Tmty_ident path -> record loc (Use (Kind.Module_type, path))
      | Tmty_signature _ -> ()
      | Tmty_functor (id, mtype1, _mtype2) ->
          (* CR jfuruse: id has no position information... *)
          record mtype1.mty_loc (Functor_parameter id)
      | Tmty_with (_, _list) -> (* CR jfuruse: list has paths, but no location *)
          ()
      | Tmty_typeof _ -> ()

    let enter_class_infos cl_info = 
      record cl_info.ci_loc (Str (Abstraction.Str_class cl_info.ci_id_class)); 
      record cl_info.ci_loc (Str (Abstraction.Str_cltype cl_info.ci_id_class_type))

    let enter_class_expr cexpr =
      let loc = cexpr.cl_loc in
      match cexpr.cl_desc with
      | Tcl_ident (path, _) -> record loc (Use (Kind.Class, path))
      | Tcl_structure _ -> ()
      | Tcl_fun (_, _, _pv (* ivars? *), _, _) -> () (* ? *) 
      | Tcl_apply _ -> ()
      | Tcl_let (_, _, var_rename, _) ->
          (* Tcf_let renames bound variables in let and keep the info inside var_rename *)
          List.iter (fun (id, exp) ->
            match exp.exp_desc with
            | Texp_ident (path, _) ->
                record loc (Str (Abstraction.Str_value_alias (id, path)))
            | _ -> assert false) var_rename
      | Tcl_constraint (_, _, _, _, _ ) -> () (* ? *)

    let enter_class_field cf =
      match cf.cf_desc with
      | Tcf_inher (_, _, _super (* ? *), vals, meths) -> 
          List.iter (fun id ->
            record cf.cf_loc (Str (Abstraction.Str_value id))) (* CR jfuruse: value? *)
            (List.map snd vals @ List.map snd meths)
      | Tcf_val (_, _, id, _, _) -> record cf.cf_loc (Str (Abstraction.Str_value id))
      | Tcf_meth (_mtname, _, _, _) -> ()
      | Tcf_constr _ -> ()
      | Tcf_let (_, _, var_rename) -> 
          (* Tcf_let renames bound variables in let and keep the info inside var_rename 
             It is lousy but we need to recover the positions... *)
          List.iter (fun (id, exp) ->
            match exp.exp_desc with
            | Texp_ident (path, _) ->
                record exp.exp_loc (Str (Abstraction.Str_value_alias (id, path)))
            | _ -> assert false) var_rename
      | Tcf_init _ -> ()

  (*
    val enter_structure : structure -> unit
    val enter_value_description : value_description -> unit
    val enter_type_declaration : type_declaration -> unit
    val enter_exception_declaration :
    exception_declaration -> unit
    val enter_package_type : package_type -> unit
    val enter_signature : signature -> unit
    val enter_modtype_declaration : modtype_declaration -> unit
    val enter_with_constraint : with_constraint -> unit
    val enter_class_signature : class_signature -> unit
    val enter_class_type_declaration :
    class_type_declaration -> unit
    val enter_class_type : class_type -> unit
    val enter_class_type_field : class_type_field -> unit
    val enter_core_field_type : core_field_type -> unit
    val enter_class_structure : class_structure -> unit
  *)
end

(* CR jfuruse: todo

  let record_module_expr_use loc modl =
    protect "Spot.Annot.record_module_expr_use" (fun () ->
      record loc (Module (Abstraction.module_expr modl));
      record loc (Mod_type modl.Typedtree.mod_type))
      ()

*)

  module Iterator = Typedtree.MakeIterator(IteratorArgument)

  let record_saved_type = function
    | Saved_implementation str 
    | Saved_structure str -> 
        recorded_top := Some (List.flatten (List.map Abstraction.structure_item str.str_items));
        Iterator.iter_structure str
    | Saved_signature sg -> 
        recorded_top := Some (List.filter_map Abstraction.signature_item sg.sig_type);
        Iterator.iter_signature sg
    | Saved_structure_item i -> Iterator.iter_structure_item i
    | Saved_signature_item i -> Iterator.iter_signature_item i
    | Saved_expression e -> Iterator.iter_expression e
    | Saved_module_type mt -> Iterator.iter_module_type mt
    | Saved_pattern p -> Iterator.iter_pattern p
    | Saved_class_expr ce -> Iterator.iter_class_expr ce

  let recorded () = Hashtbl.fold (fun k v st -> (k,v) :: st) recorded []

  let recorded_top () = !recorded_top

  let format ppf = function
    | Type typ -> 
	Printtyp.reset ();
	Printtyp.mark_loops typ;
        (* CR jfuruse: not fancy having @. *)
	Format.fprintf ppf "Type: %a@ " (Printtyp.type_scheme ~with_pos:false) typ;
	Format.fprintf ppf "XType: %a" (Printtyp.type_scheme ~with_pos:true) typ
    | Mod_type mty -> 
	Format.fprintf ppf "Type: %a@ " (Printtyp.modtype ~with_pos:false) mty;
	Format.fprintf ppf "XType: %a" (Printtyp.modtype ~with_pos:true) mty
    | Str str ->
	Format.fprintf ppf "Str: %a"
	  Abstraction.format_structure_item str
    | Use (use, path) ->
	Format.fprintf ppf "Use: %s, %s" 
	  (String.capitalize (Kind.name use)) (Path.name path)
    | Module mexp ->
	Format.fprintf ppf "Module: %a"
          Abstraction.format_module_expr mexp
    | Functor_parameter id ->
	Format.fprintf ppf "Functor_parameter: %s" (Ident.name id)
    | Non_expansive b ->
        Format.fprintf ppf "Non_expansive: %b" b

  let summary ppf = function
    | Type _typ -> 
        (* CR jfuruse: not fancy having @. *)
	Format.fprintf ppf "Type: ...@ ";
	Format.fprintf ppf "XType: ..."
    | Mod_type _mty -> 
	Format.fprintf ppf "Type: ...@ ";
	Format.fprintf ppf "XType: ..."
    | Str _str ->
	Format.fprintf ppf "Str: ..."
    | Use (use, path) ->
	Format.fprintf ppf "Use: %s, %s" 
	  (String.capitalize (Kind.name use)) (Path.name path)
    | Module _mexp ->
	Format.fprintf ppf "Module: ..."
    | Functor_parameter id ->
	Format.fprintf ppf "Functor_parameter: %s" (Ident.name id)
    | Non_expansive b ->
        Format.fprintf ppf "Non_expansive: %b" b

  let dummy = Use (Kind.Value, Path.Pident (Ident.create_persistent "dummy"))
end

(* annotation with region *)
module RAnnot = struct
  type t = Annot.t Regioned.t
  let split = Regioned.split
  let compare = Regioned.compare
  let format = Regioned.format Annot.format
end

module Tree = struct
  include Treeset.Make(RAnnot)

  open Regioned

  (* If the region maybe splitted, the original region will be gone *)
  let add t rannot = add_elem rannot t

  let iter = iter_elem

  let find_path_contains r t = 
    let probe = { region = r; value = Annot.dummy } in
    find_path_contains probe t

  let dump t = 
    iter_elem (fun ~parent rrspot ->
	let format_parent ppf = function
	  | None -> Format.fprintf ppf "ROOT"
	  | Some rrspot -> RAnnot.format ppf rrspot
	in
	Format.eprintf "@[<2>%a =>@ %a@]@."
	  format_parent parent
	  RAnnot.format rrspot) t
end

(* Spot file *)
module File = struct
  (* CR jfuruse: Current cmt/cmti only carry saved_types *)
  (* CR jfuruse: no longer used *)
  (* not record but list for future extensibility *)
  type elem =
    | Argv of string array
    | Source_path of string option (* packed module has None *)
    | Cwd of string
    | Load_paths of string list
    | Saved_types of Typedtree.saved_type array

  type t = elem list

end
