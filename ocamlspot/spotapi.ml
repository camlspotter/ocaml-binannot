(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008, 2009, 2010 Jun Furuse. All rights reserved.       *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Utils

open Spot

module Name = struct
  type t = string

  let create name = function
    | -1 -> Printf.sprintf "%s__G" name
    | n -> Printf.sprintf "%s__%d" name n
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

module Ident0 = Ident

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

module Kind = struct
  include Kind

  let to_string = function
    | Value -> "v"
    | Type -> "t"
    | Exception -> "e" 
    | Module -> "m"
    | Module_type -> "mt"
    | Class -> "c"
    | Class_type -> "ct"

  (* for messages *)
  let name = function
    | Value -> "value"
    | Type -> "type"
    | Exception -> "exception" 
    | Module -> "module"
    | Module_type -> "module_type"
    | Class -> "class"
    | Class_type -> "class_type"

  (* used for query interface *)        
  let from_string = function
    | "v" | "value" -> Value
    | "t" | "type" -> Type
    | "e" | "exception" -> Exception
    | "m" | "module" -> Module
    | "mt" | "module_type" -> Module_type
    | "c" | "class" -> Class
    | "ct" | "class_type" -> Class_type
    | _ -> raise Not_found
end

module Abstraction = struct
  include Abstraction

  let ident_of_structure_item : structure_item -> (Kind.t * Ident.t) option = function
    | Str_value id -> Some (Kind.Value, id)
    | Str_type id -> Some (Kind.Type, id)
    | Str_exception id -> Some (Kind.Exception, id) 
    | Str_module (id, _) -> Some (Kind.Module, id)
    | Str_modtype (id, _) -> Some (Kind.Module_type, id)
    | Str_class id -> Some (Kind.Class, id)
    | Str_cltype id -> Some (Kind.Class_type, id)
    | Str_include _ -> None

  open Format
  
  let rec format_module_expr ppf = function
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

	
module Annot = struct
  include Annot

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
    Format.fprintf ppf "@[<2>%s: %a@]" 
      (Region.to_string r) 
      f v
end

(* annotation with region *)
module RAnnot = struct
  open Regioned

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
