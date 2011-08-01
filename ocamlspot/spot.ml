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
open Indexed

module Position = struct
  open Lexing

  (* CR jfuruse: It is not TAB sensitive. If a user uses Emacs, and there are tabs, position specs are different! *)
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
(*
    | Some (l,c), Some b -> Printf.sprintf "%d_%d_%d" l c b
    | Some (l,c), None -> Printf.sprintf "%d_%d" l c
*)
    | Some (l,c), Some b -> Printf.sprintf "l%dc%db%d" l c b
    | Some (l,c), None -> Printf.sprintf "l%dc%d" l c
    | None, Some b -> Printf.sprintf "b%d" b
    | None, None -> assert false

  let none = { line_column = None; bytes = None }

  exception Parse_failure of string

  let parse_new s = 
    let get_num pos = 
      try
        let upos = String.index_from s pos '_' in 
        int_of_string (String.sub s pos (upos - pos)), Some (upos + 1)
      with
      | Not_found -> int_of_string (String.sub s pos (String.length s - pos)), None
    in
    let rec get_nums pos = 
      let s, pos' = get_num pos in
      match pos' with
      | None -> [s]
      | Some pos' -> s :: get_nums pos'
    in
    match get_nums 0
    with
    | [l; c; b] -> { line_column = Some (l, c);  bytes = Some b }
    | [l; c] -> { line_column = Some (l, c);  bytes = None }
    | [b] -> { line_column = None; bytes = Some b }
    | _ -> raise (Parse_failure "illegal pos token combination")

  let parse_old s =
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

  let parse s = try parse_new s with _ -> parse_old s

  let next = function
    | { bytes = Some b; _ } -> { bytes = Some (b + 1); line_column = None }
    | { line_column = Some (l,c); bytes = None; } -> { line_column = Some (l, c+1); bytes = None }
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
    fprintf ppf "@[<2>%s: @[%a@]@]" 
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
    | Primitive (** primitives and others *) 
    | Type 
    | Exception 
    | Module 
    | Module_type 
    | Class 
    | Class_type

  let to_string = function
    | Value       -> "v"
    | Primitive   -> "p"
    | Type        -> "t"
    | Exception   -> "e" 
    | Module      -> "m"
    | Module_type -> "mt"
    | Class       -> "c"
    | Class_type  -> "ct"

  (* for messages *)
  let name = function
    | Value       -> "value"
    | Primitive   -> "primitive"
    | Type        -> "type"
    | Exception   -> "exception" 
    | Module      -> "module"
    | Module_type -> "module_type"
    | Class       -> "class"
    | Class_type  -> "class_type"

  (* used for query interface *)        
  let from_string = function
    | "v"  | "value"       -> Value
    | "p"  | "primitive"   -> Primitive
    | "t"  | "type"        -> Type
    | "e"  | "exception"   -> Exception
    | "m"  | "module"      -> Module
    | "mt" | "module_type" -> Module_type
    | "c"  | "class"       -> Class
    | "ct" | "class_type"  -> Class_type
    | _                    -> raise Not_found
end

module XEnv = struct
  open Env
  let idents env =
    let sum = Env.summary env in
    let rec ids = function
      | Env_empty -> []
      | Env_value (sum, id, { Types.val_kind = Types.Val_prim _; _ }) -> `Ident (id, Kind.Primitive) :: ids sum
      | Env_value (sum, id, _) -> `Ident (id, Kind.Value) :: ids sum
      | Env_type (sum, id, _) -> `Ident (id, Kind.Type) :: ids sum
      | Env_exception (sum, id, _) -> `Ident (id, Kind.Exception) :: ids sum
      | Env_module (sum, id, _) -> `Ident (id, Kind.Module) :: ids sum
      | Env_modtype (sum, id, _) -> `Ident (id, Kind.Module_type) :: ids sum
      | Env_class (sum, id, _) -> `Ident (id, Kind.Class) :: ids sum
      | Env_cltype (sum, id, _) -> `Ident (id, Kind.Class_type) :: ids sum
      | Env_open (sum, path) -> `Open path :: ids sum
    in
    ids sum

  let format ppf env =
    let f ppf = function
      | `Ident (id, k) -> fprintf ppf "%s %a" (Kind.name k) Ident.format id
      | `Open p -> fprintf ppf "open %a" Path.format p
    in
    let ids = idents env in
    Format.list "; " f ppf ids
end

module Signature = struct
  open Kind
  open Types

  let sg_of_mtype env mty = 
    let mty = Mtype.scrape env mty in
    match mty with
    | Mty_functor _ -> assert false (* Including a functor?! *)
    | Mty_ident p -> 
        eprintf "include_coercion includes abstract module %a?@." Path.format p;
        eprintf "ENV: @[<v>%a@]@." XEnv.format env;
        (* assert false (* Including an abstract module?! *) *)
        assert false
    | Mty_signature sg -> sg

  let kidents_of_signature sg = 
    let kident_of_sigitem = function
      | Sig_value (id, {val_kind = Val_reg; _}) -> Value, id
      | Sig_value (id, _)                       -> Primitive, id
      | Sig_exception (id, _)                   -> Exception, id
      | Sig_module (id, _, _)                   -> Module, id
      | Sig_class (id, _, _)                    -> Class, id
  
      | Sig_type (id, _, _)                     -> Type, id
      | Sig_modtype (id, _)                     -> Module_type, id
      | Sig_class_type (id, _, _)               -> Class_type, id
    in
    List.map kident_of_sigitem  sg

  let include_coercion exported_value_ids sg : (t * Ident.t (*out*) * Ident.t (*in*)) list = 
    let internal_value_ids = Typemod.bound_value_identifiers sg in
    let value_id_table = List.combine internal_value_ids exported_value_ids in
    let kids = kidents_of_signature sg in
    (* Fixing internal ids to exported ids.
       Non value ids are replaced by id with -2 *)
    List.map (fun (k, id) ->
      (k, 
       (try List.assoc id value_id_table with Not_found -> 
         Ident.unsafe_create_with_stamp (Ocaml.Ident.name id) (-2) (* magic number *)),
       id))
      kids

end

(** Source code annotation information *)
module Annot = struct

  (** Definitions *)    
  type def =
    | Def_module_expr of Typedtree.module_expr
    | Def_module_type of Typedtree.module_type
    | Def_alias of Path.t
    | Def_included of Typedtree.module_expr * Ident.t
    | Def_included_sig of Typedtree.module_type

  type t =
    | Type of Types.type_expr (* sub-expression's type *)
    | Mod_type of Types.module_type
(* CR jfuruse: TODO
    | Non_expansive of bool
*)
    | Use of Kind.t * Path.t
    | Functor_parameter of Ident.t
    | Def of Kind.t * Ident.t * def option (** definition of Ident.t *) (* CR jfuruse: Kind.t and def have some invariants *)

  let equal t1 t2 = match t1, t2 with
    | Type t1,               Type t2               -> t1 == t2
    | Mod_type t1,           Mod_type t2           -> t1 == t2
    | Use (k1 ,p1),          Use (k2,p2)           -> k1 = k2 && p1 = p2
(*
    | Non_expansive b1,      Non_expansive b2      -> b1 = b2
*)
    | Functor_parameter id1, Functor_parameter id2 -> id1 = id2
    | Def (_ , id1, _),      Def (_, id2, _)       -> id1 = id2
    | (Type _ | Mod_type _ | Def _ | Functor_parameter _ | Use _ (* | Non_expansive _ *)),
      (Type _ | Mod_type _ | Def _ | Functor_parameter _ | Use _ (* | Non_expansive _ *)) -> false 

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

  let record loc annot = 
    let really_record () = Hashtbl.add recorded loc annot in
    match check_location loc with
    | Wellformed -> really_record ()
    | Flipped -> 
        if not loc.Location.loc_ghost then eprintf "%aWarning: Flipped location.@." Location.print loc; 
        really_record ()
    | Illformed -> 
        if not loc.Location.loc_ghost then eprintf "%aWarning: Ill-formed location.@." Location.print loc
    | Over_files -> ()

  let record_constr_type_use loc ty =
    let path_of_constr_type t =
      let t = Ctype.repr t in 
      match (Ctype.repr t).Types.desc with
      | Types.Tconstr (p, _, _) -> Some p
      | _ ->
          eprintf "Error: Spot.Annot.record_constr_type_use: not a constructor type: %a@." 
            (Printtyp.type_expr ~with_pos:false) ty;
          None
    in
    match path_of_constr_type ty with
    | Some path -> record loc (Use (Kind.Type, path))
    | None -> ()

  let record_module_expr_def loc id modl = record loc (Def (Kind.Module, id, Some (Def_module_expr modl)))
  let record_module_type_def loc id mty = record loc (Def (Kind.Module_type, id, Some (Def_module_type mty)))

  let record_include loc modl exported_ids =
    (* include defines new identifiers, and they must be registered into the flat db *)
    let kidents = Signature.include_coercion exported_ids (Signature.sg_of_mtype modl.mod_env modl.mod_type) in
    List.iter (fun (k, id_out, id_in) ->
      record loc (Def (k, id_out, Some (Def_included (modl, id_in))))) kidents

  let record_include_sig loc mty sg = 
    (* include defines new identifiers, and they must be registered into the flat db *)
    let kidents = Signature.include_coercion [] sg in
    List.iter (fun (k, id_out, _id_in) ->
      record loc (Def (k, id_out, Some (Def_included_sig mty)))) kidents

  module IteratorArgument = struct
    include DefaultIteratorArgument

    let enter_pattern pattern = 
      let loc = pattern.pat_loc in
      record loc (Type pattern.pat_type);
      match pattern.pat_desc with
      | Tpat_var id -> record loc (Def (Kind.Value, id, None))
      | Tpat_alias (_, TPat_constraint _) -> ()
      | Tpat_alias (_, TPat_alias id) -> record loc (Def (Kind.Value, id, None))
      | Tpat_alias (_, TPat_type _path) -> assert false (* CR jfuruse: todo *)
      | Tpat_construct (_path, constr, _pats) ->
          begin match constr.Types.cstr_tag with
          | Types.Cstr_exception p -> record loc (Use (Kind.Exception, p))
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
          | Types.Cstr_exception p -> record loc (Use (Kind.Exception, p))
          | _ -> record_constr_type_use loc constr.Types.cstr_res
          end
      | Texp_variant _ -> ()
      | Texp_record _ -> record_constr_type_use loc exp.exp_type
      | Texp_field (exp, _path, _label) -> (* CR jfuruse: we can use path *)
          record_constr_type_use loc exp.exp_type          
      | Texp_setfield (exp1, _path , _label, _exp2) -> (* CR jfuruse: we can use path *)
          record_constr_type_use loc exp1.exp_type          
      | Texp_array _ -> ()
      | Texp_ifthenelse _ -> ()
      | Texp_sequence _ -> ()
      | Texp_while _ -> ()
      | Texp_for (id, _exp1, _exp2, _dir, _exp3) -> record loc (Def (Kind.Value, id, None))
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
            record loc (Def (Kind.Value, id, None));
            record loc (Type v.val_desc.ctyp_type)
        | Tsig_type list ->
            List.iter (fun (id, decl) ->
	      record decl.typ_loc (Def (Kind.Type,id, None))
            ) list
        | Tsig_exception (id, _decl) -> record loc (Def (Kind.Exception, id, None))
        | Tsig_module (id, mtype) -> record_module_type_def mtype.mty_loc id mtype
        | Tsig_recmodule list ->
            List.iter (fun (id, mtype) -> record_module_type_def mtype.mty_loc id mtype) list
        | Tsig_modtype (id, Tmodtype_abstract) -> record loc (Def (Kind.Module, id, None))
        | Tsig_modtype (id, Tmodtype_manifest mty) -> record_module_type_def mty.mty_loc id mty
        | Tsig_open path -> record loc (Use (Kind.Module, path))
        | Tsig_include (mty, sg) -> record_include_sig loc mty sg
        | Tsig_class _list -> () (* CR jfuruse *)
        | Tsig_class_type _list -> () (* CR jfuruse *)

    let enter_structure_item item =
      let loc = item.str_loc in
      match item.str_desc with
      | Tstr_eval _ -> ()
      | Tstr_value _ -> ()
      | Tstr_primitive (id, _v) -> record loc (Def (Kind.Primitive, id, None))
      | Tstr_type list ->
          List.iter (fun (id, decl) -> record decl.typ_loc (Def (Kind.Type, id, None))) list
      | Tstr_exception (id, _decl) -> record loc (Def (Kind.Exception, id, None))
      | Tstr_exn_rebind (id, p) -> 
          record loc (Def (Kind.Exception, id, None));
          record loc (Use (Kind.Exception, p)) (* CR jfuruse: loc can be improved *)
      | Tstr_module (id, mexpr) -> record_module_expr_def loc id mexpr
      | Tstr_recmodule list ->
	  List.iter (fun (id, _mtype, mexpr) ->
	    record_module_expr_def mexpr.mod_loc id mexpr) list
      | Tstr_modtype (id, mtype) -> record_module_type_def mtype.mty_loc id mtype
      | Tstr_open path -> record loc (Use (Kind.Module, path))
      | Tstr_class _clinfoss -> ()
      | Tstr_class_type _id_clinfoss -> ()
      | Tstr_include (mexpr, exported_ids) -> record_include mexpr.mod_loc mexpr exported_ids

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
      | Ttyp_package pack -> record ct.ctyp_loc (Use (Kind.Module_type, pack.pack_name)) (* CR jfuruse ? *)

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
      (* CR jfuruse: kind? *)
      record cl_info.ci_loc (Def (Kind.Class, cl_info.ci_id_class, None));
      record cl_info.ci_loc (Def (Kind.Class_type, cl_info.ci_id_class_type, None));
      record cl_info.ci_loc (Def (Kind.Type, cl_info.ci_id_object, None));
      record cl_info.ci_loc (Def (Kind.Type , cl_info.ci_id_typesharp, None))

    let add_var_aliases loc (var_rename : (Ident.t * expression) list) = 
      List.iter (fun (id, exp) ->
        match exp.exp_desc with
        | Texp_ident (path, _) -> record loc (Def (Kind.Value (* CR ? *), id, Some (Def_alias path)))
        | _ -> assert false) var_rename

    let enter_class_expr cexpr =
      let loc = cexpr.cl_loc in
      match cexpr.cl_desc with
      | Tcl_ident (path, _) -> record loc (Use (Kind.Class, path))
      | Tcl_structure _ -> ()
      | Tcl_fun (_, _, var_rename, _, _) -> add_var_aliases loc var_rename
      | Tcl_apply _ -> ()
      | Tcl_let (_, _, var_rename, _) ->
          (* Tcf_let renames bound variables in let and keep the info inside var_rename *)
          add_var_aliases loc var_rename
      | Tcl_constraint (_, _, _, _, _ ) -> () (* ? *)

    let enter_class_field cf =
      match cf.cf_desc with
      | Tcf_inher (_, _, _super (* ? *), vals, meths) -> 
          List.iter (fun id -> record cf.cf_loc (Def (Kind.Value (* CR *), id, None))) (List.map snd vals @ List.map snd meths)
      | Tcf_val (_, _, id, _, _) -> record cf.cf_loc (Def (Kind.Value, id, None))
      | Tcf_meth (_mtname, _, _, _) -> ()
      | Tcf_constr _ -> ()
      | Tcf_let (_, _, var_rename) -> 
          (* Tcf_let renames bound variables in let and keep the info inside var_rename 
             It is lousy but we need to recover the positions... *)
          add_var_aliases cf.cf_loc var_rename
      | Tcf_init _ -> ()

    let enter_class_type ct =
      match ct.cltyp_desc with
      | Tcty_signature _csg -> () (* CR jfuruse: ? *)
      | Tcty_constr (path, _list) -> record ct.cltyp_loc (Use (Kind.Class_type, path))
      | Tcty_fun (_label, _ct, _cl) -> ()

  (* Those are left untouched.

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
    val enter_class_type_field : class_type_field -> unit
    val enter_core_field_type : core_field_type -> unit
    val enter_class_structure : class_structure -> unit
  *)
  end

  module Iterator = Typedtree.MakeIterator(IteratorArgument)

  let record_saved_type v = match v with
    | Saved_implementation str 
    | Saved_structure str -> 
        recorded_top := Some v;
        Iterator.iter_structure str
    | Saved_signature sg -> 
        recorded_top := Some v;
        Iterator.iter_signature sg
    | Saved_structure_item i -> Iterator.iter_structure_item i
    | Saved_signature_item i -> Iterator.iter_signature_item i
    | Saved_expression e -> Iterator.iter_expression e
    | Saved_module_type mt -> Iterator.iter_module_type mt
    | Saved_pattern p -> Iterator.iter_pattern p
    | Saved_class_expr ce -> Iterator.iter_class_expr ce
    | Saved_path_environments _ 
    | Saved_longident_locations _ 
    | Saved_ident_locations _  -> ()

  let recorded () = Hashtbl.fold (fun k v st -> (k,v) :: st) recorded []

  let recorded_top () = !recorded_top

  let format_def ppf = function
    | Def_module_expr mexp    -> Abstraction.Format.module_expr ppf mexp
    | Def_module_type mty     -> Abstraction.Format.module_type ppf mty
    | Def_alias p             -> fprintf ppf "alias %s" (Path.name p)
    | Def_included (_mdl, id) -> fprintf ppf "included _ %s" (Ident.name id)
    | Def_included_sig mty    -> fprintf ppf "include %a" Abstraction.Format.module_type mty

  let format ppf = function
    | Type typ -> 
	Printtyp.reset ();
	Printtyp.mark_loops typ;
        (* CR jfuruse: not fancy having @. *)
	fprintf ppf "Type: %a@ " (Printtyp.type_scheme ~with_pos:false) typ;
	fprintf ppf "XType: %a" (Printtyp.type_scheme ~with_pos:true) typ
    | Mod_type mty -> 
	fprintf ppf "Type: %a@ " (Printtyp.modtype ~with_pos:false) mty;
	fprintf ppf "XType: %a" (Printtyp.modtype ~with_pos:true) mty
    | Def (k, id, None) -> 
        fprintf ppf "Def: %s %s None" (Kind.to_string k) (Ident.name id)
    | Def (k, id, Some def) -> 
        fprintf ppf "Def: %s %s %a" (Kind.to_string k) (Ident.name id) format_def def
    | Use (use, path) ->
	fprintf ppf "Use: %s, %s" 
	  (String.capitalize (Kind.name use)) (Path.name path)
    | Functor_parameter id ->
	fprintf ppf "Functor_parameter: %s" (Ident.name id)
(*
    | Non_expansive b ->
        fprintf ppf "Non_expansive: %b" b
*)

  let summary ppf = function
    | Type _typ -> 
        (* CR jfuruse: not fancy having @. *)
	fprintf ppf "Type: ...@ ";
	fprintf ppf "XType: ..."
    | Mod_type _mty -> 
	fprintf ppf "Type: ...@ ";
	fprintf ppf "XType: ..."
    | Def (k, id, None) -> 
	fprintf ppf "Def: %s %s None" (Kind.to_string k) (Ident.name id)
    | Def (k, id, Some def) -> 
        fprintf ppf "Def: %s %s %a" (Kind.to_string k) (Ident.name id) format_def def
    | Use (use, path) ->
	fprintf ppf "Use: %s, %s" 
	  (String.capitalize (Kind.name use)) (Path.name path)
    | Functor_parameter id ->
	fprintf ppf "Functor_parameter: %s" (Ident.name id)
(*
    | Non_expansive b ->
        fprintf ppf "Non_expansive: %b" b
*)

  let dummy = Use (Kind.Value, Path.Pident (Ocaml.Ident.create_persistent "dummy"))
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
	  | None -> fprintf ppf "ROOT"
	  | Some rrspot -> RAnnot.format ppf rrspot
	in
	eprintf "@[<2>%a =>@ %a@]@."
	  format_parent parent
	  RAnnot.format rrspot) t
end
