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

open Utils

open Types
open Typedtree
open Indexed
open Format

type t = 
  | Value of Ident.t
  | Primitive of Ident.t
  | Type of Ident.t
  | Exception of Ident.t
  | Module of Ident.t * module_
  | Module_type of Ident.t * module_
  | Class of Ident.t list
  | Class_type of Ident.t list
  | Include of module_ * Ident.t list option

and module_ = 
  | Path of Path.t
  | Structure of t list
  | Functor of Ident.t * module_
  | Apply of module_ * module_
  | Constraint of module_ * module_
  | Abstract

let rec format ppf = function
  | Value id -> fprintf ppf "val %a" Ident.format id
  | Primitive id -> fprintf ppf "val prim %a" Ident.format id
  | Type id -> fprintf ppf "type %a" Ident.format id
  | Exception id -> fprintf ppf "exception %a" Ident.format id
  | Module (id, module_) -> fprintf ppf "@[<2>module %a = %a@]" Ident.format id format_module_ module_
  | Module_type (id, module_) -> fprintf ppf "@[<2>module type %a =@ %a]" Ident.format id format_module_ module_
  | Class ids -> fprintf ppf "class [@[%a@]]" (list "; " (fun ppf id -> fprintf ppf "%s" (Ident.name id))) ids
  | Class_type ids -> fprintf ppf "class type [@[%a@]]" (list "; " Ident.format) ids
  | Include (module_, ids_opt) -> 
      fprintf ppf "@[<2>include {@ @[<v>%a@] }%a@]" 
        format_module_ module_ 
        (fun ppf -> function
          | None -> ()
          | Some ids -> 
              fprintf ppf " [%a]" (list "; " Ident.format) ids) ids_opt
        
and format_module_ ppf = function
  | Path p -> fprintf ppf "%s" (Path.name p)
  | Structure ts -> fprintf ppf "{ @[<v>%a@] }" (list "; " format) ts
  | Functor (id, module_) -> fprintf ppf "\\%a -> %a" Ident.format id format_module_ module_
  | Apply (module_1, module_2) -> fprintf ppf "app(%a, %a)" format_module_ module_1 format_module_ module_2
  | Constraint (module_1, module_2) -> fprintf ppf "constraint(%a, %a)" format_module_ module_1 format_module_ module_2
  | Abstract -> fprintf ppf "<abst>"

let rec structure str : t list = List.fold_right structure_item str.str_items []

and structure_item sitem st : t list = match sitem.str_desc with
  | Tstr_eval _ -> st
  | Tstr_value (_, pat_exps) -> 
      let ids = let_bound_idents pat_exps in
      List.map (fun id -> Value id) ids @ st
  | Tstr_primitive (id, _) -> Primitive id :: st
  | Tstr_type id_decls -> List.map (fun (id,_) -> Type id) id_decls @ st
  | Tstr_exception (id, _) 
  | Tstr_exn_rebind (id, _) -> Exception id :: st 
  | Tstr_module (id, mexp) -> Module (id, module_expr mexp) :: st
  | Tstr_recmodule id_mty_mexps -> List.map (fun (id, _, mexp) -> Module (id, module_expr mexp)) id_mty_mexps @ st
  | Tstr_modtype (id, mty) -> Module_type (id, module_type mty) :: st
  | Tstr_open _ -> st
  | Tstr_class cinfoss -> List.map (fun (cinfos, _, _) -> Class [cinfos.ci_id_class; cinfos.ci_id_class_type; cinfos.ci_id_object; cinfos.ci_id_typesharp]) cinfoss @ st
  | Tstr_class_type id_cinfoss ->
      List.map (fun (id, cinfos) -> Class_type [id; cinfos.ci_id_class; cinfos.ci_id_class_type; cinfos.ci_id_object; cinfos.ci_id_typesharp]) id_cinfoss @ st 
  | Tstr_include (mexp, ids) -> Include (module_expr mexp, Some ids) :: st

and signature sg : t list = List.fold_right signature_item sg.sig_items []

and signature_item sitem st : t list = match sitem.sig_desc with
  | Tsig_value (id, { val_val = { val_kind = Val_prim _; _ }; _} ) -> Primitive id :: st
  | Tsig_value (id, _) -> Value id :: st
  | Tsig_type id_decls -> List.map (fun (id, _) -> Type id) id_decls @ st
  | Tsig_exception (id, _) -> Exception id :: st 
  | Tsig_module (id, mty) -> Module (id, module_type mty) :: st
  | Tsig_recmodule id_mtys -> List.map (fun (id, mty) -> Module (id, module_type mty)) id_mtys @ st
  | Tsig_modtype (id, mtyd) -> Module_type (id, modtype_declaration mtyd) :: st
  | Tsig_open _ -> st
  | Tsig_include (mty, _sg) (* CR jfuruse *)  -> Include (module_type mty, None) :: st
  | Tsig_class cinfoss -> List.map (fun cinfos -> Class [cinfos.ci_id_class; cinfos.ci_id_class_type; cinfos.ci_id_object; cinfos.ci_id_typesharp]) cinfoss @ st
  | Tsig_class_type cinfoss -> List.map (fun cinfos -> Class_type [cinfos.ci_id_class; cinfos.ci_id_class_type; cinfos.ci_id_object; cinfos.ci_id_typesharp]) cinfoss @ st

and module_expr mexp : module_ = match mexp.mod_desc with
  | Tmod_ident p -> Path p
  | Tmod_structure str -> Structure (structure str)
  | Tmod_functor (id, _, mexp) -> Functor (id, module_expr mexp)
  | Tmod_apply (mexp1, mexp2, _) -> Apply (module_expr mexp1, module_expr mexp2)
  | Tmod_constraint (mexp, mty, _, _) -> Constraint (module_expr mexp, types_module_type mty)
  | Tmod_unpack (_, mty) -> types_module_type mty

and module_type mty : module_ = match mty.mty_desc with
  | Tmty_ident p -> Path p
  | Tmty_signature sg -> Structure (signature sg)
  | Tmty_functor (id, _mty1, mty2) -> Functor (id, module_type mty2)
  | Tmty_with (mty, _) -> module_type mty
  | Tmty_typeof mexp -> module_expr mexp

and modtype_declaration = function 
  | Tmodtype_abstract -> Abstract
  | Tmodtype_manifest mty -> module_type mty

and types_module_type = function
  | Mty_ident p -> Path p
  | Mty_functor (id, _mty1, mty2) -> Functor (id, types_module_type mty2)
  | Mty_signature sg ->
      Structure (List.map (function
        | Sig_value (id, { val_kind = Val_prim _; _ }) -> Primitive id
        | Sig_value (id, _) -> Value id
        | Sig_type (id, _, _) -> Type id
        | Sig_exception (id, _) -> Exception id
        | Sig_module (id, mty, _) -> Module (id, types_module_type mty)
        | Sig_modtype (id, mtyd) -> Module (id, types_modtype_declaration mtyd)
        | Sig_class (id, _, _) -> Class [id]
        | Sig_class_type (id, _, _) -> Class_type [id]) sg)

and types_modtype_declaration = function
  | Modtype_abstract -> Abstract
  | Modtype_manifest mty -> types_module_type mty

module Format = struct

  let rec structure ppf str = fprintf ppf "{ @[<v>%a@] }" (Format.list "; " structure_item) str.str_items

  and structure_item ppf sitem = match sitem.str_desc with
    | Tstr_eval _ -> ()
    | Tstr_value (_, pat_exps) -> 
        List.iter (fun id -> fprintf ppf "val %a" Ident.format id) (let_bound_idents pat_exps)
    | Tstr_primitive (id, _) -> fprintf ppf "prim %a" Ident.format id
    | Tstr_type id_decls -> List.iter (fun (id, _) -> fprintf ppf "type %a" Ident.format id) id_decls
    | Tstr_exception (id, _) 
    | Tstr_exn_rebind (id, _) -> fprintf ppf "exception %a" Ident.format id
    | Tstr_module (id, mexp) -> fprintf ppf "@[<2>module %a =@ @[%a@]@]" Ident.format id module_expr mexp
    | Tstr_recmodule id_mty_mexps -> 
        fprintf ppf "@[<2>module rec @[%a@]@]"
          (Format.list " and " (fun ppf (id, _, mexp) -> 
            fprintf ppf "@[<2> %a = @ @[%a@]@]" Ident.format id module_expr mexp))
          id_mty_mexps
    | Tstr_modtype (id, mty) -> fprintf ppf "@[<2>modtype %a = @[%a@]@]" Ident.format id module_type mty
    | Tstr_open _ -> ()
    | Tstr_class cinfoss -> 
        List.iter (fun (cinfos, _, _) -> fprintf ppf "@[<2> class @[%a@]@]" class_infos cinfos) cinfoss
    | Tstr_class_type id_cinfoss ->
        List.iter (fun (id, cinfos) -> 
          fprintf ppf "@[<2>class type %a = @[%a@]@]" Ident.format id class_infos cinfos) id_cinfoss
    | Tstr_include (mexp, ids) -> 
        fprintf ppf "@[<2>include @[%a@] : { @[%a@] }@ :: { @[%a@] }@]" 
          module_expr mexp
          types_module_type mexp.mod_type
          (Format.list "; " Ident.format) ids

  and class_infos : 'a. Format.t -> 'a class_infos -> unit = fun ppf cinfos -> 
    fprintf ppf "{ class=%a; class_type=%a; type=%a; typesharp=%a }"
      Ident.format cinfos.ci_id_class
      Ident.format cinfos.ci_id_class_type
      Ident.format cinfos.ci_id_object
      Ident.format cinfos.ci_id_typesharp

  and signature ppf sg = fprintf ppf "{ @[<v>%a@] }" (Format.list "; " signature_item) sg.sig_items

  and signature_item ppf sitem = match sitem.sig_desc with
    | Tsig_value (id, { val_val = { val_kind = Val_prim _; _ }; _} ) -> fprintf ppf "prim %a" Ident.format id
    | Tsig_value (id, _) -> fprintf ppf "val %a" Ident.format id
    | Tsig_type id_decls -> 
        List.iter (fun (id, _) -> fprintf ppf "type %a" Ident.format id) id_decls
    | Tsig_exception (id, _) -> fprintf ppf "exception %a" Ident.format id
    | Tsig_module (id, mty) -> fprintf ppf "@[<2>module %a =@ @[%a@]@]" Ident.format id module_type mty
    | Tsig_recmodule id_mtys -> 
        fprintf ppf "@[<2>module rec @[%a@]@]"
          (Format.list " and " (fun ppf (id, mty) -> 
            fprintf ppf "@[<2> %a = @ @[%a@]@]" Ident.format id module_type mty))
          id_mtys
    | Tsig_modtype (id, mtyd) -> 
        fprintf ppf "@[<2>modtype %a = @[%a@]@]" Ident.format id modtype_declaration mtyd
    | Tsig_open _ -> ()
    | Tsig_include (mty, _sg) -> 
        fprintf ppf "@[<2>include @[%a@] : { all }@]" 
          module_type mty
    | Tsig_class cinfoss -> 
        List.iter (fun cinfos -> fprintf ppf "@[<2> class @[%a@]@]" class_infos cinfos) cinfoss
    | Tsig_class_type cinfoss -> 
         List.iter (fun cinfos -> 
          fprintf ppf "@[<2>class type _ = @[%a@]@]" class_infos cinfos) cinfoss

  and module_expr ppf mexp = match mexp.mod_desc with
    | Tmod_ident p -> Path.format ppf p
    | Tmod_structure str -> structure ppf str
    | Tmod_functor (id, _, mexp) -> 
        fprintf ppf "@[<2>fun %a ->@ @[%a@]@]" Ident.format id module_expr mexp
    | Tmod_apply (mexp1, mexp2, _) -> fprintf ppf "@[%a(%a)@]" module_expr mexp1 module_expr mexp2
    | Tmod_constraint (mexp, mty, _, _) -> 
        fprintf ppf "@[<2>%a@ : %a@]" module_expr mexp types_module_type mty 
    | Tmod_unpack (_, mty) -> 
        fprintf ppf "@[<2>unpack@ %a@]" types_module_type mty

  and module_type ppf mty = match mty.mty_desc with
    | Tmty_ident p -> Path.format ppf p
    | Tmty_signature sg -> signature ppf sg
    | Tmty_functor (id, _mty1, mty2) ->
        fprintf ppf "@[<2>fun %a ->@ @[%a@]@]" Ident.format id module_type mty2
    | Tmty_with (mty, _) -> module_type ppf mty
    | Tmty_typeof mexp -> module_expr ppf mexp

  and modtype_declaration ppf = function 
    | Tmodtype_abstract -> fprintf ppf "<abst>"
    | Tmodtype_manifest mty -> module_type ppf mty

  and types_module_type ppf = function
    | Mty_ident p -> Path.format ppf p
    | Mty_functor (id, _mty1, mty2) ->
        fprintf ppf "@[<2>fun %a ->@ @[%a@]@]" Ident.format id types_module_type mty2
    | Mty_signature sg ->
        fprintf ppf "@[<v>%a@]" 
          (Format.list "; " (fun ppf -> function
            | Sig_value (id, { val_kind = Val_prim _; _ }) -> fprintf ppf "prim %a" Ident.format id
            | Sig_value (id, _) -> fprintf ppf "val %a" Ident.format id
            | Sig_type (id, _, _) -> fprintf ppf "type %a" Ident.format id
            | Sig_exception (id, _) -> fprintf ppf "exception %a" Ident.format id
            | Sig_module (id, mty, _) -> 
                fprintf ppf "@[<2>module %a =@ @[%a@]@]" Ident.format id types_module_type mty
            | Sig_modtype (id, mtyd) -> 
                fprintf ppf "@[<2>modtype %a = @[%a@]@]" Ident.format id types_modtype_declaration mtyd
            | Sig_class (id, _, _) -> fprintf ppf "@[class %a@]" Ident.format id
            | Sig_class_type (id, _, _) -> fprintf ppf "@[class type %a@]" Ident.format id)) sg

  and types_modtype_declaration ppf = function
    | Modtype_abstract -> fprintf ppf "<abst>"
    | Modtype_manifest mty -> types_module_type ppf mty

  let saved_type ppf = function
    | Typedtree.Saved_implementation str | Typedtree.Saved_structure str -> structure ppf str
    | Typedtree.Saved_signature sg -> signature ppf sg
    | Typedtree.Saved_structure_item _
    | Typedtree.Saved_signature_item _
    | Typedtree.Saved_expression _
    | Typedtree.Saved_module_type _
    | Typedtree.Saved_pattern _
    | Typedtree.Saved_class_expr _ -> ()

  let saved_types ppf = 
    fprintf ppf "@[<2>{ @[%a@] }@]@." (Format.list "; " saved_type)
end

