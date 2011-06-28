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
open Spot
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
  | Value id -> fprintf ppf "val %s" (Ident.name id)
  | Primitive id -> fprintf ppf "val prim %s" (Ident.name id)
  | Type id -> fprintf ppf "type %s" (Ident.name id)
  | Exception id -> fprintf ppf "exception %s" (Ident.name id)
  | Module (id, module_) -> fprintf ppf "@[<2>module %s = %a@]" (Ident.name id) format_module_ module_
  | Module_type (id, module_) -> fprintf ppf "@[<2>module type %s = {@ %a }@]" (Ident.name id) format_module_ module_
  | Class ids -> fprintf ppf "class [%a]" (list "; " (fun ppf id -> fprintf ppf "%s" (Ident.name id))) ids
  | Class_type ids -> fprintf ppf "class type [%a]" (list "; " (fun ppf id -> fprintf ppf "%s" (Ident.name id))) ids
  | Include (module_, ids_opt) -> 
      fprintf ppf "@[<2>include {@ %a }%a@]" 
        format_module_ module_ 
        (fun ppf -> function
          | None -> ()
          | Some ids -> 
              fprintf ppf " [%a]" (list "; " (fun ppf id -> fprintf ppf "%s" (Ident.name id))) ids) ids_opt
        
and format_module_ ppf = function
  | Path p -> fprintf ppf "%s" (Path.name p)
  | Structure ts -> fprintf ppf "{ @[%a@] }" (list "; " format) ts
  | Functor (id, module_) -> fprintf ppf "\\%s -> %a" (Ident.name id) format_module_ module_
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
  | Tsig_include mty -> Include (module_type mty, None) :: st
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
