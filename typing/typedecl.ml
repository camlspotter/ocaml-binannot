(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(**** Typing of type definitions ****)

open Misc
open Parsetree
open Types
open Typedtree
open Typetexp

type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string
  | Definition_mismatch of type_expr
  | Unconsistent_constraint
  | Type_clash of (type_expr * type_expr) list
  | Null_arity_external
  | Unbound_type_var

exception Error of Location.t * error

(* Enter all declared types in the environment as abstract types *)

let rec enter_types env = function
    ([], []) ->
      (env, [])
  | ((name, sdecl) :: srem, id :: irem) ->
      let decl =
        { type_params = List.map (fun _ -> Ctype.newvar ()) sdecl.ptype_params;
          type_arity = List.length sdecl.ptype_params;
          type_kind = Type_abstract;
          type_manifest =
            match sdecl.ptype_manifest with
              None -> None
            | Some _ -> Some (Ctype.newvar ()) }
      in
      let extenv = Env.add_type id decl env in
      let (ext_env, decl_rem) = enter_types extenv (srem, irem) in
      (ext_env, (id, decl) :: decl_rem)
  | _ ->
      fatal_error "Typedecl.enter_types"

(* Translate one type declaration *)

module StringSet =
  Set.Make(struct
    type t = string
    let compare = compare
  end)

(* First pass: parameters, constraints and expansion *)
let transl_declaration env (name, sdecl) (id, decl) =
  reset_type_variables();
  begin try
    List.iter2
      (fun ty sty -> Ctype.unify env ty (enter_type_variable true sty))
      decl.type_params sdecl.ptype_params
  with Already_bound ->
    raise(Error(sdecl.ptype_loc, Repeated_parameter))
  end;

  begin match sdecl.ptype_manifest with
    None ->
      ()
  | Some sty ->
      let ty = transl_simple_type env true sty in
      if Ctype.cyclic_abbrev env id ty then
        raise(Error(sdecl.ptype_loc, Recursive_abbrev name));
      begin try
        Ctype.unify env ty (Ctype.newconstr (Path.Pident id) decl.type_params) 
      with Ctype.Unify trace ->
        raise(Error(sdecl.ptype_loc, Type_clash trace))
      end
  end;

  List.iter
    (function (sty, sty', loc) ->
       try
         Ctype.unify env (transl_simple_type env false sty)
                         (transl_simple_type env false sty')
       with Ctype.Unify _ ->
         raise(Error(loc, Unconsistent_constraint)))
    sdecl.ptype_cstrs;

  (id, decl)

(* Second pass: representation *)
let transl_declaration2 env (name, sdecl) (id, decl) =
  let (params, typ) =
    match decl.type_manifest with
      None -> (Ctype.instance_list decl.type_params, None)
    | Some typ ->
        let (params, typ) =
          Ctype.instance_parameterized_type decl.type_params typ
        in
        (params, Some typ)
  in

  (* Bind type parameters *)
  reset_type_variables();
  List.iter2
    (fun ty sty -> Ctype.unify env ty (enter_type_variable true sty))
    params sdecl.ptype_params;

  let decl' =
    { type_params = params;
      type_arity = decl.type_arity;
      type_kind =
        begin match sdecl.ptype_kind with
          Ptype_abstract ->
            Type_abstract
        | Ptype_variant cstrs ->
            let all_constrs = ref StringSet.empty in
            List.iter
              (fun (name, args) ->
                if StringSet.mem name !all_constrs then
                  raise(Error(sdecl.ptype_loc, Duplicate_constructor name));
                all_constrs := StringSet.add name !all_constrs)
              cstrs;
            if List.length cstrs > Config.max_tag then
              raise(Error(sdecl.ptype_loc, Too_many_constructors));
            Type_variant(List.map
              (fun (name, args) ->
                      (name, List.map (transl_simple_type env true) args))
              cstrs)
        | Ptype_record lbls ->
            let all_labels = ref StringSet.empty in
            List.iter
              (fun (name, mut, arg) ->
                if StringSet.mem name !all_labels then
                  raise(Error(sdecl.ptype_loc, Duplicate_label name));
                all_labels := StringSet.add name !all_labels)
              lbls;
            Type_record(List.map
              (fun (name, mut, arg) ->
                      (name, mut, transl_simple_type env true arg))
              lbls)
        end;
      type_manifest = typ } in
  (id, decl')

(* Generalize a type declaration *)

let generalize_decl decl =
  List.iter Ctype.generalize decl.type_params;
  begin match decl.type_kind with
    Type_abstract ->
      ()
  | Type_variant v ->
      List.iter (fun (_, tyl) -> List.iter Ctype.generalize tyl) v
  | Type_record r ->
      List.iter (fun (_, _, ty) -> Ctype.generalize ty) r
  end;
  begin match decl.type_manifest with
    None    -> ()
  | Some ty -> Ctype.generalize ty
  end

(*
   If both a variant/record definition and a type equation are given,
   need to check that the equation refers to a type of the same kind
   with the same constructors and labels.
*)
let check_abbrev env (_, sdecl) (id, decl) =
  match decl with
    {type_kind = (Type_variant _ | Type_record _); type_manifest = Some ty} ->
      begin match (Ctype.repr ty).desc with
        Tconstr(path, args, _) ->
          begin try
            let decl' = Env.find_type path env in
            if List.length args = List.length decl.type_params
            && Ctype.equal env false args decl.type_params
            && Includecore.type_declarations env id
                decl'
                (Subst.type_declaration (Subst.add_type id path Subst.identity)
                                        decl)
            then ()
            else raise(Error(sdecl.ptype_loc, Definition_mismatch ty))
          with Not_found ->
            raise(Error(sdecl.ptype_loc, Definition_mismatch ty))
          end
      | _ -> raise(Error(sdecl.ptype_loc, Definition_mismatch ty))
      end
  | _ -> ()

(* Check for ill-defined abbrevs *)

(* Occur check *)
let check_recursive_abbrev env (name, sdecl) (id, decl) =
  match decl.type_manifest with
    Some ty ->
      begin try Ctype.correct_abbrev env id decl.type_params ty with
        Ctype.Recursive_abbrev ->
          raise(Error(sdecl.ptype_loc, Recursive_abbrev name))
      end
  | _ ->
      ()

(* Translate a set of mutually recursive type declarations *)
let transl_type_decl env name_sdecl_list =
  (* Create identifiers. *)
  let id_list =
    List.map (fun (name, _) -> Ident.create name) name_sdecl_list
  in
  (*
     Since we've introduced fresh idents, make sure the definition
     level is at least the binding time of these events. Otherwise,
     passing one of the recursively-defined type constrs as argument
     to an abbreviation may fail.
  *)
  Ctype.init_def(Ident.current_time());
  Ctype.begin_def();
  (* Enter types. *)
  let (temp_env, temp_decl) = enter_types env (name_sdecl_list, id_list) in
  (* Translate each declaration. *)
  let decls =
    List.map2 (transl_declaration temp_env) name_sdecl_list temp_decl in
  (* Generalize intermediate type declarations. *)
  Ctype.end_def();
  List.iter (function (_, decl) -> generalize_decl decl) decls;
  (* Build an env. containing type expansions *)
  let temp_env =
    List.fold_right
      (fun (id, decl) env -> Env.add_type id decl env)
      decls env
  in
  (* Check for recursive abbrevs *)
  List.iter2 (check_recursive_abbrev temp_env) name_sdecl_list decls;
  Ctype.begin_def();
  let decls =
    List.map2 (transl_declaration2 temp_env) name_sdecl_list decls in
  (* Generalize final type declarations. *)
  Ctype.end_def();
  List.iter (function (_, decl) -> generalize_decl decl) decls;
  (* Build the final env. *)
  let newenv =
    List.fold_right
      (fun (id, decl) env -> Env.add_type id decl env)
      decls env
  in
  (* Check that all type variable are closed *)
  List.iter2
    (fun (_, sdecl) (id, decl) ->
       match Ctype.closed_type_decl decl with
         Some _ -> raise(Error(sdecl.ptype_loc, Unbound_type_var))
       | None   -> ())
    name_sdecl_list decls;
  (* Check re-exportation *)
  List.iter2 (check_abbrev newenv) name_sdecl_list decls;
  (* Done *)
  (decls, newenv)

(* Translate an exception declaration *)
let transl_exception env excdecl =
  reset_type_variables();
  Ctype.begin_def();
  let types = List.map (transl_simple_type env true) excdecl in
  Ctype.end_def();
  List.iter Ctype.generalize types;
  types

(* Translate a value declaration *)
let transl_value_decl env valdecl =
  let ty = Typetexp.transl_type_scheme env valdecl.pval_type in
  match valdecl.pval_prim with
    [] ->
      { val_type = ty; val_kind = Val_reg }
  | decl ->
      let arity = Ctype.arity ty in
      if arity = 0 then
        raise(Error(valdecl.pval_type.ptyp_loc, Null_arity_external));
      let prim = Primitive.parse_declaration arity decl in
      { val_type = ty; val_kind = Val_prim prim }

(* Translate a "with" constraint -- much simplified version of
    transl_type_decl. *)
let transl_with_constraint env sdecl =
  reset_type_variables();
  Ctype.begin_def();
  let params =
    try
      List.map (enter_type_variable true) sdecl.ptype_params
    with Already_bound ->
      raise(Error(sdecl.ptype_loc, Repeated_parameter)) in
  List.iter
    (function (ty, ty', loc) ->
       try
         Ctype.unify env (transl_simple_type env false ty)
                         (transl_simple_type env false ty')
       with Ctype.Unify _ ->
         raise(Error(loc, Unconsistent_constraint)))
    sdecl.ptype_cstrs;
  let decl =
    { type_params = params;
      type_arity = List.length params;
      type_kind = Type_abstract;
      type_manifest =
        begin match sdecl.ptype_manifest with
          None -> None
        | Some sty -> Some(transl_simple_type env true sty)
        end }
  in
  Ctype.end_def();
  generalize_decl decl;
  decl

(**** Error report ****)

open Formatmsg

let report_error = function
    Repeated_parameter ->
      print_string "A type parameter occurs several times"
  | Duplicate_constructor s ->
      print_string "Two constructors are named "; print_string s
  | Too_many_constructors ->
      print_string "Too many constructors -- maximum is ";
      print_int Config.max_tag; print_string " constructors"
  | Duplicate_label s ->
      print_string "Two labels are named "; print_string s
  | Recursive_abbrev s ->
      print_string "The type abbreviation "; print_string s;
      print_string " is cyclic" (* " expands to itself" *)
  | Definition_mismatch ty ->
      Printtyp.reset ();
      Printtyp.mark_loops ty;
      print_string
        "The variant or record definition does not match that of type";
      print_space(); Printtyp.type_expr ty
  | Unconsistent_constraint ->
      print_string "The type constraints are not consistent"
  | Type_clash trace ->
      Printtyp.unification_error true trace
        (function () ->
           print_string "This type constructor expands to type")
        (function () ->
           print_string "but is here used with type")
  | Null_arity_external ->
      print_string "External identifiers must be functions"
  | Unbound_type_var ->
      print_string "A type variable is unbound in this type declaration";
