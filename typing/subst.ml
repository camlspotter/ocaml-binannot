(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Substitutions *)

open Misc
open Path
open Types
open Btype

type t = 
  { types: (Ident.t, Path.t) Tbl.t;
    modules: (Ident.t, Path.t) Tbl.t;
    modtypes: (Ident.t, module_type) Tbl.t;
    for_saving: bool }

let identity =
  { types = Tbl.empty; modules = Tbl.empty; modtypes = Tbl.empty;
    for_saving = false }

let add_type id p s = { s with types = Tbl.add id p s.types }

let add_module id p s = { s with modules = Tbl.add id p s.modules }

let add_modtype id ty s = { s with modtypes = Tbl.add id ty s.modtypes }

let for_saving s = { s with for_saving = true }

let rec module_path s = function
    Pident id as p ->
      begin try Tbl.find id s.modules with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
  | Papply(p1, p2) ->
      Papply(module_path s p1, module_path s p2)

let type_path s = function
    Pident id as p ->
      begin try Tbl.find id s.types with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
  | Papply(p1, p2) ->
      fatal_error "Subst.type_path"

(* Special type ids for saved signatures *)

let new_id = ref (-1)

let newpersvar () =
  decr new_id; { desc = Tvar; level = generic_level; id = !new_id }

(* Similar to [Ctype.nondep_type_rec]. *)
let rec typexp s ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      if s.for_saving then
        let ty' = newpersvar () in
        save_desc ty Tvar; ty.desc <- Tsubst ty'; ty'
      else ty
  | Tsubst ty ->
      ty
(* cannot do it, since it would omit subsitution
  | Tvariant row when not (static_row row) ->
      ty
*)
  | _ ->
    let desc = ty.desc in
    save_desc ty desc;
    (* Make a stub *)
    let ty' = if s.for_saving then newpersvar () else newgenvar () in
    ty.desc <- Tsubst ty';
    ty'.desc <-
      begin match desc with
      | Tconstr(p, tl, abbrev) ->
          Tconstr(type_path s p, List.map (typexp s) tl, ref Mnil)
      | Tobject (t1, name) ->
          Tobject (typexp s t1,
                 ref (match !name with
                        None -> None
                      | Some (p, tl) ->
                          Some (type_path s p, List.map (typexp s) tl)))
      | Tvariant row ->
          let row = row_repr row in
          let more = repr row.row_more in
          (* We must substitute in a subtle way *)
          begin match more.desc with
            Tsubst ty2 ->
              (* This variant type has been already copied *)
              ty.desc <- Tsubst ty2; (* avoid Tlink in the new type *)
              Tlink ty2
          | _ ->
              let static = static_row row in
              (* Register new type first for recursion *)
              save_desc more more.desc;
              more.desc <- ty.desc;
              let more' =
                if s.for_saving then newpersvar () else
                if static then newgenvar () else more in
              (* Return a new copy *)
              let row = copy_row (typexp s) row (not s.for_saving) more' in
              let row =
                if s.for_saving then {row with row_bound = []} else row in
              match row.row_name with
                Some (p, tl) ->
                  Tvariant {row with row_name = Some (type_path s p, tl)}
              | None ->
                  Tvariant row
          end
      | Tfield(label, kind, t1, t2) ->
          begin match field_kind_repr kind with
            Fpresent ->
              Tfield(label, Fpresent, typexp s t1, typexp s t2)
          | Fabsent ->
              Tlink (typexp s t2)
          | Fvar _ (* {contents = None} *) as k ->
              Tfield(label, k, typexp s t1, typexp s t2)
          end
      | _ -> copy_type_desc (typexp s) desc
      end;
    ty'

(*
   Always make a copy of the type. If this is not done, type levels
   might not be correct.
*)
let type_expr s ty =
  let ty' = typexp s ty in
  cleanup_types ();
  ty'

let type_declaration s decl =
  let decl =
    { type_params = List.map (typexp s) decl.type_params;
      type_arity = decl.type_arity;
      type_kind =
        begin match decl.type_kind with
          Type_abstract -> Type_abstract
        | Type_variant cstrs ->
            Type_variant(
              List.map (fun (n, args) -> (n, List.map (typexp s) args))
                       cstrs)
        | Type_record(lbls, rep) ->
            Type_record(
              List.map (fun (n, mut, arg) -> (n, mut, typexp s arg))
                       lbls,
              rep)
        end;
      type_manifest =
        begin match decl.type_manifest with
          None -> None
        | Some ty -> Some(typexp s ty)
        end;
      type_variance = decl.type_variance;
    }
  in
  cleanup_types ();
  decl

let class_signature s sign =
  { cty_self = typexp s sign.cty_self;
    cty_vars = Vars.map (function (m, t) -> (m, typexp s t)) sign.cty_vars;
    cty_concr = sign.cty_concr }

let rec class_type s =
  function
    Tcty_constr (p, tyl, cty) ->
      Tcty_constr (type_path s p, List.map (typexp s) tyl, class_type s cty)
  | Tcty_signature sign ->
      Tcty_signature (class_signature s sign)
  | Tcty_fun (l, ty, cty) ->
      Tcty_fun (l, typexp s ty, class_type s cty)

let class_declaration s decl =
  let decl =
    { cty_params = List.map (typexp s) decl.cty_params;
      cty_type = class_type s decl.cty_type;
      cty_path = type_path s decl.cty_path;
      cty_new =
        begin match decl.cty_new with
          None    -> None
        | Some ty -> Some (typexp s ty)
        end }
  in
  cleanup_types ();
  decl

let cltype_declaration s decl =
  let decl =
    { clty_params = List.map (typexp s) decl.clty_params;
      clty_type = class_type s decl.clty_type;
      clty_path = type_path s decl.clty_path }
  in
  cleanup_types ();
  decl

let class_type s cty =
  let cty = class_type s cty in
  cleanup_types ();
  cty

let value_description s descr =
  { val_type = type_expr s descr.val_type;
    val_kind = descr.val_kind }

let exception_declaration s tyl =
  List.map (type_expr s) tyl

let rec rename_bound_idents s idents = function
    [] -> (List.rev idents, s)
  | Tsig_type(id, d) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_type id (Pident id') s) (id' :: idents) sg
  | Tsig_module(id, mty) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_module id (Pident id') s) (id' :: idents) sg
  | Tsig_modtype(id, d) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_modtype id (Tmty_ident(Pident id')) s)
                          (id' :: idents) sg
  | (Tsig_value(id, _) | Tsig_exception(id, _) | 
     Tsig_class(id, _) | Tsig_cltype(id, _)) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents s (id' :: idents) sg

let rec modtype s = function
    Tmty_ident p as mty ->
      begin match p with
        Pident id ->
          begin try Tbl.find id s.modtypes with Not_found -> mty end
      | Pdot(p, n, pos) ->
          Tmty_ident(Pdot(module_path s p, n, pos))
      | Papply(p1, p2) ->
          fatal_error "Subst.modtype"
      end
  | Tmty_signature sg ->
      Tmty_signature(signature s sg)
  | Tmty_functor(id, arg, res) ->
      let id' = Ident.rename id in
      Tmty_functor(id', modtype s arg,
                        modtype (add_module id (Pident id') s) res)

and signature s sg =
  (* Components of signature may be mutually recursive (e.g. type declarations
     or class and type declarations), so first build global renaming
     substitution... *)
  let (new_idents, s') = rename_bound_idents s [] sg in
  (* ... then apply it to each signature component in turn *)
  List.map2 (signature_component s') sg new_idents

and signature_component s comp newid =
  match comp with
    Tsig_value(id, d) ->
      Tsig_value(newid, value_description s d)
  | Tsig_type(id, d) ->
      Tsig_type(newid, type_declaration s d)
  | Tsig_exception(id, d) ->
      Tsig_exception(newid, exception_declaration s d)
  | Tsig_module(id, mty) ->
      Tsig_module(newid, modtype s mty)
  | Tsig_modtype(id, d) ->
      Tsig_modtype(newid, modtype_declaration s d)
  | Tsig_class(id, d) ->
      Tsig_class(newid, class_declaration s d)
  | Tsig_cltype(id, d) ->
      Tsig_cltype(newid, cltype_declaration s d)

and modtype_declaration s = function
    Tmodtype_abstract -> Tmodtype_abstract
  | Tmodtype_manifest mty -> Tmodtype_manifest(modtype s mty)
