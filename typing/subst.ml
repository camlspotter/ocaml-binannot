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
    modtypes: (Ident.t, module_type) Tbl.t }

let identity =
  { types = Tbl.empty; modules = Tbl.empty; modtypes = Tbl.empty }

let add_type id p s =
  { types = Tbl.add id p s.types;
    modules = s.modules;
    modtypes = s.modtypes }

let add_module id p s =
  { types = s.types;
    modules = Tbl.add id p s.modules;
    modtypes = s.modtypes }

let add_modtype id ty s =
  { types = s.types;
    modules = s.modules;
    modtypes = Tbl.add id ty s.modtypes }

let remove_type id s =
  { types = Tbl.remove id s.types;
    modules = s.modules;
    modtypes = s.modtypes }

let remove_module id s =
  { types = s.types;
    modules = Tbl.remove id s.modules;
    modtypes = s.modtypes }

let remove_modtype id s =
  { types = s.types;
    modules = s.modules;
    modtypes = Tbl.remove id s.modtypes }

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

(* Similar to [Ctype.nondep_type_rec]. *)
let rec typexp s ty =
  let ty = repr ty in
  if (ty.desc = Tvar) || (ty.level < lowest_level) then
    ty
  else begin
    let desc = ty.desc in
    save_desc ty desc;
    let ty' = newmarkedgenvar () in     (* Stub *)
    ty.desc <- Tlink ty';
    ty'.desc <-
      begin match desc with
        Tvar | Tlink _ ->
          fatal_error "Subst.typexp"
      | Tarrow(t1, t2) ->
          Tarrow(typexp s t1, typexp s t2)
      | Ttuple tl ->
          Ttuple(List.map (typexp s) tl)
      | Tconstr(p, tl, abbrev) ->
          Tconstr(type_path s p, List.map (typexp s) tl, ref Mnil)
      | Tobject (t1, name) ->
          Tobject (typexp s t1,
                 ref (match !name with
                        None -> None
                      | Some (p, tl) ->
                          Some (type_path s p, List.map (typexp s) tl)))
      | Tfield(label, kind, t1, t2) ->
          begin match field_kind_repr kind with
            Fpresent ->
              Tfield(label, Fpresent, typexp s t1, typexp s t2)
          | Fabsent ->
              Tlink (typexp s t2)
          | Fvar _ (* {contents = None} *) as k ->
              Tfield(label, k, typexp s t1, typexp s t2)
          end
      | Tnil ->
          Tnil
      end;
    ty'
  end

(*
   Always make a copy of the type. If this is not done, type levels
   might not be correct.
*)
let type_expr s ty =
  let ty' = typexp s ty in
  cleanup_types ();
  unmark_type ty';
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
        | Type_record lbls ->
            Type_record(
              List.map (fun (n, mut, arg) -> (n, mut, typexp s arg))
                       lbls)
        end;
      type_manifest =
        begin match decl.type_manifest with
          None -> None
        | Some ty -> Some(typexp s ty)
        end
    }
  in
  cleanup_types ();
  unmark_type_decl decl;
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
  | Tcty_fun (ty, cty) ->
      Tcty_fun (typexp s ty, class_type s cty)

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
  List.iter unmark_type decl.cty_params;
  unmark_class_type decl.cty_type;
  begin match decl.cty_new with
    None    -> ()
  | Some ty -> unmark_type ty
  end;
  decl

let cltype_declaration s decl =
  let decl =
    { clty_params = List.map (typexp s) decl.clty_params;
      clty_type = class_type s decl.clty_type;
      clty_path = type_path s decl.clty_path }
  in
  cleanup_types ();
  List.iter unmark_type decl.clty_params;
  unmark_class_type decl.clty_type;
  decl

let class_type s cty =
  let cty = class_type s cty in
  cleanup_types ();
  unmark_class_type cty;
  cty

let value_description s descr =
  { val_type = type_expr s descr.val_type;
    val_kind = descr.val_kind }

let exception_declaration s tyl =
  List.map (type_expr s) tyl

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
      Tmty_functor(id, modtype s arg, modtype (remove_module id s) res)

and signature s = function
    [] -> []
  | Tsig_value(id, d) :: sg ->
      Tsig_value(id, value_description s d) :: signature s sg
  | Tsig_type(id, d) :: sg ->
      Tsig_type(id, type_declaration s d) :: signature (remove_type id s) sg
  | Tsig_exception(id, d) :: sg ->
      Tsig_exception(id, exception_declaration s d) :: signature s sg
  | Tsig_module(id, mty) :: sg ->
      Tsig_module(id, modtype s mty) :: signature (remove_module id s) sg
  | Tsig_modtype(id, d) :: sg ->
      Tsig_modtype(id, modtype_declaration s d) ::
      signature (remove_modtype id s) sg
  | Tsig_class(id, d) :: sg ->
      Tsig_class(id, class_declaration s d) :: signature s sg
  | Tsig_cltype(id, d) :: sg ->
      Tsig_cltype(id, cltype_declaration s d) :: signature s sg

and modtype_declaration s = function
    Tmodtype_abstract -> Tmodtype_abstract
  | Tmodtype_manifest mty -> Tmodtype_manifest(modtype s mty)
