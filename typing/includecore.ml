(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Inclusion checks for the core language *)

open Misc
open Path
open Types
open Typedtree

(* Inclusion between value descriptions *)

exception Dont_match

let value_descriptions env vd1 vd2 =
  if Ctype.moregeneral env vd1.val_type vd2.val_type then begin
    match (vd1.val_kind, vd2.val_kind) with
        (Val_prim p1, Val_prim p2) ->
          if p1 = p2 then Tcoerce_none else raise Dont_match
      | (Val_prim p, _) -> Tcoerce_primitive p
      | (_, Val_prim p) -> raise Dont_match
      | (_, _) -> Tcoerce_none
  end else
    raise Dont_match

(* Inclusion between type declarations *)

let type_declarations env id decl1 decl2 =
  decl1.type_arity = decl2.type_arity &
  begin match (decl1.type_kind, decl2.type_kind) with
      (_, Type_abstract) -> true
    | (Type_variant cstrs1, Type_variant cstrs2) ->
        for_all2
          (fun (cstr1, arg1) (cstr2, arg2) ->
            cstr1 = cstr2 &
            for_all2
              (fun ty1 ty2 ->
                Ctype.equal env true (ty1::decl1.type_params)
                                     (ty2::decl2.type_params))
              arg1 arg2)
          cstrs1 cstrs2
    | (Type_record labels1, Type_record labels2) ->
        for_all2
          (fun (lbl1, mut1, ty1) (lbl2, mut2, ty2) ->
            lbl1 = lbl2 & mut1 = mut2 &
            Ctype.equal env true (ty1::decl1.type_params)
                                 (ty2::decl2.type_params))
          labels1 labels2
    | (_, _) -> false
  end &
  begin match (decl1.type_manifest, decl2.type_manifest) with
      (_, None) ->
        Ctype.equal env true decl1.type_params decl2.type_params
    | (Some ty1, Some ty2) ->
        Ctype.equal env true (ty1::decl1.type_params)
                             (ty2::decl2.type_params)
    | (None, Some ty2) ->
        let ty1 =
	  {desc = Tconstr(Pident id, decl2.type_params, ref Mnil);
	   level = Ctype.generic_level }
	in
          Ctype.equal env true decl1.type_params decl2.type_params
            &
          Ctype.equal env false [ty1] [ty2]
  end

(* Inclusion between exception declarations *)

let exception_declarations env ed1 ed2 =
  for_all2 (fun ty1 ty2 -> Ctype.equal env false [ty1] [ty2]) ed1 ed2

(* Inclusion between class types *)
let encode_val (mut, ty) rem =
  begin match mut with
    Asttypes.Mutable   -> Predef.type_unit
  | Asttypes.Immutable -> Ctype.newgenty Tvar
  end
  ::ty::rem

let vars vars1 vars2 =
  Vars.fold
    (fun lab v2 (vl1, vl2) ->
       (begin try
          encode_val (Vars.find lab vars1) vl1
        with Not_found ->
          vl1
        end,
        encode_val v2 vl2))
    vars2 ([], [])

let class_type env d1 d2 =
  (* Same abbreviations *)
  Ctype.equal env true
    (d1.cty_self::d1.cty_params) (d2.cty_self::d2.cty_params)
      &&
  (* Same concretes methods *)
  Concr.equal d1.cty_concr d2.cty_concr
      &&
  (* If virtual, stays virtual *)
  (d1.cty_new <> None or d2.cty_new = None)
      &&
  (* Less general *)
  let (v1, v2) = vars d1.cty_vars d2.cty_vars in
  let t1 = Ctype.newgenty (Ttuple (d1.cty_self::v1@d1.cty_args)) in
  let t2 = Ctype.newgenty (Ttuple (d2.cty_self::v2@d2.cty_args)) in
  Ctype.moregeneral env t1 t2
