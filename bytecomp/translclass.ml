(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Misc
open Asttypes
open Types
open Typedtree
open Lambda
open Translobj
open Translcore


let transl_label l = Lconst (Const_base (Const_string l))

let rec transl_meth_list =
  function
    []       -> Const_pointer 0
  | lab::rem -> Const_block (0, [Const_base (Const_string lab);
                                 transl_meth_list rem])

(* Instance variable initialization *)
let set_inst_var obj var id expr =
  Lprim(Parraysetu (if maybe_pointer expr then Paddrarray else Pintarray),
        [Lvar obj; Lvar id; transl_exp expr])


let transl_super tbl meths inh_methods rem =
  List.fold_right
    (fun (nm, id) rem ->
       Llet(StrictOpt, id, Lapply (oo_prim "get_method",
                                   [Lvar tbl; Lvar (Meths.find nm meths)]),
       rem))
    inh_methods rem

let transl_val tbl name id rem =
  Llet(StrictOpt, id, Lapply (oo_prim "get_variable",
                              [Lvar tbl; transl_label name]),
       rem)

let transl_private_val tbl name id rem =
  Llet(StrictOpt, id, Lapply (oo_prim "get_private_variable",
                              [Lvar tbl; transl_label name]),
       rem)

let transl_vals tbl vals rem =
  List.fold_right
    (fun (name, id) rem -> transl_val tbl name id rem)
    vals rem

let inherited_values vals =
  Lconst
    (List.fold_right
       (fun (v, _) rem ->
          Const_block(0, [Const_base (Const_string v); rem]))
       vals (Const_pointer 0))

let inherited_meths methods =
  Lconst
    (List.fold_right
       (fun v rem ->
          Const_block(0, [Const_base (Const_string v); rem]))
       methods (Const_pointer 0))

let transl_field_obj obj field (obj_init, anc_id) =
  match field with
    Cf_inher (name, args, vals, inh_meths, meths') ->
      let init = Ident.create "init" in
      (Lsequence(Lapply(Lvar init, Lvar obj :: (List.map transl_exp args)),
                 obj_init),
       init::anc_id)
  | Cf_val (name, id, priv, Some exp) ->
      (Lsequence(set_inst_var obj name id exp, obj_init),
       anc_id)
  | Cf_val (name, id, priv, None) ->
      (obj_init, anc_id)
  | Cf_meth (name, exp) ->
      (obj_init, anc_id)

let transl_field_cl tbl meths field cl_init =
  match field with
    Cf_inher (name, args, vals, inh_meths, meths') ->
      Lsequence(Lapply (oo_prim "inheritance",
                        [Lvar tbl; transl_path name;
                         inherited_values vals;
                         inherited_meths meths']),
                transl_vals tbl vals (
                transl_super tbl meths inh_meths cl_init))
  | Cf_val (name, id, priv, exp) ->
      if priv = Private then
        transl_private_val tbl name id cl_init
      else
        transl_val tbl name id cl_init
  | Cf_meth (name, exp) ->
      Lsequence(Lapply (oo_prim "set_method",
                        [Lvar tbl; transl_label name; transl_exp exp]),
                cl_init)

let transl_val_hiding tbl cl_init =
  function
    Cf_inher _ | Cf_meth _ | Cf_val (_, _, Public, _) ->
      cl_init
  | Cf_val (name, id, Private, exp) ->
      Lsequence(Lapply (oo_prim "hide_variable",
                        [Lvar tbl; transl_label name]),
                cl_init)

let bind_methods tbl public_methods lab id cl_init =
  if List.mem lab public_methods then
    Llet(Alias, id, Lvar (meth lab), cl_init)
  else
    Llet(StrictOpt, id, Lapply (oo_prim "get_method_label",
                                [Lvar tbl; transl_label lab]),
    cl_init)

let transl_fields tbl public_methods meths fields cl_init =
  match fields with
    Cf_inher _ as inh :: fields ->
      transl_field_cl tbl meths inh 
        (Meths.fold (bind_methods tbl public_methods) meths
           (List.fold_right (transl_field_cl tbl meths) fields cl_init))
  | fields ->
      Meths.fold (bind_methods tbl public_methods) meths
        (List.fold_right (transl_field_cl tbl meths) fields cl_init)


let transl_class cl_id cl =
  let public_methods = Lconst (transl_meth_list cl.cl_pub_meths) in
  let obj = Ident.create "obj" in
  let (field_init, anc_id) =
    List.fold_right (transl_field_obj obj) cl.cl_field (Lvar obj, [])
  in
  let (params, body) =
    List.fold_right
      (fun pat (params, rem) ->
        let param = name_pattern "param" [pat, ()] in
        (param::params,
         Matching.for_function pat.pat_loc None (Lvar param) [pat, rem]))
      cl.cl_args
      ([], field_init)
  in
  let obj_init = Lfunction(Curried, anc_id @ obj::params, body) in
  let table = Ident.create "table" in
  let cl_init =
    Lfunction (Curried, [table],
               List.fold_left (transl_val_hiding table)
                 (transl_fields table cl.cl_pub_meths cl.cl_meths cl.cl_field
                    (Lapply (oo_prim "set_initializer",
                             [Lvar table; obj_init])))
                 cl.cl_field)
  in
  Lapply (oo_prim "create_class", [Lvar cl_id; public_methods; cl_init])

let class_stub =
  Lprim(Pmakeblock(0, Mutable), [lambda_unit; lambda_unit; lambda_unit])
