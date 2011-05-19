(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(***********************************************************************)
(*                                                                     *)
(*                    Contributed by OCamlPro                          *)
(*                                                                     *)
(***********************************************************************)

val untype_structure : Typedtree.structure -> Parsetree.structure
val untype_signature : Typedtree.signature -> Parsetree.signature

val untype_structure_item : Typedtree.structure_item -> Parsetree.structure_item
val untype_signature_item : Typedtree.signature_item -> Parsetree.signature_item
val untype_expression : Typedtree.expression -> Parsetree.expression
val untype_module_type : Typedtree.module_type -> Parsetree.module_type
val untype_pattern : Typedtree.pattern -> Parsetree.pattern
val untype_class_expr : Typedtree.class_expr -> Parsetree.class_expr
