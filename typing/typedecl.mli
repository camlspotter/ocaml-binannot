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

(* Typing of type definitions and primitive definitions *)

open Types
open Format

val transl_type_decl:
        Env.t -> (string * Parsetree.type_declaration) list ->
                                  (Ident.t * type_declaration) list * Env.t
val transl_exception:
        Env.t -> Parsetree.exception_declaration -> exception_declaration

val transl_value_decl:
        Env.t -> Parsetree.value_description -> value_description

val transl_with_constraint:
        Env.t -> Parsetree.type_declaration -> type_declaration
    
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

val report_error: formatter -> error -> unit
