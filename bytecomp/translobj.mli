(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Lambda

val oo_prim: string -> lambda

val share: structured_constant -> lambda
val meth: string -> Ident.t

val reset_labels: unit -> unit
val transl_label_init: lambda -> lambda

val oo_wrap: Env.t -> bool -> ('a -> lambda) -> 'a -> lambda
val oo_add_class: Ident.t -> Env.t * bool
