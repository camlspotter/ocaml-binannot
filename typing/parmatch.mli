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

(* Detection of partial matches and unused match cases. *)
open Types
open Typedtree

val omega : pattern
val omegas : int -> pattern list
val omega_list : 'a list -> pattern list
val normalize_pat : pattern -> pattern
val all_record_args :
    (label_description * pattern) list -> (label_description * pattern) list

val le_pat : pattern -> pattern -> bool
val le_pats : pattern list -> pattern list -> bool
val compat : pattern -> pattern -> bool
val compats : pattern list -> pattern list -> bool
exception Empty
val lub : pattern -> pattern -> pattern
val lubs : pattern list -> pattern list -> pattern list

val get_mins : ('a -> 'a -> bool) -> 'a list -> 'a list

val set_args : pattern -> pattern list -> pattern list

val pat_of_constr : pattern -> constructor_description -> pattern
val complete_constrs :
    pattern -> constructor_tag list -> constructor_description  list

val check_partial:
        Env.t -> Location.t -> (pattern * expression) list -> partial
val check_unused: Env.t -> (pattern * expression) list -> unit


val top_pretty : Format.formatter -> pattern -> unit
