(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis && Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format

type t =                             (* A is all *)
  | Comment of string                (* C *)
  | Partial_application              (* F *)
  | Method_override of string list   (* M *)
  | Partial_match of string          (* P *)
  | Statement_type                   (* S *)
  | Unused_match                     (* U *)
  | Hide_instance_variable of string (* V *)
  | Other of string                  (* X *)
;;

val parse_options : iserror:bool -> string -> unit;;

val is_active : t -> bool;;
val is_error : t -> bool;;

val print : formatter -> t -> unit;;


exception Errors of int;;

val check_fatal : unit -> unit;;
