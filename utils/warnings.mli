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

type t =
  | Partial_match of string          (* P *)
  | Unused_match                     (* U *)
  | Method_override of string list   (* M *)
  | Hide_instance_variable of string (* V *)
  | Partial_application              (* F *)
  | Statement_type                   (* S *)
  | Comment of string                (* C *)
  | Other of string                  (* X *)
;;

val parse_options : string -> unit;;

val is_active : t -> bool;;

val message : t -> string;;
