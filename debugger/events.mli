(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Instruct

(** Current events. **)

(* The event at current position. *)
val current_event : debug_event option ref

(* Recompute the current event *)
val update_current_event : unit -> unit

(* Current position in source. *)
(* Raise `Not_found' if not on an event (beginning or end of program). *)
val current_point : unit -> string * int

val current_event_is_before : unit -> bool

