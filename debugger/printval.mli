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

val max_printer_depth : int ref
val max_printer_steps : int ref

val print_value :
  int -> Debugcom.remote_value -> Types.type_expr -> Env.t -> unit
val print_named_value :
  int -> Debugcom.remote_value -> Types.type_expr -> Env.t -> unit
val print_ident_value :
  int -> Longident.t -> Debugcom.remote_value -> Types.type_expr -> Env.t ->
    unit

val reset_named_values : unit -> unit
val find_named_value : int -> Debugcom.remote_value * Types.type_expr
