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

(* The interactive toplevel loop *)

val loop: unit -> unit

(* Read and execute a script from the given file *)

val run_script: string -> bool (* true if successful, false if error *)

(* Interface with toplevel directives *)

type directive_fun =
    Directive_none of (unit -> unit)
  | Directive_string of (string -> unit)
  | Directive_int of (int -> unit)
  | Directive_ident of (Longident.t -> unit)
  | Directive_bool of (bool -> unit)

val directive_table: (string, directive_fun) Hashtbl.t
        (* Table of known directives, with their execution function *)
val toplevel_env: Env.t ref
        (* Typing environment for the toplevel *)
val print_exception_outcome: exn -> unit
        (* Print an exception resulting from the evaluation of user code. *)
val execute_phrase: bool -> Parsetree.toplevel_phrase -> bool
        (* Execute the given toplevel phrase. Return [true] if the
           phrase executed with no errors and [false] otherwise.
           First bool says whether the values and types of the results
           should be printed. Uncaught exceptions are always printed. *)
val use_file: string -> bool
val use_silently: string -> bool
        (* Read and execute commands from a file.
           [use_file] prints the types and values of the results.
           [use_silently] does not print them. *)

(* Hooks for an external parser *)

val parse_toplevel_phrase : (Lexing.lexbuf -> Parsetree.toplevel_phrase) ref
val parse_use_file : (Lexing.lexbuf -> Parsetree.toplevel_phrase list) ref
val print_location : Location.t -> unit
val print_warning : Location.t -> Warnings.t -> unit
val input_name : string ref
