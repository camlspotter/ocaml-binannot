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

(* Error report *)

open Formatmsg
open Location

(* Report an error *)

let report_error exn =
  open_box 0;
  begin match exn with
    Lexer.Error(err, start, stop) ->
      Location.print {loc_start = start; loc_end = stop; loc_ghost = false};
      Lexer.report_error err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error err
  | Env.Error err ->
      Env.report_error err
  | Typecore.Error(loc, err) ->
      Location.print loc; Typecore.report_error err
  | Typetexp.Error(loc, err) ->
      Location.print loc; Typetexp.report_error err
  | Typedecl.Error(loc, err) ->
      Location.print loc; Typedecl.report_error err
  | Includemod.Error err ->
      Includemod.report_error err
  | Typemod.Error(loc, err) ->
      Location.print loc; Typemod.report_error err
  | Translcore.Error(loc, err) ->
      Location.print loc; Translcore.report_error err
  | Compilenv.Error code ->
      Compilenv.report_error code
  | Asmgen.Error code ->
      Asmgen.report_error code
  | Asmlink.Error code ->
      Asmlink.report_error code
  | Asmlibrarian.Error code ->
      Asmlibrarian.report_error code
  | Sys_error msg ->
      printf "I/O error: %s" msg
  | Typeclass.Error(loc, err) ->
      Location.print loc; Typeclass.report_error err
  | x ->
      close_box(); raise x
  end;
  close_box(); print_newline()
