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

(* The lexer generator. Command-line parsing. *)

open Syntax
open Lexgen

let ml_automata = ref false
let quiet_mode = ref false
let source_name = ref None
let output_name = ref None

let usage = "usage: ocamlex [options] sourcefile"

let specs =
  ["-ml", Arg.Set ml_automata,
    " Output code that does not use the Lexing module built-in automata interpreter";
   "-o", Arg.String (fun x -> source_name := Some x),
    " <file>  Set output file name to <file>";
   "-q", Arg.Set Common.quiet_mode, " Do not display informational messages";
  ] 

let _ =
  Arg.parse
    specs
    (fun name -> source_name := Some name)
    usage

  
let main () =

  let source_name = match !source_name with
  | None -> Arg.usage specs usage ; exit 2 
  | Some name -> name in
  let dest_name = match !output_name with
  | Some name -> name
  | None ->
      if Filename.check_suffix source_name ".mll" then
        Filename.chop_suffix source_name ".mll" ^ ".ml"
      else
        source_name ^ ".ml" in

  let ic = open_in_bin source_name in
  let oc = open_out dest_name in
  let tr = Common.open_tracker dest_name oc in
  let lexbuf = Lexing.from_channel ic in
  try
    let def = Parser.lexer_definition Lexer.main lexbuf in
    let (entries, transitions) = Lexgen.make_dfa def.entrypoints in
    if !ml_automata then begin
      Outputbis.output_lexdef
        source_name ic oc tr
        def.header entries transitions def.trailer
    end else begin
       let tables = Compact.compact_tables transitions in
       Output.output_lexdef source_name ic oc tr
         def.header tables entries def.trailer
    end;
    close_in ic;
    close_out oc;
    Common.close_tracker tr;
  with exn ->
    close_in ic;
    close_out oc;
    Common.close_tracker tr;
    Sys.remove dest_name;
    begin match exn with
      Parsing.Parse_error ->
        Printf.fprintf stderr
          "File \"%s\", line %d, character %d: syntax error.\n"
          source_name !Lexer.line_num
          (Lexing.lexeme_start lexbuf - !Lexer.line_start_pos)
    | Lexer.Lexical_error(msg, line, col) ->
        Printf.fprintf stderr
          "File \"%s\", line %d, character %d: %s.\n"
          source_name line col msg
    | Lexgen.Memory_overflow ->
        Printf.fprintf stderr
          "File \"%s\":\n Position memory overflow, too many bindings\n"
          source_name        
    | Output.Table_overflow ->
        Printf.fprintf stderr
          "File \"%s\":\ntransition table overflow, automaton is too big\n"
          source_name
    | _ ->
        raise exn
    end;
    exit 3

let _ = (* Printexc.catch *) main (); exit 0

