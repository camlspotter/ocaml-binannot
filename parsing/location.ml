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

open Lexing

type t =
  { loc_start: int; loc_end: int; loc_ghost: bool }

let none = { loc_start = -1; loc_end = -1; loc_ghost = true }

let symbol_rloc () =
  { loc_start = Parsing.symbol_start(); loc_end = Parsing.symbol_end();
    loc_ghost = false }

let symbol_gloc () =
  { loc_start = Parsing.symbol_start(); loc_end = Parsing.symbol_end();
    loc_ghost = true }

let rhs_loc n =
  { loc_start = Parsing.rhs_start n; loc_end = Parsing.rhs_end n;
    loc_ghost = false }

let input_name = ref ""

let input_lexbuf = ref (None : lexbuf option)

(* Terminal info *)

let status = ref Terminfo.Uninitialised


(* Print the location using standout mode. *)

let num_loc_lines = ref 0 (* number of lines already printed after input *)

let rec highlight_locations loc1 loc2 =
  match !status with
    Terminfo.Uninitialised ->
      status := Terminfo.setup stdout; highlight_locations loc1 loc2
  | Terminfo.Bad_term ->
      false
  | Terminfo.Good_term num_lines ->
      match !input_lexbuf with
        None -> false
      | Some lb ->
          try
            (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
            let pos0 = -lb.lex_abs_pos in
            (* Do nothing if the buffer does not contain the whole phrase. *)
            if pos0 < 0 then raise Exit;
            (* Count number of lines in phrase *)
            let lines = ref !num_loc_lines in
            for i = pos0 to String.length lb.lex_buffer - 1 do
              if lb.lex_buffer.[i] = '\n' then incr lines
            done;
            (* If too many lines, give up *)
            if !lines >= num_lines - 2 then raise Exit;
            (* Move cursor up that number of lines *)
            flush stdout; Terminfo.backup !lines;
            (* Print the input, switching to standout for the location *)
            let bol = ref false in
            print_string "# ";
            for pos = 0 to String.length lb.lex_buffer - pos0 - 1 do
              if !bol then (print_string "  "; bol := false);
              if pos = loc1.loc_start || pos = loc2.loc_start then
                (flush stdout; Terminfo.standout true);
              if pos = loc1.loc_end || pos = loc2.loc_end then
                (flush stdout; Terminfo.standout false);
              let c = lb.lex_buffer.[pos + pos0] in
              print_char c;
              bol := (c = '\n')
            done;
            flush stdout;
            (* Make sure standout mode is over *)
            Terminfo.standout false;
            (* Position cursor back to original location *)
            Terminfo.resume !num_loc_lines;
            true;
          with Exit -> false

(* Print the location in some way or another *)

open Formatmsg

let reset () =
  num_loc_lines := 0

let (msg_file, msg_line, msg_chars, msg_to, msg_colon, msg_head) =
  match Sys.os_type with
  | "MacOS" -> ("File \"", "\"; line ", "; characters ", " to ", "", "### ")
  | _ -> ("File \"", "\", line ", ", characters ", "-", ":", "")

let print loc =
  if String.length !input_name = 0 then
    if highlight_locations loc none then () else
      printf "Characters %i-%i:@?" loc.loc_start loc.loc_end
  else begin
    let (filename, linenum, linebeg) =
            Linenum.for_position !input_name loc.loc_start in
    print_string msg_file; print_string filename;
    print_string msg_line; print_int linenum;
    print_string msg_chars; print_int (loc.loc_start - linebeg);
    print_string msg_to; print_int (loc.loc_end - linebeg);
    print_string msg_colon;
    force_newline();
    print_string msg_head;
  end

let print_warning loc w =
 if Warnings.is_active w then begin
  print loc;
  printf "Warning: %s@?" (Warnings.message w);
  incr num_loc_lines;
 end
;;

let echo_eof () =
  print_newline ();
  incr num_loc_lines
