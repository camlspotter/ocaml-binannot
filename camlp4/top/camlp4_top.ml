(* camlp4r q_MLast.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Parsetree;
open Lexing;
open Stdpp;

value highlight_locations lb loc1 loc2 =
  try
    let pos0 = - lb.lex_abs_pos in
    do {
      if pos0 < 0 then raise Exit else ();
      let pos_at_bol = ref 0 in
      print_string "Toplevel input:\n# ";
      for pos = 0 to String.length lb.lex_buffer - pos0 - 1 do {
        let c = lb.lex_buffer.[pos + pos0] in
        if c = '\n' then do {
          if pos_at_bol.val <= fst loc1 && snd loc1 <= pos then do {
            print_string "\n  ";
            for i = pos_at_bol.val to fst loc1 - 1 do { print_char ' ' };
            for i = fst loc1 to snd loc1 - 1 do { print_char '^' };
            print_char '\n'
          }
          else if pos_at_bol.val <= fst loc1 && fst loc1 < pos then do {
            print_char '\r';
            print_char (if pos_at_bol.val = 0 then '#' else ' ');
            print_char ' ';
            for i = pos_at_bol.val to fst loc1 - 1 do { print_char '.' };
            print_char '\n'
          }
          else if pos_at_bol.val <= snd loc1 && snd loc1 < pos then do {
            for i = pos - 1 downto snd loc1 do { print_string "\008.\008" };
            print_char '\n'
          }
          else print_char '\n';
          pos_at_bol.val := pos + 1;
          if pos < String.length lb.lex_buffer - pos0 - 1 then
            print_string "  "
          else ()
        }
        else print_char c
      };
      flush stdout
    }
  with
  [ Exit -> () ]
;

value print_location lb loc =
  if String.length Toploop.input_name.val = 0 then
    highlight_locations lb loc (-1, -1)
  else Toploop.print_location Format.err_formatter (Ast2pt.mkloc loc)
;

value wrap f shfn lb =
  let cs =
    let shift = shfn lb in
    Stream.from
      (fun i ->
         if i < shift then Some ' '
         else do {
           while
             lb.lex_curr_pos >= lb.lex_buffer_len &&
             not lb.lex_eof_reached
           do {
             lb.refill_buff lb
           };
           if lb.lex_eof_reached then None
           else do {
             let c = lb.lex_buffer.[lb.lex_curr_pos] in
             lb.lex_curr_pos := lb.lex_curr_pos + 1;
             Some c
           }
         })
  in
  try f cs with
  [ Exc_located _ (Sys.Break as x) -> raise x
  | End_of_file as x -> raise x
  | x ->
      let x =
        match x with
        [ Exc_located loc x -> do { print_location lb loc; x }
        | x -> x ]
      in
      do {
        match x with
        [ Stream.Failure | Stream.Error _ -> Pcaml.sync.val cs
        | _ -> () ];
        Format.open_hovbox 0;
        Pcaml.report_error x;
        Format.close_box ();
        Format.print_newline ();
        raise Exit
      } ]
;

value toplevel_phrase cs =
  match Grammar.Entry.parse Pcaml.top_phrase cs with
  [ Some phr -> Ast2pt.phrase phr
  | None -> raise End_of_file ]
;

value use_file cs =
  let v = Pcaml.input_file.val in
  do {
    Pcaml.input_file.val := Toploop.input_name.val;
    let restore () = Pcaml.input_file.val := v in
    try
      let (pl0, eoi) =
        loop () where rec loop () =
          let (pl, stopped_at_directive) =
            Grammar.Entry.parse Pcaml.use_file cs
          in
          if stopped_at_directive then
            match pl with
            [ [MLast.StDir _ "load" (Some <:expr< $str:s$ >>)] ->
                do { Topdirs.dir_load Format.std_formatter s; loop () }
            | [MLast.StDir _ "directory" (Some <:expr< $str:s$ >>)] ->
                do { Topdirs.dir_directory s; loop () }
            | _ -> (pl, False) ]
          else (pl, True)
      in
      let pl =
        if eoi then []
        else
          loop () where rec loop () =
            let (pl, stopped_at_directive) =
              Grammar.Entry.parse Pcaml.use_file cs
            in
            if stopped_at_directive then pl @ loop () else pl
      in
      let r = pl0 @ pl in
      let r = List.map Ast2pt.phrase r in
      do { restore (); r }
    with e ->
      do { restore (); raise e }
  }
;

Toploop.parse_toplevel_phrase.val :=
  wrap toplevel_phrase (fun _ -> 0)
;

Toploop.parse_use_file.val :=
  wrap use_file (fun lb -> lb.lex_curr_pos - lb.lex_start_pos)
;

Pcaml.warning.val :=
  fun loc txt ->
    Toploop.print_warning (Ast2pt.mkloc loc) Format.err_formatter
      (Warnings.Other txt);

Printf.eprintf "\tCamlp4 Parsing version %s\n\n" Pcaml.version;
flush stderr;
