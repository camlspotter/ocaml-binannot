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

value ast2pt_directive loc =
  fun
  [ None -> Pdir_none
  | Some <:expr< $str:s$ >> -> Pdir_string s
  | Some <:expr< $int:i$ >> -> Pdir_int (int_of_string i)
  | Some <:expr< True >> -> Pdir_bool True
  | Some <:expr< False >> -> Pdir_bool False
  | Some e ->
      let sl =
        loop e where rec loop =
          fun
          [ <:expr< $lid:i$ >> | <:expr< $uid:i$ >> -> [i]
          | <:expr< $e$ . $lid:i$ >> | <:expr< $e$ . $uid:i$ >> ->
              loop e @ [i]
          | e -> raise_with_loc (MLast.loc_of_expr e) (Failure "bad ast") ]
      in
      Pdir_ident (Ast2pt.long_id_of_string_list loc sl) ]
;

value ast2pt_phrase =
  fun
  [ MLast.StDir loc d dp -> Ptop_dir d (ast2pt_directive loc dp)
  | si -> Ptop_def (Ast2pt.str_item si []) ]
;

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

value wrap f lb =
  let cs =
    Stream.from
      (fun _ ->
         do {
           while
             lb.lex_curr_pos >= String.length lb.lex_buffer &&
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
  [ Some phr -> ast2pt_phrase phr
  | None -> raise End_of_file ]
;
value use_file cs =
  let v = Pcaml.input_file.val in
  do {
    Pcaml.input_file.val := Toploop.input_name.val;
    let restore () = Pcaml.input_file.val := v in
    try
      let r =
        loop () where rec loop () =
          let (pl, stopped_at_directive) =
            Grammar.Entry.parse Pcaml.use_file cs
          in
          if stopped_at_directive then
            match List.rev pl with
            [ [MLast.StDir _ "load" (Some <:expr< $str:s$ >>) :: rpl] ->
                do {
                  Topdirs.dir_load Format.std_formatter s;
                  List.rev_append rpl (loop ())
                }
            | [MLast.StDir _ "directory" (Some <:expr< $str:s$ >>) :: rpl] ->
                do { Topdirs.dir_directory s; List.rev_append rpl (loop ()) }
            | _ -> pl @ loop () ]
          else pl
      in
      let r = List.map ast2pt_phrase r in
      do { restore (); r }
    with e ->
      do { restore (); raise e }
  }
;

Toploop.parse_toplevel_phrase.val :=
  fun lb ->
    do {
      Printf.eprintf "\tCamlp4 Parsing version %s\n\n" Pcaml.version;
      flush stderr;
      Toploop.parse_toplevel_phrase.val := wrap toplevel_phrase;
      Toploop.parse_toplevel_phrase.val lb
    };
Toploop.parse_use_file.val := wrap use_file;

Pcaml.warning.val :=
  fun loc txt ->
    Toploop.print_warning (Ast2pt.mkloc loc) Format.err_formatter
      (Warnings.Other txt);
