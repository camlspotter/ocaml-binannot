(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

exception Exc_located of Token.flocation and exn;

value raise_with_loc loc exc =
  match exc with
  [ Exc_located _ _ -> raise exc
  | _ -> raise (Exc_located loc exc) ]
;

value line_of_loc fname (bp, ep) =
  (bp.Lexing.pos_fname,
   bp.Lexing.pos_lnum,
   bp.Lexing.pos_cnum - bp.Lexing.pos_bol,
   ep.Lexing.pos_cnum - bp.Lexing.pos_bol)
;

(*
value line_of_loc fname (bp, ep) =
  try
    let ic = open_in_bin fname in
    let strm = Stream.of_channel ic in
    let rec loop fname lin =
      let rec not_a_line_dir col =
        parser cnt
          [: `c; s :] ->
            if cnt < bp then
              if c = '\n' then loop fname (lin + 1)
              else not_a_line_dir (col + 1) s
            else
              let col = col - (cnt - bp) in
              (fname, lin, col, col + ep - bp)
      in
      let rec a_line_dir str n col =
        parser
        [ [: `'\n' :] -> loop str n
        | [: `_; s :] -> a_line_dir str n (col + 1) s ]
      in
      let rec spaces col =
        parser
        [ [: `' '; s :] -> spaces (col + 1) s
        | [: :] -> col ]
      in
      let rec check_string str n col =
        parser
        [ [: `'"'; col = spaces (col + 1); s :] -> a_line_dir str n col s
        | [: `c when c <> '\n'; s :] ->
            check_string (str ^ String.make 1 c) n (col + 1) s
        | [: a = not_a_line_dir col :] -> a ]
      in
      let check_quote n col =
        parser
        [ [: `'"'; s :] -> check_string "" n (col + 1) s
        | [: a = not_a_line_dir col :] -> a ]
      in
      let rec check_num n col =
        parser
        [ [: `('0'..'9' as c); s :] ->
            check_num (10 * n + Char.code c - Char.code '0') (col + 1) s
        | [: col = spaces col; s :] -> check_quote n col s ]
      in
      let begin_line =
        parser
        [ [: `'#'; col = spaces 1; s :] -> check_num 0 col s
        | [: a = not_a_line_dir 0 :] -> a ]
      in
      begin_line strm
    in
    let r = try loop fname 1 with [ Stream.Failure -> (fname, 1, bp, ep) ] in
    do { close_in ic; r }
  with
  [ Sys_error _ -> (fname, 1, bp, ep) ]
;
*)

value loc_name = ref "_loc";
