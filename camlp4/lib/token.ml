(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type t = (string * string);
type pattern = (string * string);

exception Error of string;

type location = (int * int);
type location_function = int -> (int * int);
type lexer_func = Stream.t char -> (Stream.t t * location_function);

type lexer =
  { func : lexer_func;
    using : pattern -> unit;
    removing : pattern -> unit;
    tparse : pattern -> option (Stream.t t -> string);
    text : pattern -> string }
;

value lexer_text (con, prm) =
  if con = "" then "'" ^ prm ^ "'"
  else if prm = "" then con
  else con ^ " '" ^ prm ^ "'"
;

value locerr () = invalid_arg "Lexer: location function";
value loct_create () = (ref (Array.create 1024 None), ref False);
value loct_func (loct, ov) i =
  match
    if i < 0 || i >= Array.length loct.val then
      if ov.val then Some (0, 0) else None
    else Array.unsafe_get loct.val i
  with
  [ Some loc -> loc
  | _ -> locerr () ]
;
value loct_add (loct, ov) i loc =
  do {
    if i >= Array.length loct.val then do {
      let new_tmax = Array.length loct.val * 2 in
      if new_tmax < Sys.max_array_length then do {
        let new_loct = Array.create new_tmax None in
        Array.blit loct.val 0 new_loct 0 (Array.length loct.val);
        loct.val := new_loct;
        loct.val.(i) := Some loc
      }
      else ov.val := True
    }
    else loct.val.(i) := Some loc
  }
;

value make_stream_and_location next_token_loc =
  let loct = loct_create () in
  let ts =
    Stream.from
      (fun i ->
         let (tok, loc) = next_token_loc () in
         do { loct_add loct i loc; Some tok })
  in
  (ts, loct_func loct)
;

value lexer_func_of_parser next_token_loc cs =
  make_stream_and_location (fun () -> next_token_loc cs)
;

value lexer_func_of_ocamllex lexfun cs =
  let lb =
    Lexing.from_function
      (fun s n ->
         try do { s.[0] := Stream.next cs; 1 } with [ Stream.Failure -> 0 ])
  in
  let next_token_loc _ =
    let tok = lexfun lb in
    let loc = (Lexing.lexeme_start lb, Lexing.lexeme_end lb) in
    (tok, loc)
  in
  make_stream_and_location next_token_loc
;

(* Char and string tokens to real chars and string *)

value buff = ref (String.create 80);
value store len x =
  do {
    if len >= String.length buff.val then
      buff.val := buff.val ^ String.create (String.length buff.val)
    else ();
    buff.val.[len] := x;
    succ len
  }
;
value mstore len s =
  add_rec len 0 where rec add_rec len i =
    if i == String.length s then len else add_rec (store len s.[i]) (succ i)
;
value get_buff len = String.sub buff.val 0 len;

value valch x = Char.code x - Char.code '0';

value rec backslash s i =
  if i = String.length s then raise Not_found
  else
    match s.[i] with
    [ 'n' -> ('\n', i + 1)
    | 'r' -> ('\r', i + 1)
    | 't' -> ('\t', i + 1)
    | 'b' -> ('\b', i + 1)
    | '\\' -> ('\\', i + 1)
    | '0'..'9' as c -> backslash1 (valch c) s (i + 1)
    | _ -> raise Not_found ]
and backslash1 cod s i =
  if i = String.length s then ('\\', i - 1)
  else
    match s.[i] with
    [ '0'..'9' as c -> backslash2 (10 * cod + valch c) s (i + 1)
    | _ -> ('\\', i - 1) ]
and backslash2 cod s i =
  if i = String.length s then ('\\', i - 2)
  else
    match s.[i] with
    [ '0'..'9' as c -> (Char.chr (10 * cod + valch c), i + 1)
    | _ -> ('\\', i - 2) ]
;

value rec skip_indent s i =
  if i = String.length s then i
  else
    match s.[i] with
    [ ' ' | '\t' -> skip_indent s (i + 1)
    | _ -> i ]
;

value skip_opt_linefeed s i =
  if i = String.length s then i else if s.[i] = '\010' then i + 1 else i
;

value eval_char s =
  if String.length s = 1 then s.[0]
  else if String.length s = 0 then failwith "invalid char token"
  else if s.[0] = '\\' then
    if String.length s = 2 && s.[1] = ''' then '''
    else
      try
        let (c, i) = backslash s 1 in
        if i = String.length s then c else raise Not_found
      with
      [ Not_found -> failwith "invalid char token" ]
  else failwith "invalid char token"
;

value eval_string s =
  loop 0 0 where rec loop len i =
    if i = String.length s then get_buff len
    else
      let (len, i) =
        if s.[i] = '\\' then
          let i = i + 1 in
          if i = String.length s then failwith "invalid string token"
          else if s.[i] = '"' then (store len '"', i + 1)
          else
            match s.[i] with
            [ '\010' -> (len, skip_indent s (i + 1))
            | '\013' -> (len, skip_indent s (skip_opt_linefeed s (i + 1)))
            | c ->
                try
                  let (c, i) = backslash s i in
                  (store len c, i)
                with
                [ Not_found -> (store (store len '\\') c, i + 1) ] ]
        else (store len s.[i], i + 1)
      in
      loop len i
;
