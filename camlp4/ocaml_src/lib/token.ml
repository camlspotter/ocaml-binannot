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

(* Id *)

type t = string * string;;
type pattern = string * string;;

exception Error of string;;

type location = int * int;;
type location_function = int -> int * int;;
type lexer_func = char Stream.t -> t Stream.t * location_function;;

type lexer =
  { func : lexer_func;
    using : pattern -> unit;
    removing : pattern -> unit;
    tparse : pattern -> (t Stream.t -> string) option;
    text : pattern -> string }
;;

let lexer_text (con, prm) =
  if con = "" then "'" ^ prm ^ "'"
  else if prm = "" then con
  else con ^ " '" ^ prm ^ "'"
;;

let locerr () = invalid_arg "Lexer: location function";;
let loct_create () = ref (Array.create 1024 None);;
let loct_func loct i =
  match
    if i < 0 || i >= Array.length !loct then None
    else Array.unsafe_get !loct i
  with
    Some loc -> loc
  | _ -> locerr ()
;;
let loct_add loct i loc =
  if i >= Array.length !loct then
    begin
      let new_tmax = Array.length !loct * 2 in
      let new_loct = Array.create new_tmax None in
      Array.blit !loct 0 new_loct 0 (Array.length !loct); loct := new_loct
    end;
  !loct.(i) <- Some loc
;;

let make_stream_and_location next_token_loc =
  let loct = loct_create () in
  let ts =
    Stream.from
      (fun i ->
         let (tok, loc) = next_token_loc () in loct_add loct i loc; Some tok)
  in
  ts, loct_func loct
;;

let lexer_func_of_parser next_token_loc cs =
  make_stream_and_location (fun () -> next_token_loc cs)
;;

let lexer_func_of_ocamllex lexfun cs =
  let lb =
    Lexing.from_function
      (fun s n ->
         try s.[0] <- Stream.next cs; 1 with
           Stream.Failure -> 0)
  in
  let next_token_loc _ =
    let tok = lexfun lb in
    let loc = Lexing.lexeme_start lb, Lexing.lexeme_end lb in tok, loc
  in
  make_stream_and_location next_token_loc
;;

(* Char and string tokens to real chars and string *)

let buff = ref (String.create 80);;
let store len x =
  if len >= String.length !buff then
    buff := !buff ^ String.create (String.length !buff);
  !buff.[len] <- x;
  succ len
;;
let mstore len s =
  let rec add_rec len i =
    if i == String.length s then len else add_rec (store len s.[i]) (succ i)
  in
  add_rec len 0
;;
let get_buff len = String.sub !buff 0 len;;

let valch x = Char.code x - Char.code '0';;

let rec backslash s i =
  if i = String.length s then raise Not_found
  else
    match s.[i] with
      'n' -> '\n', i + 1
    | 'r' -> '\r', i + 1
    | 't' -> '\t', i + 1
    | 'b' -> '\b', i + 1
    | '\\' -> '\\', i + 1
    | '0'..'9' as c -> backslash1 (valch c) s (i + 1)
    | _ -> raise Not_found
and backslash1 cod s i =
  if i = String.length s then '\\', i - 1
  else
    match s.[i] with
      '0'..'9' as c -> backslash2 (10 * cod + valch c) s (i + 1)
    | _ -> '\\', i - 1
and backslash2 cod s i =
  if i = String.length s then '\\', i - 2
  else
    match s.[i] with
      '0'..'9' as c -> Char.chr (10 * cod + valch c), i + 1
    | _ -> '\\', i - 2
;;

let rec skip_indent s i =
  if i = String.length s then i
  else
    match s.[i] with
      ' ' | '\t' -> skip_indent s (i + 1)
    | _ -> i
;;

let skip_opt_linefeed s i =
  if i = String.length s then i else if s.[i] = '\010' then i + 1 else i
;;

let eval_char s =
  if String.length s = 1 then s.[0]
  else if String.length s = 0 then failwith "invalid char token"
  else if s.[0] = '\\' then
    if String.length s = 2 && s.[1] = '\'' then '\''
    else
      try
        let (c, i) = backslash s 1 in
        if i = String.length s then c else raise Not_found
      with
        Not_found -> failwith "invalid char token"
  else failwith "invalid char token"
;;

let eval_string s =
  let rec loop len i =
    if i = String.length s then get_buff len
    else
      let (len, i) =
        if s.[i] = '\\' then
          let i = i + 1 in
          if i = String.length s then failwith "invalid string token"
          else if s.[i] = '"' then store len '"', i + 1
          else
            match s.[i] with
              '\010' -> len, skip_indent s (i + 1)
            | '\013' -> len, skip_indent s (skip_opt_linefeed s (i + 1))
            | c ->
                try let (c, i) = backslash s i in store len c, i with
                  Not_found -> store (store len '\\') c, i + 1
        else store len s.[i], i + 1
      in
      loop len i
  in
  loop 0 0
;;
