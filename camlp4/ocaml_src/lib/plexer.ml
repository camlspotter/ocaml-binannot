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

open Stdpp;;
open Token;;

(* The string buffering machinery *)

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

(* The lexer *)

let rec ident len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some
      ('A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
       '\248'..'\255' | '0'..'9' | '_' | '\'' as c) ->
      Stream.junk strm__; ident (store len c) strm__
  | _ -> len
and ident2 len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some
      ('!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' | '%' |
       '.' | ':' | '<' | '>' | '|' | '$' as c) ->
      Stream.junk strm__; ident2 (store len c) strm__
  | _ -> len
and ident3 len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some
      ('0'..'9' | 'A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
       '\248'..'\255' | '_' | '!' | '%' | '&' | '*' | '+' | '-' | '.' | '/' |
       ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' | '\'' | '$' as c
         ) ->
      Stream.junk strm__; ident3 (store len c) strm__
  | _ -> len
and base_number len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some ('o' | 'O') ->
      Stream.junk strm__; octal_digits (store len 'o') strm__
  | Some ('x' | 'X') -> Stream.junk strm__; hexa_digits (store len 'x') strm__
  | Some ('b' | 'B') ->
      Stream.junk strm__; binary_digits (store len 'b') strm__
  | _ -> number len strm__
and octal_digits len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some ('0'..'7' as d) ->
      Stream.junk strm__; octal_digits (store len d) strm__
  | _ -> "INT", get_buff len
and hexa_digits len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some ('0'..'9' | 'a'..'f' | 'A'..'F' as d) ->
      Stream.junk strm__; hexa_digits (store len d) strm__
  | _ -> "INT", get_buff len
and binary_digits len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some ('0'..'1' as d) ->
      Stream.junk strm__; binary_digits (store len d) strm__
  | _ -> "INT", get_buff len
and number len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some ('0'..'9' as c) -> Stream.junk strm__; number (store len c) strm__
  | Some '.' -> Stream.junk strm__; decimal_part (store len '.') strm__
  | Some ('e' | 'E') ->
      Stream.junk strm__; exponent_part (store len 'E') strm__
  | _ -> "INT", get_buff len
and decimal_part len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some ('0'..'9' as c) ->
      Stream.junk strm__; decimal_part (store len c) strm__
  | Some ('e' | 'E') ->
      Stream.junk strm__; exponent_part (store len 'E') strm__
  | _ -> "FLOAT", get_buff len
and exponent_part len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some ('+' | '-' as c) ->
      Stream.junk strm__; end_exponent_part (store len c) strm__
  | _ -> end_exponent_part len strm__
and end_exponent_part len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some ('0'..'9' as c) ->
      Stream.junk strm__; end_exponent_part (store len c) strm__
  | _ -> "FLOAT", get_buff len
;;

let rec skip_spaces (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some (' ' | '\010' | '\013' | '\t' | '\026' | '\012') ->
      Stream.junk strm__; skip_spaces strm__
  | _ -> ()
;;

let error_on_unknown_keywords = ref false;;
let err loc msg = raise_with_loc loc (Token.Error msg);;

let next_token_fun dfa find_kwd =
  let keyword_or_error loc s =
    try ("", find_kwd s), loc with
      Not_found ->
        if !error_on_unknown_keywords then err loc ("illegal token: " ^ s)
        else ("", s), loc
  in
  let rec next_token (strm__ : _ Stream.t) =
    let bp = Stream.count strm__ in
    match Stream.peek strm__ with
      Some (' ' | '\010' | '\013' | '\t' | '\026' | '\012') ->
        Stream.junk strm__; next_token strm__
    | Some '(' -> Stream.junk strm__; left_paren bp strm__
    | Some '#' ->
        Stream.junk strm__; let s = strm__ in spaces_tabs s; linenum bp s
    | Some ('A'..'Z' | '\192'..'\214' | '\216'..'\222' as c) ->
        Stream.junk strm__;
        let s = strm__ in
        let id = get_buff (ident (store 0 c) s) in
        let loc = bp, Stream.count s in
        (try "", find_kwd id with
           Not_found -> "UIDENT", id),
        loc
    | Some ('a'..'z' | '\223'..'\246' | '\248'..'\255' | '_' as c) ->
        Stream.junk strm__;
        let s = strm__ in
        let id = get_buff (ident (store 0 c) s) in
        let loc = bp, Stream.count s in
        (try "", find_kwd id with
           Not_found -> "LIDENT", id),
        loc
    | Some ('1'..'9' as c) ->
        Stream.junk strm__;
        let tok = number (store 0 c) strm__ in
        let loc = bp, Stream.count strm__ in tok, loc
    | Some '0' ->
        Stream.junk strm__;
        let tok = base_number (store 0 '0') strm__ in
        let loc = bp, Stream.count strm__ in tok, loc
    | Some '\'' ->
        Stream.junk strm__;
        let s = strm__ in
        begin match Stream.npeek 2 s with
          [_; '\''] | ['\\'; _] ->
            let tok = "CHAR", char bp 0 s in
            let loc = bp, Stream.count s in tok, loc
        | _ -> keyword_or_error (bp, Stream.count s) "'"
        end
    | Some '"' ->
        Stream.junk strm__;
        let tok = "STRING", string bp 0 strm__ in
        let loc = bp, Stream.count strm__ in tok, loc
    | Some '$' ->
        Stream.junk strm__;
        let tok = dollar bp 0 strm__ in
        let loc = bp, Stream.count strm__ in tok, loc
    | Some ('!' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' | '%' as c) ->
        Stream.junk strm__;
        let id = get_buff (ident2 (store 0 c) strm__) in
        keyword_or_error (bp, Stream.count strm__) id
    | Some ('~' as c) ->
        Stream.junk strm__;
        begin try
          match Stream.peek strm__ with
            Some ('a'..'z' as c) ->
              Stream.junk strm__;
              let len =
                try ident (store 0 c) strm__ with
                  Stream.Failure -> raise (Stream.Error "")
              in
              let t =
                try
                  match Stream.peek strm__ with
                    Some ':' -> Stream.junk strm__; "TILDEIDENTCOLON"
                  | _ -> "TILDEIDENT"
                with
                  Stream.Failure -> raise (Stream.Error "")
              in
              let ep = Stream.count strm__ in (t, get_buff len), (bp, ep)
          | _ ->
              let id = get_buff (ident2 (store 0 c) strm__) in
              keyword_or_error (bp, Stream.count strm__) id
        with
          Stream.Failure -> raise (Stream.Error "")
        end
    | Some ('?' as c) ->
        Stream.junk strm__;
        begin try
          match Stream.peek strm__ with
            Some ('a'..'z' as c) ->
              Stream.junk strm__;
              let len =
                try ident (store 0 c) strm__ with
                  Stream.Failure -> raise (Stream.Error "")
              in
              let t =
                try
                  match Stream.peek strm__ with
                    Some ':' -> Stream.junk strm__; "QUESTIONIDENTCOLON"
                  | _ -> "QUESTIONIDENT"
                with
                  Stream.Failure -> raise (Stream.Error "")
              in
              let ep = Stream.count strm__ in (t, get_buff len), (bp, ep)
          | _ ->
              let id = get_buff (ident2 (store 0 c) strm__) in
              keyword_or_error (bp, Stream.count strm__) id
        with
          Stream.Failure -> raise (Stream.Error "")
        end
    | Some '<' -> Stream.junk strm__; less bp strm__
    | Some (':' as c1) ->
        Stream.junk strm__;
        let len =
          try
            match Stream.peek strm__ with
              Some (']' | ':' | '=' | '>' as c2) ->
                Stream.junk strm__; store (store 0 c1) c2
            | _ -> store 0 c1
          with
            Stream.Failure -> raise (Stream.Error "")
        in
        let ep = Stream.count strm__ in
        let id = get_buff len in keyword_or_error (bp, ep) id
    | Some ('>' | '|' as c1) ->
        Stream.junk strm__;
        let len =
          try
            match Stream.peek strm__ with
              Some (']' | '}' as c2) ->
                Stream.junk strm__; store (store 0 c1) c2
            | _ -> ident2 (store 0 c1) strm__
          with
            Stream.Failure -> raise (Stream.Error "")
        in
        let ep = Stream.count strm__ in
        let id = get_buff len in keyword_or_error (bp, ep) id
    | Some ('[' | '{' as c1) ->
        Stream.junk strm__;
        let s = strm__ in
        let len =
          match Stream.npeek 2 s with
            ['<'; '<' | ':'] -> store 0 c1
          | _ ->
              let (strm__ : _ Stream.t) = s in
              match Stream.peek strm__ with
                Some ('|' | '<' | ':' as c2) ->
                  Stream.junk strm__; store (store 0 c1) c2
              | _ -> store 0 c1
        in
        let ep = Stream.count s in
        let id = get_buff len in keyword_or_error (bp, ep) id
    | Some '.' ->
        Stream.junk strm__;
        let id =
          try
            match Stream.peek strm__ with
              Some '.' -> Stream.junk strm__; ".."
            | _ -> "."
          with
            Stream.Failure -> raise (Stream.Error "")
        in
        let ep = Stream.count strm__ in keyword_or_error (bp, ep) id
    | Some ';' ->
        Stream.junk strm__;
        let id =
          try
            match Stream.peek strm__ with
              Some ';' -> Stream.junk strm__; ";;"
            | _ -> ";"
          with
            Stream.Failure -> raise (Stream.Error "")
        in
        let ep = Stream.count strm__ in keyword_or_error (bp, ep) id
    | Some '\\' ->
        Stream.junk strm__;
        let ep = Stream.count strm__ in
        ("LIDENT", get_buff (ident3 0 strm__)), (bp, ep)
    | Some c ->
        Stream.junk strm__;
        let ep = Stream.count strm__ in
        keyword_or_error (bp, ep) (String.make 1 c)
    | _ -> let _ = Stream.empty strm__ in ("EOI", ""), (bp, succ bp)
  and less bp (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '<' ->
        Stream.junk strm__;
        let len =
          try quotation bp 0 strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        let ep = Stream.count strm__ in
        ("QUOTATION", ":" ^ get_buff len), (bp, ep)
    | Some ':' ->
        Stream.junk strm__;
        let i =
          try let len = ident 0 strm__ in get_buff len with
            Stream.Failure -> raise (Stream.Error "")
        in
        begin match Stream.peek strm__ with
          Some '<' ->
            Stream.junk strm__;
            let len =
              try quotation bp 0 strm__ with
                Stream.Failure -> raise (Stream.Error "")
            in
            let ep = Stream.count strm__ in
            ("QUOTATION", i ^ ":" ^ get_buff len), (bp, ep)
        | _ -> raise (Stream.Error "character '<' expected")
        end
    | _ ->
        let len = ident2 (store 0 '<') strm__ in
        let ep = Stream.count strm__ in
        let id = get_buff len in keyword_or_error (bp, ep) id
  and string bp len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '"' -> Stream.junk strm__; get_buff len
    | Some '\\' ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some c ->
            Stream.junk strm__; string bp (store (store len '\\') c) strm__
        | _ -> raise (Stream.Error "")
        end
    | Some c -> Stream.junk strm__; string bp (store len c) strm__
    | _ ->
        let ep = Stream.count strm__ in err (bp, ep) "string not terminated"
  and char bp len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '\'' ->
        Stream.junk strm__;
        let s = strm__ in
        if len = 0 then char bp (store len '\'') s else get_buff len
    | Some '\\' ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some c ->
            Stream.junk strm__; char bp (store (store len '\\') c) strm__
        | _ -> raise (Stream.Error "")
        end
    | Some c -> Stream.junk strm__; char bp (store len c) strm__
    | _ -> let ep = Stream.count strm__ in err (bp, ep) "char not terminated"
  and dollar bp len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '$' -> Stream.junk strm__; "ANTIQUOT", ":" ^ get_buff len
    | Some ('a'..'z' | 'A'..'Z' as c) ->
        Stream.junk strm__; antiquot bp (store len c) strm__
    | Some ('0'..'9' as c) ->
        Stream.junk strm__; maybe_locate bp (store len c) strm__
    | Some ':' ->
        Stream.junk strm__;
        let k = get_buff len in
        "ANTIQUOT", k ^ ":" ^ locate_or_antiquot_rest bp 0 strm__
    | Some '\\' ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some c ->
            Stream.junk strm__;
            "ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) strm__
        | _ -> raise (Stream.Error "")
        end
    | _ ->
        let s = strm__ in
        if dfa then
          let (strm__ : _ Stream.t) = s in
          match Stream.peek strm__ with
            Some c ->
              Stream.junk strm__;
              "ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s
          | _ ->
              let ep = Stream.count strm__ in
              err (bp, ep) "antiquotation not terminated"
        else "", get_buff (ident2 (store 0 '$') s)
  and maybe_locate bp len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '$' -> Stream.junk strm__; "ANTIQUOT", ":" ^ get_buff len
    | Some ('0'..'9' as c) ->
        Stream.junk strm__; maybe_locate bp (store len c) strm__
    | Some ':' ->
        Stream.junk strm__;
        "LOCATE", get_buff len ^ ":" ^ locate_or_antiquot_rest bp 0 strm__
    | Some '\\' ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some c ->
            Stream.junk strm__;
            "ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) strm__
        | _ -> raise (Stream.Error "")
        end
    | Some c ->
        Stream.junk strm__;
        "ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) strm__
    | _ ->
        let ep = Stream.count strm__ in
        err (bp, ep) "antiquotation not terminated"
  and antiquot bp len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '$' -> Stream.junk strm__; "ANTIQUOT", ":" ^ get_buff len
    | Some ('a'..'z' | 'A'..'Z' | '0'..'9' as c) ->
        Stream.junk strm__; antiquot bp (store len c) strm__
    | Some ':' ->
        Stream.junk strm__;
        let k = get_buff len in
        "ANTIQUOT", k ^ ":" ^ locate_or_antiquot_rest bp 0 strm__
    | Some '\\' ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some c ->
            Stream.junk strm__;
            "ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) strm__
        | _ -> raise (Stream.Error "")
        end
    | Some c ->
        Stream.junk strm__;
        "ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) strm__
    | _ ->
        let ep = Stream.count strm__ in
        err (bp, ep) "antiquotation not terminated"
  and locate_or_antiquot_rest bp len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '$' -> Stream.junk strm__; get_buff len
    | Some '\\' ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some c ->
            Stream.junk strm__;
            locate_or_antiquot_rest bp (store len c) strm__
        | _ -> raise (Stream.Error "")
        end
    | Some c ->
        Stream.junk strm__; locate_or_antiquot_rest bp (store len c) strm__
    | _ ->
        let ep = Stream.count strm__ in
        err (bp, ep) "antiquotation not terminated"
  and quotation bp len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '>' -> Stream.junk strm__; maybe_end_quotation bp len strm__
    | Some '<' ->
        Stream.junk strm__;
        quotation bp (maybe_nested_quotation bp (store len '<') strm__) strm__
    | Some '\\' ->
        Stream.junk strm__;
        let len =
          try
            match Stream.peek strm__ with
              Some ('>' | '<' | '\\' as c) -> Stream.junk strm__; store len c
            | _ -> store len '\\'
          with
            Stream.Failure -> raise (Stream.Error "")
        in
        quotation bp len strm__
    | Some c -> Stream.junk strm__; quotation bp (store len c) strm__
    | _ ->
        let ep = Stream.count strm__ in
        err (bp, ep) "quotation not terminated"
  and maybe_nested_quotation bp len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '<' ->
        Stream.junk strm__; mstore (quotation bp (store len '<') strm__) ">>"
    | Some ':' ->
        Stream.junk strm__;
        let len =
          try ident (store len ':') strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        begin try
          match Stream.peek strm__ with
            Some '<' ->
              Stream.junk strm__;
              mstore (quotation bp (store len '<') strm__) ">>"
          | _ -> len
        with
          Stream.Failure -> raise (Stream.Error "")
        end
    | _ -> len
  and maybe_end_quotation bp len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '>' -> Stream.junk strm__; len
    | _ -> quotation bp (store len '>') strm__
  and left_paren bp (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '*' ->
        Stream.junk strm__;
        let _ =
          try comment bp strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        begin try next_token strm__ with
          Stream.Failure -> raise (Stream.Error "")
        end
    | _ -> let ep = Stream.count strm__ in keyword_or_error (bp, ep) "("
  and comment bp (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '(' -> Stream.junk strm__; left_paren_in_comment bp strm__
    | Some '*' -> Stream.junk strm__; star_in_comment bp strm__
    | Some c -> Stream.junk strm__; comment bp strm__
    | _ ->
        let ep = Stream.count strm__ in err (bp, ep) "comment not terminated"
  and left_paren_in_comment bp (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '*' ->
        Stream.junk strm__; let s = strm__ in comment bp s; comment bp s
    | _ -> comment bp strm__
  and star_in_comment bp (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ')' -> Stream.junk strm__; ()
    | _ -> comment bp strm__
  and linenum bp (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('0'..'9') ->
        Stream.junk strm__;
        let _ =
          try digits strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        let _ =
          try spaces_tabs strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        begin match Stream.peek strm__ with
          Some '"' ->
            Stream.junk strm__;
            let _ =
              try any_to_nl strm__ with
                Stream.Failure -> raise (Stream.Error "")
            in
            next_token strm__
        | _ -> raise (Stream.Error "")
        end
    | _ -> keyword_or_error (bp, bp + 1) "#"
  and spaces_tabs (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some (' ' | '\t') -> Stream.junk strm__; spaces_tabs strm__
    | _ -> ()
  and digits (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('0'..'9') -> Stream.junk strm__; digits strm__
    | _ -> ()
  and any_to_nl (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('\013' | '\010') -> Stream.junk strm__; ()
    | Some _ -> Stream.junk strm__; any_to_nl strm__
    | _ -> ()
  in
  fun cstrm ->
    try next_token cstrm with
      Stream.Error str -> err (Stream.count cstrm, Stream.count cstrm + 1) str
;;

let dollar_for_antiquotation = ref true;;

let func kwd_table =
  let find = Hashtbl.find kwd_table in
  let dfa = !dollar_for_antiquotation in
  Token.lexer_func_of_parser (next_token_fun dfa find)
;;

let rec check_keyword_stream (strm__ : _ Stream.t) =
  let _ = check strm__ in
  let _ =
    try Stream.empty strm__ with
      Stream.Failure -> raise (Stream.Error "")
  in
  true
and check (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some
      ('A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
       '\248'..'\255') ->
      Stream.junk strm__; check_ident strm__
  | Some
      ('!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' | '%' |
       '.') ->
      Stream.junk strm__; check_ident2 strm__
  | Some '<' ->
      Stream.junk strm__;
      let s = strm__ in
      begin match Stream.npeek 1 s with
        [':' | '<'] -> ()
      | _ -> check_ident2 s
      end
  | Some ':' ->
      Stream.junk strm__;
      let _ =
        try
          match Stream.peek strm__ with
            Some (']' | ':' | '=' | '>') -> Stream.junk strm__; ()
          | _ -> ()
        with
          Stream.Failure -> raise (Stream.Error "")
      in
      let ep = Stream.count strm__ in ()
  | Some ('>' | '|') ->
      Stream.junk strm__;
      let _ =
        try
          match Stream.peek strm__ with
            Some (']' | '}') -> Stream.junk strm__; ()
          | _ -> check_ident2 strm__
        with
          Stream.Failure -> raise (Stream.Error "")
      in
      ()
  | Some ('[' | '{') ->
      Stream.junk strm__;
      let s = strm__ in
      begin match Stream.npeek 2 s with
        ['<'; '<' | ':'] -> ()
      | _ ->
          let (strm__ : _ Stream.t) = s in
          match Stream.peek strm__ with
            Some ('|' | '<' | ':') -> Stream.junk strm__; ()
          | _ -> ()
      end
  | Some ';' ->
      Stream.junk strm__;
      let _ =
        try
          match Stream.peek strm__ with
            Some ';' -> Stream.junk strm__; ()
          | _ -> ()
        with
          Stream.Failure -> raise (Stream.Error "")
      in
      ()
  | Some _ -> Stream.junk strm__; ()
  | _ -> raise Stream.Failure
and check_ident (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some
      ('A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
       '\248'..'\255' | '0'..'9' | '_' | '\'') ->
      Stream.junk strm__; check_ident strm__
  | _ -> ()
and check_ident2 (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some
      ('!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' | '%' |
       '.' | ':' | '<' | '>' | '|') ->
      Stream.junk strm__; check_ident2 strm__
  | _ -> ()
;;

let check_keyword s =
  try check_keyword_stream (Stream.of_string s) with
    _ -> false
;;

let error_no_respect_rules p_con p_prm =
  raise
    (Token.Error
       ("the token " ^
          (if p_con = "" then "\"" ^ p_prm ^ "\""
           else if p_prm = "" then p_con
           else p_con ^ " \"" ^ p_prm ^ "\"") ^
          " does not respect Plexer rules"))
;;

let using_token kwd_table (p_con, p_prm) =
  match p_con with
    "" ->
      begin try let _ = Hashtbl.find kwd_table p_prm in () with
        Not_found ->
          if check_keyword p_prm then Hashtbl.add kwd_table p_prm p_prm
          else error_no_respect_rules p_con p_prm
      end
  | "LIDENT" ->
      if p_prm = "" then ()
      else
        begin match p_prm.[0] with
          'A'..'Z' -> error_no_respect_rules p_con p_prm
        | _ -> ()
        end
  | "UIDENT" ->
      if p_prm = "" then ()
      else
        begin match p_prm.[0] with
          'a'..'z' -> error_no_respect_rules p_con p_prm
        | _ -> ()
        end
  | "TILDEIDENT" | "TILDEIDENTCOLON" | "QUESTIONIDENT" |
    "QUESTIONIDENTCOLON" | "INT" | "FLOAT" | "CHAR" | "STRING" | "QUOTATION" |
    "ANTIQUOT" | "LOCATE" | "EOI" ->
      ()
  | _ ->
      raise
        (Token.Error
           ("\
the constructor \"" ^ p_con ^
              "\" is not recognized by Plexer"))
;;

let removing_token kwd_table (p_con, p_prm) =
  if p_con = "" then Hashtbl.remove kwd_table p_prm
;;

let text =
  function
    "", t -> "'" ^ t ^ "'"
  | "LIDENT", "" -> "lowercase identifier"
  | "LIDENT", t -> "'" ^ t ^ "'"
  | "UIDENT", "" -> "uppercase identifier"
  | "UIDENT", t -> "'" ^ t ^ "'"
  | "INT", "" -> "integer"
  | "INT", s -> "'" ^ s ^ "'"
  | "FLOAT", "" -> "float"
  | "STRING", "" -> "string"
  | "CHAR", "" -> "char"
  | "QUOTATION", "" -> "quotation"
  | "ANTIQUOT", k -> "antiquot \"" ^ k ^ "\""
  | "LOCATE", "" -> "locate"
  | "EOI", "" -> "end of input"
  | con, "" -> con
  | con, prm -> con ^ " \"" ^ prm ^ "\""
;;

let eq_before_colon p e =
  let rec loop i =
    if i == String.length e then
      failwith "Internal error in Plexer: incorrect ANTIQUOT"
    else if i == String.length p then e.[i] == ':'
    else if p.[i] == e.[i] then loop (i + 1)
    else false
  in
  loop 0
;;

let after_colon e =
  try
    let i = String.index e ':' in
    String.sub e (i + 1) (String.length e - i - 1)
  with
    Not_found -> ""
;;

let tparse =
  function
    "ANTIQUOT", p_prm ->
      let p (strm__ : _ Stream.t) =
        match Stream.peek strm__ with
          Some ("ANTIQUOT", prm) when eq_before_colon p_prm prm ->
            Stream.junk strm__; after_colon prm
        | _ -> raise Stream.Failure
      in
      Some p
  | _ -> None
;;

let make () =
  let kwd_table = Hashtbl.create 301 in
  {func = func kwd_table; using = using_token kwd_table;
   removing = removing_token kwd_table; tparse = tparse; text = text}
;;
