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

open Stdpp;
open Token;

value no_quotations = ref False;

(* The string buffering machinery *)

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

(* The lexer *)

value rec ident len =
  parser
  [ [: `('A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
         '\248'..'\255' | '0'..'9' | '_' | ''' as
         c)
        ;
       s :] ->
      ident (store len c) s
  | [: :] -> len ]
and ident2 len =
  parser
  [ [: `('!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' |
         '%' | '.' | ':' | '<' | '>' | '|' | '$' as
         c)
        ;
       s :] ->
      ident2 (store len c) s
  | [: :] -> len ]
and ident3 len =
  parser
  [ [: `('0'..'9' | 'A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
         '\248'..'\255' | '_' | '!' | '%' | '&' | '*' | '+' | '-' | '.' |
         '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' | ''' |
         '$' as
         c)
        ;
       s :] ->
      ident3 (store len c) s
  | [: :] -> len ]
and base_number len =
  parser
  [ [: `'o' | 'O'; s :] -> octal_digits (store len 'o') s
  | [: `'x' | 'X'; s :] -> hexa_digits (store len 'x') s
  | [: `'b' | 'B'; s :] -> binary_digits (store len 'b') s
  | [: a = number len :] -> a ]
and octal_digits len =
  parser
  [ [: `('0'..'7' as d); s :] -> octal_digits (store len d) s
  | [: :] -> ("INT", get_buff len) ]
and hexa_digits len =
  parser
  [ [: `('0'..'9' | 'a'..'f' | 'A'..'F' as d); s :] ->
      hexa_digits (store len d) s
  | [: :] -> ("INT", get_buff len) ]
and binary_digits len =
  parser
  [ [: `('0'..'1' as d); s :] -> binary_digits (store len d) s
  | [: :] -> ("INT", get_buff len) ]
and number len =
  parser
  [ [: `('0'..'9' as c); s :] -> number (store len c) s
  | [: `'.'; s :] -> decimal_part (store len '.') s
  | [: `'e' | 'E'; s :] -> exponent_part (store len 'E') s
  | [: :] -> ("INT", get_buff len) ]
and decimal_part len =
  parser
  [ [: `('0'..'9' as c); s :] -> decimal_part (store len c) s
  | [: `'e' | 'E'; s :] -> exponent_part (store len 'E') s
  | [: :] -> ("FLOAT", get_buff len) ]
and exponent_part len =
  parser
  [ [: `('+' | '-' as c); s :] -> end_exponent_part (store len c) s
  | [: a = end_exponent_part len :] -> a ]
and end_exponent_part len =
  parser
  [ [: `('0'..'9' as c); s :] -> end_exponent_part (store len c) s
  | [: :] -> ("FLOAT", get_buff len) ]
;

value rec skip_spaces =
  parser
  [ [: `' ' | '\010' | '\013' | '\t' | '\026' | '\012'; s :] -> skip_spaces s
  | [: :] -> () ]
;

value error_on_unknown_keywords = ref False;
value err loc msg = raise_with_loc loc (Token.Error msg);

value next_token_fun dfa find_kwd =
  let keyword_or_error loc s =
    try (("", find_kwd s), loc) with
    [ Not_found ->
        if error_on_unknown_keywords.val then err loc ("illegal token: " ^ s)
        else (("", s), loc) ]
  in
  let rec next_token =
    parser bp
    [ [: `' ' | '\010' | '\013' | '\t' | '\026' | '\012'; s :] ->
        next_token s
    | [: `'('; s :] -> left_paren bp s
    | [: `'#'; s :] -> do { spaces_tabs s; linenum bp s }
    | [: `('A'..'Z' | '\192'..'\214' | '\216'..'\222' as c); s :] ->
        let id = get_buff (ident (store 0 c) s) in
        let loc = (bp, Stream.count s) in
        (try ("", find_kwd id) with [ Not_found -> ("UIDENT", id) ], loc)
    | [: `('a'..'z' | '\223'..'\246' | '\248'..'\255' | '_' as c); s :] ->
        let id = get_buff (ident (store 0 c) s) in
        let loc = (bp, Stream.count s) in
        (try ("", find_kwd id) with [ Not_found -> ("LIDENT", id) ], loc)
    | [: `('1'..'9' as c); s :] ->
        let tok = number (store 0 c) s in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `'0'; s :] ->
        let tok = base_number (store 0 '0') s in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `'''; s :] ->
        match Stream.npeek 2 s with
        [ [_; '''] | ['\\'; _] ->
            let tok = ("CHAR", char bp 0 s) in
            let loc = (bp, Stream.count s) in
            (tok, loc)
        | _ -> keyword_or_error (bp, Stream.count s) "'" ]
    | [: `'"'; s :] ->
        let tok = ("STRING", string bp 0 s) in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `'$'; s :] ->
        let tok = dollar bp 0 s in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `('!' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' | '%' as c);
         s :] ->
        let id = get_buff (ident2 (store 0 c) s) in
        keyword_or_error (bp, Stream.count s) id
    | [: `('~' as c);
         a =
           parser
           [ [: `('a'..'z' as c); len = ident (store 0 c) :] ep ->
               (("TILDEIDENT", get_buff len), (bp, ep))
           | [: s :] ->
               let id = get_buff (ident2 (store 0 c) s) in
               keyword_or_error (bp, Stream.count s) id ] :] ->
        a
    | [: `('?' as c);
         a =
           parser
           [ [: `('a'..'z' as c); len = ident (store 0 c) :] ep ->
               (("QUESTIONIDENT", get_buff len), (bp, ep))
           | [: s :] ->
               let id = get_buff (ident2 (store 0 c) s) in
               keyword_or_error (bp, Stream.count s) id ] :] ->
        a
    | [: `'<'; s :] -> less bp s
    | [: `(':' as c1);
         len =
           parser
           [ [: `(']' | ':' | '=' | '>' as c2) :] -> store (store 0 c1) c2
           | [: :] -> store 0 c1 ] :] ep ->
        let id = get_buff len in
        keyword_or_error (bp, ep) id
    | [: `('>' | '|' as c1);
         len =
           parser
           [ [: `(']' | '}' as c2) :] -> store (store 0 c1) c2
           | [: a = ident2 (store 0 c1) :] -> a ] :] ep ->
        let id = get_buff len in
        keyword_or_error (bp, ep) id
    | [: `('[' | '{' as c1); s :] ->
        let len =
          match Stream.npeek 2 s with
          [ ['<'; '<' | ':'] -> store 0 c1
          | _ ->
              match s with parser
              [ [: `('|' | '<' | ':' as c2) :] -> store (store 0 c1) c2
              | [: :] -> store 0 c1 ] ]
        in
        let ep = Stream.count s in
        let id = get_buff len in
        keyword_or_error (bp, ep) id
    | [: `'.';
         id =
           parser
           [ [: `'.' :] -> ".."
           | [: :] -> "." ] :] ep ->
        keyword_or_error (bp, ep) id
    | [: `';';
         id =
           parser
           [ [: `';' :] -> ";;"
           | [: :] -> ";" ] :] ep ->
        keyword_or_error (bp, ep) id
    | [: `'\\'; s :] ep -> (("LIDENT", get_buff (ident3 0 s)), (bp, ep))
    | [: `c :] ep -> keyword_or_error (bp, ep) (String.make 1 c)
    | [: _ = Stream.empty :] -> (("EOI", ""), (bp, succ bp)) ]
  and less bp strm =
    if no_quotations.val then
      match strm with parser
      [ [: len = ident2 (store 0 '<') :] ep ->
           let id = get_buff len in
           keyword_or_error (bp, ep) id ]
    else
      match strm with parser
      [ [: `'<'; len = quotation bp 0 :] ep ->
          (("QUOTATION", ":" ^ get_buff len), (bp, ep))
      | [: `':'; i = parser [: len = ident 0 :] -> get_buff len;
           `'<' ? "character '<' expected"; len = quotation bp 0 :] ep ->
          (("QUOTATION", i ^ ":" ^ get_buff len), (bp, ep))
      | [: len = ident2 (store 0 '<') :] ep ->
          let id = get_buff len in
          keyword_or_error (bp, ep) id ]
  and string bp len =
    parser
    [ [: `'"' :] -> get_buff len
    | [: `'\\'; `c; s :] -> string bp (store (store len '\\') c) s
    | [: `c; s :] -> string bp (store len c) s
    | [: :] ep -> err (bp, ep) "string not terminated" ]
  and char bp len =
    parser
    [ [: `'''; s :] ->
        if len = 0 then char bp (store len ''') s else get_buff len
    | [: `'\\'; `c; s :] -> char bp (store (store len '\\') c) s
    | [: `c; s :] -> char bp (store len c) s
    | [: :] ep -> err (bp, ep) "char not terminated" ]
  and dollar bp len =
    parser
    [ [: `'$' :] -> ("ANTIQUOT", ":" ^ get_buff len)
    | [: `('a'..'z' | 'A'..'Z' as c); s :] -> antiquot bp (store len c) s
    | [: `('0'..'9' as c); s :] -> maybe_locate bp (store len c) s
    | [: `':'; s :] ->
        let k = get_buff len in
        ("ANTIQUOT", k ^ ":" ^ locate_or_antiquot_rest bp 0 s)
    | [: `'\\'; `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
    | [: s :] ->
        if dfa then
          match s with parser
          [ [: `c :] ->
              ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
          | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
        else
          ("", get_buff (ident2 (store 0 '$') s)) ]
  and maybe_locate bp len =
    parser
    [ [: `'$' :] -> ("ANTIQUOT", ":" ^ get_buff len)
    | [: `('0'..'9' as c); s :] -> maybe_locate bp (store len c) s
    | [: `':'; s :] ->
        ("LOCATE", get_buff len ^ ":" ^ locate_or_antiquot_rest bp 0 s)
    | [: `'\\'; `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
    | [: `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
    | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
  and antiquot bp len =
    parser
    [ [: `'$' :] -> ("ANTIQUOT", ":" ^ get_buff len)
    | [: `('a'..'z' | 'A'..'Z' | '0'..'9' as c); s :] ->
        antiquot bp (store len c) s
    | [: `':'; s :] ->
        let k = get_buff len in
        ("ANTIQUOT", k ^ ":" ^ locate_or_antiquot_rest bp 0 s)
    | [: `'\\'; `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
    | [: `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
    | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
  and locate_or_antiquot_rest bp len =
    parser
    [ [: `'$' :] -> get_buff len
    | [: `'\\'; `c; s :] -> locate_or_antiquot_rest bp (store len c) s
    | [: `c; s :] -> locate_or_antiquot_rest bp (store len c) s
    | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
  and quotation bp len =
    parser
    [ [: `'>'; s :] -> maybe_end_quotation bp len s
    | [: `'<'; s :] ->
        quotation bp (maybe_nested_quotation bp (store len '<') strm__) s
    | [: `'\\';
         len =
           parser
           [ [: `('>' | '<' | '\\' as c) :] -> store len c
           | [: :] -> store len '\\' ];
         s :] ->
        quotation bp len s
    | [: `c; s :] -> quotation bp (store len c) s
    | [: :] ep -> err (bp, ep) "quotation not terminated" ]
  and maybe_nested_quotation bp len =
    parser
    [ [: `'<'; s :] -> mstore (quotation bp (store len '<') s) ">>"
    | [: `':'; len = ident (store len ':');
         a =
           parser
           [ [: `'<'; s :] -> mstore (quotation bp (store len '<') s) ">>"
           | [: :] -> len ] :] ->
        a
    | [: :] -> len ]
  and maybe_end_quotation bp len =
    parser
    [ [: `'>' :] -> len
    | [: a = quotation bp (store len '>') :] -> a ]
  and left_paren bp =
    parser
    [ [: `'*'; _ = comment bp; a = next_token :] -> a
    | [: :] ep -> keyword_or_error (bp, ep) "(" ]
  and comment bp =
    parser
    [ [: `'('; s :] -> left_paren_in_comment bp s
    | [: `'*'; s :] -> star_in_comment bp s
    | [: `c; s :] -> comment bp s
    | [: :] ep -> err (bp, ep) "comment not terminated" ]
  and left_paren_in_comment bp =
    parser
    [ [: `'*'; s :] -> do { comment bp s; comment bp s }
    | [: a = comment bp :] -> a ]
  and star_in_comment bp =
    parser
    [ [: `')' :] -> ()
    | [: a = comment bp :] -> a ]
  and linenum bp =
    parser
    [ [: `'0'..'9'; _ = digits; _ = spaces_tabs; `'"'; _ = any_to_nl; s :] ->
        next_token s
    | [: :] -> keyword_or_error (bp, bp + 1) "#" ]
  and spaces_tabs =
    parser
    [ [: `' ' | '\t'; s :] -> spaces_tabs s
    | [: :] -> () ]
  and digits =
    parser
    [ [: `'0'..'9'; s :] -> digits s
    | [: :] -> () ]
  and any_to_nl =
    parser
    [ [: `'\013' | '\010' :] -> ()
    | [: `_; s :] -> any_to_nl s
    | [: :] -> () ]
  in
  fun cstrm ->
    try next_token cstrm with
    [ Stream.Error str ->
        err (Stream.count cstrm, Stream.count cstrm + 1) str ]
;

value dollar_for_antiquotation = ref True;

value func kwd_table =
  let find = Hashtbl.find kwd_table in
  let dfa = dollar_for_antiquotation.val in
  Token.lexer_func_of_parser (next_token_fun dfa find)
;

value rec check_keyword_stream =
  parser [: _ = check; _ = Stream.empty :] -> True
and check =
  parser
  [ [: `'A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' | '\248'..'\255'
        ;
       s :] ->
      check_ident s
  | [: `'!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' |
        '%' | '.'
        ;
       s :] ->
      check_ident2 s
  | [: `'<'; s :] ->
      match Stream.npeek 1 s with
      [ [':' | '<'] -> ()
      | _ -> check_ident2 s ]
  | [: `':';
       _ =
         parser
         [ [: `']' | ':' | '=' | '>' :] -> ()
         | [: :] -> () ] :] ep ->
      ()
  | [: `'>' | '|';
       _ =
         parser
         [ [: `']' | '}' :] -> ()
         | [: a = check_ident2 :] -> a ] :] ->
      ()
  | [: `'[' | '{'; s :] ->
      match Stream.npeek 2 s with
      [ ['<'; '<' | ':'] -> ()
      | _ ->
          match s with parser
          [ [: `'|' | '<' | ':' :] -> ()
          | [: :] -> () ] ]
  | [: `';';
       _ =
         parser
         [ [: `';' :] -> ()
         | [: :] -> () ] :] ->
      ()
  | [: `_ :] -> () ]
and check_ident =
  parser
  [ [: `'A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
        '\248'..'\255' | '0'..'9' | '_' | '''
        ;
       s :] ->
      check_ident s
  | [: :] -> () ]
and check_ident2 =
  parser
  [ [: `'!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' |
        '%' | '.' | ':' | '<' | '>' | '|'
        ;
       s :] ->
      check_ident2 s
  | [: :] -> () ]
;

value check_keyword s =
  try check_keyword_stream (Stream.of_string s) with _ -> False
;

value error_no_respect_rules p_con p_prm =
  raise
     (Token.Error
         ("the token " ^
          (if p_con = "" then "\"" ^ p_prm ^ "\""
           else if p_prm = "" then p_con
           else p_con ^ " \"" ^ p_prm ^ "\"") ^
          " does not respect Plexer rules"))
;

value error_ident_and_keyword p_con p_prm =
  raise
    (Token.Error
        ("the token \"" ^ p_prm ^ "\" is used as " ^ p_con ^
         " and as keyword"))
;

value using_token kwd_table ident_table (p_con, p_prm) =
  match p_con with
  [ "" ->
      if not (Hashtbl.mem kwd_table p_prm) then
        if check_keyword p_prm then
          if Hashtbl.mem ident_table p_prm then
            error_ident_and_keyword (Hashtbl.find ident_table p_prm) p_prm
          else Hashtbl.add kwd_table p_prm p_prm
        else error_no_respect_rules p_con p_prm
      else ()
  | "LIDENT" ->
      if p_prm = "" then ()
      else
        match p_prm.[0] with
        [ 'A'..'Z' -> error_no_respect_rules p_con p_prm
        | _ ->
            if Hashtbl.mem kwd_table p_prm then
              error_ident_and_keyword p_con p_prm
            else Hashtbl.add ident_table p_prm p_con ]
  | "UIDENT" ->
      if p_prm = "" then ()
      else
        match p_prm.[0] with
        [ 'a'..'z' -> error_no_respect_rules p_con p_prm
        | _ ->
            if Hashtbl.mem kwd_table p_prm then
              error_ident_and_keyword p_con p_prm
            else Hashtbl.add ident_table p_prm p_con ]
  | "TILDEIDENT" | "QUESTIONIDENT" |
    "INT" | "FLOAT" | "CHAR" | "STRING" | "QUOTATION" |
    "ANTIQUOT" | "LOCATE" | "EOI" ->
      ()
  | _ ->
      raise
        (Token.Error
           ("the constructor \"" ^ p_con ^
              "\" is not recognized by Plexer")) ]
;

value removing_token kwd_table ident_table (p_con, p_prm) =
  match p_con with
  [ "" -> Hashtbl.remove kwd_table p_prm
  | "LIDENT" | "UIDENT" ->
      if p_prm <> "" then Hashtbl.remove ident_table p_prm
      else ()
  | _ -> () ]
;

value text =
  fun
  [ ("", t) -> "'" ^ t ^ "'"
  | ("LIDENT", "") -> "lowercase identifier"
  | ("LIDENT", t) -> "'" ^ t ^ "'"
  | ("UIDENT", "") -> "uppercase identifier"
  | ("UIDENT", t) -> "'" ^ t ^ "'"
  | ("INT", "") -> "integer"
  | ("INT", s) -> "'" ^ s ^ "'"
  | ("FLOAT", "") -> "float"
  | ("STRING", "") -> "string"
  | ("CHAR", "") -> "char"
  | ("QUOTATION", "") -> "quotation"
  | ("ANTIQUOT", k) -> "antiquot \"" ^ k ^ "\""
  | ("LOCATE", "") -> "locate"
  | ("EOI", "") -> "end of input"
  | (con, "") -> con
  | (con, prm) -> con ^ " \"" ^ prm ^ "\"" ]
;

value eq_before_colon p e =
  loop 0 where rec loop i =
    if i == String.length e then
      failwith "Internal error in Plexer: incorrect ANTIQUOT"
    else if i == String.length p then e.[i] == ':'
    else if p.[i] == e.[i] then loop (i + 1)
    else False
;

value after_colon e =
  try
    let i = String.index e ':' in
    String.sub e (i + 1) (String.length e - i - 1)
  with
  [ Not_found -> "" ]
;

value tok_match =
  fun
  [ ("ANTIQUOT", p_prm) ->
      fun
      [ ("ANTIQUOT", prm) when eq_before_colon p_prm prm -> after_colon prm
      | _ -> raise Stream.Failure ]
  | tok ->
      Token.default_match tok ]
;

value gmake () =
  let kwd_table = Hashtbl.create 301 in
  let id_table = Hashtbl.create 301 in
  {tok_func = func kwd_table; tok_using = using_token kwd_table id_table;
   tok_removing = removing_token kwd_table id_table;
   tok_match = tok_match; tok_text = text}
;

value tparse =
  fun
  [ ("ANTIQUOT", p_prm) ->
      let p =
        parser
          [: `("ANTIQUOT", prm) when eq_before_colon p_prm prm :] ->
            after_colon prm
      in
      Some p
  | _ -> None ]
;

value make () =
  let kwd_table = Hashtbl.create 301 in
  let id_table = Hashtbl.create 301 in
  {func = func kwd_table; using = using_token kwd_table id_table;
   removing = removing_token kwd_table id_table; tparse = tparse;
   text = text}
;
