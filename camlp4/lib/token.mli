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

(* Module [Token]: lexers for Camlp4 grammars *)

(* This module defines the Camlp4 lexer type to be used in extensible
   grammars (see module [Grammar]). It also provides some useful functions
   to create lexers (this module should be renamed [Glexer] one day). *)

(*** Token type *)

type t = (string * string);
type pattern = (string * string);
     (* Token and token patterns. Token are build by the lexer. Token
        patterns come from the EXTEND statement.
-       The first string is the constructor name (must start with
        an uppercase character). When it is empty, the second string
        is supposed to be a keyword.
-       The second string is the constructor parameter. Empty if it
        has no parameter.
-       The way tokens pattern are interpreted to parse tokens is
        done by the lexer, function [tparse] below. *)

exception Error of string;
     (* An lexing error exception to be used by lexers. *)

(*** Lexer type *)

type location = (int * int);
type location_function = int -> location;
   (* The type for a function associating a number of a token in a stream
      (starting from 0) to its source location. *)
type lexer_func 'te = Stream.t char -> (Stream.t 'te * location_function);
   (* The type for a lexer function. The character stream is the input
      stream to be lexed. The result is a pair of a token stream and
      a location function for this tokens stream. *)

type glexer 'te =
  { tok_func : lexer_func 'te;
    tok_using : pattern -> unit;
    tok_removing : pattern -> unit;
    tok_match : pattern -> 'te -> string;
    tok_text : pattern -> string }
;
    (* The type for a lexer used by Camlp4 grammars.
-      The field [tok_func] is the main lexer function. See [lexer_func]
       type above. This function may be created from a [char stream parser]
       or for an [ocamllex] function using the functions below.
-      The field [tok_using] is a function telling the lexer that the grammar
       uses this token (pattern). The lexer can check that its constructor
       is correct, and interpret some kind of tokens as keywords (to record
       them in its tables). Called by [EXTEND] statements.
-      The field [tok_removing] is a function telling the lexer that the
       grammar does not uses the given token (pattern) any more. If the
       lexer has a notion of "keywords", it can release it from its tables.
       Called by [DELETE_RULE] statements.
-      The field [tok_match] is a function taking a pattern and returning
       a function matching a token against the pattern. Warning: for
       efficency, write it as a function returning functions according
       to the values of the pattern, not a function with two parameters.
-      The field [tok_text] returns the name of some token pattern,
       used in error messages. *)

value lexer_text : pattern -> string;
    (* A simple [tok_text] function for lexers *)

value default_match : pattern -> t -> string;
    (* A simple [tok_match] function for lexers, appling to type [t] *)

(*** Lexer from char stream parsers or ocamllex function *)

(* The functions below create lexer functions either from a [char stream]
   parser or for an [ocamllex] function. With the returned function [f],
   the simplest [Token.lexer] can be written:
-  [       { Token.tok_func = f;                       ]
-  [         Token.tok_using = (fun _ -> ());          ]
-  [         Token.tok_removing = (fun _ -> ());       ]
-  [         Token.tok_match = Token.default_match;    ]
-  [         Token.tok_text = Token.lexer_text }       ]
   Note that a better [tok_using] function should check the used tokens
   and raise [Token.Error] for incorrect ones. The other functions
   [tok_removing], [tok_match] and [tok_text] may have other implementations
   as well. *)

value lexer_func_of_parser :
  (Stream.t char -> ('te * location)) -> lexer_func 'te;
    (* A lexer function from a lexer written as a char stream parser
       returning the next token and its location. *)
value lexer_func_of_ocamllex : (Lexing.lexbuf -> 'te) -> lexer_func 'te;
    (* A lexer function from a lexer created by [ocamllex] *)

value make_stream_and_location :
  (unit -> ('te * location)) -> (Stream.t 'te * location_function)
;

(*** Useful functions *)

value eval_char : string -> char;
value eval_string : string -> string;
    (* Convert a char or a string token, where the backslashes have not
       been interpreted into a real char or string; raise [Failure] if
       bad backslash sequence found; [Token.eval_char (Char.escaped c)]
       returns [c] and [Token.eval_string (String.escaped s)] returns [s] *)

(*--*)

(* deprecated since version 3.04+6; use rather type glexer *)
type lexer =
  { func : lexer_func t;
    using : pattern -> unit;
    removing : pattern -> unit;
    tparse : pattern -> option (Stream.t t -> string);
    text : pattern -> string }
;
