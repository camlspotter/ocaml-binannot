(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Formatted input functions. *)

(** Scanning buffers. *)
module Scanning : sig

type scanbuf;;
(** The type of scanning buffers. A scanning buffer is the argument passed
   to the scanning functions used by the [scanf] family of functions.
   The scanning buffer holds the current state of the scan, plus
   a function to get the next char from the input, and a token buffer
   to store the string matched so far. *)

val stdib : scanbuf;;
(** The scanning buffer reading from [stdin].
    [stdib] is equivalent to [Scanning.from_channel stdin]. *)

val from_string : string -> scanbuf;;
(** [Scanning.from_string s] returns a scanning buffer which reads
    from the given string.
    Reading starts from the first character in the string.
    The end-of-input condition is set when the end of the string is reached. *)

val from_channel : in_channel -> scanbuf;;
(** [Scanning.from_channel inchan] returns a scanning buffer which reads
    from the input channel [inchan], at the current reading position. *)

val from_function : (unit -> char) -> scanbuf;;
(** [Scanning.from_function f] returns a scanning buffer with
    the given function as its reading method.
    When scanning needs one more character, the given function is called.
    When the function has no more character to provide, it must set
    an end-of-input condition by raising the exception [End_of_file]. *)

val end_of_input : scanbuf -> bool;;
(** [Scanning.end_of_input scanbuf] tests the end of input condition
    of the given buffer. *)
val begin_of_input : scanbuf -> bool;;
(** [Scanning.begin_of_input scanbuf] tests the begin of input condition
    of the given buffer. *)

end;;

exception Scan_failure of string;;
(** The exception that formatted input functions raise when the input
   cannot be read according to the given format. *)

val bscanf :
  Scanning.scanbuf -> ('a, Scanning.scanbuf, 'b, 'b) format -> 'a -> 'b;;
(** [bscanf ib format f] reads tokens from the scanning buffer [ib] according
   to the format string [format], converts these tokens to values, and
   applies the function [f] to these values.
   The result of this application of [f] is the result of the whole construct.

   Raise [Scanf.Scan_failure] if the given input does not match the format.

   Raise [Failure] if a conversion to a number is not possible.

   Raise [End_of_file] if the end of input is encountered while scanning
   and the input matches the given format so far.

   The format is a character string which contains three types of
   objects:
   - plain characters, which are simply matched with the
   characters of the input,
   - conversion specifications, each of which causes reading and
   conversion of one argument for [f],
   - scanning indications to specify boundaries of tokens.

   Among plain characters the space character (ASCII code 32) has a
   special meaning: it matches ``whitespace'', that is any number of tab,
   space, newline and carriage return characters. Hence, a space in the format
   matches any amount of whitespace in the input.

   Conversion specifications consist in the [%] character, followed
   by an optional field width, followed by one or two conversion
   characters. The conversion characters and their meanings are:
   - [d]: reads an optionally signed decimal integer.
   - [i]: reads an optionally signed integer
     (usual input formats for hexadecimal ([0x[d]+] and [0X[d]+]),
      octal ([0o[d]+]), and binary [0b[d]+] notations are understood).
   - [u]: reads an unsigned decimal integer.
   - [x] or [X]: reads an unsigned hexadecimal integer.
   - [o]: reads an unsigned octal integer.
   - [b]: reads an unsigned binary integer.
   - [s]: reads a string argument (by default strings end with a space).
   - [S]: reads a delimited string argument (delimiters and special
     escaped characters follow the lexical conventions of Caml).
   - [c]: reads a single character.
   - [C]: reads a single delimited character (delimiters and special
     escaped characters follow the lexical conventions of Caml).
   - [f], [e], [E], [g], [G], [F]: reads an optionally signed
     floating-point number in decimal notation, in the style [dddd.ddd
     e/E+-dd].
   - [B]: reads a boolean argument ([true] or [false]).
   - [ld], [li], [lu], [lx], [lX], [lo], [lb]: reads an [int32] argument to
     the format specified by the second letter (decimal, hexadecimal, etc).
   - [nd], [ni], [nu], [nx], [nX], [no], [nb]: reads a [nativeint] argument to
     the format specified by the second letter.
   - [Ld], [Li], [Lu], [Lx], [LX], [Lo], [Lb]: reads an [int64] argument to
     the format specified by the second letter.
   - [\[ range \]]: reads characters that matches one of the characters
     mentioned in the range of characters [range] (or not mentioned in
     it, if the range starts with [^]). Returns a [string] that can be
     empty, if no character in the input matches the range.
     If a closing bracket appears in a range, it must occur as the
     first character of the range (or just after the [^] in case of
     range negation); hence [\[\]\]] matches a [\]] character and
     [\[^\]\]] matches any character that is not [\]].
   - [n]: applies [f] to the number of characters read so far.
   - [N]: applies [f] to the number of tokens read so far.
   - [%]: matches one [%] character in the input.

   The field widths are composed of an optional integer literal
   indicating the maximal width of the token to read.
   For instance, [%6d] reads an integer, having at most 6 decimal digits;
   and [%4f] reads a float with at most 4 characters.

   Scanning indications appear just after string conversions [s] and
   [\[ range \]] to delimit the end of the token. A scanning
   indication is introduced by a [@] character, followed by some
   constant character [c]. It means that the string token should end
   just before the next matching [c] (which is skipped). If no [c]
   character is encountered, the string token spreads as much as
   possible.  For instance, ["%s@\t"] reads a string up to the next
   tabulation character. If a scanning indication [\@c] does not
   follow a string conversion, it is ignored and treated as a plain
   [c] character.

   Note: the [scanf] facility is not intended for heavy duty
   lexical analysis and parsing. If it appears not expressive
   enough for your needs, several alternative exists: regular expressions
   (module [Str]), stream parsers, [ocamllex]-generated lexers,
   [ocamlyacc]-generated parsers. *)

val fscanf : in_channel -> ('a, Scanning.scanbuf, 'b, 'b) format -> 'a -> 'b;;
(** Same as {!Scanf.bscanf}, but inputs from the given channel. *)

val sscanf : string -> ('a, Scanning.scanbuf, 'b, 'b) format -> 'a -> 'b;;
(** Same as {!Scanf.bscanf}, but inputs from the given string. *)

val scanf : ('a, Scanning.scanbuf, 'b, 'b) format -> 'a -> 'b;;
(** Same as {!Scanf.bscanf}, but inputs from [stdin]
    (the standard input channel). *)

val kscanf :
  Scanning.scanbuf -> (Scanning.scanbuf -> exn -> 'a) ->
  ('b, Scanning.scanbuf, 'a, 'a) format -> 'b -> 'a;;
(** Same as {!Scanf.bscanf}, but takes an additional function argument
  [ef] that is called in case of error: if the scanning process or
  some conversion fails, the scanning function aborts and applies the
  error handling function [ef] to the scanning buffer and the
  exception that aborted evaluation. *)
