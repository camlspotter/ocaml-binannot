(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Parsing of command line arguments.

   This module provides a general mechanism for extracting options and
   arguments from the command line to the program.

   Syntax of command lines:
    A keyword is a character string starting with a [-].
    An option is a keyword alone or followed by an argument.
    The types of keywords are: [Unit], [Set], [Clear], [String],
    [Int], [Float], and [Rest].  [Unit], [Set] and [Clear] keywords take
    no argument.  [String], [Int], and [Float] keywords take the following
    word on the command line as an argument.  A [Rest] keyword takes the
    remaining of the command line as (string) arguments.
    Arguments not preceded by a keyword are called anonymous arguments.

   Examples ([cmd] is assumed to be the command name):
-   [cmd -flag           ](a unit option)
-   [cmd -int 1          ](an int option with argument [1])
-   [cmd -string foobar  ](a string option with argument ["foobar"])
-   [cmd -float 12.34    ](a float option with argument [12.34])
-   [cmd a b c           ](three anonymous arguments: ["a"], ["b"], and ["c"])
-   [cmd a b -- c d      ](two anonymous arguments and a rest option with
                           two arguments)
*)

type spec =
  | Unit of (unit -> unit)     (** Call the function with unit argument *)
  | Set of bool ref            (** Set the reference to true *)
  | Clear of bool ref          (** Set the reference to false *)
  | String of (string -> unit) (** Call the function with a string argument *)
  | Set_string of string ref   (** Set the reference to the string argument *)
  | Int of (int -> unit)       (** Call the function with an int argument *)
  | Set_int of int ref         (** Set the reference to the int argument *)
  | Float of (float -> unit)   (** Call the function with a float argument *)
  | Set_float of float ref     (** Set the reference to the float argument *)
  | Symbol of string list * (string -> unit)
                               (** Take one of the symbols as argument and
                                   call the function with the symbol. *)
  | Rest of (string -> unit)   (** Stop interpreting keywords and call the 
                                   function with each remaining argument *)

(** The concrete type describing the behavior associated
   with a keyword. *)

type key = string
type doc = string
type usage_msg = string
type annon_fun = (string -> unit)

val parse :
  (key * spec * doc) list -> annon_fun -> usage_msg -> unit
(** [Arg.parse speclist anonfun usage_msg] parses the command line.
    [speclist] is a list of triples [(key, spec, doc)].
    [key] is the option keyword, it must start with a ['-'] character.
    [spec] gives the option type and the function to call when this option
    is found on the command line.
    [doc] is a one-line description of this option.
    [anonfun] is called on anonymous arguments.
    The functions in [spec] and [anonfun] are called in the same order
    as their arguments appear on the command line.

    If an error occurs, [Arg.parse] exits the program, after printing
    an error message as follows:
-   The reason for the error: unknown option, invalid or missing argument, etc.
-   [usage_msg]
-   The list of options, each followed by the corresponding [doc] string.

    For the user to be able to specify anonymous arguments starting with a
    [-], include for example [("-", String anonfun, doc)] in [speclist].

    By default, [parse] recognizes two unit options, [-help] and [--help],
    which will display [usage_msg] and the list of options, and exit
    the program.  You can override this behaviour by specifying your
    own [-help] and [--help] options in [speclist].
*)

val parse_argv : string array ->
  (key * spec * doc) list -> annon_fun -> usage_msg -> unit
(** [Arg.parse_argv args speclist anonfun usage_msg] parses array [args] as
  if it were the command line. *)

exception Bad of string
(** Functions in [spec] or [anonfun] can raise [Arg.Bad] with an error
   message to reject invalid arguments. *)

val usage : (key * spec * doc) list -> usage_msg -> unit
(** [Arg.usage speclist usage_msg] prints an error message including
    the list of valid options.  This is the same message that
    {!Arg.parse} prints in case of error.
    [speclist] and [usage_msg] are the same as for [Arg.parse]. *)

val current : int ref
(** Position (in {!Sys.argv}) of the argument being processed.  You can
    change this value, e.g. to force {!Arg.parse} to skip some arguments.*)
