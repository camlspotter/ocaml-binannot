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

(* Module [Plexer]: a lexical analyzer *)

val make : unit -> Token.lexer;;
    (* Some lexer provided. See the module [Token]. The tokens returned
       follow the Objective Caml and the Revised syntax lexing rules.

       The meaning of the tokens are:
-      * [("", s)] is the keyword [s].
-      * [("LIDENT", s)] is the ident [s] starting with a lowercase letter.
-      * [("UIDENT", s)] is the ident [s] starting with an uppercase letter.
-      * [("INT", s)] is an integer constant whose string source is [s].
-      * [("FLOAT", s)] is a float constant whose string source is [s].
-      * [("STRING", s)] is the string constant [s].
-      * [("CHAR", s)] is the character constant [s].
-      * [("QUOTATION", "t:s")] is a quotation [t] holding the string [s].
-      * [("ANTIQUOT", "t:s")] is an antiquotation [t] holding the string [s].
-      * [("LOCATE", "i:s")] is a location directive at pos [i] holding [s].
-      * [("EOI", "")] is the end of input.

       The associated token patterns in the EXTEND statement hold the
       same names than the first string (constructor name) of the tokens
       expressions above.

       The lexer do not use global (mutable) variables: instantiations
       of [Plexer.make ()] do not perturb each other.  *)

val dollar_for_antiquotation : bool ref;;
    (* When True (default), the next call to [Plexer.make ()] returns a
       lexer where the dollar sign is used for antiquotations. If False,
       the dollar sign can be used as token. *)
