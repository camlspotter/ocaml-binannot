(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

module Id = struct
  value name = "Camlp4.PreCast";
  value version = "$Id$";
end;

type camlp4_token = Sig.Camlp4Token.t ==
  [ KEYWORD       of string
  | SYMBOL        of string
  | LIDENT        of string
  | UIDENT        of string
  | ESCAPED_IDENT of string
  | INT           of int and string
  | INT32         of int32 and string
  | INT64         of int64 and string
  | NATIVEINT     of nativeint and string
  | FLOAT         of float and string
  | CHAR          of char and string
  | STRING        of string and string
  | LABEL         of string
  | OPTLABEL      of string
  | QUOTATION     of Sig.Quotation.t
  | ANTIQUOT      of string and string
  | COMMENT       of string
  | BLANKS        of string
  | NEWLINE
  | LINE_DIRECTIVE of int and option string
  | EOI ];

module Loc = Struct.Loc;
module Warning = Struct.Warning.Make Loc;
module Ast = Struct.Camlp4Ast.Make Loc;
module Token = Struct.Token.Make Loc;
module Lexer = Struct.Lexer.Make Token;
module Gram = Struct.Grammar.Static.Make Lexer;
module DynLoader = Struct.DynLoader;
module Quotation = Struct.Quotation.Make Ast;
module Syntax = OCamlInitSyntax.Make Warning Ast Gram Quotation;
module AstFilters = Struct.AstFilters.Make Ast;
module MakeGram = Struct.Grammar.Static.Make;
