%{
(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Odoc_types

let identchar = 
  "[A-Z a-z_\192-\214\216-\246\248-\255'0-9]"
let blank = "[ \010\013\009\012]"

let remove_beginning_blanks s =
  Str.global_replace (Str.regexp ("^"^blank^"+")) "" s

let remove_trailing_blanks s =
  Str.global_replace (Str.regexp (blank^"+$")) "" s

let print_DEBUG s = print_string s; print_newline ()
%}

%token ERROR
%token END
%token <int * string option> Title
%token BOLD
%token EMP
%token CENTER
%token LEFT
%token RIGHT
%token ITALIC
%token LIST
%token ENUM
%token ITEM
%token LINK
%token CODE
%token END_CODE
%token CODE_PRE
%token END_CODE_PRE
%token VERB
%token END_VERB
%token LATEX
%token END_LATEX
%token ELE_REF
%token SUPERSCRIPT
%token SUBSCRIPT

%token BEGIN_SHORTCUT_LIST_ITEM
%token BEGIN_SHORTCUT_ENUM_ITEM
%token SHORTCUT_LIST_ITEM
%token SHORTCUT_ENUM_ITEM
%token END_SHORTCUT_LIST

%token BLANK_LINE

%token EOF
%token <string> Char

/* Start Symbols */
%start main 
%type <Odoc_types.text> main

%%
main:
  text EOF { $1 }
| EOF { [Raw ""] }
;

text:
  text_element_list { $1 }
;

text_element_list:
  text_element { [ $1 ] }
| text_element text_element_list { $1 :: $2 }
;

text_element:
  Title text END { let n, l_opt = $1 in Title (n, l_opt, $2) }
| BOLD text END { Bold $2 }
| ITALIC text END { Italic $2 }
| EMP text END { Emphasize $2 }
| SUPERSCRIPT text END { Superscript $2 }
| SUBSCRIPT text END { Subscript $2 }
| CENTER text END { Center $2 }
| LEFT text END { Left $2 }
| RIGHT text END { Right $2 }
| LIST list END { List $2 }
| ENUM list END { Enum $2 }
| CODE string END_CODE { Code $2 }
| CODE_PRE string END_CODE_PRE { CodePre $2 }
| ELE_REF string END { 
      let s2 = remove_beginning_blanks $2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, None) 
     }
| VERB string END_VERB { Verbatim $2 }
| LATEX string END_LATEX { Latex $2 }
| LINK string END text END { Link ($2, $4) }
| BLANK_LINE { Newline }
| BEGIN_SHORTCUT_LIST_ITEM shortcut_list END_SHORTCUT_LIST { List $2 }
| BEGIN_SHORTCUT_LIST_ITEM shortcut_list EOF { List $2 }
| BEGIN_SHORTCUT_ENUM_ITEM shortcut_enum END_SHORTCUT_LIST { Enum $2 }
| BEGIN_SHORTCUT_ENUM_ITEM shortcut_enum EOF { Enum $2 }
| string { Raw $1 }
;

list:
| string { [] (* A VOIR : un test pour voir qu'il n'y a que des blancs *) } 
| string list { $2 }
| list string  { $1 }
| item { [ $1 ] }
| item list { $1 :: $2 }

;

item:
    ITEM text END { $2 }
;

shortcut_list:
    text shortcut_list2  { $1 :: $2 }
| text { [ $1 ] }
;

shortcut_list2:
| SHORTCUT_LIST_ITEM shortcut_list { $2 }
;

shortcut_enum:
    text shortcut_enum2  { $1 :: $2 }
| text { [ $1 ] }
;

shortcut_enum2:
| SHORTCUT_ENUM_ITEM shortcut_enum { $2 }
;


string:
    Char { $1 }
| Char string { $1^$2 }
;

%% 
