(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis && Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Please keep them in alphabetical order *)

type t =                             (* A is all *)
  | Comment of string                (* C *)
  | Partial_application              (* F *)
  | Method_override of string list   (* M *)
  | Partial_match of string          (* P *)
  | Statement_type                   (* S *)
  | Unused_match                     (* U *)
  | Hide_instance_variable of string (* V *)
  | Other of string                  (* X *)
;;

let letter = function        (* 'a' is all *)
  | Comment _ ->                'c'
  | Partial_application ->      'f'
  | Method_override _ ->        'm'
  | Partial_match _ ->          'p'
  | Statement_type ->           's'
  | Unused_match ->             'u'
  | Hide_instance_variable _ -> 'v'
  | Other _ ->                  'x'
;;

let check c =
  try ignore (String.index "acfmpsuvxACFMPSUVX" c)
  with _ -> raise (Arg.Bad (Printf.sprintf "unknown warning option %c" c))
;;    

let active = Array.create 26 true;;
let error = Array.create 26 false;;

let translate c =
  check c;
  if c >= 'A' && c <= 'Z' then
    (Char.code c - Char.code 'A', true)
  else
    (Char.code c - Char.code 'a', false)
;;

let is_active x =
  let (n, _) = translate (letter x) in
  active.(n)
;;

let is_error x =
  let (n, _) = translate (letter x) in
  error.(n)
;;

let parse_options iserr s =
  let flags = if iserr then error else active in
  for i = 0 to String.length s - 1 do
    if s.[i] = 'A' then Array.fill flags 0 (Array.length flags) true
    else if s.[i] = 'a' then Array.fill flags 0 (Array.length flags) false
    else begin
      let (n, fl) = translate s.[i] in
      flags.(n) <- fl;
    end;
  done
;;

let message = function
  | Partial_match "" -> "this pattern-matching is not exhaustive."
  | Partial_match s ->
      "this pattern-matching is not exhaustive.\n\
       Here is an example of a value that is not matched:\n" ^ s
  | Unused_match -> "this match case is unused."
  | Method_override slist ->
      String.concat " "
        ("the following methods are overriden \
          by the inherited class:\n " :: slist)
  | Hide_instance_variable lab ->
      "this definition of an instance variable " ^ lab ^
      " hides a previously\ndefined instance variable of the same name."
  | Partial_application ->
      "this function application is partial,\n\
       maybe some arguments are missing."
  | Statement_type ->
      "this expression should have type unit."
  | Comment s -> "this is " ^ s ^ "."
  | Other s -> s
;;

let nerrors = ref 0;;

let print ppf w =
  Format.fprintf ppf "%s" (message w);
  let (n, _) = translate (letter w) in
  if error.(n) then incr nerrors;
;;

exception Errors of int;;

let check_fatal () =
  if !nerrors > 0 then begin
    let e = Errors !nerrors in
    nerrors := 0;
    raise e;
  end;
;;
