(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2011 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* escape metacharacters for /bin/sh 

   Backslash quoting the following characters

   According to the manual of /bin/sh:

   metacharacters: |  & ; ( ) < > space tab
   history expansion character: !
   quote characters: \\ \' "" 
*)

(* general escape *)
let escaped pred s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    if pred c then Buffer.add_char b '\\';
    Buffer.add_char b c) s;
  Buffer.contents b
;;

let escaped_for_shell =
  escaped (function
    | '|' | '&' | ';' | '(' | ')' | '<' | '>' | ' ' | '\t'
    | '!'
    | '\\' | '\'' | '"' -> true
    | _ -> false)
;;

let shell args = 
  Sys.command (String.concat " " (List.map escaped_for_shell args))
;;

   
   
   
   
