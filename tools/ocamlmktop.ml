(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let _ =
  let args =
    String.concat " "
      (List.map Filename.quote (List.tl (Array.to_list Sys.argv))) in
  exit(Sys.command("ocamlc -linkall toplevellib.cma " ^ args ^ " topstart.cmo"))
