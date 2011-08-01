(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(***********************************************************************)
(*                                                                     *)
(*                    Contributed by OCamlPro                          *)
(*                                                                     *)
(***********************************************************************)

type t = {
  cmt_trees : Typedtree.saved_type array;
}

let read_magic_number ic =
  let len_magic_number = String.length Config.cmo_magic_number in
  let magic_number = String.create len_magic_number in
  really_input ic magic_number 0 len_magic_number;
  magic_number

let is_magic_number magic =
  (magic = Config.cmt_magic_number)

let read ic =
  let cmt = (input_value ic : t) in
  cmt

let write_magic_number oc =
  output_string oc Config.cmt_magic_number

let write oc t =
  output_value oc t
