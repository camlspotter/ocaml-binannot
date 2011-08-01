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

val read_magic_number : in_channel -> string
val is_magic_number : string -> bool
val read : in_channel -> t
val write_magic_number : out_channel -> unit
val write : out_channel -> t -> unit
