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

(* Specific operations for the HP PA-RISC processor *)

open Formatmsg

type specific_operation =
    Ishift1add
  | Ishift2add
  | Ishift3add

(* Addressing modes *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)

(* Sizes, endianness *)

let big_endian = true

let size_addr = 4
let size_int = 4
let size_float = 8

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1

(* Printing operations and addressing modes *)

let print_addressing printreg addr arg =
  match addr with
    Ibased(s, n) ->
      printf "\"%s\"" s;
      if n <> 0 then printf " + %i" n
  | Iindexed n ->
      printreg arg.(0);
      if n <> 0 then printf " + %i" n

let print_specific_operation printreg op arg =
  match op with
    Ishift1add -> printreg arg.(0); print_string " << 1 + "; printreg arg.(1)
  | Ishift2add -> printreg arg.(0); print_string " << 2 + "; printreg arg.(1)
  | Ishift3add -> printreg arg.(0); print_string " << 3 + "; printreg arg.(1)

