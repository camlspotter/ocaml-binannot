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

(* Specific operations for the Alpha processor *)

open Formatmsg

(* Addressing modes *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)

(* Specific operations *)

type specific_operation =
    Iadd4 | Iadd8 | Isub4 | Isub8       (* Scaled adds and subs *)
  | Ireloadgp of bool                   (* The ldgp instruction *)
  | Itrunc32                            (* Truncate 64-bit int to 32 bit *)

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
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
    Iadd4 -> printreg arg.(0); print_string " * 4 + "; printreg arg.(1)
  | Iadd8 -> printreg arg.(0); print_string " * 8 + "; printreg arg.(1)
  | Isub4 -> printreg arg.(0); print_string " * 4 - "; printreg arg.(1)
  | Isub8 -> printreg arg.(0); print_string " * 8 - "; printreg arg.(1)
  | Ireloadgp _ -> print_string "ldgp"
  | Itrunc32 -> print_string "truncate32 "; printreg arg.(0)

(* Distinguish between the Digital assembler and other assemblers (e.g. gas) *)

let digital_asm =
  match Config.system with
    "digital" -> true
  | _ -> false
