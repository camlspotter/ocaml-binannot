(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Specific operations for the Motorola 68020 processor *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)
  | Iscaled of int * int                (* reg * scale + displ *)
  | Iindexed2scaled of int * int        (* reg + reg * scale + displ *)

type specific_operation =
    Ilea of addressing_mode             (* Lea gives scaled adds *)
  | Istore_int of int * addressing_mode (* Store an integer constant *)
  | Istore_symbol of string * addressing_mode (* Store a symbol *)
  | Ipush                               (* Push regs on stack *)
  | Ipush_int of int                    (* Push an integer constant *)
  | Ipush_symbol of string              (* Push a symbol *)
  | Ipush_load of addressing_mode       (* Load a scalar and push *)
  | Ipush_load_float of addressing_mode (* Load a float and push *)

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
  | Iindexed2 n -> Iindexed2(n + delta)
  | Iscaled(scale, n) -> Iscaled(scale, n + delta)
  | Iindexed2scaled(scale, n) -> Iindexed2scaled(scale, n + delta)

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1
  | Iindexed2 n -> 2
  | Iscaled(scale, n) -> 1
  | Iindexed2scaled(scale, n) -> 2

(* Printing operations and addressing modes *)

open Formatmsg

let print_addressing printreg addr arg =
  match addr with
    Ibased(s, 0) ->
      print_string "\""; print_string s; print_string "\""
  | Ibased(s, n) ->
      print_string "\""; print_string s; print_string "\" + "; print_int n
  | Iindexed n ->
      printreg arg.(0);
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed2 n ->
      printreg arg.(0); print_string " + "; printreg arg.(1);
      if n <> 0 then begin print_string " + "; print_int n end
  | Iscaled(scale, n) ->
      printreg arg.(0); print_string " * "; print_int scale;
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed2scaled(scale, n) ->
      printreg arg.(0); print_string " + "; printreg arg.(1);
      print_string " * "; print_int scale;
      if n <> 0 then begin print_string " + "; print_int n end

let print_specific_operation printreg op arg =
  match op with
    Ilea addr -> print_addressing printreg addr arg
  | Istore_int(n, addr) ->
      print_string "["; print_addressing printreg addr arg;
      print_string "] := "; print_int n
  | Istore_symbol(lbl, addr) ->
      print_string "["; print_addressing printreg addr arg;
      print_string "] := \""; print_string lbl; print_string "\""
  | Ipush ->
      print_string "push ";
      for i = 0 to Array.length arg - 1 do
        if i > 0 then print_string ", ";
        printreg arg.(i)
      done
  | Ipush_int n ->
      print_string "push "; print_int n
  | Ipush_symbol s ->
      print_string "push \""; print_string s; print_string "\""
  | Ipush_load addr ->
      print_string "push ["; print_addressing printreg addr arg;
      print_string "]"
  | Ipush_load_float addr ->
      print_string "pushfloat ["; print_addressing printreg addr arg;
      print_string "]"
