(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Transformation of Mach code into a list of pseudo-instructions. *)

type label = int
val new_label: unit -> label

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    live: Reg.Set.t }

and instruction_desc =
    Lend
  | Lop of Mach.operation
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of Mach.test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Lsetuptrap of label
  | Lpushtrap
  | Lpoptrap
  | Lraise

type fundecl =
  { fun_name: string;
    fun_body: instruction;
    fun_fast: bool }

val fundecl: Mach.fundecl -> fundecl

