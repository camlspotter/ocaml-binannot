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

(* Pretty-printing of linearized machine code *)

open Formatmsg
open Mach
open Printmach
open Linearize

let label l =
  print_string "L"; print_int l

let instr i =
  match i.desc with
    Lend -> ()
  | Lop op ->
      begin match op with
        Ialloc _ | Icall_ind | Icall_imm _ | Iextcall(_, _) ->
          open_box 1;
          print_string "{";
          regsetaddr i.live;
          print_string "}";
          close_box();
          print_cut()
      | _ -> ()
      end;
      operation op i.arg i.res
  | Lreloadretaddr ->
      print_string "reload retaddr"
  | Lreturn ->
      print_string "return "; regs i.arg
  | Llabel lbl ->
      label lbl; print_string ":"
  | Lbranch lbl ->
      print_string "goto "; label lbl
  | Lcondbranch(tst, lbl) ->
      print_string "if "; test tst i.arg; print_string " goto "; label lbl
  | Lcondbranch3(lbl0, lbl1, lbl2) ->
      print_string "switch3 "; reg i.arg.(0);
      let case n = function
        None -> ()
      | Some lbl ->
          print_cut();
          print_string "case "; print_int n;
          print_string ": goto "; label lbl in
      case 0 lbl0; case 1 lbl1; case 2 lbl2;
      print_cut(); print_string "endswitch"
  | Lswitch lblv ->
      print_string "switch "; reg i.arg.(0);
      for i = 0 to Array.length lblv - 1 do
        print_cut();
        print_string "case "; print_int i;
        print_string ": goto "; label lblv.(i)
      done;
      print_cut(); print_string "endswitch"
  | Lsetuptrap lbl ->
      print_string "setup trap "; label lbl
  | Lpushtrap ->
      print_string "push trap"
  | Lpoptrap ->
      print_string "pop trap"
  | Lraise ->
      print_string "raise "; reg i.arg.(0)

let rec all_instr i =
  match i.desc with
    Lend -> ()
  | _ -> instr i; print_cut(); all_instr i.next

let fundecl f =
  open_vbox 2;
  print_string f.fun_name; print_string ":"; print_cut();
  all_instr f.fun_body;
  close_box()
