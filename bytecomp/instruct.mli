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

(* The type of the instructions of the abstract machine *)

open Lambda

(* Structure of compilation environments *)

type compilation_env =
  { ce_stack: int Ident.tbl; (* Positions of variables in the stack *)
    ce_heap: int Ident.tbl } (* Structure of the heap-allocated env *)

(* The ce_stack component gives locations of variables residing 
   in the stack. The locations are offsets w.r.t. the origin of the
   stack frame.
   The ce_heap component gives the positions of variables residing in the
   heap-allocated environment. *)

(* Debugging events *)

type debug_event =
  { mutable ev_pos: int;                (* Position in bytecode *)
    ev_file: string;                    (* Source file name *)
    ev_char: int;                       (* Location in source file *)
    ev_kind: debug_event_kind;          (* Before/after event *)
    ev_typenv: Env.summary;             (* Typing environment *)
    ev_compenv: compilation_env;        (* Compilation environment *)
    ev_stacksize: int }                 (* Size of stack frame *)

and debug_event_kind =
    Event_before
  | Event_after of Types.type_expr

(* Abstract machine instructions *)

type label = int                        (* Symbolic code labels *)

type instruction =
    Klabel of label
  | Kacc of int
  | Kenvacc of int
  | Kpush
  | Kpop of int
  | Kassign of int
  | Kpush_retaddr of label
  | Kapply of int                       (* number of arguments *)
  | Kappterm of int * int               (* number of arguments, slot size *)
  | Kreturn of int                      (* slot size *)
  | Krestart
  | Kgrab of int                        (* number of arguments *)
  | Kclosure of label * int
  | Kclosurerec of label * int
  | Kgetglobal of Ident.t
  | Ksetglobal of Ident.t
  | Kconst of structured_constant
  | Kmakeblock of int * int             (* size, tag *)
  | Kgetfield of int
  | Ksetfield of int
  | Kdummy of int                       (* block size *)
  | Kupdate of int                      (* block size *)
  | Kvectlength
  | Kgetvectitem
  | Ksetvectitem
  | Kgetstringchar
  | Ksetstringchar
  | Kbranch of label
  | Kbranchif of label
  | Kbranchifnot of label
  | Kstrictbranchif of label
  | Kstrictbranchifnot of label
  | Kswitch of label array * label array
  | Kboolnot
  | Kpushtrap of label
  | Kpoptrap
  | Kraise
  | Kcheck_signals
  | Kccall of string * int
  | Knegint | Kaddint | Ksubint | Kmulint | Kdivint | Kmodint
  | Kandint | Korint | Kxorint | Klslint | Klsrint | Kasrint
  | Kintcomp of comparison
  | Koffsetint of int
  | Koffsetref of int
  | Kgetmethod
  | Kevent of debug_event
  | Kstop

val immed_min: int
val immed_max: int
