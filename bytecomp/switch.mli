(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(*
  This module transforms generic switches in combinations
  of if tests and switches.
*)


(* integer plus infinity, for interval limits *)

type iext = TooMuch | Int of int

(* Arguments to the Make functor *)
module type S =
  sig
    (* type of basic tests *)
    type primitive
    (* basic tests themselves *)
    val eqint : primitive
    val neint : primitive
    val leint : primitive
    val ltint : primitive
    val geint : primitive
    val gtint : primitive
    (* type of actions *)
    type act
    (* default action *)
    val default : act

    (* Various constructors, for making a binder,
        adding one integer, etc. *)
    val bind : act -> (act -> act) -> act
    val make_offset : act -> int -> act
    val make_prim : primitive -> act list -> act
    val make_isout : act -> act -> act
    val make_if : act -> act -> act -> act
   (* construct an actual switch :
      make_switch arg cases acts
      NB:  cases is in the interval form *)
    val make_switch :
      act -> (int * int * int) array -> act array -> act
  end


(*
  Make.zyva mk_const arg low high cases actions where
    - mk_const takes an integer sends a constant action.
    - arg is the argument of the switch.
    - low, high are the interval limits.
    - cases is a list of sub-interval and action indices
    - action is an array of actions.

  All these arguments specify a switch construct and zyva
  returns an action that performs the switch,
*)
module Make :
  functor (Arg : S) ->
    sig
      val zyva :
          (int -> Arg.act) ->
           Arg.act ->
           iext -> iext ->
           (int * int * int) array ->
           Arg.act array ->
           Arg.act
    end
