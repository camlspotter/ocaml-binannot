(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*         Jun Furuse, projet Cristal, INRIA Rocquencourt                *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* LablTk4 Demonstration by JPF *)

(* First, open this modules for convenience *)
open Tk

(* initialization of Tk --- the result is a toplevel widget *)
let top = openTk () 

(* create a button on top *)
(* Button.create : use of create function defined in button.ml *)
(* But you shouldn't open Button module for other widget class modules use *)
let b = Button.create text: "Hello, LablTk!" top

(* Lack of toplevel expressions in lsl, you must use dummy let exp. *)
let _ = pack [coe b] 

(* Last, you must call mainLoop *)
(* You can write just let _ = mainLoop () *)
(* But Printexc.print will help you *)
let _ = Printexc.print mainLoop ()
