(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*         Jun Furuse, projet Cristal, INRIA Rocquencourt                *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

open Tk
open Widget
open Balloon
open Protocol

let _ =
let t = openTk () in
Balloon.init ();
let b = Button.create parent: t text: "hello" in
Button.configure b command: (fun () -> destroy b);
pack [b];
Balloon.put on: b ms: 1000 "Balloon";
Printexc.catch mainLoop ()
 
