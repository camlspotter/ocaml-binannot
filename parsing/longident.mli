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

(* Long identifiers, used in parsetree. *)

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

val flatten: t -> string list
val last: t -> string
val parse: string -> t

module LongidentTbl : Hashtbl.S with type key = t

type lid2loc = Location.t LongidentTbl.t

val record_longident_locations : unit -> unit

val flush_longidents : unit -> lid2loc option

val add_longident : Location.t -> t -> unit
val remove_longident : t -> unit
