(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type stat = {
  minor_words : float;
  promoted_words : float;
  major_words : float;
  minor_collections : int;
  major_collections : int;
  heap_words : int;
  heap_chunks : int;
  live_words : int;
  live_blocks : int;
  free_words : int;
  free_blocks : int;
  largest_free : int;
  fragments : int;
  compactions : int
};;

type control = {
  mutable minor_heap_size : int;
  mutable major_heap_increment : int;
  mutable space_overhead : int;
  mutable verbose : int;
  mutable max_overhead : int;
  mutable stack_limit : int
};;

external stat : unit -> stat = "gc_stat";;
external counters : unit -> (float * float * float) = "gc_counters";;
external get : unit -> control = "gc_get";;
external set : control -> unit = "gc_set";;
external minor : unit -> unit = "gc_minor";;
external major : unit -> unit = "gc_major";;
external full_major : unit -> unit = "gc_full_major";;
external compact : unit -> unit = "gc_compaction";;

open Printf;;

let print_stat c =
  let st = stat () in
  fprintf c "minor_words: %.0f\n" st.minor_words;
  fprintf c "promoted_words: %.0f\n" st.promoted_words;
  fprintf c "major_words: %.0f\n" st.major_words;
  fprintf c "minor_collections: %d\n" st.minor_collections;
  fprintf c "major_collections: %d\n" st.major_collections;
  fprintf c "heap_words: %d\n" st.heap_words;
  fprintf c "heap_chunks: %d\n" st.heap_chunks;
  fprintf c "live_words: %d\n" st.live_words;
  fprintf c "live_blocks: %d\n" st.live_blocks;
  fprintf c "free_words: %d\n" st.free_words;
  fprintf c "free_blocks: %d\n" st.free_blocks;
  fprintf c "largest_free: %d\n" st.largest_free;
  fprintf c "fragments: %d\n" st.fragments;
  fprintf c "compactions: %d\n" st.compactions;
;;

let allocated_bytes () =
  let (mi, pro, ma) = counters () in
  (mi +. ma -. pro) *. float_of_int (Sys.word_size / 8)
;;

external finalise : ('a -> unit) -> 'a -> unit = "final_register";;


type alarm_rec = {active : alarm; f : unit -> unit}
and alarm = bool ref
;;

let rec call_alarm arec =
  if !(arec.active) then begin
    finalise call_alarm arec;
    arec.f ();
  end;
;;

let create_alarm f =
  let a = ref true in
  let arec = { active = ref true; f = f } in
  finalise call_alarm arec;
  a
;;

let delete_alarm a = a := false;;
