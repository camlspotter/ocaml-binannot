(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Instruct
open Formatmsg
open Primitives
open Debugcom
open Checkpoints
open Events
open Symbols
open Frames
open Show_source
open Breakpoints

(* Display information about the current event. *)
let show_current_event () =
  print_string "Time : "; print_int (current_time ());
  (match current_pc () with
     Some pc ->
       print_string " - pc : "; print_int pc
   | _ -> ());
  update_current_event ();
  reset_frame ();
  match current_report ()  with
    None ->
      print_newline ();
      print_string "Beginning of program."; print_newline ();
      show_no_point ()
  | Some {rep_type = (Event | Breakpoint); rep_program_pointer = pc} -> 
     let (mdle, point) = current_point () in
        print_string (" - module " ^ mdle);
        print_newline ();
        (match breakpoints_at_pc pc with
           [] ->
             ()
         | [breakpoint] ->
             print_string "Breakpoint : "; print_int breakpoint;
             print_newline ()
         | breakpoints ->
             print_string "Breakpoints : ";
             List.iter
               (function x -> print_int x; print_string " ")
               (Sort.list (<) breakpoints);
             print_newline ());
        show_point mdle point (current_event_is_before ()) true
  | Some {rep_type = Exited} ->
      print_newline (); print_string "Program exit."; print_newline ();
      show_no_point ()
  | Some {rep_type = Uncaught_exc} ->
      printf "@.Program end.@.";
      printf "@[Uncaught exception:@ ";
      Printval.print_exception (Debugcom.Remote_value.accu ());
      printf"@]@.";
      show_no_point ()
  | Some {rep_type = Trap_barrier} ->
                                        (* Trap_barrier not visible outside *)
                                        (* of module `time_travel'. *)
      Misc.fatal_error "Show_information.show_current_event"

(* Display short information about one frame. *)

let show_one_frame framenum event =
  printf "#%i  Pc : %i  %s char %i@."
         framenum event.ev_pos event.ev_module event.ev_char

(* Display information about the current frame. *)
(* --- `select frame' must have succeded before calling this function. *)
let show_current_frame selected =
  match !selected_event with
    None ->
      printf "@.No frame selected.@."
  | Some sel_ev ->
      show_one_frame !current_frame sel_ev;
      begin match breakpoints_at_pc sel_ev.ev_pos with
        [] ->
          ()
      | [breakpoint] ->
          printf "Breakpoint : %i@." breakpoint
      | breakpoints ->
          printf "Breakpoints : ";
          List.iter (function x -> printf "%i " x) (Sort.list (<) breakpoints);
          print_newline ()
      end;
      show_point sel_ev.ev_module sel_ev.ev_char
                 (selected_event_is_before ()) selected
