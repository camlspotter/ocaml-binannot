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

(**************************** Input control ****************************)

open Unix
open Primitives

(*** Actives files. ***)

(* List of the actives files. *)
let active_files =
  ref ([] : (file_descr * ((io_channel -> unit) * io_channel)) list)

(* Add a file to the list of actives files. *)
let add_file file controller =
  active_files := (file.io_fd, (controller, file))::!active_files

(* Remove a file from the list of actives files. *)
let remove_file file =
  active_files := assoc_remove !active_files file.io_fd

(* Change the controller for the given file. *)
let change_controller file controller =
  remove_file file; add_file file controller

(* Return the controller currently attached to the given file. *)
let current_controller file =
  fst (List.assoc file.io_fd !active_files)

(* Execute a function with `controller' attached to `file'. *)
(* ### controller file funct *)
let execute_with_other_controller controller file funct =
  let old_controller = current_controller file in
    change_controller file controller;
    try
      let result = funct () in
        change_controller file old_controller;
        result
    with
      x ->
        change_controller file old_controller;
        raise x

(*** The "Main Loop" ***)

let continue_main_loop =
  ref true

let exit_main_loop _ =
  continue_main_loop := false

(* Handle active files until `continue_main_loop' is false. *)
let main_loop () =
  let old_state = !continue_main_loop in
    try
      continue_main_loop := true;
      while !continue_main_loop do
        try
          let (input, _, _) =
            select (List.map fst !active_files) [] [] (-1.)
          in
            List.iter
              (function fd ->
                 let (funct, iochan) = (List.assoc fd !active_files) in
                   funct iochan)
              input
        with
          Unix_error (EINTR, _, _) -> ()
      done;
      continue_main_loop := old_state
    with
      x ->
        continue_main_loop := old_state;
        raise x

(*** Managing user inputs ***)

(* Are we in interactive mode ? *)
let interactif = ref true

let current_prompt = ref ""

(* Where the user input come from. *)
let user_channel = ref std_io

let read_user_input buffer length =
  main_loop ();
  input !user_channel.io_in buffer 0 length

(* Stop reading user input. *)
let stop_user_input () =
  remove_file !user_channel

(* Resume reading user input. *)
let resume_user_input () =
  if not (List.mem_assoc !user_channel.io_fd !active_files) then begin
    if !interactif then begin
      print_string !current_prompt;
      flush Pervasives.stdout
      end;
    add_file !user_channel exit_main_loop
    end

(* Ask user a yes or no question. *)
let yes_or_no message =
  if !interactif then
    let old_prompt = !current_prompt in
      try
        current_prompt := message ^ " ? (y or n) ";
        let answer =
          let rec ask () =
            resume_user_input ();
            let line =
              string_trim (Lexer.line (Lexing.from_function read_user_input))
            in
              stop_user_input ();
              match (if String.length line > 0 then line.[0] else ' ') with
                'y' -> true
              | 'n' -> false
              | _ ->
                print_string "Please answer y or n.";
                print_newline ();
                ask ()
          in
            ask ()
        in
          current_prompt := old_prompt;
          answer
      with
        x ->
          current_prompt := old_prompt;
          stop_user_input ();
          raise x
  else
    false
