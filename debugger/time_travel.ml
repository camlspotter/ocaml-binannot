(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(**************************** Time travel ******************************)

open Instruct
open Events
open Debugcom
open Primitives
open Checkpoints
open Breakpoints
open Trap_barrier
open Input_handling
open Debugger_config
open Program_loading

exception Current_checkpoint_lost

let remove_1st key list =
  let rec remove =
    function
      []   -> []
    | a::l -> if a == key then l else a::(remove l)
  in
    remove list

(*** Debugging. ***)

let debug_time_travel = ref false

(*** Internal utilities. ***)

(* Insert a checkpoint in the checkpoint list.
 * Raise `Exit' if there is already a checkpoint at the same time.
 *)
let insert_checkpoint ({c_time = time} as checkpoint) =
  let rec traverse =
    function
      [] -> [checkpoint]
    | (({c_time = t} as a)::l) as l' ->
        if t > time then
	  a::(traverse l)
	else if t = time then
	  raise Exit
	else
          checkpoint::l'
  in
    checkpoints := traverse !checkpoints

(* Remove a checkpoint from the checkpoint list.
 * --- No error if not found.
 *)
let remove_checkpoint checkpoint =
  checkpoints := remove_1st checkpoint !checkpoints

(* Wait for the process used by `checkpoint' to connect.
 * --- Usually not called (the process is already connected).
 *)
let wait_for_connection checkpoint =
  try
    Exec.unprotected
      (function () ->
         let old_controller = Input_handling.current_controller !connection in
           execute_with_other_controller
             (function
      	        fd ->
                  old_controller fd;
                  if checkpoint.c_valid = true then
	            exit_main_loop ())
             !connection
             main_loop)
  with
    Sys.Break ->
      checkpoint.c_parent = root;
      remove_checkpoint checkpoint;
      checkpoint.c_pid <- -1;
      raise Sys.Break

(* Select a checkpoint as current. *)
let set_current_checkpoint checkpoint =
  if !debug_time_travel then
    prerr_endline ("Select : " ^ (string_of_int checkpoint.c_pid));
  if not checkpoint.c_valid then
    wait_for_connection checkpoint;
  current_checkpoint := checkpoint;
  set_current_connection checkpoint.c_fd

(* Kill `checkpoint'. *)
let kill_checkpoint checkpoint =
  if !debug_time_travel then
    prerr_endline ("Kill : " ^ (string_of_int checkpoint.c_pid));
  if checkpoint.c_pid > 0 then		(* Ghosts don't have to be killed ! *)
    (if not checkpoint.c_valid then
       wait_for_connection checkpoint;
     stop checkpoint.c_fd;
     if checkpoint.c_parent.c_pid > 0 then
       wait_child checkpoint.c_parent.c_fd;
     checkpoint.c_parent = root;
     close_io checkpoint.c_fd;
     remove_file checkpoint.c_fd;
     remove_checkpoint checkpoint);
  checkpoint.c_pid <- -1		(* Don't exist anymore *)

(*** Cleaning the checkpoint list. ***)

(* Separe checkpoints before (<=) and after (>) `t'. *)
(* ### t checkpoints -> (after, before) *)
let cut t =
  let rec cut_t =
    function
      [] -> ([], [])
    | ({c_time = t'} as a::l) as l' ->
        if t' <= t then
      	  ([], l')
        else
      	  let (b, e) = cut_t l in
      	    (a::b, e)
  in
    cut_t

(* Partition the checkpoints list. *)
let cut2 t0 t l =
  let rec cut2_t0 t =
    function
      [] -> []
    | l ->
       let (after, before) = cut (t0 - t - 1) l in
         let l = cut2_t0 (2 * t) before in
       	   after::l
  in
    let (after, before) = cut (t0 - 1) l in
      after::(cut2_t0 t before)

(* Separe first elements and last element of a list of checkpoint. *)
let chk_merge2 cont =
  let rec chk_merge2_cont =
    function
      [] -> cont
    | [a] ->
        let (accepted, rejected) = cont in
          (a::accepted, rejected)
    | a::l ->
        let (accepted, rejected) = chk_merge2_cont l in
      	  (accepted, a::rejected)
  in chk_merge2_cont

(* Separe the checkpoint list. *)
(* ### list -> accepted * rejected *)
let rec chk_merge =
  function
    [] -> ([], [])
  | l::tail ->
       chk_merge2 (chk_merge tail) l

let new_checkpoint_list checkpoint_count accepted rejected =
  if List.length accepted >= checkpoint_count then
    let (k, l) = list_truncate2 checkpoint_count accepted in
      (k, l @ rejected)
  else
    let (k, l) =
      list_truncate2 (checkpoint_count - List.length accepted) rejected
    in
      (Sort.merge (fun {c_time = t1} {c_time = t2} -> t1 > t2) accepted k,
       l)

(* Clean the checkpoint list. *)
(* Reference time is `time'. *)
let clean_checkpoints time checkpoint_count =
  let (after, before) = cut time !checkpoints in
    let (accepted, rejected) =
      chk_merge (cut2 time !checkpoint_small_step before)
    in
      let (kept, lost) =
        new_checkpoint_list checkpoint_count accepted after
      in
	List.map kill_checkpoint (lost @ rejected);
      	checkpoints := kept

(*** Internal functions for moving. ***)

(* Find the first checkpoint before (or at) `time'.
 * Ask for reloading the program if necessary.
 *)
let find_checkpoint_before time =
  let rec find =
    function
      [] ->
      	print_string "Can't go that far in the past !"; print_newline ();
      	if yes_or_no "Reload program" then begin
      	  load_program ();
      	  find !checkpoints
	  end
	else
	  raise Toplevel
    | { c_time = t } as a::l ->
      	if t > time then
          find l
	else
	  a
  in find !checkpoints

(* Make a copy of the current checkpoint and clean the checkpoint list. *)
(* --- The new checkpoint in not put in the list. *)
let duplicate_current_checkpoint () =
  let checkpoint = !current_checkpoint in
    if not checkpoint.c_valid then
      wait_for_connection checkpoint;
    let new_checkpoint =			(* Ghost *)
      {c_time = checkpoint.c_time;
       c_pid = 0;
       c_fd = checkpoint.c_fd;
       c_valid = false;
       c_report = checkpoint.c_report;
       c_state = C_stopped;
       c_parent = checkpoint;
       c_breakpoint_version = checkpoint.c_breakpoint_version;
       c_breakpoints = checkpoint.c_breakpoints;
       c_trap_barrier = checkpoint.c_trap_barrier}
    in
      checkpoints := list_replace checkpoint new_checkpoint !checkpoints;
      set_current_checkpoint checkpoint;
      clean_checkpoints (checkpoint.c_time + 1) (!checkpoint_max_count - 1);
      if new_checkpoint.c_pid = 0 then	(* The ghost has not been killed *)
        (match do_checkpoint () with	(* Duplicate checkpoint *)
           Checkpoint_done pid ->
	     (new_checkpoint.c_pid <- pid;
	      if !debug_time_travel then
                prerr_endline ("Waiting for connection : " ^ (string_of_int pid)))
         | Checkpoint_failed ->
      	     prerr_endline
               "A fork failed. Reducing maximum number of checkpoints.";
      	     checkpoint_max_count := List.length !checkpoints - 1;
       	     remove_checkpoint new_checkpoint)

(* Ensure we stop on an event. *)
let rec stop_on_event report =
  let find_event () =
    if !debug_time_travel then
      (print_string "Searching next event..."; print_newline ());
    let report = do_go 1 in
      !current_checkpoint.c_report <- Some report;
      stop_on_event report
  in
    match report with
      {rep_type = Breakpoint; rep_program_pointer = pc} ->
      	if Some pc = !temporary_breakpoint_position then begin
					(* Others breakpoints are on events. *)
          try				(* Check if we are on an event. *)
      	    update_current_event ()
          with
       	    Not_found -> find_event ()
          end
    | {rep_type = Trap_barrier; rep_stack_pointer = trap_frame} ->
	(* No event at current position. *)
        find_event ()
    | _ ->
      	()

(* Was the movement interrupted ? *)
(* --- An exception could have been used instead, *)
(* --- but it is not clear where it should be caught. *)
(* --- For instance, we can't caught it should not be caught in `step' *)
(* --- (as `step' is used in `next_1'). *)
(* --- On the other side, other modules does not need to know *)
(* --- about this exception. *)
let interrupted = ref false

(* Internal function for running debugged program.
 * Requires `duration > 0'.
 *)
let internal_step duration =
  match current_report () with
    Some {rep_type = Exited | Uncaught_exc} -> ()
  | _ ->
      Exec.protected
        (function () ->
	   if !make_checkpoints then
             duplicate_current_checkpoint ()
	   else
	     remove_checkpoint !current_checkpoint;
           update_breakpoints ();
           update_trap_barrier ();
	   !current_checkpoint.c_state <- C_running duration;
           let report = do_go duration in
             !current_checkpoint.c_report <- Some report;
	     !current_checkpoint.c_state <- C_stopped;
	     if report.rep_type = Event then begin
               !current_checkpoint.c_time <-
                 !current_checkpoint.c_time + duration;
	       interrupted := false
	       end
	     else begin
               !current_checkpoint.c_time <-
      	          !current_checkpoint.c_time + duration
      	          - report.rep_event_count + 1;
	       interrupted := true;
	       stop_on_event report
      	       end;
             (try
                insert_checkpoint !current_checkpoint
              with
                Exit ->
                  kill_checkpoint !current_checkpoint;
                  set_current_checkpoint
      	            (find_checkpoint_before (current_time ()))));
	if !debug_time_travel then begin
          print_string "Checkpoints : pid(time)"; print_newline ();
      	  List.map
            (function {c_time = time; c_pid = pid; c_valid = valid} ->
      	       print_int pid;
               print_string "("; print_int time; print_string ")";
               if not valid then print_string "(invalid)";
      	       print_string " ")
      	    !checkpoints;
      	  print_newline ()
	  end

(*** Miscellaneous functions (exported). ***)

(* Create a checkpoint at time 0 (new program). *)
let new_checkpoint pid fd =
  let new_checkpoint =
    {c_time = 0;
     c_pid = pid;
     c_fd = fd;
     c_valid = true;
     c_report = None;
     c_state = C_stopped;
     c_parent = root;
     c_breakpoint_version = 0;
     c_breakpoints = [];
     c_trap_barrier = 0}
  in
    insert_checkpoint new_checkpoint

(* Set the file descriptor of a checkpoint *)
(* (a new process has connected with the debugger). *)
(* --- Return `true' on success (close the connection otherwise). *)
let set_file_descriptor pid fd =
  let rec find =
    function
      [] ->
	prerr_endline "Unexpected connection";
	close_io fd;
      	false
    | ({c_pid = pid'} as checkpoint)::l ->
        if pid <> pid' then
	  find l
	else
	  (checkpoint.c_fd <- fd;
	   checkpoint.c_valid <- true;
	   true)
  in
    if !debug_time_travel then
      prerr_endline ("New connection : " ^(string_of_int pid));
    find (!current_checkpoint::!checkpoints)

(* Kill all the checkpoints. *)
let kill_all_checkpoints () =
  List.iter kill_checkpoint (!current_checkpoint::!checkpoints)

(* Kill a checkpoint without killing the process. *)
(* (used when connection with the process is lost). *)
(* --- Assume that the checkpoint is valid. *)
let forget_process fd pid =
  let checkpoint =
    find (function c -> c.c_pid = pid) (!current_checkpoint::!checkpoints)
  in
    prerr_string "Lost connection with process ";
    prerr_int pid;
    if checkpoint = !current_checkpoint then begin
      prerr_endline " (active process)";
      match !current_checkpoint.c_state with
      	C_stopped ->
	  prerr_string "at time ";
	  prerr_int !current_checkpoint.c_time
      | C_running duration ->
          prerr_string "between time ";
          prerr_int !current_checkpoint.c_time;
          prerr_string " and time ";
          prerr_int (!current_checkpoint.c_time + duration)
      end;
    prerr_endline "";
    Input_handling.remove_file fd;
    close_io checkpoint.c_fd;
    remove_file checkpoint.c_fd;
    remove_checkpoint checkpoint;
    checkpoint.c_pid <- -1;		(* Don't exist anymore *)
    if checkpoint.c_parent.c_pid > 0 then
      wait_child checkpoint.c_parent.c_fd;
    if checkpoint = !current_checkpoint then
      raise Current_checkpoint_lost

(* Try to recover when the current checkpoint is lost. *)
let recover () =
  set_current_checkpoint
    (find_checkpoint_before (current_time ()))

(*** Simple movements. ***)

(* Forward stepping.  Requires `duration >= 0'. *)
let rec step_forward duration =
  if duration > !checkpoint_small_step then begin
    let first_step =
      if duration > !checkpoint_big_step then
      	!checkpoint_big_step
      else
      	!checkpoint_small_step
    in
      internal_step first_step;
      if not !interrupted then
        step_forward (duration - first_step)
    end
  else if duration != 0 then
    internal_step duration

(* Go to time `time' from current checkpoint (internal). *)
let internal_go_to time =
  let duration = time - current_time () in
    if duration > 0 then
      step_forward duration

(* Move to a given time. *)
let go_to time =
  let checkpoint = find_checkpoint_before time in
    set_current_checkpoint checkpoint;
    internal_go_to time

(* Return the time of the last breakpoint *)
(* between current time and `max_time'. *)
let rec find_last_breakpoint max_time =
  let on_breakpoint () =
    match current_report () with
      Some {rep_program_pointer = pc} ->
      	breakpoint_at_pc pc
    | _ ->
      	false
  in
    let rec find break =
      let time = current_time () in
        step_forward (max_time - time);
	if ((on_breakpoint ()) & (current_time () < max_time)) then
	  find true
	else
	  (time, break)
    in
      find (on_breakpoint ())

(* Run from `time_max' back to `time'. *)
(* --- Assume 0 <= time < time_max *)
let rec back_to time time_max =
  let
    {c_time = t} as checkpoint = find_checkpoint_before (time_max - 1)
  in
    go_to (max time t);
    let (new_time, break) = find_last_breakpoint time_max in
      if break or (new_time <= time) then
        go_to new_time
      else
      	back_to time new_time

(* Backward stepping. *)
(* --- Assume duration > 1 *)
let step_backward duration =
  let time = current_time () in
    if time > 0 then
      back_to (max 0 (time - duration)) time

(* Run the program from current time. *)
(* Stop at the first breakpoint, or at the end of the program. *)
let rec run () =
  internal_step !checkpoint_big_step;
  if not !interrupted then
    run ()

(* Run backward the program form current time. *)
(* Stop at the first breakpoint, or at the beginning of the program. *)
let back_run () =
  if current_time () > 0 then
    back_to 0 (current_time ())

(* Step in any direction. *)
(* Stop at the first brakpoint, or after `duration' steps. *)
let step duration =
  if duration >= 0 then
    step_forward duration
  else
    step_backward (-duration)

(*** Next, finish. ***)

(* Finish current fucntion. *)
let finish () =
  match !current_event with
    None ->
      prerr_endline "Program is currently not running."; raise Toplevel
  | Some curr_event ->
      initial_frame();
      let (frame, pc) = up_frame curr_event.ev_stacksize in
      if frame < 0 then begin
        prerr_endline "`finish' not meaningful in outermost frame.";
        raise Toplevel
      end;
      exec_with_trap_barrier
      	frame
	(fun () ->
           exec_with_temporary_breakpoint
             pc
             (fun () ->
                while
		  run ();
     	          match current_report () with
     	            Some {rep_type = Breakpoint;
                          rep_stack_pointer = sp;
                          rep_program_pointer = pc2} ->
     	              (pc = pc2) && (frame <> sp)
                  | _ ->
     	              false
                do
     	          ()
     	        done))

let next_1 () =
  match !current_event with
    None ->				(* Beginning of the program. *)
      step 1
  | Some event1 ->
      let (frame1, pc1) = initial_frame() in
      step 1;
      match !current_event with
        None -> ()
      | Some event2 ->
          let (frame2, pc2) = initial_frame() in
          (* Call `finish' if we've entered a function. *)
          if frame1 >= 0 && frame2 >= 0 &&
             frame2 + event2.ev_stacksize < frame1 + event1.ev_stacksize
          then finish()

(* Same as `step' (forward) but skip over function calls. *)
let rec next =
  function
    0 -> ()
  | n ->
      next_1 ();
      if not !interrupted then
        next (n - 1)
