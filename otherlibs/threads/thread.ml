(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* User-level threads *)

type t

let critical_section = ref false

(* It is mucho important that the primitives that reschedule are called 
   through an ML function call, not directly. That's because when such a
   primitive returns, the bytecode interpreter is only semi-obedient:
   it takes sp from the new thread, but keeps pc from the old thread.
   But that's OK if all calls to rescheduling primitives are immediately
   followed by a RETURN operation, which will restore the correct pc
   from the stack. *)

external thread_initialize : unit -> unit = "thread_initialize"
external thread_new : (unit -> unit) -> t = "thread_new"
external thread_yield : unit -> unit = "thread_yield"
external thread_sleep : unit -> unit = "thread_sleep"
external thread_wait_descr : Unix.file_descr -> unit = "thread_wait_descr"
external thread_wait_inchan : in_channel -> unit = "thread_wait_inchan"
external thread_join : t -> unit = "thread_join"
external thread_delay : float -> unit = "thread_wait_for"
external thread_wakeup : t -> unit = "thread_wakeup"
external thread_self : unit -> t = "thread_self"
external thread_kill : t -> unit = "thread_kill"

(* In sleep() below, we rely on the fact that signals are detected
   only at function applications and beginning of loops,
   making all other operations atomic. *)

let sleep () = critical_section := false; thread_sleep()
let wait_descr fd = thread_wait_descr fd
let wait_inchan ic = thread_wait_inchan ic
let delay duration = thread_delay duration
let join th = thread_join th
let wakeup pid = thread_wakeup pid
let self () = thread_self()
let kill pid = thread_kill pid
let exit () = thread_kill(thread_self())

(* For new, make sure the function passed to thread_new always terminates
   by calling exit. *)

let new fn arg =
  thread_new
    (fun () ->
      try
        Printexc.print fn arg; exit()
      with x ->
        flush stdout; flush stderr; exit())

(* Preemption *)

let preempt signal =
  if !critical_section then () else thread_yield()

(* Initialization of the scheduler *)

let _ =
  Sys.signal Sys.sigvtalrm (Sys.Signal_handle preempt);
  thread_initialize()
