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

(* Module [Thread]: user-level lightweight threads *)

type t
        (* The type of thread identifiers. *)

(** Thread creation and termination *)

val new : ('a -> 'b) -> 'a -> t
        (* [new funct arg] creates a new thread of control, in which the
           function application [funct arg] is executed concurrently
           with the other threads of the program. The application of [new]
           returns the identifier of the newly created thread.
           The new thread terminates when the application [funct arg]
           returns, either normally or by raising an uncaught exception.
           In the latter case, the exception is printed on standard error,
           but not propagated back to the parent thread. Similarly, the
           result of the application [funct arg] is discarded and not
           directly accessible to the parent thread. *)
val self : unit -> t
        (* Return the identifier of the calling thread. *)
val exit : unit -> unit
        (* Terminate prematurely the calling thread. *)
val kill : t -> unit
        (* Terminate prematurely the thread whose identifier is given. *)

(** Suspending threads *)

val delay: float -> unit
        (* [delay d] suspends the execution of the calling thread for
           [d] seconds. The other program threads continue to run during
           this time. *)
val wait_inchan : in_channel -> unit
        (* [wait_inchan ic] suspends the execution of the calling thread
           until at least one character is available for reading on the
           input channel [ic]. The other program threads continue to run
           during this time. In contrast, calling an input function directly
           on [ic] would block all threads in the program until data is
           available on the channel. See the module [ThreadIO] for
           higher-level input functions compatible with threads. *)
val wait_descr : Unix.file_descr -> unit
        (* Similar to [wait_inchan], but operates on a file descriptor
           from the [Unix] library instead of an input channel. *)

(** Low-level thread synchronization primitives *)

(* The following primitives provide the basis for implementing 
   synchronization functions between threads. Their direct use is
   discouraged, as they are very low-level and prone to race conditions
   and deadlocks. The modules [Mutex], [Condition] and [Event]
   provide higher-level synchronization primitives. *)

val critical_section: bool ref
        (* Setting this reference to [true] deactivate thread preemption
           (the timer interrupt that transfers control from thread to thread),
           causing the current thread to run uninterrupted until
           [critical_section] is reset to [false] or the current thread
           explicitely relinquishes control using [sleep], [delay],
           [wait_inchan] or [wait_descr]. *)
val sleep : unit -> unit
        (* Suspend the calling thread until another thread reactivates it
           using [wakeup]. Just before suspending the thread,
           [critical_section] is reset to [false]. Resetting
           [critical_section] and suspending the calling thread is an
           atomic operation. *)
val wakeup : t -> unit
        (* Reactivate the thread whose identifier is given. This thread
           is assumed to be suspended on a call to [sleep], [delay],
           [wait_inchan] or [wait_descr]. After the call to [wakeup],
           the suspended thread will resume execution at some future time.
           [wakeup] does nothing if the thread was not suspended. *)
