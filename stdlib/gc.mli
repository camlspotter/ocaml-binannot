(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Gc]:
   memory management control and statistics; finalised values
*)

type stat = {
  minor_words : int;
  promoted_words : int;
  major_words : int;
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
}
  (* The memory management counters are returned in a [stat] record.
     The fields of this record are:
-     [minor_words]  Number of words allocated in the minor heap since
             the program was started.
-     [promoted_words] Number of words allocated in the minor heap that
             survived a minor collection and were moved to the major heap
             since the program was started.
-     [major_words]  Number of words allocated in the major heap, including
             the promoted words, since the program was started.
-     [minor_collections]  Number of minor collections since the program
             was started.
-     [major_collections]  Number of major collection cycles, not counting
             the current cycle, since the program was started.
-     [heap_words]  Total size of the major heap, in words.
-     [heap_chunks]  Number of times the major heap size was increased
             since the program was started (including the initial allocation
             of the heap).
-     [live_words]  Number of words of live data in the major heap, including
             the header words.
-     [live_blocks]  Number of live blocks in the major heap.
-     [free_words]  Number of words in the free list.
-     [free_blocks]  Number of blocks in the free list.
-     [largest_free]  Size (in words) of the largest block in the free list.
-     [fragments]  Number of wasted words due to fragmentation.  These are
             1-words free blocks placed between two live blocks.  They
             cannot be inserted in the free list, thus they are not available
             for allocation.
-     [compactions]  Number of heap compactions since the program was started.

     The total amount of memory allocated by the program since it was started
     is (in words) [minor_words + major_words - promoted_words].  Multiply by
     the word size (4 on a 32-bit machine, 8 on a 64-bit machine) to get
     the number of bytes.
  *)

type control = {
  mutable minor_heap_size : int;
  mutable major_heap_increment : int;
  mutable space_overhead : int;
  mutable verbose : int;
  mutable max_overhead : int;
  mutable stack_limit : int
}

  (* The GC parameters are given as a [control] record.  The fields are:
-     [minor_heap_size]  The size (in words) of the minor heap.  Changing
             this parameter will trigger a minor collection.  Default: 32k.
-     [major_heap_increment]  The minimum number of words to add to the
             major heap when increasing it.  Default: 62k.
-     [space_overhead]  The major GC speed is computed from this parameter.
             This is the memory that will be "wasted" because the GC does not
             immediatly collect unreachable blocks.  It is expressed as a
             percentage of the memory used for live data.
             The GC will work more (use more CPU time and collect
             blocks more eagerly) if [space_overhead] is smaller.
             The computation of the GC speed assumes that the amount
             of live data is constant.  Default: 42.
-     [max_overhead]  Heap compaction is triggered when the estimated amount
             of free memory is more than [max_overhead] percent of the amount
             of live data.  If [max_overhead] is set to 0, heap
             compaction is triggered at the end of each major GC cycle
             (this setting is intended for testing purposes only).
             If [max_overhead >= 1000000], compaction is never triggered.
             Default: 1000000.
-     [verbose]  This value controls the GC messages on standard error output.
             It is a sum of some of the following flags, to print messages
             on the corresponding events:
-            [0x01] Start of major GC cycle.
-            [0x02] Minor collection and major GC slice.
-            [0x04] Growing and shrinking of the heap.
-            [0x08] Resizing of stacks and memory manager tables.
-            [0x10] Heap compaction.
-            [0x20] Change of GC parameters.
-            [0x40] Computation of major GC slice size.
-            [0x80] Calling of finalisation functions.
             Default: 0.
-     [stack_limit]  The maximum size of the stack (in words).  This is only
             relevant to the byte-code runtime, as the native code runtime
             uses the operating system's stack.  Default: 256k.
  *)

external stat : unit -> stat = "gc_stat"
  (* Return the current values of the memory management counters in a
     [stat] record. *)
external counters : unit -> (int * int * int) = "gc_counters"
  (* Return [(minor_words, promoted_words, major_words)].  Much faster
     than [stat]. *)
external get : unit -> control = "gc_get"
  (* Return the current values of the GC parameters in a [control] record. *)
external set : control -> unit = "gc_set"
  (* [set r] changes the GC parameters according to the [control] record [r].
     The normal usage is:
-    [Gc.set { (Gc.get()) with Gc.verbose = 13 }] *)
external minor : unit -> unit = "gc_minor"
  (* Trigger a minor collection. *)
external major : unit -> unit = "gc_major"
  (* Finish the current major collection cycle. *)
external full_major : unit -> unit = "gc_full_major"
  (* Finish the current major collection cycle and perform a complete
     new cycle.  This will collect all currently unreachable blocks. *)
external compact : unit -> unit = "gc_compaction";;
  (* Perform a full major collection and compact the heap.  Note that heap
     compaction is a lengthy operation. *)

val print_stat : out_channel -> unit
  (* Print the current values of the memory management counters (in
     human-readable form) into the channel argument. *)

val allocated_bytes : unit -> int
  (* Return the total number of bytes allocated since the program was
     started.  Warning: on 32-bit machines, this counter can easily
     overflow and roll over. *)


val finalise : ('a -> unit) -> 'a -> unit;;
  (* [Gc.finalise f v] registers [f] as a finalisation function for [v].
     [v] must be heap-allocated.  [f] will be called with [v] as
     argument at some point between the first time [v] becomes unreachable
     and the time [v] is collected by the GC.  Several functions can
     be registered for the same value, or even several instances of the
     same function.  Each instance will be called once (or never,
     if the program terminates before the GC deallocates [v]).
     
     A number of pitfalls are associated with finalised values:
     finalisation functions are called asynchronously, sometimes
     even during the execution of other finalisation functions.
     In a multithreaded program, finalisation functions are called
     from any thread, thus they cannot not acquire any mutex.

     Anything reachable from the closure of finalisation functions
     is considered reachable, so the following code will not work:
-    [ let v = ... in Gc.finalise (fun x -> ...) v ]
     Instead you should write:
-    [ let f = fun x -> ... ;; let v = ... in Gc.finalise f v ]
     
     The [f] function can use all features of O'Caml, including
     assignments that make the value reachable again (indeed, the value
     is already reachable from the stack during the execution of the
     function).  It can also loop forever (in this case, the other
     finalisation functions will be called during the execution of f).
     It can call [Gc.finalise] on [v] or other values to register other
     functions or even itself.  It can raise an exception; in this case
     the exception will interrupt whatever the program was doing when
     the function was called.
     
     [Gc.finalise] will raise [Invalid_argument "Gc.finalise"] if [v]
     is not heap-allocated.  Some examples of values that are not
     heap-allocated are integers, constant constructors, booleans,
     the empty array, the empty list, the unit value.  The exact list
     of what is heap-allocated or not is implementation-dependent.
     You should also be aware that some optimisations will duplicate
     some immutable values, especially floating-point numbers when
     stored into arrays, so they can be finalised and collected while
     another copy is still in use by the program.
  *)
