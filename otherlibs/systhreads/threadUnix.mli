(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [ThreadUnix]: thread-compatible system calls *)

(* This module reimplements some of the functions from [Unix]
   so that they only block the calling thread, not all threads
   in the program, if they cannot complete immediately.
   See the documentation of the [Unix] module for more
   precise descriptions of the functions below. *)

(*** Process handling *)

external execv : prog:string -> args:string array -> unit = "unix_execv"
external execve : prog:string -> args:string array -> env:string array -> unit
           = "unix_execve"
external execvp : prog:string -> args:string array -> unit = "unix_execvp"
val wait : unit -> int * Unix.process_status
val waitpid : flags:Unix.wait_flag list -> int -> int * Unix.process_status
val system : string -> Unix.process_status

(*** Basic input/output *)

val read : Unix.file_descr -> buffer:string -> pos:int -> len:int -> int
val write : Unix.file_descr -> buffer:string -> pos:int -> len:int -> int

(*** Polling *)

val select :
  read:Unix.file_descr list -> write:Unix.file_descr list ->
  exn:Unix.file_descr list -> timeout:float ->
        Unix.file_descr list * Unix.file_descr list * Unix.file_descr list

(*** Input/output with timeout *)

val timed_read :
      Unix.file_descr ->
      buffer:string -> pos:int -> len:int -> timeout:float -> int
val timed_write :
      Unix.file_descr ->
      buffer:string -> pos:int -> len:int -> timeout:float -> int
      (* Behave as [read] and [write], except that 
         [Unix_error(ETIMEDOUT,_,_)] is raised if no data is
         available for reading or ready for writing after [d] seconds.
         The delay [d] is given in the fifth argument, in seconds. *)

(*** Pipes and redirections *)

val pipe : unit -> Unix.file_descr * Unix.file_descr
val open_process_in: string -> in_channel
val open_process_out: string -> out_channel
val open_process: string -> in_channel * out_channel

(*** Time *)

external sleep : int -> unit = "unix_sleep"

(*** Sockets *)

val socket : domain:Unix.socket_domain ->
      	     type:Unix.socket_type -> proto:int -> Unix.file_descr
val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
external connect : Unix.file_descr -> Unix.sockaddr -> unit = "unix_connect"
val recv : Unix.file_descr -> buffer:string ->
      	   pos:int -> len:int -> flags:Unix.msg_flag list -> int
val recvfrom : Unix.file_descr -> buffer:string -> pos:int -> len:int ->
               flags:Unix.msg_flag list -> int * Unix.sockaddr
val send : Unix.file_descr -> buffer:string -> pos:int -> len:int ->
           flags:Unix.msg_flag list -> int
val sendto : Unix.file_descr -> buffer:string -> pos:int -> len:int ->
             flags:Unix.msg_flag list -> addr:Unix.sockaddr -> int
val open_connection : Unix.sockaddr -> in_channel * out_channel
