(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Printf

let compargs = ref ([] : string list)
let profargs = ref ([] : string list)
let toremove = ref ([] : string list)

let option opt () = compargs := opt :: !compargs
let option_with_arg opt arg = compargs := arg :: opt :: !compargs
let process_file filename = compargs := filename :: !compargs

let make_archive = ref false

let usage = "Usage: ocamlcp <options> <files>\noptions are:"

let incompatible o =
  fprintf stderr "ocamlcp: profiling is incompatible with the %s option\n" o;
  exit 2

let ismultithreaded = ref ""

module Options = Main_args.Make_options (struct
  let _a () = make_archive := true; option "-a" ()
  let _c = option "-c"
  let _cc s = option_with_arg "-cc" s
  let _cclib s = option_with_arg "-cclib" s
  let _ccopt s = option_with_arg "-ccopt" s
  let _custom = option "-custom"
  let _g = option "-g"
  let _i = option "-i"
  let _I s = option_with_arg "-I" s
  let _impl s = option_with_arg "-impl" s
  let _intf s = option_with_arg "-intf" s
  let _intf_suffix s = option_with_arg "-intf-suffix" s
  let _label = option "-label"
  let _linkall = option "-linkall"
  let _make_runtime = option "-make-runtime"
  let _noassert = option "-noassert"
  let _noautolink = option "-noautolink"
  let _o s = option_with_arg "-o" s
  let _output_obj = option "-output-obj"
  let _pp s = incompatible "-pp"
  let _rectypes = option "-rectypes"
  let _thread () = ismultithreaded := "-thread"; option "-thread" ()
  let _unsafe = option "-unsafe"
  let _use_prims s = option_with_arg "-use-prims" s
  let _use_runtime s = option_with_arg "-use-runtime" s
  let _v = option "-v"
  let _verbose = option "-verbose"
  let _w = option_with_arg "-w"
  let _nopervasives = option "-nopervasives"
  let _dparsetree = option "-dparsetree"
  let _drawlambda = option "-drawlambda"
  let _dlambda = option "-dlambda"
  let _dinstr = option "-dinstr"
  let anonymous = process_file
end)

let _ =
  let optlist = Options.list @ [
       "-p", Arg.String(fun s -> profargs := s :: "-m" :: !profargs),
             "[afilmt]  Profile constructs specified by argument:\n\
          \032     a  Everything\n\
          \032     f  Function calls and method calls\n\
          \032     i  if ... then ... else\n\
          \032     l  while, for\n\
          \032     m  match ... with\n\
          \032     t  try ... with"
    ]
  in
  Arg.parse optlist process_file usage;
  let status =
    Sys.command
      (Printf.sprintf "ocamlc -pp \"ocamlprof %s -instrument %s\" %s %s"
          !ismultithreaded
          (String.concat " " (List.rev !profargs))
          (if !make_archive then "" else "profiling.cmo")
          (String.concat " " (List.rev !compargs))) in
  exit status
