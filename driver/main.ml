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

open Clflags

let process_file name =
  if Filename.check_suffix name ".ml"
  or Filename.check_suffix name ".mlt" then begin
    Compile.implementation name;
    objfiles := (Filename.chop_extension name ^ ".cmo") :: !objfiles
  end
  else if Filename.check_suffix name ".mli" then
    Compile.interface name
  else if Filename.check_suffix name ".cmo" 
       or Filename.check_suffix name ".cma" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".o"
       or Filename.check_suffix name ".a" then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ".c" then begin
    Compile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ".o")
    :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_number () =
  print_string "The Caml Special Light compiler, version ";
  print_string Config.version;
  print_newline()

let main () =
  try
    Arg.parse
      ["-I", Arg.String(fun dir -> include_dirs := dir :: !include_dirs);
       "-c", Arg.Set compile_only;
       "-o", Arg.String(fun s -> exec_name := s; archive_name := s);
       "-i", Arg.Set print_types;
       "-a", Arg.Set make_archive;
       "-unsafe", Arg.Set fast;
       "-nopervasives", Arg.Set nopervasives;
       "-custom", Arg.Set custom_runtime;
       "-ccopt", Arg.String(fun s -> ccopts := s :: !ccopts);
       "-cclib", Arg.String(fun s -> ccobjs := s :: !ccobjs);
       "-linkall", Arg.Set link_everything;
       "-drawlambda", Arg.Set dump_rawlambda;
       "-dlambda", Arg.Set dump_lambda;
       "-dinstr", Arg.Set dump_instr;
       "-v", Arg.Unit print_version_number;
       "-", Arg.String process_file]
      process_file;
    if !make_archive then begin
      Compile.init_path();
      Bytelibrarian.create_archive (List.rev !objfiles) !archive_name
    end
    else if not !compile_only & !objfiles <> [] then begin
      Compile.init_path();
      Bytelink.link (List.rev !objfiles)
    end;
    exit 0
  with x ->
    Format.set_formatter_output stderr;
    Errors.report_error x;
    exit 2

let _ = Printexc.catch main ()
