(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Link a set of .cmo files and produce a bytecode executable. *)

open Sys
open Misc
open Config
open Instruct
open Emitcode

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Symbol_error of string * Symtable.error
  | Inconsistent_import of string * string * string
  | Custom_runtime
  | File_exists of string
  | Cannot_open_dll of string
  | Require_custom

exception Error of error

type link_action =
    Link_object of string * compilation_unit
      (* Name of .cmo file and descriptor of the unit *)
  | Link_archive of string * compilation_unit list
      (* Name of .cma file and descriptors of the units to be linked. *)

(* Add C objects and options from a library descriptor *)
(* Ignore them if -noautolink was given *)

let lib_ccobjs = ref []
let lib_ccopts = ref []

let add_ccobjs l =
  if not !Clflags.no_auto_link then begin
    if l.lib_custom then Clflags.custom_runtime := true;
    lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
    lib_ccopts := l.lib_ccopts @ !lib_ccopts
  end

(* A note on ccobj ordering:
   - Clflags.ccobjs is in reverse order w.r.t. what was given on the 
        ocamlc command line;
   - l.lib_ccobjs is also in reverse order w.r.t. what was given on the
        ocamlc -a command line when the library was created;
   - Clflags.ccobjs is reversed just before calling the C compiler for the
        custom link;
   - .cma files on the command line of ocamlc are scanned right to left;
   - Before linking, we add lib_ccobjs after Clflags.ccobjs.
   Thus, for ocamlc a.cma b.cma obj1 obj2
   where a.cma was built with ocamlc -i ... obja1 obja2
     and b.cma was built with ocamlc -i ... objb1 objb2
   lib_ccobjs starts as [],
   becomes objb2 objb1 when b.cma is scanned,
   then obja2 obja1 objb2 objb1 when a.cma is scanned.
   Clflags.ccobjs was initially obj2 obj1.
   and is set to obj2 obj1 obja2 obja1 objb2 objb1.
   Finally, the C compiler is given objb1 objb2 obja1 obja2 obj1 obj2,
   which is what we need.  (If b depends on a, a.cma must appear before
   b.cma, but b's C libraries must appear before a's C libraries.)
*)

(* First pass: determine which units are needed *)

module IdentSet =
  Set.Make(struct
    type t = Ident.t
    let compare = compare
  end)

let missing_globals = ref IdentSet.empty

let is_required (rel, pos) =
  match rel with
    Reloc_setglobal id ->
      IdentSet.mem id !missing_globals
  | _ -> false

let add_required (rel, pos) =
  match rel with
    Reloc_getglobal id ->
      missing_globals := IdentSet.add id !missing_globals
  | _ -> ()

let remove_required (rel, pos) =
  match rel with
    Reloc_setglobal id ->
      missing_globals := IdentSet.remove id !missing_globals
  | _ -> ()

let scan_file obj_name tolink =
  let file_name =
    try
      find_in_path !load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  let ic = open_in_bin file_name in
  try
    let buffer = String.create (String.length cmo_magic_number) in
    really_input ic buffer 0 (String.length cmo_magic_number);
    if buffer = cmo_magic_number then begin
      (* This is a .cmo file. It must be linked in any case.
         Read the relocation information to see which modules it
         requires. *)
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let compunit = (input_value ic : compilation_unit) in
      close_in ic;
      List.iter add_required compunit.cu_reloc;
      Link_object(file_name, compunit) :: tolink
    end
    else if buffer = cma_magic_number then begin
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      let pos_toc = input_binary_int ic in    (* Go to table of contents *)
      seek_in ic pos_toc;
      let toc = (input_value ic : library) in
      close_in ic;
      add_ccobjs toc;
      let required =
        List.fold_right
          (fun compunit reqd ->
            if compunit.cu_force_link
            || !Clflags.link_everything
            || List.exists is_required compunit.cu_reloc
            then begin
              List.iter remove_required compunit.cu_reloc;
              List.iter add_required compunit.cu_reloc;
              compunit :: reqd
            end else
              reqd)
          toc.lib_units [] in
      Link_archive(file_name, required) :: tolink
    end
    else raise(Error(Not_an_object_file file_name))
  with
    End_of_file -> close_in ic; raise(Error(Not_an_object_file file_name))
  | x -> close_in ic; raise x

(* Second pass: link in the required units *)

(* Consistency check between interfaces *)

let crc_interfaces =
  (Hashtbl.create 17 : (string, string * Digest.t) Hashtbl.t)

let check_consistency file_name cu =
  List.iter
    (fun (name, crc) ->
      if name = cu.cu_name then begin
        Hashtbl.add crc_interfaces name (file_name, crc)
      end else begin
        try
          let (auth_name, auth_crc) = Hashtbl.find crc_interfaces name in
          if crc <> auth_crc then
            raise(Error(Inconsistent_import(name, file_name, auth_name)))
        with Not_found ->
          (* Can only happen for unit for which only a .cmi file was used,
             but no .cmo is provided *)
          Hashtbl.add crc_interfaces name (file_name, crc)
      end)
    cu.cu_imports

(* Record compilation events *)

let debug_info = ref ([] : (int * string) list)

(* Link in a compilation unit *)

let link_compunit output_fun currpos_fun inchan file_name compunit =
  check_consistency file_name compunit;
  seek_in inchan compunit.cu_pos;
  let code_block = String.create compunit.cu_codesize in
  really_input inchan code_block 0 compunit.cu_codesize;
  Symtable.patch_object code_block compunit.cu_reloc;
  if !Clflags.debug && compunit.cu_debug > 0 then begin
    seek_in inchan compunit.cu_debug;
    let buffer = String.create compunit.cu_debugsize in
    really_input inchan buffer 0 compunit.cu_debugsize;
    debug_info := (currpos_fun(), buffer) :: !debug_info
  end;
  output_fun code_block;
  if !Clflags.link_everything then
    List.iter Symtable.require_primitive compunit.cu_primitives

(* Link in a .cmo file *)

let link_object output_fun currpos_fun file_name compunit =
  let inchan = open_in_bin file_name in
  try
    link_compunit output_fun currpos_fun inchan file_name compunit;
    close_in inchan
  with
    Symtable.Error msg ->
      close_in inchan; raise(Error(Symbol_error(file_name, msg)))
  | x ->
      close_in inchan; raise x

(* Link in a .cma file *)

let link_archive output_fun currpos_fun file_name units_required =
  let inchan = open_in_bin file_name in
  try
    List.iter
      (fun cu ->
         let name = file_name ^ "(" ^ cu.cu_name ^ ")" in
         try
           link_compunit output_fun currpos_fun inchan name cu
         with Symtable.Error msg ->
           raise(Error(Symbol_error(name, msg))))
      units_required;
    close_in inchan
  with x -> close_in inchan; raise x

(* Link in a .cmo or .cma file *)

let link_file output_fun currpos_fun = function
    Link_object(file_name, unit) ->
      link_object output_fun currpos_fun file_name unit
  | Link_archive(file_name, units) ->
      link_archive output_fun currpos_fun file_name units

(* Output the debugging information *)
(* Format is:
      <int32>          number of event lists
      <int32>          offset of first event list
      <output_value>   first event list
      ...
      <int32>          offset of last event list
      <output_value>   last event list *)

let output_debug_info oc =
  output_binary_int oc (List.length !debug_info);
  List.iter
    (fun (ofs, evl) -> output_binary_int oc ofs; output_string oc evl)
    !debug_info;
  debug_info := []

(* Output a list of strings with 0-termination *)

let output_stringlist oc l =
  List.iter (fun s -> output_string oc s; output_byte oc 0) l

(* Create a bytecode executable file *)

let link_bytecode tolink exec_name standalone =
  if Sys.os_type = "MacOS" then begin
    (* Create it as a text file for bytecode scripts *)
    let c = open_out_gen [Open_wronly; Open_creat] 0o777 exec_name in
    close_out c
  end;
  let outchan = open_out_gen [Open_wronly; Open_trunc; Open_creat; Open_binary]
                             0o777 exec_name in
  try
    if standalone then begin
      (* Copy the header *)
      try
        let inchan = open_in_bin (find_in_path !load_path "camlheader") in
        copy_file inchan outchan;
        close_in inchan
      with Not_found | Sys_error _ -> ()
    end;
    Bytesections.init_record outchan;
    (* The bytecode *)
    let start_code = pos_out outchan in
    Symtable.init();
    Hashtbl.clear crc_interfaces;
    let sharedobjs = Dll.extract_dll_names !Clflags.ccobjs in
    if standalone then begin
      (* Initialize the DLL machinery *)
      if List.length sharedobjs < List.length !Clflags.ccobjs
      then raise (Error Require_custom);
      Dll.add_path !load_path;
      try Dll.open_dlls sharedobjs
      with Failure reason -> raise(Error(Cannot_open_dll reason))
    end;
    let output_fun = output_string outchan
    and currpos_fun () = pos_out outchan - start_code in
    List.iter (link_file output_fun currpos_fun) tolink;
    if standalone then Dll.close_all_dlls();
    (* The final STOP instruction *)
    output_byte outchan Opcodes.opSTOP;
    output_byte outchan 0; output_byte outchan 0; output_byte outchan 0;
    Bytesections.record outchan "CODE";
    (* DLL stuff *)
    if standalone then begin
      (* The extra search path for DLLs *)
      output_stringlist outchan !Clflags.dllpaths;
      Bytesections.record outchan "DLPT";
      (* The names of the DLLs *)
      output_stringlist outchan sharedobjs;
      Bytesections.record outchan "DLLS"
    end;
    (* The names of all primitives *)
    Symtable.output_primitive_names outchan;
    Bytesections.record outchan "PRIM";
    (* The table of global data *)
    output_value outchan (Symtable.initial_global_table());
    Bytesections.record outchan "DATA";
    (* The map of global identifiers *)
    Symtable.output_global_map outchan;
    Bytesections.record outchan "SYMB";
    (* Debug info *)
    if !Clflags.debug then begin
      output_debug_info outchan;
      Bytesections.record outchan "DBUG"
    end;
    (* The table of contents and the trailer *)
    Bytesections.write_toc_and_trailer outchan;
    close_out outchan
  with x ->
    close_out outchan;
    remove_file exec_name;
    raise x

(* Output a string as a C array of unsigned ints *)

let output_code_string_counter = ref 0

let output_code_string outchan code =
  let pos = ref 0 in
  let len = String.length code in
  while !pos < len do
    let c1 = Char.code(code.[!pos]) in
    let c2 = Char.code(code.[!pos + 1]) in
    let c3 = Char.code(code.[!pos + 2]) in
    let c4 = Char.code(code.[!pos + 3]) in
    pos := !pos + 4;
    Printf.fprintf outchan "0x%02x%02x%02x%02x, " c4 c3 c2 c1;
    incr output_code_string_counter;
    if !output_code_string_counter >= 6 then begin
      output_char outchan '\n';
      output_code_string_counter := 0
    end
  done

(* Output a string as a C string *)

let output_data_string outchan data =
  let counter = ref 0 in
  for i = 0 to String.length data - 1 do
    Printf.fprintf outchan "%d, " (Char.code(data.[i]));
    incr counter;
    if !counter >= 12 then begin
      output_string outchan "\n";
      counter := 0
    end
  done

(* Output a bytecode executable as a C file *)

let link_bytecode_as_c tolink outfile =
  let outchan = open_out outfile in
  try
    (* The bytecode *)
    output_string outchan "static int caml_code[] = {\n";
    Symtable.init();
    Hashtbl.clear crc_interfaces;
    let output_fun = output_code_string outchan
    and currpos_fun () = 0 in
    List.iter (link_file output_fun currpos_fun) tolink;
    (* The final STOP instruction *)
    Printf.fprintf outchan "\n0x%x};\n\n" Opcodes.opSTOP;
    (* The table of global data *)
    output_string outchan "static char caml_data[] = {\n";
    output_data_string outchan
      (Marshal.to_string (Symtable.initial_global_table()) []);
    Printf.fprintf outchan "\n};\n\n";
    (* The table of primitives *)
    Symtable.output_primitive_table outchan;
    (* The entry point *)
    output_string outchan "\n
void caml_startup(argv)
        char ** argv;
{
  caml_startup_code(caml_code, sizeof(caml_code), caml_data, argv);
}\n";
    close_out outchan
  with x ->
    close_out outchan;
    raise x

(* Build a custom runtime *)

let rec extract suffix l =
  match l with
  | [] -> []
  | h::t when Filename.check_suffix h suffix -> h :: (extract suffix t)
  | h::t -> extract suffix t
;;

let build_custom_runtime prim_name exec_name =
  match Sys.os_type with
    "Unix" | "Cygwin" ->
      Ccomp.command
       (Printf.sprintf
          "%s -o %s -I%s %s %s %s %s %s -lcamlrun %s"
          !Clflags.c_linker
          exec_name
          Config.standard_library
          (String.concat " " (List.rev !Clflags.ccopts))
          prim_name
          (String.concat " "
            (List.map (fun dir -> if dir = "" then "" else "-L" ^ dir)
                      !load_path))
          (String.concat " "
            (List.map (fun dir -> if dir = "" then "" else
                                  Config.bytecomp_c_rpath ^ dir)
                      (!Clflags.dllpaths @
                       Dll.ld_library_path_contents() @
                       Dll.ld_conf_contents())))
          (String.concat " " (List.rev !Clflags.ccobjs))
          Config.bytecomp_c_libraries)
  | "Win32" ->
      let retcode =
      Ccomp.command
       (Printf.sprintf
          "%s /Fe%s -I%s %s %s %s %s %s"
          !Clflags.c_linker
          exec_name
          Config.standard_library
          (String.concat " " (List.rev !Clflags.ccopts))
          prim_name
          (String.concat " "
                         (List.rev_map Ccomp.expand_libname !Clflags.ccobjs))
          (Ccomp.expand_libname "-lcamlrun")
          Config.bytecomp_c_libraries) in
      (* C compiler doesn't clean up after itself *)
      remove_file (Filename.chop_suffix prim_name ".c" ^ ".obj");
      retcode
  | "MacOS" ->
      let cppc = "mrc"
      and libsppc = "\"{sharedlibraries}MathLib\" \
                     \"{ppclibraries}PPCCRuntime.o\" \
                     \"{ppclibraries}PPCToolLibs.o\" \
                     \"{sharedlibraries}StdCLib\" \
                     \"{ppclibraries}StdCRuntime.o\" \
                     \"{sharedlibraries}InterfaceLib\""
      and linkppc = "ppclink -d"
      and objsppc = extract ".x" (List.rev !Clflags.ccobjs)
      and q_prim_name = Filename.quote prim_name
      and q_stdlib = Filename.quote Config.standard_library
      and q_exec_name = Filename.quote exec_name
      in
      Ccomp.run_command (Printf.sprintf "%s -i %s %s %s -o %s.x"
        cppc
        q_stdlib
        (String.concat " " (List.rev_map Filename.quote !Clflags.ccopts))
        q_prim_name
        q_prim_name);
      Ccomp.run_command ("delete -i " ^ q_exec_name);
      Ccomp.command (Printf.sprintf
        "%s -t MPST -c 'MPS ' -o %s %s.x %s %s %s"
        linkppc
        q_exec_name
        q_prim_name
        (String.concat " " (List.map Filename.quote objsppc))
        (Filename.quote
            (Filename.concat Config.standard_library "libcamlrun.x"))
        libsppc)
  | _ -> assert false

let append_bytecode_and_cleanup bytecode_name exec_name prim_name =
  let oc = open_out_gen [Open_wronly; Open_append; Open_binary] 0 exec_name in
  let ic = open_in_bin bytecode_name in
  copy_file ic oc;
  close_in ic;
  close_out oc;
  remove_file bytecode_name;
  remove_file prim_name

(* Fix the name of the output file, if the C compiler changes it behind
   our back. *)

let fix_exec_name name =
  match Sys.os_type with
    "Win32" | "Cygwin" ->
      if String.contains name '.' then name else name ^ ".exe"
  | _ -> name

(* Main entry point (build a custom runtime if needed) *)

let link objfiles =
  let objfiles = if !Clflags.nopervasives then objfiles
                 else "stdlib.cma" :: (objfiles @ ["std_exit.cmo"]) in
  let tolink = List.fold_right scan_file objfiles [] in
  Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
  Clflags.ccopts := !lib_ccopts @ !Clflags.ccopts; (* put user's opts first *)
  if not !Clflags.custom_runtime then
    link_bytecode tolink !Clflags.exec_name true
  else if not !Clflags.output_c_object then begin
    let bytecode_name = Filename.temp_file "camlcode" "" in
    let prim_name = Filename.temp_file "camlprim" ".c" in
    try
      link_bytecode tolink bytecode_name false;
      let poc = open_out prim_name in
      Symtable.output_primitive_table poc;
      close_out poc;
      let exec_name = fix_exec_name !Clflags.exec_name in
      if build_custom_runtime prim_name exec_name <> 0
      then raise(Error Custom_runtime);
      if !Clflags.make_runtime
      then (remove_file bytecode_name; remove_file prim_name)
      else append_bytecode_and_cleanup bytecode_name exec_name prim_name
    with x ->
      remove_file bytecode_name;
      remove_file prim_name;
      raise x
  end else begin
    let c_file =
      Filename.chop_suffix !Clflags.object_name Config.ext_obj ^ ".c" in
    if Sys.file_exists c_file then raise(Error(File_exists c_file));
    try
      link_bytecode_as_c tolink c_file;
      if Ccomp.compile_file c_file <> 0
      then raise(Error Custom_runtime);
      remove_file c_file
    with x ->
      remove_file c_file;
      remove_file !Clflags.object_name;
      raise x
  end

(* Error report *)

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
      fprintf ppf "The file %s is not a bytecode object file" name
  | Symbol_error(name, err) ->
      fprintf ppf "Error while linking %s:@ %a" name
      Symtable.report_error err
  | Inconsistent_import(intf, file1, file2) ->
      fprintf ppf
        "@[<hv 0>Files %s and %s@ \
                 make inconsistent assumptions over interface %s@]"
        file1 file2 intf
  | Custom_runtime ->
      fprintf ppf "Error while building custom runtime system"
  | File_exists file ->
      fprintf ppf "Cannot overwrite existing file %s" file
  | Cannot_open_dll file ->
      fprintf ppf "Error on dynamically loaded library: %s" file
  | Require_custom ->
      fprintf ppf "Linking with non-Caml, non-shared object files requires the -custom flag"
