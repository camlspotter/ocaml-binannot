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

(* The interactive toplevel loop *)

open Lexing
open Formatmsg
open Config
open Misc
open Parsetree
open Types
open Typedtree
open Printval

type directive_fun =
    Directive_none of (unit -> unit)
  | Directive_string of (string -> unit)
  | Directive_int of (int -> unit)
  | Directive_ident of (Longident.t -> unit)
  | Directive_bool of (bool -> unit)

(* Hooks for parsing functions *)

let parse_toplevel_phrase = ref Parse.toplevel_phrase
let parse_use_file = ref Parse.use_file
let print_location = Location.print
let print_warning = Location.print_warning
let input_name = Location.input_name

(* Load in-core and execute a lambda term *)

type evaluation_outcome = Result of Obj.t | Exception of exn

let load_lambda lam =
  if !Clflags.dump_rawlambda then begin
    Printlambda.lambda lam; print_newline()
  end;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then begin
    Printlambda.lambda slam; print_newline()
  end;
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  if !Clflags.dump_instr then begin
    Printinstr.instrlist init_code;
    Printinstr.instrlist fun_code;
    print_newline()
  end;
  let (code, code_size, reloc) = Emitcode.to_memory init_code fun_code in
  let can_free = (fun_code = []) in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Symtable.update_global_table();
  try
    let retval = (Meta.reify_bytecode code code_size) () in
    if can_free then Meta.static_free code;
    Result retval
  with x ->
    if can_free then Meta.static_free code;
    Symtable.restore_state initial_symtable;
    Exception x

(* Print the outcome of an evaluation *)

let rec print_items env = function
    Tsig_value(id, decl)::rem ->
      open_box 2;
      Printtyp.value_description id decl;
      begin match decl.val_kind with
        Val_prim _ -> ()
      | _ ->
          print_string " ="; print_space();
          print_value env (Symtable.get_global_value id) decl.val_type
      end;
      close_box();
      print_space (); print_items env rem
  | Tsig_type(id, decl)::rem ->
      Printtyp.type_declaration id decl;
      print_space (); print_items env rem
  | Tsig_exception(id, decl)::rem ->
      Printtyp.exception_declaration id decl;
      print_space (); print_items env rem
  | Tsig_module(id, mty)::rem ->
      open_box 2; print_string "module "; Printtyp.ident id;
      print_string " :"; print_space(); Printtyp.modtype mty; close_box();
      print_space (); print_items env rem
  | Tsig_modtype(id, decl)::rem ->
      Printtyp.modtype_declaration id decl;
      print_space (); print_items env rem
  | Tsig_class(id, decl)::cltydecl::tydecl1::tydecl2::rem ->
      Printtyp.class_declaration id decl;
      print_space (); print_items env rem
  | Tsig_cltype(id, decl)::tydecl1::tydecl2::rem ->
      Printtyp.cltype_declaration id decl;
      print_space (); print_items env rem
  | _ ->
      ()

(* Print an exception produced by an evaluation *)

let print_exception_outcome = function
    Sys.Break ->
      print_string "Interrupted."; print_newline()
  | Out_of_memory ->
      Gc.full_major();
      print_string "Out of memory during evaluation.";
      print_newline()
  | Stack_overflow ->
      print_string "Stack overflow during evaluation (looping recursion?).";
      print_newline();
  | exn ->
      open_box 0;
      print_string "Uncaught exception: ";
      print_exception (Obj.repr exn);
      print_newline()

(* The table of toplevel directives. 
   Filled by functions from module topdirs. *)

let directive_table = (Hashtbl.create 13 : (string, directive_fun) Hashtbl.t)

(* Execute a toplevel phrase *)

let toplevel_env = ref Env.empty

let execute_phrase print_outcome phr =
  match phr with
    Ptop_def sstr ->
      let (str, sg, newenv) = Typemod.type_structure !toplevel_env sstr in
      let lam = Translmod.transl_toplevel_definition str in
      let res = load_lambda lam in
      begin match res with
        Result v ->
          if print_outcome then begin
            match str with
              [Tstr_eval exp] ->
                open_box 0;
                print_string "- : ";
                Printtyp.type_scheme exp.exp_type;
                print_space(); print_string "="; print_space();
                print_value newenv v exp.exp_type;
                close_box();
                print_newline()
            | _ ->
                open_vbox 0;
                print_items newenv sg;
                close_box();
                print_flush()
          end;
          toplevel_env := newenv;
          true
      | Exception exn ->
          print_exception_outcome exn;
          false
      end
  | Ptop_dir(dir_name, dir_arg) ->
      try
        match (Hashtbl.find directive_table dir_name, dir_arg) with
          (Directive_none f, Pdir_none) -> f (); true
        | (Directive_string f, Pdir_string s) -> f s; true
        | (Directive_int f, Pdir_int n) -> f n; true
        | (Directive_ident f, Pdir_ident lid) -> f lid; true
        | (Directive_bool f, Pdir_bool b) -> f b; true
        | (_, _) ->
            print_string "Wrong type of argument for directive `";
            print_string dir_name; print_string "'"; print_newline();
            false
      with Not_found ->
        print_string "Unknown directive `"; print_string dir_name;
        print_string "'"; print_newline();
        false

(* Temporary assignment to a reference *)

let protect r newval body =
  let oldval = !r in
  try
    r := newval; 
    let res = body() in
    r := oldval;
    res
  with x ->
    r := oldval;
    raise x

(* Read and execute commands from a file *)

let use_print_results = ref true

let use_file name =
  try
    let filename = find_in_path !Config.load_path name in
    let ic = open_in_bin filename in
    let lb = Lexing.from_channel ic in
    (* Skip initial #! line if any *)
    let buffer = String.create 2 in
    if input ic buffer 0 2 = 2 && buffer = "#!"
    then ignore(input_line ic)
    else seek_in ic 0;
    let success =
      protect Location.input_name filename (fun () ->
        try
          List.iter
            (fun ph ->
              if !Clflags.dump_parsetree then Printast.top_phrase ph;
              if execute_phrase !use_print_results ph then () else raise Exit)
            (!parse_use_file lb);
          true
        with
          Exit -> false
        | Sys.Break ->
            print_string "Interrupted."; print_newline(); false
        | x ->
            Errors.report_error x; false) in
    close_in ic;
    success
  with Not_found ->
    print_string "Cannot find file "; print_string name; print_newline();
    false

let use_silently name =
  protect use_print_results false (fun () -> use_file name)

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false;;

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    output_string stdout (if !first_line then "# " else "  "); flush stdout;
    first_line := false;
    let i = ref 0 in
    try
      while !i < len && (let c = input_char stdin in buffer.[!i] <- c; c<>'\n')
      do incr i done;
      !i + 1
    with End_of_file ->
      Location.echo_eof ();
      if !i > 0
      then (got_eof := true; !i)
      else 0
  end

(* Discard everything already in a lexer buffer *)

let empty_lexbuf lb =
  let l = String.length lb.lex_buffer in
  lb.lex_abs_pos <- (-l);
  lb.lex_curr_pos <- l

(* Toplevel initialization. Performed here instead of at the
   beginning of loop() so that user code linked in with ocamlmktop
   can call directives from Topdirs. *)

let _ =
  Sys.interactive := true;
  Symtable.init_toplevel();
  Clflags.thread_safe := true;
  Compile.init_path()

let load_ocamlinit () =
  if Sys.file_exists ".ocamlinit" then ignore(use_silently ".ocamlinit")

(* The interactive loop *)

exception PPerror

let loop() =
  print_string "        Objective Caml version ";
  print_string Config.version;
  print_newline(); print_newline();
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path);
  toplevel_env := Compile.initial_env();
  let lb = Lexing.from_function refill_lexbuf in
  Location.input_name := "";
  Location.input_lexbuf := Some lb;
  Sys.catch_break true;
  load_ocamlinit ();
  while true do
    try
      empty_lexbuf lb;
      Location.reset();
      first_line := true;
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      if !Clflags.dump_parsetree then Printast.top_phrase phr;
      ignore(execute_phrase true phr)
    with
      End_of_file -> exit 0
    | Sys.Break ->
        print_string "Interrupted."; print_newline()
    | PPerror -> ()
    | x ->
        Errors.report_error x
  done

(* Execute a script *)

let run_script name =
  Compile.init_path();
  toplevel_env := Compile.initial_env();
  Formatmsg.set_output Format.err_formatter;
  use_silently name
