(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*         Francois Rouaix, Francois Pessaux and Jun Furuse              *)
(*               projet Cristal, INRIA Rocquencourt                      *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open Tables
open Printer
open Compile
open Intf

let flag_verbose = ref false
let verbose_string s = 
  if !flag_verbose then prerr_string s
let verbose_endline s =
  if !flag_verbose then prerr_endline s

let input_name = ref "Widgets.src"
let output_dir = ref "lib"
let destfile f = Filename.concat !output_dir f

let usage () = 
  prerr_string "Usage: tkcompiler input.src\n";
  flush stderr;
  exit 1


let prerr_error_header () =
  prerr_string "File \""; prerr_string !input_name;
  prerr_string "\", line ";
  prerr_string (string_of_int !Lexer.current_line);
  prerr_string ": "


let parse_file filename =
  let ic = open_in_bin filename in
  try
    let lexbuf = Lexing.from_channel ic in
      while true do
       Parser.entry Lexer.main lexbuf
      done
  with
    Parsing.Parse_error ->
      close_in ic;
      prerr_error_header();
      prerr_string "Syntax error \n";
      exit 1
  | Lexer.Lexical_error s ->
      close_in ic;
      prerr_error_header();
      prerr_string "Lexical error (";
      prerr_string s;
      prerr_string ")\n";
      exit 1
  | Duplicate_Definition (s,s') ->
      close_in ic;
      prerr_error_header();
      prerr_string s; prerr_string " "; prerr_string s';
      prerr_string " is defined twice.\n";
      exit 1
  | Compiler_Error s ->
      close_in ic;
      prerr_error_header();
      prerr_string "Internal error: "; prerr_string s; prerr_string "\n";
      prerr_string "Please report bug\n";
      exit 1
  | End_of_file ->
      close_in ic

(* The hack to provoke the production of cCAMLtoTKoptions_constrs *)

(* Auxiliary function: the list of all the elements associated to keys
   in an hash table. *)
let elements t =
 let elems = ref [] in
 Hashtbl.iter fun:(fun key:_ data:d -> elems := d :: !elems) t;
 !elems;;

(* Verifies that duplicated clauses are semantically equivalent and
   returns a unique set of clauses. *)
let uniq_clauses = function
  | [] -> []
  | l ->
     let check_constr constr1 constr2 =
       if constr1.template <> constr2.template then
       begin
        let code1, vars11, vars12, opts1 = 
         code_of_template context_widget:"dummy" constr1.template in
        let code2, vars12, vars22, opts2 = 
         code_of_template context_widget:"dummy" constr2.template in
        let err =
         Printf.sprintf
          "uncompatible redondant clauses for variant %s:\n %s\n and\n %s"
          constr1.var_name code1 code2 in
        Format.print_newline();
        print_fullcomponent constr1;
        Format.print_newline();
        print_fullcomponent constr2;
        Format.print_newline();
        prerr_endline err;
        fatal_error err
       end in
     let t = Hashtbl.create size:11 in
     List.iter l
      fun:(fun constr ->
       let c = constr.var_name in
       if Hashtbl.mem t key:c
       then (check_constr constr (Hashtbl.find t key:c))
       else Hashtbl.add t key:c data:constr);
     elements t;;

let option_hack oc =
  if Hashtbl.mem types_table key:"options" then
   let typdef = Hashtbl.find types_table key:"options" in
   let hack =
   { parser_arity = OneToken;
     constructors = 
      begin
       let constrs = 
        List.map typdef.constructors fun:
         begin fun c -> 
         { component = Constructor;
           ml_name = c.ml_name;
           var_name = c.var_name; (* as variants *)
           template = 
             begin match c.template with
               ListArg (x :: _) -> x
             | _ -> fatal_error "bogus hack"
             end;
           result = UserDefined "options_constrs";
           safe = true }
         end in
       uniq_clauses constrs
      end;
     subtypes = [];
     requires_widget_context = false; 
     variant = false }
   in
   write_CAMLtoTK
     w:(output_string to:oc) def:hack safetype:false "options_constrs"

let compile () = 
  verbose_endline "Creating tkgen.ml ...";
  let oc = open_out_bin (destfile "tkgen.ml") in
  let oc' = open_out_bin (destfile "tkigen.ml") in
  let oc'' = open_out_bin (destfile "tkfgen.ml") in
  let sorted_types = Tsort.sort types_order in
  verbose_endline "  writing types ...";
  List.iter sorted_types fun:
  begin fun typname ->
  verbose_string ("    " ^ typname ^ " ");
  try
    let typdef = Hashtbl.find types_table key:typname in
    verbose_string "type ";
    write_type intf:(output_string to:oc)
               impl:(output_string to:oc')
               typname def:typdef;
    verbose_string "C2T ";
    write_CAMLtoTK w:(output_string to:oc') typname def:typdef;
    verbose_string "T2C ";
    if List.mem item:typname !types_returned then
    write_TKtoCAML w:(output_string to:oc') typname def:typdef;
    verbose_string "CO ";
    write_catch_optionals w:(output_string to:oc') typname def:typdef;
    verbose_endline "."
  with Not_found -> 
    if not (List.mem_assoc key:typname !types_external) then
    begin
      verbose_string "Type ";
      verbose_string typname;
      verbose_string " is undeclared external or undefined\n"
    end
    else verbose_endline "."
  end;
  verbose_endline "  option hacking ...";
  option_hack oc';
  verbose_endline "  writing functions ...";
  List.iter fun:(write_function w:(output_string to:oc'')) !function_table;
  close_out oc;
  close_out oc';
  close_out oc'';
  (* Write the interface for public functions *)
  (* this interface is used only for documentation *)
  verbose_endline "Creating tkgen.mli ...";
  let oc = open_out_bin (destfile "tkgen.mli") in
  List.iter (sort_components !function_table)
    fun:(write_function_type w:(output_string to:oc));
  close_out oc;
  verbose_endline "Creating other ml, mli ...";
  Hashtbl.iter module_table fun:
  begin fun key:wname data:wdef ->
    verbose_endline ("  "^wname);
    let modname = wname in
    let oc = open_out_bin (destfile (modname ^ ".ml")) 
    and oc' = open_out_bin (destfile (modname ^ ".mli")) in 
    begin match wdef.module_type with
      Widget -> output_string to:oc' ("(* The "^wname^" widget *)\n")
    | Family -> output_string to:oc' ("(* The "^wname^" commands  *)\n")
    end;
    output_string to:oc "open Protocol\n";
    List.iter fun:(fun s -> output_string s to:oc; output_string s to:oc')
    [ "open Tk\n";
      "open Tkintf\n";
      "open Widget\n";
      "open Textvariable\n"
    ];
    begin match wdef.module_type with
      Widget ->
        write_create w:(output_string to:oc) wname;
        write_create_p w:(output_string to:oc') wname
    | Family -> ()
    end;
    List.iter fun:(write_function w:(output_string to:oc)) 
          (sort_components wdef.commands);
    List.iter fun:(write_function_type w:(output_string to:oc'))
          (sort_components wdef.commands);
    List.iter fun:(write_external w:(output_string to:oc)) 
           (sort_components wdef.externals);
    List.iter fun:(write_external_type w:(output_string to:oc'))
           (sort_components wdef.externals);
    close_out oc;
    close_out oc'
  end;
  (* write the module list for the Makefile *)
  (* and hack to death until it works *)
  let oc = open_out_bin (destfile "modules") in
    output_string to:oc "WIDGETOBJS=";
    Hashtbl.iter module_table
    fun:(fun key:name data:_ ->
         output_string to:oc name;
         output_string to:oc ".cmo ");
    output_string to:oc "\n";
    Hashtbl.iter module_table
    fun:(fun key:name data:_ ->
         output_string to:oc name;
         output_string to:oc ".ml ");
    output_string to:oc ": tkgen.ml\n\n";
    Hashtbl.iter module_table fun:
      begin fun key:name data:_ ->
        output_string to:oc name;
        output_string to:oc ".cmo : ";
        output_string to:oc name;
        output_string to:oc ".ml\n";
        output_string to:oc name;
        output_string to:oc ".cmi : ";
        output_string to:oc name;
        output_string to:oc ".mli\n"
      end;
    close_out oc

let main () =
  Arg.parse
    keywords:[ "-verbose",  Arg.Unit (fun () -> flag_verbose := true),
               "Make output verbose" ]
    others:(fun filename -> input_name := filename)
    errmsg:"Usage: tkcompiler <source file>" ;
  try
    verbose_string "Parsing... ";
    parse_file !input_name;
    verbose_string "Compiling... ";
    compile ();
    verbose_string "Finished";
    exit 0
  with
  | Lexer.Lexical_error s ->
      prerr_string "Invalid lexical character: ";
      prerr_endline s;
      exit 1
  | Duplicate_Definition (s, s') ->
      prerr_string s; prerr_string " "; prerr_string s';
      prerr_endline " is redefined illegally";
      exit 1
  | Invalid_implicit_constructor c ->
      prerr_string "Constructor ";
      prerr_string c;
      prerr_endline " is used implicitly before defined";
      exit 1
  | Tsort.Cyclic ->
      prerr_endline "Cyclic dependency of types";
      exit 1

let () = Printexc.catch main ()
