(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Loading and installation of user-defined printer functions *)

open Misc
open Debugger_config
open Path
open Types

(* Error report *)

type error =
  | Load_failure of Dynlink.error
  | Unbound_identifier of Longident.t
  | Unavailable_module of string * Longident.t
  | Wrong_type of Longident.t
  | No_active_printer of Longident.t

exception Error of error

(* Symtable has global state, and normally holds the symbol table
   for the debuggee. We need to switch it temporarily to the
   symbol table for the debugger. *)

let debugger_symtable = ref (None: Symtable.global_map option)

let use_debugger_symtable fn arg =
  let old_symtable = Symtable.current_state() in
  begin match !debugger_symtable with
  | None ->
      Symtable.init_toplevel();
      debugger_symtable := Some(Symtable.current_state())
  | Some st ->
      Symtable.restore_state st
  end;
  try
    let result = fn arg in
    debugger_symtable := Some(Symtable.current_state());
    Symtable.restore_state old_symtable;
    result
  with exn ->
    Symtable.restore_state old_symtable;
    raise exn

(* Load a .cmo or .cma file *)

open Format

let rec loadfiles ppf name =
  try
    let filename = find_in_path !Config.load_path name in
    use_debugger_symtable Dynlink.loadfile filename;
    fprintf ppf "File %s loaded@." filename;
    true
  with
  | Dynlink.Error (Dynlink.Unavailable_unit unit) ->
      loadfiles ppf (String.uncapitalize unit ^ ".cmo")
        &&
      loadfiles ppf name
  | Not_found ->
      fprintf ppf "Cannot find file %s@." name;
      false
  | Dynlink.Error e ->
      raise(Error(Load_failure e))

let loadfile ppf name =
  if !debugger_symtable = None then begin
    Dynlink.add_interfaces stdlib_units [Config.standard_library];
    Dynlink.allow_unsafe_modules true
  end;
  ignore(loadfiles ppf name)

(* Return the value referred to by a path (as in toplevel/topdirs) *)
(* Note: evaluation proceeds in the debugger memory space, not in
   the debuggee. *)

let rec eval_path = function
    Pident id -> Symtable.get_global_value id
  | Pdot(p, s, pos) -> Obj.field (eval_path p) pos
  | Papply(p1, p2) -> fatal_error "Loadprinter.eval_path"

(* Install, remove a printer (as in toplevel/topdirs) *)

let find_printer_type lid =
  try
    let (path, desc) = Env.lookup_value lid Env.empty in
    Ctype.init_def(Ident.current_time());
    Ctype.begin_def();
    let ty_arg = Ctype.newvar() in
    Ctype.unify Env.empty
      (Ctype.newty (Tarrow("", ty_arg, Ctype.instance Predef.type_unit, Cok)))
      (Ctype.instance desc.val_type);
    Ctype.end_def();
    Ctype.generalize ty_arg;
    (ty_arg, path)
  with 
  | Not_found -> raise(Error(Unbound_identifier lid))
  | Ctype.Unify _ -> raise(Error(Wrong_type lid))
    
let install_printer ppf lid =
  let (ty_arg, path) = find_printer_type lid in
  let v =
    try
      use_debugger_symtable eval_path path
    with Symtable.Error(Symtable.Undefined_global s) ->
      raise(Error(Unavailable_module(s, lid))) in
  Printval.install_printer path ty_arg ppf (Obj.magic v : Obj.t -> unit)

let remove_printer lid =
  let (ty_arg, path) = find_printer_type lid in
  try
    Printval.remove_printer path
  with Not_found ->
    raise(Error(No_active_printer lid))

(* Error report *)

open Format

let report_error ppf = function
  | Load_failure e ->
      fprintf ppf "@[Error during code loading: %s@]@."
        (Dynlink.error_message e)
  | Unbound_identifier lid ->
      fprintf ppf "@[Unbound identifier %a@]@."
      Printtyp.longident lid
  | Unavailable_module(md, lid) ->
      fprintf ppf
        "@[The debugger does not contain the code for@ %a.@ \
           Please load an implementation of %s first.@]@."
        Printtyp.longident lid md
  | Wrong_type lid ->
      fprintf ppf "@[%a has the wrong type for a printing function.@]@."
      Printtyp.longident lid
  | No_active_printer lid ->
      fprintf ppf "@[%a is not currently active as a printing function.@]@."
      Printtyp.longident lid

      
