(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open Tk

let _ =
  let path = ref [] in
  Arg.parse
    keywords:[ "-I", Arg.String (fun s -> path := s :: !path),
               "<dir>  Add <dir> to the list of include directories" ]
    others:(fun name -> raise(Arg.Bad("don't know what to do with " ^ name)))
    errmsg:"ocamlbrowser :";
  Config.load_path := List.rev !path @ [Config.standard_library];
  begin
    try Searchid.start_env := Env.open_pers_signature "Pervasives" Env.initial
    with Env.Error _ -> ()
  end;
  
  Searchpos.view_defined_ref := Viewer.view_defined;
  Searchpos.editor_ref.contents <- Editor.f;

  let top = openTk class:"OCamlBrowser" () in
  Jg_config.init ();

  bind top events:[`Destroy] action:(fun _ -> exit 0);
  at_exit Shell.kill_all;
  

  Viewer.f on:top ();

  while true do
    try
      Printexc.print mainLoop ()
    with Protocol.TkError _ -> ()
  done
