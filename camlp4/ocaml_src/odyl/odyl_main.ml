(* camlp4r pa_ifdef.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Id *)

let go = ref (fun () -> ());;
let name = ref "odyl";;

let first_arg_no_load () =
  let rec loop i =
    if i < Array.length Sys.argv then
      match Sys.argv.(i) with
        "-I" -> loop (i + 2)
      | "-nolib" -> loop (i + 1)
      | "-where" -> loop (i + 1)
      | "--" -> i + 1
      | s ->
          if Filename.check_suffix s ".cmo" ||
             Filename.check_suffix s ".cma" then
            loop (i + 1)
          else i
    else i
  in
  loop 1
;;

Arg.current := first_arg_no_load () - 1;;

(* Load files in core *)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else
    let rec try_dir =
      function
        [] -> raise Not_found
      | dir :: rem ->
          let fullname = Filename.concat dir name in
          if Sys.file_exists fullname then fullname else try_dir rem
    in
    try_dir path
;;

exception Error of string * string;;

let nolib = ref false;;
let initialized = ref false;;
let path = ref [];;

let loadfile file =
  if not !initialized then
    begin
      begin Dynlink.init (); Dynlink.allow_unsafe_modules true end;
      initialized := true
    end;
  let path =
    if !nolib then !path else Odyl_config.standard_library :: !path
  in
  let fname =
    try find_in_path (List.rev path) file with
      Not_found -> raise (Error (file, "file not found in path"))
  in
  try Dynlink.loadfile fname with
    Dynlink.Error e -> raise (Error (fname, Dynlink.error_message e))
;;

let directory d = path := d :: !path;;
