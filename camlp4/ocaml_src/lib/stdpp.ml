(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Id *)

exception Exc_located of (int * int) * exn;;

let raise_with_loc loc exc =
  match exc with
    Exc_located (_, _) -> raise exc
  | _ -> raise (Exc_located (loc, exc))
;;

let loc_name = ref "loc";;
