(* camlp4r *)
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

let open_out_file () =
  match !(Pcaml.output_file) with
    Some f -> open_out_bin f
  | None -> set_binary_mode_out stdout true; stdout
;;

let interf ast =
  let pt = Ast2pt.interf (List.map fst ast) in
  let oc = open_out_file () in
  output_string oc Config.ast_intf_magic_number;
  output_value oc !(Pcaml.input_file);
  output_value oc pt;
  flush oc;
  match !(Pcaml.output_file) with
    Some _ -> close_out oc
  | None -> ()
;;

let implem ast =
  let pt = Ast2pt.implem (List.map fst ast) in
  let oc = open_out_file () in
  output_string oc Config.ast_impl_magic_number;
  output_value oc !(Pcaml.input_file);
  output_value oc pt;
  flush oc;
  match !(Pcaml.output_file) with
    Some _ -> close_out oc
  | None -> ()
;;

Pcaml.print_interf := interf;;
Pcaml.print_implem := implem;;
