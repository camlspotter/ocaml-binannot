(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2011 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Spot
open Spoteval

module Make( Spotconfig : Spotconfig_intf.S ) : sig

  type elem = 
    | Argv of string array
    | Source_path of string option
    | Cwd of string
    | Load_paths of string list
    | Saved_types of Typedtree.saved_type array

  type top = 
    | Saved_type of Typedtree.saved_type
    | Packed of string list

  (* loaded file *)    
  type file = {
    path : string; (* "" means no source *)
    cwd : string;
    load_paths : string list;
    argv : string array;
    top : top option;
    rannots : Annot.t Regioned.t list;
    tree : Tree.t lazy_t;
    flat : (Ident.t, Annot.t Regioned.t) Hashtbl.t;
  }
	
  val cmt_of_file : string -> string

  exception Old_spot of string * string
  val load : load_paths:string list -> string -> file
  val load_module : ?spit:bool -> load_paths:string list -> string -> file
    
(*
  val empty_env : file -> Env.t
  val invalid_env : file -> Env.t
*)

  type result = File_itself | Found_at of Annot.t Regioned.t | Predefined
  
  val find_path_in_flat : file -> Kind.t * Path.t -> PIdent.t * result

(*
  val str_of_global_ident : load_paths:string list -> Ident.t -> string * Value.structure

  val eval_packed : Env.t -> string -> Value.t
*)

  val dump_file : file -> unit
  val dump_elem : elem -> unit
  val dump_elems : elem list -> unit
end
