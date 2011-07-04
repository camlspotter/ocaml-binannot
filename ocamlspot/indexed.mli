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

open Utils

module Name : sig
  (* Name is an identifier name with its stamp number. For example, 
     an ident of name "string" with stamp 3 has the name "string__3".

     With these names, OCamlSpotter distinguishes textual
     representations of idents with the same name but with different
     stamps.
  *)
  type t = string (* CR jfuruse: should be abstracted? *)
  val create : string -> int -> t
  val parse : t -> string * int
end

module Ident : sig
  (** Modified Ident. The original Ident is available as Ocaml.Ident *)

  type t = Ident.t

  val name : t -> Name.t 
  (** returns the name of an ident, with the stamp  *)

  val unsafe_create_with_stamp : ?flags:int -> string -> int -> t
  (** create an ident with given flags and stamp *)

  val parse : Name.t -> t

  (* from the original *)    
  val global: t -> bool

  val format : Format.t -> t -> unit
end

module Path : sig
  (** Modified Path. The original Path is avaiable as Ocaml.Path *)

  type t = Path.t =
	   | Pident of Ident.t
	   | Pdot of t * string * int
	   | Papply of t * t

  val same : t -> t -> bool
  val isfree : Ident.t -> t -> bool
  val binding_time : t -> int
  val nopos : int
  val head : t -> Ident.t

  val name : t -> Name.t
  (** return the name of a path, with stamp *)

  val local : t -> bool
  (** return true if "local" *)

  val parse : Name.t -> t

  val format : Format.t -> t -> unit
end

module TypeFix : sig
  (** Replacing the original path and idents by those with stamp names *)
    
  val type_expr : Types.type_expr -> Types.type_expr
  (** put pos and stamps to type_expr *)

  val module_type : Types.module_type -> Types.module_type

end

module Printtyp : sig
  (** Modified type printers which can print types with stamp names *)

  open Types

  val reset : unit -> unit
  val mark_loops : type_expr -> unit
  val reset_names : unit -> unit
  val type_expr : ?with_pos:bool -> Format.t -> type_expr -> unit
  val type_sch : ?with_pos:bool -> Format.t -> type_expr -> unit
  val type_scheme : ?with_pos:bool -> Format.t -> type_expr -> unit
  val modtype : ?with_pos:bool -> Format.t -> module_type -> unit
end

