(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Basic operations on core types *)

open Types

val generic_level: int

val newty2: int -> type_desc -> type_expr
        (* Create a type *)
val newgenty: type_desc -> type_expr
        (* Create a generic type *)
val newgenvar: unit -> type_expr
        (* Return a fresh generic variable *)
val newmarkedvar: int -> type_expr
        (* Return a fresh marked variable *)
val newmarkedgenvar: unit -> type_expr
        (* Return a fresh marked generic variable *)

val repr: type_expr -> type_expr
        (* Return the canonical representative of a type. *)

val field_kind_repr: field_kind -> field_kind
        (* Return the canonical representative of an object field
           kind. *)

(**** Utilities for type traversal ****)

val iter_type_expr: (type_expr -> unit) -> type_expr -> unit
        (* Iteration on types *)

val save_desc: type_expr -> type_desc -> unit
        (* Save a type description *)
val cleanup_types: unit -> unit
        (* Restore type descriptions *)

val lowest_level: int
        (* Marked type: ty.level < lowest_level *)
val pivot_level: int
        (* Type marking: ty.level <- pivot_level - ty.level *)
val mark_type: type_expr -> unit
        (* Mark a type *)
val unmark_type: type_expr -> unit
val unmark_type_decl: type_declaration -> unit
val unmark_class_type: class_type -> unit
val unmark_class_signature: class_signature -> unit
        (* Remove marks from a type *)

(**** Memorization of abbreviation expansion ****)

val cleanup_abbrev: unit -> unit
        (* Flush the cache of abbreviation expansions.
           When some types are saved (using [output_value]), this
           function MUST be called just before. *)
val memorize_abbrev:
        abbrev_memo ref -> Path.t -> type_expr -> type_expr -> unit
        (* Add an expansion in the cache *)
val forget_abbrev:
        abbrev_memo ref -> Path.t -> unit
        (* Remove an abbreviation from the cache *)
