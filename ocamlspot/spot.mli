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

module Position : sig
  (* By historical reason, we have several syntaxes of source code positions

     Bytes from the head of file: 123

     Lines and columns: l10c20

     Lines, columns and bytes: l10c20b123
 
     Bytes only, with the mark of `b'ytes: b123

     Simplified lines and columns: 10_20
     
     Simplified lines, columns and bytes: 10_20_123

  *)

  type t = { line_column : (int * int) option; 
             bytes : int option; }

  val none : t
  val compare : t -> t -> int
  val of_lexing_position : Lexing.position -> t

  val next : t -> t
  (** Return the next char position *)

  exception Parse_failure of string
  val parse : string -> t (* may raise Parse_failure *)

  val to_string : t -> string
  (** Print t in the simplified formats *)
  
  val is_complete : t -> bool
  (** Returns true if the position has both of line-columns and bytes *)

  val complete : string -> t -> t
  (** [complete filepath t] tries to complete the position [t]
      by actually opening and counting the chars and lines of the file [filepath]. *)

end

module Region : sig
  (** A region is a pair of positions *)
  type t = { start : Position.t; end_ : Position.t; }
  
  val compare : t -> t -> [> `Included | `Includes | `Left | `Overwrap | `Right | `Same ]
  (** [compare t1 t2] returns:
      `Included: if [t1] is completely inside [t2]
      `Includes: if [t2] is completely inside [t1]
      `Left:     if [t1] is left of [t2] and have no intersection
      `Overwrap: if [t1] and [t2] intersect
      `Right:    if [t1] is right of [t2] and have no intersection
      `Same:     if [t1] and [t2] are exactly the same   
  *)

  val to_string : t -> string
  val of_parsing : Location.t -> t
  val split : t -> by:t -> (t * t) option
  val point_by_byte : int -> t  
  (** works only if bytes are available *)
  val point : Position.t -> t
  val length_in_bytes : t -> int
  val is_complete : t -> bool
  val complete : string -> t -> t
  val substring : string -> t -> t * string
end

module Regioned : sig
  type 'a t = { region : Region.t; value : 'a; }
  val compare : 'a t -> 'b t -> [> `Included | `Includes | `Left | `Overwrap | `Right | `Same ]
  val split : 'a t -> by:'b t -> ('a t * 'a t) option
  val format : (Format.t -> 'a -> unit) -> Format.t -> 'a t -> unit
end

module Location_bound : sig
  val upperbound : Location.t -> Location.t -> Location.t
  (** [upperbound loc by] returns [loc] but its loc_end is replaced by [by.loc_start] *)
end

module Kind : sig
  type t = 
    | Value  (** regular value *)
    | Primitive (** primitives and others *) 
    | Type 
    | Exception 
    | Module 
    | Module_type 
    | Class 
    | Class_type

  val to_string : t -> string
  val from_string : string -> t
  val name : t -> string
end

module Signature : sig
  open Types
  val sg_of_mtype : Env.t -> module_type -> signature
  val kidents_of_signature : signature -> (Kind.t * Ident.t) list
  val include_coercion : Ident.t list -> signature -> (Kind.t * Ident.t * Ident.t) list
end

module Annot : sig
  type def =
    | Def_module_expr of Typedtree.module_expr
    | Def_module_type of Typedtree.module_type
    | Def_alias of Path.t
    | Def_included of Typedtree.module_expr * Ident.t
    | Def_included_sig of Typedtree.module_type

  type t =
    | Type of Types.type_expr (* sub-expression's type *)
    | Mod_type of Types.module_type
(*
    | Non_expansive of bool
*)
    | Use of Kind.t * Path.t
    | Functor_parameter of Ident.t
    | Def of Kind.t * Ident.t * def option (* definition of Ident.t *)

  val record_saved_type : Typedtree.saved_type -> unit
  val recorded : unit -> (Location.t * t) list
  val recorded_top : unit -> Typedtree.saved_type option

  val format : Format.t -> t -> unit
  val summary : Format.t -> t -> unit
  (** same as [format] but bigger structures are omitted *)    

  val dummy : t
end

module Tree : sig
  type elem = Annot.t Regioned.t
  type t
  val empty : t
  val is_empty : t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val cardinal : t -> int
  val add : t -> elem -> t
  val find_path_contains : Region.t -> t -> (elem * t) list

  val iter : (parent:elem option -> elem -> unit) -> t -> unit
    (** Region splitted Annot may be itered more than once. *)

  val dump : t -> unit
end
