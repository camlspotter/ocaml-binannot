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

(* Annotations 

   Annotations are stored in .spot with their locations
*)

module Name : sig
  (* Name is an identifier name with its stamp number. For example, 
     an ident of name "string" with a stamp 3 has the name "string__3".

     With these names, OCamlSpotter distinguishes textual
     representations of idents with the same name but with different
     stamps.
  *)
  type t = string (* CR jfuruse: should be abstracted? *)
  val create : string -> int -> t
  val parse : t -> string * int
end

module Ident : sig
  type t = Ident.t
  val name : t -> Name.t
  val unsafe_create_with_stamp : ?flags:int -> string -> int -> t
    (** create an ident with given flags and stamp *)
  val parse : Name.t -> t

  (* from the original *)    
  val global: t -> bool
end

module Path : sig
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
  val local : t -> bool
      (** return true if "local" *)
  val parse : Name.t -> t
end

module TypeFix : sig

  val type_expr : Types.type_expr -> Types.type_expr
    (** put pos and stamps to type_expr *)

  val module_type : Types.module_type -> Types.module_type

end

module Printtyp : sig
  open Format

  open Types

  val reset : unit -> unit
  val mark_loops : Types.type_expr -> unit
  val reset_names : unit -> unit
  val type_expr :
    ?with_pos:bool -> formatter -> Types.type_expr -> unit
  val type_sch :
    ?with_pos:bool -> formatter -> Types.type_expr -> unit
  val type_scheme :
    ?with_pos:bool -> formatter -> Types.type_expr -> unit
  val modtype :
    ?with_pos:bool -> formatter -> Types.module_type -> unit
end

module Position : sig

  type t = { line_column : (int * int) option; 
             bytes : int option; }

  val none : t
  val compare : t -> t -> int
  val next : t -> t
  val of_lexing_position : Lexing.position -> t

  exception Parse_failure of string
  val parse : string -> t (* may raise Parse_failure *)

  val to_string : t -> string
  val is_complete : t -> bool
  val complete : string -> t -> t
end

module Region : sig

  type t = { start : Position.t; end_ : Position.t; }
  
  val compare : t -> t -> [> `Included | `Includes | `Left | `Overwrap | `Right | `Same ]

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
  val compare :
    'a t ->
    'b t -> [> `Included | `Includes | `Left | `Overwrap | `Right | `Same ]
  val split : 'a t -> by:'b t -> ('a t * 'a t) option
  val format : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Location_bound : sig
  val upperbound : Location.t -> Location.t -> Location.t
end

module Kind : sig
  type t = 
    | Value  (** regular value *)
    | Special_value (** primitives and others *) 
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

module Abstraction : sig

  (* module definition abstraction *)
  type module_expr = (* private *)
    | Mod_ident of Path.t (* module M = N *)
    | Mod_packed of string (* full path *)
        (* -pack overrides load paths: ocamlc -pack dir1/dir2/dir3/x.cmo *)
    | Mod_structure of structure (* module M = struct ... end *)
    | Mod_functor of Ident.t * Types.module_type * module_expr (* module M(I:S) = *)
    | Mod_apply of module_expr * module_expr (* module M = N(O) *)
    | Mod_constraint of module_expr * Types.module_type
    | Mod_unpack of Types.module_type
    | Mod_abstract (* used for Tmodtype_abstract *)

  (* structure abstraction : name - defloc asoc list *)
  and structure = structure_item list

  and structure_item = 
    | Str_value of Ident.t
    | Str_value_alias of Ident.t * Path.t
        (** [Str_value_alias(id, path)] defines [id], but its definition is at the one for [path] *)
    | Str_type of Ident.t
    | Str_exception of Ident.t
    | Str_module of Ident.t * module_expr
    | Str_modtype of Ident.t * module_expr
    | Str_class of Ident.t
    | Str_cltype of Ident.t
    | Str_include of module_expr * (Kind.t * Ident.t) list

  val ident_of_structure_item : structure_item -> (Kind.t * Ident.t) option

  open Format
  val format_module_expr : formatter -> module_expr -> unit
  val format_structure : formatter -> structure -> unit
  val format_structure_item : formatter -> structure_item -> unit
end

module Annot : sig
  type t =
    | Type of Types.type_expr (* sub-expression's type *)
    | Str of Abstraction.structure_item 
    | Use of Kind.t * Path.t
    | Module of Abstraction.module_expr
    | Functor_parameter of Ident.t
    | Non_expansive of bool
    | Mod_type of Types.module_type

  val record_saved_type : Typedtree.saved_type -> unit
  val recorded : unit -> (Location.t * t) list
  val recorded_top : unit -> Abstraction.structure option 
(*
  val record : Location.t -> t -> unit
    
  (* [record_constr_type_use loc ty] records a constructor use of type [ty]
     at the location [loc]. [ty] must be a constructor type, otherwise,
     an error message is printed out. 
  *)
  val record_constr_type_use : Location.t -> Types.type_expr -> unit
  val record_module_expr_def : Location.t -> Ident.t -> Typedtree.module_expr -> unit
  val record_module_expr_use : Location.t -> Typedtree.module_expr -> unit
  val record_include :
    Location.t -> Typedtree.module_expr -> Types.signature -> unit
  val record_include_sig :
    Location.t ->
    Types.module_type -> Types.signature -> unit
  val record_module_type_def : Location.t -> Ident.t -> Types.module_type -> unit
  val recorded : unit -> (Location.t * t) list
*)

  val format : Format.formatter -> t -> unit
  val summary : Format.formatter -> t -> unit
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

(* Spot file *)
module File : sig
  type elem =
    | Argv of string array
    | Source_path of string option
    | Cwd of string
    | Load_paths of string list
    | Saved_types of Typedtree.saved_type array

  (* marshalled type *)
  type t = elem list

end
