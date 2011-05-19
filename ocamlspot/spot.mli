(* This module is extended in ocamlspot, therefore it cannot be .mli *)

(* Annotations 

   Annotations are stored in .spot with their locations
*)

module Location_bound : sig
  val upperbound : Location.t -> Location.t -> Location.t
end

module Kind : sig
  type t = 
    | Value | Type | Exception 
    | Module | Module_type 
    | Class | Class_type
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
    | Str_type of Ident.t
    | Str_exception of Ident.t
    | Str_module of Ident.t * module_expr
    | Str_modtype of Ident.t * module_expr
    | Str_class of Ident.t
    | Str_cltype of Ident.t
    | Str_include of module_expr * (Kind.t * Ident.t) list

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
