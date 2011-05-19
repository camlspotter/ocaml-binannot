module type S = sig
  val app_version : string
  
  val version : string
  val print_version : unit -> unit
  
  val dump_file : bool
  val dump_rannots : [ `None | `Full | `Summary ]
  val dump_tree : bool
  val dump_top : bool
  val dump_flat : bool
  val dump_any : bool
  val eager_dump : bool
  
  val no_definition_analysis : bool
  
  val strict_time_stamp : bool
  
  val print_file_info : bool
  val print_interface : bool
  
  module SearchSpec : sig
    type t =
      | Pos of Spotapi.Position.t
      | Kind of Spotapi.Kind.t * Spotapi.Path.t
    val parse : string -> string * t
    val to_string : t -> string
  end
  
  val mode : [ `Dump of string
             | `Query of string * SearchSpec.t
  	   | `Use of (string * SearchSpec.t) * string list
  	   | `Recheck of string list
  	   | `Typecheck of string list ]
end

