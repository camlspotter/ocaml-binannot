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

module List : sig
  include module type of List
  val find_map_opt : ('a -> 'b option) -> 'a list -> 'b option
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
end

module Debug : sig
  val on : bool ref
  val format : ('a, Format.formatter, unit) format -> 'a
end

module Lazy : sig
  include module type of Lazy
  module Open : sig 
    val ( !! ) : 'a Lazy.t -> 'a 
    val eager : 'a -> 'a Lazy.t 
  end
  val peek : 'a t -> 'a option
  val apply : ('a -> 'b) -> 'a t -> 'b Lazy.t
  val is_val : 'a t -> bool
end
include module type of Lazy.Open

module Filename : sig
  include module type of Filename
  val split_extension : string -> string * string
  module Open : sig
    val (^/) : string -> string -> string
  end
end
include module type of Filename.Open

module Format : sig
  include module type of Format with type formatter = Format.formatter
  val list :
    string -> (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
  val option :
    (formatter -> 'a -> unit) ->
    formatter -> 'a option -> unit
  val lazy_ :
    (formatter -> 'a -> unit) ->
    formatter -> 'a Lazy.t -> unit
end

module Option : sig
  val map : f:('a -> 'b) -> 'a option -> 'b option
  val bind : 'a option -> ('a -> 'b option) -> 'b option
  val iter : f:('a -> unit) -> 'a option -> unit
end

exception Finally of exn * exn

val protect : f:('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b

module Unix : sig
  include module type of Unix

  type path = 
      { dir : string;
	base : string;
	path : string; (* dir / name *)
	stat : [ `Ok of stats | `Error of exn ];
	depth : int;
      }

  val prune : unit -> unit
  val find : f:(path -> unit) -> string list -> unit
end

module Hashtbl : sig
  include module type of Hashtbl with type ('a, 'b) t = ('a, 'b) Hashtbl.t
  val to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
end
