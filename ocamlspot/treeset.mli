(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008, 2009 Jun Furuse. All rights reserved.             *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

module type OrderedType = sig
  type t
  val compare : t -> t 
    -> [ `Included | `Includes | `Left | `Overwrap | `Right | `Same ]
  val split : t -> by:t -> (t * t) option
end

module Make (Ord : OrderedType) : sig
  type elem = Ord.t
  type 'a node = Node of elem * 'a

  module rec Node : sig 
    type t = NodeSet.t node 
    val compare : t -> t -> int 
  end and NodeSet : Xset.S with type elt = Node.t 

  include Xset.S with type elt = Node.t and type t = NodeSet.t

  val add_elem : elem -> t -> t

  val find_path_contains : elem -> t -> (elem * t) list

  val iter_elem : (parent:elem option -> elem -> 'a) -> t -> unit
end

