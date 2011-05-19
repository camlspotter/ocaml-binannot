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

(* Set with binary custom search *)

module type OrderedType = Set.OrderedType

module type S = sig
  include Set.S

  val middle : t -> elt option
  val find : elt -> t -> elt option
end

module Make(Ord : OrderedType) : S 
  with type elt = Ord.t
