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

(* Set with binary custom search *)

module type OrderedType = Set.OrderedType

module type S = sig
  include Set.S

  val middle : t -> elt option
  (** Obtain the top element in the binary tree representation of [t],
      which is usually near the median. *)

  val find : elt -> t -> elt option
  (** find the "equal" element as [elt] in [t] *)

end

module Make(Ord : OrderedType) : S 
  with type elt = Ord.t
