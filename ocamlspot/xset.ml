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
  val find : elt -> t -> elt option
end

module Make(Ord : OrderedType) : S with type elt = Ord.t = struct
  include Set.Make(Ord)

  (* find the middle element in [t] *)
  let middle t =
    if is_empty t then None
    else
      let found = ref None in
      try
	ignore (filter (fun elt -> 
	  found := Some elt;
	  raise Exit) t);
	assert false
      with
      | Exit -> !found

  (* find the "same" element as [elt] in [t] *)
  let rec find elt t =
    match middle t with
    | None -> None
    | Some elt' ->
	match Ord.compare elt elt' with 
	| 0 -> Some elt'
	| -1 ->
	    let l, present, _ = split elt' t in
	    assert present;
	    find elt l
	| 1 -> 
	    let _, present, r = split elt' t in
	    assert present;
	    find elt r
	| _ -> assert false
end
