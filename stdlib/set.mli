(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Set]: sets over ordered types *)

(* This module implements the set data structure, given a total ordering
   function over the set elements. All operations over sets
   are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and is therefore
   reasonably efficient: insertion and membership take time
   logarithmic in the size of the set, for instance. *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end
          (* The input signature of the functor [Set.Make].
             [t] is the type of the set elements.
             [compare] is a total ordering function over the set elements.
             This is a two-argument function [f] such that
             [f e1 e2] is zero if the elements [e1] and [e2] are equal,
             [f e1 e2] is strictly negative if [e1] is smaller than [e2],
             and [f e1 e2] is strictly positive if [e1] is greater than [e2].
             Example: a suitable ordering function is
             the generic structural comparison function [compare]. *)

module type S =
  sig
    type elt
          (* The type of the set elements. *)
    type t
          (* The type of sets. *)
    val empty: t
          (* The empty set. *)
    val is_empty: t -> bool
        (* Test whether a set is empty or not. *)
    val mem: elt:elt -> t -> bool
        (* [mem x s] tests whether [x] belongs to the set [s]. *)
    val add: elt:elt -> t -> t
        (* [add x s] returns a set containing all elements of [s],
           plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
    val singleton: elt -> t
        (* [singleton x] returns the one-element set containing only [x]. *)
    val remove: elt:elt -> t -> t
        (* [remove x s] returns a set containing all elements of [s],
           except [x]. If [x] was not in [s], [s] is returned unchanged. *)
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
        (* Union, intersection and set difference. *)
    val compare: t -> t -> int
        (* Total ordering between sets. Can be used as the ordering function
           for doing sets of sets. *)
    val equal: t -> t -> bool
        (* [equal s1 s2] tests whether the sets [s1] and [s2] are
           equal, that is, contain equal elements. *)
    val subset: t -> t -> bool
        (* [subset s1 s2] tests whether the set [s1] is a subset of
           the set [s2]. *)
    val iter: fun:(elt -> unit) -> t -> unit
        (* [iter f s] applies [f] in turn to all elements of [s].
           The order in which the elements of [s] are presented to [f]
           is unspecified. *)
    val fold: fun:(elt -> acc:'a -> 'a) -> t -> acc:'a -> 'a
        (* [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
           where [x1 ... xN] are the elements of [s].
           The order in which elements of [s] are presented to [f] is
           unspecified. *)
    val cardinal: t -> int
        (* Return the number of elements of a set. *)
    val elements: t -> elt list
        (* Return the list of all elements of the given set.
           The returned list is sorted in increasing order with respect
           to the ordering [Ord.compare], where [Ord] is the argument
           given to [Set.Make]. *)
    val min_elt: t -> elt
        (* Return the smallest element of the given set
           (with respect to the [Ord.compare] ordering), or raise
           [Not_found] if the set is empty. *)
    val max_elt: t -> elt
        (* Same as [min_elt], but returns the largest element of the
           given set. *)
    val choose: t -> elt
        (* Return one element of the given set, or raise [Not_found] if
           the set is empty. Which element is chosen is unspecified,
           but equal elements will be chosen for equal sets. *)
  end

module Make(Ord: OrderedType): (S with type elt = Ord.t)
        (* Functor building an implementation of the set structure
           given a totally ordered type. *)
