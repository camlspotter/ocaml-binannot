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

(* Module [Hashtbl]: hash tables and hash functions *)

(* Hash tables are hashed association tables, with in-place modification. *)

(*** Generic interface *)

type ('a, 'b) t
        (* The type of hash tables from type ['a] to type ['b]. *)

val create : int -> ('a,'b) t
        (* [Hashtbl.create n] creates a new, empty hash table, with
           initial size [n].  For best results, [n] should be on the
           order of the expected number of elements that will be in
           the table.  The table grows as needed, so [n] is just an
           initial guess. *)

val clear : ('a, 'b) t -> unit
        (* Empty a hash table. *)

val add : ('a, 'b) t -> key:'a -> data:'b -> unit
        (* [Hashtbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
           Previous bindings for [x] are not removed, but simply
           hidden. That is, after performing [Hashtbl.remove tbl x],
           the previous binding for [x], if any, is restored.
           (Same behavior as with association lists.) *)

val find : ('a, 'b) t -> 'a -> 'b
        (* [Hashtbl.find tbl x] returns the current binding of [x] in [tbl],
           or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b) t -> 'a -> 'b list
        (* [Hashtbl.find_all tbl x] returns the list of all data
           associated with [x] in [tbl].
           The current binding is returned first, then the previous
           bindings, in reverse order of introduction in the table. *)

val mem :  ('a, 'b) t -> 'a -> bool
        (* [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)

val remove : ('a, 'b) t -> 'a -> unit
        (* [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
           restoring the previous binding if it exists.
           It does nothing if [x] is not bound in [tbl]. *)

val iter : f:(key:'a -> data:'b -> unit) -> ('a, 'b) t -> unit
        (* [Hashtbl.iter f tbl] applies [f] to all bindings in table [tbl].
           [f] receives the key as first argument, and the associated value
           as second argument. The order in which the bindings are passed to
           [f] is unspecified. Each binding is presented exactly once
           to [f]. *)

(*** Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end
        (* The input signature of the functor [Hashtbl.Make].
           [t] is the type of keys.
           [equal] is the equality predicate used to compare keys.
           [hash] is a hashing function on keys, returning a non-negative
           integer. It must be such that if two keys are equal according
           to [equal], then they must have identical hash values as computed
           by [hash].
           Examples: suitable ([equal], [hash]) pairs for arbitrary key
           types include
           ([(=)], [Hashtbl.hash]) for comparing objects by structure, and
           ([(==)], [Hashtbl.hash]) for comparing objects by addresses
           (e.g. for mutable or cyclic keys). *)

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a t
    val clear: 'a t -> unit
    val add: 'a t -> key:key -> data:'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_all: 'a t -> key -> 'a list
    val mem: 'a t -> key -> bool
    val iter: f:(key:key -> data:'a -> unit) -> 'a t -> unit
  end

module Make(H: HashedType): (S with type key = H.t)

        (* The functor [Hashtbl.Make] returns a structure containing
           a type [key] of keys and a type ['a t] of hash tables
           associating data of type ['a] to keys of type [key].
           The operations perform similarly to those of the generic
           interface, but use the hashing and equality functions
           specified in the functor argument [H] instead of generic
           equality and hashing. *)

(*** The polymorphic hash primitive *)

val hash : 'a -> int
        (* [Hashtbl.hash x] associates a positive integer to any value of
           any type. It is guaranteed that
                if [x = y], then [hash x = hash y]. 
           Moreover, [hash] always terminates, even on cyclic
           structures. *)

external hash_param : int -> int -> 'a -> int = "hash_univ_param" "noalloc"
        (* [Hashtbl.hash_param n m x] computes a hash value for [x], with the
           same properties as for [hash]. The two extra parameters [n] and
           [m] give more precise control over hashing. Hashing performs a
           depth-first, right-to-left traversal of the structure [x], stopping
           after [n] meaningful nodes were encountered, or [m] nodes,
           meaningful or not, were encountered. Meaningful nodes are: integers;
           floating-point numbers; strings; characters; booleans; and constant
           constructors. Larger values of [m] and [n] means that more
           nodes are taken into account to compute the final hash
           value, and therefore collisions are less likely to happen.
           However, hashing takes longer. The parameters [m] and [n]
           govern the tradeoff between accuracy and speed. *)
