(* camlp4r *)
(* Id *)

(* Module [Extfun]: extensible functions *)

(* This module implements pattern matching extensible functions.
   To extend, use syntax [pa_extfun.cmo]:
-      [extfun e with [ pattern_matching ]] *)

type ('a, 'b) t;;
    (* The type of the extensible functions of type ['a -> 'b] *)
val empty : ('a, 'b) t;;
    (* Empty extensible function *)
val apply : ('a, 'b) t -> 'a -> 'b;;
    (* Apply an extensible function *)
exception Failure;;
    (* Match failure while applying an extensible function *)
val print : ('a, 'b) t -> unit;;
    (* Print patterns in the order they are recorded *)

(*--*)

type ('a, 'b) matching =
  { patt : patt; has_when : bool; expr : ('a, 'b) expr }
and patt =
    Eapp of patt list
  | Eacc of patt list
  | Econ of string
  | Estr of string
  | Eint of string
  | Etup of patt list
  | Evar of unit
and ('a, 'b) expr = 'a -> 'b option
;;

val extend : ('a, 'b) t -> (patt * bool * ('a, 'b) expr) list -> ('a, 'b) t;;
