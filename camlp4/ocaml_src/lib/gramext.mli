(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Id *)

type grammar =
  { gtokens : (Token.pattern, int ref) Hashtbl.t;
    mutable glexer : Token.lexer }
;;

type g_entry =
  { egram : grammar;
    ename : string;
    mutable estart : int -> Token.t Stream.t -> Obj.t;
    mutable econtinue : int -> int -> Obj.t -> Token.t Stream.t -> Obj.t;
    mutable edesc : g_desc }
and g_desc = Dlevels of g_level list | Dparser of (Token.t Stream.t -> Obj.t)
and g_level =
  { assoc : g_assoc;
    lname : string option;
    lsuffix : g_tree;
    lprefix : g_tree }
and g_assoc = NonA | RightA | LeftA
and g_symbol =
    Snterm of g_entry
  | Snterml of g_entry * string
  | Slist0 of g_symbol
  | Slist0sep of g_symbol * g_symbol
  | Slist1 of g_symbol
  | Slist1sep of g_symbol * g_symbol
  | Sopt of g_symbol
  | Sself
  | Snext
  | Stoken of Token.pattern
  | Stree of g_tree
and g_action = Obj.t
and g_tree = Node of g_node | LocAct of g_action * g_action list | DeadEnd
and g_node = { node : g_symbol; son : g_tree; brother : g_tree }
;;

type position =
  First | Last | Before of string | After of string | Level of string
;;

val levels_of_rules :
  g_entry -> position option ->
    (string option * g_assoc option * (g_symbol list * g_action) list) list ->
    g_level list;;
val srules : (g_symbol list * g_action) list -> g_symbol;;
external action : 'a -> g_action = "%identity";;

val delete_rule_in_level_list :
  g_entry -> g_symbol list -> g_level list -> g_level list;;

val warning_verbose : bool ref;;
