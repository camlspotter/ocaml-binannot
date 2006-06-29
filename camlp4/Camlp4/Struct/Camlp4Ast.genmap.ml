(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

module Make (Loc : Sig.Loc.S)
: Sig.Camlp4Ast.S with module Loc = Loc
= struct
  module Loc = Loc;
  module Ast = Sig.Camlp4Ast.Make Loc;
  include Ast;
  external loc_of_ctyp : ctyp -> Loc.t = "%field0";
  external loc_of_patt : patt -> Loc.t = "%field0";
  external loc_of_expr : expr -> Loc.t = "%field0";
  external loc_of_module_type : module_type -> Loc.t = "%field0";
  external loc_of_module_expr : module_expr -> Loc.t = "%field0";
  external loc_of_sig_item : sig_item -> Loc.t = "%field0";
  external loc_of_str_item : str_item -> Loc.t = "%field0";
  external loc_of_class_type : class_type -> Loc.t = "%field0";
  external loc_of_class_sig_item : class_sig_item -> Loc.t = "%field0";
  external loc_of_class_expr : class_expr -> Loc.t = "%field0";
  external loc_of_class_str_item : class_str_item -> Loc.t = "%field0";
  external loc_of_with_constr : with_constr -> Loc.t = "%field0";
  external loc_of_binding : binding -> Loc.t = "%field0";
  external loc_of_module_binding : module_binding -> Loc.t = "%field0";
  external loc_of_assoc : assoc -> Loc.t = "%field0";
  external loc_of_ident : ident -> Loc.t = "%field0";

  class map = Camlp4Filters.GenerateMap.generated;
  class c_expr f = object
    inherit map as super;
    method expr x = f (super#expr x);
  end;
  class c_patt f = object
    inherit map as super;
    method patt x = f (super#patt x);
  end;
  class c_ctyp f = object
    inherit map as super;
    method ctyp x = f (super#ctyp x);
  end;
  class c_str_item f = object
    inherit map as super;
    method str_item x = f (super#str_item x);
  end;
  class c_sig_item f = object
    inherit map as super;
    method sig_item x = f (super#sig_item x);
  end;
  class c_loc f = object
    inherit map as super;
    method _Loc_t x = f (super#_Loc_t x);
  end;
  value map_patt f ast = (new c_patt f)#patt ast;
  value map_loc f ast = (new c_loc f)#_Loc_t ast;
  value map_sig_item f ast = (new c_sig_item f)#sig_item ast;
  value map_str_item f ast = (new c_str_item f)#str_item ast;
  value map_ctyp f ast = (new c_ctyp f)#ctyp ast;
  value map_expr f ast = (new c_expr f)#expr ast;
  value ghost = Loc.ghost;

  value safe_string_escaped s =
    if String.length s > 2 && s.[0] = '\\' && s.[1] = '$' then s
    else String.escaped s;

  value rec is_module_longident =
    fun
    [ <:ident< $_$.$i$ >> -> is_module_longident i
    | <:ident< $i1$ $i2$ >> ->
        is_module_longident i1 && is_module_longident i2
    | <:ident< $uid:_$ >> -> True
    | _ -> False ];

  value ident_of_expr =
    let error () =
      invalid_arg "ident_of_expr: this expression is not an identifier" in
    let rec self =
      fun
      [ <:expr@_loc< $e1$ $e2$ >> -> <:ident< $self e1$ $self e2$ >>
      | <:expr@_loc< $e1$.$e2$ >> -> <:ident< $self e1$.$self e2$ >>
      | <:expr< $lid:_$ >> -> error ()
      | <:expr< $id:i$ >> -> if is_module_longident i then i else error ()
      | <:expr< $_$ >> -> error () ] in
    fun
    [ <:expr< $id:i$ >> -> i
    | <:expr@_loc< $e1$ $e2$ >> -> error ()
    | t -> self t ];

  value ident_of_ctyp =
    let error () =
      invalid_arg "ident_of_ctyp: this type is not an identifier" in
    let rec self =
      fun
      [ <:ctyp@_loc< $t1$ $t2$ >> -> <:ident< $self t1$ $self t2$ >>
      | <:ctyp< $lid:_$ >> -> error ()
      | <:ctyp< $id:i$ >> -> if is_module_longident i then i else error ()
      | <:ctyp< $_$ >> -> error () ] in
    fun
    [ <:ctyp< $id:i$ >> -> i
    | t -> self t ];

  value rec tyOr_of_list =
    fun
    [ [] -> <:ctyp@ghost<>>
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_ctyp t in <:ctyp< $t$ | $tyOr_of_list ts$ >> ];

  value rec tyAnd_of_list =
    fun
    [ [] -> <:ctyp@ghost<>>
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_ctyp t in <:ctyp< $t$ and $tyAnd_of_list ts$ >> ];

  value rec tySem_of_list =
    fun
    [ [] -> <:ctyp@ghost<>>
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_ctyp t in <:ctyp< $t$ ; $tySem_of_list ts$ >> ];

  value rec stSem_of_list =
    fun
    [ [] -> <:str_item@ghost<>>
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_str_item t in <:str_item< $t$ ; $stSem_of_list ts$ >> ];

  value rec sgSem_of_list =
    fun
    [ [] -> <:sig_item@ghost<>>
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_sig_item t in <:sig_item< $t$ ; $sgSem_of_list ts$ >> ];

  value rec biAnd_of_list =
    fun
    [ [] -> <:binding@ghost<>>
    | [b] -> b
    | [b::bs] ->
        let _loc = loc_of_binding b in <:binding< $b$ and $biAnd_of_list bs$ >> ];

  value rec wcAnd_of_list =
    fun
    [ [] -> <:with_constr@ghost<>>
    | [w] -> w
    | [w::ws] ->
        let _loc = loc_of_with_constr w in
        <:with_constr< $w$ and $wcAnd_of_list ws$ >> ];

  value rec idAcc_of_list =
    fun
    [ [] -> assert False
    | [i] -> i
    | [i::is] ->
        let _loc = loc_of_ident i in
        <:ident< $i$ . $idAcc_of_list is$ >> ];

  value rec idApp_of_list =
    fun
    [ [] -> assert False
    | [i] -> i
    | [i::is] ->
        let _loc = loc_of_ident i in
        <:ident< $i$ $idApp_of_list is$ >> ];

  value rec asOr_of_list =
    fun
    [ [] -> <:assoc@ghost<>>
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_assoc x in
        <:assoc< $x$ | $asOr_of_list xs$ >> ];

  value rec mbAnd_of_list =
    fun
    [ [] -> <:module_binding@ghost<>>
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_module_binding x in
        <:module_binding< $x$ and $mbAnd_of_list xs$ >> ];

  value rec meApp_of_list =
    fun
    [ [] -> assert False
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_module_expr x in
        <:module_expr< $x$ $meApp_of_list xs$ >> ];

  value rec ceAnd_of_list =
    fun
    [ [] -> <:class_expr@ghost<>>
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_class_expr x in
        <:class_expr< $x$ and $ceAnd_of_list xs$ >> ];

  value rec ctAnd_of_list =
    fun
    [ [] -> <:class_type@ghost<>>
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_class_type x in
        <:class_type< $x$ and $ctAnd_of_list xs$ >> ];

  value rec cgSem_of_list =
    fun
    [ [] -> <:class_sig_item@ghost<>>
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_class_sig_item x in
        <:class_sig_item< $x$; $cgSem_of_list xs$ >> ];

  value rec crSem_of_list =
    fun
    [ [] -> <:class_str_item@ghost<>>
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_class_str_item x in
        <:class_str_item< $x$; $crSem_of_list xs$ >> ];

  value rec paSem_of_list =
    fun
    [ [] -> <:patt@ghost<>>
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_patt x in
        <:patt< $x$; $paSem_of_list xs$ >> ];

  value rec paCom_of_list =
    fun
    [ [] -> <:patt@ghost<>>
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_patt x in
        <:patt< $x$, $paCom_of_list xs$ >> ];

  value rec biSem_of_list =
    fun
    [ [] -> <:binding@ghost<>>
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_binding x in
        <:binding< $x$; $biSem_of_list xs$ >> ];

  value rec exSem_of_list =
    fun
    [ [] -> <:expr@ghost<>>
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_expr x in
        <:expr< $x$; $exSem_of_list xs$ >> ];

  value rec exCom_of_list =
    fun
    [ [] -> <:expr@ghost<>>
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_expr x in
        <:expr< $x$, $exCom_of_list xs$ >> ];

  value ty_of_stl =
    fun
    [ (_loc, s, []) -> <:ctyp< $uid:s$ >>
    | (_loc, s, tl) -> <:ctyp< $uid:s$ of $tyAnd_of_list tl$ >> ];

  value ty_of_sbt =
    fun
    [ (_loc, s, True, t) -> <:ctyp< $lid:s$ : mutable $t$ >>
    | (_loc, s, False, t) -> <:ctyp< $lid:s$ : $t$ >> ];

  value bi_of_pe (p, e) = let _loc = loc_of_patt p in <:binding< $p$ = $e$ >>;
  value sum_type_of_list l = tyOr_of_list (List.map ty_of_stl l);
  value record_type_of_list l = tySem_of_list (List.map ty_of_sbt l);
  value binding_of_pel l = biAnd_of_list (List.map bi_of_pe l);

  value rec pel_of_binding =
    fun
    [ <:binding< $b1$ and $b2$ >> -> pel_of_binding b1 @ pel_of_binding b2
    | <:binding< $p$ = $e$ >> -> [(p, e)]
    | <:binding< $b1$ ; $b2$ >> -> pel_of_binding b1 @ pel_of_binding b2
    | t -> assert False ];

  value rec list_of_binding x acc =
    match x with
    [ <:binding< $b1$ and $b2$ >> | <:binding< $b1$; $b2$ >> ->
         list_of_binding b1 (list_of_binding b2 acc)
    | t -> [t :: acc] ];

  value rec list_of_with_constr x acc =
    match x with
    [ <:with_constr< $w1$ and $w2$ >> ->
         list_of_with_constr w1 (list_of_with_constr w2 acc)
    | t -> [t :: acc] ];

  value rec list_of_ctyp x acc =
    match x with
    [ <:ctyp<>> -> acc
    | <:ctyp< $x$ & $y$ >> | <:ctyp< $x$, $y$ >> |
      <:ctyp< $x$ * $y$ >> | <:ctyp< $x$; $y$ >> |
      <:ctyp< $x$ and $y$ >> | <:ctyp< $x$ | $y$ >> ->
        list_of_ctyp x (list_of_ctyp y acc)
    | x -> [x :: acc] ];

  value rec list_of_patt x acc =
    match x with
    [ <:patt<>> -> acc
    | <:patt< $x$, $y$ >> | <:patt< $x$; $y$ >> ->
        list_of_patt x (list_of_patt y acc)
    | x -> [x :: acc] ];

  value rec list_of_expr x acc =
    match x with
    [ <:expr<>> -> acc
    | <:expr< $x$, $y$ >> | <:expr< $x$; $y$ >> ->
        list_of_expr x (list_of_expr y acc)
    | x -> [x :: acc] ];

  value rec list_of_str_item x acc =
    match x with
    [ <:str_item<>> -> acc
    | <:str_item< $x$; $y$ >> ->
        list_of_str_item x (list_of_str_item y acc)
    | x -> [x :: acc] ];

  value rec list_of_sig_item x acc =
    match x with
    [ <:sig_item<>> -> acc
    | <:sig_item< $x$; $y$ >> ->
        list_of_sig_item x (list_of_sig_item y acc)
    | x -> [x :: acc] ];

  value rec list_of_class_sig_item x acc =
    match x with
    [ <:class_sig_item<>> -> acc
    | <:class_sig_item< $x$; $y$ >> ->
        list_of_class_sig_item x (list_of_class_sig_item y acc)
    | x -> [x :: acc] ];

  value rec list_of_class_str_item x acc =
    match x with
    [ <:class_str_item<>> -> acc
    | <:class_str_item< $x$; $y$ >> ->
        list_of_class_str_item x (list_of_class_str_item y acc)
    | x -> [x :: acc] ];

  value rec list_of_class_type x acc =
    match x with
    [ <:class_type< $x$ and $y$ >> ->
        list_of_class_type x (list_of_class_type y acc)
    | x -> [x :: acc] ];

  value rec list_of_class_expr x acc =
    match x with
    [ <:class_expr< $x$ and $y$ >> ->
        list_of_class_expr x (list_of_class_expr y acc)
    | x -> [x :: acc] ];

  value rec list_of_module_expr x acc =
    match x with
    [ <:module_expr< $x$ $y$ >> ->
        list_of_module_expr x (list_of_module_expr y acc)
    | x -> [x :: acc] ];

  value rec list_of_assoc x acc =
    match x with
    [ <:assoc<>> -> acc
    | <:assoc< $x$ | $y$ >> ->
        list_of_assoc x (list_of_assoc y acc)
    | x -> [x :: acc] ];

  value rec list_of_ident x acc =
    match x with
    [ <:ident< $x$ . $y$ >> | <:ident< $x$ $y$ >> ->
        list_of_ident x (list_of_ident y acc)
    | x -> [x :: acc] ];

  value rec list_of_module_binding x acc =
    match x with
    [ <:module_binding< $x$ and $y$ >> ->
        list_of_module_binding x (list_of_module_binding y acc)
    | x -> [x :: acc] ];

end;

#use "Camlp4/Struct/Camlp4Ast.tmp.ml";
