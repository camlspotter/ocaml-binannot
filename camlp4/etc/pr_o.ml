(* camlp4r q_MLast.cmo ./pa_extfun.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Pcaml;
open Spretty;
open Stdpp;

value no_ss = ref True;

value not_impl name x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  HVbox [: `S NO ("<pr_o: not impl: " ^ name ^ "; " ^ desc ^ ">") :]
;

value apply_it l f =
  apply_it_f l where rec apply_it_f =
    fun
    [ [] -> f
    | [a :: l] -> a (apply_it_f l) ]
;

value rec list elem =
  fun
  [ [] -> fun _ k -> k
  | [x] -> fun dg k -> [: `elem x dg k :]
  | [x :: l] -> fun dg k -> [: `elem x "" [: :]; list elem l dg k :] ]
;

value rec listws elem sep el dg k =
  match el with
  [ [] -> k
  | [x] -> [: `elem x dg k :]
  | [x :: l] ->
      let sdg =
        match sep with
        [ S _ x -> x
        | _ -> "" ]
      in
      [: `elem x sdg [: `sep :]; listws elem sep l dg k :] ]
;

value rec listwbws elem b sep el dg k =
  match el with
  [ [] -> [: b; k :]
  | [x] -> [: `elem b x dg k :]
  | [x :: l] ->
      let sdg =
        match sep with
        [ S _ x -> x
        | _ -> "" ]
      in
      [: `elem b x sdg [: :]; listwbws elem [: `sep :] sep l dg k :] ]
;

value level box elem next e dg k =
  let rec curr e dg k = elem curr next e dg k in
  box (curr e dg k)
;

value is_infix =
  let infixes = Hashtbl.create 73 in
  do {
    List.iter (fun s -> Hashtbl.add infixes s True)
      ["=="; "!="; "+"; "+."; "-"; "-."; "*"; "*."; "/"; "/."; "**"; "**.";
       "="; "=."; "<>"; "<>."; "<"; "<."; ">"; ">."; "<="; "<=."; ">="; ">=.";
       "^"; "@"; "asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or";
       "quo"; "&&"; "||"; "~-"; "~-."];
    fun s -> try Hashtbl.find infixes s with [ Not_found -> False ]
  }
;

value is_keyword =
  let keywords = Hashtbl.create 301 in
  do {
    List.iter (fun s -> Hashtbl.add keywords s True)
      ["!"; "!="; "#"; "$"; "%"; "&"; "&&"; "'"; "("; ")"; "*"; "**"; "+";
       ","; "-"; "-."; "->"; "."; ".."; "/"; ":"; "::"; ":="; ":>"; ";"; ";;";
       "<"; "<-"; "<="; "<>"; "="; "=="; ">"; ">="; ">]"; ">}"; "?"; "??";
       "@"; "["; "[<"; "[|"; "]"; "^"; "_"; "`"; "and"; "as"; "asr"; "assert";
       "begin"; "class"; "constraint"; "do"; "done"; "downto"; "else"; "end";
       "exception"; "external"; "false"; "for"; "fun"; "function"; "functor";
       "if"; "in"; "include"; "inherit"; "initializer"; "land"; "lazy"; "let";
       "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"; "mod"; "module";
       "mutable"; "new"; "object"; "of"; "open"; "or"; "parser"; "private";
       "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val";
       "virtual"; "when"; "while"; "with"; "{"; "{<"; "|"; "|]"; "||"; "}";
       "~"; "~-"; "~-."];
    fun s -> try Hashtbl.find keywords s with [ Not_found -> False ]
  }
;

value has_special_chars v =
  match v.[0] with
  [ 'a'..'z' | 'A'..'Z' | '\192'..'\214' | '\216'..'\246' | '\248'..'\255' |
    '_' ->
      False
  | _ ->
      if String.length v >= 2 && v.[0] == '<' &&
         (v.[1] == '<' || v.[1] == ':')
      then
        False
      else True ]
;

value var_escaped v =
  if v = "" then "$lid:\"\"$"
  else if has_special_chars v || is_infix v then "( " ^ v ^ " )"
  else if is_keyword v then v ^ "__"
  else v
;

value flag n f = if f then [: `S LR n :] else [: :];

value conv_con =
  fun
  [ "True" -> "true"
  | "False" -> "false"
  | " True" -> "True"
  | " False" -> "False"
  | x -> x ]
;

value conv_lab =
  fun
  [ "val" -> "contents"
  | x -> var_escaped x ]
;

(* default global loc *)

value loc = (0, 0);

value id_var s =
  if has_special_chars s || is_infix s then
    HVbox [: `S LR "("; `S LR s; `S LR ")" :]
  else if is_keyword s then HVbox [: `S LR (s ^ "__") :]
  else HVbox [: `S LR s :]
;

value virtual_flag =
  fun
  [ True -> [: `S LR "virtual" :]
  | _ -> [: :] ]
;

value rec_flag =
  fun
  [ True -> [: `S LR "rec" :]
  | _ -> [: :] ]
;

(* extensible printers *)

value sig_item x dg k =
  let k = if no_ss.val then k else [: `S RO ";;"; k :] in
  pr_sig_item.pr_fun "top" x "" k
;
value str_item x dg k =
  let k = if no_ss.val then k else [: `S RO ";;"; k :] in
  pr_str_item.pr_fun "top" x "" k
;
value expr e dg k = pr_expr.pr_fun "top" e dg k;
value patt e dg k = pr_patt.pr_fun "top" e dg k;
value expr1 e dg k = pr_expr.pr_fun "expr1" e dg k;
value simple_expr e dg k = pr_expr.pr_fun "simple" e dg k;
value patt1 e dg k = pr_patt.pr_fun "patt1" e dg k;
value simple_patt e dg k = pr_patt.pr_fun "simple" e dg k;
value ctyp e dg k = pr_ctyp.pr_fun "top" e dg k;
value simple_ctyp e dg k = pr_ctyp.pr_fun "simple" e dg k;
value expr_fun_args ge = Extfun.apply pr_expr_fun_args.val ge;
value class_str_item x dg k = pr_class_str_item.pr_fun "" x "" k;

(* type core *)

value mutable_flag =
  fun
  [ True -> [: `S LR "mutable" :]
  | _ -> [: :] ]
;

value private_flag =
  fun
  [ True -> [: `S LR "private" :]
  | _ -> [: :] ]
;

value rec labels loc b vl _ k =
  match vl with
  [ [] -> [: b; k :]
  | [v] ->
      [: `label True b v "" k; `LocInfo (snd loc, snd loc) (HVbox [: :]) :]
  | [v :: l] ->
      [: `label False b v "" [: :]; labels loc [: :] l "" k :] ]
and label is_last b (loc, f, m, t) _ k =
  let m = flag "mutable" m in
  let k = [: if is_last then [: :] else [: `S RO ";" :]; k :] in
  Hbox
    [: `LocInfo loc
          (HVbox
             [: `HVbox [: b; m; `S LR (conv_lab f); `S LR ":" :];
                `ctyp t "" [: :] :]);
       k :]
;

value rec ctyp_list tel _ k = listws simple_ctyp (S LR "*") tel "" k;

value rec variants loc b vl dg k =
  match vl with
  [ [] -> [: b; k :]
  | [v] ->
      [: `variant b v "" k; `LocInfo (snd loc, snd loc) (HVbox [: :]) :]
  | [v :: l] ->
      [: `variant b v "" [: :]; variants loc [: `S LR "|" :] l "" k :] ]
and variant b (loc, c, tl) _ k =
  match tl with
  [ [] ->
      HVbox [: `LocInfo loc (HVbox b); `HOVbox [: `S LR c; k :] :]
  | _ ->
      HVbox
        [: `LocInfo loc (HVbox b);
           `HOVbox [: `S LR c; `S LR "of"; ctyp_list tl "" k :] :] ]
;

value rec row_fields b rfl _ k = listwbws row_field b (S LR "|") rfl "" k
and row_field b rf _ k =
  match rf with
  [ MLast.RfTag c ao tl ->
      let c = "`" ^ c in
      match tl with
      [ [] -> HVbox [: b; `HOVbox [: `S LR c; k :] :]
      | _ ->
          let ao = if ao then [: `S LR "&" :] else [: :] in
          HVbox
            [: b;
               `HOVbox [: `S LR c; `S LR "of"; ao; ctyp_list tl "" k :] :] ]
  | MLast.RfInh t -> HVbox [: b; `ctyp t "" k :] ]
;

value rec get_type_args t tl =
  match t with
  [ <:ctyp< $t1$ $t2$ >> -> get_type_args t1 [t2 :: tl]
  | _ -> (t, tl) ]
;

value module_pref =
  apply_it
    [level (fun x -> HOVbox x)
       (fun curr next t _ k ->
          match t with
          [ <:ctyp< $t1$ $t2$ >> ->
              let (t, tl) = get_type_args t1 [t2] in
              [: curr t "" [: :];
                 list
                   (fun t _ k ->
                      HOVbox [: `S NO "("; curr t "" [: :]; `S RO ")"; k :])
                   tl "" k :]
          | <:ctyp< $t1$ . $t2$ >> ->
              [: curr t1 "" [: `S NO "." :]; `next t2 "" k :]
          | _ -> [: `next t "" k :] ])]
    simple_ctyp
;

value rec class_longident sl dg k =
  match sl with
  [ [i] -> HVbox [: `S LR i; k :]
  | [m :: sl] -> HVbox [: `S LR m; `S NO "."; `class_longident sl dg k :]
  | _ -> HVbox [: `not_impl "class_longident" sl; k :] ]
;

value rec clty_longident sl dg k =
  match sl with
  [ [i] -> HVbox [: `S LR i; k :]
  | [m :: sl] -> HVbox [: `S LR m; `S NO "."; `clty_longident sl dg k :]
  | _ -> HVbox [: `not_impl "clty_longident" sl; k :] ]
;

value rec meth_list (ml, v) dg k =
  match (ml, v) with
  [ ([f], False) -> [: `field f dg k :]
  | ([], _) -> [: `S LR ".."; k :]
  | ([f :: ml], v) ->
      [: `field f "" [: `S RO ";" :]; meth_list (ml, v) dg k :] ]
and field (lab, t) dg k =
  HVbox [: `S LR (var_escaped lab); `S LR ":"; `ctyp t dg k :];

(* patterns *)

value rec get_patt_args a al =
  match a with
  [ <:patt< $a1$ $a2$ >> -> get_patt_args a1 [a2 :: al]
  | _ -> (a, al) ]
;

value rec is_irrefut_patt =
  fun
  [ <:patt< $lid:_$ >> -> True
  | <:patt< () >> -> True
  | <:patt< _ >> -> True
  | <:patt< ($x$ as $y$) >> -> is_irrefut_patt x && is_irrefut_patt y
  | <:patt< { $list:fpl$ } >> ->
      List.for_all (fun (_, p) -> is_irrefut_patt p) fpl
  | <:patt< ($p$ : $_$) >> -> is_irrefut_patt p
  | <:patt< ($list:pl$) >> -> List.for_all is_irrefut_patt pl
  | <:patt< ? $_$ : ($p$) >> -> is_irrefut_patt p
  | <:patt< ? $_$ : ($p$ = $_$) >> -> is_irrefut_patt p
  | <:patt< ~ $_$ : $p$ >> -> is_irrefut_patt p
  | _ -> False ]
;

(* expressions *)

pr_expr_fun_args.val :=
  extfun Extfun.empty with
  [ <:expr< fun [$p$ -> $e$] >> as ge ->
      if is_irrefut_patt p then
        let (pl, e) = expr_fun_args e in
        ([p :: pl], e)
      else ([], ge)
  | ge -> ([], ge) ];

value raise_match_failure (bp, ep) k =
  HOVbox
    [: `S LR "raise"; `S LO "("; `S LR "Match_failure"; `S LO "(";
       `S LR ("\"" ^ Pcaml.input_file.val ^ "\""); `S RO ",";
       `S LR (string_of_int bp); `S RO ","; `S LR (string_of_int ep);
       `S RO ")"; `S RO ")"; k :]
;

value rec bind_list b pel _ k =
  match pel with
  [ [pe] -> let_binding b pe "" k
  | pel ->
      Vbox [: `HVbox [: :]; listwbws let_binding b (S LR "and") pel "" k :] ]
and let_binding b (p, e) _ k =
  let loc =
    let (bp1, ep1) = MLast.loc_of_patt p in
    let (bp2, ep2) = MLast.loc_of_expr e in
    (min bp1 bp2, max ep1 ep2)
  in
  LocInfo loc (BEbox (let_binding0 b p e k))
and let_binding0 b p e k =
  let (pl, e) =
    match p with
    [ <:patt< ($_$ : $_$) >> -> ([], e)
    | _ -> expr_fun_args e ]
  in
  let b = [: b; `simple_patt p "" [: :] :] in
  match (p, e) with
  [ (<:patt< $lid:_$ >>, <:expr< ($e$ : $t$) >>) ->
      [: `HVbox
            [: `HVbox b; `HVbox (list simple_patt pl "" [: `S LR ":" :]);
               `ctyp t "" [: `S LR "=" :] :];
         `expr e "" [: :]; k :]
  | _ ->
      [: `HVbox
            [: `HVbox b; `HOVbox (list simple_patt pl "" [: `S LR "=" :]) :];
         `expr e "" [: :]; k :] ]
and match_assoc_list loc pel dg k =
  match pel with
  [ [] ->
      HVbox
        [: `HVbox [: `S LR "_"; `S LR "->" :]; `raise_match_failure loc k :]
  | _ ->
      BEVbox
        [: `HVbox [: :]; listwbws match_assoc [: :] (S LR "|") pel "" k :] ]
and match_assoc b (p, w, e) dg k =
  let s =
    match w with
    [ Some e1 ->
        [: `HVbox
              [: `HVbox [: :]; `patt p "" [: :];
                 `HVbox [: `S LR "when"; `expr e1 "" [: `S LR "->" :] :] :] :]
    | _ -> [: `patt p "" [: `S LR "->" :] :] ]
  in
  HVbox [: b; `HVbox [: `HVbox s; `expr e dg k :] :]
;

value rec get_expr_args a al =
  match a with
  [ <:expr< $a1$ $a2$ >> -> get_expr_args a1 [a2 :: al]
  | _ -> (a, al) ]
;

value label lab = S LR (var_escaped lab);

value field_expr (lab, e) dg k =
  HVbox [: `label lab; `S LR "="; `expr e dg k :]
;

value type_params sl _ k =
  match sl with
  [ [] -> k
  | [(s, vari)] ->
      let b =
        match vari with
        [ (True, False) -> [: `S LO "+" :]
        | (False, True) -> [: `S LO "-" :]
        | _ -> [: :] ]
      in
      [: b; `S LO "'"; `S LR s; k :]
  | sl ->
      [: `S LO "(";
         listws (fun (s, _) _ k -> HVbox [: `S LO "'"; `S LR s; k :])
           (S RO ",") sl "" [: `S RO ")"; k :] :] ]
;

value constrain (t1, t2) _ k =
  HVbox [: `S LR "constraint"; `ctyp t1 "" [: `S LR "=" :]; `ctyp t2 "" k :]
;

value type_list b tdl _ k =
  HVbox
    [: `HVbox [: :];
       listwbws
         (fun b ((_, tn), tp, te, cl) _ k ->
            let tn = var_escaped tn in
            let cstr = list constrain cl "" k in
            match te with
            [ <:ctyp< '$s$ >> when not (List.mem_assoc s tp) ->
                HVbox [: b; type_params tp "" [: :]; `S LR tn; cstr :]
            | <:ctyp< [ $list:[]$ ] >> ->
                HVbox [: b; type_params tp "" [: :]; `S LR tn; cstr :]
            | _ ->
                HVbox
                  [: `HVbox
                        [: b; type_params tp "" [: :]; `S LR tn; `S LR "=" :];
                     `ctyp te "" [: :]; cstr :] ])
         b (S LR "and") tdl "" [: :];
       k :]
;

value external_def (s, t, pl) _ k =
  let ls =
    list (fun s _ k -> HVbox [: `S LR ("\"" ^ s ^ "\""); k :]) pl "" k
  in
  HVbox
    [: `HVbox [: `S LR "external"; `S LR (var_escaped s); `S LR ":" :];
       `ctyp t "" [: `S LR "="; ls :] :]
;

value value_description (s, t) _ k =
  HVbox
    [: `HVbox [: `S LR "val"; `S LR (var_escaped s); `S LR ":" :];
       `ctyp t "" k :]
;

value typevar s _ k = HVbox [: `S LR ("'" ^ s); k :];

value rec mod_ident sl _ k =
  match sl with
  [ [] -> k
  | [s] -> [: `S LR s; k :]
  | [s :: sl] -> [: `S LR s; `S NO "."; mod_ident sl "" k :] ]
;

value rec module_type mt k =
  let next = module_type1 in
  let rec curr mt k =
    match mt with
    [ <:module_type< functor ( $s$ : $mt1$ ) -> $mt2$ >> ->
        let head =
          HVbox
            [: `S LR "functor"; `S LO "("; `S LR s; `S LR ":";
               `HVbox (curr mt1 [: `S RO ")" :]); `S LR "->" :]
        in
        [: `head; `module_type mt2 k :]
    | _ -> [: `next mt k :] ]
  in
  HVbox (curr mt k)
and module_type1 mt k =
  let curr = module_type1 in
  let next = module_type2 in
  match mt with
  [ <:module_type< $mt$ with $list:icl$ >> ->
      HVbox
        [: `curr mt [: :]; `with_constraints [: `S LR "with" :] icl "" k :]
  | _ -> next mt k ]
and module_type2 mt k =
  let next = module_type3 in
  let rec curr mt k =
    match mt with
    [ <:module_type< sig $list:s$ end >> ->
        let ep = snd (MLast.loc_of_module_type mt) in
        [: `BEbox
              [: `S LR "sig";
                 `HVbox
                    [: `HVbox [: :]; list sig_item s "" [: :];
                       `LocInfo (ep, ep) (HVbox [: :]) :];
                 `HVbox [: `S LR "end"; k :] :] :]
    | _ -> [: `next mt k :] ]
  in
  HVbox (curr mt k)
and module_type3 mt k =
  let curr = module_type3 in
  let next = module_type5 in
  match mt with
  [ <:module_type< $mt1$ $mt2$ >> ->
      HVbox [: `curr mt1 [: :]; `S LO "("; `next mt2 [: `S RO ")"; k :] :]
  | <:module_type< $mt1$ . $mt2$ >> ->
      HVbox [: `curr mt1 [: `S NO "." :]; `next mt2 k :]
  | _ -> next mt k ]
and module_type5 mt k =
  match mt with
  [ <:module_type< $lid:s$ >> -> HVbox [: `S LR s; k :]
  | <:module_type< $uid:s$ >> -> HVbox [: `S LR s; k :]
  | _ -> HVbox [: `S LO "("; `module_type mt [: `S RO ")"; k :] :] ]
and module_declaration b mt k =
  match mt with
  [ <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ->
      module_declaration
        [: `HVbox
             [: b;
                `HVbox
                  [: `S LO "("; `S LR i; `S LR ":";
                     `module_type t [: `S RO ")" :] :] :] :]
        mt k
  | _ ->
      HVbox
        [: `HVbox [: :];
           `HVbox [: `HVbox [: b; `S LR ":" :]; `module_type mt [: :] :];
           k :] ]
and modtype_declaration (s, mt) _ k =
  match mt with
  [ <:module_type< ' $_$ >> ->
      HVbox [: `HVbox [: `S LR "module"; `S LR "type"; `S LR s; k :] :]
  | _ ->
      HVbox
        [: `HVbox [: :];
           `HVbox
              [: `HVbox [: `S LR "module"; `S LR "type"; `S LR s; `S LR "=" :];
                 `module_type mt [: :] :];
           k :] ]
and with_constraints b icl _ k =
  HVbox [: `HVbox [: :]; listwbws with_constraint b (S LR "and") icl "" k :]
and with_constraint b wc _ k =
  match wc with
  [ MLast.WcTyp _ p al e ->
      let params =
        match al with
        [ [] -> [: :]
        | [s] -> [: `S LO "'"; `S LR (fst s) :]
        | sl -> [: `S LO "("; type_params sl "" [: `S RO ")" :] :] ]
      in
      HVbox
        [: `HVbox
              [: `HVbox b; `S LR "type"; params;
                 mod_ident p "" [: `S LR "=" :] :];
           `ctyp e "" k :]
  | MLast.WcMod _ sl me ->
      HVbox
        [: b; `S LR "module"; mod_ident sl "" [: `S LR "=" :];
           `module_expr me "" k :] ]
and module_expr me _ k =
  let next = module_expr1 in
  let rec curr me dg k =
    match me with
    [ <:module_expr< struct $list:s$ end >> ->
        let ep = snd (MLast.loc_of_module_expr me) in
        [: `HVbox [: :];
           `HVbox
              [: `S LR "struct"; list str_item s "" [: :];
                 `LocInfo (ep, ep) (HVbox [: :]) :];
           `HVbox [: `S LR "end"; k :] :]
    | <:module_expr< functor ($s$ : $mt$) -> $me$ >> ->
        let head =
          HVbox
            [: `S LR "functor"; `S LO "("; `S LR s; `S LR ":";
               `module_type mt [: `S RO ")" :]; `S LR "->" :]
        in
        [: `head; curr me "" k :]
    | _ -> [: `next me "" k :] ]
  in
  HVbox (curr me "" k)
and module_expr1 me _ k =
  let next = module_expr2 in
  let rec curr me dg k =
    match me with
    [ <:module_expr< $me1$ $me2$ >> ->
        [: curr me1 "" [: :];
           `HVbox [: `S LO "("; `module_expr me2 "" [: `S RO ")"; k :] :] :]
    | _ -> [: `next me "" k :] ]
  in
  HOVbox (curr me "" k)
and module_expr2 me _ k =
  let curr = module_expr2 in
  let next = module_expr3 in
  match me with
  [ <:module_expr< $me1$ . $me2$ >> ->
      HVbox [: `curr me1 "" [: `S NO "." :]; `next me2 "" k :]
  | _ -> next me "" k ]
and module_expr3 me _ k =
  match me with
  [ <:module_expr< $uid:s$ >> -> HVbox [: `S LR s; k :]
  | <:module_expr< ( $me$ : $mt$ ) >> ->
      HVbox
        [: `S LO "("; `module_expr me "" [: `S LR ":" :];
           `module_type mt [: `S RO ")"; k :] :]
  | <:module_expr< struct $list:_$ end >> ->
      HVbox [: `S LO "("; `module_expr me "" [: `S RO ")"; k :] :]
  | x -> not_impl "module_expr2" x ]
and module_binding b me k =
  match me with
  [ <:module_expr< functor ($s$ : $mt$) -> $mb$ >> ->
      module_binding
        [: `HVbox
              [: b;
                 `HVbox
                    [: `HVbox [: `S LO "("; `S LR s; `S LR ":" :];
                       `module_type mt [: `S RO ")" :] :] :] :]
        mb k
  | <:module_expr< ( $me$ : $mt$ ) >> ->
      HVbox
        [: `HVbox [: :];
           `HVbox
              [: `HVbox
                    [: `HVbox [: b; `S LR ":" :];
                       `module_type mt [: `S LR "=" :] :];
                 `module_expr me "" [: :] :];
           k :]
  | _ ->
      HVbox
        [: `HVbox [: :];
           `HVbox [: `HVbox [: b; `S LR "=" :]; `module_expr me "" [: :] :];
           k :] ]
and class_declaration b ci _ k =
  class_fun_binding
    [: b; virtual_flag ci.MLast.ciVir; class_type_parameters ci.MLast.ciPrm;
       `S LR ci.MLast.ciNam :]
    ci.MLast.ciExp k
and class_fun_binding b ce k =
  match ce with
  [ MLast.CeFun _ p cfb ->
      class_fun_binding [: b; `simple_patt p "" [: :] :] cfb k
  | ce -> HVbox [: `HVbox [: b; `S LR "=" :]; `class_expr ce k :] ]
and class_type_parameters (loc, tpl) =
  match tpl with
  [ [] -> [: :]
  | tpl ->
      [: `S LO "[";
         listws type_parameter (S RO ",") tpl "" [: `S RO "]" :] :] ]
and type_parameter tp dg k = HVbox [: `S LO "'"; `S LR (fst tp); k :]
and class_expr ce k =
  match ce with
  [ MLast.CeFun _ p ce ->
      HVbox
        [: `S LR "fun"; `simple_patt p "" [: `S LR "->" :];
           `class_expr ce k :]
  | MLast.CeLet _ rf lb ce ->
      Vbox
        [: `HVbox [: :];
           `bind_list [: `S LR "let"; rec_flag rf :] lb "" [: `S LR "in" :];
           `class_expr ce k :]
  | ce -> class_expr1 ce k ]
and class_expr1 ce k =
  match ce with
  [ MLast.CeApp _ ce e ->
      HVbox [: `class_expr1 ce [: :]; `simple_expr e "" k :]
  | ce -> class_expr2 ce k ]
and class_expr2 ce k =
  match ce with
  [ MLast.CeCon _ ci [] -> class_longident ci "" k
  | MLast.CeCon _ ci ctcl ->
      HVbox
        [: `S LO "["; listws ctyp (S RO ",") ctcl "" [: `S RO "]" :];
           `class_longident ci "" k :]
  | MLast.CeStr _ csp cf ->
      let ep = snd (MLast.loc_of_class_expr ce) in
      BEbox
        [: `HVbox [: `S LR "object"; `class_self_patt_opt csp :];
           `HVbox
              [: `HVbox [: :];
                 list class_str_item cf "" [: :];
                 `LocInfo (ep, ep) (HVbox [: :]) :];
           `HVbox [: `S LR "end"; k :] :]
  | MLast.CeTyc _ ce ct ->
      HVbox
        [: `S LO "("; `class_expr ce [: `S LR ":" :];
           `class_type ct [: `S RO ")"; k :] :]
  | MLast.CeFun _ _ _ ->
      HVbox [: `S LO "("; `class_expr ce [: `S RO ")"; k :] :]
  | _ -> HVbox [: `not_impl "class_expr" ce; k :] ]
and class_self_patt_opt csp =
  match csp with
  [ Some p -> HVbox [: `S LO "("; `patt p "" [: `S RO ")" :] :]
  | None -> HVbox [: :] ]
and cvalue b (lab, mf, e) k =
  HVbox
    [: `HVbox [: b; mutable_flag mf; `label lab; `S LR "=" :]; `expr e "" k :]
and fun_binding b fb k =
  match fb with
  [ <:expr< fun $p$ -> $e$ >> ->
      fun_binding [: b; `simple_patt p "" [: :] :] e k
  | e -> HVbox [: `HVbox [: b; `S LR "=" :]; `expr e "" k :] ]
and class_type ct k =
  match ct with
  [ MLast.CtFun _ t ct ->
      HVbox [: `ctyp t "" [: `S LR "->" :]; `class_type ct k :]
  | _ -> class_signature ct k ]
and class_signature cs k =
  match cs with
  [ MLast.CtCon _ id [] -> clty_longident id "" k
  | MLast.CtCon _ id tl ->
      HVbox
        [: `S LO "["; listws ctyp (S RO ",") tl "" [: `S RO "]" :];
           `clty_longident id "" k :]
  | MLast.CtSig _ cst csf ->
      let ep = snd (MLast.loc_of_class_type cs) in
      class_self_type [: `S LR "object" :] cst
        [: `HVbox
              [: `HVbox [: :];
                 list class_sig_item csf "" [: :];
                 `LocInfo (ep, ep) (HVbox [: :]) :];
           `HVbox [: `S LR "end"; k :] :]
  | _ -> HVbox [: `not_impl "class_signature" cs; k :] ]
and class_self_type b cst k =
  BEbox
    [: `HVbox
          [: b;
             match cst with
             [ None -> [: :]
             | Some t -> [: `S LO "("; `ctyp t "" [: `S RO ")" :] :] ] :];
       k :]
and class_sig_item csf dg k =
  let rec curr csf dg k =
    match csf with
    [ MLast.CgCtr _ t1 t2 ->
        [: `S LR "constraint"; `ctyp t1 "" [: `S LR "=" :]; `ctyp t2 "" k :]
    | MLast.CgDcl _ s ->
        [: `HVbox [: :]; list class_sig_item s "" [: :] :]
    | MLast.CgInh _ ce -> [: `S LR "inherit"; `class_type ce k :]
    | MLast.CgMth _ lab pf t ->
        [: `HVbox
              [: `S LR "method"; private_flag pf; `label lab; `S LR ":" :];
           `ctyp t "" k :]
    | MLast.CgVal _ lab mf t ->
        [: `HVbox [: `S LR "val"; mutable_flag mf; `label lab; `S LR ":" :];
           `ctyp t "" k :]
    | MLast.CgVir _ lab pf t ->
        [: `HVbox
              [: `S LR "method"; `S LR "virtual"; private_flag pf;
                 `label lab; `S LR ":" :];
           `ctyp t "" k :] ]
  in
  LocInfo (MLast.loc_of_class_sig_item csf) (HVbox (curr csf dg k))
and class_description b ci _ k =
  HVbox
    [: `HVbox
          [: b; virtual_flag ci.MLast.ciVir;
             class_type_parameters ci.MLast.ciPrm; `S LR ci.MLast.ciNam;
             `S LR ":" :];
       `class_type ci.MLast.ciExp k :]
and class_type_declaration b ci _ k =
  HVbox
    [: `HVbox
          [: b; virtual_flag ci.MLast.ciVir;
             class_type_parameters ci.MLast.ciPrm; `S LR ci.MLast.ciNam;
             `S LR "=" :];
       `class_signature ci.MLast.ciExp k :]
;

pr_sig_item.pr_levels :=
  [{pr_label = "top";
    pr_box s x = LocInfo (MLast.loc_of_sig_item s) (HVbox x);
    pr_rules =
      extfun Extfun.empty with
      [ <:sig_item< type $list:stl$ >> ->
          fun curr next dg k -> [: `type_list [: `S LR "type" :] stl "" k :]
      | <:sig_item< declare $list:s$ end >> ->
          fun curr next dg k ->
            if s = [] then [: `S LR "(* *)" :]
            else [: `HVbox [: :]; list sig_item s "" [: :] :]
      | MLast.SgDir _ _ _ as si ->
          fun curr next dg k -> [: `not_impl "sig_item" si :]
      | <:sig_item< exception $c$ of $list:tl$ >> ->
          fun curr next dg k ->
            [: `variant [: `S LR "exception" :] (loc, c, tl) "" k :]
      | <:sig_item< value $s$ : $t$ >> ->
          fun curr next dg k -> [: `value_description (s, t) "" k :]
      | <:sig_item< external $s$ : $t$ = $list:pl$ >> ->
          fun curr next dg k -> [: `external_def (s, t, pl) "" k :]
      | <:sig_item< include $mt$ >> ->
          fun curr next dg k -> [: `S LR "include"; `module_type mt k :]
      | <:sig_item< module $s$ : $mt$ >> ->
          fun curr next dg k ->
            [: `module_declaration [: `S LR "module"; `S LR s :] mt k :]
      | <:sig_item< module type $s$ = $mt$ >> ->
          fun curr next dg k -> [: `modtype_declaration (s, mt) "" k :]
      | <:sig_item< open $sl$ >> ->
          fun curr next dg k -> [: `S LR "open"; mod_ident sl "" k :]
      | MLast.SgCls _ cd ->
          fun curr next dg k ->
            [: `HVbox [: :];
               listwbws class_description [: `S LR "class" :] (S LR "and") cd
                 "" k :]
      | MLast.SgClt _ cd ->
          fun curr next dg k ->
            [: `HVbox [: :];
               listwbws class_type_declaration
                 [: `S LR "class"; `S LR "type" :] (S LR "and") cd ""
                 k :] ]}];

pr_str_item.pr_levels :=
  [{pr_label = "top";
    pr_box s x = LocInfo (MLast.loc_of_str_item s) (HVbox x);
    pr_rules =
      extfun Extfun.empty with
      [ <:str_item< open $i$ >> ->
          fun curr next dg k -> [: `S LR "open"; mod_ident i "" k :]
      | <:str_item< $exp:e$ >> ->
          fun curr next dg k ->
            if no_ss.val then
              [: `HVbox [: `S LR "let"; `S LR "_"; `S LR "=" :];
                 `expr e "" k :]
            else [: `HVbox [: :]; `expr e "" k :]
      | <:str_item< declare $list:s$ end >> ->
          fun curr next dg k ->
            if s = [] then [: `S LR "(* *)" :]
            else [: `HVbox [: :]; list str_item s "" [: :] :]
      | <:str_item< # $s$ $opt:x$ >> ->
          fun curr next dg k ->
            let s =
              "(* #" ^ s ^ " " ^
                (match x with
                 [ Some <:expr< $str:s$ >> -> "\"" ^ s ^ "\""
                 | _ -> "?" ]) ^
                " *)"
            in
            [: `S LR s :]
      | <:str_item< exception $c$ of $list:tl$ = $b$ >> ->
          fun curr next dg k ->
            match b with
            [ [] -> [: `variant [: `S LR "exception" :] (loc, c, tl) "" k :]
            | _ ->
                [: `variant [: `S LR "exception" :] (loc, c, tl) ""
                      [: `S LR "=" :];
                   mod_ident b "" k :] ]
      | <:str_item< include $me$ >> ->
          fun curr next dg k -> [: `S LR "include"; `module_expr me "" k :]
      | <:str_item< type $list:tdl$ >> ->
          fun curr next dg k -> [: `type_list [: `S LR "type" :] tdl "" k :]
      | <:str_item< value $rec:rf$ $list:pel$ >> ->
          fun curr next dg k ->
            [: `bind_list
                  [: `S LR "let"; if rf then [: `S LR "rec" :] else [: :] :]
                  pel "" k :]
      | <:str_item< external $s$ : $t$ = $list:pl$ >> ->
          fun curr next dg k -> [: `external_def (s, t, pl) "" k :]
      | <:str_item< module $s$ = $me$ >> ->
          fun curr next dg k ->
            [: `module_binding [: `S LR "module"; `S LR s :] me k :]
      | <:str_item< module type $s$ = $mt$ >> ->
          fun curr next dg k ->
            [: `HVbox [: :];
               `HVbox
                  [: `HVbox
                        [: `S LR "module"; `S LR "type"; `S LR s;
                           `S LR "=" :];
                     `module_type mt [: :] :];
               k :]
      | MLast.StCls _ cd ->
          fun curr next dg k ->
            [: `HVbox [: :];
               listwbws class_declaration [: `S LR "class" :] (S LR "and") cd
                 "" k :]
      | MLast.StClt _ cd ->
          fun curr next dg k ->
            [: `HVbox [: :];
               listwbws class_type_declaration
                 [: `S LR "class"; `S LR "type" :] (S LR "and") cd ""
                 k :] ]}];

value ocaml_char =
  fun
  [ "'" -> "\\'"
  | "\"" -> "\\\""
  | c -> c ]
;

pr_expr.pr_levels :=
  [{pr_label = "top"; pr_box e x = LocInfo (MLast.loc_of_expr e) (HOVbox x);
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< do { $list:el$ } >> ->
          fun curr next dg k ->
            [: `HVbox [: `HVbox [: :]; listws next (S RO ";") el dg k :] :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = "expr1"; pr_box e x = LocInfo (MLast.loc_of_expr e) (HOVbox x);
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< let $rec:r$ $p1$ = $e1$ in $e$ >> ->
          fun curr next dg k ->
            let r = if r then [: `S LR "rec" :] else [: :] in
            if dg <> ";" then
              [: `HVbox
                    [: `HVbox [: :];
                       `let_binding [: `S LR "let"; r :] (p1, e1) ""
                          [: `S LR "in" :];
                       `expr e dg k :] :]
            else
              let pel = [(p1, e1)] in
              [: `BEbox
                    [: `S LR "begin";
                       `HVbox
                          [: `HVbox [: :];
                             listwbws
                               (fun b (p, e) _ k -> let_binding b (p, e) "" k)
                               [: `S LR "let"; r :] (S LR "and") pel ""
                               [: `S LR "in" :];
                             `expr e "" [: :] :];
                       `HVbox [: `S LR "end"; k :] :] :]
      | <:expr< let $rec:r$ $list:pel$ in $e$ >> ->
          fun curr next dg k ->
            let r = if r then [: `S LR "rec" :] else [: :] in
            if dg <> ";" then
              [: `Vbox
                    [: `HVbox [: :];
                       listwbws
                         (fun b (p, e) _ k -> let_binding b (p, e) "" k)
                         [: `S LR "let"; r :] (S LR "and") pel ""
                         [: `S LR "in" :];
                       `expr e dg k :] :]
            else
              [: `BEbox
                    [: `S LR "begin";
                       `HVbox
                          [: `HVbox [: :];
                             listwbws
                               (fun b (p, e) _ k -> let_binding b (p, e) "" k)
                               [: `S LR "let"; r :] (S LR "and") pel ""
                               [: `S LR "in" :];
                             `expr e "" [: :] :];
                       `HVbox [: `S LR "end"; k :] :] :]
      | <:expr< let module $m$ = $mb$ in $e$ >> ->
          fun curr next dg k ->
            if dg <> ";" then
              [: `HVbox
                    [: `HVbox [: :];
                       `module_binding
                          [: `S LR "let"; `S LR "module"; `S LR m :] mb [: :];
                       `S LR "in"; `expr e dg k :] :]
            else
              [: `BEbox
                    [: `module_binding
                          [: `S LR "begin let"; `S LR "module"; `S LR m :] mb
                          [: :];
                       `HVbox
                          [: `HVbox [: :]; `S LR "in"; `expr e dg [: :] :];
                       `HVbox [: `S LR "end"; k :] :] :]
      | <:expr< fun [ $list:pel$ ] >> as e ->
          fun curr next dg k ->
            let loc = MLast.loc_of_expr e in
            if not (List.mem dg ["|"; ";"]) then
              match pel with
              [ [] ->
                  [: `S LR "fun"; `S LR "_"; `S LR "->";
                     `raise_match_failure loc k :]
              | [(p, None, e)] ->
                  let (pl, e) = expr_fun_args e in
                  [: `BEbox
                        [: `HOVbox
                              [: `S LR "fun";
                                 list simple_patt [p :: pl] ""
                                   [: `S LR "->" :] :];
                           `expr e "" k :] :]
              | _ ->
                  [: `Vbox
                        [: `HVbox [: :]; `S LR "function";
                           `match_assoc_list loc pel "" k :] :] ]
            else
              match pel with
              [ [] ->
                  [: `S LR "(fun"; `S LR "_"; `S LR "->";
                     `raise_match_failure loc [: `S RO ")"; k :] :]
              | [(p, None, e)] ->
                  if is_irrefut_patt p then
                    let (pl, e) = expr_fun_args e in
                    [: `S LO "(";
                       `BEbox
                          [: `HOVbox
                                [: `S LR "fun";
                                   list simple_patt [p :: pl] ""
                                     [: `S LR "->" :] :];
                             `expr e "" [: `S RO ")"; k :] :] :]
                  else
                    [: `HVbox
                          [: `S LR "fun ["; `patt p "" [: `S LR "->" :] :];
                       `expr e "" [: `S LR "]"; k :] :]
              | _ ->
                  [: `Vbox
                        [: `HVbox [: :]; `S LR "begin function";
                           `match_assoc_list loc pel "" k;
                           `HVbox [: `S LR "end"; k :] :] :] ]
      | <:expr< match $e$ with [ $list:pel$ ] >> as ge ->
          fun curr next dg k ->
            let loc = MLast.loc_of_expr ge in
            if not (List.mem dg ["|"; ";"]) then
              [: `HVbox
                    [: `HVbox [: :];
                       `BEbox
                          [: `S LR "match"; `expr e "" [: :]; `S LR "with" :];
                       `match_assoc_list loc pel "" k :] :]
            else
              [: `HVbox
                    [: `HVbox [: :];
                       `BEbox
                          [: `S LR "begin match"; `expr e "" [: :];
                             `S LR "with" :];
                       `match_assoc_list loc pel "" [: :];
                       `HVbox [: `S LR "end"; k :] :] :]
      | <:expr< try $e$ with [ $list:pel$ ] >> as ge ->
          fun curr next dg k ->
            let loc = MLast.loc_of_expr ge in
            if not (List.mem dg ["|"; ";"]) then
              [: `HVbox
                    [: `HVbox [: :];
                       `BEbox
                          [: `S LR "try"; `expr e "" [: :]; `S LR "with" :];
                       `match_assoc_list loc pel "" k :] :]
            else
              [: `HVbox
                    [: `HVbox [: :];
                       `BEbox
                          [: `S LR "begin try"; `expr e "" [: :];
                             `S LR "with" :];
                       `match_assoc_list loc pel "" [: :];
                       `HVbox [: `S LR "end"; k :] :] :]
      | <:expr< if $_$ then () else raise (Assert_failure $_$) >> as e ->
          fun curr next dg k -> [: `next e dg k :]
      | <:expr< if $e1$ then $e2$ else $e3$ >> as e ->
          fun curr next dg k ->
            let eel_e =
              elseif e3 where rec elseif e =
                match e with
                [ <:expr< if $e1$ then $e2$ else $e3$ >> ->
                    let (eel, e) = elseif e3 in
                    ([(e1, e2) :: eel], e)
                | _ -> ([], e) ]
            in
            if not (List.mem dg ["else"]) then
              match eel_e with
              [ ([], <:expr< () >>) ->
                  [: `BEbox [: `S LR "if"; `expr e1 "" [: :]; `S LR "then" :];
                     `expr1 e2 dg k :]
              | (eel, <:expr< () >>) ->
                  let (eel, (e1f, e2f)) =
                    let r = List.rev eel in
                    (List.rev (List.tl r), List.hd r)
                  in
                  [: `HVbox
                        [: `HVbox [: :];
                           `HVbox
                              [: `BEbox
                                    [: `S LR "if"; `expr e1 "" [: :];
                                       `S LR "then" :];
                                 `expr1 e2 "else" [: :] :];
                           list
                             (fun (e1, e2) _ k ->
                                HVbox
                                  [: `BEbox
                                        [: `HVbox
                                              [: `S LR "else"; `S LR "if" :];
                                           `expr e1 "" [: :];
                                           `S LR "then" :];
                                     `expr1 e2 "else" k :])
                             eel "" [: :];
                           `HVbox
                              [: `BEbox
                                    [: `HVbox [: `S LR "else"; `S LR "if" :];
                                       `expr e1f "" [: :];
                                       `S LR "then" :];
                                 `expr1 e2f dg k :] :] :]
              | (eel, e) ->
                  [: `HVbox
                        [: `HVbox [: :];
                           `HVbox
                              [: `BEbox
                                    [: `S LR "if"; `expr e1 "" [: :];
                                       `S LR "then" :];
                                 `expr1 e2 "else" [: :] :];
                           list
                             (fun (e1, e2) _ k ->
                                HVbox
                                  [: `BEbox
                                        [: `HVbox
                                              [: `S LR "else"; `S LR "if" :];
                                           `expr e1 "" [: :];
                                           `S LR "then" :];
                                     `expr1 e2 "else" k :])
                             eel "" [: :];
                           `HVbox [: `S LR "else"; `expr1 e dg k :] :] :] ]
            else
              match eel_e with
              [ (_, <:expr< () >>) -> [: `next e "" k :]
              | (eel, e) ->
                  [: `HVbox
                        [: `HVbox [: :];
                           `HVbox
                              [: `BEbox
                                    [: `S LR "if"; `expr e1 "" [: :];
                                       `S LR "then" :];
                                 `expr1 e2 "" [: :] :];
                           list
                             (fun (e1, e2) _ k ->
                                HVbox
                                  [: `BEbox
                                        [: `HVbox
                                              [: `S LR "else"; `S LR "if" :];
                                           `expr e1 "" [: :];
                                           `S LR "then" :];
                                     `expr1 e2 "" [: :] :])
                             eel "" [: :];
                           `HVbox [: `S LR "else"; `expr1 e "" k :] :] :] ]
      | <:expr< for $i$ = $e1$ $to:d$ $e2$ do { $list:el$ } >> ->
          fun curr next dg k ->
            let d = if d then "to" else "downto" in
            [: `BEbox
                  [: `HOVbox
                        [: `S LR "for"; `S LR i; `S LR "=";
                           `expr e1 "" [: `S LR d :];
                           `expr e2 "" [: `S LR "do" :] :];
                     `HVbox
                        [: `HVbox [: :];
                           listws expr (S RO ";") el "" [: :] :];
                     `HVbox [: `S LR "done"; k :] :] :]
      | <:expr< while $e1$ do { $list:el$ } >> ->
          fun curr next dg k ->
            [: `BEbox
                  [: `BEbox
                        [: `S LR "while"; `expr e1 "" [: :]; `S LR "do" :];
                     `HVbox
                        [: `HVbox [: :];
                           listws expr (S RO ";") el "" [: :] :];
                     `HVbox [: `S LR "done"; k :] :] :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< ($list:el$) >> ->
          fun curr next dg k ->
            [: `HVbox [: :]; listws next (S RO ",") el "" k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $x$.val := $y$ >> ->
          fun curr next dg k ->
            [: `next x "" [: `S LR ":=" :]; `expr y dg k :]
      | <:expr< $x$ := $y$ >> ->
          fun curr next dg k ->
            [: `next x "" [: `S LR "<-" :]; `expr y dg k :]
      | e -> fun curr next dg k -> [: `next e "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox [: `HVbox [: :]; x :];
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:("||" as f)$ $x$ $y$ >> ->
          fun curr next dg k -> [: `next x "" [: `S LR f :]; curr y "" k :]
      | <:expr< $lid:("or" as f)$ $x$ $y$ >> ->
          fun curr next dg k -> [: `next x "" [: `S LR f :]; curr y "" k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox [: `HVbox [: :]; x :];
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:(("&&") as f)$ $x$ $y$ >> ->
          fun curr next dg k -> [: `next x "" [: `S LR f :]; curr y "" k :]
      | <:expr< $lid:(("&") as f)$ $x$ $y$ >> ->
          fun curr next dg k -> [: `next x "" [: `S LR f :]; curr y "" k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:op$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            match op with
            [ "=" | "<>" | "<" | "<." | "<=" | ">" | ">=" | ">=." | "==" |
              "!=" ->
                [: curr x "" [: `S LR op :]; `next y "" k :]
            | _ -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:op$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            match op with
            [ "^" | "@" -> [: `next x "" [: `S LR op :]; curr y "" k :]
            | _ -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< [$_$ :: $_$] >> as e ->
          fun curr next dg k ->
            let (el, c) =
              make_list e where rec make_list e =
                match e with
                [ <:expr< [$e$ :: $y$] >> ->
                    let (el, c) = make_list y in
                    ([e :: el], c)
                | <:expr< [] >> -> ([], None)
                | x -> ([], Some e) ]
            in
            match c with
            [ None -> [: `next e "" k :]
            | Some x ->
                [: listws next (S LR "::") el "" [: `S LR "::" :];
                   `next x "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:op$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            match op with
            [ "+" | "+." | "-" | "-." ->
                [: curr x "" [: `S LR op :]; `next y "" k :]
            | _ -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:op$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            match op with
            [ "*" | "/" | "*." | "/." | "land" | "lor" | "lxor" | "mod" ->
                [: curr x "" [: `S LR op :]; `next y "" k :]
            | _ -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:op$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            match op with
            [ "**" | "asr" | "lsl" | "lsr" ->
                [: `next x "" [: `S LR op :]; curr y "" k :]
            | _ -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:"~-"$ $x$ >> ->
          fun curr next dg k -> [: `S LR "-"; curr x "" k :]
      | <:expr< $lid:"~-."$ $x$ >> ->
          fun curr next dg k -> [: `S LR "-."; curr x "" k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $int:x$ >> -> fun curr next dg k -> [: `S LR x; k :]
      | <:expr< $flo:x$ >> -> fun curr next dg k -> [: `S LR x; k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = "apply"; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< [$_$ :: $_$] >> as e ->
          fun curr next dg k -> [: `next e "" k :]
      | <:expr< lazy ($x$) >> ->
          fun curr next dg k -> [: `S LR "lazy"; `next x "" k :]
      | <:expr< if $e$ then () else raise (Assert_failure $_$) >> ->
          fun curr next dg k -> [: `S LR "assert"; `next e "" k :]
      | <:expr<  raise (Assert_failure $_$) >> ->
          fun curr next dg k -> [: `S LR "assert"; `S LR "false"; k :]
      | <:expr< $lid:n$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            let loc = MLast.loc_of_expr e in
            if is_infix n then [: `next e "" k :]
            else [: curr <:expr< $lid:n$ $x$ >> "" [: :]; `next y "" k :]
      | <:expr< $x$ $y$ >> ->
          fun curr next dg k ->
            match get_expr_args x [y] with
            [ (_, [_]) -> [: curr x "" [: :]; `next y "" k :]
            | ((<:expr< $uid:_$ >> | <:expr< $_$ . $uid:_$ >> as a), al) ->
                [: curr a "" [: :];
                   `HOVbox
                      [: `S LO "(";
                         listws (fun x _ k -> HOVbox [: curr x "" k :])
                           (S RO ",") al "" [: `S RO ")"; k :] :] :]
            | _ -> [: curr x "" [: :]; `next y "" k :] ]
      | MLast.ExNew _ sl ->
          fun curr next dg k -> [: `S LR "new"; `class_longident sl "" k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = "dot"; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $x$ . ( $y$ ) >> ->
          fun curr next dg k ->
            [: curr x "" [: :]; `S NO ".("; `expr y "" [: `S RO ")"; k :] :]
      | <:expr< $x$ . [ $y$ ] >> ->
          fun curr next dg k ->
            [: curr x "" [: :]; `S NO ".["; `expr y "" [: `S RO "]"; k :] :]
      | <:expr< $e$. val >> ->
          fun curr next dg k -> [: `S LO "!"; `next e "" k :]
      | <:expr< $e1$ . $e2$ >> ->
          fun curr next dg k ->
            [: curr e1 "" [: :]; `S NO "."; curr e2 "" k :]
      | <:expr< $e$ # $lab$ >> ->
          fun curr next dg k ->
            [: curr e "" [: :]; `S NO "#"; `label lab; k :]
      | e -> fun curr next dg k -> [: `next e "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< [$_$ :: $_$] >> as e ->
          fun curr next dg k ->
            let (el, c) =
              make_list e where rec make_list e =
                match e with
                [ <:expr< [$e$ :: $y$] >> ->
                    let (el, c) = make_list y in
                    ([e :: el], c)
                | <:expr< [] >> -> ([], None)
                | x -> ([], Some e) ]
            in
            match c with
            [ None ->
                [: `S LO "[";
                   listws expr (S RO ";") el "" [: `S RO "]"; k :] :]
            | Some x -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = "simple";
    pr_box e x = LocInfo (MLast.loc_of_expr e) (HOVbox x);
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $int:x$ >> ->
          fun curr next dg k ->
            if x.[0] = '-' then [: `S LO "("; `S LR x; `S RO ")"; k :]
            else [: `S LR x; k :]
      | <:expr< $flo:x$ >> ->
          fun curr next dg k ->
            if x.[0] = '-' then [: `S LO "("; `S LR x; `S RO ")"; k :]
            else [: `S LR x; k :]
      | <:expr< $str:s$ >> ->
          fun curr next dg k -> [: `S LR ("\"" ^ s ^ "\""); k :]
      | <:expr< $chr:c$ >> ->
          fun curr next dg k ->
            let c = ocaml_char c in
            [: `S LR ("'" ^ c ^ "'"); k :]
      | <:expr< $uid:s$ >> ->
          fun curr next dg k -> [: `S LR (conv_con s); k :]
      | <:expr< $lid:s$ >> ->
          fun curr next dg k -> [: `S LR (var_escaped s); k :]
      | <:expr< ` $i$ >> -> fun curr next dg k -> [: `S LR ("`" ^ i); k :]
      | <:expr< ~ $i$ : $lid:j$ >> when i = j ->
          fun curr next dg k -> [: `S LR ("~" ^ i); k :]
      | <:expr< ~ $i$ : $e$ >> ->
          fun curr next dg k -> [: `S LO ("~" ^ i ^ ":"); curr e "" k :]
      | <:expr< ? $i$ : $lid:j$ >> when i = j ->
          fun curr next dg k -> [: `S LR ("?" ^ i); k :]
      | <:expr< ? $i$ : $e$ >> ->
          fun curr next dg k -> [: `S LO ("?" ^ i ^ ":"); curr e "" k :]
      | <:expr< [| $list:el$ |] >> ->
          fun curr next dg k ->
            [: `S LR "[|"; listws expr (S RO ";") el "" [: `S LR "|]"; k :] :]
      | <:expr< { $list:fel$ } >> ->
          fun curr next dg k ->
            [: `S LO "{";
               listws
                 (fun (lab, e) dg k ->
                    HVbox [: `patt lab "" [: `S LR "=" :]; `expr1 e dg k :])
                 (S RO ";") fel "" [: `S RO "}"; k :] :]
      | <:expr< { ($e$) with $list:fel$ } >> ->
          fun curr next dg k ->
            [: `HVbox [: `S LO "{"; curr e "" [: `S LR "with" :] :];
               listws
                 (fun (lab, e) dg k ->
                    HVbox [: `patt lab "" [: `S LR "=" :]; `expr1 e dg k :])
                 (S RO ";") fel "" [: `S RO "}"; k :] :]
      | <:expr< ($e$ : $t$) >> ->
          fun curr next dg k ->
            [: `S LO "("; `expr e "" [: `S LR ":" :];
               `ctyp t "" [: `S RO ")"; k :] :]
      | <:expr< ($e$ : $t1$ :> $t2$) >> ->
          fun curr next dg k ->
            [: `S LO "("; `expr e "" [: `S LR ":" :];
               `ctyp t1 "" [: `S LR ":>" :]; `ctyp t2 "" [: `S RO ")"; k :] :]
      | <:expr< ($e$ :> $t2$) >> ->
          fun curr next dg k ->
            [: `S LO "("; `expr e "" [: `S LR ":>" :];
               `ctyp t2 "" [: `S RO ")"; k :] :]
      | MLast.ExOvr _ [] -> fun curr next dg k -> [: `S LR "{< >}"; k :]
      | MLast.ExOvr _ fel ->
          fun curr next dg k ->
            [: `S LR "{<";
               listws field_expr (S RO ";") fel dg [: `S LR ">}"; k :] :]
      | <:expr< do { $list:el$ } >> ->
          fun curr next dg k ->
            match el with
            [ [e] -> curr e dg k
            | _ ->
                [: `BEbox
                      [: `S LR "begin";
                         `HVbox
                            [: `HVbox [: :];
                               listws expr1 (S RO ";") el "" [: :] :];
                         `HVbox [: `S LR "end"; k :] :] :] ]
      | <:expr< $_$ $_$ >> | <:expr< $uid:_$ $_$ $_$ >> |
        <:expr< fun [ $list:_$ ] >> | <:expr< match $_$ with [ $list:_$ ] >> |
        <:expr< if $_$ then $_$ else $_$ >> |
        <:expr< try $_$ with [ $list:_$ ] >> |
        <:expr< let $rec:_$ $list:_$ in $_$ >> |
        <:expr< for $_$ = $_$ $to:_$ $_$ do { $list:_$ } >> |
        <:expr< while $_$ do { $list:_$ } >> | <:expr< ($list: _$) >> |
        <:expr< $_$ . $_$ >> | <:expr< $_$ . ( $_$ ) >> |
        <:expr< $_$ . [ $_$ ] >> | <:expr< $_$ := $_$ >> |
        <:expr< $_$ # $_$ >> |
        <:expr< let module $_$ = $_$ in $_$ >> |
        <:expr< new $list:_$ >> as e ->
          fun curr next dg k ->
            [: `S LO "("; `expr e "" [: `HVbox [: `S RO ")"; k :] :] :]
      | e -> fun curr next dg k -> [: `next e "" k :] ]}];

pr_patt.pr_levels :=
  [{pr_label = "top"; pr_box p x = LocInfo (MLast.loc_of_patt p) (HOVCbox x);
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< ($x$ as $lid:y$) >> ->
          fun curr next dg k ->
            [: curr x "" [: :]; `S LR "as"; `S LR (var_escaped y); k :]
      | <:patt< ($x$ as $y$) >> ->
          fun curr next dg k ->
            [: curr y "" [: :]; `S LR "as"; `next x "" k :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox [: `HVbox [: :]; x :];
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< $x$ | $y$ >> ->
          fun curr next dg k -> [: curr x "" [: `S LR "|" :]; `next y "" k :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVCbox [: `HVbox [: :]; x :];
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< ($list:pl$) >> ->
          fun curr next dg k ->
            [: `HVbox [: :]; listws next (S RO ",") pl "" k :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = "patt1"; pr_box _ x = HOVbox [: `HVbox [: :]; x :];
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< $x$ .. $y$ >> ->
          fun curr next dg k -> [: curr x "" [: `S NO ".." :]; `next y "" k :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVCbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< [$_$ :: $_$] >> as p ->
          fun curr next dg k ->
            let (pl, c) =
              make_list p where rec make_list p =
                match p with
                [ <:patt< [$p$ :: $y$] >> ->
                    let (pl, c) = make_list y in
                    ([p :: pl], c)
                | <:patt< [] >> -> ([], None)
                | x -> ([], Some p) ]
            in
            match c with
            [ None ->
                [: `S LO "[";
                   listws patt (S RO ";") pl "" [: `S RO "]"; k :] :]
            | Some x ->
                [: `HVbox [: :]; listws next (S LR "::") (pl @ [x]) "" k :] ]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< [$_$ :: $_$] >> as p ->
          fun curr next dg k -> [: `next p "" k :]
      | <:patt< $x$ $y$ >> ->
          fun curr next dg k ->
            match get_patt_args x [y] with
            [ (_, [_]) -> [: curr x "" [: :]; `next y "" k :]
            | ((<:patt< $uid:_$ >> | <:patt< $_$ . $uid:_$ >> as a), al) ->
                [: curr a "" [: :];
                   `HOVbox
                      [: `S LO "(";
                         listws (fun x _ k -> HOVbox [: curr x "" k :])
                           (S RO ",") al "" [: `S RO ")"; k :] :] :]
            | _ -> [: curr x "" [: :]; `next y "" k :] ]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< $x$ . $y$ >> ->
          fun curr next dg k -> [: curr x "" [: :]; `S NO "."; `next y "" k :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = "simple";
    pr_box p x = LocInfo (MLast.loc_of_patt p) (HOVbox x);
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< [| $list:pl$ |] >> ->
          fun curr next dg k ->
            [: `S LR "[|"; listws patt (S RO ";") pl "" [: `S LR "|]"; k :] :]
      | <:patt< { $list:fpl$ } >> ->
          fun curr next dg k ->
            [: `HVbox
                  [: `S LO "{";
                     listws
                       (fun (lab, p) _ k ->
                          HVbox
                            [: `patt lab "" [: `S LR "=" :]; `patt p "" k :])
                       (S RO ";") fpl "" [: `S RO "}"; k :] :] :]
      | <:patt< [$_$ :: $_$] >> as p ->
          fun curr next dg k ->
            let (pl, c) =
              make_list p where rec make_list p =
                match p with
                [ <:patt< [$p$ :: $y$] >> ->
                    let (pl, c) = make_list y in
                    ([p :: pl], c)
                | <:patt< [] >> -> ([], None)
                | x -> ([], Some p) ]
            in
            match c with
            [ None ->
                [: `S LO "[";
                   listws patt (S RO ";") pl "" [: `S RO "]"; k :] :]
            | Some x ->
                [: `S LO "("; `patt p "" [: `HVbox [: `S RO ")"; k :] :] :] ]
      | <:patt< ($p$ : $ct$) >> ->
          fun curr next dg k ->
            [: `S LO "("; `patt p "" [: `S LR ":" :];
               `ctyp ct "" [: `S RO ")"; k :] :]
      | <:patt< $int:s$ >> -> fun curr next dg k -> [: `S LR s; k :]
      | <:patt< $flo:s$ >> -> fun curr next dg k -> [: `S LR s; k :]
      | <:patt< $str:s$ >> ->
          fun curr next dg k -> [: `S LR ("\"" ^ s ^ "\""); k :]
      | <:patt< $chr:c$ >> ->
          fun curr next dg k ->
            let c = ocaml_char c in
            [: `S LR ("'" ^ c ^ "'"); k :]
      | <:patt< $lid:i$ >> -> fun curr next dg k -> [: `id_var i; k :]
      | <:patt< $uid:i$ >> ->
          fun curr next dg k -> [: `S LR (conv_con i); k :]
      | <:patt< ` $i$ >> -> fun curr next dg k -> [: `S LR ("`" ^ i); k :]
      | <:patt< # $list:sl$ >> ->
          fun curr next dg k -> [: `S LO "#"; mod_ident sl dg k :]
      | <:patt< ~ $i$ : $lid:j$ >> when i = j ->
          fun curr next dg k -> [: `S LR ("~" ^ i); k :]
      | <:patt< ~ $i$ : $p$ >> ->
          fun curr next dg k ->
            [: `S LO ("~" ^ i ^ ":"); `simple_patt p "" k :]
      | <:patt< ? $i$ : ($p$) >> ->
          fun curr next dg k ->
            match p with
            [ <:patt< $lid:x$ >> when i = x -> [: `S LR ("?" ^ i); k :]
            | _ -> [: `S LO ("?" ^ i ^ ":"); `simple_patt p "" k :] ]
      | <:patt< ? $i$ : ($p$ = $e$) >> ->
          fun curr next dg k ->
            match p with
            [ <:patt< $lid:x$ >> when i = x ->
                [: `S LO "?"; `S LO "("; `patt p "" [: `S LR "=" :];
                   `expr e "" [: `S RO ")"; k :] :]
            | _ ->
                [: `S LO ("?" ^ i ^ ":"); `S LO "(";
                   `patt p "" [: `S LR "=" :];
                   `expr e "" [: `S RO ")"; k :] :] ]
      | <:patt< ? $i$ : ($p$ : $t$ = $e$) >> ->
          fun curr next dg k ->
            match p with
            [ <:patt< $lid:x$ >> when i = x ->
                [: `S LO "?"; `S LO "("; `patt p "" [: `S LR "=" :];
                   `expr e "" [: `S RO ")"; k :] :]
            | _ ->
                [: `S LO ("?" ^ i ^ ":"); `S LO "(";
                   `patt p "" [: `S LR "=" :];
                   `expr e "" [: `S RO ")"; k :] :] ]
      | <:patt< _ >> -> fun curr next dg k -> [: `S LR "_"; k :]
      | <:patt< $_$ $_$ >> | <:patt< ($_$ as $_$) >> | <:patt< $_$ | $_$ >> |
        <:patt< ($list:_$) >> | <:patt< $_$ .. $_$ >> as p ->
          fun curr next dg k ->
            [: `S LO "("; `patt p "" [: `HVbox [: `S RO ")"; k :] :] :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]}];

pr_ctyp.pr_levels :=
  [{pr_label = "top"; pr_box t x = LocInfo (MLast.loc_of_ctyp t) (HOVbox x);
    pr_rules =
      extfun Extfun.empty with
      [ <:ctyp< $x$ as $y$ >> ->
          fun curr next dg k -> [: curr x "" [: `S LR "as" :]; `next y "" k :]
      | t -> fun curr next dg k -> [: `next t "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:ctyp< $x$ -> $y$ >> ->
          fun curr next dg k -> [: `next x "" [: `S LR "->" :]; curr y "" k :]
      | t -> fun curr next dg k -> [: `next t "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:ctyp< ? $lab$ : $t$ >> ->
          fun curr next dg k ->
            [: `S LO "?"; `S LR lab; `S RO ":"; `next t "" k :]
      | t -> fun curr next dg k -> [: `next t "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:ctyp< ($list:tl$) >> ->
          fun curr next dg k -> listws next (S LR "*") tl "" k
      | t -> fun curr next dg k -> [: `next t "" k :] ]};
   {pr_label = "simple";
    pr_box t x = LocInfo (MLast.loc_of_ctyp t) (HOVbox x);
    pr_rules =
      extfun Extfun.empty with
      [ <:ctyp< $t1$ == $t2$ >> ->
          fun curr next dg k ->
            [: curr t1 "=" [: `S LR "=" :]; `next t2 "" k :]
      | t -> fun curr next dg k -> [: `next t "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:ctyp< ? $lab$ : $t$ >> ->
          fun curr next dg k ->
            [: `S LO "?"; `S LR lab; `S RO ":"; `next t "" k :]
      | <:ctyp< ~ $lab$ : $t$ >> ->
          fun curr next dg k -> [: `S LO (lab ^ ":"); `next t "" k :]
      | t -> fun curr next dg k -> [: `next t "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:ctyp< $t1$ $t2$ >> ->
          fun curr next dg k ->
            let (t, tl) = get_type_args t1 [t2] in
            match tl with
            [ [<:ctyp< $_$ $_$ >>] -> [: curr t2 "" [: :]; curr t1 "" k :]
            | [_] -> [: `next t2 "" [: :]; curr t1 "" k :]
            | _ ->
                [: `S LO "(";
                   listws (fun x _ k -> HOVbox [: curr x "" k :]) (S RO ",")
                     tl "" [: `S RO ")" :];
                   curr t "" k :] ]
      | t -> fun curr next dg k -> [: `next t "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:ctyp< $t1$ . $t2$ >> ->
          fun curr next dg k ->
            [: `module_pref t1 "" [: `S NO "." :]; `next t2 "" k :]
      | t -> fun curr next dg k -> [: `next t "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:ctyp< '$s$ >> ->
          fun curr next dg k -> [: `S LO "'"; `S LR (var_escaped s); k :]
      | <:ctyp< $lid:s$ >> ->
          fun curr next dg k -> [: `S LR (var_escaped s); k :]
      | <:ctyp< $uid:s$ >> -> fun curr next dg k -> [: `S LR s; k :]
      | <:ctyp< _ >> -> fun curr next dg k -> [: `S LR "_"; k :]
      | <:ctyp< { $list:ftl$ } >> as t ->
          fun curr next dg k ->
            let loc = MLast.loc_of_ctyp t in
            [: `HVbox
                  [: labels loc [: `S LR "{" :] ftl "" [: `S LR "}" :];
                     k :] :]
      | <:ctyp< [ $list:ctl$ ] >> as t ->
          fun curr next dg k ->
            let loc = MLast.loc_of_ctyp t in
            [: `Vbox
                  [: `HVbox [: :];
                     variants loc [: `S LR " " :] ctl "" [: :]; k :] :]
      | <:ctyp< [ = $list:rfl$ ] >> ->
          fun curr next dg k ->
            [: `HVbox
                  [: `HVbox [: :];
                     row_fields [: `S LR "[" :] rfl "" [: `S LR "]" :];
                     k :] :]
      | <:ctyp< [ > $list:rfl$ ] >> ->
          fun curr next dg k ->
            [: `HVbox
                  [: `HVbox [: :];
                     row_fields [: `S LR "[>" :] rfl "" [: `S LR "]" :];
                     k :] :]
      | <:ctyp< [ < $list:rfl$ > $list:sl$ ] >> ->
          fun curr next dg k ->
            let k1 = [: `S LR "]" :] in
            let k1 =
              match sl with
              [ [] -> k1
              | l ->
                  [: `S LR ">";
                     list (fun x _ k -> HVbox [: `S LR x; k :]) l "" k1 :] ]
            in
            [: `HVbox
                  [: `HVbox [: :]; row_fields [: `S LR "[<" :] rfl "" k1;
                     k :] :]
      | MLast.TyCls _ id ->
          fun curr next dg k -> [: `S LO "#"; `class_longident id "" k :]
      | MLast.TyObj _ [] False -> fun curr next dg k -> [: `S LR "<>"; k :]
      | MLast.TyObj _ ml v ->
          fun curr next dg k ->
            [: `S LR "<"; meth_list (ml, v) "" [: `S LR ">"; k :] :]
      | MLast.TyPol _ pl t ->
          fun curr next dg k ->
            if pl = [] then [: `ctyp t "" k :]
            else [: list typevar pl "" [: `S LR "." :]; `ctyp t "" k :]
      | <:ctyp< $_$ -> $_$ >> | <:ctyp< $_$ $_$ >> | <:ctyp< $_$ == $_$ >> |
        <:ctyp< $_$ . $_$ >> | <:ctyp< ($list:_$) >> | <:ctyp< $_$ as $_$ >> |
        <:ctyp< ~ $_$ : $_$ >> | <:ctyp< ? $_$ : $_$ >> as t ->
          fun curr next dg k ->
            [: `S LO "("; `ctyp t "" [: `HVbox [: `S RO ")"; k :] :] :]
      | t -> fun curr next dg k -> [: `next t "" k :] ]}];

pr_class_str_item.pr_levels :=
  [{pr_label = "";
    pr_box s x = LocInfo (MLast.loc_of_class_str_item s) (HVbox x);
    pr_rules =
      extfun Extfun.empty with
      [ MLast.CrDcl _ s ->
          fun curr next dg k ->
            [: `HVbox [: :]; list class_str_item s "" [: :] :]
      | MLast.CrInh _ ce pb ->
          fun curr next dg k ->
            [: `S LR "inherit"; `class_expr ce [: :];
               match pb with
               [ Some i -> [: `S LR "as"; `S LR i :]
               | _ -> [: :] ];
               k :]
      | MLast.CrVal _ lab mf e ->
          fun curr next dg k -> [: `cvalue [: `S LR "val" :] (lab, mf, e) k :]
      | MLast.CrVir _ lab pf t ->
          fun curr next dg k ->
            [: `S LR "method"; `S LR "virtual"; private_flag pf; `label lab;
               `S LR ":"; `ctyp t "" k :]
      | MLast.CrMth _ lab pf fb None ->
          fun curr next dg k ->
            [: `fun_binding [: `S LR "method"; private_flag pf; `label lab :]
                  fb k :]
      | MLast.CrMth _ lab pf fb (Some t) ->
          fun curr next dg k ->
            [: `HOVbox
                 [: `S LR "method"; private_flag pf; `label lab; `S LR ":";
                     `ctyp t "" [: `S LR "=" :] :];
               `expr fb "" k :]
      | MLast.CrCtr _ t1 t2 ->
          fun curr next dg k ->
            [: `HVbox [: `S LR "constraint"; `ctyp t1 "" [: `S LR "=" :] :];
               `ctyp t2 "" k :]
      | MLast.CrIni _ se ->
          fun curr next dg k -> [: `S LR "initializer"; `expr se "" k :]
      | csi -> fun curr next dg k -> [: `next csi "" k :] ]}];

value output_string_eval oc s =
  loop 0 where rec loop i =
    if i == String.length s then ()
    else if i == String.length s - 1 then output_char oc s.[i]
    else
      match (s.[i], s.[i + 1]) with
      [ ('\\', 'n') -> do { output_char oc '\n'; loop (i + 2) }
      | (c, _) -> do { output_char oc c; loop (i + 1) } ]
;

value maxl = ref 78;
value sep = Pcaml.inter_phrases;
value ncip = ref True;

value input_source ic len =
  let buff = Buffer.create 20 in
  let rec loop i =
    if i = len then Buffer.contents buff
    else do {
      let c = input_char ic in
      Buffer.add_char buff c;
      loop (i + 1)
    }
  in
  loop 0
;

value copy_source ic oc first bp ep =
  match sep.val with
  [ Some str ->
      if first then ()
      else if ep == in_channel_length ic then output_string oc "\n"
      else output_string_eval oc str
  | None ->
      do {
        seek_in ic bp;
        let s = input_source ic (ep - bp) in
        output_string oc s
      } ]
;

value copy_to_end ic oc first bp =
  let ilen = in_channel_length ic in
  if bp < ilen then copy_source ic oc first bp ilen else output_string oc "\n"
;

module Buff =
  struct
    value buff = ref (String.create 80);
    value store len x =
      do {
        if len >= String.length buff.val then
          buff.val := buff.val ^ String.create (String.length buff.val)
        else ();
        buff.val.[len] := x;
        succ len
      }
    ;
    value mstore len s =
      add_rec len 0 where rec add_rec len i =
        if i == String.length s then len
        else add_rec (store len s.[i]) (succ i)
    ;
    value get len = String.sub buff.val 0 len;
  end
;

value extract_comment strm =
  let rec find_comm nl_bef tab_bef =
    parser
    [ [: `'('; a = find_star nl_bef tab_bef :] -> a
    | [: `' '; s :] -> find_comm nl_bef (tab_bef + 1) s
    | [: `'\t'; s :] -> find_comm nl_bef (tab_bef + 8) s
    | [: `'\n'; s :] -> find_comm (nl_bef + 1) 0 s
    | [: `_; s :] -> find_comm 0 0 s
    | [: :] -> ("", nl_bef, tab_bef) ]
  and find_star nl_bef tab_bef =
    parser
    [ [: `'*'; a = insert (Buff.mstore 0 "(*") :] -> (a, nl_bef, tab_bef)
    | [: a = find_comm 0 0 :] -> a ]
  and insert len =
    parser
    [ [: `'*'; a = rparen (Buff.store len '*') :] -> a
    | [: `'('; len = find_star2 (Buff.store len '('); s :] -> insert len s
    | [: `'\t'; s :] -> insert (Buff.mstore len (String.make 8 ' ')) s
    | [: `x; s :] -> insert (Buff.store len x) s
    | [: :] -> "" ]
  and rparen len =
    parser
    [ [: `')'; s :] -> while_space (Buff.store len ')') s
    | [: a = insert len :] -> a ]
  and while_space len =
    parser
    [ [: `' '; a = while_space (Buff.store len ' ') :] -> a
    | [: `'\t'; a = while_space (Buff.mstore len (String.make 8 ' ')) :] -> a
    | [: `'\n'; a = while_space (Buff.store len '\n') :] -> a
    | [: `'('; a = find_star_again len :] -> a
    | [: :] -> Buff.get len ]
  and find_star_again len =
    parser
    [ [: `'*'; a = insert (Buff.mstore len "(*") :] -> a
    | [: :] -> Buff.get len ]
  and find_star2 len =
    parser
    [ [: `'*'; len = insert2 (Buff.store len '*'); s :] -> len
    | [: :] -> len ]
  and insert2 len =
    parser
    [ [: `'*'; a = rparen2 (Buff.store len '*') :] -> a
    | [: `'('; len = find_star2 (Buff.store len '('); s :] -> insert2 len s
    | [: `x; s :] -> insert2 (Buff.store len x) s
    | [: :] -> 0 ]
  and rparen2 len =
    parser
    [ [: `')'; s :] -> Buff.store len ')'
    | [: a = insert2 len :] -> a ]
  in
  find_comm 0 0 strm
;

value get_no_comment _ _ = ("", 0, 0, 0);

value get_comment ic beg len =
  do {
    seek_in ic beg;
    let strm =
      Stream.from (fun i -> if i >= len then None else Some (input_char ic))
    in
    let (s, nl_bef, tab_bef) = extract_comment strm in
    (s, nl_bef, tab_bef, Stream.count strm)
  }
;

value apply_printer printer ast =
  let oc =
    match Pcaml.output_file.val with
    [ Some f -> open_out_bin f
    | None -> stdout ]
  in
  let cleanup () =
    match Pcaml.output_file.val with
    [ Some _ -> close_out oc
    | None -> () ]
  in
  let pr_ch = output_char oc in
  let pr_str = output_string oc in
  let pr_nl () = output_char oc '\n' in
  if Pcaml.input_file.val <> "-" && Pcaml.input_file.val <> "" then do {
    let ic = open_in_bin Pcaml.input_file.val in
    let getcom =
      if not ncip.val && sep.val = None then get_comment ic
      else get_no_comment
    in
    try
      do {
        let (first, last_pos) =
          List.fold_left
            (fun (first, last_pos) (si, (bp, ep)) ->
               do {
                 copy_source ic oc first last_pos bp;
                 flush oc;
                 print_pretty pr_ch pr_str pr_nl "" "" maxl.val getcom bp
                   (printer si "" [: :]);
                 flush oc;
                 (False, ep)
               })
            (True, 0) ast
        in
        copy_to_end ic oc first last_pos;
        flush oc
      }
    with x ->
      do { close_in ic; cleanup (); raise x };
    close_in ic;
    cleanup ()
  }
  else do {
    List.iter
      (fun (si, _) ->
         do {
           print_pretty pr_ch pr_str pr_nl "" "" maxl.val get_no_comment 0
             (printer si "" [: :]);
           match sep.val with
           [ Some str -> output_string_eval oc str
           | None -> output_char oc '\n' ];
           flush oc
         })
      ast;
    cleanup ()
  }
;

Pcaml.print_interf.val := apply_printer sig_item;
Pcaml.print_implem.val := apply_printer str_item;

Pcaml.add_option "-l" (Arg.Int (fun x -> maxl.val := x))
  "<length> line length for pretty printing.";

Pcaml.add_option "-ss" (Arg.Clear no_ss) "Print double semicolons.";

Pcaml.add_option "-no_ss" (Arg.Set no_ss)
  "Do not print double semicolons (default).";

Pcaml.add_option "-sep_src" (Arg.Unit (fun () -> sep.val := None))
  "Read source file for text between phrases (default).";

Pcaml.add_option "-sep" (Arg.String (fun x -> sep.val := Some x))
  "<string> Use this string between phrases instead of reading source.";

Pcaml.add_option "-cip" (Arg.Clear ncip) "Add comments in phrases.";

Pcaml.add_option "-ncip" (Arg.Set ncip) "No comments in phrases (default).";

Pcaml.add_option "-tc" (Arg.Clear ncip)
  "Deprecated since version 3.05; equivalent to -cip.";
