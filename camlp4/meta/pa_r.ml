(* camlp4r pa_extend.cmo q_MLast.cmo *)
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

(* $Id$ *)

open Stdpp;
open Pcaml;

Pcaml.no_constructors_arity.val := False;

value help_sequences () =
  do {
    Printf.eprintf "\
New syntax:
     do {e1; e2; ... ; en}
     while e do {e1; e2; ... ; en}
     for v = v1 to/downto v2 do {e1; e2; ... ; en}
Old (discouraged) syntax:
     do e1; e2; ... ; en-1; return en
     while e do e1; e2; ... ; en; done
     for v = v1 to/downto v2 do e1; e2; ... ; en; done
To avoid compilation warning use the new syntax.
";
    flush stderr;
    exit 1
  }
;
Pcaml.add_option "-help_seq" (Arg.Unit help_sequences)
  "    Print explanations about new sequences and exit.";

do {
  Grammar.Unsafe.reinit_gram gram (Plexer.make ());
  Grammar.Unsafe.clear_entry interf;
  Grammar.Unsafe.clear_entry implem;
  Grammar.Unsafe.clear_entry top_phrase;
  Grammar.Unsafe.clear_entry use_file;
  Grammar.Unsafe.clear_entry module_type;
  Grammar.Unsafe.clear_entry module_expr;
  Grammar.Unsafe.clear_entry sig_item;
  Grammar.Unsafe.clear_entry str_item;
  Grammar.Unsafe.clear_entry expr;
  Grammar.Unsafe.clear_entry patt;
  Grammar.Unsafe.clear_entry ctyp;
  Grammar.Unsafe.clear_entry let_binding;
  Grammar.Unsafe.clear_entry class_type;
  Grammar.Unsafe.clear_entry class_expr;
  Grammar.Unsafe.clear_entry class_sig_item;
  Grammar.Unsafe.clear_entry class_str_item
};

value o2b =
  fun
  [ Some _ -> True
  | None -> False ]
;

value mkumin loc f arg =
  match arg with
  [ <:expr< $int:n$ >> when int_of_string n > 0 ->
      let n = "-" ^ n in
      <:expr< $int:n$ >>
  | <:expr< $flo:n$ >> when float_of_string n > 0.0 ->
      let n = "-" ^ n in
      <:expr< $flo:n$ >>
  | _ ->
      let f = "~" ^ f in
      <:expr< $lid:f$ $arg$ >> ]
;

value mklistexp loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some e -> e
        | None -> <:expr< [] >> ]
    | [e1 :: el] ->
        let loc =
          if top then loc else (fst (MLast.loc_of_expr e1), snd loc)
        in
        <:expr< [$e1$ :: $loop False el$] >> ]
;

value mklistpat loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some p -> p
        | None -> <:patt< [] >> ]
    | [p1 :: pl] ->
        let loc =
          if top then loc else (fst (MLast.loc_of_patt p1), snd loc)
        in
        <:patt< [$p1$ :: $loop False pl$] >> ]
;

(* ...suppose to flush the input in case of syntax error to avoid multiple
   errors in case of cut-and-paste in the xterm, but work bad: for example
   the input "for x = 1;" waits for another line before displaying the
   error...
value rec sync cs =
  match cs with parser
  [ [: `';' :] -> sync_semi cs
  | [: `_ :] -> sync cs ]
and sync_semi cs =
  match Stream.peek cs with 
  [ Some ('\010' | '\013') -> ()
  | _ -> sync cs ]
;
Pcaml.sync.val := sync;
*)

value type_parameter = Grammar.Entry.create gram "type_parameter";
value fun_binding = Grammar.Entry.create gram "fun_binding";
value ipatt = Grammar.Entry.create gram "ipatt";
value direction_flag = Grammar.Entry.create gram "direction_flag";
value mod_ident = Grammar.Entry.create gram "mod_ident";

EXTEND
  GLOBAL: interf implem top_phrase use_file sig_item str_item ctyp patt expr
    module_type module_expr let_binding type_parameter fun_binding ipatt
    direction_flag mod_ident;
  interf:
    [ [ si = sig_item_semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
      | "#"; n = LIDENT; dp = OPT expr; ";" ->
          ([(<:sig_item< # $n$ $opt:dp$ >>, loc)], True)
      | EOI -> ([], False) ] ]
  ;
  sig_item_semi:
    [ [ si = sig_item; ";" -> (si, loc) ] ]
  ;
  implem:
    [ [ si = str_item_semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
      | "#"; n = LIDENT; dp = OPT expr; ";" ->
          ([(<:str_item< # $n$ $opt:dp$ >>, loc)], True)
      | EOI -> ([], False) ] ]
  ;
  str_item_semi:
    [ [ si = str_item; ";" -> (si, loc) ] ]
  ;
  top_phrase:
    [ [ ph = phrase -> Some ph
      | EOI -> None ] ]
  ;
  use_file:
    [ [ si = str_item; ";"; (sil, stopped) = SELF -> ([si :: sil], stopped)
      | "#"; n = LIDENT; dp = OPT expr; ";" ->
          ([<:str_item< # $n$ $opt:dp$ >>], True)
      | EOI -> ([], False) ] ]
  ;
  phrase:
    [ [ sti = str_item; ";" -> sti
      | "#"; n = LIDENT; dp = OPT expr; ";" ->
          <:str_item< # $n$ $opt:dp$ >> ] ]
  ;
  module_expr:
    [ [ "functor"; "("; i = UIDENT; ":"; t = module_type; ")"; "->";
        me = SELF ->
          <:module_expr< functor ( $i$ : $t$ ) -> $me$ >>
      | "struct"; st = LIST0 [ s = str_item; ";" -> s ]; "end" ->
          <:module_expr< struct $list:st$ end >> ]
    | [ me1 = SELF; me2 = SELF -> <:module_expr< $me1$ $me2$ >> ]
    | [ me1 = SELF; "."; me2 = SELF -> <:module_expr< $me1$ . $me2$ >> ]
    | [ i = UIDENT -> <:module_expr< $uid:i$ >>
      | "("; me = SELF; ":"; mt = module_type; ")" ->
          <:module_expr< ( $me$ : $mt$ ) >>
      | "("; me = SELF; ")" -> <:module_expr< $me$ >> ] ]
  ;
  str_item:
    [ "top"
      [ "declare"; st = LIST0 [ s = str_item; ";" -> s ]; "end" ->
          <:str_item< declare $list:st$ end >>
      | "exception"; (c, tl) = constructor_declaration ->
          <:str_item< exception $c$ of $list:tl$ >>
      | "external"; i = LIDENT; ":"; t = ctyp; "="; pd = LIST1 STRING ->
          <:str_item< external $i$ : $t$ = $list:pd$ >>
      | "include"; me = module_expr -> <:str_item< include $me$ >>
      | "module"; i = UIDENT; mb = module_binding ->
          <:str_item< module $i$ = $mb$ >>
      | "module"; "type"; i = UIDENT; "="; mt = module_type ->
          <:str_item< module type $i$ = $mt$ >>
      | "open"; i = mod_ident -> <:str_item< open $i$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
          <:str_item< type $list:tdl$ >>
      | "value"; r = OPT "rec"; l = LIST1 let_binding SEP "and" ->
          <:str_item< value $rec:o2b r$ $list:l$ >>
      | e = expr -> <:str_item< $exp:e$ >> ] ]
  ;
  module_binding:
    [ RIGHTA
      [ "("; m = UIDENT; ":"; mt = module_type; ")"; mb = SELF ->
          <:module_expr< functor ( $m$ : $mt$ ) -> $mb$ >>
      | ":"; mt = module_type; "="; me = module_expr ->
          <:module_expr< ( $me$ : $mt$ ) >>
      | "="; me = module_expr -> <:module_expr< $me$ >> ] ]
  ;
  module_type:
    [ [ "functor"; "("; i = UIDENT; ":"; t = SELF; ")"; "->"; mt = SELF ->
          <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ]
    | [ mt = SELF; "with"; wcl = LIST1 with_constr SEP "and" ->
          <:module_type< $mt$ with $list:wcl$ >> ]
    | [ "sig"; sg = LIST0 [ s = sig_item; ";" -> s ]; "end" ->
          <:module_type< sig $list:sg$ end >> ]
    | [ m1 = SELF; m2 = SELF -> <:module_type< $m1$ $m2$ >> ]
    | [ m1 = SELF; "."; m2 = SELF -> <:module_type< $m1$ . $m2$ >> ]
    | [ i = UIDENT -> <:module_type< $uid:i$ >>
      | i = LIDENT -> <:module_type< $lid:i$ >>
      | "("; mt = SELF; ")" -> <:module_type< $mt$ >> ] ]
  ;
  sig_item:
    [ "top"
      [ "declare"; st = LIST0 [ s = sig_item; ";" -> s ]; "end" ->
          <:sig_item< declare $list:st$ end >>
      | "exception"; (c, tl) = constructor_declaration ->
          <:sig_item< exception $c$ of $list:tl$ >>
      | "external"; i = LIDENT; ":"; t = ctyp; "="; pd = LIST1 STRING ->
          <:sig_item< external $i$ : $t$ = $list:pd$ >>
      | "include"; mt = module_type -> <:sig_item< include $mt$ >>
      | "module"; i = UIDENT; mt = module_declaration ->
          <:sig_item< module $i$ : $mt$ >>
      | "module"; "type"; i = UIDENT; "="; mt = module_type ->
          <:sig_item< module type $i$ = $mt$ >>
      | "open"; i = mod_ident -> <:sig_item< open $i$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
          <:sig_item< type $list:tdl$ >>
      | "value"; i = LIDENT; ":"; t = ctyp ->
          <:sig_item< value $i$ : $t$ >> ] ]
  ;
  module_declaration:
    [ RIGHTA
      [ ":"; mt = module_type -> <:module_type< $mt$ >>
      | "("; i = UIDENT; ":"; t = module_type; ")"; mt = SELF ->
          <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ] ]
  ;
  with_constr:
    [ [ "type"; i = mod_ident; tpl = LIST0 type_parameter; "="; t = ctyp ->
          MLast.WcTyp loc i tpl t
      | "module"; i = mod_ident; "="; mt = module_type ->
          MLast.WcMod loc i mt ] ]
  ;
  expr:
    [ "top" RIGHTA
      [ "let"; o = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        x = SELF ->
          <:expr< let $rec:o2b o$ $list:l$ in $x$ >>
      | "let"; "module"; m = UIDENT; mb = module_binding; "in"; e = SELF ->
          <:expr< let module $m$ = $mb$ in $e$ >>
      | "fun"; "["; l = LIST0 match_case SEP "|"; "]" ->
          <:expr< fun [ $list:l$ ] >>
      | "fun"; p = ipatt; e = fun_def -> <:expr< fun $p$ -> $e$ >>
      | "match"; x = SELF; "with"; "["; l = LIST0 match_case SEP "|"; "]" ->
          <:expr< match $x$ with [ $list:l$ ] >>
      | "match"; x = SELF; "with"; p = ipatt; "->"; e = SELF ->
          <:expr< match $x$ with $p$ -> $e$ >>
      | "try"; x = SELF; "with"; "["; l = LIST0 match_case SEP "|"; "]" ->
          <:expr< try $x$ with [ $list:l$ ] >>
      | "try"; x = SELF; "with"; p = ipatt; "->"; e = SELF ->
          <:expr< try $x$ with $p$ -> $e$ >>
      | "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
          <:expr< if $e1$ then $e2$ else $e3$ >>
      | "do"; "{"; seq = sequence; "}" ->
          match seq with
          [ [e] -> e
          | _ -> <:expr< do { $list:seq$ } >> ]
      | "for"; i = LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; "{"; el = sequence; "}" ->
          <:expr< for $i$ = $e1$ $to:df$ $e2$ do { $list:el$ } >>
      | "while"; e = SELF; "do"; "{"; el = sequence; "}" ->
          <:expr< while $e$ do { $list:el$ } >> ]
    | "where"
      [ e = SELF; "where"; rf = OPT "rec"; lb = let_binding ->
          <:expr< let $rec:o2b rf$ $list:[lb]$ in $e$ >> ]
    | ":=" NONA
      [ e1 = SELF; ":="; e2 = SELF; dummy -> <:expr< $e1$ := $e2$ >> ]
    | "||" RIGHTA
      [ e1 = SELF; f = "||"; e2 = SELF -> <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "&&" RIGHTA
      [ e1 = SELF; f = "&&"; e2 = SELF -> <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "<" LEFTA
      [ e1 = SELF; f = [ "<" | ">" | "<=" | ">=" | "=" | "<>" | "==" | "!=" ];
        e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "^" RIGHTA
      [ e1 = SELF; f = [ "^" | "@" ]; e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "+" LEFTA
      [ e1 = SELF; f = [ "+" | "-" | "+." | "-." ]; e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "*" LEFTA
      [ e1 = SELF;
        f = [ "*" | "/" | "*." | "/." | "land" | "lor" | "lxor" | "mod" ];
        e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "**" RIGHTA
      [ e1 = SELF; f = [ "**" | "asr" | "lsl" | "lsr" ]; e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "unary minus" NONA
      [ f = [ "-" | "-." ]; e = SELF -> <:expr< $mkumin loc f e$ >> ]
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF -> <:expr< $e1$ $e2$ >>
      | "assert"; e = SELF ->
          let f = <:expr< $str:input_file.val$ >> in
          let bp = <:expr< $int:string_of_int (fst loc)$ >> in
          let ep = <:expr< $int:string_of_int (snd loc)$ >> in
          let raiser = <:expr< raise (Assert_failure ($f$, $bp$, $ep$)) >> in
          match e with
          [ <:expr< False >> -> raiser
          | _ ->
              if no_assert.val then <:expr< () >>
              else <:expr< if $e$ then () else $raiser$ >> ]
      | "lazy"; e = SELF ->
          <:expr< Pervasives.ref (Lazy.Delayed (fun () -> $e$)) >> ]
    | "." LEFTA
      [ e1 = SELF; "."; "("; e2 = SELF; ")" -> <:expr< $e1$ .( $e2$ ) >>
      | e1 = SELF; "."; "["; e2 = SELF; "]" -> <:expr< $e1$ .[ $e2$ ] >>
      | e1 = SELF; "."; e2 = SELF -> <:expr< $e1$ . $e2$ >> ]
    | "~-" NONA
      [ f = [ "~-" | "~-." ]; e = SELF -> <:expr< $lid:f$ $e$ >> ]
    | "simple"
      [ s = INT -> <:expr< $int:s$ >>
      | s = FLOAT -> <:expr< $flo:s$ >>
      | s = STRING -> <:expr< $str:s$ >>
      | s = CHAR -> <:expr< $chr:s$ >>
      | i = expr_ident -> i
      | "["; "]" -> <:expr< [] >>
      | "["; el = LIST1 expr SEP ";"; last = OPT [ "::"; e = expr -> e ];
        "]" ->
          <:expr< $mklistexp loc last el$ >>
      | "[|"; el = LIST0 expr SEP ";"; "|]" -> <:expr< [| $list:el$ |] >>
      | "{"; lel = LIST1 label_expr SEP ";"; "}" -> <:expr< { $list:lel$ } >>
      | "{"; "("; e = SELF; ")"; "with"; lel = LIST1 label_expr SEP ";";
        "}" ->
          <:expr< { ($e$) with $list:lel$ } >>
      | "("; ")" -> <:expr< () >>
      | "("; e = SELF; ":"; t = ctyp; ")" -> <:expr< ($e$ : $t$) >>
      | "("; e = SELF; ","; el = LIST1 expr SEP ","; ")" ->
          <:expr< ( $list:[e::el]$) >>
      | "("; e = SELF; ")" -> <:expr< $e$ >>
      | x = LOCATE ->
          let x =
            try
              let i = String.index x ':' in
              (int_of_string (String.sub x 0 i),
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found | Failure _ -> (0, x) ]
          in
          Pcaml.handle_expr_locate loc x
      | x = QUOTATION ->
          let x =
            try
              let i = String.index x ':' in
              (String.sub x 0 i,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found -> ("", x) ]
          in
          Pcaml.handle_expr_quotation loc x ] ]
  ;
  dummy:
    [ [ -> () ] ]
  ;
  sequence:
    [ [ "let"; o = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        el = SELF ->
          let e =
            match el with
            [ [e] -> e
            | _ -> <:expr< do { $list:el$ } >> ]
          in
          [<:expr< let $rec:o2b o$ $list:l$ in $e$ >>]
      | e = expr; ";"; el = SELF -> [e :: el]
      | e = expr; ";" -> [e]
      | e = expr -> [e] ] ]
  ;
  let_binding:
    [ [ p = ipatt; e = fun_binding -> (p, e) ] ]
  ;
  fun_binding:
    [ RIGHTA
      [ p = ipatt; e = SELF -> <:expr< fun $p$ -> $e$ >>
      | "="; e = expr -> <:expr< $e$ >>
      | ":"; t = ctyp; "="; e = expr -> <:expr< ($e$ : $t$) >> ] ]
  ;
  match_case:
    [ [ p = patt; aso = OPT [ "as"; p = patt -> p ];
        w = OPT [ "when"; e = expr -> e ]; "->"; e = expr ->
          let p =
            match aso with
            [ Some p2 -> <:patt< ($p$ as $p2$) >>
            | _ -> p ]
          in
          (p, w, e) ] ]
  ;
  label_expr:
    [ [ i = patt_label_ident; e = fun_binding -> (i, e) ] ]
  ;
  expr_ident:
    [ RIGHTA
      [ i = LIDENT -> <:expr< $lid:i$ >>
      | i = UIDENT -> <:expr< $uid:i$ >>
      | m = UIDENT; "."; i = SELF ->
          let rec loop m =
            fun
            [ <:expr< $x$ . $y$ >> -> loop <:expr< $m$ . $x$ >> y
            | e -> <:expr< $m$ . $e$ >> ]
          in
          loop <:expr< $uid:m$ >> i ] ]
  ;
  fun_def:
    [ RIGHTA
      [ p = ipatt; e = SELF -> <:expr< fun [ $p$ -> $e$ ] >>
      | "->"; e = expr -> <:expr< $e$ >> ] ]
  ;
  patt:
    [ LEFTA
      [ p1 = SELF; "|"; p2 = SELF -> <:patt< $p1$ | $p2$ >> ]
    | NONA
      [ p1 = SELF; ".."; p2 = SELF -> <:patt< $p1$ .. $p2$ >> ]
    | LEFTA
      [ p1 = SELF; p2 = SELF -> <:patt< $p1$ $p2$ >> ]
    | LEFTA
      [ p1 = SELF; "."; p2 = SELF -> <:patt< $p1$ . $p2$ >> ]
    | "simple"
      [ s = LIDENT -> <:patt< $lid:s$ >>
      | s = UIDENT -> <:patt< $uid:s$ >>
      | s = INT -> <:patt< $int:s$ >>
      | "-"; s = INT -> <:patt< $int:"-" ^ s$ >>
      | "-"; s = FLOAT -> <:patt< $flo:"-" ^ s$ >>
      | s = FLOAT -> <:patt< $flo:s$ >>
      | s = STRING -> <:patt< $str:s$ >>
      | s = CHAR -> <:patt< $chr:s$ >>
      | "["; "]" -> <:patt< [] >>
      | "["; pl = LIST1 patt SEP ";"; last = OPT [ "::"; p = patt -> p ];
        "]" ->
          <:patt< $mklistpat loc last pl$ >>
      | "[|"; pl = LIST0 patt SEP ";"; "|]" -> <:patt< [| $list:pl$ |] >>
      | "{"; lpl = LIST1 label_patt SEP ";"; "}" -> <:patt< { $list:lpl$ } >>
      | "("; ")" -> <:patt< () >>
      | "("; p = SELF; ")" -> <:patt< $p$ >>
      | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
      | "("; p = SELF; "as"; p2 = SELF; ")" -> <:patt< ($p$ as $p2$) >>
      | "("; p = SELF; ","; pl = LIST1 patt SEP ","; ")" ->
          <:patt< ( $list:[p::pl]$) >>
      | "_" -> <:patt< _ >>
      | x = LOCATE ->
          let x =
            try
              let i = String.index x ':' in
              (int_of_string (String.sub x 0 i),
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found | Failure _ -> (0, x) ]
          in
          Pcaml.handle_patt_locate loc x
      | x = QUOTATION ->
          let x =
            try
              let i = String.index x ':' in
              (String.sub x 0 i,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found -> ("", x) ]
          in
          Pcaml.handle_patt_quotation loc x ] ]
  ;
  label_patt:
    [ [ i = patt_label_ident; "="; p = patt -> (i, p) ] ]
  ;
  patt_label_ident:
    [ LEFTA
      [ p1 = SELF; "."; p2 = SELF -> <:patt< $p1$ . $p2$ >> ]
    | RIGHTA
      [ i = UIDENT -> <:patt< $uid:i$ >>
      | i = LIDENT -> <:patt< $lid:i$ >> ] ]
  ;
  ipatt:
    [ [ "{"; lpl = LIST1 label_ipatt SEP ";"; "}" -> <:patt< { $list:lpl$ } >>
      | "("; ")" -> <:patt< () >>
      | "("; p = SELF; ")" -> <:patt< $p$ >>
      | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
      | "("; p = SELF; "as"; p2 = SELF; ")" -> <:patt< ($p$ as $p2$) >>
      | "("; p = SELF; ","; pl = LIST1 ipatt SEP ","; ")" ->
          <:patt< ( $list:[p::pl]$) >>
      | s = LIDENT -> <:patt< $lid:s$ >>
      | "_" -> <:patt< _ >> ] ]
  ;
  label_ipatt:
    [ [ i = patt_label_ident; "="; p = ipatt -> (i, p) ] ]
  ;
  type_declaration:
    [ [ n = type_patt; tpl = LIST0 type_parameter; "="; tk = ctyp;
        cl = LIST0 constrain ->
          (n, tpl, tk, cl) ] ]
  ;
  type_patt:
    [ [ n = LIDENT -> (loc, n) ] ]
  ;
  constrain:
    [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> (t1, t2) ] ]
  ;
  type_parameter:
    [ [ "'"; i = ident -> (i, (False, False))
      | "+"; "'"; i = ident -> (i, (True, False))
      | "-"; "'"; i = ident -> (i, (False, True)) ] ]
  ;
  ctyp:
    [ LEFTA
      [ t1 = SELF; "=="; t2 = SELF -> <:ctyp< $t1$ == $t2$ >> ]
    | LEFTA
      [ t1 = SELF; "as"; t2 = SELF -> <:ctyp< $t1$ as $t2$ >> ]
    | "arrow" RIGHTA
      [ t1 = SELF; "->"; t2 = SELF -> <:ctyp< $t1$ -> $t2$ >> ]
    | LEFTA
      [ t1 = SELF; t2 = SELF -> <:ctyp< $t1$ $t2$ >> ]
    | LEFTA
      [ t1 = SELF; "."; t2 = SELF -> <:ctyp< $t1$ . $t2$ >> ]
    | "simple"
      [ "'"; i = ident -> <:ctyp< '$i$ >>
      | "_" -> <:ctyp< _ >>
      | i = LIDENT -> <:ctyp< $lid:i$ >>
      | i = UIDENT -> <:ctyp< $uid:i$ >>
      | "("; t = SELF; "*"; tl = LIST1 ctyp SEP "*"; ")" ->
          <:ctyp< ( $list:[t::tl]$ ) >>
      | "("; t = SELF; ")" -> <:ctyp< $t$ >>
      | "["; cdl = LIST0 constructor_declaration SEP "|"; "]" ->
          <:ctyp< [ $list:cdl$ ] >>
      | "{"; ldl = LIST1 label_declaration SEP ";"; "}" ->
          <:ctyp< { $list:ldl$ } >> ] ]
  ;
  constructor_declaration:
    [ [ ci = UIDENT; "of"; cal = LIST1 ctyp SEP "and" -> (ci, cal)
      | ci = UIDENT -> (ci, []) ] ]
  ;
  label_declaration:
    [ [ i = LIDENT; ":"; mf = OPT "mutable"; t = ctyp -> (i, o2b mf, t) ] ]
  ;
  ident:
    [ [ i = LIDENT -> i
      | i = UIDENT -> i ] ]
  ;
  mod_ident:
    [ RIGHTA
      [ i = UIDENT -> [i]
      | i = LIDENT -> [i]
      | m = UIDENT; "."; i = SELF -> [m :: i] ] ]
  ;
  direction_flag:
    [ [ "to" -> True
      | "downto" -> False ] ]
  ;
END;

(* Objects and Classes *)

EXTEND
  GLOBAL: str_item sig_item expr ctyp class_sig_item class_str_item class_type
    class_expr;
  str_item:
    [ [ "class"; cd = LIST1 class_declaration SEP "and" ->
          <:str_item< class $list:cd$ >>
      | "class"; "type"; ctd = LIST1 class_type_declaration SEP "and" ->
          <:str_item< class type $list:ctd$ >> ] ]
  ;
  sig_item:
    [ [ "class"; cd = LIST1 class_description SEP "and" ->
          <:sig_item< class $list:cd$ >>
      | "class"; "type"; ctd = LIST1 class_type_declaration SEP "and" ->
          <:sig_item< class type $list:ctd$ >> ] ]
  ;
  class_declaration:
    [ [ vf = OPT "virtual"; i = LIDENT; ctp = class_type_parameters;
        cfb = class_fun_binding ->
          {MLast.ciLoc = loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = i; MLast.ciExp = cfb} ] ]
  ;
  class_fun_binding:
    [ [ "="; ce = class_expr -> ce
      | ":"; ct = class_type; "="; ce = class_expr ->
          <:class_expr< ($ce$ : $ct$) >>
      | p = ipatt; cfb = SELF -> <:class_expr< fun $p$ -> $cfb$ >> ] ]
  ;
  class_type_parameters:
    [ [ -> (loc, [])
      | "["; tpl = LIST1 type_parameter SEP ","; "]" -> (loc, tpl) ] ]
  ;
  class_fun_def:
    [ [ p = patt LEVEL "simple"; "->"; ce = class_expr ->
          <:class_expr< fun $p$ -> $ce$ >>
      | p = patt LEVEL "simple"; cfd = SELF ->
          <:class_expr< fun $p$ -> $cfd$ >> ] ]
  ;
  class_expr:
    [ "top"
      [ "fun"; cfd = class_fun_def -> cfd
      | "let"; rf = OPT "rec"; lb = LIST1 let_binding SEP "and"; "in";
        ce = SELF ->
          <:class_expr< let $rec:o2b rf$ $list:lb$ in $ce$ >> ]
    | "apply" NONA
      [ ce = SELF; sel = LIST1 expr LEVEL "simple" ->
          <:class_expr< $ce$ $list:sel$ >> ]
    | "simple"
      [ ci = class_longident; "["; ct = ctyp; ","; ctcl = LIST1 ctyp SEP ",";
        "]" ->
          <:class_expr< $list:ci$ [ $ct$ , $list:ctcl$ ] >>
      | ci = class_longident; "["; ct = ctyp; "]" ->
          <:class_expr< $list:ci$ [ $ct$ ] >>
      | ci = class_longident -> <:class_expr< $list:ci$ >>
      | "object"; cspo = OPT class_self_patt; cf = class_structure; "end" ->
          <:class_expr< object $cspo$ $list:cf$ end >>
      | "("; ce = SELF; ":"; ct = class_type; ")" ->
          <:class_expr< ($ce$ : $ct$) >>
      | "("; ce = SELF; ")" -> ce ] ]
  ;
  class_structure:
    [ [ cf = LIST0 [ cf = class_str_item; ";" -> cf ] -> cf ] ]
  ;
  class_self_patt:
    [ [ "("; p = patt; ")" -> p
      | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >> ] ]
  ;
  class_str_item:
    [ [ "inherit"; ce = class_expr; pb = OPT [ "as"; i = LIDENT -> i ] ->
          <:class_str_item< inherit $ce$ $as:pb$ >>
      | "value"; (lab, mf, e) = cvalue ->
          <:class_str_item< value $mut:mf$ $lab$ = $e$ >>
      | "method"; "private"; "virtual"; l = label; ":"; t = ctyp ->
          <:class_str_item< method private virtual $l$ : $t$ >>
      | "method"; "virtual"; pf = OPT "private"; l = label; ":"; t = ctyp ->
          <:class_str_item< method virtual $priv:o2b pf$ $l$ : $t$ >>
      | "method"; "private"; l = label; fb = fun_binding ->
          <:class_str_item< method private $l$ = $fb$ >>
      | "method"; l = label; fb = fun_binding ->
          <:class_str_item< method $l$ = $fb$ >>
      | "type"; t1 = ctyp; "="; t2 = ctyp ->
          <:class_str_item< type $t1$ = $t2$ >>
      | "initializer"; se = expr -> <:class_str_item< initializer $se$ >> ] ]
  ;
  cvalue:
    [ [ mf = OPT "mutable"; l = label; "="; e = expr -> (l, o2b mf, e)
      | mf = OPT "mutable"; l = label; ":"; t = ctyp; "="; e = expr ->
          (l, o2b mf, <:expr< ($e$ : $t$) >>)
      | mf = OPT "mutable"; l = label; ":"; t1 = ctyp; ":>"; t2 = ctyp; "=";
        e = expr ->
          (l, o2b mf, <:expr< ($e$ : $t1$ :> $t2$) >>)
      | mf = OPT "mutable"; l = label; ":>"; t = ctyp; "="; e = expr ->
          (l, o2b mf, <:expr< ($e$ :> $t$) >>) ] ]
  ;
  label:
    [ [ i = LIDENT -> i ] ]
  ;
  class_type:
    [ [ "["; t = ctyp; "]"; "->"; ct = SELF ->
          <:class_type< [ $t$ ] -> $ct$ >>
      | "["; t = ctyp; ","; tl = LIST1 ctyp SEP ","; "]";
        id = clty_longident ->
          <:class_type< [ $t$ , $list:tl$ ] $list:id$ >>
      | "["; t = ctyp; "]"; id = clty_longident ->
          <:class_type< [ $t$ ] $list:id$ >>
      | id = clty_longident -> <:class_type< $list:id$ >>
      | "object"; cst = OPT class_self_type;
        csf = LIST0 [ csf = class_sig_item; ";" -> csf ]; "end" ->
          <:class_type< object $cst$ $list:csf$ end >> ] ]
  ;
  class_self_type:
    [ [ "("; t = ctyp; ")" -> t ] ]
  ;
  class_sig_item:
    [ [ "inherit"; cs = class_type -> <:class_sig_item< inherit $cs$ >>
      | "value"; mf = OPT "mutable"; l = label; ":"; t = ctyp ->
          <:class_sig_item< value $mut:o2b mf$ $l$ : $t$ >>
      | "method"; "private"; "virtual"; l = label; ":"; t = ctyp ->
          <:class_sig_item< method private virtual $l$ : $t$ >>
      | "method"; "virtual"; pf = OPT "private"; l = label; ":"; t = ctyp ->
          <:class_sig_item< method virtual $priv:o2b pf$ $l$ : $t$ >>
      | "method"; "private"; l = label; ":"; t = ctyp ->
          <:class_sig_item< method private $l$ : $t$ >>
      | "method"; l = label; ":"; t = ctyp ->
          <:class_sig_item< method $l$ : $t$ >>
      | "type"; t1 = ctyp; "="; t2 = ctyp ->
          <:class_sig_item< type $t1$ = $t2$ >> ] ]
  ;
  class_description:
    [ [ vf = OPT "virtual"; n = LIDENT; ctp = class_type_parameters; ":";
        ct = class_type ->
          {MLast.ciLoc = loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = n; MLast.ciExp = ct} ] ]
  ;
  class_type_declaration:
    [ [ vf = OPT "virtual"; n = LIDENT; ctp = class_type_parameters; "=";
        cs = class_type ->
          {MLast.ciLoc = loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = n; MLast.ciExp = cs} ] ]
  ;
  expr: LEVEL "apply"
    [ LEFTA
      [ "new"; i = class_longident -> <:expr< new $list:i$ >> ] ]
  ;
  expr: LEVEL "."
    [ [ e = SELF; "#"; lab = label -> <:expr< $e$ # $lab$ >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ "("; e = SELF; ":"; t1 = ctyp; ":>"; t2 = ctyp; ")" ->
          <:expr< ($e$ : $t1$ :> $t2$ ) >>
      | "("; e = SELF; ":>"; t = ctyp; ")" -> <:expr< ($e$ :> $t$) >>
      | "{<"; ">}" -> <:expr< {< >} >>
      | "{<"; fel = field_expr_list; ">}" -> <:expr< {< $list:fel$ >} >> ] ]
  ;
  field_expr_list:
    [ [ l = label; "="; e = expr; ";"; fel = SELF -> [(l, e) :: fel]
      | l = label; "="; e = expr; ";" -> [(l, e)]
      | l = label; "="; e = expr -> [(l, e)] ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "#"; id = class_longident -> <:ctyp< # $list:id$ >>
      | "<"; (ml, v) = meth_list; ">" -> <:ctyp< < $list:ml$ $v$ > >>
      | "<"; ">" -> <:ctyp< < > >> ] ]
  ;
  meth_list:
    [ [ f = field; ";"; (ml, v) = SELF -> ([f :: ml], v)
      | f = field; ";" -> ([f], False)
      | f = field -> ([f], False)
      | ".." -> ([], True) ] ]
  ;
  field:
    [ [ lab = LIDENT; ":"; t = ctyp -> (lab, t) ] ]
  ;
  clty_longident:
    [ [ m = UIDENT; "."; l = SELF -> [m :: l]
      | i = LIDENT -> [i] ] ]
  ;
  class_longident:
    [ [ m = UIDENT; "."; l = SELF -> [m :: l]
      | i = LIDENT -> [i] ] ]
  ;
END;

(* Labels *)

EXTEND
  GLOBAL: ctyp ipatt patt expr mod_ident;
  ctyp: AFTER "arrow"
    [ NONA
      [ i = TILDEIDENTCOLON; t = SELF -> <:ctyp< ~ $i$ : $t$ >>
      | i = QUESTIONIDENTCOLON; t = SELF -> <:ctyp< ? $i$ : $t$ >> ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "[|"; rfl = LIST0 row_field SEP "|"; "|]" ->
          <:ctyp< [| $list:rfl$ |] >>
      | "[|"; ">"; rfl = LIST1 row_field SEP "|"; "|]" ->
          <:ctyp< [| > $list:rfl$ |] >>
      | "[|"; "<"; rfl = LIST1 row_field SEP "|"; "|]" ->
          <:ctyp< [| < $list:rfl$ |] >>
      | "[|"; "<"; rfl = LIST1 row_field SEP "|"; ">";
        ntl = LIST1 name_tag; "|]" ->
          <:ctyp< [| < $list:rfl$ > $list:ntl$ |] >> ] ]
  ;
  row_field:
    [ [ "`"; i = ident -> MLast.RfTag i True []
      | "`"; i = ident; "of"; ao = OPT "&"; l = LIST1 ctyp SEP "&" ->
          MLast.RfTag i (o2b ao) l
      | t = ctyp -> MLast.RfInh t ] ]
  ;
  name_tag:
    [ [ "`"; i = ident -> i ] ]
  ;
  patt: LEVEL "simple"
    [ [ "`"; s = ident -> <:patt< ` $s$ >>
      | "#"; sl = mod_ident -> <:patt< # $list:sl$ >> ] ]
  ;
  ipatt:
    [ [ i = TILDEIDENTCOLON; p = SELF -> <:patt< ~ $i$ : $p$ >>
      | i = TILDEIDENT -> <:patt< ~ $i$ >>
      | i = QUESTIONIDENTCOLON; j = LIDENT -> <:patt< ? $i$ : $lid:j$ >>
      | i = QUESTIONIDENTCOLON; "("; j = LIDENT; "="; e = expr; ")" ->
          <:patt< ? $i$ : ( $lid:j$ = $e$ ) >>
      | i = QUESTIONIDENT -> <:patt< ? $i$ : $lid:i$ >>
      | "?"; "("; i = LIDENT; "="; e = expr; ")" ->
          <:patt< ? $i$ : ( $lid:i$ = $e$ ) >> ] ]
  ;
  expr: AFTER "apply"
    [ "label"
      [ i = TILDEIDENTCOLON; e = SELF -> <:expr< ~ $i$ : $e$ >>
      | i = TILDEIDENT -> <:expr< ~ $i$ >>
      | i = QUESTIONIDENTCOLON; e = SELF -> <:expr< ? $i$ : $e$ >>
      | i = QUESTIONIDENT -> <:expr< ? $i$ >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ "`"; s = ident -> <:expr< ` $s$ >> ] ]
  ;
  ident:
    [ [ i = LIDENT -> i
      | i = UIDENT -> i ] ]
  ;
END;

(* Old syntax for sequences *)

value not_yet_warned = ref True;
value warning_seq () =
  if not_yet_warned.val then do {
    not_yet_warned.val := False;
    Printf.eprintf "\
*** warning: use of old syntax
*** type \"camlp4r -help_seq\" in a shell for explanations
";
    flush stderr
  }
  else ()
;
Pcaml.add_option "-no_warn_seq" (Arg.Clear not_yet_warned)
  "    Warn when using old syntax for sequences.";

EXTEND
  GLOBAL: expr direction_flag;
  expr: LEVEL "top"
    [ [ "do"; seq = LIST0 [ e = expr; ";" -> e ]; "return"; e = SELF ->
          do { warning_seq (); <:expr< do { $list:seq @ [e]$ } >> }
      | "for"; i = LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; seq = LIST0 [ e = expr; ";" -> e ]; "done" ->
          do {
            warning_seq ();
            <:expr< for $i$ = $e1$ $to:df$ $e2$ do { $list:seq$ } >>
          }
      | "while"; e = SELF; "do"; seq = LIST0 [ e = expr; ";" -> e ]; "done" ->
          do { warning_seq (); <:expr< while $e$ do { $list:seq$ } >> } ] ]
  ;
END;
