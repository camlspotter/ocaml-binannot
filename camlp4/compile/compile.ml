(* camlp4r *)
(* $Id$ *)

#load "q_MLast.cmo";
#load "pa_extend.cmo";

open Gramext;

value strict_parsing = ref False;
value keywords = ref [];

value loc = (0, 0);

(* Watch the segmentation faults here! the compiled file must have been
   loaded in camlp4 with the option pa_extend.cmo -meta_action. *)
value magic_act (act : Obj.t) : MLast.expr = Obj.magic act;

(* Names of symbols for error messages; code borrowed to grammar.ml *)

value rec name_of_symbol entry =
  fun
  [ Snterm e -> "[" ^ e.ename ^ "]"
  | Snterml e l -> "[" ^ e.ename ^ " level " ^ l ^ "]"
  | Sself | Snext -> "[" ^ entry.ename ^ "]"
  | Stoken tok -> entry.egram.glexer.Token.text tok
  | _ -> "???" ]
;

value rec name_of_symbol_failed entry =
  fun
  [ Slist0 s -> name_of_symbol_failed entry s
  | Slist0sep s _ -> name_of_symbol_failed entry s
  | Slist1 s -> name_of_symbol_failed entry s
  | Slist1sep s _ -> name_of_symbol_failed entry s
  | Sopt s -> name_of_symbol_failed entry s
  | Stree t -> name_of_tree_failed entry t
  | s -> name_of_symbol entry s ]
and name_of_tree_failed entry =
  fun
  [ Node {node = s; brother = bro; son = son} ->
      let txt = name_of_symbol_failed entry s in
      let txt =
        match (s, son) with
        [ (Sopt _, Node _) -> txt ^ " or " ^ name_of_tree_failed entry son
        | _ -> txt ]
      in
      let txt =
        match bro with
        [ DeadEnd | LocAct _ _ -> txt
        | _ -> txt ^ " or " ^ name_of_tree_failed entry bro ]
      in
      txt
  | DeadEnd | LocAct _ _ -> "???" ]
;

value tree_failed entry prev_symb tree =
  let (s2, s3) =
    let txt = name_of_tree_failed entry tree in
    match prev_symb with
    [ Slist0 s ->
        let txt1 = name_of_symbol_failed entry s in
        ("", txt1 ^ " or " ^ txt)
    | Slist1 s ->
        let txt1 = name_of_symbol_failed entry s in
        ("", txt1 ^ " or " ^ txt)
    | Slist0sep s sep ->
        let txt1 = name_of_symbol_failed entry s in
        ("", txt1 ^ " or " ^ txt)
    | Slist1sep s sep ->
        let txt1 = name_of_symbol_failed entry s in
        ("", txt1 ^ " or " ^ txt)
    | Sopt _ | Stree _ -> ("", txt)
    | _ -> (name_of_symbol entry prev_symb, txt) ]
  in
  <:expr< P.error $str:entry.ename$ $str:s2$ $str:s3$ >>
;

(* Compilation *)

value rec find_act =
  fun
  [ DeadEnd -> failwith "find_act"
  | LocAct act _ -> (magic_act act, 0)
  | Node {son = son; brother = bro} ->
      let (act, n) = try find_act son with [ Failure _ -> find_act bro ] in
      (act, n + 1) ]
;

value level_number e l =
  match e.edesc with
  [ Dlevels elevs ->
      loop 0 elevs where rec loop n =
        fun
        [ [lev :: levs] -> if lev.lname = Some l then n else loop (n + 1) levs
        | [] -> failwith ("level " ^ l ^ " not found in entry " ^ e.ename) ]
  | Dparser _ -> 0 ]
;

value nth_patt_of_act (e, n) =
  let patt_list =
    loop e where rec loop =
      fun
      [ <:expr< fun (loc : (int * int)) -> $_$ >> -> []
      | <:expr< fun ($p$ : $_$) -> $e$ >> -> [p :: loop e]
      | <:expr< fun $p$ -> $e$ >> -> [p :: loop e]
      | _ -> failwith "nth_patt_of_act" ]
  in
  List.nth patt_list n
;

value rec last_patt_of_act =
  fun
  [ <:expr< fun ($p$ : $_$) (loc : (int * int)) -> $_$ >> -> p
  | <:expr< fun $_$ -> $e$ >> -> last_patt_of_act e
  | _ -> failwith "last_patt_of_act" ]
;

value rec final_action =
  fun
  [ <:expr< fun (loc : (int * int)) -> ($e$ : $_$) >> -> e
  | <:expr< fun $_$ -> $e$ >> -> final_action e
  | _ -> failwith "final_action" ]
;

value parse_standard_symbol e rkont fkont ending_act =
  <:expr<
     match try Some ($e$ strm__) with [ Stream.Failure -> None ] with
     [ Some $nth_patt_of_act ending_act$ -> $rkont$
     | _ -> $fkont$ ]
  >>
;

value phony_entry = Grammar.Entry.obj Pcaml.implem;

value rec parse_tree entry nlevn alevn (tree, fst_symb) act_kont kont =
  match tree with
  [ DeadEnd -> kont
  | LocAct act _ ->
      let act = magic_act act in
      act_kont False act
  | Node {node = Sself; son = LocAct act _; brother = bro} ->
      let act = magic_act act in
      let n = entry.ename ^ "_" ^ string_of_int alevn in
      let e =
        if strict_parsing.val || alevn = 0 || fst_symb then <:expr< $lid:n$ >>
        else <:expr< P.orzero $lid:n$ $lid:entry.ename ^ "_0"$ >>
      in
      let p2 =
        match bro with
        [ DeadEnd -> kont
        | _ -> parse_tree entry nlevn alevn (bro, fst_symb) act_kont kont ]
      in
      let p1 = act_kont True act in
      parse_standard_symbol e p1 p2 (act, 0)
  | Node {node = s; son = LocAct act _; brother = bro} ->
      let act = magic_act act in
      let p2 = parse_tree entry nlevn alevn (bro, fst_symb) act_kont kont in
      let p1 = act_kont False act in
      parse_symbol entry nlevn s p1 p2 (act, 0)
  | Node {node = s; son = son; brother = bro} ->
      let p2 = parse_tree entry nlevn alevn (bro, fst_symb) act_kont kont in
      let p1 =
        let err =
          let txt = tree_failed entry s son in
          <:expr< raise (Stream.Error $txt$) >>
        in
        match son with
        [ Node {brother = DeadEnd} ->
            parse_tree entry nlevn alevn (son, False) act_kont err
        | _ ->
            let p1 =
              parse_tree entry nlevn alevn (son, True) act_kont
                <:expr< raise Stream.Failure >>
            in
            <:expr< try $p1$ with [ Stream.Failure -> $err$ ] >> ]
      in
      parse_symbol entry nlevn s p1 p2 (find_act son) ]
and parse_symbol entry nlevn s rkont fkont ending_act =
  match s with
  [ Slist0 s ->
      let e = <:expr< P.list0 $symbol_parser entry nlevn s$ >> in
      parse_standard_symbol e rkont fkont ending_act
  | Slist1 s ->
      let e = <:expr< P.list1 $symbol_parser entry nlevn s$ >> in
      parse_standard_symbol e rkont fkont ending_act
  | Slist0sep s sep ->
      let e =
        <:expr<
          P.list0sep $symbol_parser entry nlevn s$
            $symbol_parser entry nlevn sep$ >>
      in
      parse_standard_symbol e rkont fkont ending_act
  | Slist1sep s sep ->
      let e =
        <:expr<
           P.list1sep $symbol_parser entry nlevn s$
             $symbol_parser entry nlevn sep$ >>
      in
      parse_standard_symbol e rkont fkont ending_act
  | Sopt s ->
      let e = <:expr< P.option $symbol_parser entry nlevn s$ >> in
      parse_standard_symbol e rkont fkont ending_act
  | Stree tree ->
      let kont = <:expr< raise Stream.Failure >> in
      let act_kont _ act =
        let e = final_action act in
        <:expr< let loc = P.gloc bp strm__ in $e$ >>
      in
      let e = parse_tree phony_entry 0 0 (tree, True) act_kont kont in
      parse_standard_symbol <:expr< fun strm__ -> $e$ >> rkont fkont ending_act
  | Snterm e ->
      let n =
        match e.edesc with
        [ Dparser _ -> e.ename
        | Dlevels _ -> e.ename ^ "_0" ]
      in
      parse_standard_symbol <:expr< $lid:n$ >> rkont fkont ending_act
  | Snterml e l ->
      let n = e.ename ^ "_" ^ string_of_int (level_number e l) in
      parse_standard_symbol <:expr< $lid:n$ >> rkont fkont ending_act
  | Sself ->
      let n = entry.ename ^ "_0" in
      parse_standard_symbol <:expr< $lid:n$ >> rkont fkont ending_act
  | Stoken tok ->
      let _ =
        do {
          if fst tok = "" && not (List.mem (snd tok) keywords.val) then
            keywords.val := [snd tok :: keywords.val]
          else ()
        }
      in
      let p =
        let patt = nth_patt_of_act ending_act in
        if snd tok = "" then <:patt< ($str:fst tok$, $patt$) >>
        else
          let p = <:patt< ($str:fst tok$, $str:snd tok$) >> in
          match patt with
          [ <:patt< _ >> -> <:patt< ($str:fst tok$, $str:snd tok$) >>
          | _ -> <:patt< ($str:fst tok$, ($str:snd tok$ as $patt$)) >> ]
      in
      <:expr<
        match Stream.peek strm__ with
        [ Some $p$ -> do { Stream.junk strm__; $rkont$ }
        | _ -> $fkont$ ]
      >>
  | _ ->
      parse_standard_symbol <:expr< ps >> rkont fkont ending_act ]
and symbol_parser entry nlevn =
  fun
  [ Snterm e ->
      let n = e.ename ^ "_0" in
      <:expr< $lid:n$ >>
  | Snterml e l ->
      let n = e.ename ^ "_" ^ string_of_int (level_number e l) in
      <:expr< $lid:n$ >>
  | Snext ->
      let n = entry.ename ^ "_" ^ string_of_int nlevn in
      if strict_parsing.val then <:expr< $lid:n$ >>
      else
        let n0 = entry.ename ^ "_0" in
        <:expr< P.orzero $lid:n$ $lid:n0$ >>
  | Stoken tok ->
      let _ =
        do {
          if fst tok = "" && not (List.mem (snd tok) keywords.val) then
            keywords.val := [snd tok :: keywords.val]
          else ()
        }
      in
      <:expr< P.token ($str:fst tok$, $str:snd tok$) >>
  | Stree tree ->
      let kont = <:expr< raise Stream.Failure >> in
      let act_kont _ act = final_action act in
      <:expr<
        fun strm__ ->
          $parse_tree phony_entry 0 0 (tree, True) act_kont kont$
      >>
  | _ ->
      <:expr< aaa >> ]
;

value rec start_parser_of_levels entry clevn levs =
  let n = entry.ename ^ "_" ^ string_of_int clevn in
  let next = entry.ename ^ "_" ^ string_of_int (clevn + 1) in
  let p = <:patt< $lid:n$ >> in
  match levs with
  [ [] -> [Some (p, <:expr< fun strm__ -> raise Stream.Failure >>)]
  | [lev :: levs] ->
      let pel = start_parser_of_levels entry (succ clevn) levs in
      match lev.lprefix with
      [ DeadEnd ->
          let ncont =
             if not strict_parsing.val && clevn = 0 then
               entry.ename ^ "_gen_cont"
             else entry.ename ^ "_" ^ string_of_int clevn ^ "_cont"
          in
          let curr =
            <:expr< let a = $lid:next$ strm__ in $lid:ncont$ bp a strm__ >>
          in
          let curr = <:expr< let bp = Stream.count strm__ in $curr$ >> in
          let e = <:expr< fun strm__ -> $curr$ >> in
          let pel = if levs = [] then [] else pel in
          [Some (p, e) :: pel]
      | tree ->
          let alevn = clevn in
          let (kont, pel) =
            match levs with
            [ [] -> (<:expr< raise Stream.Failure >>, [])
            | _ ->
                let e =
                  match (lev.assoc, lev.lsuffix) with
                  [ (NonA, _) | (_, DeadEnd) -> <:expr< $lid:next$ strm__ >>
                  | _ ->
                      let ncont =
                        entry.ename ^ "_" ^ string_of_int clevn ^ "_cont"
                      in
                      <:expr<
                        let a = $lid:next$ strm__ in
                        $lid:ncont$ bp a strm__
                      >> ]
                in
                (e, pel) ]
          in
          let act_kont end_with_self act =
            if lev.lsuffix = DeadEnd then
              <:expr< let loc = P.gloc bp strm__ in $final_action act$ >>
            else
              let ncont = entry.ename ^ "_" ^ string_of_int clevn ^ "_cont" in
              <:expr<
                let loc = P.gloc bp strm__ in
                $lid:ncont$ bp $final_action act$ strm__
              >>
          in
          let curr =
            parse_tree entry (succ clevn) alevn (tree, True) act_kont kont
          in
          let curr = <:expr< let bp = Stream.count strm__ in $curr$ >> in
          let e = <:expr< fun strm__ -> $curr$ >> in
          [Some (p, e) :: pel] ] ]
;

value rec continue_parser_of_levels entry clevn levs =
  let n = entry.ename ^ "_" ^ string_of_int clevn ^ "_cont" in
  let p = <:patt< $lid:n$ >> in
  match levs with
  [ [] -> [None]
  | [lev :: levs] ->
      let pel = continue_parser_of_levels entry (succ clevn) levs in
      match lev.lsuffix with
      [ DeadEnd ->
(*
          let ncont = entry.ename ^ "_" ^ string_of_int clevn ^ "_cont" in
          let e = <:expr< fun bp a__ strm__ -> a__ >> in
          let pel = if levs = [] then [] else pel in
*)
          [None :: pel]
      | tree ->
          let alevn =
            match lev.assoc with
            [ LeftA | NonA -> succ clevn
            | RightA -> clevn ]
          in
          let (kont, pel) =
            match levs with
            [ [] -> (<:expr< a__ >>, [])
            | _ -> (<:expr< a__ >>, pel) ]
          in
          let act_kont end_with_self act =
            let p = last_patt_of_act act in
            match lev.assoc with
            [ RightA | NonA ->
                <:expr<
                  let $p$ = a__ in
                  let loc = P.gloc bp strm__ in
                  $final_action act$
                >>
            | LeftA ->
                let ncont =
                  entry.ename ^ "_" ^ string_of_int clevn ^ "_cont"
                in
                <:expr<
                  let $p$ = a__ in
                  let loc = P.gloc bp strm__ in
                  $lid:ncont$ bp $final_action act$ strm__
                >> ]
          in
          let curr =
            parse_tree entry (succ clevn) alevn (tree, True) act_kont kont
          in
          let e = <:expr< fun bp a__ strm__ -> $curr$ >> in
          [Some (p, e) :: pel] ] ]
;

value continue_parser_of_levels_again entry levs =
  let n = entry.ename ^ "_gen_cont" in
  let e =
    loop <:expr< a__ >> 0 levs where rec loop var levn =
      fun
      [ [] -> <:expr< if x == a__ then x else $lid:n$ bp x strm__ >>
      | [lev :: levs] ->
          match lev.lsuffix with
          [ DeadEnd -> loop var (levn + 1) levs
          | _ ->
              let n = entry.ename ^ "_" ^ string_of_int levn ^ "_cont" in
              let rest = loop <:expr< x >> (levn + 1) levs in
              <:expr< let x = $lid:n$ bp $var$ strm__ in $rest$ >> ] ]
  in
  (<:patt< $lid:n$ >>, <:expr< fun bp a__ strm__ -> $e$ >>)
;

value empty_entry ename =
  let p = <:patt< $lid:ename$ >> in
  let e =
    <:expr<
      fun strm__ ->
        raise (Stream.Error $str:"entry [" ^ ename ^ "] is empty"$) >>
  in
  [Some (p, e)]
;

value start_parser_of_entry entry =
  match entry.edesc with
  [ Dlevels [] -> empty_entry entry.ename
  | Dlevels [_ :: elev] when entry.ename = "implem" ->
      start_parser_of_levels entry 0 elev
  | Dlevels elev -> start_parser_of_levels entry 0 elev
  | Dparser p -> [] ]
;

value continue_parser_of_entry entry =
  match entry.edesc with
  [ Dlevels [_ :: elev] when entry.ename = "implem" ->
      continue_parser_of_levels entry 0 elev
  | Dlevels elev -> continue_parser_of_levels entry 0 elev
  | Dparser p -> [] ]
;

value continue_parser_of_entry_again entry =
  if strict_parsing.val || entry.ename = "implem" then []
  else
    match entry.edesc with
    [ Dlevels ([_; _ :: _] as levs) ->
        [continue_parser_of_levels_again entry levs]
    | _ -> [] ]
;

value rec list_alternate l1 l2 =
  match (l1, l2) with
  [ ([x1 :: l1], [x2 :: l2]) -> [x1; x2 :: list_alternate l1 l2]
  | ([], l2) -> l2
  | (l1, []) -> l1 ]
;

value compile_entry entry =
  let pel1 = start_parser_of_entry entry in
  let pel2 = continue_parser_of_entry entry in
  let pel = list_alternate pel1 pel2 in
  List.fold_right
    (fun pe list ->
       match pe with
       [ Some pe -> [pe :: list]
       | None -> list ])
    pel (continue_parser_of_entry_again entry)
;

(* get all entries connected together *)

value rec scan_tree list =
  fun
  [ Node {node = n; son = son; brother = bro} ->
      let list = scan_symbol list n in
      let list = scan_tree list son in
      let list = scan_tree list bro in
      list
  | LocAct _ _ | DeadEnd -> list ]
and scan_symbol list =
  fun
  [ Snterm e -> scan_entry list e
  | Snterml e l -> scan_entry list e
  | Slist0 s -> scan_symbol list s
  | Slist0sep s sep -> scan_symbol (scan_symbol list s) sep
  | Slist1 s -> scan_symbol list s
  | Slist1sep s sep -> scan_symbol (scan_symbol list s) sep
  | Sopt s -> scan_symbol list s
  | Stree t -> scan_tree list t
  | Sself | Snext | Stoken _ -> list ]
and scan_level list lev =
  let list = scan_tree list lev.lsuffix in
  let list = scan_tree list lev.lprefix in
  list
and scan_levels list levs = List.fold_left scan_level list levs
and scan_entry list entry =
  if List.memq entry list then list
  else
    match entry.edesc with
    [ Dlevels levs -> scan_levels [entry :: list] levs
    | Dparser _ -> list ]
;

value all_entries_in_graph list entry =
  List.rev (scan_entry list entry)
;

(* main *)

value entries =
  [Grammar.Entry.obj Pcaml.interf;
   Grammar.Entry.obj Pcaml.implem;
   Grammar.Entry.obj Pcaml.top_phrase;
   Grammar.Entry.obj Pcaml.use_file;
   Grammar.Entry.obj Pcaml.str_item;
   Grammar.Entry.obj Pcaml.sig_item;
   Grammar.Entry.obj Pcaml.ctyp;
   Grammar.Entry.obj Pcaml.expr;
   Grammar.Entry.obj Pcaml.patt]
;

value rec list_mem_right_assoc x =
  fun
  [ [] -> False
  | [(a, b) :: l] -> x = b || list_mem_right_assoc x l ]
;

value rec expr_list =
  fun
  [ [] -> <:expr< [] >>
  | [x :: l] -> <:expr< [$str:x$ :: $expr_list l$] >> ]
;

value compile () =
  let _ = do { keywords.val := []; } in
  let list = [] in
  let list = all_entries_in_graph list (Grammar.Entry.obj Pcaml.interf) in
  let list = all_entries_in_graph list (Grammar.Entry.obj Pcaml.implem) in
  let list = all_entries_in_graph list (Grammar.Entry.obj Pcaml.top_phrase) in
  let list = all_entries_in_graph list (Grammar.Entry.obj Pcaml.use_file) in
  let list =
    List.filter (fun e -> List.memq e list) entries @
    List.filter (fun e -> not (List.memq e entries)) list
  in
  let list =
    let set = ref [] in
    List.fold_right
      (fun entry list ->
         if List.mem entry.ename set.val then
           list
         else do { set.val := [entry.ename :: set.val]; [entry :: list] })
        list []
  in
  let pell = List.map compile_entry list in
  let pel = List.flatten pell in
  let si1 = <:str_item< value rec $list:pel$ >> in
  let si2 =
    <:str_item<
      List.iter (fun kw -> P.lexer.Token.using ("", kw))
        $expr_list keywords.val$
    >>
  in
  let loc = (1, 1) in
  ([(si1, loc); (si2, loc)], False)
;

Grammar.Unsafe.reinit_gram Pcaml.gram (Plexer.make ());

EXTEND
  Pcaml.implem: FIRST
    [ [ -> compile () ] ]
  ;
END;

Pcaml.add_option "-strict_parsing" (Arg.Set strict_parsing)
  ": don't generate error recovering by trying continuations or first levels"
;
