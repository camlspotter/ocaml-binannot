(* camlp4r pa_extend.cmo *)
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

value version = Sys.ocaml_version;

value gram =
  Grammar.gcreate
    {Token.tok_func _ = failwith "no loaded parsing module";
     Token.tok_using _ = (); Token.tok_removing _ = ();
     Token.tok_match = fun []; Token.tok_text _ = ""}
;

value interf = Grammar.Entry.create gram "interf";
value implem = Grammar.Entry.create gram "implem";
value top_phrase = Grammar.Entry.create gram "top_phrase";
value use_file = Grammar.Entry.create gram "use_file";
value sig_item = Grammar.Entry.create gram "sig_item";
value str_item = Grammar.Entry.create gram "str_item";
value module_type = Grammar.Entry.create gram "module_type";
value module_expr = Grammar.Entry.create gram "module_expr";
value expr = Grammar.Entry.create gram "expr";
value patt = Grammar.Entry.create gram "patt";
value ctyp = Grammar.Entry.create gram "type";
value let_binding = Grammar.Entry.create gram "let_binding";

value class_sig_item = Grammar.Entry.create gram "class_sig_item";
value class_str_item = Grammar.Entry.create gram "class_str_item";
value class_type = Grammar.Entry.create gram "class_type";
value class_expr = Grammar.Entry.create gram "class_expr";

value parse_interf = ref (Grammar.Entry.parse interf);
value parse_implem = ref (Grammar.Entry.parse implem);

value rec skip_to_eol cs =
  match Stream.peek cs with
  [ Some '\n' -> ()
  | Some c -> do { Stream.junk cs; skip_to_eol cs }
  | _ -> () ]
;
value sync = ref skip_to_eol;

value input_file = ref "";
value output_file = ref None;

List.iter (fun (n, f) -> Quotation.add n f)
  [("id", Quotation.ExStr (fun _ s -> "$0:" ^ s ^ "$"));
   ("string", Quotation.ExStr (fun _ s -> "\"" ^ String.escaped s ^ "\""))];

value quotation_dump_file = ref (None : option string);

type err_ctx =
  [ Finding | Expanding | ParsingResult of (int * int) and string | Locating ]
;
exception Qerror of string and err_ctx and exn;

value expand_quotation loc expander shift name str =
  try expander str with
  [ Stdpp.Exc_located (p1, p2) exc ->
      let exc1 = Qerror name Expanding exc in
      raise (Stdpp.Exc_located (shift + p1, shift + p2) exc1)
  | exc ->
      let exc1 = Qerror name Expanding exc in
      raise (Stdpp.Exc_located loc exc1) ]
;

value parse_quotation_result entry loc shift name str =
  let cs = Stream.of_string str in
  try Grammar.Entry.parse entry cs with
  [ Stdpp.Exc_located iloc (Qerror _ Locating _ as exc) ->
      raise (Stdpp.Exc_located (shift + fst iloc, shift + snd iloc) exc)
  | Stdpp.Exc_located iloc (Qerror _ Expanding exc) ->
      let ctx = ParsingResult iloc str in
      let exc1 = Qerror name ctx exc in
      raise (Stdpp.Exc_located loc exc1)
  | Stdpp.Exc_located _ (Qerror _ _ _ as exc) ->
      raise (Stdpp.Exc_located loc exc)
  | Stdpp.Exc_located iloc exc ->
      let ctx = ParsingResult iloc str in
      let exc1 = Qerror name ctx exc in
      raise (Stdpp.Exc_located loc exc1) ]
;

value handle_quotation loc proj in_expr entry reloc (name, str) =
  let shift =
    match name with
    [ "" -> String.length "<<"
    | _ -> String.length "<:" + String.length name + String.length "<" ]
  in
  let shift = fst loc + shift in
  let expander =
    try Quotation.find name with exc ->
      let exc1 = Qerror name Finding exc in
      let loc = (fst loc, shift) in
      raise (Stdpp.Exc_located loc exc1)
  in
  let ast =
    match expander with
    [ Quotation.ExStr f ->
        let new_str = expand_quotation loc (f in_expr) shift name str in
        parse_quotation_result entry loc shift name new_str
    | Quotation.ExAst fe_fp ->
        expand_quotation loc (proj fe_fp) shift name str ]
  in
  reloc (fun _ -> loc) shift ast
;

value parse_locate entry shift str =
  let cs = Stream.of_string str in
  try Grammar.Entry.parse entry cs with
  [ Stdpp.Exc_located (p1, p2) exc ->
      let ctx = Locating in
      let exc1 = Qerror (Grammar.Entry.name entry) ctx exc in
      raise (Stdpp.Exc_located (shift + p1, shift + p2) exc1) ]
;

value handle_locate loc entry ast_f (pos, str) =
  let s = str in
  let loc = (pos, pos + String.length s) in
  let x = parse_locate entry (fst loc) s in
  ast_f loc x
;

value expr_anti loc e = MLast.ExAnt loc e;
value patt_anti loc p = MLast.PaAnt loc p;
value expr_eoi = Grammar.Entry.create gram "expression";
value patt_eoi = Grammar.Entry.create gram "pattern";
EXTEND
  expr_eoi:
    [ [ x = expr; EOI -> x ] ]
  ;
  patt_eoi:
    [ [ x = patt; EOI -> x ] ]
  ;
END;

value handle_expr_quotation loc x =
  handle_quotation loc fst True expr_eoi Reloc.expr x
;

value handle_expr_locate loc x = handle_locate loc expr_eoi expr_anti x;

value handle_patt_quotation loc x =
  handle_quotation loc snd False patt_eoi Reloc.patt x
;

value handle_patt_locate loc x = handle_locate loc patt_eoi patt_anti x;

value expr_reloc = Reloc.expr;
value patt_reloc = Reloc.patt;

value find_line (bp, ep) str =
  find 0 1 0 where rec find i line col =
    if i == String.length str then (line, 0, col)
    else if i == bp then (line, col, col + ep - bp)
    else if str.[i] == '\n' then find (succ i) (succ line) 0
    else find (succ i) line (succ col)
;

value loc_fmt =
  match Sys.os_type with
  [ "MacOS" ->
     format_of_string "File \"%s\"; line %d; characters %d to %d\n### "
  | _ ->
     format_of_string "File \"%s\", line %d, characters %d-%d:\n" ]
;

value report_quotation_error name ctx =
  let name = if name = "" then Quotation.default.val else name in
  do {
    Format.print_flush ();
    Format.open_hovbox 2;
    Printf.eprintf "While %s \"%s\":"
      (match ctx with
       [ Finding -> "finding quotation"
       | Expanding -> "expanding quotation"
       | ParsingResult _ _ -> "parsing result of quotation"
       | Locating -> "parsing" ])
      name;
    match ctx with
    [ ParsingResult (bp, ep) str ->
        match quotation_dump_file.val with
        [ Some dump_file ->
            do {
              Printf.eprintf " dumping result...\n";
              flush stderr;
              try
                let (line, c1, c2) = find_line (bp, ep) str in
                let oc = open_out_bin dump_file in
                do {
                  output_string oc str;
                  output_string oc "\n";
                  flush oc;
                  close_out oc;
                  Printf.eprintf loc_fmt dump_file line c1 c2;
                  flush stderr
                }
              with _ ->
                do {
                  Printf.eprintf "Error while dumping result in file \"%s\""
                    dump_file;
                  Printf.eprintf "; dump aborted.\n";
                  flush stderr
                }
            }
        | None ->
            do {
              if input_file.val = "" then
                Printf.eprintf
                  "\n(consider setting variable Pcaml.quotation_dump_file)\n"
              else Printf.eprintf " (consider using option -QD)\n";
              flush stderr
            } ]
    | _ -> do { Printf.eprintf "\n"; flush stderr } ]
  }
;

value print_format str =
  let rec flush ini cnt =
    if cnt > ini then Format.print_string (String.sub str ini (cnt - ini))
    else ()
  in
  let rec loop ini cnt =
    if cnt == String.length str then flush ini cnt
    else
      match str.[cnt] with
      [ '\n' ->
          do {
            flush ini cnt;
            Format.close_box ();
            Format.force_newline ();
            Format.open_box 2;
            loop (cnt + 1) (cnt + 1)
          }
      | ' ' ->
          do {
            flush ini cnt; Format.print_space (); loop (cnt + 1) (cnt + 1)
          }
      | _ -> loop ini (cnt + 1) ]
  in
  do { Format.open_box 2; loop 0 0; Format.close_box () }
;

value print_exn =
  fun
  [ Out_of_memory -> Format.print_string "Out of memory\n"
  | Match_failure (file, line, char) ->
      do {
        Format.print_string "Pattern matching failed, file ";
        Format.print_string file;
        Format.print_string ", line ";
        Format.print_int line;
        Format.print_string ", char ";
        Format.print_int char
      }
  | Stream.Error str -> print_format ("Parse error: " ^ str)
  | Stream.Failure -> Format.print_string "Parse failure"
  | Token.Error str ->
      do { Format.print_string "Lexing error: "; Format.print_string str }
  | Failure str ->
      do { Format.print_string "Failure: "; Format.print_string str }
  | Invalid_argument str ->
      do { Format.print_string "Invalid argument: "; Format.print_string str }
  | Sys_error msg ->
      do { Format.print_string "I/O error: "; Format.print_string msg }
  | x ->
      do {
        Format.print_string "Uncaught exception: ";
        Format.print_string
          (Obj.magic (Obj.field (Obj.field (Obj.repr x) 0) 0));
        if Obj.size (Obj.repr x) > 1 then do {
          Format.print_string " (";
          for i = 1 to Obj.size (Obj.repr x) - 1 do {
            if i > 1 then Format.print_string ", " else ();
            let arg = Obj.field (Obj.repr x) i in
            if not (Obj.is_block arg) then
              Format.print_int (Obj.magic arg : int)
            else if Obj.tag arg = 252 then do {
              Format.print_char '"';
              Format.print_string (Obj.magic arg : string);
              Format.print_char '"'
            }
            else Format.print_char '_'
          };
          Format.print_char ')'
        }
        else ()
      } ]
;

value report_error exn =
  match exn with
  [ Qerror name Finding Not_found ->
      let name = if name = "" then Quotation.default.val else name in
      do {
        Format.print_flush ();
        Format.open_hovbox 2;
        Format.printf "Unbound quotation: \"%s\"" name;
        Format.close_box ()
      }
  | Qerror name ctx exn ->
      do { report_quotation_error name ctx; print_exn exn }
  | e -> print_exn exn ]
;

value warning_default_function (bp, ep) txt =
  do { Printf.eprintf "<W> loc %d %d: %s\n" bp ep txt; flush stderr }
;

value warning = ref warning_default_function;

value no_constructors_arity = Ast2pt.no_constructors_arity;
(*value no_assert = ref False;*)

value arg_spec_list_ref = ref [];
value arg_spec_list () = arg_spec_list_ref.val;
value add_option name spec descr =
  arg_spec_list_ref.val := arg_spec_list_ref.val @ [(name, spec, descr)]
;

(* Printers *)

open Spretty;

type printer_t 'a =
  { pr_fun : mutable string -> 'a -> string -> kont -> pretty;
    pr_levels : mutable list (pr_level 'a) }
and pr_level 'a =
  { pr_label : string;
    pr_box : 'a -> Stream.t pretty -> pretty;
    pr_rules : mutable pr_rule 'a }
and pr_rule 'a =
  Extfun.t 'a (curr 'a -> next 'a -> string -> kont -> Stream.t pretty)
and curr 'a = 'a -> string -> kont -> Stream.t pretty
and next 'a = 'a -> string -> kont -> pretty
and kont = Stream.t pretty
;

value pr_str_item = {pr_fun = fun []; pr_levels = []};
value pr_sig_item = {pr_fun = fun []; pr_levels = []};
value pr_expr = {pr_fun = fun []; pr_levels = []};
value pr_patt = {pr_fun = fun []; pr_levels = []};
value pr_ctyp = {pr_fun = fun []; pr_levels = []};
value pr_class_str_item = {pr_fun = fun []; pr_levels = []};
value pr_expr_fun_args = ref Extfun.empty;

value not_impl name x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  HVbox [: `S NO ("<pr_fun: not impl: " ^ name ^ "; " ^ desc ^ ">") :]
;

value pr_fun name pr lab =
  loop False pr.pr_levels where rec loop app =
    fun
    [ [] -> fun x dg k -> not_impl name x
    | [lev :: levl] ->
        if app || lev.pr_label = lab then
          let next = loop True levl in
          let rec curr x dg k = Extfun.apply lev.pr_rules x curr next dg k in
          fun x dg k -> lev.pr_box x (curr x dg k)
        else loop app levl ]
;

pr_str_item.pr_fun := pr_fun "str_item" pr_str_item;
pr_sig_item.pr_fun := pr_fun "sig_item" pr_sig_item;
pr_expr.pr_fun := pr_fun "expr" pr_expr;
pr_patt.pr_fun := pr_fun "patt" pr_patt;
pr_ctyp.pr_fun := pr_fun "ctyp" pr_ctyp;
pr_class_str_item.pr_fun := pr_fun "class_str_item" pr_class_str_item;

value rec find_pr_level lab =
  fun
  [ [] -> failwith ("level " ^ lab ^ " not found")
  | [lev :: levl] ->
      if lev.pr_label = lab then lev else find_pr_level lab levl ]
;

value undef x = ref (fun _ -> failwith x);
value print_interf = undef "no printer";
value print_implem = undef "no printer";

value top_printer pr x =
  do {
    Format.force_newline ();
    Spretty.print_pretty Format.print_char Format.print_string
      Format.print_newline "<< " "   " 78
      (fun _ _ -> ("", 0, 0, 0)) 0 (pr.pr_fun "top" x "" [: :]);
    Format.print_string " >>";
  }
;

value inter_phrases = ref None;
