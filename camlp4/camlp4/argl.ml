(* camlp4r q_MLast.cmo *)
(* $Id$ *)

open Printf;

value action_arg s sl =
  fun
  [ Arg.Unit f -> if s = "" then do { f (); Some sl } else None
  | Arg.Set r -> if s = "" then do { r.val := True; Some sl } else None
  | Arg.Clear r -> if s = "" then do { r.val := False; Some sl } else None
  | Arg.Rest f -> do { List.iter f [s :: sl]; Some [] }
  | Arg.String f ->
      if s = "" then
        match sl with
        [ [s :: sl] -> do { f s; Some sl }
        | [] -> None ]
      else do { f s; Some sl }
  | Arg.Int f ->
      if s = "" then
        match sl with
        [ [s :: sl] ->
            try do { f (int_of_string s); Some sl } with
            [ Failure "int_of_string" -> None ]
        | [] -> None ]
      else
        try do { f (int_of_string s); Some sl } with
        [ Failure "int_of_string" -> None ]
  | Arg.Float f ->
      if s = "" then
        match sl with
        [ [s :: sl] -> do { f (float_of_string s); Some sl }
        | [] -> None ]
      else do { f (float_of_string s); Some sl } ]
;

value common_start s1 s2 =
  loop 0 where rec loop i =
    if i == String.length s1 || i == String.length s2 then i
    else if s1.[i] == s2.[i] then loop (i + 1)
    else i
;

value rec parse_arg s sl =
  fun
  [ [(name, action, _) :: spec_list] ->
      let i = common_start s name in
      if i == String.length name then
        try action_arg (String.sub s i (String.length s - i)) sl action with
        [ Arg.Bad _ -> parse_arg s sl spec_list ]
      else parse_arg s sl spec_list
  | [] -> None ]
;

value rec parse_aux spec_list anon_fun =
  fun
  [ [] -> []
  | [s :: sl] ->
      if String.length s > 1 && s.[0] = '-' then
        match parse_arg s sl spec_list with
        [ Some sl -> parse_aux spec_list anon_fun sl
        | None -> [s :: parse_aux spec_list anon_fun sl] ]
      else do { (anon_fun s : unit); parse_aux spec_list anon_fun sl } ]
;

value loc_fmt =
  match Sys.os_type with
  [ "MacOS" ->
     format_of_string "File \"%s\"; line %d; characters %d to %d\n### "
  | _ ->
     format_of_string "File \"%s\", line %d, characters %d-%d:\n" ]
;

value print_location loc =
  if Pcaml.input_file.val <> "-" then
    let (line, bp, ep) = Stdpp.line_of_loc Pcaml.input_file.val loc in
    eprintf loc_fmt Pcaml.input_file.val line bp ep
  else eprintf "At location %d-%d\n" (fst loc) (snd loc)
;

value print_warning loc s =
  do { print_location loc; eprintf "%s\n" s }
;

value process pa pr getdir =
  let name = Pcaml.input_file.val in
  do {
    Pcaml.warning.val := print_warning;
    let ic = if name = "-" then stdin else open_in_bin name in
    let cs = Stream.of_channel ic in
    let clear () = if name = "-" then () else close_in ic in
    let phr =
      try
        loop () where rec loop () =
          let (pl, stopped_at_directive) = pa cs in
          if stopped_at_directive then do {
            match getdir (List.rev pl) with
            [ Some x ->
                match x with
                [ (loc, "load", Some <:expr< $str:s$ >>) ->
                    Odyl_main.loadfile s
                | (loc, "directory", Some <:expr< $str:s$ >>) ->
                    Odyl_main.directory s
                | (loc, _, _) ->
                    Stdpp.raise_with_loc loc (Stream.Error "bad directive") ]
            | None -> () ];
            pl @ loop ()
          }
          else pl
      with x ->
        do { clear (); raise x }
    in
    clear ();
    pr phr
  }
;

value gind =
  fun
  [ [(MLast.SgDir loc n dp, _) :: _] -> Some (loc, n, dp)
  | _ -> None ]
;

value gimd =
  fun
  [ [(MLast.StDir loc n dp, _) :: _] -> Some (loc, n, dp)
  | _ -> None ]
;

value process_intf () =
  process Pcaml.parse_interf.val Pcaml.print_interf.val gind;
value process_impl () =
  process Pcaml.parse_implem.val Pcaml.print_implem.val gimd;

type file_kind =
  [ Intf
  | Impl ]
;
value file_kind = ref Intf;
value file_kind_of_name name =
  if Filename.check_suffix name ".mli" then Intf
  else if Filename.check_suffix name ".ml" then Impl
  else raise (Arg.Bad ("don't know what to do with " ^ name))
;

value print_version () =
  do {
    eprintf "Camlp4 version %s\n" Pcaml.version; flush stderr; exit 0
  }
;

value align_doc key s =
  let s =
    loop 0 where rec loop i =
      if i = String.length s then ""
      else if s.[i] = ' ' then loop (i + 1)
     else String.sub s i (String.length s - i)
  in
  let (p, s) =
    if String.length s > 0 then
      if s.[0] = '<' then
        loop 0 where rec loop i =
          if i = String.length s then ("", s)
          else if s.[i] <> '>' then loop (i + 1)
          else
            let p = String.sub s 0 (i + 1) in
            loop (i + 1) where rec loop i =
              if i >= String.length s then (p, "")
              else if s.[i] = ' ' then loop (i + 1)
              else (p, String.sub s i (String.length s - i))
      else ("", s)
    else ("", "")
  in
  let tab =
    String.make (max 1 (13 - String.length key - String.length p)) ' '
  in
  p ^ tab  ^ s
;

value usage ini_sl ext_sl =
  do {
    eprintf "\
Usage: camlp4 [load-options] [--] [other-options]
Load options:
  -I directory  Add directory in search patch for object files.
  -where        Print camlp4 library directory and exit.
  -nolib        No automatic search for object files in library directory.
  <object-file> Load this file in Camlp4 core.
Other options:
  <file>        Parse this file.\n";
    List.iter
      (fun (key, _, doc) -> eprintf "  %s %s\n" key (align_doc key doc))
    ini_sl;
    loop (ini_sl @ ext_sl) where rec loop =
      fun
      [ [(y, _, _) :: _] when y = "-help" -> ()
      | [_ :: sl] -> loop sl
      | [] -> eprintf "  -help         Display this list of options.\n" ];
    if ext_sl <> [] then do {
      eprintf "Options added by loaded object files:\n";
      List.iter
        (fun (key, _, doc) -> eprintf "  %s %s\n" key (align_doc key doc))
        ext_sl;
    }
    else ();
  }
;

value initial_spec_list =
  [("-intf",
    Arg.String
      (fun x -> do { file_kind.val := Intf; Pcaml.input_file.val := x }),
    "<file>  Parse <file> as an interface, whatever its extension.");
   ("-impl",
    Arg.String
      (fun x -> do { file_kind.val := Impl; Pcaml.input_file.val := x }),
    "<file>  Parse <file> as an implementation, whatever its extension.");
   ("-unsafe", Arg.Set Ast2pt.fast,
    "Generate unsafe accesses to array and strings.");
   ("-noassert", Arg.Set Pcaml.no_assert,
    "Don't compile assertion checks.");
   ("-verbose", Arg.Set Grammar.error_verbose,
    "More verbose in parsing errors.");
   ("-loc", Arg.String (fun x -> Stdpp.loc_name.val := x),
    "<name>   Name of the location variable (default: " ^ Stdpp.loc_name.val ^
      ")");
   ("-QD", Arg.String (fun x -> Pcaml.quotation_dump_file.val := Some x),
    "<file> Dump quotation expander result in case of syntax error.");
   ("-o", Arg.String (fun x -> Pcaml.output_file.val := Some x),
    "<file> Output on <file> instead of standard output.");
   ("-v", Arg.Unit print_version,
    "Print Camlp4 version and exit.")]
;

value anon_fun x =
  do { Pcaml.input_file.val := x; file_kind.val := file_kind_of_name x }
;

value parse spec_list anon_fun remaining_args =
  let spec_list =
    Sort.list (fun (k1, _, _) (k2, _, _) -> k1 >= k2) spec_list
  in
  try parse_aux spec_list anon_fun remaining_args with
  [ Arg.Bad s ->
      do {
        eprintf "Error: %s\n" s;
        eprintf "Use option -help for usage\n";
        flush stderr;
        exit 2
      } ]
;

value remaining_args =
  let rec loop l i =
    if i == Array.length Sys.argv then l else loop [Sys.argv.(i) :: l] (i + 1)
  in
  List.rev (loop [] (Arg.current.val + 1))
;

value report_error =
  fun
  [ Odyl_main.Error fname msg ->
      do {
        Format.print_string "Error while loading \"";
        Format.print_string fname;
        Format.print_string "\": ";
        Format.print_string msg
      }
  | exc -> Pcaml.report_error exc ]
;

value go () =
  let ext_spec_list = Pcaml.arg_spec_list () in
  let arg_spec_list = initial_spec_list @ ext_spec_list in
  do {
    match parse arg_spec_list anon_fun remaining_args with
    [ [] -> ()
    | ["-help" :: sl] -> do { usage initial_spec_list ext_spec_list; exit 0 }
    | [s :: sl] ->
        do {
          eprintf "%s: unknown or misused option\n" s;
          eprintf "Use option -help for usage\n";
          exit 2
        } ];
    try
      if Pcaml.input_file.val <> "" then
        match file_kind.val with
        [ Intf -> process_intf ()
        | Impl -> process_impl () ]
      else ()
    with exc ->
      do {
        Format.set_formatter_out_channel stderr;
        Format.open_vbox 0;
        let exc =
          match exc with
          [ Stdpp.Exc_located (bp, ep) exc ->
              do { print_location (bp, ep); exc }
          | _ -> exc ]
        in
        report_error exc;
        Format.close_box ();
        Format.print_newline ();
        exit 2
      }
  }
;

Odyl_main.name.val := "camlp4";
Odyl_main.go.val := go;
