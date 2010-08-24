(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* cvsid $Id$ *)

(** Command-line arguments. *)

module M = Odoc_messages

let current_generator = ref (None : Odoc_gen.generator option)

(** Analysis of a string defining options. Return the list of
   options according to the list giving associations between
   [(character, _)] and a list of options. *)
let analyse_option_string l s =
  List.fold_left
    (fun acc -> fun ((c,_), v) ->
      if String.contains s c then
        acc @ v
      else
        acc)
    []
    l

(** Analysis of a string defining the merge options to be used.
   Returns the list of options specified.*)
let analyse_merge_options s =
  let l = [
    (M.merge_description, [Odoc_types.Merge_description]) ;
    (M.merge_author, [Odoc_types.Merge_author]) ;
    (M.merge_version, [Odoc_types.Merge_version]) ;
    (M.merge_see, [Odoc_types.Merge_see]) ;
    (M.merge_since, [Odoc_types.Merge_since]) ;
    (M.merge_deprecated, [Odoc_types.Merge_deprecated]) ;
    (M.merge_param, [Odoc_types.Merge_param]) ;
    (M.merge_raised_exception, [Odoc_types.Merge_raised_exception]) ;
    (M.merge_return_value, [Odoc_types.Merge_return_value]) ;
    (M.merge_custom, [Odoc_types.Merge_custom]) ;
    (M.merge_all, Odoc_types.all_merge_options)
  ]
  in
  analyse_option_string l s


let f_latex_title s =
  try
    let pos = String.index s ',' in
    let n = int_of_string (String.sub s 0 pos) in
    let len = String.length s in
    let command = String.sub s (pos + 1) (len - pos - 1) in
    Odoc_latex.latex_titles := List.remove_assoc n !Odoc_latex.latex_titles ;
    Odoc_latex.latex_titles := (n, command) :: !Odoc_latex.latex_titles
  with
    Not_found
  | Invalid_argument _ ->
      incr Odoc_global.errors ;
      prerr_endline (M.wrong_format s)

let add_hidden_modules s =
  let l = Str.split (Str.regexp ",") s in
  List.iter
    (fun n ->
      let name = Str.global_replace (Str.regexp "[ \n\r\t]+") "" n in
      match name with
        "" -> ()
      | _ ->
          match name.[0] with
            'A'..'Z' -> Odoc_global.hidden_modules := name :: !Odoc_global.hidden_modules
          | _ ->
              incr Odoc_global.errors;
              prerr_endline (M.not_a_module_name name)
    )
    l

let set_generator (g : Odoc_gen.generator) = current_generator := Some g

(** The default option list *)
let options = ref [
  "-version", Arg.Unit (fun () -> print_string M.message_version ; print_newline () ; exit 0) , M.option_version ;
  "-vnum", Arg.Unit (fun () -> print_string M.config_version ;
                               print_newline () ; exit 0) , M.option_version ;
  "-v", Arg.Unit (fun () -> Odoc_global.verbose := true), M.verbose_mode ;
  "-I", Arg.String (fun s ->
       Odoc_global.include_dirs :=
         (Misc.expand_directory Config.standard_library s) :: !Odoc_global.include_dirs),
    M.include_dirs ;
  "-pp", Arg.String (fun s -> Odoc_global.preprocessor := Some s), M.preprocess ;
  "-impl", Arg.String (fun s ->
       Odoc_global.files := !Odoc_global.files @ [Odoc_global.Impl_file s]),
    M.option_impl ;
    "-intf", Arg.String (fun s ->
       Odoc_global.files := !Odoc_global.files @ [Odoc_global.Intf_file s]),
    M.option_intf ;
  "-text", Arg.String (fun s ->
       Odoc_global.files := !Odoc_global.files @ [Odoc_global.Text_file s]),
    M.option_text ;
  "-rectypes", Arg.Set Odoc_global.recursive_types, M.rectypes ;
  "-nolabels", Arg.Unit (fun () -> Odoc_global.classic := true), M.nolabels ;
  "-warn-error", Arg.Set Odoc_global.warn_error, M.werr ;
  "-hide-warnings", Arg.Clear Odoc_config.print_warnings, M.hide_warnings ;
  "-o", Arg.String (fun s -> Odoc_global.out_file := s), M.out_file ;
  "-d", Arg.String (fun s -> Odoc_global.target_dir := s), M.target_dir ;
  "-sort", Arg.Unit (fun () -> Odoc_global.sort_modules := true), M.sort_modules ;
  "-no-stop", Arg.Set Odoc_global.no_stop, M.no_stop ;
  "-no-custom-tags", Arg.Set Odoc_global.no_custom_tags, M.no_custom_tags ;
  "-stars", Arg.Set Odoc_global.remove_stars, M.remove_stars ;
  "-inv-merge-ml-mli", Arg.Set Odoc_global.inverse_merge_ml_mli, M.inverse_merge_ml_mli ;
  "-no-module-constraint-filter", Arg.Clear Odoc_global.filter_with_module_constraints,
  M.no_filter_with_module_constraints ;

  "-keep-code", Arg.Set Odoc_global.keep_code, M.keep_code^"\n" ;

  "-dump", Arg.String (fun s -> Odoc_global.dump := Some s), M.dump ;
  "-load", Arg.String (fun s -> Odoc_global.load := !Odoc_global.load @ [s]), M.load^"\n" ;

  "-t", Arg.String (fun s -> Odoc_global.title := Some s), M.option_title ;
  "-intro", Arg.String (fun s -> Odoc_global.intro_file := Some s), M.option_intro ;
  "-hide", Arg.String add_hidden_modules, M.hide_modules ;
  "-m", Arg.String (fun s -> Odoc_global.merge_options := !Odoc_global.merge_options @ (analyse_merge_options s)),
  M.merge_options ^
  "\n\n *** choosing a generator ***\n";

(* generators *)
  "-html", Arg.Unit (fun () -> set_generator
       (Odoc_gen.Html (module Odoc_html.Generator : Odoc_html.Html_generator))),
    M.generate_html ;
  "-latex", Arg.Unit (fun () -> set_generator
       (Odoc_gen.Latex (module Odoc_latex.Generator : Odoc_latex.Latex_generator))),
    M.generate_latex ;
  "-texi", Arg.Unit (fun () -> set_generator
       (Odoc_gen.Texi (module Odoc_texi.Generator : Odoc_texi.Texi_generator))),
    M.generate_texinfo ;
  "-man", Arg.Unit (fun () -> set_generator
       (Odoc_gen.Man (module Odoc_man.Generator : Odoc_man.Man_generator))),
    M.generate_man ;
  "-dot", Arg.Unit (fun () -> set_generator
       (Odoc_gen.Dot (module Odoc_dot.Generator : Odoc_dot.Dot_generator))),
    M.generate_dot ;
  "-customdir", Arg.Unit (fun () -> Printf.printf "%s\n" Odoc_config.custom_generators_path; exit 0),
  M.display_custom_generators_dir ;
  "-i", Arg.String (fun s -> ()), M.add_load_dir ;
  "-g", Arg.String (fun s -> ()), M.load_file ^
  "\n\n *** HTML options ***\n";

(* html only options *)
  "-all-params", Arg.Set Odoc_html.with_parameter_list, M.with_parameter_list ;
  "-css-style", Arg.String (fun s -> Odoc_html.css_style := Some s), M.css_style ;
  "-index-only", Arg.Set Odoc_html.index_only, M.index_only ;
  "-colorize-code", Arg.Set Odoc_html.colorize_code, M.colorize_code ;
  "-short-functors", Arg.Set Odoc_html.html_short_functors, M.html_short_functors ^
  "\n\n *** LaTeX options ***\n";

(* latex only options *)
  "-noheader", Arg.Unit (fun () -> Odoc_global.with_header := false), M.no_header ;
  "-notrailer", Arg.Unit (fun () -> Odoc_global.with_trailer := false), M.no_trailer ;
  "-sepfiles", Arg.Set Odoc_latex.separate_files, M.separate_files ;
  "-latextitle", Arg.String f_latex_title, M.latex_title Odoc_latex.latex_titles ;
  "-latex-value-prefix",
    Arg.String (fun s -> Odoc_latex.latex_value_prefix := s), M.latex_value_prefix ;
  "-latex-type-prefix",
    Arg.String (fun s -> Odoc_latex.latex_type_prefix := s), M.latex_type_prefix ;
  "-latex-exception-prefix",
    Arg.String (fun s -> Odoc_latex.latex_exception_prefix := s), M.latex_exception_prefix ;
  "-latex-attribute-prefix",
    Arg.String (fun s -> Odoc_latex.latex_attribute_prefix := s), M.latex_attribute_prefix ;
  "-latex-method-prefix",
    Arg.String (fun s -> Odoc_latex.latex_method_prefix := s), M.latex_method_prefix ;
  "-latex-module-prefix",
    Arg.String (fun s -> Odoc_latex.latex_module_prefix := s), M.latex_module_prefix ;
  "-latex-module-type-prefix",
    Arg.String (fun s -> Odoc_latex.latex_module_type_prefix := s), M.latex_module_type_prefix ;
  "-latex-class-prefix",
    Arg.String (fun s -> Odoc_latex.latex_class_prefix := s), M.latex_class_prefix ;
  "-latex-class-type-prefix",
    Arg.String (fun s -> Odoc_latex.latex_class_type_prefix := s), M.latex_class_type_prefix ;
  "-notoc", Arg.Unit (fun () -> Odoc_global.with_toc := false), M.no_toc ^
  "\n\n *** texinfo options ***\n";

(* texi only options *)
  "-noindex", Arg.Clear Odoc_global.with_index, M.no_index ;
  "-esc8", Arg.Set Odoc_texi.esc_8bits, M.esc_8bits ;
  "-info-section", Arg.String ((:=) Odoc_texi.info_section), M.info_section ;
  "-info-entry", Arg.String (fun s -> Odoc_texi.info_entry := !Odoc_texi.info_entry @ [ s ]),
  M.info_entry ^
  "\n\n *** dot options ***\n";

(* dot only options *)
  "-dot-colors", Arg.String (fun s -> Odoc_dot.dot_colors := Str.split (Str.regexp_string ",") s), M.dot_colors ;
  "-dot-include-all", Arg.Set Odoc_dot.dot_include_all, M.dot_include_all ;
  "-dot-types", Arg.Set Odoc_dot.dot_types, M.dot_types ;
  "-dot-reduce", Arg.Set Odoc_dot.dot_reduce, M.dot_reduce^
  "\n\n *** man pages options ***\n";

(* man only options *)
  "-man-mini", Arg.Set Odoc_man.man_mini, M.man_mini ;
  "-man-suffix", Arg.String (fun s -> Odoc_man.man_suffix := s), M.man_suffix ;
  "-man-section", Arg.String (fun s -> Odoc_man.man_section := s), M.man_section ;

]

let add_option o =
  let (s,_,_) = o in
  let rec iter = function
      [] -> [o]
    | (s2,f,m) :: q ->
        if s = s2 then
          o :: q
        else
          (s2,f,m) :: (iter q)
  in
  options := iter !options

let parse () =
  let anonymous f =
    let sf =
      if Filename.check_suffix f "ml" then
        Odoc_global.Impl_file f
      else
        if Filename.check_suffix f "mli" then
          Odoc_global.Intf_file f
        else
          if Filename.check_suffix f "txt" then
            Odoc_global.Text_file f
          else
            failwith (Odoc_messages.unknown_extension f)
    in
    Odoc_global.files := !Odoc_global.files @ [sf]
  in
  let _ = Arg.parse !options
      anonymous
      (M.usage^M.options_are)
  in
  (* we sort the hidden modules by name, to be sure that for example,
     A.B is before A, so we will match against A.B before A in
     Odoc_name.hide_modules.*)
  Odoc_global.hidden_modules :=
    List.sort (fun a -> fun b -> - (compare a b)) !Odoc_global.hidden_modules
