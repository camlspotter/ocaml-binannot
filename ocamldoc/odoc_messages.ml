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

(* $Id$ *)

(** The messages of the application. *)

let ok = "Ok"
let software = "OCamldoc"
let version = Config.version
let magic = version^""
let message_version = software^" "^version

(** Messages for command line *)

let usage = "Usage : "^(Sys.argv.(0))^" [options] <files>\n"
let options_are = "Options are :"
let option_version = " Print version and exit"
let latex_only = "(LaTeX only)"
let texi_only = "(TeXinfo only)"
let latex_texi_only = "(LaTeX and TeXinfo only)"
let html_only = "(HTML only)"
let html_latex_only = "(HTML and LaTeX only)"
let man_only = "(man only)"
let verbose_mode = " verbose mode"
let include_dirs = "<dir>  Add <dir> to the list of include directories"
let rectypes = " Allow arbitrary recursive types"
let preprocess = "<command>  Pipe sources through preprocessor <command>"
let load_file = "<file.cm[o|a]>  Load file defining a new documentation generator (bytecode version only)"
let nolabels = " Ignore non-optional labels in types"
let werr = "Treat ocamldoc warnings as errors"
let target_dir = "<dir>  Generate files in directory <dir>, rather than in current directory (for man and HTML generators)"
let dump = "<file>  Dump collected information into <file>"
let load = "<file>  Load information from <file> ; may be used several times"
let css_style = "<file>  Use content of <file> as CSS style definition "^html_only
let index_only = " Generate index files only "^html_only
let colorize_code = "Colorize code even in documentation pages "^html_only
let generate_html = " Generate HTML documentation"
let generate_latex = " Generate LaTeX documentation"
let generate_texinfo = " Generate TeXinfo documentation"
let generate_man = " Generate man pages"
let generate_dot = " Generate dot code of top modules dependencies"

let option_not_in_native_code op = "Option "^op^" not available in native code version."

let default_out_file = "ocamldoc.out"
let out_file = "<file> Set the ouput file name, used by texi, latex and dot generators "^
  "(default is "^default_out_file^")"

let dot_include_all = " include all modules in the dot output,\n"^
  "        not only the modules given on the command line"
let dot_types = " generate dependency graph for types instead of modules"
let default_dot_colors = [ "darkturquoise" ; "darkgoldenrod2" ; "cyan" ; "green" ; "magenta" ; "yellow" ;
                           "burlywood1" ; "aquamarine" ; "floralwhite" ; "lightpink" ;
                           "lightblue" ; "mediumturquoise" ; "salmon" ; "slategray3" ;
                         ] 
let dot_colors = "<c1,c2,...,cn>  use colors c1,c1,...,cn in the dot output\n"^
  "        (default list is "^(String.concat "," default_dot_colors)^")"
let dot_reduce = " perform a transitive reduction on the selected dependency graph before the dot output\n"

let man_mini = " Generate man pages only for modules, module types,\n"^
  "        classes and class types "^man_only
let default_man_suffix = "o"
let man_suffix = "<suffix>  use <suffix> for man page files "^
  "(default is "^default_man_suffix^") "^man_only^"\n"

let option_title = "<title>  use <title> as title for the generated documentation"
let option_intro = 
  "<file>  use content of <file> as ocamldoc text to use as introduction "^(html_latex_only)
let with_parameter_list = " display the complete list of parameters for functions and methods "^html_only
let hide_modules = " <M1,M2.M3,...> Hide the given complete module names in generated doc"
let no_header = " Suppress header in generated documentation "^latex_texi_only
let no_trailer = " Suppress trailer in generated documentation "^latex_texi_only
let separate_files = " Generate one file per toplevel module "^latex_only
let latex_title ref_titles = 
  "n,style  associate {n } to the given sectionning style\n"^
  "              (e.g. 'section') in the latex output "^latex_only^"\n"^
  "              Default sectionning is:\n"^
  (String.concat "\n"
     (List.map (fun (n,t) -> Printf.sprintf "                %d -> %s" n t) !ref_titles))

let default_latex_value_prefix = "val:"
let latex_value_prefix = "<string>  use <string> as prefix for the LaTeX labels of values. "^
  "(default is \""^default_latex_value_prefix^"\")"
let default_latex_type_prefix = "type:"
let latex_type_prefix = "<string>  use <string> as prefix for the LaTeX labels of types. "^
  "(default is \""^default_latex_type_prefix^"\")"
let default_latex_exception_prefix = "exception:"
let latex_exception_prefix = "<string>  use <string> as prefix for the LaTeX labels of exceptions. "^
  "(default is \""^default_latex_exception_prefix^"\")"
let default_latex_module_prefix = "module:"
let latex_module_prefix = "<string>  use <string> as prefix for the LaTeX labels of modules. "^
  "(default is \""^default_latex_module_prefix^"\")"
let default_latex_module_type_prefix = "moduletype:"
let latex_module_type_prefix = "<string>  use <string> as prefix for the LaTeX labels of module types. "^
  "(default is \""^default_latex_module_type_prefix^"\")"
let default_latex_class_prefix = "class:"
let latex_class_prefix = "<string>  use <string> as prefix for the LaTeX labels of classes. "^
  "(default is \""^default_latex_class_prefix^"\")"
let default_latex_class_type_prefix = "classtype:"
let latex_class_type_prefix = "<string>  use <string> as prefix for the LaTeX labels of class types. "^
  "(default is \""^default_latex_class_type_prefix^"\")"
let default_latex_attribute_prefix = "val:"
let latex_attribute_prefix = "<string>  use <string> as prefix for the LaTeX labels of attributes. "^
  "(default is \""^default_latex_attribute_prefix^"\")"
let default_latex_method_prefix = "method:"
let latex_method_prefix = "<string>  use <string> as prefix for the LaTeX labels of methods. "^
  "(default is \""^default_latex_method_prefix^"\")"

let no_toc = " Do not generate table of contents "^latex_only
let sort_modules = " Sort the list of top modules before generating the documentation"
let no_stop = " Do not stop at (**/**) comments"
let no_custom_tags = " Do not allow custom @-tags"
let remove_stars = " Remove beginning blanks of comment lines, until the first '*'"
let keep_code = " Always keep code when available"
let inverse_merge_ml_mli = "Inverse implementations and interfaces when merging"
let merge_description = ('d', "merge description")
let merge_author = ('a', "merge @author")
let merge_version = ('v', "merge @version")
let merge_see = ('l', "merge @see")
let merge_since = ('s', "merge @since")
let merge_deprecated = ('o', "merge @deprecated")
let merge_param = ('p', "merge @param")
let merge_raised_exception = ('e', "merge @raise")
let merge_return_value = ('r', "merge @return")
let merge_custom = ('c', "merge custom @-tags")
let merge_all = ('A', "merge all")

let no_index = " Do not build index for Info files "^texi_only
let esc_8bits = " Escape accentuated characters in Info files "^texi_only
let info_section = "Specify section of Info directory "^texi_only
let info_entry = "Specify Info directory entry "^texi_only^"\n"

let options_can_be = "        <options> can be one or more of the following characters:"
let string_of_options_list l = 
  List.fold_left (fun acc -> fun (c, m) -> acc^"\n        "^(String.make 1 c)^"  "^m)
    ""
    l

let merge_options = 
  "<options>  specify merge options between .mli and .ml\n"^
  options_can_be^
  (string_of_options_list
     [ merge_description ;
       merge_author ;
       merge_version ;
       merge_see ;
       merge_since ;
       merge_deprecated ;
       merge_param ;
       merge_raised_exception ;
       merge_return_value ;
       merge_custom ;
       merge_all ]
  )


(** Error and warning messages *)

let warning = "Warning"
let pwarning s = 
  prerr_endline (warning^": "^s);
  if !Odoc_global.warn_error then incr Odoc_global.errors

let bad_magic_number = 
  "Bad magic number for this ocamldoc dump!\n"^
  "This dump was not created by this version of OCamldoc."

let not_a_module_name s = s^" is not a valid module name"
let load_file_error f e = "Error while loading file "^f^":\n"^e^"\n"
let wrong_format s = "Wrong format for \""^s^"\""
let errors_occured n = (string_of_int n)^" error(s) encountered"
let parse_error = "Parse error"
let text_parse_error l c s =
  let lines = Str.split (Str.regexp_string "\n") s in
  "Syntax error in text:\n"^s^"\n"^
  "line "^(string_of_int l)^", character "^(string_of_int c)^":\n"^
  (List.nth lines l)^"\n"^
  (String.make c ' ')^"^"

let tag_not_handled tag = "Tag @"^tag^" not handled by this generator"
let bad_tree = "Incorrect tree structure."
let not_a_valid_tag s = s^" is not a valid tag."
let fun_without_param f = "Function "^f^" has no parameter.";;
let method_without_param f = "M�thode "^f^" has no parameter.";;
let anonymous_parameters f = "Function "^f^" has anonymous parameters."
let function_colon f = "Function "^f^": "
let implicit_match_in_parameter = "Parameters contain implicit pattern matching."
let unknown_extension f = "Unknown extension for file "^f^"."
let two_implementations name = "There are two implementations of module "^name^"."
let two_interfaces name = "There are two interfaces of module "^name^"."
let too_many_module_objects name = "There are two many interfaces/implementation of module "^name^"."
let exception_not_found_in_implementation exc m = "Exception "^exc^" was not found in implementation of module "^m^"."
let type_not_found_in_implementation exc m = "Type "^exc^" was not found in implementation of module "^m^"."
let module_not_found_in_implementation m m2 = "Module "^m^" was not found in implementation of module "^m2^"."
let value_not_found_in_implementation v m = "Value "^v^" was not found in implementation of module "^m^"."
let class_not_found_in_implementation c m = "Class "^c^" was not found in implementation of module "^m^"."
let attribute_not_found_in_implementation a c = "Attribute "^a^" was not found in implementation of class "^c^"."
let method_not_found_in_implementation m c = "Method "^m^" was not found in implementation of class "^c^"."
let different_types t = "Definition of type "^t^" doesn't match from interface to implementation."
let attribute_type_not_found cl att = "The type of the attribute "^att^" could not be found in the signature of class "^cl^"."
let method_type_not_found cl met = "The type of the method "^met^" could not be found in the signature of class "^cl^"."
let module_not_found m m2 = "The module "^m2^" could not be found in the signature of module "^m^"."
let module_type_not_found m mt = "The module type "^mt^" could not be found in the signature of module "^m^"."
let value_not_found m v = "The value "^v^" could not be found in the signature of module "^m^"."
let exception_not_found m e = "The exception "^e^" could not be found in the signature of module "^m^"."
let type_not_found m t = "The type "^t^" could not be found in the signature of module "^m^"."
let class_not_found m c = "The class "^c^" could not be found in the signature of module "^m^"."
let class_type_not_found m c = "The class type "^c^" could not be found in the signature of module "^m^"."
let type_not_found_in_typedtree t = "Type "^t^" was not found in typed tree."
let exception_not_found_in_typedtree e = "Exception "^e^" was not found in typed tree."
let module_type_not_found_in_typedtree mt = "Module type "^mt^" was not found in typed tree."
let module_not_found_in_typedtree m = "Module "^m^" was not found in typed tree."
let class_not_found_in_typedtree c = "Class "^c^" was not found in typed tree."
let class_type_not_found_in_typedtree ct = "Class type "^ct^" was not found in typed tree."
let inherit_classexp_not_found_in_typedtree n = "Inheritance class expression number "^(string_of_int n)^" was not found in typed tree."
let attribute_not_found_in_typedtree att = "Class attribute "^att^" was not found in typed tree."
let method_not_found_in_typedtree met = "Class method "^met^" was not found in typed tree."

let cross_module_not_found n = "Module "^n^" not found"
let cross_module_type_not_found n = "Module type "^n^" not found"
let cross_module_or_module_type_not_found n = "Module or module type "^n^" not found"
let cross_class_not_found n = "Class "^n^" not found"
let cross_class_type_not_found n = "class type "^n^" not found"
let cross_class_or_class_type_not_found n = "Class or class type "^n^" not found"
let cross_exception_not_found n = "Exception "^n^" not found"
let cross_element_not_found n = "Element "^n^" not found"
let cross_method_not_found n = "Method "^n^" not found"
let cross_attribute_not_found n = "Attribute "^n^" not found"
let cross_section_not_found n = "Section "^n^" not found"
let cross_value_not_found n = "Attribute "^n^" not found"
let cross_type_not_found n = "Type "^n^" not found"

let object_end = "object ... end"
let struct_end = "struct ... end"
let sig_end = "sig ... end"

(** Messages for verbose mode. *)

let analysing f = "Analysing file "^f^"..."
let merging = "Merging..."
let cross_referencing = "Cross referencing..."
let generating_doc = "Generating documentation..."
let loading f = "Loading "^f^"..."
let file_generated f = "File "^f^" generated."
let file_exists_dont_generate f =
  "File "^f^" exists, we don't generate it."

(** Messages for documentation generation.*)

let modul = "Module"
let modules = "Modules"
let functors = "Functors"
let values = "Simple values"
let types = "Types"
let exceptions = "Exceptions"
let record = "Record"
let variant = "Variant"
let mutab = "mutable"
let functions = "Functions"
let parameters = "Parameters"
let abstract = "Abstract"
let functo = "Functor"
let clas = "Class"
let classes = "Classes"
let attributes = "Attributes"
let methods = "Methods"
let authors = "Author(s)"
let version = "Version"
let since = "Since"
let deprecated = "Deprecated."
let raises = "Raises"
let returns = "Returns"
let inherits = "Inherits"
let inheritance = "Inheritance"
let privat = "private"
let module_type = "Module type"
let class_type = "Class type"
let description = "Description"
let interface = "Interface"
let type_parameters = "Type parameters"
let class_types = "Class types"
let module_types = "Module types"
let see_also = "See also"
let documentation = "Documentation"
let index_of = "Index of"
let top = "Top"
let index_of_values = index_of^" values"
let index_of_exceptions = index_of^" exceptions"
let index_of_types = index_of^" types"
let index_of_attributes = index_of^" class attributes"
let index_of_methods = index_of^" class methods"
let index_of_classes = index_of^" classes"
let index_of_class_types = index_of^" class types"
let index_of_modules = index_of^" modules"
let index_of_module_types = index_of^" module types"
let previous = "Previous"
let next = "Next"
let up = "Up"

