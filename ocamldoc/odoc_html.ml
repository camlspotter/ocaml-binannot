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


(** Generation of html documentation. *)

let print_DEBUG s = print_string s ; print_newline ()

open Odoc_info 
open Parameter
open Value
open Type
open Exception
open Class 
open Module


(** The functions used for naming files and html marks.*)
module Naming =
  struct
    (** The prefix for types marks. *)
    let mark_type = "TYPE"
    (** The prefix for functions marks. *)
    let mark_function = "FUN"
    (** The prefix for exceptions marks. *)
    let mark_exception = "EXCEPTION"
    (** The prefix for values marks. *)
    let mark_value = "VAL"
    (** The prefix for attributes marks. *)
    let mark_attribute = "ATT"
    (** The prefix for methods marks. *)
    let mark_method = "METHOD"

    (** The prefix for code files.. *)
    let code_prefix = "code_"
    (** The prefix for type files.. *)
    let type_prefix = "type_"

    (** Return the two html files names for the given module or class name.*)
    let html_files name =
      let html_file = name^".html" in
      let html_frame_file = name^"-frame.html" in
      (html_file, html_frame_file)

    (** Return the target for the given prefix and simple name. *)
    let target pref simple_name = pref^simple_name
    (** Return the complete link target (file#target) for the given prefix string and complete name.*)
    let complete_target pref complete_name = 
      let simple_name = Name.simple complete_name in
      let module_name = 
	let s = Name.father complete_name in
	if s = "" then simple_name else s
      in
      let (html_file, _) = html_files module_name in
      html_file^"#"^(target pref simple_name)

    (** Return the link target for the given type. *)
    let type_target t = target mark_type (Name.simple t.ty_name)
    (** Return the complete link target for the given type. *)
    let complete_type_target t = complete_target mark_type t.ty_name

    (** Return the link target for the given exception. *)
    let exception_target e = target mark_exception (Name.simple e.ex_name)
    (** Return the complete link target for the given exception. *)
    let complete_exception_target e = complete_target mark_exception e.ex_name

    (** Return the link target for the given value. *)
    let value_target v = target mark_value (Name.simple v.val_name)
    (** Return the complete link target for the given value. *)
    let complete_value_target v = complete_target mark_value v.val_name
    (** Return the complete filename for the code of the given value. *)
    let file_code_value_complete_target v = 
      let f = code_prefix^mark_value^v.val_name^".html" in
      f

    (** Return the link target for the given attribute. *)
    let attribute_target a = target mark_attribute (Name.simple a.att_value.val_name)
    (** Return the complete link target for the given attribute. *)
    let complete_attribute_target a = complete_target mark_attribute a.att_value.val_name
    (** Return the complete filename for the code of the given attribute. *)
    let file_code_attribute_complete_target a = 
      let f = code_prefix^mark_attribute^a.att_value.val_name^".html" in
      f

    (** Return the link target for the given method. *)
    let method_target m = target mark_method (Name.simple m.met_value.val_name)
    (** Return the complete link target for the given method. *)
    let complete_method_target m = complete_target mark_method m.met_value.val_name
    (** Return the complete filename for the code of the given method. *)
    let file_code_method_complete_target m = 
      let f = code_prefix^mark_method^m.met_value.val_name^".html" in
      f

    (** Return the link target for the given label section. *)
    let label_target l = target "" l
    (** Return the complete link target for the given section label. *)
    let complete_label_target l = complete_target "" l

    (** Return the complete filename for the code of the type of the 
       given module or module type name. *)
    let file_type_module_complete_target name = 
      let f = type_prefix^name^".html" in
      f

    (** Return the complete filename for the code of the type of the 
       given class or class type name. *)
    let file_type_class_complete_target name = 
      let f = type_prefix^name^".html" in
      f
  end

(** A class with a method to colorize a string which represents OCaml code. *)
class ocaml_code =
  object(self)
    method html_of_code fmt ?(with_pre=true) code =
      Odoc_ocamlhtml.html_of_code fmt ~with_pre: with_pre code
  end


(** Generation of html code from text structures. *)
class text =
  object (self)
    (** We want to display colorized code. *)
    inherit ocaml_code 

    (** Escape the strings which would clash with html syntax, and
       make some replacements (double newlines replaced by <br>). *)
    method escape s = Odoc_ocamlhtml.escape_base s

    (** Print the html code corresponding to the [text] parameter. *)
    method html_of_text fmt t = List.iter (self#html_of_text_element fmt) t

    (** Print the html code for the [text_element] in parameter. *)
    method html_of_text_element fmt te =
      print_DEBUG "text::html_of_text_element";
      match te with
      |	Odoc_info.Raw s -> self#html_of_Raw fmt s
      |	Odoc_info.Code s -> self#html_of_Code fmt s
      |	Odoc_info.CodePre s -> self#html_of_CodePre fmt s
      |	Odoc_info.Verbatim s -> self#html_of_Verbatim fmt s
      |	Odoc_info.Bold t -> self#html_of_Bold fmt t
      |	Odoc_info.Italic t -> self#html_of_Italic fmt t 
      |	Odoc_info.Emphasize t -> self#html_of_Emphasize fmt t
      |	Odoc_info.Center t -> self#html_of_Center fmt t
      |	Odoc_info.Left t -> self#html_of_Left fmt t
      |	Odoc_info.Right t -> self#html_of_Right fmt t
      |	Odoc_info.List tl -> self#html_of_List fmt tl
      |	Odoc_info.Enum tl -> self#html_of_Enum fmt tl
      |	Odoc_info.Newline -> self#html_of_Newline fmt
      |	Odoc_info.Block t -> self#html_of_Block fmt t
      |	Odoc_info.Title (n, l_opt, t) -> self#html_of_Title fmt n l_opt t
      |	Odoc_info.Latex s -> self#html_of_Latex fmt s
      |	Odoc_info.Link (s, t) -> self#html_of_Link fmt s t
      |	Odoc_info.Ref (name, ref_opt) -> self#html_of_Ref fmt name ref_opt
      |	Odoc_info.Superscript t -> self#html_of_Superscript fmt t
      |	Odoc_info.Subscript t -> self#html_of_Subscript fmt t

    method html_of_Raw fmt s = 
      Format.pp_print_string fmt (self#escape s)

    method html_of_Code fmt s =
      if !Odoc_args.colorize_code then
	self#html_of_code fmt ~with_pre: false s
      else
	Format.fprintf fmt "@{<code class=\"%s\">%s@}" Odoc_ocamlhtml.code_class (self#escape s)

    method html_of_CodePre fmt s =
      Format.fprintf fmt "@{<pre>";
      if !Odoc_args.colorize_code then
	(
	 Format.fprintf fmt "@}";
	 self#html_of_code fmt s;
	 Format.fprintf fmt "@{<pre>"
	)
      else
	Format.fprintf fmt "@{<code class=\"%s\">%s@}" Odoc_ocamlhtml.code_class (self#escape s);
      Format.fprintf fmt "@}"

    method html_of_Verbatim fmt s = Format.fprintf fmt "@{<pre>%s@}" (self#escape s)

    method html_of_Bold fmt t =
      Format.fprintf fmt "@{<b>";
      self#html_of_text fmt t;
      Format.fprintf fmt "@}"

    method html_of_Italic fmt t =
      Format.fprintf fmt "@{<i>";
      self#html_of_text fmt t;
      Format.fprintf fmt "@}"

    method html_of_Emphasize fmt t = 
      Format.fprintf fmt "@{<em>";
      self#html_of_text fmt t;
      Format.fprintf fmt "@}"

    method html_of_Center fmt t =
      Format.fprintf fmt "@{<center>";
      self#html_of_text fmt t;
      Format.fprintf fmt "@}"

    method html_of_Left fmt t =
      Format.fprintf fmt "@{<div align=left>";
      self#html_of_text fmt t;
      Format.fprintf fmt "@}"

    method html_of_Right fmt t =
      Format.fprintf fmt "@{<div align=right>";
      self#html_of_text fmt t;
      Format.fprintf fmt "@}"

    method html_of_List fmt tl = 
      Format.fprintf fmt "@{<ul>";
      List.iter 
	(fun t -> 
	  Format.fprintf fmt "@{<li>" ; 
	  self#html_of_text fmt t;
	  Format.fprintf fmt "@}")
	tl;
      Format.fprintf fmt "@}"

    method html_of_Enum fmt tl =
      Format.fprintf fmt "@{<ol>";
      List.iter 
	(fun t -> 
	  Format.fprintf fmt "@{<li>" ; 
	  self#html_of_text fmt t;
	  Format.fprintf fmt "@}")
	tl;
      Format.fprintf fmt "@}"

    method html_of_Newline fmt = 
      Format.pp_print_string fmt "\n<p>\n"

    method html_of_Block fmt t =
      Format.fprintf fmt "@{<blockquote>";
      self#html_of_text fmt t;
      Format.fprintf fmt "@}"

    method html_of_Title fmt n label_opt t =
      let css_class = "title"^(string_of_int n) in
      Format.pp_print_string fmt "<br>\n";
      (
       match label_opt with
	 None -> ()
       | Some l -> Format.fprintf fmt "@{<mark \"%s\">@}" (Naming.label_target l)
      );
      Format.fprintf fmt "@{<table cellpadding=0 cellspacing=0 width=\"100%%\">";
      Format.fprintf fmt "@{<tr class=\"%s\">@{<td>@{<div align=center>" css_class;
      Format.fprintf fmt "@{<table>@{<tr class=\"%s\">\n" css_class;
      Format.fprintf fmt "@{<td width=\"100%%\" align=center>";
      Format.fprintf fmt "@{<span class=\"%s\">" css_class;
      self#html_of_text fmt t;
      Format.fprintf fmt "@}@}@}@}@}@}@}@}"

    method html_of_Latex fmt _ = ()
      (* don't care about LaTeX stuff in HTML. *)

    method html_of_Link fmt s t =
      Format.fprintf fmt "@{<href \"%s\">" s;
      self#html_of_text fmt t;
      Format.fprintf fmt "@}"

    method html_of_Ref fmt name ref_opt =
      match ref_opt with
	None -> 
	  self#html_of_text_element fmt (Odoc_info.Code name)
      |	Some kind ->
	  let target = 
	    match kind with
	      Odoc_info.RK_module 
	    | Odoc_info.RK_module_type
	    | Odoc_info.RK_class
	    | Odoc_info.RK_class_type ->
		let (html_file, _) = Naming.html_files name in
		html_file
	    | Odoc_info.RK_value -> Naming.complete_target Naming.mark_value name
	    | Odoc_info.RK_type -> Naming.complete_target Naming.mark_type name
	    | Odoc_info.RK_exception -> Naming.complete_target Naming.mark_exception name
	    | Odoc_info.RK_attribute -> Naming.complete_target Naming.mark_attribute name
	    | Odoc_info.RK_method -> Naming.complete_target Naming.mark_method name
	    | Odoc_info.RK_section -> Naming.complete_label_target name
	  in
	  Format.fprintf fmt "@{<href \"%s\">" target;
	  self#html_of_text_element fmt (Odoc_info.Code (Odoc_info.use_hidden_modules name));
	  Format.fprintf fmt "@}"

    method html_of_Superscript fmt t =
      Format.fprintf fmt "@{<sup class=\"superscript\">";
      self#html_of_text fmt t;
      Format.fprintf fmt "@}" 

    method html_of_Subscript fmt t =
      Format.fprintf fmt "@{<sup class=\"subscript\">";
      self#html_of_text fmt t;
      Format.fprintf fmt "@}" 
  end

(** A class used to generate html code for info structures. *)
class virtual info =
  object (self)
    (** The list of pairs [(tag, f)] where [f] is a function taking
       the [text] associated to [tag] and returning html code. 
       Add a pair here to handle a tag.*)
    val mutable tag_functions = ([] : (string * (Format.formatter -> Odoc_info.text -> unit)) list)

    (** The method used to get html code from a [text]. *)
    method virtual html_of_text : Format.formatter -> Odoc_info.text -> unit

    (** Print html for an author list. *)
    method html_of_author_list fmt l =
      match l with
	[] -> ()
      | _ -> Format.fprintf fmt "@{<b>%s: @}%s<br>\n"
	    Odoc_messages.authors (String.concat ", " l)

    (** Print html code for the given optional version information.*)
    method html_of_version_opt fmt v_opt =
      match v_opt with
	None -> ()
      | Some v -> Format.fprintf fmt "@{<b>%s: @}%s<br>\n" Odoc_messages.version v

    (** Print html code for the given optional since information.*)
    method html_of_since_opt fmt s_opt =
      match s_opt with
	None -> ()
      | Some s -> Format.fprintf fmt "@{<b>%s :@}%s<br>\n" Odoc_messages.since s

    (** Print html code for the given list of raised exceptions.*)
    method html_of_raised_exceptions fmt l =
      match l with
	[] -> ()
      | (s, t) :: [] -> 
	  Format.fprintf fmt "@{<b>%s@} @{<code>%s@} " Odoc_messages.raises s;
	  self#html_of_text fmt t;
	  Format.pp_print_string fmt "<br>\n"
      | _ ->
	  Format.fprintf fmt "@{<b>%s@}@{<ul>" Odoc_messages.raises;
	  List.iter
	    (fun (ex, desc) -> 
	      Format.fprintf fmt "@{<li>@{<code>%s@} " ex;
	      self#html_of_text fmt desc;
	      Format.fprintf fmt "@}"
	    )
	    l;
	  Format.fprintf fmt "@}"

    (** Print html code for the given "see also" reference. *)
    method html_of_see fmt (see_ref, t)  =
      let t_ref = 
	match see_ref with
	  Odoc_info.See_url s -> [ Odoc_info.Link (s, t) ]
	| Odoc_info.See_file s -> (Odoc_info.Code s) :: (Odoc_info.Raw " ") :: t
	| Odoc_info.See_doc s -> (Odoc_info.Italic [Odoc_info.Raw s]) :: (Odoc_info.Raw " ") :: t
      in
      self#html_of_text fmt t_ref

    (** Print html code for the given list of "see also" references.*)
    method html_of_sees fmt l =
      match l with
	[] -> ()
      | see :: [] -> 
	  Format.fprintf fmt "@{<b>%s@} " Odoc_messages.see_also;
	  self#html_of_see fmt see;
	  Format.pp_print_string fmt "<br>\n"
      | _ ->
	  Format.fprintf fmt "@{<b>%s@}@{<ul>" Odoc_messages.see_also;
	  List.iter
	    (fun see -> 
	      Format.fprintf fmt "@{<li>";
	      self#html_of_see fmt see;
	      Format.fprintf fmt "@}"
	    )
	    l;
	  Format.fprintf fmt "@}"

    (** Print html code for the given optional return information.*)
    method html_of_return_opt fmt return_opt =
      match return_opt with
	None -> ()
      | Some s -> 
	  Format.fprintf fmt "@{<b>%s@} " Odoc_messages.returns;
	  self#html_of_text fmt s;
	  Format.pp_print_string fmt "<br>\n"

    (** Print html code for the given list of custom tagged texts. *)
    method html_of_custom fmt l =
      let buf = Buffer.create 50 in
      List.iter
	(fun (tag, text) ->
	  try
	    let f = List.assoc tag tag_functions in
	    f fmt text
	  with
	    Not_found ->
	      Odoc_info.warning (Odoc_messages.tag_not_handled tag)
	)
	l

    (** Print html code for a description, except for the [i_params] field. *)
    method html_of_info fmt info_opt =
      match info_opt with
	None -> ()
      | Some info ->
	  let module M = Odoc_info in
	  Format.fprintf fmt "@{<div class=\"info\">\n";
	  (match info.M.i_deprecated with
	    None -> ()
	  | Some d -> 
	      Format.fprintf fmt "@{<span class=\"warning\">%s@} " Odoc_messages.deprecated;
	      self#html_of_text fmt d;
	      Format.pp_print_string fmt "<br>\n"
	  );
	  (match info.M.i_desc with
	    None -> ()
	  | Some d when d = [Odoc_info.Raw ""] -> ()
	  | Some d -> self#html_of_text fmt d; Format.pp_print_string fmt "<br>\n"
	  );
	  self#html_of_author_list fmt info.M.i_authors;
	  self#html_of_version_opt fmt info.M.i_version;
	  self#html_of_since_opt fmt info.M.i_since;
	  self#html_of_raised_exceptions fmt info.M.i_raised_exceptions;
	  self#html_of_return_opt fmt info.M.i_return_value;
	  self#html_of_sees fmt info.M.i_sees;
	  self#html_of_custom fmt info.M.i_custom;
	  Format.fprintf fmt "@}\n"

    (** Print html code for the first sentence of a description. *)
    method html_of_info_first_sentence fmt info_opt =
      match info_opt with
	None -> ()
      | Some info ->
	  let module M = Odoc_info in
	  let dep = info.M.i_deprecated <> None in
	  Format.fprintf fmt "@{<div class=\"info\">";
	  if dep then Format.fprintf fmt "@{<font color=\"#CCCCCC\">";
	  (match info.M.i_desc with
	    None -> ()
	  | Some d when d = [Odoc_info.Raw ""] -> ()
	  | Some d -> 
	      self#html_of_text fmt (Odoc_info.first_sentence_of_text d)
	  );
	  if dep then Format.fprintf fmt "@}";
	  Format.fprintf fmt "@}"
  end



let opt = Odoc_info.apply_opt

(** This class is used to create objects which can generate a simple html documentation. *)
class html =
  object (self)
    inherit text
    inherit info

    (** The default style options. *)
    val mutable default_style_options =
      ["a:visited {color : #416DFF; text-decoration : none; }" ;
	"a:link {color : #416DFF; text-decoration : none;}" ;
	"a:hover {color : Red; text-decoration : none; background-color: #5FFF88}" ;
	"a:active {color : Red; text-decoration : underline; }" ;
	".keyword { font-weight : bold ; color : Red }" ;
	".keywordsign { color : #C04600 }" ; 
	".superscript { font-size : 4 }" ;
	".subscript { font-size : 4 }" ;
	".comment { color : Green }" ;
	".constructor { color : Blue }" ;
	".type { color : DarkSlateBlue }" ;
	".string { color : Maroon }" ;
	".warning { color : Red ; font-weight : bold }" ;
	".info { margin-left : 3em; margin-right : 3em }" ;
	".code { color : #465F91 ; }" ;
	".title1 { font-size : 20pt ; background-color : #416DFF }" ;
	".title2 { font-size : 20pt ; background-color : #418DFF }" ;
	".title3 { font-size : 20pt ; background-color : #41ADFF }" ;
	".title4 { font-size : 20pt ; background-color : #41CDFF }" ;
	".title5 { font-size : 20pt ; background-color : #41EDFF }" ;
	".title6 { font-size : 20pt ; background-color : #41FFFF }" ;
	"body { background-color : White }" ;
	"tr { background-color : White }" ;
      ]	
      
    (** The style file for all pages. *)
    val mutable style_file = "style.css"

    (** The code to import the style. Initialized in [init_style]. *)
    val mutable style = ""

    (** The known types names. 
       Used to know if we must create a link to a type
       when printing a type. *)
    val mutable known_types_names = []

    (** The known class and class type names. 
       Used to know if we must create a link to a class 
       or class type or not when printing a type. *)
    val mutable known_classes_names = []

    (** The known modules and module types names. 
       Used to know if we must create a link to a type or not
       when printing a module type. *)
    val mutable known_modules_names = []

    (** The main file. *)
    val mutable index = "index.html"
    (** The file for the index of values. *)
    val mutable index_values = "index_values.html"
    (** The file for the index of types. *)
    val mutable index_types = "index_types.html"
    (** The file for the index of exceptions. *)
    val mutable index_exceptions = "index_exceptions.html"
    (** The file for the index of attributes. *)
    val mutable index_attributes = "index_attributes.html"
    (** The file for the index of methods. *)
    val mutable index_methods = "index_methods.html"
    (** The file for the index of classes. *)
    val mutable index_classes = "index_classes.html"
    (** The file for the index of class types. *)
    val mutable index_class_types = "index_class_types.html"
    (** The file for the index of modules. *)
    val mutable index_modules = "index_modules.html"
    (** The file for the index of module types. *)
    val mutable index_module_types = "index_module_types.html"


    (** The list of attributes. Filled in the [generate] method. *)
    val mutable list_attributes = []
    (** The list of methods. Filled in the [generate] method. *)
    val mutable list_methods = []
    (** The list of values. Filled in the [generate] method. *)
    val mutable list_values = []
    (** The list of exceptions. Filled in the [generate] method. *)
    val mutable list_exceptions = []
    (** The list of types. Filled in the [generate] method. *)
    val mutable list_types = []
    (** The list of modules. Filled in the [generate] method. *)
    val mutable list_modules = []
    (** The list of module types. Filled in the [generate] method. *)
    val mutable list_module_types = []
    (** The list of classes. Filled in the [generate] method. *)
    val mutable list_classes = []
    (** The list of class types. Filled in the [generate] method. *)
    val mutable list_class_types = []

    (** The header of pages. Must be prepared by the [prepare_header] method.*)
    val mutable header = fun fmt -> fun ?(nav=None) -> fun _ -> ()

    (** Init the style. *)
    method init_style =
      (match !Odoc_args.css_style with
	None -> 
	  let default_style = String.concat "\n" default_style_options in
	  (
	   try
	     let chanout = open_out (Filename.concat !Odoc_args.target_dir style_file) in
	     output_string chanout default_style ;
	     flush chanout ;
	     close_out chanout
	   with
	     Sys_error s ->
	       prerr_endline s ;
	       incr Odoc_info.errors ;
	  )
      | Some f ->
	  style_file <- f 
      );
      style <- "<link rel=\"stylesheet\" href=\""^style_file^"\" type=\"text/css\">"

    (** Get the title given by the user *)
    method title = match !Odoc_args.title with None -> "" | Some t -> self#escape t

    (** Get the title given by the user completed with the given subtitle. *)
    method inner_title s = 
      (match self#title with "" -> "" | t -> t^" : ")^
      (self#escape s)

    (** Get the page header. *)
    method header fmt ?nav title = header fmt ?nav title

    (** A function to build the header of pages. *)
    method prepare_header module_list =
      let f fmt ?(nav=None) t = 
	let link_if_not_empty l m url =
	  match l with
	    [] -> ()
	  | _ -> Format.fprintf fmt "<link title=\"%s\" rel=Appendix href=\"%s\">\n" m url
	in
	Format.fprintf fmt "@{<head>%s\n" style;
	Format.fprintf fmt "<link rel=\"Start\" href=\"%s\">\n" index;
	(
	 match nav with
	   None -> ()
	 | Some (pre_opt, post_opt, name) ->
	     (match pre_opt with
	       None -> ()
	     | Some name -> 
		 Format.fprintf fmt "<link rel=\"previous\" href=\"%s\">\n"
		   (fst (Naming.html_files name))
	     );
	     (match post_opt with
	       None -> ()
	     | Some name -> 
		 Format.fprintf fmt "<link rel=\"next\" href=\"%s\">\n"
		   (fst (Naming.html_files name))
	     );
	     let father = Name.father name in
	     let href = if father = "" then index else fst (Naming.html_files father) in
	     Format.fprintf fmt "<link rel=\"Up\" href=\"%s\">\n" href
	);
	link_if_not_empty list_types Odoc_messages.index_of_types index_types;
	link_if_not_empty list_exceptions Odoc_messages.index_of_exceptions index_exceptions;
	link_if_not_empty list_values Odoc_messages.index_of_values index_values;
	link_if_not_empty list_attributes Odoc_messages.index_of_attributes index_attributes;
	link_if_not_empty list_methods Odoc_messages.index_of_methods index_methods;
	link_if_not_empty list_classes Odoc_messages.index_of_classes index_classes;
	link_if_not_empty list_class_types Odoc_messages.index_of_class_types index_class_types;
	link_if_not_empty list_modules Odoc_messages.index_of_modules index_modules;
	link_if_not_empty list_module_types Odoc_messages.index_of_module_types index_module_types;
	List.iter
	  (fun m -> 
	    let html_file = fst (Naming.html_files m.m_name) in
	    Format.fprintf fmt 
	      "<link title=\"%s\" rel=\"Chapter\" href=\"%s\">\n"
	      m.m_name html_file
	  )
	  module_list;
	Format.fprintf fmt "@{<title>%s@}@}" t
      in
      header <- f

    (** Print HTML code for navigation bar. 
       @param pre optional name for optional previous module/class 
       @param post optional name for optional next module/class
       @param name name of current module/class *)
    method navbar fmt pre post name =
      Format.fprintf fmt "@{<div class=\"navbar\">";
      (match pre with
	None -> ()
      | Some name -> 
	  Format.fprintf fmt "@{<href \"%s\">%s@}\n"
	    (fst (Naming.html_files name))
	    Odoc_messages.previous
      );
      let father = Name.father name in
      let href = if father = "" then index else fst (Naming.html_files father) in
      Format.fprintf fmt "&nbsp;@{<href \"%s\">%s@}\n&nbsp;" href Odoc_messages.up;
    
      (match post with
	None -> ()
      | Some name -> Format.fprintf fmt "@{<href \"%s\">%s@}\n"
	    (fst (Naming.html_files name)) Odoc_messages.next
      );
      Format.fprintf fmt "@}"

    (** Print html code with the given string in the keyword style.*)
    method keyword fmt s = 
      Format.fprintf fmt "@{<span class=\"keyword\">%s@}" s

    (** Print html code with the given string in the constructor style. *)
    method constructor fmt s = 
      Format.fprintf fmt "@{<span class=\"constructor\">%s@}" s

    (** Output the given ocaml code to the given file name. *)
    method private output_code in_title file code =
      try
	let (fmt, chanout) = self#formatter_of_file file in
	Format.fprintf fmt "@{<html>";
	self#header fmt (self#inner_title in_title);
	Format.fprintf fmt "@{<body>";
	self#html_of_code fmt code;
	Format.fprintf fmt "@}@}";
	Format.pp_print_flush fmt ();
	close_out chanout
      with
	Sys_error s -> 
	  incr Odoc_info.errors ;
	  prerr_endline s

    (** Take a string and return the string where fully qualified 
       type (or class or class type) idents 
       have been replaced by links to the type referenced by the ident.*)
    method create_fully_qualified_idents_links m_name s =
      let f str_t = 
	let match_s = Str.matched_string str_t in
	let rel = Name.get_relative m_name match_s in
	let s_final = Odoc_info.apply_if_equal 
	    Odoc_info.use_hidden_modules 
	    match_s
	    rel
	in
	if List.mem match_s known_types_names then
	   "<a href=\""^(Naming.complete_target Naming.mark_type match_s)^"\">"^
	   s_final^
	   "</a>"
	else
	  if List.mem match_s known_classes_names then
	    let (html_file, _) = Naming.html_files match_s in
	    "<a href=\""^html_file^"\">"^s_final^"</a>"
	  else
	    s_final
      in
      let s2 = Str.global_substitute
	  (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)")
	  f
	  s
      in
      s2

    (** Take a string and return the string where fully qualified module idents 
       have been replaced by links to the module referenced by the ident.*)
    method create_fully_qualified_module_idents_links m_name s =
      let f str_t = 
	let match_s = Str.matched_string str_t in
	if List.mem match_s known_modules_names then
	  let (html_file, _) = Naming.html_files match_s in
	  "<a href=\""^html_file^"\">"^(Name.get_relative m_name match_s)^"</a>"
	else
	  match_s
      in
      let s2 = Str.global_substitute
	  (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([A-Z][a-zA-Z_'0-9]*\\)")
	  f
	  s
      in
      s2

    (** Print html code to display a [Types.type_expr].*)
    method html_of_type_expr fmt m_name t =
      let s = String.concat "\n"
	  (Str.split (Str.regexp "\n") (Odoc_info.string_of_type_expr t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "<br>       " s in
      Format.fprintf fmt "@{<code class=\"type\">%s@}"
	(self#create_fully_qualified_idents_links m_name s2)

    (** Print html code to display a [Types.class_type].*)
    method html_of_class_type_expr fmt m_name t =
      let s = String.concat "\n"
	  (Str.split (Str.regexp "\n") (Odoc_info.string_of_class_type t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "<br>       " s in
      Format.fprintf fmt "@{<code class=\"type\">%s@}"
	(self#create_fully_qualified_idents_links m_name s2)

    (** Print html code to display a [Types.type_expr list].*)
    method html_of_type_expr_list fmt m_name sep l =
      print_DEBUG "html#html_of_type_expr_list";
      let s = Odoc_info.string_of_type_list sep l in
      print_DEBUG "html#html_of_type_expr_list: 1";
      let s2 = Str.global_replace (Str.regexp "\n") "<br>       " s in
      print_DEBUG "html#html_of_type_expr_list: 2";
      Format.fprintf fmt "@{<code class=\"type\">%s@}"
	(self#create_fully_qualified_idents_links m_name s2)

    (** Print html code to display a [Types.module_type]. *)
    method html_of_module_type fmt m_name t =
      let s = String.concat "\n"
	  (Str.split (Str.regexp "\n") (Odoc_info.string_of_module_type t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "<br>       " s in
      Format.fprintf fmt "@{<code class=\"type\">%s@}"
	(self#create_fully_qualified_module_idents_links m_name s2)

    (** Generate a file containing the module type in the given file name. *)
    method output_module_type in_title file mtyp =
      let s = String.concat "\n"
	  (Str.split (Str.regexp "\n") (Odoc_info.string_of_module_type ~complete: true mtyp))
      in
      self#output_code in_title file s

    (** Generate a file containing the class type in the given file name. *)
    method output_class_type in_title file ctyp =
      let s = String.concat "\n"
	  (Str.split (Str.regexp "\n") (Odoc_info.string_of_class_type ~complete: true ctyp))
      in
      self#output_code in_title file s


    (** Print html code for a value. *)
    method html_of_value fmt v =
      Odoc_info.reset_type_names ();
      Format.fprintf fmt "@{<pre>";
      self#keyword fmt "val";
      (* html mark *)
      Format.fprintf fmt " @{<mark \"%s\">@}" (Naming.value_target v);
      (match v.val_code with 
	None -> Format.fprintf fmt "%s" (Name.simple v.val_name)
      | Some c -> 
	  let file = Naming.file_code_value_complete_target v in
	  self#output_code v.val_name (Filename.concat !Odoc_args.target_dir file) c;
	  Format.fprintf fmt "@{<href \"%s\">%s@}" file (Name.simple v.val_name)
      );
      Format.pp_print_string fmt " : ";
      self#html_of_type_expr fmt (Name.father v.val_name) v.val_type;
      Format.fprintf fmt "@}";
      self#html_of_info fmt v.val_info;

      if !Odoc_args.with_parameter_list then
	self#html_of_parameter_list fmt (Name.father v.val_name) v.val_parameters
      else
	self#html_of_described_parameter_list fmt (Name.father v.val_name) v.val_parameters

    (** Print html code for an exception. *)
    method html_of_exception fmt e =
      Odoc_info.reset_type_names ();
      Format.fprintf fmt "@{<pre>";
      self#keyword fmt "exception";
      (* html mark *)
      Format.fprintf fmt " @{<mark \"%s\">@}" (Naming.exception_target e);
      Format.pp_print_string fmt (Name.simple e.ex_name);
      (match e.ex_args with
	[] -> ()
      |	_ -> 
	  Format.pp_print_string fmt  " ";
	  self#keyword fmt "of";
	  Format.pp_print_string fmt " ";
	  self#html_of_type_expr_list fmt (Name.father e.ex_name) " * " e.ex_args
      );
      (match e.ex_alias with
	None -> ()
      | Some ea -> 
	  Format.pp_print_string fmt " = ";
	  (
	   match ea.ea_ex with
	     None -> Format.pp_print_string fmt ea.ea_name
	   | Some e -> Format.fprintf fmt "@{<href \"%s\">%s@}" (Naming.complete_exception_target e) e.ex_name
	  )
      );
      Format.fprintf fmt "@}";
      self#html_of_info fmt e.ex_info

    (** Print html code for a type. *)
    method html_of_type fmt t =
      Odoc_info.reset_type_names ();
      let father = Name.father t.ty_name in
      Format.fprintf fmt "<br>@{<code>";
      self#keyword fmt "type";
      (* html mark *)
      Format.fprintf fmt " <mark \"%s\">" (Naming.type_target t);
      (match t.ty_parameters with
	[] -> ()
      |	tp :: [] -> self#html_of_type_expr fmt father tp; Format.pp_print_string fmt " "
      |	l -> 
	  Format.pp_print_string fmt "(";
	  self#html_of_type_expr_list fmt father ", " l;
	  Format.pp_print_string fmt ") "
      );
      Format.fprintf fmt "%s " (Name.simple t.ty_name);
      (match t.ty_manifest with 
	None -> () 
      | Some typ -> Format.pp_print_string fmt "= ";
	  self#html_of_type_expr fmt father typ;
	  Format.pp_print_string fmt " "
      );
      (match t.ty_kind with
	Type_abstract -> Format.fprintf fmt "@}"
      |	Type_variant l ->
	  Format.fprintf fmt "=<br>@}@{<table border=\"0\" cellpadding=\"1\">";
	  List.iter
	    (fun constr ->
	      Format.fprintf fmt "@{<tr>@{<td align=\"left\" valign=\"top\">@{<code>";
	      self#keyword fmt "|";
	      Format.fprintf fmt "@}@}@{<td align=\"left\" valign=\"top\">@{<code>";
	      self#constructor fmt constr.vc_name;
	      (match constr.vc_args with
		[] -> ()
	      | l -> 
		  Format.pp_print_string fmt " ";
		  self#keyword fmt "of";
		  Format.pp_print_string fmt " ";
		  self#html_of_type_expr_list fmt father " * " l
	      );
	      Format.fprintf fmt "@}@}";
	      (
	       match constr.vc_text with
		 None -> ()
	       | Some t ->
		   Format.fprintf fmt "@{<td align=\"left\" valign=\"top\">@{<code>";
		   Format.fprintf fmt "(*@}@}@{<td align=\"left\" valign=\"top\">@{<code>";
		   self#html_of_text fmt t;
		  Format.fprintf fmt "@}@}@{<td align=\"left\" valign=\"bottom\">@{<code>*)@}@}"
	      );
	      Format.fprintf fmt "@}"
	    )
	    l;
	  Format.fprintf fmt "@}\n"

      |	Type_record l ->
	  Format.fprintf fmt "= {<br>@}@{<table border=\"0\" cellpadding=\"1\">";
	  List.iter
	    (fun r ->
	      Format.fprintf fmt "@{<tr>@{<td align=\"left\" valign=\"top\">@{<code>&nbsp;&nbsp;@}";
	      Format.fprintf fmt "@}@{<td align=\"left\" valign=\"top\" >@{<code>";
	      if r.rf_mutable then self#keyword fmt "mutable&nbsp;";
	      Format.fprintf fmt "%s&nbsp;: " r.rf_name;
	      self#html_of_type_expr fmt father r.rf_type;
	      Format.fprintf fmt ";@}@}";
	      (
	       match r.rf_text with
		 None -> ()
	       | Some t ->
		   Format.fprintf fmt "@{<td align=\"left\" valign=\"top\">@{<code>(*@}@}";
		   Format.fprintf fmt "@{<td align=\"left\" valign=\"top\">@{<code>";
		   self#html_of_text fmt t;
		   Format.fprintf fmt "@}@}@{<td align=\"left\" valign=\"bottom\">@{<code>*)@}@}"
	      );
	      Format.fprintf fmt "@}"
	    )
	    l;
	  Format.fprintf fmt "@}}\n"
      );
      self#html_of_info fmt t.ty_info;
      Format.pp_print_string fmt "<br>\n"

    (** Print html code for a class attribute. *)
    method html_of_attribute fmt a =
      let module_name = Name.father (Name.father a.att_value.val_name) in
      Format.fprintf fmt "@{<pre>";
      self#keyword fmt "val";
      (* html mark *)
      Format.fprintf fmt " @{<mark \"%s\">@}" (Naming.attribute_target a);

      if a.att_mutable then 
	(
	 self#keyword fmt Odoc_messages.mutab;
	 Format.pp_print_string fmt " "
	);
      (match a.att_value.val_code with 
	None -> Format.pp_print_string fmt (Name.simple a.att_value.val_name)
      | Some c -> 
	  let file = Naming.file_code_attribute_complete_target a in
	  self#output_code a.att_value.val_name (Filename.concat !Odoc_args.target_dir file) c;
	  Format.fprintf fmt "@{<href \"%s\">%s@}" file (Name.simple a.att_value.val_name)
      );
      Format.pp_print_string fmt " : ";
      self#html_of_type_expr fmt module_name  a.att_value.val_type;
      Format.fprintf fmt "@}";
      self#html_of_info fmt a.att_value.val_info

    (** Print html code for a class method. *)
    method html_of_method fmt m =
      let module_name = Name.father (Name.father m.met_value.val_name) in
      Format.fprintf fmt "@{<pre>";
      self#keyword fmt "method" ;
      (* html mark *)
      Format.fprintf fmt " @{<mark \"%s\">@}" (Naming.method_target m);
      if m.met_private then 
	(
	 self#keyword fmt "private";
	 Format.pp_print_string fmt " ";
	);
      if m.met_virtual then 
	(
	 self#keyword fmt "virtual";
	 Format.pp_print_string fmt " ";
	);
      (match m.met_value.val_code with 
	None -> Format.pp_print_string fmt (Name.simple m.met_value.val_name)
      | Some c -> 
	  let file = Naming.file_code_method_complete_target m in
	  self#output_code m.met_value.val_name (Filename.concat !Odoc_args.target_dir file) c;
	  Format.fprintf fmt "@{<href \"%s\">%s@}" file (Name.simple m.met_value.val_name)
      );
      Format.pp_print_string fmt " : ";
      self#html_of_type_expr fmt module_name m.met_value.val_type;
      Format.fprintf fmt "@}";
      self#html_of_info fmt m.met_value.val_info;
      (if !Odoc_args.with_parameter_list then
	self#html_of_parameter_list fmt module_name m.met_value.val_parameters
      else
	self#html_of_described_parameter_list fmt module_name m.met_value.val_parameters
      )

    (** Print html code for the description of a function parameter. *)
    method html_of_parameter_description fmt p =
      match Parameter.names p with
	[] -> ()
      | name :: [] ->
	  (
           (* Only one name, no need for label for the description. *)
	   match Parameter.desc_by_name p name with
	     None -> ()
	   | Some t -> self#html_of_text fmt t
	  )
      | l ->
          (*  A list of names, we display those with a description. *)
	  let l2 = List.filter (fun n -> (Parameter.desc_by_name p n) <> None) l in
	  List.iter
	    (fun n ->
	      match Parameter.desc_by_name p n with
		None -> ()
	      | Some t -> 
		  Format.fprintf fmt "@{<code>%s@} : " n ;
		  self#html_of_text fmt t;
		  Format.pp_print_string fmt "<br>\n"
	    )
	    l2

    (** Print html code for a list of parameters. *)
    method html_of_parameter_list fmt m_name l =
      match l with
	[] -> ()
      | _ ->
	  Format.fprintf fmt "@{<div class=\"info\">";
	  Format.fprintf fmt "@{<table border=\"0\" cellpadding=\"3\" width=\"100%%\">\n@{<tr>";
	  Format.fprintf fmt
	    "@{<td align=\"left\" valign=\"top\" width=\"1%%\">@{<b>%s: @}@}" 
	    Odoc_messages.parameters;
	  Format.fprintf fmt "@{<td>@{<table border=\"0\" cellpadding=\"5\" cellspacing=\"0\">";
	  List.iter
	    (fun p ->
	      Format.fprintf fmt
		"@{<tr>@{<td align=\"center\" valign=\"top\" width=\"15%%\" class=\"code\">%s@}"
		(match Parameter.complete_name p with
		  "" -> "?"
		| s -> s);
	      Format.fprintf fmt "@{<td align=\"center\" valign=\"top\">:@}@{<td>";
	      self#html_of_type_expr fmt m_name (Parameter.typ p);
	      Format.pp_print_string fmt "<br>\n";
	      self#html_of_parameter_description fmt p;
	      Format.fprintf fmt "@}@}"
	    )
	    l;

	  Format.fprintf fmt "@}@}@}@}@}"

    (** Print html code for the parameters which have a name and description. *)
    method html_of_described_parameter_list fmt m_name l =
      (* get the params which have a name, and at least one name described. *)
      let l2 = List.filter 
	  (fun p -> 
	    List.exists 
	      (fun n -> (Parameter.desc_by_name p n) <> None)
	      (Parameter.names p))
	  l
      in
      let f p =
	Format.fprintf fmt "@{<div class=\"info\">@{<code class=\"code\">%s@} : " (Parameter.complete_name p);
	self#html_of_parameter_description fmt p;
	Format.fprintf fmt "@}"
      in
      match l2 with
	[] -> ()
      |	_ -> Format.fprintf fmt "<br>";  List.iter f l2

    (** Return html code for a list of module parameters. *)
    method html_of_module_parameter_list fmt m_name l =
      match l with
	[] -> ()
      | _ ->
	  Format.fprintf fmt "@{<table border=\"0\" cellpadding=\"3\" width=\"100%%\">@{<tr>";
	  Format.fprintf fmt "@{<td align=\"left\" valign=\"top\" width=\"1%%\">@{<b>%s: @}@}" Odoc_messages.parameters;
	  Format.fprintf fmt "@{<td>@{<table border=\"0\" cellpadding=\"5\" cellspacing=\"0\">";
	  List.iter
	    (fun (p, desc_opt) ->
	      Format.fprintf fmt "@{<tr>@{<td align=\"center\" valign=\"top\" width=\"15%%\">@{<code>%s@}@}" p.mp_name;
	      Format.fprintf fmt "@{<td align=\"center\" valign=\"top\">:@}@{<td>";
	      self#html_of_module_type fmt m_name p.mp_type;
	      (match desc_opt with
		None -> ()
	      | Some t -> Format.pp_print_string fmt "<br>\n"; self#html_of_text fmt t
	      );
	      Format.fprintf fmt "@}@}"
	    )
	    l;

	  Format.fprintf fmt "@}@}@}@}"

    (** Print html code for a module. *)
    method html_of_module fmt ?(info=true) ?(complete=true) ?(with_link=true) m =
      let (html_file, _) = Naming.html_files m.m_name in
      let father = Name.father m.m_name in
      Format.fprintf fmt "@{<pre>";
      self#keyword fmt "module";
      Format.pp_print_string fmt " ";

      if with_link then
	Format.fprintf fmt "@{<href \"%s\">%s@}" html_file (Name.simple m.m_name)
      else
	Format.fprintf fmt "%s" (Name.simple m.m_name);

      Format.pp_print_string fmt ": ";
      self#html_of_module_type fmt father m.m_type;
      Format.fprintf fmt "@}";
      if info then
	(if complete then self#html_of_info
	else self#html_of_info_first_sentence) fmt  m.m_info
      

    (** Print html code for a module type. *)
    method html_of_modtype fmt ?(info=true) ?(complete=true) ?(with_link=true) mt =
      let (html_file, _) = Naming.html_files mt.mt_name in
      let father = Name.father mt.mt_name in
      Format.fprintf fmt "@{<pre>";
      self#keyword fmt "module type";
      Format.pp_print_string fmt " ";

      if with_link then
	Format.fprintf fmt "@{<href \"%s\">%s@}" html_file (Name.simple mt.mt_name)
      else
	Format.fprintf fmt "%s" (Name.simple mt.mt_name);

      (match mt.mt_type with
	None -> ()
      |	Some mtyp -> 
	  Format.pp_print_string fmt " = ";
	  self#html_of_module_type fmt father mtyp
      );
      Format.fprintf fmt "@}";
      if info then
	(if complete then self#html_of_info else self#html_of_info_first_sentence) fmt mt.mt_info

    (** Print html code for an included module. *)
    method html_of_included_module fmt im =
      Format.fprintf fmt "@{<pre>";
      self#keyword fmt "include";
      Format.pp_print_string fmt " ";
      (
       match im.im_module with
	 None -> Format.pp_print_string fmt im.im_name
       | Some mmt ->
	   let (file, name) = 
	     match mmt with
	       Mod m -> 
		 let (html_file, _) = Naming.html_files m.m_name in
		 (html_file, m.m_name)
	     | Modtype mt ->
		 let (html_file, _) = Naming.html_files mt.mt_name in
		 (html_file, mt.mt_name)
	   in
	   Format.fprintf fmt "@{<href \"%s\">%s@}" file (Name.simple name)
      );
      Format.fprintf fmt "@}"

    (** Print html code for a class. *)
    method html_of_class fmt ?(complete=true) ?(with_link=true) c =
      let father = Name.father c.cl_name in
      Odoc_info.reset_type_names ();
      let (html_file, _) = Naming.html_files c.cl_name in
      Format.fprintf fmt "@{<pre>";
      self#keyword fmt "class";
      Format.pp_print_string fmt " ";
      (* we add a html tag, the same as for a type so we can 
	 go directly here when the class name is used as a type name *)
      Format.fprintf fmt "@{<mark \"%s\">@}"
	(Naming.type_target 
	   { ty_name = c.cl_name ;
	     ty_info = None ; ty_parameters = [] ;
	     ty_kind = Type_abstract ; ty_manifest = None ; 
	     ty_loc = Odoc_info.dummy_loc });
      print_DEBUG "html#html_of_class : virtual or not" ;
      if c.cl_virtual then 
	(
	 self#keyword fmt "virtual";
	 Format.pp_print_string fmt " "
	);
      (
       match c.cl_type_parameters with
	 [] -> ()
       | l -> 
	   Format.pp_print_string fmt "[";
	   self#html_of_type_expr_list fmt father ", " l;
	   Format.pp_print_string fmt "]"
      );
      print_DEBUG "html#html_of_class : with link or not" ;
      (
       if with_link then
	 Format.fprintf fmt "@{<href \"%s\">%s@}" html_file (Name.simple c.cl_name)
       else
	 Format.pp_print_string fmt (Name.simple c.cl_name)
      );

      Format.pp_print_string fmt " : " ;
      self#html_of_class_type_expr fmt father c.cl_type;
      Format.fprintf fmt "@}";
      print_DEBUG "html#html_of_class : info" ;
      (if complete then self#html_of_info else self#html_of_info_first_sentence) fmt c.cl_info

    (** Print html code for a class type. *)
    method html_of_class_type fmt ?(complete=true) ?(with_link=true) ct =
      Odoc_info.reset_type_names ();
      let father = Name.father ct.clt_name in
      let (html_file, _) = Naming.html_files ct.clt_name in
      Format.fprintf fmt "@{<pre>";
      self#keyword fmt "class type";
      Format.pp_print_string fmt " ";
      (* we add a html tag, the same as for a type so we can 
	 go directly here when the class type name is used as a type name *)
      Format.fprintf fmt "@{<mark \"%s\">@}"
	(Naming.type_target 
	   { ty_name = ct.clt_name ;
	     ty_info = None ; ty_parameters = [] ;
	     ty_kind = Type_abstract ; ty_manifest = None ;
	     ty_loc = Odoc_info.dummy_loc });
      if ct.clt_virtual then 
	(
	 self#keyword fmt "virtual";
	 Format.pp_print_string fmt " "
	);
      (
       match ct.clt_type_parameters with
	[] -> ()
      |	l -> 
	  Format.pp_print_string fmt "[";
	  self#html_of_type_expr_list fmt father ", " l;
	  Format.pp_print_string fmt "]"
      );

      if with_link then
	Format.fprintf fmt "@{<href \"%s\">%s@}" html_file (Name.simple ct.clt_name)
      else
	Format.pp_print_string fmt (Name.simple ct.clt_name);

      Format.pp_print_string fmt " = ";
      self#html_of_class_type_expr fmt father ct.clt_type;
      Format.fprintf fmt "@}";
      (if complete then self#html_of_info else self#html_of_info_first_sentence) fmt ct.clt_info

    (** Get html code to represent a dag, represented as in Odoc_dag2html. *)
    method html_of_dag dag =
      let f n =
	let (name, cct_opt) = n.Odoc_dag2html.valu in
	(* if we have a c_opt = Some class then we take its information
	   because we are sure the name is complete. *)
	let (name2, html_file) =
	  match cct_opt with
	    None -> (name, fst (Naming.html_files name))
	  | Some (Cl c) -> (c.cl_name, fst (Naming.html_files c.cl_name))
	  | Some (Cltype (ct, _)) -> (ct.clt_name, fst (Naming.html_files ct.clt_name))
	in
	let new_v =
	  "<table border=1>\n<tr><td>"^
	  "<a href=\""^html_file^"\">"^name2^"</a>"^
	  "</td></tr>\n</table>\n"
	in	
	{ n with Odoc_dag2html.valu = new_v }
      in
      let a = Array.map f dag.Odoc_dag2html.dag in
      Odoc_dag2html.html_of_dag { Odoc_dag2html.dag = a }

    (** Print html code for a module comment.*)
    method html_of_module_comment fmt text =
      Format.pp_print_string fmt "<br>\n";
      self#html_of_text fmt text;
      Format.pp_print_string fmt "<br><br>\n"

    (** Print html code for a class comment.*)
    method html_of_class_comment fmt text =
      (* Add some style if there is no style for the first part of the text. *)
      let text2 =
	match text with
	| (Odoc_info.Raw s) :: q -> 
	    (Odoc_info.Title (2, None, [Odoc_info.Raw s])) :: q
	| _ -> text
      in
      self#html_of_text fmt text2

    (** Print html code for the given list of inherited classes.*)
    method generate_inheritance_info fmt inher_l =
      let f inh =
	match inh.ic_class with
	  None -> (* we can't make the link. *)
	    (Odoc_info.Code inh.ic_name) ::
	    (match inh.ic_text with
	      None -> []
	    | Some t -> (Odoc_info.Raw "    ") :: t)
	| Some cct ->
	    (* we can create the link. *)
	    let real_name = (* even if it should be the same *)
	      match cct with
		Cl c -> c.cl_name
	      |	Cltype (ct, _) -> ct.clt_name
	    in
	    let (class_file, _) = Naming.html_files real_name in
	    (Odoc_info.Link (class_file, [Odoc_info.Code real_name])) ::
	    (match inh.ic_text with
	      None -> []
	    | Some t -> (Odoc_info.Raw "    ") :: t)
      in
      let text = [
	Odoc_info.Bold [Odoc_info.Raw Odoc_messages.inherits] ;
	Odoc_info.List (List.map f inher_l)
      ] 
      in
      self#html_of_text fmt text

    (** Generate html code for the inherited classes of the given class. *)
    method generate_class_inheritance_info chanout cl =
      let rec iter_kind k = 
	match k with
	  Class_structure ([], _) ->
	    ()
	| Class_structure (l, _) ->
	    self#generate_inheritance_info chanout l
	| Class_constraint (k, ct) ->
	    iter_kind k
	| Class_apply _
	| Class_constr _ ->
	    ()
      in
      iter_kind cl.cl_kind

    (** Generate html code for the inherited classes of the given class type. *)
    method generate_class_type_inheritance_info fmt clt =
      match clt.clt_kind with
	Class_signature ([], _) ->
	  ()
      |	Class_signature (l, _) ->
	  self#generate_inheritance_info fmt l
      |	Class_type _ ->
	  ()

    (** A method to create index files. *)
    method generate_elements_index :
	'a.
	'a list ->
	  ('a -> Odoc_info.Name.t) ->
	    ('a -> Odoc_info.info option) -> 
	      ('a -> string) -> string -> string -> unit =
    fun elements name info target title simple_file ->
      try
	let (fmt,chanout) = self#formatter_of_file (Filename.concat !Odoc_args.target_dir simple_file) in
	Format.fprintf fmt "@{<html>";
	self#header fmt (self#inner_title title);
	Format.fprintf fmt "@{<body>@{<center>@{<h1>%s@}@}" title;
	
	let sorted_elements = List.sort 
	    (fun e1 -> fun e2 -> compare (Name.simple (name e1)) (Name.simple (name e2)))
	    elements
	in
	let groups = Odoc_info.create_index_lists sorted_elements (fun e -> Name.simple (name e)) in
	let f_ele e =
	  let simple_name = Name.simple (name e) in
	  let father_name = Name.father (name e) in
	  Format.fprintf fmt "@{<tr>@{<td>@{<href \"%s\">%s@} " (target e) simple_name;
	  if simple_name <> father_name then 
	    Format.fprintf fmt "[@{<href \"%s\">%s@}]" (fst (Naming.html_files father_name)) father_name;
	  Format.fprintf fmt "@}@{<td>";
	  self#html_of_info_first_sentence fmt (info e);
	  Format.fprintf fmt "@}@}"
	in
	let f_group l =
	  match l with
	    [] -> ()
	  | e :: _ ->
	      let s = 
		match (Char.uppercase (Name.simple (name e)).[0]) with
		  'A'..'Z' as c -> String.make 1 c
		| _ -> ""
	      in
	      Format.fprintf fmt "@{<tr>@{<td align=\"left\"><br>%s@}@}" s;
	      List.iter f_ele l
	in
	Format.fprintf fmt "@{<table>";
	List.iter f_group groups ;
	Format.fprintf fmt "@}<br>\n@}@}";
	Format.pp_print_flush fmt ();
	close_out chanout
      with
	Sys_error s ->
	  raise (Failure s)

    (** A method to generate a list of module/class files. *)
    method generate_elements :
	'a. ('a option -> 'a option -> 'a -> unit) -> 'a list -> unit =
      fun f_generate l ->
	let rec iter pre_opt = function
	    [] -> ()
	  | ele :: [] -> f_generate pre_opt None ele
	  | ele1 :: ele2 :: q -> 
	      f_generate pre_opt (Some ele2) ele1 ;
	      iter (Some ele1) (ele2 :: q)
	in
	iter None l

    (** Generate the code of the html page for the given class.*)
    method generate_for_class pre post cl =
      Odoc_info.reset_type_names ();
      let (html_file, _) = Naming.html_files cl.cl_name in
      let type_file = Naming.file_type_class_complete_target cl.cl_name in
      try
	let (fmt, chanout) = self#formatter_of_file (Filename.concat !Odoc_args.target_dir html_file) in
	let pre_name = opt (fun c -> c.cl_name) pre in
	let post_name = opt (fun c -> c.cl_name) post in
	Format.fprintf fmt "@{<html>";
	self#header fmt
	  ~nav: (Some (pre_name, post_name, cl.cl_name))
	  (self#inner_title cl.cl_name);
   
	Format.fprintf fmt "@{<body>";
	self#navbar fmt pre_name post_name cl.cl_name;
	Format.fprintf fmt "@{<center>@{<h1>%s %s@{<href \"%s\">%s@}@}@}<br>\n"
	  Odoc_messages.clas
	  (if cl.cl_virtual then "virtual " else "")
	  type_file
	  cl.cl_name;
	self#html_of_class fmt ~with_link: false cl;

	(* parameters *)
	self#html_of_described_parameter_list fmt (Name.father cl.cl_name) cl.cl_parameters;
        (* class inheritance *)
	self#generate_class_inheritance_info fmt cl;
	(* a horizontal line *)
	Format.pp_print_string fmt "<hr width=\"100%\">\n";
	(* the various elements *)
	List.iter 
	  (fun element ->
	    match element with
	      Class_attribute a -> self#html_of_attribute fmt a
	    | Class_method m -> self#html_of_method fmt m
	    | Class_comment t -> self#html_of_class_comment fmt t
	  )
	  (Class.class_elements ~trans:false cl);
	Format.fprintf fmt "@}@}";
	Format.pp_print_flush fmt ();
	close_out chanout;

        (* generate the file with the complete class type *)
	self#output_class_type 
	  cl.cl_name
	  (Filename.concat !Odoc_args.target_dir type_file)
	  cl.cl_type
      with
	Sys_error s ->
	  raise (Failure s)

    (** Generate the code of the html page for the given class type.*)
    method generate_for_class_type pre post clt =
      Odoc_info.reset_type_names ();
      let (html_file, _) = Naming.html_files clt.clt_name in
      let type_file = Naming.file_type_class_complete_target clt.clt_name in
      try
	let (fmt, chanout) = self#formatter_of_file (Filename.concat !Odoc_args.target_dir html_file) in
	let pre_name = opt (fun ct -> ct.clt_name) pre in
	let post_name = opt (fun ct -> ct.clt_name) post in
	Format.fprintf fmt "@{<html>";
	self#header fmt
	  ~nav: (Some (pre_name, post_name, clt.clt_name))
	  (self#inner_title clt.clt_name);
	Format.fprintf fmt "@{<body>";
	self#navbar fmt pre_name post_name clt.clt_name;
	Format.fprintf fmt "@{<center>@{<h1>%s %s@{<href \"%s\">%s@}@}@}<br>\n"
	  Odoc_messages.class_type
	  (if clt.clt_virtual then "virtual " else "")
	  type_file
	  clt.clt_name;
	self#html_of_class_type fmt ~with_link: false clt;

        (* class inheritance *)
	self#generate_class_type_inheritance_info fmt clt;
	(* a horizontal line *)
	Format.pp_print_string fmt "<hr width=\"100%\">\n";
	(* the various elements *)
	List.iter 
	  (fun element ->
	    match element with
	      Class_attribute a -> self#html_of_attribute fmt a
	    | Class_method m -> self#html_of_method fmt m
	    | Class_comment t -> self#html_of_class_comment fmt t
	  )
	  (Class.class_type_elements ~trans: false clt);
	Format.fprintf fmt "@}@}";
	Format.pp_print_flush fmt ();
	close_out chanout;

        (* generate the file with the complete class type *)
	self#output_class_type 
	  clt.clt_name
	  (Filename.concat !Odoc_args.target_dir type_file)
	  clt.clt_type
      with
	Sys_error s ->
	  raise (Failure s)

    (** Generate the html file for the given module type. 
       @raise Failure if an error occurs.*)
    method generate_for_module_type pre post mt =
      try
	let (html_file, _) = Naming.html_files mt.mt_name in
	let type_file = Naming.file_type_module_complete_target mt.mt_name in
	let (fmt, chanout) = self#formatter_of_file (Filename.concat !Odoc_args.target_dir html_file) in
	let pre_name = opt (fun mt -> mt.mt_name) pre in
	let post_name = opt (fun mt -> mt.mt_name) post in
	Format.fprintf fmt "@{<html>";
	self#header fmt
	  ~nav: (Some (pre_name, post_name, mt.mt_name))
	  (self#inner_title mt.mt_name);
	Format.fprintf fmt "@{<body>";
	self#navbar fmt pre_name post_name mt.mt_name;
	Format.fprintf fmt "@{<center>@{<h1>%s " Odoc_messages.module_type;
	(match mt.mt_type with
	  Some _ -> Format.fprintf fmt "@{<href \"%s\">%s@}" type_file mt.mt_name
	   | None-> Format.pp_print_string fmt mt.mt_name
	);
	Format.fprintf fmt "@}@}<br>\n";
	self#html_of_modtype fmt ~with_link: false mt;

	(* parameters for functors *)
	self#html_of_module_parameter_list fmt "" (Module.module_type_parameters mt);
	(* a horizontal line *)
	Format.pp_print_string fmt "<hr width=\"100%\">\n";
	(* module elements *)
	List.iter 
	  (fun ele ->
	    match ele with
	      Element_module m -> self#html_of_module fmt ~complete: false m
	    | Element_module_type mt -> self#html_of_modtype fmt ~complete: false mt
	    | Element_included_module im -> self#html_of_included_module fmt im
	    | Element_class c ->self#html_of_class fmt ~complete: false c
	    | Element_class_type ct -> self#html_of_class_type fmt ~complete: false ct
	    | Element_value v -> self#html_of_value fmt v
	    | Element_exception e -> self#html_of_exception fmt e
	    | Element_type t -> self#html_of_type fmt t
	    | Element_module_comment text -> self#html_of_module_comment fmt text
	  )
	  (Module.module_type_elements mt);

	Format.fprintf fmt "@}@}";
	Format.pp_print_flush fmt ();
	close_out chanout;

        (* generate html files for submodules *)
	self#generate_elements self#generate_for_module (Module.module_type_modules mt);
        (* generate html files for module types *)
	self#generate_elements self#generate_for_module_type (Module.module_type_module_types mt);
        (* generate html files for classes *)
	self#generate_elements self#generate_for_class (Module.module_type_classes mt);
        (* generate html files for class types *)
	self#generate_elements self#generate_for_class_type (Module.module_type_class_types mt);

        (* generate the file with the complete module type *)
	(
	 match mt.mt_type with
	   None -> ()
	 | Some mty -> self#output_module_type 
	       mt.mt_name
	       (Filename.concat !Odoc_args.target_dir type_file) 
	       mty
	)
      with
	Sys_error s ->
	  raise (Failure s)

    (** Generate the html file for the given module. 
       @raise Failure if an error occurs.*)
    method generate_for_module pre post modu =
      try
	Odoc_info.verbose ("Generate for module "^modu.m_name);
	let (html_file, _) = Naming.html_files modu.m_name in
	let type_file = Naming.file_type_module_complete_target modu.m_name in
	let (fmt, chanout) = self#formatter_of_file
	    (Filename.concat !Odoc_args.target_dir html_file) 
	in

	let pre_name = opt (fun m -> m.m_name) pre in
	let post_name = opt (fun m -> m.m_name) post in
	Format.fprintf fmt "@{<html>";
	self#header fmt
	  ~nav: (Some (pre_name, post_name, modu.m_name))
	  (self#inner_title modu.m_name);

	Format.fprintf fmt "@{<body>";
	self#navbar fmt pre_name post_name modu.m_name ;
	Format.fprintf fmt "@{<center>@{<h1>%s @{<href \"%s\">%s@}@}@}<br>\n"
	  (if Module.module_is_functor modu then Odoc_messages.functo else Odoc_messages.modul)
	  type_file modu.m_name;

	self#html_of_module fmt ~with_link: false modu;

	(* parameters for functors *)
	self#html_of_module_parameter_list fmt "" (Module.module_parameters modu);
	(* a horizontal line *)
	Format.fprintf fmt "<hr width=\"100%%\">\n";
	(* module elements *)
	List.iter 
	  (fun ele ->
	    print_DEBUG "html#generate_for_module : ele ->";
	    match ele with
	      Element_module m -> self#html_of_module fmt ~complete: false m
	    | Element_module_type mt -> self#html_of_modtype fmt ~complete: false mt
	    | Element_included_module im -> self#html_of_included_module fmt im
	    | Element_class c -> self#html_of_class fmt ~complete: false c
	    | Element_class_type ct -> self#html_of_class_type fmt ~complete: false ct
	    | Element_value v ->self#html_of_value fmt v
	    | Element_exception e ->self#html_of_exception fmt e
	    | Element_type t -> self#html_of_type fmt t
	    | Element_module_comment text -> self#html_of_module_comment fmt text
	  )
	  (Module.module_elements modu);

	Format.fprintf fmt "@}@}";
	Format.pp_print_flush fmt ();
	close_out chanout;

        (* generate html files for submodules *)
	self#generate_elements  self#generate_for_module (Module.module_modules modu);
        (* generate html files for module types *)
	self#generate_elements  self#generate_for_module_type (Module.module_module_types modu);
        (* generate html files for classes *)
	self#generate_elements  self#generate_for_class (Module.module_classes modu);
        (* generate html files for class types *)
	self#generate_elements  self#generate_for_class_type (Module.module_class_types modu);
        
        (* generate the file with the complete module type *)
	self#output_module_type 
	  modu.m_name
	  (Filename.concat !Odoc_args.target_dir type_file)
	  modu.m_type
      with
	Sys_error s ->
	  raise (Failure s)

    (** Generate the [index.html] file corresponding to the given module list.
       @raise Failure if an error occurs.*)
    method generate_index module_list =
      try
	let title = match !Odoc_args.title with None -> "" | Some t -> self#escape t in
	let (fmt, chanout) = self#formatter_of_file (Filename.concat !Odoc_args.target_dir index) in
	let index_if_not_empty l url m =
	  match l with
	    [] -> ()
	  | _ -> Format.fprintf fmt "@{<href \"%s\">%s@}<br>\n" url m 
	in
	Format.fprintf fmt "@{<html>";
	self#header fmt self#title;
	Format.fprintf fmt "@{<body>@{<center>@{<h1>%s@}@}" title;
	index_if_not_empty list_types index_types Odoc_messages.index_of_types ;
	index_if_not_empty list_exceptions index_exceptions Odoc_messages.index_of_exceptions ;
	index_if_not_empty list_values index_values Odoc_messages.index_of_values ;
	index_if_not_empty list_attributes index_attributes Odoc_messages.index_of_attributes ;
	index_if_not_empty list_methods index_methods Odoc_messages.index_of_methods ;
	index_if_not_empty list_classes index_classes Odoc_messages.index_of_classes ;
	index_if_not_empty list_class_types index_class_types Odoc_messages.index_of_class_types ;
	index_if_not_empty list_modules index_modules Odoc_messages.index_of_modules ;
	index_if_not_empty list_module_types index_module_types Odoc_messages.index_of_module_types ;
	Format.fprintf fmt "<br>\n@{<table border=\"0\">";
	List.iter
	  (fun m ->
	    let (html, _) = Naming.html_files m.m_name in
	    Format.fprintf fmt "@{<tr>@{<td>@{<href \"%s\">%s@}@}@{<td>" html m.m_name;
	    self#html_of_info_first_sentence fmt m.m_info;
	    Format.fprintf fmt "@}@}"
	  )
	  module_list;
	Format.fprintf fmt "@}@}@}";
	Format.pp_print_flush fmt ();
	close_out chanout
      with
	Sys_error s ->
	  raise (Failure s)

    (** Generate the values index in the file [index_values.html]. *)
    method generate_values_index module_list =
      self#generate_elements_index 
	list_values
	(fun v -> v.val_name) 
	(fun v -> v.val_info)
	Naming.complete_value_target
	Odoc_messages.index_of_values
	index_values

    (** Generate the exceptions index in the file [index_exceptions.html]. *)
    method generate_exceptions_index module_list =
      self#generate_elements_index 
	list_exceptions
	(fun e -> e.ex_name) 
	(fun e -> e.ex_info)
	Naming.complete_exception_target
	Odoc_messages.index_of_exceptions
	index_exceptions

    (** Generate the types index in the file [index_types.html]. *)
    method generate_types_index module_list =
      self#generate_elements_index 
	list_types
	(fun t -> t.ty_name) 
	(fun t -> t.ty_info)
	Naming.complete_type_target
	Odoc_messages.index_of_types
	index_types

    (** Generate the attributes index in the file [index_attributes.html]. *)
    method generate_attributes_index module_list =
      self#generate_elements_index 
	list_attributes
	(fun a -> a.att_value.val_name) 
	(fun a -> a.att_value.val_info)
	Naming.complete_attribute_target
	Odoc_messages.index_of_attributes
	index_attributes

    (** Generate the methods index in the file [index_methods.html]. *)
    method generate_methods_index module_list =
      self#generate_elements_index 
	list_methods
	(fun m -> m.met_value.val_name) 
	(fun m -> m.met_value.val_info)
	Naming.complete_method_target
	Odoc_messages.index_of_methods
	index_methods

    (** Generate the classes index in the file [index_classes.html]. *)
    method generate_classes_index module_list =
      self#generate_elements_index
	list_classes
	(fun c -> c.cl_name) 
	(fun c -> c.cl_info)
	(fun c -> fst (Naming.html_files c.cl_name))
	Odoc_messages.index_of_classes
	index_classes

    (** Generate the class types index in the file [index_class_types.html]. *)
    method generate_class_types_index module_list =
      self#generate_elements_index
	list_class_types
	(fun ct -> ct.clt_name) 
	(fun ct -> ct.clt_info)
	(fun ct -> fst (Naming.html_files ct.clt_name))
	Odoc_messages.index_of_class_types
	index_class_types

    (** Generate the modules index in the file [index_modules.html]. *)
    method generate_modules_index module_list =
      self#generate_elements_index
	list_modules
	(fun m -> m.m_name) 
	(fun m -> m.m_info)
	(fun m -> fst (Naming.html_files m.m_name))
	Odoc_messages.index_of_modules
	index_modules

    (** Generate the module types index in the file [index_module_types.html]. *)
    method generate_module_types_index module_list =
      let module_types = Odoc_info.Search.module_types module_list in
      self#generate_elements_index
	list_module_types
	(fun mt -> mt.mt_name) 
	(fun mt -> mt.mt_info)
	(fun mt -> fst (Naming.html_files mt.mt_name))
	Odoc_messages.index_of_module_types
	index_module_types

    (** Generate all the html files from a module list. The main
       file is [index.html]. *)
    method generate module_list =
      (* init the style *)
      self#init_style ;
      (* init the lists of elements *)
      list_values <- Odoc_info.Search.values module_list ;
      list_exceptions <- Odoc_info.Search.exceptions module_list ;
      list_types <- Odoc_info.Search.types module_list ;
      list_attributes <- Odoc_info.Search.attributes module_list ;
      list_methods <- Odoc_info.Search.methods module_list ;
      list_classes <- Odoc_info.Search.classes module_list ;
      list_class_types <- Odoc_info.Search.class_types module_list ;
      list_modules <- Odoc_info.Search.modules module_list ;
      list_module_types <- Odoc_info.Search.module_types module_list ;
      
      (* prepare the page header *)
      self#prepare_header module_list ;
      (* Get the names of all known types. *)
      let types = Odoc_info.Search.types module_list in
      let type_names = List.map (fun t -> t.ty_name) types in
      known_types_names <- type_names ;
      (* Get the names of all class and class types. *)
      let classes = Odoc_info.Search.classes module_list in
      let class_types = Odoc_info.Search.class_types module_list in
      let class_names = List.map (fun c -> c.cl_name) classes in
      let class_type_names = List.map (fun ct -> ct.clt_name) class_types in
      known_classes_names <- class_names @ class_type_names ;
      (* Get the names of all known modules and module types. *)
      let module_types = Odoc_info.Search.module_types module_list in
      let modules = Odoc_info.Search.modules module_list in
      let module_type_names = List.map (fun mt -> mt.mt_name) module_types in
      let module_names = List.map (fun m -> m.m_name) modules in
      known_modules_names <- module_type_names @ module_names ;
      (* generate html for each module *)
      if not !Odoc_args.index_only then 
	self#generate_elements self#generate_for_module module_list ;

      try
	self#generate_index module_list;
	self#generate_values_index module_list ;
	self#generate_exceptions_index module_list ;
	self#generate_types_index module_list ;
	self#generate_attributes_index module_list ;
	self#generate_methods_index module_list ;
	self#generate_classes_index module_list ;
	self#generate_class_types_index module_list ;
	self#generate_modules_index module_list ;
	self#generate_module_types_index module_list ;
      with
	Failure s ->
	  prerr_endline s ;
	  incr Odoc_info.errors

    method init_formatter fmt =
      let htag s =
	try
	  let i = String.index s ' ' in
	  String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)
	with Not_found -> s, ""
      in
	    
      let mark_open_tag s =
	let tag, vals = htag s in
	match tag with 
	| "href" -> "<a href=" ^ vals ^ ">"
	| "mark" -> "<a name=" ^ vals ^ ">"
	| "ul" | "li" | "ol" | "html" | "body" 
	| "table" | "tr" | "td" | "head" -> 
	    "<" ^ tag ^(if vals = "" then "" else " "^vals)^ ">\n"
	| t -> "<" ^ t ^ " " ^ vals ^ ">"
      in
      let mark_close_tag s =
	let tag, vals = htag s in
	match tag with
	| "href" -> "</a>"
	| "mark" -> "</a>"
	| "ul" | "li" | "ol" | "html" | "body" | "pre"
	| "table" | "tr" | "td" | "head" -> "</" ^ tag ^ ">\n"
	| t -> "</" ^ t ^ ">"
      in
      Format.pp_set_formatter_tag_functions fmt
	{(Format.pp_get_formatter_tag_functions fmt ()) with 
	  Format.mark_close_tag = mark_close_tag;
	  Format.mark_open_tag = mark_open_tag}

    method formatter_of_file file = 
      let chanout = open_out file in
      let fmt = Format.formatter_of_out_channel chanout in
      self#init_formatter fmt;
      (fmt, chanout)
			    
    initializer
      Odoc_ocamlhtml.html_of_comment := 
	(fun fmt -> fun s -> self#html_of_text fmt (Odoc_text.Texter.text_of_string s));

  end


			     
