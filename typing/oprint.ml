(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*                  Projet Cristal, INRIA Rocquencourt                 *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format
open Outcometree

exception Ellipsis

let cautious f ppf arg = try f ppf arg with Ellipsis -> fprintf ppf "..."

let rec print_ident ppf =
  function
    Oide_ident s -> fprintf ppf "%s" s
  | Oide_dot (id, s) -> fprintf ppf "%a.%s" print_ident id s
  | Oide_apply (id1, id2) ->
      fprintf ppf "%a(%a)" print_ident id1 print_ident id2

let value_ident ppf name =
  if List.mem name
      ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"]
  then fprintf ppf "( %s )" name
  else match name.[0] with
  | 'a' .. 'z' | '\223' .. '\246' | '\248' .. '\255' | '_' ->
      fprintf ppf "%s" name
  | _ -> fprintf ppf "( %s )" name

(* Values *)

let print_out_value ppf tree =
  let rec print_tree ppf =
    function
      Oval_tuple tree_list ->
        fprintf ppf "@[%a@]" (print_tree_list print_tree_1 ",") tree_list
    | tree -> print_tree_1 ppf tree
  and print_tree_1 ppf =
    function
      Oval_constr (name, [param]) ->
        fprintf ppf "@[<1>%a@ %a@]" print_ident name print_simple_tree
          param
    | Oval_constr (name, (_ :: _ as params)) ->
        fprintf ppf "@[<1>%a@ (%a)@]" print_ident name
          (print_tree_list print_tree_1 ",") params
    | Oval_variant (name, Some param) ->
        fprintf ppf "@[<2>`%s@ %a@]" name print_simple_tree param
    | tree -> print_simple_tree ppf tree
  and print_simple_tree ppf =
    function
      Oval_int i -> fprintf ppf "%i" i
    | Oval_float f -> fprintf ppf "%s" (string_of_float f)
    | Oval_char c -> fprintf ppf "'%s'" (Char.escaped c)
    | Oval_string s ->
        (* String.escaped may raise [Invalid_argument "String.create"]
           if the escaped string is longer than [Sys.max_string_length] *)
        begin try
          fprintf ppf "\"%s\"" (String.escaped s)
        with Invalid_argument "String.create" ->
          fprintf ppf "<huge string>"
        end
    | Oval_list tl ->
        fprintf ppf "@[<1>[%a]@]" (print_tree_list print_tree_1 ";") tl
    | Oval_array tl ->
        fprintf ppf "@[<2>[|%a|]@]" (print_tree_list print_tree_1 ";") tl
    | Oval_constr (name, []) -> print_ident ppf name
    | Oval_variant (name, None) -> fprintf ppf "`%s" name
    | Oval_stuff s -> fprintf ppf "%s" s
    | Oval_record fel ->
        fprintf ppf "@[<1>{%a}@]" (cautious (print_fields true)) fel
    | Oval_ellipsis -> raise Ellipsis
    | Oval_printer f -> f ppf
    | tree -> fprintf ppf "@[<1>(%a)@]" (cautious print_tree) tree
  and print_fields first ppf =
    function
      [] -> ()
    | (name, tree) :: fields ->
        if not first then fprintf ppf ";@ ";
        fprintf ppf "@[<1>%a@ =@ %a@]" print_ident name
          (cautious print_tree) tree;
        print_fields false ppf fields
  and print_tree_list print_item sep ppf tree_list =
    let rec print_list first ppf =
      function
        [] -> ()
      | tree :: tree_list ->
          if not first then fprintf ppf "%s@ " sep;
          print_item ppf tree;
          print_list false ppf tree_list
    in
    cautious (print_list true) ppf tree_list
  in
  cautious print_tree ppf tree

(* Types *)

let rec print_list_init pr sep ppf = function
  | [] -> ()
  | a :: l -> sep ppf; pr ppf a; print_list_init pr sep ppf l

let rec print_list pr sep ppf = function
  | [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l

let pr_present =
  print_list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")

let rec print_out_type ppf =
  function
  | Otyp_alias (ty, s) ->
      fprintf ppf "@[%a as '%s@]" print_out_type ty s
  | ty ->
      print_out_type_1 ppf ty

and print_out_type_1 ppf =
  function
  | Otyp_arrow (lab, ty1, ty2) ->
      fprintf ppf "@[%s%a ->@ %a@]"
        (if lab <> "" then lab ^ ":" else "")
        print_out_type_2 ty1 print_out_type_1 ty2
  | ty ->
      print_out_type_2 ppf ty

and print_out_type_2 ppf =
  function
  | Otyp_tuple tyl ->
      fprintf ppf "@[<0>%a@]" (print_typlist print_simple_out_type " *") tyl
  | ty ->
      print_simple_out_type ppf ty

and print_simple_out_type ppf =
  function
  | Otyp_class (ng, id, tyl) ->
      fprintf ppf "@[%a%s#%a@]" print_typargs tyl
        (if ng then "_" else "") print_ident id
  | Otyp_constr (id, tyl) ->
      fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
  | Otyp_object (fields, rest) ->
      fprintf ppf "@[<2>< %a >@]" (print_fields rest) fields
  | Otyp_stuff s ->
      fprintf ppf "%s" s
  | Otyp_var (ng, s) ->
      fprintf ppf "'%s%s" (if ng then "_" else "") s
  | Otyp_variant (non_gen, row_fields, closed, tags) ->
      let print_present ppf =
        function
        | None | Some [] -> ()
        | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l
      in
      let print_fields ppf = function
          Ovar_fields fields ->
            print_list print_row_field (fun ppf -> fprintf ppf "@;<1 -2>| ")
              ppf fields
        | Ovar_name (id, tyl) ->
            fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
      in
      fprintf ppf "%s[%s@[<hv>@[<hv>%a@]%a]@]"
        (if non_gen then "_" else "")
        (if closed then if tags = None then " " else "< "
         else if tags = None then "> " else "? ")
        print_fields row_fields
        print_present tags
  | Otyp_alias (_, _) | Otyp_arrow (_, _, _) | Otyp_tuple _ as ty ->
      fprintf ppf "@[<1>(%a)@]" print_out_type ty
  | Otyp_abstract | Otyp_sum _ | Otyp_record _ | Otyp_manifest (_, _) -> ()

and print_fields rest ppf =
  function
  | [] ->
      begin match rest with
      | Some non_gen -> fprintf ppf "%s.." (if non_gen then "_" else "")
      | None -> ()
      end
  | [(s, t)] ->
      fprintf ppf "%s : %a" s print_out_type t;
      begin match rest with
      | Some _ -> fprintf ppf ";@ "
      | None -> ()
      end;
      print_fields rest ppf []
  | (s, t) :: l ->
      fprintf ppf "%s : %a;@ %a" s print_out_type t (print_fields rest) l

and print_row_field ppf (l, opt_amp, tyl) =
  let pr_of ppf =
    if opt_amp then fprintf ppf " of@ &@ "
    else if tyl <> [] then fprintf ppf " of@ "
    else fprintf ppf ""
  in
  fprintf ppf "@[<hv 2>`%s%t%a@]" l pr_of
    (print_typlist print_out_type " &") tyl

and print_typlist print_elem sep ppf = function
  | [] -> ()
  | [ty] -> print_elem ppf ty
  | ty :: tyl ->
      fprintf ppf "%a%s@ %a"
        print_elem ty sep (print_typlist print_elem sep) tyl

and print_typargs ppf =
  function
  | [] -> ()
  | [ty1] -> fprintf ppf "%a@ " print_simple_out_type ty1
  | tyl -> fprintf ppf "@[<1>(%a)@]@ " (print_typlist print_out_type ",") tyl

(* Class types *)

let print_out_class_params ppf =
  function
  | [] -> ()
  | tyl ->
      fprintf ppf "@[<1>[%a]@]@ "
        (print_list (fun ppf x -> fprintf ppf "'%s" x)
           (fun ppf -> fprintf ppf ", "))
        tyl

let rec print_out_class_type ppf =
  function
  | Octy_constr (id, tyl) ->
      let pr_tyl ppf = function
        | [] -> ()
        | tyl ->
            fprintf ppf "@[<1>[%a]@]@ "
              (print_typlist print_out_type ",") tyl
      in
      fprintf ppf "@[%a%a@]" pr_tyl tyl print_ident id
  | Octy_fun (lab, ty, cty) ->
      fprintf ppf "@[%s%a ->@ %a@]"
        (if lab <> "" then lab ^ ":" else "")
        print_out_type_2 ty print_out_class_type cty
  | Octy_signature (self_ty, csil) ->
      let pr_param ppf =
        function
        | Some ty -> fprintf ppf "@ @[(%a)@]" print_out_type ty
        | None -> ()
      in
      fprintf ppf "@[<hv 2>@[<2>object%a@]@ %a@;<1 -2>end@]"
        pr_param self_ty
        (print_list print_out_class_sig_item (fun ppf -> fprintf ppf "@ "))
        csil
and print_out_class_sig_item ppf =
  function
  | Ocsg_constraint (ty1, ty2) ->
     fprintf ppf "@[<2>constraint %a =@ %a@]" print_out_type ty1
       print_out_type ty2
  | Ocsg_method (name, priv, virt, ty) ->
     fprintf ppf "@[<2>method %s%s%s :@ %a@]"
       (if priv then "private " else "") (if virt then "virtual " else "")
       name print_out_type ty
  | Ocsg_value (name, mut, ty) ->
     fprintf ppf "@[<2>val %s%s :@ %a@]" (if mut then "mutable " else "") name
       print_out_type ty

(* Signature *)

let rec print_out_module_type ppf =
  function
  | Omty_abstract -> ()
  | Omty_functor (name, mty_arg, mty_res) ->
      fprintf ppf "@[<2>functor@ (%s : %a) ->@ %a@]" name
        print_out_module_type mty_arg print_out_module_type mty_res
  | Omty_ident id ->
      fprintf ppf "%a" print_ident id
  | Omty_signature sg ->
      fprintf ppf "@[<hv 2>sig@ %a@;<1 -2>end@]" print_out_signature sg
and print_out_signature ppf =
  function
  | [] -> ()
  | item :: [] -> print_out_sig_item ppf item
  | item :: items ->
      fprintf ppf "%a@ %a" print_out_sig_item item print_out_signature items
and print_out_sig_item ppf =
  function
  | Osig_class (vir_flag, name, params, clt) ->
      fprintf ppf "@[<2>class%s@ %a%s@ :@ %a@]"
        (if vir_flag then " virtual" else "")
        print_out_class_params params name print_out_class_type clt
  | Osig_class_type (vir_flag, name, params, clt) ->
      fprintf ppf "@[<2>class type%s@ %a%s@ =@ %a@]"
        (if vir_flag then " virtual" else "")
        print_out_class_params params name print_out_class_type clt
  | Osig_exception (id, tyl) ->
      fprintf ppf "@[<2>exception %a@]" print_out_constr (id, tyl)
  | Osig_modtype (name, Omty_abstract) ->
      fprintf ppf "@[<2>module type %s@]" name
  | Osig_modtype (name, mty) ->
      fprintf ppf "@[<2>module type %s =@ %a@]" name print_out_module_type mty
  | Osig_module (name, mty) ->
      fprintf ppf "@[<2>module %s :@ %a@]" name print_out_module_type mty
  | Osig_type tdl ->
      print_out_type_decl_list ppf tdl
  | Osig_value (name, ty, prims) ->
      let kwd = if prims = [] then "val" else "external" in
      let pr_prims ppf =
        function
        | [] -> ()
        | s :: sl ->
            fprintf ppf "@ = \"%s\"" s;
            List.iter (fun s -> fprintf ppf "@ \"%s\"" s) sl
      in
      fprintf ppf "@[<2>%s %a :@ %a%a@]"
        kwd value_ident name print_out_type ty pr_prims prims
and print_out_type_decl_list ppf =
  function
  | [] -> ()
  | [x] -> print_out_type_decl "type" ppf x
  | x :: l ->
      print_out_type_decl "type" ppf x;
      List.iter (fun x -> fprintf ppf "@ %a" (print_out_type_decl "and") x) l
and print_out_type_decl kwd ppf (name, args, ty, constraints) =
  let print_constraints ppf params =
    List.iter
      (fun (ty1, ty2) ->
         fprintf ppf "@ @[<2>constraint %a =@ %a@]" print_out_type ty1
           print_out_type ty2)
      params
  in
  let type_parameter ppf (ty,(co,cn)) =
    fprintf ppf "%s'%s"
      (if not cn then "+" else if not co then "-" else "") ty
  in
  let type_defined ppf =
    match args with
    | [] -> fprintf ppf "%s" name
    | [arg] -> fprintf ppf "@[%a@ %s@]" type_parameter arg name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list type_parameter (fun ppf -> fprintf ppf ",@ "))
          args name
  in
  let print_manifest ppf =
    function
    | Otyp_manifest (ty, _) -> fprintf ppf " =@ %a" print_out_type ty
    | _ -> ()
  in
  let print_name_args ppf =
    fprintf ppf "%s %t%a" kwd type_defined print_manifest ty
  in
  let ty =
    match ty with
    | Otyp_manifest (_, ty) -> ty
    | _ -> ty
  in
  match ty with
  | Otyp_abstract ->
      fprintf ppf "@[<2>@[<hv 2>%t@]%a@]"
        print_name_args print_constraints constraints
  | Otyp_record lbls ->
      fprintf ppf "@[<2>@[<hv 2>%t = {%a@;<1 -2>}@]@ %a@]"
        print_name_args
        (print_list_init print_out_label (fun ppf -> fprintf ppf "@ ")) lbls
        print_constraints constraints
  | Otyp_sum constrs ->
      fprintf ppf "@[<2>@[<hv 2>%t =@;<1 2>%a@]%a@]"
        print_name_args
        (print_list print_out_constr (fun ppf -> fprintf ppf "@ | ")) constrs
        print_constraints constraints
  | ty ->
      fprintf ppf "@[<2>@[<hv 2>%t =@ %a@]%a@]"
        print_name_args print_out_type ty
        print_constraints constraints
and print_out_constr ppf (name, tyl) =
  match tyl with
  | [] -> fprintf ppf "%s" name
  | _ ->
      fprintf ppf "@[<2>%s of@ %a@]" name
        (print_typlist print_simple_out_type " *") tyl
and print_out_label ppf (name, mut, arg) =
  fprintf ppf "@[<2>%s%s :@ %a@];"
    (if mut then "mutable " else "") name print_out_type arg

(* Phrases *)

let print_out_exception ppf exn outv =
  match exn with
  | Sys.Break ->
      fprintf ppf "Interrupted.@."
  | Out_of_memory ->
      fprintf ppf "Out of memory during evaluation.@."
  | Stack_overflow ->
      fprintf ppf "Stack overflow during evaluation (looping recursion?).@."
  | _ ->
      fprintf ppf "@[Exception:@ %a.@]@." print_out_value outv

let rec print_items ppf =
  function
  | [] -> ()
  | (tree, valopt) :: items ->
      begin match valopt with
      | Some v ->
          fprintf ppf "@[<2>%a =@ %a@]" print_out_sig_item tree
            print_out_value v
      | None ->
          fprintf ppf "@[%a@]" print_out_sig_item tree
      end;
      if items <> [] then
        fprintf ppf "@ %a" print_items items

let print_out_phrase ppf =
  function
  | Ophr_eval (outv, ty) ->
      fprintf ppf "@[- : %a@ =@ %a@]@."
        print_out_type ty print_out_value outv
  | Ophr_signature [] -> ()
  | Ophr_signature items -> fprintf ppf "@[<v>%a@]@." print_items items
  | Ophr_exception (exn, outv) -> print_out_exception ppf exn outv

(* Hooks *)

let out_value = ref print_out_value
let out_type = ref print_out_type
let out_class_type = ref print_out_class_type
let out_module_type = ref print_out_module_type
let out_sig_item = ref print_out_sig_item
let out_signature = ref print_out_signature
let out_phrase = ref print_out_phrase
