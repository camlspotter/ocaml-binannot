(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Type-checking of the module language *)

open Misc
open Longident
open Path
open Parsetree
open Types
open Typedtree
open Format

type error =
    Unbound_module of Longident.t
  | Unbound_modtype of Longident.t
  | Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.error list
  | Repeated_name of string * string
  | Non_generalizable of type_expr
  | Non_generalizable_class of Ident.t * class_declaration
  | Non_generalizable_module of module_type

exception Error of Location.t * error

(* Extract a signature from a module type *)

let extract_sig env loc mty =
  match Mtype.scrape env mty with
    Tmty_signature sg -> sg
  | _ -> raise(Error(loc, Signature_expected))

let extract_sig_open env loc mty =
  match Mtype.scrape env mty with
    Tmty_signature sg -> sg
  | _ -> raise(Error(loc, Structure_expected mty))

(* Lookup the type of a module path *)

let type_module_path env loc lid =
  try
    Env.lookup_module lid env
  with Not_found ->
    raise(Error(loc, Unbound_module lid))

(* Merge one "with" constraint in a signature *)

let merge_constraint initial_env loc sg lid constr =
  let rec merge env sg namelist =
    match (sg, namelist, constr) with
      ([], _, _) ->
        raise(Error(loc, With_no_component lid))
    | (Tsig_type(id, decl) :: rem, [s], Pwith_type sdecl)
      when Ident.name id = s ->
        let newdecl = Typedecl.transl_with_constraint initial_env sdecl in
        Includemod.type_declarations env id newdecl decl;
        Tsig_type(id, newdecl) :: rem
    | (Tsig_module(id, mty) :: rem, [s], Pwith_module lid)
      when Ident.name id = s ->
        let (path, mty') = type_module_path initial_env loc lid in
        let newmty = Mtype.strengthen env mty' path in
        ignore(Includemod.modtypes env newmty mty);
        Tsig_module(id, newmty) :: rem
    | (Tsig_module(id, mty) :: rem, s :: namelist, _) when Ident.name id = s ->
        let newsg = merge env (extract_sig env loc mty) namelist in
        Tsig_module(id, Tmty_signature newsg) :: rem
    | (item :: rem, _, _) ->
        item :: merge (Env.add_item item env) rem namelist in
  try
    merge initial_env sg (Longident.flatten lid)
  with Includemod.Error explanation ->
    raise(Error(loc, With_mismatch(lid, explanation)))

(* Auxiliaries for checking uniqueness of names in signatures and structures *)

module StringSet = Set.Make(struct type t = string let compare = compare end)

let check cl loc set_ref name =
  if StringSet.mem name !set_ref
  then raise(Error(loc, Repeated_name(cl, name)))
  else set_ref := StringSet.add name !set_ref

let check_sig_item type_names module_names modtype_names loc = function
    Tsig_type(id, _) ->
      check "type" loc type_names (Ident.name id)
  | Tsig_module(id, _) ->
      check "module" loc module_names (Ident.name id)
  | Tsig_modtype(id, _) ->
      check "module type" loc modtype_names (Ident.name id)
  | _ -> ()

(* Check and translate a module type expression *)

let rec transl_modtype env smty =
  match smty.pmty_desc with
    Pmty_ident lid ->
      begin try
        let (path, info) = Env.lookup_modtype lid env in 
        Tmty_ident path
      with Not_found ->
        raise(Error(smty.pmty_loc, Unbound_modtype lid))
      end
  | Pmty_signature ssg ->
      Tmty_signature(transl_signature env ssg)
  | Pmty_functor(param, sarg, sres) ->
      let arg = transl_modtype env sarg in
      let (id, newenv) = Env.enter_module param arg env in
      let res = transl_modtype newenv sres in
      Tmty_functor(id, arg, res)
  | Pmty_with(sbody, constraints) ->
      let body = transl_modtype env sbody in
      let init_sg = extract_sig env sbody.pmty_loc body in
      let final_sg =
        List.fold_left
          (fun sg (lid, sdecl) ->
            merge_constraint env smty.pmty_loc sg lid sdecl)
          init_sg constraints in
      Tmty_signature final_sg
      
and transl_signature env sg =
  let type_names = ref StringSet.empty
  and module_names = ref StringSet.empty
  and modtype_names = ref StringSet.empty in
  let rec transl_sig env sg =
    Ctype.init_def(Ident.current_time());
    match sg with
      [] -> []
    | item :: srem ->
        match item.psig_desc with
        | Psig_value(name, sdesc) ->
            let desc = Typedecl.transl_value_decl env sdesc in
            let (id, newenv) = Env.enter_value name desc env in
            let rem = transl_sig newenv srem in
            Tsig_value(id, desc) :: rem
        | Psig_type sdecls ->
            List.iter
              (fun (name, decl) -> check "type" item.psig_loc type_names name)
              sdecls;
            let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
            let rem = transl_sig newenv srem in
            map_end (fun (id, info) -> Tsig_type(id, info)) decls rem
        | Psig_exception(name, sarg) ->
            let arg = Typedecl.transl_exception env sarg in
            let (id, newenv) = Env.enter_exception name arg env in
            let rem = transl_sig newenv srem in
            Tsig_exception(id, arg) :: rem
        | Psig_module(name, smty) ->
            check "module" item.psig_loc module_names name;
            let mty = transl_modtype env smty in
            let (id, newenv) = Env.enter_module name mty env in
            let rem = transl_sig newenv srem in
            Tsig_module(id, mty) :: rem
        | Psig_modtype(name, sinfo) ->
            check "module type" item.psig_loc modtype_names name;
            let info = transl_modtype_info env sinfo in
            let (id, newenv) = Env.enter_modtype name info env in
            let rem = transl_sig newenv srem in
            Tsig_modtype(id, info) :: rem
        | Psig_open lid ->
            let (path, mty) = type_module_path env item.psig_loc lid in
            let sg = extract_sig_open env item.psig_loc mty in
            let newenv = Env.open_signature path sg env in
            transl_sig newenv srem
        | Psig_include smty ->
            let mty = transl_modtype env smty in
            let sg = Subst.signature Subst.identity
                       (extract_sig env smty.pmty_loc mty) in
            List.iter
              (check_sig_item type_names module_names modtype_names
                              item.psig_loc)
              sg;
            let newenv = Env.add_signature sg env in
            let rem = transl_sig newenv srem in
            sg @ rem
        | Psig_class cl ->
            List.iter
              (fun {pci_name = name} ->
                 check "type" item.psig_loc type_names name)
              cl;
            let (classes, newenv) = Typeclass.class_descriptions env cl in
            let rem = transl_sig newenv srem in
            List.flatten
              (map_end
                 (fun (i, d, i', d', i'', d'', i''', d''', _, _, _) ->
                    [Tsig_class(i, d);    Tsig_cltype(i', d');
                     Tsig_type(i'', d''); Tsig_type(i''', d''')])
                 classes [rem])
        | Psig_class_type cl ->
            List.iter
              (fun {pci_name = name} ->
                 check "type" item.psig_loc type_names name)
              cl;
            let (classes, newenv) = Typeclass.class_type_declarations env cl in
            let rem = transl_sig newenv srem in
            List.flatten
              (map_end
                 (fun (i, d, i', d', i'', d'') ->
                    [Tsig_cltype(i, d);
                     Tsig_type(i', d'); Tsig_type(i'', d'')])
                 classes [rem])
    in transl_sig env sg

and transl_modtype_info env sinfo =
  match sinfo with
    Pmodtype_abstract ->
      Tmodtype_abstract
  | Pmodtype_manifest smty ->
      Tmodtype_manifest(transl_modtype env smty)

(* Try to convert a module expression to a module path. *)

exception Not_a_path

let rec path_of_module mexp =
  match mexp.mod_desc with
    Tmod_ident p -> p
  | Tmod_apply(funct, arg, coercion) ->
      Papply(path_of_module funct, path_of_module arg)
  | _ -> raise Not_a_path

(* Check that all core type schemes in a structure are closed *)

let rec closed_modtype = function
    Tmty_ident p -> true
  | Tmty_signature sg -> List.for_all closed_signature_item sg
  | Tmty_functor(id, param, body) -> closed_modtype body

and closed_signature_item = function
    Tsig_value(id, desc) -> Ctype.closed_schema desc.val_type
  | Tsig_module(id, mty) -> closed_modtype mty
  | _ -> true

let check_nongen_scheme env = function
    Tstr_value(rec_flag, pat_exp_list) ->
      List.iter
        (fun (pat, exp) ->
          if not (Ctype.closed_schema exp.exp_type) then
            raise(Error(exp.exp_loc, Non_generalizable exp.exp_type)))
        pat_exp_list
  | Tstr_module(id, md) ->
      if not (closed_modtype md.mod_type) then
        raise(Error(md.mod_loc, Non_generalizable_module md.mod_type))
  | _ -> ()

let check_nongen_schemes env str =
  List.iter (check_nongen_scheme env) str

(* Extract the list of "value" identifiers bound by a signature.
   "Value" identifiers are identifiers for signature components that
   correspond to a run-time value: values, exceptions, modules, classes.
   Note: manifest primitives do not correspond to a run-time value! *)

let rec bound_value_identifiers = function
    [] -> []
  | Tsig_value(id, {val_kind = Val_reg}) :: rem ->
      id :: bound_value_identifiers rem
  | Tsig_exception(id, decl) :: rem -> id :: bound_value_identifiers rem
  | Tsig_module(id, mty) :: rem -> id :: bound_value_identifiers rem
  | Tsig_class(id, decl) :: rem -> id :: bound_value_identifiers rem
  | _ :: rem -> bound_value_identifiers rem

(* Type a module value expression *)

let rec type_module env smod =
  match smod.pmod_desc with
    Pmod_ident lid ->
      let (path, mty) = type_module_path env smod.pmod_loc lid in
      { mod_desc = Tmod_ident path;
        mod_type = Mtype.strengthen env mty path;
        mod_env = env;
        mod_loc = smod.pmod_loc }
  | Pmod_structure sstr ->
      let (str, sg, finalenv) = type_structure env sstr in
      { mod_desc = Tmod_structure str;
        mod_type = Tmty_signature sg;
        mod_env = env;
        mod_loc = smod.pmod_loc }
  | Pmod_functor(name, smty, sbody) ->
      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_module name mty env in
      let body = type_module newenv sbody in
      { mod_desc = Tmod_functor(id, mty, body);
        mod_type = Tmty_functor(id, mty, body.mod_type);
        mod_env = env;
        mod_loc = smod.pmod_loc }
  | Pmod_apply(sfunct, sarg) ->
      let funct = type_module env sfunct in
      let arg = type_module env sarg in
      begin match Mtype.scrape env funct.mod_type with
        Tmty_functor(param, mty_param, mty_res) as mty_functor ->
          let coercion =
            try
              Includemod.modtypes env arg.mod_type mty_param
            with Includemod.Error msg ->
              raise(Error(sarg.pmod_loc, Not_included msg)) in
          let mty_appl =
            try
              let path = path_of_module arg in
              Subst.modtype (Subst.add_module param path Subst.identity)
                            mty_res
            with Not_a_path ->
              try
                Mtype.nondep_supertype
                  (Env.add_module param arg.mod_type env) param mty_res
              with Not_found ->
                raise(Error(smod.pmod_loc,
                            Cannot_eliminate_dependency mty_functor)) in
          { mod_desc = Tmod_apply(funct, arg, coercion);
            mod_type = mty_appl;
            mod_env = env;
            mod_loc = smod.pmod_loc }
      | _ ->
          raise(Error(sfunct.pmod_loc, Cannot_apply funct.mod_type))
      end        
  | Pmod_constraint(sarg, smty) ->
      let arg = type_module env sarg in
      let mty = transl_modtype env smty in
      let coercion =
        try
          Includemod.modtypes env arg.mod_type mty
        with Includemod.Error msg ->
          raise(Error(sarg.pmod_loc, Not_included msg)) in
      { mod_desc = Tmod_constraint(arg, mty, coercion);
        mod_type = mty;
        mod_env = env;
        mod_loc = smod.pmod_loc }

and type_structure env sstr =
  let type_names = ref StringSet.empty
  and module_names = ref StringSet.empty
  and modtype_names = ref StringSet.empty in
  let rec type_struct env sstr =
    Ctype.init_def(Ident.current_time());
    match sstr with
      [] ->
        ([], [], env)
    | {pstr_desc = Pstr_eval sexpr} :: srem ->
        let expr = Typecore.type_expression env sexpr in
        let (str_rem, sig_rem, final_env) = type_struct env srem in
        (Tstr_eval expr :: str_rem, sig_rem, final_env)
    | {pstr_desc = Pstr_value(rec_flag, sdefs)} :: srem ->
        let (defs, newenv) =
          Typecore.type_binding env rec_flag sdefs in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        let bound_idents = let_bound_idents defs in
        let make_sig_value id =
          Tsig_value(id, Env.find_value (Pident id) newenv) in
        (Tstr_value(rec_flag, defs) :: str_rem,
         map_end make_sig_value bound_idents sig_rem,
         final_env)
    | {pstr_desc = Pstr_primitive(name, sdesc)} :: srem ->
        let desc = Typedecl.transl_value_decl env sdesc in
        let (id, newenv) = Env.enter_value name desc env in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_primitive(id, desc) :: str_rem,
         Tsig_value(id, desc) :: sig_rem,
         final_env)
    | {pstr_desc = Pstr_type sdecls; pstr_loc = loc} :: srem ->
        List.iter
          (fun (name, decl) -> check "type" loc type_names name)
          sdecls;
        let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_type decls :: str_rem,
         map_end (fun (id, info) -> Tsig_type(id, info)) decls sig_rem,
         final_env)
    | {pstr_desc = Pstr_exception(name, sarg)} :: srem ->
        let arg = Typedecl.transl_exception env sarg in
        let (id, newenv) = Env.enter_exception name arg env in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_exception(id, arg) :: str_rem,
         Tsig_exception(id, arg) :: sig_rem,
         final_env)
    | {pstr_desc = Pstr_exn_rebind(name, longid); pstr_loc = loc} :: srem ->
        let (path, arg) = Typedecl.transl_exn_rebind env loc longid in
        let (id, newenv) = Env.enter_exception name arg env in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_exn_rebind(id, path) :: str_rem,
         Tsig_exception(id, arg) :: sig_rem,
         final_env)
    | {pstr_desc = Pstr_module(name, smodl); pstr_loc = loc} :: srem ->
        check "module" loc module_names name;
        let modl = type_module env smodl in
        let (id, newenv) = Env.enter_module name modl.mod_type env in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_module(id, modl) :: str_rem,
         Tsig_module(id, modl.mod_type) :: sig_rem,
         final_env)
    | {pstr_desc = Pstr_modtype(name, smty); pstr_loc = loc} :: srem ->
        check "module type" loc modtype_names name;
        let mty = transl_modtype env smty in
        let (id, newenv) = Env.enter_modtype name (Tmodtype_manifest mty) env in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_modtype(id, mty) :: str_rem,
         Tsig_modtype(id, Tmodtype_manifest mty) :: sig_rem,
         final_env)
    | {pstr_desc = Pstr_open lid; pstr_loc = loc} :: srem ->
        let (path, mty) = type_module_path env loc lid in
        let sg = extract_sig_open env loc mty in
        type_struct (Env.open_signature path sg env) srem
    | {pstr_desc = Pstr_class cl; pstr_loc = loc} :: srem ->
         List.iter
           (fun {pci_name = name} -> check "type" loc type_names name)
           cl;
        let (classes, new_env) = Typeclass.class_declarations env cl in
        let (str_rem, sig_rem, final_env) = type_struct new_env srem in
        (Tstr_class
           (List.map (fun (i, _,_,_,_,_,_,_, s, m, c) ->
              (i, s, m, c)) classes) ::
         Tstr_cltype
           (List.map (fun (_,_, i, d, _,_,_,_,_,_,_) -> (i, d)) classes) ::
         Tstr_type
           (List.map (fun (_,_,_,_, i, d, _,_,_,_,_) -> (i, d)) classes) ::
         Tstr_type
           (List.map (fun (_,_,_,_,_,_, i, d, _,_,_) -> (i, d)) classes) ::
         str_rem,
         List.flatten
           (map_end
              (fun (i, d, i', d', i'', d'', i''', d''', _, _, _) ->
                 [Tsig_class(i, d);    Tsig_cltype(i', d');
                  Tsig_type(i'', d''); Tsig_type(i''', d''')])
              classes [sig_rem]),
         final_env)
    | {pstr_desc = Pstr_class_type cl; pstr_loc = loc} :: srem ->
        List.iter
          (fun {pci_name = name} -> check "type" loc type_names name)
          cl;
        let (classes, new_env) = Typeclass.class_type_declarations env cl in
        let (str_rem, sig_rem, final_env) = type_struct new_env srem in
        (Tstr_cltype
           (List.map (fun (i, d, _, _, _, _) -> (i, d)) classes) ::
         Tstr_type
           (List.map (fun (_, _, i, d, _, _) -> (i, d)) classes) ::
         Tstr_type
           (List.map (fun (_, _, _, _, i, d) -> (i, d)) classes) ::
         str_rem,
         List.flatten
           (map_end
              (fun (i, d, i', d', i'', d'') ->
                 [Tsig_cltype(i, d); Tsig_type(i', d'); Tsig_type(i'', d'')])
              classes [sig_rem]),
         final_env)
    | {pstr_desc = Pstr_include smodl; pstr_loc = loc} :: srem ->
        let modl = type_module env smodl in
        (* Rename all identifiers bound by this signature to avoid clashes *)
        let sg = Subst.signature Subst.identity
                   (extract_sig_open env smodl.pmod_loc modl.mod_type) in
        List.iter
          (check_sig_item type_names module_names modtype_names loc) sg;
        let new_env = Env.add_signature sg env in
        let (str_rem, sig_rem, final_env) = type_struct new_env srem in
        (Tstr_include (modl, bound_value_identifiers sg) :: str_rem,
         sg @ sig_rem,
         final_env)
  in type_struct env sstr

(* Fill in the forward declaration *)
let _ =
  Typecore.type_module := type_module

(* Normalize types in a signature *)

let rec normalize_modtype env = function
    Tmty_ident p -> ()
  | Tmty_signature sg -> normalize_signature env sg
  | Tmty_functor(id, param, body) -> normalize_modtype env body

and normalize_signature env = List.iter (normalize_signature_item env)

and normalize_signature_item env = function
    Tsig_value(id, desc) -> Ctype.normalize_type env desc.val_type
  | Tsig_module(id, mty) -> normalize_modtype env mty
  | _ -> ()

(* Typecheck an implementation file *)

let type_implementation sourcefile prefixname modulename initial_env ast =
  let (str, sg, finalenv) = type_structure initial_env ast in
  if !Clflags.print_types then
    fprintf std_formatter "%a@." Printtyp.signature sg;
  let coercion =
    if Sys.file_exists (prefixname ^ !Config.interface_suffix) then begin
      let intf_file =
        try find_in_path !Config.load_path (prefixname ^ ".cmi")
        with Not_found ->
          raise (Env.Error(Env.File_not_found (prefixname ^ ".cmi"))) in
      let dclsig = Env.read_signature modulename intf_file in
      Includemod.compunit sourcefile sg intf_file dclsig
    end else begin
      check_nongen_schemes finalenv str;
      normalize_signature finalenv sg;
      Env.save_signature sg modulename (prefixname ^ ".cmi");
      Tcoerce_none
    end in
  (str, coercion)

(* Error report *)

open Printtyp

let report_error ppf = function
  | Unbound_module lid -> fprintf ppf "Unbound module %a" longident lid
  | Unbound_modtype lid -> fprintf ppf "Unbound module type %a" longident lid
  | Cannot_apply mty ->
      fprintf ppf
        "@[This module is not a functor; it has type@ %a@]" modtype mty
  | Not_included errs ->
      fprintf ppf
        "@[<v>Signature mismatch:@ %a@]" Includemod.report_error errs
  | Cannot_eliminate_dependency mty ->
      fprintf ppf
        "@[This functor has type@ %a@ \
           The parameter cannot be eliminated in the result type.@  \
           Please bind the argument to a module identifier.@]" modtype mty
  | Signature_expected -> fprintf ppf "This module type is not a signature"
  | Structure_expected mty ->
      fprintf ppf
        "@[This module is not a structure; it has type@ %a" modtype mty
  | With_no_component lid ->
      fprintf ppf
        "@[The signature constrained by `with' has no component named %a@]"
        longident lid
  | With_mismatch(lid, explanation) ->
      fprintf ppf
        "@[<v>\
           @[In this `with' constraint, the new definition of %a@ \
             does not match its original definition@ \
             in the constrained signature:@]@ \
           %a@]"
        longident lid Includemod.report_error explanation
  | Repeated_name(kind, name) ->
      fprintf ppf
        "@[Multiple definition of the %s name %s.@ \
           Names must be unique in a given structure or signature.@]" kind name
  | Non_generalizable typ ->
      fprintf ppf
        "@[The type of this expression,@ %a,@ \
           contains type variables that cannot be generalized@]" type_scheme typ
  | Non_generalizable_class (id, desc) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains type variables that cannot be generalized@]"
        (class_declaration id) desc
  | Non_generalizable_module mty ->
      fprintf ppf
        "@[The type of this module,@ %a,@ \
           contains type variables that cannot be generalized@]" modtype mty
