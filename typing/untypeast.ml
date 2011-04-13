
open Misc
open Asttypes
open Typedtree
open Parsetree

(*
Some notes:

   * For Pexp_function, we cannot go back to the exact original version
   when there is a default argument, because the default argument is
   translated in the typer. The code, if printed, will not be parsable because
   new generated identifiers are not correct.

   * For Pexp_apply, it is unclear whether arguments are reordered, especially
    when there are optional arguments.

*)


let rec lident_of_path path =
  match path with
      Path.Pident id -> Longident.Lident (Ident.name id)
    | Path.Pdot (p, s, _) -> Longident.Ldot (lident_of_path p, s)
    | Path.Papply (p1, p2) ->
	Longident.Lapply (lident_of_path p1, lident_of_path p2)

let rec untype_structure str =
  List.map untype_structure_item str.str_items

and untype_structure_item item =
  let desc =
    match item.str_desc with
      Tstr_eval exp -> Pstr_eval (untype_expression exp)
    | Tstr_value (rec_flag, list) ->
        Pstr_value (rec_flag, List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) list)
    | Tstr_primitive (id, v) ->
        Pstr_primitive (Ident.name id, untype_value_description v)
    | Tstr_type list ->
        Pstr_type (List.map (fun (id, decl) ->
              Ident.name id, untype_type_declaration decl) list)
    | Tstr_exception (id, decl) ->
        Pstr_exception (Ident.name id, untype_exception_declaration decl)
    | Tstr_exn_rebind (id, p) ->
        Pstr_exn_rebind (Ident.name id, lident_of_path p)
    | Tstr_module (id, mexpr) ->
        Pstr_module (Ident.name id, untype_module_expr mexpr)
    | Tstr_recmodule list ->
        Pstr_recmodule (List.map (fun (id, mtype, mexpr) ->
              Ident.name id, untype_module_type mtype,
              untype_module_expr mexpr) list)
    | Tstr_modtype (id, mtype) ->
        Pstr_modtype (Ident.name id, untype_module_type mtype)
    | Tstr_open path -> Pstr_open (lident_of_path path)
    | Tstr_class list ->
        Pstr_class (List.map (fun (ci, _, _) ->
              { pci_virt = ci.ci_virt;
                pci_params = ci.ci_params;
                pci_name = Ident.name ci.ci_id_class;
                pci_expr = untype_class_expr ci.ci_expr;
                pci_variance = ci.ci_variance;
                pci_loc = ci.ci_loc;
              }
          ) list)
    | Tstr_class_type list ->
        Pstr_class_type (List.map (fun (id, ct) ->
              {
                pci_virt = ct.ci_virt;
                pci_params = ct.ci_params;
                pci_name = Ident.name ct.ci_id_class;
                pci_expr = untype_class_type ct.ci_expr;
                pci_variance = ct.ci_variance;
                pci_loc = ct.ci_loc;
              }
          ) list)
    | Tstr_include (mexpr, _) ->
        Pstr_include (untype_module_expr mexpr)
  in
  { pstr_desc = desc; pstr_loc = item.str_loc; }

and untype_value_description v =
  {
    pval_prim = v.val_prim;
    pval_type = untype_core_type v.val_desc;
    pval_loc = v.val_loc }

and untype_type_declaration decl =
  {
    ptype_params = decl.typ_params;
    ptype_cstrs = List.map (fun (ct1, ct2, loc) ->
        (untype_core_type ct1,
          untype_core_type ct2, loc)
    ) decl.typ_cstrs;
    ptype_kind = (match decl.typ_kind with
        Ttype_abstract -> Ptype_abstract
      | Ttype_variant list ->
          Ptype_variant (List.map (fun (s, cts, loc) ->
                (s, List.map untype_core_type cts, loc)
            ) list)
      | Ttype_record list ->
          Ptype_record (List.map (fun (s, mut, ct, loc) ->
                (s, mut, untype_core_type ct, loc)
            ) list)
    );
    ptype_private = decl.typ_private;
    ptype_manifest = (match decl.typ_manifest with
        None -> None
      | Some ct -> Some (untype_core_type ct));
    ptype_variance = decl.typ_variance;
    ptype_loc = decl.typ_loc;
  }

and untype_exception_declaration decl =
  List.map untype_core_type decl

and untype_pattern pat =
  let desc = match pat.pat_desc with
      Tpat_any -> Ppat_any
    | Tpat_var id ->
        begin
          match (Ident.name id).[0] with
            'A'..'Z' ->
              Ppat_unpack (Ident.name id)
          | _ ->
              Ppat_var (Ident.name id)
        end
    | Tpat_alias (pat, TPat_alias id) ->
        Ppat_alias (untype_pattern pat, Ident.name id)
    | Tpat_constant cst -> Ppat_constant cst
    | Tpat_tuple list ->
        Ppat_tuple (List.map untype_pattern list)
    | Tpat_construct (path, _, args) ->
        Ppat_construct (lident_of_path path,
          (match args with
              [] -> None
            | args -> Some
                  { ppat_desc = Ppat_tuple (List.map untype_pattern args);
                  ppat_loc = pat.pat_loc; }
          ), true)
    | Tpat_variant (label, pato, _) ->
        Ppat_variant (label, match pato with
            None -> None
          | Some pat -> Some (untype_pattern pat))
    | Tpat_record (list, closed) ->
        Ppat_record (List.map (fun (path, _, pat) ->
              lident_of_path path, untype_pattern pat) list, closed)
    | Tpat_array list -> Ppat_array (List.map untype_pattern list)
    | Tpat_or (p1, p2, _) -> Ppat_or (untype_pattern p1, untype_pattern p2)
    | Tpat_lazy p -> Ppat_lazy (untype_pattern p)
    | Tpat_alias (p, TPat_constraint ct) ->
        Ppat_constraint (untype_pattern p, untype_core_type ct)
    | Tpat_alias (p, TPat_type path) ->
        Ppat_type (lident_of_path path)
  in
  {
    ppat_desc = desc;
    ppat_loc = pat.pat_loc;
  }

and untype_expression exp =
  let desc =
    match exp.exp_desc with
      Texp_ident (path, _) -> Pexp_ident (lident_of_path path)
    | Texp_constant cst -> Pexp_constant cst
    | Texp_let (rec_flag, list, exp) ->
        Pexp_let (rec_flag,
          List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) list,
          untype_expression exp)
    | Texp_function (label, cases, _) ->
        Pexp_function (label, None,
          List.map (fun (pat, exp) ->
              (untype_pattern pat, untype_expression exp)) cases)
    | Texp_apply (exp, list) ->
        Pexp_apply (untype_expression exp,
          List.fold_right (fun (label, expo, _) list ->
              match expo with
                None -> list
              | Some exp -> (label, untype_expression exp) :: list
          ) list [])
    | Texp_match (exp, list, _) ->
        Pexp_match (untype_expression exp,
          List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) list)
    | Texp_try (exp, list) ->
        Pexp_try (untype_expression exp,
          List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) list)
    | Texp_tuple list ->
        Pexp_tuple (List.map untype_expression list)
    | Texp_construct (path, _, args) ->
        Pexp_construct (lident_of_path path,
          (match args with
              [] -> None
            | args -> Some
                  { pexp_desc = Pexp_tuple (List.map untype_expression args);
                  pexp_loc = exp.exp_loc; }
          ), true)

    | Texp_variant (label, expo) ->
        Pexp_variant (label, match expo with
            None -> None
          | Some exp -> Some (untype_expression exp))
    | Texp_record (list, expo) ->
        Pexp_record (List.map (fun (path, _, exp) ->
              lident_of_path path, untype_expression exp
          ) list,
          match expo with
            None -> None
          | Some exp -> Some (untype_expression exp))
    | Texp_field (exp, path, label) ->
        Pexp_field (untype_expression exp, lident_of_path path)
    | Texp_setfield (exp1, path , label, exp2) ->
        Pexp_setfield (untype_expression exp1, lident_of_path path,
          untype_expression exp2)
    | Texp_array list ->
        Pexp_array (List.map untype_expression list)
    | Texp_ifthenelse (exp1, exp2, expo) ->
        Pexp_ifthenelse (untype_expression exp1,
          untype_expression exp2,
          match expo with
            None -> None
          | Some exp -> Some (untype_expression exp))
    | Texp_sequence (exp1, exp2) ->
        Pexp_sequence (untype_expression exp1, untype_expression exp2)
    | Texp_while (exp1, exp2) ->
        Pexp_while (untype_expression exp1, untype_expression exp2)
    | Texp_for (id, exp1, exp2, dir, exp3) ->
        Pexp_for (Ident.name id,
          untype_expression exp1, untype_expression exp2,
          dir, untype_expression exp3)
    | Texp_when (exp1, exp2) ->
        Pexp_when (untype_expression exp1, untype_expression exp2)
    | Texp_send (exp, meth, _) ->
        Pexp_send (untype_expression exp, match meth with
            Tmeth_name name -> name
          | Tmeth_val id -> Ident.name id)
    | Texp_new (path, _) -> Pexp_new (lident_of_path path)
    | Texp_instvar (_, path) -> Pexp_ident (lident_of_path path)
    | Texp_setinstvar (_, path, exp) ->
        Pexp_setinstvar (Path.name path, untype_expression exp)
    | Texp_override (_, list) ->
        Pexp_override (List.map (fun (path, exp) ->
              Path.name path, untype_expression exp
          ) list)
    | Texp_letmodule (id, mexpr, exp) ->
        Pexp_letmodule (Ident.name id, untype_module_expr mexpr,
          untype_expression exp)
    | Texp_assert exp -> Pexp_assert (untype_expression exp)
    | Texp_assertfalse -> Pexp_assertfalse
    | Texp_lazy exp -> Pexp_lazy (untype_expression exp)
    | Texp_object (cl, _) ->
        Pexp_object (untype_class_structure cl)
    | Texp_pack (mexpr) ->
        Pexp_pack (untype_module_expr mexpr)
    | Texp_poly (exp, None) -> Pexp_poly(untype_expression exp, None)
    | Texp_poly (exp, Some ct) ->
        Pexp_poly (untype_expression exp, Some (untype_core_type ct))
    | Texp_open (path, exp) ->
        Pexp_open (lident_of_path path, untype_expression exp)
    | Texp_newtype (s, exp) ->
        Pexp_newtype (s, untype_expression exp)
    | Texp_constraint (exp, cto1, cto2) ->
        Pexp_constraint (untype_expression exp,
          may_map untype_core_type cto1, may_map untype_core_type cto2)

  in
  { pexp_loc = exp.exp_loc;
    pexp_desc = desc;
  }

and untype_package_type pack =
  (lident_of_path pack.pack_name,
    List.map (fun (s, ct) ->
        (s, untype_core_type ct)) pack.pack_fields)

and untype_signature sg =
  List.map untype_signature_item sg.sig_items

and untype_signature_item item =
  let desc =
    match item.sig_desc with
      Tsig_value (id, v) ->
        Psig_value (Ident.name id, untype_value_description v)
    | Tsig_type list ->
        Psig_type (List.map (fun (id, decl) ->
              Ident.name id, untype_type_declaration decl
          ) list)
    | Tsig_exception (id, decl) ->
        Psig_exception (Ident.name id, untype_exception_declaration decl)
    | Tsig_module (id, mtype) ->
        Psig_module (Ident.name id, untype_module_type mtype)
    | Tsig_recmodule list ->
        Psig_recmodule (List.map (fun (id, mtype) ->
              Ident.name id, untype_module_type mtype) list)
    | Tsig_modtype (id, mdecl) ->
        Psig_modtype (Ident.name id, untype_modtype_declaration mdecl)
    | Tsig_open path -> Psig_open (lident_of_path path)
    | Tsig_include mty -> Psig_include (untype_module_type mty)
    | Tsig_class list ->
        Psig_class (List.map untype_class_description list)
    | Tsig_class_type list ->
        Psig_class_type (List.map untype_class_type_declaration list)
  in
  { psig_desc = desc;
    psig_loc = item.sig_loc;
  }

and untype_modtype_declaration mdecl =
  match mdecl with
    Tmodtype_abstract -> Pmodtype_abstract
  | Tmodtype_manifest mtype -> Pmodtype_manifest (untype_module_type mtype)

and untype_class_description cd =
  {
    pci_virt = cd.ci_virt;
    pci_params = cd.ci_params;
    pci_name = Ident.name cd.ci_id_class;
    pci_expr = untype_class_type cd.ci_expr;
    pci_variance = cd.ci_variance;
    pci_loc = cd.ci_loc;
  }

and untype_class_type_declaration cd =
  {
    pci_virt = cd.ci_virt;
    pci_params = cd.ci_params;
    pci_name = Ident.name cd.ci_id_class;
    pci_expr = untype_class_type cd.ci_expr;
    pci_variance = cd.ci_variance;
    pci_loc = cd.ci_loc;
  }

and untype_module_type mty =
  let desc = match mty.mty_desc with
      Tmty_ident path -> Pmty_ident (lident_of_path path)
    | Tmty_signature sg -> Pmty_signature (untype_signature sg)
    | Tmty_functor (id, mtype1, mtype2) ->
        Pmty_functor (Ident.name id, untype_module_type mtype1,
          untype_module_type mtype2)
    | Tmty_with (mtype, list) ->
        Pmty_with (untype_module_type mtype,
          List.map (fun (path, withc) ->
              lident_of_path path, untype_with_constraint withc
          ) list)
    | Tmty_typeof mexpr ->
        Pmty_typeof (untype_module_expr mexpr)
  in
  {
    pmty_desc = desc;
    pmty_loc = mty.mty_loc;
  }

and untype_with_constraint cstr =
  match cstr with
    Twith_type decl -> Pwith_type (untype_type_declaration decl)
  | Twith_module path -> Pwith_module (lident_of_path path)
  | Twith_typesubst decl -> Pwith_typesubst (untype_type_declaration decl)
  | Twith_modsubst path -> Pwith_modsubst (lident_of_path path)

and untype_module_expr mexpr =
  match mexpr.mod_desc with
    Tmod_constraint (m, _, Tmodtype_implicit, _ ) ->
      untype_module_expr m
  | _ ->
      let desc = match mexpr.mod_desc with
          Tmod_ident p -> Pmod_ident (lident_of_path p)
        | Tmod_structure st -> Pmod_structure (untype_structure st)
        | Tmod_functor (id, mtype, mexpr) ->
            Pmod_functor (Ident.name id, untype_module_type mtype,
              untype_module_expr mexpr)
        | Tmod_apply (mexp1, mexp2, _) ->
            Pmod_apply (untype_module_expr mexp1, untype_module_expr mexp2)
        | Tmod_constraint (mexpr, _, Tmodtype_explicit mtype, _) ->
            Pmod_constraint (untype_module_expr mexpr,
              untype_module_type mtype)
        | Tmod_constraint (mexpr, _, Tmodtype_implicit, _) ->
            assert false
        | Tmod_unpack (exp, pack) ->
        Pmod_unpack (untype_expression exp)
        (* TODO , untype_package_type pack) *)

  in
  {
    pmod_desc = desc;
    pmod_loc = mexpr.mod_loc;
  }

and untype_class_expr cexpr =
  let desc = match cexpr.cl_desc with
    | Tcl_constraint ( { cl_desc = Tcl_ident (path, tyl) }, None, _, _, _ ) ->
        Pcl_constr (lident_of_path path,
          List.map untype_core_type tyl)
    | Tcl_structure clstr -> Pcl_structure (untype_class_structure clstr)

    | Tcl_fun (label, pat, pv, cl, partial) ->
        Pcl_fun (label, None, untype_pattern pat, untype_class_expr cl)

    | Tcl_apply (cl, args) ->
        Pcl_apply (untype_class_expr cl,
          List.fold_right (fun (label, expo, _) list ->
              match expo with
                None -> list
              | Some exp -> (label, untype_expression exp) :: list
          ) args [])

    | Tcl_let (rec_flat, bindings, ivars, cl) ->
        Pcl_let (rec_flat,
          List.map (fun (pat, exp) ->
              (untype_pattern pat, untype_expression exp)) bindings,
          untype_class_expr cl)

    | Tcl_constraint (cl, Some clty, vals, meths, concrs) ->
        Pcl_constraint (untype_class_expr cl,  untype_class_type clty)

    | Tcl_ident _ -> assert false
    | Tcl_constraint (_, None, _, _, _) -> assert false
  in
  { pcl_desc = desc;
    pcl_loc = cexpr.cl_loc;
  }

and untype_class_type ct =
  let desc = match ct.cltyp_desc with
      Tcty_signature csg -> Pcty_signature (untype_class_signature csg)
    | Tcty_constr (path, list) ->
        Pcty_constr (lident_of_path path, List.map untype_core_type list)
    | Tcty_fun (label, ct, cl) ->
        Pcty_fun (label, untype_core_type ct, untype_class_type cl)
  in
  { pcty_desc = desc;
    pcty_loc = ct.cltyp_loc }

and untype_class_signature cs =
  {
    pcsig_self = untype_core_type cs.csig_self;
    pcsig_fields = List.map untype_class_type_field cs.csig_fields;
    pcsig_loc = cs.csig_loc;
  }

and untype_class_type_field ctf =
  let desc = match ctf.ctf_desc with
      Tctf_inher ct -> Pctf_inher (untype_class_type ct)
    | Tctf_val (s, mut, virt, ct) ->
        Pctf_val (s, mut, virt, untype_core_type ct)
    | Tctf_virt  (s, priv, ct) ->
        Pctf_virt (s, priv, untype_core_type ct)
    | Tctf_meth  (s, priv, ct) ->
        Pctf_meth  (s, priv, untype_core_type ct)
    | Tctf_cstr  (ct1, ct2) ->
        Pctf_cstr (untype_core_type ct1, untype_core_type ct2)
  in
  {
    pctf_desc = desc;
    pctf_loc = ctf.ctf_loc;
  }

and untype_core_type ct =
  let desc = match ct.ctyp_desc with
      Ttyp_any -> Ptyp_any
    | Ttyp_var s -> Ptyp_var s
    | Ttyp_arrow (label, ct1, ct2) ->
        Ptyp_arrow (label, untype_core_type ct1, untype_core_type ct2)
  | Ttyp_tuple list -> Ptyp_tuple (List.map untype_core_type list)
    | Ttyp_constr (path, list) ->
        Ptyp_constr (lident_of_path path,
          List.map untype_core_type list)
    | Ttyp_object list ->
        Ptyp_object (List.map untype_core_field_type list)
    | Ttyp_class (path, list, labels) ->
        Ptyp_class (lident_of_path path,
          List.map untype_core_type list, labels)
    | Ttyp_alias (ct, s) ->
        Ptyp_alias (untype_core_type ct, s)
    | Ttyp_variant (list, bool, labels) ->
        Ptyp_variant (List.map untype_row_field list, bool, labels)
    | Ttyp_poly (list, ct) -> Ptyp_poly (list, untype_core_type ct)
    | Ttyp_package pack -> Ptyp_package (untype_package_type pack)
  in
  { ptyp_desc = desc; ptyp_loc = ct.ctyp_loc }

and untype_core_field_type cft =
  { pfield_desc = (match cft.field_desc with
        Tcfield_var -> Pfield_var
      | Tcfield (s, ct) -> Pfield (s, untype_core_type ct));
    pfield_loc = cft.field_loc; }

and untype_class_structure cs =
  { pcstr_pat = untype_pattern cs.cstr_pat;
    pcstr_fields = List.map untype_class_field cs.cstr_fields;
  }

and untype_row_field rf =
  match rf with
    Ttag (label, bool, list) ->
      Rtag (label, bool, List.map untype_core_type list)
  | Tinherit ct -> Rinherit (untype_core_type ct)

and untype_class_field cf =
  let desc = match cf.cf_desc with
      Tcf_inher (ovf, cl, super, _vals, _meths) ->
        Pcf_inher (ovf, untype_class_expr cl, super)
    | Tcf_constr (cty, cty') ->
        Pcf_constr (untype_core_type cty, untype_core_type cty')
    | Tcf_val (lab, mut, _, Tcfk_virtual cty, override) ->
        Pcf_valvirt (lab, mut, untype_core_type cty)
    | Tcf_val (lab, mut, _, Tcfk_concrete exp, override) ->
        Pcf_val (lab, mut,
          (if override then Override else Fresh),
          untype_expression exp)
    | Tcf_meth (lab, priv, Tcfk_virtual cty, override) ->
        Pcf_virt (lab, priv, untype_core_type cty)
    | Tcf_meth (lab, priv, Tcfk_concrete exp, override) ->
        Pcf_meth (lab, priv,
          (if override then Override else Fresh),
          untype_expression exp)
    | Tcf_let (rec_flag, bindings, _) ->
        Pcf_let (rec_flag, List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) bindings)
  | Tcf_init exp -> Pcf_init (untype_expression exp)
  in
  { pcf_desc = desc; pcf_loc = cf.cf_loc }
