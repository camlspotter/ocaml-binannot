(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Misc
open Parsetree
open Asttypes
open Types
open Typedtree
open Typecore
open Typetexp
open Format

type error =
    Unconsistent_constraint of (type_expr * type_expr) list
  | Method_type_mismatch of string * (type_expr * type_expr) list
  | Structure_expected of class_type
  | Cannot_apply of class_type
  | Apply_wrong_label of label
  | Pattern_type_clash of type_expr
  | Repeated_parameter
  | Unbound_class of Longident.t
  | Unbound_class_2 of Longident.t
  | Unbound_class_type of Longident.t
  | Unbound_class_type_2 of Longident.t
  | Abbrev_type_clash of type_expr * type_expr * type_expr
  | Constructor_type_mismatch of string * (type_expr * type_expr) list
  | Virtual_class of bool * string list
  | Parameter_arity_mismatch of Longident.t * int * int
  | Parameter_mismatch of (type_expr * type_expr) list
  | Bad_parameters of Ident.t * type_expr * type_expr
  | Class_match_failure of Ctype.class_match_failure list
  | Unbound_val of string
  | Unbound_type_var of (formatter -> unit) * Ctype.closed_class_failure
  | Make_nongen_seltype of type_expr
  | Non_generalizable_class of Ident.t * Types.class_declaration

exception Error of Location.t * error


                       (**********************)
                       (*  Useful constants  *)
                       (**********************)
                                   

(*
   Self type have a dummy private method, thus preventing it to become
   closed.
*)
let dummy_method = "*dummy method*"

(*
   Path associated to the temporary class type of a class being typed
   (its constructor is not available).
*)
let unbound_class = Path.Pident (Ident.create "")


                (************************************)
                (*  Some operations on class types  *)
                (************************************)
                                   

(* Fully expand the head of a class type *)
let rec scrape_class_type =
  function
    Tcty_constr (_, _, cty) -> scrape_class_type cty
  | cty                     -> cty

(* Generalize a class type *)
let rec generalize_class_type =
  function
    Tcty_constr (_, params, cty) ->
      List.iter Ctype.generalize params;
      generalize_class_type cty
  | Tcty_signature {cty_self = sty; cty_vars = vars } ->
      Ctype.generalize sty;
      Vars.iter (fun _ (_, ty) -> Ctype.generalize ty) vars
  | Tcty_fun (_, ty, cty) ->
      Ctype.generalize ty;
      generalize_class_type cty

(* Return the virtual methods of a class type *)
let virtual_methods cty =
  let sign = Ctype.signature_of_class_type cty in
  let (fields, _) = Ctype.flatten_fields (Ctype.object_fields sign.cty_self) in
  List.fold_left
    (fun virt (lab, _, _) ->
       if lab = dummy_method then virt else
       if Concr.mem lab sign.cty_concr then virt else
       lab::virt)
    [] fields

(* Return the constructor type associated to a class type *)
let rec constructor_type constr cty =
  match cty with
    Tcty_constr (_, _, cty) ->
      constructor_type constr cty
  | Tcty_signature sign ->
      constr
  | Tcty_fun (l, ty, cty) ->
      Ctype.newty (Tarrow (l, ty, constructor_type constr cty))

let rec class_body cty =
  match cty with
    Tcty_constr (_, _, cty') ->
      cty (* Only class bodies can be abbreviated *)
  | Tcty_signature sign ->
      cty
  | Tcty_fun (_, ty, cty) ->
      class_body cty

let rec extract_constraints cty =
  let sign = Ctype.signature_of_class_type cty in
  (Vars.fold (fun lab _ vars -> lab :: vars) sign.cty_vars [],
   begin let (fields, _) =
     Ctype.flatten_fields (Ctype.object_fields sign.cty_self)
   in
   List.fold_left
     (fun meths (lab, _, _) ->
        if lab = dummy_method then meths else lab::meths)
     [] fields
   end,
   sign.cty_concr)

let rec abbreviate_class_type path params cty =
  match cty with
    Tcty_constr (_, _, _) | Tcty_signature _ ->
      Tcty_constr (path, params, cty)
  | Tcty_fun (l, ty, cty) ->
      Tcty_fun (l, ty, abbreviate_class_type path params cty)

let rec closed_class_type =
  function
    Tcty_constr (_, params, _) ->
      List.for_all Ctype.closed_schema params
  | Tcty_signature sign ->
      Ctype.closed_schema sign.cty_self
        &&
      Vars.fold (fun _ (_, ty) cc -> Ctype.closed_schema ty && cc)
        sign.cty_vars
        true
  | Tcty_fun (_, ty, cty) ->
      Ctype.closed_schema ty
        &&
      closed_class_type cty

let closed_class cty =
  List.for_all Ctype.closed_schema cty.cty_params
    &&
  closed_class_type cty.cty_type

let rec limited_generalize rv =
  function
    Tcty_constr (path, params, cty) ->
      List.iter (Ctype.limited_generalize rv) params;
      limited_generalize rv cty
  | Tcty_signature sign ->
      Ctype.limited_generalize rv sign.cty_self;
      Vars.iter (fun _ (_, ty) -> Ctype.limited_generalize rv ty)
        sign.cty_vars
  | Tcty_fun (_, ty, cty) ->
      Ctype.limited_generalize rv ty;
      limited_generalize rv cty

                (***********************************)
                (*  Primitives for typing classes  *)
                (***********************************)
                                   

(* Enter a value in the method environment only *)
let enter_met_env lab kind ty val_env met_env par_env =
  let (id, val_env) =
    Env.enter_value lab {val_type = ty; val_kind = Val_unbound} val_env
  in
  (id, val_env,
   Env.add_value id {val_type = ty; val_kind = kind} met_env,
   Env.add_value id {val_type = ty; val_kind = Val_unbound} par_env)

(* Enter an instance variable in the environment *)
let enter_val cl_num vars lab mut ty val_env met_env par_env =
  let (id, val_env, met_env, par_env) as result =
    enter_met_env lab (Val_ivar (mut, cl_num)) ty val_env met_env par_env
  in
  vars := Vars.add lab (id, mut, ty) !vars;
  result

let inheritance impl self_type env concr_meths loc parent =
  match scrape_class_type parent with
    Tcty_signature cl_sig ->

      (* Methods *)
      begin try
        Ctype.unify env self_type cl_sig.cty_self
      with Ctype.Unify trace ->
        match trace with
          _::_::_::({desc = Tfield(n, _, _, _)}, _)::rem ->
            raise(Error(loc, Method_type_mismatch (n, rem)))
        | _ ->
            assert false
      end;

      if impl then begin
        let overridings = Concr.inter cl_sig.cty_concr concr_meths in
        if not (Concr.is_empty overridings) then begin
          Location.prerr_warning loc
            (Warnings.Method_override (Concr.elements overridings))
        end
      end;
      let concr_meths = Concr.union cl_sig.cty_concr concr_meths in

      (cl_sig, concr_meths)

  | _ ->
      raise(Error(loc, Structure_expected parent))

let virtual_method val_env meths self_type lab priv sty loc =
  let (_, ty') =
     Ctype.filter_self_method val_env lab priv meths self_type
  in
  let ty = transl_simple_type val_env false sty in
  try Ctype.unify val_env ty ty' with Ctype.Unify trace ->
    raise(Error(loc, Method_type_mismatch (lab, trace)))

let type_constraint val_env sty sty' loc =
  let ty  = transl_simple_type val_env false sty in
  let ty' = transl_simple_type val_env false sty' in
  try Ctype.unify val_env ty ty' with Ctype.Unify trace ->
    raise(Error(loc, Unconsistent_constraint trace))

let mkpat d = { ppat_desc = d; ppat_loc = Location.none }
let make_method cl_num expr =
  { pexp_desc =
      Pexp_function ("", None,
                     [mkpat (Ppat_alias (mkpat(Ppat_var "self-*"),
                                         "self-" ^ cl_num)),
                      expr]);
    pexp_loc = expr.pexp_loc }

(*******************************)

let rec class_type_field env self_type meths (val_sig, concr_meths) =
  function
    Pctf_inher sparent ->
      let parent = class_type env sparent in
      let (cl_sig, concr_meths) =
        inheritance false self_type env concr_meths sparent.pcty_loc parent
      in
      let val_sig =
        Vars.fold
          (fun lab (mut, ty) val_sig -> Vars.add lab (mut, ty) val_sig)
          cl_sig.cty_vars val_sig
      in
      (val_sig, concr_meths)

  | Pctf_val (lab, mut, sty_opt, loc) ->
      let (mut, ty) =
        match sty_opt with
          None     ->
            let (mut', ty) =
              try Vars.find lab val_sig with Not_found ->
                raise(Error(loc, Unbound_val lab))
            in
            (if mut = Mutable then mut' else Immutable), ty
        | Some sty ->
            mut, transl_simple_type env false sty
      in
      (Vars.add lab (mut, ty) val_sig, concr_meths)

  | Pctf_virt (lab, priv, sty, loc) ->
      virtual_method env meths self_type lab priv sty loc;
      (val_sig, concr_meths)

  | Pctf_meth (lab, priv, sty, loc)  ->
      virtual_method env meths self_type lab priv sty loc;
      (val_sig, Concr.add lab concr_meths)

  | Pctf_cstr (sty, sty', loc) ->
      type_constraint env sty sty' loc;
      (val_sig, concr_meths)

and class_signature env sty sign =
  let meths = ref Meths.empty in
  let self_type = transl_simple_type env false sty in
  
  (* Check that the binder is a correct type, and introduce a dummy
     method preventing self type from being closed. *)
  begin try
    Ctype.unify env
      (Ctype.filter_method env dummy_method Private self_type)
      (Ctype.newty (Ttuple []))
  with Ctype.Unify _ ->
    raise(Error(sty.ptyp_loc, Pattern_type_clash self_type))
  end;
  
  (* Class type fields *)
  let (val_sig, concr_meths) =
    List.fold_left (class_type_field env self_type meths)
      (Vars.empty, Concr.empty)
      sign
  in
  
  {cty_self = self_type;
   cty_vars = val_sig;
   cty_concr = concr_meths }

and class_type env scty =
  match scty.pcty_desc with
    Pcty_constr (lid, styl) ->
      let (path, decl) =
        try Env.lookup_cltype lid env with Not_found ->
          raise(Error(scty.pcty_loc, Unbound_class_type lid))
      in
      if Path.same decl.clty_path unbound_class then
        raise(Error(scty.pcty_loc, Unbound_class_type_2 lid));
      let (params, clty) =
        Ctype.instance_class decl.clty_params decl.clty_type
      in
      let sty = Ctype.self_type clty in
      if List.length params <> List.length styl then
        raise(Error(scty.pcty_loc,
                    Parameter_arity_mismatch (lid, List.length params,
                                                   List.length styl)));
      List.iter2
        (fun sty ty ->
           let ty' = transl_simple_type env false sty in
           try Ctype.unify env ty' ty with Ctype.Unify trace ->
             raise(Error(sty.ptyp_loc, Parameter_mismatch trace)))
        styl params;
      Tcty_constr (path, params, clty)

  | Pcty_signature (sty, sign) ->
      Tcty_signature (class_signature env sty sign)
      
  | Pcty_fun (l, sty, scty) ->
      let ty = transl_simple_type env false sty in
      let cty = class_type env scty in
      Tcty_fun (l, ty, cty)

(*******************************)

module StringSet = Set.Make(struct type t = string let compare = compare end)

let rec class_field cl_num self_type meths vars
    (val_env, met_env, par_env, fields, concr_meths, inh_vals) =
  function
    Pcf_inher (sparent, super) ->
      let parent = class_expr cl_num val_env par_env sparent in
      let (cl_sig, concr_meths) =
        inheritance true self_type val_env concr_meths sparent.pcl_loc
          parent.cl_type
      in
      (* Variables *)
      let (val_env, met_env, par_env, inh_vars, inh_vals) =
        Vars.fold
          (fun lab (mut, ty) (val_env, met_env, par_env, inh_vars, inh_vals) ->
             let (id, val_env, met_env, par_env) =
               enter_val cl_num vars lab mut ty val_env met_env par_env
             in
             if StringSet.mem lab inh_vals then
               Location.prerr_warning sparent.pcl_loc
                 (Warnings.Hide_instance_variable lab);
             (val_env, met_env, par_env, (lab, id) :: inh_vars,
              StringSet.add lab inh_vals))
          cl_sig.cty_vars (val_env, met_env, par_env, [], inh_vals)
      in
      (* Inherited concrete methods *)
      let inh_meths = 
        Concr.fold (fun lab rem -> (lab, Ident.create lab)::rem)
          cl_sig.cty_concr []
      in
      (* Super *)      
      let (val_env, met_env, par_env) =
        match super with
          None ->
            (val_env, met_env, par_env)
        | Some name ->
            let (id, val_env, met_env, par_env) =
              enter_met_env name (Val_anc (inh_meths, cl_num)) self_type
                val_env met_env par_env
            in
            (val_env, met_env, par_env)
      in
      (val_env, met_env, par_env,
       lazy(Cf_inher (parent, inh_vars, inh_meths))::fields,
       concr_meths, inh_vals)

  | Pcf_val (lab, mut, sexp, loc) ->
      if StringSet.mem lab inh_vals then
        Location.prerr_warning loc (Warnings.Hide_instance_variable lab);
      let exp =
        try type_exp val_env sexp with Ctype.Unify [(ty, _)] ->
          raise(Error(loc, Make_nongen_seltype ty))
      in
      let (id, val_env, met_env, par_env) =
        enter_val cl_num vars lab mut exp.exp_type val_env met_env par_env
      in
      (val_env, met_env, par_env, lazy(Cf_val (lab, id, exp)) :: fields,
       concr_meths, inh_vals)

  | Pcf_virt (lab, priv, sty, loc) ->
      virtual_method val_env meths self_type lab priv sty loc;
      (val_env, met_env, par_env, fields, concr_meths, inh_vals)

  | Pcf_meth (lab, priv, expr, loc)  ->
      let meth_expr = make_method cl_num expr in
      Ctype.raise_nongen_level ();
      let (_, ty) =
        Ctype.filter_self_method val_env lab priv meths self_type
      in
      let meth_type = Ctype.newvar () in
      let (obj_ty, res_ty) = Ctype.filter_arrow val_env meth_type "" in
      Ctype.unify val_env obj_ty self_type;
      Ctype.unify val_env res_ty ty;
      let ty' = type_approx met_env expr in
      begin try Ctype.unify met_env ty' res_ty with Ctype.Unify trace ->
        raise(Typecore.Error(expr.pexp_loc, Expr_type_clash(trace)))
      end;
      Ctype.end_def ();
      let field =
        lazy begin
          Ctype.raise_nongen_level ();
          let texp = type_expect met_env meth_expr meth_type in
          Ctype.end_def ();
          Cf_meth (lab, texp)
        end in
      (val_env, met_env, par_env, field::fields,
       Concr.add lab concr_meths, inh_vals)

  | Pcf_cstr (sty, sty', loc) ->
      type_constraint val_env sty sty' loc;
      (val_env, met_env, par_env, fields, concr_meths, inh_vals)

  | Pcf_let (rec_flag, sdefs, loc) ->
      let (defs, val_env) =
        try
          Typecore.type_let val_env rec_flag sdefs
        with Ctype.Unify [(ty, _)] ->
          raise(Error(loc, Make_nongen_seltype ty))
      in
      let (vals, met_env, par_env) =
        List.fold_right
          (fun id (vals, met_env, par_env) ->
             let expr =
               Typecore.type_exp val_env
                 {pexp_desc = Pexp_ident (Longident.Lident (Ident.name id));
                  pexp_loc = Location.none}
             in
             let desc =
               {val_type = expr.exp_type;
                val_kind = Val_ivar (Immutable, cl_num)}
             in
             let id' = Ident.create (Ident.name id) in
             ((id', expr)
              :: vals,
              Env.add_value id' desc met_env,
              Env.add_value id' desc par_env))
          (let_bound_idents defs)
          ([], met_env, par_env)
      in
      (val_env, met_env, par_env, lazy(Cf_let(rec_flag, defs, vals))::fields,
       concr_meths, inh_vals)

  | Pcf_init expr ->
      let expr = make_method cl_num expr in
      let field =
        lazy begin
          Ctype.raise_nongen_level ();
          let meth_type = Ctype.newvar () in
          let (obj_ty, res_ty) = Ctype.filter_arrow val_env meth_type "" in
          Ctype.unify val_env obj_ty self_type;
          Ctype.unify val_env res_ty (Ctype.instance Predef.type_unit);
          let texp = type_expect met_env expr meth_type in
          Ctype.end_def ();
          Cf_init texp
        end in
      (val_env, met_env, par_env, field::fields, concr_meths, inh_vals)

and class_structure cl_num val_env met_env (spat, str) =
  (* Environment for substructures *)
  let par_env = met_env in

  (* Self binder *)
  let (pat, meths, vars, val_env, meth_env, par_env) =
    type_self_pattern cl_num val_env met_env par_env spat
  in
  let self_type = pat.pat_type in

  (* Check that the binder has a correct type, and introduce a dummy
     method preventing self type from being closed. *)
  let ty = Ctype.newvar () in
  Ctype.unify val_env
      (Ctype.filter_method val_env dummy_method Private ty)
      (Ctype.newty (Ttuple []));
  begin try Ctype.unify val_env self_type ty with
    Ctype.Unify _ ->
      raise(Error(pat.pat_loc, Pattern_type_clash self_type))
  end;

  (* Class fields *)
  let (_, _, _, fields, concr_meths, _) =
    List.fold_left (class_field cl_num self_type meths vars)
      (val_env, meth_env, par_env, [], Concr.empty, StringSet.empty)
      str
  in
  let fields = List.map Lazy.force (List.rev fields) in

  {cl_field = fields;
   cl_meths = Meths.map (function (id, ty) -> id) !meths},

  {cty_self = self_type;
   cty_vars = Vars.map (function (id, mut, ty) -> (mut, ty)) !vars;
   cty_concr = concr_meths }

and class_expr cl_num val_env met_env scl =
  match scl.pcl_desc with
    Pcl_constr (lid, styl) ->
      let (path, decl) =
        try Env.lookup_class lid val_env with Not_found ->
          raise(Error(scl.pcl_loc, Unbound_class lid))
      in
      if Path.same decl.cty_path unbound_class then
        raise(Error(scl.pcl_loc, Unbound_class_2 lid));
      let tyl = List.map (transl_simple_type val_env false) styl in
      let (params, clty) =
        Ctype.instance_class decl.cty_params decl.cty_type
      in
      let clty' = abbreviate_class_type path params clty in
      if List.length params <> List.length styl then
        raise(Error(scl.pcl_loc,
                    Parameter_arity_mismatch (lid, List.length params,
                                                   List.length styl)));
      List.iter2
        (fun sty ty ->
           let ty' = transl_simple_type val_env false sty in
           try Ctype.unify val_env ty' ty with Ctype.Unify trace ->
             raise(Error(sty.ptyp_loc, Parameter_mismatch trace)))
        styl params;
      let cl =        
        {cl_desc = Tclass_ident path;
         cl_loc = scl.pcl_loc;
         cl_type = clty'}
      in
      let (vals, meths, concrs) = extract_constraints clty in
      {cl_desc = Tclass_constraint (cl, vals, meths, concrs);
       cl_loc = scl.pcl_loc;
       cl_type = clty'}
  | Pcl_structure cl_str ->
      let (desc, ty) = class_structure cl_num val_env met_env cl_str in
      {cl_desc = Tclass_structure desc;
       cl_loc = scl.pcl_loc;
       cl_type = Tcty_signature ty}
  | Pcl_fun (l, Some default, spat, sbody) ->
      let loc = default.pexp_loc in
      let scases =
        [{ppat_loc = loc; ppat_desc =
          Ppat_construct(Longident.Lident"Some",
                         Some{ppat_loc = loc; ppat_desc = Ppat_var"*sth*"},
                         false)},
         {pexp_loc = loc; pexp_desc = Pexp_ident(Longident.Lident"*sth*")};
         {ppat_loc = loc; ppat_desc =
          Ppat_construct(Longident.Lident"None", None, false)},
         default] in
      let smatch =
        {pexp_loc = loc; pexp_desc =
         Pexp_match({pexp_loc = loc; pexp_desc =
                     Pexp_ident(Longident.Lident"*opt*")},
                    scases)} in
      let sfun =
        {pcl_loc = scl.pcl_loc; pcl_desc =
         Pcl_fun(l, None, {ppat_loc = loc; ppat_desc = Ppat_var"*opt*"},
                 {pcl_loc = scl.pcl_loc; pcl_desc =
                  Pcl_let(Default, [spat, smatch], sbody)})}
      in
      class_expr cl_num val_env met_env sfun
  | Pcl_fun (l, _, spat, scl') ->
      let (pat, pv, val_env, met_env) =
        Typecore.type_class_arg_pattern cl_num val_env met_env l spat
      in
      let pv =
        List.map
          (function (id, id', ty) ->
            (id,
             Typecore.type_exp val_env
               {pexp_desc = Pexp_ident (Longident.Lident (Ident.name id));
                pexp_loc = Location.none}))
          pv
      in
      let rec all_labeled = function
          Tcty_fun ("", _, _) -> false
        | Tcty_fun (l, _, ty_fun) -> l.[0] <> '?' && all_labeled ty_fun
        | _ -> true
      in
      let partial =
        Parmatch.check_partial val_env pat.pat_loc
          [pat, (* Dummy expression *)
           {exp_desc = Texp_constant (Asttypes.Const_int 1);
            exp_loc = Location.none;
            exp_type = Ctype.none;
            exp_env = Env.empty }] in
      Ctype.raise_nongen_level ();
      let cl = class_expr cl_num val_env met_env scl' in
      Ctype.end_def ();
      if Btype.is_optional l && all_labeled cl.cl_type then
        Location.prerr_warning pat.pat_loc
          (Warnings.Other "This optional argument cannot be erased");
      {cl_desc = Tclass_fun (pat, pv, cl, partial);
       cl_loc = scl.pcl_loc;
       cl_type = Tcty_fun (l, pat.pat_type, cl.cl_type)}
  | Pcl_apply (scl', sargs) ->
      let cl = class_expr cl_num val_env met_env scl' in
      let rec type_args args omitted ty_fun sargs more_sargs =
        match ty_fun with
        | Tcty_fun (l, ty, ty_fun) when sargs <> [] || more_sargs <> [] ->
            let name = Btype.label_name l in
            let sargs, more_sargs, arg =
              if !Clflags.classic && not (Btype.is_optional l) then begin
                match sargs, more_sargs with
                  (l', sarg0)::_, _ ->
                    raise(Error(sarg0.pexp_loc, Apply_wrong_label(l')))
                | _, (l', sarg0)::more_sargs ->
                    if l <> l' && l' <> "" then
                      raise(Error(sarg0.pexp_loc, Apply_wrong_label l'))
                    else ([], more_sargs, Some(type_argument val_env sarg0 ty))
                | _ ->
                    assert false
              end else try
                let (l', sarg0, sargs, more_sargs) =
                  try
                    let (l', sarg0, sargs1, sargs2) =
                      Btype.extract_label name sargs
                    in (l', sarg0, sargs1 @ sargs2, more_sargs)
                  with Not_found ->
                    let (l', sarg0, sargs1, sargs2) =
                      Btype.extract_label name more_sargs
                    in (l', sarg0, sargs @ sargs1, sargs2)
                in
                sargs, more_sargs,
                if Btype.is_optional l' || not (Btype.is_optional l) then
                  Some (type_argument val_env sarg0 ty)
                else
                  let arg = type_argument val_env
                      sarg0 (extract_option_type val_env ty) in
                  Some (option_some arg)
              with Not_found ->
                sargs, more_sargs,
                if Btype.is_optional l &&
                  (List.mem_assoc "" sargs || List.mem_assoc "" more_sargs)
                then
                  Some (option_none ty Location.none)
                else None
            in
            let omitted = if arg = None then (l,ty) :: omitted else omitted in
            type_args (arg::args) omitted ty_fun sargs more_sargs
        | _ ->
            match sargs @ more_sargs with
              (l, sarg0)::_ ->
                if omitted <> [] then
                  raise(Error(sarg0.pexp_loc, Apply_wrong_label l))
                else
                  raise(Error(cl.cl_loc, Cannot_apply cl.cl_type))
            | [] ->
                (List.rev args,
                 List.fold_left
                   (fun ty_fun (l,ty) -> Tcty_fun(l,ty,ty_fun))
                   ty_fun omitted)
      in
      let (args, cty) =
        if !Clflags.classic then
          type_args [] [] cl.cl_type [] sargs
        else
          type_args [] [] cl.cl_type sargs []
      in
      {cl_desc = Tclass_apply (cl, args);
       cl_loc = scl.pcl_loc;
       cl_type = cty}
  | Pcl_let (rec_flag, sdefs, scl') ->
      let (defs, val_env) =
        try
          Typecore.type_let val_env rec_flag sdefs
        with Ctype.Unify [(ty, _)] ->
          raise(Error(scl.pcl_loc, Make_nongen_seltype ty))
      in
      let (vals, met_env) =
        List.fold_right
          (fun id (vals, met_env) ->
             Ctype.begin_def ();
             let expr =
               Typecore.type_exp val_env
                 {pexp_desc = Pexp_ident (Longident.Lident (Ident.name id));
                  pexp_loc = Location.none}
             in
             Ctype.end_def ();
             Ctype.generalize expr.exp_type;
             let desc =
               {val_type = expr.exp_type; val_kind = Val_ivar (Immutable,
                                                               cl_num)}
             in
             let id' = Ident.create (Ident.name id) in
             ((id', expr)
              :: vals,
              Env.add_value id' desc met_env))
          (let_bound_idents defs)
          ([], met_env)
      in
      let cl = class_expr cl_num val_env met_env scl' in
      {cl_desc = Tclass_let (rec_flag, defs, vals, cl);
       cl_loc = scl.pcl_loc;
       cl_type = cl.cl_type}
  | Pcl_constraint (scl', scty) ->
      Ctype.begin_class_def ();
      Typetexp.narrow ();
      let cl = class_expr cl_num val_env met_env scl' in
      Typetexp.widen ();
      Typetexp.narrow ();
      let clty = class_type val_env scty in
      Typetexp.widen ();
      Ctype.end_def ();

      limited_generalize (Ctype.row_variable (Ctype.self_type cl.cl_type))
          cl.cl_type;
      limited_generalize (Ctype.row_variable (Ctype.self_type clty)) clty;

      begin match Includeclass.class_types val_env cl.cl_type clty with
        []    -> ()
      | error -> raise(Error(cl.cl_loc, Class_match_failure error))
      end;
      let (vals, meths, concrs) = extract_constraints clty in
      {cl_desc = Tclass_constraint (cl, vals, meths, concrs);
       cl_loc = scl.pcl_loc;
       cl_type = snd (Ctype.instance_class [] clty)}

(*******************************)

(* Approximate the type of the constructor to allow recursive use *)
(* of optional parameters                                         *)

let var_option = Predef.type_option (Btype.newgenvar ())

let rec approx_declaration cl =
  match cl.pcl_desc with
    Pcl_fun (l, _, _, cl) ->
      let arg =
        if Btype.is_optional l then Ctype.instance var_option
        else Ctype.newvar () in
      Ctype.newty (Tarrow (l, arg, approx_declaration cl))
  | Pcl_let (_, _, cl) ->
      approx_declaration cl
  | Pcl_constraint (cl, _) ->
      approx_declaration cl
  | _ -> Ctype.newvar ()

let rec approx_description ct =
  match ct.pcty_desc with
    Pcty_fun (l, _, ct) ->
      let arg =
        if Btype.is_optional l then Ctype.instance var_option
        else Ctype.newvar () in
      Ctype.newty (Tarrow (l, arg, approx_description ct))
  | _ -> Ctype.newvar ()

(*******************************)

let temp_abbrev env id arity =
  let params = ref [] in
  for i = 1 to arity do
    params := Ctype.newvar () :: !params
  done;
  let ty = Ctype.newobj (Ctype.newvar ()) in
  let env =
    Env.add_type id
      {type_params = !params;
       type_arity = arity;
       type_kind = Type_abstract;
       type_manifest = Some ty }
      env
  in
  (!params, ty, env)

let rec initial_env define_class approx
    (res, env) (cl, id, ty_id, obj_id, cl_id) =
  (* Temporary abbreviations *)
  let arity = List.length (fst cl.pci_params) in
  let (obj_params, obj_ty, env) = temp_abbrev env obj_id arity in
  let (cl_params, cl_ty, env) = temp_abbrev env cl_id arity in
  
  (* Temporary type for the class constructor *)
  let constr_type = approx cl.pci_expr in
  let dummy_cty =
    Tcty_signature
      { cty_self = Ctype.newvar ();
        cty_vars = Vars.empty;
        cty_concr = Concr.empty }
  in
  let dummy_class =
    {cty_params = [];             (* Dummy value *)
     cty_type = dummy_cty;        (* Dummy value *)
     cty_path = unbound_class;
     cty_new =
       match cl.pci_virt with
         Virtual  -> None
       | Concrete -> Some constr_type}
  in
  let env =
    Env.add_cltype ty_id
      {clty_params = [];            (* Dummy value *)
       clty_type = dummy_cty;       (* Dummy value *)
       clty_path = unbound_class} (
    if define_class then
      Env.add_class id dummy_class env
    else
      env)
  in
  ((cl, id, ty_id,
    obj_id, obj_params, obj_ty,
    cl_id, cl_params, cl_ty,
    constr_type, dummy_class)::res,
   env)

let class_infos define_class kind
    (cl, id, ty_id,
     obj_id, obj_params, obj_ty,
     cl_id, cl_params, cl_ty,
     constr_type, dummy_class)
    (res, env) =

  reset_type_variables ();
  Ctype.begin_class_def ();
  
  (* Introduce class parameters *)
  let params =
    try
      List.map (enter_type_variable true) (fst cl.pci_params)
    with Already_bound ->
      raise(Error(snd cl.pci_params, Repeated_parameter))
  in
  
  (* Type the class expression *)
  let (expr, typ) = kind env cl.pci_expr in
  
  Ctype.end_def ();
  
  let sty = Ctype.self_type typ in

  (* Generalize the row variable *)
  let rv = Ctype.row_variable sty in
  List.iter (Ctype.limited_generalize rv) params;
  limited_generalize rv typ;

  (* Check the abbreviation for the object type *)
  let (obj_params', obj_type) = Ctype.instance_class params typ in
  let constr = Ctype.newconstr (Path.Pident obj_id) obj_params in
  begin
    let ty = Ctype.self_type obj_type in
    Ctype.hide_private_methods ty;
    Ctype.close_object ty;
    begin try
      List.iter2 (Ctype.unify env) obj_params obj_params'
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc,
            Bad_parameters (obj_id, constr,
                            Ctype.newconstr (Path.Pident obj_id)
                                            obj_params')))
    end;
    begin try
      Ctype.unify env ty constr
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc,
        Abbrev_type_clash (constr, ty, Ctype.expand_head env constr)))
    end
  end;
  
  (* Check the other temporary abbreviation (#-type) *)
  begin
    let (cl_params', cl_type) = Ctype.instance_class params typ in
    let ty = Ctype.self_type cl_type in
    Ctype.hide_private_methods ty;
    Ctype.set_object_name obj_id (Ctype.row_variable ty) cl_params ty;
    begin try
      List.iter2 (Ctype.unify env) cl_params cl_params'
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc,
            Bad_parameters (cl_id,
                            Ctype.newconstr (Path.Pident cl_id)
                                            cl_params,
                            Ctype.newconstr (Path.Pident cl_id)
                                            cl_params')))
    end;
    begin try
      Ctype.unify env ty cl_ty
    with Ctype.Unify _ ->
      let constr = Ctype.newconstr (Path.Pident cl_id) params in
      raise(Error(cl.pci_loc, Abbrev_type_clash (constr, ty, cl_ty)))
    end
  end;
  
  (* Type of the class constructor *)
  begin try
    Ctype.unify env (constructor_type constr obj_type) constr_type
  with Ctype.Unify trace ->
    raise(Error(cl.pci_loc,
                Constructor_type_mismatch (cl.pci_name, trace)))
  end;

  (* Class and class type temporary definitions *)
  let cltydef =
    {clty_params = params; clty_type = class_body typ;
     clty_path = Path.Pident obj_id}
  and clty =
    {cty_params = params; cty_type = typ;
     cty_path = Path.Pident obj_id;
     cty_new =
       match cl.pci_virt with
         Virtual  -> None
       | Concrete -> Some constr_type}
  in
  dummy_class.cty_type <- typ;
  let env =
    Env.add_cltype ty_id cltydef (
    if define_class then Env.add_class id clty env else env)
  in

  if cl.pci_virt = Concrete then begin
    match virtual_methods typ with
      []   -> ()
    | mets -> raise(Error(cl.pci_loc, Virtual_class(define_class, mets)))
  end;

  (* Misc. *)
  let arity = Ctype.class_type_arity typ in
  let pub_meths =
    let (fields, _) =
      Ctype.flatten_fields (Ctype.object_fields (Ctype.expand_head env obj_ty))
    in
    List.map (function (lab, _, _) -> lab) fields
  in
  
  (* Final definitions *)
  let (params', typ') = Ctype.instance_class params typ in
  let cltydef =
    {clty_params = params'; clty_type = class_body typ';
     clty_path = Path.Pident obj_id}
  and clty =
    {cty_params = params'; cty_type = typ';
     cty_path = Path.Pident obj_id;
     cty_new =
       match cl.pci_virt with
         Virtual  -> None
       | Concrete -> Some constr_type}
  in
  let obj_abbr =
    {type_params = obj_params;
     type_arity = List.length obj_params;
     type_kind = Type_abstract;
     type_manifest = Some obj_ty }
  in
  let (cl_params, cl_ty) =
    Ctype.instance_parameterized_type params (Ctype.self_type typ)
  in
  Ctype.hide_private_methods cl_ty;
  Ctype.set_object_name obj_id (Ctype.row_variable cl_ty) cl_params cl_ty;
  let cl_abbr =
    {type_params = cl_params;
     type_arity = List.length cl_params;
     type_kind = Type_abstract;
     type_manifest = Some cl_ty }
  in
  ((cl, id, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr,
    arity, pub_meths, expr) :: res,
   env)

let final_env define_class
    (cl, id, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr,
     arity, pub_meths, expr)
    (res, env) =

  List.iter Ctype.generalize clty.cty_params;
  generalize_class_type clty.cty_type;
  begin match clty.cty_new with
    None -> ()
  | Some ty -> Ctype.generalize ty
  end;
  List.iter Ctype.generalize obj_abbr.type_params;
  begin match obj_abbr.type_manifest with
    None    -> ()
  | Some ty -> Ctype.generalize ty
  end;
  List.iter Ctype.generalize cl_abbr.type_params;
  begin match cl_abbr.type_manifest with
    None    -> ()
  | Some ty -> Ctype.generalize ty
  end;

  if not (closed_class clty) then
    raise(Error(cl.pci_loc, Non_generalizable_class (id, clty)));

  begin match
    Ctype.closed_class clty.cty_params
      (Ctype.signature_of_class_type clty.cty_type)
  with
    None        -> ()
  | Some reason ->
      let printer =
        if define_class
        then function ppf -> Printtyp.class_declaration id ppf clty
        else function ppf -> Printtyp.cltype_declaration id ppf cltydef
      in
      raise(Error(cl.pci_loc, Unbound_type_var(printer, reason)))
  end;

  let env =
    Env.add_type obj_id obj_abbr (
    Env.add_type cl_id cl_abbr (
    Env.add_cltype ty_id cltydef (
    if define_class then Env.add_class id clty env else env)))
  in
  ((id, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr,
    arity, pub_meths, expr)::res,
   env)

(*******************************)

let type_classes define_class approx kind env cls =
  let cls =
    List.map
      (function cl ->
         (cl,
          Ident.create cl.pci_name, Ident.create cl.pci_name,
          Ident.create cl.pci_name, Ident.create ("#" ^ cl.pci_name)))
      cls
  in
  Ctype.init_def (Ident.current_time ());
  Ctype.begin_class_def ();
  let (res, env) =
    List.fold_left (initial_env define_class approx) ([], env) cls
  in
  let (res, env) =
    List.fold_right (class_infos define_class kind) res ([], env)
  in
  Ctype.end_def ();
  let (res, env) =
    List.fold_right (final_env define_class) res ([], env)
  in
  (List.rev res, env)

let class_num = ref 0
let class_declaration env sexpr =
  incr class_num;
  let expr = class_expr (string_of_int !class_num) env env sexpr in
  (expr, expr.cl_type)

let class_description env sexpr =
  let expr = class_type env sexpr in
  (expr, expr)

let class_declarations env cls =
  type_classes true approx_declaration class_declaration env cls

let class_descriptions env cls =
  type_classes true approx_description class_description env cls

let class_type_declarations env cls =
  let (decl, env) =
    type_classes false approx_description class_description env cls
  in
  (List.map
     (function
          (_, _, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr, _, _, _) ->
        (ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr))
     decl,
   env)

(*******************************)

(* Error report *)

open Format

let report_error ppf = function
  | Repeated_parameter ->
      fprintf ppf "A type parameter occurs several times"
  | Unconsistent_constraint trace ->
      Printtyp.report_unification_error ppf trace
        (function ppf ->
           fprintf ppf "The class constraints are not consistent : type")
        (function ppf ->
           fprintf ppf "is not compatible with type")
  | Method_type_mismatch (m, trace) ->
      Printtyp.report_unification_error ppf trace
        (function ppf ->
           fprintf ppf "The method %s@ has type" m)
        (function ppf ->
           fprintf ppf "but is expected to have type")
  | Structure_expected clty ->
      fprintf ppf
        "@[This class expression is not a class structure; it has type@ %a@]"
        Printtyp.class_type clty
  | Cannot_apply clty ->
      fprintf ppf
        "This class expression is not a class function, it cannot be applied"
  | Apply_wrong_label l ->
      let mark_label = function
        | "" -> "out label"
        |  l -> sprintf " label %s" l in
      fprintf ppf "This argument cannot be applied with%s" (mark_label l)
  | Pattern_type_clash ty ->
      (* XXX Trace *)
      (* XXX Revoir message d'erreur *)
      fprintf ppf "@[This pattern cannot match self: \
                    it only matches values of type@ %a@]"
      Printtyp.type_expr ty
  | Unbound_class cl ->
      fprintf ppf "Unbound class@ %a"
      Printtyp.longident cl
  | Unbound_class_2 cl ->
      fprintf ppf "The class@ %a@ is not yet completely defined"
      Printtyp.longident cl
  | Unbound_class_type cl ->
      fprintf ppf "Unbound class type@ %a"
      Printtyp.longident cl
  | Unbound_class_type_2 cl ->
      fprintf ppf "The class type@ %a@ is not yet completely defined"
      Printtyp.longident cl
  | Abbrev_type_clash (abbrev, actual, expected) ->
      (* XXX Afficher une trace ? *)
      Printtyp.reset_and_mark_loops_list [abbrev; actual; expected];
      fprintf ppf "@[The abbreviation@ %a@ expands to type@ %a@ \
       but is used with type@ %a@]"
       Printtyp.type_expr abbrev
       Printtyp.type_expr actual
       Printtyp.type_expr expected
  | Constructor_type_mismatch (c, trace) ->
      Printtyp.report_unification_error ppf trace
        (function ppf ->
           fprintf ppf "The expression \"new %s\" has type" c)
        (function ppf ->
           fprintf ppf "but is used with type")
  | Virtual_class (cl, mets) ->
      let print_mets ppf mets =
        List.iter (function met -> fprintf ppf "@ %s" met) mets in
      let cl_mark = if cl then " type" else "" in
      fprintf ppf
        "@[This class %s should be virtual@ \
           @[<2>The following methods are undefined :%a@]
         @]"
        cl_mark print_mets mets
  | Parameter_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
        "@[The class constructor %a@ expects %i type argument(s),@ \
           but is here applied to %i type argument(s)@]"
        Printtyp.longident lid expected provided
  | Parameter_mismatch trace ->
      Printtyp.report_unification_error ppf trace
        (function ppf ->
           fprintf ppf "The type parameter")
        (function ppf ->
           fprintf ppf "does not meet its constraint: it should be")
  | Bad_parameters (id, params, cstrs) ->
      Printtyp.reset_and_mark_loops_list [params; cstrs];
      fprintf ppf
        "@[The abbreviation %a@ is used with parameters@ %a@ \
           wich are incompatible with constraints@ %a@]"
        Printtyp.ident id Printtyp.type_expr params Printtyp.type_expr cstrs
  | Class_match_failure error ->
      Includeclass.report_error ppf error
  | Unbound_val lab ->
      fprintf ppf "Unbound instance variable %s" lab
  | Unbound_type_var (printer, reason) ->
      let print_labty real ppf ty =
        if real then Printtyp.type_expr ppf ty else fprintf ppf ".." in
      let print_reason ppf = function
      | Ctype.CC_Method (ty0, real, lab, ty) ->
          Printtyp.reset_and_mark_loops_list [ty; ty0];
          fprintf ppf
            "The method %s@ has type@;<1 2>%a@ where@ %a@ is unbound"
            lab Printtyp.type_expr ty (print_labty real) ty0
      | Ctype.CC_Value (ty0, real, lab, ty) ->
          Printtyp.reset_and_mark_loops_list [ty; ty0];
          fprintf ppf
            "The instance variable %s@ has type@;<1 2>%a@ \
             where@ %a@ is unbound"
            lab Printtyp.type_expr ty (print_labty real) ty0
      in
      Printtyp.reset ();
      fprintf ppf
        "@[<v>@[Some type variables are unbound in this type:@;<1 2>%t@]@ \
              @[%a@]@]"
       printer print_reason reason
  | Make_nongen_seltype ty ->
      fprintf ppf
        "@[<v>@[Self type should not occur in the non-generic type@;<1 2>\
                %a@]@,\
           It would escape the scope of its class@]"
        Printtyp.type_scheme ty
  | Non_generalizable_class (id, clty) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains type variables that cannot be generalized@]"
        (Printtyp.class_declaration id) clty
