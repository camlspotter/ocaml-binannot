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

(* Typechecking for the core language *)

open Misc
open Asttypes
open Parsetree
open Types
open Typedtree
open Btype
open Ctype

type error =
    Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of Longident.t * (type_expr * type_expr) list
  | Pattern_type_clash of (type_expr * type_expr) list
  | Multiply_bound_variable
  | Orpat_vars of Ident.t
  | Expr_type_clash of (type_expr * type_expr) list
  | Apply_non_function of type_expr
  | Apply_wrong_label of label * type_expr
  | Label_multiply_defined of Longident.t
  | Label_missing of string list
  | Label_not_mutable of Longident.t
  | Bad_format of string
  | Undefined_method of type_expr * string
  | Undefined_inherited_method of string
  | Unbound_class of Longident.t
  | Virtual_class of Longident.t
  | Private_type of string
  | Private_type_setfield of Longident.t * string
  | Unbound_instance_variable of string
  | Instance_variable_not_mutable of string
  | Not_subtype of (type_expr * type_expr) list * (type_expr * type_expr) list
  | Outside_class
  | Value_multiply_overridden of string
  | Coercion_failure of
      type_expr * type_expr * (type_expr * type_expr) list * bool
  | Too_many_arguments of bool * type_expr
  | Abstract_wrong_label of label * type_expr
  | Scoping_let_module of string * type_expr
  | Masked_instance_variable of Longident.t
  | Not_a_variant_type of Longident.t
  | Incoherent_label_order
  | Less_general of string * (type_expr * type_expr) list

exception Error of Location.t * error

(* Forward declaration, to be filled in by Typemod.type_module *)

let type_module =
  ref ((fun env md -> assert false) :
       Env.t -> Parsetree.module_expr -> Typedtree.module_expr)


(*
  Saving and outputting type information.
  We keep these function names short, because they have to be
  called each time we create a record of type [Typedtree.expression]
  or [Typedtree.pattern] that will end up in the typed AST.
*)
let re node =
  Stypes.record (Stypes.Ti_expr node);
  node
;;
let rp node =
  Stypes.record (Stypes.Ti_pat node);
  node
;;


(* Typing of constants *)

let type_constant = function
    Const_int _ -> instance Predef.type_int
  | Const_char _ -> instance Predef.type_char
  | Const_string _ -> instance Predef.type_string
  | Const_float _ -> instance Predef.type_float
  | Const_int32 _ -> instance Predef.type_int32
  | Const_int64 _ -> instance Predef.type_int64
  | Const_nativeint _ -> instance Predef.type_nativeint
  
(* Specific version of type_option, using newty rather than newgenty *)

let type_option ty =
  newty (Tconstr(Predef.path_option,[ty], ref Mnil))

let option_none ty loc =
  let cnone = Env.lookup_constructor (Longident.Lident "None") Env.initial in
  { exp_desc = Texp_construct(cnone, []);
    exp_type = ty; exp_loc = loc; exp_env = Env.initial }

let option_some texp =
  let csome = Env.lookup_constructor (Longident.Lident "Some") Env.initial in
  { exp_desc = Texp_construct(csome, [texp]); exp_loc = texp.exp_loc;
    exp_type = type_option texp.exp_type; exp_env = texp.exp_env }

let extract_option_type env ty =
  match expand_head env ty with {desc = Tconstr(path, [ty], _)}
    when Path.same path Predef.path_option -> ty
  | _ -> assert false

let rec extract_label_names sexp env ty =
  let ty = repr ty in
  match ty.desc with
  | Tconstr (path, _, _) ->
      let td = Env.find_type path env in
      let rec extract = function
      | Type_record (fields, _) -> List.map (fun (name, _, _) -> name) fields
      | Type_abstract when td.type_manifest <> None ->
          extract_label_names sexp env (expand_head env ty)
      | Type_private tkind ->
          raise (Error(sexp.pexp_loc, Private_type (Path.name path)))
      | _ -> assert false in
      extract td.type_kind
  | _ ->
      assert false

let check_private get_exc loc env ty =
  let ty = repr ty in
  match ty.desc with
  | Tconstr (path, _, _) ->
      let td = Env.find_type path env in
      begin match td.type_kind with
      | Type_private tkind ->
          raise (Error(loc, get_exc (Path.name path)))
      | _ -> () end
  | _ ->
      assert false

let check_private_type = check_private (fun s -> Private_type s)
let check_private_type_setfield lid =
  check_private (fun s -> Private_type_setfield (lid, s))

(* Typing of patterns *)

(* Creating new conjunctive types is not allowed when typing patterns *)
let unify_pat env pat expected_ty =
  try
    unify env pat.pat_type expected_ty
  with
    Unify trace ->
      raise(Error(pat.pat_loc, Pattern_type_clash(trace)))
  | Tags(l1,l2) ->
      raise(Typetexp.Error(pat.pat_loc, Typetexp.Variant_tags (l1, l2)))

let pattern_variables = ref ([]: (Ident.t * type_expr) list)
let pattern_force = ref ([] : (unit -> unit) list)
let reset_pattern () =
  pattern_variables := [];
  pattern_force := []

let enter_variable loc name ty =
  if List.exists (fun (id, _) -> Ident.name id = name) !pattern_variables
  then raise(Error(loc, Multiply_bound_variable));
  let id = Ident.create name in
  pattern_variables := (id, ty) :: !pattern_variables;
  id

let sort_pattern_variables vs =
  List.sort
    (fun (x,_) (y,_) -> Pervasives.compare (Ident.name x) (Ident.name y))
    vs

let enter_orpat_variables loc env  p1_vs p2_vs =
  (* unify_vars operate on sorted lists *)
  
  let p1_vs = sort_pattern_variables p1_vs
  and p2_vs = sort_pattern_variables p2_vs in

  let rec unify_vars p1_vs p2_vs = match p1_vs, p2_vs with  
      | (x1,t1)::rem1, (x2,t2)::rem2 when Ident.equal x1 x2 ->
          if x1==x2 then
            unify_vars rem1 rem2
          else begin
            begin try
              unify env t1 t2
            with
            | Unify trace ->
                raise(Error(loc, Pattern_type_clash(trace)))
            end ;
          (x2,x1)::unify_vars rem1 rem2
          end
      | [],[] -> []
      | (x,_)::_, [] -> raise (Error (loc, Orpat_vars x))
      | [],(x,_)::_  -> raise (Error (loc, Orpat_vars x))
      | (x,_)::_, (y,_)::_ ->
          let min_var =
            if Ident.name x < Ident.name y then x
            else y in
          raise (Error (loc, Orpat_vars min_var)) in
  unify_vars p1_vs p2_vs


let rec build_as_type env p =
  match p.pat_desc with
    Tpat_alias(p1, _) -> build_as_type env p1
  | Tpat_tuple pl ->
      let tyl = List.map (build_as_type env) pl in
      newty (Ttuple tyl)
  | Tpat_construct(cstr, pl) ->
      let tyl = List.map (build_as_type env) pl in
      let ty_args, ty_res = instance_constructor cstr in
      List.iter2 (fun (p,ty) -> unify_pat env {p with pat_type = ty})
        (List.combine pl tyl) ty_args;
      ty_res
  | Tpat_variant(l, p', _) ->
      let ty = may_map (build_as_type env) p' in
      newty (Tvariant{row_fields=[l, Rpresent ty]; row_more=newvar();
                      row_bound=[]; row_name=None;
                      row_fixed=false; row_closed=false})
  | Tpat_record lpl ->
      let lbl = fst(List.hd lpl) in
      let ty = newvar () in
      let do_label lbl =
        let _, ty_arg, ty_res = instance_label false lbl in
        unify_pat env {p with pat_type = ty} ty_res;
        if lbl.lbl_mut = Immutable && List.mem_assoc lbl lpl then begin
          let arg = List.assoc lbl lpl in
          unify_pat env {arg with pat_type = build_as_type env arg} ty_arg
        end else begin
          let _, ty_arg', ty_res' = instance_label false lbl in
          unify env ty_arg ty_arg';
          unify_pat env p ty_res'
        end in
      Array.iter do_label lbl.lbl_all;
      ty
  | Tpat_or(p1, p2, path) ->
      let ty1 = build_as_type env p1 and ty2 = build_as_type env p2 in
      unify_pat env {p2 with pat_type = ty2} ty1;
      begin match path with None -> ()
      | Some path ->
          let td = try Env.find_type path env with Not_found -> assert false in
          let params = List.map (fun _ -> newvar()) td.type_params in
          match expand_head env (newty (Tconstr (path, params, ref Mnil)))
          with {desc=Tvariant row} when static_row row ->
            unify_pat env {p1 with pat_type = ty1}
              (newty (Tvariant{row with row_closed=false; row_more=newvar()}))
          | _ -> ()
      end;
      ty1
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_array _ -> p.pat_type

let build_or_pat env loc lid =
  let path, decl =
    try Env.lookup_type lid env
    with Not_found ->
      raise(Typetexp.Error(loc, Typetexp.Unbound_type_constructor lid))
  in
  let tyl = List.map (fun _ -> newvar()) decl.type_params in
  let fields =
    let ty = expand_head env (newty(Tconstr(path, tyl, ref Mnil))) in
    match ty.desc with
      Tvariant row when static_row row ->
        (row_repr row).row_fields
    | _ -> raise(Error(loc, Not_a_variant_type lid))
  in
  let bound = ref [] in
  let pats, fields =
    List.fold_left
      (fun (pats,fields) (l,f) ->
        match row_field_repr f with
          Rpresent None ->
            (l,None) :: pats,
            (l, Reither(true,[], true, ref None)) :: fields
        | Rpresent (Some ty) ->
            bound := ty :: !bound;
            (l, Some {pat_desc=Tpat_any; pat_loc=Location.none; pat_env=env;
                      pat_type=ty})
            :: pats,
            (l, Reither(false, [ty], true, ref None)) :: fields
        | _ -> pats, fields)
      ([],[]) fields in
  let row =
    { row_fields = List.rev fields; row_more = newvar(); row_bound = !bound;
      row_closed = false; row_fixed = false; row_name = Some (path, tyl) }
  in
  let ty = newty (Tvariant row) in
  let gloc = {loc with Location.loc_ghost=true} in
  let pats =
    List.map (fun (l,p) -> {pat_desc=Tpat_variant(l,p,row); pat_loc=gloc;
                            pat_env=env; pat_type=ty})
      pats
  in
  match pats with
    [] -> raise(Error(loc, Not_a_variant_type lid))
  | pat :: pats ->
      let r =
        List.fold_left
          (fun pat pat0 -> {pat_desc=Tpat_or(pat0,pat,Some path);
                            pat_loc=gloc; pat_env=env; pat_type=ty})
          pat pats in
      rp { r with pat_loc = loc }

let rec type_pat env sp =
  match sp.ppat_desc with
    Ppat_any ->
      rp {
        pat_desc = Tpat_any;
        pat_loc = sp.ppat_loc;
        pat_type = newvar();
        pat_env = env }
  | Ppat_var name ->
      let ty = newvar() in
      let id = enter_variable sp.ppat_loc name ty in
      rp {
        pat_desc = Tpat_var id;
        pat_loc = sp.ppat_loc;
        pat_type = ty;
        pat_env = env }
  | Ppat_alias(sq, name) ->
      let q = type_pat env sq in
      begin_def ();
      let ty_var = build_as_type env q in
      end_def ();
      generalize ty_var;
      let id = enter_variable sp.ppat_loc name ty_var in
      rp {
        pat_desc = Tpat_alias(q, id);
        pat_loc = sp.ppat_loc;
        pat_type = q.pat_type;
        pat_env = env }
  | Ppat_constant cst ->
      rp {
        pat_desc = Tpat_constant cst;
        pat_loc = sp.ppat_loc;
        pat_type = type_constant cst;
        pat_env = env }
  | Ppat_tuple spl ->
      let pl = List.map (type_pat env) spl in
      rp {
        pat_desc = Tpat_tuple pl;
        pat_loc = sp.ppat_loc;
        pat_type = newty (Ttuple(List.map (fun p -> p.pat_type) pl));
        pat_env = env }
  | Ppat_construct(lid, sarg, explicit_arity) ->
      let constr =
        try
          Env.lookup_constructor lid env
        with Not_found ->
          raise(Error(sp.ppat_loc, Unbound_constructor lid)) in
      let sargs =
        match sarg with
          None -> []
        | Some {ppat_desc = Ppat_tuple spl} when explicit_arity -> spl
        | Some {ppat_desc = Ppat_tuple spl} when constr.cstr_arity > 1 -> spl
        | Some({ppat_desc = Ppat_any} as sp) when constr.cstr_arity > 1 ->
            replicate_list sp constr.cstr_arity
        | Some sp -> [sp] in
      if List.length sargs <> constr.cstr_arity then
        raise(Error(sp.ppat_loc, Constructor_arity_mismatch(lid,
                                     constr.cstr_arity, List.length sargs)));
      let args = List.map (type_pat env) sargs in
      let (ty_args, ty_res) = instance_constructor constr in
      List.iter2 (unify_pat env) args ty_args;
      rp {
        pat_desc = Tpat_construct(constr, args);
        pat_loc = sp.ppat_loc;
        pat_type = ty_res;
        pat_env = env }
  | Ppat_variant(l, sarg) ->
      let arg = may_map (type_pat env) sarg in
      let arg_type = match arg with None -> [] | Some arg -> [arg.pat_type]  in
      let row = { row_fields =
                    [l, Reither(arg = None, arg_type, true, ref None)];
                  row_bound = arg_type;
                  row_closed = false;
                  row_more = newvar ();
                  row_fixed = false;
                  row_name = None } in
      rp {
        pat_desc = Tpat_variant(l, arg, row);
        pat_loc = sp.ppat_loc;
        pat_type = newty (Tvariant row);
        pat_env = env }
  | Ppat_record lid_sp_list ->
      let rec check_duplicates = function
        [] -> ()
      | (lid, sarg) :: remainder ->
          if List.mem_assoc lid remainder
          then raise(Error(sp.ppat_loc, Label_multiply_defined lid))
          else check_duplicates remainder in
      check_duplicates lid_sp_list;
      let ty = newvar() in
      let type_label_pat (lid, sarg) =
        let label =
          try
            Env.lookup_label lid env
          with Not_found ->
            raise(Error(sp.ppat_loc, Unbound_label lid)) in
        let (_, ty_arg, ty_res) = instance_label false label in
        begin try
          unify env ty_res ty
        with Unify trace ->
          raise(Error(sp.ppat_loc, Label_mismatch(lid, trace)))
        end;
        let arg = type_pat env sarg in
        unify_pat env arg ty_arg;
        (label, arg)
      in
      rp {
        pat_desc = Tpat_record(List.map type_label_pat lid_sp_list);
        pat_loc = sp.ppat_loc;
        pat_type = ty;
        pat_env = env }
  | Ppat_array spl ->
      let pl = List.map (type_pat env) spl in
      let ty_elt = newvar() in
      List.iter (fun p -> unify_pat env p ty_elt) pl;
      rp {
        pat_desc = Tpat_array pl;
        pat_loc = sp.ppat_loc;
        pat_type = instance (Predef.type_array ty_elt);
        pat_env = env }
  | Ppat_or(sp1, sp2) ->
      let initial_pattern_variables = !pattern_variables in
      let p1 = type_pat env sp1 in
      let p1_variables = !pattern_variables in
      pattern_variables := initial_pattern_variables ;
      let p2 = type_pat env sp2 in
      let p2_variables = !pattern_variables in
      unify_pat env p2 p1.pat_type;
      let alpha_env =
        enter_orpat_variables sp.ppat_loc env p1_variables p2_variables in
      pattern_variables := p1_variables ;
      rp {
        pat_desc = Tpat_or(p1, alpha_pat alpha_env p2, None);
        pat_loc = sp.ppat_loc;
        pat_type = p1.pat_type;
        pat_env = env }
  | Ppat_constraint(sp, sty) ->
      let p = type_pat env sp in
      let ty, force = Typetexp.transl_simple_type_delayed env sty in
      unify_pat env p ty;
      pattern_force := force :: !pattern_force;
      p
  | Ppat_type lid ->
      build_or_pat env sp.ppat_loc lid

let get_ref r =
  let v = !r in r := []; v

let add_pattern_variables env =
  let pv = get_ref pattern_variables in
  List.fold_right
    (fun (id, ty) env ->
       Env.add_value id {val_type = ty; val_kind = Val_reg} env)
    pv env

let type_pattern env spat =
  reset_pattern ();
  let pat = type_pat env spat in
  let new_env = add_pattern_variables env in
  (pat, new_env, get_ref pattern_force)

let type_pattern_list env spatl =
  reset_pattern ();
  let patl = List.map (type_pat env) spatl in
  let new_env = add_pattern_variables env in
  (patl, new_env, get_ref pattern_force)

let type_class_arg_pattern cl_num val_env met_env l spat =
  reset_pattern ();
  let pat = type_pat val_env spat in
  List.iter (fun f -> f()) (get_ref pattern_force);
  if is_optional l then unify_pat val_env pat (type_option (newvar ()));
  let (pv, met_env) =
    List.fold_right
      (fun (id, ty) (pv, env) ->
         let id' = Ident.create (Ident.name id) in
         ((id', id, ty)::pv,
          Env.add_value id' {val_type = ty;
                             val_kind = Val_ivar (Immutable, cl_num)}
            env))
      !pattern_variables ([], met_env)
  in
  let val_env = add_pattern_variables val_env in
  (pat, pv, val_env, met_env)

let mkpat d = { ppat_desc = d; ppat_loc = Location.none }
let type_self_pattern cl_num val_env met_env par_env spat =
  let spat = 
    mkpat (Ppat_alias (mkpat(Ppat_alias (spat, "selfpat-*")),
                       "selfpat-" ^ cl_num))
  in
  reset_pattern ();
  let pat = type_pat val_env spat in
  List.iter (fun f -> f()) (get_ref pattern_force);
  let meths = ref Meths.empty in
  let vars = ref Vars.empty in
  let pv = !pattern_variables in
  pattern_variables := [];
  let (val_env, met_env, par_env) =
    List.fold_right
      (fun (id, ty) (val_env, met_env, par_env) ->
         (Env.add_value id {val_type = ty; val_kind = Val_unbound} val_env,
          Env.add_value id {val_type = ty;
                            val_kind = Val_self (meths, vars, cl_num)}
            met_env,
          Env.add_value id {val_type = ty; val_kind = Val_unbound} par_env))
      pv (val_env, met_env, par_env)
  in
  (pat, meths, vars, val_env, met_env, par_env)

let delayed_checks = ref []
let reset_delayed_checks () = delayed_checks := []
let add_delayed_check f = delayed_checks := f :: !delayed_checks
let force_delayed_checks () =
  List.iter (fun f -> f ()) (List.rev !delayed_checks);
  reset_delayed_checks ()

let finalize_variant pat =
  match pat.pat_desc with
    Tpat_variant(tag, opat, row) ->
      let row = row_repr row in
      let field =
        try row_field_repr (List.assoc tag row.row_fields)
        with Not_found -> Rabsent
      in
      begin match field with
      | Rabsent -> assert false
      | Reither (true, [], _, e) when not row.row_closed ->
          set_row_field e (Rpresent None)
      | Reither (false, ty::tl, _, e) when not row.row_closed ->
          set_row_field e (Rpresent (Some ty));
          begin match opat with None -> assert false
          | Some pat -> List.iter (unify_pat pat.pat_env pat) (ty::tl)
          end
      | Reither (c, l, true, e) when not row.row_fixed ->
          set_row_field e (Reither (c, [], false, ref None))
      | _ -> ()
      end;
      (* Force check of well-formedness *)
      unify_pat pat.pat_env pat
        (newty(Tvariant{row_fields=[]; row_more=newvar(); row_closed=false;
                        row_bound=[]; row_fixed=false; row_name=None}));
(*
      (* Eventually post a delayed warning check *)
      if (match row_field_repr field with Reither _ -> true | _ -> false) then
        add_delayed_check
          (fun () -> if row_field_repr field = Rabsent then
            Location.prerr_warning pat.pat_loc Warnings.Unused_match)
*)
  | _ -> ()

let rec iter_pattern f p =
  f p;
  match p.pat_desc with
    Tpat_any | Tpat_var _ | Tpat_constant _ ->
      ()
  | Tpat_alias (p, _) ->
      iter_pattern f p
  | Tpat_tuple pl ->
      List.iter (iter_pattern f) pl
  | Tpat_construct (_, pl) ->
      List.iter (iter_pattern f) pl
  | Tpat_variant (_, p, _) ->
      may (iter_pattern f) p
  | Tpat_record fl ->
      List.iter (fun (_, p) -> iter_pattern f p) fl
  | Tpat_or (p, p', _) ->
      iter_pattern f p;
      iter_pattern f p'
  | Tpat_array pl ->
      List.iter (iter_pattern f) pl

(* Generalization criterion for expressions *)

let rec is_nonexpansive exp =
  match exp.exp_desc with
    Texp_ident(_,_) -> true
  | Texp_constant _ -> true
  | Texp_let(rec_flag, pat_exp_list, body) ->
      List.for_all (fun (pat, exp) -> is_nonexpansive exp) pat_exp_list &&
      is_nonexpansive body
  | Texp_function _ -> true
  | Texp_apply(e, (None,_)::el) ->
      is_nonexpansive e && List.for_all is_nonexpansive_opt (List.map fst el)
  | Texp_tuple el ->
      List.for_all is_nonexpansive el
  | Texp_construct(_, el) ->
      List.for_all is_nonexpansive el
  | Texp_variant(_, arg) -> is_nonexpansive_opt arg
  | Texp_record(lbl_exp_list, opt_init_exp) ->
      List.for_all
        (fun (lbl, exp) -> lbl.lbl_mut = Immutable && is_nonexpansive exp)
        lbl_exp_list 
      && is_nonexpansive_opt opt_init_exp
  | Texp_field(exp, lbl) -> is_nonexpansive exp
  | Texp_array [] -> true
  | Texp_ifthenelse(cond, ifso, ifnot) ->
      is_nonexpansive ifso && is_nonexpansive_opt ifnot
  | Texp_new (_, cl_decl) when Ctype.class_type_arity cl_decl.cty_type > 0 ->
      true
  | Texp_lazy e -> true
  | _ -> false

and is_nonexpansive_opt = function
    None -> true
  | Some e -> is_nonexpansive e

(* Typing of printf formats.  
   (Handling of * modifiers contributed by Thorsten Ohl.) *)

let type_format loc fmt =
  let len = String.length fmt in
  let ty_input = newvar()
  and ty_result = newvar()
  and ty_aresult = newvar () in
  let ty_arrow gty ty = newty (Tarrow("", instance gty, ty, Cok)) in
  let incomplete i =
    raise (Error (loc, Bad_format (String.sub fmt i (len - i)))) in
  let rec scan_format i =
    if i >= len then ty_aresult, ty_result else
    match fmt.[i] with
    | '%' -> scan_flags i (i + 1)
    | _ -> scan_format (i + 1)
  and scan_flags i j =
    if j >= len then incomplete i else
    match fmt.[j] with
    | '#' | '0' | '-' | ' ' | '+' -> scan_flags i (j + 1)
    | _ -> scan_skip i j
  and scan_skip i j =
    if j >= len then incomplete i else
      match fmt.[j] with
      | '_' -> scan_rest true i j
      | _ -> scan_rest false i j
  and scan_rest skip i j =
    let rec scan_width i j =
      if j >= len then incomplete i else
      match fmt.[j] with
      | '*' ->
          let ty_aresult, ty_result = scan_dot i (j + 1) in
          ty_aresult, ty_arrow Predef.type_int ty_result
      | '_' -> scan_fixed_width i (j + 1)
      | '.' -> scan_precision i (j + 1)
      | _ -> scan_fixed_width i j
    and scan_fixed_width i j =
      if j >= len then incomplete i else
      match fmt.[j] with
      | '0' .. '9' | '-' | '+' -> scan_fixed_width i (j + 1)
      | '.' -> scan_precision i (j + 1)
      | _ -> scan_conversion i j
    and scan_dot i j =
      if j >= len then incomplete i else
      match fmt.[j] with
      | '.' -> scan_precision i (j + 1)
      | _ -> scan_conversion i j
    and scan_precision i j =
      if j >= len then incomplete i else
      match fmt.[j] with
      | '*' ->
          let ty_aresult, ty_result = scan_conversion i (j + 1) in
          ty_aresult, ty_arrow Predef.type_int ty_result
      | _ -> scan_fixed_precision i j
    and scan_fixed_precision i j =
      if j >= len then incomplete i else
      match fmt.[j] with
      | '0' .. '9' | '-' | '+' -> scan_fixed_precision i (j + 1)
      | _ -> scan_conversion i j

    and conversion j ty_arg =
      let ty_aresult, ty_result = scan_format (j + 1) in
      ty_aresult,
      if skip then ty_result else ty_arrow ty_arg ty_result

    and scan_conversion i j =
      if j >= len then incomplete i else
      match fmt.[j] with
      | '%' | '!' -> scan_format (j + 1)
      | 's' | 'S' | '[' -> conversion j Predef.type_string
      | 'c' | 'C' -> conversion j Predef.type_char
      | 'b' | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' ->
          conversion j Predef.type_int
      | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' -> conversion j Predef.type_float
      | 'B' -> conversion j Predef.type_bool
      | 'a' ->
          let ty_arg = newvar () in
          let ty_a = ty_arrow ty_input (ty_arrow ty_arg ty_aresult) in 
          let ty_aresult, ty_result = conversion j ty_arg in
          ty_aresult, ty_arrow ty_a ty_result
      | '$' ->
          let ty_arg = Predef.type_string in
          let ty_f = ty_arrow Predef.type_string Predef.type_string in 
          let ty_aresult, ty_result = conversion j ty_arg in
          ty_aresult, ty_arrow ty_f ty_result
      | 'r' ->
          let ty_res = newvar() in
          let ty_r = ty_arrow ty_input ty_res in
          let ty_aresult, ty_result = conversion j ty_res in
          ty_arrow ty_r ty_aresult, ty_result
      | 't' -> conversion j (ty_arrow ty_input ty_aresult)
      | 'n' when j + 1 = len -> conversion j Predef.type_int
      | 'l' | 'n' | 'L' as conv ->
          let j = j + 1 in
          if j >= len then incomplete i else begin
            match fmt.[j] with
            | 'b' | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
                let ty_arg =
                 match conv with
                 | 'l' -> Predef.type_int32
                 | 'n' -> Predef.type_nativeint
                 | _ -> Predef.type_int64 in
                conversion j ty_arg
            | c ->
               if conv = 'n' then conversion (j - 1) Predef.type_int else
               raise(Error(loc, Bad_format(String.sub fmt i (j - i))))
          end
      | c ->
          raise(Error(loc, Bad_format(String.sub fmt i (j - i + 1)))) in
    scan_width i j in

  let ty_ares, ty_res = scan_format 0 in
  newty
    (Tconstr(Predef.path_format,
             [ty_res; ty_input; ty_ares; ty_result],
             ref Mnil))

(* Approximate the type of an expression, for better recursion *)

let rec approx_type sty =
  match sty.ptyp_desc with
    Ptyp_arrow (p, _, sty) ->
      let ty1 = if is_optional p then type_option (newvar ()) else newvar () in
      newty (Tarrow (p, ty1, approx_type sty, Cok))
  | _ -> newvar ()

let rec type_approx env sexp =
  match sexp.pexp_desc with
    Pexp_let (_, _, e) -> type_approx env e
  | Pexp_function (p,_,(_,e)::_) when is_optional p ->
       newty (Tarrow(p, type_option (newvar ()), type_approx env e, Cok))
  | Pexp_function (p,_,(_,e)::_) ->
       newty (Tarrow(p, newvar (), type_approx env e, Cok))
  | Pexp_match (_, (_,e)::_) -> type_approx env e
  | Pexp_try (e, _) -> type_approx env e
  | Pexp_tuple l -> newty (Ttuple(List.map (type_approx env) l))
  | Pexp_ifthenelse (_,e,_) -> type_approx env e
  | Pexp_sequence (_,e) -> type_approx env e
  | Pexp_constraint (e, sty1, sty2) ->
      let ty = type_approx env e
      and ty1 = match sty1 with None -> newvar () | Some sty -> approx_type sty
      and ty2 = match sty2 with None -> newvar () | Some sty -> approx_type sty
      in begin
        try unify env ty ty1; unify env ty1 ty2; ty2
        with Unify trace ->
          raise(Error(sexp.pexp_loc, Expr_type_clash trace))
      end
  | _ -> newvar ()

(* List labels in a function type, and whether return type is a variable *)
let rec list_labels_aux env visited ls ty_fun =
  let ty = expand_head env ty_fun in
  if List.memq ty visited then
    List.rev ls, false
  else match ty.desc with
    Tarrow (l, _, ty_res, _) ->
      list_labels_aux env (ty::visited) (l::ls) ty_res
  | _ ->
      List.rev ls, ty.desc = Tvar

let list_labels env ty = list_labels_aux env [] [] ty

(* Check that all univars are safe in a type *)
let check_univars env kind exp ty_expected vars =
  let vars' =
    List.filter
      (fun t ->
        let t = repr t in
        generalize t;
        if t.desc = Tvar && t.level = generic_level then
          (log_type t; t.desc <- Tunivar; true)
        else false)
      vars in
  if List.length vars = List.length vars' then () else
  let ty = newgenty (Tpoly(repr exp.exp_type, vars'))
  and ty_expected = repr ty_expected in
  raise (Error (exp.exp_loc,
                Less_general(kind, [ty, ty; ty_expected, ty_expected])))

(* Check that a type is not a function *)
let check_partial_application env exp =
  match expand_head env exp.exp_type with
  | {desc = Tarrow _} ->
      Location.prerr_warning exp.exp_loc Warnings.Partial_application
  | _ -> ()

(* Hack to allow coercion of self. Will clean-up later. *)
let self_coercion = ref ([] : (Path.t * Location.t list ref) list)

(* Typing of expressions *)

let unify_exp env exp expected_ty =
  try
    unify env exp.exp_type expected_ty
  with
    Unify trace ->
      raise(Error(exp.exp_loc, Expr_type_clash(trace)))
  | Tags(l1,l2) ->
      raise(Typetexp.Error(exp.exp_loc, Typetexp.Variant_tags (l1, l2)))

let rec type_exp env sexp =
  match sexp.pexp_desc with
    Pexp_ident lid ->
      begin try
        let (path, desc) = Env.lookup_value lid env in
        re {
          exp_desc =
            begin match desc.val_kind with
              Val_ivar (_, cl_num) ->
                let (self_path, _) =
                  Env.lookup_value (Longident.Lident ("self-" ^ cl_num)) env
                in
                Texp_instvar(self_path, path)
            | Val_self (_, _, cl_num) ->
                let (path, _) =
                  Env.lookup_value (Longident.Lident ("self-" ^ cl_num)) env
                in
                Texp_ident(path, desc)
            | Val_unbound ->
                raise(Error(sexp.pexp_loc, Masked_instance_variable lid))
            | _ ->
                Texp_ident(path, desc)
            end;
          exp_loc = sexp.pexp_loc;
          exp_type = instance desc.val_type;
          exp_env = env }
      with Not_found ->
        raise(Error(sexp.pexp_loc, Unbound_value lid))
      end
  | Pexp_constant cst ->
      re {
        exp_desc = Texp_constant cst;
        exp_loc = sexp.pexp_loc;
        exp_type = type_constant cst;
        exp_env = env }
  | Pexp_let(rec_flag, spat_sexp_list, sbody) ->
      let (pat_exp_list, new_env) = type_let env rec_flag spat_sexp_list in
      let body = type_exp new_env sbody in
      re {
        exp_desc = Texp_let(rec_flag, pat_exp_list, body);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type;
        exp_env = env }
  | Pexp_function _ ->     (* defined in type_expect *)
      type_expect env sexp (newvar())
  | Pexp_apply(sfunct, sargs) ->
      if !Clflags.principal then begin_def ();
      let funct = type_exp env sfunct in
      if !Clflags.principal then begin
        end_def ();
        generalize_structure funct.exp_type
      end;
      let (args, ty_res) = type_application env funct sargs in
      let funct = {funct with exp_type = instance funct.exp_type} in
      re {
        exp_desc = Texp_apply(funct, args);
        exp_loc = sexp.pexp_loc;
        exp_type = ty_res;
        exp_env = env }
  | Pexp_match(sarg, caselist) ->
      let arg = type_exp env sarg in
      let ty_res = newvar() in
      let cases, partial =
        type_cases env arg.exp_type ty_res (Some sexp.pexp_loc) caselist
      in
      re {
        exp_desc = Texp_match(arg, cases, partial);
        exp_loc = sexp.pexp_loc;
        exp_type = ty_res;
        exp_env = env }
  | Pexp_try(sbody, caselist) ->
      let body = type_exp env sbody in
      let cases, _ =
        type_cases env (instance Predef.type_exn) body.exp_type None
          caselist in
      re {
        exp_desc = Texp_try(body, cases);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type;
        exp_env = env }
  | Pexp_tuple sexpl ->
      let expl = List.map (type_exp env) sexpl in
      re {
        exp_desc = Texp_tuple expl;
        exp_loc = sexp.pexp_loc;
        exp_type = newty (Ttuple(List.map (fun exp -> exp.exp_type) expl));
        exp_env = env }
  | Pexp_construct(lid, sarg, explicit_arity) ->
      type_construct env sexp.pexp_loc lid sarg explicit_arity (newvar ())
  | Pexp_variant(l, sarg) ->
      let arg = may_map (type_exp env) sarg in
      let arg_type = may_map (fun arg -> arg.exp_type) arg in
      re {
        exp_desc = Texp_variant(l, arg);
        exp_loc = sexp.pexp_loc;
        exp_type= newty (Tvariant{row_fields = [l, Rpresent arg_type];
                                  row_more = newvar ();
                                  row_bound = [];
                                  row_closed = false;
                                  row_fixed = false;
                                  row_name = None});
        exp_env = env }
  | Pexp_record(lid_sexp_list, opt_sexp) ->
      let ty = newvar() in
      let num_fields = ref 0 in
      let type_label_exp (lid, sarg) =
        let label =
          try
            Env.lookup_label lid env
          with Not_found ->
            raise(Error(sexp.pexp_loc, Unbound_label lid)) in
        begin_def ();
        if !Clflags.principal then begin_def ();
        let (vars, ty_arg, ty_res) = instance_label true label in
        if !Clflags.principal then begin
          end_def ();
          generalize_structure ty_arg;
          generalize_structure ty_res
        end;
        begin try
          unify env (instance ty_res) ty
        with Unify trace ->
          raise(Error(sexp.pexp_loc, Label_mismatch(lid, trace)))
        end;
        let arg = type_argument env sarg ty_arg in
        end_def ();
        if vars <> [] && not (is_nonexpansive arg) then
          generalize_expansive env arg.exp_type;
        check_univars env "field value" arg label.lbl_arg vars;
        num_fields := Array.length label.lbl_all;
        (label, {arg with exp_type = instance arg.exp_type}) in
      let lbl_exp_list = List.map type_label_exp lid_sexp_list in
      let rec check_duplicates seen_pos lid_sexp lbl_exp =
        match (lid_sexp, lbl_exp) with
          ((lid, _) :: rem1, (lbl, _) :: rem2) ->
            if List.mem lbl.lbl_pos seen_pos
            then raise(Error(sexp.pexp_loc, Label_multiply_defined lid))
            else check_duplicates (lbl.lbl_pos :: seen_pos) rem1 rem2
        | (_, _) -> () in
      check_duplicates [] lid_sexp_list lbl_exp_list;
      let opt_exp =
        match opt_sexp, lbl_exp_list with
          None, _ -> None
        | Some sexp, (lbl, _) :: _ ->
            let ty_exp = newvar () in
            let unify_kept lbl =
              if List.for_all (fun (lbl',_) -> lbl'.lbl_pos <> lbl.lbl_pos)
                  lbl_exp_list
              then begin
                let _, ty_arg1, ty_res1 = instance_label false lbl
                and _, ty_arg2, ty_res2 = instance_label false lbl in
                unify env ty_exp ty_res1;
                unify env ty ty_res2;
                unify env ty_arg1 ty_arg2
              end in
            Array.iter unify_kept lbl.lbl_all;
            Some(type_expect env sexp ty_exp)
        | _ -> assert false
      in
      if opt_sexp = None && List.length lid_sexp_list <> !num_fields then begin
        let present_indices =
          List.map (fun (lbl, _) -> lbl.lbl_pos) lbl_exp_list in
        let label_names = extract_label_names sexp env ty in
        let rec missing_labels n = function
            [] -> []
          | lbl :: rem ->
              if List.mem n present_indices then missing_labels (n + 1) rem
              else lbl :: missing_labels (n + 1) rem
        in
        let missing = missing_labels 0 label_names in
        raise(Error(sexp.pexp_loc, Label_missing missing))
      end;
      check_private_type sexp.pexp_loc env ty;
      re {
        exp_desc = Texp_record(lbl_exp_list, opt_exp);
        exp_loc = sexp.pexp_loc;
        exp_type = ty;
        exp_env = env }
  | Pexp_field(sarg, lid) ->
      let arg = type_exp env sarg in
      let label =
        try
          Env.lookup_label lid env
        with Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_label lid)) in
      let (_, ty_arg, ty_res) = instance_label false label in
      unify_exp env arg ty_res;
      re {
        exp_desc = Texp_field(arg, label);
        exp_loc = sexp.pexp_loc;
        exp_type = ty_arg;
        exp_env = env }
  | Pexp_setfield(srecord, lid, snewval) ->
      let record = type_exp env srecord in
      let label =
        try
          Env.lookup_label lid env
        with Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_label lid)) in
      if label.lbl_mut = Immutable then
        raise(Error(sexp.pexp_loc, Label_not_mutable lid));
      begin_def ();
      let (vars, ty_arg, ty_res) = instance_label true label in
      unify_exp env record ty_res;
      let newval = type_expect env snewval ty_arg in
      end_def ();
      if vars <> [] && not (is_nonexpansive newval) then
        generalize_expansive env newval.exp_type;
      check_univars env "field value" newval label.lbl_arg vars;
      check_private_type_setfield lid sexp.pexp_loc env ty_res;
      re {
        exp_desc = Texp_setfield(record, label, newval);
        exp_loc = sexp.pexp_loc;
        exp_type = instance Predef.type_unit;
        exp_env = env }
  | Pexp_array(sargl) ->
      let ty = newvar() in
      let argl = List.map (fun sarg -> type_expect env sarg ty) sargl in
      re {
        exp_desc = Texp_array argl;
        exp_loc = sexp.pexp_loc;
        exp_type = instance (Predef.type_array ty);
        exp_env = env }
  | Pexp_ifthenelse(scond, sifso, sifnot) ->
      let cond = type_expect env scond (instance Predef.type_bool) in
      begin match sifnot with
        None ->
          let ifso = type_expect env sifso (instance Predef.type_unit) in
          re {
            exp_desc = Texp_ifthenelse(cond, ifso, None);
            exp_loc = sexp.pexp_loc;
            exp_type = instance Predef.type_unit;
            exp_env = env }
      | Some sifnot ->
          let ifso = type_exp env sifso in
          let ifnot = type_expect env sifnot ifso.exp_type in
          re {
            exp_desc = Texp_ifthenelse(cond, ifso, Some ifnot);
            exp_loc = sexp.pexp_loc;
            exp_type = ifso.exp_type;
            exp_env = env }
      end
  | Pexp_sequence(sexp1, sexp2) ->
      let exp1 = type_statement env sexp1 in
      let exp2 = type_exp env sexp2 in
      re {
        exp_desc = Texp_sequence(exp1, exp2);
        exp_loc = sexp.pexp_loc;
        exp_type = exp2.exp_type;
        exp_env = env }
  | Pexp_while(scond, sbody) ->
      let cond = type_expect env scond (instance Predef.type_bool) in
      let body = type_statement env sbody in
      re {
        exp_desc = Texp_while(cond, body);
        exp_loc = sexp.pexp_loc;
        exp_type = instance Predef.type_unit;
        exp_env = env }
  | Pexp_for(param, slow, shigh, dir, sbody) ->
      let low = type_expect env slow (instance Predef.type_int) in
      let high = type_expect env shigh (instance Predef.type_int) in
      let (id, new_env) =
        Env.enter_value param {val_type = instance Predef.type_int;
                                val_kind = Val_reg} env in
      let body = type_statement new_env sbody in
      re {
        exp_desc = Texp_for(id, low, high, dir, body);
        exp_loc = sexp.pexp_loc;
        exp_type = instance Predef.type_unit;
        exp_env = env }
  | Pexp_constraint(sarg, sty, sty') ->
      let (arg, ty') =
        match (sty, sty') with
          (None, None) ->               (* Case actually unused *)
            let arg = type_exp env sarg in
            (arg, arg.exp_type)
        | (Some sty, None) ->
            if !Clflags.principal then begin_def ();
            let ty = Typetexp.transl_simple_type env false sty in
            if !Clflags.principal then begin
              end_def ();
              generalize_structure ty;
              let ty1 = instance ty and ty2 = instance ty in
              (type_expect env sarg ty1, ty2)
            end else
              (type_expect env sarg ty, ty)
        | (None, Some sty') ->
            let (ty', force) =
              Typetexp.transl_simple_type_delayed env sty'
            in
            let arg = type_exp env sarg in
            begin match arg.exp_desc, !self_coercion, (repr ty').desc with
              Texp_ident(_, {val_kind=Val_self _}), (path,r) :: _,
              Tconstr(path',_,_) when Path.same path path' ->
                r := sexp.pexp_loc :: !r;
                force ()
            | _ ->
                let ty, b = enlarge_type env ty' in
                force ();
                begin try Ctype.unify env arg.exp_type ty with Unify trace ->
                  raise(Error(sarg.pexp_loc,
                        Coercion_failure(ty', full_expand env ty', trace, b)))
                end
            end;
            (arg, ty')
        | (Some sty, Some sty') ->
            let (ty, force) =
              Typetexp.transl_simple_type_delayed env sty
            and (ty', force') =
              Typetexp.transl_simple_type_delayed env sty'
            in
            begin try
              let force'' = subtype env ty ty' in
              force (); force' (); force'' ()
            with Subtype (tr1, tr2) ->
              raise(Error(sexp.pexp_loc, Not_subtype(tr1, tr2)))
            end;
            (type_expect env sarg ty, ty')
      in
      re {
        exp_desc = arg.exp_desc;
        exp_loc = arg.exp_loc;
        exp_type = ty';
        exp_env = env }
  | Pexp_when(scond, sbody) ->
      let cond = type_expect env scond (instance Predef.type_bool) in
      let body = type_exp env sbody in
      re {
        exp_desc = Texp_when(cond, body);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type;
        exp_env = env }
  | Pexp_send (e, met) ->
      if !Clflags.principal then begin_def ();
      let obj = type_exp env e in
      begin try
        let (exp, typ) =
          match obj.exp_desc with
            Texp_ident(path, {val_kind = Val_self (meths, _, _)}) ->
              let (id, typ) =
                filter_self_method env met Private meths obj.exp_type
              in
              (Texp_send(obj, Tmeth_val id), typ)
          | Texp_ident(path, {val_kind = Val_anc (methods, cl_num)}) ->
              let method_id =
                begin try List.assoc met methods with Not_found ->
                  raise(Error(e.pexp_loc, Undefined_inherited_method met))
                end
              in
              begin match
                Env.lookup_value (Longident.Lident ("selfpat-" ^ cl_num)) env,
                Env.lookup_value (Longident.Lident ("self-" ^cl_num)) env
              with
                (_, ({val_kind = Val_self (meths, _, _)} as desc)),
                (path, _) ->
                  let (_, typ) =
                    filter_self_method env met Private meths obj.exp_type
                  in
                  let method_type = newvar () in
                  let (obj_ty, res_ty) = filter_arrow env method_type "" in
                  unify env obj_ty desc.val_type;
                  unify env res_ty (instance typ);
                  (Texp_apply({ exp_desc = Texp_ident(Path.Pident method_id,
                                                     {val_type = method_type;
                                                       val_kind = Val_reg});
                                exp_loc = sexp.pexp_loc;
                                exp_type = method_type;
                                exp_env = env },
                              [Some {exp_desc = Texp_ident(path, desc);
                                     exp_loc = obj.exp_loc;
                                     exp_type = desc.val_type;
                                     exp_env = env },
                               Required]),
                   typ)
              |  _ ->
                  assert false
              end
          | _ ->
              (Texp_send(obj, Tmeth_name met),
               filter_method env met Public obj.exp_type)
        in
        if !Clflags.principal then begin
          end_def ();
          generalize_structure typ;
        end;
        let typ =
          match repr typ with
            {desc = Tpoly (ty, [])} ->
              instance ty
          | {desc = Tpoly (ty, tl); level = l} ->
              if !Clflags.principal && l <> generic_level then
                Location.prerr_warning sexp.pexp_loc
                  (Warnings.Other
                     "This use of a polymorphic method is not principal");
              snd (instance_poly false tl ty)
          | {desc = Tvar} as ty ->
              let ty' = newvar () in
              unify env (instance ty) (newty(Tpoly(ty',[])));
              (* if not !Clflags.nolabels then
                 Location.prerr_warning loc (Warnings.Unknown_method met); *)
              ty'
          | _ ->
              assert false
        in
          re {
            exp_desc = exp;
            exp_loc = sexp.pexp_loc;
            exp_type = typ;
            exp_env = env }
      with Unify _ ->
        raise(Error(e.pexp_loc, Undefined_method (obj.exp_type, met)))
      end
  | Pexp_new cl ->
      let (cl_path, cl_decl) =
        try Env.lookup_class cl env with Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_class cl))
      in
        begin match cl_decl.cty_new with
          None ->
            raise(Error(sexp.pexp_loc, Virtual_class cl))
        | Some ty ->
            re {
              exp_desc = Texp_new (cl_path, cl_decl);
              exp_loc = sexp.pexp_loc;
              exp_type = instance ty;
              exp_env = env }
        end
  | Pexp_setinstvar (lab, snewval) ->
      begin try
        let (path, desc) = Env.lookup_value (Longident.Lident lab) env in
        match desc.val_kind with
          Val_ivar (Mutable, cl_num) ->
            let newval = type_expect env snewval (instance desc.val_type) in
            let (path_self, _) =
              Env.lookup_value (Longident.Lident ("self-" ^ cl_num)) env
            in
            re {
              exp_desc = Texp_setinstvar(path_self, path, newval);
              exp_loc = sexp.pexp_loc;
              exp_type = instance Predef.type_unit;
              exp_env = env }
        | Val_ivar _ ->
            raise(Error(sexp.pexp_loc, Instance_variable_not_mutable lab))
        | _ ->
            raise(Error(sexp.pexp_loc, Unbound_instance_variable lab))
      with
        Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_instance_variable lab))
      end        
  | Pexp_override lst ->
      let _ = 
       List.fold_right
        (fun (lab, _) l ->
           if List.exists ((=) lab) l then
             raise(Error(sexp.pexp_loc,
                         Value_multiply_overridden lab));
           lab::l)
        lst
        [] in
      begin match
        try
          Env.lookup_value (Longident.Lident "selfpat-*") env,
          Env.lookup_value (Longident.Lident "self-*") env
        with Not_found ->
          raise(Error(sexp.pexp_loc, Outside_class))
      with
        (_, {val_type = self_ty; val_kind = Val_self (_, vars, _)}),
        (path_self, _) ->
          let type_override (lab, snewval) =
            begin try
              let (id, _, ty) = Vars.find lab !vars in
              (Path.Pident id, type_expect env snewval (instance ty))
            with
              Not_found ->
                raise(Error(sexp.pexp_loc, Unbound_instance_variable lab))
            end
          in
          let modifs = List.map type_override lst in
          re {
            exp_desc = Texp_override(path_self, modifs);
            exp_loc = sexp.pexp_loc;
            exp_type = self_ty;
            exp_env = env }
      | _ ->
          assert false
      end
  | Pexp_letmodule(name, smodl, sbody) ->
      let ty = newvar() in
      Ident.set_current_time ty.level;
      let context = Typetexp.narrow () in
      let modl = !type_module env smodl in
      let (id, new_env) = Env.enter_module name modl.mod_type env in
      Ctype.init_def(Ident.current_time());
      Typetexp.widen context;
      let body = type_exp new_env sbody in
      (* Unification of body.exp_type with the fresh variable ty
         fails if and only if the prefix condition is violated,
         i.e. if generative types rooted at id show up in the
         type body.exp_type.  Thus, this unification enforces the
         scoping condition on "let module". *)
      begin try
        Ctype.unify new_env body.exp_type ty
      with Unify _ ->
        raise(Error(sexp.pexp_loc, Scoping_let_module(name, body.exp_type)))
      end;
      re {
        exp_desc = Texp_letmodule(id, modl, body);
        exp_loc = sexp.pexp_loc;
        exp_type = ty;
        exp_env = env }
  | Pexp_assert (e) ->
       let cond = type_expect env e (instance Predef.type_bool) in
       re {
         exp_desc = Texp_assert (cond);
         exp_loc = sexp.pexp_loc;
         exp_type = instance Predef.type_unit;
         exp_env = env;
       }
  | Pexp_assertfalse ->
       re {
         exp_desc = Texp_assertfalse;
         exp_loc = sexp.pexp_loc;
         exp_type = newvar ();
         exp_env = env;
       }
  | Pexp_lazy (e) ->
       let arg = type_exp env e in
       re {
         exp_desc = Texp_lazy arg;
         exp_loc = sexp.pexp_loc;
         exp_type = instance (Predef.type_lazy_t arg.exp_type);
         exp_env = env;
       }
  | Pexp_poly _ ->
      assert false

and type_argument env sarg ty_expected' =
  (* ty_expected' may be generic *)
  let no_labels ty =
    let ls, tvar = list_labels env ty in
    not tvar && List.for_all ((=) "") ls
  in
  let ty_expected = instance ty_expected' in
  match expand_head env ty_expected', sarg with
  | _, {pexp_desc = Pexp_function(l,_,_)} when not (is_optional l) ->
      type_expect env sarg ty_expected
  | {desc = Tarrow("",ty_arg,ty_res,_); level = lv}, _ ->
      (* apply optional arguments when expected type is "" *)
      (* we must be very careful about not breaking the semantics *)
      if !Clflags.principal then begin_def ();
      let texp = type_exp env sarg in
      if !Clflags.principal then begin
        end_def ();
        generalize_structure texp.exp_type
      end;
      let rec make_args args ty_fun =
        match (expand_head env ty_fun).desc with
        | Tarrow (l,ty_arg,ty_fun,_) when is_optional l ->
            make_args
              ((Some(option_none (instance ty_arg) sarg.pexp_loc), Optional)
               :: args)
              ty_fun
        | Tarrow (l,_,ty_res',_) when l = "" || !Clflags.classic ->
            args, ty_fun, no_labels ty_res'
        | Tvar ->  args, ty_fun, false
        |  _ -> [], texp.exp_type, false
      in
      let args, ty_fun', simple_res = make_args [] texp.exp_type in
      let warn = !Clflags.principal &&
        (lv <> generic_level || (repr ty_fun').level <> generic_level)
      and texp = {texp with exp_type = instance texp.exp_type}
      and ty_fun = instance ty_fun' in
      if not (simple_res || no_labels ty_res) then begin
        unify_exp env texp ty_expected;
        texp
      end else begin
      unify_exp env {texp with exp_type = ty_fun} ty_expected;
      if args = [] then texp else
      (* eta-expand to avoid side effects *)
      let var_pair name ty =
        let id = Ident.create name in
        {pat_desc = Tpat_var id; pat_type = ty;
         pat_loc = Location.none; pat_env = env},
        {exp_type = ty; exp_loc = Location.none; exp_env = env; exp_desc =
         Texp_ident(Path.Pident id,{val_type = ty; val_kind = Val_reg})}
      in
      let eta_pat, eta_var = var_pair "eta" ty_arg in
      let func texp =
        { texp with exp_type = ty_fun; exp_desc =
          Texp_function([eta_pat, {texp with exp_type = ty_res; exp_desc =
                                   Texp_apply (texp, args@
                                               [Some eta_var, Required])}],
                        Total) } in
      if warn then Location.prerr_warning texp.exp_loc
          (Warnings.Other "Eliminated optional argument without principality");
      if is_nonexpansive texp then func texp else
      (* let-expand to have side effects *)
      let let_pat, let_var = var_pair "let" texp.exp_type in
      re { texp with exp_type = ty_fun; exp_desc =
           Texp_let (Nonrecursive, [let_pat, texp], func let_var) }
      end
  | _ ->
      type_expect env sarg ty_expected

and type_application env funct sargs =
  (* funct.exp_type may be generic *)
  let result_type omitted ty_fun =
    List.fold_left
      (fun ty_fun (l,ty,lv) -> newty2 lv (Tarrow(l,ty,ty_fun,Cok)))
      ty_fun omitted
  in
  let has_label l ty_fun =
    let ls, tvar = list_labels env ty_fun in
    tvar || List.mem l ls
  in
  let ignored = ref [] in
  let rec type_unknown_args args omitted ty_fun = function
      [] ->
        (List.map
           (function None, x -> None, x | Some f, x -> Some (f ()), x)
           (List.rev args),
         instance (result_type omitted ty_fun))
    | (l1, sarg1) :: sargl ->
        let (ty1, ty2) =
          match (expand_head env ty_fun).desc with
            Tvar ->
              let t1 = newvar () and t2 = newvar () in
              unify env ty_fun (newty (Tarrow(l1,t1,t2,Clink(ref Cunknown))));
              (t1, t2)
          | Tarrow (l,t1,t2,_) when l = l1
            || !Clflags.classic && l1 = "" && not (is_optional l) ->
              (t1, t2)
          | td ->
              let ty_fun =
                match td with Tarrow _ -> newty td | _ -> ty_fun in
              let ty_res = result_type (omitted @ !ignored) ty_fun in
              match ty_res.desc with
                Tarrow _ ->
                  if (!Clflags.classic || not (has_label l1 ty_fun)) then
                    raise(Error(sarg1.pexp_loc, Apply_wrong_label(l1, ty_res)))
                  else
                    raise(Error(funct.exp_loc, Incoherent_label_order))
              | _ ->
                  raise(Error(funct.exp_loc, Apply_non_function
                                (expand_head env funct.exp_type)))
        in
        let optional = if is_optional l1 then Optional else Required in
        let arg1 () =
          let arg1 = type_expect env sarg1 ty1 in
          if optional = Optional then
            unify_exp env arg1 (type_option(newvar()));
          arg1
        in
        type_unknown_args ((Some arg1, optional) :: args) omitted ty2 sargl
  in
  let ignore_labels =
    !Clflags.classic ||
    begin
      let ls, tvar = list_labels env funct.exp_type in
      not tvar &&
      let labels = List.filter (fun l -> not (is_optional l)) ls in
      List.length labels = List.length sargs &&
      List.for_all (fun (l,_) -> l = "") sargs &&
      List.exists (fun l -> l <> "") labels &&
      (Location.prerr_warning funct.exp_loc Warnings.Labels_omitted;
       true)
    end
  in
  let warned = ref false in
  let rec type_args args omitted ty_fun ty_old sargs more_sargs =
    match expand_head env ty_fun with
      {desc=Tarrow (l, ty, ty_fun, com); level=lv} as ty_fun'
      when (sargs <> [] || more_sargs <> []) && commu_repr com = Cok ->
        let may_warn loc msg =
          if not !warned && !Clflags.principal && lv <> generic_level
          then begin
            warned := true;
            Location.prerr_warning loc (Warnings.Other msg)
          end
        in
        let name = label_name l
        and optional = if is_optional l then Optional else Required in
        let sargs, more_sargs, arg =
          if ignore_labels && not (is_optional l) then begin
            (* In classic mode, omitted = [] *)
            match sargs, more_sargs with
              (l', sarg0) :: _, _ ->
                raise(Error(sarg0.pexp_loc, Apply_wrong_label(l', ty_old)))
            | _, (l', sarg0) :: more_sargs ->
                if l <> l' && l' <> "" then
                  raise(Error(sarg0.pexp_loc, Apply_wrong_label(l', ty_fun')))
                else
                  ([], more_sargs, Some (fun () -> type_argument env sarg0 ty))
            | _ ->
                assert false
          end else try
            let (l', sarg0, sargs, more_sargs) =
              try
                let (l', sarg0, sargs1, sargs2) = extract_label name sargs in
                if sargs1 <> [] then
                  may_warn sarg0.pexp_loc
                    "Commuting this argument is not principal";
                (l', sarg0, sargs1 @ sargs2, more_sargs)
              with Not_found ->
                let (l', sarg0, sargs1, sargs2) =
                  extract_label name more_sargs in
                if sargs1 <> [] || sargs <> [] then
                  may_warn sarg0.pexp_loc
                    "Commuting this argument is not principal";
                (l', sarg0, sargs @ sargs1, sargs2)
            in
            sargs, more_sargs,
            if optional = Required || is_optional l' then
              Some (fun () -> type_argument env sarg0 ty)
            else begin
              may_warn sarg0.pexp_loc
                "Using an optional argument here is not principal";
              Some (fun () -> option_some (type_argument env sarg0 
                                             (extract_option_type env ty)))
            end
          with Not_found ->
            sargs, more_sargs,
            if optional = Optional &&
              (List.mem_assoc "" sargs || List.mem_assoc "" more_sargs)
            then begin
              may_warn funct.exp_loc
                "Eliminated an optional argument without principality";
              ignored := (l,ty,lv) :: !ignored;
              Some (fun () -> option_none (instance ty) Location.none)
            end else begin
              may_warn funct.exp_loc
                "Commuted an argument without principality";
              None
            end
        in
        let omitted =
          if arg = None then (l,ty,lv) :: omitted else omitted in
        let ty_old = if sargs = [] then ty_fun else ty_old in
        type_args ((arg,optional)::args) omitted ty_fun ty_old sargs more_sargs
    | _ ->
        match sargs with
          (l, sarg0) :: _ when ignore_labels ->
            raise(Error(sarg0.pexp_loc, Apply_wrong_label(l, ty_old)))
        | _ ->
            type_unknown_args args omitted (instance ty_fun)
              (sargs @ more_sargs)
  in
  match funct.exp_desc, sargs with
    (* Special case for ignore: avoid discarding warning *)
    Texp_ident (_, {val_kind=Val_prim{Primitive.prim_name="%ignore"}}),
    ["", sarg] ->
      let ty_arg, ty_res = filter_arrow env (instance funct.exp_type) "" in
      let exp = type_expect env sarg ty_arg in
      begin match (expand_head env exp.exp_type).desc with
      | Tarrow _ ->
          Location.prerr_warning exp.exp_loc Warnings.Partial_application
      | Tvar ->
          add_delayed_check (fun () -> check_partial_application env exp)
      | _ -> ()
      end;
      ([Some exp, Required], ty_res)
  | _ ->
      let ty = funct.exp_type in
      if ignore_labels then
        type_args [] [] ty ty [] sargs
      else
        type_args [] [] ty ty sargs []

and type_construct env loc lid sarg explicit_arity ty_expected =
  let constr =
    try
      Env.lookup_constructor lid env
    with Not_found ->
      raise(Error(loc, Unbound_constructor lid)) in
  let sargs =
    match sarg with
      None -> []
    | Some {pexp_desc = Pexp_tuple sel} when explicit_arity -> sel
    | Some {pexp_desc = Pexp_tuple sel} when constr.cstr_arity > 1 -> sel
    | Some se -> [se] in
  if List.length sargs <> constr.cstr_arity then
    raise(Error(loc, Constructor_arity_mismatch
                  (lid, constr.cstr_arity, List.length sargs)));
  if !Clflags.principal then begin_def ();
  let (ty_args, ty_res) = instance_constructor constr in
  if !Clflags.principal then begin
    end_def ();
    List.iter generalize_structure ty_args;
    generalize_structure ty_res
  end;
  let texp =
    re {
      exp_desc = Texp_construct(constr, []);
      exp_loc = loc;
      exp_type = instance ty_res;
      exp_env = env } in
  unify_exp env texp ty_expected;
  let args = List.map2 (type_argument env) sargs ty_args in
  check_private_type loc env ty_res;
  { texp with exp_desc = Texp_construct(constr, args) }

(* Typing of an expression with an expected type.
   Some constructs are treated specially to provide better error messages. *)

and type_expect ?in_function env sexp ty_expected =
  match sexp.pexp_desc with
    Pexp_constant(Const_string s as cst) ->
      let exp =
        re {
          exp_desc = Texp_constant cst;
          exp_loc = sexp.pexp_loc;
          exp_type =
            (* Terrible hack for format strings *)
            begin match (repr (expand_head env ty_expected)).desc with
              Tconstr(path, _, _) when Path.same path Predef.path_format ->
                type_format sexp.pexp_loc s
            | _ -> instance Predef.type_string
            end;
          exp_env = env } in
      unify_exp env exp ty_expected;
      exp
  | Pexp_construct(lid, sarg, explicit_arity) ->
      type_construct env sexp.pexp_loc lid sarg explicit_arity ty_expected
  | Pexp_let(rec_flag, spat_sexp_list, sbody) ->
      let (pat_exp_list, new_env) = type_let env rec_flag spat_sexp_list in
      let body = type_expect new_env sbody ty_expected in
      re {
        exp_desc = Texp_let(rec_flag, pat_exp_list, body);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type;
        exp_env = env }
  | Pexp_sequence(sexp1, sexp2) ->
      let exp1 = type_statement env sexp1 in
      let exp2 = type_expect env sexp2 ty_expected in
      re {
        exp_desc = Texp_sequence(exp1, exp2);
        exp_loc = sexp.pexp_loc;
        exp_type = exp2.exp_type;
        exp_env = env }
  | Pexp_function (l, Some default, [spat, sbody]) ->
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
        {pexp_loc = sexp.pexp_loc; pexp_desc =
         Pexp_function(l, None,[{ppat_loc = loc; ppat_desc = Ppat_var"*opt*"},
                                {pexp_loc = sexp.pexp_loc; pexp_desc =
                                 Pexp_let(Default, [spat, smatch], sbody)}])}
      in
      type_expect ?in_function env sfun ty_expected
  | Pexp_function (l, _, caselist) ->
      let (loc, ty_fun) =
        match in_function with Some p -> p
        | None -> (sexp.pexp_loc, ty_expected)
      in
      let (ty_arg, ty_res) =
        try filter_arrow env ty_expected l
        with Unify _ ->
          match expand_head env ty_expected with
            {desc = Tarrow _} as ty ->
              raise(Error(sexp.pexp_loc, Abstract_wrong_label(l, ty)))
          | _ ->
              raise(Error(loc,
                          Too_many_arguments (in_function <> None, ty_fun)))
      in
      if is_optional l then begin
        try unify env ty_arg (type_option(newvar()))
        with Unify _ -> assert false
      end;
      let cases, partial =
        type_cases ~in_function:(loc,ty_fun) env ty_arg ty_res
          (Some sexp.pexp_loc) caselist in
      let all_labeled ty =
        let ls, tvar = list_labels env ty in
        not (tvar || List.exists (fun l -> l = "" || l.[0] = '?') ls)
      in
      if is_optional l && all_labeled ty_res then
        Location.prerr_warning (fst (List.hd cases)).pat_loc
          (Warnings.Other "This optional argument cannot be erased");
      re {
        exp_desc = Texp_function(cases, partial);
        exp_loc = sexp.pexp_loc;
        exp_type = newty (Tarrow(l, ty_arg, ty_res, Cok));
        exp_env = env }
  | Pexp_poly(sbody, sty) ->
      let ty =
        match sty with None -> repr ty_expected
        | Some sty ->
            let ty = Typetexp.transl_simple_type env false sty in
            repr ty
      in            
      let set_type ty =
        unify_exp env
          { exp_desc = Texp_tuple []; exp_loc = sexp.pexp_loc;
            exp_type = ty; exp_env = env } ty_expected in
      begin
        match ty.desc with
          Tpoly (ty', []) ->
            if sty <> None then set_type ty;
            let exp = type_expect env sbody ty' in
            re { exp with exp_type = ty }
        | Tpoly (ty', tl) ->
            if sty <> None then set_type ty;
            (* One more level to generalize locally *)
            begin_def ();
            let vars, ty'' = instance_poly true tl ty' in
            let exp = type_expect env sbody ty'' in
            end_def ();
            check_univars env "method" exp ty_expected vars;
            re { exp with exp_type = ty }
        | _ -> assert false
      end
  | _ ->
      let exp = type_exp env sexp in
      unify_exp env exp ty_expected;
      exp

(* Typing of statements (expressions whose values are discarded) *)

and type_statement env sexp =
    let exp = type_exp env sexp in
    match (expand_head env exp.exp_type).desc with
    | Tarrow _ ->
        Location.prerr_warning sexp.pexp_loc Warnings.Partial_application;
        exp
    | Tconstr (p, _, _) when Path.same p Predef.path_unit -> exp
    | Tvar ->
        add_delayed_check (fun () -> check_partial_application env exp);
        exp
    | _ ->
        Location.prerr_warning sexp.pexp_loc Warnings.Statement_type;
        exp

(* Typing of match cases *)

and type_cases ?in_function env ty_arg ty_res partial_loc caselist =
  let ty_arg' = newvar () in
  let pattern_force = ref [] in
  let pat_env_list =
    List.map
      (fun (spat, sexp) ->
        if !Clflags.principal then begin_def ();
        let (pat, ext_env, force) = type_pattern env spat in
        pattern_force := force @ !pattern_force;
        let pat =
          if !Clflags.principal then begin
            end_def ();
            iter_pattern (fun {pat_type=t} -> generalize_structure t) pat;
            { pat with pat_type = instance pat.pat_type }
          end else pat
        in
        unify_pat env pat ty_arg';
        (pat, ext_env))
      caselist in
  (* Build dummy cases, since we cannot type yet *)
  let dummy_cases = List.map2
      (fun (pat, _) (_, act) ->
        let dummy = { exp_desc = Texp_tuple []; exp_type = newty (Ttuple[]);
                      exp_env = env; exp_loc = act.pexp_loc } in
        match act.pexp_desc with
          Pexp_when _ ->
            pat, {dummy with exp_desc = Texp_when(dummy, dummy)}
        | _           -> pat, dummy)
      pat_env_list caselist in
  (* Check partial matches here (required for polymorphic variants) *)
  let partial =
    match partial_loc with None -> Partial
    | Some loc -> Parmatch.check_partial env loc dummy_cases in
  (* Revert to normal typing of variants (also register checks) *)
  List.iter (fun (pat, _) -> iter_pattern finalize_variant pat) dummy_cases;
  (* `Contaminating' unifications start here *)
  List.iter (fun f -> f()) !pattern_force;
  begin match pat_env_list with [] -> ()
  | (pat, _) :: _ -> unify_pat env pat ty_arg
  end;
  let in_function = if List.length caselist = 1 then in_function else None in
  let cases =
    List.map2
      (fun (pat, ext_env) (spat, sexp) ->
        let exp = type_expect ?in_function ext_env sexp ty_res in
        (pat, exp))
      pat_env_list caselist in
  add_delayed_check (fun () -> Parmatch.check_unused env cases);
  cases, partial

(* Typing of let bindings *)

and type_let env rec_flag spat_sexp_list =
  begin_def();
  if !Clflags.principal then begin_def ();
  let (pat_list, new_env, force) =
    type_pattern_list env (List.map (fun (spat, sexp) -> spat) spat_sexp_list)
  in
  if rec_flag = Recursive then
    List.iter2
      (fun pat (_, sexp) -> unify_pat env pat (type_approx env sexp))
      pat_list spat_sexp_list;
  let pat_list =
    if !Clflags.principal then begin
      end_def ();
      List.map
        (fun pat ->
          iter_pattern (fun pat -> generalize_structure pat.pat_type) pat;
          {pat with pat_type = instance pat.pat_type})
        pat_list
    end else pat_list in
  (* Only bind pattern variables after generalizing *)
  List.iter (fun f -> f()) force;
  let exp_env =
    match rec_flag with Nonrecursive | Default -> env | Recursive -> new_env in
  let exp_list =
    List.map2
      (fun (spat, sexp) pat -> type_expect exp_env sexp pat.pat_type)
      spat_sexp_list pat_list in
  List.iter2
    (fun pat exp -> ignore(Parmatch.check_partial env pat.pat_loc [pat, exp]))
    pat_list exp_list;
  end_def();
  List.iter2
    (fun pat exp ->
       if not (is_nonexpansive exp) then
         iter_pattern (fun pat -> generalize_expansive env pat.pat_type) pat)
    pat_list exp_list;
  List.iter
    (fun pat -> iter_pattern (fun pat -> generalize pat.pat_type) pat)
    pat_list;
  (List.combine pat_list exp_list, new_env)

(* Typing of toplevel bindings *)

let type_binding env rec_flag spat_sexp_list =
  Typetexp.reset_type_variables();
  type_let env rec_flag spat_sexp_list

(* Typing of toplevel expressions *)

let type_expression env sexp =
  Typetexp.reset_type_variables();
  begin_def();
  let exp = type_exp env sexp in
  end_def();
  if is_nonexpansive exp then generalize exp.exp_type
  else generalize_expansive env exp.exp_type;
  exp

(* Error report *)

open Format
open Printtyp

let report_error ppf = function
  | Unbound_value lid ->
      fprintf ppf "Unbound value %a" longident lid
  | Unbound_constructor lid ->
      fprintf ppf "Unbound constructor %a" longident lid
  | Unbound_label lid ->
      fprintf ppf "Unbound record field label %a" longident lid
  | Constructor_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
       "@[The constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
       longident lid expected provided
  | Label_mismatch(lid, trace) ->
      report_unification_error ppf trace
        (function ppf ->
           fprintf ppf "The record field label %a@ belongs to the type"
                   longident lid)
        (function ppf ->
           fprintf ppf "but is here mixed with labels of type")
  | Pattern_type_clash trace ->
      report_unification_error ppf trace
        (function ppf ->
           fprintf ppf "This pattern matches values of type")
        (function ppf ->
           fprintf ppf "but is here used to match values of type")
  | Multiply_bound_variable ->
      fprintf ppf "This variable is bound several times in this matching"
  | Orpat_vars id ->
      fprintf ppf "Variable %s must occur on both sides of this | pattern"
        (Ident.name id)
  | Expr_type_clash trace ->
      report_unification_error ppf trace
        (function ppf ->
           fprintf ppf "This expression has type")
        (function ppf ->
           fprintf ppf "but is here used with type")
  | Apply_non_function typ ->
      begin match (repr typ).desc with
        Tarrow _ ->
          fprintf ppf "This function is applied to too many arguments,@ ";
          fprintf ppf "maybe you forgot a `;'"
      | _ ->
          fprintf ppf
            "This expression is not a function, it cannot be applied"
      end
  | Apply_wrong_label (l, ty) ->
      let print_label ppf = function
        | "" -> fprintf ppf "without label"
        | l ->
            fprintf ppf "with label %s%s" (if is_optional l then "" else "~") l
      in
      reset_and_mark_loops ty;
      fprintf ppf
        "@[<v>@[<2>Expecting function has type@ %a@]@.\
          This argument cannot be applied %a@]"
        type_expr ty print_label l
  | Label_multiply_defined lid ->
      fprintf ppf "The record field label %a is defined several times"
              longident lid
  | Label_missing labels ->
      let print_labels ppf = List.iter (fun lbl -> fprintf ppf "@ %s" lbl) in
      fprintf ppf "@[<hov>Some record field labels are undefined:%a@]"
        print_labels labels
  | Label_not_mutable lid ->
      fprintf ppf "The record field label %a is not mutable" longident lid
  | Bad_format s ->
      fprintf ppf "Bad format `%s'" s
  | Undefined_method (ty, me) ->
      reset_and_mark_loops ty;
      fprintf ppf
        "@[<v>@[This expression has type@;<1 2>%a@]@,\
         It has no method %s@]" type_expr ty me
  | Undefined_inherited_method me ->
      fprintf ppf "This expression has no method %s" me
  | Unbound_class cl ->
      fprintf ppf "Unbound class %a" longident cl
  | Virtual_class cl ->
      fprintf ppf "One cannot create instances of the virtual class %a"
        longident cl
  | Unbound_instance_variable v ->
      fprintf ppf "Unbound instance variable %s" v
  | Instance_variable_not_mutable v ->
      fprintf ppf "The instance variable %s is not mutable" v
  | Not_subtype(tr1, tr2) ->
      report_subtyping_error ppf tr1 "is not a subtype of type" tr2
  | Outside_class ->
      fprintf ppf "This object duplication occurs outside a method definition"
  | Value_multiply_overridden v ->
      fprintf ppf "The instance variable %s is overridden several times" v
  | Coercion_failure (ty, ty', trace, b) ->
      report_unification_error ppf trace
        (function ppf ->
           let ty, ty' = prepare_expansion (ty, ty') in
           fprintf ppf
             "This expression cannot be coerced to type@;<1 2>%a;@ it has type"
           (type_expansion ty) ty')
        (function ppf ->
           fprintf ppf "but is here used with type");
      if b then
        fprintf ppf ".@.@[<hov>%s@ %s@]"
          "This simple coercion was not fully general."
          "Consider using a double coercion."
  | Too_many_arguments (in_function, ty) ->
      reset_and_mark_loops ty;
      if in_function then begin
        fprintf ppf "This function expects too many arguments,@ ";
        fprintf ppf "it should have type@ %a"
          type_expr ty
      end else begin
        fprintf ppf "This expression should not be a function,@ ";
        fprintf ppf "the expected type is@ %a"
          type_expr ty
      end
  | Abstract_wrong_label (l, ty) ->
      let label_mark = function
        | "" -> "but its first argument is not labeled"
        |  l -> sprintf "but its first argument is labeled ~%s" l in
      reset_and_mark_loops ty;
      fprintf ppf "@[<v>@[<2>This function should have type@ %a@]@,%s@]"
      type_expr ty (label_mark l)
  | Scoping_let_module(id, ty) ->
      reset_and_mark_loops ty;
      fprintf ppf
       "This `let module' expression has type@ %a@ " type_expr ty;
      fprintf ppf
       "In this type, the locally bound module name %s escapes its scope" id
  | Masked_instance_variable lid ->
      fprintf ppf
        "The instance variable %a@ \
         cannot be accessed from the definition of another instance variable"
        longident lid
  | Private_type ty ->
      fprintf ppf "One cannot create values of the private type %s" ty
  | Private_type_setfield (lid, ty) ->
      fprintf ppf "Cannot assign field %a of the private type %s"
        longident lid ty
  | Not_a_variant_type lid ->
      fprintf ppf "The type %a@ is not a variant type" longident lid
  | Incoherent_label_order ->
      fprintf ppf "This function is applied to arguments@ ";
      fprintf ppf "in an order different from other calls.@ ";
      fprintf ppf "This is only allowed when the real type is known."
  | Less_general (kind, trace) ->
      report_unification_error ppf trace
        (fun ppf -> fprintf ppf "This %s has type" kind)
        (fun ppf -> fprintf ppf "which is less general than")
