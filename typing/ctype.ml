(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Operations on core types *)

open Misc
open Asttypes
open Types

(*
   General notes
   =============
   - As much sharing as possible should be kept : it makes types
     smaller and better abbreviated.
     When necessary, some sharing can be lost. Types will still be
     printed correctly (XXX TO DO...), and types defined by a class do
     not depend on sharing thanks to constrained abbreviations. (Of
     course, typing will still be correct.)
   - All nodes of a type have a level : that way, one know whether a
     node need to be duplicated or not when instantiating a type.
   - Levels of a type are decreasing.
   - The level of a type constructor is superior to the binding
     time of its path.

*)

(*
   A faire
   =======
   - Revoir affichage des types.
   - Types recursifs sans limitation.
   - Etendre la portee d'un alias [... as 'a] a tout le type englobant.
   - #-type implementes comme de vraies abreviations.
   - Deplacer Ctype.repr dans Types ?
   - Niveaux plus fins pour les identificateurs :
       Champ [global] renomme en [level];
       Niveau -1 : global
               0 : module toplevel
               1 : module contenu dans module toplevel
              ...
     En fait, incrementer le niveau a chaque fois que l'on rentre dans
     un module.

       3   4 6
        \ / /
       1 2 5
        \|/
         0

     [Subst] doit ecreter les niveaux (pour qu'un variable non
     generalisable dans un module de niveau 2 ne se retrouve pas
     generalisable lorsque l'on l'utilise au niveau 0).

   - Traitement de la trace de l'unification separe de la fonction
     [unify].
*)

(**** Errors ****)

exception Unify of (type_expr * type_expr) list

exception Subtype of
        (type_expr * type_expr) list * (type_expr * type_expr) list

exception Cannot_expand

(**** Type level management ****)

let generic_level = (-1)
let current_level = ref 0
let global_level = ref 1

let init_def level = current_level := level
let begin_def () = incr current_level
let end_def () = decr current_level

let reset_global_level () =
  global_level := !current_level + 1

(**** Some type creators ****)

let newgenty desc      = { desc = desc; level = generic_level }
let newgenvar ()     = newgenty Tvar

let newty desc         = { desc = desc; level = !current_level }
let newvar ()          = { desc = Tvar; level = !current_level }
let newobj fields      = newty (Tobject (fields, ref None))

let new_global_ty desc = { desc = desc; level = !global_level }
let new_global_var ()  = new_global_ty Tvar

let none = newty (Ttuple [])                (* Clearly ill-formed type *)

(**** Representative of a type ****)

let rec repr =
  function
    {desc = Tlink t'} as t ->
      let r = repr t' in
      if r != t' then t.desc <- Tlink r;
      r
  | t -> t


                  (**********************************************)
                  (*  Miscellaneous operations on object types  *)
                  (**********************************************)


(**** Object field manipulation. ****)

let flatten_fields ty =
  let rec flatten l ty =
    let ty = repr ty in
    match ty.desc with
      Tfield(s, ty1, ty2) ->
        flatten ((s, ty1)::l) ty2
    | Tvar | Tnil ->
        (l, ty)
    | _ ->
      fatal_error "Ctype.flatten_fields"
  in
    let (l, r) = flatten [] ty in
      (List.rev l, r)

let build_fields =
  List.fold_right
    (fun (s, ty1) ty2 ->
       {desc = Tfield(s, ty1, ty2);
        level = ty2.level})

let associate_fields fields1 fields2 =
  let rec associate p s s' =
    function
      (l, []) ->
        (List.rev p, (List.rev s) @ l, List.rev s')
    | ([], l') ->
        (List.rev p, List.rev s, (List.rev s') @ l')
    | ((n, t)::r, (n', t')::r') when n = n' ->
        associate ((t, t')::p) s s' (r, r')
    | ((n, t)::r, ((n', t')::_ as l')) when n < n' ->
        associate p ((n, t)::s) s' (r, l')
    | (((n, t)::r as l), (n', t')::r') (* when n > n' *) ->
        associate p s ((n', t')::s') (l, r')
  in let sort = Sort.list (fun (n, _) (n', _) -> n < n') in
  associate [] [] [] (sort fields1, sort fields2)

(**** Check whether an object is open ****)

(* XXX Il faudra penser a eventuellement expanser l'abreviation *)
let rec opened_object ty =
  match (repr ty).desc with
    Tobject (t, _)  -> opened_object t
  | Tfield(_, _, t) -> opened_object t
  | Tvar            -> true
  | _               -> false

(**** Close an object ****)

let close_object ty =
  let rec close ty =
    let ty = repr ty in
    match ty.desc with
      Tvar              -> ty.desc <- Tlink {desc = Tnil; level = ty.level}
    | Tfield(_, _, ty') -> close ty'
    | Tnil              -> ()
    | _                 -> fatal_error "Ctype.close_object (1)"
  in
  match (repr ty).desc with
    Tobject (ty, _)   -> close ty
  | Tconstr (_, _, _) -> ()             (* Already closed *)
  | _                 -> fatal_error "Ctype.close_object (2)"


(**** Object name manipulation ****)
(* XXX Bientot obsolete *)

let rec row_variable ty =
  let ty = repr ty in
  match ty.desc with
    Tfield (_, _, ty) -> row_variable ty
  | Tvar              -> ty
  | Tnil              -> raise Not_found
  | _                 -> fatal_error "Ctype.row_variable"

let set_object_name ty params id =
  match (repr ty).desc with
    Tobject (fi, nm) ->
      begin try
        nm := Some (Path.Pident id, (row_variable fi)::params)
      with Not_found ->
        ()
      end
  | Tconstr (_, _, _) ->
      ()
  | _ ->
      fatal_error "Ctype.set_object_name"

let remove_object_name ty =
  match (repr ty).desc with
    Tobject (_, nm)   -> nm := None
  | Tconstr (_, _, _) -> ()
  | _                 -> fatal_error "Ctype.remove_object_name"


                         (*****************************)
                         (*  Type level manipulation  *)
                         (*****************************)

(*
   It would be a bit more efficient to remove abbreviation expansions
   rather than generalizing them: these expansions will usually not be
   used anymore. However, this is not possible in the general case, as
   [expand_abbrev] (via [subst]) requires these expansions to be
   preserved. Does it worth duplicating this code ?
*)
let rec generalize ty =
  let ty = repr ty in
  if ty.level > !current_level then begin
    ty.level <- generic_level;
    begin match ty.desc with
      Tconstr (_, _, abbrev) ->
        generalize_expans !abbrev
    | _ -> ()
    end;
    iter_type_expr generalize ty
  end

and generalize_expans =
  function
    Mnil              ->  ()
  | Mcons(_, ty, rem) ->  generalize ty; generalize_expans rem
  | Mlink rem         ->  generalize_expans !rem

(*
   Lower in-place the level of a generic type. That way, [subst] can
   do "unification" on generic types.
*)
let rec ungeneralize ty =
  let ty = repr ty in
  if ty.level = generic_level then begin
    ty.level <- !current_level;
    begin match ty.desc with
      Tconstr (_, _, abbrev) ->
        ungeneralize_expans !abbrev
    | _ -> ()
    end;
    iter_type_expr ungeneralize ty
  end

and ungeneralize_expans =
  function
    Mnil              ->  ()
  | Mcons(_, ty, rem) ->  ungeneralize ty; ungeneralize_expans rem
  | Mlink rem         ->  ungeneralize_expans !rem

let expand_abbrev' = (* Forward declaration *)
  ref (fun env path args abbrev level -> raise Cannot_expand)

(* Lower the levels of a type. *)
(* Assume [level] is not [generic_level]. *)
(*
    The level of a type constructor must be higher than its binding
    time. That way, a type constructor cannot escape the scope of its
    definition, as would be the case in
      let x = ref []
      module M = struct type t let _ = (x : t list ref) end
*)
let rec update_level env level ty =
  let ty = repr ty in
  if ty.level > level then begin
    ty.level <- level;
    begin match ty.desc with
      Tconstr(p, tl, abbrev)  when level < Path.binding_time p ->
        (* Try first to replace an abbreviation by its expansion. *)
        begin try
          ty.desc <- Tlink (!expand_abbrev' env p tl abbrev ty.level)
          (* 
             [expand_abbrev] has already checked the level, so no need
             to recurse further.
           *)
        with Cannot_expand ->
          raise (Unify [])
        end
    | _ ->
        iter_type_expr (update_level env level) ty
    end;
  end

(* 
   Function [update_level] will never try to expand an abbreviation in
   this case ([current_level] is higher than the binding time of any
   type constructor path). So, it can be called with the empty
   environnement.
*)
let make_nongen ty = update_level Env.empty !current_level ty


                              (*******************)
                              (*  Instantiation  *)
                              (*******************)


(*
   Generic nodes are duplicated, while non-generic nodes are left
   as-is. The instance cannot be generic.
   During instantiation, the description of a generic node is first
   replaced by a link to a stub ([Tlink (newvar ())]). Once the copy
   is made, it replaces the stub.
   After instantiation, the description of generic node, which was
   stored in [saved_desc], must be put back, using [cleanup_types].
*)

let saved_desc = ref []
  (* Saved association of generic node with their description. *)
let abbreviations = ref (ref Mnil)
  (* Abbreviation memorized. *)

let rec copy =
  function  (* [repr] cannot be used here. *)
    {desc = Tlink ty'} ->
      copy ty'
  | ty when ty.level <> generic_level ->
      ty
  | ty ->
      let desc = ty.desc in
      saved_desc := (ty, desc)::!saved_desc;
      let t = newvar () in              (* Stub *)
      ty.desc <- Tlink t;
      t.desc <-
        begin match desc with
          Tvar ->
            Tvar
        | Tarrow (t1, t2) ->
            Tarrow (copy t1, copy t2)
        | Ttuple tl ->
            Ttuple (List.map copy tl)
        | Tconstr (p, tl, _) ->
            (*
               One must allocate a new reference, so that abbrevia-
               tions belonging to different branches of a type are
               independent.
               Moreover, a reference containing a [Mcons] must be
               shared, so that the memorized expansion of an abbrevi-
               ation can be released by changing the content of just
               one reference.
            *)
            Tconstr (p, List.map copy tl,
                     ref (match ! !abbreviations with
                            Mcons _ -> Mlink !abbreviations
                          | abbrev  -> abbrev))
        | Tobject (t1, {contents = name}) ->
            let name' =
              match name with
                None ->
                  None
              | Some (p, tl) ->
                  Some (p, List.map copy tl)
            in
            Tobject (copy t1, ref name')
        | Tfield (label, t1, t2) ->
            Tfield (label, copy t1, copy t2)
        | Tnil ->
            Tnil
        | Tlink t -> (* Actually unused *)
            Tlink (copy t)
        end;
      t

let cleanup_types () =
  List.iter (fun (ty, desc) -> ty.desc <- desc) !saved_desc;
  saved_desc := []

(**** Variants of instantiations ****)

let instance sch =
  let ty = copy sch in
  cleanup_types ();
  ty

let instance_list schl =
  let tyl = List.map copy schl in
  cleanup_types ();
  tyl

let instance_constructor cstr =
  let ty_res = copy cstr.cstr_res in
  let ty_args = List.map copy cstr.cstr_args in
  cleanup_types ();
  (ty_args, ty_res)

let instance_label lbl =
  let ty_res = copy lbl.lbl_res in
  let ty_arg = copy lbl.lbl_arg in
  cleanup_types ();
  (ty_arg, ty_res)

let instance_parameterized_type sch_args sch =
  let ty_args = List.map copy sch_args in
  let ty = copy sch in
  cleanup_types ();
  (ty_args, ty)

let instance_parameterized_type_2 sch_args sch_lst sch =
  let ty_args = List.map copy sch_args in
  let ty_lst = List.map copy sch_lst in
  let ty = copy sch in
  cleanup_types ();
  (ty_args, ty_lst, ty)

let instance_class cl =
  let params = List.map copy cl.cty_params in
  let args = List.map copy cl.cty_args in
  let vars =
    Vars.fold
      (fun lab (mut, ty) ->
         Vars.add lab (mut, copy ty))
      cl.cty_vars
      Vars.empty in
  let self = copy cl.cty_self in
  cleanup_types ();
  (params, args, vars, self)

(**** Instantiation with parameter substitution ****)

let unify' = (* Forward declaration *)
  ref (fun env ty1 ty2 -> raise (Unify []))

let rec subst env level abbrev path params args body =
  if level <> generic_level then begin
    let old_level = !current_level in
    current_level := level;
    let body0 = newvar () in          (* Stub *)
    begin match path with
      None      -> ()
    | Some path -> memorize_abbrev abbrev path body0
    end;
    abbreviations := abbrev;
    let (params', body') = instance_parameterized_type params body in
    abbreviations := ref Mnil;
    !unify' env body0 body';
    List.iter2 (!unify' env) params' args;
    current_level := old_level;
    body'
  end else begin
    (* One cannot expand directly to a generic type. *)
    begin_def ();
    (*
       Arguments cannot be generic either, as they are unified to the
       parameters.
    *)
    List.iter ungeneralize args;
    let ty = subst env !current_level abbrev path params args body in
    end_def ();
    generalize ty;
    ty
  end

(*
   Only the shape of the type matters, not whether is is generic or
   not. [generic_level] might be somewhat slower, but it ensures
   invariants on types are enforced (decreasing levels.), and we don't
   care about efficiency here.
*)
let substitute env params args body =
  subst env generic_level (ref Mnil) None params args body


                              (****************************)
                              (*  Abbreviation expansion  *)
                              (****************************)


(* Search whether the expansion has been memorized. *)
let rec find_expans p1 =
  function
    Mnil ->
      None
  | Mcons (p2, ty, _) when Path.same p1 p2 ->
      Some ty
  | Mcons (_, _, rem) ->
      find_expans p1 rem
  | Mlink {contents = rem} ->
      find_expans p1 rem

let previous_env = ref Env.empty

(* Expand an abbreviation. The expansion is memorized. *)
let expand_abbrev env path args abbrev level =
  (* 
     If the environnement has changed, memorized expansions might not
     be correct anymore, and so we flush the cache. This is safe but
     quite pessimistic: it would be enough to flush the cache at the
     ends of structures and signatures.
     XXX Do it !
  *)
  if env != !previous_env then begin
    cleanup_abbrev ();
    previous_env := env
  end;
  match find_expans path !abbrev with
    Some ty ->
      if level <> generic_level then
        update_level env level ty;
      ty
  | None ->
      let decl =
        (* XXX Do we really need a [try ... with] here ? *)
        try Env.find_type path env with Not_found -> raise Cannot_expand in
      match decl.type_manifest with
        Some body ->
          subst env level abbrev (Some path) decl.type_params args body
      | None ->
          raise Cannot_expand

let _ = expand_abbrev' := expand_abbrev

(* Recursively expand the root of a type. *)
let rec expand_root env ty =
  let ty = repr ty in
  match ty.desc with
    Tconstr(p, tl, abbrev) ->
      begin try
        expand_root env (expand_abbrev env p tl abbrev ty.level)
      with Cannot_expand ->
        ty
      end
  | _ ->
      ty

(* Recursively expand the root of a type.
   Also expand #-types. *)
(* XXX This is a hack ! *)
let rec full_expand env ty =
  let ty = repr (expand_root env ty) in
  match ty.desc with
    Tobject (fi, {contents = Some (_, v::_)}) when (repr v).desc = Tvar ->
      { desc = Tobject (fi, ref None); level = ty.level }
  | _ ->
      ty

(*
   Check whether the abbreviation expands to a well-defined type.
   During the typing of a class, abbreviations for correspondings
   types expand to non-generic types.
*)
let generic_abbrev env path =
  try
    let decl = Env.find_type path env in
    match decl.type_manifest with
      Some body -> (repr body).level = generic_level
    | None      -> false
  with
    Not_found ->
      false


                              (*****************)
                              (*  Unification  *)
                              (*****************)



(**** Occur check ****)

(* XXX A supprimer *)
exception Occur

let occur env ty0 ty =
  let visited = ref ([] : type_expr list) in
  let rec occur_rec ty =
    let ty = repr ty in
    if ty == ty0 then raise Occur;
    match ty.desc with
      Tvar ->
        ()
    | Tarrow(t1, t2) ->
        occur_rec t1; occur_rec t2
    | Ttuple tl ->
        List.iter occur_rec tl
    | Tconstr(p, [], abbrev) ->
        ()
    | Tconstr(p, tl, abbrev) ->
        if not (List.memq ty !visited) then begin
          visited := ty :: !visited;
          try List.iter occur_rec tl with Unify _ ->
          try
            let ty' = expand_abbrev env p tl abbrev ty.level in
            occur_rec ty'
          with Cannot_expand -> ()
        end
    | Tobject (_, _) | Tfield (_, _, _) | Tnil ->
        ()
    | Tlink _ ->
        fatal_error "Ctype.occur"
  in
    occur_rec ty

(**** Transform error trace ****)
(* XXX Move it to some other place ? *)

let expand_trace env trace =
  List.fold_right
    (fun (t1, t2) rem ->
       (repr t1, full_expand env t1)::(repr t2, full_expand env t2)::rem)
    trace []

let rec filter_trace =
  function
    (t1, t1')::(t2, t2')::rem ->
      let rem' = filter_trace rem in
      if (t1 == t1') & (t2 == t2')
      then rem'
      else (t1, t1')::(t2, t2')::rem'
  | _ ->
      []

(**** Unification ****)

(* Return whether [t0] occurs in [ty]. *)
(* XXX Renommer en occur ? *)
let deep_occur t0 ty =
  let rec occur_rec ty =
    let ty = repr ty in
    if ty.level >= 0 then begin
      if ty == t0 then raise Occur;
      ty.level <- -1 - ty.level;
      iter_type_expr occur_rec ty
    end
  and cleanup ty =
    let ty = repr ty in
    if ty.level < 0 then begin
      ty.level <- -1 - ty.level;
      iter_type_expr cleanup ty
    end
  in
  try
    occur_rec ty; cleanup ty; false
  with Occur ->
    cleanup ty; true

(* Fully expand the head of a type. *)
(* XXX Deplacer vers expand_abbrev... *)
let rec expand env ty =
  let ty = repr ty in
  match ty.desc with
    Tconstr(p, tl, abbrev) ->
      begin try
        expand env (expand_abbrev env p tl abbrev ty.level)
      with Cannot_expand ->
        ty
      end
  | _ ->
      ty

(*
   1. When unifying two non-abbreviated types, one type is made a link
      to the other. When unifying an abbreviated type with a
      non-abbreviated type, the non-abbreviated type is made a link to
      the other one. When unifying to abbreviated types, these two
      types are kept distincts, but they are made to (temporally)
      expand to the same type.
   2. Abbreviations with at least one parameter are systematically
      expanded. The overhead does not seem to high, and that way
      abbreviations where some parameters does not appear in the
      expansion, such as ['a t = int], are correctly handled. In
      particular, for this example, unifying ['a t] with ['b t] keeps
      ['a] and ['b] distincts. (Is it really important ?)
   3. Unifying an abbreviation ['a t = 'a] with ['a] should not yield
      ['a t as 'a]. Indeed, the type variable would otherwise be lost.
      This problem occurs for abbreviations expanding to a type
      variable, but also to many other constrained abbreviations (for
      instance, [(< x : 'a > -> unit) t = <x : 'a>]). The solution is
      that, if an abbreviation is unified with some subpart of its
      parameters, then the parameter actually does not get
      abbreviated.  It would be possible to check whether some
      information is indeed lost, but it probably does not worth it.
*)
let rec unify env t1 t2 =
  (* First step: special cases (optimizations) *)
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else

  try
    match (t1.desc, t2.desc) with
      (Tvar, Tconstr _) when deep_occur t1 t2 ->
        unify2 env t1 t2
    | (Tconstr _, Tvar) when deep_occur t2 t1 ->
        unify2 env t1 t2
    | (Tvar, _) ->
        begin try occur env t1 t2 with Occur ->
          raise (Unify [])
        end;
        update_level env t1.level t2;
        t1.desc <- Tlink t2
    | (_, Tvar) ->
        begin try occur env t2 t1 with Occur ->
          raise (Unify [])
        end;
        update_level env t2.level t1;
        t2.desc <- Tlink t1
    | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
        update_level env t1.level t2;
        t1.desc <- Tlink t2
    | _ ->
        unify2 env t1 t2
  with Unify trace ->
    raise (Unify ((t1, t2)::trace))

and unify2 env t1 t2 =
  (* Second step: expansion of abbreviations *)
  let t1' = expand env t1 in
  let t2' = expand env t2 in
  if t1' == t2' then () else

  if (t1 == t1') || (t2 != t2') then
    unify3 env t1 t1' t2 t2'
  else
    try unify3 env t2 t2' t1 t1' with Unify trace ->
      raise (Unify (List.map (fun (x, y) -> (y, x)) trace))

and unify3 env t1 t1' t2 t2' =
  (* Third step: truly unification *)
  (* Assumes either [t1 == t1'] or [t2 != t2'] *)
  let d1 = t1'.desc and d2 = t2'.desc in
  
  if (t2 != t2') && (deep_occur t1' t2) then begin
    (* See point 3. *)
    update_level env t1'.level t2';
    t1'.desc <- Tlink t2'
  end else begin
    update_level env t1'.level t2;
    t1'.desc <- Tlink t2
  end;

  try
    begin match (d1, d2) with
      (Tvar, _) ->
        begin try occur env t1' t2 with Occur ->
          raise (Unify [])
        end
    | (_, Tvar) ->
        begin try occur env t2' (newty d1) with Occur ->
          raise (Unify [])
        end;
        if t1 == t1' then begin
          (* The variable must be instantiated... *)
          let ty = {desc = d1; level = t1'.level} in
          update_level env t2'.level ty;
          t2'.desc <- Tlink ty
        end else begin
          t1'.desc <- d1;
          update_level env t2'.level t1;
          t2'.desc <- Tlink t1
        end
    | (Tarrow (t1, u1), Tarrow (t2, u2)) ->
        unify env t1 t2; unify env u1 u2
    | (Ttuple tl1, Ttuple tl2) ->
        unify_list env tl1 tl2
    | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _)) when Path.same p1 p2 ->
        unify_list env tl1 tl2
    | (Tobject (fi1, nm1), Tobject (fi2, nm2)) ->
        unify_fields env fi1 fi2;
        begin match !nm2 with
          Some (_, va::_) when (repr va).desc = Tvar -> ()
        | _                                          -> nm2 := !nm1
        end
    | (Tfield _, Tfield _) ->
        unify_fields env t1 t2
    | (Tnil, Tnil) ->
        ()
    | (_, _) ->
        raise (Unify [])
    end;
(*
    (* 
       Can only be done afterwards, once the row variable has
       (possibly) been instantiated.
    *)
    if t1 != t1' (* && t2 != t2' *) then begin
      match (t1.desc, t2.desc) with
        (Tconstr (p, ty::_, _), _)
            when ((repr ty).desc != Tvar)
              && weak_abbrev p
              && not (deep_occur t1 t2) ->
          update_level env t1.level t2;
          t1.desc <- Tlink t2
      | (_, Tconstr (p, ty::_, _))
            when ((repr ty).desc != Tvar)
              && weak_abbrev p
              && not (deep_occur t2 t1) ->
          update_level env t2.level t1;
          t2.desc <- Tlink t1;
          t1'.desc <- Tlink t2'
      | _ ->
          ()
    end
*)
  with Unify trace ->
    t1'.desc <- d1;
    raise (Unify trace)

and unify_list env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (unify env) tl1 tl2

and unify_fields env ty1 ty2 =          (* Optimization *)
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  let va = newvar () in
  unify env rest1 (build_fields miss2 va);
  unify env (build_fields miss1 va) rest2;
  List.iter (fun (t1, t2) -> unify env t1 t2) pairs

let unify env ty1 ty2 =
  try
    unify env ty1 ty2
  with Unify trace ->
    let trace = expand_trace env trace in
    match trace with
      t1::t2::rem ->
        raise (Unify (t1::t2::filter_trace rem))
    | _ ->
        fatal_error "Ctype.unify"

let _ = unify' := unify

(**** Special cases of unification ****)

(* Unify [t] and ['a -> 'b]. Return ['a] and ['b]. *)
let rec filter_arrow env t =
  let t = repr t in
  match t.desc with
    Tvar ->
      let t1 = newvar () and t2 = newvar () in
      let t' = newty (Tarrow (t1, t2)) in
      update_level env t.level t';
      t.desc <- Tlink t';
      (t1, t2)
  | Tarrow(t1, t2) ->
      (t1, t2)
  | Tconstr(p, tl, abbrev) ->
      begin try
        filter_arrow env (expand_abbrev env p tl abbrev t.level)
      with Cannot_expand ->
        raise (Unify [])
      end
  | _ ->
      raise (Unify [])

(* Used by [filter_method]. *)
let rec filter_method_field env name ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      let ty1 = newvar () and ty2 = newvar () in
      let ty' = newty (Tfield (name, ty1, ty2)) in
      update_level env ty.level ty';
      ty.desc <- Tlink ty';
      ty1
  | Tfield(n, ty1, ty2) ->
      if n = name then
        ty1
      else
        filter_method_field env name ty2
  | _ ->
      raise (Unify [])

(* Unify [ty] and [< name : 'a; .. >]. Return ['a]. *)
let rec filter_method env name ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      let ty1 = newvar () in
      let ty' = newobj ty1 in
      update_level env ty.level ty';
      ty.desc <- Tlink ty';
      filter_method_field env name ty1
  | Tobject(f, _) ->
      filter_method_field env name f
  | Tconstr(p, tl, abbrev) ->
      begin try
        filter_method env name (expand_abbrev env p tl abbrev ty.level)
      with Cannot_expand ->
        raise (Unify [])
      end
  | _ ->
      raise (Unify [])


                        (***********************************)
                        (*  Matching between type schemes  *)
                        (***********************************)

(* XXX A voir... *)


(* XXX This is not really an occur check !!! *)
let moregen_occur env ty0 ty =
  let visited = ref [] in
  let rec occur_rec ty =
    let ty = repr ty in
    if not (List.memq ty !visited) then begin
      visited := ty::!visited;
      begin match ty.desc with
        Tvar when ty.level = generic_level & ty0.level < !current_level ->
          (* ty0 has level = !current_level iff it is generic
             in the original type scheme. In this case, it can be freely
             instantiated. Otherwise, ty0 is not generic
             and cannot be instantiated by a type that contains
             generic variables. *)
          raise (Unify [])
      | Tconstr(p, tl, abbrev) ->
          (* XXX Pourquoi expanser ? *)
          begin try
            List.iter occur_rec tl
          with Unify lst ->
            let ty' =
              try expand_abbrev env p tl abbrev ty.level
              with Cannot_expand -> raise (Unify lst) in
            occur_rec ty'
          end
      | _ ->
          iter_type_expr occur_rec ty
      end
    end
  in
    occur_rec ty

let rec moregen env t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else
  let d1 = t1.desc in
  try
    begin match (t1.desc, t2.desc) with
      (Tvar, _) ->
        if t1.level = generic_level then raise (Unify []);
        moregen_occur env t1 t2;
        t1.desc <- Tlink t2
    | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
        moregen env t1 t2; moregen env u1 u2
    | (Ttuple tl1, Ttuple tl2) ->
        moregen_list env tl1 tl2
    | (Tconstr(p1, tl1, abbrev1), Tconstr(p2, tl2, abbrev2)) ->
        if Path.same p1 p2 then begin
          try
            t1.desc <- Tlink t2;
            moregen_list env tl1 tl2;
            t1.desc <- d1
          with Unify lst ->
          t1.desc <- d1;
          try
            moregen env (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
          with Cannot_expand ->
          try
            moregen env t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
          with Cannot_expand ->
            raise (Unify lst)
        end else begin
          try
            moregen env (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
          with Cannot_expand ->
          try
            moregen env t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
          with Cannot_expand ->
            raise (Unify [])
        end
    | (Tobject(f1, _), Tobject(f2, _)) ->
        t1.desc <- Tlink t2;
        moregen_fields env f1 f2
    | (Tconstr(p1, tl1, abbrev1), _) ->
        begin try
          moregen env (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
        with Cannot_expand ->
          raise (Unify [])
        end
    | (_, Tconstr(p2, tl2, abbrev2)) ->
        begin try
          moregen env t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
        with Cannot_expand ->
          raise (Unify [])
        end
    | (_, _) ->
        raise (Unify [])
    end
  with exn ->
    t1.desc <- d1;
    raise exn

and moregen_list env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (moregen env) tl1 tl2

and moregen_fields env ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  if miss1 <> [] then raise (Unify []);
  begin match rest1.desc with
    Tvar ->
      if rest1.level = generic_level then raise (Unify []);
      let fi = build_fields miss2 rest2 in
      moregen_occur env rest1 fi
  | Tnil ->
      if miss2 <> [] then raise (Unify []);
      if rest2.desc <> Tnil then raise (Unify [])
  | _ ->
      fatal_error "moregen_fields"
  end;
  List.iter (fun (t1, t2) -> moregen env t1 t2) pairs

let moregeneral env sch1 sch2 =
  begin_def();
  try
    moregen env (instance sch1) sch2;
    end_def();
    true
  with Unify _ ->
    end_def();
    false


                 (*********************************************)
                 (*  Equivalence between parameterized types  *)
                 (*********************************************)


(* XXX A voir... *)

(* Deux modes : avec ou sans subtitution *)
(* Equalite de deux listes de types :    *)
(*   [Ctype.equal env rename tyl1 tyl2]  *)

let equal env rename tyl1 tyl2 =
  let subst = ref [] in
  let type_pairs = ref [] in
  let rec eqtype t1 t2 =
    let t1 = repr t1 in
    let t2 = repr t2 in
    List.exists (function (t1', t2') -> t1 == t1' & t2 == t2') !type_pairs
          (* XXX May be slow... *)
      ||
    begin
      type_pairs := (t1, t2) :: !type_pairs;
      match (t1.desc, t2.desc) with
        (Tvar, Tvar) ->
          if rename then begin
            try
              List.assq t1 !subst == t2
            with Not_found ->
              subst := (t1, t2) :: !subst;
              true
          end else
            t1 == t2
      | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
          eqtype t1 t2 & eqtype u1 u2
      | (Ttuple tl1, Ttuple tl2) ->
          eqtype_list tl1 tl2
      | (Tconstr(p1, tl1, abbrev1), Tconstr(p2, tl2, abbrev2)) ->
          (Path.same p1 p2 && eqtype_list tl1 tl2)
            ||
          begin
            try
              eqtype (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
            with Cannot_expand ->
            try
              eqtype t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
            with Cannot_expand ->
              false
          end
      | (Tobject (f1, _), Tobject (f2, _)) ->
          eqtype_fields f1 f2
      | (Tconstr(p1, tl1, abbrev1), _) ->
          begin try
            eqtype (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
          with Cannot_expand ->
            false
          end
      | (_, Tconstr(p2, tl2, abbrev2)) ->
          begin try
            eqtype t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
          with Cannot_expand ->
            false
          end
      | (Tfield _, Tfield _) ->
          eqtype_fields t1 t2
      | (Tnil, Tnil) ->
          true
      | (_, _) ->
          false
    end
  and eqtype_list tl1 tl2 =
    match (tl1, tl2) with
      ([], []) -> true
    | (t1::r1, t2::r2) -> eqtype t1 t2 && eqtype_list r1 r2
    | (_, _) -> false
  and eqtype_fields ty1 ty2 =           (* Optimization *)
    let (fields1, rest1) = flatten_fields ty1
    and (fields2, rest2) = flatten_fields ty2 in
    let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
    List.length fields1 = List.length fields2
      &&
    eqtype rest1 rest2
      &&
    (miss1 = []) & (miss2 = [])
      &&
    List.for_all (function (t1, t2) -> eqtype t1 t2) pairs
  in
    List.length tyl1 = List.length tyl2
                    &&
    List.for_all2 eqtype tyl1 tyl2


                              (***************)
                              (*  Subtyping  *)
                              (***************)


(**** Build a subtype of a given type. ****)

let subtypes = ref []

(* XXX Types r�cursifs ? *)
let rec build_subtype env t =
  let t = repr t in
  match t.desc with
    Tlink t' ->                         (* Redundant ! *)
      build_subtype env t'
  | Tvar ->
      (t, false)
  | Tarrow(t1, t2) ->
      let (t1', c1) = (t1, false) in
      let (t2', c2) = build_subtype env t2 in
      if c1 or c2 then (new_global_ty (Tarrow(t1', t2')), true)
      else (t, false)
  | Ttuple tlist ->
      let (tlist', clist) =
        List.split (List.map (build_subtype env) tlist)
      in
      if List.exists (function c -> c) clist then
        (new_global_ty (Ttuple tlist'), true)
      else (t, false)
  | Tconstr(p, tl, abbrev) when generic_abbrev env p ->
      let t' = expand_abbrev env p tl abbrev t.level in
      let (t'', c) = build_subtype env t' in
      if c then (t'', true)
      else (t, false)
  | Tconstr(p, tl, abbrev) ->
      (t, false)
  | Tobject (t1, _) when opened_object t1 ->
      (t, false)
  | Tobject (t1, _) ->
      (begin try
         List.assq t !subtypes
       with Not_found ->
         let t' = new_global_var () in
         subtypes := (t, t')::!subtypes;
         let (t1', _) = build_subtype env t1 in
         t'.desc <- Tobject (t1', ref None);
         t'
       end,
       true)
  | Tfield(s, t1, t2) ->
      let (t1', _) = build_subtype env t1 in
      let (t2', _) = build_subtype env t2 in
      (new_global_ty (Tfield(s, t1', t2')), true)
  | Tnil ->
      let v = new_global_var () in
      (v, true)

let enlarge_type env ty =
  subtypes := [];
  let (ty', _) = build_subtype env ty in
  subtypes := [];
  ty'

(**** Check whether a type is a subtype of another type. ****)

(*
    During the traversal, a trace of visited types is maintained. It
    is printed in case of error.
    Constraints (pairs of types that must be equals) are accumulated
    rather than being enforced straight. Indeed, the result would
    otherwise depend on the order in which these constraints are
    enforced.
    A function enforcing these constraints is returned. That way, type
    variables can be bound to their actual values before this function
    is called (see Typecore).
    Only well-defined abbreviations are expanded (hence the tests
    [generic_abbrev ...]).
*)

let subtypes = ref [];;

let subtype_error env trace =
  raise (Subtype (expand_trace env (List.rev trace), []))

let rec subtype_rec env trace t1 t2 =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then [] else
  if List.exists (fun (t1', t2') -> t1 == t1' & t2 == t2') !subtypes then
    []
  else begin
    subtypes := (t1, t2) :: !subtypes;
    match (t1.desc, t2.desc) with
      (Tvar, _) | (_, Tvar) ->
        [(trace, t1, t2)]
    | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
        (subtype_rec env ((t2, t1)::trace) t2 t1) @
        (subtype_rec env ((u1, u2)::trace) u1 u2)
    | (Ttuple tl1, Ttuple tl2) ->
        subtype_list env trace tl1 tl2
    | (Tconstr(p1, tl1, abbrev1), Tconstr _) when generic_abbrev env p1 ->
        subtype_rec env trace (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
    | (Tconstr _, Tconstr(p2, tl2, abbrev2)) when generic_abbrev env p2 ->
        subtype_rec env trace t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
    | (Tconstr _, Tconstr _) ->
        [(trace, t1, t2)]
    | (Tconstr(p1, tl1, abbrev1), _) when generic_abbrev env p1 ->
        subtype_rec env trace (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
    | (_, Tconstr (p2, tl2, abbrev2)) when generic_abbrev env p2 ->
        subtype_rec env trace t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
    | (Tobject (f1, _), Tobject (f2, _))
              when opened_object f1 & opened_object f2 ->
        (* Same row variable implies same object. *)
        [(trace, t1, t2)]
    | (Tobject (f1, _), Tobject (f2, _)) ->
        subtype_fields env trace f1 f2
    | (_, _) ->
        subtype_error env trace
  end

and subtype_list env trace tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    subtype_error env trace;
  List.fold_left2
    (fun cstrs t1 t2 -> cstrs @ (subtype_rec env ((t1, t2)::trace) t1 t2))
    [] tl1 tl2

and subtype_fields env trace ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1 in
  let (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  begin match rest1.desc with
    Tvar   -> [(trace, rest1, build_fields miss2 (newvar ()))]
  | Tnil   -> if miss2 = [] then [] else subtype_error env trace
  | _      -> fatal_error "Ctype.subtype_fields (1)"
  end
    @
  begin match rest2.desc with
    Tvar   -> [(trace, build_fields miss1 (rest1), rest2)]
  | Tnil   -> []
  | _      -> fatal_error "Ctype.subtype_fields (2)"
  end
    @
  (List.fold_left
     (fun cstrs (t1, t2) -> cstrs @ (subtype_rec env ((t1, t2)::trace) t1 t2))
     [] pairs)

let subtype env ty1 ty2 =
  subtypes := [];
  (* Build constraint set. *)
  let cstrs = subtype_rec env [(ty1, ty2)] ty1 ty2 in
  (* Enforce constraints. *)
  function () ->
    List.iter
      (function (trace0, t1, t2) ->
         try unify env t1 t2 with Unify trace ->
           raise (Subtype (expand_trace env (List.rev trace0),
                           List.tl (List.tl trace))))
      cstrs;
    subtypes := []


                              (*************************)
                              (*  Remove dependencies  *)
                              (*************************)


let inst_subst = ref ([] : (type_expr * type_expr) list)

(* XXX A voir... *)
(* XXX Petit probleme... (deroulement) *)
(*     module F(X : sig type t end) = struct type t = X.t end;;       *)
(*     module M = F(struct type t = <x : t> end);;                    *)
(*  -> module M : sig type t = < x : < x : 'a > as 'a > end  *)
let rec nondep_type_rec env id ty =
  let ty = repr ty in
  if ty.desc = Tvar then ty else
  try newgenty (Tlink (List.assq ty !inst_subst)) with Not_found ->
            (* Tlink important permet de ne pas modifier la variable *)
    let ty' = newgenvar () in
    inst_subst := (ty, ty') :: !inst_subst;
    ty'.desc <-
      begin match ty.desc with
        Tvar ->
          Tvar
      | Tarrow(t1, t2) ->
          Tarrow(nondep_type_rec env id t1, nondep_type_rec env id t2)
      | Ttuple tl ->
          Ttuple(List.map (nondep_type_rec env id) tl)
      | Tconstr(p, tl, abbrev) ->
          if Path.isfree id p then
            begin try
              (nondep_type_rec env id
                 (expand_abbrev env p tl abbrev ty.level)).desc
            with Cannot_expand ->
              raise Not_found
            end
          else
            Tconstr(p, List.map (nondep_type_rec env id) tl, ref Mnil)
      | Tobject (t1, name) ->
          Tobject (nondep_type_rec env id t1,
                 ref (match !name with
                        None -> None
                      | Some (p, tl) ->
                          if Path.isfree id p then None
                          else Some (p, List.map (nondep_type_rec env id) tl)))
       | Tfield(label, t1, t2) ->
           Tfield(label, nondep_type_rec env id t1, nondep_type_rec env id t2)
       | Tnil ->
           Tnil
       | Tlink _ ->
           fatal_error "Ctype.nondep_type"
       end;
     ty'

let nondep_type env id ty =
  inst_subst := [];
  let ty' = nondep_type_rec env id ty in
  inst_subst := [];
  ty'

let nondep_class_type env id decl =
  inst_subst := [];
  let decl =
    { cty_params = List.map (nondep_type_rec env id) decl.cty_params;
      cty_args = List.map (nondep_type_rec env id) decl.cty_args;
      cty_vars =
        Vars.fold (fun l (m, t) -> Vars.add l (m, nondep_type_rec env id t))
          decl.cty_vars Vars.empty;
      cty_self = nondep_type_rec env id decl.cty_self;
      cty_concr = decl.cty_concr;
      cty_new =
        begin match decl.cty_new with
          None    -> None
        | Some ty -> Some (nondep_type_rec env id ty)
        end }
  in
  inst_subst := [];
  decl


                              (******************************)
                              (*  Abbreviation correctness  *)
                              (******************************)


exception Nonlinear_abbrev
exception Recursive_abbrev

(* XXX A supprimer... *)
let rec non_recursive_abbrev env path constrs ty =
  let ty = repr ty in
  match ty.desc with
    Tarrow (ty1, ty2) ->
      non_recursive_abbrev env path constrs ty1;
      non_recursive_abbrev env path constrs ty2
  | Ttuple tl ->
      List.iter (non_recursive_abbrev env path constrs) tl
  | Tconstr(p, args, abbrev) ->
      if Path.same path p then
        raise Recursive_abbrev
      else begin
        begin try
          let ty' = expand_abbrev env p args abbrev ty.level in
          if List.memq ty' constrs then () else
            non_recursive_abbrev env path (ty'::constrs) ty'
        with Cannot_expand ->
          ()
        end
      end
  | _ (* Tvar | Tobject (_, _) | Tfield (_, _, _) | Tnil *) ->
      ()

let correct_abbrev env ident params ty =
  let path = Path.Pident ident in
  non_recursive_abbrev env path [] ty


                              (*******************)
                              (*  Miscellaneous  *)
                              (*******************)


let unalias ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      ty
  | _ ->
      {desc = ty.desc; level = ty.level}

let unroll_abbrev id tl ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      ty
  | _ ->
      let ty' = {desc = ty.desc; level = ty.level} in
      ty.desc <- Tlink {desc = Tconstr (Path.Pident id, tl, ref Mnil);
                        level = ty.level};
      ty'

type closed_schema_result = Var of type_expr | Row_var of type_expr
exception Failed of closed_schema_result

let visited = ref []

let rec closed_schema_rec ty =
  let ty = repr ty in
  if not (List.memq ty !visited) then begin
    visited := ty::!visited;
    match ty.desc with
      Tvar when ty.level != generic_level -> raise (Failed (Var ty))
    | Tobject(f, {contents = Some (_, p)}) ->
        begin try closed_schema_rec f with
          Failed (Row_var v) -> raise (Failed (Var v))
        | Failed (Var v) -> raise (Failed (Row_var v))
        end;
        List.iter closed_schema_rec p
    | Tobject(f, _) ->
        begin try closed_schema_rec f with
          Failed (Row_var v) -> raise (Failed (Var v))
        | Failed (Var v) -> raise (Failed (Row_var v))
        end
    | Tfield(_, t1, t2) ->
        begin try
          closed_schema_rec t1
        with
          Failed (Row_var v) -> raise (Failed (Var v))
        | Failed (Var v) -> raise (Failed (Row_var v))
        end;
        closed_schema_rec t2
    | _ ->
        iter_type_expr closed_schema_rec ty
  end

let closed_schema ty =
  visited := [];
  try
    closed_schema_rec ty;
    visited := [];
    true
  with Failed _ ->
    visited := [];
    false

let closed_schema_verbose ty =
  visited := [];
  try
    closed_schema_rec ty;
    visited := [];
    None
  with Failed status ->
    visited := [];
    Some status

let is_generic ty =
  let ty = repr ty in
  match ty.desc with
    Tvar -> ty.level = generic_level
  | _ -> fatal_error "Ctype.is_generic"

let rec arity ty =
  match (repr ty).desc with
    Tarrow(t1, t2) -> 1 + arity t2
  | _ -> 0
