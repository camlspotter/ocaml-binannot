open Utils

module Location_bound = struct
  (* Some objects lack their location positions in typedtree,
     and Location_bound.upperbound tries to provide better approximation for them
  *)
  open Location
  let upperbound loc by = { loc with loc_end = by.loc_start }
end

module Kind = struct
  type t = 
    | Value | Type | Exception 
    | Module | Module_type 
    | Class | Class_type
end

module Abstraction = struct
  (* module definition abstraction *)

  (* CR jfuruse: types may be incompatible between compiler versions *)
  type module_expr = 
    | Mod_ident of Path.t (* module M = N *)
    | Mod_packed of string (* full path *)
        (* -pack overrides load paths: ocamlc -pack dir1/dir2/dir3/x.cmo *)
    | Mod_structure of structure (* module M = struct ... end *)
    | Mod_functor of Ident.t * Types.module_type * module_expr (* module M(I:S) = *)
    | Mod_apply of module_expr * module_expr (* module M = N(O) *)
    | Mod_constraint of module_expr * Types.module_type
    | Mod_unpack of Types.module_type
    | Mod_abstract (* used for Tmodtype_abstract *)

  (* structure abstraction : name - defloc asoc list *)
  and structure = structure_item list

  (* modtype must be identified from module, since they can have the
     same name *) 

  and structure_item = 
    | Str_value of Ident.t
    | Str_type of Ident.t
    | Str_exception of Ident.t
    | Str_module of Ident.t * module_expr
    | Str_modtype of Ident.t * module_expr
    | Str_class of Ident.t
    | Str_cltype of Ident.t
    | Str_include of module_expr * (Kind.t * Ident.t) list

  module Module_expr = struct
    (* cache key is Typedtree.module_expr *)
    module M = struct
      type t = Typedtree.module_expr
      let equal m1 m2 = m1 == m2
      let hash_source m = m.Typedtree.mod_loc
      let hash m = Hashtbl.hash (hash_source m)
    end
    include M
    module Table = Hashtbl.Make(M)
  end

  module Structure_item = struct
    (* cache key is Abstraction.structure_item, not Typedtree.structure_item *)
    module M = struct
      type t = structure_item
      let equal s1 s2 =
	match s1, s2 with
	| Str_value id1, Str_value id2 
	| Str_type id1, Str_type id2 
	| Str_exception id1, Str_exception id2
	| Str_class id1, Str_class id2
	| Str_cltype id1, Str_cltype id2 -> id1 = id2
	| Str_module (id1, mexp1) , Str_module (id2, mexp2) ->
	    id1 = id2 && Module_expr.equal mexp1 mexp2
	| Str_modtype (id1, mty1), Str_modtype (id2, mty2) ->
            id1 = id2 && Module_expr.equal mty1 mty2
	| Str_include (mexp1, kids1), Str_include (mexp2, kids2) ->
	    Module_expr.equal mexp1 mexp2 && kids1 = kids2
	| (Str_value _ | Str_type _ | Str_exception _ | Str_modtype _ 
	  | Str_class _ | Str_cltype _ | Str_module _ | Str_include _),
	  (Str_value _ | Str_type _ | Str_exception _ | Str_modtype _ 
	  | Str_class _ | Str_cltype _ | Str_module _ | Str_include _) -> false

      let hash = Hashtbl.hash
    end
    include M
    module Table = Hashtbl.Make(M)
  end

  open Types
  open Typedtree

  let cache_module_expr = Module_expr.Table.create 31
  let cache_structure_item = Structure_item.Table.create 31

  let included_sig_identifier_table = Hashtbl.create 31

(* CR jfuruse: TODO 
  let kident_of_sigitem = function
    | Tsig_value (id, _) -> Kind.Value, id
    | Tsig_exception (id, _) -> Kind.Exception, id
    | Tsig_module (id, _) ->  Kind.Module, id
    | Tsig_class (id, _) -> assert false
    | Tsig_type (id, _, _) -> Kind.Type, id
    | Tsig_modtype (id, _) -> Kind.Module_type, id
    | Tsig_cltype (id, _, _) -> Kind.Class_type, id
*)

  let rec module_expr mexp =
    try
      match Module_expr.Table.find cache_module_expr mexp with
      | None ->
          (* When a module definition finds itself in itself.
             Impossible to happen, so far. *)
          assert false
      | Some v -> v
    with
    | Not_found ->
	Module_expr.Table.replace cache_module_expr mexp None;
	let res = module_expr_sub mexp in
	Module_expr.Table.replace cache_module_expr mexp (Some res);
        res

  and module_expr_sub mexp = 
    match mexp.mod_desc with
    | Tmod_ident p -> Mod_ident p
    | Tmod_structure { str_items = str_items; str_type = _str_sig (* CR jfuruse: TODO *) } ->
	(* This may recompute abstractions of structure_items.
	   It sounds inefficient but not so much actually, since
	   module_expr is nicely cached. *)
	Mod_structure (List.flatten (List.map structure_item str_items))
    | Tmod_functor (id, mty, mexp) ->
        let mty = Mtype.scrape mexp.mod_env mty.mty_type in (* CR jfuruse: now loc is available! *)
	Mod_functor(id, mty, module_expr mexp)
    | Tmod_apply (mexp1, mexp2, _mcoercion) -> (* CR jfuruse ? *)
	Mod_apply (module_expr mexp1, module_expr mexp2)
    | Tmod_constraint (mexp, mty, _mtyconst, (* CR jfuruse: TODO *) _mcoercion) ->
	Mod_constraint (module_expr mexp, mty)
    | Tmod_unpack (_expr, mty) -> Mod_unpack mty

  and structure_item sitem = 
    (* it may recompute the same thing, but it is cheap *)
    let sitems = structure_item_sub sitem in
    (* make the same result (==) *)
    let equalize sitem =
      try
	Structure_item.Table.find cache_structure_item sitem
      with
      | Not_found -> 
	  Structure_item.Table.replace cache_structure_item sitem sitem;
	  sitem
    in
    List.map equalize sitems
    
  and structure_item_sub sitem = 
    (* CR jfuruse: now sitem.str_loc is available! *)
    match sitem.str_desc with
    | Tstr_eval _ -> []
    | Tstr_value (_, pat_exps) ->
	List.map (fun id -> Str_value id) (let_bound_idents pat_exps)
    | Tstr_primitive (id, _vdesc) -> 
	[Str_value id]
    | Tstr_type (id_descs) -> 
	List.map (fun (id, _) -> Str_type id) id_descs 
    | Tstr_exception (id ,_) ->
	[Str_exception id]
    | Tstr_exn_rebind (id, _path) -> (* CR jfuruse: path? *)
	[Str_exception id]
    | Tstr_module (id, mexp) ->
	[Str_module (id, module_expr mexp)]
    | Tstr_recmodule (idmexps) ->
	List.map (fun (id, _mty, (* CR jfuruse: todo *) mexp) ->
	  Str_module (id, module_expr mexp)) idmexps
    | Tstr_modtype (id, mty) ->
	[Str_modtype (id, module_type mty.mty_type (* CR jfuruse: todo *))]
    | Tstr_open _ -> []
(* CR jfuruse: todo
    | Tstr_class classdescs ->
	List.map (fun (class_decl, _names, _vflag) -> Str_class id) classdescs
*)
    | Tstr_class _ -> [] 
    | Tstr_class_type iddecls ->
	List.map (fun (id, _) -> Str_cltype id) iddecls
    | Tstr_include (mexp, _ids(*, sg*)) ->
	(* CR jfuruse: TODO
           let kids = List.map kident_of_sigitem sg in *)
        let kids = [] in
        [Str_include (module_expr mexp, kids)]

  (* CR jfuruse: caching like module_expr_sub *)
  and module_type : Types.module_type -> _ = function
    | Mty_ident p -> Mod_ident p
    | Mty_signature sg -> 
	Mod_structure (List.filter_map signature_item sg)
    | Mty_functor (id, mty1, mty2) ->
        (* CR jfuruse: need to scrape ? but how ? *)
        Mod_functor(id, mty1, module_type mty2)

  and signature_item (sitem : Types.signature_item) = 
    try
      match sitem with
      | Sig_value (id, _)
      | Sig_type (id, _, _)
      | Sig_exception (id, _)
      | Sig_module (id, _ , _)
      | Sig_modtype (id, _)
      | Sig_class (id, _, _)
      | Sig_class_type (id, _, _) ->
	  (* Sigitem might be defined by include, but it is not recorded
	     in signature. We here try to recover it. *)
	  (* CR jfuruse: included modules may listed more than once *)
	  let sitem, recorded = Hashtbl.find included_sig_identifier_table id in
          if !recorded then None
          else begin
            recorded := true;
            Some sitem
          end
    with
    | Not_found ->  Some (signature_item_sub sitem)
	
  and signature_item_sub = function
    | Sig_value (id, _) -> Str_value id
    | Sig_type (id, _, _) -> Str_type id
    | Sig_exception (id, _) -> Str_exception id
    | Sig_module (id, mty , _) -> Str_module (id, module_type mty)
    | Sig_modtype (id, mty_decl) -> (* todo *) Str_modtype (id, modtype_declaration mty_decl)
    | Sig_class (id, _, _) -> Str_class id
    | Sig_class_type (id, _, _) -> Str_cltype id

  and modtype_declaration = function
    | Modtype_abstract -> Mod_abstract
    | Modtype_manifest mty -> module_type mty

end

let protect name f v =
  try f v with e ->
    Format.eprintf "Error: %s: %s@." name (Printexc.to_string e)
    
module Annot = struct
  type t =
    | Type of Types.type_expr (* sub-expression's type *)
    | Str of Abstraction.structure_item 
    | Use of Kind.t * Path.t
    | Module of Abstraction.module_expr
    | Functor_parameter of Ident.t
    | Non_expansive of bool
    | Mod_type of Types.module_type

  let equal t1 t2 =
    match t1, t2 with
    | Type t1, Type t2 -> t1 == t2
    | Mod_type mty1, Mod_type mty2 -> mty1 == mty2
    | Str sitem1, Str sitem2 -> Abstraction.Structure_item.equal sitem1 sitem2
    | Module mexp1, Module mexp2 -> mexp1 == mexp2
    | Use (k1,p1), Use (k2,p2) -> k1 = k2 && p1 = p2
    | Non_expansive b1, Non_expansive b2 -> b1 = b2
    | Functor_parameter id1, Functor_parameter id2 -> id1 = id2
    | (Type _ | Str _ | Module _ | Functor_parameter _ | Use _ | Non_expansive _ 
          | Mod_type _),
      (Type _ | Str _ | Module _ | Functor_parameter _ | Use _ | Non_expansive _
          | Mod_type _) -> false 

  (* Recorded Location-Annot table. One location may have more than one binding! *)
  let recorded = (Hashtbl.create 1023 : (Location.t, t) Hashtbl.t)

  let recorded_top = ref None

  let clear () = Hashtbl.clear recorded

  type location_property = Wellformed | Flipped | Over_files | Illformed

  let check_location loc = 
    if loc.Location.loc_start == Lexing.dummy_pos || loc.Location.loc_end == Lexing.dummy_pos then Illformed
    else if loc.Location.loc_start = Lexing.dummy_pos || loc.Location.loc_end = Lexing.dummy_pos then Illformed
    else 
      (* If the file name is different between the start and the end, we cannot tell the wellformedness. *)
      if loc.Location.loc_start.Lexing.pos_fname <> loc.Location.loc_end.Lexing.pos_fname then Over_files
      else
        (* P4 creates some flipped locations where loc_start > loc_end *)
        match compare loc.Location.loc_start.Lexing.pos_cnum loc.Location.loc_end.Lexing.pos_cnum 
        with
        | -1 | 0 -> Wellformed
        | _ -> Flipped

  open Typedtree

  let record loc t = 
    let really_record () = Hashtbl.add recorded loc t in
    match check_location loc with
    | Wellformed -> really_record ()
    | Flipped -> 
        if not loc.Location.loc_ghost then Format.eprintf "%aWarning: Flipped location.@." Location.print loc; 
        really_record ()
    | Illformed -> 
        if not loc.Location.loc_ghost then Format.eprintf "%aWarning: Ill-formed location.@." Location.print loc
    | Over_files -> ()

  let record_constr_type_use loc ty =
    let path_of_constr_type t =
      let t = Ctype.repr t in 
      match (Ctype.repr t).Types.desc with
      | Types.Tconstr (p, _, _) -> Some p
      | _ ->
          Format.eprintf "Error: Spot.Annot.record_constr_type_use: not a constructor type: %a@." Printtyp.type_expr ty;
          None
    in
    match path_of_constr_type ty with
    | Some path -> record loc (Use (Kind.Type, path))
    | None -> ()

  let record_module_expr_def loc id modl =
    protect "Spot.Annot.record_module_expr_def" (fun () ->
      record loc (Str (Abstraction.Str_module 
	                  (id, 
	                  (Abstraction.module_expr modl)))))
      ()
    
  let record_module_type_def id mty =
    protect "Spot.Annot.record_module_type_def" (fun () ->
      record mty.mty_loc 
        (Str (Abstraction.Str_modtype
                (id, Abstraction.module_type mty.mty_type))))
      ()

  let record_include_sig mty (* sg *) =
    protect "Spot.Annot.record_include_sig" (fun () ->
(* CR jfuruse: TODO
      let kids = (* CR jfuruse: copy of structure_item_sub *) 
	List.map Abstraction.kident_of_sigitem sg
      in
*)
      let kids = [] in
      let sitem = Abstraction.Str_include (Abstraction.module_type mty.mty_type, kids)
      in 
      (* ocaml signature simply forgets the fact that kids are
	 included. We memorize them here. *)
      List.iter (fun (_,id) ->
	Hashtbl.add
          Abstraction.included_sig_identifier_table
	  id (sitem, ref false (* never recorded in the parent sig yet *))) kids;
      record mty.mty_loc (Str sitem))
      ()

  let record_include modl (* _sg *) =
    protect "Spot.Annot.record_include" (fun () ->
      List.iter (fun sitem -> record modl.mod_loc (Str sitem))
        (Abstraction.structure_item
           { str_desc = Typedtree.Tstr_include (modl, [] (* not used *)(*, sg *));
             str_loc = modl.mod_loc }))
      ()

  module IteratorArgument = struct
    include DefaultIteratorArgument

    let enter_pattern pattern = 
      let loc = pattern.pat_loc in
      record loc (Type pattern.pat_type);
      match pattern.pat_desc with
      | Tpat_var id -> record loc (Str (Abstraction.Str_value id))
      | Tpat_alias (_, TPat_constraint _) -> ()
      | Tpat_alias (_, TPat_alias id) -> record loc (Str (Abstraction.Str_value id))
      | Tpat_alias (_, TPat_type _path) -> assert false (* CR jfuruse: todo *)
      | Tpat_construct (_path, constr, _pats) ->
          begin match constr.Types.cstr_tag with
          | Types.Cstr_exception p ->
              record loc (Use (Kind.Exception, p))
          | _ -> record_constr_type_use loc constr.Types.cstr_res
          end
      | Tpat_record (_, _) -> record_constr_type_use loc pattern.pat_type
      | Tpat_any
      | Tpat_constant _
      | Tpat_tuple _
      | Tpat_variant _
      | Tpat_array _
      | Tpat_or _
      | Tpat_lazy _ -> ()

    let enter_expression exp =
      let loc = exp.exp_loc in
      record loc (Type exp.exp_type);
      match exp.exp_desc with
      | Texp_ident (path, _) -> record loc (Use (Kind.Value, path))

      | Texp_constant _ -> ()
      | Texp_let _ | Texp_function _ -> () (* done at bindings *)
      | Texp_apply _ -> ()
      | Texp_match _ -> ()
      | Texp_try _ -> ()
      | Texp_tuple _ -> ()
      | Texp_construct (_path, constr, _args) -> (* CR jfuruse: we can use path *)
          begin match constr.Types.cstr_tag with
          | Types.Cstr_exception p ->
              record loc (Use (Kind.Exception, p))
          | _ -> record_constr_type_use loc constr.Types.cstr_res
          end
      | Texp_variant _ -> ()
      | Texp_record _ -> record_constr_type_use loc exp.exp_type
      | Texp_field (_exp, _path, _label) -> (* CR jfuruse: we can use path *)
          record_constr_type_use loc exp.exp_type          
      | Texp_setfield (exp1, _path , _label, _exp2) -> (* CR jfuruse: we can use path *)
          record_constr_type_use loc exp1.exp_type          
      | Texp_array _ -> ()
      | Texp_ifthenelse _ -> ()
      | Texp_sequence _ -> ()
      | Texp_while _ -> ()
      | Texp_for (id, _exp1, _exp2, _dir, _exp3) ->
          record loc (Str (Abstraction.Str_value id) )
      | Texp_when _ -> ()
      | Texp_send _ -> ()
      | Texp_new (path, _) -> record loc (Use (Kind.Class, path))
      | Texp_instvar (_, _path) -> () (* CR jfuruse: todo *)
      | Texp_setinstvar (_, _path, _) -> () (* CR jfuruse: todo *)
      | Texp_override (_, _list) -> () 
      | Texp_letmodule (id, mexpr, _exp) ->
          record_module_expr_def loc id mexpr
      | Texp_assert _ -> ()
      | Texp_assertfalse -> ()
      | Texp_lazy _ -> ()
      | Texp_object (_, _) -> ()
      | Texp_pack _ -> ()
      | Texp_poly _ -> ()
      | Texp_open (path, exp) ->
          (* workaround to get better location *)
          let loc = Location_bound.upperbound loc exp.exp_loc in
          record loc (Use (Kind.Module, path))
      | Texp_newtype (_s, _exp) ->
          (* CR jfuruse: string is not sufficient! *)
          (* CR jfuruse: Texp_newtype is never used! *)
          assert false
          (* Spot.Annot.record loc (Spot.Annot.Str (Spot.Abstraction.Str_type id)); *)
      | Texp_constraint _ -> ()

    let enter_signature_item item = 
      let loc = item.sig_loc in
      match item.sig_desc with
        | Tsig_value (id, v) ->
            record loc (Str (Abstraction.Str_value id));
            record loc (Type v.val_desc.ctyp_type)
        | Tsig_type list ->
            List.iter (fun (id, decl) ->
	      record decl.typ_loc (Str (Abstraction.Str_type id))
            ) list
        | Tsig_exception (id, _decl) -> record loc (Str (Abstraction.Str_exception id))
        | Tsig_module (id, mtype) -> record_module_type_def id mtype
        | Tsig_recmodule list ->
            List.iter (fun (id, mtype) -> record_module_type_def id mtype) list
        | Tsig_modtype (id, Tmodtype_abstract) ->
	    record loc (Str (Abstraction.Str_modtype (id, Abstraction.Mod_abstract)))
        | Tsig_modtype (id, Tmodtype_manifest mty) -> record_module_type_def id mty
        | Tsig_open path -> record loc (Use (Kind.Module, path))
        | Tsig_include mty -> record_include_sig mty
        | Tsig_class _list -> () (* CR jfuruse *)
        | Tsig_class_type _list -> () (* CR jfuruse *)

    let enter_structure_item item =
      let loc = item.str_loc in
      match item.str_desc with
      | Tstr_eval _ -> ()
      | Tstr_value _ -> ()
      | Tstr_primitive (id, _v) -> record loc (Str (Abstraction.Str_value id))
      | Tstr_type list ->
          List.iter (fun (id, decl) ->
            record decl.typ_loc (Str (Abstraction.Str_type id))) list
      | Tstr_exception (id, _decl) -> record loc (Str (Abstraction.Str_exception id))
      | Tstr_exn_rebind (id, _p) -> record loc (Str (Abstraction.Str_exception id))
      | Tstr_module (id, mexpr) -> record_module_expr_def loc id mexpr
      | Tstr_recmodule list ->
	  List.iter (fun (id, _mtype, mexpr) ->
	    record_module_expr_def mexpr.mod_loc id mexpr) list
      | Tstr_modtype (id, mtype) -> record_module_type_def id mtype
      | Tstr_open path -> record loc (Use (Kind.Module, path))
      | Tstr_class _list -> () (* CR jfuruse *)
      | Tstr_class_type _list -> () (* CR jfuruse *)
      | Tstr_include (mexpr, _) -> record_include mexpr

    let enter_core_type ct =
      match ct.ctyp_desc with
      | Ttyp_any -> ()
      | Ttyp_var _s -> () (* CR jfuruse: we will be able to work on poly things *) 
      | Ttyp_arrow _ -> ()
      | Ttyp_tuple _ -> ()
      | Ttyp_constr (path, _list) -> record ct.ctyp_loc (Use (Kind.Type, path))
      | Ttyp_object _list -> ()
      | Ttyp_class _ -> ()
      | Ttyp_alias (_ct, _s) -> () (* CR jfuruse: we will be able to work on poly things *) 
      | Ttyp_variant _ -> ()
      | Ttyp_poly _ -> ()
      | Ttyp_package _pack -> ()

    let enter_module_expr mexpr =
      match mexpr.mod_desc with
      | Tmod_ident p -> record mexpr.mod_loc (Use (Kind.Module, p))
      | Tmod_structure _ -> ()
      | Tmod_functor (id, mtype, mexpr) ->
          (* CR jfuruse: id should have its position  *) 
          record (Location_bound.upperbound mexpr.mod_loc mtype.mty_loc) (Functor_parameter id);
      | Tmod_apply _ -> ()
      | Tmod_constraint _ -> ()
      | Tmod_unpack _ -> ()

(*
    val enter_structure : structure -> unit
    val enter_value_description : value_description -> unit
    val enter_type_declaration : type_declaration -> unit
    val enter_exception_declaration :
      exception_declaration -> unit
    val enter_package_type : package_type -> unit
    val enter_signature : signature -> unit
    val enter_modtype_declaration : modtype_declaration -> unit
    val enter_module_type : module_type -> unit
    val enter_with_constraint : with_constraint -> unit
    val enter_class_expr : class_expr -> unit
    val enter_class_signature : class_signature -> unit
    val enter_class_description : class_description -> unit
    val enter_class_type_declaration :
      class_type_declaration -> unit
    val enter_class_infos : 'a class_infos -> unit
    val enter_class_type : class_type -> unit
    val enter_class_type_field : class_type_field -> unit
    val enter_core_field_type : core_field_type -> unit
    val enter_class_structure : class_structure -> unit
    val enter_class_field : class_field -> unit
*)
  end

(* CR jfuruse: todo

  let record_module_expr_use loc modl =
    protect "Spot.Annot.record_module_expr_use" (fun () ->
      record loc (Module (Abstraction.module_expr modl));
      record loc (Mod_type modl.Typedtree.mod_type))
      ()

*)

  module Iterator = Typedtree.MakeIterator(IteratorArgument)

  let record_saved_type = function
    | Saved_implementation str 
    | Saved_structure str -> 
        recorded_top := Some (List.flatten (List.map Abstraction.structure_item str.str_items));
        Iterator.iter_structure str
    | Saved_signature sg -> 
        recorded_top := Some (List.filter_map Abstraction.signature_item sg.sig_type);
        Iterator.iter_signature sg
    | Saved_structure_item i -> Iterator.iter_structure_item i
    | Saved_signature_item i -> Iterator.iter_signature_item i
    | Saved_expression e -> Iterator.iter_expression e
    | Saved_module_type mt -> Iterator.iter_module_type mt
    | Saved_pattern p -> Iterator.iter_pattern p
    | Saved_class_expr ce -> Iterator.iter_class_expr ce

  let recorded () = Hashtbl.fold (fun k v st -> (k,v) :: st) recorded []

  let recorded_top () = !recorded_top
end

(* Spot file *)
module File = struct
  (* CR jfuruse: Current cmt/cmti only carry saved_types *)
  (* not record but list for future extensibility *)
  type elem =
    | Argv of string array
    | Source_path of string option (* packed module has None *)
    | Cwd of string
    | Load_paths of string list
    | Saved_types of Typedtree.saved_type array

  type t = elem list

end
