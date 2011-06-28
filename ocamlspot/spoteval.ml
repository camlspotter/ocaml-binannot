(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2011 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Format
open Utils

(* To avoid name collisions *)
module OCaml = struct
  module Format = Format
end

(* Keep the original modules *)
module Ident0 = Ident

open Spot

module PIdent = struct
  type t = {
    path : string; (* "" means predefined *)
    ident : Ident.t option; (* None means the top module *)
  }

  let format ppf id =
    Format.fprintf ppf "%s%s" 
      (if id.path = "" then ""
        else 
          (let len = String.length id.path in
          if len > 20 then
            "..." ^ String.sub id.path (len - 20) 20 
          else id.path) ^ ":")
        (match id.ident with
        | Some id -> Ident.name id
        | None -> "TOP")
end

(* CR jfuruse: this is called many times (new_include2.ml *)    
let kident_of_include exported_value_ids included_mexp = 
  let open Typedtree in
  let open Types in
  let mty = Mtype.scrape included_mexp.mod_env included_mexp.mod_type in
  match mty with
  | Mty_functor _ -> assert false (* Including a functor?! *)
  | Mty_ident _ -> assert false (* Including an abstract module?! *)
  | Mty_signature sg ->
      let internal_value_ids = Typemod.bound_value_identifiers sg in
      let value_id_table = List.combine internal_value_ids exported_value_ids in
      let kident_of_sigitem = function
        | Sig_value (id, {val_kind = Val_reg; _}) -> Kind.Value, id
        | Sig_value (id, _) -> Kind.Special_value, id
        | Sig_exception (id, _) -> Kind.Exception, id
        | Sig_module (id, _, _) ->  Kind.Module, id
        | Sig_class (id, _, _) -> Kind.Class, id

        | Sig_type (id, _, _) -> Kind.Type, id
        | Sig_modtype (id, _) -> Kind.Module_type, id
        | Sig_class_type (id, _, _) -> Kind.Class_type, id
      in
      let kids = List.map kident_of_sigitem sg in
      (* Fixing internal ids to exported ids.
         Non value ids are replaced by id with -2 *)
      let fixed = List.map (fun (k, id) ->
        (k, 
         try List.assoc id value_id_table with Not_found -> 
           Ident.unsafe_create_with_stamp (Ident0.name id) (-2) (* magic number *))) 
        kids
      in

(*
      prerr_endline "fixing kids";
      Format.eprintf "exported: @[%a@]@."
        (Format.list ", " (fun ppf id -> Format.fprintf ppf "%s" (Ident.name id))) exported_value_ids;
      Format.eprintf "sig: @[%a@]@."
        (Format.list ", " (fun ppf (k,id) -> 
          Format.fprintf ppf "%s:%s" (Kind.to_string k) (Ident.name id))) kids;
      Format.eprintf "fixed: @[%a@]@."
        (Format.list ", " (fun ppf (k,id) -> 
          Format.fprintf ppf "%s:%s" (Kind.to_string k) (Ident.name id))) fixed;
*)

      fixed
        
(* CR jfuruse: DUP *)
let kident_of_mty env mty = 
  let open Typedtree in
  let open Types in
  let mty = Mtype.scrape env mty in
  match mty with
  | Mty_functor _ -> assert false (* Including a functor?! *)
  | Mty_ident _ -> assert false (* Including an abstract module?! *)
  | Mty_signature sg ->
      let kident_of_sigitem = function
        | Sig_value (id, {val_kind = Val_reg; _}) -> Kind.Value, id
        | Sig_value (id, _) -> Kind.Special_value, id
        | Sig_exception (id, _) -> Kind.Exception, id
        | Sig_module (id, _, _) ->  Kind.Module, id
        | Sig_class (id, _, _) -> Kind.Class, id

        | Sig_type (id, _, _) -> Kind.Type, id
        | Sig_modtype (id, _) -> Kind.Module_type, id
        | Sig_class_type (id, _, _) -> Kind.Class_type, id
      in
      List.map kident_of_sigitem  sg

module Value : sig

  type module_expr_or_type = 
    | Module_expr of Typedtree.module_expr
    | Module_type of Typedtree.module_type

  type t = 
    | Ident of PIdent.t
    | Structure of PIdent.t * structure * structure option (* sig part *)
    | Closure of PIdent.t * env * Ident.t * Typedtree.module_type * module_expr_or_type
    | Parameter of PIdent.t
    | Error of exn 

  and structure = structure_item list

  and structure_item = Ident.t * (Kind.t * z)

  and z = t Lazy.t

  and env = {
    path : string;
    cwd : string;
    load_paths : string list;
    binding : binding;
  } 

  and binding
      
  module Binding : sig
    type t = binding
    val domain : t -> Ident.t list
    val domain_with_kind : t -> (Ident.t * Kind.t) list
    val find : t -> ?kind:Kind.t -> Ident.t -> Kind.t * z
      (** If [kind] is specified, and [kind] has not exportable value,
          it also checks Ident with id -2 *)
    val override : t -> structure_item -> t
    val overrides : t -> structure -> t
    val set : t -> structure -> unit
    val predef : t
    val empty : t
    val invalid : t
  end

  module Enforcer(A : sig 
  end) : sig
    val t : t -> unit
    val env : env -> unit
    val binding : binding -> unit
    val structure : structure -> unit
    val structure_item : structure_item -> unit
    val z : z -> unit
  end

  module Format : sig
    (* include module type of Format 3.12 *)
    val t : formatter -> t -> unit
    val env : formatter -> env -> unit
    val binding : formatter -> binding -> unit
    val structure : formatter -> structure -> unit
    val z : formatter -> z -> unit
  end

end = struct

  type module_expr_or_type = 
    | Module_expr of Typedtree.module_expr
    | Module_type of Typedtree.module_type

  type t = 
    | Ident of PIdent.t
    | Structure of PIdent.t * structure * structure option (* sig part *)
    | Closure of PIdent.t * env * Ident.t * Typedtree.module_type * module_expr_or_type
    | Parameter of PIdent.t
    | Error of exn 

  and structure = structure_item list

  and structure_item = Ident.t * (Kind.t * z)

  and z = t Lazy.t 

  and env = {
    path : string;
    cwd : string;
    load_paths : string list;
    binding : binding;
  }

  (* dirty hack for flat recursion *)
  and binding = structure option ref
      
  module Binding = struct
    type t = binding
    let with_check f t = 
      match !t with
      | None -> raise (Failure "Binding: premature") 
      | Some str -> f str
    let domain = with_check (List.map fst) 
    let domain_with_kind = with_check (List.map (fun (id, (k, _)) -> (id, k)))

    let find t ?kind id = 
      match kind with
      | Some (Kind.Special_value | Kind.Type | Kind.Module_type | Kind.Class_type as k) ->
          (* also checks id_X (-2) *)
          with_check 
            (fun t -> 
              snd (List.find (fun (id', (k',_)) ->
                k = k' &&
                  (id = id' 
                  || (Ident0.name id = Ident0.name id' 
                     && Ident0.binding_time id' = -2 (* CR jfuruse: magic number *))))
                     t))
            t
      | _ -> with_check (List.assoc id) t
          
    let override t v = ref (Some (with_check (fun t -> v :: t) t))
    let overrides t vs = ref (Some (with_check (fun t -> vs @ t) t))
    let invalid = ref None 
    let empty = ref (Some [])
    let predef = 
      let items = ref [] in
      let add_predefined kind id = 
        items := 
          (id, 
           (kind, eager (Ident { PIdent.path = "";
                                 ident = Some id })))
          :: !items
      in
      Predef.build_initial_env 
        (fun id _ _ -> add_predefined Kind.Type id)
        (fun id _ _ -> add_predefined Kind.Exception id) 
        ();
      List.iter (fun (_, id) -> add_predefined Kind.Value id) 
        Predef.builtin_values;
      ref (Some !items)
    let set b str = b := Some str
  end

  module Enforcer(A : sig 
  end) = struct
    (* prevent looping forever *)
    let cache = ref []
    let rec t = function
      | Structure (_, str, str_opt) -> 
          structure str;
          Option.iter str_opt ~f:structure
      | Closure (_, e, _, _, _) -> env e
      | Ident _ | Error _ | Parameter _ -> ()
    and env e = binding e.binding
    and binding b =
      match !b with
      | None -> raise (Failure "Enforcer.binding: binding is premature")
      | Some str -> structure str
    and structure str = List.iter structure_item str
    and structure_item (_, (_, zt)) = z zt
    and z zt =
      if List.memq zt !cache then ()
      else begin
        cache := zt :: !cache;
        t !!zt
      end
  end

  module Format = struct

    include Format

    let rec t ppf = function
      | Ident id -> Format.fprintf ppf "Ident(%a)" PIdent.format id
      | Parameter id -> Format.fprintf ppf "Parameter(%a)" PIdent.format id
      | Structure (pid, str, None) -> 
            Format.fprintf ppf "@[<v2>Structure(%a)@ %a None@]"
              PIdent.format pid
            structure str
      | Structure (pid, str, Some str') -> 
            Format.fprintf ppf "@[<v2>Structure(%a)@ %a (Some %a)@]"
              PIdent.format pid
              structure str
              structure str'
      | Closure (pid, _, id, _mty, module_expr_or_type) ->
            Format.fprintf ppf "(@[<2>(%a =)fun %s ->@ @[%t@]@])" 
              PIdent.format pid
              (Ident.name id)
              (fun ppf -> match module_expr_or_type with
              | Module_expr _mexp -> Format.fprintf ppf "MEXP" (* CR jfuruse *)
              | Module_type _mty -> Format.fprintf ppf "MTY" (* CR jfuruse *))
      | Error (Failure s) -> Format.fprintf ppf "ERROR(%s)" s
      | Error exn -> Format.fprintf ppf "ERROR(%s)" (Printexc.to_string exn)
            
    and env ppf env = 
      Format.fprintf ppf "{ @[path=%s;@,@[<2>load_paths=@,[@[%a@]];@]@,@[<2>structure=@,@[%a@]@]@] }"
        env.path
        (Format.list "; " (fun ppf s -> Format.fprintf ppf "%S" s)) env.load_paths
        binding env.binding
        
    and binding ppf b = 
      match !b with
      | None -> Format.fprintf ppf "PREM"
      | Some str -> structure ppf str

    and structure ppf =
      Format.fprintf ppf "{ @[<v>%a@] }"
        (Format.list "; " (fun ppf (id, (kind, t)) ->
            Format.fprintf ppf "@[<2>%s %s =@ %a@]" 
              (String.capitalize (Kind.to_string kind))
            (Ident.name id) z t))
        
    and z ppf = Format.lazy_ t ppf
  end
end

module Binding = Value.Binding

module Env = struct
  open Value
  type t = env = {
    path : string;
    cwd : string;
    load_paths : string list;
    binding : Binding.t;
  } 
  let format = Value.Format.env
  let domain t = Binding.domain t.binding
  let domain_with_kind t = Binding.domain_with_kind t.binding
  let find t ?kind id = Binding.find t.binding ?kind id
  let override t v = { t with binding = Binding.override t.binding v }
  let overrides t vs = { t with binding = Binding.overrides t.binding vs }
  let predef = {
    path = "";
    cwd = "";
    load_paths = [];
    binding = Binding.predef;
  }
end

module Eval = struct

  open Typedtree

  open Value
  module Format = OCaml.Format

  let str_of_global_ident = ref (fun ~load_paths:_ _ -> assert false : load_paths: string list -> Ident.t -> string * Value.structure)
  let packed = ref (fun _ _ -> assert false : Env.t -> string -> Value.t)

  let z_of_id env id = eager (Ident { PIdent.path = env.Env.path; ident = Some id })

  let rec find_path env (kind, p) : Value.z = 
    match p with
    | Path.Papply (p1, p2) -> 
	let v1 = find_path env (kind, p1) in
	let v2 = find_path env (kind, p2) in
	apply v1 v2
    | Path.Pident id -> 
        (* predef check first (testing) *)
        begin try snd (Env.find Env.predef id) with Not_found ->
        
        if Ident.global id then
          lazy begin 
            try
              let path, str = 
                !str_of_global_ident ~load_paths:env.load_paths id
              in
              let str = Structure ( { PIdent.path = path; ident = None }, 
                                    str,
                                    None (* CR jfuruse: todo (read .mli) *) )
              in
              Debug.format "@[<2>LOAD SUCCESS %s =@ %a@]@."
                (Ident.name id)
                Value.Format.t str;
              str
            with
            | e -> 
                Format.eprintf "LOAD FAILIURE %s: %s@." (Ident.name id) (Printexc.to_string e);
                Error e
          end
        else begin 
          lazy begin
            Debug.format "find_path %s in { %s }@." 
              (Path.name p)
              (String.concat "; " 
                 (List.map Ident.name (Env.domain env)));
            try !!(snd (Env.find env ~kind id)) with Not_found -> 
(*
              (* it may be a predefed thing *)
              try !!(snd (Env.find Env.predef id)) with Not_found ->
*)
              Error (Failure (Printf.sprintf "%s:%s not found in { %s }" 
                                (Kind.name kind)
                                (Ident.name id)
                                (String.concat "; " 
                                   (List.map Ident.name (Env.domain env)))))
          end
        end
        end
    | Path.Pdot (p, name, pos) ->
        lazy begin
          match !!(find_path env (Kind.Module, p)) with
          | Ident _ -> (try assert false with e -> Error e)
          | Parameter pid -> Parameter pid
          | Closure _ -> (try assert false with e -> Error e)
          | Error exn -> Error exn
          | Structure (pid, str, _ (* CR jfuruse *)) -> 
              Debug.format "Module %s found (%a)@." (Path.name p) PIdent.format pid;
              try
                !!(find_ident str (kind, name, pos))
              with
              | Not_found -> Error (Failure (Printf.sprintf "Not_found %s:%d" name pos))
        end

  and find_ident (str : Value.structure) (kind, name, pos) : Value.z =
    let name_filter = fun (id, (k,_)) -> k = kind && Ident0.name id = name in
    (* CR jfuruse: double check by pos! *)
    lazy begin
      try
        !!(snd (snd (List.find (fun id_value ->
          (* pos_filter id_value && *) name_filter id_value) str)))
      with
      | Not_found ->
          Debug.format "Error: Not found %s(%s) @[%a@]@."
            (String.capitalize (Kind.to_string kind))
            name
            Value.Format.structure str;
          Error (Failure (Printf.sprintf "Not found: %s__%d" name pos))
    end

  and module_expr_or_type env idopt : module_expr_or_type -> Value.z = function
    | Module_expr mexp -> module_expr env idopt mexp 
    | Module_type mty -> module_type env idopt mty

  and module_expr env idopt mexp =
    match mexp.mod_desc with
    | Tmod_ident p -> find_path env (Kind.Module, p)
    | Tmod_structure str -> 
        lazy begin
          let str = structure env str in
          Structure ({ PIdent.path= env.path; ident = idopt }, str, None)
        end
    | Tmod_functor (id, mty, mexp) -> 
        Debug.format "evaluating functor (arg %s) under %s@."
          (Ident.name id)
          (String.concat "; " (List.map Ident.name (Env.domain env)));
        eager (Closure ({ PIdent.path = env.path; ident = idopt }, 
                        env, id, mty, Module_expr mexp))
    | Tmod_constraint (mexp, _mty, _, _) -> 
        (* [mty] may not be a simple signature but an ident which is
           hard to get its definition at this point. 
           Therefore we do not constrain our result here. 
           Only the sensitive case is when a constrained module is
           included, but we can handle this case using included
           value list. 

           Types never override themselves so the including module's
           type wins against the type of the same name in the included one:

           type t (* WINS! *)
           include (struct
           type t (* ocamlspot does not hide it *)
           end : sig
        (* type system hide t *)
           end)
        *)
        module_expr env idopt (*?*) mexp
    | Tmod_apply (mexp1, mexp2, _coe) ->
        let v1 = module_expr env None mexp1 in
        let v2 = module_expr env None mexp2 in
	apply v1 v2
    | Tmod_unpack (_mty, _) -> lazy (Error (Failure "packed"))

  and module_type env idopt mexp = 
    match mexp.mty_desc with
    | Tmty_ident p -> find_path env (Kind.Module_type, p)
    | Tmty_signature sg -> 
        lazy begin
          let sg = signature env sg in
          Structure ({ PIdent.path= env.path; ident = idopt }, sg, None)
        end
    | Tmty_functor (id, mty1, mty2) ->
        Debug.format "evaluating functor (arg %s) under %s@."
          (Ident.name id)
          (String.concat "; " (List.map Ident.name (Env.domain env)));
        eager (Closure ({ PIdent.path = env.path; ident = idopt }, 
                        env, id, mty1, Module_type mty2))
    | Tmty_with (mty, _) -> module_type env None mty (* module_type * (Path.t * with_constraint) list *)
    | Tmty_typeof _mty -> assert false

  (* expand internal Include and get alist by Ident.t *)
  (* the list order is REVERSED and is last-defined-first, 
     but it is REQUIRED for environment query *)
  and structure env0 str : Value.structure =

    List.fold_left (fun str sitem ->
      match sitem.str_desc with
      | Tstr_eval _ -> str

      | Tstr_value (_, pat_exps) -> 
          let ids = let_bound_idents pat_exps in
          List.map (fun id -> (id, (Kind.Value, z_of_id env0 id))) ids @ str

      | Tstr_primitive (id, _vdesc) -> (id, (Kind.Special_value, z_of_id env0 id)) :: str

      | Tstr_type id_typedecls ->
          List.map (fun (id,_) -> (id, (Kind.Type, z_of_id env0 id))) id_typedecls @ str

      | Tstr_exception (id, _) 
      | Tstr_exn_rebind (id, _) ->  (id, (Kind.Exception, z_of_id env0 id)) :: str 

      | Tstr_open _ -> str

      | Tstr_class clist -> 
          List.map (fun (cinfo, _, _) -> (cinfo.ci_id_class, (Kind.Class, z_of_id env0 cinfo.ci_id_class))) clist @ str

      | Tstr_class_type id_ctdecls ->
          List.map (fun (id, _) -> (id, (Kind.Class, z_of_id env0 id))) id_ctdecls @ str
          
      | Tstr_module (id, mexp) -> 
          let v = lazy begin
            try
              (* create it lazily for recursiveness of flat *)
              let env = Env.overrides env0 str in
              !!(module_expr env (Some id) mexp)
            with
            | exn -> Error exn
          end
          in
          (id, (Kind.Module, v)) :: str

      | Tstr_recmodule id_mty_mexps ->
          let make_v id mexp = lazy begin
            try
              (* create it lazily for recursiveness of flat *)
              let env = Env.overrides env0 str in
              !!(module_expr env (Some id) mexp)
            with
            | exn -> Error exn
          end
          in
          List.map (fun (id, _mty, mexp) ->
            (id, (Kind.Module, make_v id mexp))) id_mty_mexps @ str

      | Tstr_modtype (id, mty) ->
          let v = lazy begin
            try
              (* create it lazily for recursiveness of flat *)
              let env = Env.overrides env0 str in
              !!(module_type env (Some id) mty) (* ??? *)
            with
            | exn -> Error exn
          end
          in
          (id, (Kind.Module_type, v)) :: str

      | Tstr_include (mexp, exported_ids) -> 
          let kids = kident_of_include exported_ids mexp in 
          let kname_ztbl : ((Kind.t * string) * z) list lazy_t = 
            lazy begin 
              let v_mexp = 
                (* createate it lazily for recursiveness of flat *)
                let env = Env.overrides env0 str in
                !!(module_expr env None(*?*) mexp)
              in
              match v_mexp with
              | Structure (_, str, _ (* CR jfuruse *) ) -> 
                  List.map (fun (id, (k, v)) -> (k, Ident0.name id), v) str
              | Parameter pid -> 
                  List.map (fun (k,id) -> (k, Ident0.name id), eager (Parameter pid)) kids
              | Ident _ -> assert false
              | Closure _ -> assert false
              | Error _ -> [] (* error *)
            end
          in
          let str' =
            List.map (fun (k, id) ->
              let v = 
                lazy begin
                  try
                    !!(List.assoc (k, Ident0.name id) !!kname_ztbl)
                  with
                  | Not_found -> Error Not_found
                end
              in
              id, (k, v)) kids
          in
          str' @ str) [] str.str_items

  and signature env0 sg : Value.structure = 

    List.fold_left (fun str sitem ->
      match sitem.sig_desc with
      | Tsig_open _ -> str
      | Tsig_value (id, { val_val = { Types.val_kind = Types.Val_prim _; _ }; _ }) -> (id, (Kind.Special_value, z_of_id env0 id)) :: str
      | Tsig_value (id, _) -> (id, (Kind.Value, z_of_id env0 id)) :: str
      | Tsig_type id_typedecls -> List.map (fun (id,_) -> (id, (Kind.Type, z_of_id env0 id))) id_typedecls @ str
      | Tsig_exception (id, _) ->  (id, (Kind.Exception, z_of_id env0 id)) :: str 
      | Tsig_class clist -> 
          List.map (fun cinfo -> (cinfo.ci_id_class, (Kind.Class, z_of_id env0 cinfo.ci_id_class))) clist @ str
      | Tsig_class_type clist ->
          List.map (fun cinfo -> (cinfo.ci_id_class, (Kind.Class, z_of_id env0 cinfo.ci_id_class))) clist @ str
      | Tsig_module (id, mty) -> 
          let v = lazy begin
            try
              (* create it lazily for recursiveness of flat *)
              let env = Env.overrides env0 str in
              !!(module_type env (Some id) mty)
            with
            | exn -> Error exn
          end
          in
          (id, (Kind.Module, v)) :: str
      | Tsig_recmodule id_mtys ->
          let make_v id mty = lazy begin
            try
              (* create it lazily for recursiveness of flat *)
              let env = Env.overrides env0 str in
              !!(module_type env (Some id) mty)
            with
            | exn -> Error exn
          end
          in
          List.map (fun (id, mty) -> (id, (Kind.Module, make_v id mty))) id_mtys @ str
      | Tsig_modtype (_id, Tmodtype_abstract) -> str (* CR jfuruse: abstract type is ignored! *)
      | Tsig_modtype (id, Tmodtype_manifest mty) ->
          let v = lazy begin
            try
              (* create it lazily for recursiveness of flat *)
              let env = Env.overrides env0 str in
              !!(module_type env (Some id) mty) (* ??? *)
            with
            | exn -> Error exn
          end
          in
          (id, (Kind.Module_type, v)) :: str

      | Tsig_include mty -> 
          let kids = kident_of_mty mty.mty_env mty.mty_type in 
          let kname_ztbl : ((Kind.t * string) * z) list lazy_t = 
            lazy begin 
              let v_mexp = 
                (* createate it lazily for recursiveness of flat *)
                let env = Env.overrides env0 str in
                !!(module_type env None(*?*) mty)
              in
              match v_mexp with
              | Structure (_, str, _ (* CR jfuruse *) ) -> 
                  List.map (fun (id, (k, v)) -> (k, Ident0.name id), v) str
              | Parameter pid -> 
                  List.map (fun (k,id) -> (k, Ident0.name id), eager (Parameter pid)) kids
              | Ident _ -> assert false
              | Closure _ -> assert false
              | Error _ -> [] (* error *)
            end
          in
          let str' =
            List.map (fun (k, id) ->
              let v = 
                lazy begin
                  try
                    !!(List.assoc (k, Ident0.name id) !!kname_ztbl)
                  with
                  | Not_found -> Error Not_found
                end
              in
              id, (k, v)) kids
          in
          str' @ str) [] sg.sig_items

  and apply v1 v2 =
    lazy begin match !!v1 with
    | Ident _ -> assert false
    | Parameter pid -> Parameter pid
    | Structure _ -> assert false
    | Error exn -> Error exn
    | Closure (_, env, id, _mty, mexp_or_mty) -> 
        match mexp_or_mty with
        | Module_expr mexp ->
            !!(module_expr (Env.override env (id, (Kind.Module, v2)))
                 None(*?*) mexp)
        | Module_type _mty -> assert false
    end

  and flat env flat_tbl : Value.structure =
    Hashtbl.fold (fun id { Regioned.region = _reg; value = annot } st -> 
      let open Annot in
      match annot with
      | Type _ | Mod_type _ | Non_expansive _ | Use _ -> st
      | Functor_parameter id -> (id, (Kind.Module, eager (Parameter { PIdent.path = env.Env.path; ident = Some id }))) :: st
      | Def (k, _id, None) -> (id, (k, z_of_id env id)) :: st
      | Def (_, _id, Some (Def_module_expr mexp)) -> (id, (Kind.Module, module_expr env (Some id) mexp)) :: st
      | Def (_, _id, Some (Def_module_type mty)) -> (id, (Kind.Module_type, module_type env (Some id) mty)) :: st
      | Def (k, _id, Some (Def_alias path)) -> (id, (k, find_path env (k, path))) :: st
      | Def (k, _id, Some (Def_included (mexp, id'))) -> 
          let z = lazy begin
            let str = 
              match !!(module_expr env None mexp) with
              | Structure (_pid, str, _) -> str
              | _ -> assert false
            in
            !!(find_ident str (k, Ident0.name id', Ident0.binding_time id'))
          end
          in
          (id, (k, z)) :: st
      | Def (k, id, Some (Def_included_sig mty)) ->
          let z = lazy begin
            let str = 
              match !!(module_type env None mty) with
              | Structure (_pid, str, _) -> str
              | _ -> assert false
            in
            !!(find_ident str (k, Ident0.name id, Ident0.binding_time id))
          end
          in
          (id, (k, z)) :: st
    ) flat_tbl []

end
