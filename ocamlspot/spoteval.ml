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

module Value : sig

  type t = 
    | Ident of PIdent.t
    | Structure of PIdent.t * structure * structure option (* sig part *)
    | Closure of PIdent.t * env * Ident.t * Types.module_type * Abstraction.module_expr
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
    val find : t -> Ident.t -> Kind.t * z
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

  type t = 
    | Ident of PIdent.t
    | Structure of PIdent.t * structure * structure option (* sig part *)
    | Closure of PIdent.t * env * Ident.t * Types.module_type * Abstraction.module_expr
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
    let error () = raise (Failure "Binding: premature")
    let with_check f t = 
      match !t with
      | None -> error ()
      | Some str -> f str
    let domain = with_check (List.map fst) 
    let find t id = with_check (List.assoc id) t
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
            Format.fprintf ppf "@[<v2>Module(%a)@ %a None@]"
              PIdent.format pid
            structure str
      | Structure (pid, str, Some str') -> 
            Format.fprintf ppf "@[<v2>Module(%a)@ %a (Some %a)@]"
              PIdent.format pid
            structure str
            structure str'
      | Closure (pid, _, id, _mty, module_expr) ->
            Format.fprintf ppf "(@[<2>(%a =)fun %s ->@ @[%a@]@])" 
              PIdent.format pid
              (Ident.name id)
              Abstraction.format_module_expr module_expr
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
  let find t id = Binding.find t.binding id
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

  open Abstraction
  open Value
  module Format = OCaml.Format

  let str_of_global_ident = ref (fun ~load_paths:_ _ -> assert false : load_paths: string list -> Ident.t -> string * Value.structure)
  let packed = ref (fun _ _ -> assert false : Env.t -> string -> Value.t)

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
          lazy begin try
            let path, str = 
              !str_of_global_ident ~load_paths:env.load_paths id
            in
            let str = Structure ( { PIdent.path = path; ident = None }, 
                                  str,
                                  None (* CR jfuruse: todo (read .mli *))
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
            try !!(snd (Env.find env id)) with Not_found -> 
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

  and module_expr env idopt : module_expr -> Value.z = function
    | Mod_abstract -> eager (Error (Failure "abstract"))
    | Mod_ident p -> 
        find_path env (Kind.Module, p)
    | Mod_packed s -> lazy (!packed env s)
    | Mod_structure str -> 
        lazy begin
          let str = structure env str in
          Structure ({ PIdent.path= env.path; ident = idopt }, str, None)
        end
    | Mod_functor (id, mty, mexp) -> 
        Debug.format "evaluating functor (arg %s) under %s@."
          (Ident.name id)
          (String.concat "; " (List.map Ident.name (Env.domain env)));
        eager (Closure ({ PIdent.path = env.path; ident = idopt }, 
                       env, id, mty, mexp))
    | Mod_constraint (mexp, _mty) -> 
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
    | Mod_apply (mexp1, mexp2) ->
        let v1 = module_expr env None mexp1 in
        let v2 = module_expr env None mexp2 in
	apply v1 v2
    | Mod_unpack _mty -> lazy (Error (Failure "packed"))

  (* expand internal Include and get alist by Ident.t *)
  (* the list order is REVERSED and is last-defined-first, 
     but it is REQUIRED for environment query *)
  and structure env0 sitems : Value.structure =

    List.fold_left (fun str sitem ->
      match sitem with
      | Str_value id 
      | Str_type id
      | Str_exception id
      | Str_class id
      | Str_cltype id ->
          (* CR jfuruse: not sure *)
          let pident = { PIdent.path = env0.Env.path; ident = Some id } in
          let v = Ident pident in
          let kind = 
            match sitem with
            | Str_value _ -> Kind.Value
            | Str_type _ -> Kind.Type
            | Str_exception _ -> Kind.Exception
            | Str_modtype _ -> Kind.Module_type
            | Str_class _ -> Kind.Class
            | Str_cltype _ -> Kind.Class_type
            | Str_module _ | Str_include _ -> assert false
          in
          (id, (kind, eager v)) :: str

      (* CR: very ad-hoc rule for functor parameter *)      
      | Str_module (id, Mod_ident (Path.Pdot (Path.Pident _id, 
                                              "parameter", 
                                              -2))) ->
          (* id = id_ *)
          let pident = { PIdent.path = env0.Env.path; ident = Some id } in
          (id, (Kind.Module, eager (Parameter pident))) :: str
          
      | Str_module (id, mexp) ->
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

      | Str_modtype (id, mexp) ->
          (* CR jfuruse: dup code *)
          let v = lazy begin
            try
              (* create it lazily for recursiveness of flat *)
              let env = Env.overrides env0 str in
              !!(module_expr env (Some id) mexp) (* ??? *)
            with
            | exn -> Error exn
          end
          in
          (id, (Kind.Module_type, v)) :: str

      | Str_include (mexp, kids) ->
          (* be careful: everything must be done lazily *)
          let v = lazy begin
            (* createate it lazily for recursiveness of flat *)
            let env = Env.overrides env0 str in
            !!(module_expr env None(*?*) mexp)
          end in
          let kname_ztbl = 
            lazy begin match !!v with
            | Structure (_, str, _ (* CR jfuruse *) ) -> 
                List.map (fun (id, (k, v)) -> (k, Ident0.name id), v) str
            | Parameter pid -> 
                List.map (fun (k,id) -> 
                  (k, Ident0.name id), eager (Parameter pid)) kids
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
          str' @ str) [] sitems

  and apply v1 v2 =
    lazy begin match !!v1 with
    | Ident _ -> assert false
    | Parameter pid -> Parameter pid
    | Structure _ -> assert false
    | Error exn -> Error exn
    | Closure (_, env, id, _mty, mexp) -> 
        !!(module_expr (Env.override env (id, (Kind.Module, v2)))
          None(*?*) mexp)
    end
end
