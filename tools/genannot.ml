(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*      Fabrice Le Fessant, projet OCamlPro, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 2010 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)
(*
Generate .annot file from a .types files.
*)

open Typedtree

let pattern_scopes = ref []

let push_None () = 
  pattern_scopes := None :: !pattern_scopes
let push_Some annot =
  pattern_scopes := (Some annot) :: !pattern_scopes
let pop_scope () =
  match !pattern_scopes with
    [] -> assert false
  | _ :: scopes -> pattern_scopes := scopes
  
module ForIterator = struct
    open Asttypes
    
    include DefaultIteratorArgument
    
    let structure_begin_scopes = ref []
    let structure_end_scopes = ref []

    let rec find_last list =
      match list with
        [] -> assert false
      | [x] -> x
      | _ :: tail -> find_last tail
      
    let enter_structure str = 
      match str.str_items with
        [] -> ()
      | _ ->
          let loc = 
            match !structure_end_scopes with
              [] -> Location.none
            | _ -> 
                let s = find_last str.str_items in
                s.str_loc
          in
          structure_end_scopes := loc :: !structure_end_scopes;
          
          let rec iter list =
            match list with
              [] -> assert false
            | [ { str_desc = Tstr_value (Nonrecursive, _); str_loc = loc } ] ->
                structure_begin_scopes := loc.Location.loc_end
                  :: !structure_begin_scopes
            | [ _ ] -> ()
            | item :: tail ->
                iter tail;
                match item, tail with
                  { str_desc = Tstr_value (Nonrecursive,_) },
                  { str_loc = loc } :: _ ->
                    structure_begin_scopes := loc.Location.loc_start
                      :: !structure_begin_scopes
                | _ -> ()
          in
          iter str.str_items
      
    let leave_structure str = 
      match str.str_items with
        [] -> ()
      | _ ->
          match !structure_end_scopes with
            [] -> assert false
          | _ :: scopes -> structure_end_scopes := scopes
              
    let enter_class_expr node =
      Stypes.record (Stypes.Ti_class node)
    let enter_module_expr node =
      Stypes.record (Stypes.Ti_mod node)
          
    let add_variable pat id =
      match !pattern_scopes with
      | [] -> assert false
      | None :: _ -> ()
      | (Some s) :: _ -> 
          Stypes.record (Stypes.An_ident (pat.pat_loc, Ident.name id, s))
    
    let enter_pattern pat =
      match pat.pat_desc with
      | Tpat_var id
      | Tpat_alias (_, TPat_alias id)
        
        -> add_variable pat id
      
      | Tpat_alias (_, (TPat_constraint _ | TPat_type _) )        
      | Tpat_any _ -> ()
      | Tpat_constant _
      | Tpat_tuple _
      | Tpat_construct _
      | Tpat_lazy _
      | Tpat_or _
      | Tpat_array _
      | Tpat_record _
      | Tpat_variant _
        -> ()
    
    let leave_pattern pat =
      Stypes.record (Stypes.Ti_pat pat)
    
    let rec name_of_path = function
      | Path.Pident id -> Ident.name id
      | Path.Pdot(p, s, pos) ->
          if Oprint.parenthesized_ident s then
            name_of_path p ^ ".( " ^ s ^ " )"
          else
            name_of_path p ^ "." ^ s
      | Path.Papply(p1, p2) -> name_of_path p1 ^ "(" ^ name_of_path p2 ^ ")" 
    
    let enter_expression exp =
      match exp.exp_desc with
        Texp_ident (path, _) ->
          let full_name = name_of_path path in
          begin
            try
              let annot = Env.find_annot path exp.exp_env in
              Stypes.record
                (Stypes.An_ident (exp.exp_loc, full_name , annot))
            with Not_found ->                 
                Printf.fprintf stderr "Path %s not found in env\n%!"
                  full_name;
          end
      
      | Texp_let (rec_flag, _, body) ->
          begin
            match rec_flag with
            | Recursive -> push_Some (Annot.Idef exp.exp_loc)
            | Nonrecursive -> push_Some (Annot.Idef body.exp_loc)
            | Default -> push_None ()
          end          
      | Texp_function _ -> push_None ()
      | Texp_match _ -> push_None ()
      | Texp_try _ -> push_None ()                            
      | _ -> ()
    
    let leave_expression exp =
      if not exp.exp_loc.Location.loc_ghost then
        Stypes.record (Stypes.Ti_expr exp);
      match exp.exp_desc with
      | Texp_let _          
      | Texp_function _
      | Texp_match _ 
      | Texp_try _ 
        -> pop_scope ()
      | _ -> ()
    
    let enter_binding pat exp =
      let scope = 
        match !pattern_scopes with
        | [] -> assert false
        | None :: _ -> Some (Annot.Idef exp.exp_loc)
        | scope :: _ -> scope    
      in
      pattern_scopes := scope :: !pattern_scopes
    
    let leave_binding _ _ = 
      pop_scope ()
    
    let enter_class_expr exp =
      match exp.cl_desc with
      | Tcl_fun _ -> push_None ()
      | Tcl_let _ -> push_None ()
      | _ -> ()
    
    let leave_class_expr exp =
      match exp.cl_desc with
      | Tcl_fun _
      | Tcl_let _ -> pop_scope ()
      | _ -> ()
    
    let enter_class_structure _ =
      push_None ()
    
    let leave_class_structure _ = 
      pop_scope ()
    
    let enter_class_field cf =
      match cf.cf_desc with
        Tcf_let _ -> push_None ()
      | _ -> ()
    
    let leave_class_field cf =
      match cf.cf_desc with
        Tcf_let _ -> pop_scope ()
      | _ -> ()

          
    let enter_structure_item s =
      Stypes.record_phrase s.str_loc;
      match s.str_desc with
        Tstr_value (rec_flag, _) -> 
          begin
            let loc = s.str_loc in
            let scope = match !structure_end_scopes with
                [] -> assert false
              | scope :: _ -> scope
            in
            match rec_flag with
            | Recursive -> push_Some
                  (Annot.Idef { scope with
                    Location.loc_start = loc.Location.loc_start})
            | Nonrecursive ->
(* TODO: do it lazily, when we start the next element ! *)
(*
                 let start = match srem with
                  | [] -> loc.Location.loc_end
                  | {pstr_loc = loc2} :: _ -> loc2.Location.loc_start
in  *)
                let start = 
                  match !structure_begin_scopes with
                    [] -> assert false
                  | loc :: tail ->
                      structure_begin_scopes := tail;
                      loc
                in
                push_Some (Annot.Idef {scope with Location.loc_start = start})
            | Default -> push_None ()
          end
      | _ -> ()

    let leave_structure_item s =
      match s.str_desc with
        Tstr_value _ -> pop_scope ()
      | _ -> ()
          
      
  end

  
  
module Iterator = MakeIterator(ForIterator)

  
let init_path () =
  let dirs =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else if !Clflags.use_vmthreads then "+vmthreads" :: !Clflags.include_dirs
    else !Clflags.include_dirs in
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs in
  Config.load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Env.reset_cache ()

open Clflags

(*
TODO: external functions have no annotations ! fix typecore.ml !
TODO: Texp_for bound idents have no annoations ! fix typecore.ml !
  *)
  
let _ =
  Clflags.annotations := true;

  Arg.parse [
    "-I", Arg.String (fun filename -> 
        include_dirs := filename :: !include_dirs),  
    "<dir>  Add <dir> to the list of include directories";

    "-thread", Arg.Unit (fun _ -> use_threads := true),
    " Generate code that supports the system threads library";
    
      "-vmthread", Arg.Unit (fun _ -> use_vmthreads := true),
    " Generate code that supports the threads library with VM-level\n\
    \     scheduling"

    
  ] (fun filename ->
        init_path();  

      let ic = open_in filename in
      let (types : saved_type array) = input_value ic in
      close_in ic;      
      match types with
        [| Saved_implementation typedtree |] ->
          Iterator.iter_structure typedtree;
          Stypes.dump (filename ^ ".annot")
      | _ ->
          Printf.fprintf stderr "File was generated with an error\n%!";
          exit 2
  ) " <filename>.types : generate corresponding .annot file"