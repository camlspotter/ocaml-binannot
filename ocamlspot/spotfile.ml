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

(* module names may corride in different source/spot files *)

open Format
open Utils

open Spot
open Spoteval
    
module Make(Spotconfig : Spotconfig_intf.S) = struct

  type elem = 
    | Argv of string array
    | Source_path of string option
    | Cwd of string
    | Load_paths of string list
    | Saved_types of Typedtree.saved_type array

  type top = 
    | Saved_type of Typedtree.saved_type
    | Packed of string list

  type file = {
    path : string; (* "" means no source *)
    cwd : string;
    load_paths : string list;
    argv : string array;

    top : top option;
    rannots : Annot.t Regioned.t list;
    tree : Tree.t lazy_t;
    flat : (Ident.t, Annot.t Regioned.t) Hashtbl.t;
    (* flat : Abstraction.structure; *)
  }

  let dump_file file =
(*
    eprintf "@[<2>{ path= %S;@ cwd= %S;@ load_paths= [ @[%a@] ];@ version= %S,%S;@ argv= [| @[%a@] |]; ... }@]@."
*)
    eprintf "@[<2>{ path= %S;@ cwd= %S;@ load_paths= [ @[%a@] ];@ argv= [| @[%a@] |];@ top= @[%a@] }@]@."
      (match file.path with 
      | "" -> "NONE"
      | s -> s)
      file.cwd
      (Format.list "; " (fun ppf s -> fprintf ppf "%S" s)) file.load_paths
      (Format.list "; " (fun ppf s -> fprintf ppf "%S" s)) (Array.to_list file.argv)
      (* CR jfuruse: OCamlspot.Dump.top *)
      (fun ppf -> function
        | None -> fprintf ppf "NoTOP"
        | Some (Saved_type _) -> fprintf ppf "saved_type..."
        | Some (Packed paths) -> 
            Format.list "; " (fun ppf s -> fprintf ppf "%S" s) ppf paths) file.top

  (* xxx.{ml,cmo,cmx,cmt} => xxx.cmt
     xxx.{mli,cmi,cmti} => xxx.cmti
   *)        
  let cmt_of_file file =
    let dirname, filename =
      try
        let slash = String.rindex file '/' in
        Some (String.sub file 0 slash),
        String.sub file (slash + 1) (String.length file - slash - 1)
      with
      | Not_found -> None, file
    in
    let filename =
      match Filename.split_extension filename with
      | body, (".cmi" | ".mli" | ".cmti") -> body ^ ".cmti"
      | body, _ -> body ^ ".cmt"
    in
    match dirname with
    | None -> filename
    | Some d -> Filename.concat d filename

  module Load : sig
    exception Old_spot of string (* spot *) * string (* source *)
    val load : load_paths:string list -> string -> file
    val load_module : ?spit:bool -> load_paths:string list -> string -> file
  end = struct

    let check_time_stamp ~spot source =
      let stat_spot = Unix.stat spot in
      let stat_source = Unix.stat source in
        (* Needs = : for packed modules, .spot and the source .cmo are written 
           almost at the same moment. *)
      stat_spot.Unix.st_mtime >= stat_source.Unix.st_mtime

    let find_alternative_source ~spot source =
        (* if [source] is not found, we try finding files with the same basename
           in
           - the directory of [spot]
           - the directory of [spot] points to (if [spot] is symlink)
         *)
        let source_base = Filename.basename source in
      let source_dirs =
          Filename.dirname spot ::
          begin 
            let stat_spot = Unix.lstat spot in
            if stat_spot.Unix.st_kind = Unix.S_LNK then
              [ Filename.dirname (Unix.readlink spot) ]
            else []
          end
        in
        List.find Sys.file_exists 
          (List.map (fun d -> 
            Filename.concat d source_base) source_dirs)

    module Context = struct
      (* CR jfuruse: dup in typemod *)
      type command_context = {
        argv : string array;
        sourcefile: string;
        cwd : string;
        load_path : string list;
      }
    end

    let load_cmt_file path : Typedtree.saved_type array * Context.command_context * string list = 
      let ic = open_in path in
      let types : Typedtree.saved_type array = input_value ic in
      let context : Context.command_context = input_value ic in
      let packed : string list = input_value ic in
      close_in ic;
      types, context, packed

    let load_directly path : file =
      Debug.format "spot loading from %s@." path;
      let file, context, packed = load_cmt_file path in

      let source_path = 
        if Filename.is_relative context.Context.sourcefile then
          Filename.concat context.Context.cwd context.Context.sourcefile 
        else context.Context.sourcefile
      in
      (* fix source_path *)
      let source_path =
        if Sys.file_exists source_path then source_path
        else
          try 
            let path = find_alternative_source ~spot:path source_path in
            Debug.format "Found an alternative source: %s@." path;
            path
          with
          | Not_found -> source_path
      in
      let cwd = context.Context.cwd in
      let load_paths = context.Context.load_path in (* CR jfuruse: load_path and file.load_paths are confusing *)
      let argv = context.Context.argv in

      let rannots, top = 
        if packed <> [] then begin
          let rannots = [] in
          let top = Some (Packed packed) in
          rannots, top

        end else begin
          Array.iter Annot.record_saved_type file;
          let rannots = 
            List.map (fun (loc, annot) ->
              { Regioned.region = Region.of_parsing loc;
                value = annot }) (Annot.recorded ())
          in
          let top = Annot.recorded_top () in
          rannots, 
          match top with
          | None -> None
          | Some st -> Some (Saved_type st)
        end
      in
        
      let tree =
        lazy begin
          List.fold_left Tree.add Tree.empty rannots
        end
      in

      let flat = 
        let tbl = Hashtbl.create 107 in
        List.iter (fun ({ Regioned.value = annot; _ } as rannot) ->
          match annot with
          | Annot.Def (_, id, _) -> Hashtbl.add tbl id rannot
          | Annot.Functor_parameter id -> Hashtbl.add tbl id rannot
          | Annot.Type _ | Annot.Mod_type _ | Annot.Use _  (* | Annot.Non_expansive _ *) -> ()) rannots;
        tbl
      in

      { (* version = version; *)
        path = source_path;
        cwd = cwd;
        load_paths = List.map (fun load_path -> cwd ^/ load_path) load_paths;
        argv = argv;
        top = top;
        rannots = rannots;
        tree = tree;
        flat = flat;
      }
        
    exception Old_spot of string (* spot *) * string (* source *)

    (* CR jfuruse: exception *)
    (* CRv2 jfuruse: add and check cache time stamp *)
    (* CR jfuruse: cache is never used for now *)
    let load_directly_with_cache : string -> file = 
      let cache = Hashtbl.create 17 in
      fun path ->
        try 
          Hashtbl.find cache path
        with
        | Not_found ->
            try
              let file = load_directly path in
              begin match file.path with 
              | "" -> ()
              | source -> 
                  if not (check_time_stamp ~spot:path source) then 
                    if Spotconfig.strict_time_stamp then 
                      raise (Old_spot (path, source))
                    else
                      eprintf "Warning: source %s is newer than the spot@." source
              end;
              Hashtbl.replace cache path file;
              file
            with
            | Not_found ->
                failwith (Printf.sprintf "failed to find spot file %s" path)

    let find_in_path load_paths body ext =
      let body_ext = body ^ ext in
      let find_in_path load_paths name = 
        try Misc.find_in_path load_paths name with Not_found ->
          Misc.find_in_path_uncap load_paths name
      in
      try find_in_path load_paths body_ext with Not_found ->
        (* We do not give up yet.
           .cmt file is not found, but we may still find a .cmi which is sym-linked to the original directory 
           where .cmt file exists
        *)
        let cminame = body ^ ".cmi" in
        try
          let cmipath = find_in_path load_paths cminame in
          let stat = Unix.lstat cmipath in
          if stat.Unix.st_kind = Unix.S_LNK then begin
            let cmipath = Filename.dirname cmipath ^/ Unix.readlink cmipath in
            let spotpath = Filename.chop_extension cmipath ^ ext in
            if Sys.file_exists spotpath then begin
              Debug.format "Found an alternative %s: %s@." ext spotpath;
              spotpath 
            end else failwith (Printf.sprintf "saved typedtree file not found: %s, neither in %s" body_ext spotpath)
          end else raise Not_found
        with
        | (Failure _ as e) -> raise e
        | _ -> failwith (Printf.sprintf "saved typedtree file not found: %s" body_ext)
      

    let load ~load_paths spotname : file =
      Debug.format "@[<2>spot searching %s in@ paths [@[%a@]]@]@." 
        spotname
        (Format.list "; " (fun ppf x -> fprintf ppf "%S" x)) load_paths;
      let body, ext = Filename.split_extension spotname in
      let path = find_in_path load_paths body ext in
      load_directly_with_cache path

    let load ~load_paths spotname : file =
      let alternate_spotname = 
        if Filename.is_relative spotname then None
        else
          Option.bind (Dotfile.find_and_load (Filename.dirname spotname)) 
            (fun (found_dir, dotfile) ->
              Option.map dotfile.Dotfile.build_dir ~f:(fun build_dir ->
                let length_found_dir = String.length found_dir in
                let found_dir' = 
                  String.sub spotname 0 length_found_dir
                in
                let rel_spotname =
                  String.sub spotname 
                    (length_found_dir + 1)
                    (String.length spotname - length_found_dir - 1)
                in
                assert (found_dir = found_dir');
                let dir = 
                  if Filename.is_relative build_dir then 
                    Filename.concat found_dir build_dir
                  else build_dir
                in
                Filename.concat dir rel_spotname))
      in
      try load ~load_paths spotname with
      | e -> 
          match alternate_spotname with
          | Some spotname -> load ~load_paths spotname
          | None -> raise e

    (* CR jfuruse: searching algorithm must be reconsidered *)        
    let load_module ?(spit=false) ~load_paths name =
      let spotname = name ^ if spit then ".cmti" else ".cmt" in
      try
        load ~load_paths spotname
      with
      | Failure s ->
          let spitname = name ^ if spit then ".cmt" else ".cmti" in
          Format.printf "%s load failed. Try to load %s@."
            spotname spitname;
          try
            load ~load_paths spitname
          with
          | Failure s' ->
                (* CR jfuruse: ugly! *)
              raise (Failure (s ^ "\n" ^ s'))
  end

  include Load

  let empty_env file =
    { Env.path = file.path;
      cwd = file.cwd;
      load_paths = file.load_paths;
      binding = Binding.empty }

  let invalid_env file =
    { Env.path = file.path;
      cwd = file.cwd;
      load_paths = file.load_paths;
      binding = Binding.invalid }
      
  type result =
      | File_itself
      | Found_at of Annot.t Regioned.t
      | Predefined

  let find_path_in_flat file path : PIdent.t * result =
    let env = 
      (* let env = invalid_env file in *)
      let env = empty_env file in (* CR jfuruse: why it was invalid? *)
      (* let str : Value.structure = Eval.structure env file.flat in *)
      let str : Value.structure = Eval.flat env file.flat in
      Binding.set env.Env.binding str; (* dirty hack *)
      env
    in
    let find_loc pid =
      match  pid.PIdent.filepath with
      | "" -> Predefined
      | _ ->
          (* CR jfuruse: loading twice... *)
          Debug.format "Finding %a@." PIdent.format pid;
          let file = Load.load ~load_paths:[] (cmt_of_file pid.PIdent.filepath) in
          match pid.PIdent.ident with
          | None -> File_itself (* the whole file *)
          | Some id -> 
              Found_at begin try
                Hashtbl.find file.flat id
              with
              | Not_found ->
                  eprintf "Error: find location of id %a failed@."
                    PIdent.format pid;
                  raise Not_found
              end
    in
    
    let eval_and_find kpath =
      (* we need evaluate the path *)
      Debug.format "eval_and_find %s@." (Path.name (snd kpath));
      let v = !!(Eval.find_path env kpath) in
      Debug.format "eval_and_find: Value=%a@." Value.Format.t v;
      match v with
      | Value.Ident id -> id, find_loc id
      | Value.Parameter id -> id, find_loc id
      | Value.Structure (id, _, _)  -> id, find_loc id
      | Value.Closure (id, _, _, _, _) -> id, find_loc id
      | Value.Error (Failure _ as e) -> raise e
      | Value.Error (Load.Old_spot _ as exn) -> raise exn
      | Value.Error exn -> raise exn
    in
    eval_and_find path

  let rec structure_of_file file : string * Value.structure =
    let structure = 
      match file.top with (* The only use of .top *)
      | Some (Saved_type (Typedtree.Saved_implementation str)) -> 
          Eval.structure (empty_env file) str

      | Some (Saved_type (Typedtree.Saved_signature sg)) -> 
          Eval.signature (empty_env file) sg

      | Some (Packed paths) -> 
          let id_strs = 
            List.map (fun path -> 
              let file = Load.load ~load_paths:file.load_paths (cmt_of_file path) in
              let path, str = structure_of_file file in 
              let id = Ocaml.Ident.create_persistent (String.capitalize (Filename.chop_extension (Filename.basename path))) in
              id, { PIdent.filepath = path; ident = None (* Top ! *) }, str
            ) paths 
          in
          List.map (fun (id, pident, str) -> 
            id, (Kind.Module, eager (Value.Structure (pident, str, None)))) id_strs
      | Some _ -> assert false
      | None -> assert false
    in
    file.path, structure

  let str_of_global_ident ~load_paths id =
    assert (Ident.global id);
    let file = Load.load_module ~spit:Spotconfig.print_interface ~load_paths (Ocaml.Ident.name id) in
    structure_of_file file

  let _ = Eval.str_of_global_ident := str_of_global_ident

(*
  let eval_packed env file =
    let f = Load.load ~load_paths:[""] (cmt_of_file (env.Env.cwd ^/ file)) in
    Value.Structure ({ PIdent.filepath = f.path; ident = None },
                    Eval.structure (empty_env f) f.top,
                    None (* packed has no .mli *))

  let _ = Eval.packed := eval_packed
*)

  let dump_elem = function
    | Source_path (Some s) -> eprintf "Source_path: %s@." s
    | Source_path None -> eprintf "Source_path: None@." 
    | Cwd s -> eprintf "Cwd: %s@." s 
    | Load_paths ds -> 
        eprintf "Load_paths: @[%a@]@."
          (Format.list "; " (fun ppf s -> fprintf ppf "%S" s)) ds
    | Argv argv ->
        eprintf "Argv: @[%a@]@."
          (Format.list "; " (fun ppf s -> fprintf ppf "%S" s)) 
            (Array.to_list argv)
    | Saved_types _saved_types ->
        eprintf "Saved_types: ...@." (* CR jfuruse *)

  let dump_elems elems = List.iter dump_elem elems
end
