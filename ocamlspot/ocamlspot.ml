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

(* Keep the original modules *)
module Ident0 = Ident

open Spot
open Spoteval

module C = Spotconfig

module File = Spotfile.Make(C)
    
module Dump = struct
  (* mainly debugging purpose *)
  let file f = File.dump_file f
  ;;

  let rannots_full file = 
    Format.eprintf "@[<2>rannots =@ @[<v>%a@]@]@."
      (Format.list "; " (Regioned.format Annot.format))
      file.File.rannots
  ;;
  
  let rannots_summary file = 
    Format.eprintf "@[<2>rannots =@ @[<v>%a@]@]@."
      (Format.list "; " (Regioned.format Annot.summary))
      file.File.rannots
  ;;
  
  let tree file = Tree.dump !!(file.File.tree)
  ;;

  let top file = 
    match file.File.top with
    | None -> Format.eprintf "NoTOP"
    | Some (File.Saved_type saved_type) -> 
        let abst = 
          match saved_type with
          | Typedtree.Saved_implementation str | Typedtree.Saved_structure str -> Abstraction.structure str
          | Typedtree.Saved_signature sg -> Abstraction.signature sg
          | Typedtree.Saved_structure_item _
          | Typedtree.Saved_signature_item _
          | Typedtree.Saved_expression _
          | Typedtree.Saved_module_type _
          | Typedtree.Saved_pattern _
          | Typedtree.Saved_class_expr _ -> assert false
        in
        Format.eprintf "@[<2>{ @[%a@] }@]@." (Format.list "; " Abstraction.format) abst
    | Some (File.Packed paths) -> 
        Format.eprintf "Packed [%a]@." (Format.list "; " (fun ppf s -> Format.fprintf ppf "%S" s)) paths
  ;;

  let flat file =
    Format.eprintf "@[<2>flat =@ @[%a@]@]@." 
      (Format.list "; "
         (fun ppf (id, rannot) ->
           Format.fprintf ppf "%s : %a"
             (Ident.name id)
             (Regioned.format Annot.format) rannot))
      (Hashtbl.to_list file.File.flat)
           
(*
    let str = 
      let env = File.invalid_env file in
      let str = Eval.structure env file.File.flat in
      Binding.set env.Env.binding str; (* dirty hack (dup code) *)
      str
    in
    if C.eager_dump then begin
      let module Enforcer = Value.Enforcer(struct end) in
      Enforcer.structure str;
    end;
    Format.eprintf "==>@.@[%a@]@." Value.Format.structure str;
*)
  ;;

end

module Main = struct

  let bye return =
    Format.printf "BYE!@.";
    exit return

  let load path =

    let file = File.load ~load_paths: ["."] path in
    
    if C.dump_file then Dump.file file;
    if C.dump_rannots = `Full then Dump.rannots_full file;
    if C.dump_rannots = `Summary then Dump.rannots_summary file;
    if C.dump_tree then Dump.tree file;
    if C.dump_top then Dump.top file;
    if C.dump_flat then Dump.flat file;

(* CR jfuruse
    if C.print_file_info then
      Format.printf "Compile: %s@."
        (String.concat " " 
            (List.map Command.escaped_for_shell 
                (Array.to_list file.File.argv)));

    if C.print_file_info then
      Format.printf "@[<v2>Included_dirs:@ %a@]@."
        (Format.list "" Format.pp_print_string)
        file.File.load_paths;
*)

    file
  ;;

  let query_by_kind_path file kind path = 
    try Some (File.find_path_in_flat file (kind, path)) with Not_found -> None
  ;;

  let print_query_result kind = function
    | None -> Format.printf "Spot: no spot@."
    | Some (pident, res) -> 
        match res with
	| File.File_itself ->
            Format.printf "Spot: <%s:all>@." pident.PIdent.path
	| File.Found_at rannot ->
            Format.printf "Spot: <%s:%s>@."
              pident.PIdent.path
              (Region.to_string rannot.Regioned.region)
	| File.Predefined ->
            Format.printf "Spot: %a: predefined %s@."
              PIdent.format pident
              (Kind.name kind);
  ;;
    
  let query_by_pos file pos = 
    let probe = Region.point pos in
    let treepath = 
      (* subtree is not used *)
      List.map fst (Tree.find_path_contains probe !!(file.File.tree))
    in
    match treepath with
    | [] -> 
        failwith (Printf.sprintf "nothing at %s" (Position.to_string pos))

    | { Regioned.region = r; _ } :: _ ->
	
	(* find annots bound to the region *)
        let annots = 
	  List.filter_map (fun rannot ->
	    if Region.compare r rannot.Regioned.region = `Same then 
	      Some rannot.Regioned.value
	    else None)
	    treepath
        in

(* CR jfuruse
	(* annots and region improvement by path *)
	let annots, r = 
	  match 
	    (* only the first Use *)
	    List.find_map_opt (function
	      | Annot.Use (_kind, path) -> 
		  (* Find subpath *)
		  begin match Pathreparse.get file.File.path r pos path with    
		  | None -> None
		  | Some (path', r) -> 
		      if path = path' then None (* as original *)
		      else Some ([Annot.Use (Kind.Module, path')], r)
		  end
	      | _ -> None) annots
	  with
	  | None -> annots, r
	  | Some (annots, r) -> annots, r
	in
*)
	  
        List.iter (fun annot -> Format.printf "%a@." Annot.format annot) annots;

	(* Tree is an older format. XTree is a newer which is the same as one for Spot *)
        Format.printf "Tree: %s@." (Region.to_string r);
        Format.printf "XTree: <%s:%s>@." file.File.path (Region.to_string r);

(*
	(* Find the innermost module *)
        let rec find_module_path = function
          | [] -> []
          | { Regioned.value = Annot.Str (Abstraction.Str_module (id, _)); _ } :: ls
          | { Regioned.value = Annot.Str (Abstraction.Str_modtype (id, _)); _ } :: ls ->
              id :: find_module_path ls
          | _ :: ls -> find_module_path ls
        in
        Format.printf "In_module: %s@."
          (String.concat "." (List.map Ident0.name (List.rev (find_module_path treepath))));
*)

        (* print "Val: val name : type" if it is a Str: val *)
        let print_sig_entry annots =
          let rec find_type = function
            | Annot.Type typ :: _ -> Some typ
            | _::xs -> find_type xs
            | [] -> None
          in
          let rec find_str_value = function
            | Annot.Def (_, id, _) :: _ -> Some id
            | _::xs -> find_str_value xs
            | [] -> None
          in
          match find_type annots, find_str_value annots with
          | Some typ, Some id ->
              Format.printf "Val: val %s : @[%a@]@."
                (Ident0.name id)
                (Printtyp.type_scheme ~with_pos:false) typ
          | _ -> ()
        in
        print_sig_entry annots;

	annots
  ;;

  let query path spec = 
    (* CR jfuruse: dup *)
    Debug.format "ocamlspot %s%s@." path (C.SearchSpec.to_string spec);
    Debug.format "cwd: %s@." (Sys.getcwd ());
    let path = File.cmt_of_file path in
    let file = load path in

    begin match spec with
    | C.SearchSpec.Kind (k,path) -> 
        print_query_result k (query_by_kind_path file k path)

    | C.SearchSpec.Pos pos -> 
	let annots = query_by_pos file pos in
        if not C.no_definition_analysis then begin
          List.iter (function
            | Annot.Use (kind, path) -> 
		print_query_result kind (query_by_kind_path file kind path)
            | _ -> ()) annots
        end
    end;

    bye 0

  let query file spec =
    try query file spec with
    | Failure s ->
        Format.eprintf "Error: %s@." s;
        bye 1
    | File.Old_spot (_spot, source) ->
        Format.eprintf "Error: source %s is newer than the spot@." source;
        bye 1
    | e ->
        Format.eprintf "uncaught exception: %s@." (Printexc.to_string e);
        bye 1

(*
  let use path spec targets =
    let targets = if targets = [] then ["."] else targets in
    (* CR jfuruse: dup *)
    Debug.format "ocamlspot %s%s@." path (C.SearchSpec.to_string spec);
    Debug.format "cwd: %s@." (Sys.getcwd ());
    let path = File.cmt_of_file path in
    let file = load path in

    let find_by_kind_path k path found =
      Unix.find targets ~f:(fun pathname ->
	match Filename.split_extension pathname.Unix.base with
	| _body, (".cmti" | ".cmt") ->
	  let file = load pathname.Unix.path in
	  Debug.format "Searching %s@." pathname.Unix.path;
	  let base_ident = function
	    | Path.Pident id -> Ident0.name id
	    | Path.Pdot (_, name, _) -> name
	    | Path.Papply _ -> assert false
	  in
	  let base = base_ident path in
	  List.iter (function
	    | { Regioned.region= region; value= Annot.Use (k', path'); } when k = k' && base = base_ident path' ->
	      begin match query_by_kind_path file k' path' with
	      | Some found' when found = found' ->
		  Format.printf "<%s:%s>: %s@." 
		    file.File.path
		    (Region.to_string region)
		    (Path.name path)
	      | None | Some _ -> ()
	      end
	    | _ -> ()) file.File.rannots
	| _ -> ());
    in

    let by_kind_path file k path =
      Debug.format "Searching %s:%s:%s ...@." 
	file.File.path 
	(Kind.to_string k) 
	(Path.name path); 
      let res = query_by_kind_path file k path in
      print_query_result k res;
      match res with
      | None -> Format.printf "No query result found.@.";
      | Some found -> find_by_kind_path k path found
    in

    let by_pos file pos = 
      Format.eprintf "Searching %s:%s ...@." 
	file.File.path 
	(Position.to_string pos);
      match List.find_map_opt (function 
	| Annot.Str str_item -> 
	    begin match Abstraction.ident_of_structure_item str_item with
	    | Some v -> Some (`Def v)
	    | None -> None
	    end
	| Annot.Use (kind, path) -> Some (`Use (kind, path))
	| _ -> None) (query_by_pos file pos)
      with
      | Some (`Def (k, id)) -> by_kind_path file k (Path.Pident id)
      | Some (`Use (k, path)) -> by_kind_path file k path
      | None -> ()
    in

    begin match spec with
    | C.SearchSpec.Kind (k,path) -> by_kind_path file k path
    | C.SearchSpec.Pos pos -> by_pos file pos
    end;
    bye 0
  ;;
*)

  let typecheck args =
    let command = Sys.argv.(0) :: args in
    prerr_endline (String.concat " " command);
    Xmain.main (Array.of_list command)
  ;;

(* CR jfuruse
  let recheck files =
    let recheck mlpath =
      Debug.format "cwd: %s@." (Sys.getcwd ());
      let path = File.cmt_of_file mlpath in
      let file = File.load ~load_paths: ["."] path in
    
      Format.printf "Compile: %s@."
        (String.concat " " 
          (List.map Command.escaped_for_shell 
            (Array.to_list file.File.argv)));
      let command = 
	Sys.argv.(0) :: List.tl (Array.to_list file.File.argv) 
      in
      Xmain.main (Array.of_list command)
    in
    List.iter recheck files
  ;;
*)

  let main () = 
    match C.mode with
    | `Dump path -> ignore (load path)
    | `Query (path, spec) -> query path spec
(*
    | `Typecheck args -> typecheck args
*)
(*
    | `Recheck args -> recheck args
*)
(*
    | `Recheck _ -> assert false
    | `Use ((path, spec), targets)-> use path spec targets
*)
    | _ -> assert false
end

let _ = Main.main ()
