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

open Utils
open Indexed

open Spot
open Spoteval

module C = Spotconfig

module File = Spotfile.Make(C)

module Dump = struct
  (* mainly debugging purpose *)
  let file f = File.dump_file f
  ;;

  let rannots_full file =
    eprintf "@[<2>rannots =@ @[<v>%a@]@]@."
      (Format.list "; " (Regioned.format Annot.format))
      file.File.rannots
  ;;

  let rannots_summary file =
    eprintf "@[<2>rannots =@ @[<v>%a@]@]@."
      (Format.list "; " (Regioned.format Annot.summary))
      file.File.rannots
  ;;

  let tree file = Tree.dump !!(file.File.tree)
  ;;

  let top file =
    match file.File.top with
    | None -> eprintf "NoTOP"
    | Some (File.Saved_type stype) -> eprintf "%a@." Abstraction.Format.saved_type stype
    | Some (File.Packed paths) ->
        eprintf "Packed [%a]@." (Format.list "; " (fun ppf s -> fprintf ppf "%S" s)) paths
  ;;

  let flat file =
    eprintf "@[<2>flat =@ @[%a@]@]@."
      (Format.list "; "
         (fun ppf (id, rannot) ->
           fprintf ppf "%s : %a"
             (Ident.name id)
             (Regioned.format Annot.format) rannot))
      (Hashtbl.to_list file.File.flat)
  ;;

end

module Main = struct

  let bye return =
    printf "BYE!@.";
    exit return

  let load path =
    let path = File.cmt_of_file path in
    let file = File.load ~load_paths: ["."] path in

    if C.dump_file then Dump.file file;
    if C.dump_rannots = `Full then Dump.rannots_full file;
    if C.dump_rannots = `Summary then Dump.rannots_summary file;
    if C.dump_tree then Dump.tree file;
    if C.dump_top then Dump.top file;
    if C.dump_flat then Dump.flat file;

    if C.print_file_info then
      printf "Compile: %s@."
        (String.concat " "
            (List.map Command.escaped_for_shell
                (Array.to_list file.File.argv)));

    if C.print_file_info then
      printf "@[<v2>Included_dirs:@ %a@]@."
        (Format.list "" pp_print_string)
        file.File.load_paths;

    file
  ;;

  let query_by_kind_path file kind path =
    try Some (File.find_path_in_flat file (kind, path)) with Not_found -> None
  ;;

  let print_query_result kind = function
    | None -> printf "Spot: no spot@."
    | Some (pident, res) ->
        match res with
        | File.File_itself ->
            printf "Spot: <%s:all>@." pident.PIdent.filepath
        | File.Found_at rannot ->
            printf "Spot: <%s:%s>@."
              pident.PIdent.filepath
              (Region.to_string rannot.Regioned.region)
        | File.Predefined ->
            printf "Spot: %a: predefined %s@."
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

        List.iter (fun annot -> printf "%a@." Annot.format annot) annots;

        (* Tree is an older format. XTree is a newer which is the same as one for Spot *)
        printf "Tree: %s@." (Region.to_string r);
        printf "XTree: <%s:%s>@." file.File.path (Region.to_string r);

        (* Find the innermost module *)
        let rec find_module_path = function
          | [] -> []
          | ( { Regioned.value = Annot.Def (Kind.Module, id, _); _ }
            | { Regioned.value = Annot.Def (Kind.Module_type, id, _); _} ) :: ls ->
              id :: find_module_path ls
          | _ :: ls -> find_module_path ls
        in
        printf "In_module: %s@."
          (String.concat "." (List.map Ocaml.Ident.name (List.rev (find_module_path treepath))));

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
              printf "Val: val %s : @[%a@]@."
                (Ocaml.Ident.name id)
                (Printtyp.type_scheme ~with_pos:false) typ
          | _ -> ()
        in
        print_sig_entry annots;

        annots
  ;;

  let query path spec =
    Debug.format "ocamlspot %s%s@." path (C.SearchSpec.to_string spec);
    Debug.format "cwd: %s@." (Sys.getcwd ());
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
        eprintf "Error: %s@." s;
        bye 1
    | File.Old_spot (_spot, source) ->
        eprintf "Error: source %s is newer than the spot@." source;
        bye 1
    | e ->
        eprintf "uncaught exception: %s@." (Printexc.to_string e);
        bye 1

  let use path spec targets =
    Debug.format "ocamlspot %s%s@." path (C.SearchSpec.to_string spec);
    Debug.format "cwd: %s@." (Sys.getcwd ());
    let targets = if targets = [] then ["."] else targets in
    let file = load path in

    let find_by_kind_path k path found =
      Unix.find targets ~f:(fun pathname ->
        match Filename.split_extension pathname.Unix.base with
        | _body, (".cmti" | ".cmt") ->
          let file = load pathname.Unix.path in
          Debug.format "Searching %s@." pathname.Unix.path;
          let base_ident = function
            | Path.Pident id -> Ocaml.Ident.name id
            | Path.Pdot (_, name, _) -> name
            | Path.Papply _ -> assert false
          in
          let base = base_ident path in
          List.iter (function
            | { Regioned.region= region; value= Annot.Use (k', path'); } when k = k' && base = base_ident path' ->
              begin match query_by_kind_path file k' path' with
              | Some found' when found = found' ->
                  printf "<%s:%s>: %s@."
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
      | None -> printf "No query result found.@.";
      | Some found -> find_by_kind_path k path found
    in

    let by_pos file pos =
      eprintf "Searching %s:%s ...@." file.File.path (Position.to_string pos);
      match List.find_map_opt (function
        | Annot.Def (kind, id, _) -> Some (`Def (kind, id))
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

(* CR jfuruse
  let typecheck args =
    let command = Sys.argv.(0) :: args in
    prerr_endline (String.concat " " command);
    Xmain.main (Array.of_list command)
  ;;

  let recheck files =
    let recheck mlpath =
      Debug.format "cwd: %s@." (Sys.getcwd ());
      let path = File.cmt_of_file mlpath in
      let file = File.load ~load_paths: ["."] path in

      printf "Compile: %s@."
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
    | `Use ((path, spec), targets)-> use path spec targets
(*
    | `Typecheck args -> typecheck args
    | `Recheck args -> recheck args
    | `Recheck _ -> assert false
*)
    | _ -> assert false
end

let _ = Main.main ()
