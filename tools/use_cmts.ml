open Types
open Typedtree

let print_locs_arg = ref false
let with_mli_arg = ref true
let all_cmi_arg = ref true

module StringMap = Map.Make(String)

type val_used = {
  mutable val_used : int;
}

type node = {
  mutable node_map : node StringMap.t;
  node_name : string;
  mutable node_used : int;
  mutable node_locs : Location.t list;
  node_parent : node;
}

let rec values = {
  node_map = StringMap.empty;
  node_name = "";
  node_used = 0;
  node_locs = [];
  node_parent = values;
}

let get_node name node =
  try
    StringMap.find name node.node_map
  with Not_found ->
    let new_node = {
      node_name = name;
      node_parent = node;
      node_map = StringMap.empty;
      node_used = 0;
      node_locs = [];
    } in
    node.node_map <- StringMap.add name new_node node.node_map;
    new_node



let rec add_signature node sg =
  match sg with
      [] -> ()
    | item :: sg ->
      begin match item with
	  Sig_value (id, _) ->
	    let name = Ident.name id in
	    let _node = get_node name node in
	    ()
	| Sig_module (id, mty, _) ->
	  let name = Ident.name id in
	  let node = get_node name node in
	  begin match mty with
	      Mty_ident _ -> ()
	    | Mty_signature sg -> add_signature node sg
	    | Mty_functor _ -> ()
	  end
	| _ -> ()
      end;
      add_signature node sg


let read_cmi filename =
  if not !with_mli_arg ||
    (let mli_filename = (Filename.chop_suffix filename ".cmi") ^ ".mli" in
     Sys.file_exists mli_filename ) then
    let ic = open_in filename in
    let len_magic_number = String.length Config.cmo_magic_number in
    let magic_number = String.create len_magic_number in
    really_input ic magic_number 0 len_magic_number;
    if magic_number = Config.cmi_magic_number then begin
      let (name, sign, comps) = input_value ic in
      let _crcs = input_value ic in
      close_in ic;
      let node = get_node name values in
      add_signature node sign
    end
    else
      failwith (Printf.sprintf "%s is not an interface file" filename)

let rec lookup_path node path =
  match path with
      Path.Pident id ->
	if Ident.persistent id then
	  let name = Ident.name id in
	  StringMap.find name node.node_map
	else raise Not_found
    | Path.Pdot (path, s, _) ->
      let node = lookup_path node path in
      node.node_used <- node.node_used + 1;
      StringMap.find s node.node_map
    | _ -> raise Not_found

module TypedtreeIteratorArgument = struct
  include DefaultIteratorArgument

  let enter_expression exp =
    match exp.exp_desc with
	Texp_ident ( path , _ ) ->
	  begin try
		  let node = lookup_path values path in
		  node.node_used <- node.node_used + 1;
		  if !print_locs_arg then
		  node.node_locs <- exp.exp_loc :: node.node_locs
	    with Not_found -> ()
	  end
      | _ -> ()

  let enter_module_expr exp =
    match exp.mod_desc with
	Tmod_ident  path ->
	  begin try
		  let node = lookup_path values path in
		  node.node_used <- node.node_used + 1;
		  if !print_locs_arg then
		    node.node_locs <- exp.mod_loc :: node.node_locs
	    with Not_found -> ()
	  end
      | _ -> ()



end

module TypedtreeIterator = MakeIterator(TypedtreeIteratorArgument)


let read_cmi filename =
  if not !with_mli_arg ||
    (let mli_filename = (Filename.chop_suffix filename ".cmi") ^ ".mli" in
     Sys.file_exists mli_filename ) then
    let ic = open_in filename in
    let len_magic_number = String.length Config.cmo_magic_number in
    let magic_number = String.create len_magic_number in
    really_input ic magic_number 0 len_magic_number;
    if magic_number = Config.cmi_magic_number then begin
      let (name, sign, comps) = input_value ic in
      let _crcs = input_value ic in
      close_in ic;
      let node = get_node name values in
      add_signature node sign
    end
    else
      failwith (Printf.sprintf "%s is not an interface file" filename)

let done_something = ref false

let read_dir dirname =
  done_something := true;
  let files = Sys.readdir dirname in
  if !all_cmi_arg then
    Array.iter (fun file ->
      if Filename.check_suffix file ".cmi" then
	read_cmi ( Filename.concat dirname file )
    ) files;
  Array.iter (fun file ->
    if Filename.check_suffix file ".cmt" then
      let filename = Filename.concat dirname file in
      let ic = open_in filename in
      let saved_array = (input_value ic : saved_type array) in
      close_in ic;
      match saved_array with
	  [| Saved_implementation str |] ->
	    TypedtreeIterator.iter_structure str
	| _ -> assert false
  ) files


let (msg_file, msg_line, msg_chars, msg_to, msg_colon, msg_head) =
  ("File \"", "\", line ", ", characters ", "-", ":", "")

let string_of_location loc =
  let open Location in
      let open Lexing in
  let (file, line, startchar) = Location.get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  let (startchar, endchar) =
    if startchar < 0 then (0, 1) else (startchar, endchar)
  in
  Printf.sprintf
    "%s%s%s%i%s%i%s%i%s\n%s" msg_file file msg_line line
    msg_chars startchar
    msg_to endchar msg_colon msg_head

;;

let finder_arg = ref []

let find_val ident =
  let open Longident in
      print_locs_arg := true;
      all_cmi_arg := false;
      let lident = Longident.parse ident in
      finder_arg := (lident, values) :: !finder_arg;
      let rec find_longident_node node lident =
	match lident with
	    Lident name -> get_node name node
	  | Ldot (lident, name) ->
	    get_node name (find_longident_node node lident)
	  | Lapply _ -> assert false
      in
      ignore (find_longident_node values lident)


let arg_list = [
  "-I", Arg.String read_dir, " <dir> : read directory <dir> for .cmt/.cmti files";
  "-cmi", Arg.String (fun filename ->
    all_cmi_arg := false;
    read_cmi filename), " <cmi_file> : read cmi file";
  "-find-val", Arg.String find_val, " <path> : find uses of value identifier";
  "-loc", Arg.Set print_locs_arg, " : print used sites";
  "-no-mli", Arg.Clear with_mli_arg, " : also consider .cmi files with no .mli file";
]
let arg_usage = String.concat "\n"
[
"use_cmts [options] [-I directory]* : analyse annotation files (.cmt) ";
"";
"\tAnnotation files (.cmt/.cmti files) are generated when compiling";
"\twith the -annot option (after ocaml version 3.13).";
"";
"Typical usages:";
"";
"\tuse_cmts -I dir1 -I dir2 -I dir3";
"";
"\t\tFor all .mli files in dir1, dir2, dir3, print which identifiers";
"\t\tare not used in these directories.";
"";
"\tuse_cmts -cmi dir1/x.cmi -I dir1 -I dir2 -I dir3";
"";
"\t\tPrint which identifiers from module X in dir1 are not used in";
"\t\tthe directories dir1, dir2 and dir3.";
"";
"\tuse_cmts -find-val X.foo -I dir1 -I dir2 -I dir3";
"";
"\t\tPrint where the value X.foo is used in";
"\t\tthe directories dir1, dir2 and dir3.";
"";
"Available Options:";
]
let _ =
  Arg.parse arg_list read_cmi arg_usage;

  if not !done_something then begin
    Arg.usage arg_list arg_usage;
    exit 2
  end;

  let rec print path node =
    if node.node_used = 0 then
      if node.node_map = StringMap.empty then
	Printf.fprintf stdout "%s not used\n%!" (String.concat "." (List.rev path))
      else
	begin
	  StringMap.iter (fun name node ->
	    print (name :: path) node
	  ) node.node_map
	end
    else begin
      if !print_locs_arg then begin
	Printf.printf "Locations for ident: %s\n%!"  (String.concat "." (List.rev path));
	List.iter (fun loc ->
	  Printf.printf "%s%!" (string_of_location loc)
	) node.node_locs;
      end;
      StringMap.iter (fun name node ->
	print (name :: path) node
      ) node.node_map
    end
  in
  print [] values

