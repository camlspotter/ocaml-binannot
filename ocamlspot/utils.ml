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

let printf  = Format.printf
let fprintf = Format.fprintf
let eprintf = Format.eprintf
let pp_print_string = Format.pp_print_string

module List = struct
  include List

  let rec find_map_opt f = function
    | [] -> None
    | x::xs ->
        match f x with
        | Some v -> Some v
        | None -> find_map_opt f xs

  let rec filter_map f lst =
    List.rev (List.fold_left (fun st x ->
      match f x with
      | Some v -> v :: st
      | None -> st) [] lst)
end

module Debug = struct
  let on = ref false

  let rec consume_args _ = Obj.magic consume_args

(*
  let printf fmt = 
    if !on then 
      Printf.kprintf (fun s -> 
	if !on then prerr_endline ("DEBUG: " ^ s)) fmt
    else consume_args
*)

  let format fmt = 
    if !on then eprintf fmt
    else Format.ifprintf Format.err_formatter fmt
end

module Lazy = struct
  include Lazy

  module Open = struct
    let (!!) = Lazy.force 
    let eager = Lazy.lazy_from_val
  end

  open Open

  let peek v = if lazy_is_val v then Some (!!v) else None
      
  let apply f v = 
    if lazy_is_val v then eager (f !!v)
    else lazy (f !!v)

  let is_val = lazy_is_val
end

include Lazy.Open

module Filename = struct
  include Filename
      
  let split_extension s = 
    try
      let body = chop_extension s in
      body, 
      String.sub s 
	(String.length body) 
	(String.length s - String.length body)
    with
    | Invalid_argument _ -> s, ""

  module Open = struct
    let (^/) p1 p2 =
      if Filename.is_relative p2 then Filename.concat p1 p2 else p2
  end
end

include Filename.Open

module Format = struct
  include Format
  type t = formatter
  let rec list sep f ppf = function
    | [] -> ()
    | [x] -> f ppf x
    | x::xs -> 
        fprintf ppf "@[%a@]%t@,%a" 
	  f x
	  (fun ppf -> fprintf ppf "%s" sep)
	  (list sep f) xs

  let option f ppf = function
    | None -> fprintf ppf "None"
    | Some v -> fprintf ppf "Some(%a)" f v 

  let lazy_ p ppf v =
    if Lazy.is_val v then p ppf (Lazy.Open.(!!) v)
    else fprintf ppf "lazy"
end

module Option = struct
  let map ~f = function
    | None -> None
    | Some v -> Some (f v)

  let bind v f = match v with
    | None -> None
    | Some v -> f v

  let iter ~f = function
    | None -> ()
    | Some v -> f v
end

exception Finally of exn * exn
;;

let protect ~f x ~(finally : 'a -> unit) =
  let res =
    try f x with exn ->
      (try finally x with final_exn -> raise (Finally (exn, final_exn)));
    raise exn
  in
  finally x;
  res
;;

module Unix = struct
  include Unix

  (* run [f] on files in [path] *)
  let folddir ~f ~init path =
    let dh = opendir path in
    protect ~f:(fun () ->
      let rec loop st =
	try
	  let st' = f st (readdir dh) in
	  loop st'
	with
	| End_of_file -> st
      in
      loop init)
      ~finally:(fun () -> closedir dh) ()
  ;;

  module Inodes = Set.Make(struct
    type t = int
    let compare : int -> int -> int = fun x y -> compare x y
  end)
  ;;
  
  type path = 
      { dir : string;
	base : string;
	path : string; (* dir / name *)
	stat : [ `Ok of stats | `Error of exn ];
	depth : int;
      }

  let path ~depth ~dir base =
    let path = match Filename.concat dir base with
      | "./." -> "."
      | s -> s
    in
    { dir = dir;
      base = base;
      path = path;
      depth = depth; 
      stat = try `Ok (stat path) with e -> `Error e;
    }
  ;;

  let kind path =
    match path.stat with
    | `Error _exn -> None
    | `Ok stat -> Some stat.st_kind
  ;;

  let is_dir path = kind path = Some S_DIR

  let inode path = 
    match path.stat with
    | `Ok stat -> Some stat.st_ino
    | `Error _ -> None
  ;;

  exception Prune

  let prune () = raise Prune

  let find ~f fnames =

    (* visited cache *)
    let visited = ref Inodes.empty in
    let if_not_visited_then path ~f = match inode path with
      | None -> ()
      | Some inode ->
	  if Inodes.mem inode !visited then ()
	  else begin
	    visited := Inodes.add inode !visited;
	    f path
	  end
    in

    let rec find_dir pth =
      try 
	f pth;
	let subdirs =
	  folddir pth.path ~init:[] ~f:(fun dirs -> function
	    | "." | ".." -> dirs
	    | name -> 
		let pth = path ~depth:(pth.depth + 1) ~dir:pth.path name in
		if try is_dir pth with _ -> false then pth::dirs
		else begin find_non_dir pth; dirs end)
	in
	List.iter (if_not_visited_then ~f:find_dir) subdirs
      with
      | Prune -> ()

    and find_non_dir path = try f path with Prune -> ()
    in

    List.iter (fun fname ->
      let path = 
	path ~depth: 0 ~dir:(Filename.dirname fname) (Filename.basename fname)
      in
      if is_dir path then find_dir path
      else find_non_dir path) fnames
  ;;
end

module Hashtbl = struct
  include Hashtbl
  let to_list tbl = Hashtbl.fold (fun k v st -> (k,v)::st) tbl []
end

let protect name f v =
  try f v with e ->
    eprintf "Error: %s: %s@." name (Printexc.to_string e)

