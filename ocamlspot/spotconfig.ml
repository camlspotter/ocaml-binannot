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

(* Versions and command line options *)

open Format
open Utils

open Spot
open Spoteval

let app_version = "2.0.0"

let version = Printf.sprintf "%s for ocaml %s" app_version Sys.ocaml_version
    
let print_version () = Format.eprintf "ocamlspot %s@." version
    
let rev_anonargs = ref []
let dump_file = ref false
let dump_rannots = ref `None
let dump_tree = ref false
let dump_top = ref false
let dump_flat = ref false
let eager_dump = ref false
let no_definition_analysis = ref false
let strict_time_stamp = ref false
let print_file_info = ref false
let print_interface = ref false
let rest_args_rev = ref []

let _ = 
  Arg.parse 
    [ "--version", Arg.Unit print_version, "\t: print version information";
      "-version", Arg.Unit print_version, "\t: (deprecated)";

      "-n", Arg.Set no_definition_analysis, "\t: no definition analysis";
      "--no-analysis", Arg.Set no_definition_analysis, "\t: no definition analysis";
      
      "-i", Arg.Set print_file_info, "\t: print file information";
      "--info", Arg.Set print_file_info, "\t: print file information";

      "--strict-time-stamp", Arg.Set strict_time_stamp, "\t: error at newer source files than their spots";

      "--interface", Arg.Set print_interface, 
      "\t: show the interface rather than the definition (experimental)";

      "--debug", Arg.Set Debug.on, "\t: print debug information";
      "-debug", Arg.Set Debug.on, "\t: (deprecated)";
      "--dump-file", Arg.Set dump_file, "\t: dump spot file"; 
      "--dump-rannots", Arg.Unit (fun () -> dump_rannots := `Full), "\t: dump loc-annots";
      "--dump-rannots-summary", Arg.Unit (fun () -> dump_rannots := `Summary), "\t: dump loc-annots";
      "--dump-tree", Arg.Set dump_tree, "\t: dump annot tree";
      "--dump-top", Arg.Set dump_top, "\t: dump top"; 
      "--dump-flat", Arg.Set dump_flat, "\t: dump flat"; 
      "--eager-dump", Arg.Set eager_dump, "\t: eager evaluation at dump";
    ]
    (fun s -> rev_anonargs := s :: !rev_anonargs)
    (Printf.sprintf 
        "ocamlspot version %s\n\
\n\
Synopsis:\n\
\tDefinition query:\n\
\t\tocamlspot <search>\n\
\t\tocamlspot query <search>\n\
\n\
\tUse query:\n\
\t\tocamlspot use <search> <targets>\n\
\n\
\tType check and spot creation:\n\
\t\tocamlspot typecheck <args>\n\
\n\
\tRetype check and spot recreation:\n\
\t\tocamlspot recheck <targets>\n\
\n\
\t<search> ::= <file> | <file>:<pos> | <file>:<kind>:<path>\n\
\t<pos> ::= l<line>c<column> | b<bytes>\n\
\t<kind> ::= v|t|e|m|mt|c|ct\n\
\n\
Options:"
        version)

let dump_file = !dump_file
let dump_rannots = !dump_rannots
let dump_tree = !dump_tree
let dump_top  = !dump_top 
let dump_flat = !dump_flat
let eager_dump = !eager_dump
let no_definition_analysis = !no_definition_analysis
let strict_time_stamp = !strict_time_stamp
let print_file_info = !print_file_info
let print_interface = !print_interface

let dump_any = 
  dump_file || dump_rannots <> `None || dump_tree || dump_top || dump_flat

module SearchSpec = struct
  type t = 
      | Pos of Position.t
      | Kind of Kind.t * Path.t

  let parse s : string * t =
    try
      let at = String.rindex s ':' in
      try
        let at2 = String.rindex_from s (at - 1) ':' in
        String.sub s 0 at2,
        Kind 
          (Kind.from_string (String.sub s (at2+1) (at-at2-1)),
           let s = String.sub s (at+1) (String.length s - at - 1) in 
           try Path.parse s with
           | _ -> failwith ("illegal path: " ^ s))
      with
      | Invalid_argument _ | Not_found -> 
          String.sub s 0 at,
          Pos 
		  (Position.parse 
                     (String.sub s (at+1) (String.length s - at - 1)))
    with
    | Failure s -> failwith s
    | Position.Parse_failure s -> failwith ("illegal file:pos: " ^ s)
    | Not_found -> failwith ("strange search spec: " ^ s)

  let to_string = function
    | Pos pos -> ":" ^ Position.to_string pos
    | Kind (k, path) -> 
        Printf.sprintf ":%s:%s"
          (String.capitalize (Kind.to_string k))
          (Path.name path)
end

let rest_args = List.rev !rest_args_rev
let anonargs = List.rev !rev_anonargs

let mode = 
  if dump_any then begin 
    match anonargs with
    | [ spec ] -> `Dump spec
    | _ -> failwith "You cannot specify mode with --dump"
  end else begin
    Debug.format "anonargs = [%a]@." 
      (Format.list " " Format.pp_print_string) 
      anonargs;
    match anonargs with
    | [ "query"; spec ] -> `Query (SearchSpec.parse spec)
    | "use" :: spec :: targets -> `Use (SearchSpec.parse spec, targets)
    | "typecheck" :: rest -> `Typecheck rest
    | "recheck" :: rest -> `Recheck rest
    | [ spec ] -> `Query (SearchSpec.parse spec)
    | _ -> failwith "At most one search spec is allowed"
  end
