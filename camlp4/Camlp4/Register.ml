(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)
open PreCast;

value sig_item_parser = ref (fun ?directive_handler:(_) _ _ -> failwith "No interface parser");
value str_item_parser = ref (fun ?directive_handler:(_) _ _ -> failwith "No implementation parser");

value sig_item_printer = ref (fun ?input_file:(_) ?output_file:(_) _ -> failwith "No interface printer");
value str_item_printer = ref (fun ?input_file:(_) ?output_file:(_) _ -> failwith "No implementation printer");

value callbacks = Queue.create ();

value iter_and_take_callbacks f =
  let rec loop () = loop (f (Queue.take callbacks)) in
  try loop () with [ Queue.Empty -> () ];

value declare_dyn_module m f =
  (* let () = Format.eprintf "declare_dyn_module: %s@." m in *)
  Queue.add (m, f) callbacks;

module Plugin (Id : Sig.Id.S) (Maker : functor (Unit : sig end) -> sig end) = struct
  declare_dyn_module Id.name (fun _ -> let module M = Maker (struct end) in ());
end;

module SyntaxExtension (Id : Sig.Id.S) (Maker : Sig.SyntaxExtension.S) = struct
  declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
end;

module OCamlSyntaxExtension
  (Id : Sig.Id.S) (Maker : functor (Syn : Sig.Camlp4Syntax.S) -> Sig.Camlp4Syntax.S) =
struct
  declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
end;

module SyntaxPlugin (Id : Sig.Id.S) (Maker : functor (Syn : Sig.Syntax.S) -> sig end) = struct
  declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
end;

module Printer
  (Id : Sig.Id.S) (Maker : functor (Syn : Sig.Syntax.S)
                                -> Sig.Printer.S with module Ast = Syn.Ast) =
struct
  declare_dyn_module Id.name (fun _ ->
    let module M = Maker Syntax in do {
      sig_item_printer.val := M.print_interf;
      str_item_printer.val := M.print_implem;
    });
end;

module OCamlPrinter
  (Id : Sig.Id.S) (Maker : functor (Syn : Sig.Camlp4Syntax.S)
                                -> Sig.Printer.S with module Ast = Syn.Ast) =
struct
  declare_dyn_module Id.name (fun _ ->
    let module M = Maker Syntax in do {
      sig_item_printer.val := M.print_interf;
      str_item_printer.val := M.print_implem;
    });
end;

module Parser
  (Id : Sig.Id.S) (Maker : functor (Ast : Sig.Ast.S)
                                -> Sig.Parser.S with module Ast = Ast) =
struct
  declare_dyn_module Id.name (fun _ ->
    let module M = Maker PreCast.Ast in do {
      sig_item_parser.val := M.parse_interf;
      str_item_parser.val := M.parse_implem;
    });
end;

module OCamlParser
  (Id : Sig.Id.S) (Maker : functor (Ast : Sig.Camlp4Ast.S)
                                -> Sig.Parser.S with module Ast = Ast) =
struct
  declare_dyn_module Id.name (fun _ ->
    let module M = Maker PreCast.Ast in do {
      sig_item_parser.val := M.parse_interf;
      str_item_parser.val := M.parse_implem;
    });
end;

module AstFilter
  (Id : Sig.Id.S) (Maker : functor (F : Sig.AstFilters.S) -> sig end) =
struct
  declare_dyn_module Id.name (fun _ -> let module M = Maker AstFilters in ());
end;

let module M = Syntax.Parser in do {
  sig_item_parser.val := M.parse_interf;
  str_item_parser.val := M.parse_implem;
};

module CurrentParser = struct
  module Ast = Ast;
  value parse_interf ?directive_handler loc strm =
    sig_item_parser.val ?directive_handler loc strm;
  value parse_implem ?directive_handler loc strm =
    str_item_parser.val ?directive_handler loc strm; 
end;

module CurrentPrinter = struct
  module Ast = Ast;
  value print_interf ?input_file ?output_file ast =
    sig_item_printer.val ?input_file ?output_file ast;
  value print_implem ?input_file ?output_file ast =
    str_item_printer.val ?input_file ?output_file ast; 
end;

