(* Build libraries of .cmo files *)

(* Format of a library file:
      Obj.magic number (Config.cma_magic_number)
      absolute offset of content table
      blocks of relocatable bytecode
      content table = list of compilation units
*)

val create_archive: string list -> string -> unit

type error =
    File_not_found of string
  | Not_an_object_file of string

exception Error of error

val report_error: error -> unit
