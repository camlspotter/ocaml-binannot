(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* To print values *)

open Misc
open Formatmsg
open Longident
open Path
open Types

module type OBJ =
  sig
    type t

    val obj : t -> 'a
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
  end

module type S =
  sig
    type t

    val install_printer : Path.t -> Types.type_expr -> (t -> unit) -> unit
    val remove_printer : Path.t -> unit

    val print_exception : t -> unit
    val print_value :
          int -> int -> (int -> t -> Types.type_expr -> bool) ->
          Env.t -> t -> type_expr -> unit
  end

module Make(O : OBJ) = struct

    type t = O.t

    (* Given an exception value, we cannot recover its type,
       hence we cannot print its arguments in general.
       Here, we do a feeble attempt to print
       integer, string and float arguments... *)

    let print_exception_args obj start_offset =
      if O.size obj > start_offset then begin
        open_box 1;
        print_string "(";
        for i = start_offset to O.size obj - 1 do
          if i > start_offset then begin print_string ","; print_space() end;
          let arg = O.field obj i in
          if not (O.is_block arg) then
            print_int(O.obj arg : int)  (* Note: this could be a char! *)
          else if O.tag arg = Obj.string_tag then begin
            print_string "\"";
            print_string (String.escaped (O.obj arg : string));
            print_string "\""
          end else if O.tag arg = Obj.double_tag then
            print_float (O.obj arg : float)
          else
            print_string "_"
        done;
        print_string ")";
        close_box()
      end

    let print_exception bucket =
      let name = (O.obj(O.field(O.field bucket 0) 0) : string) in
      print_string name;
      if (name = "Match_failure" || name = "Assert_failure")
      && O.size bucket = 2
      && O.tag(O.field bucket 1) = 0
      then print_exception_args (O.field bucket 1) 0
      else print_exception_args bucket 1

    (* The user-defined printers. Also used for some builtin types. *)

    let printers = ref ([
      Pident(Ident.create "print_int"), Predef.type_int,
        (fun x -> print_int (O.obj x : int));
      Pident(Ident.create "print_float"), Predef.type_float,
        (fun x -> print_float(O.obj x : float));
      Pident(Ident.create "print_char"), Predef.type_char,
        (fun x -> print_string "'";
                  print_string (Char.escaped (O.obj x : char));
                  print_string "'");
      Pident(Ident.create "print_string"), Predef.type_string,
        (fun x -> print_string "\"";
                  print_string (String.escaped (O.obj x : string));
                  print_string "\"")
    ] : (Path.t * type_expr * (O.t -> unit)) list)

    let install_printer path ty fn =
      let print_val obj =
        try fn obj with
          exn ->
            print_string "<printer ";
            Printtyp.path path;
            print_string " raised an exception>" in
      printers := (path, ty, print_val) :: !printers

    let remove_printer path =
      let rec remove = function
        [] -> raise Not_found
      | (p, ty, fn as printer) :: rem ->
          if Path.same p path then rem else printer :: remove rem in
      printers := remove !printers

    let find_printer env ty =
      let rec find = function
        [] -> raise Not_found
      | (name, sch, printer) :: remainder ->
          if Ctype.moregeneral env false sch ty
          then printer
          else find remainder
      in find !printers

    (* Print a constructor or label, giving it the same prefix as the type
       it comes from. Attempt to omit the prefix if the type comes from
       a module that has been opened. *)

    let print_qualified lookup_fun env ty_path name =
      match ty_path with
        Pident id ->
          print_string name
      | Pdot(p, s, pos) ->
          if try
               match (lookup_fun (Lident name) env).desc with
                 Tconstr(ty_path', _, _) -> Path.same ty_path ty_path'
               | _ -> false
             with Not_found -> false
          then print_string name
          else (Printtyp.path p; print_string "."; print_string name)
      | Papply(p1, p2) ->
          Printtyp.path ty_path

    let print_constr =
      print_qualified
        (fun lid env -> (Env.lookup_constructor lid env).cstr_res)

    and print_label =
      print_qualified (fun lid env -> (Env.lookup_label lid env).lbl_res)

    (* An abstract type *)

    let abstract_type =
      Ctype.newty (Tconstr (Pident (Ident.create "abstract"), [], ref Mnil))

    (* The main printing function *)

    exception Ellipsis

    let cautious f arg = try f arg with Ellipsis -> print_string "..."

    let print_value max_steps max_depth check_depth env obj ty =

      let printer_steps = ref max_steps in

      let rec print_val prio depth obj ty =
        decr printer_steps;
        if !printer_steps < 0 or depth < 0 then raise Ellipsis;
        try
          find_printer env ty obj; ()
        with Not_found ->
          match (Ctype.repr ty).desc with
            Tvar ->
              print_string "<poly>"
          | Tarrow(ty1, ty2) ->
              print_string "<fun>"
          | Ttuple(ty_list) ->
              if check_depth depth obj ty then begin
                if prio > 0
                then begin open_box 1; print_string "(" end
                else open_box 0;
                print_val_list 1 depth obj ty_list;
                if prio > 0 then print_string ")";
                close_box()
              end
          | Tconstr(path, [], _) when Path.same path Predef.path_exn ->
              if check_depth depth obj ty then begin
                if prio > 1
                then begin open_box 2; print_string "(" end
                else open_box 1;
                print_exception obj;
                if prio > 1 then print_string ")";
                close_box()
              end
          | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_list ->
              if O.is_block obj then begin
                if check_depth depth obj ty then begin
                  let rec print_conses cons =
                    print_val 0 (depth - 1) (O.field cons 0) ty_arg;
                    let next_obj = O.field cons 1 in
                    if O.is_block next_obj then begin
                      print_string ";"; print_space();
                      print_conses next_obj
                    end
                  in
                  open_box 1;
                  print_string "[";
                  cautious print_conses obj;
                  print_string "]";
                  close_box()
                end
              end else
                print_string "[]"
          | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_array ->
              let length = O.size obj in
              if length = 0 then
                print_string "[||]"
              else if check_depth depth obj ty then begin
                let rec print_items i =
                  if i < length then begin
                    if i > 0 then begin print_string ";"; print_space() end;
                    print_val 0 (depth - 1) (O.field obj i) ty_arg;
                    print_items (i + 1)
                  end in
                open_box 2;
                print_string "[|";
                cautious print_items 0;
                print_string "|]";
                close_box()
              end
          | Tconstr(path, ty_list, _) ->
              begin try
                let decl = Env.find_type path env in
                match decl with
                  {type_kind = Type_abstract; type_manifest = None} ->
                    print_string "<abstr>"
                | {type_kind = Type_abstract; type_manifest = Some body} ->
                    print_val prio depth obj
                      (try Ctype.apply env decl.type_params body ty_list with
                         Ctype.Cannot_apply -> abstract_type)
                | {type_kind = Type_variant constr_list} ->
                    let tag =
                      if O.is_block obj
                      then Cstr_block(O.tag obj)
                      else Cstr_constant(O.obj obj) in
                    let (constr_name, constr_args) =
                      Datarepr.find_constr_by_tag tag constr_list in
                    let ty_args =
                      List.map
                        (function ty ->
                           try Ctype.apply env decl.type_params ty ty_list with
                             Ctype.Cannot_apply -> abstract_type)
                        constr_args in
                    begin match ty_args with
                      [] ->
                        print_constr env path constr_name
                    | [ty1] ->
                        if check_depth depth obj ty then begin
                          if prio > 1
                          then begin open_box 2; print_string "(" end
                          else open_box 1;
                          print_constr env path constr_name;
                          print_space();
                          cautious (print_val 2 (depth - 1)
                                      (O.field obj 0)) ty1;
                          if prio > 1 then print_string ")";
                          close_box()
                        end
                    | tyl ->
                        if check_depth depth obj ty then begin
                          if prio > 1
                          then begin open_box 2; print_string "(" end
                          else open_box 1;
                          print_constr env path constr_name;
                          print_space();
                          open_box 1;
                          print_string "(";
                          print_val_list 1 depth obj tyl;
                          print_string ")";
                          close_box();
                          if prio > 1 then print_string ")";
                          close_box()
                        end
                    end
                | {type_kind = Type_record lbl_list} ->
                    if check_depth depth obj ty then begin
                      let rec print_fields pos = function
                        [] -> ()
                      | (lbl_name, _, lbl_arg) :: remainder ->
                          if pos > 0 then begin
                            print_string ";"; print_space()
                          end;
                          open_box 1;
                          print_label env path lbl_name;
                          print_string "="; print_cut();
                          let ty_arg =
                            try
                              Ctype.apply env decl.type_params lbl_arg ty_list
                            with
                              Ctype.Cannot_apply -> abstract_type
                          in
                          cautious (print_val 0 (depth - 1)
                                      (O.field obj pos)) ty_arg;
                          close_box();
                          print_fields (pos + 1) remainder in
                      open_box 1;
                      print_string "{";
                      cautious (print_fields 0) lbl_list;
                      print_string "}";
                      close_box()
                    end
              with
                Not_found ->                (* raised by Env.find_type *)
                  print_string "<abstr>"
              | Datarepr.Constr_not_found -> (* raised by find_constr_by_tag *)
                  print_string "<unknown constructor>"
              end
          | Tobject (_, _) ->
              print_string "<obj>"
          | Tfield(_, _, _, _) | Tnil | Tlink _ ->
              fatal_error "Printval.print_value"

      and print_val_list prio depth obj ty_list =
        let rec print_list i = function
          [] -> ()
        | ty :: ty_list ->
            if i > 0 then begin print_string ","; print_space() end;
            print_val prio (depth - 1) (O.field obj i) ty;
            print_list (i + 1) ty_list in
      cautious (print_list 0) ty_list

    in cautious (print_val 0 max_depth obj) ty

end
