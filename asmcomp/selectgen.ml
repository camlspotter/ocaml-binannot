(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

open Misc
open Cmm
open Reg
open Mach

type environment = (Ident.t, Reg.t array) Tbl.t

(* Infer the type of the result of an operation *)

let oper_result_type = function
    Capply ty -> ty
  | Cextcall(s, ty, alloc) -> ty
  | Cload ty -> ty
  | Cloadchunk c -> typ_int
  | Calloc -> typ_addr
  | Cstore -> typ_void
  | Cstorechunk c -> typ_void
  | Caddi | Csubi | Cmuli | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccmpi _ | Ccmpa _ | Ccmpf _ -> typ_int
  | Cadda | Csuba -> typ_addr
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf -> typ_float
  | Cfloatofint -> typ_float
  | Cintoffloat -> typ_int
  | Craise -> typ_void
  | Ccheckbound -> typ_void
  | _ -> fatal_error "Selection.oper_result_type"

(* Infer the size in bytes of the result of a simple expression *)

let size_expr env exp =
  let rec size localenv = function
      Cconst_int _ | Cconst_natint _ -> Arch.size_int
    | Cconst_symbol _ | Cconst_pointer _ -> Arch.size_addr
    | Cconst_float _ -> Arch.size_float
    | Cvar id ->
        begin try
          Tbl.find id localenv
        with Not_found ->
        try
          let regs = Tbl.find id env in
          size_machtype (Array.map (fun r -> r.typ) regs)
        with Not_found ->
          fatal_error("Selection.size_expr: unbound var " ^ Ident.name id)
        end
    | Ctuple el ->
        List.fold_right (fun e sz -> size localenv e + sz) el 0
    | Cop(op, args) ->
        size_machtype(oper_result_type op)
    | Clet(id, arg, body) ->
        size (Tbl.add id (size localenv arg) localenv) body
    | _ ->
        fatal_error "Selection.size_expr"
  in size Tbl.empty exp

(* Says if an expression is "simple". A "simple" expression has no
   side-effects and its execution can be delayed until its value
   is really needed. In the case of e.g. an [alloc] instruction,
   the non-simple arguments are computed in right-to-left order
   first, then the block is allocated, then the simple arguments are
   evaluated and stored. *)

let rec is_simple_expr = function
    Cconst_int _ -> true
  | Cconst_natint _ -> true
  | Cconst_float _ -> true
  | Cconst_symbol _ -> true
  | Cconst_pointer _ -> true
  | Cvar _ -> true
  | Ctuple el -> List.for_all is_simple_expr el
  | Clet(id, arg, body) -> is_simple_expr arg && is_simple_expr body
  | Cop(op, args) ->
      begin match op with
        (* The following may have side effects *)
        Capply _ | Cextcall(_, _, _) | Calloc | Cstore | Cstorechunk _ | 
        Craise -> false
        (* The remaining operations are simple if their args are *)
      | _ -> List.for_all is_simple_expr args
      end
  | _ -> false

(* Swap the two arguments of an integer comparison *)

let swap_intcomp = function
    Isigned cmp -> Isigned(swap_comparison cmp)
  | Iunsigned cmp -> Iunsigned(swap_comparison cmp)

(* Naming of registers *)

let all_regs_anonymous rv =
  try
    for i = 0 to Array.length rv - 1 do
      if String.length rv.(i).name > 0 then raise Exit
    done;
    true
  with Exit ->
    false

let name_regs id rv =
  if Array.length rv = 1 then
    rv.(0).name <- Ident.name id
  else
    for i = 0 to Array.length rv - 1 do
      rv.(i).name <- Ident.name id ^ "#" ^ string_of_int i
    done

(* "Join" two instruction sequences, making sure they return their results
   in the same registers. *)

let join r1 seq1 r2 seq2 =
  let l1 = Array.length r1 and l2 = Array.length r2 in
  if l1 = 0 then r2
  else if l2 = 0 then r1
  else begin
    let r = Array.create l1 Reg.dummy in
    for i = 0 to l1-1 do
      if String.length r1.(i).name = 0 then begin
        r.(i) <- r1.(i);
        seq2#insert_move r2.(i) r1.(i)
      end else if String.length r2.(i).name = 0 then begin
        r.(i) <- r2.(i);
        seq1#insert_move r1.(i) r2.(i)
      end else begin
        r.(i) <- Reg.create r1.(i).typ;
        seq1#insert_move r1.(i) r.(i);
        seq2#insert_move r2.(i) r.(i)
      end
    done;
    r
  end

(* Same, for N branches *)

let join_array rs =
  let some_res = ref [||] in
  for i = 0 to Array.length rs - 1 do
    let (r, s) = rs.(i) in
    if Array.length r > 0 then some_res := r
  done;
  let size_res = Array.length !some_res in
  if size_res = 0 then [||] else begin
    let res = Array.create size_res Reg.dummy in
    for i = 0 to size_res - 1 do
      res.(i) <- Reg.create (!some_res).(i).typ
    done;
    for i = 0 to Array.length rs - 1 do
      let (r, s) = rs.(i) in
      if Array.length r > 0 then s#insert_moves r res
    done;
    res
  end

(* The default instruction selection class *)

class virtual selector_generic () as self =

(* Says whether an integer constant is a suitable immediate argument *)

virtual is_immediate : int -> bool

(* Selection of addressing modes *)

virtual select_addressing :
  Cmm.expression -> Arch.addressing_mode * Cmm.expression

(* Default instruction selection for stores *)

method select_store addr arg =
  (Istore(Word, addr), arg)

(* Default instruction selection for operators *)

method select_operation op args =
  match (op, args) with
    (Capply ty, Cconst_symbol s :: rem) -> (Icall_imm s, rem)
  | (Capply ty, _) -> (Icall_ind, args)
  | (Cextcall(s, ty, alloc), _) -> (Iextcall(s, alloc), args)
  | (Cload ty, [arg]) ->
      let (addr, eloc) = self#select_addressing arg in
      (Iload(Word, addr), [eloc])
  | (Cloadchunk chunk, [arg]) ->
      let (addr, eloc) = self#select_addressing arg in
      (Iload(chunk, addr), [eloc])
  | (Cstore, [arg1; arg2]) ->
      let (addr, eloc) = self#select_addressing arg1 in
      let (op, newarg2) = self#select_store addr arg2 in
      (op, [newarg2; eloc])
      (* Inversion addr/datum in Istore *)
  | (Cstorechunk chunk, [arg1; arg2]) ->
      let (addr, eloc) = self#select_addressing arg1 in
      (Istore(chunk, addr), [arg2; eloc])
      (* Inversion addr/datum in Istore *)
  | (Calloc, _) -> (Ialloc 0, args)
  | (Caddi, _) -> self#select_arith_comm Iadd args
  | (Csubi, _) -> self#select_arith Isub args
  | (Cmuli, [arg1; Cconst_int n]) ->
      let l = Misc.log2 n in
      if n = 1 lsl l
      then (Iintop_imm(Ilsl, l), [arg1])
      else self#select_arith_comm Imul args
  | (Cmuli, [Cconst_int n; arg1]) ->
      let l = Misc.log2 n in
      if n = 1 lsl l
      then (Iintop_imm(Ilsl, l), [arg1])
      else self#select_arith_comm Imul args
  | (Cmuli, _) -> self#select_arith_comm Imul args
  | (Cdivi, _) -> self#select_arith Idiv args
  | (Cmodi, _) -> self#select_arith_comm Imod args
  | (Cand, _) -> self#select_arith_comm Iand args
  | (Cor, _) -> self#select_arith_comm Ior args
  | (Cxor, _) -> self#select_arith_comm Ixor args
  | (Clsl, _) -> self#select_shift Ilsl args
  | (Clsr, _) -> self#select_shift Ilsr args
  | (Casr, _) -> self#select_shift Iasr args
  | (Ccmpi comp, _) -> self#select_arith_comp (Isigned comp) args
  | (Cadda, _) -> self#select_arith_comm Iadd args
  | (Csuba, _) -> self#select_arith Isub args
  | (Ccmpa comp, _) -> self#select_arith_comp (Iunsigned comp) args
  | (Cnegf, _) -> (Inegf, args)
  | (Cabsf, _) -> (Iabsf, args)
  | (Caddf, _) -> (Iaddf, args)
  | (Csubf, _) -> (Isubf, args)  
  | (Cmulf, _) -> (Imulf, args)  
  | (Cdivf, _) -> (Idivf, args)
  | (Cfloatofint, _) -> (Ifloatofint, args)
  | (Cintoffloat, _) -> (Iintoffloat, args)
  | (Ccheckbound, _) -> self#select_arith Icheckbound args
  | _ -> fatal_error "Selection.select_oper"

method select_arith_comm op = function
    [arg; Cconst_int n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [arg; Cconst_pointer n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int n; arg] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_pointer n; arg] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method select_arith op = function
    [arg; Cconst_int n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [arg; Cconst_pointer n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method select_shift op = function
    [arg; Cconst_int n] when n >= 0 & n < Arch.size_int * 8 ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method select_arith_comp cmp = function
    [arg; Cconst_int n] when self#is_immediate n ->
      (Iintop_imm(Icomp cmp, n), [arg])
  | [arg; Cconst_pointer n] when self#is_immediate n ->
      (Iintop_imm(Icomp cmp, n), [arg])
  | [Cconst_int n; arg] when self#is_immediate n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), [arg])
  | [Cconst_pointer n; arg] when self#is_immediate n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), [arg])
  | args ->
      (Iintop(Icomp cmp), args)

(* Instruction selection for conditionals *)

method select_condition = function
    Cop(Ccmpi cmp, [arg1; Cconst_int n]) when self#is_immediate n ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst_int n; arg2]) when self#is_immediate n ->
      (Iinttest_imm(Isigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, args) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop(Ccmpa cmp, [arg1; Cconst_pointer n]) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [Cconst_pointer n; arg2]) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, args) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | Cop(Ccmpf cmp, args) ->
      (Ifloattest(cmp, false), Ctuple args)
  | Cop(Cand, [arg; Cconst_int 1]) ->
      (Ioddtest, arg)
  | arg ->
      (Itruetest, arg)

(* Buffering of instruction sequences *)

val private mutable instr_seq = dummy_instr

method insert desc arg res =
  instr_seq <- instr_cons desc arg res instr_seq

method extract =
  let rec extract res i =
    if i == dummy_instr
    then res
    else extract (instr_cons i.desc i.arg i.res res) i.next in
  extract (end_instr()) instr_seq

(* Insert a sequence of moves from one pseudoreg set to another. *)

method insert_move src dst =
  if src.stamp <> dst.stamp then
    self#insert (Iop Imove) [|src|] [|dst|]

method insert_moves src dst =
  for i = 0 to Array.length src - 1 do
    self#insert_move src.(i) dst.(i)
  done

(* Insert moves and stack offsets for function arguments and results *)

method insert_move_args arg loc stacksize =
  if stacksize <> 0 then self#insert (Iop(Istackoffset stacksize)) [||] [||];
  self#insert_moves arg loc

method insert_move_results loc res stacksize =
  if stacksize <> 0 then self#insert(Iop(Istackoffset(-stacksize))) [||] [||];
  self#insert_moves loc res

(* Add an Iop opcode. Can be overriden by processor description
   to insert moves before and after the operation, i.e. for two-address 
   instructions, or instructions using dedicated registers. *)

method insert_op op rs rd =
  self#insert (Iop op) rs rd;
  rd

(* Add the instructions for the given expression
   at the end of the self sequence *)

method emit_expr env exp =
  match exp with
    Cconst_int n ->
      let r = Reg.createv typ_int in
      self#insert_op (Iconst_int(Nativeint.from n)) [||] r
  | Cconst_natint n ->
      let r = Reg.createv typ_int in
      self#insert_op (Iconst_int n) [||] r
  | Cconst_float n ->
      let r = Reg.createv typ_float in
      self#insert_op (Iconst_float n) [||] r
  | Cconst_symbol n ->
      let r = Reg.createv typ_addr in
      self#insert_op (Iconst_symbol n) [||] r
  | Cconst_pointer n ->
      let r = Reg.createv typ_addr in
      self#insert_op (Iconst_int(Nativeint.from n)) [||] r
  | Cvar v ->
      begin try
        Tbl.find v env
      with Not_found ->
        fatal_error("Selection.emit_expr: unbound var " ^ Ident.name v)
      end
  | Clet(v, e1, e2) ->
      self#emit_expr (self#emit_let env v e1) e2
  | Cassign(v, e1) ->
      let rv =
        try
          Tbl.find v env
        with Not_found ->
          fatal_error ("Selection.emit_expr: unbound var " ^ Ident.name v) in
      let r1 = self#emit_expr env e1 in
      self#insert_moves r1 rv;
      [||]
  | Ctuple [] ->
      [||]
  | Ctuple exp_list ->
      let (simple_list, ext_env) = self#emit_parts_list env exp_list in
      self#emit_tuple ext_env simple_list
  | Cop(Cproj(ofs, len), [Cop(Cload ty, [arg])]) ->
      let byte_offset = size_machtype(Array.sub ty 0 ofs) in
      self#emit_expr env
        (Cop(Cload(Array.sub ty ofs len),
             [Cop(Cadda, [arg; Cconst_int byte_offset])]))
  | Cop(Cproj(ofs, len), [arg]) ->
      let r = self#emit_expr env arg in
      Array.sub r ofs len
  | Cop(Craise, [arg]) ->
      let r1 = self#emit_expr env arg in
      let rd = [|Proc.loc_exn_bucket|] in
      self#insert (Iop Imove) r1 rd;
      self#insert Iraise rd [||];
      [||]
  | Cop(Ccmpf comp, args) ->
      self#emit_expr env (Cifthenelse(exp, Cconst_int 1, Cconst_int 0))
  | Cop(op, args) ->
      let (simple_args, env) = self#emit_parts_list env args in
      let ty = oper_result_type op in
      let (new_op, new_args) = self#select_operation op simple_args in
      begin match new_op with
        Icall_ind ->
          Proc.contains_calls := true;
          let r1 = self#emit_tuple env new_args in
          let rarg = Array.sub r1 1 (Array.length r1 - 1) in
          let rd = Reg.createv ty in
          let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
          let loc_res = Proc.loc_results rd in
          self#insert_move_args rarg loc_arg stack_ofs;
          self#insert (Iop Icall_ind)
                      (Array.append [|r1.(0)|] loc_arg) loc_res;
          self#insert_move_results loc_res rd stack_ofs;
          rd
      | Icall_imm lbl ->
          Proc.contains_calls := true;
          let r1 = self#emit_tuple env new_args in
          let rd = Reg.createv ty in
          let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
          let loc_res = Proc.loc_results rd in
          self#insert_move_args r1 loc_arg stack_ofs;
          self#insert (Iop(Icall_imm lbl)) loc_arg loc_res;
          self#insert_move_results loc_res rd stack_ofs;
          rd
      | Iextcall(lbl, alloc) ->
          Proc.contains_calls := true;
          let (loc_arg, stack_ofs) = self#emit_extcall_args env new_args in
          let rd = Reg.createv ty in
          let loc_res = Proc.loc_external_results rd in
          self#insert (Iop(Iextcall(lbl, alloc))) loc_arg loc_res;
          self#insert_move_results loc_res rd stack_ofs;
          rd
      | Ialloc _ ->
          Proc.contains_calls := true;
          let rd = Reg.createv typ_addr in
          let size = size_expr env (Ctuple new_args) in
          self#insert (Iop(Ialloc size)) [||] rd;
          self#emit_stores env new_args rd 
            (Arch.offset_addressing Arch.identity_addressing (-Arch.size_int));
          rd
      | op ->
          let r1 = self#emit_tuple env new_args in
          let rd = Reg.createv ty in
          self#insert_op op r1 rd
      end        
  | Csequence(e1, e2) ->
      self#emit_expr env e1;
      self#emit_expr env e2
  | Cifthenelse(econd, eif, eelse) ->
      let (cond, earg) = self#select_condition econd in
      let rarg = self#emit_expr env earg in
      let (rif, sif) = self#emit_sequence env eif in
      let (relse, selse) = self#emit_sequence env eelse in
      let r = join rif sif relse selse in
      self#insert (Iifthenelse(cond, sif#extract, selse#extract)) rarg [||];
      r
  | Cswitch(esel, index, ecases) ->
      let rsel = self#emit_expr env esel in
      let rscases = Array.map (self#emit_sequence env) ecases in
      let r = join_array rscases in
      self#insert (Iswitch(index, Array.map (fun (r, s) -> s#extract) rscases))
                  rsel [||];
      r
  | Cloop(ebody) ->
      let (rarg, sbody) = self#emit_sequence env ebody in
      self#insert (Iloop(sbody#extract)) [||] [||];
      [||]
  | Ccatch(e1, e2) ->
      let (r1, s1) = self#emit_sequence env e1 in
      let (r2, s2) = self#emit_sequence env e2 in
      let r = join r1 s1 r2 s2 in
      self#insert (Icatch(s1#extract, s2#extract)) [||] [||];
      r
  | Cexit ->
      self#insert Iexit [||] [||];
      [||]
  | Ctrywith(e1, v, e2) ->
      Proc.contains_calls := true;
      let (r1, s1) = self#emit_sequence env e1 in
      let rv = Reg.createv typ_addr in
      let (r2, s2) = self#emit_sequence (Tbl.add v rv env) e2 in
      let r = join r1 s1 r2 s2 in
      self#insert
        (Itrywith(s1#extract,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv
                             s2#extract))
        [||] [||];
      r

method emit_sequence env exp =
  let s = {< instr_seq = dummy_instr >} in
  let r = s#emit_expr env exp in
  (r, s)

method emit_let env v e1 =
  let r1 = self#emit_expr env e1 in
  if all_regs_anonymous r1 then begin
    name_regs v r1;
    Tbl.add v r1 env
  end else begin
    let rv = Array.create (Array.length r1) Reg.dummy in
    for i = 0 to Array.length r1 - 1 do rv.(i) <- Reg.create r1.(i).typ done;
    name_regs v rv;
    self#insert_moves r1 rv;
    Tbl.add v rv env
  end

method emit_parts env exp =
  if is_simple_expr exp then
    (exp, env)
  else begin
    let r = self#emit_expr env exp in
    if Array.length r = 0 then
      (Ctuple [], env)
    else begin
      let id = Ident.create "bind" in
      if all_regs_anonymous r then
        (Cvar id, Tbl.add id r env)
      else begin
        let rv = Array.create (Array.length r) Reg.dummy in
        for i = 0 to Array.length r - 1 do
          rv.(i) <- Reg.create r.(i).typ
        done;
        self#insert_moves r rv;
        (Cvar id, Tbl.add id rv env)
      end          
    end
  end

method emit_parts_list env exp_list =
  match exp_list with
    [] -> ([], env)
  | exp :: rem ->
      (* This ensures right-to-left evaluation, consistent with the
         bytecode compiler *)
      let (new_rem, new_env) = self#emit_parts_list env rem in
      let (new_exp, fin_env) = self#emit_parts new_env exp in
      (new_exp :: new_rem, fin_env)

method emit_tuple env exp_list =
  let rec emit_list = function
    [] -> []
  | exp :: rem ->
      (* Again, force right-to-left evaluation *)
      let loc_rem = emit_list rem in
      let loc_exp = self#emit_expr env exp in
      loc_exp :: loc_rem in
  Array.concat(emit_list exp_list)

method emit_extcall_args env args =
  let r1 = self#emit_tuple env args in
  let (loc_arg, stack_ofs as arg_stack) = Proc.loc_external_arguments r1 in
  self#insert_move_args r1 loc_arg stack_ofs;
  arg_stack

method emit_stores env data regs_addr addr =
  let a = ref addr in
  List.iter
    (fun e ->
      let (op, arg) = self#select_store !a e in
      let r = self#emit_expr env arg in
      self#insert (Iop op) (Array.append r regs_addr) [||];
      a := Arch.offset_addressing !a (size_expr env e))
    data

(* Same, but in tail position *)

method emit_return env exp =
  let r = self#emit_expr env exp in
  let loc = Proc.loc_results r in
  self#insert_moves r loc;
  self#insert Ireturn loc [||]

method emit_tail env exp =
  match exp with
    Clet(v, e1, e2) ->
      self#emit_tail (self#emit_let env v e1) e2
  | Cop(Capply ty as op, args) ->
      let (simple_args, env) = self#emit_parts_list env args in
      let (new_op, new_args) = self#select_operation op simple_args in
      begin match new_op with
        Icall_ind ->
          let r1 = self#emit_tuple env new_args in
          let rarg = Array.sub r1 1 (Array.length r1 - 1) in
          let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
          if stack_ofs = 0 then begin
            self#insert_moves rarg loc_arg;
            self#insert (Iop Itailcall_ind)
                        (Array.append [|r1.(0)|] loc_arg) [||]
          end else begin
            Proc.contains_calls := true;
            let rd = Reg.createv ty in
            let loc_res = Proc.loc_results rd in
            self#insert_move_args rarg loc_arg stack_ofs;
            self#insert (Iop Icall_ind)
                        (Array.append [|r1.(0)|] loc_arg) loc_res;
            self#insert(Iop(Istackoffset(-stack_ofs))) [||] [||];
            self#insert Ireturn loc_res [||]
          end
      | Icall_imm lbl ->
          let r1 = self#emit_tuple env new_args in
          let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
          if stack_ofs = 0 then begin
            self#insert_moves r1 loc_arg;
            self#insert (Iop(Itailcall_imm lbl)) loc_arg [||]
          end else begin
            Proc.contains_calls := true;
            let rd = Reg.createv ty in
            let loc_res = Proc.loc_results rd in
            self#insert_move_args r1 loc_arg stack_ofs;
            self#insert (Iop(Icall_imm lbl)) loc_arg loc_res;
            self#insert(Iop(Istackoffset(-stack_ofs))) [||] [||];
            self#insert Ireturn loc_res [||]
          end
      | _ -> fatal_error "Selection.emit_tail"
      end
  | Cop(Craise, [e1]) ->
      let r1 = self#emit_expr env e1 in
      let rd = [|Proc.loc_exn_bucket|] in
      self#insert (Iop Imove) r1 rd;
      self#insert Iraise rd [||]
  | Csequence(e1, e2) ->
      self#emit_expr env e1;
      self#emit_tail env e2
  | Cifthenelse(econd, eif, eelse) ->
      let (cond, earg) = self#select_condition econd in
      let rarg = self#emit_expr env earg in
      self#insert (Iifthenelse(cond, self#emit_tail_sequence env eif,
                                     self#emit_tail_sequence env eelse))
                  rarg [||]
  | Cswitch(esel, index, ecases) ->
      let rsel = self#emit_expr env esel in
      self#insert
        (Iswitch(index, Array.map (self#emit_tail_sequence env) ecases))
        rsel [||]
  | Ccatch(e1, e2) ->
      self#insert (Icatch(self#emit_tail_sequence env e1,
                          self#emit_tail_sequence env e2))
                  [||] [||]
  | Cexit ->
      self#insert Iexit [||] [||]
  | Ctrywith(e1, v, e2) ->
      Proc.contains_calls := true;
      let (r1, s1) = self#emit_sequence env e1 in
      let rv = Reg.createv typ_addr in
      let s2 = self#emit_tail_sequence (Tbl.add v rv env) e2 in
      let loc = Proc.loc_results r1 in
      self#insert
        (Itrywith(s1#extract,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv s2))
        [||] [||];
      self#insert_moves r1 loc;
      self#insert Ireturn loc [||]
  | _ ->
      self#emit_return env exp

method emit_tail_sequence env exp =
  let s = {< instr_seq = dummy_instr >} in
  s#emit_tail env exp;
  s#extract

(* Sequentialization of a function definition *)

method emit_fundecl f =
  Proc.contains_calls := false;
  let rargs =
    List.map
      (fun (id, ty) -> let r = Reg.createv ty in name_regs id r; r)
      f.Cmm.fun_args in
  let rarg = Array.concat rargs in
  let loc_arg = Proc.loc_parameters rarg in
  let env =
    List.fold_right2
      (fun (id, ty) r env -> Tbl.add id r env)
      f.Cmm.fun_args rargs Tbl.empty in
  self#insert_moves loc_arg rarg;
  self#emit_tail env f.Cmm.fun_body;
  { fun_name = f.Cmm.fun_name;
    fun_args = loc_arg;
    fun_body = self#extract;
    fun_fast = f.Cmm.fun_fast }

end
