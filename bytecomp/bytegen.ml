(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(*  bytegen.ml : translation of lambda terms to lists of instructions. *)

open Misc
open Asttypes
open Primitive
open Typedtree
open Lambda
open Instruct

(**** Label generation ****)

let label_counter = ref 0

let new_label () =
  incr label_counter; !label_counter

(**** Structure of the compilation environment. ****)

type compilation_env =
  { ce_stack: int Ident.tbl; (* Positions of variables in the stack *)
    ce_heap: int Ident.tbl } (* Structure of the heap-allocated env *)

(* The ce_stack component gives locations of variables residing 
   in the stack. The locations are offsets w.r.t. the origin of the
   stack frame.
   The ce_heap component gives the positions of variables residing in the
   heap-allocated environment. *)

let empty_env =
  { ce_stack = Ident.empty; ce_heap = Ident.empty }

(* Add a stack-allocated variable *)

let add_var id pos env =
  { ce_stack = Ident.add id pos env.ce_stack;
    ce_heap = env.ce_heap }

(**** Examination of the continuation ****)

(* Return a label to the beginning of the given continuation.
   If the sequence starts with a branch, use the target of that branch
   as the label, thus avoiding a jump to a jump. *)

let label_code = function
    Kbranch lbl :: _ as cont -> (lbl, cont)
  | Klabel lbl :: _ as cont -> (lbl, cont)
  | cont -> let lbl = new_label() in (lbl, Klabel lbl :: cont)

(* Return a branch to the continuation. That is, an instruction that,
   when executed, branches to the continuation or performs what the
   continuation performs. We avoid generating branches to branches and
   branches to returns. *)

let make_branch cont =
  match cont with
    (Kbranch _ as branch) :: _ -> (branch, cont)
  | (Kreturn _ as return) :: _ -> (return, cont)
  | Kraise :: _ -> (Kraise, cont)
  | Klabel lbl :: _ -> (Kbranch lbl, cont)
  | _ -> let lbl = new_label() in (Kbranch lbl, Klabel lbl :: cont)

(* Discard all instructions up to the next label.
   This function is to be applied to the continuation before adding a
   non-terminating instruction (branch, raise, return) in front of it. *)

let rec discard_dead_code = function
    [] -> []
  | (Klabel _ | Krestart | Ksetglobal _) :: _ as cont -> cont
  | _ :: cont -> discard_dead_code cont

(* Check if we're in tailcall position *)

let rec is_tailcall = function
    Kreturn _ :: _ -> true
  | Klabel _ :: c -> is_tailcall c
  | _ -> false

(* Add a Kpop N instruction in front of a continuation *)

let rec add_pop n cont =
  if n = 0 then cont else
    match cont with
      Kpop m :: cont -> add_pop (n + m) cont
    | Kreturn m :: cont -> Kreturn(n + m) :: cont
    | Kraise :: _ -> cont
    | _ -> Kpop n :: cont

(* Add the constant "unit" in front of a continuation *)

let add_const_unit = function
    (Kacc _ | Kconst _ | Kgetglobal _ | Kpush_retaddr _) :: _ as cont -> cont
  | cont -> Kconst const_unit :: cont

(**** Auxiliary for compiling "let rec" ****)

let rec size_of_lambda = function
    Lfunction(param, body) as funct ->
      1 + IdentSet.cardinal(free_variables funct)
  | Lprim(Pmakeblock(tag, mut), args) ->
      List.length args
  | Llet(str, id, arg, body) ->
      size_of_lambda body
  | _ ->
      fatal_error "Codegen.size_of_lambda"

(**** Compilation of a lambda expression ****)

(* The label to which Lstaticfail branches, and the stack size at that point.*)

let lbl_staticfail = ref 0
and sz_staticfail = ref 0

(* Function bodies that remain to be compiled *)

let functions_to_compile  =
  (Stack.new () : (Ident.t list * lambda * label * Ident.t list) Stack.t)

(* Compile an expression.
   The value of the expression is left in the accumulator.
   env = compilation environment
   exp = the lambda expression to compile
   sz = current size of the stack frame
   cont = list of instructions to execute afterwards
   Result = list of instructions that evaluate exp, then perform cont. *)

let rec comp_expr env exp sz cont =
  match exp with
    Lvar id ->
      begin try
        let pos = Ident.find_same id env.ce_stack in
        Kacc(sz - pos) :: cont
      with Not_found ->
      try
        let pos = Ident.find_same id env.ce_heap in
        Kenvacc(pos) :: cont
      with Not_found ->
        Ident.print id; print_newline();
        fatal_error "Codegen.comp_expr: var"
      end
  | Lconst cst ->
      Kconst cst :: cont
  | Lapply(func, args) ->
      let nargs = List.length args in
      if is_tailcall cont then
        comp_args env args sz
          (Kpush :: comp_expr env func (sz + nargs)
            (Kappterm(nargs, sz + nargs) :: discard_dead_code cont))
      else
        if nargs < 4 then
          comp_args env args sz
            (Kpush :: comp_expr env func (sz + nargs) (Kapply nargs :: cont))
        else begin
          let (lbl, cont1) = label_code cont in
          Kpush_retaddr lbl ::
          comp_args env args (sz + 3)
            (Kpush :: comp_expr env func (sz + 3 + nargs)
                      (Kapply nargs :: cont1))
        end
  | Lfunction(params, body) ->
      let lbl = new_label() in
      let fv = IdentSet.elements(free_variables exp) in
      Stack.push (params, body, lbl, fv) functions_to_compile;
      comp_args env (List.map (fun n -> Lvar n) fv) sz
        (Kclosure(lbl, List.length fv) :: cont)
  | Llet(str, id, arg, body) ->
      comp_expr env arg sz
        (Kpush :: comp_expr (add_var id (sz+1) env) body (sz+1)
          (add_pop 1 cont))
  | Lletrec(([id, Lfunction(params, funct_body)] as decl), let_body) ->
      let lbl = new_label() in
      let fv =
        IdentSet.elements (free_variables (Lletrec(decl, lambda_unit))) in
      Stack.push (params, funct_body, lbl, id :: fv) functions_to_compile;
      comp_args env (List.map (fun n -> Lvar n) fv) sz
        (Kclosurerec(lbl, List.length fv) :: Kpush ::
          (comp_expr (add_var id (sz+1) env) let_body (sz+1)
                     (add_pop 1 cont)))
  | Lletrec(decl, body) ->
      let ndecl = List.length decl in
      let decl_size =
        List.map (fun (id, exp) -> (id, exp, size_of_lambda exp)) decl in
      let rec comp_decl new_env sz i = function
          [] ->
            comp_expr new_env body sz (add_pop ndecl cont)
        | (id, exp, blocksize) :: rem ->
            comp_expr new_env exp sz
              (Kpush :: Kacc i :: Kupdate blocksize ::
               comp_decl new_env sz (i-1) rem) in
      let rec comp_init new_env sz = function
          [] ->
            comp_decl new_env sz ndecl decl_size
        | (id, exp, blocksize) :: rem ->
            Kdummy blocksize :: Kpush ::
            comp_init (add_var id (sz+1) new_env) (sz+1) rem in
      comp_init env sz decl_size
  | Lprim(Pidentity, [arg]) ->
      comp_expr env arg sz cont
  | Lprim(Pnot, [arg]) ->
      let newcont =
        match cont with
          Kbranchif lbl :: cont1 -> Kbranchifnot lbl :: cont1
        | Kbranchifnot lbl :: cont1 -> Kbranchif lbl :: cont1
        | _ -> Kboolnot :: cont in
      comp_expr env arg sz newcont
  | Lprim(Psequand, [exp1; exp2]) ->
      begin match cont with
        Kbranchifnot lbl :: _ ->
          comp_expr env exp1 sz (Kbranchifnot lbl ::
            comp_expr env exp2 sz cont)
      | Kbranchif lbl :: cont1 ->
          let (lbl2, cont2) = label_code cont1 in
          comp_expr env exp1 sz (Kbranchifnot lbl2 ::
            comp_expr env exp2 sz (Kbranchif lbl :: cont2))
      | _ ->
          let (lbl, cont1) = label_code cont in
          comp_expr env exp1 sz (Kstrictbranchifnot lbl ::
            comp_expr env exp2 sz cont1)
      end
  | Lprim(Psequor, [exp1; exp2]) ->
      begin match cont with
        Kbranchif lbl :: _ ->
          comp_expr env exp1 sz (Kbranchif lbl ::
            comp_expr env exp2 sz cont)
      | Kbranchifnot lbl :: cont1 ->
          let (lbl2, cont2) = label_code cont1 in
          comp_expr env exp1 sz (Kbranchif lbl2 ::
            comp_expr env exp2 sz (Kbranchifnot lbl :: cont2))
      | _ ->
          let (lbl, cont1) = label_code cont in
          comp_expr env exp1 sz (Kstrictbranchif lbl ::
            comp_expr env exp2 sz cont1)
      end
  | Lprim(Praise, [arg]) ->
      comp_expr env arg sz (Kraise :: discard_dead_code cont)
  | Lprim((Paddint | Psubint as prim), [arg; Lconst(Const_base(Const_int n))])
    when n >= immed_min & n <= immed_max ->
      let ofs = if prim == Paddint then n else -n in
      comp_expr env arg sz (Koffsetint ofs :: cont)
  | Lprim(p, args) ->
      let instr =
        match p with
          Pgetglobal id -> Kgetglobal id
        | Psetglobal id -> Ksetglobal id
        | Pintcomp cmp -> Kintcomp cmp
        | Pmakeblock(tag, mut) -> Kmakeblock(List.length args, tag)
        | Pfield n -> Kgetfield n
        | Psetfield(n, ptr) -> Ksetfield n
        | Pfloatfield n -> Kgetfield n
        | Psetfloatfield n -> Ksetfield n
        | Pccall p -> Kccall(p.prim_name, p.prim_arity)
        | Pnegint -> Knegint
        | Paddint -> Kaddint
        | Psubint -> Ksubint
        | Pmulint -> Kmulint
        | Pdivint -> Kdivint
        | Pmodint -> Kmodint
        | Pandint -> Kandint
        | Porint -> Korint
        | Pxorint -> Kxorint
        | Plslint -> Klslint
        | Plsrint -> Klsrint
        | Pasrint -> Kasrint
        | Poffsetint n -> Koffsetint n
        | Poffsetref n -> Koffsetref n
        | Pintoffloat -> Kccall("int_of_float", 1)
        | Pfloatofint -> Kccall("float_of_int", 1)
        | Pnegfloat -> Kccall("neg_float", 1)
        | Pabsfloat -> Kccall("abs_float", 1)
        | Paddfloat -> Kccall("add_float", 2)
        | Psubfloat -> Kccall("sub_float", 2)
        | Pmulfloat -> Kccall("mul_float", 2)
        | Pdivfloat -> Kccall("div_float", 2)
        | Pfloatcomp Ceq -> Kccall("eq_float", 2)
        | Pfloatcomp Cneq -> Kccall("neq_float", 2)
        | Pfloatcomp Clt -> Kccall("lt_float", 2)
        | Pfloatcomp Cgt -> Kccall("gt_float", 2)
        | Pfloatcomp Cle -> Kccall("le_float", 2)
        | Pfloatcomp Cge -> Kccall("ge_float", 2)
        | Pstringlength -> Kccall("ml_string_length", 1)
        | Pstringrefs -> Kccall("string_get", 2)
        | Pstringsets -> Kccall("string_set", 3)
        | Pstringrefu -> Kgetstringchar
        | Pstringsetu -> Ksetstringchar
        | Pmakearray kind -> Kmakeblock(List.length args, 0)
        | Parraylength kind -> Kvectlength
        | Parrayrefs kind -> Kccall("array_get", 2)
        | Parraysets kind -> Kccall("array_set", 3)
        | Parrayrefu kind -> Kgetvectitem
        | Parraysetu kind -> Ksetvectitem
        | Pbittest -> Kccall("bitvect_test", 2)
        | _ -> fatal_error "Codegen.comp_expr: prim" in
      comp_args env args sz (instr :: cont)
  | Lcatch(body, Lstaticfail) ->
      comp_expr env body sz cont
  | Lcatch(body, handler) ->
      let (branch1, cont1) = make_branch cont in
      let (lbl_handler, cont2) = label_code (comp_expr env handler sz cont1) in
      let saved_lbl_staticfail = !lbl_staticfail
      and saved_sz_staticfail = !sz_staticfail in
      lbl_staticfail := lbl_handler;
      sz_staticfail := sz;
      let cont3 = comp_expr env body sz (branch1 :: cont2) in
      lbl_staticfail := saved_lbl_staticfail;
      sz_staticfail := saved_sz_staticfail;
      cont3
  | Lstaticfail ->
      add_pop (sz - !sz_staticfail)
              (Kbranch !lbl_staticfail :: discard_dead_code cont)
  | Ltrywith(body, id, handler) ->
      let (branch1, cont1) = make_branch cont in
      let lbl_handler = new_label() in
      Kpushtrap lbl_handler :: 
        comp_expr env body (sz+4) (Kpoptrap :: branch1 :: 
          Klabel lbl_handler :: Kpush ::
            comp_expr (add_var id (sz+1) env) handler (sz+1) (add_pop 1 cont1))
  | Lifthenelse(cond, ifso, ifnot) ->
      comp_binary_test env cond ifso ifnot sz cont
  | Lsequence(exp1, exp2) ->
      comp_expr env exp1 sz (comp_expr env exp2 sz cont)
  | Lwhile(cond, body) ->
      let lbl_loop = new_label() in
      let lbl_test = new_label() in
      Kbranch lbl_test :: Klabel lbl_loop :: Kcheck_signals ::
        comp_expr env body sz
          (Klabel lbl_test ::
            comp_expr env cond sz (Kbranchif lbl_loop :: add_const_unit cont))
  | Lfor(param, start, stop, dir, body) ->
      let lbl_loop = new_label() in
      let lbl_test = new_label() in
      let offset = match dir with Upto -> 1 | Downto -> -1 in
      let comp = match dir with Upto -> Cle | Downto -> Cge in
      comp_expr env start sz
        (Kpush :: comp_expr env stop (sz+1)
          (Kpush :: Kbranch lbl_test ::
           Klabel lbl_loop :: Kcheck_signals ::
           comp_expr (add_var param (sz+1) env) body (sz+2)
             (Kacc 1 :: Koffsetint offset :: Kassign 1 ::
              Klabel lbl_test ::
              Kacc 0 :: Kpush :: Kacc 2 :: Kintcomp comp ::
              Kbranchif lbl_loop ::
              add_const_unit (add_pop 2 cont))))
  | Lswitch(arg, sw) ->
      let (branch, cont1) = make_branch cont in
      let c = ref (discard_dead_code cont1) in
      let act_consts = Array.new sw.sw_numconsts Lstaticfail in
      List.iter (fun (n, act) -> act_consts.(n) <- act) sw.sw_consts;
      let act_blocks = Array.new sw.sw_numblocks Lstaticfail in
      List.iter (fun (n, act) -> act_blocks.(n) <- act) sw.sw_blocks;
      let lbl_consts = Array.new sw.sw_numconsts 0 in
      let lbl_blocks = Array.new sw.sw_numblocks 0 in
      for i = sw.sw_numblocks - 1 downto 0 do
        let (lbl, c1) =
          label_code(comp_expr env act_blocks.(i) sz (branch :: !c)) in
        lbl_blocks.(i) <- lbl;
        c := discard_dead_code c1
      done;
      for i = sw.sw_numconsts - 1 downto 0 do
        let (lbl, c1) =
          label_code(comp_expr env act_consts.(i) sz (branch :: !c)) in
        lbl_consts.(i) <- lbl;
        c := discard_dead_code c1
      done;
      if sw.sw_checked then c := comp_expr env Lstaticfail sz !c;        
      comp_expr env arg sz (Kswitch(lbl_consts, lbl_blocks) :: !c)
  | Lassign(id, expr) ->
      begin try
        let pos = Ident.find_same id env.ce_stack in
        comp_expr env expr sz (Kassign(sz - pos) :: cont)
      with Not_found ->
        fatal_error "Codegen.comp_expr: assign"
      end


(* Compile a list of arguments [e1; ...; eN] to a primitive operation.
   The values of eN ... e2 are pushed on the stack, e2 at top of stack,
   then e3, then ... The value of e1 is left in the accumulator. *)

and comp_args env argl sz cont =
  comp_expr_list env (List.rev argl) sz cont

and comp_expr_list env exprl sz cont =
  match exprl with
    [] -> cont
  | [exp] -> comp_expr env exp sz cont
  | exp :: rem ->
      comp_expr env exp sz (Kpush :: comp_expr_list env rem (sz+1) cont)

(* Compile an if-then-else test. *)

and comp_binary_test env cond ifso ifnot sz cont =
  let cont_cond =
    if ifnot = Lconst const_unit then begin
      let (lbl_end, cont1) = label_code cont in
      Kstrictbranchifnot lbl_end :: comp_expr env ifso sz cont1
    end else
    if ifso = Lstaticfail & sz = !sz_staticfail then
      Kbranchif !lbl_staticfail :: comp_expr env ifnot sz cont
    else
    if ifnot = Lstaticfail & sz = !sz_staticfail then
      Kbranchifnot !lbl_staticfail :: comp_expr env ifso sz cont
    else begin
      let (branch_end, cont1) = make_branch cont in
      let (lbl_not, cont2) = label_code(comp_expr env ifnot sz cont1) in
      Kbranchifnot lbl_not :: comp_expr env ifso sz (branch_end :: cont2)
    end in
  comp_expr env cond sz cont_cond

(**** Compilation of functions ****)

let comp_function (params, fun_body, entry_lbl, free_vars) cont =
  let arity = List.length params in
  let rec pos_args pos delta = function
      [] -> Ident.empty
    | id :: rem -> Ident.add id pos (pos_args (pos+delta) delta rem) in
  let env =
    { ce_stack = pos_args arity (-1) params;
      ce_heap = pos_args 0 1 free_vars } in
  let cont1 =
    comp_expr env fun_body arity (Kreturn arity :: cont) in
  if arity > 1 then
    Krestart :: Klabel entry_lbl :: Kgrab(arity - 1) :: cont1
  else
    Klabel entry_lbl :: cont1

let comp_remainder cont =
  let c = ref cont in
  begin try
    while true do
      c := comp_function (Stack.pop functions_to_compile) !c
    done
  with Stack.Empty ->
    ()
  end;
  !c

(**** Compilation of a lambda phrase ****)

let compile_implementation expr =
  Stack.clear functions_to_compile;
  label_counter := 0;
  lbl_staticfail := 0;
  sz_staticfail := 0;
  let init_code = comp_expr empty_env expr 0 [] in
  if Stack.length functions_to_compile > 0 then begin
    let lbl_init = new_label() in
    Kbranch lbl_init :: comp_remainder (Klabel lbl_init :: init_code)
  end else
    init_code

let compile_phrase expr =
  Stack.clear functions_to_compile;
  label_counter := 0;
  lbl_staticfail := 0;
  sz_staticfail := 0;
  let init_code = comp_expr empty_env expr 0 [Kstop] in
  let fun_code = comp_remainder [] in
  (init_code, fun_code)

