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

(* Instruction scheduling *)

open Misc
open Reg
open Mach
open Linearize

(* Representation of the code DAG. *)

type code_dag_node =
  { instr: instruction;                 (* The instruction *)
    delay: int;           (* How many cycles before result is available *)
    mutable sons: (code_dag_node * int) list;
                                        (* Instructions that depend on it *)
    mutable date: int;                  (* Start date *)
    mutable length: int;                (* Length of longest path to result *)
    mutable ancestors: int;             (* Number of ancestors *)
    mutable emitted_ancestors: int }    (* Number of emitted ancestors *)

let dummy_node =
  { instr = end_instr; delay = 0; sons = []; date = 0;
    length = -1; ancestors = 0; emitted_ancestors = 0 }

(* The code dag itself is represented by two tables from registers to nodes:
   - "results" maps registers to the instructions that produced them;
   - "uses" maps registers to the instructions that use them. *)

let code_results = (Hashtbl.create 31 : (location, code_dag_node) Hashtbl.t)
let code_uses = (Hashtbl.create 31 : (location, code_dag_node) Hashtbl.t)

let clear_code_dag () =
  Hashtbl.clear code_results;
  Hashtbl.clear code_uses

(* Add an edge to the code DAG *)

let add_edge ancestor son delay =
  ancestor.sons <- (son, delay) :: ancestor.sons;
  son.ancestors <- son.ancestors + 1

(* Compute length of longest path to a result.
   For leafs of the DAG, see whether their result is used in the instruction
   immediately following the basic block (a "critical" output). *)

let is_critical critical_outputs results =
  try
    for i = 0 to Array.length results - 1 do
      let r = results.(i).loc in
      for j = 0 to Array.length critical_outputs - 1 do
        if critical_outputs.(j).loc = r then raise Exit
      done
    done;
    false
  with Exit ->
    true

let rec longest_path critical_outputs node =
  if node.length < 0 then begin
    match node.sons with
      [] ->
        node.length <-
          if is_critical critical_outputs node.instr.res
          or node.instr.desc = Lreloadretaddr (* alway critical *)
          then node.delay
          else 0
    | sons ->
        node.length <- 
          List.fold_left
            (fun len (son, delay) ->
              max len (longest_path critical_outputs son + delay))
            0 sons
  end;
  node.length

(* Remove an instruction from the ready queue *)

let rec remove_instr node = function
    [] -> []
  | instr :: rem ->
      if instr == node then rem else instr :: remove_instr node rem

(* We treat Lreloadretaddr as a word-sized load *)

let some_load = (Iload(Cmm.Word, Arch.identity_addressing))

(* The generic scheduler *)

class virtual scheduler_generic () as self =

(* Determine whether an operation ends a basic block or not.
   Can be overriden for some processors to signal specific instructions
   that terminate a basic block, e.g. Istore_symbol for the 386. *)

method oper_in_basic_block = function
    Icall_ind -> false
  | Icall_imm _ -> false
  | Itailcall_ind -> false
  | Itailcall_imm _ -> false
  | Iextcall(_, _) -> false
  | Istackoffset _ -> false
  | Istore(_, _) -> false
  | Ialloc _ -> false
  | _ -> true

(* Determine whether an instruction ends a basic block or not *)

method instr_in_basic_block instr =
  match instr.desc with
    Lop op -> self#oper_in_basic_block op
  | Lreloadretaddr -> true
  | _ -> false

(* Estimate the delay needed to evaluate an operation. *)

virtual oper_latency : Mach.operation -> int

(* Estimate the delay needed to evaluate an instruction *)

method instr_latency instr =
  match instr.desc with
    Lop op ->
      self#oper_latency op
  | Lreloadretaddr ->
      self#oper_latency some_load
  | _ ->
      assert false

(* Estimate the number of cycles consumed by emitting an operation. *)

virtual oper_issue_cycles : Mach.operation -> int

(* Estimate the number of cycles consumed by emitting an instruction. *)

method instr_issue_cycles instr =
  match instr.desc with
    Lop op ->
      self#oper_issue_cycles op
  | Lreloadretaddr ->
      self#oper_issue_cycles some_load
  | _ ->
      assert false

(* Add an instruction to the code dag *)

method add_instruction ready_queue instr =
  let delay = self#instr_latency instr in
  let node =
    { instr = instr;
      delay = delay;
      sons = [];
      date = 0;
      length = -1;
      ancestors = 0;
      emitted_ancestors = 0 } in
  (* Add edges from all instructions that define one of the registers used
     (RAW dependencies) *)
  for i = 0 to Array.length instr.arg - 1 do
    try
      let ancestor = Hashtbl.find code_results instr.arg.(i).loc in
      add_edge ancestor node ancestor.delay
    with Not_found ->
      ()
  done;
  (* Also add edges from all instructions that use one of the result regs
     of this instruction (WAR dependencies). *)
  for i = 0 to Array.length instr.res - 1 do
    let ancestors = Hashtbl.find_all code_uses instr.res.(i).loc in
    List.iter (fun ancestor -> add_edge ancestor node 0) ancestors
  done;
  (* Also add edges from all instructions that have already defined one
     of the results of this instruction (WAW dependencies). *)
  for i = 0 to Array.length instr.res - 1 do
    try
      let ancestor = Hashtbl.find code_results instr.res.(i).loc in
      add_edge ancestor node 0
    with Not_found ->
      ()
  done;
  (* Remember the registers used and produced by this instruction *)
  for i = 0 to Array.length instr.res - 1 do
    Hashtbl.add code_results instr.res.(i).loc node
  done;
  for i = 0 to Array.length instr.arg - 1 do
    Hashtbl.add code_uses instr.arg.(i).loc node
  done;
  (* If this is a root instruction (all arguments already computed),
     add it to the ready queue *)
  if node.ancestors = 0 then node :: ready_queue else ready_queue

(* Given a list of instructions and a date, choose one or several
   that are ready to be computed (start date <= current date)
   and that we can emit in one cycle.  Favor instructions with
   maximal distance to result.  If we can't find any, return None.
   This does not take multiple issues into account, though. *)

method ready_instruction date queue =
  let rec extract best = function
    [] ->
      if best == dummy_node then None else Some best
  | instr :: rem ->
      let new_best =
        if instr.date <= date && instr.length > best.length
        then instr else best in
      extract new_best rem in
  extract dummy_node queue
  
(* Schedule a basic block, adding its instructions in front of the given
   instruction sequence *)

method reschedule ready_queue date cont =
  if ready_queue = [] then cont else begin
    match self#ready_instruction date ready_queue with
      None ->
        self#reschedule ready_queue (date + 1) cont
    | Some node ->
        (* Remove node from queue *)
        let new_queue = ref (remove_instr node ready_queue) in
        (* Update the start date and number of ancestors emitted of
           all descendents of this node. Enter those that become ready
           in the queue. *)
        List.iter
          (fun (son, delay) ->
            let completion_date = date + delay in
            if son.date < completion_date then son.date <- completion_date;
            son.emitted_ancestors <- son.emitted_ancestors + 1;
            if son.emitted_ancestors = son.ancestors then
              new_queue := son :: !new_queue)
          node.sons;
        let issue_cycles = self#instr_issue_cycles node.instr in
        instr_cons node.instr.desc node.instr.arg node.instr.res
          (self#reschedule !new_queue (date + issue_cycles) cont)
  end

(* Entry point *)
(* Don't bother to schedule for initialization code and the like. *)

method schedule_fundecl f =

  let rec schedule i =
    match i.desc with
      Lend -> i
    | _ ->
        if self#instr_in_basic_block i then begin
          clear_code_dag();
          schedule_block [] i
        end else
          { desc = i.desc; arg = i.arg; res = i.res; live = i.live;
            next = schedule i.next }

  and schedule_block ready_queue i =
    if self#instr_in_basic_block i then
      schedule_block (self#add_instruction ready_queue i) i.next
    else begin
      let critical_outputs =
        match i.desc with
          Lop(Icall_ind | Itailcall_ind) -> [| i.arg.(0) |]
        | Lop(Icall_imm _ | Itailcall_imm _ | Iextcall(_, _)) -> [||]
        | Lreturn -> [||]
        | _ -> i.arg in
      List.iter (longest_path critical_outputs) ready_queue;
      self#reschedule ready_queue 0 (schedule i)
    end in

  if f.fun_fast then begin
    let new_body = schedule f.fun_body in
    clear_code_dag();
    { fun_name = f.fun_name;
      fun_body = new_body;
      fun_fast = f.fun_fast }
  end else
    f

end
