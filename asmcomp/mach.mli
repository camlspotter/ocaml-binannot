(* Representation of machine code by sequences of pseudoinstructions *)

type integer_comparison =
    Isigned of Cmm.comparison
  | Iunsigned of Cmm.comparison

type integer_operation =
    Iadd | Isub | Imul | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Icomp of integer_comparison

type test =
    Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of Cmm.comparison

type operation =
    Imove
  | Ispill
  | Ireload
  | Iconst_int of int
  | Iconst_float of string
  | Iconst_symbol of string
  | Icall_ind
  | Icall_imm of string
  | Itailcall_ind
  | Itailcall_imm of string
  | Iextcall of string
  | Istackoffset of int
  | Iload of Cmm.memory_chunk * Arch.addressing_mode
  | Istore of Cmm.memory_chunk * Arch.addressing_mode
  | Ialloc of int
  | Imodify
  | Iintop of integer_operation
  | Iintop_imm of integer_operation * int
  | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat
  | Ispecific of Arch.specific_operation

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    mutable live: Reg.Set.t }

and instruction_desc =
    Iend
  | Iop of operation
  | Ireturn
  | Iifthenelse of test * instruction * instruction
  | Iswitch of int array * instruction array
  | Iloop of instruction
  | Icatch of instruction * instruction
  | Iexit
  | Itrywith of instruction * instruction
  | Iraise

type fundecl =
  { fun_name: string;
    fun_args: Reg.t array;
    fun_body: instruction;
    fun_fast: bool }

val dummy_instr: instruction
val end_instr: unit -> instruction
val instr_cons: 
      instruction_desc -> Reg.t array -> Reg.t array -> instruction ->
        instruction
val instr_cons_live: 
      instruction_desc -> Reg.t array -> Reg.t array -> Reg.Set.t ->
        instruction -> instruction
val instr_iter: (instruction -> unit) -> instruction -> unit

