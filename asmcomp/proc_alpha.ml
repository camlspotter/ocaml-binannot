(* Description of the Alpha processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Exceptions raised to signal cases not handled here *)

exception Use_default

(* Instruction selection *)

let select_addressing = function
    Cconst(Const_symbol s) ->
      (Ibased(s, 0), Ctuple [])
  | Cop(Cadda, [Cconst(Const_symbol s); Cconst(Const_int n)]) ->
      (Ibased(s, n), Ctuple [])
  | Cop(Cadda, [arg; Cconst(Const_int n)]) ->
      (Iindexed n, arg)
  | arg ->
      (Iindexed 0, arg)

let select_oper op args =
  match (op, args) with
    ((Caddi|Cadda),
     [arg2; Cop(Clsl, [arg1; Cconst(Const_int(2|3 as shift))])]) ->
      (Ispecific(if shift = 2 then Iadd4 else Iadd8), Ctuple[arg1; arg2])
  | ((Caddi|Cadda),
     [arg2; Cop(Cmuli, [arg1; Cconst(Const_int(4|8 as mult))])]) ->
      (Ispecific(if mult = 4 then Iadd4 else Iadd8), Ctuple[arg1; arg2])
  | ((Caddi|Cadda),
     [arg2; Cop(Cmuli, [Cconst(Const_int(4|8 as mult)); arg1])]) ->
      (Ispecific(if mult = 4 then Iadd4 else Iadd8), Ctuple[arg1; arg2])
  | (Caddi, [Cop(Clsl, [arg1; Cconst(Const_int(2|3 as shift))]); arg2]) ->
      (Ispecific(if shift = 2 then Iadd4 else Iadd8), Ctuple[arg1; arg2])
  | (Caddi, [Cop(Cmuli, [arg1; Cconst(Const_int(4|8 as mult))]); arg2]) ->
      (Ispecific(if mult = 4 then Iadd4 else Iadd8), Ctuple[arg1; arg2])
  | (Caddi, [Cop(Cmuli, [Cconst(Const_int(4|8 as mult)); arg1]); arg2]) ->
      (Ispecific(if mult = 4 then Iadd4 else Iadd8), Ctuple[arg1; arg2])
  | (Csubi, [Cop(Clsl, [arg1; Cconst(Const_int(2|3 as shift))]); arg2]) ->
      (Ispecific(if shift = 2 then Isub4 else Isub8), Ctuple[arg1; arg2])
  | _ ->
      raise Use_default

let pseudoregs_for_operation op arg res = raise Use_default

let is_immediate (n:int) = true

(* Registers available for register allocation *)

(* Register map:
    $0 - $7     0 - 7       function results
    $8          8           general purpose
    $9 - $12    9 - 12      function arguments ($9 - $15 are preserved by C)
    $13                     allocation pointer
    $14                     allocation limit
    $15                     trap pointer
    $16 - $21   13 - 18     more function arguments, C function arguments
    $22 - $23   19 - 20     more function arguments
    $24, $25                temporaries
    $26-$30                 stack ptr, global ptr, etc
    $31                     always zero

    $f0 - $f1   100 - 101   function results
    $f10 - $f15 102 - 107   more function results
    $f2 - $f9   108 - 115   function arguments ($f2 - $f9 preserved by C)
    $f16 - $f21 116 - 121   C function arguments
    $f22 - $f29 122 - 129   general purpose
    $f30                    temporary
    $f31                    always zero *)

let int_reg_name = [|
  (* 0-8 *)    "$0"; "$1"; "$2"; "$3"; "$4"; "$5"; "$6"; "$7"; "$8";
  (* 9-12 *)   "$9"; "$10"; "$11"; "$12";
  (* 13-18 *)  "$16"; "$17"; "$18"; "$19"; "$20"; "$21";
  (* 19-20 *)  "$22"; "$23"
|]
  
let float_reg_name = [|
  (* 100-107 *)"$f0"; "$f1"; "$f10"; "$f11"; "$f12"; "$f13"; "$f14"; "$f15";
  (* 108-115 *)"$f2"; "$f3"; "$f4"; "$f5"; "$f6"; "$f7"; "$f8"; "$f9";
  (* 116-121 *)"$f16"; "$f17"; "$f18"; "$f19"; "$f20"; "$f21";
  (* 122-127 *)"$f22"; "$f23"; "$f24"; "$f25"; "$f26"; "$f27";
  (* 128-129 *)"$f28"; "$f29"
|]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 21; 30 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.new 21 Reg.dummy in
  for i = 0 to 20 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.new 30 Reg.dummy in
  for i = 0 to 29 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
  v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float make_stack
                        arg =
  let loc = Array.new (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int;
          incr int
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, Misc.align !ofs 16)         (* Keep stack 16-aligned *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 9 20 108 115 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 9 20 108 115 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 7 100 107 not_supported res in loc

(* On the Alpha, C functions have calling conventions similar to those
   for Caml functions, except that integer and floating-point registers
   for arguments are allocated "in sequence". E.g. a function
   taking a float f1 and two ints i2 and i3 will put f1 in the
   first float reg, i2 in the second int reg and i3 in the third int reg. *)

let ext_calling_conventions first_int last_int first_float last_float arg =
  let loc = Array.new (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int; incr int; incr float
        end else 
          fatal_error "Proc.ext_calling_conventions: cannot call"
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float; incr int; incr float
        end else
          fatal_error "Proc.ext_calling_conventions: cannot call"
  done;
  loc

let loc_external_arguments arg =
  (ext_calling_conventions 13 18 116 121 arg, 0)
let loc_external_results res =
  ext_calling_conventions 0 0 100 100 res

let loc_exn_bucket = phys_reg 0         (* $0 *)

(* Registers destroyed by operations *)

let destroyed_at_call = all_phys_regs
let destroyed_at_raise = all_phys_regs
let destroyed_at_extcall = (* $9 -$15, $f2 - $f9 preserved *)
  Array.of_list(List.map phys_reg
    [0; 1; 2; 3; 4; 5; 6; 7; 8; 13; 14; 15; 16;
     17; 18; 19; 20; 100; 101; 102; 103; 104; 105; 106; 107; 116; 117;
     118; 119; 120; 121; 122; 123; 124; 125; 126; 127; 128; 129])

let destroyed_at_oper op = [||]

(* Reloading *)

let reload_test makereg tst args = raise Use_default
let reload_operation makereg op args res = raise Use_default

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let stack_offset = ref 0
let contains_calls = ref false

let frame_size () =
  let size =
    !stack_offset +
    8 * (num_stack_slots.(0) + num_stack_slots.(1)) +
    (if !contains_calls then 8 else 0) in
  Misc.align size 16

let slot_offset loc class =
  match loc with
    Incoming n -> frame_size() + n
  | Local n ->
      if class = 0
      then !stack_offset + n * 8
      else !stack_offset + (num_stack_slots.(0) + n) * 8
  | Outgoing n -> n
