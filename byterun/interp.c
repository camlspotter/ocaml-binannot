/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* The bytecode interpreter */
#include <stdio.h>
#include "alloc.h"
#include "backtrace.h"
#include "callback.h"
#include "debugger.h"
#include "fail.h"
#include "fix_code.h"
#include "instrtrace.h"
#include "instruct.h"
#include "interp.h"
#include "major_gc.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"
#include "signals.h"
#include "stacks.h"

/* Registers for the abstract machine:
        pc         the code pointer
        sp         the stack pointer (grows downward)
        accu       the accumulator
        env        heap-allocated environment
        trapsp     pointer to the current trap frame
        extra_args number of extra arguments provided by the caller

sp is a local copy of the global variable extern_sp. */

/* Instruction decoding */

#ifdef THREADED_CODE
#  define Instruct(name) lbl_##name
#  if defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
#    define Jumptbl_base ((char *) &&lbl_ACC0)
#  else
#    define Jumptbl_base ((char *) 0)
#    define jumptbl_base ((char *) 0)
#  endif
#  ifdef DEBUG
#    define Next goto next_instr
#  else
#    ifdef __ia64__
#      define Next goto *(void *)(jumptbl_base + *((uint32 *) pc)++)
#    else
#      define Next goto *(void *)(jumptbl_base + *pc++)
#    endif
#  endif
#else
#  define Instruct(name) case name
#  define Next break
#endif

/* GC interface */

#define Setup_for_gc { sp -= 2; sp[0] = accu; sp[1] = env; extern_sp = sp; }
#define Restore_after_gc { accu = sp[0]; env = sp[1]; sp += 2; }
#define Setup_for_c_call { *--sp = env; extern_sp = sp; }
#define Restore_after_c_call { sp = extern_sp; env = *sp++; }

/* An event frame must look like accu + a C_CALL frame + a RETURN 1 frame */
#define Setup_for_event \
  { sp -= 6; \
    sp[0] = accu; /* accu */ \
    sp[1] = Val_unit; /* C_CALL frame: dummy environment */ \
    sp[2] = Val_unit; /* RETURN frame: dummy local 0 */ \
    sp[3] = (value) pc; /* RETURN frame: saved return address */ \
    sp[4] = env; /* RETURN frame: saved environment */ \
    sp[5] = Val_long(extra_args); /* RETURN frame: saved extra args */ \
    extern_sp = sp; }
#define Restore_after_event \
  { sp = extern_sp; accu = sp[0]; \
    pc = (code_t) sp[3]; env = sp[4]; extra_args = Long_val(sp[5]); \
    sp += 6; }

/* Debugger interface */

#define Setup_for_debugger \
   { sp -= 4; \
     sp[0] = accu; sp[1] = (value)(pc - 1); \
     sp[2] = env; sp[3] = Val_long(extra_args); \
     extern_sp = sp; }
#define Restore_after_debugger { sp += 4; }

#ifdef THREADED_CODE
#define Restart_curr_instr \
  goto *(jumptable[saved_code[pc - 1 - start_code]])
#else
#define Restart_curr_instr \
  curr_instr = saved_code[pc - 1 - start_code]; \
  goto dispatch_instr
#endif

/* Register optimization.
   Some compilers underestimate the use of the local variables representing
   the abstract machine registers, and don't put them in hardware registers,
   which slows down the interpreter considerably.
   For GCC, I have hand-assigned hardware registers for several architectures.
*/

#if defined(__GNUC__) && !defined(DEBUG)
#ifdef __mips__
#define PC_REG asm("$16")
#define SP_REG asm("$17")
#define ACCU_REG asm("$18")
#endif
#ifdef __sparc__
#define PC_REG asm("%l0")
#define SP_REG asm("%l1")
#define ACCU_REG asm("%l2")
#endif
#ifdef __alpha__
#ifdef __CRAY__
#define PC_REG asm("r9")
#define SP_REG asm("r10")
#define ACCU_REG asm("r11")
#define JUMPTBL_BASE_REG asm("r12")
#else
#define PC_REG asm("$9")
#define SP_REG asm("$10")
#define ACCU_REG asm("$11")
#define JUMPTBL_BASE_REG asm("$12")
#endif
#endif
#ifdef __i386__
#define PC_REG asm("%esi")
#define SP_REG asm("%edi")
#define ACCU_REG
#endif
#if defined(PPC) || defined(_POWER) || defined(_IBMR2)
#define PC_REG asm("26")
#define SP_REG asm("27")
#define ACCU_REG asm("28")
#endif
#ifdef __hppa__
#define PC_REG asm("%r18")
#define SP_REG asm("%r17")
#define ACCU_REG asm("%r16")
#endif
#ifdef __mc68000__
#define PC_REG asm("a5")
#define SP_REG asm("a4")
#define ACCU_REG asm("d7")
#endif
#ifdef __arm__
#define PC_REG asm("r9")
#define SP_REG asm("r8")
#define ACCU_REG asm("r7")
#endif
#ifdef __ia64__
#define PC_REG asm("36")
#define SP_REG asm("37")
#define ACCU_REG asm("38")
#define JUMPTBL_BASE_REG asm("39")
#endif
#endif

/* Division and modulus madness */

#ifdef NONSTANDARD_DIV_MOD
static long safe_div(long p, long q);
static long safe_mod(long p, long q);
#endif

/* The interpreter itself */

value interprete(code_t prog, asize_t prog_size)
{
#ifdef PC_REG
  register code_t pc PC_REG;
  register value * sp SP_REG;
  register value accu ACCU_REG;
#else
  register code_t pc;
  register value * sp;
  register value accu;
#endif
#if defined(THREADED_CODE) && defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
#ifdef JUMPTBL_BASE_REG
  register char * jumptbl_base JUMPTBL_BASE_REG;
#else
  register char * jumptbl_base;
#endif
#endif
  value env;
  long extra_args;
  struct longjmp_buffer * initial_external_raise;
  int initial_sp_offset;
  /* volatile prevents collapsing initial_local_roots with another
     local variable, like Digital Unix 4.0 C compiler does (wrongly) */
  struct caml__roots_block * volatile initial_local_roots;
  struct longjmp_buffer raise_buf;
  value * modify_dest, modify_newval;
#ifndef THREADED_CODE
  opcode_t curr_instr;
#endif

#ifdef THREADED_CODE
  static void * jumptable[] = {
#    include "jumptbl.h"
  };
#endif

  if (prog == NULL) {           /* Interpreter is initializing */
#ifdef THREADED_CODE
    instr_table = (char **) jumptable;
    instr_base = Jumptbl_base;
#endif
    return Val_unit;
  }

#if defined(THREADED_CODE) && defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
  jumptbl_base = Jumptbl_base;
#endif
  initial_local_roots = local_roots;
  initial_sp_offset = (char *) stack_high - (char *) extern_sp;
  initial_external_raise = external_raise;
  callback_depth++;

  if (sigsetjmp(raise_buf.buf, 0)) {
    local_roots = initial_local_roots;
    sp = extern_sp;
    accu = exn_bucket;
    pc = NULL;
    goto raise_exception;
  }
  external_raise = &raise_buf;

  sp = extern_sp;
  pc = prog;
  extra_args = 0;
  env = Atom(0);
  accu = Val_int(0);

#ifdef THREADED_CODE
#ifdef DEBUG
 next_instr:
  if (icount-- == 0) stop_here ();
  Assert(sp >= stack_low);
  Assert(sp <= stack_high);
#endif
  goto *(void *)(jumptbl_base + *pc++); /* Jump to the first instruction */
#else
  while(1) {
#ifdef DEBUG
    if (icount-- == 0) stop_here ();
    if (trace_flag) disasm_instr(pc);
    Assert(sp >= stack_low);
    Assert(sp <= stack_high);
#endif
    curr_instr = *pc++;

  dispatch_instr:
    switch(curr_instr) {
#endif

/* Basic stack operations */

    Instruct(ACC0):
      accu = sp[0]; Next;
    Instruct(ACC1):
      accu = sp[1]; Next;
    Instruct(ACC2):
      accu = sp[2]; Next;
    Instruct(ACC3):
      accu = sp[3]; Next;
    Instruct(ACC4):
      accu = sp[4]; Next;
    Instruct(ACC5):
      accu = sp[5]; Next;
    Instruct(ACC6):
      accu = sp[6]; Next;
    Instruct(ACC7):
      accu = sp[7]; Next;

    Instruct(PUSH): Instruct(PUSHACC0):
      *--sp = accu; Next;
    Instruct(PUSHACC1):
      *--sp = accu; accu = sp[1]; Next;
    Instruct(PUSHACC2):
      *--sp = accu; accu = sp[2]; Next;
    Instruct(PUSHACC3):
      *--sp = accu; accu = sp[3]; Next;
    Instruct(PUSHACC4):
      *--sp = accu; accu = sp[4]; Next;
    Instruct(PUSHACC5):
      *--sp = accu; accu = sp[5]; Next;
    Instruct(PUSHACC6):
      *--sp = accu; accu = sp[6]; Next;
    Instruct(PUSHACC7):
      *--sp = accu; accu = sp[7]; Next;

    Instruct(PUSHACC):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ACC):
      accu = sp[*pc++];
      Next;

    Instruct(POP):
      sp += *pc++;
      Next;
    Instruct(ASSIGN):
      sp[*pc++] = accu;
      accu = Val_unit;
      Next;

/* Access in heap-allocated environment */

    Instruct(ENVACC1):
      accu = Field(env, 1); Next;
    Instruct(ENVACC2):
      accu = Field(env, 2); Next;
    Instruct(ENVACC3):
      accu = Field(env, 3); Next;
    Instruct(ENVACC4):
      accu = Field(env, 4); Next;

    Instruct(PUSHENVACC1):
      *--sp = accu; accu = Field(env, 1); Next;
    Instruct(PUSHENVACC2):
      *--sp = accu; accu = Field(env, 2); Next;
    Instruct(PUSHENVACC3):
      *--sp = accu; accu = Field(env, 3); Next;
    Instruct(PUSHENVACC4):
      *--sp = accu; accu = Field(env, 4); Next;

    Instruct(PUSHENVACC):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ENVACC):
      accu = Field(env, *pc++);
      Next;

/* Function application */

    Instruct(PUSH_RETADDR): {
      sp -= 3;
      sp[0] = (value) (pc + *pc);
      sp[1] = env;
      sp[2] = Val_long(extra_args);
      pc++;
      Next;
    }
    Instruct(APPLY): {
      extra_args = *pc - 1;
      pc = Code_val(accu);
      env = accu;
      goto check_stacks;
    }
    Instruct(APPLY1): {
      value arg1 = sp[0];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = (value)pc;
      sp[2] = env;
      sp[3] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 0;
      goto check_stacks;
    }
    Instruct(APPLY2): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = (value)pc;
      sp[3] = env;
      sp[4] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 1;
      goto check_stacks;
    }
    Instruct(APPLY3): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      value arg3 = sp[2];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = arg3;
      sp[3] = (value)pc;
      sp[4] = env;
      sp[5] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 2;
      goto check_stacks;
    }

    Instruct(APPTERM): {
      int nargs = *pc++;
      int slotsize = *pc;
      value * newsp;
      int i;
      /* Slide the nargs bottom words of the current frame to the top
         of the frame, and discard the remainder of the frame */
      newsp = sp + slotsize - nargs;
      for (i = nargs - 1; i >= 0; i--) newsp[i] = sp[i];
      sp = newsp;
      pc = Code_val(accu);
      env = accu;
      extra_args += nargs - 1;
      goto check_stacks;
    }
    Instruct(APPTERM1): {
      value arg1 = sp[0];
      sp = sp + *pc - 1;
      sp[0] = arg1;
      pc = Code_val(accu);
      env = accu;
      goto check_stacks;
    }
    Instruct(APPTERM2): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      sp = sp + *pc - 2;
      sp[0] = arg1;
      sp[1] = arg2;
      pc = Code_val(accu);
      env = accu;
      extra_args += 1;
      goto check_stacks;
    }
    Instruct(APPTERM3): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      value arg3 = sp[2];
      sp = sp + *pc - 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = arg3;
      pc = Code_val(accu);
      env = accu;
      extra_args += 2;
      goto check_stacks;
    }

    Instruct(RETURN): {
      sp += *pc++;
      if (extra_args > 0) {
        extra_args--;
        pc = Code_val(accu);
        env = accu;
      } else {
        pc = (code_t)(sp[0]);
        env = sp[1];
        extra_args = Long_val(sp[2]);
        sp += 3;
      }
      Next;
    }

    Instruct(RESTART): {
      int num_args = Wosize_val(env) - 2;
      int i;
      sp -= num_args;
      for (i = 0; i < num_args; i++) sp[i] = Field(env, i + 2);
      env = Field(env, 1);
      extra_args += num_args;
      Next;
    }

    Instruct(GRAB): {
      int required = *pc++;
      if (extra_args >= required) {
        extra_args -= required;
      } else {
        mlsize_t num_args, i;
        num_args = 1 + extra_args; /* arg1 + extra args */
        Alloc_small(accu, num_args + 2, Closure_tag);
        Field(accu, 1) = env;
        for (i = 0; i < num_args; i++) Field(accu, i + 2) = sp[i];
        Code_val(accu) = pc - 3; /* Point to the preceding RESTART instr. */
        sp += num_args;
        pc = (code_t)(sp[0]);
        env = sp[1];
        extra_args = Long_val(sp[2]);
        sp += 3;
      }
      Next;
    }

    Instruct(CLOSURE): {
      int nvars = *pc++;
      int i;
      if (nvars > 0) *--sp = accu;
      Alloc_small(accu, 1 + nvars, Closure_tag);
      Code_val(accu) = pc + *pc;
      pc++;
      for (i = 0; i < nvars; i++) Field(accu, i + 1) = sp[i];
      sp += nvars;
      Next;
    }

    Instruct(CLOSUREREC): {
      int nfuncs = *pc++;
      int nvars = *pc++;
      int i;
      value * p;
      if (nvars > 0) *--sp = accu;
      Alloc_small(accu, nfuncs * 2 - 1 + nvars, Closure_tag);
      p = &Field(accu, nfuncs * 2 - 1);
      for (i = 0; i < nvars; i++) {
        *p++ = sp[i];
      }
      sp += nvars;
      p = &Field(accu, 0);
      *p = (value) (pc + pc[0]);
      *--sp = accu;
      p++;
      for (i = 1; i < nfuncs; i++) {
        *p = Make_header(i * 2, Infix_tag, Caml_white);  /* color irrelevant. */
        p++;
        *p = (value) (pc + pc[i]);
        *--sp = (value) p;
        p++;
      }
      pc += nfuncs;
      Next;
    }

    Instruct(PUSHOFFSETCLOSURE):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE):
      accu = env + *pc++ * sizeof(value); Next;

    Instruct(PUSHOFFSETCLOSUREM2):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSUREM2):
      accu = env - 2 * sizeof(value); Next;
    Instruct(PUSHOFFSETCLOSURE0):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE0):
      accu = env; Next;
    Instruct(PUSHOFFSETCLOSURE2):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE2):
      accu = env + 2 * sizeof(value); Next;


/* Access to global variables */

    Instruct(PUSHGETGLOBAL):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBAL):
      accu = Field(global_data, *pc);
      pc++;
      Next;

    Instruct(PUSHGETGLOBALFIELD):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBALFIELD): {
      accu = Field(global_data, *pc);
      pc++;
      accu = Field(accu, *pc);
      pc++;
      Next;
    }

    Instruct(SETGLOBAL):
      modify(&Field(global_data, *pc), accu);
      accu = Val_unit;
      pc++;
      Next;

/* Allocation of blocks */

    Instruct(PUSHATOM0):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ATOM0):
      accu = Atom(0); Next;

    Instruct(PUSHATOM):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ATOM):
      accu = Atom(*pc++); Next;

    Instruct(MAKEBLOCK): {
      mlsize_t wosize = *pc++;
      tag_t tag = *pc++;
      mlsize_t i;
      value block;
      Alloc_small(block, wosize, tag);
      Field(block, 0) = accu;
      for (i = 1; i < wosize; i++) Field(block, i) = *sp++;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK1): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 1, tag);
      Field(block, 0) = accu;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK2): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 2, tag);
      Field(block, 0) = accu;
      Field(block, 1) = sp[0];
      sp += 1;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK3): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 3, tag);
      Field(block, 0) = accu;
      Field(block, 1) = sp[0];
      Field(block, 2) = sp[1];
      sp += 2;
      accu = block;
      Next;
    }
    Instruct(MAKEFLOATBLOCK): {
      mlsize_t size = *pc++;
      mlsize_t i;
      value block;
      Alloc_small(block, size * Double_wosize, Double_array_tag);
      Store_double_field(block, 0, Double_val(accu));
      for (i = 1; i < size; i++){
        Store_double_field(block, i, Double_val(*sp));
        ++ sp;
      }
      accu = block;
      Next;
    }

/* Access to components of blocks */

    Instruct(GETFIELD0):
      accu = Field(accu, 0); Next;
    Instruct(GETFIELD1):
      accu = Field(accu, 1); Next;
    Instruct(GETFIELD2):
      accu = Field(accu, 2); Next;
    Instruct(GETFIELD3):
      accu = Field(accu, 3); Next;
    Instruct(GETFIELD):
      accu = Field(accu, *pc); pc++; Next;
    Instruct(GETFLOATFIELD): {
      double d = Double_field(accu, *pc);
      Alloc_small(accu, Double_wosize, Double_tag);
      Store_double_val(accu, d);
      pc++;
      Next;
    }

    Instruct(SETFIELD0):
      modify_dest = &Field(accu, 0);
      modify_newval = *sp++;
    modify:
      Modify(modify_dest, modify_newval);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD1):
      modify_dest = &Field(accu, 1);
      modify_newval = *sp++;
      goto modify;
    Instruct(SETFIELD2):
      modify_dest = &Field(accu, 2);
      modify_newval = *sp++;
      goto modify;
    Instruct(SETFIELD3):
      modify_dest = &Field(accu, 3);
      modify_newval = *sp++;
      goto modify;
    Instruct(SETFIELD):
      modify_dest = &Field(accu, *pc);
      pc++;
      modify_newval = *sp++;
      goto modify;
    Instruct(SETFLOATFIELD):
      Store_double_field(accu, *pc, Double_val(*sp));
      accu = Val_unit;
      sp++;
      pc++;
      Next;

/* Array operations */

    Instruct(VECTLENGTH): {
      mlsize_t size = Wosize_val(accu);
      if (Tag_val(accu) == Double_array_tag) size = size / Double_wosize;
      accu = Val_long(size);
      Next;
    }
    Instruct(GETVECTITEM):
      accu = Field(accu, Long_val(sp[0]));
      sp += 1;
      Next;
    Instruct(SETVECTITEM):
      modify_dest = &Field(accu, Long_val(sp[0]));
      modify_newval = sp[1];
      sp += 2;
      goto modify;

/* String operations */

    Instruct(GETSTRINGCHAR):
      accu = Val_int(Byte_u(accu, Long_val(sp[0])));
      sp += 1;
      Next;
    Instruct(SETSTRINGCHAR):
      Byte_u(accu, Long_val(sp[0])) = Int_val(sp[1]);
      sp += 2;
      accu = Val_unit;
      Next;

/* Branches and conditional branches */

    Instruct(BRANCH):
      pc += *pc;
      Next;
    Instruct(BRANCHIF):
      if (accu != Val_false) pc += *pc; else pc++;
      Next;
    Instruct(BRANCHIFNOT):
      if (accu == Val_false) pc += *pc; else pc++;
      Next;
    Instruct(SWITCH): {
      uint32 sizes = *pc++;
      if (Is_block(accu)) {
        long index = Tag_val(accu);
        Assert (index >= 0);
        Assert (index < (sizes >> 16));
        pc += pc[(sizes & 0xFFFF) + index];
      } else {
        long index = Long_val(accu);
        Assert ((unsigned long) index < (sizes & 0xFFFF)) ;
        pc += pc[index];
      }
      Next;
    }
    Instruct(BOOLNOT):
      accu = Val_not(accu);
      Next;

/* Exceptions */

    Instruct(PUSHTRAP):
      sp -= 4;
      Trap_pc(sp) = pc + *pc;
      Trap_link(sp) = trapsp;
      sp[2] = env;
      sp[3] = Val_long(extra_args);
      trapsp = sp;
      pc++;
      Next;

    Instruct(POPTRAP):
      if (something_to_do) {
        /* We must check here so that if a signal is pending and its
           handler triggers an exception, the exception is trapped
           by the current try...with, not the enclosing one. */
        pc--; /* restart the POPTRAP after processing the signal */
        goto process_signal;
      }
      trapsp = Trap_link(sp);
      sp += 4;
      Next;

    Instruct(RAISE):
    raise_exception:
      if (trapsp >= trap_barrier) debugger(TRAP_BARRIER);
      if (backtrace_active) stash_backtrace(accu, pc, sp);
      sp = trapsp;
      if ((char *) sp >= (char *) stack_high - initial_sp_offset) {
        external_raise = initial_external_raise;
        extern_sp = sp;
        callback_depth--;
        return Make_exception_result(accu);
      }
      pc = Trap_pc(sp);
      trapsp = Trap_link(sp);
      env = sp[2];
      extra_args = Long_val(sp[3]);
      sp += 4;
      Next;

/* Stack checks */

    check_stacks:
      if (sp < stack_threshold) {
        extern_sp = sp;
        realloc_stack(Stack_threshold / sizeof(value));
        sp = extern_sp;
      }
      /* Fall through CHECK_SIGNALS */

/* Signal handling */

    Instruct(CHECK_SIGNALS):    /* accu not preserved */
      if (something_to_do) goto process_signal;
      Next;

    process_signal:
      something_to_do = 0;
      Setup_for_event;
      process_event();
      Restore_after_event;
      Next;

/* Calling C functions */

    Instruct(C_CALL1):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu);
      Restore_after_c_call;
      pc++;
      Next;
    Instruct(C_CALL2):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1]);
      Restore_after_c_call;
      sp += 1;
      pc++;
      Next;
    Instruct(C_CALL3):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1], sp[2]);
      Restore_after_c_call;
      sp += 2;
      pc++;
      Next;
    Instruct(C_CALL4):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1], sp[2], sp[3]);
      Restore_after_c_call;
      sp += 3;
      pc++;
      Next;
    Instruct(C_CALL5):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1], sp[2], sp[3], sp[4]);
      Restore_after_c_call;
      sp += 4;
      pc++;
      Next;
    Instruct(C_CALLN): {
      int nargs = *pc++;
      *--sp = accu;
      Setup_for_c_call;
      accu = Primitive(*pc)(sp + 1, nargs);
      Restore_after_c_call;
      sp += nargs;
      pc++;
      Next;
    }

/* Integer constants */

    Instruct(CONST0):
      accu = Val_int(0); Next;
    Instruct(CONST1):
      accu = Val_int(1); Next;
    Instruct(CONST2):
      accu = Val_int(2); Next;
    Instruct(CONST3):
      accu = Val_int(3); Next;

    Instruct(PUSHCONST0):
      *--sp = accu; accu = Val_int(0); Next;
    Instruct(PUSHCONST1):
      *--sp = accu; accu = Val_int(1); Next;
    Instruct(PUSHCONST2):
      *--sp = accu; accu = Val_int(2); Next;
    Instruct(PUSHCONST3):
      *--sp = accu; accu = Val_int(3); Next;

    Instruct(PUSHCONSTINT):
      *--sp = accu;
      /* Fallthrough */
    Instruct(CONSTINT):
      accu = Val_int(*pc);
      pc++;
      Next;

/* Integer arithmetic */

    Instruct(NEGINT):
      accu = (value)(2 - (long)accu); Next;
    Instruct(ADDINT):
      accu = (value)((long) accu + (long) *sp++ - 1); Next;
    Instruct(SUBINT):
      accu = (value)((long) accu - (long) *sp++ + 1); Next;
    Instruct(MULINT):
      accu = Val_long(Long_val(accu) * Long_val(*sp++)); Next;

    Instruct(DIVINT): {
      long divisor = Long_val(*sp++);
      if (divisor == 0) { Setup_for_c_call; raise_zero_divide(); }
#ifdef NONSTANDARD_DIV_MOD
      accu = Val_long(safe_div(Long_val(accu), divisor));
#else
      accu = Val_long(Long_val(accu) / divisor);
#endif
      Next;
    }
    Instruct(MODINT): {
      long divisor = Long_val(*sp++);
      if (divisor == 0) { Setup_for_c_call; raise_zero_divide(); }
#ifdef NONSTANDARD_DIV_MOD
      accu = Val_long(safe_mod(Long_val(accu), divisor));
#else
      accu = Val_long(Long_val(accu) % divisor);
#endif
      Next;
    }
    Instruct(ANDINT):
      accu = (value)((long) accu & (long) *sp++); Next;
    Instruct(ORINT):
      accu = (value)((long) accu | (long) *sp++); Next;
    Instruct(XORINT):
      accu = (value)(((long) accu ^ (long) *sp++) | 1); Next;
    Instruct(LSLINT):
      accu = (value)((((long) accu - 1) << Long_val(*sp++)) + 1); Next;
    Instruct(LSRINT):
      accu = (value)((((unsigned long) accu - 1) >> Long_val(*sp++)) | 1);
      Next;
    Instruct(ASRINT):
      accu = (value)((((long) accu - 1) >> Long_val(*sp++)) | 1); Next;

#define Integer_comparison(sign,opname,tst) \
    Instruct(opname): \
      accu = Val_int((sign long) accu tst (sign long) *sp++); Next;

    Integer_comparison(signed,EQ, ==)
    Integer_comparison(signed,NEQ, !=)
    Integer_comparison(signed,LTINT, <)
    Integer_comparison(signed,LEINT, <=)
    Integer_comparison(signed,GTINT, >)
    Integer_comparison(signed,GEINT, >=)
    Integer_comparison(unsigned,ULTINT, <)
    Integer_comparison(unsigned,UGEINT, >=)

#define Integer_branch_comparison(sign,opname,tst,debug) \
    Instruct(opname): \
      if ( *pc++ tst ((sign long)Long_val(accu))) { \
        pc += *pc ; \
      } else { \
        pc++ ; \
      } ; Next;

    Integer_branch_comparison(signed,BEQ, ==, "==")
    Integer_branch_comparison(signed,BNEQ, !=, "!=")
    Integer_branch_comparison(signed,BLTINT, <, "<")
    Integer_branch_comparison(signed,BLEINT, <=, "<=")
    Integer_branch_comparison(signed,BGTINT, >, ">")
    Integer_branch_comparison(signed,BGEINT, >=, ">=")
    Integer_branch_comparison(unsigned,BULTINT, <, "<")
    Integer_branch_comparison(unsigned,BUGEINT, >=, ">=")

    Instruct(OFFSETINT):
      accu += *pc << 1;
      pc++;
      Next;
    Instruct(OFFSETREF):
      Field(accu, 0) += *pc << 1;
      accu = Val_unit;
      pc++;
      Next;
    Instruct(ISINT):
      accu = Val_long(accu & 1);
      Next;
    
/* Object-oriented operations */

#define Lookup(obj, lab) \
  Field (Field (Field (obj, 0), ((lab) >> 16) / sizeof (value)), \
         ((lab) / sizeof (value)) & 0xFF)

    Instruct(GETMETHOD):
      accu = Lookup(sp[0], accu);
      Next;

/* Debugging and machine control */

    Instruct(STOP):
      external_raise = initial_external_raise;
      extern_sp = sp;
      callback_depth--;
      return accu;

    Instruct(EVENT):
      if (--event_count == 0) {
        Setup_for_debugger;
        debugger(EVENT_COUNT);
        Restore_after_debugger;
      }
      Restart_curr_instr;

    Instruct(BREAK):
      Setup_for_debugger;
      debugger(BREAKPOINT);
      Restore_after_debugger;
      Restart_curr_instr;

#ifndef THREADED_CODE
    default:
#if _MSC_VER >= 1200
      __assume(0);
#else
      fatal_error_arg("Fatal error: bad opcode (%lx)\n",
                      (char *)(long)(*(pc-1)));
#endif
    }
  }
#endif
}

#ifdef NONSTANDARD_DIV_MOD
long safe_div(long p, long q)
{
  unsigned long ap = p >= 0 ? p : -p;
  unsigned long aq = q >= 0 ? q : -q;
  unsigned long ar = ap / aq;
  return (p ^ q) >= 0 ? ar : -ar;
}

long safe_mod(long p, long q)
{
  unsigned long ap = p >= 0 ? p : -p;
  unsigned long aq = q >= 0 ? q : -q;
  unsigned long ar = ap % aq;
  return p >= 0 ? ar : -ar;
}
#endif
