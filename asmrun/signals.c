/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <signal.h>
#include <stdio.h>
#if defined(TARGET_sparc) && defined(SYS_solaris)
#include <ucontext.h>
#endif
#include "alloc.h"
#include "callback.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "fail.h"
#include "signals.h"
#include "stack.h"
#include "sys.h"

#if defined(TARGET_power) && defined(SYS_rhapsody)
/* Confer machdep/ppc/unix_signal.c and mach/ppc/thread_status.h
   in the Darwin sources */
#define CONTEXT_GPR(ctx, regno) \
  (((unsigned long *)((ctx)->sc_regs))[2 + (regno)])
#endif

#if defined(TARGET_power) && defined(SYS_aix)
#ifdef _AIXVERSION_430
#define STRUCT_SIGCONTEXT struct __sigcontext
#define CONTEXT_GPR(ctx, regno) \
  ((ctx)->__sc_jmpbuf.__jmp_context.__gpr[regno])
#else
#define STRUCT_SIGCONTEXT struct sigcontext
#define CONTEXT_GPR(ctx, regno) \
  ((ctx)->sc_jmpbuf.jmp_context.gpr[regno])
#endif
#endif

volatile int async_signal_mode = 0;
volatile int pending_signal = 0;
volatile int force_major_slice = 0;
value signal_handlers = 0;
void (*enter_blocking_section_hook)() = NULL;
void (*leave_blocking_section_hook)() = NULL;

static int rev_convert_signal_number(int signo);

/* Execute a signal handler immediately. */

void execute_signal(int signal_number, int in_signal_handler)
{
  value res;
#ifdef POSIX_SIGNALS
  sigset_t sigs;
  /* Block the signal before executing the handler, and record in sigs
     the original signal mask */
  sigemptyset(&sigs);
  sigaddset(&sigs, signal_number);
  sigprocmask(SIG_BLOCK, &sigs, &sigs);
#endif
  res = callback_exn(Field(signal_handlers, signal_number),
                     Val_int(rev_convert_signal_number(signal_number)));
#ifdef POSIX_SIGNALS
  if (! in_signal_handler) {
    /* Restore the original signal mask */
    sigprocmask(SIG_SETMASK, &sigs, NULL);
  } else if (Is_exception_result(res)) {
    /* Restore the original signal mask and unblock the signal itself */
    sigdelset(&sigs, signal_number);
    sigprocmask(SIG_SETMASK, &sigs, NULL);
  }
#endif
  if (Is_exception_result(res)) mlraise(Extract_exception(res));
}

/* This routine is the common entry point for garbage collection
   and signal handling */

void garbage_collection(void)
{
  int sig;

  if (young_ptr < young_start || force_major_slice) minor_collection();
  /* If a signal arrives between the following two instructions,
     it will be lost. */
  sig = pending_signal;
  pending_signal = 0;
  young_limit = young_start;
  if (sig) execute_signal(sig, 0);
}

/* Trigger a garbage collection as soon as possible */

void urge_major_slice (void)
{
  force_major_slice = 1;
  young_limit = young_end;
  /* This is only moderately effective on ports that cache young_limit
     in a register, since modify() is called directly, not through
     caml_c_call, so it may take a while before the register is reloaded
     from young_limit. */
}

void enter_blocking_section(void)
{
  int sig;

  while (1){
    Assert (!async_signal_mode);
    /* If a signal arrives between the next two instructions,
       it will be lost. */
    sig = pending_signal;
    pending_signal = 0;
    young_limit = young_start;
    if (sig) execute_signal(sig, 0);
    async_signal_mode = 1;
    if (!pending_signal) break;
    async_signal_mode = 0;
  }
  if (enter_blocking_section_hook != NULL) enter_blocking_section_hook();
}

void leave_blocking_section(void)
{
  if (leave_blocking_section_hook != NULL) leave_blocking_section_hook();
  Assert(async_signal_mode);
  async_signal_mode = 0;
}

#if defined(TARGET_alpha) || defined(TARGET_mips)
void handle_signal(int sig, int code, struct sigcontext * context)
#elif defined(TARGET_power) && defined(SYS_aix)
void handle_signal(int sig, int code, STRUCT_SIGCONTEXT * context)
#elif defined(TARGET_power) && defined(SYS_elf)
void handle_signal(int sig, struct sigcontext * context)
#elif defined(TARGET_power) && defined(SYS_rhapsody)
void handle_signal(int sig, int code, struct sigcontext * context)
#else
void handle_signal(int sig)
#endif
{
#if !defined(POSIX_SIGNALS) && !defined(BSD_SIGNALS)
  signal(sig, handle_signal);
#endif
  if (async_signal_mode) {
    /* We are interrupting a C function blocked on I/O.
       Callback the Caml code immediately. */
    leave_blocking_section();
    execute_signal(sig, 1);
    enter_blocking_section();
  } else {
    /* We can't execute the signal code immediately.
       Instead, we remember the signal and play with the allocation limit
       so that the next allocation will trigger a garbage collection. */
    pending_signal = sig;
    young_limit = young_end;
    /* Some ports cache young_limit in a register.
       Use the signal context to modify that register too, but not if
       we are inside C code (i.e. caml_last_return_address != 0). */
    if (caml_last_return_address == 0) {
#if defined(TARGET_alpha)
      /* Cached in register $14 */
      context->sc_regs[14] = (long) young_limit;
#endif
#if defined(TARGET_mips)
      /* Cached in register $23 */
      context->sc_regs[23] = (int) young_limit;
#endif
#if defined(TARGET_power) && defined(SYS_aix)
      /* Cached in register 30 */
      CONTEXT_GPR(context, 30) = (ulong_t) young_limit;
#endif
#if defined(TARGET_power) && defined(SYS_elf)
      /* Cached in register 30 */
      context->regs->gpr[30] = (unsigned long) young_limit;
#endif
#if defined(TARGET_power) && defined(SYS_rhapsody)
      /* Cached in register 30 */
      CONTEXT_GPR(context, 30) = (unsigned long) young_limit;
#endif
    }
  }
}

#ifndef SIGABRT
#define SIGABRT -1
#endif
#ifndef SIGALRM
#define SIGALRM -1
#endif
#ifndef SIGFPE
#define SIGFPE -1
#endif
#ifndef SIGHUP
#define SIGHUP -1
#endif
#ifndef SIGILL
#define SIGILL -1
#endif
#ifndef SIGINT
#define SIGINT -1
#endif
#ifndef SIGKILL
#define SIGKILL -1
#endif
#ifndef SIGPIPE
#define SIGPIPE -1
#endif
#ifndef SIGQUIT
#define SIGQUIT -1
#endif
#ifndef SIGSEGV
#define SIGSEGV -1
#endif
#ifndef SIGTERM
#define SIGTERM -1
#endif
#ifndef SIGUSR1
#define SIGUSR1 -1
#endif
#ifndef SIGUSR2
#define SIGUSR2 -1
#endif
#ifndef SIGCHLD
#define SIGCHLD -1
#endif
#ifndef SIGCONT
#define SIGCONT -1
#endif
#ifndef SIGSTOP
#define SIGSTOP -1
#endif
#ifndef SIGTSTP
#define SIGTSTP -1
#endif
#ifndef SIGTTIN
#define SIGTTIN -1
#endif
#ifndef SIGTTOU
#define SIGTTOU -1
#endif
#ifndef SIGVTALRM
#define SIGVTALRM -1
#endif
#ifndef SIGPROF
#define SIGPROF -1
#endif

static int posix_signals[] = {
  SIGABRT, SIGALRM, SIGFPE, SIGHUP, SIGILL, SIGINT, SIGKILL, SIGPIPE,
  SIGQUIT, SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGCONT,
  SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU, SIGVTALRM, SIGPROF
};

int convert_signal_number(int signo)
{
  if (signo < 0 && signo >= -(sizeof(posix_signals) / sizeof(int)))
    return posix_signals[-signo-1];
  else
    return signo;
}

static int rev_convert_signal_number(int signo)
{
  int i;
  for (i = 0; i < sizeof(posix_signals) / sizeof(int); i++)
    if (signo == posix_signals[i]) return -i - 1;
  return signo;
}

#ifndef NSIG
#define NSIG 32
#endif

value install_signal_handler(value signal_number, value action) /* ML */
{
  CAMLparam2 (signal_number, action);
  int sig;
  void (*act)(int signo), (*oldact)(int signo);
#ifdef POSIX_SIGNALS
  struct sigaction sigact, oldsigact;
#endif
  CAMLlocal1 (res);

  sig = convert_signal_number(Int_val(signal_number));
  if (sig < 0 || sig >= NSIG) 
    invalid_argument("Sys.signal: unavailable signal");
  switch(action) {
  case Val_int(0):              /* Signal_default */
    act = SIG_DFL;
    break;
  case Val_int(1):              /* Signal_ignore */
    act = SIG_IGN;
    break;
  default:                      /* Signal_handle */
    act = handle_signal;
    break;
  }
#ifdef POSIX_SIGNALS
  sigact.sa_handler = act;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  if (sigaction(sig, &sigact, &oldsigact) == -1) sys_error(NO_ARG);
  oldact = oldsigact.sa_handler;
#else
  oldact = signal(sig, act);
  if (oldact == SIG_ERR) sys_error(NO_ARG);
#endif
  if (oldact == (void (*)(int)) handle_signal) {
    res = alloc_small(1, 0);          /* Signal_handle */
    Field(res, 0) = Field(signal_handlers, sig);
  }
  else if (oldact == SIG_IGN)
    res = Val_int(1);           /* Signal_ignore */
  else
    res = Val_int(0);           /* Signal_default */
  if (Is_block(action)) {
    if (signal_handlers == 0) {
      signal_handlers = alloc(NSIG, 0);
      register_global_root(&signal_handlers);
    }
    modify(&Field(signal_handlers, sig), Field(action, 0));
  }
  CAMLreturn (res);
}

/* Machine- and OS-dependent handling of bound check trap */

#if defined(TARGET_sparc) && defined(SYS_sunos)
static void trap_handler(int sig, int code, 
                         struct sigcontext * context, char * address)
{
  int * sp;
  /* Unblock SIGILL */
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGILL);
  sigprocmask(SIG_UNBLOCK, &mask, NULL);
  if (code != ILL_TRAP_FAULT(5)) {
    fprintf(stderr, "Fatal error: illegal instruction, code 0x%x\n", code);
    exit(100);
  }
  /* Recover young_ptr and caml_exception_pointer from the %l5 and %l6 regs */
  sp = (int *) context->sc_sp;
  caml_exception_pointer = (char *) sp[5];
  young_ptr = (char *) sp[6];
  array_bound_error();
}
#endif

#if defined(TARGET_sparc) && defined(SYS_solaris)
static void trap_handler(int sig, siginfo_t * info, void * arg)
{
  ucontext_t * context;
  int * sp;

  if (info->si_code != ILL_ILLTRP) {
    fprintf(stderr, "Fatal error: illegal instruction, code 0x%x\n",
            info->si_code);
    exit(100);
  }
  /* Recover young_ptr and caml_exception_pointer from the %l5 and %l6 regs */
  context = (ucontext_t *) arg;
  sp = (int *) context->uc_mcontext.gregs[REG_SP];
  caml_exception_pointer = (char *) sp[5];
  young_ptr = (char *) sp[6];
  array_bound_error();
}
#endif

#if defined(TARGET_sparc) && (defined(SYS_bsd) || defined(SYS_linux))
static void trap_handler(int sig)
{
  /* TODO: recover registers from context and call array_bound_error */
  fatal_error("Fatal error: out-of-bound access in array or string\n");
}
#endif

#if defined(TARGET_power) && defined(SYS_aix)
static void trap_handler(int sig, int code, STRUCT_SIGCONTEXT * context)
{
  /* Unblock SIGTRAP */
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGTRAP);
  sigprocmask(SIG_UNBLOCK, &mask, NULL);
  /* Recover young_ptr and caml_exception_pointer from registers 31 and 29 */
  caml_exception_pointer = (char *) CONTEXT_GPR(context, 29);
  young_ptr = (char *) CONTEXT_GPR(context, 31);
  array_bound_error();
}
#endif

#if defined(TARGET_power) && defined(SYS_elf)
static void trap_handler(int sig, struct sigcontext * context)
{
  /* Recover young_ptr and caml_exception_pointer from registers 31 and 29 */
  caml_exception_pointer = (char *) context->regs->gpr[29];
  young_ptr = (char *) context->regs->gpr[31];
  array_bound_error();
}
#endif

#if defined(TARGET_power) && defined(SYS_rhapsody)
static void trap_handler(int sig, int code, struct sigcontext * context)
{
  /* Unblock SIGTRAP */
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGTRAP);
  sigprocmask(SIG_UNBLOCK, &mask, NULL);
  /* Recover young_ptr and caml_exception_pointer from registers 31 and 29 */
  caml_exception_pointer = (char *) CONTEXT_GPR(context, 29);
  young_ptr = (char *) CONTEXT_GPR(context, 31);
  array_bound_error();
}
#endif

/* Initialization of signal stuff */

void init_signals(void)
{
#if defined(TARGET_sparc) && \
      (defined(SYS_sunos) || defined(SYS_bsd) || defined(SYS_linux))
  struct sigaction act;
  act.sa_handler = (void (*)(int)) trap_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  sigaction(SIGILL, &act, NULL);
#endif
#if defined(TARGET_sparc) && defined(SYS_solaris)
  struct sigaction act;
  act.sa_sigaction = trap_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO | SA_NODEFER;
  sigaction(SIGILL, &act, NULL);
#endif
#if defined(TARGET_power)
  struct sigaction act;
  act.sa_handler = (void (*)(int)) trap_handler;
  sigemptyset(&act.sa_mask);
#if defined(SYS_rhapsody) || defined(SYS_aix)
  act.sa_flags = 0;
#else
  act.sa_flags = SA_NODEFER;
#endif
  sigaction(SIGTRAP, &act, NULL);
#endif
}
