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

/* structure of the stacks */

#ifndef _stacks_
#define _stacks_


#include "misc.h"
#include "mlvalues.h"
#include "memory.h"

CAMLextern value * stack_low;
CAMLextern value * stack_high;
CAMLextern value * stack_threshold;
CAMLextern value * extern_sp;
CAMLextern value * trapsp;
CAMLextern value * trap_barrier;

#define Trap_pc(tp) (((code_t *)(tp))[0])
#define Trap_link(tp) (((value **)(tp))[1])

void reset_roots (void);
void init_stack (unsigned long init_max_size);
void realloc_stack (asize_t required_size);
void change_max_stack_size (unsigned long new_max_size);


#endif /* _stacks_ */

