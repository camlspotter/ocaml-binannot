/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _signals_
#define _signals_

#include "misc.h"

extern value signal_handlers;
extern Volatile int pending_signal;
extern Volatile int something_to_do;
extern Volatile int force_major_slice;
extern Volatile int async_signal_mode;

void enter_blocking_section P((void));
void leave_blocking_section P((void));
void urge_major_slice P((void));

extern void (*enter_blocking_section_hook)();
extern void (*leave_blocking_section_hook)();

#endif /* _signals_ */

