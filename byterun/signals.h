/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _signals_
#define _signals_

#include "misc.h"
#include "mlvalues.h"

extern value signal_handlers;
extern volatile int pending_signal;
extern volatile int something_to_do;
extern volatile int force_major_slice;
extern volatile int async_signal_mode;

void enter_blocking_section (void);
void leave_blocking_section (void);
void urge_major_slice (void);
int convert_signal_number (int);
void execute_signal(int signal_number, int in_signal_handler);

extern void (*enter_blocking_section_hook)();
extern void (*leave_blocking_section_hook)();

#endif /* _signals_ */

