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
extern int volatile pending_signal;
extern int volatile something_to_do;
extern int volatile force_major_slice;
extern int volatile async_signal_mode;

void enter_blocking_section (void);
void leave_blocking_section (void);
void urge_major_slice (void);
int convert_signal_number (int);
void execute_signal(int signal_number, int in_signal_handler);
void process_event(void);

extern void (*enter_blocking_section_hook)(void);
extern void (*leave_blocking_section_hook)(void);
extern void (* volatile async_action_hook)(void);

#endif /* _signals_ */

