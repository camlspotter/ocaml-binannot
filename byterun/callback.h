/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Callbacks from C to Caml */

#ifndef _callback_
#define _callback_

#include "mlvalues.h"

value callback (value closure, value arg);
value callback2 (value closure, value arg1, value arg2);
value callback3 (value closure, value arg1, value arg2, value arg3);
value callbackN (value closure, int narg, value args[]);

value callback_exn (value closure, value arg);
value callback2_exn (value closure, value arg1, value arg2);
value callback3_exn (value closure, value arg1, value arg2, value arg3);
value callbackN_exn (value closure, int narg, value args[]);

#define Make_exception_result(v) ((v) | 2)
#define Is_exception_result(v) (((v) & 3) == 2)
#define Extract_exception(v) ((v) & ~3)
char * format_caml_exception(value exn);

value * caml_named_value (char * name);

void caml_main (char ** argv);
void caml_startup (char ** argv);

extern int callback_depth;

#endif
