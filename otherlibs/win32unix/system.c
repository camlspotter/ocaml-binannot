/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <memory.h>
#include <alloc.h>
#include "unixsupport.h"
#include <process.h>
#include <stdio.h>

value win_system(cmd)
     value cmd;
{
  int ret;
  value st;

  enter_blocking_section();
  _flushall();
  ret = system(String_val(c));;
  leave_blocking_section();
  if (ret == -1) uerror("system", Nothing);
  st = alloc(1, 0); /* 0: Exited */
  Field(st, 0) = Val_int(ret);
  return st;
}



