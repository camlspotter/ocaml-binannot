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

#include <mlvalues.h>
#include "unixsupport.h"
#include <sys/types.h>
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

value unix_closedir(d)           /* ML */
     value d;
{
  closedir((DIR *) d);
  return Val_unit;
}
