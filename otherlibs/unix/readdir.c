/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* This avoids "overflow" errors when reading from very large directories
   (PR#609) */
#define _FILE_OFFSET_BITS 64

#include <mlvalues.h>
#include <fail.h>
#include <alloc.h>
#include "unixsupport.h"
#include <sys/types.h>
#ifdef HAS_DIRENT
#include <dirent.h>
typedef struct dirent directory_entry;
#else
#include <sys/dir.h>
typedef struct direct directory_entry;
#endif

CAMLprim value unix_readdir(value d)
{
  directory_entry * e;

  e = readdir((DIR *) d);
  if (e == (directory_entry *) NULL) raise_end_of_file();
  return copy_string(e->d_name);
}
