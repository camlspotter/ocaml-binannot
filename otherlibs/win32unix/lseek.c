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

#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

static int seek_command_table[] = {
  FILE_BEGIN, FILE_CURRENT, FILE_END
};

CAMLprim value unix_lseek(value fd, value ofs, value cmd)
{
  long ret;
  ret = SetFilePointer(Handle_val(fd), Long_val(ofs), NULL,
                       seek_command_table[Int_val(cmd)]);
  if (ret == -1) {
    win32_maperr(GetLastError());
    uerror("lseek", Nothing);
  }
  return Val_long(ret);
}
