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
#include <fcntl.h>

#define SIZEBUF 1024

value unix_pipe(value unit)                /* ML */
{
  SECURITY_ATTRIBUTES attr;
  HANDLE readh, writeh;
  value readfd = Val_unit, writefd = Val_unit, res;

  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = TRUE;
  if (! CreatePipe(&readh, &writeh, &attr, SIZEBUF)) {
    _dosmaperr(GetLastError());
    uerror("pipe", Nothing);
  }
  Begin_roots2(readfd, writefd)
    readfd = win_alloc_handle(readh);
    writefd = win_alloc_handle(writeh);
    res = alloc_tuple(2);
    Field(res, 0) = readfd;
    Field(res, 1) = writefd;
  End_roots();
  return res;
}
