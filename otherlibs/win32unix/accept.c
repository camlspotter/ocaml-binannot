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
#include <alloc.h>
#include <memory.h>
#include <signals.h>
#include "unixsupport.h"
#include "socketaddr.h"

CAMLprim value unix_accept(sock)
     value sock;
{
  SOCKET sconn = Socket_val(sock);
  SOCKET snew;
  value fd = Val_unit, adr = Val_unit, res;
  int oldvalue, oldvaluelen, newvalue, retcode;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  oldvaluelen = sizeof(oldvalue);
  retcode = getsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
                       (char *) &oldvalue, &oldvaluelen);
  if (retcode == 0) {
    /* Set sockets to synchronous mode */
    newvalue = SO_SYNCHRONOUS_NONALERT;
    setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, 
               (char *) &newvalue, sizeof(newvalue));
  }
  addr_len = sizeof(sock_addr);
  enter_blocking_section();
  snew = accept(sconn, &addr.s_gen, &addr_len);
  leave_blocking_section();
  if (retcode == 0) {
    /* Restore initial mode */
    setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, 
               (char *) &oldvalue, oldvaluelen);
  }
  if (snew == INVALID_SOCKET) {
    win32_maperr(WSAGetLastError());
    uerror("accept", Nothing);
  }
  Begin_roots2 (fd, adr)
    fd = win_alloc_socket(snew);
    adr = alloc_sockaddr(&addr, addr_len);
    res = alloc_small(2, 0);
    Field(res, 0) = fd;
    Field(res, 1) = adr;
  End_roots();
  return res;
}

