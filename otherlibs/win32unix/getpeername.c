/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"
#include "socketaddr.h"

value unix_getpeername(sock)          /* ML */
     value sock;
{
  int retcode;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  addr_len = sizeof(sock_addr);
  retcode = getpeername((SOCKET) Handle_val(sock),
                        &addr.s_gen, &addr_len);
  if (retcode == -1) unix_error(WSAGetLastError(), "getpeername", Nothing);
  return alloc_sockaddr(&addr, addr_len);
}
