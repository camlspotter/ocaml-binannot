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
#include <alloc.h>
#include <memory.h>
#include "unixsupport.h"
#include "socketaddr.h"

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

value unix_recv(value sock, value buff, value ofs, value len, value flags)
{
  int ret;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];

  Begin_root (buff);
    numbytes = Long_val(len);
    if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
    enter_blocking_section();
    ret = recv((SOCKET) Handle_val(sock), iobuf, (int) numbytes,
               convert_flag_list(flags, msg_flag_table));
    leave_blocking_section();
    if (ret == -1) unix_error(WSAGetLastError(), "recv", Nothing);
    bcopy(iobuf, &Byte(buff, Long_val(ofs)), ret);
  End_roots();
  return Val_int(ret);
}

value unix_recvfrom(value sock, value buff, value ofs, value len, value flags) /* ML */
{
  int ret;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  value res;
  value adr = Val_unit;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  Begin_roots2 (buff, adr);
    numbytes = Long_val(len);
    if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
    addr_len = sizeof(sock_addr);
    enter_blocking_section();
    ret = recvfrom((SOCKET) Handle_val(sock),
                   iobuf, (int) numbytes,
                   convert_flag_list(flags, msg_flag_table),
                   &addr.s_gen, &addr_len);
    leave_blocking_section();
    if (ret == -1) unix_error(WSAGetLastError(), "recvfrom", Nothing);
    bcopy(iobuf, &Byte(buff, Long_val(ofs)), ret);
    adr = alloc_sockaddr(&addr, addr_len);
    res = alloc_small(2, 0);
    Field(res, 0) = Val_int(ret);
    Field(res, 1) = adr;
  End_roots();
  return res;
}

value unix_send(value sock, value buff, value ofs, value len, value flags) /* ML */
{
  int ret;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];

  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  bcopy(&Byte(buff, Long_val(ofs)), iobuf, numbytes);
  enter_blocking_section();
  ret = send((SOCKET) Handle_val(sock), iobuf, (int) numbytes,
             convert_flag_list(flags, msg_flag_table));
  leave_blocking_section();
  if (ret == -1) unix_error(WSAGetLastError(), "send", Nothing);
  return Val_int(ret);
}

value unix_sendto_native(value sock, value buff, value ofs, value len, value flags, value dest)
{
  int ret;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  union sock_addr_union addr;
  socklen_param_type addr_len;

  get_sockaddr(dest, &addr, &addr_len);
  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  bcopy(&Byte(buff, Long_val(ofs)), iobuf, numbytes);
  enter_blocking_section();
  ret = sendto((SOCKET) Handle_val(sock),
               iobuf, (int) numbytes,
               convert_flag_list(flags, msg_flag_table),
               &addr.s_gen, addr_len);
  leave_blocking_section();
  if (ret == -1) unix_error(WSAGetLastError(), "sendto", Nothing);
  return Val_int(ret);
}

value unix_sendto(argv, argc)    /* ML */
     value * argv;
     int argc;
{
  return unix_sendto_native
           (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}
