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

#include <string.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <signals.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS
#include "socketaddr.h"
#endif

#if defined(HAS_SOCKETS) && defined(MSG_OOB) && defined(MSG_DONTROUTE) && defined(MSG_PEEK)

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

value unix_recv(value sock, value buff, value ofs, value len, value flags) /* ML */
{
  int ret;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];

  Begin_root (buff);
    numbytes = Long_val(len);
    if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
    enter_blocking_section();
    ret = recv(Int_val(sock), iobuf, (int) numbytes,
	       convert_flag_list(flags, msg_flag_table));
    leave_blocking_section();
    if (ret == -1) uerror("recv", Nothing);
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

  Begin_roots2 (buff, adr);
    numbytes = Long_val(len);
    if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
    sock_addr_len = sizeof(sock_addr);
    enter_blocking_section();
    ret = recvfrom(Int_val(sock), iobuf, (int) numbytes,
		   convert_flag_list(flags, msg_flag_table),
		   &sock_addr.s_gen, &sock_addr_len);
    leave_blocking_section();
    if (ret == -1) uerror("recvfrom", Nothing);
    bcopy(iobuf, &Byte(buff, Long_val(ofs)), ret);
    adr = alloc_sockaddr();
    res = alloc_tuple(2);
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
  ret = send(Int_val(sock), iobuf, (int) numbytes,
             convert_flag_list(flags, msg_flag_table));
  leave_blocking_section();
  if (ret == -1) uerror("send", Nothing);
  return Val_int(ret);
}

value unix_sendto_native(value sock, value buff, value ofs, value len, value flags, value dest)
{
  int ret;
  long numbytes;
  char iobuf[UNIX_BUFFER_SIZE];

  get_sockaddr(dest);
  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  bcopy(&Byte(buff, Long_val(ofs)), iobuf, numbytes);
  enter_blocking_section();
  ret = sendto(Int_val(sock), iobuf, (int) numbytes,
               convert_flag_list(flags, msg_flag_table),
               &sock_addr.s_gen, sock_addr_len);
  leave_blocking_section();
  if (ret == -1) uerror("sendto", Nothing);
  return Val_int(ret);
}

value unix_sendto(value *argv, int argc)    /* ML */
{
  return unix_sendto_native
           (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

#else

value unix_recv(value sock, value buff, value ofs, value len, value flags)
{ invalid_argument("recv not implemented"); }

value unix_recvfrom(value sock, value buff, value ofs, value len, value flags)
{ invalid_argument("recvfrom not implemented"); }

value unix_send(value sock, value buff, value ofs, value len, value flags)
{ invalid_argument("send not implemented"); }

value unix_sendto_native(value sock, value buff, value ofs, value len, value flags, value dest)
{ invalid_argument("sendto not implemented"); }

value unix_sendto(value *argv, int argc)
{ invalid_argument("sendto not implemented"); }

#endif
