/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
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
#include <signals.h>
#include "unixsupport.h"

#ifdef HAS_SELECT

#include <sys/types.h>
#include <sys/time.h>
#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>

#endif

#ifdef __OpenBSD__
#include <string.h>
#endif

#ifdef FD_ISSET
typedef fd_set file_descr_set;
#else
typedef int file_descr_set;
#define FD_SETSIZE (sizeof(int) * 8)
#define FD_SET(fd,fds) (*(fds) |= 1 << (fd))
#define FD_CLR(fd,fds) (*(fds) &= ~(1 << (fd)))
#define FD_ISSET(fd,fds) (*(fds) & (1 << (fd)))
#define FD_ZERO(fds) (*(fds) = 0)
#endif

static void fdlist_to_fdset(value fdlist, file_descr_set *fdset)
{
  value l;
  FD_ZERO(fdset);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    FD_SET(Int_val(Field(l, 0)), fdset);
  }
}

static value fdset_to_fdlist(file_descr_set *fdset)
{
  int i;
  value res = Val_int(0);

  Begin_root(res);
    for (i = FD_SETSIZE - 1; i >= 0; i--) {
      if (FD_ISSET(i, fdset)) {
	value newres = alloc_small(2, 0);
	Field(newres, 0) = Val_int(i);
	Field(newres, 1) = res;
	res = newres;
      }
    }
  End_roots();
  return res;
}

value unix_select(value readfds, value writefds, value exceptfds, value timeout) /* ML */
{
  file_descr_set read, write, except;
  double tm;
  struct timeval tv;
  struct timeval * tvp;
  int retcode;
  value res;
  value read_list = Val_unit, write_list = Val_unit, except_list = Val_unit;

  Begin_roots3 (read_list, write_list, except_list);
    fdlist_to_fdset(readfds, &read);
    fdlist_to_fdset(writefds, &write);
    fdlist_to_fdset(exceptfds, &except);
    tm = Double_val(timeout);
    if (tm < 0.0)
      tvp = (struct timeval *) NULL;
    else {
      tv.tv_sec = (int) tm;
      tv.tv_usec = (int) (1e6 * (tm - (int) tm));
      tvp = &tv;
    }
    enter_blocking_section();
    retcode = select(FD_SETSIZE, &read, &write, &except, tvp);
    leave_blocking_section();
    if (retcode == -1) uerror("select", Nothing);
    read_list = fdset_to_fdlist(&read);
    write_list = fdset_to_fdlist(&write);
    except_list = fdset_to_fdlist(&except);
    res = alloc_small(3, 0);
    Field(res, 0) = read_list;
    Field(res, 1) = write_list;
    Field(res, 2) = except_list;
  End_roots();
  return res;
}

#else

value unix_select(value readfds, value writefds, value exceptfds, value timeout)
{ invalid_argument("select not implemented"); }

#endif
