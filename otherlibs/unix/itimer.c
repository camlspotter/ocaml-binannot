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
#include <alloc.h>
#include <memory.h>
#include "unixsupport.h"

#ifdef HAS_SETITIMER

#include <sys/time.h>

#define Get_timeval(tv) \
  (double) tv.tv_sec + (double) tv.tv_usec / 1e6
#define Set_timeval(tv, d) \
  tv.tv_sec = (int)(d), \
  tv.tv_usec = (int) (1e6 * ((d) - tv.tv_sec))

static value unix_convert_itimer(struct itimerval *tp)
{
  value res;
  value interval = Val_unit, v = Val_unit;

  Begin_roots2(interval, v);
    interval = copy_double(Get_timeval(tp->it_interval));
    v = copy_double(Get_timeval(tp->it_value));
    res = alloc_tuple(2);
    Field(res, 0) = interval;
    Field(res, 1) = v;
  End_roots();
  return res;
}

static value unix_convert_itimer_native(struct itimerval *tp)
{
  value res = alloc(Double_wosize * 2, Double_array_tag);
  Store_double_field(res, 0, Get_timeval(tp->it_interval));
  Store_double_field(res, 1, Get_timeval(tp->it_value));
  return res;
}

static int itimers[3] = { ITIMER_REAL, ITIMER_VIRTUAL, ITIMER_PROF };

value unix_setitimer(value which, value newval)
{
  struct itimerval new, old;
  Set_timeval(new.it_interval, Double_val(Field(newval, 0)));
  Set_timeval(new.it_value, Double_val(Field(newval, 1)));
  if (setitimer(itimers[Int_val(which)], &new, &old) == -1)
    uerror("setitimer", Nothing);
  return unix_convert_itimer(&old);
}
     
value unix_setitimer_native(value which, value newval)
{
  struct itimerval new, old;
  Set_timeval(new.it_interval, Double_field(newval, 0));
  Set_timeval(new.it_value, Double_field(newval, 1));
  if (setitimer(itimers[Int_val(which)], &new, &old) == -1)
    uerror("setitimer", Nothing);
  return unix_convert_itimer_native(&old);
}
     
value unix_getitimer(value which)
{
  struct itimerval val;
  if (getitimer(itimers[Int_val(which)], &val) == -1)
    uerror("getitimer", Nothing);
  return unix_convert_itimer(&val);
}

value unix_getitimer_native(value which)
{
  struct itimerval val;
  if (getitimer(itimers[Int_val(which)], &val) == -1)
    uerror("getitimer", Nothing);
  return unix_convert_itimer_native(&val);
}

#else

value unix_setitimer(value which, value newval)
{ invalid_argument("setitimer not implemented"); }
value unix_getitimer(value which)
{ invalid_argument("getitimer not implemented"); }
value unix_setitimer_native(value which, value newval)
{ invalid_argument("setitimer not implemented"); }
value unix_getitimer_native(value which)
{ invalid_argument("getitimer not implemented"); }

#endif
