/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Operations on strings */

#include <string.h>
#include <ctype.h>
#include "alloc.h"
#include "fail.h"
#include "mlvalues.h"
#include "misc.h"
#ifdef HAS_LOCALE
#include <locale.h>
#endif

CAMLexport mlsize_t string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return temp - Byte (s, temp);
}

CAMLprim value ml_string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return Val_long(temp - Byte (s, temp));
}

CAMLprim value create_string(value len)
{
  mlsize_t size = Long_val(len);
  if (size > Bsize_wsize (Max_wosize) - 1) invalid_argument("String.create");
  return alloc_string(size);
}

CAMLprim value string_get(value str, value index)
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= string_length(str)) invalid_argument("String.get");
  return Val_int(Byte_u(str, idx));
}

CAMLprim value string_set(value str, value index, value newval)
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= string_length(str)) invalid_argument("String.set");
  Byte_u(str, idx) = Int_val(newval);
  return Val_unit;
}

CAMLprim value string_equal(value s1, value s2)
{
  mlsize_t sz1 = Wosize_val(s1);
  mlsize_t sz2 = Wosize_val(s2);
  value * p1, * p2;
  if (sz1 != sz2) return Val_false;
  for(p1 = Op_val(s1), p2 = Op_val(s2); sz1 > 0; sz1--, p1++, p2++)
    if (*p1 != *p2) return Val_false;
  return Val_true;
}

CAMLprim value string_notequal(value s1, value s2)
{
  return Val_not(string_equal(s1, s2));
}

CAMLprim value string_compare(value s1, value s2)
{
  mlsize_t len1, len2, len;
  int res;

  len1 = string_length(s1);
  len2 = string_length(s2); 
  res = memcmp(String_val(s1), String_val(s2), len1 <= len2 ? len1 : len2);
  if (res < 0) return Val_int(-1);
  if (res > 0) return Val_int(1);
  if (len1 < len2) return Val_int(-1);
  if (len1 > len2) return Val_int(1);
  return Val_int(0);
}

CAMLprim value string_lessthan(value s1, value s2)
{
  return string_compare(s1, s2) < Val_int(0) ? Val_true : Val_false;
}
  
CAMLprim value string_lessequal(value s1, value s2)
{
  return string_compare(s1, s2) <= Val_int(0) ? Val_true : Val_false;
}
  
CAMLprim value string_greaterthan(value s1, value s2)
{
  return string_compare(s1, s2) > Val_int(0) ? Val_true : Val_false;
}
  
CAMLprim value string_greaterequal(value s1, value s2)
{
  return string_compare(s1, s2) >= Val_int(0) ? Val_true : Val_false;
}
  
CAMLprim value blit_string(value s1, value ofs1, value s2, value ofs2, value n)
{
  memmove(&Byte(s2, Long_val(ofs2)), &Byte(s1, Long_val(ofs1)), Int_val(n));
  return Val_unit;
}

CAMLprim value fill_string(value s, value offset, value len, value init)
{
  memset(&Byte(s, Long_val(offset)), Int_val(init), Long_val(len));
  return Val_unit;
}

CAMLprim value is_printable(value chr)
{
  int c;

#ifdef HAS_LOCALE
  static int locale_is_set = 0;
  if (! locale_is_set) {
    setlocale(LC_CTYPE, "");
    locale_is_set = 1;
  }
#endif
  c = Int_val(chr);
  return Val_bool(isprint(c));
}

CAMLprim value bitvect_test(value bv, value n)
{
  int pos = Int_val(n);
  return Val_int(Byte_u(bv, pos >> 3) & (1 << (pos & 7)));
}

