/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* 1. Allocation functions doing the same work as the macros in the
      case where [Setup_for_gc] and [Restore_after_gc] are no-ops.
   2. Convenience functions related to allocation.
*/

#include <string.h>
#include "alloc.h"
#include "custom.h"
#include "major_gc.h"
#include "memory.h"
#include "mlvalues.h"
#include "stacks.h"

#define Setup_for_gc
#define Restore_after_gc

value alloc (mlsize_t wosize, tag_t tag)
{
  value result;
  mlsize_t i;

  Assert (wosize > 0);
  if (wosize <= Max_young_wosize){
    Alloc_small (result, wosize, tag);
    if (tag < No_scan_tag){
      for (i = 0; i < wosize; i++) Field (result, i) = 0;
    }
  }else{
    result = alloc_shr (wosize, tag);
    if (tag < No_scan_tag) memset (Bp_val (result), 0, Bsize_wsize (wosize));
    result = check_urgent_gc (result);
  }
  return result;
}

value alloc_small (mlsize_t wosize, tag_t tag)
{
  value result;

  Assert (wosize > 0 && wosize <= Max_young_wosize);
  Alloc_small (result, wosize, tag);
  return result;
}

value alloc_tuple(mlsize_t n)
{
  return alloc(n, 0);
}

value alloc_string (mlsize_t len)
{
  value result;
  mlsize_t offset_index;
  mlsize_t wosize = (len + sizeof (value)) / sizeof (value);

  if (wosize <= Max_young_wosize) {
    Alloc_small (result, wosize, String_tag);
  }else{
    result = alloc_shr (wosize, String_tag);
    result = check_urgent_gc (result);
  }
  Field (result, wosize - 1) = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  Byte (result, offset_index) = offset_index - len;
  return result;
}

value alloc_custom(struct custom_operations * ops,
                   unsigned long size,
                   mlsize_t mem,
                   mlsize_t max)
{
  mlsize_t wosize;
  value result;

  wosize = 1 + (size + sizeof(value) - 1) / sizeof(value);
  if (ops->finalize == NULL && wosize <= Max_young_wosize) {
    result = alloc_small(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
  } else {
    result = alloc_shr(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
    adjust_gc_speed(mem, max);
    result = check_urgent_gc(result);
  }
  return result;
}

value alloc_final (mlsize_t len, final_fun fun, mlsize_t mem, mlsize_t max)
{
  return alloc_custom(final_custom_operations(fun),
                      len * sizeof(value), mem, max);
}

value copy_string(char *s)
{
  int len;
  value res;

  len = strlen(s);
  res = alloc_string(len);
  bcopy(s, String_val(res), len);
  return res;
}

value alloc_array(value (*funct)(char *), char ** arr)
{
  CAMLparam0 ();
  mlsize_t nbr, n;
  CAMLlocal2 (v, result);

  nbr = 0;
  while (arr[nbr] != 0) nbr++;
  if (nbr == 0) {
    CAMLreturn (Atom(0));
  } else {
    result = alloc (nbr, 0);
    for (n = 0; n < nbr; n++) {
      /* The two statements below must be separate because of evaluation
         order (don't take the address &Field(result, n) before
         calling funct, which may cause a GC and move result). */
      v = funct(arr[n]);
      modify(&Field(result, n), v);
    }
    CAMLreturn (result);
  }
}

value copy_string_array(char **arr)
{
  return alloc_array(copy_string, arr);
}

int convert_flag_list(value list, int *flags)
{
  int res;
  res = 0;
  while (list != Val_int(0)) {
    res |= flags[Int_val(Field(list, 0))];
    list = Field(list, 1);
  }
  return res;
}

/* For compiling let rec over values */

value alloc_dummy(value size) /* ML */
{
  mlsize_t wosize = Int_val(size);

  if (wosize == 0) return Atom(0);
  return alloc (wosize, 0);
}

value update_dummy(value dummy, value newval) /* ML */
{
  mlsize_t size, i;
  size = Wosize_val(newval);
  Assert (size == Wosize_val(dummy));
  Tag_val(dummy) = Tag_val(newval);
  for (i = 0; i < size; i++)
    modify(&Field(dummy, i), Field(newval, i));
  return Val_unit;
}
