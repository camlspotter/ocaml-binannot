/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Manuel Serrano and Xavier Leroy, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <string.h>

#include "alloc.h"
#include "custom.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"

CAMLextern value alloc_custom(struct custom_operations * ops,
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

CAMLextern int custom_compare_default(value v1, value v2)
{
  failwith("equal: abstract value");
  return 0;
}

CAMLextern void custom_serialize_default(value v, unsigned long * wsize_32,
                                         unsigned long * wsize_64)
{
  failwith("output_value: abstract value");
}

struct custom_operations_list {
  struct custom_operations * ops;
  struct custom_operations_list * next;
};

static struct custom_operations_list * custom_ops_table = NULL;

CAMLextern void register_custom_operations(struct custom_operations * ops)
{
  struct custom_operations_list * l =
    stat_alloc(sizeof(struct custom_operations_list));
  Assert(ops->identifier != NULL);
  Assert(ops->deserialize != NULL);
  l->ops = ops;
  l->next = custom_ops_table;
  custom_ops_table = l;
}

struct custom_operations * find_custom_operations(char * ident)
{
  struct custom_operations_list * l;
  for (l = custom_ops_table; l != NULL; l = l->next)
    if (strcmp(l->ops->identifier, ident) == 0) return l->ops;
  return NULL;
}

static struct custom_operations_list * custom_ops_final_table = NULL;

struct custom_operations * final_custom_operations(final_fun fn)
{
  struct custom_operations_list * l;
  struct custom_operations * ops;
  for (l = custom_ops_final_table; l != NULL; l = l->next)
    if (l->ops->finalize == fn) return l->ops;
  ops = stat_alloc(sizeof(struct custom_operations));
  ops->identifier = "_final";
  ops->finalize = fn;
  ops->compare = custom_compare_default;
  ops->hash = custom_hash_default;
  ops->serialize = custom_serialize_default;
  ops->deserialize = custom_deserialize_default;
  l = stat_alloc(sizeof(struct custom_operations_list));
  l->ops = ops;
  l->next = custom_ops_final_table;
  custom_ops_final_table = l;
  return ops;
}

extern struct custom_operations int32_ops, nativeint_ops;
#ifdef ARCH_INT64_TYPE
extern struct custom_operations int64_ops;
#endif

void init_custom_operations(void)
{
  register_custom_operations(&int32_ops);
  register_custom_operations(&nativeint_ops);
#ifdef ARCH_INT64_TYPE
  register_custom_operations(&int64_ops);
#endif
}
