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

/* Primitives for the toplevel */

#include "alloc.h"
#include "config.h"
#include "fail.h"
#include "fix_code.h"
#include "interp.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"
#include "stacks.h"

#ifndef NATIVE_CODE

value get_global_data(value unit)     /* ML */
{
  return global_data;
}

value reify_bytecode(value prog, value len) /* ML */
{
  value clos;
#ifdef ARCH_BIG_ENDIAN
  fixup_endianness((code_t) prog, (asize_t) Long_val(len));
#endif
#ifdef THREADED_CODE
  thread_code((code_t) prog, (asize_t) Long_val(len));
#endif
  clos = alloc(1, Closure_tag);
  Code_val(clos) = (code_t) prog;
  return clos;
}

value realloc_global(value size)      /* ML */
{
  mlsize_t requested_size, actual_size, i;
  value new_global_data;

  requested_size = Long_val(size);
  actual_size = Wosize_val(global_data);
  if (requested_size >= actual_size) {
    requested_size = (requested_size + 0x100) & 0xFFFFFF00;
    gc_message (0x08, "Growing global data to %lu entries\n", requested_size);
    new_global_data = alloc_shr(requested_size, 0);
    for (i = 0; i < actual_size; i++)
      initialize(&Field(new_global_data, i), Field(global_data, i));
    for (i = actual_size; i < requested_size; i++){
      Field (new_global_data, i) = Val_long (0);
    }
    global_data = new_global_data;
  }
  return Val_unit;
}
    
value available_primitives(value unit)    /* ML */
{
  return copy_string_array(names_of_cprim);
}

value get_current_environment(value unit) /* ML */
{
  return *extern_sp;
}

#else

/* Dummy definitions to support compilation of ocamlc.opt */

value get_global_data(value unit)
{
  invalid_argument("Meta.get_global_data");
}

value realloc_global(value size)
{
  invalid_argument("Meta.realloc_global");
}
    
value available_primitives(value unit)
{
  invalid_argument("Meta.available_primitives");
}

#endif
