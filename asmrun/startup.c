/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Start-up code */

#include <stdio.h>
#include <stdlib.h>
#include "fail.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "misc.h"
#include "mlvalues.h"
#include "sys.h"
#ifdef HAS_UI
#include "ui.h"
#endif

header_t atom_table[256];
char * static_data_start, * static_data_end;

/* Initialize the atom table */

static void init_atoms()
{
  int i;
  extern struct { char * begin; char * end; } caml_data_segments[];

  for (i = 0; i < 256; i++) atom_table[i] = Make_header(0, i, White);
  static_data_start = caml_data_segments[0].begin;
  static_data_end = caml_data_segments[0].end;
  for (i = 1; caml_data_segments[i].begin != 0; i++) {
    if (caml_data_segments[i].begin < static_data_start)
      static_data_start = caml_data_segments[i].begin;
    if (caml_data_segments[i].end > static_data_end)
      static_data_end = caml_data_segments[i].end;
  }
}

/* Configuration parameters and flags */

static int verbose_init = 0;
static int percent_free_init = Percent_free_def;
static int max_percent_free_init = Max_percent_free_def;
static unsigned long minor_heap_init = Minor_heap_def;
static unsigned long heap_chunk_init = Heap_chunk_def;
static unsigned long heap_size_init = Init_heap_def;
static unsigned long max_stack_init = Max_stack_def;

/* Parse the CAMLRUNPARAM variable */
/* The option letter for each runtime option is the first letter of the
   last word of the ML name of the option (see [stdlib/gc.mli]).
   Except for l (maximum stack size) and h (initial heap size).
*/
/* Note: option l is irrelevant to the native-code runtime. */

static void scanmult (opt, var)
     char *opt;
     unsigned long *var;
{
  char mult = ' ';
  sscanf (opt, "=%lu%c", var, &mult);
  if (mult == 'k') *var = *var * 1024;
  if (mult == 'M') *var = *var * (1024 * 1024);
  if (mult == 'G') *var = *var * (1024 * 1024 * 1024);
}

static void parse_camlrunparam()
{
  char *opt = getenv ("CAMLRUNPARAM");
  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 's':	scanmult (opt, &minor_heap_init); break;
      case 'i': scanmult (opt, &heap_chunk_init); break;
      case 'h': scanmult (opt, &heap_size_init); break;
      case 'l': scanmult (opt, &max_stack_init); break;
      case 'o': sscanf (opt, "=%d", &percent_free_init); break;
      case 'O': sscanf (opt, "=%d", &max_percent_free_init); break;
      case 'v': sscanf (opt, "=%d", &verbose_init); break;
      }
    }
  }
}

extern void caml_start_program P((void));
extern void init_ieee_floats P((void));
extern void init_signals P((void));

void caml_main(argv)
     char ** argv;
{
  init_ieee_floats();
#ifdef DEBUG
  verbose_init = 1;
#endif
  parse_camlrunparam();
  init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
	   percent_free_init, max_percent_free_init, verbose_init);
  init_atoms();
  init_signals();
  sys_init(argv);
  caml_start_program();
}

void caml_startup(argv)
     char ** argv;
{
  caml_main(argv);
}
