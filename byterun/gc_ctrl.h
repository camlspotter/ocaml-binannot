/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _gc_ctrl_
#define _gc_ctrl_

#include "misc.h"

extern long
     stat_minor_words,
     stat_promoted_words,
     stat_major_words,
     stat_minor_collections,
     stat_major_collections,
     stat_heap_size,
     stat_compactions;

void init_gc (unsigned long, unsigned long, unsigned long,
              unsigned long, unsigned long, unsigned long);


#ifdef DEBUG
void heap_check (void);
#endif

#endif /* _gc_ctrl_ */
