/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Free lists of heap blocks. */

#ifndef CAML_FREELIST_H
#define CAML_FREELIST_H


#include "misc.h"
#include "mlvalues.h"

extern asize_t fl_cur_size;     /* size in words */

char *fl_allocate (mlsize_t);
void fl_init_merge (void);
void fl_reset (void);
char *fl_merge_block (char *);
void fl_add_block (char *);
void make_free_blocks (value *, mlsize_t, int);


#endif /* CAML_FREELIST_H */
