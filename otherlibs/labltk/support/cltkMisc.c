/*************************************************************************/
/*                                                                       */
/*                Objective Caml LablTk library                          */
/*                                                                       */
/*         Francois Rouaix, Francois Pessaux and Jun Furuse              */
/*               projet Cristal, INRIA Rocquencourt                      */
/*            Jacques Garrigue, Kyoto University RIMS                    */
/*                                                                       */
/*   Copyright 1999 Institut National de Recherche en Informatique et    */
/*   en Automatique and Kyoto University.  All rights reserved.          */
/*   This file is distributed under the terms of the GNU Library         */
/*   General Public License.                                             */
/*                                                                       */
/*************************************************************************/

/* $Id$ */

#include <string.h>
#include <tcl.h>
#include <tk.h>
#include <mlvalues.h>
#include <memory.h>
#include "camltk.h"

/* Parsing results */
value camltk_splitlist (value v) /* ML */
{
  int argc;
  char **argv;
  int result;

  CheckInit();

  /* argv is allocated by Tcl, to be freed by us */
  result = Tcl_SplitList(cltclinterp,String_val(v),&argc,&argv);
  switch(result) {
  case TCL_OK:
   { value res = copy_string_list(argc,argv);
     free((char *)argv);        /* only one large block was allocated */
     return res;
   }
  case TCL_ERROR:
  default:
    tk_error(cltclinterp->result);
  }
}

/* Copy a Caml string to the C heap. Should deallocate with stat_free */
char *string_to_c(value s)
{
  int l = string_length(s);
  char *res = stat_alloc(l + 1);
  memmove (res, String_val (s), l);
  res[l] = '\0';
  return res;
}


