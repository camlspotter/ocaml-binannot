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
#include "unixsupport.h"

value unix_chown(value path, value uid, value gid) /* ML */
{
  int ret;
  ret = chown(String_val(path), Int_val(uid), Int_val(gid));
  if (ret == -1) uerror("chown", path);
  return Val_unit;
}
