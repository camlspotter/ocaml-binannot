/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*                 File contributed by Lionel Fourquaux                */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <windows.h>
#include <mlvalues.h>
#include <fail.h>
#include "unixsupport.h"
  
typedef  
BOOL (WINAPI *tCreateHardLink)(
  LPCTSTR lpFileName,
  LPCTSTR lpExistingFileName,
  LPSECURITY_ATTRIBUTES lpSecurityAttributes  
);
  
value unix_link(value path1, value path2)    /* ML */
{ 
  HMODULE hModKernel32;
  tCreateHardLink pCreateHardLink; 
  hModKernel32 = GetModuleHandle("KERNEL32.DLL");
  pCreateHardLink =
    (tCreateHardLink) GetProcAddress(hModKernel32, "CreateHardLinkA");
  if (pCreateHardLink == NULL)
    invalid_argument("Unix.link not implemented");
  if (! pCreateHardLink(String_val(path2), String_val(path1), NULL)) {
    _dosmaperr(GetLastError());
    uerror("link", path2);
  }
  return Val_unit;
} 
