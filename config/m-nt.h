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

/* Machine configuration, Intel x86 processors, Win32, Visual C++ compiler */

#undef ARCH_SIXTYFOUR
#undef ARCH_BIG_ENDIAN
#undef ARCH_ALIGN_DOUBLE
#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define SIZEOF_SHORT 2
#define ARCH_INT64_TYPE __int64
#define ARCH_UINT64_TYPE unsigned __int64
#define ARCH_INT64_PRINTF_FORMAT "I64"
#undef NONSTANDARD_DIV_MOD

