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

/* Processor dependencies */

#define ARCH_SIXTYFOUR

/* Define ARCH_SIXTYFOUR if the processor has a natural word size of 64 bits.
   That is, both sizeof(long) = 8 and sizeof(char *) = 8.
   Otherwise, leave ARCH_SIXTYFOUR undefined. This assumes
   sizeof(long) = sizeof(char *) = 4. */

#define ARCH_BIG_ENDIAN

/* Define ARCH_BIG_ENDIAN if the processor is big endian (the most
   significant byte of an integer stored in memory comes first).
   Leave ARCH_BIG_ENDIAN undefined if the processor is little-endian
   (the least significant byte comes first).
*/

#define ARCH_ALIGN_DOUBLE

/* Define ARCH_ALIGN_DOUBLE if the processor requires doubles to be
   doubleword-aligned. Leave ARCH_ALIGN_DOUBLE undefined if the processor
   supports word-aligned doubles. */

#undef ARCH_CODE32

/* Define ARCH_CODE32 if, on a 64-bit machine, code pointers fit in 32 bits,
   i.e. the code segment resides in the low 4G of the addressing space.
   ARCH_CODE32 is ignored on 32-bit machines. */

#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define SIZEOF_SHORT 2

/* Define SIZEOF_INT, SIZEOF_LONG and SIZEOF_SHORT to the sizes in byte
   of the C types "int", "long" and "short", respectively. */

#define ARCH_INT64_TYPE long long
#define ARCH_UINT64_TYPE unsigned long long

/* Define ARCH_INT64_TYPE and ARCH_UINT64_TYPE to 64-bit integer types,
   typically "long long" and "unsigned long long" on 32-bit platforms,
   and "long" and "unsigned long" on 64-bit platforms.
   If the C compiler doesn't support any 64-bit integer type,
   leave both ARCH_INT64_TYPE and ARCH_UINT64_TYPE undefined. */

#define ARCH_INT64_PRINTF_FORMAT "ll"

/* Define ARCH_INT64_PRINTF_FORMAT to the printf format used for formatting
   values of type ARCH_INT64_TYPE.  This is usually "ll" on 32-bit
   platforms and "l" on 64-bit platforms.
   Leave undefined if ARCH_INT64_TYPE is undefined.  */
