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

/* Structured input/output */

#ifndef __intext__
#define __intext__

#include "misc.h"
#include "mlvalues.h"
#include "io.h"

/* Magic number */

#define Intext_magic_number 0x8495A6BE

/* Codes for the compact format */

#define PREFIX_SMALL_BLOCK 0x80
#define PREFIX_SMALL_INT 0x40
#define PREFIX_SMALL_STRING 0x20
#define CODE_INT8 0x0
#define CODE_INT16 0x1
#define CODE_INT32 0x2
#define CODE_INT64 0x3
#define CODE_SHARED8 0x4
#define CODE_SHARED16 0x5
#define CODE_SHARED32 0x6
#define CODE_BLOCK32 0x8
#define CODE_STRING8 0x9
#define CODE_STRING32 0xA
#define CODE_DOUBLE_BIG 0xB
#define CODE_DOUBLE_LITTLE 0xC
#define CODE_DOUBLE_ARRAY8_BIG 0xD
#define CODE_DOUBLE_ARRAY8_LITTLE 0xE
#define CODE_DOUBLE_ARRAY32_BIG 0xF
#define CODE_DOUBLE_ARRAY32_LITTLE 0x7
#define CODE_CODEPOINTER 0x10
#define CODE_INFIXPOINTER 0x11
#define CODE_CUSTOM 0x12

#ifdef ARCH_BIG_ENDIAN
#define CODE_DOUBLE_NATIVE CODE_DOUBLE_BIG
#define CODE_DOUBLE_ARRAY8_NATIVE CODE_DOUBLE_ARRAY8_BIG
#define CODE_DOUBLE_ARRAY32_NATIVE CODE_DOUBLE_ARRAY32_BIG
#else
#define CODE_DOUBLE_NATIVE CODE_DOUBLE_LITTLE
#define CODE_DOUBLE_ARRAY8_NATIVE CODE_DOUBLE_ARRAY8_LITTLE
#define CODE_DOUBLE_ARRAY32_NATIVE CODE_DOUBLE_ARRAY32_LITTLE
#endif

/* Initial sizes of data structures for extern */

#ifndef INITIAL_EXTERN_BLOCK_SIZE
#define INITIAL_EXTERN_BLOCK_SIZE 8192
#endif

#ifndef INITIAL_EXTERN_TABLE_SIZE
#define INITIAL_EXTERN_TABLE_SIZE 2039
#endif

/* Maximal value of initial_ofs above which we should start again with
   initial_ofs = 1. Should be low enough to prevent rollover of initial_ofs
   next time we extern a structure. Since a structure contains at most 
   2^N / (2 * sizeof(value)) heap objects (N = 32 or 64 depending on target),
   any value below 2^N - (2^N / (2 * sizeof(value))) suffices.
   We just take 2^(N-1) for simplicity. */

#define INITIAL_OFFSET_MAX ((unsigned long)1 << (8 * sizeof(value) - 1))

/* The entry points */

CAMLextern void output_val (struct channel * chan, value v, value flags);
CAMLextern void output_value_to_malloc(value v, value flags,
                                       /*out*/ char ** buf,
                                       /*out*/ long * len);
CAMLextern value input_val (struct channel * chan);
CAMLextern value input_val_from_string (value str, long ofs);
CAMLextern value input_value_from_malloc(char * data, long ofs);

/* Functions for writing user-defined marshallers */

CAMLextern void serialize_int_1(int i);
CAMLextern void serialize_int_2(int i);
CAMLextern void serialize_int_4(int32 i);
CAMLextern void serialize_int_8(int64 i);
CAMLextern void serialize_float_4(float f);
CAMLextern void serialize_float_8(double f);
CAMLextern void serialize_block_1(void * data, long len);
CAMLextern void serialize_block_2(void * data, long len);
CAMLextern void serialize_block_4(void * data, long len);
CAMLextern void serialize_block_8(void * data, long len);

CAMLextern int deserialize_uint_1(void);
CAMLextern int deserialize_sint_1(void);
CAMLextern int deserialize_uint_2(void);
CAMLextern int deserialize_sint_2(void);
CAMLextern uint32 deserialize_uint_4(void);
CAMLextern int32 deserialize_sint_4(void);
CAMLextern uint64 deserialize_uint_8(void);
CAMLextern int64 deserialize_sint_8(void);
CAMLextern float deserialize_float_4(void);
CAMLextern double deserialize_float_8(void);
CAMLextern void deserialize_block_1(void * data, long len);
CAMLextern void deserialize_block_2(void * data, long len);
CAMLextern void deserialize_block_4(void * data, long len);
CAMLextern void deserialize_block_8(void * data, long len);
CAMLextern void deserialize_error(char * msg);

/* Auxiliary stuff for sending code pointers */
unsigned char * code_checksum (void);

#ifndef NATIVE_CODE
#include "fix_code.h"
#define code_area_start ((char *) start_code)
#define code_area_end ((char *) start_code + code_size)
#else
extern char * code_area_start, * code_area_end;
#endif

#endif

