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

/* Handling of blocks of bytecode (endianness switch, threading). */

#ifndef _fix_code_
#define _fix_code_


#include "config.h"
#include "misc.h"
#include "mlvalues.h"

extern code_t start_code;
extern asize_t code_size;
extern unsigned char * saved_code;
extern unsigned char code_md5[16];

void load_code (int fd, asize_t len);
void fixup_endianness (code_t code, asize_t len);
void set_instruction (code_t pos, opcode_t instr);
int is_instruction (opcode_t instr1, opcode_t instr2);

#ifdef THREADED_CODE
extern char ** instr_table;
extern char * instr_base;
void thread_code (code_t code, asize_t len);
#endif

#endif
