/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifdef SYS_sunos
#define Call_gen_code _call_gen_code
#define Caml_c_call _caml_c_call
#else
#define Call_gen_code call_gen_code
#define Caml_c_call caml_c_call
#endif

        .global Call_gen_code
Call_gen_code:
        save    %sp, -96, %sp
        mov     %i0, %l0
        mov     %i1, %i0
        mov     %i2, %i1
        mov     %i3, %i2
        mov     %i4, %i3
        mov     %i5, %i4
        call    %l0
        nop
        mov     %o0, %i0
        ret
        restore

        .global Caml_c_call
Caml_c_call:
        jmp     %g4
        nop
