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

/* Asm part of the runtime system, Mips processor */

        .comm   young_start, 4
        .comm   young_end, 4
        .comm   young_ptr, 4
        .comm   gc_entry_regs, 4 * 32
        .comm   gc_entry_float_regs, 8 * 16
        .comm   caml_top_of_stack, 4
        .comm   caml_bottom_of_stack, 4
        .comm   caml_last_return_address, 4
        .comm   caml_exception_pointer, 4
        .comm   remembered_ptr, 4
        .comm   remembered_end, 4

#define SAVE(r) sw $/**/r, gc_entry_regs + r * 4
#define LOAD(r) lw $/**/r, gc_entry_regs + r * 4
#define FSAVE(r) s.d $f/**/r, gc_entry_float_regs + r * 4
#define FLOAD(r) l.d $f/**/r, gc_entry_float_regs + r * 4

/* Allocation */

        .text
        .globl  caml_alloc1
        .globl  caml_alloc2
        .globl  caml_alloc3
        .globl  caml_alloc
        .globl  caml_call_gc
        .ent    caml_alloc1

/* caml_alloc* : all code generator registers preserved. */

caml_alloc1:
        subu    $22, $22, 8
        bltu    $22, $23, $100
        j	$31
$100:   li      $25, 8
        b       caml_call_gc

caml_alloc2:
        subu    $22, $22, 12
        bltu    $22, $23, $101
        j	$31
$101:   li      $25, 12
        b       caml_call_gc

caml_alloc3:
        subu    $22, $22, 16
        bltu    $22, $23, $102
        j	$31
$102:   li      $25, 16
        b       caml_call_gc

caml_alloc:
        subu    $22, $22, $25
        bltu    $22, $23, caml_call_gc
        j       $31
        
caml_call_gc:
    /* Record lowest stack address and return address */
        sw      $31, caml_last_return_address
        sw      $sp, caml_bottom_of_stack
    /* Save requested size */
        subu    $sp, $sp, 8
        sw      $31, 4($sp)
        sw      $25, 0($sp)
    /* Save current allocation pointer for debugging purposes */
        sw      $22, young_ptr
    /* Save all regs used by the code generator in the arrays
    /* gc_entry_regs and gc_entry_float_regs. */
        SAVE(2)
        SAVE(3)
        SAVE(4)
        SAVE(5)
        SAVE(6)
        SAVE(7)
        SAVE(8)
        SAVE(9)
        SAVE(10)
        SAVE(11)
        SAVE(12)
        SAVE(13)
        SAVE(14)
        SAVE(15)
        SAVE(16)
        SAVE(17)
        SAVE(18)
        SAVE(19)
        SAVE(20)
        SAVE(21)
        FSAVE(0)
        FSAVE(2)
        FSAVE(4)
        FSAVE(6)
        FSAVE(8)
        FSAVE(12)
        FSAVE(14)
        FSAVE(16)
        FSAVE(18)
        FSAVE(20)
        FSAVE(22)
        FSAVE(24)
        FSAVE(26)
        FSAVE(28)
        FSAVE(30)
    /* Call the garbage collector */
        jal     minor_collection
    /* Restore all regs used by the code generator */
        LOAD(2)
        LOAD(3)
        LOAD(4)
        LOAD(5)
        LOAD(6)
        LOAD(7)
        LOAD(8)
        LOAD(9)
        LOAD(10)
        LOAD(11)
        LOAD(12)
        LOAD(13)
        LOAD(14)
        LOAD(15)
        LOAD(16)
        LOAD(17)
        LOAD(18)
        LOAD(19)
        LOAD(20)
        LOAD(21)
        FLOAD(0)
        FLOAD(2)
        FLOAD(4)
        FLOAD(6)
        FLOAD(8)
        FLOAD(12)
        FLOAD(14)
        FLOAD(16)
        FLOAD(18)
        FLOAD(20)
        FLOAD(22)
        FLOAD(24)
        FLOAD(26)
        FLOAD(28)
        FLOAD(30)
    /* Reload new allocation pointer and allocation limit */
        lw      $22, young_ptr
        lw      $23, young_start
    /* Allocate space for the block */
        lw      $25, 0($sp)
        subu    $22, $22, $25
    /* Return to caller */
        lw	$31, 4($sp)
        addu    $sp, $sp, 8
        j       $31

        .end    caml_alloc1

/* Call a C function from Caml */

        .globl  caml_c_call
        .ent    caml_c_call

caml_c_call:
    /* Function to call is in $25 */
    /* Record lowest stack address and return address */
        sw      $31, caml_last_return_address
        sw      $sp, caml_bottom_of_stack
    /* Make the exception handler and alloc ptr available to the C code */
        sw      $22, young_ptr
        sw      $30, caml_exception_pointer
    /* Call the function */
        jal     $25
    /* Reload alloc ptr */
        lw      $22, young_ptr
    /* Return */
        lw      $31, caml_last_return_address
        j       $31

        .end    caml_c_call

/* Start the Caml program */

        .globl  caml_start_program
        .globl  stray_exn_handler
        .ent    caml_start_program
caml_start_program:
	subu    $sp, $sp, 88
        sw      $31, 84($sp)
    /* Save all callee-save registers */
        sw      $16, 0($sp)
        sw      $17, 4($sp)
        sw      $18, 8($sp)
        sw      $19, 12($sp)
        sw      $20, 16($sp)
        sw      $21, 20($sp)
        sw      $22, 24($sp)
        sw      $23, 28($sp)
        sw      $30, 32($sp)
        s.d     $f20, 36($sp)
        s.d     $f22, 44($sp)
        s.d     $f24, 52($sp)
        s.d     $f26, 60($sp)
        s.d     $f28, 68($sp)
        s.d     $f30, 76($sp)
    /* Build an exception handler */
	subu    $sp, $sp, 8
	la	$2, stray_exn_handler
	sw 	$2, 4($sp)
	move    $30, $sp
        sw      $sp, caml_top_of_stack
    /* Initialize allocation registers */
	lw	$22, young_ptr
	lw	$23, young_start
    /* Go for it */
        jal     caml_program
    /* Pop handler */
        addu    $sp, $sp, 8
    /* Return with zero code */
        li      $2, 0
    /* Restore registers */
stray_exn_handler:
        lw      $31, 84($sp)
        lw      $16, 0($sp)
        lw      $17, 4($sp)
        lw      $18, 8($sp)
        lw      $19, 12($sp)
        lw      $20, 16($sp)
        lw      $21, 20($sp)
        lw      $22, 24($sp)
        lw      $23, 28($sp)
        lw      $30, 32($sp)
        l.d     $f20, 36($sp)
        l.d     $f22, 44($sp)
        l.d     $f24, 52($sp)
        l.d     $f26, 60($sp)
        l.d     $f28, 68($sp)
        l.d     $f30, 76($sp)
        addu    $sp, $sp, 88
        j       $31

        .end    caml_start_program

/* Raise an exception from C */

        .globl  raise_caml_exception
        .ent    raise_caml_exception
raise_caml_exception:
        move    $2, $4
        lw      $22, young_ptr
        lw      $23, young_start
        lw      $sp, caml_exception_pointer
        lw      $30, 0($sp)
        lw      $2, 8($sp)
        addu    $sp, $sp, 8
        j       $2

        .end    raise_caml_exception
