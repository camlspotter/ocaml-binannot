        .globl  call_gen_code
        .ent    call_gen_code
	
call_gen_code:
        lda     $sp, -80($sp)
        stq     $26, 0($sp)
        stq     $9, 8($sp)
        stq     $10, 16($sp)
        stq     $11, 24($sp)
        stq     $12, 32($sp)
        stt     $f2, 40($sp)
        stt     $f3, 48($sp)
        stt     $f4, 56($sp)
        stt     $f5, 64($sp)
        mov     $16, $27
        mov     $17, $16
        mov     $18, $17
        mov     $19, $18
        mov     $20, $19
        jsr     ($27)
        ldq     $26, 0($sp)
        ldq     $9, 8($sp)
        ldq     $10, 16($sp)
        ldq     $11, 24($sp)
        ldq     $12, 32($sp)
        ldt     $f2, 40($sp)
        ldt     $f3, 48($sp)
        ldt     $f4, 56($sp)
        ldt     $f5, 64($sp)
	lda     $sp, 80($sp)
        ret     ($26)

        .end    call_gen_code

        .globl  caml_c_call
        .ent    caml_c_call
caml_c_call:
        mov     $25, $27
        jmp     ($25)

        .end    caml_c_call
