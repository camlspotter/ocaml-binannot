# Asm part of the runtime system, Intel 386 processor

        .comm   _young_start, 4
        .comm   _young_ptr, 4
        .comm   _gc_entry_regs, 4 * 7
        .comm   _caml_bottom_of_stack, 4
        .comm   _caml_last_return_address, 4
        .comm   _remembered_ptr, 4
        .comm   _remembered_end, 4
        .comm   _caml_exception_pointer, 4

# Allocation

        .text
        .globl  _caml_alloc1
        .globl  _caml_alloc2
        .globl  _caml_alloc3
        .globl  _caml_alloc
	.globl  _caml_call_gc

        .align  4
_caml_alloc1:
        movl    _young_ptr, %eax
        subl    $8, %eax
        movl    %eax, _young_ptr
        cmpl    _young_start, %eax
        jb      L100
        ret
L100:   movl    $8, %eax
        jmp     _caml_call_gc

        .align  4
_caml_alloc2:
        movl    _young_ptr, %eax
        subl    $12, %eax
        movl    %eax, _young_ptr
        cmpl    _young_start, %eax
        jb      L101
        ret
L101:   movl    $12, %eax
        jmp     _caml_call_gc

        .align  4
_caml_alloc3:
        movl    _young_ptr, %eax
        subl    $16, %eax
        movl    %eax, _young_ptr
        cmpl    _young_start, %eax
        jb      L102
        ret
L102:   movl    $16, %eax
        jmp     _caml_call_gc

        .align  4
_caml_alloc:
        pushl   %eax
        movl    _young_ptr, %eax
        subl    (%esp), %eax
        movl    %eax, _young_ptr
        cmpl    _young_start, %eax
        jb      L103
        addl    $4, %esp
        ret
L103:   popl    %eax

_caml_call_gc:
    # Record lowest stack address and return address
        popl    _caml_last_return_address
        movl    %esp, _caml_bottom_of_stack
    # Save all regs used by the code generator
        movl    %ebx, _gc_entry_regs + 4
        movl    %ecx, _gc_entry_regs + 8
        movl    %edx, _gc_entry_regs + 12
        movl    %esi, _gc_entry_regs + 16
        movl    %edi, _gc_entry_regs + 20
        movl    %ebp, _gc_entry_regs + 24
    # Pass the desired size as first argument
        pushl   %eax
    # Call the garbage collector
        call    _garbage_collection
        add     $4, %esp
    # Restore all regs used by the code generator
        movl    _gc_entry_regs + 4, %ebx
        movl    _gc_entry_regs + 8, %ecx
        movl    _gc_entry_regs + 12, %edx
        movl    _gc_entry_regs + 16, %esi
        movl    _gc_entry_regs + 20, %edi
        movl    _gc_entry_regs + 24, %ebp
    # Reload result of allocation in %eax
        movl    _young_ptr, %eax
    # Return to caller
        pushl   _caml_last_return_address
        ret

# Modification

        .globl  _caml_modify
        .globl  _caml_fast_modify

        .align  4
_caml_modify:
        testb   $4, -3(%eax)
        jz      _caml_fast_modify
        ret

_caml_fast_modify:
    # Store address of object in remembered set
        pushl   %eax
        movl    _remembered_ptr, %eax
        popl    (%eax)
        addl    $4, %eax
        movl    %eax, _remembered_ptr
        cmpl    _remembered_end, %eax
        ja      _caml_modify_realloc
        ret

_caml_modify_realloc:
    # Reallocate the remembered set while preserving all regs
        pushl   %ecx
        pushl   %edx
    # (%eax dead, %ebx, %esi, %edi, %ebp preserved by C)
        call    _realloc_remembered
        popl    %edx
        popl    %ecx
        ret

# Call a C function from Caml

        .globl  _caml_c_call

        .align  4
_caml_c_call:
    # Record lowest stack address and return address
        popl    _caml_last_return_address
        movl    %esp, _caml_bottom_of_stack
    # Call the function (address in %eax)
        call    *%eax
    # Return to caller
        movl    _caml_last_return_address, %edx  #  %edx dead here
        jmp     *%edx

# Start the Caml program

        .globl  _caml_start_program
        .align  4
_caml_start_program:
    # Save callee-save registers
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp
    # Build an exception handler
        pushl   $0
        pushl   $L104
        movl    %esp, _caml_exception_pointer
    # Go for it
        call    _caml_program
    # Pop handler
        addl    $8, %esp
    # Zero return code
        xorl    %eax, %eax
L104:
    # Restore registers and return
        popl    %ebp
        popl    %edi
        popl    %esi
        popl    %ebx
        ret

# Raise an exception from C

        .globl  _raise_caml_exception
        .align  4
_raise_caml_exception:
        movl    4(%esp), %eax
        movl    _caml_exception_pointer, %esp
        popl    %edx
        popl    _caml_exception_pointer
        jmp     *%edx
