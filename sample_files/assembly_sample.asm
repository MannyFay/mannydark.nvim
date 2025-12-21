; ==============================================================================
; Comprehensive x86-64 Assembly Sample - Syntax Highlighting Demonstration
; ==============================================================================

; This file demonstrates x86-64 assembly (NASM syntax) features
; for syntax highlighting purposes.

; ==============================================================================
; Comments
; ==============================================================================

; Single line comment

; Multiple
; line
; comments

; ==============================================================================
; Assembler Directives
; ==============================================================================

bits 64                         ; 64-bit mode
default rel                     ; RIP-relative addressing

; Section declarations
section .data                   ; Initialized data

section .bss                    ; Uninitialized data

section .text                   ; Code section

section .rodata                 ; Read-only data

; ==============================================================================
; Data Definitions
; ==============================================================================

section .data

; Bytes
byte_val:       db 42           ; Define byte
byte_array:     db 1, 2, 3, 4, 5
byte_string:    db 'Hello', 0   ; Null-terminated string
newline:        db 10           ; Newline character

; Words (16-bit)
word_val:       dw 1000
word_array:     dw 100, 200, 300

; Double words (32-bit)
dword_val:      dd 100000
dword_array:    dd 1, 2, 3, 4

; Quad words (64-bit)
qword_val:      dq 1000000000000
qword_array:    dq 0x123456789ABCDEF

; Floating point
float_val:      dd 3.14159      ; Single precision
double_val:     dq 2.71828      ; Double precision
float80_val:    dt 1.234567890  ; Extended precision

; Multiple values
zeros:          times 100 db 0
pattern:        times 16 dd 0xDEADBEEF

; String definitions
hello_msg:      db 'Hello, World!', 10, 0
format_str:     db '%d + %d = %d', 10, 0
buffer_size     equ 1024

; Alignment
align 16
aligned_data:   dq 0, 0

; ==============================================================================
; BSS Section (Uninitialized Data)
; ==============================================================================

section .bss

resb_buffer:    resb 256        ; Reserve 256 bytes
resw_buffer:    resw 128        ; Reserve 128 words
resd_buffer:    resd 64         ; Reserve 64 double words
resq_buffer:    resq 32         ; Reserve 32 quad words

; Aligned buffer
alignb 4096
page_buffer:    resb 4096

; ==============================================================================
; Constants and Equates
; ==============================================================================

; Numeric constants
BUFFER_SIZE     equ 1024
MAX_VALUE       equ 0xFFFFFFFF
SYS_READ        equ 0
SYS_WRITE       equ 1
SYS_EXIT        equ 60
STDOUT          equ 1
STDIN           equ 0

; Calculated constants
ARRAY_LEN       equ ($ - byte_array)
STRUCT_SIZE     equ 32

; ==============================================================================
; Macros
; ==============================================================================

; Simple macro
%macro push_all 0
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    push r8
    push r9
    push r10
    push r11
    push r12
    push r13
    push r14
    push r15
%endmacro

%macro pop_all 0
    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop r10
    pop r9
    pop r8
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
%endmacro

; Macro with parameters
%macro print_string 2
    mov rax, SYS_WRITE
    mov rdi, STDOUT
    mov rsi, %1
    mov rdx, %2
    syscall
%endmacro

; Variadic macro
%macro debug_print 1+
    section .data
    %%msg: db %1, 10, 0
    section .text
    push rdi
    lea rdi, [%%msg]
    call puts
    pop rdi
%endmacro

; Conditional macro
%macro alloc_stack 1
    %if %1 > 0
        sub rsp, %1
    %endif
%endmacro

; Recursive macro
%macro countdown 1
    %if %1 > 0
        db %1
        countdown %1-1
    %endif
%endmacro

; ==============================================================================
; Conditional Assembly
; ==============================================================================

%define DEBUG 1

%ifdef DEBUG
    %define LOG(x) call debug_log
%else
    %define LOG(x)
%endif

%if BUFFER_SIZE > 512
    %warning "Large buffer size"
%elif BUFFER_SIZE < 64
    %error "Buffer too small"
%endif

%ifenv HOME
    ; Environment variable exists
%endif

; ==============================================================================
; Text Section (Code)
; ==============================================================================

section .text

global _start
global main
extern printf
extern scanf
extern malloc
extern free
extern puts

; ==============================================================================
; Basic Instructions
; ==============================================================================

_start:
main:
    ; Function prologue
    push rbp
    mov rbp, rsp
    sub rsp, 32                 ; Allocate local space

    ; Data movement
    mov rax, 42                 ; Immediate to register
    mov rbx, rax                ; Register to register
    mov [buffer], rax           ; Register to memory
    mov rcx, [buffer]           ; Memory to register
    mov qword [rsp], 0          ; Immediate to memory

    ; Different sizes
    mov al, 0xFF                ; 8-bit
    mov ax, 0xFFFF              ; 16-bit
    mov eax, 0xFFFFFFFF         ; 32-bit (zero-extends to 64-bit)
    mov rax, 0xFFFFFFFFFFFFFFFF ; 64-bit

    ; LEA instruction
    lea rax, [hello_msg]        ; Load effective address
    lea rbx, [rax + rcx*8 + 16] ; Complex addressing

    ; Exchange
    xchg rax, rbx               ; Exchange values
    xadd [buffer], rax          ; Exchange and add

    ; Move with sign/zero extension
    movzx rax, byte [byte_val]  ; Zero extend byte to qword
    movsx rax, word [word_val]  ; Sign extend word to qword
    movsxd rax, dword [dword_val] ; Sign extend dword to qword

    ; Conditional moves
    cmp rax, rbx
    cmove rcx, rdx              ; Move if equal
    cmovne rcx, rdx             ; Move if not equal
    cmovg rcx, rdx              ; Move if greater
    cmovl rcx, rdx              ; Move if less

; ==============================================================================
; Arithmetic Instructions
; ==============================================================================

arithmetic_demo:
    ; Addition
    add rax, rbx                ; rax += rbx
    add rax, 10                 ; rax += 10
    add [buffer], rax           ; memory += rax
    adc rax, rbx                ; Add with carry

    ; Subtraction
    sub rax, rbx                ; rax -= rbx
    sbb rax, rbx                ; Subtract with borrow
    neg rax                     ; Two's complement negation

    ; Multiplication
    imul rax, rbx               ; Signed: rax *= rbx
    imul rax, rbx, 10           ; Signed: rax = rbx * 10
    mul rbx                     ; Unsigned: rdx:rax = rax * rbx

    ; Division
    xor rdx, rdx                ; Clear rdx for division
    idiv rbx                    ; Signed: rax = rdx:rax / rbx, rdx = remainder
    div rbx                     ; Unsigned division

    ; Increment/Decrement
    inc rax                     ; rax++
    dec rax                     ; rax--
    inc qword [buffer]          ; memory++

; ==============================================================================
; Logical and Bit Instructions
; ==============================================================================

logical_demo:
    ; Logical operations
    and rax, rbx                ; Bitwise AND
    or rax, rbx                 ; Bitwise OR
    xor rax, rbx                ; Bitwise XOR
    not rax                     ; Bitwise NOT
    test rax, rbx               ; AND without storing result

    ; Shifts
    shl rax, 4                  ; Shift left logical
    shr rax, 4                  ; Shift right logical
    sal rax, 4                  ; Shift arithmetic left
    sar rax, 4                  ; Shift arithmetic right
    shl rax, cl                 ; Shift by register

    ; Rotates
    rol rax, 4                  ; Rotate left
    ror rax, 4                  ; Rotate right
    rcl rax, 1                  ; Rotate through carry left
    rcr rax, 1                  ; Rotate through carry right

    ; Bit manipulation
    bt rax, 5                   ; Bit test
    bts rax, 5                  ; Bit test and set
    btr rax, 5                  ; Bit test and reset
    btc rax, 5                  ; Bit test and complement

    ; Bit scanning
    bsf rax, rbx                ; Bit scan forward
    bsr rax, rbx                ; Bit scan reverse
    lzcnt rax, rbx              ; Leading zero count
    tzcnt rax, rbx              ; Trailing zero count
    popcnt rax, rbx             ; Population count

; ==============================================================================
; Control Flow
; ==============================================================================

control_flow:
    ; Comparisons
    cmp rax, rbx
    test rax, rax               ; Test for zero

    ; Unconditional jump
    jmp .continue

    ; Conditional jumps (after cmp/test)
.compare:
    je .equal                   ; Jump if equal (ZF=1)
    jne .not_equal              ; Jump if not equal (ZF=0)
    jg .greater                 ; Jump if greater (signed)
    jge .greater_equal          ; Jump if greater or equal
    jl .less                    ; Jump if less (signed)
    jle .less_equal             ; Jump if less or equal
    ja .above                   ; Jump if above (unsigned)
    jae .above_equal            ; Jump if above or equal
    jb .below                   ; Jump if below (unsigned)
    jbe .below_equal            ; Jump if below or equal

    ; Flag jumps
    jz .zero                    ; Jump if zero
    jnz .not_zero               ; Jump if not zero
    js .sign                    ; Jump if sign (negative)
    jns .not_sign               ; Jump if not sign
    jo .overflow                ; Jump if overflow
    jno .not_overflow           ; Jump if not overflow
    jc .carry                   ; Jump if carry
    jnc .not_carry              ; Jump if not carry

.equal:
.not_equal:
.greater:
.greater_equal:
.less:
.less_equal:
.above:
.above_equal:
.below:
.below_equal:
.zero:
.not_zero:
.sign:
.not_sign:
.overflow:
.not_overflow:
.carry:
.not_carry:
.continue:

; ==============================================================================
; Loops
; ==============================================================================

loop_demo:
    mov rcx, 10                 ; Loop counter

.loop_start:
    ; Loop body
    dec rcx
    jnz .loop_start             ; Manual loop

    ; LOOP instruction
    mov rcx, 10
.loop2:
    nop
    loop .loop2                 ; Decrement RCX and jump if not zero

    ; String operations with REP
    mov rcx, 100
    lea rsi, [source]
    lea rdi, [dest]
    rep movsb                   ; Copy RCX bytes

    ; REPE/REPNE
    mov rcx, 100
    repe cmpsb                  ; Compare while equal
    repne scasb                 ; Scan while not equal

; ==============================================================================
; Function Calls
; ==============================================================================

function_calls:
    ; Direct call
    call my_function

    ; Indirect call
    mov rax, my_function
    call rax

    ; Calling C functions (System V AMD64 ABI)
    ; Arguments: rdi, rsi, rdx, rcx, r8, r9 (then stack)
    ; Return: rax (and rdx for 128-bit)

    ; printf example
    lea rdi, [format_str]       ; Format string
    mov rsi, 10                 ; First argument
    mov rdx, 20                 ; Second argument
    mov rcx, 30                 ; Third argument
    xor rax, rax                ; No XMM arguments
    call printf

    ; malloc example
    mov rdi, 1024               ; Size
    call malloc
    test rax, rax               ; Check for NULL
    jz .alloc_failed
    mov [buffer_ptr], rax

.alloc_failed:

; ==============================================================================
; Function Definition
; ==============================================================================

my_function:
    ; Prologue
    push rbp
    mov rbp, rsp
    sub rsp, 32                 ; Local variables
    push rbx                    ; Callee-saved registers
    push r12
    push r13

    ; Function body
    mov rax, rdi                ; First argument
    add rax, rsi                ; Second argument

    ; Epilogue
    pop r13
    pop r12
    pop rbx
    mov rsp, rbp
    pop rbp
    ret

; Function with stack frame
calculate_sum:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    mov [rbp-8], rdi            ; Local variable
    mov [rbp-16], rsi           ; Local variable

    mov rax, [rbp-8]
    add rax, [rbp-16]

    leave                       ; Equivalent to mov rsp, rbp; pop rbp
    ret

; ==============================================================================
; System Calls (Linux)
; ==============================================================================

syscall_demo:
    ; Write to stdout
    mov rax, SYS_WRITE          ; syscall number
    mov rdi, STDOUT             ; fd
    lea rsi, [hello_msg]        ; buffer
    mov rdx, 14                 ; count
    syscall

    ; Exit
    mov rax, SYS_EXIT
    xor rdi, rdi                ; exit code 0
    syscall

; ==============================================================================
; Stack Operations
; ==============================================================================

stack_demo:
    push rax                    ; Push register
    push qword 42               ; Push immediate
    push qword [buffer]         ; Push memory

    pop rax                     ; Pop to register
    pop qword [buffer]          ; Pop to memory

    ; Stack frame access
    mov rax, [rbp+16]           ; Argument (above return address)
    mov rbx, [rbp-8]            ; Local variable

    ; Adjust stack
    sub rsp, 32                 ; Allocate
    add rsp, 32                 ; Deallocate

; ==============================================================================
; SIMD Instructions (SSE/AVX)
; ==============================================================================

simd_demo:
    ; SSE data movement
    movaps xmm0, [aligned_data] ; Move aligned packed single
    movups xmm1, [buffer]       ; Move unaligned packed single
    movss xmm2, [float_val]     ; Move scalar single
    movsd xmm3, [double_val]    ; Move scalar double

    ; SSE arithmetic
    addps xmm0, xmm1            ; Add packed singles
    subps xmm0, xmm1            ; Subtract packed singles
    mulps xmm0, xmm1            ; Multiply packed singles
    divps xmm0, xmm1            ; Divide packed singles
    sqrtps xmm0, xmm1           ; Square root

    addss xmm0, xmm1            ; Add scalar single
    addsd xmm0, xmm1            ; Add scalar double

    ; SSE comparisons
    cmpps xmm0, xmm1, 0         ; Compare packed singles (eq)
    cmpeqps xmm0, xmm1          ; Compare equal
    cmpltps xmm0, xmm1          ; Compare less than

    ; SSE conversions
    cvtsi2ss xmm0, rax          ; Convert int to scalar single
    cvtss2si rax, xmm0          ; Convert scalar single to int
    cvtps2pd xmm0, xmm1         ; Convert packed single to double

    ; AVX instructions (256-bit)
    vmovaps ymm0, [aligned_data]
    vaddps ymm0, ymm1, ymm2     ; Three-operand form
    vfmadd213ps ymm0, ymm1, ymm2 ; Fused multiply-add

    ; Shuffles and blends
    shufps xmm0, xmm1, 0x1B     ; Shuffle
    blendps xmm0, xmm1, 0x05    ; Blend
    pshufb xmm0, xmm1           ; Byte shuffle

    ; Horizontal operations
    haddps xmm0, xmm1           ; Horizontal add
    dpps xmm0, xmm1, 0xFF       ; Dot product

; ==============================================================================
; String Instructions
; ==============================================================================

string_demo:
    cld                         ; Clear direction flag (forward)
    ; std                       ; Set direction flag (backward)

    ; Move string
    lea rsi, [source]
    lea rdi, [dest]
    mov rcx, 100
    rep movsb                   ; Move bytes
    rep movsw                   ; Move words
    rep movsd                   ; Move dwords
    rep movsq                   ; Move qwords

    ; Store string
    mov al, 0
    lea rdi, [dest]
    mov rcx, 100
    rep stosb                   ; Store AL to [RDI]

    ; Load string
    lea rsi, [source]
    lodsb                       ; Load [RSI] to AL

    ; Compare string
    lea rsi, [source]
    lea rdi, [dest]
    mov rcx, 100
    repe cmpsb                  ; Compare while equal

    ; Scan string
    mov al, 0
    lea rdi, [dest]
    mov rcx, 100
    repne scasb                 ; Scan for AL

; ==============================================================================
; Addressing Modes
; ==============================================================================

addressing_demo:
    ; Immediate
    mov rax, 42
    mov rax, 0x1234
    mov rax, 0b10101010
    mov rax, 0o777

    ; Register
    mov rax, rbx

    ; Direct memory
    mov rax, [buffer]
    mov rax, [0x1000]

    ; Register indirect
    mov rax, [rbx]

    ; Base + displacement
    mov rax, [rbx + 16]
    mov rax, [rbx - 8]

    ; Base + index
    mov rax, [rbx + rcx]

    ; Base + index * scale
    mov rax, [rbx + rcx*2]
    mov rax, [rbx + rcx*4]
    mov rax, [rbx + rcx*8]

    ; Base + index * scale + displacement
    mov rax, [rbx + rcx*8 + 16]

    ; RIP-relative
    mov rax, [rel buffer]
    lea rax, [rel hello_msg]

; ==============================================================================
; Segment and Special Registers
; ==============================================================================

segment_demo:
    ; Segment overrides
    mov rax, [fs:0x28]          ; Thread-local storage (stack canary)
    mov rax, [gs:0x00]          ; Kernel data

    ; Flags register
    pushfq                      ; Push flags
    popfq                       ; Pop flags

    lahf                        ; Load AH from flags
    sahf                        ; Store AH to flags

    stc                         ; Set carry flag
    clc                         ; Clear carry flag
    cmc                         ; Complement carry flag

    std                         ; Set direction flag
    cld                         ; Clear direction flag

; ==============================================================================
; Atomic and Synchronization
; ==============================================================================

atomic_demo:
    ; Lock prefix for atomic operations
    lock inc qword [counter]
    lock xadd [counter], rax
    lock cmpxchg [buffer], rbx
    lock cmpxchg16b [oword_buffer]

    ; Memory barriers
    mfence                      ; Full memory fence
    sfence                      ; Store fence
    lfence                      ; Load fence

    ; Pause (spin-wait)
    pause

; ==============================================================================
; Miscellaneous Instructions
; ==============================================================================

misc_demo:
    nop                         ; No operation
    nop dword [rax]             ; Multi-byte NOP

    cpuid                       ; CPU identification

    rdtsc                       ; Read time stamp counter

    xgetbv                      ; Get extended control register

    ; Prefetch
    prefetcht0 [buffer]         ; Prefetch to all cache levels
    prefetcht1 [buffer]         ; Prefetch to L2 and above
    prefetchnta [buffer]        ; Non-temporal prefetch

    ; Cache control
    clflush [buffer]            ; Cache line flush

    ; Byte swap
    bswap rax                   ; Reverse byte order

; ==============================================================================
; Data Definitions
; ==============================================================================

section .data

buffer:         times 256 db 0
counter:        dq 0
buffer_ptr:     dq 0
source:         times 256 db 'A'
dest:           times 256 db 0
oword_buffer:   times 16 db 0

; Structure-like layout
struc Person
    .name:  resb 64
    .age:   resd 1
    .email: resb 128
endstruc

person1:
    istruc Person
        at Person.name,  db 'John Doe', 0
        at Person.age,   dd 30
        at Person.email, db 'john@example.com', 0
    iend

; ==============================================================================
; End of File
; ==============================================================================
