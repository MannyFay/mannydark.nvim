-------------------------------------------------------------------------------
-- Assembly Language
-- Highlighting for various assembly languages and assemblers:
-- Generic ASM, NASM, MASM, FASM, TASM, GAS, ARM, 68000, Z80, IA-64, etc.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local asm       = {}


-------------------------------------------------------------------------------
-- Settings

asm.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Generic Assembly (asm.vim - GNU Assembler style)

  -- Labels
  highlight(0, 'asmLabel',              { fg = colors.orange,     bg = 'NONE' })  -- label:
  highlight(0, 'asmIdentifier',         { fg = colors.white,      bg = 'NONE' })  -- Identifiers

  -- Instructions/Opcodes
  highlight(0, 'asmOpcode',             { fg = colors.pink,       bg = 'NONE' })  -- Instructions
  highlight(0, 'asmStatement',          { fg = colors.pink,       bg = 'NONE' })  -- Statements

  -- Directives
  highlight(0, 'asmDirective',          { fg = colors.blue,       bg = 'NONE' })  -- .section, .text, .data, etc.
  highlight(0, 'asmInclude',            { fg = colors.pink,       bg = 'NONE' })  -- .include
  highlight(0, 'asmCond',               { fg = colors.blue,       bg = 'NONE' })  -- .if, .else, .endif
  highlight(0, 'asmMacro',              { fg = colors.pink,       bg = 'NONE' })  -- .macro, .endm

  -- Types/Storage
  highlight(0, 'asmType',               { link = "Type" })  -- .long, .byte, .word, .ascii, etc.

  -- Registers
  highlight(0, 'asmRegister',           { fg = colors.purple,     bg = 'NONE' })  -- %eax, %rsp, etc.

  -- Numbers
  highlight(0, 'asmDecimal',            { fg = colors.greenLight, bg = 'NONE' })  -- Decimal numbers
  highlight(0, 'asmOctal',              { fg = colors.greenLight, bg = 'NONE' })  -- 0777 octal
  highlight(0, 'asmHexadecimal',        { fg = colors.greenLight, bg = 'NONE' })  -- 0x1F hex
  highlight(0, 'asmBinary',             { fg = colors.greenLight, bg = 'NONE' })  -- 0b1010 binary
  highlight(0, 'asmFloat',              { fg = colors.greenLight, bg = 'NONE' })  -- Floating point

  -- Strings
  highlight(0, 'asmString',             { link = "String" })  -- "strings"
  highlight(0, 'asmCharacter',          { fg = colors.redLight,   bg = 'NONE' })  -- 'c' characters

  -- Comments
  highlight(0, 'asmComment',            { link = "Comment" })  -- ; # // /* */ comments
  highlight(0, 'asmTodo',               { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME

  -- Special
  highlight(0, 'asmSpecial',            { fg = colors.pink,       bg = 'NONE' })  -- Special characters
  highlight(0, 'asmSpecialChar',        { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences


  -----------------------------------------------------------------------------
  -- NASM (Netwide Assembler) - x86/x64

  -- Labels
  highlight(0, 'nasmLabel',             { fg = colors.orange,     bg = 'NONE' })  -- label:
  highlight(0, 'nasmLocalLabel',        { fg = colors.orange,     bg = 'NONE' })  -- .local_label
  highlight(0, 'nasmSpecialLabel',      { fg = colors.orange,     bg = 'NONE' })  -- @@, .., $, $$

  -- Instructions (all mapped to pink as they're the core operations)
  highlight(0, 'nasmInstruction',       { fg = colors.pink,       bg = 'NONE' })  -- Standard instructions
  highlight(0, 'nasmInstructn',         { fg = colors.pink,       bg = 'NONE' })  -- Instructions
  highlight(0, 'nasmStdInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- Standard 8086/8088
  highlight(0, 'nasmSIMDInstruction',   { fg = colors.pink,       bg = 'NONE' })  -- SIMD instructions
  highlight(0, 'nasmSSEInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- SSE
  highlight(0, 'nasmSSE2Instruction',   { fg = colors.pink,       bg = 'NONE' })  -- SSE2
  highlight(0, 'nasmSSE3Instruction',   { fg = colors.pink,       bg = 'NONE' })  -- SSE3
  highlight(0, 'nasmSSSE3Instruction',  { fg = colors.pink,       bg = 'NONE' })  -- SSSE3
  highlight(0, 'nasmSSE41Instruction',  { fg = colors.pink,       bg = 'NONE' })  -- SSE4.1
  highlight(0, 'nasmSSE42Instruction',  { fg = colors.pink,       bg = 'NONE' })  -- SSE4.2
  highlight(0, 'nasmAVXInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- AVX
  highlight(0, 'nasmAVX2Instruction',   { fg = colors.pink,       bg = 'NONE' })  -- AVX2
  highlight(0, 'nasmAVX512Instruction', { fg = colors.pink,       bg = 'NONE' })  -- AVX-512
  highlight(0, 'nasmAESInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- AES-NI
  highlight(0, 'nasmSHAInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- SHA
  highlight(0, 'nasmBMIInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- BMI/ABM
  highlight(0, 'nasmFMAInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- FMA3/FMA4
  highlight(0, 'nasmVMXInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- VMX (virtualization)
  highlight(0, 'nasmSVMInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- SVM (AMD-V)
  highlight(0, 'nasmXOPInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- XOP (AMD)
  highlight(0, 'nasmCETInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- CET
  highlight(0, 'nasmSGXInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- SGX
  highlight(0, 'nasmAMXInstruction',    { fg = colors.pink,       bg = 'NONE' })  -- AMX

  -- Directives
  highlight(0, 'nasmDirective',         { fg = colors.blue,       bg = 'NONE' })  -- General directives
  highlight(0, 'nasmStdDirective',      { fg = colors.blue,       bg = 'NONE' })  -- Standard directives
  highlight(0, 'nasmFmtDirective',      { fg = colors.blue,       bg = 'NONE' })  -- Format directives

  -- Preprocessor
  highlight(0, 'nasmPreProc',           { fg = colors.pink,       bg = 'NONE' })  -- %macro, %define, etc.
  highlight(0, 'nasmDefine',            { fg = colors.pink,       bg = 'NONE' })  -- %define, %xdefine
  highlight(0, 'nasmInclude',           { fg = colors.pink,       bg = 'NONE' })  -- %include
  highlight(0, 'nasmMacro',             { fg = colors.pink,       bg = 'NONE' })  -- %macro, %endmacro
  highlight(0, 'nasmPreCondit',         { fg = colors.pink,       bg = 'NONE' })  -- %if, %ifdef, %else, %endif

  -- Storage/Types
  highlight(0, 'nasmStorage',           { fg = colors.turquoise,  bg = 'NONE' })  -- section, segment
  highlight(0, 'nasmType',              { link = "Type" })  -- db, dw, dd, dq, resb, resw
  highlight(0, 'nasmStructure',         { fg = colors.turquoise,  bg = 'NONE' })  -- struc, endstruc

  -- Registers (8-bit, 16-bit, 32-bit, 64-bit)
  highlight(0, 'nasmRegister',          { fg = colors.purple,     bg = 'NONE' })  -- General registers
  highlight(0, 'nasmGen08Register',     { fg = colors.purple,     bg = 'NONE' })  -- al, ah, bl, bh, cl, ch, dl, dh
  highlight(0, 'nasmGen16Register',     { fg = colors.purple,     bg = 'NONE' })  -- ax, bx, cx, dx, si, di, bp, sp
  highlight(0, 'nasmGen32Register',     { fg = colors.purple,     bg = 'NONE' })  -- eax, ebx, ecx, edx, esi, edi, ebp, esp
  highlight(0, 'nasmGen64Register',     { fg = colors.purple,     bg = 'NONE' })  -- rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, r8-r15
  highlight(0, 'nasmExtRegister',       { fg = colors.purple,     bg = 'NONE' })  -- Extended registers (r8-r15 variants)
  highlight(0, 'nasmSegRegister',       { fg = colors.purple,     bg = 'NONE' })  -- cs, ds, es, fs, gs, ss
  highlight(0, 'nasmSpcRegister',       { fg = colors.purple,     bg = 'NONE' })  -- ip, eip, rip, flags, eflags, rflags
  highlight(0, 'nasmFpuRegister',       { fg = colors.purple,     bg = 'NONE' })  -- st0-st7
  highlight(0, 'nasmMmxRegister',       { fg = colors.purple,     bg = 'NONE' })  -- mm0-mm7
  highlight(0, 'nasmAvxRegister',       { fg = colors.purple,     bg = 'NONE' })  -- xmm0-xmm15, ymm0-ymm15, zmm0-zmm31
  highlight(0, 'nasmCtrlRegister',      { fg = colors.purple,     bg = 'NONE' })  -- cr0-cr15
  highlight(0, 'nasmDebugRegister',     { fg = colors.purple,     bg = 'NONE' })  -- dr0-dr15
  highlight(0, 'nasmTestRegister',      { fg = colors.purple,     bg = 'NONE' })  -- tr0-tr7

  -- Numbers
  highlight(0, 'nasmBinNumber',         { link = "Number" })  -- 1010b binary
  highlight(0, 'nasmOctNumber',         { link = "Number" })  -- 777o/q octal
  highlight(0, 'nasmDecNumber',         { link = "Number" })  -- 123 decimal
  highlight(0, 'nasmHexNumber',         { link = "Number" })  -- 0x1F, 1Fh hex
  highlight(0, 'nasmBinFloat',          { fg = colors.greenLight, bg = 'NONE' })  -- Binary floats
  highlight(0, 'nasmOctFloat',          { fg = colors.greenLight, bg = 'NONE' })  -- Octal floats
  highlight(0, 'nasmDecFloat',          { fg = colors.greenLight, bg = 'NONE' })  -- Decimal floats
  highlight(0, 'nasmHexFloat',          { fg = colors.greenLight, bg = 'NONE' })  -- Hex floats
  highlight(0, 'nasmSpecFloat',         { fg = colors.greenLight, bg = 'NONE' })  -- Special floats (inf, nan)
  highlight(0, 'nasmBcdConst',          { fg = colors.greenLight, bg = 'NONE' })  -- BCD constants
  highlight(0, 'nasmNumber',            { link = "Number" })  -- Generic numbers

  -- Strings
  highlight(0, 'nasmString',            { link = "String" })  -- "strings", 'strings'
  highlight(0, 'nasmCString',           { link = "String" })  -- C-style strings
  highlight(0, 'nasmCStringEscape',     { link = "String" })  -- \n, \t, etc.
  highlight(0, 'nasmCStringFormat',     { link = "String" })  -- %d, %s, etc.

  -- Comments
  highlight(0, 'nasmComment',           { link = "Comment" })  -- ; comments
  highlight(0, 'nasmSpecialComment',    { link = "Comment" })  -- Special comments
  highlight(0, 'nasmInCommentTodo',     { link = "Comment" })  -- TODO in comments

  -- Errors
  highlight(0, 'nasmLabelError',        { fg = colors.red,        bg = 'NONE', undercurl = true })
  highlight(0, 'nasmNumberError',       { link = "Number" })
  highlight(0, 'nasmStringError',       { link = "String" })
  highlight(0, 'nasmTypeError',         { link = "Type" })
  highlight(0, 'nasmRegisterError',     { fg = colors.red,        bg = 'NONE', undercurl = true })
  highlight(0, 'nasmInstructnError',    { fg = colors.red,        bg = 'NONE', undercurl = true })
  highlight(0, 'nasmPreProcError',      { fg = colors.red,        bg = 'NONE', undercurl = true })
  highlight(0, 'nasmMemRefError',       { fg = colors.red,        bg = 'NONE', undercurl = true })


  -----------------------------------------------------------------------------
  -- MASM (Microsoft Macro Assembler) - x86/x64

  -- Labels
  highlight(0, 'masmLabel',             { fg = colors.orange,     bg = 'NONE' })  -- Labels
  highlight(0, 'masmIdentifier',        { fg = colors.white,      bg = 'NONE' })  -- Identifiers

  -- Instructions
  highlight(0, 'masmOpcode',            { fg = colors.pink,       bg = 'NONE' })  -- CPU opcodes
  highlight(0, 'masmOpFloat',           { fg = colors.pink,       bg = 'NONE' })  -- FPU opcodes

  -- Directives
  highlight(0, 'masmDirective',         { fg = colors.blue,       bg = 'NONE' })  -- Directives

  -- Types/Operators
  highlight(0, 'masmType',              { link = "Type" })  -- db, dw, dd, dq, byte, word, etc.
  highlight(0, 'masmOperator',          { link = "Operator" })  -- Operators

  -- Registers
  highlight(0, 'masmRegister',          { fg = colors.purple,     bg = 'NONE' })  -- Registers

  -- Options
  highlight(0, 'masmOption',            { fg = colors.blue,       bg = 'NONE' })  -- OPTION directive
  highlight(0, 'masmOptionOpt',         { fg = colors.purple,     bg = 'NONE' })  -- Option values
  highlight(0, 'masmContextOpt',        { fg = colors.purple,     bg = 'NONE' })  -- Context options
  highlight(0, 'masmModelOpt',          { fg = colors.purple,     bg = 'NONE' })  -- MODEL options
  highlight(0, 'masmSegmentOpt',        { fg = colors.purple,     bg = 'NONE' })  -- SEGMENT options
  highlight(0, 'masmProcOpt',           { fg = colors.purple,     bg = 'NONE' })  -- PROC options
  highlight(0, 'masmAssumeOpt',         { fg = colors.purple,     bg = 'NONE' })  -- ASSUME options
  highlight(0, 'masmExpression',        { fg = colors.white,      bg = 'NONE' })  -- Expressions

  -- Numbers
  highlight(0, 'masmDecimal',           { fg = colors.greenLight, bg = 'NONE' })  -- Decimal
  highlight(0, 'masmBinary',            { fg = colors.greenLight, bg = 'NONE' })  -- Binary
  highlight(0, 'masmOctal',             { fg = colors.greenLight, bg = 'NONE' })  -- Octal
  highlight(0, 'masmHexadecimal',       { fg = colors.greenLight, bg = 'NONE' })  -- Hexadecimal
  highlight(0, 'masmFloatRaw',          { fg = colors.greenLight, bg = 'NONE' })  -- Raw float
  highlight(0, 'masmFloat',             { fg = colors.greenLight, bg = 'NONE' })  -- Float

  -- Strings
  highlight(0, 'masmString',            { link = "String" })  -- Strings

  -- Comments
  highlight(0, 'masmComment',           { link = "Comment" })  -- ; comments

  -- Title/Text areas
  highlight(0, 'masmTitle',             { fg = colors.redLight,   bg = 'NONE' })  -- TITLE directive content
  highlight(0, 'masmTitleArea',         { fg = colors.redLight,   bg = 'NONE' })  -- Title area
  highlight(0, 'masmText',              { fg = colors.redLight,   bg = 'NONE' })  -- TEXT content
  highlight(0, 'masmTextArea',          { fg = colors.redLight,   bg = 'NONE' })  -- Text area


  -----------------------------------------------------------------------------
  -- FASM (Flat Assembler) - x86/x64

  -- Labels
  highlight(0, 'fasmLabel',             { fg = colors.orange,     bg = 'NONE' })  -- Labels

  -- Instructions
  highlight(0, 'fasmInstr',             { fg = colors.pink,       bg = 'NONE' })  -- Instructions

  -- Directives
  highlight(0, 'fasmDirective',         { fg = colors.blue,       bg = 'NONE' })  -- Directives
  highlight(0, 'fasmPreprocess',        { fg = colors.pink,       bg = 'NONE' })  -- Preprocessor

  -- Types/Data
  highlight(0, 'fasmDataDirectives',    { fg = colors.turquoise,  bg = 'NONE' })  -- db, dw, dd, dq, rb, rw, etc.
  highlight(0, 'fasmAddressSizes',      { fg = colors.turquoise,  bg = 'NONE' })  -- byte, word, dword, qword, etc.

  -- Registers
  highlight(0, 'fasmRegister',          { fg = colors.purple,     bg = 'NONE' })  -- Registers

  -- Operators
  highlight(0, 'fasmOperator',          { link = "Operator" })  -- Operators
  highlight(0, 'fasmNumericOperator',   { link = "Operator" })  -- Numeric operators
  highlight(0, 'fasmLogicalOperator',   { link = "Operator" })  -- Logical operators

  -- Numbers
  highlight(0, 'fasmBinaryNumber',      { link = "Number" })  -- Binary
  highlight(0, 'fasmOctalNumber',       { link = "Number" })  -- Octal
  highlight(0, 'fasmDecimalNumber',     { link = "Number" })  -- Decimal
  highlight(0, 'fasmHexNumber',         { link = "Number" })  -- Hexadecimal
  highlight(0, 'fasmFPUNumber',         { link = "Number" })  -- FPU numbers

  -- Strings
  highlight(0, 'fasmString',            { link = "String" })  -- Strings

  -- Comments
  highlight(0, 'fasmComment',           { link = "Comment" })  -- ; comments

  -- Special
  highlight(0, 'fasmSymbol',            { fg = colors.white,      bg = 'NONE' })  -- Symbols
  highlight(0, 'fasmSpecial',           { fg = colors.pink,       bg = 'NONE' })  -- Special


  -----------------------------------------------------------------------------
  -- TASM (Turbo Assembler) - x86

  -- Labels
  highlight(0, 'tasmLabel',             { fg = colors.orange,     bg = 'NONE' })  -- Labels

  -- Instructions
  highlight(0, 'tasmInstruction',       { fg = colors.pink,       bg = 'NONE' })  -- CPU instructions
  highlight(0, 'tasmCoprocInstr',       { fg = colors.pink,       bg = 'NONE' })  -- FPU instructions
  highlight(0, 'tasmMMXinst',           { fg = colors.pink,       bg = 'NONE' })  -- MMX instructions

  -- Directives
  highlight(0, 'tasmDirective',         { fg = colors.blue,       bg = 'NONE' })  -- Directives

  -- Registers
  highlight(0, 'tasmRegister',          { fg = colors.purple,     bg = 'NONE' })  -- Registers

  -- Numbers
  highlight(0, 'tasmDec',               { fg = colors.greenLight, bg = 'NONE' })  -- Decimal
  highlight(0, 'tasmHex',               { fg = colors.greenLight, bg = 'NONE' })  -- Hexadecimal
  highlight(0, 'tasmOct',               { fg = colors.greenLight, bg = 'NONE' })  -- Octal
  highlight(0, 'tasmBin',               { fg = colors.greenLight, bg = 'NONE' })  -- Binary

  -- Strings
  highlight(0, 'tasmString',            { link = "String" })  -- Strings

  -- Comments
  highlight(0, 'tasmComment',           { link = "Comment" })  -- ; comments


  -----------------------------------------------------------------------------
  -- Motorola 68000 Assembly

  -- Labels
  highlight(0, 'asm68kLabel',           { fg = colors.orange,     bg = 'NONE' })  -- Labels

  -- Instructions
  highlight(0, 'asm68kOpcode',          { fg = colors.pink,       bg = 'NONE' })  -- Opcodes

  -- Directives
  highlight(0, 'asm68kDirective',       { fg = colors.blue,       bg = 'NONE' })  -- Directives
  highlight(0, 'asm68kCond',            { fg = colors.blue,       bg = 'NONE' })  -- Conditional
  highlight(0, 'asm68kRepeat',          { fg = colors.blue,       bg = 'NONE' })  -- Repeat/loop
  highlight(0, 'asm68kMacro',           { fg = colors.pink,       bg = 'NONE' })  -- Macros
  highlight(0, 'asm68kPreCond',         { fg = colors.pink,       bg = 'NONE' })  -- Conditional assembly

  -- Registers (d0-d7, a0-a7, sp, pc, sr, ccr, etc.)
  highlight(0, 'asm68kReg',             { fg = colors.purple,     bg = 'NONE' })  -- Registers

  -- Numbers
  highlight(0, 'asm68kImmediate',       { fg = colors.greenLight, bg = 'NONE' })  -- Immediate data (#)

  -- Strings
  highlight(0, 'asm68kString',          { link = "String" })  -- Strings

  -- Comments
  highlight(0, 'asm68kComment',         { link = "Comment" })  -- ; or * comments

  -- Operators
  highlight(0, 'asm68kOperator',        { link = "Operator" })  -- Operators


  -----------------------------------------------------------------------------
  -- Z80 Assembly

  -- Labels
  highlight(0, 'z8aLabel',              { fg = colors.orange,     bg = 'NONE' })  -- Labels
  highlight(0, 'z8aSpecialLabel',       { fg = colors.orange,     bg = 'NONE' })  -- Special labels

  -- Instructions
  highlight(0, 'z8aInstruction',        { fg = colors.pink,       bg = 'NONE' })  -- Instructions
  highlight(0, 'z8aSpecInst',           { fg = colors.pink,       bg = 'NONE' })  -- Special instructions

  -- Directives
  highlight(0, 'z8aPreProc',            { fg = colors.pink,       bg = 'NONE' })  -- Preprocessor
  highlight(0, 'z8aInclude',            { fg = colors.pink,       bg = 'NONE' })  -- .include
  highlight(0, 'z8aPreCondit',          { fg = colors.pink,       bg = 'NONE' })  -- .if, .else, .endif

  -- Identifiers
  highlight(0, 'z8aIdentifier',         { fg = colors.white,      bg = 'NONE' })  -- Identifiers

  -- Numbers
  highlight(0, 'z8aNumber',             { link = "Number" })  -- Numbers

  -- Strings
  highlight(0, 'z8aString',             { link = "String" })  -- Strings

  -- Comments
  highlight(0, 'z8aComment',            { link = "Comment" })  -- ; comments


  -----------------------------------------------------------------------------
  -- IA-64 (Itanium) Assembly

  -- Labels
  highlight(0, 'ia64Label',             { fg = colors.orange,     bg = 'NONE' })  -- Labels

  -- Instructions
  highlight(0, 'ia64opcode',            { fg = colors.pink,       bg = 'NONE' })  -- Opcodes

  -- Directives
  highlight(0, 'ia64Directive',         { fg = colors.blue,       bg = 'NONE' })  -- Directives

  -- Registers (r0-r127, f0-f127, b0-b7, p0-p63, ar.*, cr.*, etc.)
  highlight(0, 'ia64registers',         { fg = colors.purple,     bg = 'NONE' })  -- Registers

  -- Data types
  highlight(0, 'ia64data',              { fg = colors.turquoise,  bg = 'NONE' })  -- Data types

  -- Numbers
  highlight(0, 'ia64Decimal',           { fg = colors.greenLight, bg = 'NONE' })  -- Decimal
  highlight(0, 'ia64Octal',             { fg = colors.greenLight, bg = 'NONE' })  -- Octal
  highlight(0, 'ia64Binary',            { fg = colors.greenLight, bg = 'NONE' })  -- Binary
  highlight(0, 'ia64Hex',               { fg = colors.greenLight, bg = 'NONE' })  -- Hexadecimal
  highlight(0, 'ia64Float',             { fg = colors.greenLight, bg = 'NONE' })  -- Float

  -- Strings
  highlight(0, 'ia64string',            { link = "String" })  -- Strings

  -- Comments
  highlight(0, 'ia64Comment',           { link = "Comment" })  -- // and /* */ comments
  highlight(0, 'ia64Todo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO

  -- Operators
  highlight(0, 'ia64delimiter',         { link = "Delimiter" })  -- Delimiters
  highlight(0, 'ia64operator',          { link = "Operator" })  -- Operators

  -- Identifiers
  highlight(0, 'ia64Identifier',        { fg = colors.white,      bg = 'NONE' })  -- Identifiers


  -----------------------------------------------------------------------------
  -- H8300 Assembly

  highlight(0, 'asmh8300Opcode',        { fg = colors.pink,       bg = 'NONE' })  -- Opcodes
  highlight(0, 'asmh8300Register',      { fg = colors.purple,     bg = 'NONE' })  -- Registers
  highlight(0, 'asmh8300Directive',     { fg = colors.blue,       bg = 'NONE' })  -- Directives


  -----------------------------------------------------------------------------
  -- ARM Assembly

  -- Labels
  highlight(0, 'armLabel',              { fg = colors.orange,     bg = 'NONE' })  -- Labels

  -- Instructions
  highlight(0, 'armOpcode',             { fg = colors.pink,       bg = 'NONE' })  -- Opcodes
  highlight(0, 'armInstruction',        { fg = colors.pink,       bg = 'NONE' })  -- Instructions
  highlight(0, 'armCondition',          { fg = colors.pink,       bg = 'NONE' })  -- Condition codes (eq, ne, cs, cc, etc.)

  -- Directives
  highlight(0, 'armDirective',          { fg = colors.blue,       bg = 'NONE' })  -- Directives
  highlight(0, 'armMacro',              { fg = colors.pink,       bg = 'NONE' })  -- Macros

  -- Registers (r0-r15, sp, lr, pc, cpsr, spsr, etc.)
  highlight(0, 'armRegister',           { fg = colors.purple,     bg = 'NONE' })  -- Registers

  -- Numbers
  highlight(0, 'armNumber',             { link = "Number" })  -- Numbers
  highlight(0, 'armImmediate',          { fg = colors.greenLight, bg = 'NONE' })  -- Immediate values (#)

  -- Strings
  highlight(0, 'armString',             { link = "String" })  -- Strings

  -- Comments
  highlight(0, 'armComment',            { link = "Comment" })  -- ; @ comments


  -----------------------------------------------------------------------------
  -- MIPS Assembly

  -- Labels
  highlight(0, 'mipsLabel',             { fg = colors.orange,     bg = 'NONE' })  -- Labels

  -- Instructions
  highlight(0, 'mipsInstruction',       { fg = colors.pink,       bg = 'NONE' })  -- Instructions

  -- Directives
  highlight(0, 'mipsDirective',         { fg = colors.blue,       bg = 'NONE' })  -- .text, .data, .globl, etc.

  -- Registers ($0-$31, $zero, $at, $v0-$v1, $a0-$a3, $t0-$t9, $s0-$s7, $gp, $sp, $fp, $ra)
  highlight(0, 'mipsRegister',          { fg = colors.purple,     bg = 'NONE' })  -- Registers

  -- Numbers
  highlight(0, 'mipsNumber',            { link = "Number" })  -- Numbers

  -- Strings
  highlight(0, 'mipsString',            { link = "String" })  -- Strings

  -- Comments
  highlight(0, 'mipsComment',           { link = "Comment" })  -- # comments


  -----------------------------------------------------------------------------
  -- RISC-V Assembly

  -- Labels
  highlight(0, 'riscvLabel',            { fg = colors.orange,     bg = 'NONE' })  -- Labels

  -- Instructions
  highlight(0, 'riscvInstruction',      { fg = colors.pink,       bg = 'NONE' })  -- Instructions
  highlight(0, 'riscvOpcode',           { fg = colors.pink,       bg = 'NONE' })  -- Opcodes

  -- Directives
  highlight(0, 'riscvDirective',        { fg = colors.blue,       bg = 'NONE' })  -- Directives

  -- Registers (x0-x31, zero, ra, sp, gp, tp, t0-t6, s0-s11, a0-a7, f0-f31, ft0-ft11, fs0-fs11, fa0-fa7)
  highlight(0, 'riscvRegister',         { fg = colors.purple,     bg = 'NONE' })  -- Registers

  -- Numbers
  highlight(0, 'riscvNumber',           { link = "Number" })  -- Numbers

  -- Strings
  highlight(0, 'riscvString',           { link = "String" })  -- Strings

  -- Comments
  highlight(0, 'riscvComment',          { link = "Comment" })  -- # comments


  -----------------------------------------------------------------------------
  -- x86/x64 Common Groups (used by multiple assemblers)

  -- General purpose registers (8-bit)
  highlight(0, 'asmReg8',               { fg = colors.purple,     bg = 'NONE' })  -- al, ah, bl, bh, cl, ch, dl, dh
  highlight(0, 'asmReg8Ex',             { fg = colors.purple,     bg = 'NONE' })  -- sil, dil, bpl, spl, r8b-r15b

  -- General purpose registers (16-bit)
  highlight(0, 'asmReg16',              { fg = colors.purple,     bg = 'NONE' })  -- ax, bx, cx, dx, si, di, bp, sp
  highlight(0, 'asmReg16Ex',            { fg = colors.purple,     bg = 'NONE' })  -- r8w-r15w

  -- General purpose registers (32-bit)
  highlight(0, 'asmReg32',              { fg = colors.purple,     bg = 'NONE' })  -- eax, ebx, ecx, edx, esi, edi, ebp, esp
  highlight(0, 'asmReg32Ex',            { fg = colors.purple,     bg = 'NONE' })  -- r8d-r15d

  -- General purpose registers (64-bit)
  highlight(0, 'asmReg64',              { fg = colors.purple,     bg = 'NONE' })  -- rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, r8-r15

  -- Segment registers
  highlight(0, 'asmSegReg',             { fg = colors.purple,     bg = 'NONE' })  -- cs, ds, es, fs, gs, ss

  -- Control registers
  highlight(0, 'asmCtrlReg',            { fg = colors.purple,     bg = 'NONE' })  -- cr0-cr15

  -- Debug registers
  highlight(0, 'asmDbgReg',             { fg = colors.purple,     bg = 'NONE' })  -- dr0-dr15

  -- FPU registers
  highlight(0, 'asmFpuReg',             { fg = colors.purple,     bg = 'NONE' })  -- st0-st7

  -- MMX registers
  highlight(0, 'asmMmxReg',             { fg = colors.purple,     bg = 'NONE' })  -- mm0-mm7

  -- SSE/AVX registers
  highlight(0, 'asmXmmReg',             { fg = colors.purple,     bg = 'NONE' })  -- xmm0-xmm31
  highlight(0, 'asmYmmReg',             { fg = colors.purple,     bg = 'NONE' })  -- ymm0-ymm31
  highlight(0, 'asmZmmReg',             { fg = colors.purple,     bg = 'NONE' })  -- zmm0-zmm31

  -- Mask registers (AVX-512)
  highlight(0, 'asmKReg',               { fg = colors.purple,     bg = 'NONE' })  -- k0-k7

  -- Size specifiers
  highlight(0, 'asmSize',               { fg = colors.turquoise,  bg = 'NONE' })  -- byte, word, dword, qword, oword, xword, yword, zword

  -- Memory operands
  highlight(0, 'asmMemRef',             { fg = colors.white,      bg = 'NONE' })  -- [mem] references


  -----------------------------------------------------------------------------
  -- Common x86/x64 Instructions (grouped by category)

  -- Data movement
  highlight(0, 'asmMovInstr',           { fg = colors.pink,       bg = 'NONE' })  -- mov, movzx, movsx, lea, xchg, push, pop

  -- Arithmetic
  highlight(0, 'asmArithInstr',         { fg = colors.pink,       bg = 'NONE' })  -- add, sub, mul, imul, div, idiv, inc, dec, neg

  -- Logical
  highlight(0, 'asmLogicInstr',         { fg = colors.pink,       bg = 'NONE' })  -- and, or, xor, not, shl, shr, sal, sar, rol, ror

  -- Comparison
  highlight(0, 'asmCmpInstr',           { fg = colors.pink,       bg = 'NONE' })  -- cmp, test

  -- Control flow
  highlight(0, 'asmJmpInstr',           { fg = colors.pink,       bg = 'NONE' })  -- jmp, call, ret, jz, jnz, je, jne, jg, jl, etc.

  -- Stack
  highlight(0, 'asmStackInstr',         { fg = colors.pink,       bg = 'NONE' })  -- push, pop, pusha, popa, enter, leave

  -- String operations
  highlight(0, 'asmStringInstr',        { link = "String" })  -- movs, cmps, scas, lods, stos, rep, repz, repnz

  -- FPU instructions
  highlight(0, 'asmFpuInstr',           { fg = colors.pink,       bg = 'NONE' })  -- fld, fst, fadd, fsub, fmul, fdiv, fsin, fcos, fsqrt

  -- SSE instructions
  highlight(0, 'asmSseInstr',           { fg = colors.pink,       bg = 'NONE' })  -- movaps, movups, addps, subps, mulps, divps, etc.

  -- AVX instructions
  highlight(0, 'asmAvxInstr',           { fg = colors.pink,       bg = 'NONE' })  -- vaddps, vsubps, vmulps, vdivps, etc.

  -- System instructions
  highlight(0, 'asmSysInstr',           { fg = colors.pink,       bg = 'NONE' })  -- int, syscall, sysret, cpuid, rdtsc, etc.


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.asm)

  -- Labels
  highlight(0, '@label.asm',                   { fg = colors.orange,     bg = 'NONE' })  -- Labels

  -- Variables/Registers
  highlight(0, '@variable.asm',                { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.asm',        { link = "Variable" })  -- Registers

  -- Functions
  highlight(0, '@function.asm',                { link = "Function" })  -- Labels as functions
  highlight(0, '@function.builtin.asm',        { link = "Function" })  -- Instructions/meta

  -- Constants
  highlight(0, '@constant.asm',                { link = "Constant" })  -- Named constants

  -- Keywords
  highlight(0, '@keyword.asm',                 { link = "Keyword" })  -- byte, word, dword, ptr, etc.
  highlight(0, '@keyword.directive.asm',       { link = "Keyword" })  -- Directives

  -- Operators
  highlight(0, '@operator.asm',                { link = "Operator" })  -- +, -, *, /, etc.

  -- Numbers
  highlight(0, '@number.asm',                  { link = "Number" })  -- Numbers
  highlight(0, '@number.float.asm',            { link = "Number" })  -- Floats

  -- Strings
  highlight(0, '@string.asm',                  { link = "String" })  -- Strings

  -- Comments
  highlight(0, '@comment.asm',                 { link = "Comment" })  -- Comments
  highlight(0, '@spell.asm',                   { link = "Normal" })  -- Spell check in comments

  -- Punctuation
  highlight(0, '@punctuation.bracket.asm',     { link = "Normal" })  -- [], ()
  highlight(0, '@punctuation.delimiter.asm',   { link = "Delimiter" })  -- , :


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.asm)

  highlight(0, '@lsp.type.variable.asm',       { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.asm',      { link = "Variable" })  -- Parameters
  highlight(0, '@lsp.type.function.asm',       { link = "Function" })  -- Labels
  highlight(0, '@lsp.type.keyword.asm',        { link = "Keyword" })  -- Instructions
  highlight(0, '@lsp.type.macro.asm',          { fg = colors.pink,       bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.type.asm',           { link = "Type"})  -- Types/sizes
  highlight(0, '@lsp.type.operator.asm',       { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.asm',         { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.asm',         { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.comment.asm',        { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.register.asm',       { fg = colors.purple,     bg = 'NONE' })  -- Registers


  -----------------------------------------------------------------------------
  -- Additional Architecture-Specific Groups

  -- PowerPC Assembly
  highlight(0, 'ppcOpcode',             { fg = colors.pink,       bg = 'NONE' })  -- PowerPC opcodes
  highlight(0, 'ppcRegister',           { fg = colors.purple,     bg = 'NONE' })  -- r0-r31, f0-f31
  highlight(0, 'ppcDirective',          { link = "Keyword"})  -- Directives
  highlight(0, 'ppcComment',            { link = "Comment" })  -- Comments

  -- SPARC Assembly
  highlight(0, 'sparcOpcode',           { fg = colors.pink,       bg = 'NONE' })  -- SPARC opcodes
  highlight(0, 'sparcRegister',         { fg = colors.purple,     bg = 'NONE' })  -- %g0-%g7, %o0-%o7, %l0-%l7, %i0-%i7
  highlight(0, 'sparcDirective',        { link = "Keyword"})  -- Directives
  highlight(0, 'sparcComment',          { link = "Comment" })  -- ! comments
  -- AVR Assembly
  highlight(0, 'avrOpcode',             { fg = colors.pink,       bg = 'NONE' })  -- AVR opcodes
  highlight(0, 'avrRegister',           { fg = colors.purple,     bg = 'NONE' })  -- r0-r31
  highlight(0, 'avrDirective',          { link = "Keyword" })  -- Directives
  highlight(0, 'avrComment',            { link = "Comment" })  -- ; comments

  -- 6502 Assembly
  highlight(0, 'asm6502Opcode',         { fg = colors.pink,       bg = 'NONE' })  -- 6502 opcodes
  highlight(0, 'asm6502Register',       { fg = colors.purple,     bg = 'NONE' })  -- A, X, Y
  highlight(0, 'asm6502Directive',      { link = "Keyword"})  -- Directives
  highlight(0, 'asm6502Comment',        { link = "Comment" })  -- ; comments

  -- 8051 Assembly
  highlight(0, 'asm8051Opcode',         { fg = colors.pink,       bg = 'NONE' })  -- 8051 opcodes
  highlight(0, 'asm8051Register',       { fg = colors.purple,     bg = 'NONE' })  -- R0-R7, A, B, DPTR
  highlight(0, 'asm8051Directive',      { link = "Keyword" })  -- Directives
  highlight(0, 'asm8051Comment',        { link = "Comment" })  -- ; comments

  -- PIC Assembly
  highlight(0, 'picOpcode',             { fg = colors.pink,       bg = 'NONE' })  -- PIC opcodes
  highlight(0, 'picRegister',           { fg = colors.purple,     bg = 'NONE' })  -- W, F, registers
  highlight(0, 'picDirective',          { link = "Keyword" })  -- Directives
  highlight(0, 'picComment',            { link = "Comment" })  -- ; comments


  -----------------------------------------------------------------------------
  -- WebAssembly Text Format (WAT) - included here for completeness

  -- Keywords
  highlight(0, 'watKeyword',            { link = "Keyword" })  -- module, func, param, result, local, etc.
  highlight(0, 'watInstruction',        { link = "Keyword" })  -- i32.add, f64.mul, call, etc.

  -- Types
  highlight(0, 'watType',               { link = "Type" })  -- i32, i64, f32, f64, funcref, externref

  -- Identifiers
  highlight(0, 'watIdentifier',         { fg = colors.orange,     bg = 'NONE' })  -- $func_name, $local_name

  -- Numbers
  highlight(0, 'watNumber',             { link = "Number" })  -- Numbers

  -- Strings
  highlight(0, 'watString',             { link = "String" })  -- Strings

  -- Comments
  highlight(0, 'watComment',            { link = "Comment" })  -- ;; comments
  highlight(0, 'watBlockComment',       { link = "Comment" })  -- (; ;) block comments

end

return asm
