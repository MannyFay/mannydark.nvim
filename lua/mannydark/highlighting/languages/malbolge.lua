-------------------------------------------------------------------------------
-- Malbolge Files
-- Highlighting for .mal, .malbolge files.
-- Malbolge is an esoteric language designed by Ben Olmstead in 1998 to be
-- as difficult to program in as possible. Named after the 8th circle of Hell.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local malbolge = {}


-------------------------------------------------------------------------------
-- Settings

malbolge.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)
  -- Malbolge has 8 instructions determined by (C + [C]) % 94
  -- Source uses encrypted ASCII (33-126), shown here in normalized form

  -- Jump Instruction (opcode 4)
  -- i: C = [D] - Set code pointer to value at data pointer
  highlight(0, 'malbolgeJump',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- i - unconditional jump
  highlight(0, 'malbolgeJumpNorm',             { fg = colors.turquoise,  bg = 'NONE', bold = true })  -- Normalized 'i'

  -- Output Instruction (opcode 5)
  -- <: PRINT(A % 256) - Output accumulator as ASCII character
  highlight(0, 'malbolgeOutput',               { fg = colors.orange,     bg = 'NONE'            })  -- < - output character
  highlight(0, 'malbolgeOutputNorm',           { fg = colors.orange,     bg = 'NONE', bold = true })  -- Normalized '<'

  -- Rotate Right / Input Instruction (opcode 23)
  -- /: A = [D] = ROTATE_RIGHT([D]) - Tritwise rotate right
  highlight(0, 'malbolgeRotate',               { fg = colors.green,      bg = 'NONE'            })  -- / - rotate right
  highlight(0, 'malbolgeInput',                { fg = colors.purple,     bg = 'NONE'            })  -- Input operation
  highlight(0, 'malbolgeRotateNorm',           { fg = colors.green,      bg = 'NONE', bold = true })  -- Normalized '/'

  -- Move Data Pointer Instruction (opcode 39)
  -- *: D = [D] - Set data pointer to value at data pointer
  highlight(0, 'malbolgeMoveD',                { fg = colors.blue,       bg = 'NONE'            })  -- * - move data pointer
  highlight(0, 'malbolgeMoveDNorm',            { fg = colors.blue,       bg = 'NONE', bold = true })  -- Normalized '*'

  -- Crazy Operation Instruction (opcode 40)
  -- j: A = [D] = CRAZY_OP(A, [D]) - Tritwise "crazy" operation
  highlight(0, 'malbolgeCrazy',                { fg = colors.pink,       bg = 'NONE'            })  -- j - crazy operation
  highlight(0, 'malbolgeCrazyNorm',            { fg = colors.pink,       bg = 'NONE', bold = true })  -- Normalized 'j'

  -- NOP Instruction (opcode 62)
  -- p: No operation
  highlight(0, 'malbolgeNop',                  { fg = colors.gray,       bg = 'NONE'            })  -- p - no operation
  highlight(0, 'malbolgeNopNorm',              { fg = colors.gray,       bg = 'NONE'            })  -- Normalized 'p'

  -- Halt Instruction (opcode 68)
  -- o: Stop execution
  highlight(0, 'malbolgeHalt',                 { fg = colors.red,        bg = 'NONE', bold = true })  -- o - halt/stop
  highlight(0, 'malbolgeHaltNorm',             { fg = colors.red,        bg = 'NONE', bold = true })  -- Normalized 'o'

  -- Unused/Invalid Instruction (opcode 81)
  -- v: Invalid or special (varies by implementation)
  highlight(0, 'malbolgeInvalid',              { fg = colors.red,        bg = 'NONE', undercurl = true })  -- v - invalid
  highlight(0, 'malbolgeReserved',             { fg = colors.gray,       bg = 'NONE'            })  -- Reserved opcode

  -- Generic Instruction Groups
  highlight(0, 'malbolgeInstruction',          { fg = colors.white,      bg = 'NONE'            })  -- Any instruction
  highlight(0, 'malbolgeInstructionEncrypted', { fg = colors.white,      bg = colors.grayBlue  })  -- Encrypted form
  highlight(0, 'malbolgeInstructionNormalized',{ fg = colors.white,      bg = 'NONE', bold = true })  -- Normalized form


  -----------------------------------------------------------------------------
  -- Encrypted Source Characters
  -- Malbolge source uses ASCII 33-126, encrypted based on position
  -- These are the actual characters appearing in .mal files

  highlight(0, 'malbolgeSourceChar',           { fg = colors.white,      bg = 'NONE'            })  -- Any source character
  highlight(0, 'malbolgeSourcePrintable',      { fg = colors.white,      bg = 'NONE'            })  -- Printable ASCII (33-126)
  highlight(0, 'malbolgeSourceDigit',          { fg = colors.greenLight, bg = 'NONE'            })  -- 0-9 in source
  highlight(0, 'malbolgeSourceUpper',          { fg = colors.blue,       bg = 'NONE'            })  -- A-Z in source
  highlight(0, 'malbolgeSourceLower',          { fg = colors.turquoise,  bg = 'NONE'            })  -- a-z in source
  highlight(0, 'malbolgeSourceSymbol',         { fg = colors.orange,     bg = 'NONE'            })  -- Symbols in source
  highlight(0, 'malbolgeSourceSpecial',        { fg = colors.pink,       bg = 'NONE'            })  -- Special chars

  -- Common encrypted patterns (position-dependent)
  highlight(0, 'malbolgeEncryptedJump',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Decrypts to jump
  highlight(0, 'malbolgeEncryptedOutput',      { fg = colors.orange,     bg = 'NONE'            })  -- Decrypts to output
  highlight(0, 'malbolgeEncryptedRotate',      { fg = colors.green,      bg = 'NONE'            })  -- Decrypts to rotate
  highlight(0, 'malbolgeEncryptedMoveD',       { fg = colors.blue,       bg = 'NONE'            })  -- Decrypts to move D
  highlight(0, 'malbolgeEncryptedCrazy',       { fg = colors.pink,       bg = 'NONE'            })  -- Decrypts to crazy
  highlight(0, 'malbolgeEncryptedNop',         { fg = colors.gray,       bg = 'NONE'            })  -- Decrypts to NOP
  highlight(0, 'malbolgeEncryptedHalt',        { fg = colors.red,        bg = 'NONE'            })  -- Decrypts to halt


  -----------------------------------------------------------------------------
  -- Registers (A, C, D)

  highlight(0, 'malbolgeRegister',             { fg = colors.purple,     bg = 'NONE', bold = true })  -- Any register
  highlight(0, 'malbolgeRegisterA',            { fg = colors.purple,     bg = 'NONE', bold = true })  -- A - Accumulator
  highlight(0, 'malbolgeRegisterC',            { fg = colors.yellow,     bg = 'NONE', bold = true })  -- C - Code pointer
  highlight(0, 'malbolgeRegisterD',            { fg = colors.blue,       bg = 'NONE', bold = true })  -- D - Data pointer


  -----------------------------------------------------------------------------
  -- Memory Model
  -- 59049 words (3^10), each word is 10 trits (0-59048)

  highlight(0, 'malbolgeMemory',               { fg = colors.white,      bg = 'NONE'            })  -- Memory reference
  highlight(0, 'malbolgeMemoryAddress',        { fg = colors.greenLight, bg = 'NONE'            })  -- Memory address
  highlight(0, 'malbolgeMemoryValue',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Memory value
  highlight(0, 'malbolgeMemoryBracket',        { fg = colors.gray,       bg = 'NONE'            })  -- [D] brackets


  -----------------------------------------------------------------------------
  -- Ternary (Base-3) Numbers
  -- Malbolge uses trits (0, 1, 2) instead of bits

  highlight(0, 'malbolgeTrit',                 { fg = colors.greenLight, bg = 'NONE'            })  -- Single trit (0, 1, 2)
  highlight(0, 'malbolgeTrit0',                { fg = colors.gray,       bg = 'NONE'            })  -- Trit value 0
  highlight(0, 'malbolgeTrit1',                { fg = colors.green,      bg = 'NONE'            })  -- Trit value 1
  highlight(0, 'malbolgeTrit2',                { fg = colors.greenLight, bg = 'NONE'            })  -- Trit value 2
  highlight(0, 'malbolgeTernary',              { fg = colors.greenLight, bg = 'NONE'            })  -- Ternary number
  highlight(0, 'malbolgeDecimal',              { fg = colors.greenLight, bg = 'NONE'            })  -- Decimal equivalent


  -----------------------------------------------------------------------------
  -- Crazy Operation Table Visualization
  -- The tritwise operation that makes Malbolge so confusing

  highlight(0, 'malbolgeCrazyTable',           { fg = colors.pink,       bg = 'NONE'            })  -- Crazy op table
  highlight(0, 'malbolgeCrazyInput',           { fg = colors.white,      bg = 'NONE'            })  -- Input trits
  highlight(0, 'malbolgeCrazyOutput',          { fg = colors.pink,       bg = 'NONE'            })  -- Output trit
  highlight(0, 'malbolgeCrazyHeader',          { fg = colors.gray,       bg = 'NONE', bold = true })  -- Table header


  -----------------------------------------------------------------------------
  -- Encryption/Decryption Tables
  -- Translation table applied after each instruction

  highlight(0, 'malbolgeEncryptTable',         { fg = colors.orange,     bg = 'NONE'            })  -- Encryption table
  highlight(0, 'malbolgeDecryptTable',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Decryption table
  highlight(0, 'malbolgeTableOriginal',        { fg = colors.white,      bg = 'NONE'            })  -- Original character
  highlight(0, 'malbolgeTableTranslated',      { fg = colors.yellow,     bg = 'NONE'            })  -- Translated character


  -----------------------------------------------------------------------------
  -- Whitespace and Comments
  -- Whitespace is ignored in Malbolge (skipped during load)

  highlight(0, 'malbolgeWhitespace',           { fg = 'NONE',            bg = 'NONE'            })  -- Ignored whitespace
  highlight(0, 'malbolgeNewline',              { fg = 'NONE',            bg = 'NONE'            })  -- Ignored newline
  highlight(0, 'malbolgeComment',              { fg = colors.red,        bg = 'NONE'            })  -- Comment (if supported)


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.malbolge)

  -- Instructions
  highlight(0, '@keyword.malbolge',                    { fg = colors.blue,       bg = 'NONE' })  -- General instruction
  highlight(0, '@keyword.control.malbolge',            { fg = colors.turquoise,  bg = 'NONE' })  -- Jump instruction
  highlight(0, '@keyword.operator.malbolge',           { fg = colors.pink,       bg = 'NONE' })  -- Crazy operation
  highlight(0, '@keyword.return.malbolge',             { fg = colors.red,        bg = 'NONE', bold = true })  -- Halt

  -- I/O
  highlight(0, '@function.malbolge',                   { fg = colors.orange,     bg = 'NONE' })  -- Output
  highlight(0, '@function.builtin.malbolge',           { fg = colors.purple,     bg = 'NONE' })  -- Input

  -- Data Operations
  highlight(0, '@operator.malbolge',                   { fg = colors.green,      bg = 'NONE' })  -- Rotate
  highlight(0, '@variable.malbolge',                   { fg = colors.blue,       bg = 'NONE' })  -- Data pointer ops

  -- Numbers
  highlight(0, '@number.malbolge',                     { fg = colors.greenLight, bg = 'NONE' })  -- Ternary numbers

  -- NOP
  highlight(0, '@comment.malbolge',                    { fg = colors.gray,       bg = 'NONE' })  -- NOP / unused

  -- Registers
  highlight(0, '@variable.builtin.malbolge',           { fg = colors.purple,     bg = 'NONE', bold = true })  -- Registers


  -----------------------------------------------------------------------------
  -- Debug/Trace View Highlighting
  -- For debuggers and execution visualization

  highlight(0, 'malbolgeDbgInstruction',       { fg = colors.white,      bg = 'NONE', bold = true })  -- Current instruction
  highlight(0, 'malbolgeDbgCurrent',           { fg = colors.white,      bg = colors.blueLink   })  -- Current position
  highlight(0, 'malbolgeDbgBreakpoint',        { fg = colors.white,      bg = colors.red        })  -- Breakpoint
  highlight(0, 'malbolgeDbgRegA',              { fg = colors.purple,     bg = 'NONE'            })  -- Accumulator value
  highlight(0, 'malbolgeDbgRegC',              { fg = colors.yellow,     bg = 'NONE'            })  -- Code pointer value
  highlight(0, 'malbolgeDbgRegD',              { fg = colors.blue,       bg = 'NONE'            })  -- Data pointer value
  highlight(0, 'malbolgeDbgMemory',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Memory dump
  highlight(0, 'malbolgeDbgOutput',            { fg = colors.orange,     bg = 'NONE'            })  -- Program output
  highlight(0, 'malbolgeDbgInput',             { fg = colors.purple,     bg = 'NONE'            })  -- Program input
  highlight(0, 'malbolgeDbgStep',              { fg = colors.green,      bg = 'NONE'            })  -- Step indicator
  highlight(0, 'malbolgeDbgEncryption',        { fg = colors.pink,       bg = 'NONE'            })  -- Post-encryption state


  -----------------------------------------------------------------------------
  -- Normalized Malbolge (HeLL - Human-Editable Low-Level)
  -- A more readable representation using normalized opcodes

  highlight(0, 'hellInstruction',              { fg = colors.white,      bg = 'NONE', bold = true })  -- HeLL instruction
  highlight(0, 'hellJump',                     { fg = colors.turquoise,  bg = 'NONE', bold = true })  -- i - jump
  highlight(0, 'hellOutput',                   { fg = colors.orange,     bg = 'NONE', bold = true })  -- < - output
  highlight(0, 'hellRotate',                   { fg = colors.green,      bg = 'NONE', bold = true })  -- / - rotate
  highlight(0, 'hellMoveD',                    { fg = colors.blue,       bg = 'NONE', bold = true })  -- * - move D
  highlight(0, 'hellCrazy',                    { fg = colors.pink,       bg = 'NONE', bold = true })  -- j - crazy
  highlight(0, 'hellNop',                      { fg = colors.gray,       bg = 'NONE'            })  -- p - NOP
  highlight(0, 'hellHalt',                     { fg = colors.red,        bg = 'NONE', bold = true })  -- o - halt
  highlight(0, 'hellComment',                  { fg = colors.red,        bg = 'NONE'            })  -- Comment
  highlight(0, 'hellLabel',                    { fg = colors.yellow,     bg = 'NONE', bold = true })  -- Label


  -----------------------------------------------------------------------------
  -- Malbolge Unshackled
  -- Extended version without the 59049 memory limit

  highlight(0, 'malbolgeUnshackledInstr',      { fg = colors.white,      bg = 'NONE'            })  -- Unshackled instruction
  highlight(0, 'malbolgeUnshackledAddress',    { fg = colors.greenLight, bg = 'NONE'            })  -- Extended address
  highlight(0, 'malbolgeUnshackledRotWidth',   { fg = colors.green,      bg = 'NONE'            })  -- Variable rotation width


  -----------------------------------------------------------------------------
  -- Error Highlighting

  highlight(0, 'malbolgeError',                { fg = colors.white,      bg = colors.red        })  -- Syntax error
  highlight(0, 'malbolgeInvalidChar',          { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Invalid character
  highlight(0, 'malbolgeOutOfRange',           { fg = colors.orange,     bg = 'NONE', undercurl = true })  -- Out of range (not 33-126)
  highlight(0, 'malbolgeInvalidOpcode',        { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Invalid opcode result


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.malbolge)

  highlight(0, '@lsp.type.function.malbolge',          { fg = colors.orange,     bg = 'NONE' })  -- I/O function
  highlight(0, '@lsp.type.keyword.malbolge',           { fg = colors.blue,       bg = 'NONE' })  -- Instruction
  highlight(0, '@lsp.type.operator.malbolge',          { fg = colors.pink,       bg = 'NONE' })  -- Operations
  highlight(0, '@lsp.type.variable.malbolge',          { fg = colors.purple,     bg = 'NONE' })  -- Register
  highlight(0, '@lsp.type.number.malbolge',            { fg = colors.greenLight, bg = 'NONE' })  -- Number


  -----------------------------------------------------------------------------
  -- Instruction Category Colors (for visual distinction)
  -- Each instruction type has a distinct color for easier reading

  highlight(0, 'MalbolgeI',                    { fg = colors.turquoise,  bg = 'NONE', bold = true })  -- i: Jump
  highlight(0, 'MalbolgeAngleBracket',         { fg = colors.orange,     bg = 'NONE', bold = true })  -- <: Output
  highlight(0, 'MalbolgeSlash',                { fg = colors.green,      bg = 'NONE', bold = true })  -- /: Rotate
  highlight(0, 'MalbolgeStar',                 { fg = colors.blue,       bg = 'NONE', bold = true })  -- *: Move D
  highlight(0, 'MalbolgeJ',                    { fg = colors.pink,       bg = 'NONE', bold = true })  -- j: Crazy
  highlight(0, 'MalbolgeP',                    { fg = colors.gray,       bg = 'NONE'            })  -- p: NOP
  highlight(0, 'MalbolgeO',                    { fg = colors.red,        bg = 'NONE', bold = true })  -- o: Halt
  highlight(0, 'MalbolgeV',                    { fg = colors.red,        bg = 'NONE', undercurl = true })  -- v: Invalid


  -----------------------------------------------------------------------------
  -- Filetype Links

  highlight(0, 'malInstr',             { link = 'malbolgeInstruction'     })
  highlight(0, 'malJump',              { link = 'malbolgeJump'            })
  highlight(0, 'malOutput',            { link = 'malbolgeOutput'          })
  highlight(0, 'malRotate',            { link = 'malbolgeRotate'          })
  highlight(0, 'malCrazy',             { link = 'malbolgeCrazy'           })
  highlight(0, 'malNop',               { link = 'malbolgeNop'             })
  highlight(0, 'malHalt',              { link = 'malbolgeHalt'            })
  highlight(0, 'malRegister',          { link = 'malbolgeRegister'        })
  highlight(0, 'malMemory',            { link = 'malbolgeMemory'          })
  highlight(0, 'malTrit',              { link = 'malbolgeTrit'            })
  highlight(0, 'malComment',           { link = 'malbolgeComment'         })
end

return malbolge
