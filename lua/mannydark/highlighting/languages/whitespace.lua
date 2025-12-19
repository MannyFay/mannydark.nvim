-------------------------------------------------------------------------------
-- Whitespace Files
-- Highlighting for .ws, .whitespace files.
-- Whitespace is an esoteric language where only space, tab, and linefeed
-- are significant. All other characters are ignored (comments).
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local whitespace = {}


-------------------------------------------------------------------------------
-- Settings

whitespace.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)
  -- Note: Since actual whitespace is invisible, these groups work with:
  -- 1. Visual representations (S/T/L or [Space]/[Tab]/[LF])
  -- 2. Whitespace assembly (human-readable mnemonics)
  -- 3. Special display modes that make whitespace visible

  -- IMP (Instruction Modification Parameters) - Category Indicators
  highlight(0, 'whitespaceIMP',                { fg = colors.pink,       bg = 'NONE', bold = true })  -- IMP prefix
  highlight(0, 'whitespaceIMPStack',           { fg = colors.blue,       bg = 'NONE', bold = true })  -- [Space] - Stack manipulation
  highlight(0, 'whitespaceIMPArith',           { fg = colors.green,      bg = 'NONE', bold = true })  -- [Tab][Space] - Arithmetic
  highlight(0, 'whitespaceIMPHeap',            { fg = colors.purple,     bg = 'NONE', bold = true })  -- [Tab][Tab] - Heap access
  highlight(0, 'whitespaceIMPFlow',            { fg = colors.turquoise,  bg = 'NONE', bold = true })  -- [LF] - Flow control
  highlight(0, 'whitespaceIMPIO',              { fg = colors.orange,     bg = 'NONE', bold = true })  -- [Tab][LF] - I/O

  -- Stack Manipulation Instructions (IMP: Space)
  highlight(0, 'whitespaceStackPush',          { fg = colors.blue,       bg = 'NONE'            })  -- [Space] Number - Push
  highlight(0, 'whitespaceStackDup',           { fg = colors.blue,       bg = 'NONE'            })  -- [LF][Space] - Duplicate top
  highlight(0, 'whitespaceStackCopy',          { fg = colors.blue,       bg = 'NONE'            })  -- [Tab][Space] Number - Copy nth
  highlight(0, 'whitespaceStackSwap',          { fg = colors.blue,       bg = 'NONE'            })  -- [LF][Tab] - Swap top two
  highlight(0, 'whitespaceStackDiscard',       { fg = colors.blue,       bg = 'NONE'            })  -- [LF][LF] - Discard top
  highlight(0, 'whitespaceStackSlide',         { fg = colors.blue,       bg = 'NONE'            })  -- [Tab][LF] Number - Slide n off
  highlight(0, 'whitespaceStack',              { fg = colors.blue,       bg = 'NONE'            })  -- Generic stack operation

  -- Arithmetic Instructions (IMP: Tab Space)
  highlight(0, 'whitespaceArithAdd',           { fg = colors.green,      bg = 'NONE'            })  -- [Space][Space] - Addition
  highlight(0, 'whitespaceArithSub',           { fg = colors.green,      bg = 'NONE'            })  -- [Space][Tab] - Subtraction
  highlight(0, 'whitespaceArithMul',           { fg = colors.green,      bg = 'NONE'            })  -- [Space][LF] - Multiplication
  highlight(0, 'whitespaceArithDiv',           { fg = colors.green,      bg = 'NONE'            })  -- [Tab][Space] - Integer Division
  highlight(0, 'whitespaceArithMod',           { fg = colors.green,      bg = 'NONE'            })  -- [Tab][Tab] - Modulo
  highlight(0, 'whitespaceArith',              { fg = colors.green,      bg = 'NONE'            })  -- Generic arithmetic

  -- Heap Access Instructions (IMP: Tab Tab)
  highlight(0, 'whitespaceHeapStore',          { fg = colors.purple,     bg = 'NONE'            })  -- [Space] - Store
  highlight(0, 'whitespaceHeapRetrieve',       { fg = colors.purple,     bg = 'NONE'            })  -- [Tab] - Retrieve
  highlight(0, 'whitespaceHeap',               { fg = colors.purple,     bg = 'NONE'            })  -- Generic heap operation

  -- Flow Control Instructions (IMP: LF)
  highlight(0, 'whitespaceFlowMark',           { fg = colors.turquoise,  bg = 'NONE'            })  -- [Space][Space] Label - Mark location
  highlight(0, 'whitespaceFlowCall',           { fg = colors.turquoise,  bg = 'NONE'            })  -- [Space][Tab] Label - Call subroutine
  highlight(0, 'whitespaceFlowJump',           { fg = colors.turquoise,  bg = 'NONE'            })  -- [Space][LF] Label - Unconditional jump
  highlight(0, 'whitespaceFlowJumpZero',       { fg = colors.turquoise,  bg = 'NONE'            })  -- [Tab][Space] Label - Jump if zero
  highlight(0, 'whitespaceFlowJumpNeg',        { fg = colors.turquoise,  bg = 'NONE'            })  -- [Tab][Tab] Label - Jump if negative
  highlight(0, 'whitespaceFlowReturn',         { fg = colors.turquoise,  bg = 'NONE'            })  -- [Tab][LF] - Return from subroutine
  highlight(0, 'whitespaceFlowExit',           { fg = colors.turquoise,  bg = 'NONE', bold = true })  -- [LF][LF] - End program
  highlight(0, 'whitespaceFlow',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Generic flow control

  -- I/O Instructions (IMP: Tab LF)
  highlight(0, 'whitespaceIOOutChar',          { fg = colors.orange,     bg = 'NONE'            })  -- [Space][Space] - Output as character
  highlight(0, 'whitespaceIOOutNum',           { fg = colors.orange,     bg = 'NONE'            })  -- [Space][Tab] - Output as number
  highlight(0, 'whitespaceIOInChar',           { fg = colors.orange,     bg = 'NONE'            })  -- [Tab][Space] - Read character
  highlight(0, 'whitespaceIOInNum',            { fg = colors.orange,     bg = 'NONE'            })  -- [Tab][Tab] - Read number
  highlight(0, 'whitespaceIO',                 { fg = colors.orange,     bg = 'NONE'            })  -- Generic I/O

  -- Numbers (binary encoded: Space=0, Tab=1)
  highlight(0, 'whitespaceNumber',             { fg = colors.greenLight, bg = 'NONE'            })  -- Encoded number
  highlight(0, 'whitespaceNumberSign',         { fg = colors.greenLight, bg = 'NONE'            })  -- Sign (Space=+, Tab=-)
  highlight(0, 'whitespaceNumberBit',          { fg = colors.greenLight, bg = 'NONE'            })  -- Binary digit
  highlight(0, 'whitespaceNumberTerminator',   { fg = colors.greenLight, bg = 'NONE'            })  -- LF terminator

  -- Labels (sequence of Space/Tab terminated by LF)
  highlight(0, 'whitespaceLabel',              { fg = colors.yellow,     bg = 'NONE'            })  -- Label definition/reference
  highlight(0, 'whitespaceLabelDef',           { fg = colors.yellow,     bg = 'NONE', bold = true })  -- Label definition (at mark)
  highlight(0, 'whitespaceLabelRef',           { fg = colors.yellow,     bg = 'NONE'            })  -- Label reference (at jump/call)

  -- Comments (all non-whitespace characters are comments!)
  highlight(0, 'whitespaceComment',            { fg = colors.red,        bg = 'NONE'            })  -- Non-whitespace characters

  -- Visual Representation Characters
  -- When Whitespace is displayed as visible characters (S, T, L or [Space], [Tab], [LF])
  highlight(0, 'whitespaceVisualSpace',        { fg = colors.blue,       bg = colors.blueLight  })  -- Visual Space (S)
  highlight(0, 'whitespaceVisualTab',          { fg = colors.green,      bg = colors.blueLight  })  -- Visual Tab (T)
  highlight(0, 'whitespaceVisualLF',           { fg = colors.purple,     bg = colors.blueLight  })  -- Visual Linefeed (L)
  highlight(0, 'whitespaceVisual',             { fg = colors.white,      bg = colors.blueLight  })  -- Any visual marker


  -----------------------------------------------------------------------------
  -- Whitespace Assembly / Human-Readable Mnemonics
  -- Used by interpreters and debuggers for readable representation

  -- Stack Manipulation Mnemonics
  highlight(0, 'wsasmPush',                    { fg = colors.blue,       bg = 'NONE'            })  -- push
  highlight(0, 'wsasmDup',                     { fg = colors.blue,       bg = 'NONE'            })  -- dup, duplicate
  highlight(0, 'wsasmCopy',                    { fg = colors.blue,       bg = 'NONE'            })  -- copy, pick
  highlight(0, 'wsasmSwap',                    { fg = colors.blue,       bg = 'NONE'            })  -- swap
  highlight(0, 'wsasmDrop',                    { fg = colors.blue,       bg = 'NONE'            })  -- drop, discard, pop
  highlight(0, 'wsasmSlide',                   { fg = colors.blue,       bg = 'NONE'            })  -- slide

  -- Arithmetic Mnemonics
  highlight(0, 'wsasmAdd',                     { fg = colors.green,      bg = 'NONE'            })  -- add
  highlight(0, 'wsasmSub',                     { fg = colors.green,      bg = 'NONE'            })  -- sub, subtract
  highlight(0, 'wsasmMul',                     { fg = colors.green,      bg = 'NONE'            })  -- mul, multiply
  highlight(0, 'wsasmDiv',                     { fg = colors.green,      bg = 'NONE'            })  -- div, divide
  highlight(0, 'wsasmMod',                     { fg = colors.green,      bg = 'NONE'            })  -- mod, modulo

  -- Heap Mnemonics
  highlight(0, 'wsasmStore',                   { fg = colors.purple,     bg = 'NONE'            })  -- store, heap_store
  highlight(0, 'wsasmRetrieve',                { fg = colors.purple,     bg = 'NONE'            })  -- retrieve, heap_get, load

  -- Flow Control Mnemonics
  highlight(0, 'wsasmLabel',                   { fg = colors.yellow,     bg = 'NONE', bold = true })  -- label, mark
  highlight(0, 'wsasmCall',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- call, gosub
  highlight(0, 'wsasmJump',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- jump, jmp, goto
  highlight(0, 'wsasmJumpZero',                { fg = colors.turquoise,  bg = 'NONE'            })  -- jz, jump_zero, beq
  highlight(0, 'wsasmJumpNeg',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- jn, jump_neg, blt
  highlight(0, 'wsasmReturn',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- ret, return
  highlight(0, 'wsasmExit',                    { fg = colors.turquoise,  bg = 'NONE', bold = true })  -- exit, end, halt

  -- I/O Mnemonics
  highlight(0, 'wsasmOutChar',                 { fg = colors.orange,     bg = 'NONE'            })  -- outchar, printc, putc
  highlight(0, 'wsasmOutNum',                  { fg = colors.orange,     bg = 'NONE'            })  -- outnum, printi, putn
  highlight(0, 'wsasmInChar',                  { fg = colors.orange,     bg = 'NONE'            })  -- inchar, readc, getc
  highlight(0, 'wsasmInNum',                   { fg = colors.orange,     bg = 'NONE'            })  -- innum, readi, getn

  -- Assembly Operands
  highlight(0, 'wsasmNumber',                  { fg = colors.greenLight, bg = 'NONE'            })  -- Numeric literal
  highlight(0, 'wsasmLabelName',               { fg = colors.yellow,     bg = 'NONE'            })  -- Label name/identifier
  highlight(0, 'wsasmComment',                 { fg = colors.red,        bg = 'NONE'            })  -- Assembly comment
  highlight(0, 'wsasmDirective',               { fg = colors.pink,       bg = 'NONE'            })  -- Assembler directive


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.whitespace)

  -- Instruction Categories
  highlight(0, '@keyword.whitespace',                  { fg = colors.blue,       bg = 'NONE' })  -- General instruction
  highlight(0, '@keyword.operator.whitespace',         { fg = colors.green,      bg = 'NONE' })  -- Arithmetic ops
  highlight(0, '@keyword.storage.whitespace',          { fg = colors.purple,     bg = 'NONE' })  -- Heap access
  highlight(0, '@keyword.control.whitespace',          { fg = colors.turquoise,  bg = 'NONE' })  -- Flow control
  highlight(0, '@keyword.function.whitespace',         { fg = colors.turquoise,  bg = 'NONE' })  -- Call/Return

  -- Stack Operations
  highlight(0, '@function.builtin.whitespace',         { fg = colors.blue,       bg = 'NONE' })  -- Stack operations

  -- I/O
  highlight(0, '@function.whitespace',                 { fg = colors.orange,     bg = 'NONE' })  -- I/O operations

  -- Data
  highlight(0, '@number.whitespace',                   { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@label.whitespace',                    { fg = colors.yellow,     bg = 'NONE' })  -- Labels

  -- Whitespace Characters (when made visible)
  highlight(0, '@character.whitespace',                { fg = colors.white,      bg = colors.blueLight })  -- Visual char
  highlight(0, '@character.special.whitespace',        { fg = colors.purple,     bg = colors.blueLight })  -- Special marker

  -- Comments
  highlight(0, '@comment.whitespace',                  { fg = colors.red,        bg = 'NONE' })  -- Non-whitespace

  -- Punctuation (terminators)
  highlight(0, '@punctuation.whitespace',              { fg = colors.gray,       bg = 'NONE' })  -- Terminators


  -----------------------------------------------------------------------------
  -- Special Display Mode Highlighting
  -- For editors that show actual whitespace with visible markers

  -- Actual Whitespace Characters (with background)
  highlight(0, 'WhitespaceSpace',              { fg = 'NONE',            bg = colors.blueDark   })  -- Space character
  highlight(0, 'WhitespaceTab',                { fg = 'NONE',            bg = colors.greenDark  })  -- Tab character
  highlight(0, 'WhitespaceLF',                 { fg = 'NONE',            bg = colors.purpleDark })  -- Linefeed

  -- Semantic highlighting by instruction context
  highlight(0, 'WhitespaceStackOp',            { fg = 'NONE',            bg = colors.blueDark   })  -- Stack manipulation
  highlight(0, 'WhitespaceArithOp',            { fg = 'NONE',            bg = colors.greenDark  })  -- Arithmetic
  highlight(0, 'WhitespaceHeapOp',             { fg = 'NONE',            bg = colors.purpleDark })  -- Heap access
  highlight(0, 'WhitespaceFlowOp',             { fg = 'NONE',            bg = colors.turquoiseDark })  -- Flow control
  highlight(0, 'WhitespaceIOOp',               { fg = 'NONE',            bg = colors.orangeDark })  -- I/O


  -----------------------------------------------------------------------------
  -- Whitelips IDE Style Highlighting
  -- Based on the popular online Whitespace IDE

  highlight(0, 'whitelipsSpace',               { fg = colors.blueLink,   bg = 'NONE'            })  -- Space marker
  highlight(0, 'whitelipsTab',                 { fg = colors.green,      bg = 'NONE'            })  -- Tab marker
  highlight(0, 'whitelipsLF',                  { fg = colors.purple,     bg = 'NONE'            })  -- LF marker
  highlight(0, 'whitelipsInstruction',         { fg = colors.blue,       bg = 'NONE', bold = true })  -- Instruction
  highlight(0, 'whitelipsOperand',             { fg = colors.greenLight, bg = 'NONE'            })  -- Operand
  highlight(0, 'whitelipsLabel',               { fg = colors.yellow,     bg = 'NONE'            })  -- Label
  highlight(0, 'whitelipsComment',             { fg = colors.red,        bg = 'NONE'            })  -- Comment


  -----------------------------------------------------------------------------
  -- Debug/Trace View Highlighting
  -- For debuggers and execution traces

  highlight(0, 'whitespaceDbgStack',           { fg = colors.blue,       bg = 'NONE'            })  -- Stack state
  highlight(0, 'whitespaceDbgHeap',            { fg = colors.purple,     bg = 'NONE'            })  -- Heap state
  highlight(0, 'whitespaceDbgPC',              { fg = colors.yellow,     bg = 'NONE', bold = true })  -- Program counter
  highlight(0, 'whitespaceDbgCurrent',         { fg = colors.white,      bg = colors.blueLink   })  -- Current instruction
  highlight(0, 'whitespaceDbgBreakpoint',      { fg = colors.white,      bg = colors.red        })  -- Breakpoint
  highlight(0, 'whitespaceDbgOutput',          { fg = colors.orange,     bg = 'NONE'            })  -- Program output


  -----------------------------------------------------------------------------
  -- Alternative Notations
  -- STL notation (S=Space, T=Tab, L=Linefeed)

  highlight(0, 'whitespaceSTL_S',              { fg = colors.blue,       bg = 'NONE'            })  -- S character
  highlight(0, 'whitespaceSTL_T',              { fg = colors.green,      bg = 'NONE'            })  -- T character
  highlight(0, 'whitespaceSTL_L',              { fg = colors.purple,     bg = 'NONE'            })  -- L character
  highlight(0, 'whitespaceSTL_N',              { fg = colors.gray,       bg = 'NONE'            })  -- N (newline in output)

  -- Bracket notation ([Space], [Tab], [LF])
  highlight(0, 'whitespaceBracketSpace',       { fg = colors.blue,       bg = 'NONE'            })  -- [Space]
  highlight(0, 'whitespaceBracketTab',         { fg = colors.green,      bg = 'NONE'            })  -- [Tab]
  highlight(0, 'whitespaceBracketLF',          { fg = colors.purple,     bg = 'NONE'            })  -- [LF]
  highlight(0, 'whitespaceBracket',            { fg = colors.gray,       bg = 'NONE'            })  -- [ ] brackets


  -----------------------------------------------------------------------------
  -- Error Highlighting

  highlight(0, 'whitespaceError',              { fg = colors.white,      bg = colors.red        })  -- Syntax error
  highlight(0, 'whitespaceInvalidOp',          { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Invalid opcode
  highlight(0, 'whitespaceStackUnderflow',     { fg = colors.orange,     bg = 'NONE', undercurl = true })  -- Stack underflow
  highlight(0, 'whitespaceUndefLabel',         { fg = colors.yellow,     bg = 'NONE', undercurl = true })  -- Undefined label


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.whitespace)

  highlight(0, '@lsp.type.function.whitespace',        { fg = colors.orange,     bg = 'NONE' })  -- I/O function
  highlight(0, '@lsp.type.keyword.whitespace',         { fg = colors.blue,       bg = 'NONE' })  -- Instruction
  highlight(0, '@lsp.type.operator.whitespace',        { fg = colors.green,      bg = 'NONE' })  -- Arithmetic
  highlight(0, '@lsp.type.variable.whitespace',        { fg = colors.purple,     bg = 'NONE' })  -- Heap variable
  highlight(0, '@lsp.type.number.whitespace',          { fg = colors.greenLight, bg = 'NONE' })  -- Number
  highlight(0, '@lsp.type.label.whitespace',           { fg = colors.yellow,     bg = 'NONE' })  -- Label
  highlight(0, '@lsp.type.comment.whitespace',         { fg = colors.red,        bg = 'NONE' })  -- Comment


  -----------------------------------------------------------------------------
  -- Filetype Links

  highlight(0, 'wsSpace',              { link = 'whitespaceVisualSpace'    })
  highlight(0, 'wsTab',                { link = 'whitespaceVisualTab'      })
  highlight(0, 'wsLF',                 { link = 'whitespaceVisualLF'       })
  highlight(0, 'wsCommand',            { link = 'whitespaceStack'          })
  highlight(0, 'wsNumber',             { link = 'whitespaceNumber'         })
  highlight(0, 'wsLabel',              { link = 'whitespaceLabel'          })
  highlight(0, 'wsComment',            { link = 'whitespaceComment'        })
end

return whitespace
