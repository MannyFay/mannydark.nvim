-------------------------------------------------------------------------------
-- Brainfuck Files
-- Highlighting for .bf, .b, .brainfuck files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local brainfuck = {}


-------------------------------------------------------------------------------
-- Settings

brainfuck.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Pointer Movement
  highlight(0, 'brainfuckPointerRight', { fg = colors.blue,       bg = 'NONE'            })  -- > increment pointer
  highlight(0, 'brainfuckPointerLeft',  { fg = colors.blue,       bg = 'NONE'            })  -- < decrement pointer
  highlight(0, 'brainfuckPointer',      { fg = colors.blue,       bg = 'NONE'            })  -- > < pointer operations

  -- Cell Value Operations
  highlight(0, 'brainfuckIncrement',    { fg = colors.green,      bg = 'NONE'            })  -- + increment cell
  highlight(0, 'brainfuckDecrement',    { fg = colors.red,        bg = 'NONE'            })  -- - decrement cell
  highlight(0, 'brainfuckValue',        { fg = colors.greenLight, bg = 'NONE'            })  -- + - value operations

  -- Input/Output
  highlight(0, 'brainfuckOutput',       { fg = colors.orange,     bg = 'NONE'            })  -- . output
  highlight(0, 'brainfuckInput',        { fg = colors.purple,     bg = 'NONE'            })  -- , input
  highlight(0, 'brainfuckIO',           { fg = colors.orange,     bg = 'NONE'            })  -- . , I/O operations

  -- Loops
  highlight(0, 'brainfuckLoopStart',    { fg = colors.turquoise,  bg = 'NONE'            })  -- [ loop start
  highlight(0, 'brainfuckLoopEnd',      { fg = colors.turquoise,  bg = 'NONE'            })  -- ] loop end
  highlight(0, 'brainfuckLoop',         { fg = colors.turquoise,  bg = 'NONE'            })  -- [ ] loop brackets

  -- Comments (everything else)
  highlight(0, 'brainfuckComment',      { link = "Comment" })  -- Any non-command character


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.brainfuck)

  -- Commands
  highlight(0, '@keyword.brainfuck',              { link = "Keyword" })  -- General commands
  highlight(0, '@keyword.operator.brainfuck',     { link = "Operator" })  -- > < pointer
  highlight(0, '@operator.brainfuck',             { link = "Operator" }) -- + - value

  -- I/O
  highlight(0, '@function.brainfuck',             { link = "Function" })  -- . output
  highlight(0, '@function.builtin.brainfuck',     { link = "Function" })  -- , input

  -- Loops
  highlight(0, '@punctuation.bracket.brainfuck',  { fg = colors.turquoise, bg = 'NONE' })  -- [ ]
  highlight(0, '@keyword.repeat.brainfuck',       { link = "Keyword" })  -- [ ] loops

  -- Comments
  highlight(0, '@comment.brainfuck',              { link = "Comment" })  -- Non-command characters


  -----------------------------------------------------------------------------
  -- Alternative Color Scheme (each command different color)
  -- These provide more distinctive highlighting if preferred

  highlight(0, 'bfMoveRight',           { fg = colors.blue,       bg = 'NONE'            })  -- >
  highlight(0, 'bfMoveLeft',            { fg = colors.blueLink,   bg = 'NONE'            })  -- <
  highlight(0, 'bfIncrement',           { fg = colors.green,      bg = 'NONE'            })  -- +
  highlight(0, 'bfDecrement',           { fg = colors.red,        bg = 'NONE'            })  -- -
  highlight(0, 'bfOutput',              { fg = colors.orange,     bg = 'NONE'            })  -- .
  highlight(0, 'bfInput',               { fg = colors.purple,     bg = 'NONE'            })  -- ,
  highlight(0, 'bfLoopOpen',            { fg = colors.turquoise,  bg = 'NONE'            })  -- [
  highlight(0, 'bfLoopClose',           { fg = colors.pink,       bg = 'NONE'            })  -- ]
  highlight(0, 'bfComment',             { link = "Comment" })  -- Everything else
end

return brainfuck
