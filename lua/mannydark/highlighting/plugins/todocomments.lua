-------------------------------------------------------------------------------
-- todo-comments.nvim - Highlight, list and search todo comments
-- https://github.com/folke/todo-comments.nvim
-------------------------------------------------------------------------------

local colors       = require('mannydark.palette')
local highlight    = vim.api.nvim_set_hl
local todocomments = {}


-------------------------------------------------------------------------------
-- Settings

todocomments.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- BUG - Errors, bugs, issues (red/error)
  -- Alt: FIXME, FIX, FIXIT, ISSUE, PROBLEM, PROB

  highlight(0, 'TodoFgBUG',         { fg = colors.red,       bg = 'NONE'                 })
  highlight(0, 'TodoBgBUG',         { fg = colors.black,     bg = colors.red, bold = true })
  highlight(0, 'TodoSignBUG',       { fg = colors.red,       bg = 'NONE'                 })

  -----------------------------------------------------------------------------
  -- TODO - Things to do (red/error)
  -- Alt: TASK, MAKE, DO

  highlight(0, 'TodoFgTODO',        { fg = colors.red,       bg = 'NONE'                 })
  highlight(0, 'TodoBgTODO',        { fg = colors.black,     bg = colors.red, bold = true })
  highlight(0, 'TodoSignTODO',      { fg = colors.red,       bg = 'NONE'                 })

  -----------------------------------------------------------------------------
  -- HACK - Hacky workarounds (orange/warning)
  -- Alt: WORKAROUND

  highlight(0, 'TodoFgHACK',        { fg = colors.orange,    bg = 'NONE'                 })
  highlight(0, 'TodoBgHACK',        { fg = colors.black,     bg = colors.orange, bold = true })
  highlight(0, 'TodoSignHACK',      { fg = colors.orange,    bg = 'NONE'                 })

  -----------------------------------------------------------------------------
  -- WARN - Warnings (orange/warning)
  -- Alt: WARNING, XXX

  highlight(0, 'TodoFgWARN',        { fg = colors.orange,    bg = 'NONE'                 })
  highlight(0, 'TodoBgWARN',        { fg = colors.black,     bg = colors.orange, bold = true })
  highlight(0, 'TodoSignWARN',      { fg = colors.orange,    bg = 'NONE'                 })

  -----------------------------------------------------------------------------
  -- PERFORMANCE - Performance optimizations (orange/warning)
  -- Alt: OPTIM, PERF, OPTIMIZE

  highlight(0, 'TodoFgPERFORMANCE', { fg = colors.orange,    bg = 'NONE'                 })
  highlight(0, 'TodoBgPERFORMANCE', { fg = colors.black,     bg = colors.orange, bold = true })
  highlight(0, 'TodoSignPERFORMANCE', { fg = colors.orange,  bg = 'NONE'                 })

  -----------------------------------------------------------------------------
  -- NOTE - Notes and information (green/hint)
  -- Alt: INFO, MARK

  highlight(0, 'TodoFgNOTE',        { fg = colors.green,     bg = 'NONE'                 })
  highlight(0, 'TodoBgNOTE',        { fg = colors.black,     bg = colors.green, bold = true })
  highlight(0, 'TodoSignNOTE',      { fg = colors.green,     bg = 'NONE'                 })

  -----------------------------------------------------------------------------
  -- TEST - Testing related (turquoise/test)
  -- Alt: TESTING, PASSED, FAILED

  highlight(0, 'TodoFgTEST',        { fg = colors.turquoise, bg = 'NONE'                 })
  highlight(0, 'TodoBgTEST',        { fg = colors.black,     bg = colors.turquoise, bold = true })
  highlight(0, 'TodoSignTEST',      { fg = colors.turquoise, bg = 'NONE'                 })

  -----------------------------------------------------------------------------
  -- LAYOUT - Design and UI (purple/layout)
  -- Alt: DESIGN, UI

  highlight(0, 'TodoFgLAYOUT',      { fg = colors.purple,    bg = 'NONE'                 })
  highlight(0, 'TodoBgLAYOUT',      { fg = colors.black,     bg = colors.purple, bold = true })
  highlight(0, 'TodoSignLAYOUT',    { fg = colors.purple,    bg = 'NONE'                 })

  -----------------------------------------------------------------------------
  -- QUESTION - Questions and help needed (green/hint)
  -- Alt: HELP, QUESTIONS, HELPWANTED

  highlight(0, 'TodoFgQUESTION',    { fg = colors.green,     bg = 'NONE'                 })
  highlight(0, 'TodoBgQUESTION',    { fg = colors.black,     bg = colors.green, bold = true })
  highlight(0, 'TodoSignQUESTION', { fg = colors.green,      bg = 'NONE'                 })

  -----------------------------------------------------------------------------
  -- IDEA - Ideas and suggestions (green/hint)
  -- Alt: idea

  highlight(0, 'TodoFgIDEA',        { fg = colors.green,     bg = 'NONE'                 })
  highlight(0, 'TodoBgIDEA',        { fg = colors.black,     bg = colors.green, bold = true })
  highlight(0, 'TodoSignIDEA',      { fg = colors.green,     bg = 'NONE'                 })

  -----------------------------------------------------------------------------
  -- FIX - Legacy/default group (maps to BUG)

  highlight(0, 'TodoFgFIX',         { fg = colors.red,       bg = 'NONE'                 })
  highlight(0, 'TodoBgFIX',         { fg = colors.black,     bg = colors.red, bold = true })
  highlight(0, 'TodoSignFIX',       { fg = colors.red,       bg = 'NONE'                 })

  -----------------------------------------------------------------------------
  -- PERF - Legacy/default group (maps to PERFORMANCE)

  highlight(0, 'TodoFgPERF',        { fg = colors.orange,    bg = 'NONE'                 })
  highlight(0, 'TodoBgPERF',        { fg = colors.black,     bg = colors.orange, bold = true })
  highlight(0, 'TodoSignPERF',      { fg = colors.orange,    bg = 'NONE'                 })
end

return todocomments
