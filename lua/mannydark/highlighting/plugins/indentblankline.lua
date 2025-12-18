-------------------------------------------------------------------------------
-- indent-blankline.nvim - Indent guides
-- Supports both v2 (legacy) and v3 highlight groups
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local ibl       = {}


-------------------------------------------------------------------------------
-- Settings

ibl.setupHighlighting = function()
  -- indent-blankline v3 (Ibl*) groups
  highlight(0, 'IblIndent',     { fg = colors.grayDark, bg = 'NONE'                        })  -- Indent line characters
  highlight(0, 'IblWhitespace', { fg = colors.grayDark, bg = 'NONE'                        })  -- Whitespace characters
  highlight(0, 'IblScope',      { fg = colors.gray,     bg = 'NONE'                        })  -- Scope highlighting (current context)

  -- Rainbow indent colors (optional, for rainbow-delimiters integration)
  highlight(0, 'RainbowRed',    { fg = colors.red,       bg = 'NONE'                       })
  highlight(0, 'RainbowYellow', { fg = colors.orange,    bg = 'NONE'                       })
  highlight(0, 'RainbowBlue',   { fg = colors.blue,      bg = 'NONE'                       })
  highlight(0, 'RainbowOrange', { fg = colors.orange,    bg = 'NONE'                       })
  highlight(0, 'RainbowGreen',  { fg = colors.green,     bg = 'NONE'                       })
  highlight(0, 'RainbowViolet', { fg = colors.purple,    bg = 'NONE'                       })
  highlight(0, 'RainbowCyan',   { fg = colors.turquoise, bg = 'NONE'                       })

  -- indent-blankline v2 (legacy) groups
  highlight(0, 'IndentBlanklineChar',               { fg = colors.grayDark, bg = 'NONE'   })  -- Indent line character
  highlight(0, 'IndentBlanklineSpaceChar',          { fg = colors.grayDark, bg = 'NONE'   })  -- Space characters in indentation
  highlight(0, 'IndentBlanklineSpaceCharBlankline', { fg = colors.grayDark, bg = 'NONE'   })  -- Space characters on blank lines
  highlight(0, 'IndentBlanklineContextChar',        { fg = colors.gray,     bg = 'NONE'   })  -- Context indent line (current scope)
  highlight(0, 'IndentBlanklineContextStart',       { fg = 'NONE', bg = 'NONE', underline = true, sp = colors.gray })  -- Underline at context start
  highlight(0, 'IndentBlanklineContextSpaceChar',   { fg = colors.gray,     bg = 'NONE'   })  -- Space in context

  -- Scope underline (v3)
  highlight(0, '@ibl.scope.underline.1', { fg = 'NONE', bg = 'NONE', underline = true, sp = colors.gray })
end

return ibl
