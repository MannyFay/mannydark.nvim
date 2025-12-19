-----------------------------------------------------------------------------------------------------------------------
-- indent-blankline.nvim - Indent guides
-- Supports both v2 (legacy) and v3 highlight groups
-----------------------------------------------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local ibl       = {}


-------------------------------------------------------------------------------
-- Settings

ibl.setupHighlighting = function()

  -- indent-blankline v3 (Ibl*) groups:
  highlight(0, "IblIndent",     { link = "MannydarkFgGrayDark" })  -- Indent line characters.
  highlight(0, "IblWhitespace", { link = "MannydarkFgGrayDark" })  -- Whitespace characters.
  highlight(0, "IblScope",      { link = "MannydarkFgGray"     })  -- Scope highlighting (current context).

  -- Rainbow indent colors (optional, for rainbow-delimiters integration):
  highlight(0, "RainbowBlue",   { link = "MannydarkFgBlue"      })
  highlight(0, "RainbowCyan",   { link = "MannydarkFgTurquoise" })
  highlight(0, "RainbowGreen",  { link = "MannydarkFgGreen"     })
  highlight(0, "RainbowOrange", { link = "MannydarkFgOrange"    })
  highlight(0, "RainbowRed",    { link = "MannydarkFgRed"       })
  highlight(0, "RainbowViolet", { link = "MannydarkFgPurple"    })
  highlight(0, "RainbowYellow", { link = "MannydarkFgYellow"    })

  -- indent-blankline v2 (legacy) groups:
  highlight(0, "IndentBlanklineChar",               { link = "MannydarkFgGrayDark" })  -- Indent line character.
  highlight(0, "IndentBlanklineSpaceChar",          { link = "MannydarkFgGrayDark" })  -- Space characters in indentation.
  highlight(0, "IndentBlanklineSpaceCharBlankline", { link = "MannydarkFgGrayDark" })  -- Space characters on blank lines.
  highlight(0, "IndentBlanklineContextChar",        { link = "MannydarkFgGray"     })  -- Context indent line (current scope).
  highlight(0, "IndentBlanklineContextSpaceChar",   { link = "MannydarkFgGray"     })  -- Space in context.
  highlight(0, "IndentBlanklineContextStart",       { fg = "NONE", bg = "NONE", underline = true, sp = colors.gray })  -- Underline at context start.

  -- Scope underline (v3):
  highlight(0, "@ibl.scope.underline.1", { fg = "NONE", bg = "NONE", underline = true, sp = colors.gray })
end

return ibl
