-------------------------------------------------------------------------------
-- ZSH
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local zsh       = {}


--------------------------------------------------------------
-- Settings

zsh.setupHighlighting = function()
  highlight(0, 'zshVariableDef',     { fg = colors.purple,   bg = 'NONE'            })  -- Name of Variables.
  highlight(0, 'zshOperator',        { fg = colors.white,    bg = 'NONE'            })  -- Operators like &&.
  highlight(0, 'zshDelim',           { fg = colors.white,    bg = 'NONE'            })  -- Delimiters like ().
  highlight(0, 'zshFlag',            { fg = colors.blue,     bg = 'NONE'            })  -- Option flags.
  highlight(0, 'zshDeref',           { fg = colors.purple,   bg = 'NONE'            })  -- Name of Constants.
  highlight(0, 'zshStringDelimiter', { fg = colors.redLight, bg = 'NONE'            })  -- Quotes of strings.
  highlight(0, 'zshSubstQuoted',     { fg = colors.purple,   bg = 'NONE'            })  -- Name of constants in strings.
  highlight(0, 'zshSubstDelim',      { fg = colors.redLight, bg = 'NONE'            })  -- Delimiters like ${} in strings.
  highlight(0, 'zshKSHFunction',     { fg = colors.orange,   bg = 'NONE'            })  -- Function names.
  highlight(0, 'zshSubst',           { fg = colors.purple,   bg = 'NONE'            })  -- Name of constants in strings.

  ----------------------- Not used by now:
  highlight(0, 'zshBraces',          { fg = colors.pink,     bg = colors.white      })
  highlight(0, 'zshBrackets',        { fg = colors.pink,     bg = colors.gray       })
  highlight(0, 'zshFunction',        { fg = colors.pink,     bg = colors.purple     })
end

return zsh

