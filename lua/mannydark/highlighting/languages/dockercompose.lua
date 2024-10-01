-------------------------------------------------------------------------------
-- Docker Compose
-------------------------------------------------------------------------------

local colors        = require('mannydark.palette')
local highlight     = vim.api.nvim_set_hl
local dockerCompose = {}


-------------------------------------------------------------------------------
-- Settings

dockerCompose.setupHighlighting = function()
  highlight(0, 'dockercomposeComment',  { fg = colors.red,      bg = 'NONE'              })  -- Comments.
  highlight(0, 'dockercomposeKeywords', { fg = colors.blue,     bg = 'NONE'              })  -- Keywords.
  highlight(0, 'dockercomposeString1',  { fg = colors.redLight, bg = 'NONE'              })  -- Strings in single quotes.
  highlight(0, 'dockercomposeString',   { fg = colors.redLight, bg = 'NONE'              })  -- Strings in double quotes.
  highlight(0, 'dockercomposeTodo',     { fg = colors.red,      bg = 'NONE', bold = true })  -- TODO comments.

  ----------------------- Not used by now:
  highlight(0, 'bashStatement',         { fg = colors.blue,     bg = colors.gray      })
  highlight(0, 'dockercomposeEmail',    { fg = colors.blue,     bg = colors.orange    })
  highlight(0, 'dockercomposeUrl',      { fg = colors.blue,     bg = colors.purple    })
end

return dockerCompose

