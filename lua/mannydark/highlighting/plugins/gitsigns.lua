-------------------------------------------------------------------------------
-- Git Signs Neovim Plugin
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local gitSigns  = {}


--------------------------------------------------------------
-- Settings

gitSigns.setupHighlighting = function()
  highlight(0, 'SignAdd',            { fg = colors.green,  bg = 'NONE'                           })
  highlight(0, 'SignChange',         { fg = colors.blue,   bg = 'NONE'                           })
  highlight(0, 'SignDelete',         { fg = colors.red,    bg = 'NONE'                           })
  highlight(0, 'GitSignsAdd',        { fg = colors.green,  bg = 'NONE'                           })
  highlight(0, 'GitSignsChange',     { fg = colors.blue,   bg = 'NONE'                           })
  highlight(0, 'GitSignsDelete',     { fg = colors.red,    bg = 'NONE'                           })
  highlight(0, 'GitignoreSeparator', { fg = colors.white,  bg = 'NONE'                           })  -- '/' in .gitignore files.
  highlight(0, 'GitSignsCurrentLineBlame', { fg = colors.gray,  bg = 'NONE'                           })


  ----------------------- Not used by now:
  highlight(0, 'DiffText',           { fg = colors.green,  bg = colors.red                       })
  highlight(0, 'DiffAdd',            { fg = colors.pink,   bg = colors.blue                      })
  highlight(0, 'DiffChange',         { fg = colors.orange, bg = colors.purple, underline = true, })
  highlight(0, 'DiffDelete',         { fg = colors.blue,   bg = colors.green                     })
end

return gitSigns
