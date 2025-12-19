-------------------------------------------------------------------------------
-- Git Signs Neovim Plugin
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local gitSigns  = {}


-------------------------------------------------------------------------------
-- Settings

gitSigns.setupHighlighting = function()
  highlight(0, 'SignAdd',            { link = "@diff.plus"                           })
  highlight(0, 'SignChange',         { link = "Comment"                         })
  highlight(0, 'SignDelete',         { link = "Comment"                          })
  highlight(0, 'GitSignsAdd',        { link = "@diff.plus"                           })
  highlight(0, 'GitSignsChange',     { link = "@diff.delta"                         })
  highlight(0, 'GitSignsDelete',     { link = "@diff.minus"                          })
  highlight(0, 'GitignoreSeparator', { fg = colors.white,  bg = 'NONE'                           })  -- '/' in .gitignore files.
  highlight(0, 'GitSignsCurrentLineBlame', { fg = colors.gray,  bg = 'NONE'                      })  -- Current line blame.


  ----------------------- Not used by now:
  highlight(0, 'DiffText',           { fg = colors.green,  bg = colors.red                       })
  highlight(0, 'DiffAdd',            { link = "Comment"                          })
  highlight(0, 'DiffChange',         { fg = colors.orange, bg = colors.purple, underline = true, })
  highlight(0, 'DiffDelete',         { fg = colors.blue,   bg = colors.green                     })
end

return gitSigns
