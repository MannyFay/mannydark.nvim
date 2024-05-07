-------------------------------------------------------------------------------
-- Telescope Neovim Plugin
-- -- https://github.com/nvim-telescope/telescope.nvim
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local telescope = {}


--------------------------------------------------------------
-- Settings

telescope.setupHighlighting = function()
  highlight(0, "TelescopePromptTitle",   { fg = colors.white, bg = colors.grayDark      })
  highlight(0, "TelescopeResultsTitle",  { fg = colors.white, bg = colors.grayDark      })
  highlight(0, "TelescopePreviewTitle",  { fg = colors.white, bg = colors.grayDark      })
  highlight(0, "TelescopeNormal",        { fg = colors.white, bg = colors.black         })
  highlight(0, "TelescopeBorder",        { fg = colors.white, bg = colors.grayDark      })
  highlight(0, "TelescopePromptBorder",  { fg = colors.grayDark, bg = colors.grayDark   })
  highlight(0, "TelescopeResultsBorder", { fg = colors.grayDark, bg = colors.grayDark   })
  highlight(0, "TelescopePreviewBorder", { fg = colors.grayDark, bg = colors.grayDark   })
  highlight(0, "TelescopeSelection",     { fg = colors.white, bg = colors.grayDark      })
  highlight(0, "TelescopePromptCounter", { fg = colors.greenLight, bg = colors.grayDark })
  highlight(0, "TelescopePromptPrefix",  { fg = colors.white, bg = 'NONE'               })  -- Magnifying glass.
  highlight(0, "TelescopeMatching",      { fg = colors.black, bg = colors.blue          })  -- Matched text in autocomplete menu.

  ----------------------- Not used by now:
  highlight(0, "TelescopeSelectionCaret", { fg = colors.red,  bg = colors.pink          })
  highlight(0, "TelescopePreviewHyphen",  { fg = colors.green, bg = colors.pink         })
end

return telescope

