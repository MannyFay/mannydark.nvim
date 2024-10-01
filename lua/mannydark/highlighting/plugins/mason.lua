-------------------------------------------------------------------------------
-- Mason Plugin
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local mason     = {}


-------------------------------------------------------------------------------
-- Settings

mason.setupHighlighting = function()
  highlight(0, 'MasonHighlightBlockBold', { fg = 'NONE',         bg = colors.blue                   })  -- Active element.
  highlight(0, 'MasonWarning',            { fg = colors.orange,  bg = 'NONE',      underline = true })  -- Warnings.
  highlight(0, 'MasonMuted',              { fg = colors.gray,    bg = 'NONE',                       })  -- Sign of uninstalled packages.
  highlight(0, 'MasonHeader',             { fg = colors.purple,  bg = 'NONE',      underline = true })  -- Mason title in window.
  highlight(0, 'MasonMutedBlock',         { fg = colors.black,   bg = colors.gray,                  })  -- Inactive element.
end

return mason

