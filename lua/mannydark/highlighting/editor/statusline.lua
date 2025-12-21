-------------------------------------------------------------------------------
-- Status Line
-------------------------------------------------------------------------------

local colors     = require("mannydark.palette")
local highlight  = vim.api.nvim_set_hl
local statusLine = {}


--------------------------------------------------------------
-- Settings

statusLine.setupHighlighting = function()
  highlight(0, "StatusLine",          { fg = "NONE",       bg = colors.grayDark })  -- Little edge left to the status line.
  highlight(0, "StatusLineNC",        { fg = colors.black, bg = colors.grayDark })  -- Little edge left to the status line if inactive.

  ----------------------- Not used by now:
  highlight(0, "StatusLineSeparator", { fg = colors.green, bg = colors.red      })
  highlight(0, "StatusLineTerm",      { fg = colors.red,   bg = colors.black    })
  highlight(0, "StatusLineTermNC",    { fg = colors.white, bg = colors.purple   })
end

return statusLine

