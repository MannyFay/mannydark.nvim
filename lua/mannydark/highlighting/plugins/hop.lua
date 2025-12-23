-------------------------------------------------------------------------------
-- Hop Neovim Plugin Highlighting
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local hopPlugin = {}


--------------------------------------------------------------
-- Settings

hopPlugin.setupHighlighting = function()
  highlight(0, "HopCursor",    { fg = colors.blue, bg = colors.blue })  -- Cursor in Hop mode.
  highlight(0, "HopNextKey",   { fg = colors.blue, bg = "NONE" }     )  -- Closest keys with one char.
  highlight(0, "HopNextKey1",  { fg = colors.purple, bg = "NONE" }   )  -- Next closest keys.
  highlight(0, "HopNextKey2",  { fg = colors.blue, bg = "NONE" }     )  -- Many next to each other keys.
  highlight(0, "HopUnmatched", { fg = colors.gray, bg = "NONE" }     )  -- Regular not matching text if Hop is active.

  ----------------------- Not used by now:
  highlight(0, "HopPreview", { fg = colors.pink, bg = colors.purple })
end

return hopPlugin

