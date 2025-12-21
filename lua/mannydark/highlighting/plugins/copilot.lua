-------------------------------------------------------------------------------
-- Copilot Neovim Plugin
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local copilot   = {}


--------------------------------------------------------------
-- Settings

copilot.setupHighlighting = function()
  highlight(0, "CopilotSuggestion", { fg = colors.gray, bg = "NONE" })  -- Provided suggestions.
end

return copilot

