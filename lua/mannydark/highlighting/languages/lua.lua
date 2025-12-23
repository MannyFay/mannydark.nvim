-------------------------------------------------------------------------------
-- Lua
-- Uses links to languagedefaults.lua for consistency
-------------------------------------------------------------------------------

local highlight = vim.api.nvim_set_hl
local lua       = {}

lua.setupHighlighting = function()
  highlight(0, "@constructor.lua", { link = "Delimiter" })
end

return lua
