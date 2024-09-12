-------------------------------------------------------------------------------
-- Laravel Blade Files
-- Highlighting for Laravel Blade files (*.blade.php).
-------------------------------------------------------------------------------

local colors = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local laravelBlade = {}

--------------------------------------------------------------
-- Settings

laravelBlade.setupHighlighting = function()
	highlight(0, "bladeDelimiter", { fg = colors.white, bg = "NONE" })
	highlight(0, "bladeEcho", { fg = colors.orange, bg = "NONE" })
	highlight(0, "bladePhpParenBlock", { fg = colors.white, bg = "NONE" })
	highlight(0, "bladeTodo", { fg = colors.red, bg = "NONE", bold = true })
	highlight(0, "bladeComment", { fg = colors.red, bg = "NONE" })
	highlight(0, "bladeKeyword", { fg = colors.blue, bg = "NONE" })
	highlight(0, "bladePhpRegion", { fg = colors.green, bg = "NONE" })
end

return laravelBlade
