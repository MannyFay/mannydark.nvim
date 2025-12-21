-------------------------------------------------------------------------------
-- Flash.nvim - Search labels, enhanced character motions
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local flash     = {}


-------------------------------------------------------------------------------
-- Settings

flash.setupHighlighting = function()
  -- Core flash highlights
  highlight(0, "FlashBackdrop", { fg = colors.gray, bg = "NONE" }                       )  -- Dimmed background text
  highlight(0, "FlashMatch",    { fg = colors.turquoise, bg = "NONE", underline = true })  -- Search matches
  highlight(0, "FlashCurrent",  { fg = colors.white, bg = colors.grayDark, bold = true })  -- Current match under cursor
  highlight(0, "FlashLabel",    { fg = colors.black, bg = colors.pink, bold = true }    )  -- Jump labels (highly visible)
  highlight(0, "FlashCursor",   { fg = colors.black, bg = colors.white }                )  -- Cursor position

  -- Prompt highlights
  highlight(0, "FlashPrompt",     { fg = colors.white, bg = "NONE" })  -- Prompt area
  highlight(0, "FlashPromptIcon", { fg = colors.blue, bg = "NONE" } )  -- Prompt icon (e.g., search icon)
end

return flash
