-----------------------------------------------------------------------------------------------------------------------
-- indent-blankline.nvim - Indent guides
-- Supports both v2 (legacy) and v3 highlight groups
-----------------------------------------------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local ibl       = {}


-------------------------------------------------------------------------------
-- Settings

ibl.setupHighlighting = function()

  -- indent-blankline v3 base groups:
  highlight(0, "IblIndent",     { fg = colors.grayDark, bg = "NONE" })  -- Indent line characters (inactive).
  highlight(0, "IblWhitespace", { fg = colors.grayDark, bg = "NONE" })  -- Whitespace characters.
  highlight(0, "IblScope",      { fg = colors.gray,     bg = "NONE" })  -- Scope highlighting (active/current context).

  -- indent-blankline v3 actual rendering groups (these are what ibl uses internally):
  -- ibl creates @ibl.indent.char.1, @ibl.scope.char.1, etc. and expects them to link to Ibl* groups
  -- We define them explicitly to ensure they work regardless of ibl's initialization order
  highlight(0, "@ibl.indent.char.1",     { fg = colors.grayDark, bg = "NONE" })
  highlight(0, "@ibl.indent.char.2",     { fg = colors.grayDark, bg = "NONE" })
  highlight(0, "@ibl.indent.char.3",     { fg = colors.grayDark, bg = "NONE" })
  highlight(0, "@ibl.indent.char.4",     { fg = colors.grayDark, bg = "NONE" })
  highlight(0, "@ibl.whitespace.char.1", { fg = colors.grayDark, bg = "NONE" })
  highlight(0, "@ibl.scope.char.1",      { fg = colors.gray,     bg = "NONE" })
  highlight(0, "@ibl.scope.underline.1", { sp = colors.gray, underline = true })

  -- Rainbow indent colors (optional, for rainbow-delimiters integration):
  highlight(0, "RainbowBlue",   { fg = colors.blue,      bg = "NONE" })
  highlight(0, "RainbowCyan",   { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "RainbowGreen",  { fg = colors.green,     bg = "NONE" })
  highlight(0, "RainbowOrange", { fg = colors.orange,    bg = "NONE" })
  highlight(0, "RainbowRed",    { fg = colors.red,       bg = "NONE" })
  highlight(0, "RainbowViolet", { fg = colors.purple,    bg = "NONE" })
  highlight(0, "RainbowYellow", { fg = colors.yellow,    bg = "NONE" })

  -- indent-blankline v2 (legacy) groups:
  highlight(0, "IndentBlanklineChar",               { fg = colors.grayDark, bg = "NONE" })
  highlight(0, "IndentBlanklineSpaceChar",          { fg = colors.grayDark, bg = "NONE" })
  highlight(0, "IndentBlanklineSpaceCharBlankline", { fg = colors.grayDark, bg = "NONE" })
  highlight(0, "IndentBlanklineContextChar",        { fg = colors.gray,     bg = "NONE" })
  highlight(0, "IndentBlanklineContextSpaceChar",   { fg = colors.gray,     bg = "NONE" })
  highlight(0, "IndentBlanklineContextStart",       { sp = colors.gray, underline = true })
end

return ibl
