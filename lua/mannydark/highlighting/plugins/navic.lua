-------------------------------------------------------------------------------
-- Navic Neovim Plugin
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local navic     = {}


--------------------------------------------------------------
-- Settings

navic.setupHighlighting = function()
  ----------------------- Not used by now:
  highlight(0, "NavicIconsFile",          { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsModule",        { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsNamespace",     { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsPackage",       { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsClass",         { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsMethod",        { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsProperty",      { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsField",         { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsConstructor",   { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsEnum",          { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsInterface",     { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsFunction",      { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsVariable",      { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsConstant",      { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsString",        { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsNumber",        { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsBoolean",       { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsArray",         { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsObject",        { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsKey",           { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsKeyword",       { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsNull",          { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsEnumMember",    { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsStruct",        { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsEvent",         { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsOperator",      { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicIconsTypeParameter", { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicText",               { fg = colors.orange, bg = colors.green })
  highlight(0, "NavicSeparator",          { fg = colors.orange, bg = colors.green })
end

return navic

