-------------------------------------------------------------------------------
-- Which-Key.nvim - Keybinding popup
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local whichKey  = {}


-------------------------------------------------------------------------------
-- Settings

whichKey.setupHighlighting = function()
  -- Base highlights
  highlight(0, 'WhichKey',          { fg = colors.turquoise, bg = 'NONE'           })  -- Key characters
  highlight(0, 'WhichKeyGroup',     { fg = colors.blue,      bg = 'NONE'           })  -- Group name (+prefix)
  highlight(0, 'WhichKeyDesc',      { fg = colors.white,     bg = 'NONE'           })  -- Description text
  highlight(0, 'WhichKeySeparator', { fg = colors.gray,      bg = 'NONE'           })  -- Separator (→)
  highlight(0, 'WhichKeyValue',     { fg = colors.gray,      bg = 'NONE'           })  -- Command value

  -- Window highlights
  highlight(0, 'WhichKeyNormal', { fg = colors.white,     bg = 'NONE'           })  -- Normal text
  highlight(0, 'WhichKeyFloat',  { fg = colors.white,     bg = 'NONE'           })  -- Float window
  highlight(0, 'WhichKeyBorder', { fg = colors.turquoise, bg = 'NONE'           })  -- Window border
  highlight(0, 'WhichKeyTitle',  { fg = colors.turquoise, bg = 'NONE', bold = true })  -- Window title

  -- Icon highlights (matching mini.icons colors)
  highlight(0, 'WhichKeyIcon',       { fg = colors.gray,      bg = 'NONE' })  -- Default icon
  highlight(0, 'WhichKeyIconAzure',  { fg = colors.blueLink,  bg = 'NONE' })  -- Azure
  highlight(0, 'WhichKeyIconBlue',   { fg = colors.blue,      bg = 'NONE' })  -- Blue
  highlight(0, 'WhichKeyIconCyan',   { fg = colors.turquoise, bg = 'NONE' })  -- Cyan
  highlight(0, 'WhichKeyIconGreen',  { fg = colors.green,     bg = 'NONE' })  -- Green
  highlight(0, 'WhichKeyIconGrey',   { fg = colors.gray,      bg = 'NONE' })  -- Grey
  highlight(0, 'WhichKeyIconOrange', { fg = colors.orange,    bg = 'NONE' })  -- Orange
  highlight(0, 'WhichKeyIconPurple', { fg = colors.purple,    bg = 'NONE' })  -- Purple
  highlight(0, 'WhichKeyIconRed',    { fg = colors.red,       bg = 'NONE' })  -- Red
  highlight(0, 'WhichKeyIconYellow', { fg = colors.yellow,    bg = 'NONE' })  -- Yellow
end

return whichKey
