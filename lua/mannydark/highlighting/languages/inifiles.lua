-------------------------------------------------------------------------------
-- Configuration Files
-- Highlighting for files like .editorconfig.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local ini       = {}


--------------------------------------------------------------
-- Settings

ini.setupHighlighting = function()
  highlight(0, 'dosiniLabel',   { fg = colors.purple,     bg = 'NONE' })  -- Setting constants.
  highlight(0, 'dosiniValue',   { fg = colors.blue,       bg = 'NONE' })  -- Regular setting values.
  highlight(0, 'dosiniComment', { fg = colors.red,        bg = 'NONE' })  -- Comments.
  highlight(0, 'dosiniHeader',  { fg = colors.white,      bg = 'NONE' })  -- Section headers.
  highlight(0, 'dosiniNumber',  { fg = colors.greenLight, bg = 'NONE' })  -- Number setting values.
end

return ini

