------------------------------------------------------------------------------
-- JSON
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local json      = {}


--------------------------------------------------------------
-- Settings

json.setupHighlighting = function()
  highlight(0, 'jsonNoise',        { fg = colors.white,    bg = 'NONE' })
  highlight(0, 'jsonKeyword',      { fg = colors.blue,     bg = 'NONE' })
  highlight(0, 'jsonKeywordMatch', { fg = colors.white,    bg = 'NONE' })
  highlight(0, 'jsonQuote',        { fg = colors.white,    bg = 'NONE' })
  highlight(0, 'jsonString',       { fg = colors.redLight, bg = 'NONE' })
  highlight(0, 'jsonEscape',       { fg = colors.blue,     bg = 'NONE' })
  highlight(0, 'jsonBoolean',      { fg = colors.blue,     bg = 'NONE' })
  highlight(0, 'jsonBraces',       { fg = colors.white,    bg = 'NONE' })
end

return json
