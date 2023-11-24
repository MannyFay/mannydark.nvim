------------------------------------------------------------------------------
-- Buffer
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local buffer    = {}


--------------------------------------------------------------
-- Settings

buffer.setupHighlighting = function()
  ----------------------- Not used by now:
  highlight(0, 'BufferCurrent',        { fg = colors.green,  bg = colors.purple               })
  highlight(0, 'BufferCurrentIndex',   { fg = colors.orange, bg = colors.green                })
  highlight(0, 'BufferCurrentMod',     { fg = colors.purple, bg = colors.blue                 })
  highlight(0, 'BufferCurrentSign',    { fg = colors.blue,   bg = colors.orange               })
  highlight(0, 'BufferCurrentTarget',  { fg = colors.red,    bg = colors.green,  bold = true, })
  highlight(0, 'BufferVisible',        { fg = colors.green,  bg = colors.pink                 })
  highlight(0, 'BufferVisibleIndex',   { fg = colors.orange, bg = colors.purple               })
  highlight(0, 'BufferVisibleMod',     { fg = colors.purple, bg = colors.pink                 })
  highlight(0, 'BufferVisibleSign',    { fg = colors.blue,   bg = colors.pink                 })
  highlight(0, 'BufferVisibleTarget',  { fg = colors.red,    bg = colors.orange, bold = true, })
  highlight(0, 'BufferInactive',       { fg = colors.pink,   bg = colors.green                })
  highlight(0, 'BufferInactiveIndex',  { fg = colors.blue,   bg = colors.purple               })
  highlight(0, 'BufferInactiveMod',    { fg = colors.orange, bg = colors.pink                 })
  highlight(0, 'BufferInactiveSign',   { fg = colors.gray,   bg = colors.yellow               })
  highlight(0, 'BufferInactiveTarget', { fg = colors.red,    bg = colors.green,  bold = true, })
end

return buffer
