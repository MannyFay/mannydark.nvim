------------------------------------------------------------------------------
-- NvimCMP Neovim Plugin
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local nvimCmp   = {}


--------------------------------------------------------------
-- Settings

nvimCmp.setupHighlighting = function()
  highlight(0, 'CmpItemAbbrMatch',         { fg = colors.black, bg = colors.gray                       })
  highlight(0, 'CmpItemAbbrDeprecated',    { fg = colors.gray,  bg = 'NONE',      strikethrough = true })

  ----------------------- Not used by now:
  highlight(0, 'CmpItemAbbrMatchFuzzy',    { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindFunction',      { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindMethod',        { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindConstructor',   { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindClass',         { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindEnum',          { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindEvent',         { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindInterface',     { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindStruct',        { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindVariable',      { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindField',         { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindProperty',      { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindEnumMember',    { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindConstant',      { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindKeyword',       { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindModule',        { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindValue',         { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindUnit',          { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindText',          { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindSnippet',       { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindFile',          { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindFolder',        { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindColor',         { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindReference',     { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindOperator',      { fg = colors.green, bg = colors.orange                     })
  highlight(0, 'CmpItemKindTypeParameter', { fg = colors.green, bg = colors.orange                     })
end

return nvimCmp
