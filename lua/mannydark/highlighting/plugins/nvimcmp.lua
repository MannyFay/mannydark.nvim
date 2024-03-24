-------------------------------------------------------------------------------
-- NvimCMP Neovim Plugin
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local nvimCmp   = {}


--------------------------------------------------------------
-- Settings

nvimCmp.setupHighlighting = function()
  highlight(0, 'CmpItemAbbrMatch',         { fg = colors.blue,      bg = 'NONE'                            })  -- Matched text in autocomplete menu.
  highlight(0, 'CmpItemAbbrDeprecated',    { fg = colors.gray,      bg = 'NONE',      strikethrough = true })  -- Deprecated text in autocomplete menu.
  highlight(0, 'CmpItemAbbrMatchFuzzy',    { fg = colors.blue,      bg = 'NONE'                            })  -- Fuzzy matched text in autocomplete menu.
  highlight(0, 'CmpItemKindSnippet',       { fg = colors.green,     bg = 'NONE'                            })  -- Snippet kind marker text.
  highlight(0, 'CmpItemKindText',          { fg = colors.white,     bg = 'NONE'                            })  -- Text kind marker text.
  highlight(0, 'CmpItemKindFunction',      { fg = colors.orange,    bg = 'NONE'                            })  -- Function kind marker text.
  highlight(0, 'CmpItemKindKeyword',       { fg = colors.blue,      bg = 'NONE'                            })  -- Keyword kind marker text.
  highlight(0, 'CmpItemKindVariable',      { fg = colors.purple,    bg = 'NONE'                            })  -- Variable kind marker text.
  highlight(0, 'CmpItemKindClass',         { fg = colors.turquoise, bg = 'NONE'                            })  -- Class kind marker text.
  highlight(0, 'CmpItemKindProperty',      { fg = colors.turquoise, bg = 'NONE'                            })  -- Class properties kind marker text.
  highlight(0, 'CmpItemKindFolder',        { fg = colors.blue,      bg = 'NONE'                            })  -- Folder kind marker text.
  highlight(0, 'CmpItemKindFile',          { fg = colors.white,     bg = 'NONE'                            })  -- File kind marker text.
  highlight(0, 'CmpItemKindField',         { fg = colors.purple,    bg = 'NONE'                            })  -- Field (object?) kind marker text.
  highlight(0, 'CmpItemKindMethod',        { fg = colors.orange,    bg = 'NONE'                            })  -- Method kind marker text.
  highlight(0, 'CmpItemKindUnit',          { fg = colors.turquoise, bg = 'NONE'                            })  -- Unit kind marker text.
  highlight(0, 'CmpItemKindConstant',      { fg = colors.purple,    bg = 'NONE'                            })  -- Constant kind marker text.
  highlight(0, 'CmpItemKindModule',        { fg = colors.turquoise, bg = 'NONE'                            })  -- Module kind marker text.
  highlight(0, 'CmpItemKindValue',         { fg = colors.white,     bg = 'NONE'                            })  -- Value kind marker text.
  highlight(0, 'CmpItemKindConstructor',   { fg = colors.orange,    bg = 'NONE'                            })  -- Constructor kind marker text.
  highlight(0, 'CmpItemKindInterface',     { fg = colors.turquoise, bg = 'NONE'                            })  -- Interface kind marker text.
  highlight(0, 'CmpItemKindColor',         { fg = colors.purple,    bg = 'NONE'                            })  -- Color kind marker text.
  highlight(0, 'CmpItemKindReference',     { fg = colors.white,     bg = 'NONE'                            })  -- Reference kind marker text.
  highlight(0, 'CmpItemKindEnum',          { fg = colors.green,     bg = 'NONE'                            })  -- Enum kind marker text.

  ----------------------- Not used by now:
  highlight(0, 'CmpItemKindEvent',         { fg = colors.green, bg = colors.turquoise                      })
  highlight(0, 'CmpItemKindStruct',        { fg = colors.green, bg = colors.gray                           })
  highlight(0, 'CmpItemKindEnumMember',    { fg = colors.pink, bg = colors.green                           })
  highlight(0, 'CmpItemKindOperator',      { fg = colors.green, bg = colors.pink                           })
  highlight(0, 'CmpItemKindTypeParameter', { fg = colors.green, bg = colors.turquoise                      })
end

return nvimCmp

