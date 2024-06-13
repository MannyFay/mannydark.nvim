----------------------------------------------------------------------------
-- Language Defaults
-- If there is now specific highlighting for languages, these are the defaults.
------------------------------------------------------------------------------

local colors           = require('mannydark.palette')
local highlight        = vim.api.nvim_set_hl
local languageDefaults = {}


--------------------------------------------------------------
-- Settings

languageDefaults.setupHighlighting = function()
  highlight(0, 'Comment',     { fg = colors.red,        bg = 'NONE' })  -- Block and line comments.
  highlight(0, 'Variable',    { fg = colors.purple,     bg = 'NONE' })  -- All kinds of variables.
  highlight(0, 'String',      { fg = colors.redLight,   bg = 'NONE' })  -- Main setting for everything between quotes.
  highlight(0, 'Number',      { fg = colors.greenLight, bg = 'NONE' })  -- All kinds of integer numbers.
  highlight(0, 'Float',       { fg = colors.greenLight, bg = 'NONE' })  -- All kinds of floating point numbers.
  highlight(0, 'Boolean',     { fg = colors.blue,       bg = 'NONE' })  -- The boolean values 'true' and 'false'.
  highlight(0, 'Constant',    { fg = colors.purple,     bg = 'NONE' })  -- All kinds of constants.
  highlight(0, 'Function',    { fg = colors.orange,     bg = 'NONE' })  -- All kinds of functions.
  highlight(0, 'Keyword',     { fg = colors.blue,       bg = 'NONE' })  -- Keywords of programming languages.
  highlight(0, 'Character',   { fg = colors.white,      bg = 'NONE' })  -- Regular characters in a code file.
  highlight(0, 'Conditional', { fg = colors.blue,       bg = 'NONE' })  -- Conditional statements like 'if', 'else', etc.
  highlight(0, 'PreProc',     { fg = colors.redLight,   bg = 'NONE' })  -- Like ${} in shell scripts.
  highlight(0, 'Error',       { fg = 'NONE',            bg = 'NONE', sp = colors.red, undercurl = true       })

  ----------------------- Not used by now:
  highlight(0, 'Type',           { fg = colors.turquoise, bg = 'NONE'                   })
  highlight(0, 'Delimiter',      { fg = colors.blue,      bg = 'NONE'                   })
  highlight(0, 'Exception',      { fg = colors.blue,      bg = 'NONE'                   })
  highlight(0, 'Identifier',     { fg = colors.purple,    bg = 'NONE'                   })
  highlight(0, 'Include',        { fg = colors.blue,      bg = 'NONE'                   })
  highlight(0, 'MatchParen',     { fg = colors.white,     bg = colors.gray              })
  highlight(0, 'Normal',         { fg = colors.white,     bg = colors.black             })
  highlight(0, 'Operator',       { fg = colors.white,     bg = 'NONE'                   })
  highlight(0, 'Special',        { fg = colors.white,     bg = 'NONE'                   })
  highlight(0, 'Todo',           { fg = colors.red,       bg = 'NONE', bold = true      })
  highlight(0, 'Title',          { fg = colors.white,     bg = 'NONE'                   })
  highlight(0, 'Statement',      { fg = colors.blue,      bg = 'NONE'                   })
  highlight(0, 'Structure',      { fg = colors.blue,      bg = 'NONE'                   })
  highlight(0, 'SpecialChar',    { fg = colors.white,     bg = 'NONE'                   })
  highlight(0, 'Repeat',         { fg = colors.purple,    bg = 'NONE'                   })
  highlight(0, 'StorageClass',   { fg = colors.cyan,      bg = 'NONE'                   })
  highlight(0, 'Typedef',        { fg = colors.purple,    bg = 'NONE'                   })
  highlight(0, 'Define',         { fg = colors.purple,    bg = 'NONE'                   })
  highlight(0, 'Macro',          { fg = colors.purple,    bg = 'NONE'                   })
  highlight(0, 'Debug',          { fg = colors.gray,      bg = 'NONE'                   })
  highlight(0, 'Label',          { fg = colors.blue,      bg = 'NONE'                   })
  highlight(0, 'SpecialComment', { fg = colors.green,     bg = 'NONE'                   })
  highlight(0, 'Tag',            { fg = colors.blue,      bg = 'NONE'                   })
  highlight(0, 'Bold',           { fg = 'NONE',           bg = 'NONE', bold = true      })
  highlight(0, 'Italic',         { fg = 'NONE',           bg = 'NONE', italic = true    })
  highlight(0, 'Underlined',     { fg = 'NONE',           bg = 'NONE', underline = true })
  highlight(0, 'Ignore',         { fg = colors.orange,    bg = 'NONE', bold = true      })
  highlight(0, 'PreCondit',      { fg = colors.purple,    bg = 'NONE'                   })
end

return languageDefaults
