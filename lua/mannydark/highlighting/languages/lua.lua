------------------------------------------------------------------------------
-- Lua
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local lua       = {}


--------------------------------------------------------------
-- Settings

lua.setupHighlighting = function()
  highlight(0, 'luaLocal',           { fg = colors.blue,       bg = 'NONE'           })  -- 'local' keyword.
  highlight(0, 'luaFuncCall',        { fg = colors.orange,     bg = 'NONE'           })  -- Function that is called.
  highlight(0, 'luaFuncKeyword',     { fg = colors.blue,       bg = 'NONE'           })  -- 'function' keyword.
  highlight(0, 'luaFloat',           { fg = colors.greenLight, bg = 'NONE'           })  -- Floating point numbers.
  highlight(0, 'luaComma',           { fg = colors.white,      bg = 'NONE'           })  -- Commata.
  highlight(0, 'luaCommentLong',     { fg = colors.red,        bg = 'NONE'           })  -- Content of comments.
  highlight(0, 'luaCommentLongTag',  { fg = colors.red,        bg = 'NONE'           })  -- Tags of comments.
  highlight(0, 'luaNoise',           { fg = colors.white,      bg = 'NONE'           })  -- Dot opperator.
  highlight(0, 'luaSpecialValue',    { fg = 'NONE',            bg = 'NONE'           })  -- Require function.
  highlight(0, 'luaStringLongTag',   { fg = colors.redLight,   bg = 'NONE'           })  -- Brackets [] of long string.
  highlight(0, 'luaStringLong',      { fg = colors.redLight,   bg = 'NONE'           })  -- Content of long strings.
  highlight(0, 'luaParens',          { fg = colors.white,      bg = 'NONE'           })  -- Parentheses ().
  highlight(0, 'luaFuncParens',      { fg = colors.white,      bg = 'NONE'           })  -- Parentheses () of function keyword.
  highlight(0, 'luaFunction',        { fg = colors.blue,       bg = 'NONE'           })
  highlight(0, 'luaCond',            { fg = colors.blue,       bg = 'NONE'           })
  highlight(0, 'luaStatement',       { fg = colors.blue,       bg = 'NONE'           })
  highlight(0, 'luaOperator',        { fg = colors.blue,       bg = 'NONE'           })
  highlight(0, 'luaSymbolOperator',  { fg = colors.white,      bg = 'NONE'           })
  highlight(0, 'luaComment',         { fg = colors.red,        bg = 'NONE'           })
  highlight(0, 'luaConstant',        { fg = colors.purple,     bg = 'NONE'           })
  highlight(0, 'luaString2',         { fg = colors.redLight,   bg = 'NONE'           })
  highlight(0, 'luaStringDelimiter', { fg = colors.redLight,   bg = 'NONE'           })
  highlight(0, 'luaString',          { fg = colors.redLight,   bg = 'NONE'           })
  highlight(0, 'luaNumber',          { fg = colors.greenLight, bg = 'NONE'           })
  highlight(0, 'luaTable',           { fg = colors.white,      bg = 'NONE'           })
  highlight(0, 'luaFunc',            { fg = colors.orange,     bg = 'NONE'           })
  highlight(0, 'luaBraces',          { fg = colors.white,      bg = 'NONE'           })

  ----------------------- Not used by now:
  highlight(0, 'luaBuiltIn',         { fg = colors.white,      bg = colors.blue      })
  highlight(0, 'luaParen',           { fg = colors.white,      bg = colors.purple    })
  highlight(0, 'luaBracket',         { fg = colors.white,      bg = colors.white     })
  highlight(0, 'luaSpecialTable',    { fg = colors.white,      bg = colors.turquoise })
  highlight(0, 'luaEllipsis',        { fg = colors.white,      bg = colors.green     })
  highlight(0, 'luaError',           { fg = colors.blue,       bg = 'NONE'           })
  highlight(0, 'luaIfThen',          { fg = colors.white,      bg = 'NONE'           })
  highlight(0, 'luaBlock',           { fg = colors.purple,     bg = colors.blue      })
  highlight(0, 'luaLoop',            { fg = colors.pink,       bg = 'NONE'           })
  highlight(0, 'luaGoto',            { fg = colors.white,      bg = colors.turquoise })
  highlight(0, 'luaLabel',           { fg = colors.white,      bg = colors.pink      })
  highlight(0, 'luaSemiCol',         { fg = colors.turquoise,  bg = colors.purple    })
  highlight(0, 'luaErrHand',         { fg = colors.white,      bg = 'NONE'           })
  highlight(0, 'luaBrackets',        { fg = colors.white,      bg = 'NONE'           })
  highlight(0, 'luaNotEqOperator',   { fg = colors.white,      bg = 'NONE'           })
  highlight(0, 'luaCommentTodo',     { fg = colors.white,      bg = colors.purple    })
  highlight(0, 'luaDocTag',          { fg = colors.turquoise,  bg = 'NONE'           })
  highlight(0, 'luaFuncSig',         { fg = colors.blue,       bg = 'NONE'           })
  highlight(0, 'luaFuncId',          { fg = colors.white,      bg = 'NONE'           })
  highlight(0, 'luaFuncArgs',        { fg = colors.pink,       bg = 'NONE'           })
  highlight(0, 'luaFuncTable',       { fg = colors.white,      bg = 'NONE'           })
  highlight(0, 'luaFuncName',        { fg = colors.white,      bg = colors.pink      })
  highlight(0, 'luaFuncArgName',     { fg = colors.blue,       bg = 'NONE'           })
  highlight(0, 'luaFuncArgComma',    { fg = colors.white,      bg = colors.purple    })
  highlight(0, 'luaThenEnd',         { fg = colors.white,      bg = colors.blue      })
  highlight(0, 'luaElseifThen',      { fg = colors.white,      bg = colors.turquoise })
  highlight(0, 'luaElse',            { fg = colors.white,      bg = 'NONE'           })
  highlight(0, 'luaRepeat',          { fg = colors.white,      bg = 'NONE'           })
  highlight(0, 'luaLoopBlock',       { fg = colors.white,      bg = colors.purple    })
  highlight(0, 'luaIn',              { fg = colors.white,      bg = 'NONE'           })
  highlight(0, 'luaGotoLabel',       { fg = colors.white,      bg = colors.blue      })
  highlight(0, 'luaStringSpecial',   { fg = colors.purple,     bg = 'NONE'           })
end

return lua
