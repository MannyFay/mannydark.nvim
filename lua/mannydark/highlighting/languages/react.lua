-------------------------------------------------------------------------------
-- React
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local react     = {}

-------------------------------------------------------------------------------
---- Settings

react.setupHighlighting = function()
  highlight(0, "jsxOpenPunct",     { fg = colors.white,     bg = "NONE" })  -- < and > of JSX/HTML tag.
  highlight(0, "jsxClosePunct",    { fg = colors.white,     bg = "NONE" })  -- </ of JSX/HTML tag.
  highlight(0, "jsxTagName",       { fg = colors.blue,      bg = "NONE" })  -- Name of JSX/HTML tag.
  highlight(0, "jsxComponentName", { fg = colors.turquoise, bg = "NONE" })  -- Name of used React components in JSX/HTML.
  highlight(0, "jsxCloseString",   { fg = colors.turquoise, bg = "NONE" })  -- /> of JSX/HTML tag. -> WTF, wrong description?
  highlight(0, "tsxTagName",       { fg = colors.purple,    bg = "NONE" })  -- HTML tags with component names.
  highlight(0, "tsxCloseString",   { fg = colors.white,     bg = "NONE" })  -- /> of HTML tags with component names.





----------------------- Not used by now:
-- jsxElement     xxx cleared
-- jsxOpenTag     xxx cleared
-- jsxAttrib      xxx links to Type
-- jsxExpressionBlock xxx cleared
-- jsxSpreadOperator xxx links to Operator
-- jsxTag         xxx links to Function
-- jsxComment     xxx links to Comment
-- jsxCloseTag    xxx links to jsxCloseString
-- jsxBraces      xxx links to Special
-- jsxDot         xxx links to Operator
-- jsxNamespace   xxx links to Operator
-- jsxString      xxx links to String
-- jsxEqual       xxx links to Operator
-- jsxAttribKeyword xxx links to jsxAttrib
-- jsxTaggedRegion xxx cleared
-- jsxBackticks   xxx cleared
-- jsxPunct       xxx links to jsxCloseString
-- jsxRegion      xxx cleared



-- tsxIntrinsicTagName xxx links to htmlTagName
-- tsxAttrib      xxx links to Type
-- tsxEscJs       xxx links to tsxEscapeJs
-- tsxTag         xxx links to htmlTag
-- tsxRegion      xxx cleared
-- tsxCloseTag    xxx links to htmlTag
-- tsxCommentInvalid xxx links to Error
-- tsxFragment    xxx cleared
-- tsxBlockComment xxx links to Comment
-- tsxLineComment xxx links to Comment
-- tsxEntityPunct xxx cleared
-- tsxEntity      xxx cleared
-- tsxEqual       xxx cleared
-- tsxString      xxx links to String
-- tsxNameSpace   xxx links to Function
-- tsxEscapeJs    xxx cleared



end

return react

