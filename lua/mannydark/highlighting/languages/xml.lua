-------------------------------------------------------------------------------
-- XML Language
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local xml       = {}


--------------------------------------------------------------
-- Settings

xml.setupHighlighting = function()
  highlight(0, 'xmlTag',             { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'xmlTagName',         { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'xmlEndTag',          { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'xmlProcessingDelim', { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'xmlAttribPunct',     { fg = colors.white,     bg = 'NONE' })
  highlight(0, 'xmlString',          { fg = colors.redLight,  bg = 'NONE' })  -- Attribute strings.
  highlight(0, 'xmlProcessing',      { fg = 'NONE',           bg = 'NONE' })  -- White spaces.
  highlight(0, 'xmlEqual',           { fg = colors.white,     bg = 'NONE' })  -- Equal sign.
  highlight(0, 'xmlAttrib',          { fg = colors.turquoise, bg = 'NONE' })  -- 'xml' and attribute names.
  highlight(0, 'xmlCommentStart',    { fg = colors.red,       bg = 'NONE' })  -- '<!' of comments.
  highlight(0, 'xmlComment',         { fg = colors.red,       bg = 'NONE' })  -- '>' of comments.
  highlight(0, 'xmlCommentPart',     { fg = colors.red,       bg = 'NONE' })  -- Comment text.
  highlight(0, 'xmlError',           { fg = colors.red,       bg = 'NONE' })  -- Error characters.

  ----------------------- Not used by now:
  highlight(0, 'xmlEntity',          { fg = colors.red, bg = colors.blue })
  highlight(0, 'xmlNamespace',       { fg = colors.red, bg = colors.pink })
  highlight(0, 'xmlCdata',           { fg = colors.red, bg = colors.redLight })

  highlight(0, 'xmlRegion',          { fg = colors.green, bg = colors.blueLink })

  highlight(0, 'xmlEntityPunct',     { fg = colors.blue, bg = colors.red })
  highlight(0, 'xmlCommentError',    { fg = colors.blue, bg = colors.pink })
  highlight(0, 'xmlTodo',            { fg = colors.blue, bg = colors.blueLink })
  highlight(0, 'xmlCdataStart',      { fg = colors.blue, bg = colors.greenLight })
  highlight(0, 'xmlCdataEnd',        { fg = colors.red, bg = colors.white })
  highlight(0, 'xmlCdataCdata',      { fg = colors.red, bg = colors.gray })
  highlight(0, 'xmlDocTypeKeyword',  { fg = colors.blue, bg = colors.turquoise })
  highlight(0, 'xmlInlineDTD',       { fg = colors.orange, bg = colors.green })
  highlight(0, 'xmlDocTypeDecl',     { fg = colors.orange, bg = colors.blue })
  highlight(0, 'xmlDocType',         { fg = colors.orange, bg = colors.red })
end

return xml

