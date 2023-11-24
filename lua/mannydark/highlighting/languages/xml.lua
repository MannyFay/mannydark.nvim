------------------------------------------------------------------------------
-- XML Language
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local xml       = {}


--------------------------------------------------------------
-- Settings

xml.setupHighlighting = function()
  highlight(0, 'xmlTag',             { fg = colors.blue,  bg = 'NONE' })
  highlight(0, 'xmlTagName',         { fg = colors.blue,  bg = 'NONE' })
  highlight(0, 'xmlEndTag',          { fg = colors.blue,  bg = 'NONE' })
  highlight(0, 'xmlProcessingDelim', { fg = colors.blue,  bg = 'NONE' })
  highlight(0, 'xmlAttribPunct',     { fg = colors.white, bg = 'NONE' })

  ----------------------- Not used by now:
  highlight(0, 'xmlError',           { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlEntity',          { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlString',          { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlEqual',           { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlAttrib',          { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlNamespace',       { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlCdata',           { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlRegion',          { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlComment',         { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlProcessing',      { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlEntityPunct',     { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlCommentStart',    { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlCommentError',    { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlCommentPart',     { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlTodo',            { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlCdataStart',      { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlCdataEnd',        { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlCdataCdata',      { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlDocTypeKeyword',  { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlInlineDTD',       { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlDocTypeDecl',     { fg = colors.red, bg = colors.green })
  highlight(0, 'xmlDocType',         { fg = colors.red, bg = colors.green })
end

return xml
