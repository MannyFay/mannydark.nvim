------------------------------------------------------------------------------
-- YML
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local yml      = {}


--------------------------------------------------------------
-- Settings

yml.setupHighlighting = function()
  highlight(0, 'yamlPlainScalar',              { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlTSField',                  { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlBlockMappingKey',          { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlTodo',                     { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlComment',                  { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlTAGDirective',             { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlYAMLDirective',            { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlReservedDirective',        { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlDirective',                { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlTagHandle',                { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlTagPrefix',                { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlYAMLVersion',              { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlFlowStringDelimiter',      { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlEscape',                   { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlKeyValueDelimiter',        { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlFlowString',               { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlSingleEscape',             { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlBlockScalarHeader',        { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlBool',                     { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlNull',                     { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlFlowMapping',              { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlFlowCollection',           { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlFlowMappingKey',           { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlFlowMappingMerge',         { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlPlainScalar',              { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlFloat',                    { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlTimestamp',                { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlInteger',                  { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlMappingKeyStart',          { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlFlowIndicator',            { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlBlockMappingMerge',        { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlBlockCollectionItemStart', { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlNodeTag',                  { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlAnchor',                   { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlAlias',                    { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlDocumentStart',            { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlDocumentEnd',              { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlDirectiveName',            { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlString',                   { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlConstant',                 { fg = colors.pink, bg = colors.greenLight })
end

return yml
