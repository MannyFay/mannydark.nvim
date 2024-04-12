-------------------------------------------------------------------------------
-- YML
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local yml      = {}


--------------------------------------------------------------
-- Settings

yml.setupHighlighting = function()
  highlight(0, 'yamlKeyValueDelimiter',        { fg = colors.white,      bg = 'NONE' })  -- Colon between key and value.
  highlight(0, 'yamlPlainScalar',              { fg = colors.white,      bg = 'NONE' })  -- Name of containers.
  highlight(0, 'yamlBlockMappingKey',          { fg = colors.blue,       bg = 'NONE' })  -- Name of services.
  highlight(0, 'yamlTodo',                     { fg = colors.red,        bg = 'NONE' })  -- TODO comments.
  highlight(0, 'yamlComment',                  { fg = colors.red,        bg = 'NONE' })  -- Comments.
  highlight(0, 'yamlInteger',                  { fg = colors.greenLight, bg = 'NONE' })  -- Integer numbers.
  highlight(0, 'yamlBlockCollectionItemStart', { fg = colors.white,      bg = 'NONE' })  -- Hyphens of list items.
  highlight(0, 'yamlFlowString',               { fg = colors.redLight,   bg = 'NONE' })  -- Content of strings in brackets [].
  highlight(0, 'yamlFlowIndicator',            { fg = colors.white,      bg = 'NONE' })  -- Brackets [] of flow strings.
  highlight(0, 'yamlFlowStringDelimiter',      { fg = colors.redLight,   bg = 'NONE' })  -- Quotes of flow strings.
  highlight(0, 'yamlFlowCollection',           { fg = colors.white,      bg = 'NONE' })  -- Commas of flow collections.
  highlight(0, 'yamlDocumentStart',            { fg = colors.blue,       bg = 'NONE' })  -- Three dashes at the beginning of a document.
  highlight(0, 'yamlBool',                     { fg = colors.blue,       bg = 'NONE' })  -- Boolean values.
  highlight(0, 'yamlFloat',                    { fg = colors.greenLight, bg = 'NONE' })  -- Float numbers.
  highlight(0, 'yamlEscape',                   { fg = colors.white,      bg = 'NONE' })  -- Escape characters.
  highlight(0, 'yamlFlowMapping',              { fg = colors.blue,       bg = 'NONE' })  -- Content between curly brackets {}.

  ----------------------- Not used by now:
  highlight(0, 'yamlTSField',                  { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlTAGDirective',             { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlYAMLDirective',            { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlReservedDirective',        { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlDirective',                { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlTagHandle',                { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlTagPrefix',                { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlYAMLVersion',              { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlSingleEscape',             { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlBlockScalarHeader',        { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlNull',                     { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlFlowMappingKey',           { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlFlowMappingMerge',         { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlTimestamp',                { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlMappingKeyStart',          { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlBlockMappingMerge',        { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlNodeTag',                  { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlAnchor',                   { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlAlias',                    { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlDocumentEnd',              { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlDirectiveName',            { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlString',                   { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'yamlConstant',                 { fg = colors.pink, bg = colors.greenLight })
end

return yml

