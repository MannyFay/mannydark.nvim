-------------------------------------------------------------------------------
-- CSS
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local css       = {}


--------------------------------------------------------------
-- Settings

css.setupHighlighting = function()
  highlight(0, 'cssAtKeyword',            { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssAtRule',               { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssAttr',                 { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssAttrComma',            { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssAttributeSelector',    { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssAttrRegion',           { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssBackgroundAttr',       { fg = colors.orange,     bg = 'NONE'     })
  highlight(0, 'cssBackgroundProp',       { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssBoxAttr',              { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssBoxProp',              { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssBraceError',           { fg = 'NONE',            bg = colors.red })
  highlight(0, 'cssBraces',               { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssClassName',            { fg = colors.turquoise,  bg = 'NONE'     })
  highlight(0, 'cssClassNameDot',         { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssColor',                { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssComment',              { fg = colors.red,        bg = 'NONE'     })
  highlight(0, 'cssCommonAttr',           { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssCustomProp',           { fg = colors.purple,     bg = 'NONE'     })
  highlight(0, 'cssError',                { fg = 'NONE',            bg = colors.red })
  highlight(0, 'cssFlexibleBoxAttr',      { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssFlexibleBoxProp',      { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssFontAttr',             { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssFontDescriptorProp',   { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssFontProp',             { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssFunction',             { fg = colors.orange,     bg = 'NONE'     })
  highlight(0, 'cssFunctionComma',        { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssFunctionName',         { fg = colors.orange,     bg = 'NONE'     })
  highlight(0, 'cssGeneratedContentProp', { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssGridProp',             { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssIdentifier',           { fg = colors.turquoise,  bg = 'NONE'     })
  highlight(0, 'cssImportant',            { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssKeyFrameProp',         { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssListProp',             { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssMediaProp',            { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssMultiColumnProp',      { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssNoise',                { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssPageProp',             { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssPositioningAttr',      { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssPositioningProp',      { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssProp',                 { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssPseudoClass',          { fg = colors.turquoise,  bg = 'NONE'     })
  highlight(0, 'cssPseudoClassId',        { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssSelectorOp',           { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssSelectorOp2',          { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssStringQQ',             { fg = colors.redLight,   bg = 'NONE'     })
  highlight(0, 'cssTableAttr',            { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssTagName',              { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssTextAttr',             { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssTextProp',             { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssTransformProp',        { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssTransitionAttr',       { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssUIAttr',               { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssUIProp',               { fg = colors.blue,       bg = 'NONE'     })
  highlight(0, 'cssUnitDecorators',       { fg = colors.white,      bg = 'NONE'     })
  highlight(0, 'cssValueAngle',           { fg = colors.greenLight, bg = 'NONE'     })
  highlight(0, 'cssValueLength',          { fg = colors.greenLight, bg = 'NONE'     })
  highlight(0, 'cssValueNumber',          { fg = colors.greenLight, bg = 'NONE'     })
  highlight(0, 'cssBorderProp',           { fg = colors.white,      bg = 'NONE'     })

  ----------------------- Not used by now:
  highlight(0, 'cssInclude',              { fg = colors.purple,     bg = 'NONE'     })
  highlight(0, 'cssPseudoClassLang',      { fg = colors.yellow,     bg = 'NONE'     })
  highlight(0, 'cssDefinition',           { fg = colors.greenLight, bg = 'NONE'     })
  highlight(0, 'cssVendor',               { fg = colors.orange,     bg = 'NONE'     })
  highlight(0, 'cssStyle',                { fg = colors.pink,       bg = 'NONE'     })
end

return css

