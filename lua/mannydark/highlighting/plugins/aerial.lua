-------------------------------------------------------------------------------
-- Aerial.nvim - Code outline / symbol navigation
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local aerial    = {}


-------------------------------------------------------------------------------
-- Settings

aerial.setupHighlighting = function()
  -- Base highlights
  highlight(0, 'AerialNormal',      { fg = colors.white,     bg = 'NONE'          })  -- Normal text
  highlight(0, 'AerialNormalFloat', { fg = colors.white,     bg = 'NONE'          })  -- Float window text
  highlight(0, 'AerialLine',        { fg = 'NONE',           bg = colors.grayDark })  -- Current line
  highlight(0, 'AerialLineNC',      { fg = 'NONE',           bg = colors.grayDark })  -- Current line (inactive)

  -- Visibility modifiers
  highlight(0, 'AerialPrivate',   { fg = colors.gray, bg = 'NONE' })  -- Private symbols
  highlight(0, 'AerialProtected', { fg = colors.gray, bg = 'NONE' })  -- Protected symbols

  -- Guide lines (indent)
  highlight(0, 'AerialGuide',  { fg = colors.grayDark, bg = 'NONE' })  -- Indent guide
  highlight(0, 'AerialGuide1', { fg = colors.grayDark, bg = 'NONE' })  -- Guide level 1
  highlight(0, 'AerialGuide2', { fg = colors.grayDark, bg = 'NONE' })  -- Guide level 2
  highlight(0, 'AerialGuide3', { fg = colors.grayDark, bg = 'NONE' })  -- Guide level 3
  highlight(0, 'AerialGuide4', { fg = colors.grayDark, bg = 'NONE' })  -- Guide level 4
  highlight(0, 'AerialGuide5', { fg = colors.grayDark, bg = 'NONE' })  -- Guide level 5
  highlight(0, 'AerialGuide6', { fg = colors.grayDark, bg = 'NONE' })  -- Guide level 6
  highlight(0, 'AerialGuide7', { fg = colors.grayDark, bg = 'NONE' })  -- Guide level 7
  highlight(0, 'AerialGuide8', { fg = colors.grayDark, bg = 'NONE' })  -- Guide level 8
  highlight(0, 'AerialGuide9', { fg = colors.grayDark, bg = 'NONE' })  -- Guide level 9

  -- Symbol kind text highlights
  highlight(0, 'AerialArray',         { fg = colors.orange,    bg = 'NONE' })  -- Array
  highlight(0, 'AerialBoolean',       { fg = colors.blue,      bg = 'NONE' })  -- Boolean
  highlight(0, 'AerialClass',         { fg = colors.turquoise, bg = 'NONE' })  -- Class
  highlight(0, 'AerialConstant',      { fg = colors.orange,    bg = 'NONE' })  -- Constant
  highlight(0, 'AerialConstructor',   { fg = colors.turquoise, bg = 'NONE' })  -- Constructor
  highlight(0, 'AerialEnum',          { fg = colors.turquoise, bg = 'NONE' })  -- Enum
  highlight(0, 'AerialEnumMember',    { fg = colors.purple,    bg = 'NONE' })  -- Enum member
  highlight(0, 'AerialEvent',         { fg = colors.orange,    bg = 'NONE' })  -- Event
  highlight(0, 'AerialField',         { fg = colors.purple,    bg = 'NONE' })  -- Field
  highlight(0, 'AerialFile',          { fg = colors.white,     bg = 'NONE' })  -- File
  highlight(0, 'AerialFunction',      { fg = colors.orange,    bg = 'NONE' })  -- Function
  highlight(0, 'AerialInterface',     { fg = colors.turquoise, bg = 'NONE' })  -- Interface
  highlight(0, 'AerialKey',           { fg = colors.purple,    bg = 'NONE' })  -- Key
  highlight(0, 'AerialMethod',        { fg = colors.orange,    bg = 'NONE' })  -- Method
  highlight(0, 'AerialModule',        { fg = colors.turquoise, bg = 'NONE' })  -- Module
  highlight(0, 'AerialNamespace',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespace
  highlight(0, 'AerialNull',          { fg = colors.blue,      bg = 'NONE' })  -- Null
  highlight(0, 'AerialNumber',        { fg = colors.greenLight, bg = 'NONE' }) -- Number
  highlight(0, 'AerialObject',        { fg = colors.turquoise, bg = 'NONE' })  -- Object
  highlight(0, 'AerialOperator',      { fg = colors.white,     bg = 'NONE' })  -- Operator
  highlight(0, 'AerialPackage',       { fg = colors.turquoise, bg = 'NONE' })  -- Package
  highlight(0, 'AerialProperty',      { fg = colors.purple,    bg = 'NONE' })  -- Property
  highlight(0, 'AerialString',        { fg = colors.redLight,  bg = 'NONE' })  -- String
  highlight(0, 'AerialStruct',        { fg = colors.turquoise, bg = 'NONE' })  -- Struct
  highlight(0, 'AerialTypeParameter', { fg = colors.turquoise, bg = 'NONE' })  -- Type parameter
  highlight(0, 'AerialVariable',      { fg = colors.purple,    bg = 'NONE' })  -- Variable

  -- Symbol kind icon highlights (same colors as text for consistency)
  highlight(0, 'AerialArrayIcon',         { fg = colors.orange,    bg = 'NONE' })
  highlight(0, 'AerialBooleanIcon',       { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'AerialClassIcon',         { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'AerialConstantIcon',      { fg = colors.orange,    bg = 'NONE' })
  highlight(0, 'AerialConstructorIcon',   { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'AerialEnumIcon',          { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'AerialEnumMemberIcon',    { fg = colors.purple,    bg = 'NONE' })
  highlight(0, 'AerialEventIcon',         { fg = colors.orange,    bg = 'NONE' })
  highlight(0, 'AerialFieldIcon',         { fg = colors.purple,    bg = 'NONE' })
  highlight(0, 'AerialFileIcon',          { fg = colors.white,     bg = 'NONE' })
  highlight(0, 'AerialFunctionIcon',      { fg = colors.orange,    bg = 'NONE' })
  highlight(0, 'AerialInterfaceIcon',     { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'AerialKeyIcon',           { fg = colors.purple,    bg = 'NONE' })
  highlight(0, 'AerialMethodIcon',        { fg = colors.orange,    bg = 'NONE' })
  highlight(0, 'AerialModuleIcon',        { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'AerialNamespaceIcon',     { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'AerialNullIcon',          { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'AerialNumberIcon',        { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, 'AerialObjectIcon',        { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'AerialOperatorIcon',      { fg = colors.white,     bg = 'NONE' })
  highlight(0, 'AerialPackageIcon',       { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'AerialPropertyIcon',      { fg = colors.purple,    bg = 'NONE' })
  highlight(0, 'AerialStringIcon',        { fg = colors.redLight,  bg = 'NONE' })
  highlight(0, 'AerialStructIcon',        { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'AerialTypeParameterIcon', { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'AerialVariableIcon',      { fg = colors.purple,    bg = 'NONE' })
end

return aerial
