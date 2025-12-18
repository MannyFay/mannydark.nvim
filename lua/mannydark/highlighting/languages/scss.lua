-------------------------------------------------------------------------------
-- SCSS Highlighting
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local scss      = {}


-------------------------------------------------------------------------------
-- Highlighting

scss.setupHighlighting = function()

  ---------------------------------------------------------------
  -- SCSS-Specific Syntax Groups (scss-syntax.vim)
  ---------------------------------------------------------------

  -- Variables
  highlight(0, 'scssVariable',                  { fg = colors.purple,     bg = 'NONE' })           -- $variable
  highlight(0, 'scssVariableValue',             { fg = colors.white,      bg = 'NONE' })           -- Variable value
  highlight(0, 'scssVariableAssignment',        { fg = colors.white,      bg = 'NONE' })           -- Assignment operator
  highlight(0, 'scssGlobal',                    { fg = colors.blue,       bg = 'NONE' })           -- !global flag
  highlight(0, 'scssDefault',                   { fg = colors.blue,       bg = 'NONE' })           -- !default flag

  -- Values & Types
  highlight(0, 'scssNull',                      { fg = colors.purple,     bg = 'NONE' })           -- null
  highlight(0, 'scssBoolean',                   { fg = colors.purple,     bg = 'NONE' })           -- true, false
  highlight(0, 'scssBooleanOp',                 { fg = colors.blue,       bg = 'NONE' })           -- and, or, not

  -- Mixins
  highlight(0, 'scssMixin',                     { fg = colors.blue,       bg = 'NONE' })           -- @mixin keyword
  highlight(0, 'scssMixinName',                 { fg = colors.orange,     bg = 'NONE' })           -- Mixin name
  highlight(0, 'scssMixinParams',               { fg = colors.purple,     bg = 'NONE' })           -- Mixin parameters
  highlight(0, 'scssContent',                   { fg = colors.blue,       bg = 'NONE' })           -- @content
  highlight(0, 'scssContentBlock',              { fg = colors.blue,       bg = 'NONE' })           -- Content block

  -- Functions
  highlight(0, 'scssFunctionDefinition',        { fg = colors.blue,       bg = 'NONE' })           -- @function keyword
  highlight(0, 'scssFunctionName',              { fg = colors.orange,     bg = 'NONE' })           -- Function name
  highlight(0, 'scssFunctionCall',              { fg = colors.orange,     bg = 'NONE' })           -- Function call
  highlight(0, 'scssReturn',                    { fg = colors.blue,       bg = 'NONE' })           -- @return

  -- Include & Extend
  highlight(0, 'scssInclude',                   { fg = colors.blue,       bg = 'NONE' })           -- @include
  highlight(0, 'scssExtend',                    { fg = colors.blue,       bg = 'NONE' })           -- @extend
  highlight(0, 'scssOptional',                  { fg = colors.blue,       bg = 'NONE' })           -- !optional flag
  highlight(0, 'scssPlaceholder',               { fg = colors.turquoise,  bg = 'NONE' })           -- %placeholder

  -- Imports & Modules (Sass Modules System)
  highlight(0, 'scssImport',                    { fg = colors.blue,       bg = 'NONE' })           -- @import
  highlight(0, 'scssUse',                       { fg = colors.blue,       bg = 'NONE' })           -- @use
  highlight(0, 'scssForward',                   { fg = colors.blue,       bg = 'NONE' })           -- @forward
  highlight(0, 'scssAs',                        { fg = colors.blue,       bg = 'NONE' })           -- as keyword
  highlight(0, 'scssWith',                      { fg = colors.blue,       bg = 'NONE' })           -- with keyword
  highlight(0, 'scssNamespace',                 { fg = colors.turquoise,  bg = 'NONE' })           -- Namespace prefix
  highlight(0, 'scssModuleName',                { fg = colors.redLight,   bg = 'NONE' })           -- Module name string

  -- Control Flow
  highlight(0, 'scssIf',                        { fg = colors.blue,       bg = 'NONE' })           -- @if
  highlight(0, 'scssElse',                      { fg = colors.blue,       bg = 'NONE' })           -- @else, @else if
  highlight(0, 'scssWhile',                     { fg = colors.blue,       bg = 'NONE' })           -- @while
  highlight(0, 'scssForKeyword',                { fg = colors.blue,       bg = 'NONE' })           -- @for
  highlight(0, 'scssEachKeyword',               { fg = colors.blue,       bg = 'NONE' })           -- @each
  highlight(0, 'scssFrom',                      { fg = colors.blue,       bg = 'NONE' })           -- from keyword
  highlight(0, 'scssTo',                        { fg = colors.blue,       bg = 'NONE' })           -- to keyword
  highlight(0, 'scssThrough',                   { fg = colors.blue,       bg = 'NONE' })           -- through keyword
  highlight(0, 'scssIn',                        { fg = colors.blue,       bg = 'NONE' })           -- in keyword

  -- Debug & Error
  highlight(0, 'scssDebug',                     { fg = colors.blue,       bg = 'NONE' })           -- @debug
  highlight(0, 'scssWarn',                      { fg = colors.yellow,     bg = 'NONE' })           -- @warn
  highlight(0, 'scssError',                     { fg = colors.red,        bg = 'NONE' })           -- @error

  -- At-Rules
  highlight(0, 'scssAtRoot',                    { fg = colors.blue,       bg = 'NONE' })           -- @at-root
  highlight(0, 'scssAtRule',                    { fg = colors.blue,       bg = 'NONE' })           -- Generic at-rules
  highlight(0, 'scssAtRuleKeyword',             { fg = colors.blue,       bg = 'NONE' })           -- At-rule keywords

  -- Selectors
  highlight(0, 'scssSelectorChar',              { fg = colors.blue,       bg = 'NONE' })           -- Selector chars (., #, etc.)
  highlight(0, 'scssSelectorName',              { fg = colors.turquoise,  bg = 'NONE' })           -- Selector names
  highlight(0, 'scssAmpersand',                 { fg = colors.blue,       bg = 'NONE' })           -- Parent selector &
  highlight(0, 'scssNesting',                   { fg = colors.white,      bg = 'NONE' })           -- Nested rules

  -- Properties
  highlight(0, 'scssNestedProperty',            { fg = colors.turquoise,  bg = 'NONE' })           -- Nested properties
  highlight(0, 'scssProperty',                  { fg = colors.turquoise,  bg = 'NONE' })           -- CSS properties
  highlight(0, 'scssPropertyValue',             { fg = colors.white,      bg = 'NONE' })           -- Property values

  -- Interpolation
  highlight(0, 'scssInterpolation',             { fg = colors.purple,     bg = 'NONE' })           -- #{...}
  highlight(0, 'scssInterpolationDelimiter',    { fg = colors.blue,       bg = 'NONE' })           -- #{ and }

  -- Maps & Lists
  highlight(0, 'scssMapParens',                 { fg = colors.white,      bg = 'NONE' })           -- Map parentheses
  highlight(0, 'scssMap',                       { fg = colors.white,      bg = 'NONE' })           -- Map content
  highlight(0, 'scssMapKey',                    { fg = colors.turquoise,  bg = 'NONE' })           -- Map keys
  highlight(0, 'scssMapValue',                  { fg = colors.white,      bg = 'NONE' })           -- Map values
  highlight(0, 'scssList',                      { fg = colors.white,      bg = 'NONE' })           -- Lists

  -- Comments
  highlight(0, 'scssComment',                   { fg = colors.red,        bg = 'NONE' })           -- // comments
  highlight(0, 'scssCssComment',                { fg = colors.red,        bg = 'NONE' })           -- /* */ comments
  highlight(0, 'scssStickyCommentChar',         { fg = colors.red,        bg = 'NONE' })           -- /*! */ sticky comments
  highlight(0, 'scssTodo',                      { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO in comments

  -- Operators
  highlight(0, 'scssOperator',                  { fg = colors.white,      bg = 'NONE' })           -- Math operators
  highlight(0, 'scssMathOperator',              { fg = colors.white,      bg = 'NONE' })           -- +, -, *, /, %
  highlight(0, 'scssComparisonOperator',        { fg = colors.white,      bg = 'NONE' })           -- ==, !=, <, >, <=, >=

  -- Strings
  highlight(0, 'scssString',                    { fg = colors.redLight,   bg = 'NONE' })           -- Strings
  highlight(0, 'scssStringDelimiter',           { fg = colors.redLight,   bg = 'NONE' })           -- String delimiters

  -- Numbers & Units
  highlight(0, 'scssNumber',                    { fg = colors.greenLight, bg = 'NONE' })           -- Numbers
  highlight(0, 'scssUnit',                      { fg = colors.greenLight, bg = 'NONE' })           -- Units (px, em, etc.)

  -- Colors
  highlight(0, 'scssColor',                     { fg = colors.greenLight, bg = 'NONE' })           -- Color values
  highlight(0, 'scssColorName',                 { fg = colors.greenLight, bg = 'NONE' })           -- Named colors

  ---------------------------------------------------------------
  -- CSS Base Syntax Groups (inherited by SCSS)
  ---------------------------------------------------------------

  -- Comments
  highlight(0, 'cssComment',                    { fg = colors.red,        bg = 'NONE' })

  -- Selectors
  highlight(0, 'cssTagName',                    { fg = colors.blue,       bg = 'NONE' })           -- HTML tag names
  highlight(0, 'cssSelectorOp',                 { fg = colors.white,      bg = 'NONE' })           -- Selector operators
  highlight(0, 'cssSelectorOp2',                { fg = colors.white,      bg = 'NONE' })           -- More selector ops
  highlight(0, 'cssClassName',                  { fg = colors.turquoise,  bg = 'NONE' })           -- .class
  highlight(0, 'cssClassNameDot',               { fg = colors.turquoise,  bg = 'NONE' })           -- The dot before class
  highlight(0, 'cssIdentifier',                 { fg = colors.turquoise,  bg = 'NONE' })           -- #id
  highlight(0, 'cssAttrComma',                  { fg = colors.white,      bg = 'NONE' })           -- Comma in selectors
  highlight(0, 'cssPseudoClass',                { fg = colors.turquoise,  bg = 'NONE' })           -- :pseudo-class
  highlight(0, 'cssPseudoClassId',              { fg = colors.turquoise,  bg = 'NONE' })           -- Pseudo-class id
  highlight(0, 'cssPseudoClassFn',              { fg = colors.turquoise,  bg = 'NONE' })           -- Pseudo-class function
  highlight(0, 'cssPseudoElement',              { fg = colors.turquoise,  bg = 'NONE' })           -- ::pseudo-element
  highlight(0, 'cssAttributeSelector',          { fg = colors.turquoise,  bg = 'NONE' })           -- [attribute]

  -- Properties (General)
  highlight(0, 'cssProp',                       { fg = colors.turquoise,  bg = 'NONE' })           -- CSS properties
  highlight(0, 'cssCustomProp',                 { fg = colors.purple,     bg = 'NONE' })           -- --custom-property
  highlight(0, 'cssVendor',                     { fg = colors.turquoise,  bg = 'NONE' })           -- -webkit-, -moz-, etc.

  -- Property Categories
  highlight(0, 'cssAnimationProp',              { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssBackgroundProp',             { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssBorderProp',                 { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssBoxProp',                    { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssColorProp',                  { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssDimensionProp',              { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssFlexibleBoxProp',            { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssFontProp',                   { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssGridProp',                   { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssListProp',                   { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssMarginProp',                 { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssMultiColumnProp',            { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssPaddingProp',                { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssPositioningProp',            { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssPrintProp',                  { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssTableProp',                  { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssTextProp',                   { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssTransformProp',              { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssTransitionProp',             { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'cssUIProp',                     { fg = colors.turquoise,  bg = 'NONE' })

  -- Attribute Values
  highlight(0, 'cssAttr',                       { fg = colors.white,      bg = 'NONE' })           -- General attributes
  highlight(0, 'cssAnimationAttr',              { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssBackgroundAttr',             { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssBorderAttr',                 { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssBoxAttr',                    { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssCommonAttr',                 { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssFlexibleBoxAttr',            { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssFontAttr',                   { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssGridAttr',                   { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssListAttr',                   { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssMultiColumnAttr',            { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssPositioningAttr',            { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssTableAttr',                  { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssTextAttr',                   { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssTransformAttr',              { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssTransitionAttr',             { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'cssUIAttr',                     { fg = colors.white,      bg = 'NONE' })

  -- Values
  highlight(0, 'cssColor',                      { fg = colors.greenLight, bg = 'NONE' })           -- Color values
  highlight(0, 'cssValueLength',                { fg = colors.greenLight, bg = 'NONE' })           -- Length values
  highlight(0, 'cssValueInteger',               { fg = colors.greenLight, bg = 'NONE' })           -- Integer values
  highlight(0, 'cssValueNumber',                { fg = colors.greenLight, bg = 'NONE' })           -- Number values
  highlight(0, 'cssValueAngle',                 { fg = colors.greenLight, bg = 'NONE' })           -- Angle values
  highlight(0, 'cssValueTime',                  { fg = colors.greenLight, bg = 'NONE' })           -- Time values
  highlight(0, 'cssValueFrequency',             { fg = colors.greenLight, bg = 'NONE' })           -- Frequency values
  highlight(0, 'cssUnitDecorators',             { fg = colors.greenLight, bg = 'NONE' })           -- Unit decorators

  -- Functions
  highlight(0, 'cssFunction',                   { fg = colors.orange,     bg = 'NONE' })           -- CSS functions
  highlight(0, 'cssFunctionName',               { fg = colors.orange,     bg = 'NONE' })           -- Function names
  highlight(0, 'cssFunctionComma',              { fg = colors.white,      bg = 'NONE' })           -- Comma in functions
  highlight(0, 'cssURL',                        { fg = colors.redLight,   bg = 'NONE' })           -- url()
  highlight(0, 'cssCalc',                       { fg = colors.orange,     bg = 'NONE' })           -- calc()
  highlight(0, 'cssGradient',                   { fg = colors.orange,     bg = 'NONE' })           -- Gradient functions

  -- At-Rules
  highlight(0, 'cssAtRule',                     { fg = colors.blue,       bg = 'NONE' })           -- @rules
  highlight(0, 'cssAtKeyword',                  { fg = colors.blue,       bg = 'NONE' })           -- @keyword
  highlight(0, 'cssMedia',                      { fg = colors.blue,       bg = 'NONE' })           -- @media
  highlight(0, 'cssMediaType',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Media types
  highlight(0, 'cssMediaFeature',               { fg = colors.turquoise,  bg = 'NONE' })           -- Media features
  highlight(0, 'cssMediaKeyword',               { fg = colors.blue,       bg = 'NONE' })           -- Media keywords
  highlight(0, 'cssKeyFrame',                   { fg = colors.blue,       bg = 'NONE' })           -- @keyframes
  highlight(0, 'cssKeyFrameSelector',           { fg = colors.turquoise,  bg = 'NONE' })           -- from, to, %
  highlight(0, 'cssFontDescriptor',             { fg = colors.blue,       bg = 'NONE' })           -- @font-face
  highlight(0, 'cssPage',                       { fg = colors.blue,       bg = 'NONE' })           -- @page
  highlight(0, 'cssPageMargin',                 { fg = colors.blue,       bg = 'NONE' })           -- Page margin
  highlight(0, 'cssSupports',                   { fg = colors.blue,       bg = 'NONE' })           -- @supports
  highlight(0, 'cssCharset',                    { fg = colors.blue,       bg = 'NONE' })           -- @charset
  highlight(0, 'cssNameSpace',                  { fg = colors.blue,       bg = 'NONE' })           -- @namespace
  highlight(0, 'cssImport',                     { fg = colors.blue,       bg = 'NONE' })           -- @import

  -- Important
  highlight(0, 'cssImportant',                  { fg = colors.red,        bg = 'NONE', bold = true })  -- !important

  -- Strings
  highlight(0, 'cssStringQ',                    { fg = colors.redLight,   bg = 'NONE' })           -- Single quoted
  highlight(0, 'cssStringQQ',                   { fg = colors.redLight,   bg = 'NONE' })           -- Double quoted

  -- Punctuation
  highlight(0, 'cssBraces',                     { fg = colors.white,      bg = 'NONE' })           -- { }
  highlight(0, 'cssNoise',                      { fg = colors.white,      bg = 'NONE' })           -- ; :
  highlight(0, 'cssUnicodeEscape',              { fg = colors.purple,     bg = 'NONE' })           -- Unicode escapes

  -- Errors
  highlight(0, 'cssBraceError',                 { fg = colors.red,        bg = 'NONE' })           -- Brace errors
  highlight(0, 'cssError',                      { fg = colors.red,        bg = 'NONE' })           -- General errors

  -- Hacks
  highlight(0, 'cssHacks',                      { fg = colors.grey,       bg = 'NONE' })           -- Browser hacks

  ---------------------------------------------------------------
  -- Treesitter Captures (@xxx.scss)
  ---------------------------------------------------------------

  -- Keywords
  highlight(0, '@keyword.scss',                         { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.function.scss',                { fg = colors.blue,       bg = 'NONE' })       -- @function, @mixin
  highlight(0, '@keyword.return.scss',                  { fg = colors.blue,       bg = 'NONE' })       -- @return
  highlight(0, '@keyword.import.scss',                  { fg = colors.blue,       bg = 'NONE' })       -- @import, @use, @forward
  highlight(0, '@keyword.repeat.scss',                  { fg = colors.blue,       bg = 'NONE' })       -- @for, @each, @while
  highlight(0, '@keyword.conditional.scss',             { fg = colors.blue,       bg = 'NONE' })       -- @if, @else
  highlight(0, '@keyword.directive.scss',               { fg = colors.blue,       bg = 'NONE' })       -- At-rules
  highlight(0, '@keyword.operator.scss',                { fg = colors.blue,       bg = 'NONE' })       -- and, or, not
  highlight(0, '@keyword.modifier.scss',                { fg = colors.blue,       bg = 'NONE' })       -- !default, !global

  -- Functions
  highlight(0, '@function.scss',                        { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.call.scss',                   { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.builtin.scss',                { fg = colors.orange,     bg = 'NONE' })

  -- Variables
  highlight(0, '@variable.scss',                        { fg = colors.purple,     bg = 'NONE' })       -- $variables
  highlight(0, '@variable.parameter.scss',              { fg = colors.purple,     bg = 'NONE' })       -- Function parameters
  highlight(0, '@variable.builtin.scss',                { fg = colors.purple,     bg = 'NONE' })       -- Built-in variables

  -- Properties
  highlight(0, '@property.scss',                        { fg = colors.turquoise,  bg = 'NONE' })       -- CSS properties
  highlight(0, '@property.class.scss',                  { fg = colors.turquoise,  bg = 'NONE' })

  -- Types & Tags
  highlight(0, '@type.scss',                            { fg = colors.turquoise,  bg = 'NONE' })       -- Type selectors
  highlight(0, '@tag.scss',                             { fg = colors.blue,       bg = 'NONE' })       -- HTML tags
  highlight(0, '@tag.attribute.scss',                   { fg = colors.turquoise,  bg = 'NONE' })

  -- Constants
  highlight(0, '@constant.scss',                        { fg = colors.purple,     bg = 'NONE' })       -- Constants
  highlight(0, '@constant.builtin.scss',                { fg = colors.purple,     bg = 'NONE' })       -- null, true, false

  -- Strings
  highlight(0, '@string.scss',                          { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@string.special.scss',                  { fg = colors.purple,     bg = 'NONE' })

  -- Numbers
  highlight(0, '@number.scss',                          { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@number.float.scss',                    { fg = colors.greenLight, bg = 'NONE' })

  -- Operators
  highlight(0, '@operator.scss',                        { fg = colors.white,      bg = 'NONE' })

  -- Punctuation
  highlight(0, '@punctuation.bracket.scss',             { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@punctuation.delimiter.scss',           { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@punctuation.special.scss',             { fg = colors.blue,       bg = 'NONE' })       -- #{ }

  -- Attributes
  highlight(0, '@attribute.scss',                       { fg = colors.turquoise,  bg = 'NONE' })

  -- Modules
  highlight(0, '@module.scss',                          { fg = colors.turquoise,  bg = 'NONE' })       -- Namespace modules

  -- Comments
  highlight(0, '@comment.scss',                         { fg = colors.red,        bg = 'NONE' })
  highlight(0, '@comment.documentation.scss',           { fg = colors.red,        bg = 'NONE' })
  highlight(0, '@spell.scss',                           { fg = 'NONE',            bg = 'NONE' })

  -- Character Special
  highlight(0, '@character.special.scss',               { fg = colors.blue,       bg = 'NONE' })       -- &, *, etc.

  ---------------------------------------------------------------
  -- Treesitter Captures (@xxx.css) - Inherited
  ---------------------------------------------------------------

  highlight(0, '@keyword.css',                          { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.directive.css',                { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.import.css',                   { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.operator.css',                 { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.modifier.css',                 { fg = colors.blue,       bg = 'NONE' })

  highlight(0, '@function.css',                         { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.builtin.css',                 { fg = colors.orange,     bg = 'NONE' })

  highlight(0, '@variable.css',                         { fg = colors.purple,     bg = 'NONE' })

  highlight(0, '@property.css',                         { fg = colors.turquoise,  bg = 'NONE' })

  highlight(0, '@type.css',                             { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@tag.css',                              { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@tag.attribute.css',                    { fg = colors.turquoise,  bg = 'NONE' })

  highlight(0, '@constant.css',                         { fg = colors.purple,     bg = 'NONE' })

  highlight(0, '@string.css',                           { fg = colors.redLight,   bg = 'NONE' })

  highlight(0, '@number.css',                           { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@number.float.css',                     { fg = colors.greenLight, bg = 'NONE' })

  highlight(0, '@operator.css',                         { fg = colors.white,      bg = 'NONE' })

  highlight(0, '@punctuation.bracket.css',              { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@punctuation.delimiter.css',            { fg = colors.white,      bg = 'NONE' })

  highlight(0, '@attribute.css',                        { fg = colors.turquoise,  bg = 'NONE' })

  highlight(0, '@module.css',                           { fg = colors.turquoise,  bg = 'NONE' })

  highlight(0, '@comment.css',                          { fg = colors.red,        bg = 'NONE' })

  highlight(0, '@character.special.css',                { fg = colors.blue,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- SCSS Built-in Functions
  ---------------------------------------------------------------

  -- Color Functions
  highlight(0, 'scssFuncRgb',                   { fg = colors.orange,     bg = 'NONE' })           -- rgb(), rgba()
  highlight(0, 'scssFuncHsl',                   { fg = colors.orange,     bg = 'NONE' })           -- hsl(), hsla()
  highlight(0, 'scssFuncAdjustHue',             { fg = colors.orange,     bg = 'NONE' })           -- adjust-hue()
  highlight(0, 'scssFuncLighten',               { fg = colors.orange,     bg = 'NONE' })           -- lighten()
  highlight(0, 'scssFuncDarken',                { fg = colors.orange,     bg = 'NONE' })           -- darken()
  highlight(0, 'scssFuncSaturate',              { fg = colors.orange,     bg = 'NONE' })           -- saturate()
  highlight(0, 'scssFuncDesaturate',            { fg = colors.orange,     bg = 'NONE' })           -- desaturate()
  highlight(0, 'scssFuncGrayscale',             { fg = colors.orange,     bg = 'NONE' })           -- grayscale()
  highlight(0, 'scssFuncComplement',            { fg = colors.orange,     bg = 'NONE' })           -- complement()
  highlight(0, 'scssFuncInvert',                { fg = colors.orange,     bg = 'NONE' })           -- invert()
  highlight(0, 'scssFuncMix',                   { fg = colors.orange,     bg = 'NONE' })           -- mix()
  highlight(0, 'scssFuncOpacify',               { fg = colors.orange,     bg = 'NONE' })           -- opacify(), fade-in()
  highlight(0, 'scssFuncTransparentize',        { fg = colors.orange,     bg = 'NONE' })           -- transparentize(), fade-out()
  highlight(0, 'scssFuncAdjustColor',           { fg = colors.orange,     bg = 'NONE' })           -- adjust-color()
  highlight(0, 'scssFuncScaleColor',            { fg = colors.orange,     bg = 'NONE' })           -- scale-color()
  highlight(0, 'scssFuncChangeColor',           { fg = colors.orange,     bg = 'NONE' })           -- change-color()
  highlight(0, 'scssFuncIeHexStr',              { fg = colors.orange,     bg = 'NONE' })           -- ie-hex-str()

  -- String Functions
  highlight(0, 'scssFuncUnquote',               { fg = colors.orange,     bg = 'NONE' })           -- unquote()
  highlight(0, 'scssFuncQuote',                 { fg = colors.orange,     bg = 'NONE' })           -- quote()
  highlight(0, 'scssFuncStrLength',             { fg = colors.orange,     bg = 'NONE' })           -- str-length()
  highlight(0, 'scssFuncStrInsert',             { fg = colors.orange,     bg = 'NONE' })           -- str-insert()
  highlight(0, 'scssFuncStrIndex',              { fg = colors.orange,     bg = 'NONE' })           -- str-index()
  highlight(0, 'scssFuncStrSlice',              { fg = colors.orange,     bg = 'NONE' })           -- str-slice()
  highlight(0, 'scssFuncToUpperCase',           { fg = colors.orange,     bg = 'NONE' })           -- to-upper-case()
  highlight(0, 'scssFuncToLowerCase',           { fg = colors.orange,     bg = 'NONE' })           -- to-lower-case()

  -- Number Functions
  highlight(0, 'scssFuncPercentage',            { fg = colors.orange,     bg = 'NONE' })           -- percentage()
  highlight(0, 'scssFuncRound',                 { fg = colors.orange,     bg = 'NONE' })           -- round()
  highlight(0, 'scssFuncCeil',                  { fg = colors.orange,     bg = 'NONE' })           -- ceil()
  highlight(0, 'scssFuncFloor',                 { fg = colors.orange,     bg = 'NONE' })           -- floor()
  highlight(0, 'scssFuncAbs',                   { fg = colors.orange,     bg = 'NONE' })           -- abs()
  highlight(0, 'scssFuncMin',                   { fg = colors.orange,     bg = 'NONE' })           -- min()
  highlight(0, 'scssFuncMax',                   { fg = colors.orange,     bg = 'NONE' })           -- max()
  highlight(0, 'scssFuncRandom',                { fg = colors.orange,     bg = 'NONE' })           -- random()

  -- List Functions
  highlight(0, 'scssFuncLength',                { fg = colors.orange,     bg = 'NONE' })           -- length()
  highlight(0, 'scssFuncNth',                   { fg = colors.orange,     bg = 'NONE' })           -- nth()
  highlight(0, 'scssFuncSetNth',                { fg = colors.orange,     bg = 'NONE' })           -- set-nth()
  highlight(0, 'scssFuncJoin',                  { fg = colors.orange,     bg = 'NONE' })           -- join()
  highlight(0, 'scssFuncAppend',                { fg = colors.orange,     bg = 'NONE' })           -- append()
  highlight(0, 'scssFuncZip',                   { fg = colors.orange,     bg = 'NONE' })           -- zip()
  highlight(0, 'scssFuncIndex',                 { fg = colors.orange,     bg = 'NONE' })           -- index()
  highlight(0, 'scssFuncListSeparator',         { fg = colors.orange,     bg = 'NONE' })           -- list-separator()
  highlight(0, 'scssFuncIsBracketed',           { fg = colors.orange,     bg = 'NONE' })           -- is-bracketed()

  -- Map Functions
  highlight(0, 'scssFuncMapGet',                { fg = colors.orange,     bg = 'NONE' })           -- map-get()
  highlight(0, 'scssFuncMapMerge',              { fg = colors.orange,     bg = 'NONE' })           -- map-merge()
  highlight(0, 'scssFuncMapRemove',             { fg = colors.orange,     bg = 'NONE' })           -- map-remove()
  highlight(0, 'scssFuncMapKeys',               { fg = colors.orange,     bg = 'NONE' })           -- map-keys()
  highlight(0, 'scssFuncMapValues',             { fg = colors.orange,     bg = 'NONE' })           -- map-values()
  highlight(0, 'scssFuncMapHasKey',             { fg = colors.orange,     bg = 'NONE' })           -- map-has-key()

  -- Selector Functions
  highlight(0, 'scssFuncSelectorNest',          { fg = colors.orange,     bg = 'NONE' })           -- selector-nest()
  highlight(0, 'scssFuncSelectorAppend',        { fg = colors.orange,     bg = 'NONE' })           -- selector-append()
  highlight(0, 'scssFuncSelectorExtend',        { fg = colors.orange,     bg = 'NONE' })           -- selector-extend()
  highlight(0, 'scssFuncSelectorReplace',       { fg = colors.orange,     bg = 'NONE' })           -- selector-replace()
  highlight(0, 'scssFuncSelectorUnify',         { fg = colors.orange,     bg = 'NONE' })           -- selector-unify()
  highlight(0, 'scssFuncIsSuperselector',       { fg = colors.orange,     bg = 'NONE' })           -- is-superselector()
  highlight(0, 'scssFuncSimpleSelectors',       { fg = colors.orange,     bg = 'NONE' })           -- simple-selectors()
  highlight(0, 'scssFuncSelectorParse',         { fg = colors.orange,     bg = 'NONE' })           -- selector-parse()

  -- Introspection Functions
  highlight(0, 'scssFuncFeatureExists',         { fg = colors.orange,     bg = 'NONE' })           -- feature-exists()
  highlight(0, 'scssFuncVariableExists',        { fg = colors.orange,     bg = 'NONE' })           -- variable-exists()
  highlight(0, 'scssFuncGlobalVariableExists',  { fg = colors.orange,     bg = 'NONE' })           -- global-variable-exists()
  highlight(0, 'scssFuncFunctionExists',        { fg = colors.orange,     bg = 'NONE' })           -- function-exists()
  highlight(0, 'scssFuncMixinExists',           { fg = colors.orange,     bg = 'NONE' })           -- mixin-exists()
  highlight(0, 'scssFuncContentExists',         { fg = colors.orange,     bg = 'NONE' })           -- content-exists()
  highlight(0, 'scssFuncInspect',               { fg = colors.orange,     bg = 'NONE' })           -- inspect()
  highlight(0, 'scssFuncType',                  { fg = colors.orange,     bg = 'NONE' })           -- type-of()
  highlight(0, 'scssFuncUnit',                  { fg = colors.orange,     bg = 'NONE' })           -- unit()
  highlight(0, 'scssFuncUnitless',              { fg = colors.orange,     bg = 'NONE' })           -- unitless()
  highlight(0, 'scssFuncComparable',            { fg = colors.orange,     bg = 'NONE' })           -- comparable()
  highlight(0, 'scssFuncCall',                  { fg = colors.orange,     bg = 'NONE' })           -- call()
  highlight(0, 'scssFuncGetFunction',           { fg = colors.orange,     bg = 'NONE' })           -- get-function()

  -- Meta Functions (Dart Sass)
  highlight(0, 'scssFuncMetaLoad',              { fg = colors.orange,     bg = 'NONE' })           -- meta.load-css()
  highlight(0, 'scssFuncMetaModuleVariables',   { fg = colors.orange,     bg = 'NONE' })           -- meta.module-variables()
  highlight(0, 'scssFuncMetaModuleFunctions',   { fg = colors.orange,     bg = 'NONE' })           -- meta.module-functions()

  -- Math Functions (Dart Sass)
  highlight(0, 'scssFuncMathDiv',               { fg = colors.orange,     bg = 'NONE' })           -- math.div()
  highlight(0, 'scssFuncMathClamp',             { fg = colors.orange,     bg = 'NONE' })           -- math.clamp()
  highlight(0, 'scssFuncMathSin',               { fg = colors.orange,     bg = 'NONE' })           -- math.sin()
  highlight(0, 'scssFuncMathCos',               { fg = colors.orange,     bg = 'NONE' })           -- math.cos()
  highlight(0, 'scssFuncMathTan',               { fg = colors.orange,     bg = 'NONE' })           -- math.tan()
  highlight(0, 'scssFuncMathAsin',              { fg = colors.orange,     bg = 'NONE' })           -- math.asin()
  highlight(0, 'scssFuncMathAcos',              { fg = colors.orange,     bg = 'NONE' })           -- math.acos()
  highlight(0, 'scssFuncMathAtan',              { fg = colors.orange,     bg = 'NONE' })           -- math.atan()
  highlight(0, 'scssFuncMathAtan2',             { fg = colors.orange,     bg = 'NONE' })           -- math.atan2()
  highlight(0, 'scssFuncMathSqrt',              { fg = colors.orange,     bg = 'NONE' })           -- math.sqrt()
  highlight(0, 'scssFuncMathLog',               { fg = colors.orange,     bg = 'NONE' })           -- math.log()
  highlight(0, 'scssFuncMathPow',               { fg = colors.orange,     bg = 'NONE' })           -- math.pow()
  highlight(0, 'scssFuncMathHypot',             { fg = colors.orange,     bg = 'NONE' })           -- math.hypot()

  ---------------------------------------------------------------
  -- LSP Semantic Tokens
  ---------------------------------------------------------------

  highlight(0, '@lsp.type.class.scss',                  { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.property.scss',               { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.function.scss',               { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.variable.scss',               { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.parameter.scss',              { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.scss',                { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.string.scss',                 { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@lsp.type.number.scss',                 { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@lsp.type.operator.scss',               { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.comment.scss',                { fg = colors.red,        bg = 'NONE' })

  highlight(0, '@lsp.type.class.css',                   { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.property.css',                { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.function.css',                { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.variable.css',                { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.css',                 { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.string.css',                  { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@lsp.type.number.css',                  { fg = colors.greenLight, bg = 'NONE' })

  ---------------------------------------------------------------
  -- Tailwind CSS (commonly used with SCSS)
  ---------------------------------------------------------------

  highlight(0, 'tailwindClass',                 { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'tailwindModifier',              { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'tailwindArbitrary',             { fg = colors.purple,     bg = 'NONE' })

  ---------------------------------------------------------------
  -- PostCSS (often used with SCSS in build pipelines)
  ---------------------------------------------------------------

  highlight(0, 'postcssAtRule',                 { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'postcssNesting',                { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'postcssCustomMedia',            { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'postcssApply',                  { fg = colors.blue,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- Plugin Support
  ---------------------------------------------------------------

  -- nvim-cmp
  highlight(0, 'CmpItemKindScss',               { fg = colors.pink,       bg = 'NONE' })
  highlight(0, 'CmpItemKindCss',                { fg = colors.blue,       bg = 'NONE' })

  -- vim-css-color (color preview)
  highlight(0, 'cssColorRed',                   { fg = '#FF0000',         bg = 'NONE' })
  highlight(0, 'cssColorGreen',                 { fg = '#00FF00',         bg = 'NONE' })
  highlight(0, 'cssColorBlue',                  { fg = '#0000FF',         bg = 'NONE' })

end

return scss
