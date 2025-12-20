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
  highlight(0, 'scssVariable',                  { link = "Variable" })           -- $variable
  highlight(0, 'scssVariableValue',             { link = "Variable" })           -- Variable value
  highlight(0, 'scssVariableAssignment',        { link = "Variable" })           -- Assignment operator
  highlight(0, 'scssGlobal',                    { fg = colors.blue,       bg = 'NONE' })           -- !global flag
  highlight(0, 'scssDefault',                   { fg = colors.blue,       bg = 'NONE' })           -- !default flag

  -- Values & Types
  highlight(0, 'scssNull',                      { fg = colors.purple,     bg = 'NONE' })           -- null
  highlight(0, 'scssBoolean',                   { link = "Boolean" })           -- true, false
  highlight(0, 'scssBooleanOp',                 { link = "Boolean" })           -- and, or, not

  -- Mixins
  highlight(0, 'scssMixin',                     { fg = colors.blue,       bg = 'NONE' })           -- @mixin keyword
  highlight(0, 'scssMixinName',                 { fg = colors.orange,     bg = 'NONE' })           -- Mixin name
  highlight(0, 'scssMixinParams',               { fg = colors.purple,     bg = 'NONE' })           -- Mixin parameters
  highlight(0, 'scssContent',                   { fg = colors.blue,       bg = 'NONE' })           -- @content
  highlight(0, 'scssContentBlock',              { fg = colors.blue,       bg = 'NONE' })           -- Content block

  -- Functions
  highlight(0, 'scssFunctionDefinition',        { link = "Function" })           -- @function keyword
  highlight(0, 'scssFunctionName',              { link = "Function" })           -- Function name
  highlight(0, 'scssFunctionCall',              { link = "Function" })           -- Function call
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
  highlight(0, 'scssForKeyword',                { link = "Keyword" })           -- @for
  highlight(0, 'scssEachKeyword',               { link = "Keyword" })           -- @each
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
  highlight(0, 'scssAtRuleKeyword',             { link = "Keyword" })           -- At-rule keywords

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
  highlight(0, 'scssInterpolationDelimiter',    { link = "Delimiter" })           -- #{ and }

  -- Maps & Lists
  highlight(0, 'scssMapParens',                 { fg = colors.white,      bg = 'NONE' })           -- Map parentheses
  highlight(0, 'scssMap',                       { fg = colors.white,      bg = 'NONE' })           -- Map content
  highlight(0, 'scssMapKey',                    { fg = colors.turquoise,  bg = 'NONE' })           -- Map keys
  highlight(0, 'scssMapValue',                  { fg = colors.white,      bg = 'NONE' })           -- Map values
  highlight(0, 'scssList',                      { fg = colors.white,      bg = 'NONE' })           -- Lists

  -- Comments
  highlight(0, 'scssComment',                   { link = "Comment" })           -- // comments
  highlight(0, 'scssCssComment',                { link = "Comment" })           -- /* */ comments
  highlight(0, 'scssStickyCommentChar',         { link = "Comment" })           -- /*! */ sticky comments
  highlight(0, 'scssTodo',                      { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO in comments

  -- Operators
  highlight(0, 'scssOperator',                  { link = "Operator" })           -- Math operators
  highlight(0, 'scssMathOperator',              { link = "Operator" })           -- +, -, *, /, %
  highlight(0, 'scssComparisonOperator',        { link = "Operator" })           -- ==, !=, <, >, <=, >=

  -- Strings
  highlight(0, 'scssString',                    { link = "String" })           -- Strings
  highlight(0, 'scssStringDelimiter',           { link = "Delimiter" })           -- String delimiters

  -- Numbers & Units
  highlight(0, 'scssNumber',                    { link = "Number" })           -- Numbers
  highlight(0, 'scssUnit',                      { fg = colors.greenLight, bg = 'NONE' })           -- Units (px, em, etc.)

  -- Colors
  highlight(0, 'scssColor',                     { fg = colors.greenLight, bg = 'NONE' })           -- Color values
  highlight(0, 'scssColorName',                 { fg = colors.greenLight, bg = 'NONE' })           -- Named colors

  ---------------------------------------------------------------
  -- CSS Base Syntax Groups (inherited by SCSS)
  ---------------------------------------------------------------

  -- Comments
  highlight(0, 'cssComment',                    { link = "Comment" })

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
  highlight(0, 'cssValueNumber',                { link = "Number" })           -- Number values
  highlight(0, 'cssValueAngle',                 { fg = colors.greenLight, bg = 'NONE' })           -- Angle values
  highlight(0, 'cssValueTime',                  { fg = colors.greenLight, bg = 'NONE' })           -- Time values
  highlight(0, 'cssValueFrequency',             { fg = colors.greenLight, bg = 'NONE' })           -- Frequency values
  highlight(0, 'cssUnitDecorators',             { fg = colors.greenLight, bg = 'NONE' })           -- Unit decorators

  -- Functions
  highlight(0, 'cssFunction',                   { link = "Function" })           -- CSS functions
  highlight(0, 'cssFunctionName',               { link = "Function" })           -- Function names
  highlight(0, 'cssFunctionComma',              { link = "Function" })           -- Comma in functions
  highlight(0, 'cssURL',                        { fg = colors.redLight,   bg = 'NONE' })           -- url()
  highlight(0, 'cssCalc',                       { fg = colors.orange,     bg = 'NONE' })           -- calc()
  highlight(0, 'cssGradient',                   { fg = colors.orange,     bg = 'NONE' })           -- Gradient functions

  -- At-Rules
  highlight(0, 'cssAtRule',                     { fg = colors.blue,       bg = 'NONE' })           -- @rules
  highlight(0, 'cssAtKeyword',                  { link = "Keyword" })           -- @keyword
  highlight(0, 'cssMedia',                      { fg = colors.blue,       bg = 'NONE' })           -- @media
  highlight(0, 'cssMediaType',                  { link = "Type" })           -- Media types
  highlight(0, 'cssMediaFeature',               { fg = colors.turquoise,  bg = 'NONE' })           -- Media features
  highlight(0, 'cssMediaKeyword',               { link = "Keyword" })           -- Media keywords
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
  highlight(0, 'cssStringQ',                    { link = "String" })           -- Single quoted
  highlight(0, 'cssStringQQ',                   { link = "String" })           -- Double quoted

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
  highlight(0, '@keyword.scss',                         { link = "Keyword" })
  highlight(0, '@keyword.function.scss',                { link = "Keyword" })       -- @function, @mixin
  highlight(0, '@keyword.return.scss',                  { link = "Keyword" })       -- @return
  highlight(0, '@keyword.import.scss',                  { link = "Keyword" })       -- @import, @use, @forward
  highlight(0, '@keyword.repeat.scss',                  { link = "Keyword" })       -- @for, @each, @while
  highlight(0, '@keyword.conditional.scss',             { link = "Conditional" })       -- @if, @else
  highlight(0, '@keyword.directive.scss',               { link = "Keyword" })       -- At-rules
  highlight(0, '@keyword.operator.scss',                { link = "Operator" })       -- and, or, not
  highlight(0, '@keyword.modifier.scss',                { link = "Keyword" })       -- !default, !global

  -- Functions
  highlight(0, '@function.scss',                        { link = "Function" })
  highlight(0, '@function.call.scss',                   { link = "Function" })
  highlight(0, '@function.builtin.scss',                { link = "Function" })

  -- Variables
  highlight(0, '@variable.scss',                        { link = "Variable" })       -- $variables
  highlight(0, '@variable.parameter.scss',              { link = "Variable" })       -- Function parameters
  highlight(0, '@variable.builtin.scss',                { link = "Variable" })       -- Built-in variables

  -- Properties
  highlight(0, '@property.scss',                        { fg = colors.turquoise,  bg = 'NONE' })       -- CSS properties
  highlight(0, '@property.class.scss',                  { fg = colors.turquoise,  bg = 'NONE' })

  -- Types & Tags
  highlight(0, '@type.scss',                            { link = "Type" })       -- Type selectors
  highlight(0, '@tag.scss',                             { fg = colors.blue,       bg = 'NONE' })       -- HTML tags
  highlight(0, '@tag.attribute.scss',                   { fg = colors.turquoise,  bg = 'NONE' })

  -- Constants
  highlight(0, '@constant.scss',                        { link = "Constant" })       -- Constants
  highlight(0, '@constant.builtin.scss',                { link = "Constant" })       -- null, true, false

  -- Strings
  highlight(0, '@string.scss',                          { link = "String" })
  highlight(0, '@string.special.scss',                  { link = "String" })

  -- Numbers
  highlight(0, '@number.scss',                          { link = "Number" })
  highlight(0, '@number.float.scss',                    { link = "Number" })

  -- Operators
  highlight(0, '@operator.scss',                        { link = "Operator" })

  -- Punctuation
  highlight(0, '@punctuation.bracket.scss',             { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@punctuation.delimiter.scss',           { link = "Delimiter" })
  highlight(0, '@punctuation.special.scss',             { fg = colors.blue,       bg = 'NONE' })       -- #{ }

  -- Attributes
  highlight(0, '@attribute.scss',                       { fg = colors.turquoise,  bg = 'NONE' })

  -- Modules
  highlight(0, '@module.scss',                          { fg = colors.turquoise,  bg = 'NONE' })       -- Namespace modules

  -- Comments
  highlight(0, '@comment.scss',                         { link = "Comment" })
  highlight(0, '@comment.documentation.scss',           { link = "Comment" })
  highlight(0, '@spell.scss',                           { fg = 'NONE',            bg = 'NONE' })

  -- Character Special
  highlight(0, '@character.special.scss',               { fg = colors.blue,       bg = 'NONE' })       -- &, *, etc.

  ---------------------------------------------------------------
  -- Treesitter Captures (@xxx.scss)
  ---------------------------------------------------------------

  highlight(0, '@keyword.scss',                         { link = "Keyword" })
  highlight(0, '@keyword.directive.scss',               { link = "Keyword" })
  highlight(0, '@keyword.import.scss',                  { link = "Keyword" })
  highlight(0, '@keyword.operator.scss',                { link = "Operator" })
  highlight(0, '@keyword.modifier.scss',                { link = "Keyword" })

  highlight(0, '@function.scss',                        { link = "Function" })
  highlight(0, '@function.builtin.scss',                { link = "Function" })

  highlight(0, '@variable.scss',                        { link = "Variable" })

  highlight(0, '@property.scss',                        { fg = colors.turquoise,  bg = 'NONE' })

  highlight(0, '@type.scss',                            { link = "Type" })
  highlight(0, '@tag.scss',                             { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@tag.attribute.scss',                   { fg = colors.turquoise,  bg = 'NONE' })

  highlight(0, '@constant.scss',                        { link = "Constant" })

  highlight(0, '@string.scss',                          { link = "String" })

  highlight(0, '@number.scss',                          { link = "Number" })
  highlight(0, '@number.float.scss',                    { link = "Number" })

  highlight(0, '@operator.scss',                        { link = "Operator" })

  highlight(0, '@punctuation.bracket.scss',             { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@punctuation.delimiter.scss',           { link = "Delimiter" })

  highlight(0, '@attribute.scss',                       { fg = colors.turquoise,  bg = 'NONE' })

  highlight(0, '@module.scss',                          { fg = colors.turquoise,  bg = 'NONE' })

  highlight(0, '@comment.scss',                         { link = "Comment" })

  highlight(0, '@character.special.scss',               { fg = colors.blue,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- SCSS Built-in Functions
  ---------------------------------------------------------------

  -- Color Functions
  highlight(0, 'scssFuncRgb',                   { link = "Function" })           -- rgb(), rgba()
  highlight(0, 'scssFuncHsl',                   { link = "Function" })           -- hsl(), hsla()
  highlight(0, 'scssFuncAdjustHue',             { link = "Function" })           -- adjust-hue()
  highlight(0, 'scssFuncLighten',               { link = "Function" })           -- lighten()
  highlight(0, 'scssFuncDarken',                { link = "Function" })           -- darken()
  highlight(0, 'scssFuncSaturate',              { link = "Function" })           -- saturate()
  highlight(0, 'scssFuncDesaturate',            { link = "Function" })           -- desaturate()
  highlight(0, 'scssFuncGrayscale',             { link = "Function" })           -- grayscale()
  highlight(0, 'scssFuncComplement',            { link = "Function" })           -- complement()
  highlight(0, 'scssFuncInvert',                { link = "Function" })           -- invert()
  highlight(0, 'scssFuncMix',                   { link = "Function" })           -- mix()
  highlight(0, 'scssFuncOpacify',               { link = "Function" })           -- opacify(), fade-in()
  highlight(0, 'scssFuncTransparentize',        { link = "Function" })           -- transparentize(), fade-out()
  highlight(0, 'scssFuncAdjustColor',           { link = "Function" })           -- adjust-color()
  highlight(0, 'scssFuncScaleColor',            { link = "Function" })           -- scale-color()
  highlight(0, 'scssFuncChangeColor',           { link = "Function" })           -- change-color()
  highlight(0, 'scssFuncIeHexStr',              { link = "Function" })           -- ie-hex-str()

  -- String Functions
  highlight(0, 'scssFuncUnquote',               { link = "Function" })           -- unquote()
  highlight(0, 'scssFuncQuote',                 { link = "Function" })           -- quote()
  highlight(0, 'scssFuncStrLength',             { link = "Function" })           -- str-length()
  highlight(0, 'scssFuncStrInsert',             { link = "Function" })           -- str-insert()
  highlight(0, 'scssFuncStrIndex',              { link = "Function" })           -- str-index()
  highlight(0, 'scssFuncStrSlice',              { link = "Function" })           -- str-slice()
  highlight(0, 'scssFuncToUpperCase',           { link = "Function" })           -- to-upper-case()
  highlight(0, 'scssFuncToLowerCase',           { link = "Function" })           -- to-lower-case()

  -- Number Functions
  highlight(0, 'scssFuncPercentage',            { link = "Function" })           -- percentage()
  highlight(0, 'scssFuncRound',                 { link = "Function" })           -- round()
  highlight(0, 'scssFuncCeil',                  { link = "Function" })           -- ceil()
  highlight(0, 'scssFuncFloor',                 { link = "Function" })           -- floor()
  highlight(0, 'scssFuncAbs',                   { link = "Function" })           -- abs()
  highlight(0, 'scssFuncMin',                   { link = "Function" })           -- min()
  highlight(0, 'scssFuncMax',                   { link = "Function" })           -- max()
  highlight(0, 'scssFuncRandom',                { link = "Function" })           -- random()

  -- List Functions
  highlight(0, 'scssFuncLength',                { link = "Function" })           -- length()
  highlight(0, 'scssFuncNth',                   { link = "Function" })           -- nth()
  highlight(0, 'scssFuncSetNth',                { link = "Function" })           -- set-nth()
  highlight(0, 'scssFuncJoin',                  { link = "Function" })           -- join()
  highlight(0, 'scssFuncAppend',                { link = "Function" })           -- append()
  highlight(0, 'scssFuncZip',                   { link = "Function" })           -- zip()
  highlight(0, 'scssFuncIndex',                 { link = "Function" })           -- index()
  highlight(0, 'scssFuncListSeparator',         { link = "Function" })           -- list-separator()
  highlight(0, 'scssFuncIsBracketed',           { link = "Function" })           -- is-bracketed()

  -- Map Functions
  highlight(0, 'scssFuncMapGet',                { link = "Function" })           -- map-get()
  highlight(0, 'scssFuncMapMerge',              { link = "Function" })           -- map-merge()
  highlight(0, 'scssFuncMapRemove',             { link = "Function" })           -- map-remove()
  highlight(0, 'scssFuncMapKeys',               { link = "Function" })           -- map-keys()
  highlight(0, 'scssFuncMapValues',             { link = "Function" })           -- map-values()
  highlight(0, 'scssFuncMapHasKey',             { link = "Function" })           -- map-has-key()

  -- Selector Functions
  highlight(0, 'scssFuncSelectorNest',          { link = "Function" })           -- selector-nest()
  highlight(0, 'scssFuncSelectorAppend',        { link = "Function" })           -- selector-append()
  highlight(0, 'scssFuncSelectorExtend',        { link = "Function" })           -- selector-extend()
  highlight(0, 'scssFuncSelectorReplace',       { link = "Function" })           -- selector-replace()
  highlight(0, 'scssFuncSelectorUnify',         { link = "Function" })           -- selector-unify()
  highlight(0, 'scssFuncIsSuperselector',       { link = "Function" })           -- is-superselector()
  highlight(0, 'scssFuncSimpleSelectors',       { link = "Function" })           -- simple-selectors()
  highlight(0, 'scssFuncSelectorParse',         { link = "Function" })           -- selector-parse()

  -- Introspection Functions
  highlight(0, 'scssFuncFeatureExists',         { link = "Function" })           -- feature-exists()
  highlight(0, 'scssFuncVariableExists',        { link = "Function" })           -- variable-exists()
  highlight(0, 'scssFuncGlobalVariableExists',  { link = "Function" })           -- global-variable-exists()
  highlight(0, 'scssFuncFunctionExists',        { link = "Function" })           -- function-exists()
  highlight(0, 'scssFuncMixinExists',           { link = "Function" })           -- mixin-exists()
  highlight(0, 'scssFuncContentExists',         { link = "Function" })           -- content-exists()
  highlight(0, 'scssFuncInspect',               { link = "Function" })           -- inspect()
  highlight(0, 'scssFuncType',                  { link = "Type" })           -- type-of()
  highlight(0, 'scssFuncUnit',                  { link = "Function" })           -- unit()
  highlight(0, 'scssFuncUnitless',              { link = "Function" })           -- unitless()
  highlight(0, 'scssFuncComparable',            { link = "Function" })           -- comparable()
  highlight(0, 'scssFuncCall',                  { link = "Function" })           -- call()
  highlight(0, 'scssFuncGetFunction',           { link = "Function" })           -- get-function()

  -- Meta Functions (Dart Sass)
  highlight(0, 'scssFuncMetaLoad',              { link = "Function" })           -- meta.load-css()
  highlight(0, 'scssFuncMetaModuleVariables',   { link = "Function" })           -- meta.module-variables()
  highlight(0, 'scssFuncMetaModuleFunctions',   { link = "Function" })           -- meta.module-functions()

  -- Math Functions (Dart Sass)
  highlight(0, 'scssFuncMathDiv',               { link = "Function" })           -- math.div()
  highlight(0, 'scssFuncMathClamp',             { link = "Function" })           -- math.clamp()
  highlight(0, 'scssFuncMathSin',               { link = "Function" })           -- math.sin()
  highlight(0, 'scssFuncMathCos',               { link = "Function" })           -- math.cos()
  highlight(0, 'scssFuncMathTan',               { link = "Function" })           -- math.tan()
  highlight(0, 'scssFuncMathAsin',              { link = "Function" })           -- math.asin()
  highlight(0, 'scssFuncMathAcos',              { link = "Function" })           -- math.acos()
  highlight(0, 'scssFuncMathAtan',              { link = "Function" })           -- math.atan()
  highlight(0, 'scssFuncMathAtan2',             { link = "Function" })           -- math.atan2()
  highlight(0, 'scssFuncMathSqrt',              { link = "Function" })           -- math.sqrt()
  highlight(0, 'scssFuncMathLog',               { link = "Function" })           -- math.log()
  highlight(0, 'scssFuncMathPow',               { link = "Function" })           -- math.pow()
  highlight(0, 'scssFuncMathHypot',             { link = "Function" })           -- math.hypot()

  ---------------------------------------------------------------
  -- LSP Semantic Tokens
  ---------------------------------------------------------------

  highlight(0, '@lsp.type.class.scss',                  { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.property.scss',               { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.function.scss',               { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.variable.scss',               { link = "Variable" })
  highlight(0, '@lsp.type.parameter.scss',              { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.scss',                { link = "Keyword" })
  highlight(0, '@lsp.type.string.scss',                 { link = "String" })
  highlight(0, '@lsp.type.number.scss',                 { link = "Number" })
  highlight(0, '@lsp.type.operator.scss',               { link = "Operator" })
  highlight(0, '@lsp.type.comment.scss',                { link = "Comment" })


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
