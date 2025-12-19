--------------------------------------------------------------------------------------------------------------
-- CSS
--------------------------------------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local css       = {}


-------------------------------------------------------------------------------
-- Settings

css.setupHighlighting = function()


  -------------------------------------------------------------------------
  -- Vim Legacy CSS Syntax Groups
  -------------------------------------------------------------------------

  -- At-Rules
  highlight(0, 'cssAtKeyword',              { link = "Keyword"            })  -- @keyword
  highlight(0, 'cssAtRule',                 { link = "Keyword"            })  -- @media, @keyframes, etc.
  highlight(0, 'cssAtRuleLogical',          { link = "Keyword"            })  -- and, or, not in @media
  highlight(0, 'cssCharset',                { link = "Keyword"            })  -- @charset
  highlight(0, 'cssImportant',              { fg = colors.pink,       bg = 'NONE'            })  -- !important
  highlight(0, 'cssInclude',                { fg = colors.blue,       bg = 'NONE'            })  -- @import
  highlight(0, 'cssIncludeKeyword',         { fg = colors.blue,       bg = 'NONE'            })  -- @import keyword
  highlight(0, 'cssKeyFrame',               { fg = colors.blue,       bg = 'NONE'            })  -- @keyframes
  highlight(0, 'cssKeyFrameSelector',       { fg = colors.turquoise,  bg = 'NONE'            })  -- from, to, percentage
  highlight(0, 'cssKeyFrameProp',           { fg = colors.greenLight, bg = 'NONE'            })  -- Keyframe percentage values
  highlight(0, 'cssMedia',                  { fg = colors.blue,       bg = 'NONE'            })  -- @media
  highlight(0, 'cssMediaType',              { fg = colors.turquoise,  bg = 'NONE'            })  -- screen, print, all
  highlight(0, 'cssMediaComma',             { fg = colors.white,      bg = 'NONE'            })  -- Comma in media queries
  highlight(0, 'cssMediaKeyword',           { fg = colors.blue,       bg = 'NONE'            })  -- and, not, only
  highlight(0, 'cssPage',                   { fg = colors.blue,       bg = 'NONE'            })  -- @page
  highlight(0, 'cssPagePseudo',             { fg = colors.pink,       bg = 'NONE'            })  -- :first, :left, :right in @page
  highlight(0, 'cssPageMarginProp',         { fg = colors.blue,       bg = 'NONE'            })  -- @page margin properties
  highlight(0, 'cssSupports',               { fg = colors.blue,       bg = 'NONE'            })  -- @supports
  highlight(0, 'cssLayer',                  { fg = colors.blue,       bg = 'NONE'            })  -- @layer
  highlight(0, 'cssContainer',              { fg = colors.blue,       bg = 'NONE'            })  -- @container
  highlight(0, 'cssScope',                  { fg = colors.blue,       bg = 'NONE'            })  -- @scope
  highlight(0, 'cssStartingStyle',          { fg = colors.blue,       bg = 'NONE'            })  -- @starting-style
  highlight(0, 'cssFontFace',               { fg = colors.blue,       bg = 'NONE'            })  -- @font-face
  highlight(0, 'cssCounterStyle',           { fg = colors.blue,       bg = 'NONE'            })  -- @counter-style
  highlight(0, 'cssProperty',               { fg = colors.blue,       bg = 'NONE'            })  -- @property

  -- Selectors
  highlight(0, 'cssTagName',                { fg = colors.blue,       bg = 'NONE'            })  -- HTML element names
  highlight(0, 'cssClassName',              { fg = colors.turquoise,  bg = 'NONE'            })  -- .class-name
  highlight(0, 'cssClassNameDot',           { fg = colors.white,      bg = 'NONE'            })  -- The dot before class name
  highlight(0, 'cssIdentifier',             { fg = colors.turquoise,  bg = 'NONE'            })  -- #id-name
  highlight(0, 'cssIdName',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- ID name
  highlight(0, 'cssIdHash',                 { fg = colors.white,      bg = 'NONE'            })  -- The hash before ID
  highlight(0, 'cssSelectorOp',             { fg = colors.white,      bg = 'NONE'            })  -- Selector operators (>, +, ~)
  highlight(0, 'cssSelectorOp2',            { fg = colors.white,      bg = 'NONE'            })  -- Additional selector operators
  highlight(0, 'cssAttributeSelector',      { fg = colors.turquoise,  bg = 'NONE'            })  -- [attribute] selectors
  highlight(0, 'cssUniversalSelector',      { fg = colors.white,      bg = 'NONE'            })  -- * selector
  highlight(0, 'cssCombinator',             { fg = colors.white,      bg = 'NONE'            })  -- Combinators (>, +, ~, space)

  -- Pseudo-classes and Pseudo-elements
  highlight(0, 'cssPseudoClass',            { link = "Keyword"            })  -- :hover, :active, :focus, etc.
  highlight(0, 'cssPseudoClassId',          { link = "Keyword"            })  -- Pseudo-class names
  highlight(0, 'cssPseudoClassFn',          { link = "Keyword"            })  -- :nth-child(), :not(), etc.
  highlight(0, 'cssPseudoClassLang',        { link = "Keyword"            })  -- :lang()
  highlight(0, 'cssPseudoElement',          { link = "Keyword"            })  -- ::before, ::after, etc.
  highlight(0, 'cssNot',                    { fg = colors.pink,       bg = 'NONE'            })  -- :not()
  highlight(0, 'cssIs',                     { fg = colors.pink,       bg = 'NONE'            })  -- :is()
  highlight(0, 'cssWhere',                  { fg = colors.pink,       bg = 'NONE'            })  -- :where()
  highlight(0, 'cssHas',                    { fg = colors.pink,       bg = 'NONE'            })  -- :has()
  highlight(0, 'cssDir',                    { fg = colors.pink,       bg = 'NONE'            })  -- :dir()
  highlight(0, 'cssState',                  { fg = colors.pink,       bg = 'NONE'            })  -- :checked, :disabled, :valid, etc.

  -- Properties (by category)
  highlight(0, 'cssProp',                   { fg = colors.blue,       bg = 'NONE'            })  -- Generic property
  highlight(0, 'cssAnimationProp',          { fg = colors.blue,       bg = 'NONE'            })  -- animation-*
  highlight(0, 'cssBackgroundProp',         { fg = colors.blue,       bg = 'NONE'            })  -- background-*
  highlight(0, 'cssBorderProp',             { fg = colors.blue,       bg = 'NONE'            })  -- border-*
  highlight(0, 'cssBoxProp',                { fg = colors.blue,       bg = 'NONE'            })  -- box-*, margin, padding
  highlight(0, 'cssCascadeProp',            { fg = colors.blue,       bg = 'NONE'            })  -- all, initial, inherit, unset, revert
  highlight(0, 'cssColorProp',              { fg = colors.blue,       bg = 'NONE'            })  -- color, opacity
  highlight(0, 'cssDimensionProp',          { fg = colors.blue,       bg = 'NONE'            })  -- width, height, min-*, max-*
  highlight(0, 'cssFlexibleBoxProp',        { fg = colors.blue,       bg = 'NONE'            })  -- flex-*, justify-*, align-*
  highlight(0, 'cssFontProp',               { fg = colors.blue,       bg = 'NONE'            })  -- font-*
  highlight(0, 'cssFontDescriptorProp',     { fg = colors.blue,       bg = 'NONE'            })  -- @font-face descriptors
  highlight(0, 'cssGeneratedContentProp',   { fg = colors.blue,       bg = 'NONE'            })  -- content, counter-*, quotes
  highlight(0, 'cssGridProp',               { fg = colors.blue,       bg = 'NONE'            })  -- grid-*, gap
  highlight(0, 'cssHyerlinkProp',           { fg = colors.blue,       bg = 'NONE'            })  -- Hyperlink properties (vim typo)
  highlight(0, 'cssHyperlinkProp',          { fg = colors.blue,       bg = 'NONE'            })  -- Hyperlink properties
  highlight(0, 'cssInteractProp',           { fg = colors.blue,       bg = 'NONE'            })  -- cursor, user-select, pointer-events
  highlight(0, 'cssListProp',               { fg = colors.blue,       bg = 'NONE'            })  -- list-style-*
  highlight(0, 'cssMediaProp',              { fg = colors.blue,       bg = 'NONE'            })  -- Media query properties
  highlight(0, 'cssMultiColumnProp',        { fg = colors.blue,       bg = 'NONE'            })  -- column-*, columns
  highlight(0, 'cssObjectProp',             { fg = colors.blue,       bg = 'NONE'            })  -- object-fit, object-position
  highlight(0, 'cssOutlineProp',            { fg = colors.blue,       bg = 'NONE'            })  -- outline-*
  highlight(0, 'cssOverflowProp',           { fg = colors.blue,       bg = 'NONE'            })  -- overflow-*
  highlight(0, 'cssPageProp',               { fg = colors.blue,       bg = 'NONE'            })  -- page-*, orphans, widows
  highlight(0, 'cssPositioningProp',        { fg = colors.blue,       bg = 'NONE'            })  -- position, top, right, bottom, left, z-index
  highlight(0, 'cssPrintProp',              { fg = colors.blue,       bg = 'NONE'            })  -- Print-related properties
  highlight(0, 'cssScrollProp',             { fg = colors.blue,       bg = 'NONE'            })  -- scroll-*, scroll-snap-*
  highlight(0, 'cssTableProp',              { fg = colors.blue,       bg = 'NONE'            })  -- table-*, border-collapse, etc.
  highlight(0, 'cssTextProp',               { fg = colors.blue,       bg = 'NONE'            })  -- text-*, line-height, letter-spacing
  highlight(0, 'cssTransformProp',          { fg = colors.blue,       bg = 'NONE'            })  -- transform-*
  highlight(0, 'cssTransitionProp',         { fg = colors.blue,       bg = 'NONE'            })  -- transition-*
  highlight(0, 'cssUIProp',                 { fg = colors.blue,       bg = 'NONE'            })  -- appearance, resize, etc.
  highlight(0, 'cssIEUIProp',               { fg = colors.blue,       bg = 'NONE'            })  -- IE-specific UI properties
  highlight(0, 'cssAuralProp',              { fg = colors.blue,       bg = 'NONE'            })  -- Aural/speech properties
  highlight(0, 'cssMobileTextProp',         { fg = colors.blue,       bg = 'NONE'            })  -- Mobile text properties
  highlight(0, 'cssAspectRatioProp',        { fg = colors.blue,       bg = 'NONE'            })  -- aspect-ratio
  highlight(0, 'cssContainProp',            { fg = colors.blue,       bg = 'NONE'            })  -- contain, container-*
  highlight(0, 'cssContainerProp',          { fg = colors.blue,       bg = 'NONE'            })  -- container-name, container-type
  highlight(0, 'cssFilterProp',             { fg = colors.blue,       bg = 'NONE'            })  -- filter, backdrop-filter
  highlight(0, 'cssMaskProp',               { fg = colors.blue,       bg = 'NONE'            })  -- mask-*
  highlight(0, 'cssClipProp',               { fg = colors.blue,       bg = 'NONE'            })  -- clip, clip-path
  highlight(0, 'cssShapeProp',              { fg = colors.blue,       bg = 'NONE'            })  -- shape-outside, shape-margin
  highlight(0, 'cssWillChangeProp',         { fg = colors.blue,       bg = 'NONE'            })  -- will-change
  highlight(0, 'cssWritingModeProp',        { fg = colors.blue,       bg = 'NONE'            })  -- writing-mode, direction, unicode-bidi
  highlight(0, 'cssLogicalProp',            { fg = colors.blue,       bg = 'NONE'            })  -- Logical properties (inline-*, block-*)
  highlight(0, 'cssAccentColorProp',        { fg = colors.blue,       bg = 'NONE'            })  -- accent-color
  highlight(0, 'cssColorSchemeProp',        { fg = colors.blue,       bg = 'NONE'            })  -- color-scheme
  highlight(0, 'cssForcedColorsProp',       { fg = colors.blue,       bg = 'NONE'            })  -- forced-color-adjust
  highlight(0, 'cssPrintColorProp',         { fg = colors.blue,       bg = 'NONE'            })  -- print-color-adjust
  highlight(0, 'cssViewTransitionProp',     { fg = colors.blue,       bg = 'NONE'            })  -- view-transition-name
  highlight(0, 'cssAnchorProp',             { fg = colors.blue,       bg = 'NONE'            })  -- anchor-name, position-anchor

  -- Attributes/Values (by category)
  highlight(0, 'cssAttr',                   { fg = colors.white,      bg = 'NONE'            })  -- Generic attribute value
  highlight(0, 'cssAnimationAttr',          { fg = colors.white,      bg = 'NONE'            })  -- animation values
  highlight(0, 'cssBackgroundAttr',         { fg = colors.white,      bg = 'NONE'            })  -- background values
  highlight(0, 'cssBorderAttr',             { fg = colors.white,      bg = 'NONE'            })  -- border values (solid, dashed, etc.)
  highlight(0, 'cssBoxAttr',                { fg = colors.white,      bg = 'NONE'            })  -- box values
  highlight(0, 'cssCommonAttr',             { fg = colors.white,      bg = 'NONE'            })  -- Common values (auto, inherit, none)
  highlight(0, 'cssFlexibleBoxAttr',        { fg = colors.white,      bg = 'NONE'            })  -- flex values (row, column, wrap)
  highlight(0, 'cssFontAttr',               { fg = colors.white,      bg = 'NONE'            })  -- font values (bold, italic, etc.)
  highlight(0, 'cssFontDescriptorAttr',     { fg = colors.white,      bg = 'NONE'            })  -- @font-face descriptor values
  highlight(0, 'cssGeneratedContentAttr',   { fg = colors.white,      bg = 'NONE'            })  -- content values
  highlight(0, 'cssGradientAttr',           { fg = colors.white,      bg = 'NONE'            })  -- Gradient values
  highlight(0, 'cssGridAttr',               { fg = colors.white,      bg = 'NONE'            })  -- grid values
  highlight(0, 'cssListAttr',               { fg = colors.white,      bg = 'NONE'            })  -- list-style values
  highlight(0, 'cssMediaAttr',              { fg = colors.white,      bg = 'NONE'            })  -- Media query values
  highlight(0, 'cssMultiColumnAttr',        { fg = colors.white,      bg = 'NONE'            })  -- column values
  highlight(0, 'cssObjectAttr',             { fg = colors.white,      bg = 'NONE'            })  -- object-fit values (cover, contain)
  highlight(0, 'cssPositioningAttr',        { fg = colors.white,      bg = 'NONE'            })  -- position values (static, relative, etc.)
  highlight(0, 'cssPrintAttr',              { fg = colors.white,      bg = 'NONE'            })  -- Print values
  highlight(0, 'cssTableAttr',              { fg = colors.white,      bg = 'NONE'            })  -- table values
  highlight(0, 'cssTextAttr',               { fg = colors.white,      bg = 'NONE'            })  -- text values (center, justify, etc.)
  highlight(0, 'cssTransformAttr',          { fg = colors.white,      bg = 'NONE'            })  -- transform function values
  highlight(0, 'cssTransitionAttr',         { fg = colors.white,      bg = 'NONE'            })  -- transition values
  highlight(0, 'cssUIAttr',                 { fg = colors.white,      bg = 'NONE'            })  -- UI values
  highlight(0, 'cssIEUIAttr',               { fg = colors.white,      bg = 'NONE'            })  -- IE-specific UI values
  highlight(0, 'cssAuralAttr',              { fg = colors.white,      bg = 'NONE'            })  -- Aural values
  highlight(0, 'cssDisplayAttr',            { fg = colors.white,      bg = 'NONE'            })  -- display values (block, flex, grid, etc.)
  highlight(0, 'cssVisibilityAttr',         { fg = colors.white,      bg = 'NONE'            })  -- visibility values
  highlight(0, 'cssOverflowAttr',           { fg = colors.white,      bg = 'NONE'            })  -- overflow values (hidden, scroll, auto)
  highlight(0, 'cssAttrRegion',             { fg = colors.white,      bg = 'NONE'            })  -- Attribute region
  highlight(0, 'cssAttrComma',              { fg = colors.white,      bg = 'NONE'            })  -- Comma in attribute lists

  -- Values
  highlight(0, 'cssValue',                  { fg = colors.white,      bg = 'NONE'            })  -- Generic value
  highlight(0, 'cssValueInteger',           { fg = colors.greenLight, bg = 'NONE'            })  -- Integer values
  highlight(0, 'cssValueNumber',            { fg = colors.greenLight, bg = 'NONE'            })  -- Numeric values
  highlight(0, 'cssValueLength',            { fg = colors.greenLight, bg = 'NONE'            })  -- Length values (10px, 2em, etc.)
  highlight(0, 'cssValueAngle',             { fg = colors.greenLight, bg = 'NONE'            })  -- Angle values (45deg, 1turn)
  highlight(0, 'cssValueTime',              { fg = colors.greenLight, bg = 'NONE'            })  -- Time values (1s, 500ms)
  highlight(0, 'cssValueFrequency',         { fg = colors.greenLight, bg = 'NONE'            })  -- Frequency values (Hz, kHz)
  highlight(0, 'cssValuePercentage',        { fg = colors.greenLight, bg = 'NONE'            })  -- Percentage values
  highlight(0, 'cssValueResolution',        { fg = colors.greenLight, bg = 'NONE'            })  -- Resolution values (dpi, dpcm, dppx)
  highlight(0, 'cssUnitDecorators',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Units (px, em, rem, %, etc.)
  highlight(0, 'cssUnit',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- Units
  highlight(0, 'cssNumber',                 { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers

  -- Colors
  highlight(0, 'cssColor',                  { fg = colors.greenLight, bg = 'NONE'            })  -- Color values
  highlight(0, 'cssColorHex',               { fg = colors.greenLight, bg = 'NONE'            })  -- Hex colors (#fff, #ffffff)
  highlight(0, 'cssColorKeyword',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Named colors (red, blue, etc.)
  highlight(0, 'cssNamedColor',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Named colors
  highlight(0, 'cssSystemColor',            { fg = colors.turquoise,  bg = 'NONE'            })  -- System colors (Canvas, CanvasText)

  -- Functions
  highlight(0, 'cssFunction',               { fg = colors.orange,     bg = 'NONE'            })  -- Generic function
  highlight(0, 'cssFunctionName',           { fg = colors.orange,     bg = 'NONE'            })  -- Function name
  highlight(0, 'cssFunctionComma',          { fg = colors.white,      bg = 'NONE'            })  -- Commas in functions
  highlight(0, 'cssURL',                    { fg = colors.orange,     bg = 'NONE'            })  -- url()
  highlight(0, 'cssURLContainer',           { fg = colors.white,      bg = 'NONE'            })  -- url() container
  highlight(0, 'cssMathGroup',              { fg = colors.orange,     bg = 'NONE'            })  -- calc(), min(), max(), clamp()
  highlight(0, 'cssMathParens',             { fg = colors.white,      bg = 'NONE'            })  -- Parentheses in math functions
  highlight(0, 'cssCalc',                   { fg = colors.orange,     bg = 'NONE'            })  -- calc()
  highlight(0, 'cssMin',                    { fg = colors.orange,     bg = 'NONE'            })  -- min()
  highlight(0, 'cssMax',                    { fg = colors.orange,     bg = 'NONE'            })  -- max()
  highlight(0, 'cssClamp',                  { fg = colors.orange,     bg = 'NONE'            })  -- clamp()
  highlight(0, 'cssVar',                    { fg = colors.orange,     bg = 'NONE'            })  -- var()
  highlight(0, 'cssEnv',                    { fg = colors.orange,     bg = 'NONE'            })  -- env()
  highlight(0, 'cssAttr',                   { fg = colors.orange,     bg = 'NONE'            })  -- attr()
  highlight(0, 'cssImage',                  { fg = colors.orange,     bg = 'NONE'            })  -- image()
  highlight(0, 'cssImageSet',               { fg = colors.orange,     bg = 'NONE'            })  -- image-set()
  highlight(0, 'cssGradientFunc',           { fg = colors.orange,     bg = 'NONE'            })  -- linear-gradient(), radial-gradient()
  highlight(0, 'cssLinearGradient',         { fg = colors.orange,     bg = 'NONE'            })  -- linear-gradient()
  highlight(0, 'cssRadialGradient',         { fg = colors.orange,     bg = 'NONE'            })  -- radial-gradient()
  highlight(0, 'cssConicGradient',          { fg = colors.orange,     bg = 'NONE'            })  -- conic-gradient()
  highlight(0, 'cssRepeatingGradient',      { fg = colors.orange,     bg = 'NONE'            })  -- repeating-*-gradient()
  highlight(0, 'cssFilterFunc',             { fg = colors.orange,     bg = 'NONE'            })  -- blur(), brightness(), etc.
  highlight(0, 'cssTransformFunc',          { fg = colors.orange,     bg = 'NONE'            })  -- rotate(), scale(), translate(), etc.
  highlight(0, 'cssRotate',                 { fg = colors.orange,     bg = 'NONE'            })  -- rotate()
  highlight(0, 'cssScale',                  { fg = colors.orange,     bg = 'NONE'            })  -- scale()
  highlight(0, 'cssTranslate',              { fg = colors.orange,     bg = 'NONE'            })  -- translate()
  highlight(0, 'cssSkew',                   { fg = colors.orange,     bg = 'NONE'            })  -- skew()
  highlight(0, 'cssMatrix',                 { fg = colors.orange,     bg = 'NONE'            })  -- matrix()
  highlight(0, 'cssPerspective',            { fg = colors.orange,     bg = 'NONE'            })  -- perspective()
  highlight(0, 'cssColorFunc',              { fg = colors.orange,     bg = 'NONE'            })  -- rgb(), hsl(), etc.
  highlight(0, 'cssRgb',                    { fg = colors.orange,     bg = 'NONE'            })  -- rgb(), rgba()
  highlight(0, 'cssHsl',                    { fg = colors.orange,     bg = 'NONE'            })  -- hsl(), hsla()
  highlight(0, 'cssHwb',                    { fg = colors.orange,     bg = 'NONE'            })  -- hwb()
  highlight(0, 'cssLab',                    { fg = colors.orange,     bg = 'NONE'            })  -- lab()
  highlight(0, 'cssLch',                    { fg = colors.orange,     bg = 'NONE'            })  -- lch()
  highlight(0, 'cssOklab',                  { fg = colors.orange,     bg = 'NONE'            })  -- oklab()
  highlight(0, 'cssOklch',                  { fg = colors.orange,     bg = 'NONE'            })  -- oklch()
  highlight(0, 'cssColorMix',               { fg = colors.orange,     bg = 'NONE'            })  -- color-mix()
  highlight(0, 'cssColor',                  { fg = colors.orange,     bg = 'NONE'            })  -- color()
  highlight(0, 'cssLightDark',              { fg = colors.orange,     bg = 'NONE'            })  -- light-dark()
  highlight(0, 'cssCounterFunc',            { fg = colors.orange,     bg = 'NONE'            })  -- counter(), counters()
  highlight(0, 'cssShapeFunc',              { fg = colors.orange,     bg = 'NONE'            })  -- circle(), ellipse(), polygon(), inset()
  highlight(0, 'cssPath',                   { fg = colors.orange,     bg = 'NONE'            })  -- path()
  highlight(0, 'cssFit',                    { fg = colors.orange,     bg = 'NONE'            })  -- fit-content()
  highlight(0, 'cssMinmax',                 { fg = colors.orange,     bg = 'NONE'            })  -- minmax()
  highlight(0, 'cssRepeat',                 { fg = colors.orange,     bg = 'NONE'            })  -- repeat()

  -- Custom Properties (CSS Variables)
  highlight(0, 'cssCustomProp',             { link = "Constant"            })  -- --custom-property
  highlight(0, 'cssCustomProperty',         { link = "Keyword"            })  -- --custom-property
  highlight(0, 'cssVariable',               { link = "Constant"            })  -- CSS variable
  highlight(0, 'cssVariableValue',          { fg = colors.white,      bg = 'NONE'            })  -- Variable value

  -- Strings
  highlight(0, 'cssStringQ',                { fg = colors.redLight,   bg = 'NONE'            })  -- Single-quoted string
  highlight(0, 'cssStringQQ',               { fg = colors.redLight,   bg = 'NONE'            })  -- Double-quoted string
  highlight(0, 'cssString',                 { fg = colors.redLight,   bg = 'NONE'            })  -- Generic string
  highlight(0, 'cssQuote',                  { fg = colors.white,      bg = 'NONE'            })  -- Quote characters
  highlight(0, 'cssSpecialCharQ',           { fg = colors.pink,       bg = 'NONE'            })  -- Special char in single-quoted
  highlight(0, 'cssSpecialCharQQ',          { fg = colors.pink,       bg = 'NONE'            })  -- Special char in double-quoted

  -- Comments
  highlight(0, 'cssComment',                { link = "Comment" })  -- /* comment */

  -- Operators and Punctuation
  highlight(0, 'cssNoise',                  { fg = colors.white,      bg = 'NONE'            })  -- Noise characters
  highlight(0, 'cssBraces',                 { fg = colors.white,      bg = 'NONE'            })  -- { }
  highlight(0, 'cssParens',                 { fg = colors.white,      bg = 'NONE'            })  -- ( )
  highlight(0, 'cssBrackets',               { fg = colors.white,      bg = 'NONE'            })  -- [ ]
  highlight(0, 'cssSemicolon',              { fg = colors.white,      bg = 'NONE'            })  -- ;
  highlight(0, 'cssColon',                  { fg = colors.white,      bg = 'NONE'            })  -- :
  highlight(0, 'cssComma',                  { fg = colors.white,      bg = 'NONE'            })  -- ,
  highlight(0, 'cssOperator',               { link = "Constant"})  -- Operators
  highlight(0, 'cssDefinition',             { fg = colors.white,      bg = 'NONE'            })  -- Definition context

  -- Vendor Prefixes
  highlight(0, 'cssVendor',                 { fg = colors.orange,     bg = 'NONE'            })  -- -webkit-, -moz-, -ms-, -o-
  highlight(0, 'cssWebkit',                 { fg = colors.orange,     bg = 'NONE'            })  -- -webkit-
  highlight(0, 'cssMoz',                    { fg = colors.orange,     bg = 'NONE'            })  -- -moz-
  highlight(0, 'cssMs',                     { fg = colors.orange,     bg = 'NONE'            })  -- -ms-
  highlight(0, 'cssO',                      { fg = colors.orange,     bg = 'NONE'            })  -- -o-

  -- Escapes and Special Characters
  highlight(0, 'cssUnicodeEscape',          { fg = colors.pink,       bg = 'NONE'            })  -- Unicode escapes
  highlight(0, 'cssEscape',                 { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences
  highlight(0, 'cssHacks',                  { fg = colors.orange,     bg = 'NONE'            })  -- CSS hacks

  -- Errors
  highlight(0, 'cssError',                  { fg = colors.white,      bg = colors.red        })  -- Syntax errors
  highlight(0, 'cssBraceError',             { fg = colors.white,      bg = colors.red        })  -- Brace mismatch

  -- Style (for embedded CSS)
  highlight(0, 'cssStyle',                  { fg = colors.white,      bg = 'NONE'            })  -- Style context


  -------------------------------------------------------------------------
  -- Treesitter CSS Captures
  -------------------------------------------------------------------------

  -- Keywords
  highlight(0, '@keyword.css',              { link = "Keyword"            })  -- Generic keyword
  highlight(0, '@keyword.directive.css',    { link = "Keyword"            })  -- @-rules
  highlight(0, '@keyword.import.css',       { link = "Keyword"            })  -- @import
  highlight(0, '@keyword.operator.css',     { link = "Keyword"            })  -- and, or, not
  highlight(0, '@keyword.modifier.css',     { fg = colors.pink,       bg = 'NONE'            })  -- !important

  -- Tags and Types
  highlight(0, '@tag.css',                  { link = "Keyword"})  -- HTML element names
  highlight(0, '@type.css',                 { link = "Type"            })  -- Class/ID selectors
  highlight(0, '@constant.css',             { link = "Constant"            })  -- Named constants

  -- Properties
  highlight(0, '@property.css',             { link = "Constant"            })  -- CSS properties like font-size.

  -- Functions
  highlight(0, '@function.css',             { fg = colors.orange,     bg = 'NONE'            })  -- CSS functions

  -- Variables
  highlight(0, '@variable.css',             { fg = colors.purple,     bg = 'NONE'            })  -- CSS custom properties

  -- Strings
  highlight(0, '@string.css',               { fg = colors.redLight,   bg = 'NONE'            })  -- Strings

  -- Numbers
  highlight(0, '@number.css',               { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, '@number.float.css',         { fg = colors.greenLight, bg = 'NONE'            })  -- Floating point numbers

  -- Attributes
  highlight(0, '@attribute.css',            { link = "Keyword"            })  -- Attribute selectors
  highlight(0, '@tag.attribute.css',        { link = "Keyword"            })  -- Tag attributes

  -- Modules
  highlight(0, '@module.css',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespace/module

  -- Operators
  highlight(0, '@operator.css',             { link = "Keyword"            })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.css',  { fg = colors.white,      bg = 'NONE'            })  -- Brackets, braces, parens
  highlight(0, '@punctuation.delimiter.css', { fg = colors.white,     bg = 'NONE'            })  -- Semicolons, colons, commas

  -- Special Characters
  highlight(0, '@character.special.css',    { fg = colors.pink,       bg = 'NONE'            })  -- Special characters

  -- Comments
  highlight(0, '@comment.css',              { link = "Pink" })  -- ??? Comments
  highlight(0, '@lsp.type.comment.css',     { link = "Pink" })  -- ??? Comments
  highlight(0, '@spell.css',                { link = "Comment"                          })  -- Comments.


  -------------------------------------------------------------------------
  -- LSP Semantic Tokens (limited support in CSS)
  -------------------------------------------------------------------------

  highlight(0, '@lsp.type.class.css',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Class selectors
  highlight(0, '@lsp.type.property.css',    { fg = colors.blue,       bg = 'NONE'            })  -- Properties
  highlight(0, '@lsp.type.variable.css',    { link = "Constant"            })  -- Variables
  highlight(0, '@lsp.type.function.css',    { fg = colors.orange,     bg = 'NONE'            })  -- Functions
  highlight(0, '@lsp.type.string.css',      { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
  highlight(0, '@lsp.type.number.css',      { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, '@lsp.type.keyword.css',     { fg = colors.blue,       bg = 'NONE'            })  -- Keywords
  highlight(0, '@lsp.type.operator.css',    { fg = colors.white,      bg = 'NONE'            })  -- Operators

end

return css
