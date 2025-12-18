-------------------------------------------------------------------------------
-- MATLAB Language Files
-- Highlighting for .m files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local matlab    = {}


-------------------------------------------------------------------------------
-- Settings

matlab.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords / Control Flow
  highlight(0, 'matlabStatement',       { fg = colors.blue,       bg = 'NONE' })  -- return
  highlight(0, 'matlabConditional',     { fg = colors.blue,       bg = 'NONE' })  -- if, else, elseif, end
  highlight(0, 'matlabRepeat',          { fg = colors.blue,       bg = 'NONE' })  -- for, while, parfor
  highlight(0, 'matlabLabel',           { fg = colors.blue,       bg = 'NONE' })  -- case, switch, otherwise
  highlight(0, 'matlabExceptions',      { fg = colors.blue,       bg = 'NONE' })  -- try, catch
  highlight(0, 'matlabKeyword',         { fg = colors.blue,       bg = 'NONE' })  -- General keywords

  -- Function Definition
  highlight(0, 'matlabFunction',        { fg = colors.blue,       bg = 'NONE' })  -- function keyword
  highlight(0, 'matlabFunctionName',    { fg = colors.orange,     bg = 'NONE' })  -- Function name
  highlight(0, 'matlabArguments',       { fg = colors.blue,       bg = 'NONE' })  -- arguments block

  -- Object-Oriented Keywords
  highlight(0, 'matlabOO',              { fg = colors.blue,       bg = 'NONE' })  -- classdef, properties, methods, events
  highlight(0, 'matlabClassdef',        { fg = colors.blue,       bg = 'NONE' })  -- classdef
  highlight(0, 'matlabProperties',      { fg = colors.blue,       bg = 'NONE' })  -- properties
  highlight(0, 'matlabMethods',         { fg = colors.blue,       bg = 'NONE' })  -- methods
  highlight(0, 'matlabEvents',          { fg = colors.blue,       bg = 'NONE' })  -- events
  highlight(0, 'matlabEnumeration',     { fg = colors.blue,       bg = 'NONE' })  -- enumeration

  -- OOP Attributes
  highlight(0, 'matlabAttribute',       { fg = colors.pink,       bg = 'NONE' })  -- Access, Abstract, Sealed, etc.
  highlight(0, 'matlabAccess',          { fg = colors.pink,       bg = 'NONE' })  -- public, private, protected
  highlight(0, 'matlabAbstract',        { fg = colors.pink,       bg = 'NONE' })  -- Abstract
  highlight(0, 'matlabSealed',          { fg = colors.pink,       bg = 'NONE' })  -- Sealed
  highlight(0, 'matlabHidden',          { fg = colors.pink,       bg = 'NONE' })  -- Hidden
  highlight(0, 'matlabStatic',          { fg = colors.pink,       bg = 'NONE' })  -- Static
  highlight(0, 'matlabConstant',        { fg = colors.pink,       bg = 'NONE' })  -- Constant attribute

  -- Scope Modifiers
  highlight(0, 'matlabScope',           { fg = colors.blue,       bg = 'NONE' })  -- global, persistent

  -- Special Keywords
  highlight(0, 'matlabImport',          { fg = colors.blue,       bg = 'NONE' })  -- import
  highlight(0, 'matlabSpmd',            { fg = colors.blue,       bg = 'NONE' })  -- spmd (parallel)
  highlight(0, 'matlabBreak',           { fg = colors.blue,       bg = 'NONE' })  -- break, continue
  highlight(0, 'matlabReturn',          { fg = colors.blue,       bg = 'NONE' })  -- return

  -- Types
  highlight(0, 'matlabType',            { fg = colors.turquoise,  bg = 'NONE' })  -- Type names
  highlight(0, 'matlabTypeNumeric',     { fg = colors.turquoise,  bg = 'NONE' })  -- double, single, int*, uint*
  highlight(0, 'matlabTypeLogical',     { fg = colors.turquoise,  bg = 'NONE' })  -- logical
  highlight(0, 'matlabTypeChar',        { fg = colors.turquoise,  bg = 'NONE' })  -- char, string
  highlight(0, 'matlabTypeCell',        { fg = colors.turquoise,  bg = 'NONE' })  -- cell
  highlight(0, 'matlabTypeStruct',      { fg = colors.turquoise,  bg = 'NONE' })  -- struct
  highlight(0, 'matlabTypeTable',       { fg = colors.turquoise,  bg = 'NONE' })  -- table, timetable
  highlight(0, 'matlabTypeFuncHandle',  { fg = colors.turquoise,  bg = 'NONE' })  -- function_handle

  -- Constants
  highlight(0, 'matlabConstant',        { fg = colors.blue,       bg = 'NONE' })  -- Constants
  highlight(0, 'matlabBoolean',         { fg = colors.blue,       bg = 'NONE' })  -- true, false
  highlight(0, 'matlabPi',              { fg = colors.blue,       bg = 'NONE' })  -- pi
  highlight(0, 'matlabEps',             { fg = colors.blue,       bg = 'NONE' })  -- eps
  highlight(0, 'matlabInf',             { fg = colors.blue,       bg = 'NONE' })  -- inf, Inf
  highlight(0, 'matlabNaN',             { fg = colors.blue,       bg = 'NONE' })  -- nan, NaN
  highlight(0, 'matlabImagUnit',        { fg = colors.blue,       bg = 'NONE' })  -- i, j, 1i, 1j

  -- Built-in Functions
  highlight(0, 'matlabFunc',            { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions
  highlight(0, 'matlabImplicit',        { fg = colors.orange,     bg = 'NONE' })  -- Implicit functions

  -- Operators
  highlight(0, 'matlabOperator',        { fg = colors.white,      bg = 'NONE' })  -- General operators
  highlight(0, 'matlabArithmeticOp',    { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, \, ^
  highlight(0, 'matlabElementWiseOp',   { fg = colors.white,      bg = 'NONE' })  -- .*, ./, .\, .^
  highlight(0, 'matlabRelationalOp',    { fg = colors.white,      bg = 'NONE' })  -- ==, ~=, <, >, <=, >=
  highlight(0, 'matlabLogicalOp',       { fg = colors.white,      bg = 'NONE' })  -- &, |, &&, ||, ~
  highlight(0, 'matlabAssignOp',        { fg = colors.white,      bg = 'NONE' })  -- =
  highlight(0, 'matlabColonOp',         { fg = colors.white,      bg = 'NONE' })  -- : (range/indexing)
  highlight(0, 'matlabTransposeOp',     { fg = colors.white,      bg = 'NONE' })  -- ', .'

  -- Variables / Parameters
  highlight(0, 'matlabVariable',        { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, 'matlabParameter',       { fg = colors.purple,     bg = 'NONE' })  -- Function parameters
  highlight(0, 'matlabField',           { fg = colors.purple,     bg = 'NONE' })  -- Struct fields
  highlight(0, 'matlabProperty',        { fg = colors.purple,     bg = 'NONE' })  -- Object properties

  -- Strings
  highlight(0, 'matlabString',          { fg = colors.redLight,   bg = 'NONE' })  -- 'single' or "double"
  highlight(0, 'matlabCharArray',       { fg = colors.redLight,   bg = 'NONE' })  -- 'char array'
  highlight(0, 'matlabStringLiteral',   { fg = colors.redLight,   bg = 'NONE' })  -- "string literal"
  highlight(0, 'matlabStringEscape',    { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences
  highlight(0, 'matlabFormatSpec',      { fg = colors.pink,       bg = 'NONE' })  -- %d, %f, %s, etc.

  -- Numbers
  highlight(0, 'matlabNumber',          { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, 'matlabFloat',           { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'matlabComplex',         { fg = colors.greenLight, bg = 'NONE' })  -- Complex numbers (3+4i)
  highlight(0, 'matlabSciNotation',     { fg = colors.greenLight, bg = 'NONE' })  -- Scientific notation (1e-5)
  highlight(0, 'matlabHex',             { fg = colors.greenLight, bg = 'NONE' })  -- Hex numbers (0x...)
  highlight(0, 'matlabBinary',          { fg = colors.greenLight, bg = 'NONE' })  -- Binary (0b...)

  -- Comments
  highlight(0, 'matlabComment',         { fg = colors.red,        bg = 'NONE' })  -- % comment
  highlight(0, 'matlabBlockComment',    { fg = colors.red,        bg = 'NONE' })  -- %{ ... %}
  highlight(0, 'matlabCellComment',     { fg = colors.red,        bg = 'NONE', bold = true })  -- %% cell
  highlight(0, 'matlabTodo',            { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME
  highlight(0, 'matlabLineContinue',    { fg = colors.gray,       bg = 'NONE' })  -- ... continuation

  -- Delimiters / Punctuation
  highlight(0, 'matlabDelimiter',       { fg = colors.white,      bg = 'NONE' })  -- General delimiters
  highlight(0, 'matlabParen',           { fg = colors.white,      bg = 'NONE' })  -- ()
  highlight(0, 'matlabBracket',         { fg = colors.white,      bg = 'NONE' })  -- []
  highlight(0, 'matlabBrace',           { fg = colors.white,      bg = 'NONE' })  -- {}
  highlight(0, 'matlabComma',           { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'matlabSemicolon',       { fg = colors.white,      bg = 'NONE' })  -- ;
  highlight(0, 'matlabDot',             { fg = colors.white,      bg = 'NONE' })  -- .

  -- Special
  highlight(0, 'matlabFuncHandle',      { fg = colors.pink,       bg = 'NONE' })  -- @ function handle
  highlight(0, 'matlabCommand',         { fg = colors.orange,     bg = 'NONE' })  -- ! shell command

  -- Errors
  highlight(0, 'matlabError',           { fg = colors.red,        bg = 'NONE' })  -- Syntax errors
  highlight(0, 'matlabTab',             { fg = colors.red,        bg = 'NONE' })  -- Tab characters (often error)


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.matlab)

  -- Variables
  highlight(0, '@variable.matlab',              { fg = colors.white,     bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.matlab',      { fg = colors.blue,      bg = 'NONE' })  -- Built-in variables
  highlight(0, '@variable.member.matlab',       { fg = colors.purple,    bg = 'NONE' })  -- Struct/object members
  highlight(0, '@variable.parameter.matlab',    { fg = colors.purple,    bg = 'NONE' })  -- Parameters

  -- Constants
  highlight(0, '@constant.matlab',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.matlab',      { fg = colors.blue,      bg = 'NONE' })  -- pi, eps, inf, nan, i, j

  -- Functions
  highlight(0, '@function.matlab',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.matlab',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.matlab',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.method.matlab',       { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@function.method.call.matlab',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls

  -- Types
  highlight(0, '@type.matlab',                  { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@type.builtin.matlab',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.matlab',       { fg = colors.turquoise, bg = 'NONE' })  -- Class definitions

  -- Keywords
  highlight(0, '@keyword.matlab',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.matlab',      { fg = colors.blue,      bg = 'NONE' })  -- function
  highlight(0, '@keyword.return.matlab',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.conditional.matlab',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, elseif, switch, case
  highlight(0, '@keyword.repeat.matlab',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, parfor
  highlight(0, '@keyword.exception.matlab',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch
  highlight(0, '@keyword.import.matlab',        { fg = colors.blue,      bg = 'NONE' })  -- import
  highlight(0, '@keyword.type.matlab',          { fg = colors.blue,      bg = 'NONE' })  -- classdef
  highlight(0, '@keyword.directive.matlab',     { fg = colors.pink,      bg = 'NONE' })  -- Attributes

  -- Strings
  highlight(0, '@string.matlab',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.matlab',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.matlab',        { fg = colors.pink,      bg = 'NONE' })  -- Format specifiers

  -- Numbers
  highlight(0, '@number.matlab',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.float.matlab',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.matlab',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.matlab',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.matlab', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Operators
  highlight(0, '@operator.matlab',              { fg = colors.white,     bg = 'NONE' })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.matlab',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.matlab', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.matlab',   { fg = colors.pink,      bg = 'NONE' })  -- @, ...

  -- Spell
  highlight(0, '@spell.matlab',                 { fg = colors.red,       bg = 'NONE' })  -- Spellchecked


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.matlab)

  highlight(0, '@lsp.type.variable.matlab',      { fg = colors.white,     bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.matlab',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.matlab',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.matlab',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.matlab',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.class.matlab',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.type.matlab',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.namespace.matlab',     { fg = colors.turquoise, bg = 'NONE' })  -- Packages
  highlight(0, '@lsp.type.enum.matlab',          { fg = colors.turquoise, bg = 'NONE' })  -- Enumerations
  highlight(0, '@lsp.type.enumMember.matlab',    { fg = colors.purple,    bg = 'NONE' })  -- Enum members
  highlight(0, '@lsp.type.keyword.matlab',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.matlab',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.comment.matlab',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.string.matlab',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.matlab',        { fg = colors.greenLight, bg = 'NONE' })  -- Numbers

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.matlab',  { fg = colors.purple,   bg = 'NONE' })  -- Constants
  highlight(0, '@lsp.typemod.function.builtin.matlab',   { fg = colors.orange,   bg = 'NONE' })  -- Built-ins


  -----------------------------------------------------------------------------
  -- Data Types (Custom highlight groups)

  -- Numeric Types
  highlight(0, 'matlabTypeDouble',      { fg = colors.turquoise,  bg = 'NONE' })  -- double
  highlight(0, 'matlabTypeSingle',      { fg = colors.turquoise,  bg = 'NONE' })  -- single
  highlight(0, 'matlabTypeInt8',        { fg = colors.turquoise,  bg = 'NONE' })  -- int8
  highlight(0, 'matlabTypeInt16',       { fg = colors.turquoise,  bg = 'NONE' })  -- int16
  highlight(0, 'matlabTypeInt32',       { fg = colors.turquoise,  bg = 'NONE' })  -- int32
  highlight(0, 'matlabTypeInt64',       { fg = colors.turquoise,  bg = 'NONE' })  -- int64
  highlight(0, 'matlabTypeUint8',       { fg = colors.turquoise,  bg = 'NONE' })  -- uint8
  highlight(0, 'matlabTypeUint16',      { fg = colors.turquoise,  bg = 'NONE' })  -- uint16
  highlight(0, 'matlabTypeUint32',      { fg = colors.turquoise,  bg = 'NONE' })  -- uint32
  highlight(0, 'matlabTypeUint64',      { fg = colors.turquoise,  bg = 'NONE' })  -- uint64


  -----------------------------------------------------------------------------
  -- Built-in Functions by Category

  -- Math Functions
  highlight(0, 'matlabFuncMath',        { fg = colors.orange,     bg = 'NONE' })  -- abs, sqrt, exp, log, etc.

  -- Trigonometric Functions
  highlight(0, 'matlabFuncTrig',        { fg = colors.orange,     bg = 'NONE' })  -- sin, cos, tan, asin, acos, atan, etc.

  -- Matrix Operations
  highlight(0, 'matlabFuncMatrix',      { fg = colors.orange,     bg = 'NONE' })  -- zeros, ones, eye, rand, randn, etc.

  -- Linear Algebra
  highlight(0, 'matlabFuncLinAlg',      { fg = colors.orange,     bg = 'NONE' })  -- inv, det, eig, svd, rank, norm, etc.

  -- Statistics
  highlight(0, 'matlabFuncStats',       { fg = colors.orange,     bg = 'NONE' })  -- mean, median, std, var, cov, corrcoef, etc.

  -- Array Operations
  highlight(0, 'matlabFuncArray',       { fg = colors.orange,     bg = 'NONE' })  -- size, length, numel, reshape, repmat, etc.

  -- Sorting and Searching
  highlight(0, 'matlabFuncSort',        { fg = colors.orange,     bg = 'NONE' })  -- sort, sortrows, find, max, min, etc.

  -- String Functions
  highlight(0, 'matlabFuncString',      { fg = colors.orange,     bg = 'NONE' })  -- sprintf, fprintf, strcat, strcmp, etc.

  -- File I/O
  highlight(0, 'matlabFuncIO',          { fg = colors.orange,     bg = 'NONE' })  -- fopen, fclose, fread, fwrite, fscanf, etc.

  -- Data Import/Export
  highlight(0, 'matlabFuncData',        { fg = colors.orange,     bg = 'NONE' })  -- load, save, csvread, csvwrite, xlsread, etc.

  -- Graphics
  highlight(0, 'matlabFuncPlot',        { fg = colors.orange,     bg = 'NONE' })  -- plot, figure, subplot, hold, title, xlabel, etc.

  -- 3D Graphics
  highlight(0, 'matlabFuncPlot3D',      { fg = colors.orange,     bg = 'NONE' })  -- plot3, mesh, surf, contour, etc.

  -- Image Processing
  highlight(0, 'matlabFuncImage',       { fg = colors.orange,     bg = 'NONE' })  -- imread, imshow, imwrite, imresize, etc.

  -- Type Conversion
  highlight(0, 'matlabFuncConvert',     { fg = colors.orange,     bg = 'NONE' })  -- double, single, int32, char, string, etc.

  -- Type Checking
  highlight(0, 'matlabFuncIsType',      { fg = colors.orange,     bg = 'NONE' })  -- isnumeric, ischar, islogical, iscell, etc.

  -- Control Flow Functions
  highlight(0, 'matlabFuncControl',     { fg = colors.orange,     bg = 'NONE' })  -- error, warning, assert, return, etc.

  -- Environment
  highlight(0, 'matlabFuncEnv',         { fg = colors.orange,     bg = 'NONE' })  -- clear, clc, who, whos, exist, etc.

  -- Time Functions
  highlight(0, 'matlabFuncTime',        { fg = colors.orange,     bg = 'NONE' })  -- tic, toc, pause, clock, date, now, etc.


  -----------------------------------------------------------------------------
  -- Built-in Constants

  highlight(0, 'matlabConstPi',         { fg = colors.blue,       bg = 'NONE' })  -- pi
  highlight(0, 'matlabConstE',          { fg = colors.blue,       bg = 'NONE' })  -- exp(1)
  highlight(0, 'matlabConstEps',        { fg = colors.blue,       bg = 'NONE' })  -- eps
  highlight(0, 'matlabConstRealmax',    { fg = colors.blue,       bg = 'NONE' })  -- realmax
  highlight(0, 'matlabConstRealmin',    { fg = colors.blue,       bg = 'NONE' })  -- realmin
  highlight(0, 'matlabConstIntmax',     { fg = colors.blue,       bg = 'NONE' })  -- intmax
  highlight(0, 'matlabConstIntmin',     { fg = colors.blue,       bg = 'NONE' })  -- intmin
  highlight(0, 'matlabConstInf',        { fg = colors.blue,       bg = 'NONE' })  -- inf, Inf
  highlight(0, 'matlabConstNaN',        { fg = colors.blue,       bg = 'NONE' })  -- nan, NaN
  highlight(0, 'matlabConstI',          { fg = colors.blue,       bg = 'NONE' })  -- i, j (imaginary unit)
  highlight(0, 'matlabConstTrue',       { fg = colors.blue,       bg = 'NONE' })  -- true
  highlight(0, 'matlabConstFalse',      { fg = colors.blue,       bg = 'NONE' })  -- false
  highlight(0, 'matlabConstAns',        { fg = colors.blue,       bg = 'NONE' })  -- ans


  -----------------------------------------------------------------------------
  -- OOP Class Definition

  -- Class Blocks
  highlight(0, 'matlabClassBlock',      { fg = colors.blue,       bg = 'NONE' })  -- classdef block
  highlight(0, 'matlabPropertiesBlock', { fg = colors.blue,       bg = 'NONE' })  -- properties block
  highlight(0, 'matlabMethodsBlock',    { fg = colors.blue,       bg = 'NONE' })  -- methods block
  highlight(0, 'matlabEventsBlock',     { fg = colors.blue,       bg = 'NONE' })  -- events block
  highlight(0, 'matlabEnumBlock',       { fg = colors.blue,       bg = 'NONE' })  -- enumeration block

  -- Class Attributes
  highlight(0, 'matlabAttrAccess',      { fg = colors.pink,       bg = 'NONE' })  -- Access = public/private/protected
  highlight(0, 'matlabAttrAbstract',    { fg = colors.pink,       bg = 'NONE' })  -- Abstract = true
  highlight(0, 'matlabAttrSealed',      { fg = colors.pink,       bg = 'NONE' })  -- Sealed = true
  highlight(0, 'matlabAttrHidden',      { fg = colors.pink,       bg = 'NONE' })  -- Hidden = true
  highlight(0, 'matlabAttrStatic',      { fg = colors.pink,       bg = 'NONE' })  -- Static = true
  highlight(0, 'matlabAttrConstant',    { fg = colors.pink,       bg = 'NONE' })  -- Constant = true
  highlight(0, 'matlabAttrDependent',   { fg = colors.pink,       bg = 'NONE' })  -- Dependent = true
  highlight(0, 'matlabAttrTransient',   { fg = colors.pink,       bg = 'NONE' })  -- Transient = true
  highlight(0, 'matlabAttrGetAccess',   { fg = colors.pink,       bg = 'NONE' })  -- GetAccess
  highlight(0, 'matlabAttrSetAccess',   { fg = colors.pink,       bg = 'NONE' })  -- SetAccess
  highlight(0, 'matlabAttrGetObserv',   { fg = colors.pink,       bg = 'NONE' })  -- GetObservable
  highlight(0, 'matlabAttrSetObserv',   { fg = colors.pink,       bg = 'NONE' })  -- SetObservable

  -- Inheritance
  highlight(0, 'matlabInheritance',     { fg = colors.white,      bg = 'NONE' })  -- < superclass
  highlight(0, 'matlabSuperclass',      { fg = colors.turquoise,  bg = 'NONE' })  -- Superclass name
  highlight(0, 'matlabHandle',          { fg = colors.turquoise,  bg = 'NONE' })  -- handle class


  -----------------------------------------------------------------------------
  -- Parallel Computing

  highlight(0, 'matlabParfor',          { fg = colors.blue,       bg = 'NONE' })  -- parfor
  highlight(0, 'matlabSpmdBlock',       { fg = colors.blue,       bg = 'NONE' })  -- spmd
  highlight(0, 'matlabParallel',        { fg = colors.orange,     bg = 'NONE' })  -- parpool, parfeval, etc.


  -----------------------------------------------------------------------------
  -- Special Syntax

  -- Cell Arrays
  highlight(0, 'matlabCellArray',       { fg = colors.white,      bg = 'NONE' })  -- {cell, array}

  -- Anonymous Functions
  highlight(0, 'matlabAnonymousFunc',   { fg = colors.pink,       bg = 'NONE' })  -- @(x) x.^2

  -- Indexing
  highlight(0, 'matlabIndexParen',      { fg = colors.white,      bg = 'NONE' })  -- A(1,2)
  highlight(0, 'matlabIndexBrace',      { fg = colors.white,      bg = 'NONE' })  -- C{1,2}
  highlight(0, 'matlabIndexDot',        { fg = colors.white,      bg = 'NONE' })  -- S.field

  -- End Keyword in Indexing
  highlight(0, 'matlabEndIndex',        { fg = colors.blue,       bg = 'NONE' })  -- A(1:end)

  -- Colon Operator
  highlight(0, 'matlabColonRange',      { fg = colors.white,      bg = 'NONE' })  -- 1:10, 1:2:10

  -- Concatenation
  highlight(0, 'matlabConcat',          { fg = colors.white,      bg = 'NONE' })  -- [A, B; C, D]


  -----------------------------------------------------------------------------
  -- Documentation / Help

  -- Help Text
  highlight(0, 'matlabHelpText',        { fg = colors.red,        bg = 'NONE' })  -- Help comments after function
  highlight(0, 'matlabHelpH1Line',      { fg = colors.red,        bg = 'NONE' })  -- First help line (summary)

  -- Sections
  highlight(0, 'matlabSection',         { fg = colors.red,        bg = 'NONE', bold = true })  -- %% Section


  -----------------------------------------------------------------------------
  -- Simulink (if editing .m files related to Simulink)

  highlight(0, 'matlabSimulink',        { fg = colors.orange,     bg = 'NONE' })  -- Simulink functions
  highlight(0, 'matlabSimulinkBlock',   { fg = colors.orange,     bg = 'NONE' })  -- Block operations


  -----------------------------------------------------------------------------
  -- Common Toolbox Functions

  -- Signal Processing
  highlight(0, 'matlabFuncSignal',      { fg = colors.orange,     bg = 'NONE' })  -- fft, ifft, filter, conv, etc.

  -- Control System
  highlight(0, 'matlabFuncControl',     { fg = colors.orange,     bg = 'NONE' })  -- tf, ss, bode, step, etc.

  -- Optimization
  highlight(0, 'matlabFuncOptim',       { fg = colors.orange,     bg = 'NONE' })  -- fmincon, fsolve, linprog, etc.

  -- Symbolic Math
  highlight(0, 'matlabFuncSymbolic',    { fg = colors.orange,     bg = 'NONE' })  -- sym, syms, diff, int, solve, etc.

  -- Machine Learning
  highlight(0, 'matlabFuncML',          { fg = colors.orange,     bg = 'NONE' })  -- fitctree, predict, trainNetwork, etc.
end

return matlab
