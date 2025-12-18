-------------------------------------------------------------------------------
-- Nix Files
-- Highlighting for .nix files (Nix expression language).
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local nix       = {}


-------------------------------------------------------------------------------
-- Settings

nix.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Comments
  highlight(0, 'nixComment',               { fg = colors.red,        bg = 'NONE' })  -- # comments
  highlight(0, 'nixLineComment',           { fg = colors.red,        bg = 'NONE' })  -- # single line
  highlight(0, 'nixBlockComment',          { fg = colors.red,        bg = 'NONE' })  -- /* block */
  highlight(0, 'nixTodo',                  { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME

  -- Keywords
  highlight(0, 'nixKeyword',               { fg = colors.pink,       bg = 'NONE' })  -- General keywords
  highlight(0, 'nixLetExprKeyword',        { fg = colors.pink,       bg = 'NONE' })  -- let, in
  highlight(0, 'nixLet',                   { fg = colors.pink,       bg = 'NONE' })  -- let
  highlight(0, 'nixIn',                    { fg = colors.pink,       bg = 'NONE' })  -- in
  highlight(0, 'nixWithExprKeyword',       { fg = colors.pink,       bg = 'NONE' })  -- with
  highlight(0, 'nixWith',                  { fg = colors.pink,       bg = 'NONE' })  -- with
  highlight(0, 'nixInherit',               { fg = colors.pink,       bg = 'NONE' })  -- inherit
  highlight(0, 'nixRecKeyword',            { fg = colors.pink,       bg = 'NONE' })  -- rec
  highlight(0, 'nixRec',                   { fg = colors.pink,       bg = 'NONE' })  -- rec
  highlight(0, 'nixAssertKeyword',         { fg = colors.pink,       bg = 'NONE' })  -- assert
  highlight(0, 'nixAssert',                { fg = colors.pink,       bg = 'NONE' })  -- assert
  highlight(0, 'nixOrKeyword',             { fg = colors.pink,       bg = 'NONE' })  -- or (default value)

  -- Conditional
  highlight(0, 'nixIfExprKeyword',         { fg = colors.pink,       bg = 'NONE' })  -- if, then, else
  highlight(0, 'nixIf',                    { fg = colors.pink,       bg = 'NONE' })  -- if
  highlight(0, 'nixThen',                  { fg = colors.pink,       bg = 'NONE' })  -- then
  highlight(0, 'nixElse',                  { fg = colors.pink,       bg = 'NONE' })  -- else

  -- Boolean and Null
  highlight(0, 'nixBoolean',               { fg = colors.blue,       bg = 'NONE' })  -- true, false
  highlight(0, 'nixTrue',                  { fg = colors.blue,       bg = 'NONE' })  -- true
  highlight(0, 'nixFalse',                 { fg = colors.blue,       bg = 'NONE' })  -- false
  highlight(0, 'nixNull',                  { fg = colors.blue,       bg = 'NONE' })  -- null

  -- Numbers
  highlight(0, 'nixInteger',               { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, 'nixFloat',                 { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Strings
  highlight(0, 'nixString',                { fg = colors.redLight,   bg = 'NONE' })  -- Multi-line ''...''
  highlight(0, 'nixSimpleString',          { fg = colors.redLight,   bg = 'NONE' })  -- Double-quoted "..."
  highlight(0, 'nixStringDelimiter',       { fg = colors.redLight,   bg = 'NONE' })  -- String delimiters

  -- Escape Sequences
  highlight(0, 'nixStringSpecial',         { fg = colors.pink,       bg = 'NONE' })  -- Escapes in ''...''
  highlight(0, 'nixSimpleStringSpecial',   { fg = colors.pink,       bg = 'NONE' })  -- Escapes in "..."
  highlight(0, 'nixEscape',                { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, \\, etc.

  -- Invalid Escapes
  highlight(0, 'nixInvalidStringEscape',       { fg = colors.red, bg = 'NONE', undercurl = true })
  highlight(0, 'nixInvalidSimpleStringEscape', { fg = colors.red, bg = 'NONE', undercurl = true })

  -- Interpolation
  highlight(0, 'nixInterpolation',         { fg = colors.purple,     bg = 'NONE' })  -- ${...} content
  highlight(0, 'nixInterpolationDelimiter',{ fg = colors.pink,       bg = 'NONE' })  -- ${ and }
  highlight(0, 'nixInterpolationParam',    { fg = colors.white,      bg = 'NONE' })  -- Variable in ${}
  highlight(0, 'nixAntiquotation',         { fg = colors.purple,     bg = 'NONE' })  -- Antiquotation

  -- Paths
  highlight(0, 'nixPath',                  { fg = colors.turquoise,  bg = 'NONE' })  -- ./path, /path
  highlight(0, 'nixHomePath',              { fg = colors.turquoise,  bg = 'NONE' })  -- ~/path
  highlight(0, 'nixSearchPath',            { fg = colors.turquoise,  bg = 'NONE' })  -- <nixpkgs>
  highlight(0, 'nixSearchPathRef',         { fg = colors.turquoise,  bg = 'NONE' })  -- Search path reference
  highlight(0, 'nixPathDelimiter',         { fg = colors.pink,       bg = 'NONE' })  -- < > in paths

  -- URIs
  highlight(0, 'nixURI',                   { fg = colors.turquoise,  bg = 'NONE' })  -- https://...

  -- Identifiers and Attributes
  highlight(0, 'nixIdentifier',            { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, 'nixVariable',              { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, 'nixAttribute',             { fg = colors.blue,       bg = 'NONE' })  -- Attribute names
  highlight(0, 'nixAttributePath',         { fg = colors.blue,       bg = 'NONE' })  -- Attribute paths
  highlight(0, 'nixAttributeDot',          { fg = colors.white,      bg = 'NONE' })  -- . in attr access

  -- Functions
  highlight(0, 'nixFunction',              { fg = colors.orange,     bg = 'NONE' })  -- Function definitions
  highlight(0, 'nixFunctionCall',          { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, 'nixLambda',                { fg = colors.pink,       bg = 'NONE' })  -- : in lambda

  -- Function Arguments
  highlight(0, 'nixArgumentDefinition',    { fg = colors.purple,     bg = 'NONE' })  -- Parameter names
  highlight(0, 'nixSimpleFunctionArgument',{ fg = colors.purple,     bg = 'NONE' })  -- Simple args (x:)
  highlight(0, 'nixFormalArgument',        { fg = colors.purple,     bg = 'NONE' })  -- Formal args ({x, y}:)
  highlight(0, 'nixArgumentSeparator',     { fg = colors.white,      bg = 'NONE' })  -- , between args
  highlight(0, 'nixArgumentEllipsis',      { fg = colors.pink,       bg = 'NONE' })  -- ...
  highlight(0, 'nixArgOperator',           { fg = colors.pink,       bg = 'NONE' })  -- @ binding
  highlight(0, 'nixDefaultArg',            { fg = colors.white,      bg = 'NONE' })  -- ? default value

  -- Operators
  highlight(0, 'nixOperator',              { fg = colors.white,      bg = 'NONE' })  -- General operators

  -- Arithmetic Operators
  highlight(0, 'nixArithmeticOp',          { fg = colors.white,      bg = 'NONE' })  -- + - * /

  -- Comparison Operators
  highlight(0, 'nixComparisonOp',          { fg = colors.white,      bg = 'NONE' })  -- < <= > >= == !=

  -- Logical Operators
  highlight(0, 'nixLogicalOp',             { fg = colors.white,      bg = 'NONE' })  -- && || !

  -- Special Operators
  highlight(0, 'nixUpdateOp',              { fg = colors.pink,       bg = 'NONE' })  -- // (update)
  highlight(0, 'nixConcatOp',              { fg = colors.pink,       bg = 'NONE' })  -- ++ (list concat)
  highlight(0, 'nixHasAttrOp',             { fg = colors.pink,       bg = 'NONE' })  -- ? (has attribute)
  highlight(0, 'nixImplicationOp',         { fg = colors.pink,       bg = 'NONE' })  -- -> (implication)
  highlight(0, 'nixNegationOp',            { fg = colors.white,      bg = 'NONE' })  -- - (arithmetic neg)
  highlight(0, 'nixNotOp',                 { fg = colors.white,      bg = 'NONE' })  -- ! (logical not)

  -- Delimiters and Punctuation
  highlight(0, 'nixBraces',                { fg = colors.white,      bg = 'NONE' })  -- { }
  highlight(0, 'nixListBracket',           { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'nixParens',                { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'nixComma',                 { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'nixColon',                 { fg = colors.pink,       bg = 'NONE' })  -- : (lambda/function)
  highlight(0, 'nixSemicolon',             { fg = colors.white,      bg = 'NONE' })  -- ;
  highlight(0, 'nixDot',                   { fg = colors.white,      bg = 'NONE' })  -- .
  highlight(0, 'nixEquals',                { fg = colors.white,      bg = 'NONE' })  -- =


  -----------------------------------------------------------------------------
  -- Built-in Functions

  highlight(0, 'nixBuiltin',               { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions
  highlight(0, 'nixSimpleBuiltin',         { fg = colors.orange,     bg = 'NONE' })  -- Non-namespaced builtins
  highlight(0, 'nixNamespacedBuiltin',     { fg = colors.orange,     bg = 'NONE' })  -- builtins.* functions

  -- Import and Derivation
  highlight(0, 'nixImport',                { fg = colors.orange,     bg = 'NONE' })  -- import
  highlight(0, 'nixDerivation',            { fg = colors.orange,     bg = 'NONE' })  -- derivation

  -- Exception Functions
  highlight(0, 'nixAbort',                 { fg = colors.orange,     bg = 'NONE' })  -- abort
  highlight(0, 'nixThrow',                 { fg = colors.orange,     bg = 'NONE' })  -- throw
  highlight(0, 'nixTryEval',               { fg = colors.orange,     bg = 'NONE' })  -- tryEval

  -- Type Checking
  highlight(0, 'nixIsAttrs',               { fg = colors.orange,     bg = 'NONE' })  -- isAttrs
  highlight(0, 'nixIsBool',                { fg = colors.orange,     bg = 'NONE' })  -- isBool
  highlight(0, 'nixIsFloat',               { fg = colors.orange,     bg = 'NONE' })  -- isFloat
  highlight(0, 'nixIsFunction',            { fg = colors.orange,     bg = 'NONE' })  -- isFunction
  highlight(0, 'nixIsInt',                 { fg = colors.orange,     bg = 'NONE' })  -- isInt
  highlight(0, 'nixIsList',                { fg = colors.orange,     bg = 'NONE' })  -- isList
  highlight(0, 'nixIsNull',                { fg = colors.orange,     bg = 'NONE' })  -- isNull
  highlight(0, 'nixIsPath',                { fg = colors.orange,     bg = 'NONE' })  -- isPath
  highlight(0, 'nixIsString',              { fg = colors.orange,     bg = 'NONE' })  -- isString
  highlight(0, 'nixTypeOf',                { fg = colors.orange,     bg = 'NONE' })  -- typeOf

  -- Arithmetic
  highlight(0, 'nixAdd',                   { fg = colors.orange,     bg = 'NONE' })  -- add
  highlight(0, 'nixSub',                   { fg = colors.orange,     bg = 'NONE' })  -- sub
  highlight(0, 'nixMul',                   { fg = colors.orange,     bg = 'NONE' })  -- mul
  highlight(0, 'nixDiv',                   { fg = colors.orange,     bg = 'NONE' })  -- div
  highlight(0, 'nixLessThan',              { fg = colors.orange,     bg = 'NONE' })  -- lessThan
  highlight(0, 'nixFloor',                 { fg = colors.orange,     bg = 'NONE' })  -- floor
  highlight(0, 'nixCeil',                  { fg = colors.orange,     bg = 'NONE' })  -- ceil

  -- Bitwise
  highlight(0, 'nixBitAnd',                { fg = colors.orange,     bg = 'NONE' })  -- bitAnd
  highlight(0, 'nixBitOr',                 { fg = colors.orange,     bg = 'NONE' })  -- bitOr
  highlight(0, 'nixBitXor',                { fg = colors.orange,     bg = 'NONE' })  -- bitXor

  -- List Functions
  highlight(0, 'nixLength',                { fg = colors.orange,     bg = 'NONE' })  -- length
  highlight(0, 'nixHead',                  { fg = colors.orange,     bg = 'NONE' })  -- head
  highlight(0, 'nixTail',                  { fg = colors.orange,     bg = 'NONE' })  -- tail
  highlight(0, 'nixElemAt',                { fg = colors.orange,     bg = 'NONE' })  -- elemAt
  highlight(0, 'nixElem',                  { fg = colors.orange,     bg = 'NONE' })  -- elem
  highlight(0, 'nixMap',                   { fg = colors.orange,     bg = 'NONE' })  -- map
  highlight(0, 'nixFilter',                { fg = colors.orange,     bg = 'NONE' })  -- filter
  highlight(0, 'nixSort',                  { fg = colors.orange,     bg = 'NONE' })  -- sort
  highlight(0, 'nixConcatLists',           { fg = colors.orange,     bg = 'NONE' })  -- concatLists
  highlight(0, 'nixConcatMap',             { fg = colors.orange,     bg = 'NONE' })  -- concatMap
  highlight(0, 'nixGenList',               { fg = colors.orange,     bg = 'NONE' })  -- genList
  highlight(0, 'nixFoldl',                 { fg = colors.orange,     bg = 'NONE' })  -- foldl'
  highlight(0, 'nixAll',                   { fg = colors.orange,     bg = 'NONE' })  -- all
  highlight(0, 'nixAny',                   { fg = colors.orange,     bg = 'NONE' })  -- any
  highlight(0, 'nixPartition',             { fg = colors.orange,     bg = 'NONE' })  -- partition
  highlight(0, 'nixGroupBy',               { fg = colors.orange,     bg = 'NONE' })  -- groupBy

  -- Attribute Set Functions
  highlight(0, 'nixAttrNames',             { fg = colors.orange,     bg = 'NONE' })  -- attrNames
  highlight(0, 'nixAttrValues',            { fg = colors.orange,     bg = 'NONE' })  -- attrValues
  highlight(0, 'nixHasAttr',               { fg = colors.orange,     bg = 'NONE' })  -- hasAttr
  highlight(0, 'nixGetAttr',               { fg = colors.orange,     bg = 'NONE' })  -- getAttr
  highlight(0, 'nixRemoveAttrs',           { fg = colors.orange,     bg = 'NONE' })  -- removeAttrs
  highlight(0, 'nixIntersectAttrs',        { fg = colors.orange,     bg = 'NONE' })  -- intersectAttrs
  highlight(0, 'nixMapAttrs',              { fg = colors.orange,     bg = 'NONE' })  -- mapAttrs
  highlight(0, 'nixListToAttrs',           { fg = colors.orange,     bg = 'NONE' })  -- listToAttrs
  highlight(0, 'nixCatAttrs',              { fg = colors.orange,     bg = 'NONE' })  -- catAttrs
  highlight(0, 'nixZipAttrsWith',          { fg = colors.orange,     bg = 'NONE' })  -- zipAttrsWith

  -- String Functions
  highlight(0, 'nixStringLength',          { fg = colors.orange,     bg = 'NONE' })  -- stringLength
  highlight(0, 'nixSubstring',             { fg = colors.orange,     bg = 'NONE' })  -- substring
  highlight(0, 'nixSplit',                 { fg = colors.orange,     bg = 'NONE' })  -- split
  highlight(0, 'nixMatch',                 { fg = colors.orange,     bg = 'NONE' })  -- match
  highlight(0, 'nixReplaceStrings',        { fg = colors.orange,     bg = 'NONE' })  -- replaceStrings
  highlight(0, 'nixConcatStringsSep',      { fg = colors.orange,     bg = 'NONE' })  -- concatStringsSep
  highlight(0, 'nixBaseNameOf',            { fg = colors.orange,     bg = 'NONE' })  -- baseNameOf
  highlight(0, 'nixDirOf',                 { fg = colors.orange,     bg = 'NONE' })  -- dirOf
  highlight(0, 'nixToString',              { fg = colors.orange,     bg = 'NONE' })  -- toString

  -- File/Path Functions
  highlight(0, 'nixReadFile',              { fg = colors.orange,     bg = 'NONE' })  -- readFile
  highlight(0, 'nixReadDir',               { fg = colors.orange,     bg = 'NONE' })  -- readDir
  highlight(0, 'nixReadFileType',          { fg = colors.orange,     bg = 'NONE' })  -- readFileType
  highlight(0, 'nixPathExists',            { fg = colors.orange,     bg = 'NONE' })  -- pathExists
  highlight(0, 'nixToFile',                { fg = colors.orange,     bg = 'NONE' })  -- toFile
  highlight(0, 'nixFilterSource',          { fg = colors.orange,     bg = 'NONE' })  -- filterSource
  highlight(0, 'nixPath',                  { fg = colors.orange,     bg = 'NONE' })  -- path
  highlight(0, 'nixStorePath',             { fg = colors.orange,     bg = 'NONE' })  -- storePath
  highlight(0, 'nixFindFile',              { fg = colors.orange,     bg = 'NONE' })  -- findFile

  -- Fetch Functions
  highlight(0, 'nixFetchurl',              { fg = colors.orange,     bg = 'NONE' })  -- fetchurl
  highlight(0, 'nixFetchTarball',          { fg = colors.orange,     bg = 'NONE' })  -- fetchTarball
  highlight(0, 'nixFetchGit',              { fg = colors.orange,     bg = 'NONE' })  -- fetchGit
  highlight(0, 'nixFetchClosure',          { fg = colors.orange,     bg = 'NONE' })  -- fetchClosure

  -- Serialization
  highlight(0, 'nixToJSON',                { fg = colors.orange,     bg = 'NONE' })  -- toJSON
  highlight(0, 'nixToXML',                 { fg = colors.orange,     bg = 'NONE' })  -- toXML
  highlight(0, 'nixFromJSON',              { fg = colors.orange,     bg = 'NONE' })  -- fromJSON
  highlight(0, 'nixFromTOML',              { fg = colors.orange,     bg = 'NONE' })  -- fromTOML

  -- Hash/Crypto
  highlight(0, 'nixHashString',            { fg = colors.orange,     bg = 'NONE' })  -- hashString
  highlight(0, 'nixHashFile',              { fg = colors.orange,     bg = 'NONE' })  -- hashFile

  -- Evaluation Control
  highlight(0, 'nixSeq',                   { fg = colors.orange,     bg = 'NONE' })  -- seq
  highlight(0, 'nixDeepSeq',               { fg = colors.orange,     bg = 'NONE' })  -- deepSeq
  highlight(0, 'nixTrace',                 { fg = colors.orange,     bg = 'NONE' })  -- trace
  highlight(0, 'nixTraceVerbose',          { fg = colors.orange,     bg = 'NONE' })  -- traceVerbose

  -- Version Functions
  highlight(0, 'nixCompareVersions',       { fg = colors.orange,     bg = 'NONE' })  -- compareVersions
  highlight(0, 'nixParseDrvName',          { fg = colors.orange,     bg = 'NONE' })  -- parseDrvName
  highlight(0, 'nixSplitVersion',          { fg = colors.orange,     bg = 'NONE' })  -- splitVersion

  -- Other
  highlight(0, 'nixFunctionArgs',          { fg = colors.orange,     bg = 'NONE' })  -- functionArgs
  highlight(0, 'nixGenericClosure',        { fg = colors.orange,     bg = 'NONE' })  -- genericClosure
  highlight(0, 'nixGetEnv',                { fg = colors.orange,     bg = 'NONE' })  -- getEnv
  highlight(0, 'nixPlaceholder',           { fg = colors.orange,     bg = 'NONE' })  -- placeholder
  highlight(0, 'nixOutputOf',              { fg = colors.orange,     bg = 'NONE' })  -- outputOf


  -----------------------------------------------------------------------------
  -- Built-in Constants

  highlight(0, 'nixBuiltinConstant',       { fg = colors.purple,     bg = 'NONE' })  -- Built-in constants
  highlight(0, 'nixBuiltins',              { fg = colors.purple,     bg = 'NONE' })  -- builtins
  highlight(0, 'nixNixVersion',            { fg = colors.purple,     bg = 'NONE' })  -- nixVersion
  highlight(0, 'nixLangVersion',           { fg = colors.purple,     bg = 'NONE' })  -- langVersion
  highlight(0, 'nixCurrentSystem',         { fg = colors.purple,     bg = 'NONE' })  -- currentSystem
  highlight(0, 'nixCurrentTime',           { fg = colors.purple,     bg = 'NONE' })  -- currentTime
  highlight(0, 'nixNixPath',               { fg = colors.purple,     bg = 'NONE' })  -- nixPath
  highlight(0, 'nixStoreDir',              { fg = colors.purple,     bg = 'NONE' })  -- storeDir


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.nix)

  -- Comments
  highlight(0, '@comment.nix',                 { fg = colors.red,        bg = 'NONE' })  -- Comments

  -- Keywords
  highlight(0, '@keyword.nix',                 { fg = colors.pink,       bg = 'NONE' })  -- assert, in, inherit, let, rec, with
  highlight(0, '@keyword.conditional.nix',     { fg = colors.pink,       bg = 'NONE' })  -- if, then, else
  highlight(0, '@keyword.operator.nix',        { fg = colors.pink,       bg = 'NONE' })  -- or
  highlight(0, '@keyword.import.nix',          { fg = colors.pink,       bg = 'NONE' })  -- import
  highlight(0, '@keyword.exception.nix',       { fg = colors.orange,     bg = 'NONE' })  -- abort, throw

  -- Variables
  highlight(0, '@variable.nix',                { fg = colors.white,      bg = 'NONE' })  -- General identifiers
  highlight(0, '@variable.parameter.nix',      { fg = colors.purple,     bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.parameter.builtin.nix', { fg = colors.pink,    bg = 'NONE' })  -- ...
  highlight(0, '@variable.member.nix',         { fg = colors.blue,       bg = 'NONE' })  -- Attribute access

  -- Functions
  highlight(0, '@function.nix',                { fg = colors.orange,     bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.nix',           { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.nix',        { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions

  -- Strings
  highlight(0, '@string.nix',                  { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.nix',           { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.path.nix',     { fg = colors.turquoise,  bg = 'NONE' })  -- Paths
  highlight(0, '@string.special.url.nix',      { fg = colors.turquoise,  bg = 'NONE' })  -- URIs

  -- Numbers
  highlight(0, '@number.nix',                  { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.nix',            { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.nix',                 { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Constants
  highlight(0, '@constant.nix',                { fg = colors.blue,       bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.nix',        { fg = colors.purple,     bg = 'NONE' })  -- null, builtins, nixVersion

  -- Operators
  highlight(0, '@operator.nix',                { fg = colors.white,      bg = 'NONE' })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.nix',     { fg = colors.white,      bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.nix',   { fg = colors.white,      bg = 'NONE' })  -- . ; : ,
  highlight(0, '@punctuation.special.nix',     { fg = colors.pink,       bg = 'NONE' })  -- ${ }


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.nix)

  highlight(0, '@lsp.type.variable.nix',       { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.nix',      { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.nix',       { fg = colors.blue,       bg = 'NONE' })  -- Attributes
  highlight(0, '@lsp.type.function.nix',       { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.string.nix',         { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.nix',         { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.keyword.nix',        { fg = colors.pink,       bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.comment.nix',        { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.namespace.nix',      { fg = colors.turquoise,  bg = 'NONE' })  -- Namespaces


  -----------------------------------------------------------------------------
  -- NixOS/Nixpkgs Common Patterns

  -- Nixpkgs Functions
  highlight(0, 'nixpkgsMkDerivation',      { fg = colors.orange,     bg = 'NONE' })  -- mkDerivation
  highlight(0, 'nixpkgsStdenv',            { fg = colors.purple,     bg = 'NONE' })  -- stdenv
  highlight(0, 'nixpkgsFetchFromGitHub',   { fg = colors.orange,     bg = 'NONE' })  -- fetchFromGitHub
  highlight(0, 'nixpkgsFetchFromGitLab',   { fg = colors.orange,     bg = 'NONE' })  -- fetchFromGitLab
  highlight(0, 'nixpkgsFetchPypi',         { fg = colors.orange,     bg = 'NONE' })  -- fetchPypi
  highlight(0, 'nixpkgsFetchCrate',        { fg = colors.orange,     bg = 'NONE' })  -- fetchCrate
  highlight(0, 'nixpkgsFetchNuGet',        { fg = colors.orange,     bg = 'NONE' })  -- fetchNuGet
  highlight(0, 'nixpkgsFetchzip',          { fg = colors.orange,     bg = 'NONE' })  -- fetchzip

  -- Build Helpers
  highlight(0, 'nixpkgsBuildPythonPackage',{ fg = colors.orange,     bg = 'NONE' })  -- buildPythonPackage
  highlight(0, 'nixpkgsBuildGoModule',     { fg = colors.orange,     bg = 'NONE' })  -- buildGoModule
  highlight(0, 'nixpkgsBuildRustPackage',  { fg = colors.orange,     bg = 'NONE' })  -- buildRustPackage
  highlight(0, 'nixpkgsBuildNpmPackage',   { fg = colors.orange,     bg = 'NONE' })  -- buildNpmPackage

  -- Library Functions (lib.*)
  highlight(0, 'nixpkgsLib',               { fg = colors.purple,     bg = 'NONE' })  -- lib
  highlight(0, 'nixpkgsLibMkIf',           { fg = colors.orange,     bg = 'NONE' })  -- lib.mkIf
  highlight(0, 'nixpkgsLibMkOption',       { fg = colors.orange,     bg = 'NONE' })  -- lib.mkOption
  highlight(0, 'nixpkgsLibMkEnableOption', { fg = colors.orange,     bg = 'NONE' })  -- lib.mkEnableOption
  highlight(0, 'nixpkgsLibMkForce',        { fg = colors.orange,     bg = 'NONE' })  -- lib.mkForce
  highlight(0, 'nixpkgsLibMkDefault',      { fg = colors.orange,     bg = 'NONE' })  -- lib.mkDefault
  highlight(0, 'nixpkgsLibMkMerge',        { fg = colors.orange,     bg = 'NONE' })  -- lib.mkMerge
  highlight(0, 'nixpkgsLibMapAttrs',       { fg = colors.orange,     bg = 'NONE' })  -- lib.mapAttrs
  highlight(0, 'nixpkgsLibFilterAttrs',    { fg = colors.orange,     bg = 'NONE' })  -- lib.filterAttrs
  highlight(0, 'nixpkgsLibGenAttrs',       { fg = colors.orange,     bg = 'NONE' })  -- lib.genAttrs
  highlight(0, 'nixpkgsLibOptional',       { fg = colors.orange,     bg = 'NONE' })  -- lib.optional
  highlight(0, 'nixpkgsLibOptionals',      { fg = colors.orange,     bg = 'NONE' })  -- lib.optionals
  highlight(0, 'nixpkgsLibOptionalString', { fg = colors.orange,     bg = 'NONE' })  -- lib.optionalString
  highlight(0, 'nixpkgsLibOptionalAttrs',  { fg = colors.orange,     bg = 'NONE' })  -- lib.optionalAttrs
  highlight(0, 'nixpkgsLibConcatStrings',  { fg = colors.orange,     bg = 'NONE' })  -- lib.concatStrings
  highlight(0, 'nixpkgsLibConcatMapStrings', { fg = colors.orange,   bg = 'NONE' })  -- lib.concatMapStrings
  highlight(0, 'nixpkgsLibFlatten',        { fg = colors.orange,     bg = 'NONE' })  -- lib.flatten
  highlight(0, 'nixpkgsLibUnique',         { fg = colors.orange,     bg = 'NONE' })  -- lib.unique

  -- Types (for NixOS modules)
  highlight(0, 'nixpkgsTypes',             { fg = colors.turquoise,  bg = 'NONE' })  -- types
  highlight(0, 'nixpkgsTypesStr',          { fg = colors.turquoise,  bg = 'NONE' })  -- types.str
  highlight(0, 'nixpkgsTypesInt',          { fg = colors.turquoise,  bg = 'NONE' })  -- types.int
  highlight(0, 'nixpkgsTypesBool',         { fg = colors.turquoise,  bg = 'NONE' })  -- types.bool
  highlight(0, 'nixpkgsTypesPath',         { fg = colors.turquoise,  bg = 'NONE' })  -- types.path
  highlight(0, 'nixpkgsTypesAttrs',        { fg = colors.turquoise,  bg = 'NONE' })  -- types.attrs
  highlight(0, 'nixpkgsTypesListOf',       { fg = colors.turquoise,  bg = 'NONE' })  -- types.listOf
  highlight(0, 'nixpkgsTypesAttrsOf',      { fg = colors.turquoise,  bg = 'NONE' })  -- types.attrsOf
  highlight(0, 'nixpkgsTypesEnum',         { fg = colors.turquoise,  bg = 'NONE' })  -- types.enum
  highlight(0, 'nixpkgsTypesNullOr',       { fg = colors.turquoise,  bg = 'NONE' })  -- types.nullOr
  highlight(0, 'nixpkgsTypesEither',       { fg = colors.turquoise,  bg = 'NONE' })  -- types.either
  highlight(0, 'nixpkgsTypesOneOf',        { fg = colors.turquoise,  bg = 'NONE' })  -- types.oneOf
  highlight(0, 'nixpkgsTypesSubmodule',    { fg = colors.turquoise,  bg = 'NONE' })  -- types.submodule
  highlight(0, 'nixpkgsTypesPackage',      { fg = colors.turquoise,  bg = 'NONE' })  -- types.package


  -----------------------------------------------------------------------------
  -- NixOS Module Attributes

  highlight(0, 'nixosModuleOption',        { fg = colors.blue,       bg = 'NONE' })  -- options
  highlight(0, 'nixosModuleConfig',        { fg = colors.blue,       bg = 'NONE' })  -- config
  highlight(0, 'nixosModuleMeta',          { fg = colors.blue,       bg = 'NONE' })  -- meta
  highlight(0, 'nixosModuleImports',       { fg = colors.blue,       bg = 'NONE' })  -- imports

  -- Common NixOS Options
  highlight(0, 'nixosOptionEnable',        { fg = colors.blue,       bg = 'NONE' })  -- enable
  highlight(0, 'nixosOptionPackage',       { fg = colors.blue,       bg = 'NONE' })  -- package
  highlight(0, 'nixosOptionExtraOptions',  { fg = colors.blue,       bg = 'NONE' })  -- extraOptions
  highlight(0, 'nixosOptionSettings',      { fg = colors.blue,       bg = 'NONE' })  -- settings


  -----------------------------------------------------------------------------
  -- Flakes

  highlight(0, 'nixFlakeInput',            { fg = colors.blue,       bg = 'NONE' })  -- inputs
  highlight(0, 'nixFlakeOutput',           { fg = colors.blue,       bg = 'NONE' })  -- outputs
  highlight(0, 'nixFlakeDescription',      { fg = colors.blue,       bg = 'NONE' })  -- description
  highlight(0, 'nixFlakeUrl',              { fg = colors.turquoise,  bg = 'NONE' })  -- url

  -- Flake Output Types
  highlight(0, 'nixFlakePackages',         { fg = colors.turquoise,  bg = 'NONE' })  -- packages
  highlight(0, 'nixFlakeDevShells',        { fg = colors.turquoise,  bg = 'NONE' })  -- devShells
  highlight(0, 'nixFlakeApps',             { fg = colors.turquoise,  bg = 'NONE' })  -- apps
  highlight(0, 'nixFlakeNixosConfigurations', { fg = colors.turquoise, bg = 'NONE' })  -- nixosConfigurations
  highlight(0, 'nixFlakeNixosModules',     { fg = colors.turquoise,  bg = 'NONE' })  -- nixosModules
  highlight(0, 'nixFlakeOverlays',         { fg = colors.turquoise,  bg = 'NONE' })  -- overlays
  highlight(0, 'nixFlakeTemplates',        { fg = colors.turquoise,  bg = 'NONE' })  -- templates
  highlight(0, 'nixFlakeFormatter',        { fg = colors.turquoise,  bg = 'NONE' })  -- formatter
  highlight(0, 'nixFlakeChecks',           { fg = colors.turquoise,  bg = 'NONE' })  -- checks
  highlight(0, 'nixFlakeLib',              { fg = colors.turquoise,  bg = 'NONE' })  -- lib

  -- Flake References
  highlight(0, 'nixFlakeRefGithub',        { fg = colors.turquoise,  bg = 'NONE' })  -- github:owner/repo
  highlight(0, 'nixFlakeRefGitlab',        { fg = colors.turquoise,  bg = 'NONE' })  -- gitlab:owner/repo
  highlight(0, 'nixFlakeRefSourcehut',     { fg = colors.turquoise,  bg = 'NONE' })  -- sourcehut:~owner/repo
  highlight(0, 'nixFlakeRefPath',          { fg = colors.turquoise,  bg = 'NONE' })  -- path:./local
  highlight(0, 'nixFlakeRefNixpkgs',       { fg = colors.turquoise,  bg = 'NONE' })  -- nixpkgs


  -----------------------------------------------------------------------------
  -- Home Manager

  highlight(0, 'homeManagerPrograms',      { fg = colors.blue,       bg = 'NONE' })  -- programs
  highlight(0, 'homeManagerServices',      { fg = colors.blue,       bg = 'NONE' })  -- services
  highlight(0, 'homeManagerHome',          { fg = colors.blue,       bg = 'NONE' })  -- home
  highlight(0, 'homeManagerXdg',           { fg = colors.blue,       bg = 'NONE' })  -- xdg


  -----------------------------------------------------------------------------
  -- Error Highlighting

  highlight(0, 'nixError',                 { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax errors
  highlight(0, 'nixEvalError',             { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Evaluation errors
end

return nix
