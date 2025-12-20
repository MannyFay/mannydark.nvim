-------------------------------------------------------------------------------
-- Elm
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local elm       = {}


-------------------------------------------------------------------------------
-- Settings

elm.setupHighlighting = function()


  -------------------------------------------------------------------------
  -- Vim Elm Syntax Groups (vim-elm-syntax)
  -------------------------------------------------------------------------

  -- Keywords and Control Flow
  highlight(0, 'elmConditional',          { link = "Conditional" })  -- if, then, else, case, of
  highlight(0, 'elmKeyword',              { link = "Keyword" })  -- let, in
  highlight(0, 'elmLet',                  { fg = colors.blue,       bg = 'NONE'            })  -- let
  highlight(0, 'elmIn',                   { fg = colors.blue,       bg = 'NONE'            })  -- in
  highlight(0, 'elmCase',                 { fg = colors.blue,       bg = 'NONE'            })  -- case
  highlight(0, 'elmOf',                   { fg = colors.blue,       bg = 'NONE'            })  -- of

  -- Module and Import
  highlight(0, 'elmImport',               { fg = colors.blue,       bg = 'NONE'            })  -- module, import, exposing, as, where
  highlight(0, 'elmModule',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names
  highlight(0, 'elmModuleName',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Module name in declaration
  highlight(0, 'elmModuleKeyword',        { link = "Keyword" })  -- module keyword
  highlight(0, 'elmImportKeyword',        { link = "Keyword" })  -- import keyword
  highlight(0, 'elmExposing',             { fg = colors.blue,       bg = 'NONE'            })  -- exposing keyword
  highlight(0, 'elmAs',                   { fg = colors.blue,       bg = 'NONE'            })  -- as keyword
  highlight(0, 'elmWhere',                { fg = colors.blue,       bg = 'NONE'            })  -- where keyword

  -- Types
  highlight(0, 'elmType',                 { link = "Type" })  -- Type names (capitalized)
  highlight(0, 'elmTypedef',              { link = "Type" })  -- type, port keywords
  highlight(0, 'elmTypeKeyword',          { link = "Keyword" })  -- type keyword
  highlight(0, 'elmAlias',                { fg = colors.blue,       bg = 'NONE'            })  -- alias keyword
  highlight(0, 'elmTypeAlias',            { link = "Type" })  -- Type alias name
  highlight(0, 'elmNumberType',           { link = "Number" })  -- number type keyword
  highlight(0, 'elmTopLevelDecl',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Top-level type declarations
  highlight(0, 'elmTopLevelTypedef',      { link = "Type" })  -- Type definitions

  -- Type Annotations
  highlight(0, 'elmTypeAnnotation',       { link = "Type" })  -- Type annotations
  highlight(0, 'elmTypeVariable',         { link = "Type" })  -- Type variables (lowercase in types: a, b, msg)
  highlight(0, 'elmTypeConstraint',       { link = "Type" })  -- Type constraints

  -- Built-in Types
  highlight(0, 'elmBasicType',            { link = "Type" })  -- Int, Float, String, Bool, Char
  highlight(0, 'elmInt',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- Int type
  highlight(0, 'elmFloatType',            { link = "Type" })  -- Float type
  highlight(0, 'elmStringType',           { link = "String" })  -- String type
  highlight(0, 'elmBoolType',             { link = "Type" })  -- Bool type
  highlight(0, 'elmCharType',             { link = "Type" })  -- Char type
  highlight(0, 'elmListType',             { link = "Type" })  -- List type
  highlight(0, 'elmMaybe',                { fg = colors.turquoise,  bg = 'NONE'            })  -- Maybe type
  highlight(0, 'elmResult',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Result type
  highlight(0, 'elmCmd',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- Cmd type
  highlight(0, 'elmSub',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- Sub type
  highlight(0, 'elmTask',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Task type
  highlight(0, 'elmHtml',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Html type
  highlight(0, 'elmMsg',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- Msg type
  highlight(0, 'elmModel',                { fg = colors.turquoise,  bg = 'NONE'            })  -- Model type

  -- Constructors
  highlight(0, 'elmConstructor',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Data constructors (capitalized)
  highlight(0, 'elmJust',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Just constructor
  highlight(0, 'elmNothing',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Nothing constructor
  highlight(0, 'elmOk',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- Ok constructor
  highlight(0, 'elmErr',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- Err constructor
  highlight(0, 'elmTrue',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- True constructor
  highlight(0, 'elmFalse',                { fg = colors.turquoise,  bg = 'NONE'            })  -- False constructor
  highlight(0, 'elmUnit',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- () unit type/value

  -- Functions
  highlight(0, 'elmFuncName',             { link = "Function" })  -- Function names
  highlight(0, 'elmTopLevelFunction',     { link = "Function" })  -- Top-level function definitions
  highlight(0, 'elmFunctionCall',         { link = "Function" })  -- Function calls
  highlight(0, 'elmLambdaFunc',           { link = "Function" })  -- Lambda function: \x -> ...
  highlight(0, 'elmLambda',               { fg = colors.blue,       bg = 'NONE'            })  -- \ (backslash for lambda)
  highlight(0, 'elmArrow',                { fg = colors.blue,       bg = 'NONE'            })  -- -> (arrow)

  -- Port
  highlight(0, 'elmPort',                 { fg = colors.pink,       bg = 'NONE'            })  -- port keyword
  highlight(0, 'elmPortKeyword',          { link = "Keyword" })  -- port keyword
  highlight(0, 'elmPortName',             { fg = colors.orange,     bg = 'NONE'            })  -- Port function name

  -- Operators
  highlight(0, 'elmOperator',             { link = "Operator" })  -- Standard operators
  highlight(0, 'elmCoreOperator',         { link = "Operator" })  -- elm/core operators
  highlight(0, 'elmParserOperator',       { link = "Operator" })  -- elm/parser operators: |= |.
  highlight(0, 'elmUrlOperator',          { link = "Operator" })  -- elm/url operators: </> <?>
  highlight(0, 'elmPipe',                 { fg = colors.white,      bg = 'NONE'            })  -- |> (pipe forward)
  highlight(0, 'elmPipeBack',             { fg = colors.white,      bg = 'NONE'            })  -- <| (pipe backward)
  highlight(0, 'elmCompose',              { fg = colors.white,      bg = 'NONE'            })  -- >> (compose forward)
  highlight(0, 'elmComposeBack',          { fg = colors.white,      bg = 'NONE'            })  -- << (compose backward)
  highlight(0, 'elmCons',                 { fg = colors.white,      bg = 'NONE'            })  -- :: (cons)
  highlight(0, 'elmConcat',               { fg = colors.white,      bg = 'NONE'            })  -- ++ (concat)
  highlight(0, 'elmRange',                { fg = colors.white,      bg = 'NONE'            })  -- .. (range, deprecated)
  highlight(0, 'elmMathOperator',         { link = "Operator" })  -- + - * / // ^ %
  highlight(0, 'elmCompareOperator',      { link = "Operator" })  -- == /= < > <= >=
  highlight(0, 'elmBoolOperator',         { link = "Operator" })  -- && || not
  highlight(0, 'elmAssignment',           { fg = colors.white,      bg = 'NONE'            })  -- = (assignment/definition)

  -- Numbers
  highlight(0, 'elmNumber',               { link = "Number" })  -- Integer literals
  highlight(0, 'elmFloat',                { fg = colors.greenLight, bg = 'NONE'            })  -- Floating-point literals
  highlight(0, 'elmHexNumber',            { link = "Number" })  -- Hexadecimal: 0x1A

  -- Strings and Characters
  highlight(0, 'elmString',               { link = "String" })  -- Double-quoted strings
  highlight(0, 'elmTripleString',         { link = "String" })  -- Triple-quoted strings (multiline)
  highlight(0, 'elmChar',                 { fg = colors.redLight,   bg = 'NONE'            })  -- Character literals: 'a'
  highlight(0, 'elmStringEscape',         { link = "String" })  -- Escape sequences: \n, \t, etc.
  highlight(0, 'elmUnicode',              { fg = colors.pink,       bg = 'NONE'            })  -- Unicode escapes: \u{...}

  -- Comments
  highlight(0, 'elmComment',              { link = "Comment" })  -- Block comments: {- -}
  highlight(0, 'elmLineComment',          { link = "Comment" })  -- Line comments: --
  highlight(0, 'elmDocComment',           { link = "Comment" })  -- Documentation comments: {-| -}
  highlight(0, 'elmTodo',                 { fg = colors.pink,       bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Punctuation and Delimiters
  highlight(0, 'elmDelimiter',            { link = "Delimiter" })  -- Commas, semicolons
  highlight(0, 'elmBraces',               { fg = colors.white,      bg = 'NONE'            })  -- ( ) [ ] { }
  highlight(0, 'elmParen',                { fg = colors.white,      bg = 'NONE'            })  -- Parentheses
  highlight(0, 'elmBracket',              { fg = colors.white,      bg = 'NONE'            })  -- Square brackets
  highlight(0, 'elmCurly',                { fg = colors.white,      bg = 'NONE'            })  -- Curly braces
  highlight(0, 'elmComma',                { fg = colors.white,      bg = 'NONE'            })  -- Commas
  highlight(0, 'elmColon',                { fg = colors.white,      bg = 'NONE'            })  -- : (type annotation separator)
  highlight(0, 'elmDot',                  { fg = colors.white,      bg = 'NONE'            })  -- . (record access, qualified names)
  highlight(0, 'elmPipe',                 { fg = colors.white,      bg = 'NONE'            })  -- | (union type separator)
  highlight(0, 'elmUnderscore',           { fg = colors.white,      bg = 'NONE'            })  -- _ (wildcard pattern)

  -- Records
  highlight(0, 'elmRecord',               { fg = colors.white,      bg = 'NONE'            })  -- Record literals
  highlight(0, 'elmRecordField',          { fg = colors.blue,       bg = 'NONE'            })  -- Record field names
  highlight(0, 'elmRecordAccess',         { fg = colors.blue,       bg = 'NONE'            })  -- .field access
  highlight(0, 'elmRecordUpdate',         { fg = colors.blue,       bg = 'NONE'            })  -- { record | field = value }

  -- Tuples
  highlight(0, 'elmTuple',                { fg = colors.white,      bg = 'NONE'            })  -- Tuple values: (a, b)
  highlight(0, 'elmTupleFunction',        { link = "Function" })  -- Tuple constructors: Tuple.pair

  -- Pattern Matching
  highlight(0, 'elmPattern',              { fg = colors.white,      bg = 'NONE'            })  -- Pattern in case/let
  highlight(0, 'elmPatternConstructor',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Constructor in pattern
  highlight(0, 'elmWildcard',             { fg = colors.white,      bg = 'NONE'            })  -- _ wildcard pattern

  -- Blocks
  highlight(0, 'elmCaseBlock',            { fg = colors.white,      bg = 'NONE'            })  -- Case expression blocks
  highlight(0, 'elmCaseItemBlock',        { fg = colors.white,      bg = 'NONE'            })  -- Individual case items
  highlight(0, 'elmLetBlock',             { fg = colors.white,      bg = 'NONE'            })  -- Let-in expressions

  -- Debug (special highlighting - these should stand out)
  highlight(0, 'elmDebug',                { fg = colors.orange,     bg = 'NONE', bold = true })  -- Debug.log, Debug.todo, Debug.toString
  highlight(0, 'elmDebugLog',             { fg = colors.orange,     bg = 'NONE', bold = true })  -- Debug.log
  highlight(0, 'elmDebugTodo',            { fg = colors.orange,     bg = 'NONE', bold = true })  -- Debug.todo
  highlight(0, 'elmDebugToString',        { link = "String" })  -- Debug.toString

  -- Core Library Functions (commonly used)
  highlight(0, 'elmCoreFunction',         { link = "Function" })  -- Core library functions
  highlight(0, 'elmListFunction',         { link = "Function" })  -- List.map, List.filter, etc.
  highlight(0, 'elmMaybeFunction',        { link = "Function" })  -- Maybe.map, Maybe.withDefault
  highlight(0, 'elmResultFunction',       { link = "Function" })  -- Result.map, Result.andThen
  highlight(0, 'elmStringFunction',       { link = "String" })  -- String.concat, String.length
  highlight(0, 'elmDictFunction',         { link = "Function" })  -- Dict.get, Dict.insert
  highlight(0, 'elmSetFunction',          { link = "Function" })  -- Set.member, Set.insert
  highlight(0, 'elmArrayFunction',        { link = "Function" })  -- Array.get, Array.set
  highlight(0, 'elmJsonFunction',         { link = "Function" })  -- Json.Decode, Json.Encode
  highlight(0, 'elmHtmlFunction',         { link = "Function" })  -- Html.div, Html.text
  highlight(0, 'elmHttpFunction',         { link = "Function" })  -- Http.get, Http.post


  -------------------------------------------------------------------------
  -- Treesitter Elm Captures
  -------------------------------------------------------------------------

  -- Comments
  highlight(0, '@comment.elm',                   { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.elm',     { link = "Comment" })  -- Doc comments
  highlight(0, '@spell.elm',                     { link = '@comment.elm'                              })  -- Spell check

  -- Keywords
  highlight(0, '@keyword.elm',                   { link = "Keyword" })  -- Keywords
  highlight(0, '@keyword.conditional.elm',       { link = "Conditional" })  -- if, then, else, case, of
  highlight(0, '@keyword.import.elm',            { link = "Keyword" })  -- module, import, exposing, as

  -- Functions
  highlight(0, '@function.elm',                  { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.elm',             { link = "Function" })  -- Function calls

  -- Variables
  highlight(0, '@variable.elm',                  { link = "Variable" })  -- Variables
  highlight(0, '@variable.member.elm',           { link = "Variable" })  -- Record fields
  highlight(0, '@variable.parameter.elm',        { link = "Variable" })  -- Function parameters

  -- Types
  highlight(0, '@type.elm',                      { link = "Type" })  -- Types
  highlight(0, '@type.definition.elm',           { link = "Type" })  -- Type definitions
  highlight(0, '@constructor.elm',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Constructors

  -- Modules
  highlight(0, '@module.elm',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names

  -- Properties
  highlight(0, '@property.elm',                  { fg = colors.blue,       bg = 'NONE'            })  -- Record properties

  -- Operators
  highlight(0, '@operator.elm',                  { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.elm',       { fg = colors.white,      bg = 'NONE'            })  -- Brackets
  highlight(0, '@punctuation.delimiter.elm',     { link = "Delimiter" })  -- Delimiters
  highlight(0, '@punctuation.special.elm',       { fg = colors.white,      bg = 'NONE'            })  -- Special punctuation

  -- Literals
  highlight(0, '@number.elm',                    { link = "Number" })  -- Numbers
  highlight(0, '@boolean.elm',                   { link = "Boolean" })  -- Booleans
  highlight(0, '@string.elm',                    { link = "String" })  -- Strings
  highlight(0, '@character.elm',                 { fg = colors.redLight,   bg = 'NONE'            })  -- Characters
  highlight(0, '@character.special.elm',         { fg = colors.pink,       bg = 'NONE'            })  -- Special characters/escapes


  -------------------------------------------------------------------------
  -- LSP Semantic Tokens
  -------------------------------------------------------------------------

  highlight(0, '@lsp.type.type.elm',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Types
  highlight(0, '@lsp.type.class.elm',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Type classes
  highlight(0, '@lsp.type.function.elm',         { fg = colors.orange,     bg = 'NONE'            })  -- Functions
  highlight(0, '@lsp.type.method.elm',           { fg = colors.orange,     bg = 'NONE'            })  -- Methods
  highlight(0, '@lsp.type.property.elm',         { fg = colors.blue,       bg = 'NONE'            })  -- Properties
  highlight(0, '@lsp.type.variable.elm',         { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.elm',        { fg = colors.white,      bg = 'NONE'            })  -- Parameters
  highlight(0, '@lsp.type.namespace.elm',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Modules
  highlight(0, '@lsp.type.enum.elm',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Union types
  highlight(0, '@lsp.type.enumMember.elm',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Constructors
  highlight(0, '@lsp.type.string.elm',           { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.elm',           { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.elm',          { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.elm',         { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.comment.elm',          { link = "Comment" })  -- Comments

  highlight(0, '@lsp.mod.declaration.elm',       { fg = colors.orange,     bg = 'NONE'            })  -- Declarations
  highlight(0, '@lsp.mod.definition.elm',        { fg = colors.orange,     bg = 'NONE'            })  -- Definitions
  highlight(0, '@lsp.mod.readonly.elm',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Read-only
  highlight(0, '@lsp.mod.defaultLibrary.elm',    { fg = colors.turquoise,  bg = 'NONE'            })  -- Standard library

end

return elm
