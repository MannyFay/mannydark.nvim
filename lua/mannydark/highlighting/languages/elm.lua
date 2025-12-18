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
  highlight(0, 'elmConditional',          { fg = colors.blue,       bg = 'NONE'            })  -- if, then, else, case, of
  highlight(0, 'elmKeyword',              { fg = colors.blue,       bg = 'NONE'            })  -- let, in
  highlight(0, 'elmLet',                  { fg = colors.blue,       bg = 'NONE'            })  -- let
  highlight(0, 'elmIn',                   { fg = colors.blue,       bg = 'NONE'            })  -- in
  highlight(0, 'elmCase',                 { fg = colors.blue,       bg = 'NONE'            })  -- case
  highlight(0, 'elmOf',                   { fg = colors.blue,       bg = 'NONE'            })  -- of

  -- Module and Import
  highlight(0, 'elmImport',               { fg = colors.blue,       bg = 'NONE'            })  -- module, import, exposing, as, where
  highlight(0, 'elmModule',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names
  highlight(0, 'elmModuleName',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Module name in declaration
  highlight(0, 'elmModuleKeyword',        { fg = colors.blue,       bg = 'NONE'            })  -- module keyword
  highlight(0, 'elmImportKeyword',        { fg = colors.blue,       bg = 'NONE'            })  -- import keyword
  highlight(0, 'elmExposing',             { fg = colors.blue,       bg = 'NONE'            })  -- exposing keyword
  highlight(0, 'elmAs',                   { fg = colors.blue,       bg = 'NONE'            })  -- as keyword
  highlight(0, 'elmWhere',                { fg = colors.blue,       bg = 'NONE'            })  -- where keyword

  -- Types
  highlight(0, 'elmType',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names (capitalized)
  highlight(0, 'elmTypedef',              { fg = colors.blue,       bg = 'NONE'            })  -- type, port keywords
  highlight(0, 'elmTypeKeyword',          { fg = colors.blue,       bg = 'NONE'            })  -- type keyword
  highlight(0, 'elmAlias',                { fg = colors.blue,       bg = 'NONE'            })  -- alias keyword
  highlight(0, 'elmTypeAlias',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Type alias name
  highlight(0, 'elmNumberType',           { fg = colors.turquoise,  bg = 'NONE'            })  -- number type keyword
  highlight(0, 'elmTopLevelDecl',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Top-level type declarations
  highlight(0, 'elmTopLevelTypedef',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Type definitions

  -- Type Annotations
  highlight(0, 'elmTypeAnnotation',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Type annotations
  highlight(0, 'elmTypeVariable',         { fg = colors.purple,     bg = 'NONE'            })  -- Type variables (lowercase in types: a, b, msg)
  highlight(0, 'elmTypeConstraint',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Type constraints

  -- Built-in Types
  highlight(0, 'elmBasicType',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Int, Float, String, Bool, Char
  highlight(0, 'elmInt',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- Int type
  highlight(0, 'elmFloatType',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Float type
  highlight(0, 'elmStringType',           { fg = colors.turquoise,  bg = 'NONE'            })  -- String type
  highlight(0, 'elmBoolType',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Bool type
  highlight(0, 'elmCharType',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Char type
  highlight(0, 'elmListType',             { fg = colors.turquoise,  bg = 'NONE'            })  -- List type
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
  highlight(0, 'elmFuncName',             { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'elmTopLevelFunction',     { fg = colors.orange,     bg = 'NONE'            })  -- Top-level function definitions
  highlight(0, 'elmFunctionCall',         { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'elmLambdaFunc',           { fg = colors.blue,       bg = 'NONE'            })  -- Lambda function: \x -> ...
  highlight(0, 'elmLambda',               { fg = colors.blue,       bg = 'NONE'            })  -- \ (backslash for lambda)
  highlight(0, 'elmArrow',                { fg = colors.blue,       bg = 'NONE'            })  -- -> (arrow)

  -- Port
  highlight(0, 'elmPort',                 { fg = colors.pink,       bg = 'NONE'            })  -- port keyword
  highlight(0, 'elmPortKeyword',          { fg = colors.pink,       bg = 'NONE'            })  -- port keyword
  highlight(0, 'elmPortName',             { fg = colors.orange,     bg = 'NONE'            })  -- Port function name

  -- Operators
  highlight(0, 'elmOperator',             { fg = colors.white,      bg = 'NONE'            })  -- Standard operators
  highlight(0, 'elmCoreOperator',         { fg = colors.white,      bg = 'NONE'            })  -- elm/core operators
  highlight(0, 'elmParserOperator',       { fg = colors.white,      bg = 'NONE'            })  -- elm/parser operators: |= |.
  highlight(0, 'elmUrlOperator',          { fg = colors.white,      bg = 'NONE'            })  -- elm/url operators: </> <?>
  highlight(0, 'elmPipe',                 { fg = colors.white,      bg = 'NONE'            })  -- |> (pipe forward)
  highlight(0, 'elmPipeBack',             { fg = colors.white,      bg = 'NONE'            })  -- <| (pipe backward)
  highlight(0, 'elmCompose',              { fg = colors.white,      bg = 'NONE'            })  -- >> (compose forward)
  highlight(0, 'elmComposeBack',          { fg = colors.white,      bg = 'NONE'            })  -- << (compose backward)
  highlight(0, 'elmCons',                 { fg = colors.white,      bg = 'NONE'            })  -- :: (cons)
  highlight(0, 'elmConcat',               { fg = colors.white,      bg = 'NONE'            })  -- ++ (concat)
  highlight(0, 'elmRange',                { fg = colors.white,      bg = 'NONE'            })  -- .. (range, deprecated)
  highlight(0, 'elmMathOperator',         { fg = colors.white,      bg = 'NONE'            })  -- + - * / // ^ %
  highlight(0, 'elmCompareOperator',      { fg = colors.white,      bg = 'NONE'            })  -- == /= < > <= >=
  highlight(0, 'elmBoolOperator',         { fg = colors.white,      bg = 'NONE'            })  -- && || not
  highlight(0, 'elmAssignment',           { fg = colors.white,      bg = 'NONE'            })  -- = (assignment/definition)

  -- Numbers
  highlight(0, 'elmNumber',               { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'elmFloat',                { fg = colors.greenLight, bg = 'NONE'            })  -- Floating-point literals
  highlight(0, 'elmHexNumber',            { fg = colors.greenLight, bg = 'NONE'            })  -- Hexadecimal: 0x1A

  -- Strings and Characters
  highlight(0, 'elmString',               { fg = colors.redLight,   bg = 'NONE'            })  -- Double-quoted strings
  highlight(0, 'elmTripleString',         { fg = colors.redLight,   bg = 'NONE'            })  -- Triple-quoted strings (multiline)
  highlight(0, 'elmChar',                 { fg = colors.redLight,   bg = 'NONE'            })  -- Character literals: 'a'
  highlight(0, 'elmStringEscape',         { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences: \n, \t, etc.
  highlight(0, 'elmUnicode',              { fg = colors.pink,       bg = 'NONE'            })  -- Unicode escapes: \u{...}

  -- Comments
  highlight(0, 'elmComment',              { fg = colors.gray,       bg = 'NONE', italic = true })  -- Block comments: {- -}
  highlight(0, 'elmLineComment',          { fg = colors.gray,       bg = 'NONE', italic = true })  -- Line comments: --
  highlight(0, 'elmDocComment',           { fg = colors.gray,       bg = 'NONE', italic = true })  -- Documentation comments: {-| -}
  highlight(0, 'elmTodo',                 { fg = colors.pink,       bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Punctuation and Delimiters
  highlight(0, 'elmDelimiter',            { fg = colors.white,      bg = 'NONE'            })  -- Commas, semicolons
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
  highlight(0, 'elmTupleFunction',        { fg = colors.orange,     bg = 'NONE'            })  -- Tuple constructors: Tuple.pair

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
  highlight(0, 'elmDebugToString',        { fg = colors.orange,     bg = 'NONE', bold = true })  -- Debug.toString

  -- Core Library Functions (commonly used)
  highlight(0, 'elmCoreFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- Core library functions
  highlight(0, 'elmListFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- List.map, List.filter, etc.
  highlight(0, 'elmMaybeFunction',        { fg = colors.orange,     bg = 'NONE'            })  -- Maybe.map, Maybe.withDefault
  highlight(0, 'elmResultFunction',       { fg = colors.orange,     bg = 'NONE'            })  -- Result.map, Result.andThen
  highlight(0, 'elmStringFunction',       { fg = colors.orange,     bg = 'NONE'            })  -- String.concat, String.length
  highlight(0, 'elmDictFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- Dict.get, Dict.insert
  highlight(0, 'elmSetFunction',          { fg = colors.orange,     bg = 'NONE'            })  -- Set.member, Set.insert
  highlight(0, 'elmArrayFunction',        { fg = colors.orange,     bg = 'NONE'            })  -- Array.get, Array.set
  highlight(0, 'elmJsonFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- Json.Decode, Json.Encode
  highlight(0, 'elmHtmlFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- Html.div, Html.text
  highlight(0, 'elmHttpFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- Http.get, Http.post


  -------------------------------------------------------------------------
  -- Treesitter Elm Captures
  -------------------------------------------------------------------------

  -- Comments
  highlight(0, '@comment.elm',                   { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments
  highlight(0, '@comment.documentation.elm',     { fg = colors.gray,       bg = 'NONE', italic = true })  -- Doc comments
  highlight(0, '@spell.elm',                     { link = '@comment.elm'                              })  -- Spell check

  -- Keywords
  highlight(0, '@keyword.elm',                   { fg = colors.blue,       bg = 'NONE'            })  -- Keywords
  highlight(0, '@keyword.conditional.elm',       { fg = colors.blue,       bg = 'NONE'            })  -- if, then, else, case, of
  highlight(0, '@keyword.import.elm',            { fg = colors.blue,       bg = 'NONE'            })  -- module, import, exposing, as

  -- Functions
  highlight(0, '@function.elm',                  { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions
  highlight(0, '@function.call.elm',             { fg = colors.orange,     bg = 'NONE'            })  -- Function calls

  -- Variables
  highlight(0, '@variable.elm',                  { fg = colors.white,      bg = 'NONE'            })  -- Variables
  highlight(0, '@variable.member.elm',           { fg = colors.blue,       bg = 'NONE'            })  -- Record fields
  highlight(0, '@variable.parameter.elm',        { fg = colors.white,      bg = 'NONE'            })  -- Function parameters

  -- Types
  highlight(0, '@type.elm',                      { fg = colors.turquoise,  bg = 'NONE'            })  -- Types
  highlight(0, '@type.definition.elm',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Type definitions
  highlight(0, '@constructor.elm',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Constructors

  -- Modules
  highlight(0, '@module.elm',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names

  -- Properties
  highlight(0, '@property.elm',                  { fg = colors.blue,       bg = 'NONE'            })  -- Record properties

  -- Operators
  highlight(0, '@operator.elm',                  { fg = colors.white,      bg = 'NONE'            })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.elm',       { fg = colors.white,      bg = 'NONE'            })  -- Brackets
  highlight(0, '@punctuation.delimiter.elm',     { fg = colors.white,      bg = 'NONE'            })  -- Delimiters
  highlight(0, '@punctuation.special.elm',       { fg = colors.white,      bg = 'NONE'            })  -- Special punctuation

  -- Literals
  highlight(0, '@number.elm',                    { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, '@boolean.elm',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- Booleans
  highlight(0, '@string.elm',                    { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
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
  highlight(0, '@lsp.type.variable.elm',         { fg = colors.white,      bg = 'NONE'            })  -- Variables
  highlight(0, '@lsp.type.parameter.elm',        { fg = colors.white,      bg = 'NONE'            })  -- Parameters
  highlight(0, '@lsp.type.namespace.elm',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Modules
  highlight(0, '@lsp.type.enum.elm',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Union types
  highlight(0, '@lsp.type.enumMember.elm',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Constructors
  highlight(0, '@lsp.type.string.elm',           { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
  highlight(0, '@lsp.type.number.elm',           { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, '@lsp.type.keyword.elm',          { fg = colors.blue,       bg = 'NONE'            })  -- Keywords
  highlight(0, '@lsp.type.operator.elm',         { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, '@lsp.type.comment.elm',          { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments

  highlight(0, '@lsp.mod.declaration.elm',       { fg = colors.orange,     bg = 'NONE'            })  -- Declarations
  highlight(0, '@lsp.mod.definition.elm',        { fg = colors.orange,     bg = 'NONE'            })  -- Definitions
  highlight(0, '@lsp.mod.readonly.elm',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Read-only
  highlight(0, '@lsp.mod.defaultLibrary.elm',    { fg = colors.turquoise,  bg = 'NONE'            })  -- Standard library

end

return elm
