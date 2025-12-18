-------------------------------------------------------------------------------
-- Lua
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local lua       = {}


--------------------------------------------------------------
-- Settings

lua.setupHighlighting = function()

  ---------------------------------------------------------------
  -- Vim Legacy Syntax Groups
  ---------------------------------------------------------------

  -- Keywords & Statements
  highlight(0, 'luaStatement',            { fg = colors.blue,       bg = 'NONE' })           -- General statements (return, break, etc.)
  highlight(0, 'luaRepeat',               { fg = colors.blue,       bg = 'NONE' })           -- Loop keywords (for, while, repeat, until)
  highlight(0, 'luaFor',                  { fg = colors.blue,       bg = 'NONE' })           -- 'for' keyword
  highlight(0, 'luaWhile',                { fg = colors.blue,       bg = 'NONE' })           -- 'while' keyword
  highlight(0, 'luaCond',                 { fg = colors.blue,       bg = 'NONE' })           -- Conditionals (if, then, else, elseif)
  highlight(0, 'luaCondElse',             { fg = colors.blue,       bg = 'NONE' })           -- 'else' keyword
  highlight(0, 'luaCondElseif',           { fg = colors.blue,       bg = 'NONE' })           -- 'elseif' keyword
  highlight(0, 'luaCondEnd',              { fg = colors.blue,       bg = 'NONE' })           -- 'end' for conditionals
  highlight(0, 'luaCondStart',            { fg = colors.blue,       bg = 'NONE' })           -- Start of conditional
  highlight(0, 'luaFunction',             { fg = colors.blue,       bg = 'NONE' })           -- 'function' keyword
  highlight(0, 'luaFuncKeyword',          { fg = colors.blue,       bg = 'NONE' })           -- 'function' keyword (alternate)
  highlight(0, 'luaLocal',                { fg = colors.blue,       bg = 'NONE' })           -- 'local' keyword
  highlight(0, 'luaIn',                   { fg = colors.blue,       bg = 'NONE' })           -- 'in' keyword
  highlight(0, 'luaThen',                 { fg = colors.blue,       bg = 'NONE' })           -- 'then' keyword
  highlight(0, 'luaDo',                   { fg = colors.blue,       bg = 'NONE' })           -- 'do' keyword
  highlight(0, 'luaEnd',                  { fg = colors.blue,       bg = 'NONE' })           -- 'end' keyword
  highlight(0, 'luaReturn',               { fg = colors.blue,       bg = 'NONE' })           -- 'return' keyword
  highlight(0, 'luaBreak',                { fg = colors.blue,       bg = 'NONE' })           -- 'break' keyword
  highlight(0, 'luaGoto',                 { fg = colors.blue,       bg = 'NONE' })           -- 'goto' keyword
  highlight(0, 'luaLabel',                { fg = colors.yellow,     bg = 'NONE' })           -- Labels (::label::)

  -- Operators
  highlight(0, 'luaOperator',             { fg = colors.blue,       bg = 'NONE' })           -- Word operators (and, or, not)
  highlight(0, 'luaSymbolOperator',       { fg = colors.white,      bg = 'NONE' })           -- Symbol operators (+, -, *, /, etc.)
  highlight(0, 'luaBinaryOperator',       { fg = colors.white,      bg = 'NONE' })           -- Binary operators
  highlight(0, 'luaUnaryOperator',        { fg = colors.white,      bg = 'NONE' })           -- Unary operators
  highlight(0, 'luaRelationalOperator',   { fg = colors.white,      bg = 'NONE' })           -- Relational operators (==, ~=, <, >, etc.)
  highlight(0, 'luaLengthOperator',       { fg = colors.white,      bg = 'NONE' })           -- Length operator (#)
  highlight(0, 'luaConcatOperator',       { fg = colors.white,      bg = 'NONE' })           -- Concatenation (..)
  highlight(0, 'luaNotEqOperator',        { fg = colors.white,      bg = 'NONE' })           -- Not equal (~=)
  highlight(0, 'luaEqOperator',           { fg = colors.white,      bg = 'NONE' })           -- Equality (==)

  -- Strings
  highlight(0, 'luaString',               { fg = colors.redLight,   bg = 'NONE' })           -- Regular strings
  highlight(0, 'luaString2',              { fg = colors.redLight,   bg = 'NONE' })           -- Alternate string style
  highlight(0, 'luaStringDelimiter',      { fg = colors.redLight,   bg = 'NONE' })           -- String delimiters (' and ")
  highlight(0, 'luaStringLong',           { fg = colors.redLight,   bg = 'NONE' })           -- Long strings [[...]]
  highlight(0, 'luaStringLongTag',        { fg = colors.redLight,   bg = 'NONE' })           -- Long string delimiters [[ and ]]
  highlight(0, 'luaStringSpecial',        { fg = colors.purple,     bg = 'NONE' })           -- Escape sequences in strings
  highlight(0, 'luaCharacter',            { fg = colors.redLight,   bg = 'NONE' })           -- Character literals

  -- Numbers
  highlight(0, 'luaNumber',               { fg = colors.greenLight, bg = 'NONE' })           -- Integer numbers
  highlight(0, 'luaFloat',                { fg = colors.greenLight, bg = 'NONE' })           -- Floating point numbers
  highlight(0, 'luaHexNumber',            { fg = colors.greenLight, bg = 'NONE' })           -- Hexadecimal numbers

  -- Constants & Booleans
  highlight(0, 'luaConstant',             { fg = colors.purple,     bg = 'NONE' })           -- Constants (nil, true, false)
  highlight(0, 'luaNil',                  { fg = colors.purple,     bg = 'NONE' })           -- nil value
  highlight(0, 'luaTrue',                 { fg = colors.blue,       bg = 'NONE' })           -- true value
  highlight(0, 'luaFalse',                { fg = colors.blue,       bg = 'NONE' })           -- false value
  highlight(0, 'luaBoolean',              { fg = colors.blue,       bg = 'NONE' })           -- Boolean values

  -- Functions
  highlight(0, 'luaFunc',                 { fg = colors.orange,     bg = 'NONE' })           -- Built-in functions
  highlight(0, 'luaFuncCall',             { fg = colors.orange,     bg = 'NONE' })           -- Function calls
  highlight(0, 'luaFuncName',             { fg = colors.orange,     bg = 'NONE' })           -- Function names in definitions
  highlight(0, 'luaFuncId',               { fg = colors.orange,     bg = 'NONE' })           -- Function identifier
  highlight(0, 'luaFuncSig',              { fg = colors.orange,     bg = 'NONE' })           -- Function signature
  highlight(0, 'luaFuncArgs',             { fg = colors.purple,     bg = 'NONE' })           -- Function arguments
  highlight(0, 'luaFuncArgName',          { fg = colors.purple,     bg = 'NONE' })           -- Function argument names
  highlight(0, 'luaFuncParens',           { fg = colors.white,      bg = 'NONE' })           -- Function parentheses
  highlight(0, 'luaFuncArgComma',         { fg = colors.white,      bg = 'NONE' })           -- Commas between function parameters
  highlight(0, 'luaFuncTable',            { fg = colors.white,      bg = 'NONE' })           -- Table as function argument
  highlight(0, 'luaMetaMethod',           { fg = colors.orange,     bg = 'NONE' })           -- Metamethods (__index, __newindex, etc.)
  highlight(0, 'luaSpecialValue',         { fg = colors.orange,     bg = 'NONE' })           -- Special values (require, etc.)

  -- Tables
  highlight(0, 'luaTable',                { fg = colors.white,      bg = 'NONE' })           -- Tables
  highlight(0, 'luaTableBlock',           { fg = colors.white,      bg = 'NONE' })           -- Table blocks
  highlight(0, 'luaTableConstructor',     { fg = colors.white,      bg = 'NONE' })           -- Table constructor {}
  highlight(0, 'luaTableField',           { fg = colors.white,      bg = 'NONE' })           -- Table fields
  highlight(0, 'luaSpecialTable',         { fg = colors.purple,     bg = 'NONE' })           -- Special tables (_G, _ENV, etc.)

  -- Comments
  highlight(0, 'luaComment',              { fg = colors.red,        bg = 'NONE' })           -- Single-line comments
  highlight(0, 'luaCommentDelimiter',     { fg = colors.red,        bg = 'NONE' })           -- Comment delimiters (--)
  highlight(0, 'luaCommentLong',          { fg = colors.red,        bg = 'NONE' })           -- Long comments --[[...]]
  highlight(0, 'luaCommentLongTag',       { fg = colors.red,        bg = 'NONE' })           -- Long comment delimiters --[[ and ]]
  highlight(0, 'luaCommentTodo',          { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO in comments
  highlight(0, 'luaTodo',                 { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Punctuation & Delimiters
  highlight(0, 'luaParens',               { fg = colors.white,      bg = 'NONE' })           -- Parentheses ()
  highlight(0, 'luaParen',                { fg = colors.white,      bg = 'NONE' })           -- Parentheses (alternate)
  highlight(0, 'luaBraces',               { fg = colors.white,      bg = 'NONE' })           -- Braces {}
  highlight(0, 'luaBracket',              { fg = colors.white,      bg = 'NONE' })           -- Brackets []
  highlight(0, 'luaBrackets',             { fg = colors.white,      bg = 'NONE' })           -- Brackets (alternate)
  highlight(0, 'luaComma',                { fg = colors.white,      bg = 'NONE' })           -- Commas
  highlight(0, 'luaSemiCol',              { fg = colors.white,      bg = 'NONE' })           -- Semicolons
  highlight(0, 'luaNoise',                { fg = colors.white,      bg = 'NONE' })           -- Syntax noise (dots, colons)
  highlight(0, 'luaDot',                  { fg = colors.white,      bg = 'NONE' })           -- Dot operator
  highlight(0, 'luaColon',                { fg = colors.white,      bg = 'NONE' })           -- Colon operator
  highlight(0, 'luaEllipsis',             { fg = colors.purple,     bg = 'NONE' })           -- Vararg (...)

  -- Variables
  highlight(0, 'luaVar',                  { fg = colors.white,      bg = 'NONE' })           -- Variables
  highlight(0, 'luaVarName',              { fg = colors.white,      bg = 'NONE' })           -- Variable names
  highlight(0, 'luaIdentifier',           { fg = colors.white,      bg = 'NONE' })           -- Identifiers
  highlight(0, 'luaSelf',                 { fg = colors.purple,     bg = 'NONE' })           -- self reference
  highlight(0, 'luaBuiltIn',              { fg = colors.purple,     bg = 'NONE' })           -- Built-in variables

  -- Control Flow
  highlight(0, 'luaIfThen',               { fg = colors.blue,       bg = 'NONE' })           -- if-then block
  highlight(0, 'luaThenEnd',              { fg = colors.blue,       bg = 'NONE' })           -- then-end block
  highlight(0, 'luaElseifThen',           { fg = colors.blue,       bg = 'NONE' })           -- elseif-then block
  highlight(0, 'luaElse',                 { fg = colors.blue,       bg = 'NONE' })           -- else block
  highlight(0, 'luaBlock',                { fg = colors.white,      bg = 'NONE' })           -- Code blocks
  highlight(0, 'luaLoop',                 { fg = colors.blue,       bg = 'NONE' })           -- Loop constructs
  highlight(0, 'luaLoopBlock',            { fg = colors.blue,       bg = 'NONE' })           -- Loop blocks
  highlight(0, 'luaGotoLabel',            { fg = colors.yellow,     bg = 'NONE' })           -- Goto labels

  -- Errors
  highlight(0, 'luaError',                { fg = colors.red,        bg = 'NONE' })           -- Errors
  highlight(0, 'luaParenError',           { fg = colors.red,        bg = 'NONE' })           -- Parenthesis errors
  highlight(0, 'luaBraceError',           { fg = colors.red,        bg = 'NONE' })           -- Brace errors
  highlight(0, 'luaCommentError',         { fg = colors.red,        bg = 'NONE' })           -- Comment errors
  highlight(0, 'luaStringError',          { fg = colors.red,        bg = 'NONE' })           -- String errors

  -- Special
  highlight(0, 'luaSpecial',              { fg = colors.purple,     bg = 'NONE' })           -- Special elements
  highlight(0, 'luaShebang',              { fg = colors.red,        bg = 'NONE' })           -- Shebang line

  ---------------------------------------------------------------
  -- Lua Built-in Library Functions
  ---------------------------------------------------------------

  -- Basic Functions
  highlight(0, 'luaFuncPrint',            { fg = colors.orange,     bg = 'NONE' })           -- print
  highlight(0, 'luaFuncType',             { fg = colors.orange,     bg = 'NONE' })           -- type
  highlight(0, 'luaFuncPairs',            { fg = colors.orange,     bg = 'NONE' })           -- pairs
  highlight(0, 'luaFuncIpairs',           { fg = colors.orange,     bg = 'NONE' })           -- ipairs
  highlight(0, 'luaFuncNext',             { fg = colors.orange,     bg = 'NONE' })           -- next
  highlight(0, 'luaFuncSelect',           { fg = colors.orange,     bg = 'NONE' })           -- select
  highlight(0, 'luaFuncError',            { fg = colors.orange,     bg = 'NONE' })           -- error
  highlight(0, 'luaFuncAssert',           { fg = colors.orange,     bg = 'NONE' })           -- assert
  highlight(0, 'luaFuncPcall',            { fg = colors.orange,     bg = 'NONE' })           -- pcall
  highlight(0, 'luaFuncXpcall',           { fg = colors.orange,     bg = 'NONE' })           -- xpcall
  highlight(0, 'luaFuncRequire',          { fg = colors.orange,     bg = 'NONE' })           -- require
  highlight(0, 'luaFuncLoad',             { fg = colors.orange,     bg = 'NONE' })           -- load
  highlight(0, 'luaFuncLoadfile',         { fg = colors.orange,     bg = 'NONE' })           -- loadfile
  highlight(0, 'luaFuncDofile',           { fg = colors.orange,     bg = 'NONE' })           -- dofile
  highlight(0, 'luaFuncTostring',         { fg = colors.orange,     bg = 'NONE' })           -- tostring
  highlight(0, 'luaFuncTonumber',         { fg = colors.orange,     bg = 'NONE' })           -- tonumber
  highlight(0, 'luaFuncRawget',           { fg = colors.orange,     bg = 'NONE' })           -- rawget
  highlight(0, 'luaFuncRawset',           { fg = colors.orange,     bg = 'NONE' })           -- rawset
  highlight(0, 'luaFuncRawequal',         { fg = colors.orange,     bg = 'NONE' })           -- rawequal
  highlight(0, 'luaFuncRawlen',           { fg = colors.orange,     bg = 'NONE' })           -- rawlen
  highlight(0, 'luaFuncSetmetatable',     { fg = colors.orange,     bg = 'NONE' })           -- setmetatable
  highlight(0, 'luaFuncGetmetatable',     { fg = colors.orange,     bg = 'NONE' })           -- getmetatable
  highlight(0, 'luaFuncCollectgarbage',   { fg = colors.orange,     bg = 'NONE' })           -- collectgarbage

  -- Library Namespaces
  highlight(0, 'luaLibraryString',        { fg = colors.turquoise,  bg = 'NONE' })           -- string library
  highlight(0, 'luaLibraryTable',         { fg = colors.turquoise,  bg = 'NONE' })           -- table library
  highlight(0, 'luaLibraryMath',          { fg = colors.turquoise,  bg = 'NONE' })           -- math library
  highlight(0, 'luaLibraryIo',            { fg = colors.turquoise,  bg = 'NONE' })           -- io library
  highlight(0, 'luaLibraryOs',            { fg = colors.turquoise,  bg = 'NONE' })           -- os library
  highlight(0, 'luaLibraryCoroutine',     { fg = colors.turquoise,  bg = 'NONE' })           -- coroutine library
  highlight(0, 'luaLibraryDebug',         { fg = colors.turquoise,  bg = 'NONE' })           -- debug library
  highlight(0, 'luaLibraryPackage',       { fg = colors.turquoise,  bg = 'NONE' })           -- package library
  highlight(0, 'luaLibraryUtf8',          { fg = colors.turquoise,  bg = 'NONE' })           -- utf8 library

  ---------------------------------------------------------------
  -- Treesitter Captures (@xxx.lua)
  ---------------------------------------------------------------

  -- Variables
  highlight(0, '@variable.lua',                     { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@variable.builtin.lua',             { fg = colors.purple,     bg = 'NONE' })   -- self, _G, _ENV
  highlight(0, '@variable.member.lua',              { fg = colors.white,      bg = 'NONE' })   -- Table fields
  highlight(0, '@variable.parameter.lua',           { fg = colors.purple,     bg = 'NONE' })   -- Function parameters
  highlight(0, '@variable.parameter.builtin.lua',   { fg = colors.purple,     bg = 'NONE' })   -- Built-in parameters (...)

  -- Constants
  highlight(0, '@constant.lua',                     { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@constant.builtin.lua',             { fg = colors.purple,     bg = 'NONE' })   -- nil, true, false
  highlight(0, '@boolean.lua',                      { fg = colors.purple,     bg = 'NONE' })   -- true, false

  -- Numbers
  highlight(0, '@number.lua',                       { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@number.float.lua',                 { fg = colors.greenLight, bg = 'NONE' })

  -- Strings
  highlight(0, '@string.lua',                       { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@string.escape.lua',                { fg = colors.purple,     bg = 'NONE' })   -- Escape sequences
  highlight(0, '@string.regexp.lua',                { fg = colors.redLight,   bg = 'NONE' })   -- Patterns
  highlight(0, '@string.special.lua',               { fg = colors.purple,     bg = 'NONE' })   -- Special strings
  highlight(0, '@character.lua',                    { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@character.special.lua',            { fg = colors.purple,     bg = 'NONE' })

  -- Functions
  highlight(0, '@function.lua',                     { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.call.lua',                { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.builtin.lua',             { fg = colors.orange,     bg = 'NONE' })   -- print, pairs, etc.
  highlight(0, '@function.method.lua',              { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.method.call.lua',         { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.macro.lua',               { fg = colors.orange,     bg = 'NONE' })

  -- Types
  highlight(0, '@type.lua',                         { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@type.builtin.lua',                 { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@type.definition.lua',              { fg = colors.turquoise,  bg = 'NONE' })

  -- Keywords
  highlight(0, '@keyword.lua',                      { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.function.lua',             { fg = colors.blue,       bg = 'NONE' })   -- function
  highlight(0, '@keyword.operator.lua',             { fg = colors.blue,       bg = 'NONE' })   -- and, or, not
  highlight(0, '@keyword.return.lua',               { fg = colors.blue,       bg = 'NONE' })   -- return
  highlight(0, '@keyword.conditional.lua',          { fg = colors.blue,       bg = 'NONE' })   -- if, then, else, elseif
  highlight(0, '@keyword.repeat.lua',               { fg = colors.blue,       bg = 'NONE' })   -- for, while, repeat, until
  highlight(0, '@keyword.coroutine.lua',            { fg = colors.blue,       bg = 'NONE' })   -- coroutine keywords
  highlight(0, '@keyword.directive.lua',            { fg = colors.blue,       bg = 'NONE' })   -- directives
  highlight(0, '@keyword.import.lua',               { fg = colors.blue,       bg = 'NONE' })   -- require
  highlight(0, '@keyword.exception.lua',            { fg = colors.blue,       bg = 'NONE' })   -- error handling

  -- Operators
  highlight(0, '@operator.lua',                     { fg = colors.white,      bg = 'NONE' })

  -- Punctuation
  highlight(0, '@punctuation.delimiter.lua',        { fg = colors.white,      bg = 'NONE' })   -- , ; :
  highlight(0, '@punctuation.bracket.lua',          { fg = colors.white,      bg = 'NONE' })   -- () [] {}
  highlight(0, '@punctuation.special.lua',          { fg = colors.white,      bg = 'NONE' })   -- Special punctuation

  -- Constructors & Properties
  highlight(0, '@constructor.lua',                  { fg = colors.white,      bg = 'NONE' })   -- Table constructors {}
  highlight(0, '@property.lua',                     { fg = colors.white,      bg = 'NONE' })   -- Table keys
  highlight(0, '@field.lua',                        { fg = colors.white,      bg = 'NONE' })   -- Table fields

  -- Labels
  highlight(0, '@label.lua',                        { fg = colors.yellow,     bg = 'NONE' })   -- ::label::

  -- Attributes
  highlight(0, '@attribute.lua',                    { fg = colors.purple,     bg = 'NONE' })   -- Attributes <const>, <close>

  -- Modules
  highlight(0, '@module.lua',                       { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@module.builtin.lua',               { fg = colors.purple,     bg = 'NONE' })   -- _G, _ENV

  -- Comments
  highlight(0, '@comment.lua',                      { fg = colors.red,        bg = 'NONE' })
  highlight(0, '@comment.documentation.lua',        { fg = colors.red,        bg = 'NONE' })
  highlight(0, '@comment.todo.lua',                 { fg = colors.red,        bg = 'NONE', bold = true })
  highlight(0, '@comment.note.lua',                 { fg = colors.turquoise,  bg = 'NONE', bold = true })
  highlight(0, '@comment.warning.lua',              { fg = colors.yellow,     bg = 'NONE', bold = true })
  highlight(0, '@comment.error.lua',                { fg = colors.red,        bg = 'NONE', bold = true })

  -- Misc
  highlight(0, '@none.lua',                         { fg = 'NONE',            bg = 'NONE' })
  highlight(0, '@conceal.lua',                      { fg = colors.grey,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- LSP Semantic Tokens (lua-language-server)
  ---------------------------------------------------------------

  -- Token Types
  highlight(0, '@lsp.type.keyword.lua',             { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.variable.lua',            { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.function.lua',            { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.method.lua',              { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.property.lua',            { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.parameter.lua',           { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.class.lua',               { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.type.lua',                { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.macro.lua',               { fg = colors.turquoise,  bg = 'NONE' })   -- Aliases, enums
  highlight(0, '@lsp.type.string.lua',              { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@lsp.type.number.lua',              { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@lsp.type.operator.lua',            { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.struct.lua',              { fg = colors.yellow,     bg = 'NONE' })   -- Labels, goto targets
  highlight(0, '@lsp.type.comment.lua',             { fg = colors.red,        bg = 'NONE' })
  highlight(0, '@lsp.type.enumMember.lua',          { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.event.lua',               { fg = colors.yellow,     bg = 'NONE' })   -- Diagnostic names
  highlight(0, '@lsp.type.decorator.lua',           { fg = colors.purple,     bg = 'NONE' })   -- Attributes
  highlight(0, '@lsp.type.namespace.lua',           { fg = colors.turquoise,  bg = 'NONE' })   -- Namespaces, modules

  -- Token Modifiers
  highlight(0, '@lsp.mod.declaration.lua',          { fg = 'NONE',            bg = 'NONE' })                    -- Declaration sites
  highlight(0, '@lsp.mod.definition.lua',           { fg = 'NONE',            bg = 'NONE' })                    -- Definition sites
  highlight(0, '@lsp.mod.readonly.lua',             { fg = 'NONE',            bg = 'NONE', italic = true })     -- Immutable values
  highlight(0, '@lsp.mod.defaultLibrary.lua',       { fg = 'NONE',            bg = 'NONE' })                    -- Standard library
  highlight(0, '@lsp.mod.global.lua',               { fg = 'NONE',            bg = 'NONE' })                    -- Global scope
  highlight(0, '@lsp.mod.abstract.lua',             { fg = 'NONE',            bg = 'NONE', italic = true })     -- Abstract/close
  highlight(0, '@lsp.mod.static.lua',               { fg = 'NONE',            bg = 'NONE', italic = true })     -- Static values
  highlight(0, '@lsp.mod.modification.lua',         { fg = 'NONE',            bg = 'NONE' })                    -- Modifications
  highlight(0, '@lsp.mod.async.lua',                { fg = 'NONE',            bg = 'NONE', italic = true })     -- Async functions
  highlight(0, '@lsp.mod.documentation.lua',        { fg = 'NONE',            bg = 'NONE' })                    -- Documentation
  highlight(0, '@lsp.mod.deprecated.lua',           { fg = 'NONE',            bg = 'NONE', strikethrough = true })

  -- Combined Type + Modifier
  highlight(0, '@lsp.typemod.variable.global.lua',          { fg = colors.purple,     bg = 'NONE' })            -- Global variables
  highlight(0, '@lsp.typemod.variable.defaultLibrary.lua',  { fg = colors.purple,     bg = 'NONE' })            -- Built-in variables
  highlight(0, '@lsp.typemod.variable.readonly.lua',        { fg = colors.purple,     bg = 'NONE', italic = true })
  highlight(0, '@lsp.typemod.function.defaultLibrary.lua',  { fg = colors.orange,     bg = 'NONE' })            -- Built-in functions
  highlight(0, '@lsp.typemod.function.global.lua',          { fg = colors.orange,     bg = 'NONE' })            -- Global functions
  highlight(0, '@lsp.typemod.function.declaration.lua',     { fg = colors.orange,     bg = 'NONE' })            -- Function declarations
  highlight(0, '@lsp.typemod.function.definition.lua',      { fg = colors.orange,     bg = 'NONE' })            -- Function definitions
  highlight(0, '@lsp.typemod.method.defaultLibrary.lua',    { fg = colors.orange,     bg = 'NONE' })            -- Built-in methods
  highlight(0, '@lsp.typemod.parameter.declaration.lua',    { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.typemod.property.declaration.lua',     { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.typemod.namespace.defaultLibrary.lua', { fg = colors.turquoise,  bg = 'NONE' })            -- Built-in modules

  ---------------------------------------------------------------
  -- LuaDoc / EmmyLua Annotations
  ---------------------------------------------------------------

  -- Comment Doc Markers
  highlight(0, 'luaDocTag',                         { fg = colors.turquoise,  bg = 'NONE' })           -- General doc tag
  highlight(0, 'luaDocTagType',                     { fg = colors.turquoise,  bg = 'NONE' })           -- Type tags
  highlight(0, 'luaDocKeyword',                     { fg = colors.turquoise,  bg = 'NONE' })           -- Doc keywords
  highlight(0, 'luaDocDelimiter',                   { fg = colors.white,      bg = 'NONE' })           -- Doc delimiters

  -- Type Annotations
  highlight(0, '@lsp.type.type.luadoc',             { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@tag.luadoc',                       { fg = colors.turquoise,  bg = 'NONE' })           -- @tags
  highlight(0, '@keyword.luadoc',                   { fg = colors.turquoise,  bg = 'NONE' })           -- Doc keywords
  highlight(0, '@type.luadoc',                      { fg = colors.turquoise,  bg = 'NONE' })           -- Type names
  highlight(0, '@variable.luadoc',                  { fg = colors.purple,     bg = 'NONE' })           -- Parameter names
  highlight(0, '@parameter.luadoc',                 { fg = colors.purple,     bg = 'NONE' })           -- Parameters

  -- Specific Annotation Tags
  highlight(0, 'luaDocClass',                       { fg = colors.turquoise,  bg = 'NONE', bold = true })   -- @class
  highlight(0, 'luaDocField',                       { fg = colors.turquoise,  bg = 'NONE' })                 -- @field
  highlight(0, 'luaDocType',                        { fg = colors.turquoise,  bg = 'NONE' })                 -- @type
  highlight(0, 'luaDocAlias',                       { fg = colors.turquoise,  bg = 'NONE' })                 -- @alias
  highlight(0, 'luaDocEnum',                        { fg = colors.turquoise,  bg = 'NONE' })                 -- @enum
  highlight(0, 'luaDocGeneric',                     { fg = colors.turquoise,  bg = 'NONE' })                 -- @generic
  highlight(0, 'luaDocParam',                       { fg = colors.turquoise,  bg = 'NONE' })                 -- @param
  highlight(0, 'luaDocParamName',                   { fg = colors.purple,     bg = 'NONE' })                 -- Parameter name
  highlight(0, 'luaDocParamType',                   { fg = colors.turquoise,  bg = 'NONE' })                 -- Parameter type
  highlight(0, 'luaDocReturn',                      { fg = colors.turquoise,  bg = 'NONE' })                 -- @return
  highlight(0, 'luaDocReturnType',                  { fg = colors.turquoise,  bg = 'NONE' })                 -- Return type
  highlight(0, 'luaDocVararg',                      { fg = colors.turquoise,  bg = 'NONE' })                 -- @vararg
  highlight(0, 'luaDocOverload',                    { fg = colors.turquoise,  bg = 'NONE' })                 -- @overload
  highlight(0, 'luaDocAsync',                       { fg = colors.turquoise,  bg = 'NONE', italic = true })  -- @async
  highlight(0, 'luaDocOperator',                    { fg = colors.turquoise,  bg = 'NONE' })                 -- @operator
  highlight(0, 'luaDocPrivate',                     { fg = colors.turquoise,  bg = 'NONE', italic = true })  -- @private
  highlight(0, 'luaDocProtected',                   { fg = colors.turquoise,  bg = 'NONE', italic = true })  -- @protected
  highlight(0, 'luaDocPackage',                     { fg = colors.turquoise,  bg = 'NONE', italic = true })  -- @package
  highlight(0, 'luaDocDeprecated',                  { fg = colors.turquoise,  bg = 'NONE', strikethrough = true })  -- @deprecated
  highlight(0, 'luaDocNodiscard',                   { fg = colors.turquoise,  bg = 'NONE' })                 -- @nodiscard
  highlight(0, 'luaDocVersion',                     { fg = colors.turquoise,  bg = 'NONE' })                 -- @version
  highlight(0, 'luaDocCast',                        { fg = colors.turquoise,  bg = 'NONE' })                 -- @cast
  highlight(0, 'luaDocAs',                          { fg = colors.turquoise,  bg = 'NONE' })                 -- @as
  highlight(0, 'luaDocDiagnostic',                  { fg = colors.turquoise,  bg = 'NONE' })                 -- @diagnostic
  highlight(0, 'luaDocSee',                         { fg = colors.turquoise,  bg = 'NONE' })                 -- @see
  highlight(0, 'luaDocSource',                      { fg = colors.turquoise,  bg = 'NONE' })                 -- @source
  highlight(0, 'luaDocModule',                      { fg = colors.turquoise,  bg = 'NONE' })                 -- @module
  highlight(0, 'luaDocMeta',                        { fg = colors.turquoise,  bg = 'NONE', italic = true })  -- @meta

  -- Type Syntax in Annotations
  highlight(0, 'luaDocTypeTable',                   { fg = colors.turquoise,  bg = 'NONE' })                 -- table<K, V>
  highlight(0, 'luaDocTypeFunction',                { fg = colors.turquoise,  bg = 'NONE' })                 -- fun(...): ...
  highlight(0, 'luaDocTypeArray',                   { fg = colors.turquoise,  bg = 'NONE' })                 -- type[]
  highlight(0, 'luaDocTypeUnion',                   { fg = colors.turquoise,  bg = 'NONE' })                 -- type1|type2
  highlight(0, 'luaDocTypeOptional',                { fg = colors.turquoise,  bg = 'NONE' })                 -- type?
  highlight(0, 'luaDocTypeLiteral',                 { fg = colors.redLight,   bg = 'NONE' })                 -- "literal"
  highlight(0, 'luaDocTypeBuiltin',                 { fg = colors.turquoise,  bg = 'NONE' })                 -- string, number, etc.
  highlight(0, 'luaDocTypeNil',                     { fg = colors.purple,     bg = 'NONE' })                 -- nil
  highlight(0, 'luaDocTypeBoolean',                 { fg = colors.purple,     bg = 'NONE' })                 -- boolean
  highlight(0, 'luaDocTypeNumber',                  { fg = colors.greenLight, bg = 'NONE' })                 -- number, integer
  highlight(0, 'luaDocTypeString',                  { fg = colors.redLight,   bg = 'NONE' })                 -- string
  highlight(0, 'luaDocTypeAny',                     { fg = colors.turquoise,  bg = 'NONE', italic = true })  -- any
  highlight(0, 'luaDocTypeUnknown',                 { fg = colors.turquoise,  bg = 'NONE', italic = true })  -- unknown
  highlight(0, 'luaDocTypeSelf',                    { fg = colors.purple,     bg = 'NONE' })                 -- self

  ---------------------------------------------------------------
  -- Neovim-Specific Lua API
  ---------------------------------------------------------------

  -- vim Namespace
  highlight(0, 'luaVimApi',                         { fg = colors.purple,     bg = 'NONE' })           -- vim
  highlight(0, 'luaVimApiFunc',                     { fg = colors.orange,     bg = 'NONE' })           -- vim.api functions
  highlight(0, 'luaVimApiMethod',                   { fg = colors.orange,     bg = 'NONE' })           -- vim.api methods
  highlight(0, 'luaVimFn',                          { fg = colors.orange,     bg = 'NONE' })           -- vim.fn functions
  highlight(0, 'luaVimCmd',                         { fg = colors.orange,     bg = 'NONE' })           -- vim.cmd
  highlight(0, 'luaVimOpt',                         { fg = colors.white,      bg = 'NONE' })           -- vim.opt
  highlight(0, 'luaVimG',                           { fg = colors.purple,     bg = 'NONE' })           -- vim.g (globals)
  highlight(0, 'luaVimB',                           { fg = colors.purple,     bg = 'NONE' })           -- vim.b (buffer)
  highlight(0, 'luaVimW',                           { fg = colors.purple,     bg = 'NONE' })           -- vim.w (window)
  highlight(0, 'luaVimT',                           { fg = colors.purple,     bg = 'NONE' })           -- vim.t (tabpage)
  highlight(0, 'luaVimO',                           { fg = colors.white,      bg = 'NONE' })           -- vim.o (options)
  highlight(0, 'luaVimBo',                          { fg = colors.white,      bg = 'NONE' })           -- vim.bo (buffer options)
  highlight(0, 'luaVimWo',                          { fg = colors.white,      bg = 'NONE' })           -- vim.wo (window options)

  -- vim Sub-modules
  highlight(0, 'luaVimKeymap',                      { fg = colors.turquoise,  bg = 'NONE' })           -- vim.keymap
  highlight(0, 'luaVimLsp',                         { fg = colors.turquoise,  bg = 'NONE' })           -- vim.lsp
  highlight(0, 'luaVimTreesitter',                  { fg = colors.turquoise,  bg = 'NONE' })           -- vim.treesitter
  highlight(0, 'luaVimDiagnostic',                  { fg = colors.turquoise,  bg = 'NONE' })           -- vim.diagnostic
  highlight(0, 'luaVimUi',                          { fg = colors.turquoise,  bg = 'NONE' })           -- vim.ui
  highlight(0, 'luaVimFs',                          { fg = colors.turquoise,  bg = 'NONE' })           -- vim.fs
  highlight(0, 'luaVimHealth',                      { fg = colors.turquoise,  bg = 'NONE' })           -- vim.health
  highlight(0, 'luaVimJson',                        { fg = colors.turquoise,  bg = 'NONE' })           -- vim.json
  highlight(0, 'luaVimRegex',                       { fg = colors.turquoise,  bg = 'NONE' })           -- vim.regex
  highlight(0, 'luaVimSpell',                       { fg = colors.turquoise,  bg = 'NONE' })           -- vim.spell
  highlight(0, 'luaVimVersion',                     { fg = colors.turquoise,  bg = 'NONE' })           -- vim.version
  highlight(0, 'luaVimText',                        { fg = colors.turquoise,  bg = 'NONE' })           -- vim.text
  highlight(0, 'luaVimIter',                        { fg = colors.turquoise,  bg = 'NONE' })           -- vim.iter
  highlight(0, 'luaVimLpeg',                        { fg = colors.turquoise,  bg = 'NONE' })           -- vim.lpeg
  highlight(0, 'luaVimRe',                          { fg = colors.turquoise,  bg = 'NONE' })           -- vim.re
  highlight(0, 'luaVimMpack',                       { fg = colors.turquoise,  bg = 'NONE' })           -- vim.mpack
  highlight(0, 'luaVimHighlight',                   { fg = colors.turquoise,  bg = 'NONE' })           -- vim.highlight

  -- vim Utility Functions
  highlight(0, 'luaVimSchedule',                    { fg = colors.orange,     bg = 'NONE' })           -- vim.schedule
  highlight(0, 'luaVimDefer',                       { fg = colors.orange,     bg = 'NONE' })           -- vim.defer_fn
  highlight(0, 'luaVimLoop',                        { fg = colors.turquoise,  bg = 'NONE' })           -- vim.loop / vim.uv
  highlight(0, 'luaVimNotify',                      { fg = colors.orange,     bg = 'NONE' })           -- vim.notify
  highlight(0, 'luaVimInspect',                     { fg = colors.orange,     bg = 'NONE' })           -- vim.inspect
  highlight(0, 'luaVimPrettyPrint',                 { fg = colors.orange,     bg = 'NONE' })           -- vim.print
  highlight(0, 'luaVimDeepequal',                   { fg = colors.orange,     bg = 'NONE' })           -- vim.deep_equal
  highlight(0, 'luaVimDeepcopy',                    { fg = colors.orange,     bg = 'NONE' })           -- vim.deepcopy
  highlight(0, 'luaVimTblDeepExtend',               { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_deep_extend

  -- vim.tbl_* Functions
  highlight(0, 'luaVimTbl',                         { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_* family
  highlight(0, 'luaVimTblContains',                 { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_contains
  highlight(0, 'luaVimTblCount',                    { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_count
  highlight(0, 'luaVimTblExtend',                   { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_extend
  highlight(0, 'luaVimTblFilter',                   { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_filter
  highlight(0, 'luaVimTblFlatten',                  { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_flatten
  highlight(0, 'luaVimTblGet',                      { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_get
  highlight(0, 'luaVimTblIsarray',                  { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_isarray
  highlight(0, 'luaVimTblIsempty',                  { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_isempty
  highlight(0, 'luaVimTblIslist',                   { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_islist
  highlight(0, 'luaVimTblKeys',                     { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_keys
  highlight(0, 'luaVimTblMap',                      { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_map
  highlight(0, 'luaVimTblValues',                   { fg = colors.orange,     bg = 'NONE' })           -- vim.tbl_values

  -- vim.list_* Functions
  highlight(0, 'luaVimList',                        { fg = colors.orange,     bg = 'NONE' })           -- vim.list_* family
  highlight(0, 'luaVimListContains',                { fg = colors.orange,     bg = 'NONE' })           -- vim.list_contains
  highlight(0, 'luaVimListExtend',                  { fg = colors.orange,     bg = 'NONE' })           -- vim.list_extend
  highlight(0, 'luaVimListSlice',                   { fg = colors.orange,     bg = 'NONE' })           -- vim.list_slice

  -- vim String/Misc Functions
  highlight(0, 'luaVimSplit',                       { fg = colors.orange,     bg = 'NONE' })           -- vim.split
  highlight(0, 'luaVimGsplit',                      { fg = colors.orange,     bg = 'NONE' })           -- vim.gsplit
  highlight(0, 'luaVimTrim',                        { fg = colors.orange,     bg = 'NONE' })           -- vim.trim
  highlight(0, 'luaVimStartswith',                  { fg = colors.orange,     bg = 'NONE' })           -- vim.startswith
  highlight(0, 'luaVimEndswith',                    { fg = colors.orange,     bg = 'NONE' })           -- vim.endswith
  highlight(0, 'luaVimPesc',                        { fg = colors.orange,     bg = 'NONE' })           -- vim.pesc
  highlight(0, 'luaVimValidate',                    { fg = colors.orange,     bg = 'NONE' })           -- vim.validate
  highlight(0, 'luaVimIsCallable',                  { fg = colors.orange,     bg = 'NONE' })           -- vim.is_callable
  highlight(0, 'luaVimIsThread',                    { fg = colors.orange,     bg = 'NONE' })           -- vim.is_thread

  -- nvim_* API Functions
  highlight(0, 'luaNvimBufGetName',                 { fg = colors.orange,     bg = 'NONE' })           -- nvim_buf_get_name
  highlight(0, 'luaNvimBufSetLines',                { fg = colors.orange,     bg = 'NONE' })           -- nvim_buf_set_lines
  highlight(0, 'luaNvimBufGetLines',                { fg = colors.orange,     bg = 'NONE' })           -- nvim_buf_get_lines
  highlight(0, 'luaNvimSetHl',                      { fg = colors.orange,     bg = 'NONE' })           -- nvim_set_hl
  highlight(0, 'luaNvimGetHl',                      { fg = colors.orange,     bg = 'NONE' })           -- nvim_get_hl
  highlight(0, 'luaNvimCreateAutocmd',              { fg = colors.orange,     bg = 'NONE' })           -- nvim_create_autocmd
  highlight(0, 'luaNvimCreateAugroup',              { fg = colors.orange,     bg = 'NONE' })           -- nvim_create_augroup
  highlight(0, 'luaNvimCreateUserCommand',          { fg = colors.orange,     bg = 'NONE' })           -- nvim_create_user_command
  highlight(0, 'luaNvimSetKeymap',                  { fg = colors.orange,     bg = 'NONE' })           -- nvim_set_keymap
  highlight(0, 'luaNvimBufSetKeymap',               { fg = colors.orange,     bg = 'NONE' })           -- nvim_buf_set_keymap
  highlight(0, 'luaNvimGetCurrentBuf',              { fg = colors.orange,     bg = 'NONE' })           -- nvim_get_current_buf
  highlight(0, 'luaNvimGetCurrentWin',              { fg = colors.orange,     bg = 'NONE' })           -- nvim_get_current_win
  highlight(0, 'luaNvimGetCursorPos',               { fg = colors.orange,     bg = 'NONE' })           -- nvim_win_get_cursor
  highlight(0, 'luaNvimSetCursorPos',               { fg = colors.orange,     bg = 'NONE' })           -- nvim_win_set_cursor
  highlight(0, 'luaNvimExecAutocmds',               { fg = colors.orange,     bg = 'NONE' })           -- nvim_exec_autocmds
  highlight(0, 'luaNvimNotify',                     { fg = colors.orange,     bg = 'NONE' })           -- nvim_notify
  highlight(0, 'luaNvimErr',                        { fg = colors.orange,     bg = 'NONE' })           -- nvim_err_writeln
  highlight(0, 'luaNvimEcho',                       { fg = colors.orange,     bg = 'NONE' })           -- nvim_echo

  ---------------------------------------------------------------
  -- Luau (Roblox) Extensions
  ---------------------------------------------------------------

  highlight(0, 'luauType',                          { fg = colors.turquoise,  bg = 'NONE' })           -- Type annotations
  highlight(0, 'luauTypeBuiltin',                   { fg = colors.turquoise,  bg = 'NONE' })           -- Built-in types
  highlight(0, 'luauTypePrimitive',                 { fg = colors.turquoise,  bg = 'NONE' })           -- Primitive types
  highlight(0, 'luauTypeof',                        { fg = colors.blue,       bg = 'NONE' })           -- typeof keyword
  highlight(0, 'luauExport',                        { fg = colors.blue,       bg = 'NONE' })           -- export keyword
  highlight(0, 'luauContinue',                      { fg = colors.blue,       bg = 'NONE' })           -- continue keyword
  highlight(0, 'luauTypeAssertion',                 { fg = colors.turquoise,  bg = 'NONE' })           -- :: type assertion
  highlight(0, 'luauTypeFunction',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Function type
  highlight(0, 'luauTypeTable',                     { fg = colors.turquoise,  bg = 'NONE' })           -- Table type
  highlight(0, 'luauTypeUnion',                     { fg = colors.turquoise,  bg = 'NONE' })           -- Union type |
  highlight(0, 'luauTypeIntersection',              { fg = colors.turquoise,  bg = 'NONE' })           -- Intersection type &
  highlight(0, 'luauTypeOptional',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Optional type ?
  highlight(0, 'luauTypeGeneric',                   { fg = colors.turquoise,  bg = 'NONE' })           -- Generic type <T>
  highlight(0, 'luauTypePack',                      { fg = colors.turquoise,  bg = 'NONE' })           -- Type pack ...T

  -- Roblox Globals
  highlight(0, 'luauRobloxGlobal',                  { fg = colors.purple,     bg = 'NONE' })           -- Roblox globals
  highlight(0, 'luauGame',                          { fg = colors.purple,     bg = 'NONE' })           -- game
  highlight(0, 'luauWorkspace',                     { fg = colors.purple,     bg = 'NONE' })           -- workspace
  highlight(0, 'luauScript',                        { fg = colors.purple,     bg = 'NONE' })           -- script
  highlight(0, 'luauInstance',                      { fg = colors.turquoise,  bg = 'NONE' })           -- Instance
  highlight(0, 'luauVector3',                       { fg = colors.turquoise,  bg = 'NONE' })           -- Vector3
  highlight(0, 'luauCFrame',                        { fg = colors.turquoise,  bg = 'NONE' })           -- CFrame
  highlight(0, 'luauColor3',                        { fg = colors.turquoise,  bg = 'NONE' })           -- Color3
  highlight(0, 'luauUDim2',                         { fg = colors.turquoise,  bg = 'NONE' })           -- UDim2

  ---------------------------------------------------------------
  -- Plugin-Specific Groups
  ---------------------------------------------------------------

  -- nvim-cmp
  highlight(0, 'CmpItemKindLuaKeyword',             { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'CmpItemKindLuaFunction',            { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'CmpItemKindLuaVariable',            { fg = colors.white,      bg = 'NONE' })

  -- telescope
  highlight(0, 'TelescopeLuaEntry',                 { fg = colors.white,      bg = 'NONE' })

  -- nvim-dap (debugging)
  highlight(0, 'DapLuaBreakpoint',                  { fg = colors.red,        bg = 'NONE' })
  highlight(0, 'DapLuaLogpoint',                    { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'DapLuaConditional',                 { fg = colors.yellow,     bg = 'NONE' })

  ---------------------------------------------------------------
  -- LSP Inlay Hints for Lua
  ---------------------------------------------------------------

  highlight(0, 'LspInlayHintLuaType',               { fg = colors.grey,       bg = 'NONE', italic = true })
  highlight(0, 'LspInlayHintLuaParameter',          { fg = colors.grey,       bg = 'NONE', italic = true })

end

return lua
