-------------------------------------------------------------------------------
-- C Files
-- Highlighting for .c, .h files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local c       = {}


-------------------------------------------------------------------------------
-- Settings

c.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'cStatement',          { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, goto
  highlight(0, 'cConditional',        { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch
  highlight(0, 'cRepeat',             { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'cLabel',              { fg = colors.blue,       bg = 'NONE'            })  -- case, default
  highlight(0, 'cOperator',           { fg = colors.blue,       bg = 'NONE'            })  -- sizeof, typeof, alignof
  highlight(0, 'cStorageClass',       { fg = colors.blue,       bg = 'NONE'            })  -- static, extern, register, auto, volatile, const, restrict, inline
  highlight(0, 'cStructure',          { fg = colors.blue,       bg = 'NONE'            })  -- struct, union, enum, typedef

  -- Types
  highlight(0, 'cType',               { fg = colors.turquoise,  bg = 'NONE'            })  -- int, char, float, double, void, short, long, signed, unsigned
  highlight(0, 'cTypedef',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Typedef'd types
  highlight(0, 'cStructName',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Struct names
  highlight(0, 'cEnumName',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum names
  highlight(0, 'cUnionName',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Union names

  -- C99/C11/C23 Types
  highlight(0, 'cType99',             { fg = colors.turquoise,  bg = 'NONE'            })  -- _Bool, _Complex, _Imaginary
  highlight(0, 'cType11',             { fg = colors.turquoise,  bg = 'NONE'            })  -- _Alignas, _Alignof, _Atomic, _Generic, _Noreturn, _Static_assert, _Thread_local

  -- Constants
  highlight(0, 'cConstant',           { fg = colors.blue,       bg = 'NONE'            })  -- NULL, true, false, __FILE__, __LINE__, __func__
  highlight(0, 'cBoolean',            { fg = colors.blue,       bg = 'NONE'            })  -- true, false
  highlight(0, 'cNull',               { fg = colors.blue,       bg = 'NONE'            })  -- NULL

  -- Functions
  highlight(0, 'cFunction',           { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'cUserFunction',       { fg = colors.orange,     bg = 'NONE'            })  -- User-defined functions

  -- Variables
  highlight(0, 'cIdentifier',         { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'cParameter',          { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'cMember',             { fg = colors.purple,     bg = 'NONE'            })  -- Struct/union members

  -- Preprocessor
  highlight(0, 'cPreProc',            { fg = colors.pink,       bg = 'NONE'            })  -- General preprocessor
  highlight(0, 'cInclude',            { fg = colors.pink,       bg = 'NONE'            })  -- #include
  highlight(0, 'cDefine',             { fg = colors.pink,       bg = 'NONE'            })  -- #define
  highlight(0, 'cMacro',              { fg = colors.pink,       bg = 'NONE'            })  -- Macro names
  highlight(0, 'cPreCondit',          { fg = colors.pink,       bg = 'NONE'            })  -- #if, #ifdef, #ifndef, #else, #elif, #endif
  highlight(0, 'cIncluded',           { fg = colors.redLight,   bg = 'NONE'            })  -- <header.h> or "header.h"
  highlight(0, 'cPreProcDefine',      { fg = colors.pink,       bg = 'NONE'            })  -- #define value
  highlight(0, 'cUndef',              { fg = colors.pink,       bg = 'NONE'            })  -- #undef
  highlight(0, 'cPragma',             { fg = colors.pink,       bg = 'NONE'            })  -- #pragma
  highlight(0, 'cError',              { fg = colors.pink,       bg = 'NONE'            })  -- #error
  highlight(0, 'cWarning',            { fg = colors.pink,       bg = 'NONE'            })  -- #warning

  -- Strings
  highlight(0, 'cString',             { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'cCharacter',          { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'cSpecialChar',        { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, \0, etc.
  highlight(0, 'cFormat',             { fg = colors.pink,       bg = 'NONE'            })  -- %d, %s, %f, etc. in printf

  -- Numbers
  highlight(0, 'cNumber',             { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'cInteger',            { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'cFloat',              { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'cOctal',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0777 octal
  highlight(0, 'cHex',                { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF hex
  highlight(0, 'cBinary',             { fg = colors.greenLight, bg = 'NONE'            })  -- 0b1010 binary (C23)

  -- Operators
  highlight(0, 'cOperatorSign',       { fg = colors.white,      bg = 'NONE'            })  -- + - * / % = < > ! & | ^ ~ ? :
  highlight(0, 'cPointerOperator',    { fg = colors.white,      bg = 'NONE'            })  -- * & pointer operators
  highlight(0, 'cMemberAccess',       { fg = colors.white,      bg = 'NONE'            })  -- . -> member access
  highlight(0, 'cArrow',              { fg = colors.white,      bg = 'NONE'            })  -- ->

  -- Comments
  highlight(0, 'cComment',            { fg = colors.red,        bg = 'NONE'            })  -- /* */ and // comments
  highlight(0, 'cCommentL',           { fg = colors.red,        bg = 'NONE'            })  -- // line comments
  highlight(0, 'cCommentStart',       { fg = colors.red,        bg = 'NONE'            })  -- /* start
  highlight(0, 'cTodo',               { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX, etc.

  -- Errors
  highlight(0, 'cSpaceError',         { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Space errors
  highlight(0, 'cBadContinuation',    { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Bad line continuation

  -- Special
  highlight(0, 'cSpecial',            { fg = colors.pink,       bg = 'NONE'            })  -- Special characters
  highlight(0, 'cParenError',         { fg = colors.red,        bg = 'NONE'            })  -- Parenthesis errors
  highlight(0, 'cBracketError',       { fg = colors.red,        bg = 'NONE'            })  -- Bracket errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.c)

  -- Variables
  highlight(0, '@variable.c',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.c',      { fg = colors.blue,      bg = 'NONE' })  -- Built-in variables
  highlight(0, '@variable.parameter.c',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.c',       { fg = colors.purple,    bg = 'NONE' })  -- Struct/union members

  -- Constants
  highlight(0, '@constant.c',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.c',      { fg = colors.blue,      bg = 'NONE' })  -- NULL, true, false
  highlight(0, '@constant.macro.c',        { fg = colors.pink,      bg = 'NONE' })  -- Macro constants

  -- Functions
  highlight(0, '@function.c',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.c',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.c',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.macro.c',        { fg = colors.pink,      bg = 'NONE' })  -- Macro functions
  highlight(0, '@constructor.c',           { fg = colors.turquoise, bg = 'NONE' })  -- Compound literals

  -- Types
  highlight(0, '@type.c',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.c',          { fg = colors.turquoise, bg = 'NONE' })  -- int, char, float, etc.
  highlight(0, '@type.definition.c',       { fg = colors.turquoise, bg = 'NONE' })  -- typedef definitions
  highlight(0, '@type.qualifier.c',        { fg = colors.blue,      bg = 'NONE' })  -- const, volatile, restrict

  -- Keywords
  highlight(0, '@keyword.c',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.c',      { fg = colors.blue,      bg = 'NONE' })  -- (not really used in C)
  highlight(0, '@keyword.type.c',          { fg = colors.blue,      bg = 'NONE' })  -- struct, union, enum, typedef
  highlight(0, '@keyword.modifier.c',      { fg = colors.blue,      bg = 'NONE' })  -- static, extern, const, volatile
  highlight(0, '@keyword.return.c',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.repeat.c',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, do
  highlight(0, '@keyword.conditional.c',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, switch, case, default
  highlight(0, '@keyword.operator.c',      { fg = colors.blue,      bg = 'NONE' })  -- sizeof, typeof, alignof
  highlight(0, '@keyword.directive.c',     { fg = colors.pink,      bg = 'NONE' })  -- Preprocessor directives

  -- Preprocessor
  highlight(0, '@preproc.c',               { fg = colors.pink,      bg = 'NONE' })  -- Preprocessor directives
  highlight(0, '@include.c',               { fg = colors.pink,      bg = 'NONE' })  -- #include
  highlight(0, '@define.c',                { fg = colors.pink,      bg = 'NONE' })  -- #define

  -- Strings
  highlight(0, '@string.c',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.c',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@character.c',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.c',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.c',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.c',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.c',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.c', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Labels
  highlight(0, '@label.c',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels for goto
  highlight(0, '@property.c',              { fg = colors.purple,    bg = 'NONE' })  -- Struct/union members

  -- Operators and Punctuation
  highlight(0, '@operator.c',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.c',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.c', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.c',   { fg = colors.pink,      bg = 'NONE' })  -- # in preprocessor


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.c)

  highlight(0, '@lsp.type.variable.c',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.c',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.c',      { fg = colors.purple,    bg = 'NONE' })  -- Struct members
  highlight(0, '@lsp.type.function.c',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.macro.c',         { fg = colors.pink,      bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.type.c',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.struct.c',        { fg = colors.turquoise, bg = 'NONE' })  -- Structs
  highlight(0, '@lsp.type.enum.c',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.c',    { fg = colors.purple,    bg = 'NONE' })  -- Enum constants
  highlight(0, '@lsp.type.namespace.c',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces (rare in C)
  highlight(0, '@lsp.type.typeParameter.c', { fg = colors.turquoise, bg = 'NONE' })  -- Type parameters (rare in C)
  highlight(0, '@lsp.type.keyword.c',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.c',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.c',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.c',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.c',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.c',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.c',    { fg = colors.purple,    bg = 'NONE' })  -- const variables
  highlight(0, '@lsp.typemod.variable.static.c',      { fg = colors.purple,    bg = 'NONE' })  -- static variables
  highlight(0, '@lsp.typemod.variable.globalScope.c', { fg = colors.purple,    bg = 'NONE' })  -- global variables
  highlight(0, '@lsp.typemod.function.declaration.c', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.static.c',      { fg = colors.orange,    bg = 'NONE' })  -- static functions
  highlight(0, '@lsp.typemod.type.declaration.c',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.c',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return c
