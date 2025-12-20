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
  highlight(0, 'cStatement',          { link = "Keyword"      })  -- return, break, continue, goto
  highlight(0, 'cConditional',        { link = "Conditional" })  -- if, else, switch
  highlight(0, 'cRepeat',             { link = "Keyword"      })  -- for, while, do
  highlight(0, 'cLabel',              { link = "Keyword"      })  -- case, default
  highlight(0, 'cOperator',           { link = "Operator" })  -- sizeof, typeof, alignof
  highlight(0, 'cStorageClass',       { link = "Keyword"      })  -- static, extern, register, auto, volatile, const, restrict, inline
  highlight(0, 'cStructure',          { link = "Keyword"      })  -- struct, union, enum, typedef

  -- Types
  highlight(0, 'cType',               { link = "Type" })  -- int, char, float, double, void, short, long, signed, unsigned
  highlight(0, 'cTypedef',            { link = "Type" })  -- Typedef'd types
  highlight(0, 'cStructName',         { link = "Type"     })  -- Struct names
  highlight(0, 'cEnumName',           { link = "Type"     })  -- Enum names
  highlight(0, 'cUnionName',          { link = "Type"     })  -- Union names

  -- C99/C11/C23 Types
  highlight(0, 'cType99',             { link = "Type" })  -- _Bool, _Complex, _Imaginary
  highlight(0, 'cType11',             { link = "Type" })  -- _Alignas, _Alignof, _Atomic, _Generic, _Noreturn, _Static_assert, _Thread_local

  -- Constants
  highlight(0, 'cBoolean',            { link = "Boolean" })  -- true, false
  highlight(0, 'cConstant',           { link = "Constant" })  -- NULL, true, false, __FILE__, __LINE__, __func__
  highlight(0, 'cNull',               { link = "Keyword"        })  -- NULL

  -- Functions
  highlight(0, 'cFunction',           { link = "Function" })  -- Function names
  highlight(0, 'cUserFunction',       { link = "Function" })  -- User-defined functions

  -- Variables
  highlight(0, 'cIdentifier',         { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'cParameter',          { link = "Variable"  })  -- Function parameters
  highlight(0, 'cMember',             { link = "Variable"  })  -- Struct/union members

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
  highlight(0, 'cString',             { link = "String" })  -- "strings"
  highlight(0, 'cCharacter',          { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'cSpecialChar',        { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, \0, etc.
  highlight(0, 'cFormat',             { fg = colors.pink,       bg = 'NONE'            })  -- %d, %s, %f, etc. in printf

  -- Numbers
  highlight(0, 'cFloat',              { link = "Float"})  -- Floats
  highlight(0, 'cNumber',             { link = "Number" })  -- Numbers
  highlight(0, 'cInteger',            { link = "Number"           })  -- Integers
  highlight(0, 'cOctal',              { link = "Number"           })  -- 0777 octal
  highlight(0, 'cHex',                { link = "Number"           })  -- 0xFF hex
  highlight(0, 'cBinary',             { link = "Number"           })  -- 0b1010 binary (C23)

  -- Operators
  highlight(0, 'cOperatorSign',       { link = "Operator" })  -- + - * / % = < > ! & | ^ ~ ? :
  highlight(0, 'cPointerOperator',    { link = "Operator" })  -- * & pointer operators
  highlight(0, 'cMemberAccess',       { link = "Operator"      })  -- . -> member access
  highlight(0, 'cArrow',              { link = "Operator"      })  -- ->

  -- Comments
  highlight(0, 'cComment',            { link = "Comment" })  -- /* */ and // comments
  highlight(0, 'cCommentL',           { link = "Comment" })  -- // line comments
  highlight(0, 'cCommentStart',       { link = "Comment" })  -- /* start
  highlight(0, 'cTodo',               { link = "Comment" })  -- TODO, FIXME, XXX, etc.

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
  highlight(0, '@variable.c',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.c',      { link = "Variable" })  -- Built-in variables
  highlight(0, '@variable.parameter.c',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.c',       { link = "Variable" })  -- Struct/union members

  -- Constants
  highlight(0, '@constant.c',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.c',      { link = "Constant" })  -- NULL, true, false
  highlight(0, '@constant.macro.c',        { link = "Constant" })  -- Macro constants

  -- Functions
  highlight(0, '@function.c',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.c',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.c',      { link = "Function" })  -- Built-in functions
  highlight(0, '@function.macro.c',        { link = "Function" })  -- Macro functions
  highlight(0, '@constructor.c',           { fg = colors.turquoise, bg = 'NONE' })  -- Compound literals

  -- Types
  highlight(0, '@type.c',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.c',          { link = "Type" })  -- int, char, float, etc.
  highlight(0, '@type.definition.c',       { link = "Type" })  -- typedef definitions
  highlight(0, '@type.qualifier.c',        { link = "Type" })  -- const, volatile, restrict

  -- Keywords
  highlight(0, '@keyword.c',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.c',      { link = "Keyword" })  -- (not really used in C)
  highlight(0, '@keyword.type.c',          { link = "Keyword" })  -- struct, union, enum, typedef
  highlight(0, '@keyword.modifier.c',      { link = "Keyword" })  -- static, extern, const, volatile
  highlight(0, '@keyword.return.c',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.repeat.c',        { link = "Keyword" })  -- for, while, do
  highlight(0, '@keyword.conditional.c',   { link = "Conditional" })  -- if, else, switch, case, default
  highlight(0, '@keyword.operator.c',      { link = "Operator" })  -- sizeof, typeof, alignof
  highlight(0, '@keyword.directive.c',     { link = "Keyword" })  -- Preprocessor directives

  -- Preprocessor
  highlight(0, '@preproc.c',               { fg = colors.pink,      bg = 'NONE' })  -- Preprocessor directives
  highlight(0, '@include.c',               { fg = colors.pink,      bg = 'NONE' })  -- #include
  highlight(0, '@define.c',                { fg = colors.pink,      bg = 'NONE' })  -- #define

  -- Strings
  highlight(0, '@string.c',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.c',         { link = "String" })  -- Escape sequences
  highlight(0, '@character.c',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.c',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.c',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.c',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.c',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.c', { link = "Comment" })  -- Doc comments

  -- Labels
  highlight(0, '@label.c',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels for goto
  highlight(0, '@property.c',              { fg = colors.purple,    bg = 'NONE' })  -- Struct/union members

  -- Operators and Punctuation
  highlight(0, '@operator.c',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.c',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.c', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.c',   { fg = colors.pink,      bg = 'NONE' })  -- # in preprocessor


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.c)

  highlight(0, '@lsp.type.variable.c',      { link = "Variable" })  -- Variables
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
  highlight(0, '@lsp.type.keyword.c',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.c',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.c',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.c',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.c',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.c',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.c',    { link = "Variable" })  -- const variables
  highlight(0, '@lsp.typemod.variable.static.c',      { link = "Variable" })  -- static variables
  highlight(0, '@lsp.typemod.variable.globalScope.c', { link = "Variable" })  -- global variables
  highlight(0, '@lsp.typemod.function.declaration.c', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.static.c',      { fg = colors.orange,    bg = 'NONE' })  -- static functions
  highlight(0, '@lsp.typemod.type.declaration.c',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.c',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return c
