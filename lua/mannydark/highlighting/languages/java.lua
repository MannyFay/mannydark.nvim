-------------------------------------------------------------------------------
-- Java Files
-- Highlighting for .java files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local java    = {}


-------------------------------------------------------------------------------
-- Settings

java.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'javaStatement',      { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue
  highlight(0, 'javaConditional',    { link = "Conditional" })  -- if, else, switch
  highlight(0, 'javaRepeat',         { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'javaException',      { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, finally, throw, throws
  highlight(0, 'javaOperator',       { link = "Operator" })  -- new, instanceof
  highlight(0, 'javaAssert',         { fg = colors.blue,       bg = 'NONE'            })  -- assert
  highlight(0, 'javaStorageClass',   { fg = colors.blue,       bg = 'NONE'            })  -- static, final, transient, volatile
  highlight(0, 'javaMethodDecl',     { link = "Function" })  -- synchronized, native, strictfp
  highlight(0, 'javaClassDecl',      { fg = colors.blue,       bg = 'NONE'            })  -- class, interface, enum, extends, implements
  highlight(0, 'javaScopeDecl',      { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected
  highlight(0, 'javaBranch',         { fg = colors.blue,       bg = 'NONE'            })  -- break, continue
  highlight(0, 'javaLabel',          { fg = colors.blue,       bg = 'NONE'            })  -- case, default
  highlight(0, 'javaTypedef',        { link = "Type" })  -- this, super
  highlight(0, 'javaConstant',       { link = "Constant" })  -- null, true, false

  -- Types
  highlight(0, 'javaType',           { link = "Type" })  -- boolean, byte, char, int, long, float, double, short, void
  highlight(0, 'javaExternal',       { fg = colors.turquoise,  bg = 'NONE'            })  -- import, package
  highlight(0, 'javaC_Java',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Class, Object, String, etc.
  highlight(0, 'javaE_Java',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Exception classes
  highlight(0, 'javaX_Java',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Extended classes
  highlight(0, 'javaR_Java',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Runtime classes
  highlight(0, 'javaLangObject',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Object methods

  -- Functions
  highlight(0, 'javaFuncDef',        { link = "Function" })  -- Function definitions

  -- Variables
  highlight(0, 'javaVarArg',         { fg = colors.purple,     bg = 'NONE'            })  -- Varargs ...

  -- Annotations
  highlight(0, 'javaAnnotation',     { fg = colors.pink,       bg = 'NONE'            })  -- @Override, @Deprecated, etc.

  -- Strings
  highlight(0, 'javaString',         { link = "String" })  -- "strings"
  highlight(0, 'javaCharacter',      { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'javaSpecialChar',    { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'javaSpecialCharError', { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Invalid escapes

  -- Numbers
  highlight(0, 'javaNumber',         { link = "Number" })  -- Numbers

  -- Comments
  highlight(0, 'javaComment',        { link = "Comment" })  -- // and /* */ comments
  highlight(0, 'javaLineComment',    { link = "Comment" })  -- // comments
  highlight(0, 'javaComment2String', { link = "Comment" })  -- Strings in comments
  highlight(0, 'javaCommentString',  { link = "Comment" })  -- Strings in comments
  highlight(0, 'javaTodo',           { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Javadoc
  highlight(0, 'javaDocComment',     { link = "Comment" })  -- /** */ doc comments
  highlight(0, 'javaCommentTitle',   { link = "Comment" })  -- Doc comment titles
  highlight(0, 'javaDocTags',        { fg = colors.green,      bg = 'NONE'            })  -- @param, @return, etc.
  highlight(0, 'javaDocParam',       { fg = colors.purple,     bg = 'NONE'            })  -- Parameter names in docs
  highlight(0, 'javaDocSeeTagParam', { fg = colors.purple,     bg = 'NONE'            })  -- @see parameters

  -- Errors
  highlight(0, 'javaError',          { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Syntax errors
  highlight(0, 'javaSpaceError',     { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Space errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.java)

  -- Variables
  highlight(0, '@variable.java',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.java',      { link = "Variable" })  -- this, super
  highlight(0, '@variable.parameter.java',    { link = "Variable" })  -- Method parameters
  highlight(0, '@variable.member.java',       { link = "Variable" })  -- Fields

  -- Constants
  highlight(0, '@constant.java',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.java',      { link = "Constant" })  -- null, true, false

  -- Functions
  highlight(0, '@function.java',              { link = "Function" })  -- Method definitions
  highlight(0, '@function.call.java',         { link = "Function" })  -- Method calls
  highlight(0, '@function.method.java',       { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.java',  { link = "Function" })  -- Method calls
  highlight(0, '@constructor.java',           { fg = colors.turquoise, bg = 'NONE' })  -- Constructor calls

  -- Types
  highlight(0, '@type.java',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.java',          { link = "Type" })  -- Primitive types (int, boolean, etc.)
  highlight(0, '@type.definition.java',       { link = "Type" })  -- Type definitions
  highlight(0, '@type.qualifier.java',        { link = "Type" })  -- Type qualifiers

  -- Annotations
  highlight(0, '@attribute.java',             { fg = colors.pink,      bg = 'NONE' })  -- Annotations

  -- Keywords
  highlight(0, '@keyword.java',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.java',      { link = "Keyword" })  -- void (in method context)
  highlight(0, '@keyword.type.java',          { link = "Keyword" })  -- class, interface, enum
  highlight(0, '@keyword.modifier.java',      { link = "Keyword" })  -- public, private, static, final
  highlight(0, '@keyword.return.java',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.import.java',        { link = "Keyword" })  -- import, package
  highlight(0, '@keyword.repeat.java',        { link = "Keyword" })  -- for, while, do
  highlight(0, '@keyword.conditional.java',   { link = "Conditional" })  -- if, else, switch, case
  highlight(0, '@keyword.exception.java',     { link = "Keyword" })  -- try, catch, finally, throw, throws
  highlight(0, '@keyword.operator.java',      { link = "Operator" })  -- new, instanceof

  -- Strings
  highlight(0, '@string.java',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.java',         { link = "String" })  -- Escape sequences
  highlight(0, '@character.java',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.java',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.java',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.java',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.java',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.java', { link = "Comment" })  -- Javadoc comments

  -- Modules
  highlight(0, '@module.java',                { fg = colors.turquoise, bg = 'NONE' })  -- Package names
  highlight(0, '@label.java',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels
  highlight(0, '@property.java',              { fg = colors.purple,    bg = 'NONE' })  -- Fields

  -- Operators and Punctuation
  highlight(0, '@operator.java',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.java',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.java', { link = "Delimiter" })  -- , ; :


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.java)

  highlight(0, '@lsp.type.variable.java',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.java',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.java',      { fg = colors.purple,    bg = 'NONE' })  -- Fields
  highlight(0, '@lsp.type.function.java',      { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.method.java',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.class.java',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.interface.java',     { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.enum.java',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.java',    { fg = colors.purple,    bg = 'NONE' })  -- Enum constants
  highlight(0, '@lsp.type.type.java',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.typeParameter.java', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type params <T>
  highlight(0, '@lsp.type.namespace.java',     { fg = colors.turquoise, bg = 'NONE' })  -- Packages
  highlight(0, '@lsp.type.annotation.java',    { fg = colors.pink,      bg = 'NONE' })  -- Annotations
  highlight(0, '@lsp.type.keyword.java',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.java',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.java',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.java',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.java',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.java',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.java',    { link = "Variable" })  -- final variables
  highlight(0, '@lsp.typemod.variable.static.java',      { link = "Variable" })  -- static fields
  highlight(0, '@lsp.typemod.method.static.java',        { fg = colors.orange,    bg = 'NONE' })  -- static methods
  highlight(0, '@lsp.typemod.method.declaration.java',   { fg = colors.orange,    bg = 'NONE' })  -- method declarations
  highlight(0, '@lsp.typemod.class.declaration.java',    { fg = colors.turquoise, bg = 'NONE' })  -- class declarations
  highlight(0, '@lsp.typemod.interface.declaration.java', { fg = colors.turquoise, bg = 'NONE' }) -- interface declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.java',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return java
