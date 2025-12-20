-------------------------------------------------------------------------------
-- Zig Files
-- Highlighting for .zig files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local zig     = {}


-------------------------------------------------------------------------------
-- Settings

zig.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'zigKeyword',          { link = "Keyword" })  -- General keywords
  highlight(0, 'zigStatement',        { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue
  highlight(0, 'zigConditional',      { link = "Conditional" })  -- if, else, switch
  highlight(0, 'zigRepeat',           { fg = colors.blue,       bg = 'NONE'            })  -- for, while
  highlight(0, 'zigLabel',            { fg = colors.blue,       bg = 'NONE'            })  -- Labels
  highlight(0, 'zigStorage',          { fg = colors.blue,       bg = 'NONE'            })  -- const, var, extern, export, pub
  highlight(0, 'zigStructure',        { fg = colors.blue,       bg = 'NONE'            })  -- struct, enum, union, packed, opaque
  highlight(0, 'zigFn',               { fg = colors.blue,       bg = 'NONE'            })  -- fn

  -- Error Handling
  highlight(0, 'zigError',            { fg = colors.blue,       bg = 'NONE'            })  -- error
  highlight(0, 'zigTry',              { fg = colors.blue,       bg = 'NONE'            })  -- try
  highlight(0, 'zigCatch',            { fg = colors.blue,       bg = 'NONE'            })  -- catch
  highlight(0, 'zigUnreachable',      { fg = colors.blue,       bg = 'NONE'            })  -- unreachable

  -- Memory Management
  highlight(0, 'zigDefer',            { fg = colors.blue,       bg = 'NONE'            })  -- defer
  highlight(0, 'zigErrdefer',         { fg = colors.blue,       bg = 'NONE'            })  -- errdefer

  -- Compile-time
  highlight(0, 'zigComptime',         { fg = colors.blue,       bg = 'NONE'            })  -- comptime
  highlight(0, 'zigInline',           { fg = colors.blue,       bg = 'NONE'            })  -- inline
  highlight(0, 'zigNoInline',         { fg = colors.blue,       bg = 'NONE'            })  -- noinline

  -- Async
  highlight(0, 'zigAsync',            { fg = colors.blue,       bg = 'NONE'            })  -- async
  highlight(0, 'zigAwait',            { fg = colors.blue,       bg = 'NONE'            })  -- await
  highlight(0, 'zigSuspend',          { fg = colors.blue,       bg = 'NONE'            })  -- suspend
  highlight(0, 'zigResume',           { fg = colors.blue,       bg = 'NONE'            })  -- resume
  highlight(0, 'zigNosuspend',        { fg = colors.blue,       bg = 'NONE'            })  -- nosuspend

  -- Testing
  highlight(0, 'zigTest',             { fg = colors.blue,       bg = 'NONE'            })  -- test

  -- Other Keywords
  highlight(0, 'zigPub',              { fg = colors.blue,       bg = 'NONE'            })  -- pub
  highlight(0, 'zigUsingnamespace',   { fg = colors.blue,       bg = 'NONE'            })  -- usingnamespace
  highlight(0, 'zigOrelse',           { fg = colors.blue,       bg = 'NONE'            })  -- orelse
  highlight(0, 'zigAnd',              { fg = colors.blue,       bg = 'NONE'            })  -- and
  highlight(0, 'zigOr',               { fg = colors.blue,       bg = 'NONE'            })  -- or
  highlight(0, 'zigAlign',            { fg = colors.blue,       bg = 'NONE'            })  -- align
  highlight(0, 'zigAllowzero',        { fg = colors.blue,       bg = 'NONE'            })  -- allowzero
  highlight(0, 'zigVolatile',         { fg = colors.blue,       bg = 'NONE'            })  -- volatile
  highlight(0, 'zigLinksection',      { fg = colors.blue,       bg = 'NONE'            })  -- linksection
  highlight(0, 'zigCallconv',         { fg = colors.blue,       bg = 'NONE'            })  -- callconv
  highlight(0, 'zigThreadlocal',      { fg = colors.blue,       bg = 'NONE'            })  -- threadlocal

  -- Types
  highlight(0, 'zigType',             { link = "Type" })  -- Type names
  highlight(0, 'zigBuiltinType',      { link = "Type" })  -- i8, i16, i32, i64, i128, u8, u16, u32, u64, u128, isize, usize, f16, f32, f64, f80, f128, bool, void, noreturn, type, anyerror, anytype, anyframe, anyopaque, comptime_int, comptime_float
  highlight(0, 'zigOptionalType',     { link = "Type" })  -- ?T optional types
  highlight(0, 'zigErrorUnion',       { fg = colors.turquoise,  bg = 'NONE'            })  -- !T error union types
  highlight(0, 'zigPointerType',      { link = "Type" })  -- *T, [*]T, [*:0]T pointer types
  highlight(0, 'zigSliceType',        { link = "Type" })  -- []T slice types
  highlight(0, 'zigArrayType',        { link = "Type" })  -- [N]T array types

  -- Constants
  highlight(0, 'zigConstant',         { link = "Constant" })  -- Constants
  highlight(0, 'zigBoolean',          { link = "Boolean" })  -- true, false
  highlight(0, 'zigNull',             { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'zigUndefined',        { fg = colors.blue,       bg = 'NONE'            })  -- undefined

  -- Builtins
  highlight(0, 'zigBuiltin',          { link = "Function" })  -- @import, @intCast, @as, @typeInfo, @This, @typeName, etc.
  highlight(0, 'zigBuiltinFn',        { fg = colors.pink,       bg = 'NONE'            })  -- All @builtin functions

  -- Functions
  highlight(0, 'zigFunction',         { link = "Function" })  -- Function definitions
  highlight(0, 'zigFunctionCall',     { link = "Function" })  -- Function calls
  highlight(0, 'zigMethod',           { link = "Function" })  -- Method calls

  -- Variables
  highlight(0, 'zigIdentifier',       { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'zigParameter',        { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'zigField',            { fg = colors.purple,     bg = 'NONE'            })  -- Struct/union fields
  highlight(0, 'zigCapture',          { fg = colors.purple,     bg = 'NONE'            })  -- Capture variables |val|

  -- Strings
  highlight(0, 'zigString',           { link = "String" })  -- "strings"
  highlight(0, 'zigMultilineString',  { link = "String" })  -- \\multiline strings
  highlight(0, 'zigStringEscape',     { link = "String" })  -- \n, \t, \x, \u{}, etc.
  highlight(0, 'zigChar',             { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals

  -- Numbers
  highlight(0, 'zigNumber',           { link = "Number" })  -- Numbers
  highlight(0, 'zigInteger',          { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'zigFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'zigBinary',           { fg = colors.greenLight, bg = 'NONE'            })  -- 0b1010 binary
  highlight(0, 'zigOctal',            { fg = colors.greenLight, bg = 'NONE'            })  -- 0o777 octal
  highlight(0, 'zigHex',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF hex

  -- Operators
  highlight(0, 'zigOperator',         { link = "Operator" })  -- + - * / % = < > ! & | ^ ~ ?
  highlight(0, 'zigArrow',            { fg = colors.white,      bg = 'NONE'            })  -- => arrow
  highlight(0, 'zigPtrOperator',      { link = "Operator" })  -- .* pointer dereference, .? optional unwrap
  highlight(0, 'zigErrorUnionOp',     { fg = colors.white,      bg = 'NONE'            })  -- ! error union operator

  -- Comments
  highlight(0, 'zigComment',          { link = "Comment" })  -- // comments
  highlight(0, 'zigDocComment',       { link = "Comment" })  -- /// doc comments
  highlight(0, 'zigTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.zig)

  -- Variables
  highlight(0, '@variable.zig',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.zig',      { link = "Variable" })  -- Built-in variables
  highlight(0, '@variable.parameter.zig',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.zig',       { link = "Variable" })  -- Struct/union fields

  -- Constants
  highlight(0, '@constant.zig',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.zig',      { link = "Constant" })  -- true, false, null, undefined

  -- Functions
  highlight(0, '@function.zig',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.zig',         { link = "Function" })  -- Function calls
  highlight(0, '@function.method.zig',       { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.zig',  { link = "Function" })  -- Method calls
  highlight(0, '@function.builtin.zig',      { link = "Function" })  -- @builtins
  highlight(0, '@constructor.zig',           { fg = colors.turquoise, bg = 'NONE' })  -- Struct initialization

  -- Types
  highlight(0, '@type.zig',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.zig',          { link = "Type" })  -- i32, u64, bool, etc.
  highlight(0, '@type.definition.zig',       { link = "Type" })  -- Type definitions
  highlight(0, '@type.qualifier.zig',        { link = "Type" })  -- const, var

  -- Keywords
  highlight(0, '@keyword.zig',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.zig',      { link = "Keyword" })  -- fn
  highlight(0, '@keyword.type.zig',          { link = "Keyword" })  -- struct, enum, union
  highlight(0, '@keyword.modifier.zig',      { link = "Keyword" })  -- pub, extern, export, packed, volatile
  highlight(0, '@keyword.return.zig',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.import.zig',        { link = "Keyword" })  -- usingnamespace
  highlight(0, '@keyword.repeat.zig',        { link = "Keyword" })  -- for, while, inline for/while
  highlight(0, '@keyword.conditional.zig',   { link = "Conditional" })  -- if, else, switch
  highlight(0, '@keyword.exception.zig',     { link = "Keyword" })  -- try, catch, error, errdefer
  highlight(0, '@keyword.operator.zig',      { link = "Operator" })  -- and, or, orelse
  highlight(0, '@keyword.coroutine.zig',     { link = "Keyword" })  -- async, await, suspend, resume

  -- Builtins
  highlight(0, '@attribute.zig',             { fg = colors.pink,      bg = 'NONE' })  -- @builtins

  -- Strings
  highlight(0, '@string.zig',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.zig',         { link = "String" })  -- Escape sequences
  highlight(0, '@character.zig',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.zig',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.zig',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.zig',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.zig',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.zig', { link = "Comment" })  -- Doc comments

  -- Labels
  highlight(0, '@label.zig',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels
  highlight(0, '@module.zig',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@property.zig',              { fg = colors.purple,    bg = 'NONE' })  -- Struct/union fields

  -- Operators and Punctuation
  highlight(0, '@operator.zig',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.zig',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.zig', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.zig',   { fg = colors.pink,      bg = 'NONE' })  -- @ in builtins


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.zig)

  highlight(0, '@lsp.type.variable.zig',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.zig',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.zig',      { fg = colors.purple,    bg = 'NONE' })  -- Fields
  highlight(0, '@lsp.type.function.zig',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.zig',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.builtin.zig',       { fg = colors.pink,      bg = 'NONE' })  -- @builtins
  highlight(0, '@lsp.type.type.zig',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.struct.zig',        { fg = colors.turquoise, bg = 'NONE' })  -- Structs
  highlight(0, '@lsp.type.enum.zig',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.union.zig',         { fg = colors.turquoise, bg = 'NONE' })  -- Unions
  highlight(0, '@lsp.type.enumMember.zig',    { fg = colors.purple,    bg = 'NONE' })  -- Enum members
  highlight(0, '@lsp.type.namespace.zig',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.typeParameter.zig', { fg = colors.turquoise, bg = 'NONE' })  -- Type parameters
  highlight(0, '@lsp.type.keyword.zig',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.zig',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.zig',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.zig',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.zig',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.zig',       { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.errorTag.zig',      { fg = colors.red,       bg = 'NONE' })  -- Error tags

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.zig',    { link = "Variable" })  -- const variables
  highlight(0, '@lsp.typemod.variable.comptime.zig',    { link = "Variable" })  -- comptime variables
  highlight(0, '@lsp.typemod.function.declaration.zig', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.generic.zig',     { fg = colors.orange,    bg = 'NONE' })  -- Generic functions
  highlight(0, '@lsp.typemod.type.declaration.zig',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.zig',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return zig
