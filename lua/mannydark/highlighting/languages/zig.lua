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
  highlight(0, 'zigKeyword',          { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'zigStatement',        { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue
  highlight(0, 'zigConditional',      { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch
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
  highlight(0, 'zigType',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'zigBuiltinType',      { fg = colors.turquoise,  bg = 'NONE'            })  -- i8, i16, i32, i64, i128, u8, u16, u32, u64, u128, isize, usize, f16, f32, f64, f80, f128, bool, void, noreturn, type, anyerror, anytype, anyframe, anyopaque, comptime_int, comptime_float
  highlight(0, 'zigOptionalType',     { fg = colors.turquoise,  bg = 'NONE'            })  -- ?T optional types
  highlight(0, 'zigErrorUnion',       { fg = colors.turquoise,  bg = 'NONE'            })  -- !T error union types
  highlight(0, 'zigPointerType',      { fg = colors.turquoise,  bg = 'NONE'            })  -- *T, [*]T, [*:0]T pointer types
  highlight(0, 'zigSliceType',        { fg = colors.turquoise,  bg = 'NONE'            })  -- []T slice types
  highlight(0, 'zigArrayType',        { fg = colors.turquoise,  bg = 'NONE'            })  -- [N]T array types

  -- Constants
  highlight(0, 'zigConstant',         { fg = colors.blue,       bg = 'NONE'            })  -- Constants
  highlight(0, 'zigBoolean',          { fg = colors.blue,       bg = 'NONE'            })  -- true, false
  highlight(0, 'zigNull',             { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'zigUndefined',        { fg = colors.blue,       bg = 'NONE'            })  -- undefined

  -- Builtins
  highlight(0, 'zigBuiltin',          { fg = colors.pink,       bg = 'NONE'            })  -- @import, @intCast, @as, @typeInfo, @This, @typeName, etc.
  highlight(0, 'zigBuiltinFn',        { fg = colors.pink,       bg = 'NONE'            })  -- All @builtin functions

  -- Functions
  highlight(0, 'zigFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions
  highlight(0, 'zigFunctionCall',     { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'zigMethod',           { fg = colors.orange,     bg = 'NONE'            })  -- Method calls

  -- Variables
  highlight(0, 'zigIdentifier',       { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'zigParameter',        { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'zigField',            { fg = colors.purple,     bg = 'NONE'            })  -- Struct/union fields
  highlight(0, 'zigCapture',          { fg = colors.purple,     bg = 'NONE'            })  -- Capture variables |val|

  -- Strings
  highlight(0, 'zigString',           { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'zigMultilineString',  { fg = colors.redLight,   bg = 'NONE'            })  -- \\multiline strings
  highlight(0, 'zigStringEscape',     { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, \x, \u{}, etc.
  highlight(0, 'zigChar',             { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals

  -- Numbers
  highlight(0, 'zigNumber',           { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'zigInteger',          { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'zigFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'zigBinary',           { fg = colors.greenLight, bg = 'NONE'            })  -- 0b1010 binary
  highlight(0, 'zigOctal',            { fg = colors.greenLight, bg = 'NONE'            })  -- 0o777 octal
  highlight(0, 'zigHex',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF hex

  -- Operators
  highlight(0, 'zigOperator',         { fg = colors.white,      bg = 'NONE'            })  -- + - * / % = < > ! & | ^ ~ ?
  highlight(0, 'zigArrow',            { fg = colors.white,      bg = 'NONE'            })  -- => arrow
  highlight(0, 'zigPtrOperator',      { fg = colors.white,      bg = 'NONE'            })  -- .* pointer dereference, .? optional unwrap
  highlight(0, 'zigErrorUnionOp',     { fg = colors.white,      bg = 'NONE'            })  -- ! error union operator

  -- Comments
  highlight(0, 'zigComment',          { fg = colors.red,        bg = 'NONE'            })  -- // comments
  highlight(0, 'zigDocComment',       { fg = colors.red,        bg = 'NONE'            })  -- /// doc comments
  highlight(0, 'zigTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.zig)

  -- Variables
  highlight(0, '@variable.zig',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.zig',      { fg = colors.blue,      bg = 'NONE' })  -- Built-in variables
  highlight(0, '@variable.parameter.zig',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.zig',       { fg = colors.purple,    bg = 'NONE' })  -- Struct/union fields

  -- Constants
  highlight(0, '@constant.zig',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.zig',      { fg = colors.blue,      bg = 'NONE' })  -- true, false, null, undefined

  -- Functions
  highlight(0, '@function.zig',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.zig',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.method.zig',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.zig',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.builtin.zig',      { fg = colors.pink,      bg = 'NONE' })  -- @builtins
  highlight(0, '@constructor.zig',           { fg = colors.turquoise, bg = 'NONE' })  -- Struct initialization

  -- Types
  highlight(0, '@type.zig',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.zig',          { fg = colors.turquoise, bg = 'NONE' })  -- i32, u64, bool, etc.
  highlight(0, '@type.definition.zig',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier.zig',        { fg = colors.blue,      bg = 'NONE' })  -- const, var

  -- Keywords
  highlight(0, '@keyword.zig',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.zig',      { fg = colors.blue,      bg = 'NONE' })  -- fn
  highlight(0, '@keyword.type.zig',          { fg = colors.blue,      bg = 'NONE' })  -- struct, enum, union
  highlight(0, '@keyword.modifier.zig',      { fg = colors.blue,      bg = 'NONE' })  -- pub, extern, export, packed, volatile
  highlight(0, '@keyword.return.zig',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.import.zig',        { fg = colors.blue,      bg = 'NONE' })  -- usingnamespace
  highlight(0, '@keyword.repeat.zig',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, inline for/while
  highlight(0, '@keyword.conditional.zig',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, switch
  highlight(0, '@keyword.exception.zig',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, error, errdefer
  highlight(0, '@keyword.operator.zig',      { fg = colors.blue,      bg = 'NONE' })  -- and, or, orelse
  highlight(0, '@keyword.coroutine.zig',     { fg = colors.blue,      bg = 'NONE' })  -- async, await, suspend, resume

  -- Builtins
  highlight(0, '@attribute.zig',             { fg = colors.pink,      bg = 'NONE' })  -- @builtins

  -- Strings
  highlight(0, '@string.zig',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.zig',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@character.zig',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.zig',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.zig',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.zig',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.zig',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.zig', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Labels
  highlight(0, '@label.zig',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels
  highlight(0, '@module.zig',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@property.zig',              { fg = colors.purple,    bg = 'NONE' })  -- Struct/union fields

  -- Operators and Punctuation
  highlight(0, '@operator.zig',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.zig',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.zig', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.zig',   { fg = colors.pink,      bg = 'NONE' })  -- @ in builtins


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.zig)

  highlight(0, '@lsp.type.variable.zig',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
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
  highlight(0, '@lsp.type.keyword.zig',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.zig',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.zig',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.zig',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.zig',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.zig',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.errorTag.zig',      { fg = colors.red,       bg = 'NONE' })  -- Error tags

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.zig',    { fg = colors.purple,    bg = 'NONE' })  -- const variables
  highlight(0, '@lsp.typemod.variable.comptime.zig',    { fg = colors.purple,    bg = 'NONE' })  -- comptime variables
  highlight(0, '@lsp.typemod.function.declaration.zig', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.generic.zig',     { fg = colors.orange,    bg = 'NONE' })  -- Generic functions
  highlight(0, '@lsp.typemod.type.declaration.zig',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.zig',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return zig
