-------------------------------------------------------------------------------
-- Swift Files
-- Highlighting for .swift files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local swift   = {}


-------------------------------------------------------------------------------
-- Settings

swift.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'swiftKeyword',          { link = "Keyword" })  -- General keywords
  highlight(0, 'swiftStatement',        { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, fallthrough
  highlight(0, 'swiftConditional',      { link = "Conditional" })  -- if, else, switch, case, default, guard
  highlight(0, 'swiftRepeat',           { fg = colors.blue,       bg = 'NONE'            })  -- for, while, repeat
  highlight(0, 'swiftException',        { fg = colors.blue,       bg = 'NONE'            })  -- do, try, catch, throw, throws, rethrows
  highlight(0, 'swiftLabel',            { fg = colors.blue,       bg = 'NONE'            })  -- Labels
  highlight(0, 'swiftImport',           { fg = colors.blue,       bg = 'NONE'            })  -- import
  highlight(0, 'swiftDefer',            { fg = colors.blue,       bg = 'NONE'            })  -- defer
  highlight(0, 'swiftTypeDefinition',   { link = "Type" })  -- class, struct, enum, protocol, extension, actor
  highlight(0, 'swiftStorageClass',     { fg = colors.blue,       bg = 'NONE'            })  -- static, class (modifier), final
  highlight(0, 'swiftAccessControl',    { fg = colors.blue,       bg = 'NONE'            })  -- public, private, internal, fileprivate, open
  highlight(0, 'swiftModifier',         { fg = colors.blue,       bg = 'NONE'            })  -- lazy, weak, unowned, mutating, nonmutating
  highlight(0, 'swiftInOut',            { fg = colors.blue,       bg = 'NONE'            })  -- inout
  highlight(0, 'swiftFuncKeyword',      { link = "Keyword" })  -- func, init, deinit, subscript
  highlight(0, 'swiftVarKeyword',       { link = "Keyword" })  -- var, let
  highlight(0, 'swiftTypealias',        { link = "Type" })  -- typealias
  highlight(0, 'swiftAssociatedtype',   { fg = colors.blue,       bg = 'NONE'            })  -- associatedtype
  highlight(0, 'swiftWhere',            { fg = colors.blue,       bg = 'NONE'            })  -- where
  highlight(0, 'swiftAs',               { fg = colors.blue,       bg = 'NONE'            })  -- as, as?, as!
  highlight(0, 'swiftIs',               { fg = colors.blue,       bg = 'NONE'            })  -- is
  highlight(0, 'swiftIn',               { fg = colors.blue,       bg = 'NONE'            })  -- in

  -- Async/Await/Actors
  highlight(0, 'swiftAsync',            { fg = colors.blue,       bg = 'NONE'            })  -- async, await
  highlight(0, 'swiftActor',            { fg = colors.blue,       bg = 'NONE'            })  -- actor
  highlight(0, 'swiftIsolated',         { fg = colors.blue,       bg = 'NONE'            })  -- isolated, nonisolated

  -- Special Keywords
  highlight(0, 'swiftSelf',             { fg = colors.blue,       bg = 'NONE'            })  -- self, Self
  highlight(0, 'swiftSuper',            { fg = colors.blue,       bg = 'NONE'            })  -- super
  highlight(0, 'swiftNil',              { fg = colors.blue,       bg = 'NONE'            })  -- nil
  highlight(0, 'swiftBoolean',          { link = "Boolean" })  -- true, false
  highlight(0, 'swiftAvailability',     { fg = colors.blue,       bg = 'NONE'            })  -- #available, #unavailable

  -- Types
  highlight(0, 'swiftType',             { link = "Type" })  -- Type names
  highlight(0, 'swiftTypeIdentifier',   { link = "Type" })  -- User-defined types
  highlight(0, 'swiftBuiltinType',      { link = "Type" })  -- Int, String, Bool, Double, Float, Array, Dictionary, Set, Optional
  highlight(0, 'swiftProtocolType',     { link = "Type" })  -- Protocol names
  highlight(0, 'swiftGenericParameter', { fg = colors.turquoise,  bg = 'NONE'            })  -- Generic type parameters <T>
  highlight(0, 'swiftOptional',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Optional types

  -- Functions
  highlight(0, 'swiftFuncDefinition',   { link = "Function" })  -- Function definitions
  highlight(0, 'swiftFuncCall',         { link = "Function" })  -- Function calls
  highlight(0, 'swiftMethod',           { link = "Function" })  -- Method names
  highlight(0, 'swiftInit',             { fg = colors.orange,     bg = 'NONE'            })  -- init, deinit

  -- Variables and Parameters
  highlight(0, 'swiftIdentifier',       { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'swiftParameter',        { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'swiftProperty',         { fg = colors.purple,     bg = 'NONE'            })  -- Properties
  highlight(0, 'swiftArgLabel',         { fg = colors.purple,     bg = 'NONE'            })  -- Argument labels

  -- Attributes
  highlight(0, 'swiftAttribute',        { fg = colors.pink,       bg = 'NONE'            })  -- @available, @objc, @IBOutlet, @IBAction
  highlight(0, 'swiftPropertyWrapper',  { fg = colors.pink,       bg = 'NONE'            })  -- @State, @Binding, @Published, @ObservedObject
  highlight(0, 'swiftResultBuilder',    { fg = colors.pink,       bg = 'NONE'            })  -- @ViewBuilder, @resultBuilder

  -- Strings
  highlight(0, 'swiftString',           { link = "String" })  -- "strings"
  highlight(0, 'swiftMultilineString',  { link = "String" })  -- """multiline"""
  highlight(0, 'swiftStringDelimiter',  { link = "Delimiter" })  -- String delimiters
  highlight(0, 'swiftInterpolation',    { fg = colors.pink,       bg = 'NONE'            })  -- \(interpolation)
  highlight(0, 'swiftStringEscape',     { link = "String" })  -- \n, \t, etc.
  highlight(0, 'swiftCharacter',        { fg = colors.redLight,   bg = 'NONE'            })  -- Character literals

  -- Numbers
  highlight(0, 'swiftNumber',           { link = "Number" })  -- Numbers
  highlight(0, 'swiftInteger',          { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'swiftFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'swiftBinary',           { fg = colors.greenLight, bg = 'NONE'            })  -- 0b binary
  highlight(0, 'swiftOctal',            { fg = colors.greenLight, bg = 'NONE'            })  -- 0o octal
  highlight(0, 'swiftHex',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex

  -- Operators
  highlight(0, 'swiftOperator',         { link = "Operator" })  -- Operators
  highlight(0, 'swiftOptionalChain',    { fg = colors.white,      bg = 'NONE'            })  -- ?. optional chaining
  highlight(0, 'swiftNilCoalescing',    { fg = colors.white,      bg = 'NONE'            })  -- ?? nil coalescing
  highlight(0, 'swiftForceUnwrap',      { fg = colors.white,      bg = 'NONE'            })  -- ! force unwrap
  highlight(0, 'swiftRange',            { fg = colors.white,      bg = 'NONE'            })  -- ..., ..<
  highlight(0, 'swiftArrow',            { fg = colors.white,      bg = 'NONE'            })  -- -> return type arrow

  -- Comments
  highlight(0, 'swiftComment',          { link = "Comment" })  -- // and /* */ comments
  highlight(0, 'swiftLineComment',      { link = "Comment" })  -- // comments
  highlight(0, 'swiftBlockComment',     { link = "Comment" })  -- /* */ comments
  highlight(0, 'swiftTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Documentation
  highlight(0, 'swiftDocComment',       { link = "Comment" })  -- /// doc comments
  highlight(0, 'swiftDocTag',           { fg = colors.green,      bg = 'NONE'            })  -- - Parameter:, - Returns:, - Throws:

  -- Preprocessor
  highlight(0, 'swiftPreprocessor',     { fg = colors.pink,       bg = 'NONE'            })  -- #if, #elseif, #else, #endif
  highlight(0, 'swiftCompilerDirective', { fg = colors.pink,      bg = 'NONE'            })  -- #warning, #error, #sourceLocation


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.swift)

  -- Variables
  highlight(0, '@variable.swift',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.swift',      { link = "Variable" })  -- self, Self, super
  highlight(0, '@variable.parameter.swift',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.swift',       { link = "Variable" })  -- Properties

  -- Constants
  highlight(0, '@constant.swift',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.swift',      { link = "Constant" })  -- nil, true, false

  -- Functions
  highlight(0, '@function.swift',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.swift',         { link = "Function" })  -- Function calls
  highlight(0, '@function.method.swift',       { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.swift',  { link = "Function" })  -- Method calls
  highlight(0, '@function.builtin.swift',      { link = "Function" })  -- Built-in functions
  highlight(0, '@constructor.swift',           { fg = colors.turquoise, bg = 'NONE' })  -- init calls

  -- Types
  highlight(0, '@type.swift',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.swift',          { link = "Type" })  -- Built-in types
  highlight(0, '@type.definition.swift',       { link = "Type" })  -- Type definitions
  highlight(0, '@type.qualifier.swift',        { link = "Type" })  -- Type qualifiers

  -- Attributes
  highlight(0, '@attribute.swift',             { fg = colors.pink,      bg = 'NONE' })  -- @attributes

  -- Keywords
  highlight(0, '@keyword.swift',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.swift',      { link = "Keyword" })  -- func
  highlight(0, '@keyword.type.swift',          { link = "Keyword" })  -- class, struct, enum, protocol, extension
  highlight(0, '@keyword.modifier.swift',      { link = "Keyword" })  -- public, private, static, final, mutating
  highlight(0, '@keyword.return.swift',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.import.swift',        { link = "Keyword" })  -- import
  highlight(0, '@keyword.repeat.swift',        { link = "Keyword" })  -- for, while, repeat
  highlight(0, '@keyword.conditional.swift',   { link = "Conditional" })  -- if, else, switch, case, guard
  highlight(0, '@keyword.exception.swift',     { link = "Keyword" })  -- do, try, catch, throw
  highlight(0, '@keyword.operator.swift',      { link = "Operator" })  -- as, is, in
  highlight(0, '@keyword.coroutine.swift',     { link = "Keyword" })  -- async, await

  -- Strings
  highlight(0, '@string.swift',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.swift',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.special.swift',        { link = "String" })  -- \() interpolation
  highlight(0, '@string.regexp.swift',         { link = "String" })  -- Regex literals

  -- Numbers
  highlight(0, '@number.swift',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.swift',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.swift',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.swift',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.swift', { link = "Comment" })  -- Doc comments

  -- Modules
  highlight(0, '@module.swift',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@label.swift',                 { fg = colors.purple,    bg = 'NONE' })  -- Argument labels
  highlight(0, '@property.swift',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Operators and Punctuation
  highlight(0, '@operator.swift',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.swift',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.swift', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.swift',   { fg = colors.pink,      bg = 'NONE' })  -- \() interpolation


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.swift)

  highlight(0, '@lsp.type.variable.swift',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.swift',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.swift',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.swift',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.swift',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.class.swift',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.struct.swift',        { fg = colors.turquoise, bg = 'NONE' })  -- Structs
  highlight(0, '@lsp.type.enum.swift',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.swift',    { fg = colors.purple,    bg = 'NONE' })  -- Enum cases
  highlight(0, '@lsp.type.protocol.swift',      { fg = colors.turquoise, bg = 'NONE' })  -- Protocols
  highlight(0, '@lsp.type.type.swift',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.typeParameter.swift', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type params
  highlight(0, '@lsp.type.namespace.swift',     { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.keyword.swift',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.swift',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.swift',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.swift',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.swift',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.swift',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.static.swift',       { link = "Variable" })  -- Static properties
  highlight(0, '@lsp.typemod.function.declaration.swift',  { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.method.static.swift',         { fg = colors.orange,    bg = 'NONE' })  -- Static methods
  highlight(0, '@lsp.typemod.class.declaration.swift',     { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
  highlight(0, '@lsp.typemod.struct.declaration.swift',    { fg = colors.turquoise, bg = 'NONE' })  -- Struct declarations
  highlight(0, '@lsp.typemod.protocol.declaration.swift',  { fg = colors.turquoise, bg = 'NONE' })  -- Protocol declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.swift',   { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return swift
