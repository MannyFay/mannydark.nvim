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
  highlight(0, 'swiftKeyword',          { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'swiftStatement',        { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, fallthrough
  highlight(0, 'swiftConditional',      { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch, case, default, guard
  highlight(0, 'swiftRepeat',           { fg = colors.blue,       bg = 'NONE'            })  -- for, while, repeat
  highlight(0, 'swiftException',        { fg = colors.blue,       bg = 'NONE'            })  -- do, try, catch, throw, throws, rethrows
  highlight(0, 'swiftLabel',            { fg = colors.blue,       bg = 'NONE'            })  -- Labels
  highlight(0, 'swiftImport',           { fg = colors.blue,       bg = 'NONE'            })  -- import
  highlight(0, 'swiftDefer',            { fg = colors.blue,       bg = 'NONE'            })  -- defer
  highlight(0, 'swiftTypeDefinition',   { fg = colors.blue,       bg = 'NONE'            })  -- class, struct, enum, protocol, extension, actor
  highlight(0, 'swiftStorageClass',     { fg = colors.blue,       bg = 'NONE'            })  -- static, class (modifier), final
  highlight(0, 'swiftAccessControl',    { fg = colors.blue,       bg = 'NONE'            })  -- public, private, internal, fileprivate, open
  highlight(0, 'swiftModifier',         { fg = colors.blue,       bg = 'NONE'            })  -- lazy, weak, unowned, mutating, nonmutating
  highlight(0, 'swiftInOut',            { fg = colors.blue,       bg = 'NONE'            })  -- inout
  highlight(0, 'swiftFuncKeyword',      { fg = colors.blue,       bg = 'NONE'            })  -- func, init, deinit, subscript
  highlight(0, 'swiftVarKeyword',       { fg = colors.blue,       bg = 'NONE'            })  -- var, let
  highlight(0, 'swiftTypealias',        { fg = colors.blue,       bg = 'NONE'            })  -- typealias
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
  highlight(0, 'swiftBoolean',          { fg = colors.blue,       bg = 'NONE'            })  -- true, false
  highlight(0, 'swiftAvailability',     { fg = colors.blue,       bg = 'NONE'            })  -- #available, #unavailable

  -- Types
  highlight(0, 'swiftType',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'swiftTypeIdentifier',   { fg = colors.turquoise,  bg = 'NONE'            })  -- User-defined types
  highlight(0, 'swiftBuiltinType',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Int, String, Bool, Double, Float, Array, Dictionary, Set, Optional
  highlight(0, 'swiftProtocolType',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Protocol names
  highlight(0, 'swiftGenericParameter', { fg = colors.turquoise,  bg = 'NONE'            })  -- Generic type parameters <T>
  highlight(0, 'swiftOptional',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Optional types

  -- Functions
  highlight(0, 'swiftFuncDefinition',   { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions
  highlight(0, 'swiftFuncCall',         { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'swiftMethod',           { fg = colors.orange,     bg = 'NONE'            })  -- Method names
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
  highlight(0, 'swiftString',           { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'swiftMultilineString',  { fg = colors.redLight,   bg = 'NONE'            })  -- """multiline"""
  highlight(0, 'swiftStringDelimiter',  { fg = colors.redLight,   bg = 'NONE'            })  -- String delimiters
  highlight(0, 'swiftInterpolation',    { fg = colors.pink,       bg = 'NONE'            })  -- \(interpolation)
  highlight(0, 'swiftStringEscape',     { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'swiftCharacter',        { fg = colors.redLight,   bg = 'NONE'            })  -- Character literals

  -- Numbers
  highlight(0, 'swiftNumber',           { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'swiftInteger',          { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'swiftFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'swiftBinary',           { fg = colors.greenLight, bg = 'NONE'            })  -- 0b binary
  highlight(0, 'swiftOctal',            { fg = colors.greenLight, bg = 'NONE'            })  -- 0o octal
  highlight(0, 'swiftHex',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex

  -- Operators
  highlight(0, 'swiftOperator',         { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'swiftOptionalChain',    { fg = colors.white,      bg = 'NONE'            })  -- ?. optional chaining
  highlight(0, 'swiftNilCoalescing',    { fg = colors.white,      bg = 'NONE'            })  -- ?? nil coalescing
  highlight(0, 'swiftForceUnwrap',      { fg = colors.white,      bg = 'NONE'            })  -- ! force unwrap
  highlight(0, 'swiftRange',            { fg = colors.white,      bg = 'NONE'            })  -- ..., ..<
  highlight(0, 'swiftArrow',            { fg = colors.white,      bg = 'NONE'            })  -- -> return type arrow

  -- Comments
  highlight(0, 'swiftComment',          { fg = colors.red,        bg = 'NONE'            })  -- // and /* */ comments
  highlight(0, 'swiftLineComment',      { fg = colors.red,        bg = 'NONE'            })  -- // comments
  highlight(0, 'swiftBlockComment',     { fg = colors.red,        bg = 'NONE'            })  -- /* */ comments
  highlight(0, 'swiftTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Documentation
  highlight(0, 'swiftDocComment',       { fg = colors.red,        bg = 'NONE'            })  -- /// doc comments
  highlight(0, 'swiftDocTag',           { fg = colors.green,      bg = 'NONE'            })  -- - Parameter:, - Returns:, - Throws:

  -- Preprocessor
  highlight(0, 'swiftPreprocessor',     { fg = colors.pink,       bg = 'NONE'            })  -- #if, #elseif, #else, #endif
  highlight(0, 'swiftCompilerDirective', { fg = colors.pink,      bg = 'NONE'            })  -- #warning, #error, #sourceLocation


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.swift)

  -- Variables
  highlight(0, '@variable.swift',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.swift',      { fg = colors.blue,      bg = 'NONE' })  -- self, Self, super
  highlight(0, '@variable.parameter.swift',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.swift',       { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Constants
  highlight(0, '@constant.swift',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.swift',      { fg = colors.blue,      bg = 'NONE' })  -- nil, true, false

  -- Functions
  highlight(0, '@function.swift',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.swift',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.method.swift',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.swift',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.builtin.swift',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@constructor.swift',           { fg = colors.turquoise, bg = 'NONE' })  -- init calls

  -- Types
  highlight(0, '@type.swift',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.swift',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.swift',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier.swift',        { fg = colors.blue,      bg = 'NONE' })  -- Type qualifiers

  -- Attributes
  highlight(0, '@attribute.swift',             { fg = colors.pink,      bg = 'NONE' })  -- @attributes

  -- Keywords
  highlight(0, '@keyword.swift',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.swift',      { fg = colors.blue,      bg = 'NONE' })  -- func
  highlight(0, '@keyword.type.swift',          { fg = colors.blue,      bg = 'NONE' })  -- class, struct, enum, protocol, extension
  highlight(0, '@keyword.modifier.swift',      { fg = colors.blue,      bg = 'NONE' })  -- public, private, static, final, mutating
  highlight(0, '@keyword.return.swift',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.import.swift',        { fg = colors.blue,      bg = 'NONE' })  -- import
  highlight(0, '@keyword.repeat.swift',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, repeat
  highlight(0, '@keyword.conditional.swift',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, switch, case, guard
  highlight(0, '@keyword.exception.swift',     { fg = colors.blue,      bg = 'NONE' })  -- do, try, catch, throw
  highlight(0, '@keyword.operator.swift',      { fg = colors.blue,      bg = 'NONE' })  -- as, is, in
  highlight(0, '@keyword.coroutine.swift',     { fg = colors.blue,      bg = 'NONE' })  -- async, await

  -- Strings
  highlight(0, '@string.swift',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.swift',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.swift',        { fg = colors.pink,      bg = 'NONE' })  -- \() interpolation
  highlight(0, '@string.regexp.swift',         { fg = colors.redLight,  bg = 'NONE' })  -- Regex literals

  -- Numbers
  highlight(0, '@number.swift',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.swift',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.swift',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.swift',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.swift', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Modules
  highlight(0, '@module.swift',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@label.swift',                 { fg = colors.purple,    bg = 'NONE' })  -- Argument labels
  highlight(0, '@property.swift',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Operators and Punctuation
  highlight(0, '@operator.swift',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.swift',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.swift', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.swift',   { fg = colors.pink,      bg = 'NONE' })  -- \() interpolation


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.swift)

  highlight(0, '@lsp.type.variable.swift',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
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
  highlight(0, '@lsp.type.keyword.swift',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.swift',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.swift',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.swift',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.swift',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.swift',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.static.swift',       { fg = colors.purple,    bg = 'NONE' })  -- Static properties
  highlight(0, '@lsp.typemod.function.declaration.swift',  { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.method.static.swift',         { fg = colors.orange,    bg = 'NONE' })  -- Static methods
  highlight(0, '@lsp.typemod.class.declaration.swift',     { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
  highlight(0, '@lsp.typemod.struct.declaration.swift',    { fg = colors.turquoise, bg = 'NONE' })  -- Struct declarations
  highlight(0, '@lsp.typemod.protocol.declaration.swift',  { fg = colors.turquoise, bg = 'NONE' })  -- Protocol declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.swift',   { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return swift
