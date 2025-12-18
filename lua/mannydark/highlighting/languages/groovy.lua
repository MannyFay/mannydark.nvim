-------------------------------------------------------------------------------
-- Groovy Files
-- Highlighting for .groovy, .gradle, .gvy, .gy, .gsh files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local groovy  = {}


-------------------------------------------------------------------------------
-- Settings

groovy.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'groovyKeyword',         { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'groovyStatement',       { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue
  highlight(0, 'groovyConditional',     { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch
  highlight(0, 'groovyRepeat',          { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'groovyLabel',           { fg = colors.blue,       bg = 'NONE'            })  -- case, default
  highlight(0, 'groovyBranch',          { fg = colors.blue,       bg = 'NONE'            })  -- break, continue

  -- Keywords - Exception Handling
  highlight(0, 'groovyException',       { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, finally, throw, throws
  highlight(0, 'groovyAssert',          { fg = colors.blue,       bg = 'NONE'            })  -- assert

  -- Keywords - OOP
  highlight(0, 'groovyClassDecl',       { fg = colors.blue,       bg = 'NONE'            })  -- class
  highlight(0, 'groovyInterfaceDecl',   { fg = colors.blue,       bg = 'NONE'            })  -- interface
  highlight(0, 'groovyEnumDecl',        { fg = colors.blue,       bg = 'NONE'            })  -- enum
  highlight(0, 'groovyTrait',           { fg = colors.blue,       bg = 'NONE'            })  -- trait
  highlight(0, 'groovyRecord',          { fg = colors.blue,       bg = 'NONE'            })  -- record
  highlight(0, 'groovyExtends',         { fg = colors.blue,       bg = 'NONE'            })  -- extends
  highlight(0, 'groovyImplements',      { fg = colors.blue,       bg = 'NONE'            })  -- implements
  highlight(0, 'groovyNew',             { fg = colors.blue,       bg = 'NONE'            })  -- new
  highlight(0, 'groovyInstanceof',      { fg = colors.blue,       bg = 'NONE'            })  -- instanceof
  highlight(0, 'groovyThis',            { fg = colors.blue,       bg = 'NONE'            })  -- this
  highlight(0, 'groovySuper',           { fg = colors.blue,       bg = 'NONE'            })  -- super

  -- Keywords - Groovy Specific
  highlight(0, 'groovyDef',             { fg = colors.blue,       bg = 'NONE'            })  -- def
  highlight(0, 'groovyVar',             { fg = colors.blue,       bg = 'NONE'            })  -- var
  highlight(0, 'groovyAs',              { fg = colors.blue,       bg = 'NONE'            })  -- as (type coercion)
  highlight(0, 'groovyIn',              { fg = colors.blue,       bg = 'NONE'            })  -- in (for-each, membership)

  -- Keywords - Modifiers
  highlight(0, 'groovyModifier',        { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected
  highlight(0, 'groovyStatic',          { fg = colors.blue,       bg = 'NONE'            })  -- static
  highlight(0, 'groovyFinal',           { fg = colors.blue,       bg = 'NONE'            })  -- final
  highlight(0, 'groovyAbstract',        { fg = colors.blue,       bg = 'NONE'            })  -- abstract
  highlight(0, 'groovyNative',          { fg = colors.blue,       bg = 'NONE'            })  -- native
  highlight(0, 'groovyTransient',       { fg = colors.blue,       bg = 'NONE'            })  -- transient
  highlight(0, 'groovyVolatile',        { fg = colors.blue,       bg = 'NONE'            })  -- volatile
  highlight(0, 'groovySynchronized',    { fg = colors.blue,       bg = 'NONE'            })  -- synchronized
  highlight(0, 'groovySealed',          { fg = colors.blue,       bg = 'NONE'            })  -- sealed, non-sealed, permits

  -- Keywords - Imports/Package
  highlight(0, 'groovyImport',          { fg = colors.pink,       bg = 'NONE'            })  -- import
  highlight(0, 'groovyPackage',         { fg = colors.pink,       bg = 'NONE'            })  -- package

  -- Types - Primitive
  highlight(0, 'groovyType',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'groovyPrimitiveType',   { fg = colors.turquoise,  bg = 'NONE'            })  -- int, long, float, double, boolean, char, byte, short
  highlight(0, 'groovyVoid',            { fg = colors.turquoise,  bg = 'NONE'            })  -- void

  -- Types - Groovy/Java Classes
  highlight(0, 'groovyBuiltinType',     { fg = colors.turquoise,  bg = 'NONE'            })  -- String, Integer, List, Map, etc.
  highlight(0, 'groovyGenericType',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Generic type parameters <T>
  highlight(0, 'groovyClassName',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names

  -- Functions/Methods
  highlight(0, 'groovyFunction',        { fg = colors.orange,     bg = 'NONE'            })  -- Function/method definitions
  highlight(0, 'groovyFunctionCall',    { fg = colors.orange,     bg = 'NONE'            })  -- Function/method calls
  highlight(0, 'groovyBuiltinFunc',     { fg = colors.orange,     bg = 'NONE'            })  -- println, print, etc.
  highlight(0, 'groovyConstructor',     { fg = colors.orange,     bg = 'NONE'            })  -- Constructor calls

  -- Variables
  highlight(0, 'groovyIdentifier',      { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'groovyParameter',       { fg = colors.purple,     bg = 'NONE'            })  -- Method parameters
  highlight(0, 'groovyField',           { fg = colors.purple,     bg = 'NONE'            })  -- Class fields
  highlight(0, 'groovyProperty',        { fg = colors.purple,     bg = 'NONE'            })  -- Properties

  -- Closure Implicit Variables
  highlight(0, 'groovyClosureVar',      { fg = colors.blue,       bg = 'NONE'            })  -- it, delegate, owner
  highlight(0, 'groovyIt',              { fg = colors.blue,       bg = 'NONE'            })  -- it (implicit closure param)
  highlight(0, 'groovyDelegate',        { fg = colors.blue,       bg = 'NONE'            })  -- delegate
  highlight(0, 'groovyOwner',           { fg = colors.blue,       bg = 'NONE'            })  -- owner

  -- Constants
  highlight(0, 'groovyConstant',        { fg = colors.purple,     bg = 'NONE'            })  -- Constants
  highlight(0, 'groovyBoolean',         { fg = colors.blue,       bg = 'NONE'            })  -- true, false
  highlight(0, 'groovyNull',            { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'groovyEnumConstant',    { fg = colors.purple,     bg = 'NONE'            })  -- Enum values

  -- Strings - Single Quoted (Java String)
  highlight(0, 'groovyString',          { fg = colors.redLight,   bg = 'NONE'            })  -- 'single quoted'
  highlight(0, 'groovyTripleString',    { fg = colors.redLight,   bg = 'NONE'            })  -- '''triple single'''

  -- Strings - Double Quoted (GString)
  highlight(0, 'groovyGString',         { fg = colors.redLight,   bg = 'NONE'            })  -- "double quoted"
  highlight(0, 'groovyTripleGString',   { fg = colors.redLight,   bg = 'NONE'            })  -- """triple double"""
  highlight(0, 'groovyInterpolation',   { fg = colors.pink,       bg = 'NONE'            })  -- ${...} interpolation
  highlight(0, 'groovyInterpolationBraces', { fg = colors.pink,   bg = 'NONE'            })  -- ${ }

  -- Strings - Slashy (Regex)
  highlight(0, 'groovySlashyString',    { fg = colors.redLight,   bg = 'NONE'            })  -- /slashy/
  highlight(0, 'groovyDollarSlashy',    { fg = colors.redLight,   bg = 'NONE'            })  -- $/dollar slashy/$
  highlight(0, 'groovyRegex',           { fg = colors.redLight,   bg = 'NONE'            })  -- Regex patterns

  -- String Escapes
  highlight(0, 'groovyEscape',          { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'groovyUnicodeEscape',   { fg = colors.pink,       bg = 'NONE'            })  -- \uXXXX

  -- Numbers
  highlight(0, 'groovyNumber',          { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'groovyInteger',         { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'groovyLong',            { fg = colors.greenLight, bg = 'NONE'            })  -- 123L
  highlight(0, 'groovyFloat',           { fg = colors.greenLight, bg = 'NONE'            })  -- 3.14f
  highlight(0, 'groovyDouble',          { fg = colors.greenLight, bg = 'NONE'            })  -- 3.14d
  highlight(0, 'groovyBigInteger',      { fg = colors.greenLight, bg = 'NONE'            })  -- 123G
  highlight(0, 'groovyBigDecimal',      { fg = colors.greenLight, bg = 'NONE'            })  -- 3.14G
  highlight(0, 'groovyHex',             { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF
  highlight(0, 'groovyOctal',           { fg = colors.greenLight, bg = 'NONE'            })  -- 0777
  highlight(0, 'groovyBinary',          { fg = colors.greenLight, bg = 'NONE'            })  -- 0b1010

  -- Operators - Standard
  highlight(0, 'groovyOperator',        { fg = colors.white,      bg = 'NONE'            })  -- + - * / % = < >
  highlight(0, 'groovyAssignment',      { fg = colors.white,      bg = 'NONE'            })  -- = += -= *= /= %=

  -- Operators - Groovy Special
  highlight(0, 'groovySafeNav',         { fg = colors.white,      bg = 'NONE'            })  -- ?. safe navigation
  highlight(0, 'groovySpread',          { fg = colors.white,      bg = 'NONE'            })  -- *. spread
  highlight(0, 'groovySpreadMap',       { fg = colors.white,      bg = 'NONE'            })  -- *: spread map
  highlight(0, 'groovyElvis',           { fg = colors.white,      bg = 'NONE'            })  -- ?: Elvis
  highlight(0, 'groovyElvisAssign',     { fg = colors.white,      bg = 'NONE'            })  -- ?= Elvis assignment
  highlight(0, 'groovySpaceship',       { fg = colors.white,      bg = 'NONE'            })  -- <=> spaceship
  highlight(0, 'groovyRange',           { fg = colors.white,      bg = 'NONE'            })  -- .. range
  highlight(0, 'groovyRangeExcl',       { fg = colors.white,      bg = 'NONE'            })  -- ..< exclusive range
  highlight(0, 'groovySafeIndex',       { fg = colors.white,      bg = 'NONE'            })  -- ?[] safe index

  -- Operators - Regex
  highlight(0, 'groovyFind',            { fg = colors.white,      bg = 'NONE'            })  -- =~ find
  highlight(0, 'groovyMatch',           { fg = colors.white,      bg = 'NONE'            })  -- ==~ match
  highlight(0, 'groovyPattern',         { fg = colors.white,      bg = 'NONE'            })  -- ~ pattern

  -- Operators - Other
  highlight(0, 'groovyTernary',         { fg = colors.white,      bg = 'NONE'            })  -- ? : ternary
  highlight(0, 'groovyMemberAccess',    { fg = colors.white,      bg = 'NONE'            })  -- . member access
  highlight(0, 'groovyMethodRef',       { fg = colors.white,      bg = 'NONE'            })  -- .& method reference
  highlight(0, 'groovyMethodPointer',   { fg = colors.white,      bg = 'NONE'            })  -- .& method pointer

  -- Closures
  highlight(0, 'groovyClosure',         { fg = colors.white,      bg = 'NONE'            })  -- { } closure braces
  highlight(0, 'groovyClosureArrow',    { fg = colors.white,      bg = 'NONE'            })  -- -> arrow

  -- Annotations
  highlight(0, 'groovyAnnotation',      { fg = colors.pink,       bg = 'NONE'            })  -- @Annotation
  highlight(0, 'groovyAnnotationName',  { fg = colors.pink,       bg = 'NONE'            })  -- Annotation name

  -- Common Annotations
  highlight(0, 'groovyCompileStatic',   { fg = colors.pink,       bg = 'NONE'            })  -- @CompileStatic
  highlight(0, 'groovyTypeChecked',     { fg = colors.pink,       bg = 'NONE'            })  -- @TypeChecked
  highlight(0, 'groovyGrab',            { fg = colors.pink,       bg = 'NONE'            })  -- @Grab, @Grapes
  highlight(0, 'groovyImmutable',       { fg = colors.pink,       bg = 'NONE'            })  -- @Immutable
  highlight(0, 'groovySingleton',       { fg = colors.pink,       bg = 'NONE'            })  -- @Singleton
  highlight(0, 'groovyDelegate',        { fg = colors.pink,       bg = 'NONE'            })  -- @Delegate
  highlight(0, 'groovyLazy',            { fg = colors.pink,       bg = 'NONE'            })  -- @Lazy
  highlight(0, 'groovyBuilder',         { fg = colors.pink,       bg = 'NONE'            })  -- @Builder
  highlight(0, 'groovyCanonical',       { fg = colors.pink,       bg = 'NONE'            })  -- @Canonical

  -- Comments
  highlight(0, 'groovyComment',         { fg = colors.red,        bg = 'NONE'            })  -- // and /* */ comments
  highlight(0, 'groovyLineComment',     { fg = colors.red,        bg = 'NONE'            })  -- // comments
  highlight(0, 'groovyBlockComment',    { fg = colors.red,        bg = 'NONE'            })  -- /* */ comments
  highlight(0, 'groovyDocComment',      { fg = colors.red,        bg = 'NONE'            })  -- /** */ Groovydoc
  highlight(0, 'groovyDocTag',          { fg = colors.green,      bg = 'NONE'            })  -- @param, @return, etc.
  highlight(0, 'groovyTodo',            { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Shebang
  highlight(0, 'groovyShebang',         { fg = colors.red,        bg = 'NONE'            })  -- #!/usr/bin/env groovy

  -- Error
  highlight(0, 'groovyError',           { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.groovy)

  -- Variables
  highlight(0, '@variable.groovy',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.groovy',      { fg = colors.blue,      bg = 'NONE' })  -- this, super, it, delegate, owner
  highlight(0, '@variable.parameter.groovy',    { fg = colors.purple,    bg = 'NONE' })  -- Method parameters
  highlight(0, '@variable.member.groovy',       { fg = colors.purple,    bg = 'NONE' })  -- Fields

  -- Constants
  highlight(0, '@constant.groovy',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.groovy',      { fg = colors.blue,      bg = 'NONE' })  -- true, false, null

  -- Functions/Methods
  highlight(0, '@function.groovy',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.groovy',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.groovy',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.method.groovy',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.macro.groovy',        { fg = colors.pink,      bg = 'NONE' })  -- AST macros
  highlight(0, '@constructor.groovy',           { fg = colors.orange,    bg = 'NONE' })  -- Constructors

  -- Types
  highlight(0, '@type.groovy',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.groovy',          { fg = colors.turquoise, bg = 'NONE' })  -- int, String, List, etc.
  highlight(0, '@type.definition.groovy',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier.groovy',        { fg = colors.blue,      bg = 'NONE' })  -- final, static

  -- Keywords
  highlight(0, '@keyword.groovy',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.type.groovy',          { fg = colors.blue,      bg = 'NONE' })  -- class, interface, enum, trait
  highlight(0, '@keyword.modifier.groovy',      { fg = colors.blue,      bg = 'NONE' })  -- public, private, static, final
  highlight(0, '@keyword.function.groovy',      { fg = colors.blue,      bg = 'NONE' })  -- def, return
  highlight(0, '@keyword.operator.groovy',      { fg = colors.blue,      bg = 'NONE' })  -- instanceof, as, in
  highlight(0, '@keyword.return.groovy',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.repeat.groovy',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, do
  highlight(0, '@keyword.conditional.groovy',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, switch, case
  highlight(0, '@keyword.conditional.ternary.groovy', { fg = colors.white, bg = 'NONE' })  -- ? : ternary
  highlight(0, '@keyword.exception.groovy',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, finally, throw
  highlight(0, '@keyword.import.groovy',        { fg = colors.pink,      bg = 'NONE' })  -- import, package
  highlight(0, '@keyword.directive.groovy',     { fg = colors.pink,      bg = 'NONE' })  -- Shebang

  -- Strings
  highlight(0, '@string.groovy',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.groovy',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.groovy',        { fg = colors.pink,      bg = 'NONE' })  -- Interpolation ${}, slashy
  highlight(0, '@string.regex.groovy',          { fg = colors.redLight,  bg = 'NONE' })  -- Regex patterns
  highlight(0, '@character.groovy',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals
  highlight(0, '@character.special.groovy',     { fg = colors.pink,      bg = 'NONE' })  -- Special characters

  -- Numbers
  highlight(0, '@number.groovy',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.groovy',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.groovy',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.groovy',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.groovy', { fg = colors.red,       bg = 'NONE' })  -- Groovydoc

  -- Labels
  highlight(0, '@label.groovy',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels

  -- Annotations/Attributes
  highlight(0, '@attribute.groovy',             { fg = colors.pink,      bg = 'NONE' })  -- @Annotation

  -- Properties
  highlight(0, '@property.groovy',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Operators and Punctuation
  highlight(0, '@operator.groovy',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.groovy',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.groovy', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.groovy',   { fg = colors.pink,      bg = 'NONE' })  -- $ in interpolation


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.groovy)

  highlight(0, '@lsp.type.variable.groovy',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.groovy',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.groovy',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.groovy',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.groovy',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.type.groovy',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.groovy',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.interface.groovy',     { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.enum.groovy',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.groovy',    { fg = colors.purple,    bg = 'NONE' })  -- Enum values
  highlight(0, '@lsp.type.namespace.groovy',     { fg = colors.turquoise, bg = 'NONE' })  -- Packages
  highlight(0, '@lsp.type.typeParameter.groovy', { fg = colors.turquoise, bg = 'NONE' })  -- Generic types <T>
  highlight(0, '@lsp.type.keyword.groovy',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.groovy',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.groovy',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.groovy',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.groovy',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.groovy',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.decorator.groovy',     { fg = colors.pink,      bg = 'NONE' })  -- Annotations

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.groovy',    { fg = colors.purple,    bg = 'NONE' })  -- final variables
  highlight(0, '@lsp.typemod.variable.static.groovy',      { fg = colors.purple,    bg = 'NONE' })  -- static variables
  highlight(0, '@lsp.typemod.function.declaration.groovy', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.method.declaration.groovy',   { fg = colors.orange,    bg = 'NONE' })  -- Method declarations
  highlight(0, '@lsp.typemod.method.static.groovy',        { fg = colors.orange,    bg = 'NONE' })  -- Static methods
  highlight(0, '@lsp.typemod.type.declaration.groovy',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.groovy',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@lsp.typemod.class.abstract.groovy',       { fg = colors.turquoise, bg = 'NONE' })  -- Abstract classes
end

return groovy
