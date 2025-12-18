-------------------------------------------------------------------------------
-- CoffeeScript
-------------------------------------------------------------------------------

local colors       = require('mannydark.palette')
local highlight    = vim.api.nvim_set_hl
local coffeescript = {}


-------------------------------------------------------------------------------
-- Settings

coffeescript.setupHighlighting = function()


  -------------------------------------------------------------------------
  -- Vim CoffeeScript Syntax Groups (vim-coffee-script)
  -------------------------------------------------------------------------

  -- Statements and Control Flow
  highlight(0, 'coffeeStatement',         { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, throw
  highlight(0, 'coffeeRepeat',            { fg = colors.blue,       bg = 'NONE'            })  -- for, while, until, loop
  highlight(0, 'coffeeConditional',       { fg = colors.blue,       bg = 'NONE'            })  -- if, else, unless, switch, when, then
  highlight(0, 'coffeeException',         { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, finally
  highlight(0, 'coffeeKeyword',           { fg = colors.blue,       bg = 'NONE'            })  -- new, in, of, class, extends, super, yield, await, do, by

  -- Operators
  highlight(0, 'coffeeOperator',          { fg = colors.blue,       bg = 'NONE'            })  -- instanceof, typeof, delete, and, or, not, is, isnt
  highlight(0, 'coffeeExtendedOp',        { fg = colors.white,      bg = 'NONE'            })  -- Symbol operators: + - * / % = < > ! & | ^ ~
  highlight(0, 'coffeeSpecialOp',         { fg = colors.white,      bg = 'NONE'            })  -- Commas and semicolons
  highlight(0, 'coffeeAssign',            { fg = colors.white,      bg = 'NONE'            })  -- Assignment operators: = += -= *= /= etc.
  highlight(0, 'coffeeArrow',             { fg = colors.blue,       bg = 'NONE'            })  -- -> and =>
  highlight(0, 'coffeeFatArrow',          { fg = colors.blue,       bg = 'NONE'            })  -- => (fat arrow, bound function)
  highlight(0, 'coffeeThinArrow',         { fg = colors.blue,       bg = 'NONE'            })  -- -> (thin arrow, unbound function)
  highlight(0, 'coffeeRange',             { fg = colors.white,      bg = 'NONE'            })  -- .. and ... (range operators)
  highlight(0, 'coffeeSplat',             { fg = colors.pink,       bg = 'NONE'            })  -- ... (splat operator)
  highlight(0, 'coffeeExistential',       { fg = colors.pink,       bg = 'NONE'            })  -- ? (existential operator)
  highlight(0, 'coffeeSoak',              { fg = colors.pink,       bg = 'NONE'            })  -- ?. (soak/optional chaining)

  -- Constants and Literals
  highlight(0, 'coffeeBoolean',           { fg = colors.turquoise,  bg = 'NONE'            })  -- true, false, on, off, yes, no
  highlight(0, 'coffeeGlobal',            { fg = colors.turquoise,  bg = 'NONE'            })  -- null, undefined
  highlight(0, 'coffeeNull',              { fg = colors.turquoise,  bg = 'NONE'            })  -- null
  highlight(0, 'coffeeUndefined',         { fg = colors.turquoise,  bg = 'NONE'            })  -- undefined
  highlight(0, 'coffeeConstant',          { fg = colors.turquoise,  bg = 'NONE'            })  -- SCREAMING_CAPS constants

  -- Numbers
  highlight(0, 'coffeeNumber',            { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'coffeeFloat',             { fg = colors.greenLight, bg = 'NONE'            })  -- Floating-point numbers
  highlight(0, 'coffeeHexNumber',         { fg = colors.greenLight, bg = 'NONE'            })  -- Hexadecimal: 0x1A
  highlight(0, 'coffeeBinaryNumber',      { fg = colors.greenLight, bg = 'NONE'            })  -- Binary: 0b1010
  highlight(0, 'coffeeOctalNumber',       { fg = colors.greenLight, bg = 'NONE'            })  -- Octal: 0o17

  -- Strings
  highlight(0, 'coffeeString',            { fg = colors.redLight,   bg = 'NONE'            })  -- String literals (single and double-quoted)
  highlight(0, 'coffeeSingleString',      { fg = colors.redLight,   bg = 'NONE'            })  -- Single-quoted strings
  highlight(0, 'coffeeDoubleString',      { fg = colors.redLight,   bg = 'NONE'            })  -- Double-quoted strings
  highlight(0, 'coffeeHeredoc',           { fg = colors.redLight,   bg = 'NONE'            })  -- Heredoc strings (""" or ''')
  highlight(0, 'coffeeStringDelimiter',   { fg = colors.redLight,   bg = 'NONE'            })  -- String delimiters
  highlight(0, 'coffeeEscape',            { fg = colors.pink,       bg = 'NONE'            })  -- String escape sequences: \n, \t, etc.
  highlight(0, 'coffeeInterp',            { fg = colors.pink,       bg = 'NONE'            })  -- String interpolations: #{...}
  highlight(0, 'coffeeInterpDelim',       { fg = colors.pink,       bg = 'NONE'            })  -- Interpolation delimiters: #{ }

  -- Regular Expressions
  highlight(0, 'coffeeRegex',             { fg = colors.orange,     bg = 'NONE'            })  -- Regular expression literals: /pattern/
  highlight(0, 'coffeeHeregex',           { fg = colors.orange,     bg = 'NONE'            })  -- Multi-line regex (heredoc-style): ///pattern///
  highlight(0, 'coffeeHeregexComment',    { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments within heregex
  highlight(0, 'coffeeRegexFlags',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Regex flags: g, i, m, etc.
  highlight(0, 'coffeeRegexEscape',       { fg = colors.pink,       bg = 'NONE'            })  -- Regex escape sequences
  highlight(0, 'coffeeRegexCharClass',    { fg = colors.turquoise,  bg = 'NONE'            })  -- Character classes: [a-z]

  -- Variables and Identifiers
  highlight(0, 'coffeeSpecialVar',        { fg = colors.pink,       bg = 'NONE'            })  -- this, prototype, arguments
  highlight(0, 'coffeeThis',              { fg = colors.pink,       bg = 'NONE'            })  -- this
  highlight(0, 'coffeePrototype',         { fg = colors.pink,       bg = 'NONE'            })  -- prototype
  highlight(0, 'coffeeArguments',         { fg = colors.pink,       bg = 'NONE'            })  -- arguments
  highlight(0, 'coffeeSpecialIdent',      { fg = colors.purple,     bg = 'NONE'            })  -- Instance variables: @property
  highlight(0, 'coffeeAtSign',            { fg = colors.purple,     bg = 'NONE'            })  -- @ sign for instance variables
  highlight(0, 'coffeeIdentifier',        { fg = colors.white,      bg = 'NONE'            })  -- Regular identifiers
  highlight(0, 'coffeeVariable',          { fg = colors.white,      bg = 'NONE'            })  -- Variables

  -- Objects and Classes
  highlight(0, 'coffeeObject',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Capitalized identifiers (class-like names)
  highlight(0, 'coffeeClass',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names
  highlight(0, 'coffeeClassName',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Class name in class definition
  highlight(0, 'coffeeExtends',           { fg = colors.blue,       bg = 'NONE'            })  -- extends keyword
  highlight(0, 'coffeeSuper',             { fg = colors.pink,       bg = 'NONE'            })  -- super keyword
  highlight(0, 'coffeeObjAssign',         { fg = colors.blue,       bg = 'NONE'            })  -- Object property assignments: key:
  highlight(0, 'coffeeProperty',          { fg = colors.blue,       bg = 'NONE'            })  -- Object properties
  highlight(0, 'coffeeDotAccess',         { fg = colors.white,      bg = 'NONE'            })  -- Property access via dot notation
  highlight(0, 'coffeeProtoAccess',       { fg = colors.white,      bg = 'NONE'            })  -- Prototype access via :: operator

  -- Functions
  highlight(0, 'coffeeFunction',          { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions
  highlight(0, 'coffeeFunctionName',      { fg = colors.orange,     bg = 'NONE'            })  -- Function name
  highlight(0, 'coffeeMethod',            { fg = colors.orange,     bg = 'NONE'            })  -- Method definitions
  highlight(0, 'coffeeMethodName',        { fg = colors.orange,     bg = 'NONE'            })  -- Method name
  highlight(0, 'coffeeParam',             { fg = colors.white,      bg = 'NONE'            })  -- Function parameters
  highlight(0, 'coffeeParamDefault',      { fg = colors.white,      bg = 'NONE'            })  -- Default parameter values

  -- Embedded JavaScript
  highlight(0, 'coffeeEmbed',             { fg = colors.orange,     bg = 'NONE'            })  -- Embedded JavaScript code: `js code`
  highlight(0, 'coffeeEmbedDelim',        { fg = colors.orange,     bg = 'NONE'            })  -- Backticks for embedded JS

  -- Comments
  highlight(0, 'coffeeComment',           { fg = colors.gray,       bg = 'NONE', italic = true })  -- Single-line comments: #
  highlight(0, 'coffeeBlockComment',      { fg = colors.gray,       bg = 'NONE', italic = true })  -- Multi-line comments: ###...###
  highlight(0, 'coffeeTodo',              { fg = colors.pink,       bg = 'NONE', bold = true })  -- TODO, FIXME, XXX markers
  highlight(0, 'coffeeShebang',           { fg = colors.gray,       bg = 'NONE'            })  -- Shebang line: #!/usr/bin/env coffee

  -- Punctuation and Brackets
  highlight(0, 'coffeeParen',             { fg = colors.white,      bg = 'NONE'            })  -- Parentheses: ( )
  highlight(0, 'coffeeBracket',           { fg = colors.white,      bg = 'NONE'            })  -- Brackets: [ ]
  highlight(0, 'coffeeBrace',             { fg = colors.white,      bg = 'NONE'            })  -- Braces: { }
  highlight(0, 'coffeeCurlies',           { fg = colors.white,      bg = 'NONE'            })  -- Curly braces
  highlight(0, 'coffeeColon',             { fg = colors.white,      bg = 'NONE'            })  -- Colons
  highlight(0, 'coffeeSemicolon',         { fg = colors.white,      bg = 'NONE'            })  -- Semicolons (rarely used in CoffeeScript)

  -- Errors
  highlight(0, 'coffeeReservedError',     { fg = colors.white,      bg = colors.red        })  -- Reserved JavaScript keywords (error)
  highlight(0, 'coffeeSpaceError',        { fg = 'NONE',            bg = colors.red        })  -- Trailing whitespace errors
  highlight(0, 'coffeeSemicolonError',    { fg = 'NONE',            bg = colors.red        })  -- Trailing semicolon errors
  highlight(0, 'coffeeError',             { fg = colors.white,      bg = colors.red        })  -- Generic syntax errors

  -- Comprehensions
  highlight(0, 'coffeeComprehension',     { fg = colors.blue,       bg = 'NONE'            })  -- List comprehensions
  highlight(0, 'coffeeCompFor',           { fg = colors.blue,       bg = 'NONE'            })  -- for in comprehensions
  highlight(0, 'coffeeCompWhen',          { fg = colors.blue,       bg = 'NONE'            })  -- when clause in comprehensions
  highlight(0, 'coffeeCompBy',            { fg = colors.blue,       bg = 'NONE'            })  -- by clause in comprehensions

  -- Destructuring
  highlight(0, 'coffeeDestructure',       { fg = colors.purple,     bg = 'NONE'            })  -- Destructuring assignments
  highlight(0, 'coffeeDestructureArray',  { fg = colors.purple,     bg = 'NONE'            })  -- Array destructuring: [a, b] = arr
  highlight(0, 'coffeeDestructureObject', { fg = colors.purple,     bg = 'NONE'            })  -- Object destructuring: {a, b} = obj

  -- Built-in Objects and Functions
  highlight(0, 'coffeeBuiltinObject',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Array, Object, Function, String, Number, Boolean
  highlight(0, 'coffeeBuiltinFunction',   { fg = colors.orange,     bg = 'NONE'            })  -- console, Math, JSON, etc.
  highlight(0, 'coffeeConsole',           { fg = colors.turquoise,  bg = 'NONE'            })  -- console
  highlight(0, 'coffeeMath',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Math
  highlight(0, 'coffeeJSON',              { fg = colors.turquoise,  bg = 'NONE'            })  -- JSON
  highlight(0, 'coffeeDate',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Date
  highlight(0, 'coffeeRegExp',            { fg = colors.turquoise,  bg = 'NONE'            })  -- RegExp
  highlight(0, 'coffeePromise',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Promise
  highlight(0, 'coffeeArray',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Array
  highlight(0, 'coffeeMap',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Map
  highlight(0, 'coffeeSet',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Set

  -- Array/String Methods (commonly used in CoffeeScript)
  highlight(0, 'coffeeArrayMethod',       { fg = colors.orange,     bg = 'NONE'            })  -- map, filter, reduce, forEach, etc.
  highlight(0, 'coffeeStringMethod',      { fg = colors.orange,     bg = 'NONE'            })  -- split, join, replace, etc.


  -------------------------------------------------------------------------
  -- Treesitter CoffeeScript Captures (for future compatibility)
  -------------------------------------------------------------------------

  -- Note: CoffeeScript doesn't have official nvim-treesitter support yet
  -- These captures are defined for future compatibility if a parser is added

  -- Keywords
  highlight(0, '@keyword.coffee',                { fg = colors.blue,       bg = 'NONE'            })  -- Keywords
  highlight(0, '@keyword.function.coffee',       { fg = colors.blue,       bg = 'NONE'            })  -- Function keywords
  highlight(0, '@keyword.operator.coffee',       { fg = colors.blue,       bg = 'NONE'            })  -- Operator keywords (and, or, not, is, isnt)
  highlight(0, '@keyword.return.coffee',         { fg = colors.blue,       bg = 'NONE'            })  -- return
  highlight(0, '@keyword.conditional.coffee',    { fg = colors.blue,       bg = 'NONE'            })  -- if, else, unless, switch, when, then
  highlight(0, '@keyword.repeat.coffee',         { fg = colors.blue,       bg = 'NONE'            })  -- for, while, until, loop
  highlight(0, '@keyword.exception.coffee',      { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, finally
  highlight(0, '@keyword.import.coffee',         { fg = colors.blue,       bg = 'NONE'            })  -- import, from (CoffeeScript 2)
  highlight(0, '@keyword.export.coffee',         { fg = colors.blue,       bg = 'NONE'            })  -- export (CoffeeScript 2)

  -- Functions
  highlight(0, '@function.coffee',               { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, '@function.call.coffee',          { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, '@function.method.coffee',        { fg = colors.orange,     bg = 'NONE'            })  -- Method names
  highlight(0, '@function.method.call.coffee',   { fg = colors.orange,     bg = 'NONE'            })  -- Method calls
  highlight(0, '@function.builtin.coffee',       { fg = colors.orange,     bg = 'NONE'            })  -- Built-in functions

  -- Variables
  highlight(0, '@variable.coffee',               { fg = colors.white,      bg = 'NONE'            })  -- Variables
  highlight(0, '@variable.builtin.coffee',       { fg = colors.pink,       bg = 'NONE'            })  -- this, arguments
  highlight(0, '@variable.member.coffee',        { fg = colors.purple,     bg = 'NONE'            })  -- @property (instance variables)
  highlight(0, '@variable.parameter.coffee',     { fg = colors.white,      bg = 'NONE'            })  -- Function parameters

  -- Types and Classes
  highlight(0, '@type.coffee',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- Types
  highlight(0, '@type.builtin.coffee',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Built-in types
  highlight(0, '@constructor.coffee',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names (constructors)

  -- Properties
  highlight(0, '@property.coffee',               { fg = colors.blue,       bg = 'NONE'            })  -- Object properties
  highlight(0, '@field.coffee',                  { fg = colors.blue,       bg = 'NONE'            })  -- Object fields

  -- Constants
  highlight(0, '@constant.coffee',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Constants (SCREAMING_CAPS)
  highlight(0, '@constant.builtin.coffee',       { fg = colors.turquoise,  bg = 'NONE'            })  -- true, false, null, undefined
  highlight(0, '@boolean.coffee',                { fg = colors.turquoise,  bg = 'NONE'            })  -- Booleans

  -- Numbers
  highlight(0, '@number.coffee',                 { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, '@number.float.coffee',           { fg = colors.greenLight, bg = 'NONE'            })  -- Floating point numbers

  -- Strings
  highlight(0, '@string.coffee',                 { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
  highlight(0, '@string.escape.coffee',          { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences
  highlight(0, '@string.special.coffee',         { fg = colors.pink,       bg = 'NONE'            })  -- Interpolations
  highlight(0, '@string.regex.coffee',           { fg = colors.orange,     bg = 'NONE'            })  -- Regular expressions

  -- Comments
  highlight(0, '@comment.coffee',                { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments
  highlight(0, '@comment.block.coffee',          { fg = colors.gray,       bg = 'NONE', italic = true })  -- Block comments
  highlight(0, '@comment.todo.coffee',           { fg = colors.pink,       bg = 'NONE', bold = true })  -- TODO comments

  -- Operators
  highlight(0, '@operator.coffee',               { fg = colors.white,      bg = 'NONE'            })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.coffee',    { fg = colors.white,      bg = 'NONE'            })  -- Brackets: ( ) [ ] { }
  highlight(0, '@punctuation.delimiter.coffee',  { fg = colors.white,      bg = 'NONE'            })  -- Delimiters: , ; :
  highlight(0, '@punctuation.special.coffee',    { fg = colors.pink,       bg = 'NONE'            })  -- Special: @ # #{ }

  -- Embedded JavaScript
  highlight(0, '@embedded.coffee',               { fg = colors.orange,     bg = 'NONE'            })  -- Embedded JS code


  -------------------------------------------------------------------------
  -- LSP Semantic Tokens
  -------------------------------------------------------------------------

  highlight(0, '@lsp.type.class.coffee',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Classes
  highlight(0, '@lsp.type.function.coffee',      { fg = colors.orange,     bg = 'NONE'            })  -- Functions
  highlight(0, '@lsp.type.method.coffee',        { fg = colors.orange,     bg = 'NONE'            })  -- Methods
  highlight(0, '@lsp.type.property.coffee',      { fg = colors.blue,       bg = 'NONE'            })  -- Properties
  highlight(0, '@lsp.type.variable.coffee',      { fg = colors.white,      bg = 'NONE'            })  -- Variables
  highlight(0, '@lsp.type.parameter.coffee',     { fg = colors.white,      bg = 'NONE'            })  -- Parameters
  highlight(0, '@lsp.type.string.coffee',        { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
  highlight(0, '@lsp.type.number.coffee',        { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, '@lsp.type.keyword.coffee',       { fg = colors.blue,       bg = 'NONE'            })  -- Keywords
  highlight(0, '@lsp.type.operator.coffee',      { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, '@lsp.type.comment.coffee',       { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments

  highlight(0, '@lsp.mod.readonly.coffee',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Read-only variables
  highlight(0, '@lsp.mod.defaultLibrary.coffee', { fg = colors.turquoise,  bg = 'NONE'            })  -- Default library

end

return coffeescript
