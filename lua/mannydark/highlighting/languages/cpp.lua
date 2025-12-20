-------------------------------------------------------------------------------
-- C++ Files
-- Highlighting for .cpp, .cc, .cxx, .hpp, .hh, .hxx, .h files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local cpp     = {}


-------------------------------------------------------------------------------
-- Settings

cpp.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'cppStatement',        { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, goto
  highlight(0, 'cppConditional',      { link = "Conditional" })  -- if, else, switch
  highlight(0, 'cppRepeat',           { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'cppLabel',            { fg = colors.blue,       bg = 'NONE'            })  -- case, default
  highlight(0, 'cppOperator',         { link = "Operator" })  -- sizeof, typeid, alignof, decltype, noexcept
  highlight(0, 'cppStorageClass',     { fg = colors.blue,       bg = 'NONE'            })  -- static, extern, mutable, thread_local, register
  highlight(0, 'cppStructure',        { fg = colors.blue,       bg = 'NONE'            })  -- class, struct, union, enum, typedef, typename
  highlight(0, 'cppAccess',           { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected
  highlight(0, 'cppModifier',         { fg = colors.blue,       bg = 'NONE'            })  -- const, volatile, constexpr, consteval, constinit, inline, virtual, explicit, final, override

  -- OOP Keywords
  highlight(0, 'cppClass',            { fg = colors.blue,       bg = 'NONE'            })  -- class
  highlight(0, 'cppVirtual',          { fg = colors.blue,       bg = 'NONE'            })  -- virtual
  highlight(0, 'cppOverride',         { fg = colors.blue,       bg = 'NONE'            })  -- override
  highlight(0, 'cppFinal',            { fg = colors.blue,       bg = 'NONE'            })  -- final
  highlight(0, 'cppExplicit',         { fg = colors.blue,       bg = 'NONE'            })  -- explicit
  highlight(0, 'cppFriend',           { fg = colors.blue,       bg = 'NONE'            })  -- friend
  highlight(0, 'cppInheritance',      { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected (inheritance)

  -- Memory Management
  highlight(0, 'cppNew',              { fg = colors.blue,       bg = 'NONE'            })  -- new
  highlight(0, 'cppDelete',           { fg = colors.blue,       bg = 'NONE'            })  -- delete

  -- Templates
  highlight(0, 'cppTemplate',         { fg = colors.blue,       bg = 'NONE'            })  -- template
  highlight(0, 'cppTypename',         { link = "Type" })  -- typename

  -- Namespaces
  highlight(0, 'cppNamespace',        { fg = colors.blue,       bg = 'NONE'            })  -- namespace
  highlight(0, 'cppUsing',            { fg = colors.blue,       bg = 'NONE'            })  -- using

  -- Exceptions
  highlight(0, 'cppException',        { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, throw
  highlight(0, 'cppNoexcept',         { fg = colors.blue,       bg = 'NONE'            })  -- noexcept

  -- Casts
  highlight(0, 'cppCast',             { fg = colors.blue,       bg = 'NONE'            })  -- static_cast, dynamic_cast, const_cast, reinterpret_cast

  -- C++11/14/17/20/23 Keywords
  highlight(0, 'cppAuto',             { fg = colors.blue,       bg = 'NONE'            })  -- auto
  highlight(0, 'cppDecltype',         { fg = colors.blue,       bg = 'NONE'            })  -- decltype
  highlight(0, 'cppConstexpr',        { fg = colors.blue,       bg = 'NONE'            })  -- constexpr, consteval, constinit
  highlight(0, 'cppLambda',           { fg = colors.blue,       bg = 'NONE'            })  -- Lambda-related
  highlight(0, 'cppConcept',          { fg = colors.blue,       bg = 'NONE'            })  -- concept, requires (C++20)
  highlight(0, 'cppCoroutine',        { fg = colors.blue,       bg = 'NONE'            })  -- co_await, co_yield, co_return (C++20)
  highlight(0, 'cppModule',           { fg = colors.blue,       bg = 'NONE'            })  -- module, import, export (C++20)

  -- Types
  highlight(0, 'cppType',             { link = "Type" })  -- int, char, float, double, void, bool, wchar_t, char8_t, char16_t, char32_t
  highlight(0, 'cppTypedef',          { link = "Type" })  -- Typedef'd types
  highlight(0, 'cppClassName',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names
  highlight(0, 'cppStructName',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Struct names
  highlight(0, 'cppEnumName',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum names
  highlight(0, 'cppTemplateParam',    { fg = colors.turquoise,  bg = 'NONE'            })  -- Template parameters

  -- STL Types
  highlight(0, 'cppSTLtype',          { fg = colors.turquoise,  bg = 'NONE'            })  -- vector, map, string, etc.
  highlight(0, 'cppSTLcontainer',     { fg = colors.turquoise,  bg = 'NONE'            })  -- STL containers
  highlight(0, 'cppSTLiterator',      { fg = colors.turquoise,  bg = 'NONE'            })  -- STL iterators
  highlight(0, 'cppSmartPtr',         { fg = colors.turquoise,  bg = 'NONE'            })  -- unique_ptr, shared_ptr, weak_ptr

  -- Constants
  highlight(0, 'cppConstant',         { link = "Constant" })  -- nullptr, true, false, NULL
  highlight(0, 'cppBoolean',          { link = "Boolean" })  -- true, false
  highlight(0, 'cppNullptr',          { fg = colors.blue,       bg = 'NONE'            })  -- nullptr
  highlight(0, 'cppThis',             { fg = colors.blue,       bg = 'NONE'            })  -- this

  -- Functions
  highlight(0, 'cppFunction',         { link = "Function" })  -- Function names
  highlight(0, 'cppMethod',           { link = "Function" })  -- Method names
  highlight(0, 'cppConstructor',      { fg = colors.orange,     bg = 'NONE'            })  -- Constructors
  highlight(0, 'cppDestructor',       { fg = colors.orange,     bg = 'NONE'            })  -- Destructors (~ClassName)
  highlight(0, 'cppOperatorOverload', { link = "Operator" })  -- operator+, operator<<, etc.
  highlight(0, 'cppSTLfunction',      { fg = colors.orange,     bg = 'NONE'            })  -- STL functions/algorithms

  -- Variables
  highlight(0, 'cppIdentifier',       { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'cppParameter',        { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'cppMember',           { fg = colors.purple,     bg = 'NONE'            })  -- Class/struct members

  -- Preprocessor (inherited from C)
  highlight(0, 'cppPreProc',          { fg = colors.pink,       bg = 'NONE'            })  -- General preprocessor
  highlight(0, 'cppInclude',          { fg = colors.pink,       bg = 'NONE'            })  -- #include
  highlight(0, 'cppDefine',           { fg = colors.pink,       bg = 'NONE'            })  -- #define
  highlight(0, 'cppMacro',            { fg = colors.pink,       bg = 'NONE'            })  -- Macro names
  highlight(0, 'cppPreCondit',        { fg = colors.pink,       bg = 'NONE'            })  -- #if, #ifdef, etc.
  highlight(0, 'cppIncluded',         { fg = colors.redLight,   bg = 'NONE'            })  -- <header> or "header"

  -- Attributes (C++11+)
  highlight(0, 'cppAttribute',        { fg = colors.pink,       bg = 'NONE'            })  -- [[nodiscard]], [[deprecated]], [[maybe_unused]], etc.

  -- Strings
  highlight(0, 'cppString',           { link = "String" })  -- "strings"
  highlight(0, 'cppRawString',        { link = "String" })  -- R"(raw strings)"
  highlight(0, 'cppCharacter',        { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'cppSpecialChar',      { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'cppFormat',           { fg = colors.pink,       bg = 'NONE'            })  -- Format specifiers

  -- Numbers
  highlight(0, 'cppNumber',           { link = "Number" })  -- Numbers
  highlight(0, 'cppFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'cppBinary',           { fg = colors.greenLight, bg = 'NONE'            })  -- 0b1010 binary literals
  highlight(0, 'cppHex',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF hex
  highlight(0, 'cppOctal',            { fg = colors.greenLight, bg = 'NONE'            })  -- 0777 octal

  -- Operators
  highlight(0, 'cppOperatorSign',     { link = "Operator" })  -- + - * / % = < > ! & | ^ ~ ? :
  highlight(0, 'cppPointerOperator',  { link = "Operator" })  -- * & pointer operators
  highlight(0, 'cppReference',        { fg = colors.white,      bg = 'NONE'            })  -- & reference, && rvalue reference
  highlight(0, 'cppMemberAccess',     { fg = colors.white,      bg = 'NONE'            })  -- . -> .* ->* member access
  highlight(0, 'cppScope',            { fg = colors.white,      bg = 'NONE'            })  -- :: scope resolution

  -- Comments
  highlight(0, 'cppComment',          { link = "Comment" })  -- /* */ and // comments
  highlight(0, 'cppCommentL',         { link = "Comment" })  -- // line comments
  highlight(0, 'cppTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Doxygen
  highlight(0, 'cppDoxygenComment',   { link = "Comment" })  -- Doxygen comments
  highlight(0, 'cppDoxygenTag',       { fg = colors.green,      bg = 'NONE'            })  -- @param, @return, @brief, etc.
  highlight(0, 'cppDoxygenParam',     { fg = colors.purple,     bg = 'NONE'            })  -- Parameter names in docs


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.cpp)

  -- Variables
  highlight(0, '@variable.cpp',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.cpp',      { link = "Variable" })  -- this
  highlight(0, '@variable.parameter.cpp',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.cpp',       { link = "Variable" })  -- Class/struct members

  -- Constants
  highlight(0, '@constant.cpp',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.cpp',      { link = "Constant" })  -- nullptr, true, false
  highlight(0, '@constant.macro.cpp',        { link = "Constant" })  -- Macro constants

  -- Functions
  highlight(0, '@function.cpp',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.cpp',         { link = "Function" })  -- Function calls
  highlight(0, '@function.method.cpp',       { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.cpp',  { link = "Function" })  -- Method calls
  highlight(0, '@function.builtin.cpp',      { link = "Function" })  -- Built-in functions
  highlight(0, '@function.macro.cpp',        { link = "Function" })  -- Macro functions
  highlight(0, '@constructor.cpp',           { fg = colors.turquoise, bg = 'NONE' })  -- Constructors

  -- Types
  highlight(0, '@type.cpp',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.cpp',          { link = "Type" })  -- int, char, bool, auto, etc.
  highlight(0, '@type.definition.cpp',       { link = "Type" })  -- typedef/using definitions
  highlight(0, '@type.qualifier.cpp',        { link = "Type" })  -- const, volatile, mutable

  -- Attributes
  highlight(0, '@attribute.cpp',             { fg = colors.pink,      bg = 'NONE' })  -- [[attributes]]

  -- Keywords
  highlight(0, '@keyword.cpp',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.cpp',      { link = "Keyword" })  -- (not really used)
  highlight(0, '@keyword.type.cpp',          { link = "Keyword" })  -- class, struct, union, enum, typedef, typename
  highlight(0, '@keyword.modifier.cpp',      { link = "Keyword" })  -- static, const, virtual, override, final, explicit
  highlight(0, '@keyword.return.cpp',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.import.cpp',        { link = "Keyword" })  -- import (C++20 modules)
  highlight(0, '@keyword.repeat.cpp',        { link = "Keyword" })  -- for, while, do
  highlight(0, '@keyword.conditional.cpp',   { link = "Conditional" })  -- if, else, switch, case
  highlight(0, '@keyword.exception.cpp',     { link = "Keyword" })  -- try, catch, throw, noexcept
  highlight(0, '@keyword.operator.cpp',      { link = "Operator" })  -- new, delete, sizeof, typeid, alignof
  highlight(0, '@keyword.coroutine.cpp',     { link = "Keyword" })  -- co_await, co_yield, co_return
  highlight(0, '@keyword.directive.cpp',     { link = "Keyword" })  -- Preprocessor directives

  -- Preprocessor
  highlight(0, '@preproc.cpp',               { fg = colors.pink,      bg = 'NONE' })  -- Preprocessor directives
  highlight(0, '@include.cpp',               { fg = colors.pink,      bg = 'NONE' })  -- #include
  highlight(0, '@define.cpp',                { fg = colors.pink,      bg = 'NONE' })  -- #define

  -- Strings
  highlight(0, '@string.cpp',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.cpp',         { link = "String" })  -- Escape sequences
  highlight(0, '@character.cpp',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.cpp',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.cpp',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.cpp',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.cpp',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.cpp', { link = "Comment" })  -- Doxygen comments

  -- Labels and Namespaces
  highlight(0, '@label.cpp',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels
  highlight(0, '@module.cpp',                { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@property.cpp',              { fg = colors.purple,    bg = 'NONE' })  -- Class members

  -- Operators and Punctuation
  highlight(0, '@operator.cpp',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.cpp',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.cpp', { link = "Delimiter" })  -- , ; : ::
  highlight(0, '@punctuation.special.cpp',   { fg = colors.pink,      bg = 'NONE' })  -- # in preprocessor


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.cpp)

  highlight(0, '@lsp.type.variable.cpp',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.cpp',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.cpp',      { fg = colors.purple,    bg = 'NONE' })  -- Class members
  highlight(0, '@lsp.type.function.cpp',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.cpp',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.macro.cpp',         { fg = colors.pink,      bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.class.cpp',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.struct.cpp',        { fg = colors.turquoise, bg = 'NONE' })  -- Structs
  highlight(0, '@lsp.type.enum.cpp',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.cpp',    { fg = colors.purple,    bg = 'NONE' })  -- Enum constants
  highlight(0, '@lsp.type.type.cpp',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.typeParameter.cpp', { fg = colors.turquoise, bg = 'NONE' })  -- Template parameters
  highlight(0, '@lsp.type.namespace.cpp',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.concept.cpp',       { fg = colors.turquoise, bg = 'NONE' })  -- Concepts (C++20)
  highlight(0, '@lsp.type.keyword.cpp',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.cpp',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.cpp',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.cpp',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.cpp',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.cpp',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.cpp',      { link = "Variable" })  -- const variables
  highlight(0, '@lsp.typemod.variable.static.cpp',        { link = "Variable" })  -- static members
  highlight(0, '@lsp.typemod.variable.globalScope.cpp',   { link = "Variable" })  -- global variables
  highlight(0, '@lsp.typemod.function.declaration.cpp',   { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.virtual.cpp',       { fg = colors.orange,    bg = 'NONE' })  -- Virtual functions
  highlight(0, '@lsp.typemod.function.static.cpp',        { fg = colors.orange,    bg = 'NONE' })  -- Static functions
  highlight(0, '@lsp.typemod.method.virtual.cpp',         { fg = colors.orange,    bg = 'NONE' })  -- Virtual methods
  highlight(0, '@lsp.typemod.class.declaration.cpp',      { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
  highlight(0, '@lsp.typemod.class.abstract.cpp',         { fg = colors.turquoise, bg = 'NONE' })  -- Abstract classes
  highlight(0, '@lsp.typemod.type.defaultLibrary.cpp',    { fg = colors.turquoise, bg = 'NONE' })  -- STL types
  highlight(0, '@lsp.typemod.typeParameter.declaration.cpp', { fg = colors.turquoise, bg = 'NONE' }) -- Template param declarations
end

return cpp
