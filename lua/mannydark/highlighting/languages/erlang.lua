-------------------------------------------------------------------------------
-- Erlang
-- Highlighting for .erl, .hrl, .app, .app.src, .escript files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local erlang    = {}


-------------------------------------------------------------------------------
-- Settings

erlang.setupHighlighting = function()


  ---------------------------------------------------------------------------
  -- Vim Legacy Syntax Groups (vim-erlang-runtime / vim runtime)
  ---------------------------------------------------------------------------

  -- Keywords - Control Flow
  highlight(0, 'erlangKeyword',           { link = "Keyword" })  -- case, of, end, if, receive, after, begin, catch, try, fun, when, query
  highlight(0, 'erlangConditional',       { link = "Conditional" })  -- if, case, of
  highlight(0, 'erlangException',         { fg = colors.blue,       bg = 'NONE' })  -- try, catch, throw, error, exit

  -- Booleans
  highlight(0, 'erlangBoolean',           { link = "Boolean" })  -- true, false

  -- Atoms
  highlight(0, 'erlangAtom',              { fg = colors.pink,       bg = 'NONE' })  -- atoms (lowercase identifiers)
  highlight(0, 'erlangQuotedAtom',        { fg = colors.pink,       bg = 'NONE' })  -- 'quoted atoms'
  highlight(0, 'erlangQuotedAtomModifier', { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences in quoted atoms

  -- Variables
  highlight(0, 'erlangVariable',          { link = "Variable" })  -- Variables (Uppercase start)
  highlight(0, 'erlangAnonymousVariable', { link = "Variable" })  -- _ anonymous variable

  -- Functions
  highlight(0, 'erlangLocalFuncCall',     { link = "Function" })  -- Local function calls
  highlight(0, 'erlangLocalFuncRef',      { link = "Function" })  -- fun local/arity
  highlight(0, 'erlangGlobalFuncCall',    { link = "Function" })  -- module:function() calls
  highlight(0, 'erlangGlobalFuncRef',     { link = "Function" })  -- fun module:func/arity
  highlight(0, 'erlangFunction',          { link = "Function" })  -- Function names
  highlight(0, 'erlangFun',               { fg = colors.blue,       bg = 'NONE' })  -- fun keyword

  -- Built-in Functions (BIFs)
  highlight(0, 'erlangBIF',               { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions (is_atom, self, spawn, etc.)

  -- Modules
  highlight(0, 'erlangModule',            { fg = colors.turquoise,  bg = 'NONE' })  -- Module names

  -- Numbers
  highlight(0, 'erlangNumberInteger',     { link = "Number" })  -- Integers (including base#value)
  highlight(0, 'erlangNumberFloat',       { link = "Number" })  -- Floats
  highlight(0, 'erlangNumber',            { link = "Number" })  -- General numbers

  -- Strings
  highlight(0, 'erlangString',            { link = "String" })  -- "strings"
  highlight(0, 'erlangStringTripleQuoted', { link = "String" })  -- """triple quoted"""
  highlight(0, 'erlangStringModifier',    { link = "String" })  -- Escape sequences in strings
  highlight(0, 'erlangModifier',          { fg = colors.pink,       bg = 'NONE' })  -- General escape sequences

  -- Characters
  highlight(0, 'erlangCharacter',         { fg = colors.redLight,   bg = 'NONE' })  -- $c character literals

  -- Operators
  highlight(0, 'erlangOperator',          { link = "Operator" })  -- Operators (+, -, *, /, div, rem, band, bor, bxor, bnot, bsl, bsr, not, and, or, xor, orelse, andalso)
  highlight(0, 'erlangEqualsBinary',      { fg = colors.white,      bg = 'NONE' })  -- =:=, =/= (exact equality)
  highlight(0, 'erlangCompareOperator',   { link = "Operator" })  -- ==, /=, <, >, =<, >=
  highlight(0, 'erlangListOperator',      { link = "Operator" })  -- ++, -- (list operations)
  highlight(0, 'erlangSendOperator',      { link = "Operator" })  -- ! (send message)
  highlight(0, 'erlangMatchOperator',     { link = "Operator" })  -- = (pattern match)

  -- Arrows
  highlight(0, 'erlangRightArrow',        { fg = colors.white,      bg = 'NONE' })  -- ->
  highlight(0, 'erlangDoubleArrow',       { fg = colors.white,      bg = 'NONE' })  -- =>
  highlight(0, 'erlangLeftArrow',         { fg = colors.white,      bg = 'NONE' })  -- <-
  highlight(0, 'erlangListComprehension', { fg = colors.white,      bg = 'NONE' })  -- <- || in comprehensions

  -- Delimiters/Brackets
  highlight(0, 'erlangBracket',           { fg = colors.white,      bg = 'NONE' })  -- (), [], {}
  highlight(0, 'erlangPipe',              { fg = colors.white,      bg = 'NONE' })  -- | (list tail)
  highlight(0, 'erlangDelimiter',         { link = "Delimiter" })  -- , ; . :
  highlight(0, 'erlangSemicolon',         { fg = colors.white,      bg = 'NONE' })  -- ;
  highlight(0, 'erlangComma',             { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'erlangDot',               { fg = colors.white,      bg = 'NONE' })  -- .

  -- Records
  highlight(0, 'erlangRecord',            { fg = colors.pink,       bg = 'NONE' })  -- #record
  highlight(0, 'erlangQuotedRecord',      { fg = colors.pink,       bg = 'NONE' })  -- #'quoted record'
  highlight(0, 'erlangRecordDef',         { fg = colors.pink,       bg = 'NONE' })  -- -record(name, {...})

  -- Maps
  highlight(0, 'erlangMap',               { fg = colors.pink,       bg = 'NONE' })  -- #{}
  highlight(0, 'erlangMapKey',            { fg = colors.pink,       bg = 'NONE' })  -- Map keys

  -- Macros
  highlight(0, 'erlangMacro',             { fg = colors.pink,       bg = 'NONE' })  -- ?MACRO
  highlight(0, 'erlangQuotedMacro',       { fg = colors.pink,       bg = 'NONE' })  -- ?'quoted macro'
  highlight(0, 'erlangDefine',            { fg = colors.pink,       bg = 'NONE' })  -- -define(MACRO, ...)

  -- Bitstrings/Binaries
  highlight(0, 'erlangBinary',            { fg = colors.white,      bg = 'NONE' })  -- << >>
  highlight(0, 'erlangBitType',           { link = "Type" })  -- binary, integer, float, utf8, utf16, utf32, signed, unsigned, big, little, native
  highlight(0, 'erlangBitSize',           { fg = colors.greenLight, bg = 'NONE' })  -- Size specifications

  -- Attributes/Directives
  highlight(0, 'erlangAttribute',         { fg = colors.pink,       bg = 'NONE' })  -- -module, -export, -import, -compile, -vsn, -author, -copyright, -on_load
  highlight(0, 'erlangDocAttribute',      { fg = colors.pink,       bg = 'NONE' })  -- -doc attribute (OTP 27+)
  highlight(0, 'erlangUnknownAttribute',  { fg = colors.pink,       bg = 'NONE' })  -- Unknown/custom attributes
  highlight(0, 'erlangInclude',           { fg = colors.pink,       bg = 'NONE' })  -- -include, -include_lib
  highlight(0, 'erlangPreCondit',         { fg = colors.pink,       bg = 'NONE' })  -- -ifdef, -ifndef, -else, -endif, -undef

  -- Type Specifications
  highlight(0, 'erlangType',              { link = "Type" })  -- -type, -opaque, -export_type
  highlight(0, 'erlangSpec',              { fg = colors.turquoise,  bg = 'NONE' })  -- -spec
  highlight(0, 'erlangCallback',          { fg = colors.turquoise,  bg = 'NONE' })  -- -callback
  highlight(0, 'erlangTypeName',          { link = "Type" })  -- Type names
  highlight(0, 'erlangTypeVar',           { link = "Type" })  -- Type variables

  -- Built-in Types
  highlight(0, 'erlangBuiltinType',       { link = "Type" })  -- any, none, pid, port, reference, float, atom, binary, bitstring, boolean, byte, char, nil, number, list, maybe_improper_list, mfa, module, no_return, node, non_neg_integer, pos_integer, neg_integer, nonempty_string, iodata, iolist, function, tuple, term, timeout, string, map

  -- Behaviours
  highlight(0, 'erlangBehaviour',         { fg = colors.pink,       bg = 'NONE' })  -- -behaviour, -behavior
  highlight(0, 'erlangBehaviourName',     { fg = colors.turquoise,  bg = 'NONE' })  -- gen_server, gen_statem, gen_event, supervisor, application

  -- Comments
  highlight(0, 'erlangComment',           { link = "Comment" })  -- % comments
  highlight(0, 'erlangCommentAnnotation', { link = "Comment" })  -- @doc, @spec, etc. in comments
  highlight(0, 'erlangTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Documentation (EDoc / OTP 27+ -doc)
  highlight(0, 'erlangDocString',         { link = "String" })  -- Documentation strings
  highlight(0, 'erlangDocStringCluster',  { link = "String" })  -- Doc string clusters
  highlight(0, 'erlangDocStringDelimiter', { link = "Delimiter" })  -- Doc string delimiters
  highlight(0, 'erlangDocStringContained', { link = "String" })  -- Contained doc strings
  highlight(0, 'erlangInnerDocAttribute', { fg = colors.pink,       bg = 'NONE' })  -- Inner doc attributes

  -- Shebang
  highlight(0, 'erlangShebang',           { fg = colors.green,      bg = 'NONE' })  -- #!/usr/bin/env escript

  -- Special
  highlight(0, 'erlangSpecialCharacter',  { fg = colors.pink,       bg = 'NONE' })  -- Special characters
  highlight(0, 'erlangEscape',            { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences

  -- Process/Concurrency
  highlight(0, 'erlangSpawn',             { fg = colors.orange,     bg = 'NONE' })  -- spawn, spawn_link, spawn_monitor
  highlight(0, 'erlangReceive',           { fg = colors.blue,       bg = 'NONE' })  -- receive keyword
  highlight(0, 'erlangAfter',             { fg = colors.blue,       bg = 'NONE' })  -- after keyword
  highlight(0, 'erlangSend',              { fg = colors.white,      bg = 'NONE' })  -- ! operator

  -- Guards
  highlight(0, 'erlangGuard',             { fg = colors.blue,       bg = 'NONE' })  -- when keyword
  highlight(0, 'erlangGuardBIF',          { fg = colors.orange,     bg = 'NONE' })  -- Guard BIFs (is_atom, is_binary, etc.)


  ---------------------------------------------------------------------------
  -- OTP-specific highlighting
  ---------------------------------------------------------------------------

  -- Common OTP Modules
  highlight(0, 'erlangOTPModule',         { fg = colors.turquoise,  bg = 'NONE' })  -- gen_server, supervisor, etc.

  -- OTP Callbacks
  highlight(0, 'erlangOTPCallback',       { fg = colors.orange,     bg = 'NONE' })  -- init, handle_call, handle_cast, handle_info, terminate, code_change

  -- ETS/DETS/Mnesia
  highlight(0, 'erlangETS',               { fg = colors.orange,     bg = 'NONE' })  -- ets:*, dets:*, mnesia:*


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.erlang)
  ---------------------------------------------------------------------------

  -- Variables
  highlight(0, '@variable.erlang',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.parameter.erlang',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.builtin.erlang',      { link = "Variable" })  -- _ anonymous

  -- Constants (Atoms)
  highlight(0, '@constant.erlang',              { link = "Constant" })  -- Atoms
  highlight(0, '@constant.builtin.erlang',      { link = "Constant" })  -- true, false, undefined

  -- Strings
  highlight(0, '@string.erlang',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.erlang',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.special.erlang',        { link = "String" })  -- Special strings
  highlight(0, '@string.special.symbol.erlang', { link = "String" })  -- Atoms/symbols

  -- Characters
  highlight(0, '@character.erlang',             { fg = colors.redLight,   bg = 'NONE' })  -- $c character literals
  highlight(0, '@character.special.erlang',     { fg = colors.pink,       bg = 'NONE' })  -- Special characters

  -- Numbers
  highlight(0, '@number.erlang',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.erlang',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.erlang',               { link = "Boolean" })  -- true, false

  -- Functions
  highlight(0, '@function.erlang',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.erlang',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.erlang',      { link = "Function" })  -- BIFs

  -- Modules
  highlight(0, '@module.erlang',                { fg = colors.turquoise,  bg = 'NONE' })  -- Module names

  -- Types
  highlight(0, '@type.erlang',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.erlang',          { link = "Type" })  -- Built-in types
  highlight(0, '@type.definition.erlang',       { link = "Type" })  -- Type definitions

  -- Keywords
  highlight(0, '@keyword.erlang',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.erlang',      { link = "Keyword" })  -- fun
  highlight(0, '@keyword.operator.erlang',      { link = "Operator" })  -- and, or, not, andalso, orelse
  highlight(0, '@keyword.conditional.erlang',   { link = "Conditional" })  -- if, case, of
  highlight(0, '@keyword.exception.erlang',     { link = "Keyword" })  -- try, catch, throw, error, exit
  highlight(0, '@keyword.directive.erlang',     { link = "Keyword" })  -- Attributes (-module, -export, etc.)

  -- Properties (Record fields)
  highlight(0, '@property.erlang',              { fg = colors.orange,     bg = 'NONE' })  -- Record field names

  -- Operators
  highlight(0, '@operator.erlang',              { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.erlang',   { fg = colors.white,      bg = 'NONE' })  -- (), [], {}, << >>
  highlight(0, '@punctuation.delimiter.erlang', { link = "Delimiter" })  -- , ; . : |
  highlight(0, '@punctuation.special.erlang',   { fg = colors.pink,       bg = 'NONE' })  -- # (records), ? (macros)

  -- Comments
  highlight(0, '@comment.erlang',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.erlang', { link = "Comment" })  -- EDoc comments
  highlight(0, '@comment.discard.erlang',       { link = "Comment" })  -- Disabled code

  -- Labels
  highlight(0, '@label.erlang',                 { fg = colors.pink,       bg = 'NONE' })  -- Labels

  -- Attributes/Preprocessor
  highlight(0, '@attribute.erlang',             { fg = colors.pink,       bg = 'NONE' })  -- Module attributes
  highlight(0, '@preproc.erlang',               { fg = colors.pink,       bg = 'NONE' })  -- Preprocessor directives


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.erlang)
  -- Based on ELP (Erlang Language Platform)
  ---------------------------------------------------------------------------

  highlight(0, '@lsp.type.variable.erlang',     { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.erlang',    { fg = colors.purple,     bg = 'NONE' })  -- Function parameters
  highlight(0, '@lsp.type.function.erlang',     { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.type.erlang',         { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.erlang',        { fg = colors.turquoise,  bg = 'NONE' })  -- Behaviours/Types
  highlight(0, '@lsp.type.namespace.erlang',    { fg = colors.turquoise,  bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.enum.erlang',         { fg = colors.pink,       bg = 'NONE' })  -- Atoms (enum-like)
  highlight(0, '@lsp.type.enumMember.erlang',   { fg = colors.pink,       bg = 'NONE' })  -- Atom values
  highlight(0, '@lsp.type.property.erlang',     { fg = colors.orange,     bg = 'NONE' })  -- Record fields
  highlight(0, '@lsp.type.macro.erlang',        { fg = colors.pink,       bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.keyword.erlang',      { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.erlang',     { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.erlang',       { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.erlang',       { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.comment.erlang',      { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.erlang',    { link = "Variable" })  -- Constants
  highlight(0, '@lsp.typemod.function.declaration.erlang', { fg = colors.orange,    bg = 'NONE' })  -- Function defs
  highlight(0, '@lsp.typemod.function.defaultLibrary.erlang', { fg = colors.orange, bg = 'NONE' })  -- BIFs


  ---------------------------------------------------------------------------
  -- EDoc Documentation Tags
  ---------------------------------------------------------------------------

  highlight(0, 'erlangEdocTag',           { fg = colors.pink,       bg = 'NONE' })  -- @doc, @spec, @type, @author, @since, @deprecated, @hidden, @private, @param, @returns, @see, @equiv, @todo
  highlight(0, 'erlangEdocParam',         { fg = colors.purple,     bg = 'NONE' })  -- Parameter names in @param
  highlight(0, 'erlangEdocType',          { link = "Type" })  -- Type references in docs
  highlight(0, 'erlangEdocLink',          { fg = colors.blue,       bg = 'NONE', underline = true })  -- {@link ...}
  highlight(0, 'erlangEdocCode',          { fg = colors.redLight,   bg = 'NONE' })  -- `code` in docs


  ---------------------------------------------------------------------------
  -- Common Process Dictionary / Reserved Atoms
  ---------------------------------------------------------------------------

  highlight(0, 'erlangReservedAtom',      { fg = colors.blue,       bg = 'NONE' })  -- undefined, infinity, timeout, noreply, ok, error, exit, throw, normal, shutdown, kill
  highlight(0, 'erlangProcessFlag',       { fg = colors.pink,       bg = 'NONE' })  -- trap_exit, priority, min_heap_size, etc.


  ---------------------------------------------------------------------------
  -- Error Highlighting
  ---------------------------------------------------------------------------

  highlight(0, 'erlangError',             { fg = colors.red,        bg = 'NONE' })  -- Syntax errors
  highlight(0, 'erlangWarning',           { fg = colors.orange,     bg = 'NONE' })  -- Warnings
end

return erlang
