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
  highlight(0, 'erlangKeyword',           { fg = colors.blue,       bg = 'NONE' })  -- case, of, end, if, receive, after, begin, catch, try, fun, when, query
  highlight(0, 'erlangConditional',       { fg = colors.blue,       bg = 'NONE' })  -- if, case, of
  highlight(0, 'erlangException',         { fg = colors.blue,       bg = 'NONE' })  -- try, catch, throw, error, exit

  -- Booleans
  highlight(0, 'erlangBoolean',           { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Atoms
  highlight(0, 'erlangAtom',              { fg = colors.pink,       bg = 'NONE' })  -- atoms (lowercase identifiers)
  highlight(0, 'erlangQuotedAtom',        { fg = colors.pink,       bg = 'NONE' })  -- 'quoted atoms'
  highlight(0, 'erlangQuotedAtomModifier', { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences in quoted atoms

  -- Variables
  highlight(0, 'erlangVariable',          { fg = colors.purple,     bg = 'NONE' })  -- Variables (Uppercase start)
  highlight(0, 'erlangAnonymousVariable', { fg = colors.gray,       bg = 'NONE' })  -- _ anonymous variable

  -- Functions
  highlight(0, 'erlangLocalFuncCall',     { fg = colors.orange,     bg = 'NONE' })  -- Local function calls
  highlight(0, 'erlangLocalFuncRef',      { fg = colors.orange,     bg = 'NONE' })  -- fun local/arity
  highlight(0, 'erlangGlobalFuncCall',    { fg = colors.orange,     bg = 'NONE' })  -- module:function() calls
  highlight(0, 'erlangGlobalFuncRef',     { fg = colors.orange,     bg = 'NONE' })  -- fun module:func/arity
  highlight(0, 'erlangFunction',          { fg = colors.orange,     bg = 'NONE' })  -- Function names
  highlight(0, 'erlangFun',               { fg = colors.blue,       bg = 'NONE' })  -- fun keyword

  -- Built-in Functions (BIFs)
  highlight(0, 'erlangBIF',               { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions (is_atom, self, spawn, etc.)

  -- Modules
  highlight(0, 'erlangModule',            { fg = colors.turquoise,  bg = 'NONE' })  -- Module names

  -- Numbers
  highlight(0, 'erlangNumberInteger',     { fg = colors.greenLight, bg = 'NONE' })  -- Integers (including base#value)
  highlight(0, 'erlangNumberFloat',       { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'erlangNumber',            { fg = colors.greenLight, bg = 'NONE' })  -- General numbers

  -- Strings
  highlight(0, 'erlangString',            { fg = colors.redLight,   bg = 'NONE' })  -- "strings"
  highlight(0, 'erlangStringTripleQuoted', { fg = colors.redLight,  bg = 'NONE' })  -- """triple quoted"""
  highlight(0, 'erlangStringModifier',    { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences in strings
  highlight(0, 'erlangModifier',          { fg = colors.pink,       bg = 'NONE' })  -- General escape sequences

  -- Characters
  highlight(0, 'erlangCharacter',         { fg = colors.redLight,   bg = 'NONE' })  -- $c character literals

  -- Operators
  highlight(0, 'erlangOperator',          { fg = colors.white,      bg = 'NONE' })  -- Operators (+, -, *, /, div, rem, band, bor, bxor, bnot, bsl, bsr, not, and, or, xor, orelse, andalso)
  highlight(0, 'erlangEqualsBinary',      { fg = colors.white,      bg = 'NONE' })  -- =:=, =/= (exact equality)
  highlight(0, 'erlangCompareOperator',   { fg = colors.white,      bg = 'NONE' })  -- ==, /=, <, >, =<, >=
  highlight(0, 'erlangListOperator',      { fg = colors.white,      bg = 'NONE' })  -- ++, -- (list operations)
  highlight(0, 'erlangSendOperator',      { fg = colors.white,      bg = 'NONE' })  -- ! (send message)
  highlight(0, 'erlangMatchOperator',     { fg = colors.white,      bg = 'NONE' })  -- = (pattern match)

  -- Arrows
  highlight(0, 'erlangRightArrow',        { fg = colors.white,      bg = 'NONE' })  -- ->
  highlight(0, 'erlangDoubleArrow',       { fg = colors.white,      bg = 'NONE' })  -- =>
  highlight(0, 'erlangLeftArrow',         { fg = colors.white,      bg = 'NONE' })  -- <-
  highlight(0, 'erlangListComprehension', { fg = colors.white,      bg = 'NONE' })  -- <- || in comprehensions

  -- Delimiters/Brackets
  highlight(0, 'erlangBracket',           { fg = colors.white,      bg = 'NONE' })  -- (), [], {}
  highlight(0, 'erlangPipe',              { fg = colors.white,      bg = 'NONE' })  -- | (list tail)
  highlight(0, 'erlangDelimiter',         { fg = colors.white,      bg = 'NONE' })  -- , ; . :
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
  highlight(0, 'erlangBitType',           { fg = colors.turquoise,  bg = 'NONE' })  -- binary, integer, float, utf8, utf16, utf32, signed, unsigned, big, little, native
  highlight(0, 'erlangBitSize',           { fg = colors.greenLight, bg = 'NONE' })  -- Size specifications

  -- Attributes/Directives
  highlight(0, 'erlangAttribute',         { fg = colors.pink,       bg = 'NONE' })  -- -module, -export, -import, -compile, -vsn, -author, -copyright, -on_load
  highlight(0, 'erlangDocAttribute',      { fg = colors.pink,       bg = 'NONE' })  -- -doc attribute (OTP 27+)
  highlight(0, 'erlangUnknownAttribute',  { fg = colors.pink,       bg = 'NONE' })  -- Unknown/custom attributes
  highlight(0, 'erlangInclude',           { fg = colors.pink,       bg = 'NONE' })  -- -include, -include_lib
  highlight(0, 'erlangPreCondit',         { fg = colors.pink,       bg = 'NONE' })  -- -ifdef, -ifndef, -else, -endif, -undef

  -- Type Specifications
  highlight(0, 'erlangType',              { fg = colors.turquoise,  bg = 'NONE' })  -- -type, -opaque, -export_type
  highlight(0, 'erlangSpec',              { fg = colors.turquoise,  bg = 'NONE' })  -- -spec
  highlight(0, 'erlangCallback',          { fg = colors.turquoise,  bg = 'NONE' })  -- -callback
  highlight(0, 'erlangTypeName',          { fg = colors.turquoise,  bg = 'NONE' })  -- Type names
  highlight(0, 'erlangTypeVar',           { fg = colors.purple,     bg = 'NONE' })  -- Type variables

  -- Built-in Types
  highlight(0, 'erlangBuiltinType',       { fg = colors.turquoise,  bg = 'NONE' })  -- any, none, pid, port, reference, float, atom, binary, bitstring, boolean, byte, char, nil, number, list, maybe_improper_list, mfa, module, no_return, node, non_neg_integer, pos_integer, neg_integer, nonempty_string, iodata, iolist, function, tuple, term, timeout, string, map

  -- Behaviours
  highlight(0, 'erlangBehaviour',         { fg = colors.pink,       bg = 'NONE' })  -- -behaviour, -behavior
  highlight(0, 'erlangBehaviourName',     { fg = colors.turquoise,  bg = 'NONE' })  -- gen_server, gen_statem, gen_event, supervisor, application

  -- Comments
  highlight(0, 'erlangComment',           { fg = colors.red,        bg = 'NONE' })  -- % comments
  highlight(0, 'erlangCommentAnnotation', { fg = colors.red,        bg = 'NONE', bold = true })  -- @doc, @spec, etc. in comments
  highlight(0, 'erlangTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Documentation (EDoc / OTP 27+ -doc)
  highlight(0, 'erlangDocString',         { fg = colors.red,        bg = 'NONE' })  -- Documentation strings
  highlight(0, 'erlangDocStringCluster',  { fg = colors.red,        bg = 'NONE' })  -- Doc string clusters
  highlight(0, 'erlangDocStringDelimiter', { fg = colors.red,       bg = 'NONE' })  -- Doc string delimiters
  highlight(0, 'erlangDocStringContained', { fg = colors.red,       bg = 'NONE' })  -- Contained doc strings
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
  highlight(0, '@variable.erlang',              { fg = colors.purple,     bg = 'NONE' })  -- Variables
  highlight(0, '@variable.parameter.erlang',    { fg = colors.purple,     bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.builtin.erlang',      { fg = colors.pink,       bg = 'NONE' })  -- _ anonymous

  -- Constants (Atoms)
  highlight(0, '@constant.erlang',              { fg = colors.pink,       bg = 'NONE' })  -- Atoms
  highlight(0, '@constant.builtin.erlang',      { fg = colors.blue,       bg = 'NONE' })  -- true, false, undefined

  -- Strings
  highlight(0, '@string.erlang',                { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.erlang',         { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.erlang',        { fg = colors.pink,       bg = 'NONE' })  -- Special strings
  highlight(0, '@string.special.symbol.erlang', { fg = colors.pink,       bg = 'NONE' })  -- Atoms/symbols

  -- Characters
  highlight(0, '@character.erlang',             { fg = colors.redLight,   bg = 'NONE' })  -- $c character literals
  highlight(0, '@character.special.erlang',     { fg = colors.pink,       bg = 'NONE' })  -- Special characters

  -- Numbers
  highlight(0, '@number.erlang',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.erlang',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.erlang',               { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Functions
  highlight(0, '@function.erlang',              { fg = colors.orange,     bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.erlang',         { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.erlang',      { fg = colors.orange,     bg = 'NONE' })  -- BIFs

  -- Modules
  highlight(0, '@module.erlang',                { fg = colors.turquoise,  bg = 'NONE' })  -- Module names

  -- Types
  highlight(0, '@type.erlang',                  { fg = colors.turquoise,  bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.erlang',          { fg = colors.turquoise,  bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.erlang',       { fg = colors.turquoise,  bg = 'NONE' })  -- Type definitions

  -- Keywords
  highlight(0, '@keyword.erlang',               { fg = colors.blue,       bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.erlang',      { fg = colors.blue,       bg = 'NONE' })  -- fun
  highlight(0, '@keyword.operator.erlang',      { fg = colors.blue,       bg = 'NONE' })  -- and, or, not, andalso, orelse
  highlight(0, '@keyword.conditional.erlang',   { fg = colors.blue,       bg = 'NONE' })  -- if, case, of
  highlight(0, '@keyword.exception.erlang',     { fg = colors.blue,       bg = 'NONE' })  -- try, catch, throw, error, exit
  highlight(0, '@keyword.directive.erlang',     { fg = colors.pink,       bg = 'NONE' })  -- Attributes (-module, -export, etc.)

  -- Properties (Record fields)
  highlight(0, '@property.erlang',              { fg = colors.orange,     bg = 'NONE' })  -- Record field names

  -- Operators
  highlight(0, '@operator.erlang',              { fg = colors.white,      bg = 'NONE' })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.erlang',   { fg = colors.white,      bg = 'NONE' })  -- (), [], {}, << >>
  highlight(0, '@punctuation.delimiter.erlang', { fg = colors.white,      bg = 'NONE' })  -- , ; . : |
  highlight(0, '@punctuation.special.erlang',   { fg = colors.pink,       bg = 'NONE' })  -- # (records), ? (macros)

  -- Comments
  highlight(0, '@comment.erlang',               { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.erlang', { fg = colors.red,        bg = 'NONE' })  -- EDoc comments
  highlight(0, '@comment.discard.erlang',       { fg = colors.gray,       bg = 'NONE' })  -- Disabled code

  -- Labels
  highlight(0, '@label.erlang',                 { fg = colors.pink,       bg = 'NONE' })  -- Labels

  -- Attributes/Preprocessor
  highlight(0, '@attribute.erlang',             { fg = colors.pink,       bg = 'NONE' })  -- Module attributes
  highlight(0, '@preproc.erlang',               { fg = colors.pink,       bg = 'NONE' })  -- Preprocessor directives


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.erlang)
  -- Based on ELP (Erlang Language Platform)
  ---------------------------------------------------------------------------

  highlight(0, '@lsp.type.variable.erlang',     { fg = colors.purple,     bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.erlang',    { fg = colors.purple,     bg = 'NONE' })  -- Function parameters
  highlight(0, '@lsp.type.function.erlang',     { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.type.erlang',         { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.erlang',        { fg = colors.turquoise,  bg = 'NONE' })  -- Behaviours/Types
  highlight(0, '@lsp.type.namespace.erlang',    { fg = colors.turquoise,  bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.enum.erlang',         { fg = colors.pink,       bg = 'NONE' })  -- Atoms (enum-like)
  highlight(0, '@lsp.type.enumMember.erlang',   { fg = colors.pink,       bg = 'NONE' })  -- Atom values
  highlight(0, '@lsp.type.property.erlang',     { fg = colors.orange,     bg = 'NONE' })  -- Record fields
  highlight(0, '@lsp.type.macro.erlang',        { fg = colors.pink,       bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.keyword.erlang',      { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.erlang',     { fg = colors.white,      bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.erlang',       { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.erlang',       { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.comment.erlang',      { fg = colors.red,        bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.erlang',    { fg = colors.pink,      bg = 'NONE' })  -- Constants
  highlight(0, '@lsp.typemod.function.declaration.erlang', { fg = colors.orange,    bg = 'NONE' })  -- Function defs
  highlight(0, '@lsp.typemod.function.defaultLibrary.erlang', { fg = colors.orange, bg = 'NONE' })  -- BIFs


  ---------------------------------------------------------------------------
  -- EDoc Documentation Tags
  ---------------------------------------------------------------------------

  highlight(0, 'erlangEdocTag',           { fg = colors.pink,       bg = 'NONE' })  -- @doc, @spec, @type, @author, @since, @deprecated, @hidden, @private, @param, @returns, @see, @equiv, @todo
  highlight(0, 'erlangEdocParam',         { fg = colors.purple,     bg = 'NONE' })  -- Parameter names in @param
  highlight(0, 'erlangEdocType',          { fg = colors.turquoise,  bg = 'NONE' })  -- Type references in docs
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
