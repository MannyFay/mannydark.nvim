-------------------------------------------------------------------------------
-- Clojure Files
-- Highlighting for .clj, .cljs, .cljc, .edn files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local clojure = {}


-------------------------------------------------------------------------------
-- Settings

clojure.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Special Forms
  highlight(0, 'clojureSpecial',        { fg = colors.blue,       bg = 'NONE'            })  -- Special forms
  highlight(0, 'clojureDef',            { fg = colors.blue,       bg = 'NONE'            })  -- def
  highlight(0, 'clojureIf',             { fg = colors.blue,       bg = 'NONE'            })  -- if
  highlight(0, 'clojureDo',             { fg = colors.blue,       bg = 'NONE'            })  -- do
  highlight(0, 'clojureLet',            { fg = colors.blue,       bg = 'NONE'            })  -- let
  highlight(0, 'clojureQuote',          { fg = colors.blue,       bg = 'NONE'            })  -- quote
  highlight(0, 'clojureVar',            { fg = colors.blue,       bg = 'NONE'            })  -- var
  highlight(0, 'clojureFn',             { fg = colors.blue,       bg = 'NONE'            })  -- fn
  highlight(0, 'clojureLoop',           { fg = colors.blue,       bg = 'NONE'            })  -- loop
  highlight(0, 'clojureRecur',          { fg = colors.blue,       bg = 'NONE'            })  -- recur
  highlight(0, 'clojureThrow',          { fg = colors.blue,       bg = 'NONE'            })  -- throw
  highlight(0, 'clojureTry',            { fg = colors.blue,       bg = 'NONE'            })  -- try
  highlight(0, 'clojureCatch',          { fg = colors.blue,       bg = 'NONE'            })  -- catch
  highlight(0, 'clojureFinally',        { fg = colors.blue,       bg = 'NONE'            })  -- finally
  highlight(0, 'clojureNew',            { fg = colors.blue,       bg = 'NONE'            })  -- new
  highlight(0, 'clojureSet',            { fg = colors.blue,       bg = 'NONE'            })  -- set!
  highlight(0, 'clojureDot',            { fg = colors.blue,       bg = 'NONE'            })  -- . (dot)
  highlight(0, 'clojureMonitor',        { fg = colors.blue,       bg = 'NONE'            })  -- monitor-enter, monitor-exit

  -- Definition Macros
  highlight(0, 'clojureDefine',         { fg = colors.blue,       bg = 'NONE'            })  -- defn, defn-, defmacro, etc.
  highlight(0, 'clojureDefn',           { fg = colors.blue,       bg = 'NONE'            })  -- defn
  highlight(0, 'clojureDefnPrivate',    { fg = colors.blue,       bg = 'NONE'            })  -- defn-
  highlight(0, 'clojureDefmacro',       { fg = colors.blue,       bg = 'NONE'            })  -- defmacro
  highlight(0, 'clojureDefmulti',       { fg = colors.blue,       bg = 'NONE'            })  -- defmulti
  highlight(0, 'clojureDefmethod',      { fg = colors.blue,       bg = 'NONE'            })  -- defmethod
  highlight(0, 'clojureDefonce',        { fg = colors.blue,       bg = 'NONE'            })  -- defonce
  highlight(0, 'clojureDefstruct',      { fg = colors.blue,       bg = 'NONE'            })  -- defstruct
  highlight(0, 'clojureDefprotocol',    { fg = colors.blue,       bg = 'NONE'            })  -- defprotocol
  highlight(0, 'clojureDefrecord',      { fg = colors.blue,       bg = 'NONE'            })  -- defrecord
  highlight(0, 'clojureDeftype',        { fg = colors.blue,       bg = 'NONE'            })  -- deftype
  highlight(0, 'clojureDeclare',        { fg = colors.blue,       bg = 'NONE'            })  -- declare
  highlight(0, 'clojureNs',             { fg = colors.blue,       bg = 'NONE'            })  -- ns

  -- Branching Macros
  highlight(0, 'clojureCond',           { fg = colors.blue,       bg = 'NONE'            })  -- cond, condp, case
  highlight(0, 'clojureWhen',           { fg = colors.blue,       bg = 'NONE'            })  -- when, when-not, when-let, when-first
  highlight(0, 'clojureIfNot',          { fg = colors.blue,       bg = 'NONE'            })  -- if-not, if-let, if-some
  highlight(0, 'clojureAnd',            { fg = colors.blue,       bg = 'NONE'            })  -- and
  highlight(0, 'clojureOr',             { fg = colors.blue,       bg = 'NONE'            })  -- or

  -- Looping Macros
  highlight(0, 'clojureFor',            { fg = colors.blue,       bg = 'NONE'            })  -- for
  highlight(0, 'clojureDoseq',          { fg = colors.blue,       bg = 'NONE'            })  -- doseq
  highlight(0, 'clojureDotimes',        { fg = colors.blue,       bg = 'NONE'            })  -- dotimes
  highlight(0, 'clojureWhile',          { fg = colors.blue,       bg = 'NONE'            })  -- while

  -- Threading Macros
  highlight(0, 'clojureThread',         { fg = colors.blue,       bg = 'NONE'            })  -- ->, ->>, as->, some->, some->>
  highlight(0, 'clojureCondThread',     { fg = colors.blue,       bg = 'NONE'            })  -- cond->, cond->>

  -- Dynamic Scope Macros
  highlight(0, 'clojureBinding',        { fg = colors.blue,       bg = 'NONE'            })  -- binding
  highlight(0, 'clojureLocking',        { fg = colors.blue,       bg = 'NONE'            })  -- locking
  highlight(0, 'clojureWith',           { fg = colors.blue,       bg = 'NONE'            })  -- with-open, with-out-str, with-in-str, etc.

  -- Import/Require
  highlight(0, 'clojureImport',         { fg = colors.pink,       bg = 'NONE'            })  -- import
  highlight(0, 'clojureRequire',        { fg = colors.pink,       bg = 'NONE'            })  -- require
  highlight(0, 'clojureUse',            { fg = colors.pink,       bg = 'NONE'            })  -- use
  highlight(0, 'clojureRefer',          { fg = colors.pink,       bg = 'NONE'            })  -- refer
  highlight(0, 'clojureNsOptions',      { fg = colors.pink,       bg = 'NONE'            })  -- :require, :import, :use, :refer, :as, :only

  -- Functions
  highlight(0, 'clojureFunction',       { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'clojureFunctionCall',   { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'clojureBuiltinFunc',    { fg = colors.orange,     bg = 'NONE'            })  -- Built-in functions
  highlight(0, 'clojureMacro',          { fg = colors.orange,     bg = 'NONE'            })  -- Macro calls

  -- Core Functions
  highlight(0, 'clojureCoreFunc',       { fg = colors.orange,     bg = 'NONE'            })  -- map, filter, reduce, etc.
  highlight(0, 'clojurePrint',          { fg = colors.orange,     bg = 'NONE'            })  -- print, println, pr, prn
  highlight(0, 'clojureStr',            { fg = colors.orange,     bg = 'NONE'            })  -- str, format
  highlight(0, 'clojureMath',           { fg = colors.orange,     bg = 'NONE'            })  -- +, -, *, /, inc, dec, etc.

  -- Types
  highlight(0, 'clojureType',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'clojureJavaClass',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Java class names
  highlight(0, 'clojureProtocol',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Protocol names
  highlight(0, 'clojureRecord',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Record types

  -- Variables/Symbols
  highlight(0, 'clojureSymbol',         { fg = colors.purple,     bg = 'NONE'            })  -- Symbols
  highlight(0, 'clojureVariable',       { fg = colors.purple,     bg = 'NONE'            })  -- Variables
  highlight(0, 'clojureParameter',      { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'clojureNamespace',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespace part of symbols

  -- Keywords
  highlight(0, 'clojureKeyword',        { fg = colors.pink,       bg = 'NONE'            })  -- :keyword
  highlight(0, 'clojureKeywordNs',      { fg = colors.pink,       bg = 'NONE'            })  -- ::namespaced-keyword
  highlight(0, 'clojureKeywordColon',   { fg = colors.pink,       bg = 'NONE'            })  -- : prefix

  -- Constants/Literals
  highlight(0, 'clojureConstant',       { fg = colors.blue,       bg = 'NONE'            })  -- Constants
  highlight(0, 'clojureBoolean',        { fg = colors.blue,       bg = 'NONE'            })  -- true, false
  highlight(0, 'clojureNil',            { fg = colors.blue,       bg = 'NONE'            })  -- nil
  highlight(0, 'clojureSpecialValue',   { fg = colors.blue,       bg = 'NONE'            })  -- ##Inf, ##-Inf, ##NaN

  -- Strings
  highlight(0, 'clojureString',         { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'clojureStringEscape',   { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, \", \\
  highlight(0, 'clojureRegex',          { fg = colors.redLight,   bg = 'NONE'            })  -- #"regex"

  -- Characters
  highlight(0, 'clojureCharacter',      { fg = colors.redLight,   bg = 'NONE'            })  -- \a, \newline, \space, \tab
  highlight(0, 'clojureCharSpecial',    { fg = colors.pink,       bg = 'NONE'            })  -- \newline, \space, \tab, \uNNNN

  -- Numbers
  highlight(0, 'clojureNumber',         { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'clojureInteger',        { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'clojureFloat',          { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'clojureRatio',          { fg = colors.greenLight, bg = 'NONE'            })  -- Ratios (1/2, 3/4)
  highlight(0, 'clojureHex',            { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF hex
  highlight(0, 'clojureOctal',          { fg = colors.greenLight, bg = 'NONE'            })  -- 0777 octal
  highlight(0, 'clojureBinary',         { fg = colors.greenLight, bg = 'NONE'            })  -- 2r1010 binary
  highlight(0, 'clojureRadix',          { fg = colors.greenLight, bg = 'NONE'            })  -- NrXXX radix notation

  -- Reader Macros
  highlight(0, 'clojureQuoteMark',      { fg = colors.pink,       bg = 'NONE'            })  -- ' quote
  highlight(0, 'clojureSyntaxQuote',    { fg = colors.pink,       bg = 'NONE'            })  -- ` syntax-quote
  highlight(0, 'clojureUnquote',        { fg = colors.pink,       bg = 'NONE'            })  -- ~ unquote
  highlight(0, 'clojureUnquoteSplice',  { fg = colors.pink,       bg = 'NONE'            })  -- ~@ unquote-splicing
  highlight(0, 'clojureDeref',          { fg = colors.pink,       bg = 'NONE'            })  -- @ deref
  highlight(0, 'clojureMeta',           { fg = colors.pink,       bg = 'NONE'            })  -- ^ metadata

  -- Dispatch Macros (#)
  highlight(0, 'clojureDispatch',       { fg = colors.pink,       bg = 'NONE'            })  -- # dispatch character
  highlight(0, 'clojureSetLiteral',     { fg = colors.pink,       bg = 'NONE'            })  -- #{} set literal
  highlight(0, 'clojureDiscard',        { fg = colors.red,        bg = 'NONE'            })  -- #_ discard
  highlight(0, 'clojureAnonFn',         { fg = colors.pink,       bg = 'NONE'            })  -- #() anonymous function
  highlight(0, 'clojureAnonArg',        { fg = colors.purple,     bg = 'NONE'            })  -- %, %1, %2, %&
  highlight(0, 'clojureVarQuote',       { fg = colors.pink,       bg = 'NONE'            })  -- #' var quote
  highlight(0, 'clojureTaggedLiteral',  { fg = colors.pink,       bg = 'NONE'            })  -- #inst, #uuid
  highlight(0, 'clojureReaderCond',     { fg = colors.pink,       bg = 'NONE'            })  -- #?, #?@

  -- Data Structure Delimiters
  highlight(0, 'clojureParen',          { fg = colors.white,      bg = 'NONE'            })  -- () list
  highlight(0, 'clojureVector',         { fg = colors.white,      bg = 'NONE'            })  -- [] vector
  highlight(0, 'clojureMap',            { fg = colors.white,      bg = 'NONE'            })  -- {} map
  highlight(0, 'clojureSetBraces',      { fg = colors.white,      bg = 'NONE'            })  -- #{} set

  -- Java Interop
  highlight(0, 'clojureJavaMethod',     { fg = colors.orange,     bg = 'NONE'            })  -- .method
  highlight(0, 'clojureJavaStaticMethod', { fg = colors.orange,   bg = 'NONE'            })  -- Class/method
  highlight(0, 'clojureJavaField',      { fg = colors.purple,     bg = 'NONE'            })  -- .-field

  -- Comments
  highlight(0, 'clojureComment',        { fg = colors.red,        bg = 'NONE'            })  -- ; comments
  highlight(0, 'clojureCommentForm',    { fg = colors.red,        bg = 'NONE'            })  -- (comment ...)
  highlight(0, 'clojureTodo',           { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Error
  highlight(0, 'clojureError',          { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.clojure)

  -- Variables
  highlight(0, '@variable.clojure',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.clojure',      { fg = colors.blue,      bg = 'NONE' })  -- &form, &env
  highlight(0, '@variable.parameter.clojure',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.clojure',       { fg = colors.purple,    bg = 'NONE' })  -- Java fields

  -- Constants
  highlight(0, '@constant.clojure',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.clojure',      { fg = colors.blue,      bg = 'NONE' })  -- true, false, nil

  -- Functions
  highlight(0, '@function.clojure',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.clojure',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.clojure',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.macro.clojure',        { fg = colors.orange,    bg = 'NONE' })  -- Macro calls
  highlight(0, '@function.method.clojure',       { fg = colors.orange,    bg = 'NONE' })  -- Java methods
  highlight(0, '@constructor.clojure',           { fg = colors.turquoise, bg = 'NONE' })  -- Type constructors

  -- Types
  highlight(0, '@type.clojure',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.clojure',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types

  -- Modules/Namespaces
  highlight(0, '@module.clojure',                { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces

  -- Keywords
  highlight(0, '@keyword.clojure',               { fg = colors.blue,      bg = 'NONE' })  -- Special forms
  highlight(0, '@keyword.function.clojure',      { fg = colors.blue,      bg = 'NONE' })  -- fn, defn, defmacro
  highlight(0, '@keyword.conditional.clojure',   { fg = colors.blue,      bg = 'NONE' })  -- if, when, cond, case
  highlight(0, '@keyword.repeat.clojure',        { fg = colors.blue,      bg = 'NONE' })  -- loop, recur, for, doseq
  highlight(0, '@keyword.exception.clojure',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, finally, throw
  highlight(0, '@keyword.import.clojure',        { fg = colors.pink,      bg = 'NONE' })  -- require, import, use
  highlight(0, '@keyword.coroutine.clojure',     { fg = colors.blue,      bg = 'NONE' })  -- go, async (core.async)
  highlight(0, '@keyword.operator.clojure',      { fg = colors.blue,      bg = 'NONE' })  -- and, or, not

  -- Strings
  highlight(0, '@string.clojure',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.clojure',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.regexp.clojure',         { fg = colors.redLight,  bg = 'NONE' })  -- Regex #"..."
  highlight(0, '@string.special.symbol.clojure', { fg = colors.pink,      bg = 'NONE' })  -- Keywords :foo

  -- Characters
  highlight(0, '@character.clojure',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.clojure',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.float.clojure',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.clojure',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.clojure',               { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- Labels (unused but reserved)
  highlight(0, '@label.clojure',                 { fg = colors.pink,      bg = 'NONE' })  -- Labels

  -- Operators and Punctuation
  highlight(0, '@operator.clojure',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.clojure',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.clojure', { fg = colors.white,     bg = 'NONE' })  -- Delimiters
  highlight(0, '@punctuation.special.clojure',   { fg = colors.pink,      bg = 'NONE' })  -- # @ ^ ` ~ '


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.clojure)

  highlight(0, '@lsp.type.variable.clojure',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.clojure',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.function.clojure',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.macro.clojure',         { fg = colors.orange,    bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.type.clojure',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.clojure',         { fg = colors.turquoise, bg = 'NONE' })  -- Java classes
  highlight(0, '@lsp.type.namespace.clojure',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.keyword.clojure',       { fg = colors.pink,      bg = 'NONE' })  -- Keywords :foo
  highlight(0, '@lsp.type.string.clojure',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.clojure',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.clojure',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.function.definition.clojure',  { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@lsp.typemod.variable.readonly.clojure',    { fg = colors.purple,    bg = 'NONE' })  -- Immutable bindings
  highlight(0, '@lsp.typemod.variable.defaultLibrary.clojure', { fg = colors.orange, bg = 'NONE' })  -- Core functions
end

return clojure
