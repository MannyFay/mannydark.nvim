-------------------------------------------------------------------------------
-- Racket
-- Highlighting for .rkt, .rktl, .rktd, .scrbl files.
-- Covers Racket-specific extensions beyond standard Scheme.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local racket    = {}


-------------------------------------------------------------------------------
-- Settings

racket.setupHighlighting = function()


  ---------------------------------------------------------------------------
  -- Vim Legacy Syntax Groups (vim runtime racket.vim)
  ---------------------------------------------------------------------------

  -- Core Syntax (Special Forms)
  highlight(0, 'racketSyntax',            { fg = colors.blue,       bg = 'NONE' })  -- Special forms and keywords

  -- Functions
  highlight(0, 'racketFunc',              { link = "Function" })  -- Built-in functions

  -- Extension Syntax
  highlight(0, 'racketExtSyntax',         { fg = colors.pink,       bg = 'NONE' })  -- #:keyword arguments
  highlight(0, 'racketExtFunc',           { link = "Function" })  -- Extension functions

  -- Strings
  highlight(0, 'racketString',            { link = "String" })  -- "strings"
  highlight(0, 'racketStringEscape',      { link = "String" })  -- Valid escape sequences
  highlight(0, 'racketStringEscapeError', { link = "String" })  -- Invalid escape sequences
  highlight(0, 'racketUStringEscape',     { link = "String" })  -- Unicode escapes
  highlight(0, 'racketHereString',        { link = "String" })  -- #<< here-strings

  -- Numbers
  highlight(0, 'racketNumber',            { link = "Number" })  -- Numbers
  highlight(0, 'racketNumberError',       { link = "Number" })  -- Malformed numbers
  highlight(0, 'racketContainedNumberError', { link = "Number" })  -- Invalid digits

  -- Booleans
  highlight(0, 'racketBoolean',           { link = "Boolean" })  -- #t, #f, #true, #false

  -- Characters
  highlight(0, 'racketChar',              { fg = colors.redLight,   bg = 'NONE' })  -- #\char

  -- Parentheses/Brackets
  highlight(0, 'racketParen',             { fg = colors.white,      bg = 'NONE' })  -- (), [], {}

  -- Delimiters
  highlight(0, 'racketDelimiter',         { link = "Delimiter" })  -- Dot delimiter

  -- Structures
  highlight(0, 'racketStruc',             { fg = colors.pink,       bg = 'NONE' })  -- #(...) vectors, #s(...) structs

  -- Constants
  highlight(0, 'racketConstant',          { link = "Constant" })  -- *name*, <name>

  -- Quote/Quasiquote
  highlight(0, 'racketQuote',             { fg = colors.pink,       bg = 'NONE' })  -- ', #', `
  highlight(0, 'racketUnquote',           { fg = colors.pink,       bg = 'NONE' })  -- , ,@

  -- Literals
  highlight(0, 'racketLit',               { fg = colors.pink,       bg = 'NONE' })  -- Hash literals

  -- Regex
  highlight(0, 'racketRe',                { fg = colors.pink,       bg = 'NONE' })  -- #rx, #px regex prefixes
  highlight(0, 'racketRegexp',            { fg = colors.redLight,   bg = 'NONE' })  -- Regex content

  -- Comments
  highlight(0, 'racketComment',           { link = "Comment" })  -- ; comments
  highlight(0, 'racketMultilineComment',  { link = "Comment" })  -- #| ... |# block comments
  highlight(0, 'racketFormComment',       { link = "Comment" })  -- #; form comments
  highlight(0, 'racketSharpBang',         { fg = colors.green,      bg = 'NONE' })  -- #! shebang, #lang
  highlight(0, 'racketTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO
  highlight(0, 'racketNote',              { fg = colors.blue,       bg = 'NONE', bold = true })  -- NOTE

  -- Errors
  highlight(0, 'racketError',             { fg = colors.red,        bg = 'NONE' })  -- Syntax errors


  ---------------------------------------------------------------------------
  -- Module System
  ---------------------------------------------------------------------------

  highlight(0, 'racketModule',            { fg = colors.blue,       bg = 'NONE' })  -- module, module*, module+
  highlight(0, 'racketModuleBegin',       { fg = colors.blue,       bg = 'NONE' })  -- #%module-begin
  highlight(0, 'racketRequire',           { fg = colors.pink,       bg = 'NONE' })  -- require
  highlight(0, 'racketProvide',           { fg = colors.pink,       bg = 'NONE' })  -- provide
  highlight(0, 'racketLang',              { fg = colors.turquoise,  bg = 'NONE' })  -- #lang racket, etc.
  highlight(0, 'racketLangName',          { fg = colors.turquoise,  bg = 'NONE' })  -- Language name after #lang

  -- Require/Provide Forms
  highlight(0, 'racketRequireForm',       { fg = colors.pink,       bg = 'NONE' })  -- only-in, except-in, prefix-in, rename-in, combine-in, relative-in, only-meta-in, for-syntax, for-template, for-label, for-meta, submod
  highlight(0, 'racketProvideForm',       { fg = colors.pink,       bg = 'NONE' })  -- all-defined-out, all-from-out, rename-out, except-out, prefix-out, struct-out, contract-out, protect-out


  ---------------------------------------------------------------------------
  -- Definition Forms
  ---------------------------------------------------------------------------

  highlight(0, 'racketDefine',            { fg = colors.blue,       bg = 'NONE' })  -- define
  highlight(0, 'racketDefineValues',      { fg = colors.blue,       bg = 'NONE' })  -- define-values
  highlight(0, 'racketDefineSyntax',      { fg = colors.blue,       bg = 'NONE' })  -- define-syntax, define-syntaxes, define-for-syntax
  highlight(0, 'racketDefineSyntaxRule',  { fg = colors.blue,       bg = 'NONE' })  -- define-syntax-rule
  highlight(0, 'racketDefineMatch',       { fg = colors.blue,       bg = 'NONE' })  -- define/match

  -- Starred Definitions (internal)
  highlight(0, 'racketDefineStar',        { fg = colors.blue,       bg = 'NONE' })  -- define*, define*-values, define*-syntax


  ---------------------------------------------------------------------------
  -- Lambda/Function Forms
  ---------------------------------------------------------------------------

  highlight(0, 'racketLambda',            { fg = colors.blue,       bg = 'NONE' })  -- lambda, λ
  highlight(0, 'racketCaseLambda',        { fg = colors.blue,       bg = 'NONE' })  -- case-lambda
  highlight(0, 'racketMatchLambda',       { fg = colors.blue,       bg = 'NONE' })  -- match-lambda, match-lambda*, match-lambda**


  ---------------------------------------------------------------------------
  -- Binding Forms
  ---------------------------------------------------------------------------

  highlight(0, 'racketLet',               { fg = colors.blue,       bg = 'NONE' })  -- let, let*, letrec
  highlight(0, 'racketLetValues',         { fg = colors.blue,       bg = 'NONE' })  -- let-values, let*-values
  highlight(0, 'racketLetSyntax',         { fg = colors.blue,       bg = 'NONE' })  -- let-syntax, letrec-syntax, let-syntaxes, letrec-syntaxes
  highlight(0, 'racketLetRecSyntaxValues', { fg = colors.blue,      bg = 'NONE' })  -- letrec-syntaxes+values
  highlight(0, 'racketLocal',             { fg = colors.blue,       bg = 'NONE' })  -- local
  highlight(0, 'racketShared',            { fg = colors.blue,       bg = 'NONE' })  -- shared


  ---------------------------------------------------------------------------
  -- Control Flow
  ---------------------------------------------------------------------------

  highlight(0, 'racketIf',                { fg = colors.blue,       bg = 'NONE' })  -- if
  highlight(0, 'racketCond',              { fg = colors.blue,       bg = 'NONE' })  -- cond
  highlight(0, 'racketCase',              { fg = colors.blue,       bg = 'NONE' })  -- case
  highlight(0, 'racketWhen',              { fg = colors.blue,       bg = 'NONE' })  -- when
  highlight(0, 'racketUnless',            { fg = colors.blue,       bg = 'NONE' })  -- unless
  highlight(0, 'racketAnd',               { fg = colors.blue,       bg = 'NONE' })  -- and
  highlight(0, 'racketOr',                { fg = colors.blue,       bg = 'NONE' })  -- or
  highlight(0, 'racketElse',              { fg = colors.blue,       bg = 'NONE' })  -- else
  highlight(0, 'racketArrow',             { fg = colors.blue,       bg = 'NONE' })  -- =>

  -- Begin Forms
  highlight(0, 'racketBegin',             { fg = colors.blue,       bg = 'NONE' })  -- begin, begin0, begin-for-syntax


  ---------------------------------------------------------------------------
  -- For Loops (Racket-specific)
  ---------------------------------------------------------------------------

  highlight(0, 'racketFor',               { fg = colors.blue,       bg = 'NONE' })  -- for, for*
  highlight(0, 'racketForList',           { fg = colors.blue,       bg = 'NONE' })  -- for/list, for*/list
  highlight(0, 'racketForVector',         { fg = colors.blue,       bg = 'NONE' })  -- for/vector, for*/vector
  highlight(0, 'racketForHash',           { fg = colors.blue,       bg = 'NONE' })  -- for/hash, for*/hash, for/hasheq, for*/hasheq, for/hasheqv, for*/hasheqv
  highlight(0, 'racketForAnd',            { fg = colors.blue,       bg = 'NONE' })  -- for/and, for*/and
  highlight(0, 'racketForOr',             { fg = colors.blue,       bg = 'NONE' })  -- for/or, for*/or
  highlight(0, 'racketForLists',          { fg = colors.blue,       bg = 'NONE' })  -- for/lists, for*/lists
  highlight(0, 'racketForFirst',          { fg = colors.blue,       bg = 'NONE' })  -- for/first, for*/first
  highlight(0, 'racketForLast',           { fg = colors.blue,       bg = 'NONE' })  -- for/last, for*/last
  highlight(0, 'racketForFold',           { fg = colors.blue,       bg = 'NONE' })  -- for/fold, for*/fold, for/fold/derived, for*/fold/derived
  highlight(0, 'racketForSet',            { fg = colors.blue,       bg = 'NONE' })  -- for/set, for*/set, for/seteq, for*/seteq
  highlight(0, 'racketForFlvector',       { fg = colors.blue,       bg = 'NONE' })  -- for/flvector, for*/flvector
  highlight(0, 'racketForFxvector',       { fg = colors.blue,       bg = 'NONE' })  -- for/fxvector, for*/fxvector

  -- Sequence Forms
  highlight(0, 'racketInSequence',        { fg = colors.blue,       bg = 'NONE' })  -- in-list, in-vector, in-string, in-range, in-naturals, in-hash, in-dict, in-set


  ---------------------------------------------------------------------------
  -- Pattern Matching (Racket-specific)
  ---------------------------------------------------------------------------

  highlight(0, 'racketMatch',             { fg = colors.blue,       bg = 'NONE' })  -- match, match*, match/values
  highlight(0, 'racketMatchLet',          { fg = colors.blue,       bg = 'NONE' })  -- match-let, match-let*, match-let-values, match-let*-values
  highlight(0, 'racketMatchLetrec',       { fg = colors.blue,       bg = 'NONE' })  -- match-letrec
  highlight(0, 'racketMatchDefine',       { fg = colors.blue,       bg = 'NONE' })  -- match-define, match-define-values


  ---------------------------------------------------------------------------
  -- Contracts (Racket-specific)
  ---------------------------------------------------------------------------

  highlight(0, 'racketContractArrow',     { fg = colors.blue,       bg = 'NONE' })  -- ->, ->*, ->i, ->d, case->, dynamic->*, unconstrained-domain->
  highlight(0, 'racketWithContract',      { fg = colors.blue,       bg = 'NONE' })  -- with-contract
  highlight(0, 'racketDefineContract',    { fg = colors.blue,       bg = 'NONE' })  -- define/contract
  highlight(0, 'racketDefineStructContract', { fg = colors.blue,    bg = 'NONE' })  -- define-struct/contract

  -- Contract Combinators
  highlight(0, 'racketContractCombinator', { fg = colors.orange,    bg = 'NONE' })  -- or/c, and/c, not/c, =/c, </c, >/c, <=/c, >=/c, between/c, real-in, integer-in, char-in, listof, vectorof, hash/c, box/c, cons/c, list/c, syntax/c, struct/c, parameter/c, promise/c, flat-contract, any/c, none/c


  ---------------------------------------------------------------------------
  -- Structs
  ---------------------------------------------------------------------------

  highlight(0, 'racketStruct',            { fg = colors.blue,       bg = 'NONE' })  -- struct
  highlight(0, 'racketDefineStruct',      { fg = colors.blue,       bg = 'NONE' })  -- define-struct, define-struct/derived
  highlight(0, 'racketStructOption',      { fg = colors.pink,       bg = 'NONE' })  -- #:mutable, #:transparent, #:prefab, #:auto-value, #:guard, #:property, #:inspector, #:super, #:constructor-name, #:extra-constructor-name, #:omit-define-syntaxes, #:omit-define-values


  ---------------------------------------------------------------------------
  -- Classes and Objects (Racket-specific)
  ---------------------------------------------------------------------------

  highlight(0, 'racketClass',             { fg = colors.blue,       bg = 'NONE' })  -- class, class*, class/derived
  highlight(0, 'racketInterface',         { fg = colors.blue,       bg = 'NONE' })  -- interface, interface*
  highlight(0, 'racketMixin',             { fg = colors.blue,       bg = 'NONE' })  -- mixin

  -- Class Members
  highlight(0, 'racketPublic',            { fg = colors.blue,       bg = 'NONE' })  -- public, public*, pubment, pubment*, public-final, public-final*
  highlight(0, 'racketPrivate',           { fg = colors.blue,       bg = 'NONE' })  -- private*
  highlight(0, 'racketOverride',          { fg = colors.blue,       bg = 'NONE' })  -- override, override*, overment, overment*, override-final, override-final*
  highlight(0, 'racketAugment',           { fg = colors.blue,       bg = 'NONE' })  -- augment, augment*, augride, augride*, augment-final, augment-final*
  highlight(0, 'racketAbstract',          { fg = colors.blue,       bg = 'NONE' })  -- abstract
  highlight(0, 'racketInherit',           { fg = colors.blue,       bg = 'NONE' })  -- inherit, inherit/super, inherit/inner
  highlight(0, 'racketInit',              { fg = colors.blue,       bg = 'NONE' })  -- init, init-field, init-rest
  highlight(0, 'racketField',             { fg = colors.blue,       bg = 'NONE' })  -- field
  highlight(0, 'racketSuper',             { fg = colors.blue,       bg = 'NONE' })  -- super, inner, rename-inner, rename-super
  highlight(0, 'racketDefinePublic',      { fg = colors.blue,       bg = 'NONE' })  -- define/public, define/pubment, define/public-final
  highlight(0, 'racketDefinePrivate',     { fg = colors.blue,       bg = 'NONE' })  -- define/private
  highlight(0, 'racketDefineOverride',    { fg = colors.blue,       bg = 'NONE' })  -- define/override, define/overment, define/override-final
  highlight(0, 'racketDefineAugment',     { fg = colors.blue,       bg = 'NONE' })  -- define/augment, define/augride, define/augment-final

  -- Object Creation
  highlight(0, 'racketNew',               { fg = colors.blue,       bg = 'NONE' })  -- new
  highlight(0, 'racketMakeObject',        { fg = colors.blue,       bg = 'NONE' })  -- make-object
  highlight(0, 'racketInstantiate',       { fg = colors.blue,       bg = 'NONE' })  -- instantiate
  highlight(0, 'racketSend',              { fg = colors.blue,       bg = 'NONE' })  -- send, send*, send/apply, send/keyword-apply, dynamic-send
  highlight(0, 'racketGetField',          { fg = colors.blue,       bg = 'NONE' })  -- get-field, set-field!, field-bound?


  ---------------------------------------------------------------------------
  -- Continuations and Control
  ---------------------------------------------------------------------------

  highlight(0, 'racketCallCC',            { fg = colors.blue,       bg = 'NONE' })  -- call/cc, call-with-current-continuation
  highlight(0, 'racketCallEC',            { fg = colors.blue,       bg = 'NONE' })  -- call/ec, call-with-escape-continuation
  highlight(0, 'racketCallComp',          { fg = colors.blue,       bg = 'NONE' })  -- call/comp, call-with-composable-continuation
  highlight(0, 'racketPrompt',            { fg = colors.blue,       bg = 'NONE' })  -- prompt, prompt-at, prompt0, prompt0-at
  highlight(0, 'racketControl',           { fg = colors.blue,       bg = 'NONE' })  -- control, control-at, control0, control0-at
  highlight(0, 'racketShift',             { fg = colors.blue,       bg = 'NONE' })  -- shift, shift-at, shift0, shift0-at
  highlight(0, 'racketReset',             { fg = colors.blue,       bg = 'NONE' })  -- reset, reset-at, reset0, reset0-at
  highlight(0, 'racketAbort',             { fg = colors.blue,       bg = 'NONE' })  -- abort, abort/cc
  highlight(0, 'racketLetCC',             { fg = colors.blue,       bg = 'NONE' })  -- let/cc, let/ec
  highlight(0, 'racketDynamicWind',       { fg = colors.blue,       bg = 'NONE' })  -- dynamic-wind
  highlight(0, 'racketContinuationMark',  { fg = colors.blue,       bg = 'NONE' })  -- with-continuation-mark


  ---------------------------------------------------------------------------
  -- Exception Handling
  ---------------------------------------------------------------------------

  highlight(0, 'racketWithHandlers',      { fg = colors.blue,       bg = 'NONE' })  -- with-handlers, with-handlers*
  highlight(0, 'racketRaise',             { fg = colors.blue,       bg = 'NONE' })  -- raise, raise-user-error, raise-argument-error, raise-result-error
  highlight(0, 'racketError',             { fg = colors.blue,       bg = 'NONE' })  -- error

  -- Exception Types
  highlight(0, 'racketExnType',           { link = "Type" })  -- exn, exn:fail, exn:fail:contract, exn:fail:syntax, exn:fail:read, exn:fail:filesystem, exn:fail:network, exn:break


  ---------------------------------------------------------------------------
  -- Parameters
  ---------------------------------------------------------------------------

  highlight(0, 'racketParameterize',      { fg = colors.blue,       bg = 'NONE' })  -- parameterize, parameterize*
  highlight(0, 'racketMakeParameter',     { fg = colors.orange,     bg = 'NONE' })  -- make-parameter, make-derived-parameter


  ---------------------------------------------------------------------------
  -- Quoting and Syntax
  ---------------------------------------------------------------------------

  highlight(0, 'racketQuoteSyntax',       { fg = colors.blue,       bg = 'NONE' })  -- quote-syntax
  highlight(0, 'racketSyntaxRules',       { fg = colors.blue,       bg = 'NONE' })  -- syntax-rules
  highlight(0, 'racketSyntaxCase',        { fg = colors.blue,       bg = 'NONE' })  -- syntax-case
  highlight(0, 'racketSyntaxId',          { fg = colors.blue,       bg = 'NONE' })  -- syntax-id-rules


  ---------------------------------------------------------------------------
  -- Assignment
  ---------------------------------------------------------------------------

  highlight(0, 'racketSet',               { fg = colors.blue,       bg = 'NONE' })  -- set!, set!-values


  ---------------------------------------------------------------------------
  -- Iteration
  ---------------------------------------------------------------------------

  highlight(0, 'racketDo',                { fg = colors.blue,       bg = 'NONE' })  -- do


  ---------------------------------------------------------------------------
  -- Promises/Lazy
  ---------------------------------------------------------------------------

  highlight(0, 'racketDelay',             { fg = colors.blue,       bg = 'NONE' })  -- delay, lazy, delay/name, delay/strict, delay/sync, delay/thread, delay/idle
  highlight(0, 'racketForce',             { fg = colors.orange,     bg = 'NONE' })  -- force


  ---------------------------------------------------------------------------
  -- I/O Functions
  ---------------------------------------------------------------------------

  highlight(0, 'racketIO',                { fg = colors.orange,     bg = 'NONE' })  -- read, write, display, displayln, print, fprintf, printf, eprintf, format
  highlight(0, 'racketPort',              { fg = colors.orange,     bg = 'NONE' })  -- open-input-file, open-output-file, close-input-port, close-output-port, current-input-port, current-output-port


  ---------------------------------------------------------------------------
  -- Typed Racket
  ---------------------------------------------------------------------------

  highlight(0, 'racketTyped',             { link = "Type" })  -- :, :type, ann, inst, row-inst
  highlight(0, 'racketTypeAnnotation',    { link = "Type" })  -- Type annotations
  highlight(0, 'racketDefineType',        { link = "Type" })  -- define-type
  highlight(0, 'racketTypedLambda',       { link = "Type" })  -- lambda:, λ:
  highlight(0, 'racketTypedDefine',       { link = "Type" })  -- define:
  highlight(0, 'racketTypedLet',          { link = "Type" })  -- let:, let*:, letrec:
  highlight(0, 'racketTypedStruct',       { link = "Type" })  -- struct:
  highlight(0, 'racketCast',              { fg = colors.blue,       bg = 'NONE' })  -- cast, assert


  ---------------------------------------------------------------------------
  -- Scribble (Documentation)
  ---------------------------------------------------------------------------

  highlight(0, 'scribbleAt',              { fg = colors.pink,       bg = 'NONE' })  -- @ syntax
  highlight(0, 'scribbleKeyword',         { link = "Keyword" })  -- @title, @section, @subsection, @defproc, @defform, @racket, @racketblock
  highlight(0, 'scribbleText',            { fg = colors.white,      bg = 'NONE' })  -- Text content


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.racket)
  ---------------------------------------------------------------------------

  -- Variables
  highlight(0, '@variable.racket',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.parameter.racket',    { link = "Variable" })  -- Parameters
  highlight(0, '@variable.builtin.racket',      { link = "Variable" })  -- Language names, special vars

  -- Constants
  highlight(0, '@constant.racket',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.racket',      { link = "Constant" })  -- #t, #f

  -- Booleans
  highlight(0, '@boolean.racket',               { link = "Boolean" })  -- #t, #f, #true, #false

  -- Numbers
  highlight(0, '@number.racket',                { link = "Number" })  -- Numbers
  highlight(0, '@number.float.racket',          { link = "Number" })  -- Floats

  -- Strings
  highlight(0, '@string.racket',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.racket',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.regexp.racket',         { link = "String" })  -- Regular expressions
  highlight(0, '@string.special.symbol.racket', { link = "String" })  -- Symbols, keywords

  -- Characters
  highlight(0, '@character.racket',             { fg = colors.redLight,   bg = 'NONE' })  -- #\char
  highlight(0, '@character.special.racket',     { fg = colors.pink,       bg = 'NONE' })  -- #\newline, #\space

  -- Functions
  highlight(0, '@function.racket',              { link = "Function" })  -- Function definitions/calls
  highlight(0, '@function.call.racket',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.racket',      { link = "Function" })  -- Built-in procedures
  highlight(0, '@function.macro.racket',        { link = "Function" })  -- Macros

  -- Keywords
  highlight(0, '@keyword.racket',               { link = "Keyword" })  -- Special forms
  highlight(0, '@keyword.function.racket',      { link = "Keyword" })  -- define, lambda
  highlight(0, '@keyword.conditional.racket',   { link = "Conditional" })  -- if, cond, case, when, unless
  highlight(0, '@keyword.repeat.racket',        { link = "Keyword" })  -- for, for/list, do
  highlight(0, '@keyword.import.racket',        { link = "Keyword" })  -- require
  highlight(0, '@keyword.export.racket',        { link = "Keyword" })  -- provide
  highlight(0, '@keyword.exception.racket',     { link = "Keyword" })  -- with-handlers, raise

  -- Modules
  highlight(0, '@module.racket',                { fg = colors.turquoise,  bg = 'NONE' })  -- Module names

  -- Types
  highlight(0, '@type.racket',                  { link = "Type" })  -- Types (Typed Racket)
  highlight(0, '@type.builtin.racket',          { link = "Type" })  -- Built-in types

  -- Operators
  highlight(0, '@operator.racket',              { link = "Operator" })  -- +, -, *, /, =, <, >, etc.

  -- Punctuation
  highlight(0, '@punctuation.bracket.racket',   { fg = colors.white,      bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.racket', { link = "Delimiter" })  -- Delimiters
  highlight(0, '@punctuation.special.racket',   { fg = colors.pink,       bg = 'NONE' })  -- ', `, ,, ,@, #, @

  -- Comments
  highlight(0, '@comment.racket',               { link = "Comment" })  -- Comments

  -- Attributes
  highlight(0, '@attribute.racket',             { fg = colors.pink,       bg = 'NONE' })  -- #:keyword attributes


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.racket)
  ---------------------------------------------------------------------------

  highlight(0, '@lsp.type.variable.racket',     { link = "Variable" })
  highlight(0, '@lsp.type.parameter.racket',    { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.function.racket',     { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.macro.racket',        { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.type.racket',         { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.class.racket',        { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.namespace.racket',    { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.racket',      { link = "Keyword" })
  highlight(0, '@lsp.type.string.racket',       { link = "String" })
  highlight(0, '@lsp.type.number.racket',       { link = "Number" })
  highlight(0, '@lsp.type.comment.racket',      { link = "Comment" })

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.racket',    { link = "Variable" })
  highlight(0, '@lsp.typemod.function.declaration.racket', { fg = colors.orange, bg = 'NONE' })
  highlight(0, '@lsp.typemod.function.defaultLibrary.racket', { fg = colors.orange, bg = 'NONE' })


  ---------------------------------------------------------------------------
  -- Rainbow Parentheses
  ---------------------------------------------------------------------------

  highlight(0, 'racketLevel0',            { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'racketLevel1',            { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'racketLevel2',            { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'racketLevel3',            { fg = colors.pink,       bg = 'NONE' })
  highlight(0, 'racketLevel4',            { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'racketLevel5',            { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'racketLevel6',            { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, 'racketLevel7',            { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, 'racketLevel8',            { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'racketLevel9',            { fg = colors.orange,     bg = 'NONE' })
end

return racket
