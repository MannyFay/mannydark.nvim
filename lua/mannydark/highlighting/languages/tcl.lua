-------------------------------------------------------------------------------
-- Tcl Files
-- Highlighting for .tcl, .tk files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local tcl     = {}


-------------------------------------------------------------------------------
-- Settings

tcl.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'tclConditional',       { fg = colors.blue,       bg = 'NONE'            })  -- if, then, else, elseif, switch
  highlight(0, 'tclRepeat',            { fg = colors.blue,       bg = 'NONE'            })  -- while, for, foreach, break, continue
  highlight(0, 'tclLooping',           { fg = colors.blue,       bg = 'NONE'            })  -- Loop constructs
  highlight(0, 'tclLabel',             { fg = colors.blue,       bg = 'NONE'            })  -- default, case labels

  -- Keywords - Exception Handling
  highlight(0, 'tclException',         { fg = colors.blue,       bg = 'NONE'            })  -- catch, try, throw, finally, error

  -- Keywords - Procedures
  highlight(0, 'tclProcCommand',       { fg = colors.blue,       bg = 'NONE'            })  -- proc, apply, coroutine, return, tailcall, yield, yieldto
  highlight(0, 'tclProc',              { fg = colors.blue,       bg = 'NONE'            })  -- proc keyword
  highlight(0, 'tclProcName',          { fg = colors.orange,     bg = 'NONE'            })  -- Procedure names

  -- Keywords - General
  highlight(0, 'tclKeyword',           { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'tclStatement',         { fg = colors.blue,       bg = 'NONE'            })  -- Statement keywords
  highlight(0, 'tclPrimary',           { fg = colors.blue,       bg = 'NONE'            })  -- Primary commands

  -- Built-in Commands - Core
  highlight(0, 'tclCommand',           { fg = colors.orange,     bg = 'NONE'            })  -- Built-in commands
  highlight(0, 'tcltkCommand',         { fg = colors.orange,     bg = 'NONE'            })  -- Tcl/Tk commands
  highlight(0, 'tcltkCommandColor',    { fg = colors.orange,     bg = 'NONE'            })  -- Command color

  -- Built-in Commands - Specific Categories
  highlight(0, 'tclAfter',             { fg = colors.orange,     bg = 'NONE'            })  -- after
  highlight(0, 'tclAppend',            { fg = colors.orange,     bg = 'NONE'            })  -- append
  highlight(0, 'tclArray',             { fg = colors.orange,     bg = 'NONE'            })  -- array
  highlight(0, 'tclBinary',            { fg = colors.orange,     bg = 'NONE'            })  -- binary
  highlight(0, 'tclChan',              { fg = colors.orange,     bg = 'NONE'            })  -- chan
  highlight(0, 'tclClock',             { fg = colors.orange,     bg = 'NONE'            })  -- clock
  highlight(0, 'tclDict',              { fg = colors.orange,     bg = 'NONE'            })  -- dict
  highlight(0, 'tclEncoding',          { fg = colors.orange,     bg = 'NONE'            })  -- encoding
  highlight(0, 'tclExec',              { fg = colors.orange,     bg = 'NONE'            })  -- exec
  highlight(0, 'tclExpr',              { fg = colors.orange,     bg = 'NONE'            })  -- expr
  highlight(0, 'tclFile',              { fg = colors.orange,     bg = 'NONE'            })  -- file
  highlight(0, 'tclFileevent',         { fg = colors.orange,     bg = 'NONE'            })  -- fileevent
  highlight(0, 'tclFormat',            { fg = colors.orange,     bg = 'NONE'            })  -- format
  highlight(0, 'tclGlob',              { fg = colors.orange,     bg = 'NONE'            })  -- glob
  highlight(0, 'tclHistory',           { fg = colors.orange,     bg = 'NONE'            })  -- history
  highlight(0, 'tclInfo',              { fg = colors.orange,     bg = 'NONE'            })  -- info
  highlight(0, 'tclInterp',            { fg = colors.orange,     bg = 'NONE'            })  -- interp
  highlight(0, 'tclList',              { fg = colors.orange,     bg = 'NONE'            })  -- list commands (lappend, lindex, linsert, etc.)
  highlight(0, 'tclNamespace',         { fg = colors.orange,     bg = 'NONE'            })  -- namespace
  highlight(0, 'tclOpen',              { fg = colors.orange,     bg = 'NONE'            })  -- open
  highlight(0, 'tclPackage',           { fg = colors.orange,     bg = 'NONE'            })  -- package
  highlight(0, 'tclPuts',              { fg = colors.orange,     bg = 'NONE'            })  -- puts
  highlight(0, 'tclRead',              { fg = colors.orange,     bg = 'NONE'            })  -- read
  highlight(0, 'tclRegexp',            { fg = colors.orange,     bg = 'NONE'            })  -- regexp
  highlight(0, 'tclRegsub',            { fg = colors.orange,     bg = 'NONE'            })  -- regsub
  highlight(0, 'tclScan',              { fg = colors.orange,     bg = 'NONE'            })  -- scan
  highlight(0, 'tclSeek',              { fg = colors.orange,     bg = 'NONE'            })  -- seek
  highlight(0, 'tclSet',               { fg = colors.orange,     bg = 'NONE'            })  -- set
  highlight(0, 'tclSocket',            { fg = colors.orange,     bg = 'NONE'            })  -- socket
  highlight(0, 'tclSource',            { fg = colors.orange,     bg = 'NONE'            })  -- source
  highlight(0, 'tclString',            { fg = colors.orange,     bg = 'NONE'            })  -- string commands
  highlight(0, 'tclSubst',             { fg = colors.orange,     bg = 'NONE'            })  -- subst
  highlight(0, 'tclTrace',             { fg = colors.orange,     bg = 'NONE'            })  -- trace
  highlight(0, 'tclUnset',             { fg = colors.orange,     bg = 'NONE'            })  -- unset
  highlight(0, 'tclUpdate',            { fg = colors.orange,     bg = 'NONE'            })  -- update
  highlight(0, 'tclUplevel',           { fg = colors.orange,     bg = 'NONE'            })  -- uplevel
  highlight(0, 'tclUpvar',             { fg = colors.orange,     bg = 'NONE'            })  -- upvar
  highlight(0, 'tclVariable',          { fg = colors.orange,     bg = 'NONE'            })  -- variable
  highlight(0, 'tclVwait',             { fg = colors.orange,     bg = 'NONE'            })  -- vwait

  -- Scope/Variable Keywords
  highlight(0, 'tclGlobal',            { fg = colors.blue,       bg = 'NONE'            })  -- global
  highlight(0, 'tclUpvarLevel',        { fg = colors.blue,       bg = 'NONE'            })  -- Upvar level specifiers

  -- Namespace Commands
  highlight(0, 'tclNamespaceSwitch',   { fg = colors.blue,       bg = 'NONE'            })  -- children, code, current, delete, eval, export, import, etc.
  highlight(0, 'tclMagicName',         { fg = colors.pink,       bg = 'NONE'            })  -- Magic namespace names

  -- TclOO Commands
  highlight(0, 'ooKeyword',            { fg = colors.blue,       bg = 'NONE'            })  -- OO keywords
  highlight(0, 'tclOOClass',           { fg = colors.blue,       bg = 'NONE'            })  -- oo::class, class
  highlight(0, 'tclOODefine',          { fg = colors.blue,       bg = 'NONE'            })  -- oo::define
  highlight(0, 'tclOOObjdefine',       { fg = colors.blue,       bg = 'NONE'            })  -- oo::objdefine
  highlight(0, 'tclOOMethod',          { fg = colors.blue,       bg = 'NONE'            })  -- method
  highlight(0, 'tclOOConstructor',     { fg = colors.blue,       bg = 'NONE'            })  -- constructor
  highlight(0, 'tclOODestructor',      { fg = colors.blue,       bg = 'NONE'            })  -- destructor
  highlight(0, 'tclOOMixin',           { fg = colors.blue,       bg = 'NONE'            })  -- mixin
  highlight(0, 'tclOOSuperclass',      { fg = colors.blue,       bg = 'NONE'            })  -- superclass
  highlight(0, 'tclOOSelf',            { fg = colors.blue,       bg = 'NONE'            })  -- self
  highlight(0, 'tclOOMy',              { fg = colors.blue,       bg = 'NONE'            })  -- my
  highlight(0, 'tclOONext',            { fg = colors.blue,       bg = 'NONE'            })  -- next, nextto

  -- Math Functions
  highlight(0, 'tclMaths',             { fg = colors.orange,     bg = 'NONE'            })  -- Math functions in expr
  highlight(0, 'tcltkMaths',           { fg = colors.orange,     bg = 'NONE'            })  -- abs, acos, asin, atan, atan2, ceil, cos, cosh, double, exp, floor, fmod, hypot, int, log, log10, max, min, pow, rand, round, sin, sinh, sqrt, srand, tan, tanh, wide

  -- Variables
  highlight(0, 'tclVarRef',            { fg = colors.purple,     bg = 'NONE'            })  -- $variable references
  highlight(0, 'tclVar',               { fg = colors.purple,     bg = 'NONE'            })  -- Variable names
  highlight(0, 'tclVarBrace',          { fg = colors.purple,     bg = 'NONE'            })  -- ${variable}
  highlight(0, 'tclVarParen',          { fg = colors.purple,     bg = 'NONE'            })  -- $array(index)

  -- Special Variables
  highlight(0, 'tclVars',              { fg = colors.pink,       bg = 'NONE'            })  -- Built-in variables
  highlight(0, 'tclSpecialVar',        { fg = colors.pink,       bg = 'NONE'            })  -- argc, argv, argv0, env, errorCode, errorInfo, etc.
  highlight(0, 'tclEnv',               { fg = colors.pink,       bg = 'NONE'            })  -- env array
  highlight(0, 'tclTclPlatform',       { fg = colors.pink,       bg = 'NONE'            })  -- tcl_platform array

  -- Types/Classes
  highlight(0, 'tclType',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'tclClassName',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names
  highlight(0, 'tclNamespaceName',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespace names

  -- Booleans
  highlight(0, 'tclBoolean',           { fg = colors.blue,       bg = 'NONE'            })  -- true, false, on, off, yes, no

  -- Strings
  highlight(0, 'tclStringLiteral',     { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'tclQuote',             { fg = colors.redLight,   bg = 'NONE'            })  -- Quoted strings
  highlight(0, 'tclBraces',            { fg = colors.redLight,   bg = 'NONE'            })  -- {braced strings}

  -- String Escapes
  highlight(0, 'tclSpecial',           { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences
  highlight(0, 'tclEscape',            { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, \x, etc.

  -- Numbers
  highlight(0, 'tclNumber',            { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'tclInteger',           { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'tclFloat',             { fg = colors.greenLight, bg = 'NONE'            })  -- Floating-point
  highlight(0, 'tclHex',               { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex
  highlight(0, 'tclOctal',             { fg = colors.greenLight, bg = 'NONE'            })  -- 0o octal

  -- Operators
  highlight(0, 'tclOperator',          { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'tclMathOp',            { fg = colors.white,      bg = 'NONE'            })  -- Math operators in expr
  highlight(0, 'tclCompareOp',         { fg = colors.white,      bg = 'NONE'            })  -- Comparison operators
  highlight(0, 'tclLogicalOp',         { fg = colors.white,      bg = 'NONE'            })  -- && || !
  highlight(0, 'tclBitOp',             { fg = colors.white,      bg = 'NONE'            })  -- & | ^ ~ << >>
  highlight(0, 'tclTernary',           { fg = colors.white,      bg = 'NONE'            })  -- ? : ternary
  highlight(0, 'tclExpand',            { fg = colors.pink,       bg = 'NONE'            })  -- {*} expansion operator

  -- Switches/Options
  highlight(0, 'tcltkSwitch',          { fg = colors.pink,       bg = 'NONE'            })  -- -option switches
  highlight(0, 'tclOption',            { fg = colors.pink,       bg = 'NONE'            })  -- Command options
  highlight(0, 'tclFlag',              { fg = colors.pink,       bg = 'NONE'            })  -- Flags

  -- Subcommand Options
  highlight(0, 'tcltkStringSwitch',    { fg = colors.blue,       bg = 'NONE'            })  -- string subcommands
  highlight(0, 'tcltkArraySwitch',     { fg = colors.blue,       bg = 'NONE'            })  -- array subcommands
  highlight(0, 'tcltkLsortSwitch',     { fg = colors.blue,       bg = 'NONE'            })  -- lsort options
  highlight(0, 'tcltkPackSwitch',      { fg = colors.blue,       bg = 'NONE'            })  -- pack options
  highlight(0, 'tcltkNamespaceSwitch', { fg = colors.blue,       bg = 'NONE'            })  -- namespace subcommands

  -- Tk Widget Commands
  highlight(0, 'tkKeyword',            { fg = colors.blue,       bg = 'NONE'            })  -- Tk keywords
  highlight(0, 'tkWidget',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Widget names
  highlight(0, 'tkWidgetCommand',      { fg = colors.orange,     bg = 'NONE'            })  -- button, label, entry, canvas, frame, toplevel, etc.
  highlight(0, 'tkDialog',             { fg = colors.orange,     bg = 'NONE'            })  -- Dialog commands
  highlight(0, 'tkReserved',           { fg = colors.blue,       bg = 'NONE'            })  -- Reserved Tk keywords

  -- Tk Geometry Managers
  highlight(0, 'tkPack',               { fg = colors.orange,     bg = 'NONE'            })  -- pack
  highlight(0, 'tkGrid',               { fg = colors.orange,     bg = 'NONE'            })  -- grid
  highlight(0, 'tkPlace',              { fg = colors.orange,     bg = 'NONE'            })  -- place

  -- Tk Widget Options
  highlight(0, 'tcltkWidgetSwitch',    { fg = colors.pink,       bg = 'NONE'            })  -- -background, -foreground, -text, -command, etc.

  -- Tk Event Handling
  highlight(0, 'tkEvent',              { fg = colors.pink,       bg = 'NONE'            })  -- <Button-1>, <Key>, etc.
  highlight(0, 'tkBind',               { fg = colors.orange,     bg = 'NONE'            })  -- bind

  -- Comments
  highlight(0, 'tclComment',           { fg = colors.red,        bg = 'NONE'            })  -- # comments
  highlight(0, 'tclTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Error
  highlight(0, 'tclError',             { fg = colors.red,        bg = 'NONE'            })  -- Errors

  -- Substitution
  highlight(0, 'tclCmdSubst',          { fg = colors.purple,     bg = 'NONE'            })  -- [command substitution]
  highlight(0, 'tclVarSubst',          { fg = colors.purple,     bg = 'NONE'            })  -- $variable substitution
  highlight(0, 'tclBackslashSubst',    { fg = colors.pink,       bg = 'NONE'            })  -- Backslash substitution


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.tcl)

  -- Variables
  highlight(0, '@variable.tcl',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.tcl',      { fg = colors.pink,      bg = 'NONE' })  -- argc, argv, env, errorCode, errorInfo, etc.
  highlight(0, '@variable.parameter.tcl',    { fg = colors.purple,    bg = 'NONE' })  -- Procedure parameters

  -- Constants
  highlight(0, '@constant.tcl',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.tcl',      { fg = colors.blue,      bg = 'NONE' })  -- true, false, yes, no, on, off

  -- Functions
  highlight(0, '@function.tcl',              { fg = colors.orange,    bg = 'NONE' })  -- Procedure definitions
  highlight(0, '@function.call.tcl',         { fg = colors.orange,    bg = 'NONE' })  -- Procedure calls
  highlight(0, '@function.builtin.tcl',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in commands (puts, gets, cd, exec, etc.)
  highlight(0, '@function.method.tcl',       { fg = colors.orange,    bg = 'NONE' })  -- OO methods
  highlight(0, '@function.method.call.tcl',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls

  -- Types
  highlight(0, '@type.tcl',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type/class names
  highlight(0, '@type.builtin.tcl',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types

  -- Modules/Namespaces
  highlight(0, '@module.tcl',                { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces

  -- Keywords
  highlight(0, '@keyword.tcl',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.tcl',      { fg = colors.blue,      bg = 'NONE' })  -- proc
  highlight(0, '@keyword.type.tcl',          { fg = colors.blue,      bg = 'NONE' })  -- namespace, class declarations
  highlight(0, '@keyword.operator.tcl',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@keyword.return.tcl',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.repeat.tcl',        { fg = colors.blue,      bg = 'NONE' })  -- while, for, foreach
  highlight(0, '@keyword.conditional.tcl',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, elseif, switch
  highlight(0, '@keyword.exception.tcl',     { fg = colors.blue,      bg = 'NONE' })  -- catch, try, throw, finally
  highlight(0, '@keyword.import.tcl',        { fg = colors.pink,      bg = 'NONE' })  -- package require, source
  highlight(0, '@keyword.coroutine.tcl',     { fg = colors.blue,      bg = 'NONE' })  -- coroutine, yield, yieldto

  -- Labels
  highlight(0, '@label.tcl',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels

  -- Strings
  highlight(0, '@string.tcl',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.tcl',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@character.tcl',             { fg = colors.redLight,  bg = 'NONE' })  -- Characters
  highlight(0, '@character.special.tcl',     { fg = colors.pink,      bg = 'NONE' })  -- Special characters

  -- Numbers
  highlight(0, '@number.tcl',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.tcl',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.tcl',               { fg = colors.blue,      bg = 'NONE' })  -- true, false, yes, no, on, off

  -- Comments
  highlight(0, '@comment.tcl',               { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- Operators and Punctuation
  highlight(0, '@operator.tcl',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.tcl',   { fg = colors.white,     bg = 'NONE' })  -- [], {}, ()
  highlight(0, '@punctuation.delimiter.tcl', { fg = colors.white,     bg = 'NONE' })  -- ; :
  highlight(0, '@punctuation.special.tcl',   { fg = colors.pink,      bg = 'NONE' })  -- $ in variables, {*} expansion


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.tcl)

  highlight(0, '@lsp.type.variable.tcl',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.tcl',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.tcl',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.tcl',      { fg = colors.orange,    bg = 'NONE' })  -- Procedures
  highlight(0, '@lsp.type.method.tcl',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.type.tcl',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.tcl',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.namespace.tcl',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.keyword.tcl',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.tcl',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.tcl',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.tcl',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.tcl',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.tcl',    { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@lsp.typemod.function.declaration.tcl', { fg = colors.orange,    bg = 'NONE' })  -- Procedure declarations
  highlight(0, '@lsp.typemod.function.defaultLibrary.tcl', { fg = colors.orange, bg = 'NONE' })  -- Built-in commands
  highlight(0, '@lsp.typemod.class.declaration.tcl',    { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
end

return tcl

