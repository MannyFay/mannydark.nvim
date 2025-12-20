-------------------------------------------------------------------------------
-- Logo Programming Language
-- Highlighting for Logo and its dialects (UCBLogo, Terrapin Logo, etc.)
-- Known for turtle graphics and educational programming
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local logo      = {}


-------------------------------------------------------------------------------
-- Settings

logo.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Procedure Definition

  highlight(0, 'logoKeyword',             { link = "Keyword" })  -- Keywords
  highlight(0, 'logoTo',                  { fg = colors.blue,       bg = 'NONE' })  -- to (procedure definition)
  highlight(0, 'logoEnd',                 { fg = colors.blue,       bg = 'NONE' })  -- end (procedure end)
  highlight(0, 'logoDefine',              { fg = colors.blue,       bg = 'NONE' })  -- define
  highlight(0, 'logoProcedure',           { fg = colors.orange,     bg = 'NONE' })  -- Procedure names


  -----------------------------------------------------------------------------
  -- Control Structures

  highlight(0, 'logoControl',             { fg = colors.blue,       bg = 'NONE' })  -- Control structures
  highlight(0, 'logoRepeat',              { fg = colors.blue,       bg = 'NONE' })  -- repeat
  highlight(0, 'logoForever',             { fg = colors.blue,       bg = 'NONE' })  -- forever
  highlight(0, 'logoFor',                 { fg = colors.blue,       bg = 'NONE' })  -- for
  highlight(0, 'logoWhile',               { fg = colors.blue,       bg = 'NONE' })  -- while, do.while
  highlight(0, 'logoUntil',               { fg = colors.blue,       bg = 'NONE' })  -- until, do.until
  highlight(0, 'logoIf',                  { fg = colors.blue,       bg = 'NONE' })  -- if
  highlight(0, 'logoIfelse',              { fg = colors.blue,       bg = 'NONE' })  -- ifelse
  highlight(0, 'logoTest',                { fg = colors.blue,       bg = 'NONE' })  -- test
  highlight(0, 'logoIftrue',              { fg = colors.blue,       bg = 'NONE' })  -- iftrue (ift)
  highlight(0, 'logoIffalse',             { fg = colors.blue,       bg = 'NONE' })  -- iffalse (iff)
  highlight(0, 'logoCase',                { fg = colors.blue,       bg = 'NONE' })  -- case
  highlight(0, 'logoCond',                { fg = colors.blue,       bg = 'NONE' })  -- cond
  highlight(0, 'logoStop',                { fg = colors.blue,       bg = 'NONE' })  -- stop
  highlight(0, 'logoOutput',              { fg = colors.blue,       bg = 'NONE' })  -- output (op)
  highlight(0, 'logoCatch',               { fg = colors.blue,       bg = 'NONE' })  -- catch
  highlight(0, 'logoThrow',               { fg = colors.blue,       bg = 'NONE' })  -- throw
  highlight(0, 'logoError',               { fg = colors.blue,       bg = 'NONE' })  -- error
  highlight(0, 'logoPause',               { fg = colors.blue,       bg = 'NONE' })  -- pause
  highlight(0, 'logoContinue',            { fg = colors.blue,       bg = 'NONE' })  -- continue (co)
  highlight(0, 'logoWait',                { fg = colors.blue,       bg = 'NONE' })  -- wait
  highlight(0, 'logoBye',                 { fg = colors.blue,       bg = 'NONE' })  -- bye, quit
  highlight(0, 'logoGoto',                { fg = colors.blue,       bg = 'NONE' })  -- goto
  highlight(0, 'logoTag',                 { fg = colors.blue,       bg = 'NONE' })  -- tag
  highlight(0, 'logoRun',                 { fg = colors.blue,       bg = 'NONE' })  -- run, runresult
  highlight(0, 'logoIgnore',              { fg = colors.blue,       bg = 'NONE' })  -- ignore


  -----------------------------------------------------------------------------
  -- Turtle Motion Commands

  highlight(0, 'logoTurtleMotion',        { fg = colors.pink,       bg = 'NONE' })  -- Turtle motion
  highlight(0, 'logoForward',             { fg = colors.pink,       bg = 'NONE' })  -- forward (fd)
  highlight(0, 'logoBack',                { fg = colors.pink,       bg = 'NONE' })  -- back (bk)
  highlight(0, 'logoLeft',                { fg = colors.pink,       bg = 'NONE' })  -- left (lt)
  highlight(0, 'logoRight',               { fg = colors.pink,       bg = 'NONE' })  -- right (rt)
  highlight(0, 'logoSetpos',              { fg = colors.pink,       bg = 'NONE' })  -- setpos, setxy
  highlight(0, 'logoSetx',                { fg = colors.pink,       bg = 'NONE' })  -- setx
  highlight(0, 'logoSety',                { fg = colors.pink,       bg = 'NONE' })  -- sety
  highlight(0, 'logoSetheading',          { fg = colors.pink,       bg = 'NONE' })  -- setheading (seth)
  highlight(0, 'logoHome',                { fg = colors.pink,       bg = 'NONE' })  -- home
  highlight(0, 'logoArc',                 { fg = colors.pink,       bg = 'NONE' })  -- arc


  -----------------------------------------------------------------------------
  -- Turtle State Queries

  highlight(0, 'logoTurtleQuery',         { fg = colors.orange,     bg = 'NONE' })  -- Turtle queries
  highlight(0, 'logoPos',                 { fg = colors.orange,     bg = 'NONE' })  -- pos
  highlight(0, 'logoXcor',                { fg = colors.orange,     bg = 'NONE' })  -- xcor
  highlight(0, 'logoYcor',                { fg = colors.orange,     bg = 'NONE' })  -- ycor
  highlight(0, 'logoHeading',             { fg = colors.orange,     bg = 'NONE' })  -- heading
  highlight(0, 'logoTowards',             { fg = colors.orange,     bg = 'NONE' })  -- towards
  highlight(0, 'logoScrunch',             { fg = colors.orange,     bg = 'NONE' })  -- scrunch


  -----------------------------------------------------------------------------
  -- Pen Control

  highlight(0, 'logoPenControl',          { fg = colors.pink,       bg = 'NONE' })  -- Pen control
  highlight(0, 'logoPendown',             { fg = colors.pink,       bg = 'NONE' })  -- pendown (pd)
  highlight(0, 'logoPenup',               { fg = colors.pink,       bg = 'NONE' })  -- penup (pu)
  highlight(0, 'logoPenpaint',            { fg = colors.pink,       bg = 'NONE' })  -- penpaint (ppt)
  highlight(0, 'logoPenerase',            { fg = colors.pink,       bg = 'NONE' })  -- penerase (pe)
  highlight(0, 'logoPenreverse',          { fg = colors.pink,       bg = 'NONE' })  -- penreverse (px)
  highlight(0, 'logoSetpencolor',         { fg = colors.pink,       bg = 'NONE' })  -- setpencolor (setpc)
  highlight(0, 'logoSetpensize',          { fg = colors.pink,       bg = 'NONE' })  -- setpensize
  highlight(0, 'logoSetpenpattern',       { fg = colors.pink,       bg = 'NONE' })  -- setpenpattern
  highlight(0, 'logoSetpen',              { fg = colors.pink,       bg = 'NONE' })  -- setpen
  highlight(0, 'logoSetpalette',          { fg = colors.pink,       bg = 'NONE' })  -- setpalette
  highlight(0, 'logoSetbackground',       { fg = colors.pink,       bg = 'NONE' })  -- setbackground (setbg)


  -----------------------------------------------------------------------------
  -- Pen Queries

  highlight(0, 'logoPenQuery',            { fg = colors.orange,     bg = 'NONE' })  -- Pen queries
  highlight(0, 'logoPendownp',            { fg = colors.orange,     bg = 'NONE' })  -- pendownp
  highlight(0, 'logoPenmode',             { fg = colors.orange,     bg = 'NONE' })  -- penmode
  highlight(0, 'logoPencolor',            { fg = colors.orange,     bg = 'NONE' })  -- pencolor (pc)
  highlight(0, 'logoPalette',             { fg = colors.orange,     bg = 'NONE' })  -- palette
  highlight(0, 'logoPensize',             { fg = colors.orange,     bg = 'NONE' })  -- pensize
  highlight(0, 'logoPen',                 { fg = colors.orange,     bg = 'NONE' })  -- pen
  highlight(0, 'logoBackground',          { fg = colors.orange,     bg = 'NONE' })  -- background (bg)


  -----------------------------------------------------------------------------
  -- Window/Screen Control

  highlight(0, 'logoWindowControl',       { fg = colors.pink,       bg = 'NONE' })  -- Window control
  highlight(0, 'logoShowturtle',          { fg = colors.pink,       bg = 'NONE' })  -- showturtle (st)
  highlight(0, 'logoHideturtle',          { fg = colors.pink,       bg = 'NONE' })  -- hideturtle (ht)
  highlight(0, 'logoClean',               { fg = colors.pink,       bg = 'NONE' })  -- clean
  highlight(0, 'logoClearscreen',         { fg = colors.pink,       bg = 'NONE' })  -- clearscreen (cs)
  highlight(0, 'logoWrap',                { fg = colors.pink,       bg = 'NONE' })  -- wrap
  highlight(0, 'logoWindow',              { fg = colors.pink,       bg = 'NONE' })  -- window
  highlight(0, 'logoFence',               { fg = colors.pink,       bg = 'NONE' })  -- fence
  highlight(0, 'logoFill',                { fg = colors.pink,       bg = 'NONE' })  -- fill
  highlight(0, 'logoFilled',              { fg = colors.pink,       bg = 'NONE' })  -- filled
  highlight(0, 'logoLabel',               { fg = colors.pink,       bg = 'NONE' })  -- label
  highlight(0, 'logoSetlabelheight',      { fg = colors.pink,       bg = 'NONE' })  -- setlabelheight
  highlight(0, 'logoTextscreen',          { fg = colors.pink,       bg = 'NONE' })  -- textscreen (ts)
  highlight(0, 'logoFullscreen',          { fg = colors.pink,       bg = 'NONE' })  -- fullscreen (fs)
  highlight(0, 'logoSplitscreen',         { fg = colors.pink,       bg = 'NONE' })  -- splitscreen (ss)
  highlight(0, 'logoSetscrunch',          { fg = colors.pink,       bg = 'NONE' })  -- setscrunch
  highlight(0, 'logoRefresh',             { fg = colors.pink,       bg = 'NONE' })  -- refresh
  highlight(0, 'logoNorefresh',           { fg = colors.pink,       bg = 'NONE' })  -- norefresh
  highlight(0, 'logoDraw',                { fg = colors.pink,       bg = 'NONE' })  -- draw
  highlight(0, 'logoGrid',                { fg = colors.pink,       bg = 'NONE' })  -- grid
  highlight(0, 'logoGridon',              { fg = colors.pink,       bg = 'NONE' })  -- gridon
  highlight(0, 'logoGridoff',             { fg = colors.pink,       bg = 'NONE' })  -- gridoff


  -----------------------------------------------------------------------------
  -- Color Commands

  highlight(0, 'logoColorCmd',            { fg = colors.pink,       bg = 'NONE' })  -- Color commands
  highlight(0, 'logoSetpc',               { fg = colors.pink,       bg = 'NONE' })  -- setpc
  highlight(0, 'logoSetbg',               { fg = colors.pink,       bg = 'NONE' })  -- setbg
  highlight(0, 'logoColor',               { fg = colors.orange,     bg = 'NONE' })  -- color
  highlight(0, 'logoColorname',           { fg = colors.orange,     bg = 'NONE' })  -- colorname
  highlight(0, 'logoColors',              { fg = colors.orange,     bg = 'NONE' })  -- colors


  -----------------------------------------------------------------------------
  -- Data Structure Primitives - Constructors

  highlight(0, 'logoConstructor',         { fg = colors.orange,     bg = 'NONE' })  -- Constructors
  highlight(0, 'logoWord',                { fg = colors.orange,     bg = 'NONE' })  -- word
  highlight(0, 'logoList',                { fg = colors.orange,     bg = 'NONE' })  -- list
  highlight(0, 'logoSentence',            { fg = colors.orange,     bg = 'NONE' })  -- sentence (se)
  highlight(0, 'logoFput',                { fg = colors.orange,     bg = 'NONE' })  -- fput
  highlight(0, 'logoLput',                { fg = colors.orange,     bg = 'NONE' })  -- lput
  highlight(0, 'logoArray',               { fg = colors.orange,     bg = 'NONE' })  -- array
  highlight(0, 'logoMdarray',             { fg = colors.orange,     bg = 'NONE' })  -- mdarray
  highlight(0, 'logoListtoarray',         { fg = colors.orange,     bg = 'NONE' })  -- listtoarray
  highlight(0, 'logoArraytolist',         { fg = colors.orange,     bg = 'NONE' })  -- arraytolist
  highlight(0, 'logoCombine',             { fg = colors.orange,     bg = 'NONE' })  -- combine
  highlight(0, 'logoReverse',             { fg = colors.orange,     bg = 'NONE' })  -- reverse
  highlight(0, 'logoGensym',              { fg = colors.orange,     bg = 'NONE' })  -- gensym


  -----------------------------------------------------------------------------
  -- Data Structure Primitives - Selectors

  highlight(0, 'logoSelector',            { fg = colors.orange,     bg = 'NONE' })  -- Selectors
  highlight(0, 'logoFirst',               { fg = colors.orange,     bg = 'NONE' })  -- first
  highlight(0, 'logoFirsts',              { fg = colors.orange,     bg = 'NONE' })  -- firsts
  highlight(0, 'logoLast',                { fg = colors.orange,     bg = 'NONE' })  -- last
  highlight(0, 'logoButfirst',            { fg = colors.orange,     bg = 'NONE' })  -- butfirst (bf)
  highlight(0, 'logoButfirsts',           { fg = colors.orange,     bg = 'NONE' })  -- butfirsts (bfs)
  highlight(0, 'logoButlast',             { fg = colors.orange,     bg = 'NONE' })  -- butlast (bl)
  highlight(0, 'logoItem',                { fg = colors.orange,     bg = 'NONE' })  -- item
  highlight(0, 'logoMditem',              { fg = colors.orange,     bg = 'NONE' })  -- mditem
  highlight(0, 'logoPick',                { fg = colors.orange,     bg = 'NONE' })  -- pick
  highlight(0, 'logoRemove',              { fg = colors.orange,     bg = 'NONE' })  -- remove
  highlight(0, 'logoRemdup',              { fg = colors.orange,     bg = 'NONE' })  -- remdup
  highlight(0, 'logoQuoted',              { fg = colors.orange,     bg = 'NONE' })  -- quoted


  -----------------------------------------------------------------------------
  -- Data Structure Primitives - Mutators

  highlight(0, 'logoMutator',             { fg = colors.orange,     bg = 'NONE' })  -- Mutators
  highlight(0, 'logoSetitem',             { fg = colors.orange,     bg = 'NONE' })  -- setitem
  highlight(0, 'logoMdsetitem',           { fg = colors.orange,     bg = 'NONE' })  -- mdsetitem
  highlight(0, 'logoPush',                { fg = colors.orange,     bg = 'NONE' })  -- push
  highlight(0, 'logoPop',                 { fg = colors.orange,     bg = 'NONE' })  -- pop
  highlight(0, 'logoQueue',               { fg = colors.orange,     bg = 'NONE' })  -- queue
  highlight(0, 'logoDequeue',             { fg = colors.orange,     bg = 'NONE' })  -- dequeue


  -----------------------------------------------------------------------------
  -- Data Structure Primitives - Predicates

  highlight(0, 'logoPredicate',           { fg = colors.orange,     bg = 'NONE' })  -- Predicates
  highlight(0, 'logoWordp',               { fg = colors.orange,     bg = 'NONE' })  -- wordp
  highlight(0, 'logoListp',               { fg = colors.orange,     bg = 'NONE' })  -- listp
  highlight(0, 'logoArrayp',              { fg = colors.orange,     bg = 'NONE' })  -- arrayp
  highlight(0, 'logoEmptyp',              { fg = colors.orange,     bg = 'NONE' })  -- emptyp
  highlight(0, 'logoEqualp',              { fg = colors.orange,     bg = 'NONE' })  -- equalp
  highlight(0, 'logoNotequalp',           { fg = colors.orange,     bg = 'NONE' })  -- notequalp
  highlight(0, 'logoBeforep',             { fg = colors.orange,     bg = 'NONE' })  -- beforep
  highlight(0, 'logoMemberp',             { fg = colors.orange,     bg = 'NONE' })  -- memberp
  highlight(0, 'logoSubstringp',          { link = "String" })  -- substringp
  highlight(0, 'logoNumberp',             { link = "Number" })  -- numberp


  -----------------------------------------------------------------------------
  -- Data Structure Primitives - Queries

  highlight(0, 'logoDataQuery',           { fg = colors.orange,     bg = 'NONE' })  -- Data queries
  highlight(0, 'logoCount',               { fg = colors.orange,     bg = 'NONE' })  -- count
  highlight(0, 'logoAscii',               { fg = colors.orange,     bg = 'NONE' })  -- ascii
  highlight(0, 'logoRawascii',            { fg = colors.orange,     bg = 'NONE' })  -- rawascii
  highlight(0, 'logoChar',                { fg = colors.orange,     bg = 'NONE' })  -- char
  highlight(0, 'logoMember',              { fg = colors.orange,     bg = 'NONE' })  -- member
  highlight(0, 'logoLowercase',           { fg = colors.orange,     bg = 'NONE' })  -- lowercase
  highlight(0, 'logoUppercase',           { fg = colors.orange,     bg = 'NONE' })  -- uppercase
  highlight(0, 'logoParse',               { fg = colors.orange,     bg = 'NONE' })  -- parse
  highlight(0, 'logoRunparse',            { fg = colors.orange,     bg = 'NONE' })  -- runparse


  -----------------------------------------------------------------------------
  -- Arithmetic Operations

  highlight(0, 'logoArithmetic',          { fg = colors.orange,     bg = 'NONE' })  -- Arithmetic
  highlight(0, 'logoSum',                 { fg = colors.orange,     bg = 'NONE' })  -- sum (+)
  highlight(0, 'logoDifference',          { fg = colors.orange,     bg = 'NONE' })  -- difference (-)
  highlight(0, 'logoMinus',               { fg = colors.orange,     bg = 'NONE' })  -- minus
  highlight(0, 'logoProduct',             { fg = colors.orange,     bg = 'NONE' })  -- product (*)
  highlight(0, 'logoQuotient',            { fg = colors.orange,     bg = 'NONE' })  -- quotient (/)
  highlight(0, 'logoRemainder',           { fg = colors.orange,     bg = 'NONE' })  -- remainder
  highlight(0, 'logoModulo',              { fg = colors.orange,     bg = 'NONE' })  -- modulo
  highlight(0, 'logoInt',                 { fg = colors.orange,     bg = 'NONE' })  -- int
  highlight(0, 'logoRound',               { fg = colors.orange,     bg = 'NONE' })  -- round
  highlight(0, 'logoSqrt',                { fg = colors.orange,     bg = 'NONE' })  -- sqrt
  highlight(0, 'logoPower',               { fg = colors.orange,     bg = 'NONE' })  -- power
  highlight(0, 'logoExp',                 { fg = colors.orange,     bg = 'NONE' })  -- exp
  highlight(0, 'logoLog10',               { fg = colors.orange,     bg = 'NONE' })  -- log10
  highlight(0, 'logoLn',                  { fg = colors.orange,     bg = 'NONE' })  -- ln
  highlight(0, 'logoAbs',                 { fg = colors.orange,     bg = 'NONE' })  -- abs


  -----------------------------------------------------------------------------
  -- Trigonometric Functions

  highlight(0, 'logoTrigonometry',        { fg = colors.orange,     bg = 'NONE' })  -- Trigonometry
  highlight(0, 'logoSin',                 { fg = colors.orange,     bg = 'NONE' })  -- sin
  highlight(0, 'logoCos',                 { fg = colors.orange,     bg = 'NONE' })  -- cos
  highlight(0, 'logoTan',                 { fg = colors.orange,     bg = 'NONE' })  -- tan
  highlight(0, 'logoRadsin',              { fg = colors.orange,     bg = 'NONE' })  -- radsin
  highlight(0, 'logoRadcos',              { fg = colors.orange,     bg = 'NONE' })  -- radcos
  highlight(0, 'logoArctan',              { fg = colors.orange,     bg = 'NONE' })  -- arctan
  highlight(0, 'logoRadarctan',           { fg = colors.orange,     bg = 'NONE' })  -- radarctan
  highlight(0, 'logoArcsin',              { fg = colors.orange,     bg = 'NONE' })  -- arcsin (asin)
  highlight(0, 'logoArccos',              { fg = colors.orange,     bg = 'NONE' })  -- arccos (acos)


  -----------------------------------------------------------------------------
  -- Comparison Operations

  highlight(0, 'logoComparison',          { fg = colors.orange,     bg = 'NONE' })  -- Comparison
  highlight(0, 'logoLessp',               { fg = colors.orange,     bg = 'NONE' })  -- lessp (<)
  highlight(0, 'logoGreaterp',            { fg = colors.orange,     bg = 'NONE' })  -- greaterp (>)
  highlight(0, 'logoLessequalp',          { fg = colors.orange,     bg = 'NONE' })  -- lessequalp (<=)
  highlight(0, 'logoGreaterequalp',       { fg = colors.orange,     bg = 'NONE' })  -- greaterequalp (>=)


  -----------------------------------------------------------------------------
  -- Random Numbers

  highlight(0, 'logoRandom',              { fg = colors.orange,     bg = 'NONE' })  -- random
  highlight(0, 'logoRerandom',            { fg = colors.orange,     bg = 'NONE' })  -- rerandom


  -----------------------------------------------------------------------------
  -- Logical Operations

  highlight(0, 'logoLogical',             { fg = colors.blue,       bg = 'NONE' })  -- Logical
  highlight(0, 'logoAnd',                 { fg = colors.blue,       bg = 'NONE' })  -- and
  highlight(0, 'logoOr',                  { fg = colors.blue,       bg = 'NONE' })  -- or
  highlight(0, 'logoNot',                 { fg = colors.blue,       bg = 'NONE' })  -- not


  -----------------------------------------------------------------------------
  -- Bitwise Operations

  highlight(0, 'logoBitwise',             { fg = colors.orange,     bg = 'NONE' })  -- Bitwise
  highlight(0, 'logoBitand',              { fg = colors.orange,     bg = 'NONE' })  -- bitand
  highlight(0, 'logoBitor',               { fg = colors.orange,     bg = 'NONE' })  -- bitor
  highlight(0, 'logoBitxor',              { fg = colors.orange,     bg = 'NONE' })  -- bitxor
  highlight(0, 'logoBitnot',              { fg = colors.orange,     bg = 'NONE' })  -- bitnot
  highlight(0, 'logoAshift',              { fg = colors.orange,     bg = 'NONE' })  -- ashift
  highlight(0, 'logoLshift',              { fg = colors.orange,     bg = 'NONE' })  -- lshift


  -----------------------------------------------------------------------------
  -- Variables

  highlight(0, 'logoVariable',            { link = "Variable" })  -- Variables
  highlight(0, 'logoVariableRef',         { link = "Variable" })  -- :variable reference
  highlight(0, 'logoMake',                { fg = colors.blue,       bg = 'NONE' })  -- make
  highlight(0, 'logoName',                { fg = colors.blue,       bg = 'NONE' })  -- name
  highlight(0, 'logoLocal',               { fg = colors.blue,       bg = 'NONE' })  -- local
  highlight(0, 'logoLocalmake',           { fg = colors.blue,       bg = 'NONE' })  -- localmake
  highlight(0, 'logoThing',               { fg = colors.orange,     bg = 'NONE' })  -- thing
  highlight(0, 'logoGlobal',              { fg = colors.blue,       bg = 'NONE' })  -- global


  -----------------------------------------------------------------------------
  -- Property Lists

  highlight(0, 'logoPropertyList',        { fg = colors.orange,     bg = 'NONE' })  -- Property lists
  highlight(0, 'logoPprop',               { fg = colors.orange,     bg = 'NONE' })  -- pprop
  highlight(0, 'logoGprop',               { fg = colors.orange,     bg = 'NONE' })  -- gprop
  highlight(0, 'logoRemprop',             { fg = colors.orange,     bg = 'NONE' })  -- remprop
  highlight(0, 'logoPlist',               { fg = colors.orange,     bg = 'NONE' })  -- plist


  -----------------------------------------------------------------------------
  -- Input/Output

  highlight(0, 'logoIO',                  { fg = colors.orange,     bg = 'NONE' })  -- I/O
  highlight(0, 'logoPrint',               { fg = colors.orange,     bg = 'NONE' })  -- print (pr)
  highlight(0, 'logoType',                { link = "Type" })  -- type
  highlight(0, 'logoShow',                { fg = colors.orange,     bg = 'NONE' })  -- show
  highlight(0, 'logoReadlist',            { fg = colors.orange,     bg = 'NONE' })  -- readlist (rl)
  highlight(0, 'logoReadword',            { fg = colors.orange,     bg = 'NONE' })  -- readword (rw)
  highlight(0, 'logoReadchar',            { fg = colors.orange,     bg = 'NONE' })  -- readchar (rc)
  highlight(0, 'logoReadchars',           { fg = colors.orange,     bg = 'NONE' })  -- readchars (rcs)
  highlight(0, 'logoShell',               { fg = colors.orange,     bg = 'NONE' })  -- shell


  -----------------------------------------------------------------------------
  -- File Operations

  highlight(0, 'logoFile',                { fg = colors.orange,     bg = 'NONE' })  -- File operations
  highlight(0, 'logoOpenread',            { fg = colors.orange,     bg = 'NONE' })  -- openread
  highlight(0, 'logoOpenwrite',           { fg = colors.orange,     bg = 'NONE' })  -- openwrite
  highlight(0, 'logoOpenappend',          { fg = colors.orange,     bg = 'NONE' })  -- openappend
  highlight(0, 'logoOpenupdate',          { fg = colors.orange,     bg = 'NONE' })  -- openupdate
  highlight(0, 'logoClose',               { fg = colors.orange,     bg = 'NONE' })  -- close
  highlight(0, 'logoAllopen',             { fg = colors.orange,     bg = 'NONE' })  -- allopen
  highlight(0, 'logoCloseall',            { fg = colors.orange,     bg = 'NONE' })  -- closeall
  highlight(0, 'logoErasefile',           { fg = colors.orange,     bg = 'NONE' })  -- erasefile (erf)
  highlight(0, 'logoDribble',             { fg = colors.orange,     bg = 'NONE' })  -- dribble
  highlight(0, 'logoNodribble',           { fg = colors.orange,     bg = 'NONE' })  -- nodribble
  highlight(0, 'logoSetread',             { fg = colors.orange,     bg = 'NONE' })  -- setread
  highlight(0, 'logoSetwrite',            { fg = colors.orange,     bg = 'NONE' })  -- setwrite
  highlight(0, 'logoReader',              { fg = colors.orange,     bg = 'NONE' })  -- reader
  highlight(0, 'logoWriter',              { fg = colors.orange,     bg = 'NONE' })  -- writer
  highlight(0, 'logoEofp',                { fg = colors.orange,     bg = 'NONE' })  -- eofp
  highlight(0, 'logoFilep',               { fg = colors.orange,     bg = 'NONE' })  -- filep


  -----------------------------------------------------------------------------
  -- Terminal Control

  highlight(0, 'logoTerminal',            { fg = colors.orange,     bg = 'NONE' })  -- Terminal
  highlight(0, 'logoKeyp',                { fg = colors.orange,     bg = 'NONE' })  -- keyp
  highlight(0, 'logoCleartext',           { fg = colors.orange,     bg = 'NONE' })  -- cleartext (ct)
  highlight(0, 'logoSetcursor',           { fg = colors.orange,     bg = 'NONE' })  -- setcursor
  highlight(0, 'logoCursor',              { fg = colors.orange,     bg = 'NONE' })  -- cursor
  highlight(0, 'logoSetmargins',          { fg = colors.orange,     bg = 'NONE' })  -- setmargins
  highlight(0, 'logoSettextcolor',        { fg = colors.orange,     bg = 'NONE' })  -- settextcolor (settc)
  highlight(0, 'logoSetfont',             { fg = colors.orange,     bg = 'NONE' })  -- setfont
  highlight(0, 'logoFont',                { fg = colors.orange,     bg = 'NONE' })  -- font


  -----------------------------------------------------------------------------
  -- Workspace Management

  highlight(0, 'logoWorkspace',           { fg = colors.orange,     bg = 'NONE' })  -- Workspace
  highlight(0, 'logoText',                { fg = colors.orange,     bg = 'NONE' })  -- text
  highlight(0, 'logoFulltext',            { fg = colors.orange,     bg = 'NONE' })  -- fulltext
  highlight(0, 'logoCopydef',             { fg = colors.orange,     bg = 'NONE' })  -- copydef
  highlight(0, 'logoContents',            { fg = colors.orange,     bg = 'NONE' })  -- contents
  highlight(0, 'logoBuried',              { fg = colors.orange,     bg = 'NONE' })  -- buried
  highlight(0, 'logoTraced',              { fg = colors.orange,     bg = 'NONE' })  -- traced
  highlight(0, 'logoProcedures',          { fg = colors.orange,     bg = 'NONE' })  -- procedures
  highlight(0, 'logoPrimitives',          { fg = colors.orange,     bg = 'NONE' })  -- primitives
  highlight(0, 'logoNames',               { fg = colors.orange,     bg = 'NONE' })  -- names
  highlight(0, 'logoPlists',              { fg = colors.orange,     bg = 'NONE' })  -- plists
  highlight(0, 'logoArity',               { fg = colors.orange,     bg = 'NONE' })  -- arity
  highlight(0, 'logoNodes',               { fg = colors.orange,     bg = 'NONE' })  -- nodes


  -----------------------------------------------------------------------------
  -- Workspace Inspection

  highlight(0, 'logoInspection',          { fg = colors.orange,     bg = 'NONE' })  -- Inspection
  highlight(0, 'logoPo',                  { fg = colors.orange,     bg = 'NONE' })  -- po
  highlight(0, 'logoPoall',               { fg = colors.orange,     bg = 'NONE' })  -- poall
  highlight(0, 'logoPops',                { fg = colors.orange,     bg = 'NONE' })  -- pops
  highlight(0, 'logoPons',                { fg = colors.orange,     bg = 'NONE' })  -- pons
  highlight(0, 'logoPopls',               { fg = colors.orange,     bg = 'NONE' })  -- popls
  highlight(0, 'logoPot',                 { fg = colors.orange,     bg = 'NONE' })  -- pot
  highlight(0, 'logoPots',                { fg = colors.orange,     bg = 'NONE' })  -- pots


  -----------------------------------------------------------------------------
  -- Workspace Control

  highlight(0, 'logoWorkspaceCtrl',       { fg = colors.orange,     bg = 'NONE' })  -- Workspace control
  highlight(0, 'logoErase',               { fg = colors.orange,     bg = 'NONE' })  -- erase (er)
  highlight(0, 'logoErall',               { fg = colors.orange,     bg = 'NONE' })  -- erall
  highlight(0, 'logoErps',                { fg = colors.orange,     bg = 'NONE' })  -- erps
  highlight(0, 'logoErns',                { fg = colors.orange,     bg = 'NONE' })  -- erns
  highlight(0, 'logoErpls',               { fg = colors.orange,     bg = 'NONE' })  -- erpls
  highlight(0, 'logoBury',                { fg = colors.orange,     bg = 'NONE' })  -- bury
  highlight(0, 'logoBuryall',             { fg = colors.orange,     bg = 'NONE' })  -- buryall
  highlight(0, 'logoUnbury',              { fg = colors.orange,     bg = 'NONE' })  -- unbury
  highlight(0, 'logoUnburyall',           { fg = colors.orange,     bg = 'NONE' })  -- unburyall
  highlight(0, 'logoBuriedp',             { fg = colors.orange,     bg = 'NONE' })  -- buriedp
  highlight(0, 'logoTrace',               { fg = colors.orange,     bg = 'NONE' })  -- trace
  highlight(0, 'logoUntrace',             { fg = colors.orange,     bg = 'NONE' })  -- untrace
  highlight(0, 'logoTracedp',             { fg = colors.orange,     bg = 'NONE' })  -- tracedp
  highlight(0, 'logoStep',                { fg = colors.orange,     bg = 'NONE' })  -- step
  highlight(0, 'logoUnstep',              { fg = colors.orange,     bg = 'NONE' })  -- unstep
  highlight(0, 'logoSteppedp',            { fg = colors.orange,     bg = 'NONE' })  -- steppedp
  highlight(0, 'logoEdit',                { fg = colors.orange,     bg = 'NONE' })  -- edit (ed)
  highlight(0, 'logoEditfile',            { fg = colors.orange,     bg = 'NONE' })  -- editfile
  highlight(0, 'logoEdall',               { fg = colors.orange,     bg = 'NONE' })  -- edall
  highlight(0, 'logoSave',                { fg = colors.orange,     bg = 'NONE' })  -- save
  highlight(0, 'logoLoad',                { fg = colors.orange,     bg = 'NONE' })  -- load
  highlight(0, 'logoHelp',                { fg = colors.orange,     bg = 'NONE' })  -- help
  highlight(0, 'logoGc',                  { fg = colors.orange,     bg = 'NONE' })  -- gc


  -----------------------------------------------------------------------------
  -- Template-Based Iteration

  highlight(0, 'logoTemplate',            { fg = colors.orange,     bg = 'NONE' })  -- Template iteration
  highlight(0, 'logoApply',               { fg = colors.orange,     bg = 'NONE' })  -- apply
  highlight(0, 'logoInvoke',              { fg = colors.orange,     bg = 'NONE' })  -- invoke
  highlight(0, 'logoForeach',             { fg = colors.orange,     bg = 'NONE' })  -- foreach
  highlight(0, 'logoMap',                 { fg = colors.orange,     bg = 'NONE' })  -- map
  highlight(0, 'logoMapSe',               { fg = colors.orange,     bg = 'NONE' })  -- map.se
  highlight(0, 'logoFilter',              { fg = colors.orange,     bg = 'NONE' })  -- filter
  highlight(0, 'logoFind',                { fg = colors.orange,     bg = 'NONE' })  -- find
  highlight(0, 'logoReduce',              { fg = colors.orange,     bg = 'NONE' })  -- reduce
  highlight(0, 'logoCrossmap',            { fg = colors.orange,     bg = 'NONE' })  -- crossmap
  highlight(0, 'logoCascade',             { fg = colors.orange,     bg = 'NONE' })  -- cascade
  highlight(0, 'logoTransfer',            { fg = colors.orange,     bg = 'NONE' })  -- transfer


  -----------------------------------------------------------------------------
  -- Mouse Queries

  highlight(0, 'logoMouse',               { fg = colors.orange,     bg = 'NONE' })  -- Mouse
  highlight(0, 'logoMousepos',            { fg = colors.orange,     bg = 'NONE' })  -- mousepos
  highlight(0, 'logoClickpos',            { fg = colors.orange,     bg = 'NONE' })  -- clickpos
  highlight(0, 'logoButtonp',             { fg = colors.orange,     bg = 'NONE' })  -- buttonp
  highlight(0, 'logoButton',              { fg = colors.orange,     bg = 'NONE' })  -- button


  -----------------------------------------------------------------------------
  -- Picture Management

  highlight(0, 'logoPicture',             { fg = colors.orange,     bg = 'NONE' })  -- Picture
  highlight(0, 'logoSavepict',            { fg = colors.orange,     bg = 'NONE' })  -- savepict
  highlight(0, 'logoLoadpict',            { fg = colors.orange,     bg = 'NONE' })  -- loadpict
  highlight(0, 'logoEpspict',             { fg = colors.orange,     bg = 'NONE' })  -- epspict


  -----------------------------------------------------------------------------
  -- Objects (UCBLogo OOP)

  highlight(0, 'logoObject',              { fg = colors.turquoise,  bg = 'NONE' })  -- Objects
  highlight(0, 'logoKindof',              { fg = colors.turquoise,  bg = 'NONE' })  -- kindof
  highlight(0, 'logoSomething',           { fg = colors.turquoise,  bg = 'NONE' })  -- something
  highlight(0, 'logoOneof',               { fg = colors.turquoise,  bg = 'NONE' })  -- oneof
  highlight(0, 'logoExist',               { fg = colors.turquoise,  bg = 'NONE' })  -- exist
  highlight(0, 'logoHave',                { fg = colors.turquoise,  bg = 'NONE' })  -- have
  highlight(0, 'logoHavemake',            { fg = colors.turquoise,  bg = 'NONE' })  -- havemake
  highlight(0, 'logoSelf',                { fg = colors.purple,     bg = 'NONE' })  -- self
  highlight(0, 'logoParents',             { fg = colors.turquoise,  bg = 'NONE' })  -- parents
  highlight(0, 'logoTalkto',              { fg = colors.turquoise,  bg = 'NONE' })  -- talkto
  highlight(0, 'logoAsk',                 { fg = colors.turquoise,  bg = 'NONE' })  -- ask
  highlight(0, 'logoUsual',               { fg = colors.turquoise,  bg = 'NONE' })  -- usual


  -----------------------------------------------------------------------------
  -- Macros

  highlight(0, 'logoMacro',               { fg = colors.pink,       bg = 'NONE' })  -- Macros
  highlight(0, 'logoDefmacro',            { fg = colors.pink,       bg = 'NONE' })  -- .defmacro
  highlight(0, 'logoMacrop',              { fg = colors.pink,       bg = 'NONE' })  -- macrop
  highlight(0, 'logoMacroexpand',         { fg = colors.pink,       bg = 'NONE' })  -- macroexpand


  -----------------------------------------------------------------------------
  -- Literals

  -- Numbers
  highlight(0, 'logoNumber',              { link = "Number" })  -- Numbers
  highlight(0, 'logoFloat',               { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Strings/Words
  highlight(0, 'logoString',              { link = "String" })  -- "string
  highlight(0, 'logoQuotedWord',          { fg = colors.redLight,   bg = 'NONE' })  -- "word

  -- Lists
  highlight(0, 'logoListLiteral',         { fg = colors.white,      bg = 'NONE' })  -- [list]

  -- Boolean
  highlight(0, 'logoBoolean',             { link = "Boolean" })  -- true, false


  -----------------------------------------------------------------------------
  -- Operators

  highlight(0, 'logoOperator',            { link = "Operator" })  -- Operators
  highlight(0, 'logoArithOp',             { fg = colors.white,      bg = 'NONE' })  -- + - * /
  highlight(0, 'logoCompareOp',           { fg = colors.white,      bg = 'NONE' })  -- < > = <= >=
  highlight(0, 'logoAssignOp',            { fg = colors.white,      bg = 'NONE' })  -- Assignment


  -----------------------------------------------------------------------------
  -- Delimiters

  highlight(0, 'logoDelimiter',           { link = "Delimiter" })  -- Delimiters
  highlight(0, 'logoBracket',             { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'logoParenthesis',         { fg = colors.white,      bg = 'NONE' })  -- ( )


  -----------------------------------------------------------------------------
  -- Special Characters

  highlight(0, 'logoSpecial',             { fg = colors.pink,       bg = 'NONE' })  -- Special
  highlight(0, 'logoColon',               { fg = colors.pink,       bg = 'NONE' })  -- : (variable prefix)
  highlight(0, 'logoQuote',               { fg = colors.pink,       bg = 'NONE' })  -- " (word quote)
  highlight(0, 'logoBackquote',           { fg = colors.pink,       bg = 'NONE' })  -- ` (backquote)


  -----------------------------------------------------------------------------
  -- Comments

  highlight(0, 'logoComment',             { link = "Comment" })  -- ; comments
  highlight(0, 'logoTodo',                { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO


  -----------------------------------------------------------------------------
  -- Special Variables

  highlight(0, 'logoSpecialVar',          { link = "Variable" })  -- Special variables
  highlight(0, 'logoRepcount',            { fg = colors.purple,     bg = 'NONE' })  -- repcount


  -----------------------------------------------------------------------------
  -- Treesitter Groups

  highlight(0, '@keyword.logo',               { link = "Keyword" })  -- Keywords
  highlight(0, '@keyword.function.logo',      { link = "Keyword" })  -- to, end
  highlight(0, '@keyword.return.logo',        { link = "Keyword" })  -- output, stop
  highlight(0, '@keyword.repeat.logo',        { link = "Keyword" })  -- repeat, forever
  highlight(0, '@keyword.conditional.logo',   { link = "Conditional" })  -- if, ifelse

  highlight(0, '@function.logo',              { link = "Function" })  -- Functions
  highlight(0, '@function.builtin.logo',      { link = "Function" })  -- Built-in primitives
  highlight(0, '@function.call.logo',         { link = "Function" })  -- Function calls

  highlight(0, '@function.turtle.logo',       { link = "Function" })  -- Turtle commands

  highlight(0, '@variable.logo',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.logo',      { link = "Variable" })  -- Built-in variables

  highlight(0, '@constant.logo',              { link = "Constant" })  -- Constants

  highlight(0, '@number.logo',                { link = "Number" })  -- Numbers
  highlight(0, '@number.float.logo',          { link = "Number" })  -- Floats

  highlight(0, '@string.logo',                { link = "String" })  -- Strings

  highlight(0, '@operator.logo',              { link = "Operator" })  -- Operators

  highlight(0, '@punctuation.bracket.logo',   { fg = colors.white,      bg = 'NONE' })  -- [ ] ( )
  highlight(0, '@punctuation.delimiter.logo', { link = "Delimiter" })  -- Delimiters
  highlight(0, '@punctuation.special.logo',   { fg = colors.pink,       bg = 'NONE' })  -- : "

  highlight(0, '@comment.logo',               { link = "Comment" })  -- Comments


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens

  highlight(0, '@lsp.type.variable.logo',     { link = "Variable" })
  highlight(0, '@lsp.type.function.logo',     { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.logo',      { link = "Keyword" })
  highlight(0, '@lsp.type.operator.logo',     { link = "Operator" })
  highlight(0, '@lsp.type.string.logo',       { link = "String" })
  highlight(0, '@lsp.type.number.logo',       { link = "Number" })
  highlight(0, '@lsp.type.comment.logo',      { link = "Comment" })

end

return logo
