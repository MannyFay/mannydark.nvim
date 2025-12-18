-------------------------------------------------------------------------------
-- Verilog / SystemVerilog
-- Highlighting for .v, .vh, .sv, .svh files
-- Covers Verilog (IEEE 1364) and SystemVerilog (IEEE 1800)
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local verilog   = {}


-------------------------------------------------------------------------------
-- Settings

verilog.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Verilog Keywords (IEEE 1364)

  -- Module/Primitive Definition
  highlight(0, 'verilogStatement',      { fg = colors.blue,       bg = 'NONE' })  -- General statements
  highlight(0, 'verilogKeyword',        { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, 'verilogModule',         { fg = colors.blue,       bg = 'NONE' })  -- module keyword
  highlight(0, 'verilogEndmodule',      { fg = colors.blue,       bg = 'NONE' })  -- endmodule keyword
  highlight(0, 'verilogMacromodule',    { fg = colors.blue,       bg = 'NONE' })  -- macromodule keyword
  highlight(0, 'verilogPrimitive',      { fg = colors.blue,       bg = 'NONE' })  -- primitive keyword
  highlight(0, 'verilogEndprimitive',   { fg = colors.blue,       bg = 'NONE' })  -- endprimitive keyword
  highlight(0, 'verilogTable',          { fg = colors.blue,       bg = 'NONE' })  -- table keyword
  highlight(0, 'verilogEndtable',       { fg = colors.blue,       bg = 'NONE' })  -- endtable keyword

  -- Port Directions
  highlight(0, 'verilogInput',          { fg = colors.blue,       bg = 'NONE' })  -- input keyword
  highlight(0, 'verilogOutput',         { fg = colors.blue,       bg = 'NONE' })  -- output keyword
  highlight(0, 'verilogInout',          { fg = colors.blue,       bg = 'NONE' })  -- inout keyword

  -- Net Types
  highlight(0, 'verilogWire',           { fg = colors.blue,       bg = 'NONE' })  -- wire keyword
  highlight(0, 'verilogReg',            { fg = colors.blue,       bg = 'NONE' })  -- reg keyword
  highlight(0, 'verilogTri',            { fg = colors.blue,       bg = 'NONE' })  -- tri, tri0, tri1, triand, trior, trireg
  highlight(0, 'verilogWand',           { fg = colors.blue,       bg = 'NONE' })  -- wand keyword
  highlight(0, 'verilogWor',            { fg = colors.blue,       bg = 'NONE' })  -- wor keyword
  highlight(0, 'verilogSupply',         { fg = colors.blue,       bg = 'NONE' })  -- supply0, supply1

  -- Data Types
  highlight(0, 'verilogInteger',        { fg = colors.blue,       bg = 'NONE' })  -- integer keyword
  highlight(0, 'verilogReal',           { fg = colors.blue,       bg = 'NONE' })  -- real keyword
  highlight(0, 'verilogRealtime',       { fg = colors.blue,       bg = 'NONE' })  -- realtime keyword
  highlight(0, 'verilogTime',           { fg = colors.blue,       bg = 'NONE' })  -- time keyword
  highlight(0, 'verilogEvent',          { fg = colors.blue,       bg = 'NONE' })  -- event keyword

  -- Parameters
  highlight(0, 'verilogParameter',      { fg = colors.blue,       bg = 'NONE' })  -- parameter keyword
  highlight(0, 'verilogLocalparam',     { fg = colors.blue,       bg = 'NONE' })  -- localparam keyword
  highlight(0, 'verilogSpecparam',      { fg = colors.blue,       bg = 'NONE' })  -- specparam keyword
  highlight(0, 'verilogDefparam',       { fg = colors.blue,       bg = 'NONE' })  -- defparam keyword

  -- Procedural Blocks
  highlight(0, 'verilogAlways',         { fg = colors.blue,       bg = 'NONE' })  -- always keyword
  highlight(0, 'verilogInitial',        { fg = colors.blue,       bg = 'NONE' })  -- initial keyword
  highlight(0, 'verilogAssign',         { fg = colors.blue,       bg = 'NONE' })  -- assign keyword
  highlight(0, 'verilogDeassign',       { fg = colors.blue,       bg = 'NONE' })  -- deassign keyword
  highlight(0, 'verilogForce',          { fg = colors.blue,       bg = 'NONE' })  -- force keyword
  highlight(0, 'verilogRelease',        { fg = colors.blue,       bg = 'NONE' })  -- release keyword

  -- Control Flow
  highlight(0, 'verilogConditional',    { fg = colors.blue,       bg = 'NONE' })  -- if, else
  highlight(0, 'verilogCase',           { fg = colors.blue,       bg = 'NONE' })  -- case, casex, casez, endcase
  highlight(0, 'verilogDefault',        { fg = colors.blue,       bg = 'NONE' })  -- default keyword
  highlight(0, 'verilogRepeat',         { fg = colors.blue,       bg = 'NONE' })  -- for, while, repeat, forever

  -- Block Delimiters
  highlight(0, 'verilogBegin',          { fg = colors.blue,       bg = 'NONE' })  -- begin keyword
  highlight(0, 'verilogEnd',            { fg = colors.blue,       bg = 'NONE' })  -- end keyword
  highlight(0, 'verilogFork',           { fg = colors.blue,       bg = 'NONE' })  -- fork keyword
  highlight(0, 'verilogJoin',           { fg = colors.blue,       bg = 'NONE' })  -- join keyword

  -- Functions/Tasks
  highlight(0, 'verilogFunction',       { fg = colors.blue,       bg = 'NONE' })  -- function keyword
  highlight(0, 'verilogEndfunction',    { fg = colors.blue,       bg = 'NONE' })  -- endfunction keyword
  highlight(0, 'verilogTask',           { fg = colors.blue,       bg = 'NONE' })  -- task keyword
  highlight(0, 'verilogEndtask',        { fg = colors.blue,       bg = 'NONE' })  -- endtask keyword
  highlight(0, 'verilogAutomatic',      { fg = colors.blue,       bg = 'NONE' })  -- automatic keyword

  -- Generate
  highlight(0, 'verilogGenerate',       { fg = colors.blue,       bg = 'NONE' })  -- generate keyword
  highlight(0, 'verilogEndgenerate',    { fg = colors.blue,       bg = 'NONE' })  -- endgenerate keyword
  highlight(0, 'verilogGenvar',         { fg = colors.blue,       bg = 'NONE' })  -- genvar keyword

  -- Timing
  highlight(0, 'verilogWait',           { fg = colors.blue,       bg = 'NONE' })  -- wait keyword
  highlight(0, 'verilogDisable',        { fg = colors.blue,       bg = 'NONE' })  -- disable keyword
  highlight(0, 'verilogSpecify',        { fg = colors.blue,       bg = 'NONE' })  -- specify keyword
  highlight(0, 'verilogEndspecify',     { fg = colors.blue,       bg = 'NONE' })  -- endspecify keyword

  -- Gate Primitives
  highlight(0, 'verilogGate',           { fg = colors.pink,       bg = 'NONE' })  -- and, or, nand, nor, xor, xnor, not, buf
  highlight(0, 'verilogBufif',          { fg = colors.pink,       bg = 'NONE' })  -- bufif0, bufif1, notif0, notif1
  highlight(0, 'verilogMos',            { fg = colors.pink,       bg = 'NONE' })  -- nmos, pmos, cmos, rnmos, rpmos, rcmos
  highlight(0, 'verilogTran',           { fg = colors.pink,       bg = 'NONE' })  -- tran, tranif0, tranif1, rtran, rtranif0, rtranif1
  highlight(0, 'verilogPullup',         { fg = colors.pink,       bg = 'NONE' })  -- pullup, pulldown

  -- Signal Attributes
  highlight(0, 'verilogSigned',         { fg = colors.blue,       bg = 'NONE' })  -- signed keyword
  highlight(0, 'verilogUnsigned',       { fg = colors.blue,       bg = 'NONE' })  -- unsigned keyword
  highlight(0, 'verilogScalared',       { fg = colors.blue,       bg = 'NONE' })  -- scalared keyword
  highlight(0, 'verilogVectored',       { fg = colors.blue,       bg = 'NONE' })  -- vectored keyword

  -- Drive Strengths
  highlight(0, 'verilogStrength',       { fg = colors.purple,     bg = 'NONE' })  -- Strength values
  highlight(0, 'verilogStrong',         { fg = colors.purple,     bg = 'NONE' })  -- strong0, strong1
  highlight(0, 'verilogWeak',           { fg = colors.purple,     bg = 'NONE' })  -- weak0, weak1
  highlight(0, 'verilogHighz',          { fg = colors.purple,     bg = 'NONE' })  -- highz0, highz1
  highlight(0, 'verilogPull',           { fg = colors.purple,     bg = 'NONE' })  -- pull0, pull1

  -- Configuration
  highlight(0, 'verilogConfig',         { fg = colors.blue,       bg = 'NONE' })  -- config keyword
  highlight(0, 'verilogEndconfig',      { fg = colors.blue,       bg = 'NONE' })  -- endconfig keyword
  highlight(0, 'verilogDesign',         { fg = colors.blue,       bg = 'NONE' })  -- design keyword
  highlight(0, 'verilogInstance',       { fg = colors.blue,       bg = 'NONE' })  -- instance keyword
  highlight(0, 'verilogCell',           { fg = colors.blue,       bg = 'NONE' })  -- cell keyword
  highlight(0, 'verilogUse',            { fg = colors.blue,       bg = 'NONE' })  -- use keyword
  highlight(0, 'verilogLiblist',        { fg = colors.blue,       bg = 'NONE' })  -- liblist keyword
  highlight(0, 'verilogLibrary',        { fg = colors.blue,       bg = 'NONE' })  -- library keyword

  -- Edge Specifiers
  highlight(0, 'verilogEdge',           { fg = colors.blue,       bg = 'NONE' })  -- posedge, negedge, edge


  -----------------------------------------------------------------------------
  -- SystemVerilog Keywords (IEEE 1800)

  -- Data Types (Extended)
  highlight(0, 'systemverilogStatement', { fg = colors.blue,      bg = 'NONE' })  -- General statements
  highlight(0, 'systemverilogTypeDef',   { fg = colors.blue,      bg = 'NONE' })  -- Type definitions
  highlight(0, 'systemverilogLogic',     { fg = colors.blue,      bg = 'NONE' })  -- logic keyword
  highlight(0, 'systemverilogBit',       { fg = colors.blue,      bg = 'NONE' })  -- bit keyword
  highlight(0, 'systemverilogByte',      { fg = colors.blue,      bg = 'NONE' })  -- byte keyword
  highlight(0, 'systemverilogShortint',  { fg = colors.blue,      bg = 'NONE' })  -- shortint keyword
  highlight(0, 'systemverilogInt',       { fg = colors.blue,      bg = 'NONE' })  -- int keyword
  highlight(0, 'systemverilogLongint',   { fg = colors.blue,      bg = 'NONE' })  -- longint keyword
  highlight(0, 'systemverilogShortreal', { fg = colors.blue,      bg = 'NONE' })  -- shortreal keyword
  highlight(0, 'systemverilogString',    { fg = colors.blue,      bg = 'NONE' })  -- string type keyword
  highlight(0, 'systemverilogVoid',      { fg = colors.blue,      bg = 'NONE' })  -- void keyword
  highlight(0, 'systemverilogChandle',   { fg = colors.blue,      bg = 'NONE' })  -- chandle keyword

  -- Aggregate Types
  highlight(0, 'systemverilogStruct',    { fg = colors.blue,      bg = 'NONE' })  -- struct keyword
  highlight(0, 'systemverilogUnion',     { fg = colors.blue,      bg = 'NONE' })  -- union keyword
  highlight(0, 'systemverilogEnum',      { fg = colors.blue,      bg = 'NONE' })  -- enum keyword
  highlight(0, 'systemverilogTypedef',   { fg = colors.blue,      bg = 'NONE' })  -- typedef keyword
  highlight(0, 'systemverilogPacked',    { fg = colors.blue,      bg = 'NONE' })  -- packed keyword
  highlight(0, 'systemverilogTagged',    { fg = colors.blue,      bg = 'NONE' })  -- tagged keyword

  -- Arrays
  highlight(0, 'systemverilogArray',     { fg = colors.blue,      bg = 'NONE' })  -- Array declarations
  highlight(0, 'systemverilogQueue',     { fg = colors.turquoise, bg = 'NONE' })  -- Queue type [$]
  highlight(0, 'systemverilogAssocArray', { fg = colors.turquoise, bg = 'NONE' }) -- Associative array [*]

  -- Object-Oriented Programming
  highlight(0, 'systemverilogClass',     { fg = colors.blue,      bg = 'NONE' })  -- class keyword
  highlight(0, 'systemverilogEndclass',  { fg = colors.blue,      bg = 'NONE' })  -- endclass keyword
  highlight(0, 'systemverilogExtends',   { fg = colors.blue,      bg = 'NONE' })  -- extends keyword
  highlight(0, 'systemverilogImplements', { fg = colors.blue,     bg = 'NONE' })  -- implements keyword
  highlight(0, 'systemverilogNew',       { fg = colors.blue,      bg = 'NONE' })  -- new keyword
  highlight(0, 'systemverilogThis',      { fg = colors.purple,    bg = 'NONE' })  -- this keyword
  highlight(0, 'systemverilogSuper',     { fg = colors.purple,    bg = 'NONE' })  -- super keyword
  highlight(0, 'systemverilogNull',      { fg = colors.blue,      bg = 'NONE' })  -- null keyword
  highlight(0, 'systemverilogVirtual',   { fg = colors.blue,      bg = 'NONE' })  -- virtual keyword
  highlight(0, 'systemverilogPure',      { fg = colors.blue,      bg = 'NONE' })  -- pure keyword
  highlight(0, 'systemverilogExtern',    { fg = colors.blue,      bg = 'NONE' })  -- extern keyword
  highlight(0, 'systemverilogStatic',    { fg = colors.blue,      bg = 'NONE' })  -- static keyword
  highlight(0, 'systemverilogLocal',     { fg = colors.blue,      bg = 'NONE' })  -- local keyword
  highlight(0, 'systemverilogProtected', { fg = colors.blue,      bg = 'NONE' })  -- protected keyword
  highlight(0, 'systemverilogConst',     { fg = colors.blue,      bg = 'NONE' })  -- const keyword

  -- Interface/Modport
  highlight(0, 'systemverilogInterface', { fg = colors.blue,      bg = 'NONE' })  -- interface keyword
  highlight(0, 'systemverilogEndinterface', { fg = colors.blue,   bg = 'NONE' })  -- endinterface keyword
  highlight(0, 'systemverilogModport',   { fg = colors.blue,      bg = 'NONE' })  -- modport keyword

  -- Package
  highlight(0, 'systemverilogPackage',   { fg = colors.blue,      bg = 'NONE' })  -- package keyword
  highlight(0, 'systemverilogEndpackage', { fg = colors.blue,     bg = 'NONE' })  -- endpackage keyword
  highlight(0, 'systemverilogImport',    { fg = colors.pink,      bg = 'NONE' })  -- import keyword
  highlight(0, 'systemverilogExport',    { fg = colors.pink,      bg = 'NONE' })  -- export keyword

  -- Program
  highlight(0, 'systemverilogProgram',   { fg = colors.blue,      bg = 'NONE' })  -- program keyword
  highlight(0, 'systemverilogEndprogram', { fg = colors.blue,     bg = 'NONE' })  -- endprogram keyword

  -- Checker
  highlight(0, 'systemverilogChecker',   { fg = colors.blue,      bg = 'NONE' })  -- checker keyword
  highlight(0, 'systemverilogEndchecker', { fg = colors.blue,     bg = 'NONE' })  -- endchecker keyword

  -- Clocking
  highlight(0, 'systemverilogClocking',  { fg = colors.blue,      bg = 'NONE' })  -- clocking keyword
  highlight(0, 'systemverilogEndclocking', { fg = colors.blue,    bg = 'NONE' })  -- endclocking keyword

  -- Always Blocks (Extended)
  highlight(0, 'systemverilogAlwaysComb', { fg = colors.blue,     bg = 'NONE' })  -- always_comb keyword
  highlight(0, 'systemverilogAlwaysFf',  { fg = colors.blue,      bg = 'NONE' })  -- always_ff keyword
  highlight(0, 'systemverilogAlwaysLatch', { fg = colors.blue,    bg = 'NONE' })  -- always_latch keyword

  -- Control Flow (Extended)
  highlight(0, 'systemverilogConditional', { fg = colors.blue,    bg = 'NONE' })  -- if, else, unique, priority
  highlight(0, 'systemverilogRepeat',    { fg = colors.blue,      bg = 'NONE' })  -- for, foreach, while, do, repeat, forever
  highlight(0, 'systemverilogForeach',   { fg = colors.blue,      bg = 'NONE' })  -- foreach keyword
  highlight(0, 'systemverilogReturn',    { fg = colors.blue,      bg = 'NONE' })  -- return keyword
  highlight(0, 'systemverilogBreak',     { fg = colors.blue,      bg = 'NONE' })  -- break keyword
  highlight(0, 'systemverilogContinue',  { fg = colors.blue,      bg = 'NONE' })  -- continue keyword
  highlight(0, 'systemverilogUnique',    { fg = colors.blue,      bg = 'NONE' })  -- unique keyword
  highlight(0, 'systemverilogUnique0',   { fg = colors.blue,      bg = 'NONE' })  -- unique0 keyword
  highlight(0, 'systemverilogPriority',  { fg = colors.blue,      bg = 'NONE' })  -- priority keyword

  -- Fork/Join (Extended)
  highlight(0, 'systemverilogJoinAny',   { fg = colors.blue,      bg = 'NONE' })  -- join_any keyword
  highlight(0, 'systemverilogJoinNone',  { fg = colors.blue,      bg = 'NONE' })  -- join_none keyword
  highlight(0, 'systemverilogForkjoin',  { fg = colors.blue,      bg = 'NONE' })  -- forkjoin keyword

  -- Randomization
  highlight(0, 'systemverilogRand',      { fg = colors.blue,      bg = 'NONE' })  -- rand keyword
  highlight(0, 'systemverilogRandc',     { fg = colors.blue,      bg = 'NONE' })  -- randc keyword
  highlight(0, 'systemverilogConstraint', { fg = colors.blue,     bg = 'NONE' })  -- constraint keyword
  highlight(0, 'systemverilogWith',      { fg = colors.blue,      bg = 'NONE' })  -- with keyword
  highlight(0, 'systemverilogInside',    { fg = colors.blue,      bg = 'NONE' })  -- inside keyword
  highlight(0, 'systemverilogDist',      { fg = colors.blue,      bg = 'NONE' })  -- dist keyword
  highlight(0, 'systemverilogSolve',     { fg = colors.blue,      bg = 'NONE' })  -- solve keyword
  highlight(0, 'systemverilogBefore',    { fg = colors.blue,      bg = 'NONE' })  -- before keyword
  highlight(0, 'systemverilogSoft',      { fg = colors.blue,      bg = 'NONE' })  -- soft keyword
  highlight(0, 'systemverilogRandcase',  { fg = colors.blue,      bg = 'NONE' })  -- randcase keyword
  highlight(0, 'systemverilogRandsequence', { fg = colors.blue,   bg = 'NONE' })  -- randsequence keyword

  -- Assertions
  highlight(0, 'systemverilogAssert',    { fg = colors.blue,      bg = 'NONE' })  -- assert keyword
  highlight(0, 'systemverilogAssume',    { fg = colors.blue,      bg = 'NONE' })  -- assume keyword
  highlight(0, 'systemverilogExpect',    { fg = colors.blue,      bg = 'NONE' })  -- expect keyword
  highlight(0, 'systemverilogRestrict',  { fg = colors.blue,      bg = 'NONE' })  -- restrict keyword

  -- Properties/Sequences
  highlight(0, 'systemverilogProperty',  { fg = colors.blue,      bg = 'NONE' })  -- property keyword
  highlight(0, 'systemverilogEndproperty', { fg = colors.blue,    bg = 'NONE' })  -- endproperty keyword
  highlight(0, 'systemverilogSequence',  { fg = colors.blue,      bg = 'NONE' })  -- sequence keyword
  highlight(0, 'systemverilogEndsequence', { fg = colors.blue,    bg = 'NONE' })  -- endsequence keyword
  highlight(0, 'systemverilogFirstMatch', { fg = colors.blue,     bg = 'NONE' })  -- first_match keyword
  highlight(0, 'systemverilogThroughout', { fg = colors.blue,     bg = 'NONE' })  -- throughout keyword
  highlight(0, 'systemverilogWithin',    { fg = colors.blue,      bg = 'NONE' })  -- within keyword
  highlight(0, 'systemverilogMatches',   { fg = colors.blue,      bg = 'NONE' })  -- matches keyword

  -- Temporal Operators
  highlight(0, 'systemverilogIff',       { fg = colors.blue,      bg = 'NONE' })  -- iff keyword
  highlight(0, 'systemverilogImplies',   { fg = colors.blue,      bg = 'NONE' })  -- |-> |=> operators
  highlight(0, 'systemverilogIntersect', { fg = colors.blue,      bg = 'NONE' })  -- intersect keyword
  highlight(0, 'systemverilogUntil',     { fg = colors.blue,      bg = 'NONE' })  -- until, until_with
  highlight(0, 'systemverilogSUntil',    { fg = colors.blue,      bg = 'NONE' })  -- s_until, s_until_with

  -- Coverage
  highlight(0, 'systemverilogCover',     { fg = colors.blue,      bg = 'NONE' })  -- cover keyword
  highlight(0, 'systemverilogCovergroup', { fg = colors.blue,     bg = 'NONE' })  -- covergroup keyword
  highlight(0, 'systemverilogEndgroup',  { fg = colors.blue,      bg = 'NONE' })  -- endgroup keyword
  highlight(0, 'systemverilogCoverpoint', { fg = colors.blue,     bg = 'NONE' })  -- coverpoint keyword
  highlight(0, 'systemverilogCross',     { fg = colors.blue,      bg = 'NONE' })  -- cross keyword
  highlight(0, 'systemverilogBins',      { fg = colors.blue,      bg = 'NONE' })  -- bins keyword
  highlight(0, 'systemverilogBinsof',    { fg = colors.blue,      bg = 'NONE' })  -- binsof keyword
  highlight(0, 'systemverilogIllegalBins', { fg = colors.blue,    bg = 'NONE' })  -- illegal_bins keyword
  highlight(0, 'systemverilogIgnoreBins', { fg = colors.blue,     bg = 'NONE' })  -- ignore_bins keyword
  highlight(0, 'systemverilogWildcard',  { fg = colors.blue,      bg = 'NONE' })  -- wildcard keyword

  -- Timing
  highlight(0, 'systemverilogTimeunit',  { fg = colors.blue,      bg = 'NONE' })  -- timeunit keyword
  highlight(0, 'systemverilogTimeprecision', { fg = colors.blue,  bg = 'NONE' })  -- timeprecision keyword

  -- Bind
  highlight(0, 'systemverilogBind',      { fg = colors.blue,      bg = 'NONE' })  -- bind keyword

  -- Let
  highlight(0, 'systemverilogLet',       { fg = colors.blue,      bg = 'NONE' })  -- let keyword

  -- Alias
  highlight(0, 'systemverilogAlias',     { fg = colors.blue,      bg = 'NONE' })  -- alias keyword

  -- Wait statements
  highlight(0, 'systemverilogWaitOrder', { fg = colors.blue,      bg = 'NONE' })  -- wait_order keyword


  -----------------------------------------------------------------------------
  -- Types

  highlight(0, 'verilogType',           { fg = colors.turquoise,  bg = 'NONE' })  -- Type names
  highlight(0, 'systemverilogType',     { fg = colors.turquoise,  bg = 'NONE' })  -- SV Type names

  -- Net Types
  highlight(0, 'verilogNetType',        { fg = colors.turquoise,  bg = 'NONE' })  -- wire, reg, tri, etc.

  -- Variable Types
  highlight(0, 'verilogVarType',        { fg = colors.turquoise,  bg = 'NONE' })  -- integer, real, time, etc.

  -- SystemVerilog Types
  highlight(0, 'systemverilogDataType', { fg = colors.turquoise,  bg = 'NONE' })  -- logic, bit, byte, int, etc.


  -----------------------------------------------------------------------------
  -- Labels

  highlight(0, 'verilogLabel',          { fg = colors.orange,     bg = 'NONE' })  -- Labels
  highlight(0, 'systemverilogLabel',    { fg = colors.orange,     bg = 'NONE' })  -- SV Labels


  -----------------------------------------------------------------------------
  -- Constants and Literals

  -- Numbers
  highlight(0, 'verilogNumber',         { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, 'verilogBinary',         { fg = colors.greenLight, bg = 'NONE' })  -- Binary (4'b1010)
  highlight(0, 'verilogOctal',          { fg = colors.greenLight, bg = 'NONE' })  -- Octal (8'o77)
  highlight(0, 'verilogDecimal',        { fg = colors.greenLight, bg = 'NONE' })  -- Decimal (8'd255)
  highlight(0, 'verilogHex',            { fg = colors.greenLight, bg = 'NONE' })  -- Hex (8'hFF)
  highlight(0, 'verilogReal',           { fg = colors.greenLight, bg = 'NONE' })  -- Real numbers
  highlight(0, 'systemverilogNumber',   { fg = colors.greenLight, bg = 'NONE' })  -- SV numbers

  -- Constants
  highlight(0, 'verilogConstant',       { fg = colors.purple,     bg = 'NONE' })  -- Constants
  highlight(0, 'verilogGlobal',         { fg = colors.purple,     bg = 'NONE' })  -- Global constants

  -- Strings
  highlight(0, 'verilogString',         { fg = colors.redLight,   bg = 'NONE' })  -- String literals
  highlight(0, 'verilogEscape',         { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences (\n, \t)
  highlight(0, 'systemverilogBlockString', { fg = colors.redLight, bg = 'NONE' }) -- SV block strings

  -- Characters
  highlight(0, 'verilogCharacter',      { fg = colors.redLight,   bg = 'NONE' })  -- Character constants

  -- Time Literals
  highlight(0, 'verilogTimeUnit',       { fg = colors.turquoise,  bg = 'NONE' })  -- 1ns, 1ps, 1us, etc.


  -----------------------------------------------------------------------------
  -- Operators

  highlight(0, 'verilogOperator',       { fg = colors.white,      bg = 'NONE' })  -- Operators
  highlight(0, 'systemverilogOperator', { fg = colors.white,      bg = 'NONE' })  -- SV operators

  -- Bitwise Operators
  highlight(0, 'verilogBitwiseOp',      { fg = colors.white,      bg = 'NONE' })  -- &, |, ^, ~

  -- Logical Operators
  highlight(0, 'verilogLogicalOp',      { fg = colors.white,      bg = 'NONE' })  -- &&, ||, !

  -- Comparison Operators
  highlight(0, 'verilogCompareOp',      { fg = colors.white,      bg = 'NONE' })  -- ==, !=, ===, !==, <, >, <=, >=

  -- Arithmetic Operators
  highlight(0, 'verilogArithOp',        { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, %, **

  -- Shift Operators
  highlight(0, 'verilogShiftOp',        { fg = colors.white,      bg = 'NONE' })  -- <<, >>, <<<, >>>

  -- Reduction Operators
  highlight(0, 'verilogReductionOp',    { fg = colors.white,      bg = 'NONE' })  -- &, ~&, |, ~|, ^, ~^

  -- Assignment Operators
  highlight(0, 'verilogAssignOp',       { fg = colors.white,      bg = 'NONE' })  -- =, <=, +=, -=, etc.

  -- Ternary Operator
  highlight(0, 'verilogTernaryOp',      { fg = colors.white,      bg = 'NONE' })  -- ? :


  -----------------------------------------------------------------------------
  -- Preprocessor Directives

  highlight(0, 'verilogDirective',      { fg = colors.pink,       bg = 'NONE' })  -- Preprocessor directives
  highlight(0, 'verilogDefine',         { fg = colors.pink,       bg = 'NONE' })  -- `define
  highlight(0, 'verilogUndef',          { fg = colors.pink,       bg = 'NONE' })  -- `undef
  highlight(0, 'verilogIfdef',          { fg = colors.pink,       bg = 'NONE' })  -- `ifdef, `ifndef
  highlight(0, 'verilogElsif',          { fg = colors.pink,       bg = 'NONE' })  -- `elsif
  highlight(0, 'verilogElse',           { fg = colors.pink,       bg = 'NONE' })  -- `else
  highlight(0, 'verilogEndif',          { fg = colors.pink,       bg = 'NONE' })  -- `endif
  highlight(0, 'verilogInclude',        { fg = colors.pink,       bg = 'NONE' })  -- `include
  highlight(0, 'verilogTimescale',      { fg = colors.pink,       bg = 'NONE' })  -- `timescale
  highlight(0, 'verilogDefaultNettype', { fg = colors.pink,       bg = 'NONE' })  -- `default_nettype
  highlight(0, 'verilogResetall',       { fg = colors.pink,       bg = 'NONE' })  -- `resetall
  highlight(0, 'verilogCelldefine',     { fg = colors.pink,       bg = 'NONE' })  -- `celldefine, `endcelldefine
  highlight(0, 'verilogLine',           { fg = colors.pink,       bg = 'NONE' })  -- `line
  highlight(0, 'verilogMacro',          { fg = colors.pink,       bg = 'NONE' })  -- Macro usage


  -----------------------------------------------------------------------------
  -- System Tasks and Functions

  highlight(0, 'verilogSystemTask',     { fg = colors.orange,     bg = 'NONE' })  -- System tasks ($xxx)
  highlight(0, 'verilogSystemFunction', { fg = colors.orange,     bg = 'NONE' })  -- System functions

  -- Display Tasks
  highlight(0, 'verilogDisplay',        { fg = colors.orange,     bg = 'NONE' })  -- $display, $displayb, $displayh, $displayo
  highlight(0, 'verilogWrite',          { fg = colors.orange,     bg = 'NONE' })  -- $write, $writeb, $writeh, $writeo
  highlight(0, 'verilogStrobe',         { fg = colors.orange,     bg = 'NONE' })  -- $strobe, $strobeb, $strobeh, $strobeo
  highlight(0, 'verilogMonitor',        { fg = colors.orange,     bg = 'NONE' })  -- $monitor, $monitorb, $monitorh, $monitoro
  highlight(0, 'verilogMonitorOn',      { fg = colors.orange,     bg = 'NONE' })  -- $monitoron
  highlight(0, 'verilogMonitorOff',     { fg = colors.orange,     bg = 'NONE' })  -- $monitoroff

  -- File I/O Tasks
  highlight(0, 'verilogFopen',          { fg = colors.orange,     bg = 'NONE' })  -- $fopen
  highlight(0, 'verilogFclose',         { fg = colors.orange,     bg = 'NONE' })  -- $fclose
  highlight(0, 'verilogFwrite',         { fg = colors.orange,     bg = 'NONE' })  -- $fwrite, $fwriteb, $fwriteh, $fwriteo
  highlight(0, 'verilogFdisplay',       { fg = colors.orange,     bg = 'NONE' })  -- $fdisplay
  highlight(0, 'verilogFstrobe',        { fg = colors.orange,     bg = 'NONE' })  -- $fstrobe
  highlight(0, 'verilogFmonitor',       { fg = colors.orange,     bg = 'NONE' })  -- $fmonitor
  highlight(0, 'verilogFscanf',         { fg = colors.orange,     bg = 'NONE' })  -- $fscanf, $sscanf
  highlight(0, 'verilogFread',          { fg = colors.orange,     bg = 'NONE' })  -- $fread
  highlight(0, 'verilogFseek',          { fg = colors.orange,     bg = 'NONE' })  -- $fseek, $ftell, $rewind
  highlight(0, 'verilogFflush',         { fg = colors.orange,     bg = 'NONE' })  -- $fflush
  highlight(0, 'verilogFeof',           { fg = colors.orange,     bg = 'NONE' })  -- $feof
  highlight(0, 'verilogFerror',         { fg = colors.orange,     bg = 'NONE' })  -- $ferror

  -- Memory Tasks
  highlight(0, 'verilogReadmemb',       { fg = colors.orange,     bg = 'NONE' })  -- $readmemb
  highlight(0, 'verilogReadmemh',       { fg = colors.orange,     bg = 'NONE' })  -- $readmemh
  highlight(0, 'verilogWritememb',      { fg = colors.orange,     bg = 'NONE' })  -- $writememb
  highlight(0, 'verilogWritememh',      { fg = colors.orange,     bg = 'NONE' })  -- $writememh

  -- Simulation Control
  highlight(0, 'verilogFinish',         { fg = colors.orange,     bg = 'NONE' })  -- $finish
  highlight(0, 'verilogStop',           { fg = colors.orange,     bg = 'NONE' })  -- $stop
  highlight(0, 'verilogExit',           { fg = colors.orange,     bg = 'NONE' })  -- $exit

  -- Time Functions
  highlight(0, 'verilogTimeFunc',       { fg = colors.orange,     bg = 'NONE' })  -- $time, $stime, $realtime

  -- Random Functions
  highlight(0, 'verilogRandom',         { fg = colors.orange,     bg = 'NONE' })  -- $random
  highlight(0, 'verilogUrandom',        { fg = colors.orange,     bg = 'NONE' })  -- $urandom, $urandom_range
  highlight(0, 'verilogSrandom',        { fg = colors.orange,     bg = 'NONE' })  -- $srandom

  -- Waveform Dumping
  highlight(0, 'verilogDumpfile',       { fg = colors.orange,     bg = 'NONE' })  -- $dumpfile
  highlight(0, 'verilogDumpvars',       { fg = colors.orange,     bg = 'NONE' })  -- $dumpvars
  highlight(0, 'verilogDumpall',        { fg = colors.orange,     bg = 'NONE' })  -- $dumpall
  highlight(0, 'verilogDumpflush',      { fg = colors.orange,     bg = 'NONE' })  -- $dumpflush
  highlight(0, 'verilogDumplimit',      { fg = colors.orange,     bg = 'NONE' })  -- $dumplimit
  highlight(0, 'verilogDumpoff',        { fg = colors.orange,     bg = 'NONE' })  -- $dumpoff
  highlight(0, 'verilogDumpon',         { fg = colors.orange,     bg = 'NONE' })  -- $dumpon

  -- Conversion Functions
  highlight(0, 'verilogItoreal',        { fg = colors.orange,     bg = 'NONE' })  -- $itor
  highlight(0, 'verilogRtoi',           { fg = colors.orange,     bg = 'NONE' })  -- $rtoi
  highlight(0, 'verilogBitstoreal',     { fg = colors.orange,     bg = 'NONE' })  -- $bitstoreal
  highlight(0, 'verilogRealtobits',     { fg = colors.orange,     bg = 'NONE' })  -- $realtobits
  highlight(0, 'verilogSigned',         { fg = colors.orange,     bg = 'NONE' })  -- $signed
  highlight(0, 'verilogUnsigned',       { fg = colors.orange,     bg = 'NONE' })  -- $unsigned

  -- Math Functions
  highlight(0, 'verilogClog2',          { fg = colors.orange,     bg = 'NONE' })  -- $clog2
  highlight(0, 'verilogLn',             { fg = colors.orange,     bg = 'NONE' })  -- $ln
  highlight(0, 'verilogLog10',          { fg = colors.orange,     bg = 'NONE' })  -- $log10
  highlight(0, 'verilogExp',            { fg = colors.orange,     bg = 'NONE' })  -- $exp
  highlight(0, 'verilogSqrt',           { fg = colors.orange,     bg = 'NONE' })  -- $sqrt
  highlight(0, 'verilogPow',            { fg = colors.orange,     bg = 'NONE' })  -- $pow
  highlight(0, 'verilogFloor',          { fg = colors.orange,     bg = 'NONE' })  -- $floor
  highlight(0, 'verilogCeil',           { fg = colors.orange,     bg = 'NONE' })  -- $ceil
  highlight(0, 'verilogSin',            { fg = colors.orange,     bg = 'NONE' })  -- $sin, $cos, $tan
  highlight(0, 'verilogAsin',           { fg = colors.orange,     bg = 'NONE' })  -- $asin, $acos, $atan
  highlight(0, 'verilogAtan2',          { fg = colors.orange,     bg = 'NONE' })  -- $atan2
  highlight(0, 'verilogHypot',          { fg = colors.orange,     bg = 'NONE' })  -- $hypot
  highlight(0, 'verilogSinh',           { fg = colors.orange,     bg = 'NONE' })  -- $sinh, $cosh, $tanh
  highlight(0, 'verilogAsinh',          { fg = colors.orange,     bg = 'NONE' })  -- $asinh, $acosh, $atanh

  -- Array Query Functions (SystemVerilog)
  highlight(0, 'systemverilogArrayFunc', { fg = colors.orange,    bg = 'NONE' })  -- $size, $dimensions, etc.
  highlight(0, 'verilogSize',           { fg = colors.orange,     bg = 'NONE' })  -- $size
  highlight(0, 'verilogDimensions',     { fg = colors.orange,     bg = 'NONE' })  -- $dimensions
  highlight(0, 'verilogUnpacked',       { fg = colors.orange,     bg = 'NONE' })  -- $unpacked_dimensions
  highlight(0, 'verilogLeft',           { fg = colors.orange,     bg = 'NONE' })  -- $left
  highlight(0, 'verilogRight',          { fg = colors.orange,     bg = 'NONE' })  -- $right
  highlight(0, 'verilogLow',            { fg = colors.orange,     bg = 'NONE' })  -- $low
  highlight(0, 'verilogHigh',           { fg = colors.orange,     bg = 'NONE' })  -- $high
  highlight(0, 'verilogIncrement',      { fg = colors.orange,     bg = 'NONE' })  -- $increment

  -- Assertion System Functions
  highlight(0, 'verilogSampled',        { fg = colors.orange,     bg = 'NONE' })  -- $sampled
  highlight(0, 'verilogRose',           { fg = colors.orange,     bg = 'NONE' })  -- $rose
  highlight(0, 'verilogFell',           { fg = colors.orange,     bg = 'NONE' })  -- $fell
  highlight(0, 'verilogStable',         { fg = colors.orange,     bg = 'NONE' })  -- $stable
  highlight(0, 'verilogChanged',        { fg = colors.orange,     bg = 'NONE' })  -- $changed
  highlight(0, 'verilogPast',           { fg = colors.orange,     bg = 'NONE' })  -- $past
  highlight(0, 'verilogOnehot',         { fg = colors.orange,     bg = 'NONE' })  -- $onehot, $onehot0
  highlight(0, 'verilogIsunknown',      { fg = colors.orange,     bg = 'NONE' })  -- $isunknown
  highlight(0, 'verilogCountones',      { fg = colors.orange,     bg = 'NONE' })  -- $countones

  -- Type Functions
  highlight(0, 'verilogTypename',       { fg = colors.orange,     bg = 'NONE' })  -- $typename
  highlight(0, 'verilogBits',           { fg = colors.orange,     bg = 'NONE' })  -- $bits
  highlight(0, 'verilogCast',           { fg = colors.orange,     bg = 'NONE' })  -- $cast

  -- Coverage Functions
  highlight(0, 'verilogCoverageFunc',   { fg = colors.orange,     bg = 'NONE' })  -- $coverage_control, etc.

  -- Randomization Methods
  highlight(0, 'verilogRandomize',      { fg = colors.orange,     bg = 'NONE' })  -- randomize()
  highlight(0, 'verilogPreRandomize',   { fg = colors.orange,     bg = 'NONE' })  -- pre_randomize()
  highlight(0, 'verilogPostRandomize',  { fg = colors.orange,     bg = 'NONE' })  -- post_randomize()

  -- String Methods (SystemVerilog)
  highlight(0, 'systemverilogStringMethod', { fg = colors.orange, bg = 'NONE' })  -- len, putc, getc, substr, etc.

  -- Array Methods (SystemVerilog)
  highlight(0, 'systemverilogArrayMethod', { fg = colors.orange,  bg = 'NONE' })  -- find, sort, reverse, shuffle, etc.


  -----------------------------------------------------------------------------
  -- Comments

  highlight(0, 'verilogComment',        { fg = colors.red,        bg = 'NONE' })  -- // and /* */ comments
  highlight(0, 'verilogLineComment',    { fg = colors.red,        bg = 'NONE' })  -- // comments
  highlight(0, 'verilogBlockComment',   { fg = colors.red,        bg = 'NONE' })  -- /* */ comments
  highlight(0, 'verilogTodo',           { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO
  highlight(0, 'systemverilogComment',  { fg = colors.red,        bg = 'NONE' })  -- SV comments


  -----------------------------------------------------------------------------
  -- Module/Instance Names

  highlight(0, 'verilogModuleName',     { fg = colors.turquoise,  bg = 'NONE' })  -- Module names
  highlight(0, 'verilogInstanceName',   { fg = colors.white,      bg = 'NONE' })  -- Instance names
  highlight(0, 'verilogPortName',       { fg = colors.purple,     bg = 'NONE' })  -- Port names
  highlight(0, 'verilogParameterName',  { fg = colors.purple,     bg = 'NONE' })  -- Parameter names


  -----------------------------------------------------------------------------
  -- Identifiers

  highlight(0, 'verilogIdentifier',     { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, 'verilogSignalName',     { fg = colors.white,      bg = 'NONE' })  -- Signal names
  highlight(0, 'verilogFunctionName',   { fg = colors.orange,     bg = 'NONE' })  -- Function names
  highlight(0, 'verilogTaskName',       { fg = colors.orange,     bg = 'NONE' })  -- Task names


  -----------------------------------------------------------------------------
  -- Errors

  highlight(0, 'verilogError',          { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.verilog)

  -- Variables
  highlight(0, '@variable.verilog',              { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.verilog',      { fg = colors.purple,     bg = 'NONE' })  -- Built-in variables
  highlight(0, '@variable.member.verilog',       { fg = colors.purple,     bg = 'NONE' })  -- Struct members
  highlight(0, '@variable.parameter.verilog',    { fg = colors.purple,     bg = 'NONE' })  -- Parameters

  -- Constants
  highlight(0, '@constant.verilog',              { fg = colors.purple,     bg = 'NONE' })  -- Constants

  -- Types
  highlight(0, '@type.verilog',                  { fg = colors.turquoise,  bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.verilog',          { fg = colors.turquoise,  bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.verilog',       { fg = colors.turquoise,  bg = 'NONE' })  -- Type definitions

  -- Functions
  highlight(0, '@function.verilog',              { fg = colors.orange,     bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.verilog',         { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.verilog',      { fg = colors.orange,     bg = 'NONE' })  -- System tasks/functions

  -- Constructors
  highlight(0, '@constructor.verilog',           { fg = colors.turquoise,  bg = 'NONE' })  -- new()

  -- Keywords
  highlight(0, '@keyword.verilog',               { fg = colors.blue,       bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.type.verilog',          { fg = colors.blue,       bg = 'NONE' })  -- wire, reg, logic, etc.
  highlight(0, '@keyword.function.verilog',      { fg = colors.blue,       bg = 'NONE' })  -- function, task
  highlight(0, '@keyword.return.verilog',        { fg = colors.blue,       bg = 'NONE' })  -- return
  highlight(0, '@keyword.repeat.verilog',        { fg = colors.blue,       bg = 'NONE' })  -- for, while, repeat, forever
  highlight(0, '@keyword.conditional.verilog',   { fg = colors.blue,       bg = 'NONE' })  -- if, else, case
  highlight(0, '@keyword.conditional.ternary.verilog', { fg = colors.white, bg = 'NONE' })  -- ? :
  highlight(0, '@keyword.operator.verilog',      { fg = colors.white,      bg = 'NONE' })  -- and, or, not (gate)
  highlight(0, '@keyword.modifier.verilog',      { fg = colors.blue,       bg = 'NONE' })  -- signed, unsigned
  highlight(0, '@keyword.import.verilog',        { fg = colors.pink,       bg = 'NONE' })  -- import
  highlight(0, '@keyword.directive.define.verilog', { fg = colors.pink,    bg = 'NONE' })  -- `define, `include

  -- Modules
  highlight(0, '@module.verilog',                { fg = colors.turquoise,  bg = 'NONE' })  -- Module names

  -- Labels
  highlight(0, '@label.verilog',                 { fg = colors.orange,     bg = 'NONE' })  -- Labels

  -- Attributes
  highlight(0, '@attribute.verilog',             { fg = colors.pink,       bg = 'NONE' })  -- (* attributes *)

  -- Strings
  highlight(0, '@string.verilog',                { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@string.special.path.verilog',   { fg = colors.redLight,   bg = 'NONE' })  -- Include paths

  -- Numbers
  highlight(0, '@number.verilog',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers

  -- Operators
  highlight(0, '@operator.verilog',              { fg = colors.white,      bg = 'NONE' })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.verilog',   { fg = colors.white,      bg = 'NONE' })  -- ( ) [ ] { }
  highlight(0, '@punctuation.delimiter.verilog', { fg = colors.white,      bg = 'NONE' })  -- , ; :

  -- Comments
  highlight(0, '@comment.verilog',               { fg = colors.red,        bg = 'NONE' })  -- Comments


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.systemverilog) - extends verilog

  highlight(0, '@variable.systemverilog',            { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@variable.builtin.systemverilog',    { fg = colors.purple,     bg = 'NONE' })  -- this, super
  highlight(0, '@variable.member.systemverilog',     { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@variable.parameter.systemverilog',  { fg = colors.purple,     bg = 'NONE' })

  highlight(0, '@type.systemverilog',                { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@type.builtin.systemverilog',        { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@type.definition.systemverilog',     { fg = colors.turquoise,  bg = 'NONE' })

  highlight(0, '@function.systemverilog',            { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.call.systemverilog',       { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.builtin.systemverilog',    { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.method.systemverilog',     { fg = colors.orange,     bg = 'NONE' })

  highlight(0, '@constructor.systemverilog',         { fg = colors.turquoise,  bg = 'NONE' })

  highlight(0, '@keyword.systemverilog',             { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.type.systemverilog',        { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.function.systemverilog',    { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.return.systemverilog',      { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.repeat.systemverilog',      { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.conditional.systemverilog', { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.modifier.systemverilog',    { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.import.systemverilog',      { fg = colors.pink,       bg = 'NONE' })

  highlight(0, '@module.systemverilog',              { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@label.systemverilog',               { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@attribute.systemverilog',           { fg = colors.pink,       bg = 'NONE' })

  highlight(0, '@string.systemverilog',              { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@number.systemverilog',              { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@operator.systemverilog',            { fg = colors.white,      bg = 'NONE' })

  highlight(0, '@punctuation.bracket.systemverilog', { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@punctuation.delimiter.systemverilog', { fg = colors.white,    bg = 'NONE' })

  highlight(0, '@comment.systemverilog',             { fg = colors.red,        bg = 'NONE' })


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.verilog)

  highlight(0, '@lsp.type.variable.verilog',      { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.parameter.verilog',     { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.property.verilog',      { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.function.verilog',      { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.method.verilog',        { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.type.verilog',          { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.class.verilog',         { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.struct.verilog',        { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.enum.verilog',          { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.enumMember.verilog',    { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.namespace.verilog',     { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.verilog',       { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.modifier.verilog',      { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.operator.verilog',      { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.string.verilog',        { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@lsp.type.number.verilog',        { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@lsp.type.comment.verilog',       { fg = colors.red,        bg = 'NONE' })
  highlight(0, '@lsp.type.macro.verilog',         { fg = colors.pink,       bg = 'NONE' })

  -- LSP Semantic Tokens (@lsp.type.xxx.systemverilog)
  highlight(0, '@lsp.type.variable.systemverilog',   { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.parameter.systemverilog',  { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.property.systemverilog',   { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.function.systemverilog',   { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.method.systemverilog',     { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.type.systemverilog',       { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.class.systemverilog',      { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.struct.systemverilog',     { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.enum.systemverilog',       { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.enumMember.systemverilog', { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.namespace.systemverilog',  { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.systemverilog',    { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.modifier.systemverilog',   { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.operator.systemverilog',   { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.string.systemverilog',     { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@lsp.type.number.systemverilog',     { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@lsp.type.comment.systemverilog',    { fg = colors.red,        bg = 'NONE' })
  highlight(0, '@lsp.type.macro.systemverilog',      { fg = colors.pink,       bg = 'NONE' })


  -----------------------------------------------------------------------------
  -- UVM (Universal Verification Methodology) - Common Classes

  highlight(0, 'uvmClass',              { fg = colors.turquoise,  bg = 'NONE' })  -- uvm_component, uvm_object, etc.
  highlight(0, 'uvmMacro',              { fg = colors.pink,       bg = 'NONE' })  -- `uvm_info, `uvm_error, etc.
  highlight(0, 'uvmPhase',              { fg = colors.orange,     bg = 'NONE' })  -- build_phase, run_phase, etc.
  highlight(0, 'uvmFactory',            { fg = colors.orange,     bg = 'NONE' })  -- create, type_id, etc.

end

return verilog
