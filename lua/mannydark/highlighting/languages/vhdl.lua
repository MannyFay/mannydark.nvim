-------------------------------------------------------------------------------
-- VHDL (VHSIC Hardware Description Language)
-- Highlighting for .vhd, .vhdl files
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local vhdl      = {}


-------------------------------------------------------------------------------
-- Settings

vhdl.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Legacy Syntax Groups

  -- Keywords - General Statements
  highlight(0, 'vhdlStatement',         { fg = colors.blue,       bg = 'NONE' })  -- General keywords
  highlight(0, 'vhdlKeyword',           { link = "Keyword" })  -- Keywords

  -- Keywords - Structural
  highlight(0, 'vhdlStructure',         { fg = colors.blue,       bg = 'NONE' })  -- architecture, entity, component
  highlight(0, 'vhdlEntity',            { fg = colors.blue,       bg = 'NONE' })  -- entity keyword
  highlight(0, 'vhdlArchitecture',      { fg = colors.blue,       bg = 'NONE' })  -- architecture keyword
  highlight(0, 'vhdlComponent',         { fg = colors.blue,       bg = 'NONE' })  -- component keyword
  highlight(0, 'vhdlConfiguration',     { fg = colors.blue,       bg = 'NONE' })  -- configuration keyword
  highlight(0, 'vhdlPackage',           { fg = colors.blue,       bg = 'NONE' })  -- package keyword
  highlight(0, 'vhdlBlock',             { fg = colors.blue,       bg = 'NONE' })  -- block keyword
  highlight(0, 'vhdlGenerate',          { fg = colors.blue,       bg = 'NONE' })  -- generate keyword
  highlight(0, 'vhdlContext',           { fg = colors.blue,       bg = 'NONE' })  -- context keyword (VHDL-2008)

  -- Keywords - Process/Procedure/Function
  highlight(0, 'vhdlProcess',           { fg = colors.blue,       bg = 'NONE' })  -- process keyword
  highlight(0, 'vhdlProcedure',         { fg = colors.blue,       bg = 'NONE' })  -- procedure keyword
  highlight(0, 'vhdlFunction',          { link = "Function" })  -- function keyword
  highlight(0, 'vhdlImpure',            { fg = colors.blue,       bg = 'NONE' })  -- impure keyword
  highlight(0, 'vhdlPure',              { fg = colors.blue,       bg = 'NONE' })  -- pure keyword

  -- Keywords - Control Flow
  highlight(0, 'vhdlConditional',       { link = "Conditional" })  -- if, then, else, elsif, case, when
  highlight(0, 'vhdlRepeat',            { fg = colors.blue,       bg = 'NONE' })  -- for, while, loop, next, exit
  highlight(0, 'vhdlLabel',             { fg = colors.orange,     bg = 'NONE' })  -- Labels

  -- Keywords - Begin/End
  highlight(0, 'vhdlBegin',             { fg = colors.blue,       bg = 'NONE' })  -- begin keyword
  highlight(0, 'vhdlEnd',               { fg = colors.blue,       bg = 'NONE' })  -- end keyword
  highlight(0, 'vhdlIs',                { fg = colors.blue,       bg = 'NONE' })  -- is keyword

  -- Keywords - Signal/Variable/Constant
  highlight(0, 'vhdlSignal',            { fg = colors.blue,       bg = 'NONE' })  -- signal keyword
  highlight(0, 'vhdlVariable',          { link = "Variable" })  -- variable keyword
  highlight(0, 'vhdlConstant',          { link = "Constant" })  -- constant keyword
  highlight(0, 'vhdlFile',              { fg = colors.blue,       bg = 'NONE' })  -- file keyword
  highlight(0, 'vhdlAlias',             { fg = colors.blue,       bg = 'NONE' })  -- alias keyword
  highlight(0, 'vhdlShared',            { fg = colors.blue,       bg = 'NONE' })  -- shared keyword

  -- Keywords - Port/Generic
  highlight(0, 'vhdlPort',              { fg = colors.blue,       bg = 'NONE' })  -- port keyword
  highlight(0, 'vhdlGeneric',           { fg = colors.blue,       bg = 'NONE' })  -- generic keyword
  highlight(0, 'vhdlMap',               { fg = colors.blue,       bg = 'NONE' })  -- map keyword

  -- Keywords - Direction
  highlight(0, 'vhdlDirection',         { fg = colors.blue,       bg = 'NONE' })  -- in, out, inout, buffer, linkage
  highlight(0, 'vhdlIn',                { fg = colors.blue,       bg = 'NONE' })  -- in keyword
  highlight(0, 'vhdlOut',               { fg = colors.blue,       bg = 'NONE' })  -- out keyword
  highlight(0, 'vhdlInout',             { fg = colors.blue,       bg = 'NONE' })  -- inout keyword
  highlight(0, 'vhdlBuffer',            { fg = colors.blue,       bg = 'NONE' })  -- buffer keyword
  highlight(0, 'vhdlLinkage',           { fg = colors.blue,       bg = 'NONE' })  -- linkage keyword

  -- Keywords - Range
  highlight(0, 'vhdlRange',             { fg = colors.blue,       bg = 'NONE' })  -- range keyword
  highlight(0, 'vhdlDownto',            { fg = colors.blue,       bg = 'NONE' })  -- downto keyword
  highlight(0, 'vhdlTo',                { fg = colors.blue,       bg = 'NONE' })  -- to keyword

  -- Keywords - Type Definition
  highlight(0, 'vhdlTypedef',           { link = "Type" })  -- type, subtype keywords
  highlight(0, 'vhdlRecord',            { fg = colors.blue,       bg = 'NONE' })  -- record keyword
  highlight(0, 'vhdlArray',             { fg = colors.blue,       bg = 'NONE' })  -- array keyword
  highlight(0, 'vhdlAccess',            { fg = colors.blue,       bg = 'NONE' })  -- access keyword
  highlight(0, 'vhdlUnits',             { fg = colors.blue,       bg = 'NONE' })  -- units keyword
  highlight(0, 'vhdlProtected',         { fg = colors.blue,       bg = 'NONE' })  -- protected keyword (VHDL-2000)

  -- Keywords - Library/Use
  highlight(0, 'vhdlLibrary',           { fg = colors.pink,       bg = 'NONE' })  -- library keyword
  highlight(0, 'vhdlUse',               { fg = colors.pink,       bg = 'NONE' })  -- use keyword

  -- Keywords - Assertion/Report
  highlight(0, 'vhdlAssert',            { fg = colors.blue,       bg = 'NONE' })  -- assert keyword
  highlight(0, 'vhdlReport',            { fg = colors.blue,       bg = 'NONE' })  -- report keyword
  highlight(0, 'vhdlSeverity',          { fg = colors.blue,       bg = 'NONE' })  -- severity keyword

  -- Keywords - Wait
  highlight(0, 'vhdlWait',              { fg = colors.blue,       bg = 'NONE' })  -- wait keyword
  highlight(0, 'vhdlUntil',             { fg = colors.blue,       bg = 'NONE' })  -- until keyword
  highlight(0, 'vhdlAfter',             { fg = colors.blue,       bg = 'NONE' })  -- after keyword
  highlight(0, 'vhdlOn',                { fg = colors.blue,       bg = 'NONE' })  -- on keyword

  -- Keywords - Other
  highlight(0, 'vhdlReturn',            { fg = colors.blue,       bg = 'NONE' })  -- return keyword
  highlight(0, 'vhdlNull',              { fg = colors.blue,       bg = 'NONE' })  -- null keyword
  highlight(0, 'vhdlOpen',              { fg = colors.blue,       bg = 'NONE' })  -- open keyword
  highlight(0, 'vhdlOthers',            { fg = colors.blue,       bg = 'NONE' })  -- others keyword
  highlight(0, 'vhdlAll',               { fg = colors.blue,       bg = 'NONE' })  -- all keyword
  highlight(0, 'vhdlNew',               { fg = colors.blue,       bg = 'NONE' })  -- new keyword
  highlight(0, 'vhdlSelect',            { fg = colors.blue,       bg = 'NONE' })  -- select keyword
  highlight(0, 'vhdlWith',              { fg = colors.blue,       bg = 'NONE' })  -- with keyword
  highlight(0, 'vhdlGroup',             { fg = colors.blue,       bg = 'NONE' })  -- group keyword
  highlight(0, 'vhdlPostponed',         { fg = colors.blue,       bg = 'NONE' })  -- postponed keyword
  highlight(0, 'vhdlGuarded',           { fg = colors.blue,       bg = 'NONE' })  -- guarded keyword
  highlight(0, 'vhdlBus',               { fg = colors.blue,       bg = 'NONE' })  -- bus keyword
  highlight(0, 'vhdlRegister',          { fg = colors.blue,       bg = 'NONE' })  -- register keyword
  highlight(0, 'vhdlDisconnect',        { fg = colors.blue,       bg = 'NONE' })  -- disconnect keyword
  highlight(0, 'vhdlReject',            { fg = colors.blue,       bg = 'NONE' })  -- reject keyword
  highlight(0, 'vhdlInertial',          { fg = colors.blue,       bg = 'NONE' })  -- inertial keyword
  highlight(0, 'vhdlTransport',         { fg = colors.blue,       bg = 'NONE' })  -- transport keyword
  highlight(0, 'vhdlUnaffected',        { fg = colors.blue,       bg = 'NONE' })  -- unaffected keyword
  highlight(0, 'vhdlLiteral',           { fg = colors.blue,       bg = 'NONE' })  -- literal keyword

  -- VHDL-2008 Keywords
  highlight(0, 'vhdlForce',             { fg = colors.blue,       bg = 'NONE' })  -- force keyword
  highlight(0, 'vhdlRelease',           { fg = colors.blue,       bg = 'NONE' })  -- release keyword
  highlight(0, 'vhdlDefault',           { fg = colors.blue,       bg = 'NONE' })  -- default keyword
  highlight(0, 'vhdlParameter',         { fg = colors.blue,       bg = 'NONE' })  -- parameter keyword

  -- VHDL-2008 PSL Keywords
  highlight(0, 'vhdlAssume',            { fg = colors.blue,       bg = 'NONE' })  -- assume keyword
  highlight(0, 'vhdlCover',             { fg = colors.blue,       bg = 'NONE' })  -- cover keyword
  highlight(0, 'vhdlFairness',          { fg = colors.blue,       bg = 'NONE' })  -- fairness keyword
  highlight(0, 'vhdlProperty',          { fg = colors.blue,       bg = 'NONE' })  -- property keyword
  highlight(0, 'vhdlRestrict',          { fg = colors.blue,       bg = 'NONE' })  -- restrict keyword
  highlight(0, 'vhdlSequence',          { fg = colors.blue,       bg = 'NONE' })  -- sequence keyword
  highlight(0, 'vhdlStrong',            { fg = colors.blue,       bg = 'NONE' })  -- strong keyword
  highlight(0, 'vhdlVmode',             { fg = colors.blue,       bg = 'NONE' })  -- vmode keyword
  highlight(0, 'vhdlVpkg',              { fg = colors.blue,       bg = 'NONE' })  -- vpkg keyword
  highlight(0, 'vhdlVprop',             { fg = colors.blue,       bg = 'NONE' })  -- vprop keyword
  highlight(0, 'vhdlVunit',             { fg = colors.blue,       bg = 'NONE' })  -- vunit keyword

  -- VHDL-2019 Keywords
  highlight(0, 'vhdlPrivate',           { fg = colors.blue,       bg = 'NONE' })  -- private keyword
  highlight(0, 'vhdlView',              { fg = colors.blue,       bg = 'NONE' })  -- view keyword


  -----------------------------------------------------------------------------
  -- Types

  -- Standard Types
  highlight(0, 'vhdlType',              { link = "Type" })  -- Type names
  highlight(0, 'vhdlStdType',           { link = "Type" })  -- Standard types

  -- Basic Types (std.standard)
  highlight(0, 'vhdlTypeBit',           { link = "Type" })  -- bit
  highlight(0, 'vhdlTypeBoolean',       { link = "Boolean" })  -- boolean
  highlight(0, 'vhdlTypeCharacter',     { link = "Type" })  -- character
  highlight(0, 'vhdlTypeInteger',       { link = "Type" })  -- integer
  highlight(0, 'vhdlTypeNatural',       { link = "Type" })  -- natural
  highlight(0, 'vhdlTypePositive',      { link = "Type" })  -- positive
  highlight(0, 'vhdlTypeReal',          { link = "Type" })  -- real
  highlight(0, 'vhdlTypeTime',          { link = "Type" })  -- time
  highlight(0, 'vhdlTypeString',        { link = "String" })  -- string
  highlight(0, 'vhdlTypeLine',          { link = "Type" })  -- line
  highlight(0, 'vhdlTypeText',          { link = "Type" })  -- text
  highlight(0, 'vhdlTypeSide',          { link = "Type" })  -- side
  highlight(0, 'vhdlTypeWidth',         { link = "Type" })  -- width

  -- Vector Types
  highlight(0, 'vhdlVector',            { fg = colors.turquoise,  bg = 'NONE' })  -- Vector types
  highlight(0, 'vhdlTypeBitVector',     { link = "Type" })  -- bit_vector
  highlight(0, 'vhdlTypeBooleanVector', { link = "Boolean" })  -- boolean_vector
  highlight(0, 'vhdlTypeIntegerVector', { link = "Type" })  -- integer_vector
  highlight(0, 'vhdlTypeRealVector',    { link = "Type" })  -- real_vector
  highlight(0, 'vhdlTypeTimeVector',    { link = "Type" })  -- time_vector

  -- IEEE std_logic_1164 Types
  highlight(0, 'vhdlTypeStdUlogic',         { link = "Type" })  -- std_ulogic
  highlight(0, 'vhdlTypeStdLogic',          { link = "Type" })  -- std_logic
  highlight(0, 'vhdlTypeStdUlogicVector',   { link = "Type" })  -- std_ulogic_vector
  highlight(0, 'vhdlTypeStdLogicVector',    { link = "Type" })  -- std_logic_vector

  -- IEEE numeric_std Types
  highlight(0, 'vhdlTypeSigned',            { link = "Type" })  -- signed
  highlight(0, 'vhdlTypeUnsigned',          { link = "Type" })  -- unsigned
  highlight(0, 'vhdlTypeUnresolvedSigned',  { link = "Type" })  -- unresolved_signed
  highlight(0, 'vhdlTypeUnresolvedUnsigned', { link = "Type" })  -- unresolved_unsigned
  highlight(0, 'vhdlTypeUSigned',           { link = "Type" })  -- u_signed
  highlight(0, 'vhdlTypeUUnsigned',         { link = "Type" })  -- u_unsigned

  -- IEEE std_logic_1164 Subtypes
  highlight(0, 'vhdlTypeX01',           { link = "Type" })  -- X01
  highlight(0, 'vhdlTypeX01Z',          { link = "Type" })  -- X01Z
  highlight(0, 'vhdlTypeUX01',          { link = "Type" })  -- UX01
  highlight(0, 'vhdlTypeUX01Z',         { link = "Type" })  -- UX01Z

  -- IEEE fixed/float Types
  highlight(0, 'vhdlTypeSfixed',        { link = "Type" })  -- sfixed
  highlight(0, 'vhdlTypeUfixed',        { link = "Type" })  -- ufixed
  highlight(0, 'vhdlTypeFloat',         { link = "Type" })  -- float
  highlight(0, 'vhdlTypeFloat32',       { link = "Type" })  -- float32
  highlight(0, 'vhdlTypeFloat64',       { link = "Type" })  -- float64
  highlight(0, 'vhdlTypeFloat128',      { link = "Type" })  -- float128


  -----------------------------------------------------------------------------
  -- Attributes

  highlight(0, 'vhdlAttribute',         { fg = colors.pink,       bg = 'NONE' })  -- Attributes

  -- Array Attributes
  highlight(0, 'vhdlAttrHigh',          { fg = colors.pink,       bg = 'NONE' })  -- 'high
  highlight(0, 'vhdlAttrLow',           { fg = colors.pink,       bg = 'NONE' })  -- 'low
  highlight(0, 'vhdlAttrLeft',          { fg = colors.pink,       bg = 'NONE' })  -- 'left
  highlight(0, 'vhdlAttrRight',         { fg = colors.pink,       bg = 'NONE' })  -- 'right
  highlight(0, 'vhdlAttrLength',        { fg = colors.pink,       bg = 'NONE' })  -- 'length
  highlight(0, 'vhdlAttrRange',         { fg = colors.pink,       bg = 'NONE' })  -- 'range
  highlight(0, 'vhdlAttrReverseRange',  { fg = colors.pink,       bg = 'NONE' })  -- 'reverse_range
  highlight(0, 'vhdlAttrAscending',     { fg = colors.pink,       bg = 'NONE' })  -- 'ascending

  -- Signal Attributes
  highlight(0, 'vhdlAttrEvent',         { fg = colors.pink,       bg = 'NONE' })  -- 'event
  highlight(0, 'vhdlAttrActive',        { fg = colors.pink,       bg = 'NONE' })  -- 'active
  highlight(0, 'vhdlAttrLastEvent',     { fg = colors.pink,       bg = 'NONE' })  -- 'last_event
  highlight(0, 'vhdlAttrLastActive',    { fg = colors.pink,       bg = 'NONE' })  -- 'last_active
  highlight(0, 'vhdlAttrLastValue',     { fg = colors.pink,       bg = 'NONE' })  -- 'last_value
  highlight(0, 'vhdlAttrDelayed',       { fg = colors.pink,       bg = 'NONE' })  -- 'delayed
  highlight(0, 'vhdlAttrStable',        { fg = colors.pink,       bg = 'NONE' })  -- 'stable
  highlight(0, 'vhdlAttrQuiet',         { fg = colors.pink,       bg = 'NONE' })  -- 'quiet
  highlight(0, 'vhdlAttrTransaction',   { fg = colors.pink,       bg = 'NONE' })  -- 'transaction
  highlight(0, 'vhdlAttrDriving',       { fg = colors.pink,       bg = 'NONE' })  -- 'driving
  highlight(0, 'vhdlAttrDrivingValue',  { fg = colors.pink,       bg = 'NONE' })  -- 'driving_value

  -- Type Attributes
  highlight(0, 'vhdlAttrBase',          { fg = colors.pink,       bg = 'NONE' })  -- 'base
  highlight(0, 'vhdlAttrSubtype',       { fg = colors.pink,       bg = 'NONE' })  -- 'subtype
  highlight(0, 'vhdlAttrElement',       { fg = colors.pink,       bg = 'NONE' })  -- 'element
  highlight(0, 'vhdlAttrLeftof',        { fg = colors.pink,       bg = 'NONE' })  -- 'leftof
  highlight(0, 'vhdlAttrRightof',       { fg = colors.pink,       bg = 'NONE' })  -- 'rightof
  highlight(0, 'vhdlAttrPos',           { fg = colors.pink,       bg = 'NONE' })  -- 'pos
  highlight(0, 'vhdlAttrVal',           { fg = colors.pink,       bg = 'NONE' })  -- 'val
  highlight(0, 'vhdlAttrSucc',          { fg = colors.pink,       bg = 'NONE' })  -- 'succ
  highlight(0, 'vhdlAttrPred',          { fg = colors.pink,       bg = 'NONE' })  -- 'pred
  highlight(0, 'vhdlAttrImage',         { fg = colors.pink,       bg = 'NONE' })  -- 'image
  highlight(0, 'vhdlAttrValue',         { fg = colors.pink,       bg = 'NONE' })  -- 'value

  -- Object Attributes
  highlight(0, 'vhdlAttrSimpleName',    { fg = colors.pink,       bg = 'NONE' })  -- 'simple_name
  highlight(0, 'vhdlAttrInstanceName',  { fg = colors.pink,       bg = 'NONE' })  -- 'instance_name
  highlight(0, 'vhdlAttrPathName',      { fg = colors.pink,       bg = 'NONE' })  -- 'path_name
  highlight(0, 'vhdlAttrForeign',       { fg = colors.pink,       bg = 'NONE' })  -- 'foreign

  -- Interface Attributes (VHDL-2008)
  highlight(0, 'vhdlAttrConverse',      { fg = colors.pink,       bg = 'NONE' })  -- 'converse


  -----------------------------------------------------------------------------
  -- Constants and Literals

  -- Boolean Values
  highlight(0, 'vhdlBoolean',           { link = "Boolean" })  -- true, false

  -- Severity Levels
  highlight(0, 'vhdlSeverityLevel',     { fg = colors.blue,       bg = 'NONE' })  -- note, warning, error, failure

  -- File Open Kind
  highlight(0, 'vhdlFileOpenKind',      { fg = colors.blue,       bg = 'NONE' })  -- read_mode, write_mode, append_mode

  -- Std_logic Values ('0', '1', 'X', 'Z', 'U', 'W', 'L', 'H', '-')
  highlight(0, 'vhdlLogicValue',        { fg = colors.greenLight, bg = 'NONE' })  -- Logic values

  -- Numbers
  highlight(0, 'vhdlNumber',            { link = "Number" })  -- Numbers
  highlight(0, 'vhdlInteger',           { fg = colors.greenLight, bg = 'NONE' })  -- Integer literals
  highlight(0, 'vhdlReal',              { fg = colors.greenLight, bg = 'NONE' })  -- Real literals
  highlight(0, 'vhdlBased',             { fg = colors.greenLight, bg = 'NONE' })  -- Based literals (2#1010#, 16#FF#)

  -- Time Units
  highlight(0, 'vhdlTime',              { fg = colors.greenLight, bg = 'NONE' })  -- Time literals
  highlight(0, 'vhdlTimeUnit',          { fg = colors.turquoise,  bg = 'NONE' })  -- fs, ps, ns, us, ms, sec, min, hr

  -- Characters
  highlight(0, 'vhdlCharacter',         { fg = colors.redLight,   bg = 'NONE' })  -- Character literals

  -- Strings
  highlight(0, 'vhdlString',            { link = "String" })  -- String literals

  -- Bit String Literals
  highlight(0, 'vhdlBitString',         { link = "String" })  -- B"1010", X"FF", O"777"


  -----------------------------------------------------------------------------
  -- Operators

  highlight(0, 'vhdlOperator',          { link = "Operator" })  -- Operators

  -- Logical Operators
  highlight(0, 'vhdlLogicalOp',         { fg = colors.blue,       bg = 'NONE' })  -- and, or, nand, nor, xor, xnor, not

  -- Relational Operators
  highlight(0, 'vhdlRelationalOp',      { fg = colors.white,      bg = 'NONE' })  -- =, /=, <, <=, >, >=

  -- Shift Operators
  highlight(0, 'vhdlShiftOp',           { fg = colors.blue,       bg = 'NONE' })  -- sll, srl, sla, sra, rol, ror

  -- Arithmetic Operators
  highlight(0, 'vhdlArithOp',           { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, mod, rem, abs, **

  -- Concatenation
  highlight(0, 'vhdlConcatOp',          { fg = colors.white,      bg = 'NONE' })  -- &

  -- Assignment Operators
  highlight(0, 'vhdlAssign',            { fg = colors.white,      bg = 'NONE' })  -- <=, :=, =>


  -----------------------------------------------------------------------------
  -- Special Characters

  highlight(0, 'vhdlSpecial',           { fg = colors.pink,       bg = 'NONE' })  -- Special characters
  highlight(0, 'vhdlDelimiter',         { link = "Delimiter" })  -- Delimiters (, ; :)
  highlight(0, 'vhdlBracket',           { fg = colors.white,      bg = 'NONE' })  -- Brackets ( ) [ ]


  -----------------------------------------------------------------------------
  -- Functions and Procedures (User-defined)

  highlight(0, 'vhdlFunctionName',      { link = "Function" })  -- Function names
  highlight(0, 'vhdlProcedureName',     { fg = colors.orange,     bg = 'NONE' })  -- Procedure names


  -----------------------------------------------------------------------------
  -- IEEE Library Functions

  -- std_logic_1164 Functions
  highlight(0, 'vhdlStd1164Func',       { link = "Function" })  -- std_logic_1164 functions

  -- Conversion Functions
  highlight(0, 'vhdlFuncToBit',         { link = "Function" })  -- to_bit
  highlight(0, 'vhdlFuncToBitVector',   { link = "Function" })  -- to_bitvector
  highlight(0, 'vhdlFuncToStdUlogic',   { link = "Function" })  -- to_stdulogic
  highlight(0, 'vhdlFuncToStdLogicVector',   { link = "Function" })  -- to_stdlogicvector
  highlight(0, 'vhdlFuncToStdUlogicVector',  { link = "Function" })  -- to_stdulogicvector
  highlight(0, 'vhdlFuncToX01',         { link = "Function" })  -- to_x01
  highlight(0, 'vhdlFuncToX01Z',        { link = "Function" })  -- to_x01z
  highlight(0, 'vhdlFuncToUX01',        { link = "Function" })  -- to_ux01

  -- Edge Detection Functions
  highlight(0, 'vhdlFuncRisingEdge',    { link = "Function" })  -- rising_edge
  highlight(0, 'vhdlFuncFallingEdge',   { link = "Function" })  -- falling_edge

  -- Unknown Detection Functions
  highlight(0, 'vhdlFuncIsX',           { link = "Function" })  -- is_x

  -- numeric_std Functions
  highlight(0, 'vhdlNumericFunc',       { link = "Function" })  -- numeric_std functions

  -- Conversion Functions
  highlight(0, 'vhdlFuncToInteger',     { link = "Function" })  -- to_integer
  highlight(0, 'vhdlFuncToUnsigned',    { link = "Function" })  -- to_unsigned
  highlight(0, 'vhdlFuncToSigned',      { link = "Function" })  -- to_signed

  -- Resize Function
  highlight(0, 'vhdlFuncResize',        { link = "Function" })  -- resize

  -- Shift/Rotate Functions
  highlight(0, 'vhdlFuncShiftLeft',     { link = "Function" })  -- shift_left
  highlight(0, 'vhdlFuncShiftRight',    { link = "Function" })  -- shift_right
  highlight(0, 'vhdlFuncRotateLeft',    { link = "Function" })  -- rotate_left
  highlight(0, 'vhdlFuncRotateRight',   { link = "Function" })  -- rotate_right

  -- Match Functions
  highlight(0, 'vhdlFuncStdMatch',      { link = "Function" })  -- std_match

  -- To01 Function
  highlight(0, 'vhdlFuncTo01',          { link = "Function" })  -- to_01

  -- numeric_std_unsigned Functions
  highlight(0, 'vhdlFuncMaximum',       { link = "Function" })  -- maximum
  highlight(0, 'vhdlFuncMinimum',       { link = "Function" })  -- minimum

  -- math_real Functions
  highlight(0, 'vhdlMathFunc',          { link = "Function" })  -- Math functions
  highlight(0, 'vhdlFuncCeil',          { link = "Function" })  -- ceil
  highlight(0, 'vhdlFuncFloor',         { link = "Function" })  -- floor
  highlight(0, 'vhdlFuncRound',         { link = "Function" })  -- round
  highlight(0, 'vhdlFuncTrunc',         { link = "Function" })  -- trunc
  highlight(0, 'vhdlFuncSign',          { link = "Function" })  -- sign
  highlight(0, 'vhdlFuncSqrt',          { link = "Function" })  -- sqrt
  highlight(0, 'vhdlFuncCbrt',          { link = "Function" })  -- cbrt
  highlight(0, 'vhdlFuncExp',           { link = "Function" })  -- exp
  highlight(0, 'vhdlFuncLog',           { link = "Function" })  -- log, log2, log10
  highlight(0, 'vhdlFuncSin',           { link = "Function" })  -- sin, sinh, arcsin, arcsinh
  highlight(0, 'vhdlFuncCos',           { link = "Function" })  -- cos, cosh, arccos, arccosh
  highlight(0, 'vhdlFuncTan',           { link = "Function" })  -- tan, tanh, arctan, arctanh

  -- math_real Constants
  highlight(0, 'vhdlMathConst',         { link = "Constant" })  -- MATH_E, MATH_PI, etc.


  -----------------------------------------------------------------------------
  -- Comments

  highlight(0, 'vhdlComment',           { link = "Comment" })  -- -- comments
  highlight(0, 'vhdlBlockComment',      { link = "Comment" })  -- /* */ comments (VHDL-2008)
  highlight(0, 'vhdlTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO
  highlight(0, 'vhdlFixme',             { fg = colors.red,        bg = 'NONE', bold = true })  -- FIXME


  -----------------------------------------------------------------------------
  -- Preprocessor

  highlight(0, 'vhdlPreProc',           { fg = colors.pink,       bg = 'NONE' })  -- `ifdef, etc.
  highlight(0, 'vhdlInclude',           { fg = colors.pink,       bg = 'NONE' })  -- Include directives


  -----------------------------------------------------------------------------
  -- Errors

  highlight(0, 'vhdlError',             { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax errors
  highlight(0, 'vhdlSpaceError',        { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Space errors


  -----------------------------------------------------------------------------
  -- Entity/Architecture Names

  highlight(0, 'vhdlEntityName',        { fg = colors.turquoise,  bg = 'NONE' })  -- Entity names
  highlight(0, 'vhdlArchName',          { fg = colors.turquoise,  bg = 'NONE' })  -- Architecture names
  highlight(0, 'vhdlComponentName',     { fg = colors.turquoise,  bg = 'NONE' })  -- Component names
  highlight(0, 'vhdlConfigName',        { fg = colors.turquoise,  bg = 'NONE' })  -- Configuration names
  highlight(0, 'vhdlPackageName',       { fg = colors.turquoise,  bg = 'NONE' })  -- Package names


  -----------------------------------------------------------------------------
  -- Signal/Variable/Constant Names

  highlight(0, 'vhdlSignalName',        { fg = colors.white,      bg = 'NONE' })  -- Signal names
  highlight(0, 'vhdlVariableName',      { link = "Variable" })  -- Variable names
  highlight(0, 'vhdlConstantName',      { link = "Constant" })  -- Constant names
  highlight(0, 'vhdlPortName',          { fg = colors.purple,     bg = 'NONE' })  -- Port names
  highlight(0, 'vhdlGenericName',       { fg = colors.purple,     bg = 'NONE' })  -- Generic names


  -----------------------------------------------------------------------------
  -- Identifiers

  highlight(0, 'vhdlIdentifier',        { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, 'vhdlExtendedId',        { fg = colors.white,      bg = 'NONE' })  -- \extended identifiers\


  -----------------------------------------------------------------------------
  -- Libraries

  highlight(0, 'vhdlLibraryName',       { fg = colors.turquoise,  bg = 'NONE' })  -- Library names (ieee, std, work)
  highlight(0, 'vhdlPackageRef',        { fg = colors.turquoise,  bg = 'NONE' })  -- Package references


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.vhdl)

  -- Variables
  highlight(0, '@variable.vhdl',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.member.vhdl',       { link = "Variable" })  -- Record members
  highlight(0, '@variable.parameter.vhdl',    { link = "Variable" })  -- Parameters

  -- Constants
  highlight(0, '@constant.vhdl',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.vhdl',      { link = "Constant" })  -- true, false, null
  highlight(0, '@constant.macro.vhdl',        { link = "Constant" })  -- Macro constants

  -- Types
  highlight(0, '@type.vhdl',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.vhdl',          { link = "Type" })  -- Built-in types
  highlight(0, '@type.definition.vhdl',       { link = "Type" })  -- Type definitions

  -- Functions
  highlight(0, '@function.vhdl',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.builtin.vhdl',      { link = "Function" })  -- Built-in functions
  highlight(0, '@function.method.vhdl',       { link = "Function" })  -- Procedures

  -- Constructors
  highlight(0, '@constructor.vhdl',           { fg = colors.turquoise,  bg = 'NONE' })  -- Aggregate constructors

  -- Keywords
  highlight(0, '@keyword.vhdl',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.vhdl',      { link = "Keyword" })  -- function, procedure
  highlight(0, '@keyword.operator.vhdl',      { link = "Operator" })  -- and, or, not, etc.
  highlight(0, '@keyword.import.vhdl',        { link = "Keyword" })  -- library, use
  highlight(0, '@keyword.type.vhdl',          { link = "Keyword" })  -- type, subtype
  highlight(0, '@keyword.modifier.vhdl',      { link = "Keyword" })  -- signal, variable, constant
  highlight(0, '@keyword.repeat.vhdl',        { link = "Keyword" })  -- for, while, loop
  highlight(0, '@keyword.return.vhdl',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.conditional.vhdl',   { link = "Conditional" })  -- if, then, else, case, when
  highlight(0, '@keyword.conditional.ternary.vhdl', { link = "Conditional" })  -- when/else expressions
  highlight(0, '@keyword.coroutine.vhdl',     { link = "Keyword" })  -- process, wait
  highlight(0, '@keyword.debug.vhdl',         { link = "Keyword" })  -- assert, report
  highlight(0, '@keyword.directive.vhdl',     { link = "Keyword" })  -- Preprocessor

  -- Modules
  highlight(0, '@module.vhdl',                { fg = colors.turquoise,  bg = 'NONE' })  -- Entity/Architecture names
  highlight(0, '@module.builtin.vhdl',        { fg = colors.turquoise,  bg = 'NONE' })  -- ieee, std, work

  -- Labels
  highlight(0, '@label.vhdl',                 { fg = colors.orange,     bg = 'NONE' })  -- Labels

  -- Attributes
  highlight(0, '@attribute.vhdl',             { fg = colors.pink,       bg = 'NONE' })  -- Attributes
  highlight(0, '@attribute.builtin.vhdl',     { fg = colors.pink,       bg = 'NONE' })  -- Built-in attributes

  -- Properties
  highlight(0, '@property.vhdl',              { fg = colors.purple,     bg = 'NONE' })  -- Record fields

  -- Strings
  highlight(0, '@string.vhdl',                { link = "String" })  -- Strings

  -- Characters
  highlight(0, '@character.vhdl',             { fg = colors.redLight,   bg = 'NONE' })  -- Characters

  -- Numbers
  highlight(0, '@number.vhdl',                { link = "Number" })  -- Numbers
  highlight(0, '@number.float.vhdl',          { link = "Number" })  -- Reals

  -- Booleans
  highlight(0, '@boolean.vhdl',               { link = "Boolean" })  -- true, false

  -- Operators
  highlight(0, '@operator.vhdl',              { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.vhdl',   { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, '@punctuation.delimiter.vhdl', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.vhdl',   { fg = colors.pink,       bg = 'NONE' })  -- ' for attributes

  -- Comments
  highlight(0, '@comment.vhdl',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.vhdl', { link = "Comment" })  -- Doc comments
  highlight(0, '@comment.error.vhdl',         { link = "Comment" })  -- Error comments
  highlight(0, '@comment.warning.vhdl',       { link = "Comment" })  -- Warning comments

  -- Spell
  highlight(0, '@spell.vhdl',                 { fg = colors.red,        bg = 'NONE' })  -- Spell check


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.vhdl)

  highlight(0, '@lsp.type.variable.vhdl',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.vhdl',     { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.vhdl',      { fg = colors.purple,     bg = 'NONE' })  -- Record fields
  highlight(0, '@lsp.type.function.vhdl',      { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.vhdl',        { fg = colors.orange,     bg = 'NONE' })  -- Procedures
  highlight(0, '@lsp.type.type.vhdl',          { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.vhdl',         { fg = colors.turquoise,  bg = 'NONE' })  -- Entities
  highlight(0, '@lsp.type.struct.vhdl',        { fg = colors.turquoise,  bg = 'NONE' })  -- Records
  highlight(0, '@lsp.type.enum.vhdl',          { fg = colors.turquoise,  bg = 'NONE' })  -- Enum types
  highlight(0, '@lsp.type.enumMember.vhdl',    { fg = colors.purple,     bg = 'NONE' })  -- Enum values
  highlight(0, '@lsp.type.namespace.vhdl',     { fg = colors.turquoise,  bg = 'NONE' })  -- Libraries/packages
  highlight(0, '@lsp.type.keyword.vhdl',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.vhdl',      { fg = colors.blue,       bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.vhdl',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.vhdl',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.vhdl',        { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.comment.vhdl',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.vhdl',    { link = "Variable" })  -- Constants
  highlight(0, '@lsp.typemod.variable.declaration.vhdl', { link = "Variable" })  -- Declarations
  highlight(0, '@lsp.typemod.function.declaration.vhdl', { fg = colors.orange,    bg = 'NONE' })  -- Function defs
  highlight(0, '@lsp.typemod.type.declaration.vhdl',     { fg = colors.turquoise, bg = 'NONE' })  -- Type defs
  highlight(0, '@lsp.typemod.type.defaultLibrary.vhdl',  { fg = colors.turquoise, bg = 'NONE' })  -- IEEE types


  -----------------------------------------------------------------------------
  -- Common VHDL Patterns

  -- Clock/Reset Signals (common naming conventions)
  highlight(0, 'vhdlClockSignal',       { fg = colors.white,      bg = 'NONE' })  -- clk, clock
  highlight(0, 'vhdlResetSignal',       { fg = colors.white,      bg = 'NONE' })  -- rst, reset

  -- Enable/Valid Signals
  highlight(0, 'vhdlEnableSignal',      { fg = colors.white,      bg = 'NONE' })  -- en, enable
  highlight(0, 'vhdlValidSignal',       { fg = colors.white,      bg = 'NONE' })  -- valid

  -- Data Signals
  highlight(0, 'vhdlDataSignal',        { fg = colors.white,      bg = 'NONE' })  -- data_in, data_out

end

return vhdl
