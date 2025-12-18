-------------------------------------------------------------------------------
-- WebAssembly (WAT/WAST)
-- Highlighting for .wat, .wast files (WebAssembly Text Format).
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local wasm    = {}


-------------------------------------------------------------------------------
-- Settings

wasm.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy) - wat*

  -- Module Structure Keywords
  highlight(0, 'watModule',           { fg = colors.blue,       bg = 'NONE'            })  -- module
  highlight(0, 'watFunc',             { fg = colors.blue,       bg = 'NONE'            })  -- func
  highlight(0, 'watParam',            { fg = colors.blue,       bg = 'NONE'            })  -- param
  highlight(0, 'watResult',           { fg = colors.blue,       bg = 'NONE'            })  -- result
  highlight(0, 'watLocal',            { fg = colors.blue,       bg = 'NONE'            })  -- local
  highlight(0, 'watGlobal',           { fg = colors.blue,       bg = 'NONE'            })  -- global
  highlight(0, 'watMemory',           { fg = colors.blue,       bg = 'NONE'            })  -- memory
  highlight(0, 'watTable',            { fg = colors.blue,       bg = 'NONE'            })  -- table
  highlight(0, 'watType',             { fg = colors.turquoise,  bg = 'NONE'            })  -- type (type definitions)
  highlight(0, 'watImport',           { fg = colors.blue,       bg = 'NONE'            })  -- import
  highlight(0, 'watExport',           { fg = colors.blue,       bg = 'NONE'            })  -- export
  highlight(0, 'watStart',            { fg = colors.blue,       bg = 'NONE'            })  -- start
  highlight(0, 'watElem',             { fg = colors.blue,       bg = 'NONE'            })  -- elem
  highlight(0, 'watData',             { fg = colors.blue,       bg = 'NONE'            })  -- data
  highlight(0, 'watMut',              { fg = colors.blue,       bg = 'NONE'            })  -- mut (mutable global)
  highlight(0, 'watOffset',           { fg = colors.blue,       bg = 'NONE'            })  -- offset
  highlight(0, 'watAlign',            { fg = colors.blue,       bg = 'NONE'            })  -- align
  highlight(0, 'watShared',           { fg = colors.blue,       bg = 'NONE'            })  -- shared (memory)

  -- Value Types
  highlight(0, 'watValueType',        { fg = colors.turquoise,  bg = 'NONE'            })  -- i32, i64, f32, f64
  highlight(0, 'watI32',              { fg = colors.turquoise,  bg = 'NONE'            })  -- i32
  highlight(0, 'watI64',              { fg = colors.turquoise,  bg = 'NONE'            })  -- i64
  highlight(0, 'watF32',              { fg = colors.turquoise,  bg = 'NONE'            })  -- f32
  highlight(0, 'watF64',              { fg = colors.turquoise,  bg = 'NONE'            })  -- f64
  highlight(0, 'watV128',             { fg = colors.turquoise,  bg = 'NONE'            })  -- v128 (SIMD)

  -- Reference Types
  highlight(0, 'watRefType',          { fg = colors.turquoise,  bg = 'NONE'            })  -- funcref, externref
  highlight(0, 'watFuncref',          { fg = colors.turquoise,  bg = 'NONE'            })  -- funcref
  highlight(0, 'watExternref',        { fg = colors.turquoise,  bg = 'NONE'            })  -- externref
  highlight(0, 'watAnyref',           { fg = colors.turquoise,  bg = 'NONE'            })  -- anyref

  -- Control Flow Instructions
  highlight(0, 'watControlInst',      { fg = colors.blue,       bg = 'NONE'            })  -- Control instructions
  highlight(0, 'watBlock',            { fg = colors.blue,       bg = 'NONE'            })  -- block
  highlight(0, 'watLoop',             { fg = colors.blue,       bg = 'NONE'            })  -- loop
  highlight(0, 'watIf',               { fg = colors.blue,       bg = 'NONE'            })  -- if
  highlight(0, 'watElse',             { fg = colors.blue,       bg = 'NONE'            })  -- else
  highlight(0, 'watEnd',              { fg = colors.blue,       bg = 'NONE'            })  -- end
  highlight(0, 'watBr',               { fg = colors.blue,       bg = 'NONE'            })  -- br, br_if, br_table
  highlight(0, 'watReturn',           { fg = colors.blue,       bg = 'NONE'            })  -- return
  highlight(0, 'watCall',             { fg = colors.orange,     bg = 'NONE'            })  -- call, call_indirect
  highlight(0, 'watUnreachable',      { fg = colors.red,        bg = 'NONE'            })  -- unreachable
  highlight(0, 'watNop',              { fg = colors.white,      bg = 'NONE'            })  -- nop
  highlight(0, 'watDrop',             { fg = colors.white,      bg = 'NONE'            })  -- drop
  highlight(0, 'watSelect',           { fg = colors.blue,       bg = 'NONE'            })  -- select

  -- Variable Instructions
  highlight(0, 'watInstGetSet',       { fg = colors.orange,     bg = 'NONE'            })  -- get/set/tee instructions
  highlight(0, 'watLocalGet',         { fg = colors.orange,     bg = 'NONE'            })  -- local.get
  highlight(0, 'watLocalSet',         { fg = colors.orange,     bg = 'NONE'            })  -- local.set
  highlight(0, 'watLocalTee',         { fg = colors.orange,     bg = 'NONE'            })  -- local.tee
  highlight(0, 'watGlobalGet',        { fg = colors.orange,     bg = 'NONE'            })  -- global.get
  highlight(0, 'watGlobalSet',        { fg = colors.orange,     bg = 'NONE'            })  -- global.set

  -- Memory Instructions
  highlight(0, 'watMemoryInst',       { fg = colors.orange,     bg = 'NONE'            })  -- Memory instructions
  highlight(0, 'watLoad',             { fg = colors.orange,     bg = 'NONE'            })  -- *.load, *.load8_s, *.load8_u, etc.
  highlight(0, 'watStore',            { fg = colors.orange,     bg = 'NONE'            })  -- *.store, *.store8, etc.
  highlight(0, 'watMemorySize',       { fg = colors.orange,     bg = 'NONE'            })  -- memory.size
  highlight(0, 'watMemoryGrow',       { fg = colors.orange,     bg = 'NONE'            })  -- memory.grow
  highlight(0, 'watMemoryCopy',       { fg = colors.orange,     bg = 'NONE'            })  -- memory.copy
  highlight(0, 'watMemoryFill',       { fg = colors.orange,     bg = 'NONE'            })  -- memory.fill
  highlight(0, 'watMemoryInit',       { fg = colors.orange,     bg = 'NONE'            })  -- memory.init
  highlight(0, 'watDataDrop',         { fg = colors.orange,     bg = 'NONE'            })  -- data.drop

  -- Table Instructions
  highlight(0, 'watTableInst',        { fg = colors.orange,     bg = 'NONE'            })  -- Table instructions
  highlight(0, 'watTableGet',         { fg = colors.orange,     bg = 'NONE'            })  -- table.get
  highlight(0, 'watTableSet',         { fg = colors.orange,     bg = 'NONE'            })  -- table.set
  highlight(0, 'watTableSize',        { fg = colors.orange,     bg = 'NONE'            })  -- table.size
  highlight(0, 'watTableGrow',        { fg = colors.orange,     bg = 'NONE'            })  -- table.grow
  highlight(0, 'watTableCopy',        { fg = colors.orange,     bg = 'NONE'            })  -- table.copy
  highlight(0, 'watTableInit',        { fg = colors.orange,     bg = 'NONE'            })  -- table.init
  highlight(0, 'watElemDrop',         { fg = colors.orange,     bg = 'NONE'            })  -- elem.drop

  -- Numeric Instructions - Constants
  highlight(0, 'watConst',            { fg = colors.orange,     bg = 'NONE'            })  -- *.const
  highlight(0, 'watI32Const',         { fg = colors.orange,     bg = 'NONE'            })  -- i32.const
  highlight(0, 'watI64Const',         { fg = colors.orange,     bg = 'NONE'            })  -- i64.const
  highlight(0, 'watF32Const',         { fg = colors.orange,     bg = 'NONE'            })  -- f32.const
  highlight(0, 'watF64Const',         { fg = colors.orange,     bg = 'NONE'            })  -- f64.const
  highlight(0, 'watV128Const',        { fg = colors.orange,     bg = 'NONE'            })  -- v128.const

  -- Numeric Instructions - Arithmetic
  highlight(0, 'watInstGeneral',      { fg = colors.orange,     bg = 'NONE'            })  -- General instructions
  highlight(0, 'watInstWithType',     { fg = colors.orange,     bg = 'NONE'            })  -- Type-prefixed instructions
  highlight(0, 'watArithInst',        { fg = colors.orange,     bg = 'NONE'            })  -- Arithmetic instructions
  highlight(0, 'watAdd',              { fg = colors.orange,     bg = 'NONE'            })  -- *.add
  highlight(0, 'watSub',              { fg = colors.orange,     bg = 'NONE'            })  -- *.sub
  highlight(0, 'watMul',              { fg = colors.orange,     bg = 'NONE'            })  -- *.mul
  highlight(0, 'watDiv',              { fg = colors.orange,     bg = 'NONE'            })  -- *.div_s, *.div_u, *.div
  highlight(0, 'watRem',              { fg = colors.orange,     bg = 'NONE'            })  -- *.rem_s, *.rem_u

  -- Numeric Instructions - Bitwise
  highlight(0, 'watBitwiseInst',      { fg = colors.orange,     bg = 'NONE'            })  -- Bitwise instructions
  highlight(0, 'watAnd',              { fg = colors.orange,     bg = 'NONE'            })  -- *.and
  highlight(0, 'watOr',               { fg = colors.orange,     bg = 'NONE'            })  -- *.or
  highlight(0, 'watXor',              { fg = colors.orange,     bg = 'NONE'            })  -- *.xor
  highlight(0, 'watShl',              { fg = colors.orange,     bg = 'NONE'            })  -- *.shl
  highlight(0, 'watShr',              { fg = colors.orange,     bg = 'NONE'            })  -- *.shr_s, *.shr_u
  highlight(0, 'watRotl',             { fg = colors.orange,     bg = 'NONE'            })  -- *.rotl
  highlight(0, 'watRotr',             { fg = colors.orange,     bg = 'NONE'            })  -- *.rotr
  highlight(0, 'watClz',              { fg = colors.orange,     bg = 'NONE'            })  -- *.clz (count leading zeros)
  highlight(0, 'watCtz',              { fg = colors.orange,     bg = 'NONE'            })  -- *.ctz (count trailing zeros)
  highlight(0, 'watPopcnt',           { fg = colors.orange,     bg = 'NONE'            })  -- *.popcnt (population count)

  -- Numeric Instructions - Comparison
  highlight(0, 'watCompareInst',      { fg = colors.orange,     bg = 'NONE'            })  -- Comparison instructions
  highlight(0, 'watEq',               { fg = colors.orange,     bg = 'NONE'            })  -- *.eq
  highlight(0, 'watNe',               { fg = colors.orange,     bg = 'NONE'            })  -- *.ne
  highlight(0, 'watLt',               { fg = colors.orange,     bg = 'NONE'            })  -- *.lt_s, *.lt_u, *.lt
  highlight(0, 'watLe',               { fg = colors.orange,     bg = 'NONE'            })  -- *.le_s, *.le_u, *.le
  highlight(0, 'watGt',               { fg = colors.orange,     bg = 'NONE'            })  -- *.gt_s, *.gt_u, *.gt
  highlight(0, 'watGe',               { fg = colors.orange,     bg = 'NONE'            })  -- *.ge_s, *.ge_u, *.ge
  highlight(0, 'watEqz',              { fg = colors.orange,     bg = 'NONE'            })  -- *.eqz (equal to zero)

  -- Numeric Instructions - Conversion
  highlight(0, 'watConvertInst',      { fg = colors.orange,     bg = 'NONE'            })  -- Conversion instructions
  highlight(0, 'watWrap',             { fg = colors.orange,     bg = 'NONE'            })  -- i32.wrap_i64
  highlight(0, 'watExtend',           { fg = colors.orange,     bg = 'NONE'            })  -- i64.extend_i32_s, i64.extend_i32_u
  highlight(0, 'watTrunc',            { fg = colors.orange,     bg = 'NONE'            })  -- *.trunc_*
  highlight(0, 'watConvert',          { fg = colors.orange,     bg = 'NONE'            })  -- *.convert_*
  highlight(0, 'watPromote',          { fg = colors.orange,     bg = 'NONE'            })  -- f64.promote_f32
  highlight(0, 'watDemote',           { fg = colors.orange,     bg = 'NONE'            })  -- f32.demote_f64
  highlight(0, 'watReinterpret',      { fg = colors.orange,     bg = 'NONE'            })  -- *.reinterpret_*

  -- Numeric Instructions - Floating Point
  highlight(0, 'watFloatInst',        { fg = colors.orange,     bg = 'NONE'            })  -- Floating point instructions
  highlight(0, 'watAbs',              { fg = colors.orange,     bg = 'NONE'            })  -- *.abs
  highlight(0, 'watNeg',              { fg = colors.orange,     bg = 'NONE'            })  -- *.neg
  highlight(0, 'watSqrt',             { fg = colors.orange,     bg = 'NONE'            })  -- *.sqrt
  highlight(0, 'watFloor',            { fg = colors.orange,     bg = 'NONE'            })  -- *.floor
  highlight(0, 'watCeil',             { fg = colors.orange,     bg = 'NONE'            })  -- *.ceil
  highlight(0, 'watNearest',          { fg = colors.orange,     bg = 'NONE'            })  -- *.nearest
  highlight(0, 'watMin',              { fg = colors.orange,     bg = 'NONE'            })  -- *.min
  highlight(0, 'watMax',              { fg = colors.orange,     bg = 'NONE'            })  -- *.max
  highlight(0, 'watCopysign',         { fg = colors.orange,     bg = 'NONE'            })  -- *.copysign

  -- SIMD Instructions (v128)
  highlight(0, 'watSimdInst',         { fg = colors.orange,     bg = 'NONE'            })  -- SIMD instructions
  highlight(0, 'watV128Load',         { fg = colors.orange,     bg = 'NONE'            })  -- v128.load*
  highlight(0, 'watV128Store',        { fg = colors.orange,     bg = 'NONE'            })  -- v128.store
  highlight(0, 'watSplat',            { fg = colors.orange,     bg = 'NONE'            })  -- *.splat
  highlight(0, 'watExtractLane',      { fg = colors.orange,     bg = 'NONE'            })  -- *.extract_lane*
  highlight(0, 'watReplaceLane',      { fg = colors.orange,     bg = 'NONE'            })  -- *.replace_lane
  highlight(0, 'watShuffle',          { fg = colors.orange,     bg = 'NONE'            })  -- i8x16.shuffle
  highlight(0, 'watSwizzle',          { fg = colors.orange,     bg = 'NONE'            })  -- i8x16.swizzle
  highlight(0, 'watBitmask',          { fg = colors.orange,     bg = 'NONE'            })  -- *.bitmask
  highlight(0, 'watNarrow',           { fg = colors.orange,     bg = 'NONE'            })  -- *.narrow*
  highlight(0, 'watWiden',            { fg = colors.orange,     bg = 'NONE'            })  -- *.widen* (deprecated, use extend)
  highlight(0, 'watDot',              { fg = colors.orange,     bg = 'NONE'            })  -- i32x4.dot_i16x8_s

  -- SIMD Lane Types
  highlight(0, 'watSimdType',         { fg = colors.turquoise,  bg = 'NONE'            })  -- i8x16, i16x8, i32x4, i64x2, f32x4, f64x2
  highlight(0, 'watI8x16',            { fg = colors.turquoise,  bg = 'NONE'            })  -- i8x16
  highlight(0, 'watI16x8',            { fg = colors.turquoise,  bg = 'NONE'            })  -- i16x8
  highlight(0, 'watI32x4',            { fg = colors.turquoise,  bg = 'NONE'            })  -- i32x4
  highlight(0, 'watI64x2',            { fg = colors.turquoise,  bg = 'NONE'            })  -- i64x2
  highlight(0, 'watF32x4',            { fg = colors.turquoise,  bg = 'NONE'            })  -- f32x4
  highlight(0, 'watF64x2',            { fg = colors.turquoise,  bg = 'NONE'            })  -- f64x2

  -- Reference Instructions
  highlight(0, 'watRefInst',          { fg = colors.orange,     bg = 'NONE'            })  -- Reference instructions
  highlight(0, 'watRefNull',          { fg = colors.blue,       bg = 'NONE'            })  -- ref.null
  highlight(0, 'watRefIsNull',        { fg = colors.orange,     bg = 'NONE'            })  -- ref.is_null
  highlight(0, 'watRefFunc',          { fg = colors.orange,     bg = 'NONE'            })  -- ref.func

  -- Atomic Instructions (Threads)
  highlight(0, 'watAtomicInst',       { fg = colors.orange,     bg = 'NONE'            })  -- Atomic instructions
  highlight(0, 'watAtomicLoad',       { fg = colors.orange,     bg = 'NONE'            })  -- *.atomic.load*
  highlight(0, 'watAtomicStore',      { fg = colors.orange,     bg = 'NONE'            })  -- *.atomic.store*
  highlight(0, 'watAtomicRmw',        { fg = colors.orange,     bg = 'NONE'            })  -- *.atomic.rmw.*
  highlight(0, 'watAtomicCmpxchg',    { fg = colors.orange,     bg = 'NONE'            })  -- *.atomic.rmw.cmpxchg
  highlight(0, 'watAtomicWait',       { fg = colors.orange,     bg = 'NONE'            })  -- memory.atomic.wait*
  highlight(0, 'watAtomicNotify',     { fg = colors.orange,     bg = 'NONE'            })  -- memory.atomic.notify
  highlight(0, 'watAtomicFence',      { fg = colors.orange,     bg = 'NONE'            })  -- atomic.fence

  -- Identifiers
  highlight(0, 'watNamedVar',         { fg = colors.purple,     bg = 'NONE'            })  -- $identifier (named variables/labels)
  highlight(0, 'watUnnamedVar',       { fg = colors.pink,       bg = 'NONE'            })  -- Numeric indices
  highlight(0, 'watIdentifier',       { fg = colors.purple,     bg = 'NONE'            })  -- General identifiers
  highlight(0, 'watLabel',            { fg = colors.purple,     bg = 'NONE'            })  -- Block/loop labels

  -- Literals
  highlight(0, 'watNumber',           { fg = colors.greenLight, bg = 'NONE'            })  -- Integer numbers
  highlight(0, 'watFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Float numbers
  highlight(0, 'watHexNumber',        { fg = colors.greenLight, bg = 'NONE'            })  -- Hexadecimal (0x...)
  highlight(0, 'watInfinity',         { fg = colors.greenLight, bg = 'NONE'            })  -- inf
  highlight(0, 'watNan',              { fg = colors.greenLight, bg = 'NONE'            })  -- nan, nan:0x...
  highlight(0, 'watString',           { fg = colors.redLight,   bg = 'NONE'            })  -- "string"
  highlight(0, 'watStringSpecial',    { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences in strings
  highlight(0, 'watEscapedUtf8',      { fg = colors.pink,       bg = 'NONE'            })  -- UTF-8 escapes

  -- Delimiters
  highlight(0, 'watListDelimiter',    { fg = colors.white,      bg = 'NONE'            })  -- ( ) parentheses
  highlight(0, 'watParens',           { fg = colors.white,      bg = 'NONE'            })  -- Parentheses
  highlight(0, 'watList',             { fg = 'NONE',            bg = 'NONE'            })  -- S-expression list

  -- Operators
  highlight(0, 'watOperator',         { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'watDollar',           { fg = colors.purple,     bg = 'NONE'            })  -- $ prefix

  -- Instruction Suffixes
  highlight(0, 'watSuffix',           { fg = colors.pink,       bg = 'NONE'            })  -- _s, _u suffixes (signed/unsigned)
  highlight(0, 'watSignedSuffix',     { fg = colors.pink,       bg = 'NONE'            })  -- _s (signed)
  highlight(0, 'watUnsignedSuffix',   { fg = colors.pink,       bg = 'NONE'            })  -- _u (unsigned)

  -- Comments
  highlight(0, 'watComment',          { fg = colors.red,        bg = 'NONE'            })  -- ;; line comments
  highlight(0, 'watBlockComment',     { fg = colors.red,        bg = 'NONE'            })  -- (; block comments ;)
  highlight(0, 'watTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Parameter Instructions (conditional/param)
  highlight(0, 'watParamInst',        { fg = colors.blue,       bg = 'NONE'            })  -- Parametric instructions

  -- Errors
  highlight(0, 'watError',            { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.wat / @xxx.wast / @xxx.wasm)

  -- Variables
  highlight(0, '@variable.wat',               { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.wast',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.wasm',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.wat',       { fg = colors.pink,      bg = 'NONE' })  -- Built-in variables
  highlight(0, '@variable.builtin.wast',      { fg = colors.pink,      bg = 'NONE' })  -- Built-in variables
  highlight(0, '@variable.parameter.wat',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@variable.parameter.wast',    { fg = colors.purple,    bg = 'NONE' })  -- Parameters

  -- Types
  highlight(0, '@type.wat',                   { fg = colors.turquoise, bg = 'NONE' })  -- Types (i32, i64, f32, f64, v128)
  highlight(0, '@type.wast',                  { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@type.wasm',                  { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@type.builtin.wat',           { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.builtin.wast',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.wat',        { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.definition.wast',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions

  -- Functions
  highlight(0, '@function.wat',               { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@function.wast',              { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@function.wasm',              { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@function.call.wat',          { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.call.wast',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.wat',       { fg = colors.orange,    bg = 'NONE' })  -- Built-in instructions
  highlight(0, '@function.builtin.wast',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in instructions

  -- Keywords
  highlight(0, '@keyword.wat',                { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@keyword.wast',               { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@keyword.wasm',               { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@keyword.function.wat',       { fg = colors.blue,      bg = 'NONE' })  -- func
  highlight(0, '@keyword.function.wast',      { fg = colors.blue,      bg = 'NONE' })  -- func
  highlight(0, '@keyword.import.wat',         { fg = colors.blue,      bg = 'NONE' })  -- import, export
  highlight(0, '@keyword.import.wast',        { fg = colors.blue,      bg = 'NONE' })  -- import, export
  highlight(0, '@keyword.return.wat',         { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.return.wast',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.repeat.wat',         { fg = colors.blue,      bg = 'NONE' })  -- loop, block
  highlight(0, '@keyword.repeat.wast',        { fg = colors.blue,      bg = 'NONE' })  -- loop, block
  highlight(0, '@keyword.conditional.wat',    { fg = colors.blue,      bg = 'NONE' })  -- if, else, br_if
  highlight(0, '@keyword.conditional.wast',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, br_if
  highlight(0, '@keyword.modifier.wat',       { fg = colors.blue,      bg = 'NONE' })  -- mut
  highlight(0, '@keyword.modifier.wast',      { fg = colors.blue,      bg = 'NONE' })  -- mut

  -- Labels
  highlight(0, '@label.wat',                  { fg = colors.purple,    bg = 'NONE' })  -- Block/loop labels
  highlight(0, '@label.wast',                 { fg = colors.purple,    bg = 'NONE' })  -- Block/loop labels

  -- Strings
  highlight(0, '@string.wat',                 { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.wast',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.wasm',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.wat',          { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.escape.wast',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.wat',                 { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.wast',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.wasm',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.float.wat',           { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, '@number.float.wast',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Constants
  highlight(0, '@constant.wat',               { fg = colors.pink,      bg = 'NONE' })  -- Constants
  highlight(0, '@constant.wast',              { fg = colors.pink,      bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.wat',       { fg = colors.pink,      bg = 'NONE' })  -- inf, nan
  highlight(0, '@constant.builtin.wast',      { fg = colors.pink,      bg = 'NONE' })  -- inf, nan

  -- Comments
  highlight(0, '@comment.wat',                { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.wast',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.wasm',               { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- Operators and Punctuation
  highlight(0, '@operator.wat',               { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@operator.wast',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.wat',    { fg = colors.white,     bg = 'NONE' })  -- ()
  highlight(0, '@punctuation.bracket.wast',   { fg = colors.white,     bg = 'NONE' })  -- ()
  highlight(0, '@punctuation.delimiter.wat',  { fg = colors.white,     bg = 'NONE' })  -- Delimiters
  highlight(0, '@punctuation.delimiter.wast', { fg = colors.white,     bg = 'NONE' })  -- Delimiters
  highlight(0, '@punctuation.special.wat',    { fg = colors.purple,    bg = 'NONE' })  -- $ prefix
  highlight(0, '@punctuation.special.wast',   { fg = colors.purple,    bg = 'NONE' })  -- $ prefix

  -- Modules
  highlight(0, '@module.wat',                 { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@module.wast',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@namespace.wat',              { fg = colors.turquoise, bg = 'NONE' })  -- Import namespaces
  highlight(0, '@namespace.wast',             { fg = colors.turquoise, bg = 'NONE' })  -- Import namespaces


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.wat / @lsp.type.xxx.wasm)

  highlight(0, '@lsp.type.type.wat',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.type.wasm',         { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.function.wat',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.function.wasm',     { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.variable.wat',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.variable.wasm',     { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.wat',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.parameter.wasm',    { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.keyword.wat',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.keyword.wasm',      { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.string.wat',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.string.wasm',       { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.wat',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.number.wasm',       { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.wat',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.comment.wasm',      { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.namespace.wat',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.namespace.wasm',    { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.function.declaration.wat',  { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.declaration.wasm', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.variable.readonly.wat',     { fg = colors.pink,      bg = 'NONE' })  -- Immutable globals
  highlight(0, '@lsp.typemod.variable.readonly.wasm',    { fg = colors.pink,      bg = 'NONE' })  -- Immutable globals
end

return wasm


