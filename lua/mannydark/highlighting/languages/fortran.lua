-------------------------------------------------------------------------------
-- Fortran Files
-- Highlighting for .f, .f90, .f95, .f03, .f08, .for, .fpp files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local fortran = {}


-------------------------------------------------------------------------------
-- Settings

fortran.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Program Units
  highlight(0, 'fortranProgram',        { fg = colors.blue,       bg = 'NONE'            })  -- PROGRAM
  highlight(0, 'fortranModule',         { fg = colors.blue,       bg = 'NONE'            })  -- MODULE, SUBMODULE
  highlight(0, 'fortranSubroutine',     { fg = colors.blue,       bg = 'NONE'            })  -- SUBROUTINE
  highlight(0, 'fortranFunction',       { fg = colors.blue,       bg = 'NONE'            })  -- FUNCTION
  highlight(0, 'fortranBlockData',      { fg = colors.blue,       bg = 'NONE'            })  -- BLOCK DATA
  highlight(0, 'fortranEnd',            { fg = colors.blue,       bg = 'NONE'            })  -- END, ENDPROGRAM, ENDMODULE, etc.

  -- Keywords
  highlight(0, 'fortranKeyword',        { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'fortranStatement',      { fg = colors.blue,       bg = 'NONE'            })  -- RETURN, STOP, EXIT, CYCLE
  highlight(0, 'fortranConditional',    { fg = colors.blue,       bg = 'NONE'            })  -- IF, THEN, ELSE, ELSEIF, ENDIF, SELECT, CASE
  highlight(0, 'fortranRepeat',         { fg = colors.blue,       bg = 'NONE'            })  -- DO, WHILE, ENDDO, FORALL, WHERE
  highlight(0, 'fortranCall',           { fg = colors.blue,       bg = 'NONE'            })  -- CALL
  highlight(0, 'fortranInclude',        { fg = colors.blue,       bg = 'NONE'            })  -- USE, INCLUDE
  highlight(0, 'fortranOnly',           { fg = colors.blue,       bg = 'NONE'            })  -- ONLY
  highlight(0, 'fortranImplicit',       { fg = colors.blue,       bg = 'NONE'            })  -- IMPLICIT NONE
  highlight(0, 'fortranContains',       { fg = colors.blue,       bg = 'NONE'            })  -- CONTAINS
  highlight(0, 'fortranInterface',      { fg = colors.blue,       bg = 'NONE'            })  -- INTERFACE, END INTERFACE
  highlight(0, 'fortranProcedure',      { fg = colors.blue,       bg = 'NONE'            })  -- PROCEDURE
  highlight(0, 'fortranAssociate',      { fg = colors.blue,       bg = 'NONE'            })  -- ASSOCIATE, END ASSOCIATE
  highlight(0, 'fortranBlock',          { fg = colors.blue,       bg = 'NONE'            })  -- BLOCK, END BLOCK

  -- Storage and Attributes
  highlight(0, 'fortranStorageClass',   { fg = colors.blue,       bg = 'NONE'            })  -- SAVE, DATA, PARAMETER, EQUIVALENCE
  highlight(0, 'fortranAttribute',      { fg = colors.blue,       bg = 'NONE'            })  -- INTENT, OPTIONAL, POINTER, TARGET, ALLOCATABLE
  highlight(0, 'fortranIntent',         { fg = colors.blue,       bg = 'NONE'            })  -- INTENT(IN), INTENT(OUT), INTENT(INOUT)
  highlight(0, 'fortranPublic',         { fg = colors.blue,       bg = 'NONE'            })  -- PUBLIC, PRIVATE
  highlight(0, 'fortranExternal',       { fg = colors.blue,       bg = 'NONE'            })  -- EXTERNAL, INTRINSIC
  highlight(0, 'fortranCommon',         { fg = colors.blue,       bg = 'NONE'            })  -- COMMON
  highlight(0, 'fortranNamelist',       { fg = colors.blue,       bg = 'NONE'            })  -- NAMELIST
  highlight(0, 'fortranSequence',       { fg = colors.blue,       bg = 'NONE'            })  -- SEQUENCE

  -- Memory Management
  highlight(0, 'fortranAllocate',       { fg = colors.blue,       bg = 'NONE'            })  -- ALLOCATE, DEALLOCATE
  highlight(0, 'fortranNullify',        { fg = colors.blue,       bg = 'NONE'            })  -- NULLIFY

  -- Types
  highlight(0, 'fortranType',           { fg = colors.turquoise,  bg = 'NONE'            })  -- INTEGER, REAL, DOUBLE PRECISION, COMPLEX, CHARACTER, LOGICAL
  highlight(0, 'fortranTypeR',          { fg = colors.turquoise,  bg = 'NONE'            })  -- REAL types
  highlight(0, 'fortranTypeOb',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Obsolete types
  highlight(0, 'fortranStructure',      { fg = colors.turquoise,  bg = 'NONE'            })  -- TYPE, CLASS, END TYPE
  highlight(0, 'fortranDerived',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Derived type names
  highlight(0, 'fortranKind',           { fg = colors.turquoise,  bg = 'NONE'            })  -- KIND specifiers
  highlight(0, 'fortranDimension',      { fg = colors.blue,       bg = 'NONE'            })  -- DIMENSION

  -- Functions and Procedures
  highlight(0, 'fortranIntrinsic',      { fg = colors.orange,     bg = 'NONE'            })  -- Intrinsic functions (SIN, COS, ABS, etc.)
  highlight(0, 'fortranIntrinsicR',     { fg = colors.orange,     bg = 'NONE'            })  -- Real intrinsics
  highlight(0, 'fortranIntrinsicVec',   { fg = colors.orange,     bg = 'NONE'            })  -- Vector intrinsics
  highlight(0, 'fortranUnitHeader',     { fg = colors.orange,     bg = 'NONE'            })  -- Function/Subroutine names
  highlight(0, 'fortranCall',           { fg = colors.orange,     bg = 'NONE'            })  -- CALL statement

  -- Variables
  highlight(0, 'fortranIdentifier',     { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'fortranTarget',         { fg = colors.purple,     bg = 'NONE'            })  -- TARGET variables

  -- I/O
  highlight(0, 'fortranIO',             { fg = colors.blue,       bg = 'NONE'            })  -- READ, WRITE, PRINT, OPEN, CLOSE, INQUIRE
  highlight(0, 'fortranReadWrite',      { fg = colors.blue,       bg = 'NONE'            })  -- READ, WRITE
  highlight(0, 'fortranIOStatement',    { fg = colors.blue,       bg = 'NONE'            })  -- I/O statements
  highlight(0, 'fortranFormat',         { fg = colors.pink,       bg = 'NONE'            })  -- FORMAT specifiers
  highlight(0, 'fortranFormatSpec',     { fg = colors.pink,       bg = 'NONE'            })  -- Format specifications (I, F, E, A, X, etc.)

  -- Strings
  highlight(0, 'fortranString',         { fg = colors.redLight,   bg = 'NONE'            })  -- "strings" and 'strings'
  highlight(0, 'fortranStringDelim',    { fg = colors.redLight,   bg = 'NONE'            })  -- String delimiters
  highlight(0, 'fortranCharacter',      { fg = colors.redLight,   bg = 'NONE'            })  -- CHARACTER literals

  -- Numbers
  highlight(0, 'fortranNumber',         { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'fortranFloat',          { fg = colors.greenLight, bg = 'NONE'            })  -- Floating point
  highlight(0, 'fortranFloatExp',       { fg = colors.greenLight, bg = 'NONE'            })  -- Exponential notation
  highlight(0, 'fortranFloatIll',       { fg = colors.greenLight, bg = 'NONE'            })  -- Illegal floats
  highlight(0, 'fortranBinary',         { fg = colors.greenLight, bg = 'NONE'            })  -- Binary constants (B'...')
  highlight(0, 'fortranOctal',          { fg = colors.greenLight, bg = 'NONE'            })  -- Octal constants (O'...')
  highlight(0, 'fortranHex',            { fg = colors.greenLight, bg = 'NONE'            })  -- Hex constants (Z'...')

  -- Booleans
  highlight(0, 'fortranBoolean',        { fg = colors.blue,       bg = 'NONE'            })  -- .TRUE., .FALSE.

  -- Operators
  highlight(0, 'fortranOperator',       { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'fortranLogical',        { fg = colors.blue,       bg = 'NONE'            })  -- .AND., .OR., .NOT., .EQV., .NEQV.
  highlight(0, 'fortranRelational',     { fg = colors.white,      bg = 'NONE'            })  -- .EQ., .NE., .LT., .LE., .GT., .GE.
  highlight(0, 'fortranArithmetic',     { fg = colors.white,      bg = 'NONE'            })  -- +, -, *, /, **
  highlight(0, 'fortranConcat',         { fg = colors.white,      bg = 'NONE'            })  -- // concatenation

  -- Comments
  highlight(0, 'fortranComment',        { fg = colors.red,        bg = 'NONE'            })  -- ! comments
  highlight(0, 'fortranCommentLine',    { fg = colors.red,        bg = 'NONE'            })  -- Full line comments
  highlight(0, 'fortranCommentFixed',   { fg = colors.red,        bg = 'NONE'            })  -- C or * in column 1 (fixed-form)
  highlight(0, 'fortranTodo',           { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Preprocessor
  highlight(0, 'fortranPreProc',        { fg = colors.pink,       bg = 'NONE'            })  -- Preprocessor directives
  highlight(0, 'fortranInclude',        { fg = colors.pink,       bg = 'NONE'            })  -- #include
  highlight(0, 'fortranCppIf',          { fg = colors.pink,       bg = 'NONE'            })  -- #if, #ifdef, #endif
  highlight(0, 'fortranDefine',         { fg = colors.pink,       bg = 'NONE'            })  -- #define

  -- Labels
  highlight(0, 'fortranLabelNumber',    { fg = colors.gray,       bg = 'NONE'            })  -- Line labels/numbers
  highlight(0, 'fortranTarget',         { fg = colors.gray,       bg = 'NONE'            })  -- GOTO targets

  -- OpenMP/OpenACC
  highlight(0, 'fortranOpenMP',         { fg = colors.green,      bg = 'NONE'            })  -- !$OMP directives
  highlight(0, 'fortranOpenACC',        { fg = colors.green,      bg = 'NONE'            })  -- !$ACC directives
  highlight(0, 'fortranParallel',       { fg = colors.green,      bg = 'NONE'            })  -- Parallel directives

  -- Errors
  highlight(0, 'fortranError',          { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Syntax errors
  highlight(0, 'fortranObsolete',       { fg = colors.gray,       bg = 'NONE'            })  -- Obsolete features


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.fortran)

  -- Variables
  highlight(0, '@variable.fortran',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.fortran',      { fg = colors.blue,      bg = 'NONE' })  -- Built-in variables
  highlight(0, '@variable.parameter.fortran',    { fg = colors.purple,    bg = 'NONE' })  -- Subroutine/function parameters
  highlight(0, '@variable.member.fortran',       { fg = colors.purple,    bg = 'NONE' })  -- Derived type components

  -- Constants
  highlight(0, '@constant.fortran',              { fg = colors.purple,    bg = 'NONE' })  -- Constants (PARAMETER)
  highlight(0, '@constant.builtin.fortran',      { fg = colors.blue,      bg = 'NONE' })  -- .TRUE., .FALSE.

  -- Functions
  highlight(0, '@function.fortran',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.fortran',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.fortran',      { fg = colors.orange,    bg = 'NONE' })  -- Intrinsic functions
  highlight(0, '@function.method.fortran',       { fg = colors.orange,    bg = 'NONE' })  -- Type-bound procedures
  highlight(0, '@constructor.fortran',           { fg = colors.turquoise, bg = 'NONE' })  -- Derived type constructors

  -- Types
  highlight(0, '@type.fortran',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.fortran',          { fg = colors.turquoise, bg = 'NONE' })  -- INTEGER, REAL, etc.
  highlight(0, '@type.definition.fortran',       { fg = colors.turquoise, bg = 'NONE' })  -- TYPE definitions

  -- Keywords
  highlight(0, '@keyword.fortran',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.fortran',      { fg = colors.blue,      bg = 'NONE' })  -- FUNCTION, SUBROUTINE
  highlight(0, '@keyword.type.fortran',          { fg = colors.blue,      bg = 'NONE' })  -- TYPE, CLASS
  highlight(0, '@keyword.modifier.fortran',      { fg = colors.blue,      bg = 'NONE' })  -- PUBLIC, PRIVATE, INTENT
  highlight(0, '@keyword.return.fortran',        { fg = colors.blue,      bg = 'NONE' })  -- RETURN
  highlight(0, '@keyword.import.fortran',        { fg = colors.blue,      bg = 'NONE' })  -- USE, INCLUDE
  highlight(0, '@keyword.repeat.fortran',        { fg = colors.blue,      bg = 'NONE' })  -- DO, WHILE, FORALL
  highlight(0, '@keyword.conditional.fortran',   { fg = colors.blue,      bg = 'NONE' })  -- IF, THEN, ELSE, SELECT, CASE
  highlight(0, '@keyword.exception.fortran',     { fg = colors.blue,      bg = 'NONE' })  -- STOP, ERROR STOP
  highlight(0, '@keyword.operator.fortran',      { fg = colors.blue,      bg = 'NONE' })  -- .AND., .OR., .NOT.

  -- Strings
  highlight(0, '@string.fortran',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.fortran',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@character.fortran',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.fortran',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.fortran',          { fg = colors.greenLight, bg = 'NONE' })  -- Reals/Floats

  -- Booleans
  highlight(0, '@boolean.fortran',               { fg = colors.blue,      bg = 'NONE' })  -- .TRUE., .FALSE.

  -- Comments
  highlight(0, '@comment.fortran',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.fortran', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Modules
  highlight(0, '@module.fortran',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@label.fortran',                 { fg = colors.gray,      bg = 'NONE' })  -- Line labels
  highlight(0, '@property.fortran',              { fg = colors.purple,    bg = 'NONE' })  -- Derived type components

  -- Operators and Punctuation
  highlight(0, '@operator.fortran',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.fortran',   { fg = colors.white,     bg = 'NONE' })  -- (), []
  highlight(0, '@punctuation.delimiter.fortran', { fg = colors.white,     bg = 'NONE' })  -- , ; :

  -- Preprocessor
  highlight(0, '@preproc.fortran',               { fg = colors.pink,      bg = 'NONE' })  -- Preprocessor directives
  highlight(0, '@define.fortran',                { fg = colors.pink,      bg = 'NONE' })  -- #define


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.fortran)

  highlight(0, '@lsp.type.variable.fortran',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.fortran',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.fortran',      { fg = colors.purple,    bg = 'NONE' })  -- Derived type components
  highlight(0, '@lsp.type.function.fortran',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.fortran',        { fg = colors.orange,    bg = 'NONE' })  -- Type-bound procedures
  highlight(0, '@lsp.type.type.fortran',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.fortran',         { fg = colors.turquoise, bg = 'NONE' })  -- Derived types
  highlight(0, '@lsp.type.namespace.fortran',     { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.keyword.fortran',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.fortran',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.fortran',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.fortran',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.fortran',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.fortran',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.fortran',   { fg = colors.purple,    bg = 'NONE' })  -- PARAMETER constants
  highlight(0, '@lsp.typemod.function.declaration.fortran', { fg = colors.orange,   bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.type.declaration.fortran',    { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.fortran', { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return fortran
