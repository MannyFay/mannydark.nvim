-------------------------------------------------------------------------------
-- SAS Language Files
-- Highlighting for .sas files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local sas       = {}


-------------------------------------------------------------------------------
-- Settings

sas.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Step Keywords
  highlight(0, 'sasStep',               { fg = colors.blue,       bg = 'NONE', bold = true })  -- DATA, PROC, RUN, QUIT
  highlight(0, 'sasDataStep',           { fg = colors.blue,       bg = 'NONE', bold = true })  -- DATA
  highlight(0, 'sasProcStep',           { fg = colors.blue,       bg = 'NONE', bold = true })  -- PROC
  highlight(0, 'sasRun',                { fg = colors.blue,       bg = 'NONE', bold = true })  -- RUN
  highlight(0, 'sasQuit',               { fg = colors.blue,       bg = 'NONE', bold = true })  -- QUIT
  highlight(0, 'sasEndsas',             { fg = colors.blue,       bg = 'NONE', bold = true })  -- ENDSAS

  -- Control Flow
  highlight(0, 'sasConditional',        { fg = colors.blue,       bg = 'NONE' })  -- IF, THEN, ELSE
  highlight(0, 'sasRepeat',             { fg = colors.blue,       bg = 'NONE' })  -- DO, END, WHILE, UNTIL, TO, BY
  highlight(0, 'sasSelect',             { fg = colors.blue,       bg = 'NONE' })  -- SELECT, WHEN, OTHERWISE
  highlight(0, 'sasGoto',               { fg = colors.blue,       bg = 'NONE' })  -- GO, GOTO, LINK, RETURN
  highlight(0, 'sasLeave',              { fg = colors.blue,       bg = 'NONE' })  -- LEAVE, CONTINUE
  highlight(0, 'sasStop',               { fg = colors.blue,       bg = 'NONE' })  -- STOP, ABORT, ERROR

  -- Data Step Statements
  highlight(0, 'sasStatement',          { fg = colors.blue,       bg = 'NONE' })  -- General statements
  highlight(0, 'sasSet',                { fg = colors.blue,       bg = 'NONE' })  -- SET
  highlight(0, 'sasMerge',              { fg = colors.blue,       bg = 'NONE' })  -- MERGE
  highlight(0, 'sasUpdate',             { fg = colors.blue,       bg = 'NONE' })  -- UPDATE, MODIFY
  highlight(0, 'sasBy',                 { fg = colors.blue,       bg = 'NONE' })  -- BY
  highlight(0, 'sasInput',              { fg = colors.blue,       bg = 'NONE' })  -- INPUT
  highlight(0, 'sasOutput',             { fg = colors.blue,       bg = 'NONE' })  -- OUTPUT
  highlight(0, 'sasDatalines',          { fg = colors.blue,       bg = 'NONE' })  -- DATALINES, CARDS, LINES
  highlight(0, 'sasDrop',               { fg = colors.blue,       bg = 'NONE' })  -- DROP
  highlight(0, 'sasKeep',               { fg = colors.blue,       bg = 'NONE' })  -- KEEP
  highlight(0, 'sasRename',             { fg = colors.blue,       bg = 'NONE' })  -- RENAME
  highlight(0, 'sasRetain',             { fg = colors.blue,       bg = 'NONE' })  -- RETAIN
  highlight(0, 'sasWhere',              { fg = colors.blue,       bg = 'NONE' })  -- WHERE
  highlight(0, 'sasArray',              { fg = colors.blue,       bg = 'NONE' })  -- ARRAY
  highlight(0, 'sasLength',             { fg = colors.blue,       bg = 'NONE' })  -- LENGTH
  highlight(0, 'sasFormat',             { fg = colors.blue,       bg = 'NONE' })  -- FORMAT
  highlight(0, 'sasInformat',           { fg = colors.blue,       bg = 'NONE' })  -- INFORMAT
  highlight(0, 'sasLabel',              { fg = colors.blue,       bg = 'NONE' })  -- LABEL
  highlight(0, 'sasAttrib',             { fg = colors.blue,       bg = 'NONE' })  -- ATTRIB
  highlight(0, 'sasFile',               { fg = colors.blue,       bg = 'NONE' })  -- FILE, INFILE
  highlight(0, 'sasPut',                { fg = colors.blue,       bg = 'NONE' })  -- PUT, PUTLOG
  highlight(0, 'sasDelete',             { fg = colors.blue,       bg = 'NONE' })  -- DELETE, REMOVE
  highlight(0, 'sasReplace',            { fg = colors.blue,       bg = 'NONE' })  -- REPLACE

  -- Global Statements
  highlight(0, 'sasGlobal',             { fg = colors.blue,       bg = 'NONE' })  -- Global statements
  highlight(0, 'sasLibname',            { fg = colors.blue,       bg = 'NONE' })  -- LIBNAME
  highlight(0, 'sasFilename',           { fg = colors.blue,       bg = 'NONE' })  -- FILENAME
  highlight(0, 'sasOptions',            { fg = colors.blue,       bg = 'NONE' })  -- OPTIONS
  highlight(0, 'sasTitle',              { fg = colors.blue,       bg = 'NONE' })  -- TITLE, TITLE1-TITLE10
  highlight(0, 'sasFootnote',           { fg = colors.blue,       bg = 'NONE' })  -- FOOTNOTE, FOOTNOTE1-FOOTNOTE10
  highlight(0, 'sasODS',                { fg = colors.blue,       bg = 'NONE' })  -- ODS
  highlight(0, 'sasPage',               { fg = colors.blue,       bg = 'NONE' })  -- PAGE, SKIP
  highlight(0, 'sasLock',               { fg = colors.blue,       bg = 'NONE' })  -- LOCK
  highlight(0, 'sasDM',                 { fg = colors.blue,       bg = 'NONE' })  -- DM
  highlight(0, 'sasCatname',            { fg = colors.blue,       bg = 'NONE' })  -- CATNAME

  -- Procedures (PROC names)
  highlight(0, 'sasProcName',           { fg = colors.turquoise,  bg = 'NONE' })  -- Procedure names

  -- Common Procedures
  highlight(0, 'sasProcPrint',          { fg = colors.turquoise,  bg = 'NONE' })  -- PRINT
  highlight(0, 'sasProcMeans',          { fg = colors.turquoise,  bg = 'NONE' })  -- MEANS, SUMMARY
  highlight(0, 'sasProcFreq',           { fg = colors.turquoise,  bg = 'NONE' })  -- FREQ
  highlight(0, 'sasProcSort',           { fg = colors.turquoise,  bg = 'NONE' })  -- SORT
  highlight(0, 'sasProcSQL',            { fg = colors.turquoise,  bg = 'NONE' })  -- SQL
  highlight(0, 'sasProcFormat',         { fg = colors.turquoise,  bg = 'NONE' })  -- FORMAT
  highlight(0, 'sasProcContents',       { fg = colors.turquoise,  bg = 'NONE' })  -- CONTENTS
  highlight(0, 'sasProcDatasets',       { fg = colors.turquoise,  bg = 'NONE' })  -- DATASETS
  highlight(0, 'sasProcTranspose',      { fg = colors.turquoise,  bg = 'NONE' })  -- TRANSPOSE
  highlight(0, 'sasProcImport',         { fg = colors.turquoise,  bg = 'NONE' })  -- IMPORT, EXPORT
  highlight(0, 'sasProcReport',         { fg = colors.turquoise,  bg = 'NONE' })  -- REPORT
  highlight(0, 'sasProcTabulate',       { fg = colors.turquoise,  bg = 'NONE' })  -- TABULATE

  -- Statistical Procedures
  highlight(0, 'sasProcStat',           { fg = colors.turquoise,  bg = 'NONE' })  -- Statistical procedures
  highlight(0, 'sasProcReg',            { fg = colors.turquoise,  bg = 'NONE' })  -- REG
  highlight(0, 'sasProcLogistic',       { fg = colors.turquoise,  bg = 'NONE' })  -- LOGISTIC
  highlight(0, 'sasProcGLM',            { fg = colors.turquoise,  bg = 'NONE' })  -- GLM, ANOVA
  highlight(0, 'sasProcMixed',          { fg = colors.turquoise,  bg = 'NONE' })  -- MIXED
  highlight(0, 'sasProcTtest',          { fg = colors.turquoise,  bg = 'NONE' })  -- TTEST
  highlight(0, 'sasProcCorr',           { fg = colors.turquoise,  bg = 'NONE' })  -- CORR
  highlight(0, 'sasProcFactor',         { fg = colors.turquoise,  bg = 'NONE' })  -- FACTOR
  highlight(0, 'sasProcCluster',        { fg = colors.turquoise,  bg = 'NONE' })  -- CLUSTER
  highlight(0, 'sasProcSurvey',         { fg = colors.turquoise,  bg = 'NONE' })  -- SURVEYFREQ, SURVEYMEANS, SURVEYREG

  -- Graphics Procedures
  highlight(0, 'sasProcGraph',          { fg = colors.turquoise,  bg = 'NONE' })  -- Graphics procedures
  highlight(0, 'sasProcSGplot',         { fg = colors.turquoise,  bg = 'NONE' })  -- SGPLOT, SGPANEL, SGSCATTER
  highlight(0, 'sasProcGplot',          { fg = colors.turquoise,  bg = 'NONE' })  -- GPLOT, GCHART
  highlight(0, 'sasProcTemplate',       { fg = colors.turquoise,  bg = 'NONE' })  -- TEMPLATE

  -- Macro Language
  highlight(0, 'sasMacro',              { fg = colors.pink,       bg = 'NONE' })  -- %MACRO, %MEND
  highlight(0, 'sasMacroKeyword',       { fg = colors.pink,       bg = 'NONE' })  -- Macro keywords
  highlight(0, 'sasMacroVar',           { fg = colors.pink,       bg = 'NONE' })  -- &macrovar
  highlight(0, 'sasMacroVarRef',        { fg = colors.pink,       bg = 'NONE' })  -- &var references
  highlight(0, 'sasMacroFunc',          { fg = colors.pink,       bg = 'NONE' })  -- %function()
  highlight(0, 'sasMacroLet',           { fg = colors.pink,       bg = 'NONE' })  -- %LET
  highlight(0, 'sasMacroIf',            { fg = colors.pink,       bg = 'NONE' })  -- %IF, %THEN, %ELSE
  highlight(0, 'sasMacroDo',            { fg = colors.pink,       bg = 'NONE' })  -- %DO, %END, %TO, %BY, %WHILE, %UNTIL
  highlight(0, 'sasMacroPut',           { fg = colors.pink,       bg = 'NONE' })  -- %PUT
  highlight(0, 'sasMacroInclude',       { fg = colors.pink,       bg = 'NONE' })  -- %INCLUDE
  highlight(0, 'sasMacroGlobal',        { fg = colors.pink,       bg = 'NONE' })  -- %GLOBAL, %LOCAL
  highlight(0, 'sasMacroSysfunc',       { fg = colors.pink,       bg = 'NONE' })  -- %SYSFUNC
  highlight(0, 'sasMacroEval',          { fg = colors.pink,       bg = 'NONE' })  -- %EVAL, %SYSEVALF
  highlight(0, 'sasMacroStr',           { fg = colors.pink,       bg = 'NONE' })  -- %STR, %NRSTR, %BQUOTE, %NRBQUOTE
  highlight(0, 'sasMacroScan',          { fg = colors.pink,       bg = 'NONE' })  -- %SCAN, %SUBSTR, %INDEX
  highlight(0, 'sasMacroUpcase',        { fg = colors.pink,       bg = 'NONE' })  -- %UPCASE, %LOWCASE
  highlight(0, 'sasMacroLength',        { fg = colors.pink,       bg = 'NONE' })  -- %LENGTH
  highlight(0, 'sasMacroSymexist',      { fg = colors.pink,       bg = 'NONE' })  -- %SYMEXIST, %SYMGLOBL, %SYMLOCAL
  highlight(0, 'sasMacroSysdel',        { fg = colors.pink,       bg = 'NONE' })  -- %SYMDEL

  -- Functions
  highlight(0, 'sasFunction',           { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, 'sasFuncCall',           { fg = colors.orange,     bg = 'NONE' })  -- CALL routines

  -- Math Functions
  highlight(0, 'sasFuncMath',           { fg = colors.orange,     bg = 'NONE' })  -- ABS, SQRT, EXP, LOG, etc.

  -- String Functions
  highlight(0, 'sasFuncString',         { fg = colors.orange,     bg = 'NONE' })  -- SUBSTR, TRIM, STRIP, COMPRESS, etc.

  -- Date/Time Functions
  highlight(0, 'sasFuncDateTime',       { fg = colors.orange,     bg = 'NONE' })  -- TODAY, DATE, TIME, MDY, etc.

  -- Statistical Functions
  highlight(0, 'sasFuncStat',           { fg = colors.orange,     bg = 'NONE' })  -- MEAN, SUM, MIN, MAX, etc.

  -- Special Functions
  highlight(0, 'sasFuncSpecial',        { fg = colors.orange,     bg = 'NONE' })  -- LAG, DIF, INPUT, PUT, etc.

  -- Hash Object
  highlight(0, 'sasHash',               { fg = colors.turquoise,  bg = 'NONE' })  -- HASH, HITER
  highlight(0, 'sasHashMethod',         { fg = colors.orange,     bg = 'NONE' })  -- Hash methods
  highlight(0, 'sasHashAttr',           { fg = colors.purple,     bg = 'NONE' })  -- Hash attributes

  -- Declare/DCL
  highlight(0, 'sasDeclare',            { fg = colors.blue,       bg = 'NONE' })  -- DECLARE, DCL
  highlight(0, 'sasJavaobj',            { fg = colors.turquoise,  bg = 'NONE' })  -- JAVAOBJ

  -- Operators
  highlight(0, 'sasOperator',           { fg = colors.white,      bg = 'NONE' })  -- General operators
  highlight(0, 'sasArithOp',            { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, **
  highlight(0, 'sasCompareOp',          { fg = colors.white,      bg = 'NONE' })  -- =, ^=, ~=, <, >, <=, >=
  highlight(0, 'sasCompareWord',        { fg = colors.blue,       bg = 'NONE' })  -- EQ, NE, LT, GT, LE, GE
  highlight(0, 'sasLogicalOp',          { fg = colors.white,      bg = 'NONE' })  -- &, |, ^, ~
  highlight(0, 'sasLogicalWord',        { fg = colors.blue,       bg = 'NONE' })  -- AND, OR, NOT, IN
  highlight(0, 'sasConcatOp',           { fg = colors.white,      bg = 'NONE' })  -- || (concatenation)
  highlight(0, 'sasRangeOp',            { fg = colors.blue,       bg = 'NONE' })  -- : (range in WHERE)
  highlight(0, 'sasOfOp',               { fg = colors.blue,       bg = 'NONE' })  -- OF (in array functions)

  -- Variables / Parameters
  highlight(0, 'sasVariable',           { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, 'sasParameter',          { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, 'sasDatasetOption',      { fg = colors.purple,     bg = 'NONE' })  -- Dataset options

  -- Special Variables
  highlight(0, 'sasAutoVar',            { fg = colors.blue,       bg = 'NONE' })  -- _N_, _ERROR_, FIRST., LAST.
  highlight(0, 'sasReserved',           { fg = colors.blue,       bg = 'NONE' })  -- _NULL_, _DATA_, _ALL_, etc.

  -- Formats / Informats
  highlight(0, 'sasFormatName',         { fg = colors.turquoise,  bg = 'NONE' })  -- Format names
  highlight(0, 'sasFormatChar',         { fg = colors.turquoise,  bg = 'NONE' })  -- $CHAR., $w., etc.
  highlight(0, 'sasFormatNum',          { fg = colors.turquoise,  bg = 'NONE' })  -- BEST., COMMA., DOLLAR., etc.
  highlight(0, 'sasFormatDate',         { fg = colors.turquoise,  bg = 'NONE' })  -- DATE9., DATETIME., MMDDYY., etc.

  -- Strings
  highlight(0, 'sasString',             { fg = colors.redLight,   bg = 'NONE' })  -- 'string' or "string"
  highlight(0, 'sasStringDelim',        { fg = colors.redLight,   bg = 'NONE' })  -- String delimiters
  highlight(0, 'sasEscapeSeq',          { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences

  -- Date/Time Literals
  highlight(0, 'sasDateLiteral',        { fg = colors.greenLight, bg = 'NONE' })  -- '01JAN2020'd
  highlight(0, 'sasTimeLiteral',        { fg = colors.greenLight, bg = 'NONE' })  -- '12:30:00't
  highlight(0, 'sasDatetimeLiteral',    { fg = colors.greenLight, bg = 'NONE' })  -- '01JAN2020:12:30:00'dt

  -- Numbers
  highlight(0, 'sasNumber',             { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, 'sasFloat',              { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'sasSciNotation',        { fg = colors.greenLight, bg = 'NONE' })  -- Scientific notation
  highlight(0, 'sasHex',                { fg = colors.greenLight, bg = 'NONE' })  -- Hex constants

  -- Missing Values
  highlight(0, 'sasMissing',            { fg = colors.blue,       bg = 'NONE' })  -- . (numeric missing)
  highlight(0, 'sasMissingSpecial',     { fg = colors.blue,       bg = 'NONE' })  -- .A-.Z, ._

  -- Comments
  highlight(0, 'sasComment',            { fg = colors.red,        bg = 'NONE' })  -- /* comment */
  highlight(0, 'sasLineComment',        { fg = colors.red,        bg = 'NONE' })  -- * comment;
  highlight(0, 'sasMacroComment',       { fg = colors.red,        bg = 'NONE' })  -- %* comment;
  highlight(0, 'sasBlockComment',       { fg = colors.red,        bg = 'NONE' })  -- /* block */
  highlight(0, 'sasTodo',               { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, TBD

  -- Delimiters / Punctuation
  highlight(0, 'sasDelimiter',          { fg = colors.white,      bg = 'NONE' })  -- General delimiters
  highlight(0, 'sasSemicolon',          { fg = colors.white,      bg = 'NONE' })  -- ;
  highlight(0, 'sasParen',              { fg = colors.white,      bg = 'NONE' })  -- ()
  highlight(0, 'sasComma',              { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'sasDot',                { fg = colors.white,      bg = 'NONE' })  -- .
  highlight(0, 'sasColon',              { fg = colors.white,      bg = 'NONE' })  -- :

  -- ODS Destinations
  highlight(0, 'sasODSDest',            { fg = colors.turquoise,  bg = 'NONE' })  -- HTML, PDF, RTF, EXCEL, etc.
  highlight(0, 'sasODSOption',          { fg = colors.purple,     bg = 'NONE' })  -- ODS options

  -- Errors
  highlight(0, 'sasError',              { fg = colors.red,        bg = 'NONE' })  -- Syntax errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.sas) - If available

  -- Variables
  highlight(0, '@variable.sas',              { fg = colors.white,     bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.sas',      { fg = colors.blue,      bg = 'NONE' })  -- _N_, _ERROR_, etc.
  highlight(0, '@variable.parameter.sas',    { fg = colors.purple,    bg = 'NONE' })  -- Parameters

  -- Constants
  highlight(0, '@constant.sas',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.sas',      { fg = colors.blue,      bg = 'NONE' })  -- _NULL_, _DATA_, etc.

  -- Functions
  highlight(0, '@function.sas',              { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@function.call.sas',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.sas',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.macro.sas',        { fg = colors.pink,      bg = 'NONE' })  -- Macro functions

  -- Types
  highlight(0, '@type.sas',                  { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@type.builtin.sas',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types

  -- Keywords
  highlight(0, '@keyword.sas',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.sas',      { fg = colors.blue,      bg = 'NONE' })  -- DATA, PROC
  highlight(0, '@keyword.return.sas',        { fg = colors.blue,      bg = 'NONE' })  -- RETURN
  highlight(0, '@keyword.conditional.sas',   { fg = colors.blue,      bg = 'NONE' })  -- IF, THEN, ELSE
  highlight(0, '@keyword.repeat.sas',        { fg = colors.blue,      bg = 'NONE' })  -- DO, END, WHILE, UNTIL
  highlight(0, '@keyword.operator.sas',      { fg = colors.blue,      bg = 'NONE' })  -- AND, OR, NOT, IN, EQ, etc.

  -- Strings
  highlight(0, '@string.sas',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.sas',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.sas',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.float.sas',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Comments
  highlight(0, '@comment.sas',               { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- Operators
  highlight(0, '@operator.sas',              { fg = colors.white,     bg = 'NONE' })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.sas',   { fg = colors.white,     bg = 'NONE' })  -- ()
  highlight(0, '@punctuation.delimiter.sas', { fg = colors.white,     bg = 'NONE' })  -- ; ,
  highlight(0, '@punctuation.special.sas',   { fg = colors.pink,      bg = 'NONE' })  -- & %


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.sas)

  highlight(0, '@lsp.type.variable.sas',      { fg = colors.white,     bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.sas',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.function.sas',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.macro.sas',         { fg = colors.pink,      bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.keyword.sas',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.sas',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.comment.sas',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.string.sas',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.sas',        { fg = colors.greenLight, bg = 'NONE' })  -- Numbers


  -----------------------------------------------------------------------------
  -- Data Step Functions (Custom highlight groups)

  -- Mathematical Functions
  highlight(0, 'sasFuncAbs',            { fg = colors.orange,     bg = 'NONE' })  -- ABS
  highlight(0, 'sasFuncSqrt',           { fg = colors.orange,     bg = 'NONE' })  -- SQRT
  highlight(0, 'sasFuncExp',            { fg = colors.orange,     bg = 'NONE' })  -- EXP
  highlight(0, 'sasFuncLog',            { fg = colors.orange,     bg = 'NONE' })  -- LOG, LOG2, LOG10
  highlight(0, 'sasFuncRound',          { fg = colors.orange,     bg = 'NONE' })  -- ROUND, CEIL, FLOOR, INT
  highlight(0, 'sasFuncMod',            { fg = colors.orange,     bg = 'NONE' })  -- MOD
  highlight(0, 'sasFuncSign',           { fg = colors.orange,     bg = 'NONE' })  -- SIGN
  highlight(0, 'sasFuncTrig',           { fg = colors.orange,     bg = 'NONE' })  -- SIN, COS, TAN, ATAN, etc.

  -- String Functions
  highlight(0, 'sasFuncSubstr',         { fg = colors.orange,     bg = 'NONE' })  -- SUBSTR
  highlight(0, 'sasFuncTrim',           { fg = colors.orange,     bg = 'NONE' })  -- TRIM, STRIP, COMPRESS
  highlight(0, 'sasFuncUpcase',         { fg = colors.orange,     bg = 'NONE' })  -- UPCASE, LOWCASE, PROPCASE
  highlight(0, 'sasFuncIndex',          { fg = colors.orange,     bg = 'NONE' })  -- INDEX, FIND, INDEXC
  highlight(0, 'sasFuncLength',         { fg = colors.orange,     bg = 'NONE' })  -- LENGTH, LENGTHN, LENGTHC
  highlight(0, 'sasFuncScan',           { fg = colors.orange,     bg = 'NONE' })  -- SCAN
  highlight(0, 'sasFuncCat',            { fg = colors.orange,     bg = 'NONE' })  -- CAT, CATS, CATT, CATX
  highlight(0, 'sasFuncTranslate',      { fg = colors.orange,     bg = 'NONE' })  -- TRANSLATE, TRANWRD
  highlight(0, 'sasFuncReverse',        { fg = colors.orange,     bg = 'NONE' })  -- REVERSE
  highlight(0, 'sasFuncRepeat',         { fg = colors.orange,     bg = 'NONE' })  -- REPEAT
  highlight(0, 'sasFuncVerify',         { fg = colors.orange,     bg = 'NONE' })  -- VERIFY
  highlight(0, 'sasFuncQuote',          { fg = colors.orange,     bg = 'NONE' })  -- QUOTE, DEQUOTE

  -- Date/Time Functions
  highlight(0, 'sasFuncToday',          { fg = colors.orange,     bg = 'NONE' })  -- TODAY, DATE
  highlight(0, 'sasFuncTime',           { fg = colors.orange,     bg = 'NONE' })  -- TIME
  highlight(0, 'sasFuncDatetime',       { fg = colors.orange,     bg = 'NONE' })  -- DATETIME
  highlight(0, 'sasFuncMdy',            { fg = colors.orange,     bg = 'NONE' })  -- MDY
  highlight(0, 'sasFuncYmd',            { fg = colors.orange,     bg = 'NONE' })  -- YMD
  highlight(0, 'sasFuncHms',            { fg = colors.orange,     bg = 'NONE' })  -- HMS
  highlight(0, 'sasFuncDhms',           { fg = colors.orange,     bg = 'NONE' })  -- DHMS
  highlight(0, 'sasFuncYear',           { fg = colors.orange,     bg = 'NONE' })  -- YEAR
  highlight(0, 'sasFuncMonth',          { fg = colors.orange,     bg = 'NONE' })  -- MONTH
  highlight(0, 'sasFuncDay',            { fg = colors.orange,     bg = 'NONE' })  -- DAY
  highlight(0, 'sasFuncWeekday',        { fg = colors.orange,     bg = 'NONE' })  -- WEEKDAY
  highlight(0, 'sasFuncQtr',            { fg = colors.orange,     bg = 'NONE' })  -- QTR
  highlight(0, 'sasFuncWeek',           { fg = colors.orange,     bg = 'NONE' })  -- WEEK
  highlight(0, 'sasFuncHour',           { fg = colors.orange,     bg = 'NONE' })  -- HOUR
  highlight(0, 'sasFuncMinute',         { fg = colors.orange,     bg = 'NONE' })  -- MINUTE
  highlight(0, 'sasFuncSecond',         { fg = colors.orange,     bg = 'NONE' })  -- SECOND
  highlight(0, 'sasFuncDatepart',       { fg = colors.orange,     bg = 'NONE' })  -- DATEPART
  highlight(0, 'sasFuncTimepart',       { fg = colors.orange,     bg = 'NONE' })  -- TIMEPART
  highlight(0, 'sasFuncIntck',          { fg = colors.orange,     bg = 'NONE' })  -- INTCK
  highlight(0, 'sasFuncIntnx',          { fg = colors.orange,     bg = 'NONE' })  -- INTNX

  -- Numeric/Statistical Functions
  highlight(0, 'sasFuncSum',            { fg = colors.orange,     bg = 'NONE' })  -- SUM
  highlight(0, 'sasFuncMean',           { fg = colors.orange,     bg = 'NONE' })  -- MEAN
  highlight(0, 'sasFuncMin',            { fg = colors.orange,     bg = 'NONE' })  -- MIN
  highlight(0, 'sasFuncMax',            { fg = colors.orange,     bg = 'NONE' })  -- MAX
  highlight(0, 'sasFuncN',              { fg = colors.orange,     bg = 'NONE' })  -- N
  highlight(0, 'sasFuncNmiss',          { fg = colors.orange,     bg = 'NONE' })  -- NMISS
  highlight(0, 'sasFuncStd',            { fg = colors.orange,     bg = 'NONE' })  -- STD
  highlight(0, 'sasFuncVar',            { fg = colors.orange,     bg = 'NONE' })  -- VAR
  highlight(0, 'sasFuncRange',          { fg = colors.orange,     bg = 'NONE' })  -- RANGE
  highlight(0, 'sasFuncMedian',         { fg = colors.orange,     bg = 'NONE' })  -- MEDIAN
  highlight(0, 'sasFuncOrdinal',        { fg = colors.orange,     bg = 'NONE' })  -- ORDINAL
  highlight(0, 'sasFuncPctl',           { fg = colors.orange,     bg = 'NONE' })  -- PCTL

  -- Type Conversion Functions
  highlight(0, 'sasFuncInput',          { fg = colors.orange,     bg = 'NONE' })  -- INPUT
  highlight(0, 'sasFuncPut',            { fg = colors.orange,     bg = 'NONE' })  -- PUT
  highlight(0, 'sasFuncInputc',         { fg = colors.orange,     bg = 'NONE' })  -- INPUTC, INPUTN
  highlight(0, 'sasFuncPutc',           { fg = colors.orange,     bg = 'NONE' })  -- PUTC, PUTN

  -- Logical/Missing Functions
  highlight(0, 'sasFuncMissing',        { fg = colors.orange,     bg = 'NONE' })  -- MISSING
  highlight(0, 'sasFuncCoalesce',       { fg = colors.orange,     bg = 'NONE' })  -- COALESCE, COALESCEC
  highlight(0, 'sasFuncIfn',            { fg = colors.orange,     bg = 'NONE' })  -- IFN, IFC
  highlight(0, 'sasFuncChoose',         { fg = colors.orange,     bg = 'NONE' })  -- CHOOSE, CHOOSEC, CHOOSEN

  -- Random Number Functions
  highlight(0, 'sasFuncRand',           { fg = colors.orange,     bg = 'NONE' })  -- RAND
  highlight(0, 'sasFuncRanuni',         { fg = colors.orange,     bg = 'NONE' })  -- RANUNI
  highlight(0, 'sasFuncRannor',         { fg = colors.orange,     bg = 'NONE' })  -- RANNOR

  -- Lag/Diff Functions
  highlight(0, 'sasFuncLag',            { fg = colors.orange,     bg = 'NONE' })  -- LAG, LAG1-LAGn
  highlight(0, 'sasFuncDif',            { fg = colors.orange,     bg = 'NONE' })  -- DIF, DIF1-DIFn

  -- Array Functions
  highlight(0, 'sasFuncDim',            { fg = colors.orange,     bg = 'NONE' })  -- DIM
  highlight(0, 'sasFuncHbound',         { fg = colors.orange,     bg = 'NONE' })  -- HBOUND
  highlight(0, 'sasFuncLbound',         { fg = colors.orange,     bg = 'NONE' })  -- LBOUND

  -- Regular Expression Functions
  highlight(0, 'sasFuncPrxmatch',       { fg = colors.orange,     bg = 'NONE' })  -- PRXMATCH
  highlight(0, 'sasFuncPrxparse',       { fg = colors.orange,     bg = 'NONE' })  -- PRXPARSE
  highlight(0, 'sasFuncPrxchange',      { fg = colors.orange,     bg = 'NONE' })  -- PRXCHANGE
  highlight(0, 'sasFuncPrxposn',        { fg = colors.orange,     bg = 'NONE' })  -- PRXPOSN


  -----------------------------------------------------------------------------
  -- CALL Routines

  highlight(0, 'sasCallSymput',         { fg = colors.orange,     bg = 'NONE' })  -- CALL SYMPUT, SYMPUTX
  highlight(0, 'sasCallExecute',        { fg = colors.orange,     bg = 'NONE' })  -- CALL EXECUTE
  highlight(0, 'sasCallMissing',        { fg = colors.orange,     bg = 'NONE' })  -- CALL MISSING
  highlight(0, 'sasCallSortn',          { fg = colors.orange,     bg = 'NONE' })  -- CALL SORTN, SORTC
  highlight(0, 'sasCallSet',            { fg = colors.orange,     bg = 'NONE' })  -- CALL SET
  highlight(0, 'sasCallScan',           { fg = colors.orange,     bg = 'NONE' })  -- CALL SCAN
  highlight(0, 'sasCallPrxchange',      { fg = colors.orange,     bg = 'NONE' })  -- CALL PRXCHANGE, etc.
  highlight(0, 'sasCallVname',          { fg = colors.orange,     bg = 'NONE' })  -- CALL VNAME
  highlight(0, 'sasCallLabel',          { fg = colors.orange,     bg = 'NONE' })  -- CALL LABEL
  highlight(0, 'sasCallCats',           { fg = colors.orange,     bg = 'NONE' })  -- CALL CATS, CATT, CATX


  -----------------------------------------------------------------------------
  -- Special SAS Variables

  highlight(0, 'sasVarN',               { fg = colors.blue,       bg = 'NONE' })  -- _N_ (observation number)
  highlight(0, 'sasVarError',           { fg = colors.blue,       bg = 'NONE' })  -- _ERROR_
  highlight(0, 'sasVarFirst',           { fg = colors.blue,       bg = 'NONE' })  -- FIRST.variable
  highlight(0, 'sasVarLast',            { fg = colors.blue,       bg = 'NONE' })  -- LAST.variable
  highlight(0, 'sasVarIORC',            { fg = colors.blue,       bg = 'NONE' })  -- _IORC_
  highlight(0, 'sasVarMsg',             { fg = colors.blue,       bg = 'NONE' })  -- _MSG_
  highlight(0, 'sasVarCmd',             { fg = colors.blue,       bg = 'NONE' })  -- _CMD_
  highlight(0, 'sasVarFile',            { fg = colors.blue,       bg = 'NONE' })  -- _FILE_
  highlight(0, 'sasVarInfile',          { fg = colors.blue,       bg = 'NONE' })  -- _INFILE_


  -----------------------------------------------------------------------------
  -- Reserved Words

  highlight(0, 'sasResNull',            { fg = colors.blue,       bg = 'NONE' })  -- _NULL_
  highlight(0, 'sasResData',            { fg = colors.blue,       bg = 'NONE' })  -- _DATA_
  highlight(0, 'sasResLast',            { fg = colors.blue,       bg = 'NONE' })  -- _LAST_
  highlight(0, 'sasResAll',             { fg = colors.blue,       bg = 'NONE' })  -- _ALL_
  highlight(0, 'sasResChar',            { fg = colors.blue,       bg = 'NONE' })  -- _CHARACTER_, _CHAR_
  highlight(0, 'sasResNum',             { fg = colors.blue,       bg = 'NONE' })  -- _NUMERIC_, _NUM_
  highlight(0, 'sasResAutomatic',       { fg = colors.blue,       bg = 'NONE' })  -- _AUTOMATIC_
  highlight(0, 'sasResUser',            { fg = colors.blue,       bg = 'NONE' })  -- _USER_
  highlight(0, 'sasResTemporary',       { fg = colors.blue,       bg = 'NONE' })  -- _TEMPORARY_


  -----------------------------------------------------------------------------
  -- PROC SQL Specific

  highlight(0, 'sasSQLSelect',          { fg = colors.blue,       bg = 'NONE' })  -- SELECT
  highlight(0, 'sasSQLFrom',            { fg = colors.blue,       bg = 'NONE' })  -- FROM
  highlight(0, 'sasSQLWhere',           { fg = colors.blue,       bg = 'NONE' })  -- WHERE
  highlight(0, 'sasSQLGroup',           { fg = colors.blue,       bg = 'NONE' })  -- GROUP BY
  highlight(0, 'sasSQLHaving',          { fg = colors.blue,       bg = 'NONE' })  -- HAVING
  highlight(0, 'sasSQLOrder',           { fg = colors.blue,       bg = 'NONE' })  -- ORDER BY
  highlight(0, 'sasSQLJoin',            { fg = colors.blue,       bg = 'NONE' })  -- JOIN, LEFT, RIGHT, FULL, INNER, OUTER
  highlight(0, 'sasSQLOn',              { fg = colors.blue,       bg = 'NONE' })  -- ON
  highlight(0, 'sasSQLAs',              { fg = colors.blue,       bg = 'NONE' })  -- AS
  highlight(0, 'sasSQLCreate',          { fg = colors.blue,       bg = 'NONE' })  -- CREATE TABLE, VIEW
  highlight(0, 'sasSQLInsert',          { fg = colors.blue,       bg = 'NONE' })  -- INSERT INTO
  highlight(0, 'sasSQLUpdate',          { fg = colors.blue,       bg = 'NONE' })  -- UPDATE, SET
  highlight(0, 'sasSQLDelete',          { fg = colors.blue,       bg = 'NONE' })  -- DELETE
  highlight(0, 'sasSQLDrop',            { fg = colors.blue,       bg = 'NONE' })  -- DROP
  highlight(0, 'sasSQLAlter',           { fg = colors.blue,       bg = 'NONE' })  -- ALTER
  highlight(0, 'sasSQLDistinct',        { fg = colors.blue,       bg = 'NONE' })  -- DISTINCT
  highlight(0, 'sasSQLUnion',           { fg = colors.blue,       bg = 'NONE' })  -- UNION, INTERSECT, EXCEPT
  highlight(0, 'sasSQLCase',            { fg = colors.blue,       bg = 'NONE' })  -- CASE, WHEN, THEN, ELSE, END
  highlight(0, 'sasSQLNull',            { fg = colors.blue,       bg = 'NONE' })  -- NULL, IS NULL, IS NOT NULL
  highlight(0, 'sasSQLBetween',         { fg = colors.blue,       bg = 'NONE' })  -- BETWEEN, LIKE, IN
  highlight(0, 'sasSQLAggregate',       { fg = colors.orange,     bg = 'NONE' })  -- COUNT, SUM, AVG, MIN, MAX
  highlight(0, 'sasSQLCalculated',      { fg = colors.blue,       bg = 'NONE' })  -- CALCULATED


  -----------------------------------------------------------------------------
  -- ODS Graphics / SGPLOT Specific

  highlight(0, 'sasSGStatement',        { fg = colors.blue,       bg = 'NONE' })  -- SGPLOT statements
  highlight(0, 'sasSGPlot',             { fg = colors.orange,     bg = 'NONE' })  -- SCATTER, SERIES, VBAR, HBAR, etc.
  highlight(0, 'sasSGOption',           { fg = colors.purple,     bg = 'NONE' })  -- SGPLOT options


  -----------------------------------------------------------------------------
  -- PROC IML Specific

  highlight(0, 'sasIMLKeyword',         { fg = colors.blue,       bg = 'NONE' })  -- IML keywords
  highlight(0, 'sasIMLFunction',        { fg = colors.orange,     bg = 'NONE' })  -- IML functions
  highlight(0, 'sasIMLMatrix',          { fg = colors.turquoise,  bg = 'NONE' })  -- Matrix operations


  -----------------------------------------------------------------------------
  -- Dataset/Library References

  highlight(0, 'sasLibRef',             { fg = colors.turquoise,  bg = 'NONE' })  -- Library reference (LIBNAME.DATASET)
  highlight(0, 'sasDatasetRef',         { fg = colors.white,      bg = 'NONE' })  -- Dataset name
  highlight(0, 'sasDatasetOpt',         { fg = colors.purple,     bg = 'NONE' })  -- (KEEP=, DROP=, WHERE=, etc.)
end

return sas
