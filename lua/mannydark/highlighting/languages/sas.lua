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
  highlight(0, 'sasConditional',        { link = "Conditional" })  -- IF, THEN, ELSE
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
  highlight(0, 'sasMacroKeyword',       { link = "Keyword" })  -- Macro keywords
  highlight(0, 'sasMacroVar',           { link = "Variable" })  -- &macrovar
  highlight(0, 'sasMacroVarRef',        { link = "Variable" })  -- &var references
  highlight(0, 'sasMacroFunc',          { link = "Function" })  -- %function()
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
  highlight(0, 'sasFunction',           { link = "Function" })  -- Functions
  highlight(0, 'sasFuncCall',           { link = "Function" })  -- CALL routines

  -- Math Functions
  highlight(0, 'sasFuncMath',           { link = "Function" })  -- ABS, SQRT, EXP, LOG, etc.

  -- String Functions
  highlight(0, 'sasFuncString',         { link = "String" })  -- SUBSTR, TRIM, STRIP, COMPRESS, etc.

  -- Date/Time Functions
  highlight(0, 'sasFuncDateTime',       { link = "Function" })  -- TODAY, DATE, TIME, MDY, etc.

  -- Statistical Functions
  highlight(0, 'sasFuncStat',           { link = "Function" })  -- MEAN, SUM, MIN, MAX, etc.

  -- Special Functions
  highlight(0, 'sasFuncSpecial',        { link = "Function" })  -- LAG, DIF, INPUT, PUT, etc.

  -- Hash Object
  highlight(0, 'sasHash',               { fg = colors.turquoise,  bg = 'NONE' })  -- HASH, HITER
  highlight(0, 'sasHashMethod',         { link = "Function" })  -- Hash methods
  highlight(0, 'sasHashAttr',           { fg = colors.purple,     bg = 'NONE' })  -- Hash attributes

  -- Declare/DCL
  highlight(0, 'sasDeclare',            { fg = colors.blue,       bg = 'NONE' })  -- DECLARE, DCL
  highlight(0, 'sasJavaobj',            { fg = colors.turquoise,  bg = 'NONE' })  -- JAVAOBJ

  -- Operators
  highlight(0, 'sasOperator',           { link = "Operator" })  -- General operators
  highlight(0, 'sasArithOp',            { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, **
  highlight(0, 'sasCompareOp',          { fg = colors.white,      bg = 'NONE' })  -- =, ^=, ~=, <, >, <=, >=
  highlight(0, 'sasCompareWord',        { fg = colors.blue,       bg = 'NONE' })  -- EQ, NE, LT, GT, LE, GE
  highlight(0, 'sasLogicalOp',          { fg = colors.white,      bg = 'NONE' })  -- &, |, ^, ~
  highlight(0, 'sasLogicalWord',        { fg = colors.blue,       bg = 'NONE' })  -- AND, OR, NOT, IN
  highlight(0, 'sasConcatOp',           { fg = colors.white,      bg = 'NONE' })  -- || (concatenation)
  highlight(0, 'sasRangeOp',            { fg = colors.blue,       bg = 'NONE' })  -- : (range in WHERE)
  highlight(0, 'sasOfOp',               { fg = colors.blue,       bg = 'NONE' })  -- OF (in array functions)

  -- Variables / Parameters
  highlight(0, 'sasVariable',           { link = "Variable" })  -- Variables
  highlight(0, 'sasParameter',          { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, 'sasDatasetOption',      { fg = colors.purple,     bg = 'NONE' })  -- Dataset options

  -- Special Variables
  highlight(0, 'sasAutoVar',            { link = "Variable" })  -- _N_, _ERROR_, FIRST., LAST.
  highlight(0, 'sasReserved',           { fg = colors.blue,       bg = 'NONE' })  -- _NULL_, _DATA_, _ALL_, etc.

  -- Formats / Informats
  highlight(0, 'sasFormatName',         { fg = colors.turquoise,  bg = 'NONE' })  -- Format names
  highlight(0, 'sasFormatChar',         { fg = colors.turquoise,  bg = 'NONE' })  -- $CHAR., $w., etc.
  highlight(0, 'sasFormatNum',          { fg = colors.turquoise,  bg = 'NONE' })  -- BEST., COMMA., DOLLAR., etc.
  highlight(0, 'sasFormatDate',         { fg = colors.turquoise,  bg = 'NONE' })  -- DATE9., DATETIME., MMDDYY., etc.

  -- Strings
  highlight(0, 'sasString',             { link = "String" })  -- 'string' or "string"
  highlight(0, 'sasStringDelim',        { link = "Delimiter" })  -- String delimiters
  highlight(0, 'sasEscapeSeq',          { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences

  -- Date/Time Literals
  highlight(0, 'sasDateLiteral',        { fg = colors.greenLight, bg = 'NONE' })  -- '01JAN2020'd
  highlight(0, 'sasTimeLiteral',        { fg = colors.greenLight, bg = 'NONE' })  -- '12:30:00't
  highlight(0, 'sasDatetimeLiteral',    { fg = colors.greenLight, bg = 'NONE' })  -- '01JAN2020:12:30:00'dt

  -- Numbers
  highlight(0, 'sasNumber',             { link = "Number" })  -- Numbers
  highlight(0, 'sasFloat',              { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'sasSciNotation',        { fg = colors.greenLight, bg = 'NONE' })  -- Scientific notation
  highlight(0, 'sasHex',                { fg = colors.greenLight, bg = 'NONE' })  -- Hex constants

  -- Missing Values
  highlight(0, 'sasMissing',            { fg = colors.blue,       bg = 'NONE' })  -- . (numeric missing)
  highlight(0, 'sasMissingSpecial',     { fg = colors.blue,       bg = 'NONE' })  -- .A-.Z, ._

  -- Comments
  highlight(0, 'sasComment',            { link = "Comment" })  -- /* comment */
  highlight(0, 'sasLineComment',        { link = "Comment" })  -- * comment;
  highlight(0, 'sasMacroComment',       { link = "Comment" })  -- %* comment;
  highlight(0, 'sasBlockComment',       { link = "Comment" })  -- /* block */
  highlight(0, 'sasTodo',               { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, TBD

  -- Delimiters / Punctuation
  highlight(0, 'sasDelimiter',          { link = "Delimiter" })  -- General delimiters
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
  highlight(0, '@variable.sas',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.sas',      { link = "Variable" })  -- _N_, _ERROR_, etc.
  highlight(0, '@variable.parameter.sas',    { link = "Variable" })  -- Parameters

  -- Constants
  highlight(0, '@constant.sas',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.sas',      { link = "Constant" })  -- _NULL_, _DATA_, etc.

  -- Functions
  highlight(0, '@function.sas',              { link = "Function" })  -- Functions
  highlight(0, '@function.call.sas',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.sas',      { link = "Function" })  -- Built-in functions
  highlight(0, '@function.macro.sas',        { link = "Function" })  -- Macro functions

  -- Types
  highlight(0, '@type.sas',                  { link = "Type" })  -- Types
  highlight(0, '@type.builtin.sas',          { link = "Type" })  -- Built-in types

  -- Keywords
  highlight(0, '@keyword.sas',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.sas',      { link = "Keyword" })  -- DATA, PROC
  highlight(0, '@keyword.return.sas',        { link = "Keyword" })  -- RETURN
  highlight(0, '@keyword.conditional.sas',   { link = "Conditional" })  -- IF, THEN, ELSE
  highlight(0, '@keyword.repeat.sas',        { link = "Keyword" })  -- DO, END, WHILE, UNTIL
  highlight(0, '@keyword.operator.sas',      { link = "Operator" })  -- AND, OR, NOT, IN, EQ, etc.

  -- Strings
  highlight(0, '@string.sas',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.sas',         { link = "String" })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.sas',                { link = "Number" })  -- Numbers
  highlight(0, '@number.float.sas',          { link = "Number" })  -- Floats

  -- Comments
  highlight(0, '@comment.sas',               { link = "Comment" })  -- Comments

  -- Operators
  highlight(0, '@operator.sas',              { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.sas',   { fg = colors.white,     bg = 'NONE' })  -- ()
  highlight(0, '@punctuation.delimiter.sas', { link = "Delimiter" })  -- ; ,
  highlight(0, '@punctuation.special.sas',   { fg = colors.pink,      bg = 'NONE' })  -- & %


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.sas)

  highlight(0, '@lsp.type.variable.sas',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.sas',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.function.sas',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.macro.sas',         { fg = colors.pink,      bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.keyword.sas',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.sas',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.comment.sas',       { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.string.sas',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.sas',        { link = "Number" })  -- Numbers


  -----------------------------------------------------------------------------
  -- Data Step Functions (Custom highlight groups)

  -- Mathematical Functions
  highlight(0, 'sasFuncAbs',            { link = "Function" })  -- ABS
  highlight(0, 'sasFuncSqrt',           { link = "Function" })  -- SQRT
  highlight(0, 'sasFuncExp',            { link = "Function" })  -- EXP
  highlight(0, 'sasFuncLog',            { link = "Function" })  -- LOG, LOG2, LOG10
  highlight(0, 'sasFuncRound',          { link = "Function" })  -- ROUND, CEIL, FLOOR, INT
  highlight(0, 'sasFuncMod',            { link = "Function" })  -- MOD
  highlight(0, 'sasFuncSign',           { link = "Function" })  -- SIGN
  highlight(0, 'sasFuncTrig',           { link = "Function" })  -- SIN, COS, TAN, ATAN, etc.

  -- String Functions
  highlight(0, 'sasFuncSubstr',         { link = "Function" })  -- SUBSTR
  highlight(0, 'sasFuncTrim',           { link = "Function" })  -- TRIM, STRIP, COMPRESS
  highlight(0, 'sasFuncUpcase',         { link = "Function" })  -- UPCASE, LOWCASE, PROPCASE
  highlight(0, 'sasFuncIndex',          { link = "Function" })  -- INDEX, FIND, INDEXC
  highlight(0, 'sasFuncLength',         { link = "Function" })  -- LENGTH, LENGTHN, LENGTHC
  highlight(0, 'sasFuncScan',           { link = "Function" })  -- SCAN
  highlight(0, 'sasFuncCat',            { link = "Function" })  -- CAT, CATS, CATT, CATX
  highlight(0, 'sasFuncTranslate',      { link = "Function" })  -- TRANSLATE, TRANWRD
  highlight(0, 'sasFuncReverse',        { link = "Function" })  -- REVERSE
  highlight(0, 'sasFuncRepeat',         { link = "Function" })  -- REPEAT
  highlight(0, 'sasFuncVerify',         { link = "Function" })  -- VERIFY
  highlight(0, 'sasFuncQuote',          { link = "Function" })  -- QUOTE, DEQUOTE

  -- Date/Time Functions
  highlight(0, 'sasFuncToday',          { link = "Function" })  -- TODAY, DATE
  highlight(0, 'sasFuncTime',           { link = "Function" })  -- TIME
  highlight(0, 'sasFuncDatetime',       { link = "Function" })  -- DATETIME
  highlight(0, 'sasFuncMdy',            { link = "Function" })  -- MDY
  highlight(0, 'sasFuncYmd',            { link = "Function" })  -- YMD
  highlight(0, 'sasFuncHms',            { link = "Function" })  -- HMS
  highlight(0, 'sasFuncDhms',           { link = "Function" })  -- DHMS
  highlight(0, 'sasFuncYear',           { link = "Function" })  -- YEAR
  highlight(0, 'sasFuncMonth',          { link = "Function" })  -- MONTH
  highlight(0, 'sasFuncDay',            { link = "Function" })  -- DAY
  highlight(0, 'sasFuncWeekday',        { link = "Function" })  -- WEEKDAY
  highlight(0, 'sasFuncQtr',            { link = "Function" })  -- QTR
  highlight(0, 'sasFuncWeek',           { link = "Function" })  -- WEEK
  highlight(0, 'sasFuncHour',           { link = "Function" })  -- HOUR
  highlight(0, 'sasFuncMinute',         { link = "Function" })  -- MINUTE
  highlight(0, 'sasFuncSecond',         { link = "Function" })  -- SECOND
  highlight(0, 'sasFuncDatepart',       { link = "Function" })  -- DATEPART
  highlight(0, 'sasFuncTimepart',       { link = "Function" })  -- TIMEPART
  highlight(0, 'sasFuncIntck',          { link = "Function" })  -- INTCK
  highlight(0, 'sasFuncIntnx',          { link = "Function" })  -- INTNX

  -- Numeric/Statistical Functions
  highlight(0, 'sasFuncSum',            { link = "Function" })  -- SUM
  highlight(0, 'sasFuncMean',           { link = "Function" })  -- MEAN
  highlight(0, 'sasFuncMin',            { link = "Function" })  -- MIN
  highlight(0, 'sasFuncMax',            { link = "Function" })  -- MAX
  highlight(0, 'sasFuncN',              { link = "Function" })  -- N
  highlight(0, 'sasFuncNmiss',          { link = "Function" })  -- NMISS
  highlight(0, 'sasFuncStd',            { link = "Function" })  -- STD
  highlight(0, 'sasFuncVar',            { link = "Function" })  -- VAR
  highlight(0, 'sasFuncRange',          { link = "Function" })  -- RANGE
  highlight(0, 'sasFuncMedian',         { link = "Function" })  -- MEDIAN
  highlight(0, 'sasFuncOrdinal',        { link = "Function" })  -- ORDINAL
  highlight(0, 'sasFuncPctl',           { link = "Function" })  -- PCTL

  -- Type Conversion Functions
  highlight(0, 'sasFuncInput',          { link = "Function" })  -- INPUT
  highlight(0, 'sasFuncPut',            { link = "Function" })  -- PUT
  highlight(0, 'sasFuncInputc',         { link = "Function" })  -- INPUTC, INPUTN
  highlight(0, 'sasFuncPutc',           { link = "Function" })  -- PUTC, PUTN

  -- Logical/Missing Functions
  highlight(0, 'sasFuncMissing',        { link = "Function" })  -- MISSING
  highlight(0, 'sasFuncCoalesce',       { link = "Function" })  -- COALESCE, COALESCEC
  highlight(0, 'sasFuncIfn',            { link = "Function" })  -- IFN, IFC
  highlight(0, 'sasFuncChoose',         { link = "Function" })  -- CHOOSE, CHOOSEC, CHOOSEN

  -- Random Number Functions
  highlight(0, 'sasFuncRand',           { link = "Function" })  -- RAND
  highlight(0, 'sasFuncRanuni',         { link = "Function" })  -- RANUNI
  highlight(0, 'sasFuncRannor',         { link = "Function" })  -- RANNOR

  -- Lag/Diff Functions
  highlight(0, 'sasFuncLag',            { link = "Function" })  -- LAG, LAG1-LAGn
  highlight(0, 'sasFuncDif',            { link = "Function" })  -- DIF, DIF1-DIFn

  -- Array Functions
  highlight(0, 'sasFuncDim',            { link = "Function" })  -- DIM
  highlight(0, 'sasFuncHbound',         { link = "Function" })  -- HBOUND
  highlight(0, 'sasFuncLbound',         { link = "Function" })  -- LBOUND

  -- Regular Expression Functions
  highlight(0, 'sasFuncPrxmatch',       { link = "Function" })  -- PRXMATCH
  highlight(0, 'sasFuncPrxparse',       { link = "Function" })  -- PRXPARSE
  highlight(0, 'sasFuncPrxchange',      { link = "Function" })  -- PRXCHANGE
  highlight(0, 'sasFuncPrxposn',        { link = "Function" })  -- PRXPOSN


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

  highlight(0, 'sasVarN',               { link = "Variable" })  -- _N_ (observation number)
  highlight(0, 'sasVarError',           { link = "Variable" })  -- _ERROR_
  highlight(0, 'sasVarFirst',           { link = "Variable" })  -- FIRST.variable
  highlight(0, 'sasVarLast',            { link = "Variable" })  -- LAST.variable
  highlight(0, 'sasVarIORC',            { link = "Variable" })  -- _IORC_
  highlight(0, 'sasVarMsg',             { link = "Variable" })  -- _MSG_
  highlight(0, 'sasVarCmd',             { link = "Variable" })  -- _CMD_
  highlight(0, 'sasVarFile',            { link = "Variable" })  -- _FILE_
  highlight(0, 'sasVarInfile',          { link = "Variable" })  -- _INFILE_


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

  highlight(0, 'sasIMLKeyword',         { link = "Keyword" })  -- IML keywords
  highlight(0, 'sasIMLFunction',        { link = "Function" })  -- IML functions
  highlight(0, 'sasIMLMatrix',          { fg = colors.turquoise,  bg = 'NONE' })  -- Matrix operations


  -----------------------------------------------------------------------------
  -- Dataset/Library References

  highlight(0, 'sasLibRef',             { fg = colors.turquoise,  bg = 'NONE' })  -- Library reference (LIBNAME.DATASET)
  highlight(0, 'sasDatasetRef',         { fg = colors.white,      bg = 'NONE' })  -- Dataset name
  highlight(0, 'sasDatasetOpt',         { fg = colors.purple,     bg = 'NONE' })  -- (KEEP=, DROP=, WHERE=, etc.)
end

return sas
