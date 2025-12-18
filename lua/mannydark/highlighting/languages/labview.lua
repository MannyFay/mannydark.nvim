-------------------------------------------------------------------------------
-- LabVIEW
-- Highlighting for LabVIEW-related text files:
-- - MathScript (.m files in LabVIEW context)
-- - Formula Node (C-like syntax)
-- - Project files (.lvproj, .lvlib, .lvclass - XML-based)
-- Note: .vi files are binary and cannot be syntax highlighted
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local labview   = {}


-------------------------------------------------------------------------------
-- Settings

labview.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- MathScript Syntax (MATLAB-like)
  -- Used in MathScript Window and MathScript Node

  -- Keywords - Control Flow
  highlight(0, 'labviewMathScriptKeyword',    { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, 'labviewMathScriptIf',         { fg = colors.blue,       bg = 'NONE' })  -- if
  highlight(0, 'labviewMathScriptElse',       { fg = colors.blue,       bg = 'NONE' })  -- else, elseif
  highlight(0, 'labviewMathScriptEnd',        { fg = colors.blue,       bg = 'NONE' })  -- end
  highlight(0, 'labviewMathScriptFor',        { fg = colors.blue,       bg = 'NONE' })  -- for
  highlight(0, 'labviewMathScriptWhile',      { fg = colors.blue,       bg = 'NONE' })  -- while
  highlight(0, 'labviewMathScriptSwitch',     { fg = colors.blue,       bg = 'NONE' })  -- switch, case, otherwise
  highlight(0, 'labviewMathScriptBreak',      { fg = colors.blue,       bg = 'NONE' })  -- break, continue
  highlight(0, 'labviewMathScriptReturn',     { fg = colors.blue,       bg = 'NONE' })  -- return
  highlight(0, 'labviewMathScriptTry',        { fg = colors.blue,       bg = 'NONE' })  -- try, catch

  -- Keywords - Functions
  highlight(0, 'labviewMathScriptFunction',   { fg = colors.blue,       bg = 'NONE' })  -- function keyword
  highlight(0, 'labviewMathScriptGlobal',     { fg = colors.blue,       bg = 'NONE' })  -- global keyword
  highlight(0, 'labviewMathScriptPersistent', { fg = colors.blue,       bg = 'NONE' })  -- persistent keyword

  -- Built-in Variables
  highlight(0, 'labviewMathScriptBuiltinVar', { fg = colors.purple,     bg = 'NONE' })  -- Built-in variables
  highlight(0, 'labviewMathScriptAns',        { fg = colors.purple,     bg = 'NONE' })  -- ans
  highlight(0, 'labviewMathScriptPi',         { fg = colors.purple,     bg = 'NONE' })  -- pi
  highlight(0, 'labviewMathScriptInf',        { fg = colors.purple,     bg = 'NONE' })  -- inf, Inf
  highlight(0, 'labviewMathScriptNaN',        { fg = colors.purple,     bg = 'NONE' })  -- nan, NaN
  highlight(0, 'labviewMathScriptI',          { fg = colors.purple,     bg = 'NONE' })  -- i, j (imaginary)
  highlight(0, 'labviewMathScriptEps',        { fg = colors.purple,     bg = 'NONE' })  -- eps
  highlight(0, 'labviewMathScriptTrue',       { fg = colors.blue,       bg = 'NONE' })  -- true
  highlight(0, 'labviewMathScriptFalse',      { fg = colors.blue,       bg = 'NONE' })  -- false

  -- Matrix/Vector Creation Functions
  highlight(0, 'labviewMathScriptMatrixFunc', { fg = colors.orange,     bg = 'NONE' })  -- Matrix functions
  highlight(0, 'labviewMathScriptZeros',      { fg = colors.orange,     bg = 'NONE' })  -- zeros
  highlight(0, 'labviewMathScriptOnes',       { fg = colors.orange,     bg = 'NONE' })  -- ones
  highlight(0, 'labviewMathScriptEye',        { fg = colors.orange,     bg = 'NONE' })  -- eye
  highlight(0, 'labviewMathScriptDiag',       { fg = colors.orange,     bg = 'NONE' })  -- diag
  highlight(0, 'labviewMathScriptLinspace',   { fg = colors.orange,     bg = 'NONE' })  -- linspace
  highlight(0, 'labviewMathScriptLogspace',   { fg = colors.orange,     bg = 'NONE' })  -- logspace
  highlight(0, 'labviewMathScriptMeshgrid',   { fg = colors.orange,     bg = 'NONE' })  -- meshgrid
  highlight(0, 'labviewMathScriptRand',       { fg = colors.orange,     bg = 'NONE' })  -- rand, randn
  highlight(0, 'labviewMathScriptReshape',    { fg = colors.orange,     bg = 'NONE' })  -- reshape

  -- Math Functions
  highlight(0, 'labviewMathScriptMathFunc',   { fg = colors.orange,     bg = 'NONE' })  -- Math functions
  highlight(0, 'labviewMathScriptSqrt',       { fg = colors.orange,     bg = 'NONE' })  -- sqrt
  highlight(0, 'labviewMathScriptAbs',        { fg = colors.orange,     bg = 'NONE' })  -- abs
  highlight(0, 'labviewMathScriptExp',        { fg = colors.orange,     bg = 'NONE' })  -- exp
  highlight(0, 'labviewMathScriptLog',        { fg = colors.orange,     bg = 'NONE' })  -- log, log10, log2
  highlight(0, 'labviewMathScriptSin',        { fg = colors.orange,     bg = 'NONE' })  -- sin, cos, tan
  highlight(0, 'labviewMathScriptAsin',       { fg = colors.orange,     bg = 'NONE' })  -- asin, acos, atan
  highlight(0, 'labviewMathScriptSinh',       { fg = colors.orange,     bg = 'NONE' })  -- sinh, cosh, tanh
  highlight(0, 'labviewMathScriptFloor',      { fg = colors.orange,     bg = 'NONE' })  -- floor, ceil, round
  highlight(0, 'labviewMathScriptMod',        { fg = colors.orange,     bg = 'NONE' })  -- mod, rem
  highlight(0, 'labviewMathScriptSign',       { fg = colors.orange,     bg = 'NONE' })  -- sign
  highlight(0, 'labviewMathScriptMax',        { fg = colors.orange,     bg = 'NONE' })  -- max, min
  highlight(0, 'labviewMathScriptSum',        { fg = colors.orange,     bg = 'NONE' })  -- sum, prod
  highlight(0, 'labviewMathScriptMean',       { fg = colors.orange,     bg = 'NONE' })  -- mean, median, std

  -- Matrix Operations
  highlight(0, 'labviewMathScriptMatrixOp',   { fg = colors.orange,     bg = 'NONE' })  -- Matrix operations
  highlight(0, 'labviewMathScriptSize',       { fg = colors.orange,     bg = 'NONE' })  -- size
  highlight(0, 'labviewMathScriptLength',     { fg = colors.orange,     bg = 'NONE' })  -- length
  highlight(0, 'labviewMathScriptNumel',      { fg = colors.orange,     bg = 'NONE' })  -- numel
  highlight(0, 'labviewMathScriptTranspose',  { fg = colors.orange,     bg = 'NONE' })  -- transpose
  highlight(0, 'labviewMathScriptInv',        { fg = colors.orange,     bg = 'NONE' })  -- inv
  highlight(0, 'labviewMathScriptDet',        { fg = colors.orange,     bg = 'NONE' })  -- det
  highlight(0, 'labviewMathScriptEig',        { fg = colors.orange,     bg = 'NONE' })  -- eig
  highlight(0, 'labviewMathScriptSvd',        { fg = colors.orange,     bg = 'NONE' })  -- svd
  highlight(0, 'labviewMathScriptRank',       { fg = colors.orange,     bg = 'NONE' })  -- rank
  highlight(0, 'labviewMathScriptNorm',       { fg = colors.orange,     bg = 'NONE' })  -- norm

  -- Control System Functions
  highlight(0, 'labviewMathScriptControlFunc', { fg = colors.orange,    bg = 'NONE' })  -- Control functions
  highlight(0, 'labviewMathScriptTf',         { fg = colors.orange,     bg = 'NONE' })  -- tf
  highlight(0, 'labviewMathScriptZpk',        { fg = colors.orange,     bg = 'NONE' })  -- zpk
  highlight(0, 'labviewMathScriptSs',         { fg = colors.orange,     bg = 'NONE' })  -- ss
  highlight(0, 'labviewMathScriptStep',       { fg = colors.orange,     bg = 'NONE' })  -- step
  highlight(0, 'labviewMathScriptBode',       { fg = colors.orange,     bg = 'NONE' })  -- bode, bodemag
  highlight(0, 'labviewMathScriptNyquist',    { fg = colors.orange,     bg = 'NONE' })  -- nyquist
  highlight(0, 'labviewMathScriptRlocus',     { fg = colors.orange,     bg = 'NONE' })  -- rlocus
  highlight(0, 'labviewMathScriptMargin',     { fg = colors.orange,     bg = 'NONE' })  -- margin
  highlight(0, 'labviewMathScriptFeedback',   { fg = colors.orange,     bg = 'NONE' })  -- feedback
  highlight(0, 'labviewMathScriptSeries',     { fg = colors.orange,     bg = 'NONE' })  -- series
  highlight(0, 'labviewMathScriptParallel',   { fg = colors.orange,     bg = 'NONE' })  -- parallel
  highlight(0, 'labviewMathScriptPade',       { fg = colors.orange,     bg = 'NONE' })  -- pade

  -- Signal Processing Functions
  highlight(0, 'labviewMathScriptSignalFunc', { fg = colors.orange,     bg = 'NONE' })  -- Signal processing
  highlight(0, 'labviewMathScriptFft',        { fg = colors.orange,     bg = 'NONE' })  -- fft, ifft
  highlight(0, 'labviewMathScriptFilter',     { fg = colors.orange,     bg = 'NONE' })  -- filter
  highlight(0, 'labviewMathScriptConv',       { fg = colors.orange,     bg = 'NONE' })  -- conv
  highlight(0, 'labviewMathScriptXcorr',      { fg = colors.orange,     bg = 'NONE' })  -- xcorr
  highlight(0, 'labviewMathScriptInterp',     { fg = colors.orange,     bg = 'NONE' })  -- interp1, interp2

  -- Plotting Functions
  highlight(0, 'labviewMathScriptPlotFunc',   { fg = colors.orange,     bg = 'NONE' })  -- Plotting functions
  highlight(0, 'labviewMathScriptPlot',       { fg = colors.orange,     bg = 'NONE' })  -- plot
  highlight(0, 'labviewMathScriptStem',       { fg = colors.orange,     bg = 'NONE' })  -- stem
  highlight(0, 'labviewMathScriptBar',        { fg = colors.orange,     bg = 'NONE' })  -- bar
  highlight(0, 'labviewMathScriptHistogram',  { fg = colors.orange,     bg = 'NONE' })  -- histogram, hist
  highlight(0, 'labviewMathScriptSurf',       { fg = colors.orange,     bg = 'NONE' })  -- surf, mesh
  highlight(0, 'labviewMathScriptContour',    { fg = colors.orange,     bg = 'NONE' })  -- contour
  highlight(0, 'labviewMathScriptFigure',     { fg = colors.orange,     bg = 'NONE' })  -- figure
  highlight(0, 'labviewMathScriptSubplot',    { fg = colors.orange,     bg = 'NONE' })  -- subplot
  highlight(0, 'labviewMathScriptTitle',      { fg = colors.orange,     bg = 'NONE' })  -- title
  highlight(0, 'labviewMathScriptXlabel',     { fg = colors.orange,     bg = 'NONE' })  -- xlabel, ylabel, zlabel
  highlight(0, 'labviewMathScriptLegend',     { fg = colors.orange,     bg = 'NONE' })  -- legend
  highlight(0, 'labviewMathScriptGrid',       { fg = colors.orange,     bg = 'NONE' })  -- grid
  highlight(0, 'labviewMathScriptAxis',       { fg = colors.orange,     bg = 'NONE' })  -- axis
  highlight(0, 'labviewMathScriptHold',       { fg = colors.orange,     bg = 'NONE' })  -- hold
  highlight(0, 'labviewMathScriptSemilogx',   { fg = colors.orange,     bg = 'NONE' })  -- semilogx, semilogy, loglog

  -- File I/O Functions
  highlight(0, 'labviewMathScriptFileFunc',   { fg = colors.orange,     bg = 'NONE' })  -- File functions
  highlight(0, 'labviewMathScriptLoad',       { fg = colors.orange,     bg = 'NONE' })  -- load
  highlight(0, 'labviewMathScriptSave',       { fg = colors.orange,     bg = 'NONE' })  -- save
  highlight(0, 'labviewMathScriptFopen',      { fg = colors.orange,     bg = 'NONE' })  -- fopen
  highlight(0, 'labviewMathScriptFclose',     { fg = colors.orange,     bg = 'NONE' })  -- fclose
  highlight(0, 'labviewMathScriptFread',      { fg = colors.orange,     bg = 'NONE' })  -- fread, fwrite
  highlight(0, 'labviewMathScriptFprintf',    { fg = colors.orange,     bg = 'NONE' })  -- fprintf, fscanf
  highlight(0, 'labviewMathScriptDlmread',    { fg = colors.orange,     bg = 'NONE' })  -- dlmread, dlmwrite
  highlight(0, 'labviewMathScriptCsvread',    { fg = colors.orange,     bg = 'NONE' })  -- csvread, csvwrite

  -- Workspace Commands
  highlight(0, 'labviewMathScriptWorkspace',  { fg = colors.orange,     bg = 'NONE' })  -- Workspace commands
  highlight(0, 'labviewMathScriptWho',        { fg = colors.orange,     bg = 'NONE' })  -- who, whos
  highlight(0, 'labviewMathScriptClear',      { fg = colors.orange,     bg = 'NONE' })  -- clear
  highlight(0, 'labviewMathScriptClc',        { fg = colors.orange,     bg = 'NONE' })  -- clc
  highlight(0, 'labviewMathScriptClose',      { fg = colors.orange,     bg = 'NONE' })  -- close
  highlight(0, 'labviewMathScriptFormat',     { fg = colors.orange,     bg = 'NONE' })  -- format
  highlight(0, 'labviewMathScriptDisp',       { fg = colors.orange,     bg = 'NONE' })  -- disp
  highlight(0, 'labviewMathScriptHelp',       { fg = colors.orange,     bg = 'NONE' })  -- help

  -- Path Commands
  highlight(0, 'labviewMathScriptPathCmd',    { fg = colors.orange,     bg = 'NONE' })  -- Path commands
  highlight(0, 'labviewMathScriptPwd',        { fg = colors.orange,     bg = 'NONE' })  -- pwd
  highlight(0, 'labviewMathScriptCd',         { fg = colors.orange,     bg = 'NONE' })  -- cd
  highlight(0, 'labviewMathScriptDir',        { fg = colors.orange,     bg = 'NONE' })  -- dir, ls

  -- Operators
  highlight(0, 'labviewMathScriptOperator',   { fg = colors.white,      bg = 'NONE' })  -- Operators
  highlight(0, 'labviewMathScriptArithOp',    { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, ^
  highlight(0, 'labviewMathScriptDotOp',      { fg = colors.white,      bg = 'NONE' })  -- .*, ./, .^
  highlight(0, 'labviewMathScriptCompareOp',  { fg = colors.white,      bg = 'NONE' })  -- ==, ~=, <, >, <=, >=
  highlight(0, 'labviewMathScriptLogicalOp',  { fg = colors.white,      bg = 'NONE' })  -- &, |, ~, &&, ||
  highlight(0, 'labviewMathScriptColon',      { fg = colors.white,      bg = 'NONE' })  -- : colon operator
  highlight(0, 'labviewMathScriptTransposeOp', { fg = colors.white,     bg = 'NONE' })  -- ' transpose

  -- Numbers
  highlight(0, 'labviewMathScriptNumber',     { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, 'labviewMathScriptFloat',      { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'labviewMathScriptComplex',    { fg = colors.greenLight, bg = 'NONE' })  -- Complex numbers

  -- Strings
  highlight(0, 'labviewMathScriptString',     { fg = colors.redLight,   bg = 'NONE' })  -- 'strings'

  -- Comments
  highlight(0, 'labviewMathScriptComment',    { fg = colors.red,        bg = 'NONE' })  -- % comments
  highlight(0, 'labviewMathScriptBlockComment', { fg = colors.red,      bg = 'NONE' })  -- %{ %}
  highlight(0, 'labviewMathScriptTodo',       { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO

  -- Delimiters
  highlight(0, 'labviewMathScriptDelimiter',  { fg = colors.white,      bg = 'NONE' })  -- ; , ( ) [ ] { }
  highlight(0, 'labviewMathScriptSemicolon',  { fg = colors.white,      bg = 'NONE' })  -- ; suppresses output


  -----------------------------------------------------------------------------
  -- Formula Node Syntax (C-like)
  -- Used in Formula Node structure on block diagrams

  -- Keywords
  highlight(0, 'labviewFormulaKeyword',       { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, 'labviewFormulaIf',            { fg = colors.blue,       bg = 'NONE' })  -- if
  highlight(0, 'labviewFormulaElse',          { fg = colors.blue,       bg = 'NONE' })  -- else
  highlight(0, 'labviewFormulaFor',           { fg = colors.blue,       bg = 'NONE' })  -- for
  highlight(0, 'labviewFormulaWhile',         { fg = colors.blue,       bg = 'NONE' })  -- while
  highlight(0, 'labviewFormulaDo',            { fg = colors.blue,       bg = 'NONE' })  -- do
  highlight(0, 'labviewFormulaSwitch',        { fg = colors.blue,       bg = 'NONE' })  -- switch
  highlight(0, 'labviewFormulaCase',          { fg = colors.blue,       bg = 'NONE' })  -- case
  highlight(0, 'labviewFormulaDefault',       { fg = colors.blue,       bg = 'NONE' })  -- default
  highlight(0, 'labviewFormulaBreak',         { fg = colors.blue,       bg = 'NONE' })  -- break
  highlight(0, 'labviewFormulaContinue',      { fg = colors.blue,       bg = 'NONE' })  -- continue

  -- Built-in Functions
  highlight(0, 'labviewFormulaFunction',      { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, 'labviewFormulaAbs',           { fg = colors.orange,     bg = 'NONE' })  -- abs
  highlight(0, 'labviewFormulaAcos',          { fg = colors.orange,     bg = 'NONE' })  -- acos, acosh
  highlight(0, 'labviewFormulaAsin',          { fg = colors.orange,     bg = 'NONE' })  -- asin, asinh
  highlight(0, 'labviewFormulaAtan',          { fg = colors.orange,     bg = 'NONE' })  -- atan, atan2, atanh
  highlight(0, 'labviewFormulaCeil',          { fg = colors.orange,     bg = 'NONE' })  -- ceil
  highlight(0, 'labviewFormulaCos',           { fg = colors.orange,     bg = 'NONE' })  -- cos, cosh
  highlight(0, 'labviewFormulaCot',           { fg = colors.orange,     bg = 'NONE' })  -- cot
  highlight(0, 'labviewFormulaCsc',           { fg = colors.orange,     bg = 'NONE' })  -- csc
  highlight(0, 'labviewFormulaExp',           { fg = colors.orange,     bg = 'NONE' })  -- exp, expm1
  highlight(0, 'labviewFormulaFloor',         { fg = colors.orange,     bg = 'NONE' })  -- floor
  highlight(0, 'labviewFormulaGetexp',        { fg = colors.orange,     bg = 'NONE' })  -- getexp
  highlight(0, 'labviewFormulaGetman',        { fg = colors.orange,     bg = 'NONE' })  -- getman
  highlight(0, 'labviewFormulaInt',           { fg = colors.orange,     bg = 'NONE' })  -- int, intrz
  highlight(0, 'labviewFormulaLn',            { fg = colors.orange,     bg = 'NONE' })  -- ln, lnp1
  highlight(0, 'labviewFormulaLog',           { fg = colors.orange,     bg = 'NONE' })  -- log, log2
  highlight(0, 'labviewFormulaMax',           { fg = colors.orange,     bg = 'NONE' })  -- max
  highlight(0, 'labviewFormulaMin',           { fg = colors.orange,     bg = 'NONE' })  -- min
  highlight(0, 'labviewFormulaMod',           { fg = colors.orange,     bg = 'NONE' })  -- mod
  highlight(0, 'labviewFormulaPow',           { fg = colors.orange,     bg = 'NONE' })  -- pow
  highlight(0, 'labviewFormulaRand',          { fg = colors.orange,     bg = 'NONE' })  -- rand
  highlight(0, 'labviewFormulaRem',           { fg = colors.orange,     bg = 'NONE' })  -- rem
  highlight(0, 'labviewFormulaSec',           { fg = colors.orange,     bg = 'NONE' })  -- sec
  highlight(0, 'labviewFormulaSign',          { fg = colors.orange,     bg = 'NONE' })  -- sign
  highlight(0, 'labviewFormulaSin',           { fg = colors.orange,     bg = 'NONE' })  -- sin, sinc, sinh
  highlight(0, 'labviewFormulaSizeOfDim',     { fg = colors.orange,     bg = 'NONE' })  -- sizeOfDim
  highlight(0, 'labviewFormulaSqrt',          { fg = colors.orange,     bg = 'NONE' })  -- sqrt
  highlight(0, 'labviewFormulaTan',           { fg = colors.orange,     bg = 'NONE' })  -- tan, tanh

  -- Constants
  highlight(0, 'labviewFormulaConstant',      { fg = colors.purple,     bg = 'NONE' })  -- Constants
  highlight(0, 'labviewFormulaPi',            { fg = colors.purple,     bg = 'NONE' })  -- pi

  -- Operators
  highlight(0, 'labviewFormulaOperator',      { fg = colors.white,      bg = 'NONE' })  -- Operators
  highlight(0, 'labviewFormulaArithOp',       { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, %
  highlight(0, 'labviewFormulaCompareOp',     { fg = colors.white,      bg = 'NONE' })  -- ==, !=, <, >, <=, >=
  highlight(0, 'labviewFormulaLogicalOp',     { fg = colors.white,      bg = 'NONE' })  -- &&, ||, !
  highlight(0, 'labviewFormulaBitwiseOp',     { fg = colors.white,      bg = 'NONE' })  -- &, |, ^, ~, <<, >>
  highlight(0, 'labviewFormulaAssignOp',      { fg = colors.white,      bg = 'NONE' })  -- =, +=, -=, *=, /=
  highlight(0, 'labviewFormulaTernaryOp',     { fg = colors.white,      bg = 'NONE' })  -- ? :
  highlight(0, 'labviewFormulaIncDecOp',      { fg = colors.white,      bg = 'NONE' })  -- ++, --

  -- Numbers
  highlight(0, 'labviewFormulaNumber',        { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, 'labviewFormulaFloat',         { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'labviewFormulaHex',           { fg = colors.greenLight, bg = 'NONE' })  -- 0xFF hex

  -- Strings
  highlight(0, 'labviewFormulaString',        { fg = colors.redLight,   bg = 'NONE' })  -- "strings"

  -- Comments
  highlight(0, 'labviewFormulaComment',       { fg = colors.red,        bg = 'NONE' })  -- // and /* */ comments
  highlight(0, 'labviewFormulaLineComment',   { fg = colors.red,        bg = 'NONE' })  -- // comments
  highlight(0, 'labviewFormulaBlockComment',  { fg = colors.red,        bg = 'NONE' })  -- /* */ comments
  highlight(0, 'labviewFormulaTodo',          { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO

  -- Delimiters
  highlight(0, 'labviewFormulaDelimiter',     { fg = colors.white,      bg = 'NONE' })  -- ; , ( ) [ ] { }
  highlight(0, 'labviewFormulaSemicolon',     { fg = colors.white,      bg = 'NONE' })  -- ; statement end


  -----------------------------------------------------------------------------
  -- LabVIEW Project Files (XML-based)
  -- .lvproj, .lvlib, .lvclass files

  -- XML Tags/Elements
  highlight(0, 'labviewXmlTag',               { fg = colors.pink,       bg = 'NONE' })  -- XML tags
  highlight(0, 'labviewXmlTagName',           { fg = colors.pink,       bg = 'NONE' })  -- Tag names
  highlight(0, 'labviewXmlEndTag',            { fg = colors.pink,       bg = 'NONE' })  -- </tag>
  highlight(0, 'labviewXmlAttribute',         { fg = colors.orange,     bg = 'NONE' })  -- Attribute names
  highlight(0, 'labviewXmlAttributeValue',    { fg = colors.redLight,   bg = 'NONE' })  -- Attribute values
  highlight(0, 'labviewXmlString',            { fg = colors.redLight,   bg = 'NONE' })  -- String content
  highlight(0, 'labviewXmlComment',           { fg = colors.red,        bg = 'NONE' })  -- <!-- comments -->
  highlight(0, 'labviewXmlCdata',             { fg = colors.redLight,   bg = 'NONE' })  -- CDATA sections
  highlight(0, 'labviewXmlDeclaration',       { fg = colors.pink,       bg = 'NONE' })  -- <?xml ?>
  highlight(0, 'labviewXmlEntity',            { fg = colors.purple,     bg = 'NONE' })  -- &amp; &lt; &gt;

  -- LabVIEW-Specific XML Elements
  highlight(0, 'labviewProjProject',          { fg = colors.turquoise,  bg = 'NONE' })  -- <Project>
  highlight(0, 'labviewProjItem',             { fg = colors.turquoise,  bg = 'NONE' })  -- <Item>
  highlight(0, 'labviewProjProperty',         { fg = colors.turquoise,  bg = 'NONE' })  -- <Property>
  highlight(0, 'labviewProjTarget',           { fg = colors.turquoise,  bg = 'NONE' })  -- <Target>
  highlight(0, 'labviewProjBuild',            { fg = colors.turquoise,  bg = 'NONE' })  -- <BuildSpec>
  highlight(0, 'labviewProjDependencies',     { fg = colors.turquoise,  bg = 'NONE' })  -- <Dependencies>

  -- Property Names
  highlight(0, 'labviewProjPropName',         { fg = colors.purple,     bg = 'NONE' })  -- Property names
  highlight(0, 'labviewProjPropType',         { fg = colors.turquoise,  bg = 'NONE' })  -- Property types
  highlight(0, 'labviewProjPropValue',        { fg = colors.redLight,   bg = 'NONE' })  -- Property values


  -----------------------------------------------------------------------------
  -- LabVIEW NXG Files
  -- .gvi, .gcomp, .gtype files

  highlight(0, 'labviewNxgElement',           { fg = colors.turquoise,  bg = 'NONE' })  -- NXG elements
  highlight(0, 'labviewNxgAttribute',         { fg = colors.orange,     bg = 'NONE' })  -- NXG attributes
  highlight(0, 'labviewNxgValue',             { fg = colors.redLight,   bg = 'NONE' })  -- NXG values


  -----------------------------------------------------------------------------
  -- Treesitter Groups (for potential future parsers)

  -- MathScript
  highlight(0, '@keyword.labview',            { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, '@keyword.function.labview',   { fg = colors.blue,       bg = 'NONE' })  -- function
  highlight(0, '@keyword.return.labview',     { fg = colors.blue,       bg = 'NONE' })  -- return
  highlight(0, '@keyword.repeat.labview',     { fg = colors.blue,       bg = 'NONE' })  -- for, while
  highlight(0, '@keyword.conditional.labview', { fg = colors.blue,      bg = 'NONE' })  -- if, else, switch

  highlight(0, '@function.labview',           { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@function.builtin.labview',   { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.call.labview',      { fg = colors.orange,     bg = 'NONE' })  -- Function calls

  highlight(0, '@variable.labview',           { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.labview',   { fg = colors.purple,     bg = 'NONE' })  -- ans, pi, inf, etc.

  highlight(0, '@constant.labview',           { fg = colors.purple,     bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.labview',   { fg = colors.purple,     bg = 'NONE' })  -- pi, inf, nan

  highlight(0, '@number.labview',             { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.float.labview',       { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  highlight(0, '@string.labview',             { fg = colors.redLight,   bg = 'NONE' })  -- Strings

  highlight(0, '@operator.labview',           { fg = colors.white,      bg = 'NONE' })  -- Operators

  highlight(0, '@punctuation.bracket.labview', { fg = colors.white,     bg = 'NONE' })  -- ( ) [ ] { }
  highlight(0, '@punctuation.delimiter.labview', { fg = colors.white,   bg = 'NONE' })  -- , ; :

  highlight(0, '@comment.labview',            { fg = colors.red,        bg = 'NONE' })  -- Comments


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens

  highlight(0, '@lsp.type.variable.labview',  { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.parameter.labview', { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.function.labview',  { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.labview',   { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.operator.labview',  { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.string.labview',    { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@lsp.type.number.labview',    { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@lsp.type.comment.labview',   { fg = colors.red,        bg = 'NONE' })


  -----------------------------------------------------------------------------
  -- Data Acquisition / Instrument Control Terms
  -- Common in LabVIEW programming

  highlight(0, 'labviewDAQ',                  { fg = colors.turquoise,  bg = 'NONE' })  -- DAQ terms
  highlight(0, 'labviewVISA',                 { fg = colors.turquoise,  bg = 'NONE' })  -- VISA terms
  highlight(0, 'labviewGPIB',                 { fg = colors.turquoise,  bg = 'NONE' })  -- GPIB terms
  highlight(0, 'labviewSerial',               { fg = colors.turquoise,  bg = 'NONE' })  -- Serial terms
  highlight(0, 'labviewTCP',                  { fg = colors.turquoise,  bg = 'NONE' })  -- TCP/IP terms
  highlight(0, 'labviewModbus',               { fg = colors.turquoise,  bg = 'NONE' })  -- Modbus terms
  highlight(0, 'labviewOPC',                  { fg = colors.turquoise,  bg = 'NONE' })  -- OPC terms


  -----------------------------------------------------------------------------
  -- VI Types and Structures

  highlight(0, 'labviewVI',                   { fg = colors.turquoise,  bg = 'NONE' })  -- VI related
  highlight(0, 'labviewSubVI',                { fg = colors.orange,     bg = 'NONE' })  -- SubVI
  highlight(0, 'labviewExpressVI',            { fg = colors.orange,     bg = 'NONE' })  -- Express VI
  highlight(0, 'labviewPolymorphicVI',        { fg = colors.orange,     bg = 'NONE' })  -- Polymorphic VI
  highlight(0, 'labviewGlobalVI',             { fg = colors.purple,     bg = 'NONE' })  -- Global VI
  highlight(0, 'labviewFunctionalGlobal',     { fg = colors.purple,     bg = 'NONE' })  -- Functional Global

  -- Structures
  highlight(0, 'labviewStructure',            { fg = colors.blue,       bg = 'NONE' })  -- Structures
  highlight(0, 'labviewWhileLoop',            { fg = colors.blue,       bg = 'NONE' })  -- While Loop
  highlight(0, 'labviewForLoop',              { fg = colors.blue,       bg = 'NONE' })  -- For Loop
  highlight(0, 'labviewCaseStructure',        { fg = colors.blue,       bg = 'NONE' })  -- Case Structure
  highlight(0, 'labviewEventStructure',       { fg = colors.blue,       bg = 'NONE' })  -- Event Structure
  highlight(0, 'labviewSequence',             { fg = colors.blue,       bg = 'NONE' })  -- Sequence
  highlight(0, 'labviewTimedLoop',            { fg = colors.blue,       bg = 'NONE' })  -- Timed Loop


  -----------------------------------------------------------------------------
  -- Data Types (as they appear in text descriptions)

  highlight(0, 'labviewDataType',             { fg = colors.turquoise,  bg = 'NONE' })  -- Data types
  highlight(0, 'labviewBoolean',              { fg = colors.turquoise,  bg = 'NONE' })  -- Boolean
  highlight(0, 'labviewNumeric',              { fg = colors.turquoise,  bg = 'NONE' })  -- Numeric types
  highlight(0, 'labviewString',               { fg = colors.turquoise,  bg = 'NONE' })  -- String type
  highlight(0, 'labviewArray',                { fg = colors.turquoise,  bg = 'NONE' })  -- Array type
  highlight(0, 'labviewCluster',              { fg = colors.turquoise,  bg = 'NONE' })  -- Cluster type
  highlight(0, 'labviewWaveform',             { fg = colors.turquoise,  bg = 'NONE' })  -- Waveform type
  highlight(0, 'labviewPath',                 { fg = colors.turquoise,  bg = 'NONE' })  -- Path type
  highlight(0, 'labviewRefnum',               { fg = colors.turquoise,  bg = 'NONE' })  -- Refnum type
  highlight(0, 'labviewVariant',              { fg = colors.turquoise,  bg = 'NONE' })  -- Variant type
  highlight(0, 'labviewClass',                { fg = colors.turquoise,  bg = 'NONE' })  -- Class type

end

return labview
