-------------------------------------------------------------------------------
-- Cypher (Neo4j Graph Query Language)
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local cypher    = {}


-------------------------------------------------------------------------------
-- Settings

cypher.setupHighlighting = function()


  ---------------------------------------------------------------------------
  -- Reading Clauses
  ---------------------------------------------------------------------------

  highlight(0, 'cypherKeyword',        { link = "Keyword" })  -- General keywords
  highlight(0, 'cypherClause',         { fg = colors.blue,       bg = 'NONE' })  -- Clause keywords

  -- MATCH
  highlight(0, 'cypherMatch',          { fg = colors.blue,       bg = 'NONE' })  -- MATCH
  highlight(0, 'cypherOptionalMatch',  { fg = colors.blue,       bg = 'NONE' })  -- OPTIONAL MATCH
  highlight(0, 'cypherOptional',       { fg = colors.blue,       bg = 'NONE' })  -- OPTIONAL

  -- WHERE
  highlight(0, 'cypherWhere',          { fg = colors.blue,       bg = 'NONE' })  -- WHERE

  -- RETURN
  highlight(0, 'cypherReturn',         { fg = colors.blue,       bg = 'NONE' })  -- RETURN
  highlight(0, 'cypherAs',             { fg = colors.blue,       bg = 'NONE' })  -- AS
  highlight(0, 'cypherDistinct',       { fg = colors.blue,       bg = 'NONE' })  -- DISTINCT

  -- WITH
  highlight(0, 'cypherWith',           { fg = colors.blue,       bg = 'NONE' })  -- WITH

  -- UNWIND
  highlight(0, 'cypherUnwind',         { fg = colors.blue,       bg = 'NONE' })  -- UNWIND

  -- CALL / YIELD
  highlight(0, 'cypherCall',           { fg = colors.blue,       bg = 'NONE' })  -- CALL
  highlight(0, 'cypherYield',          { fg = colors.blue,       bg = 'NONE' })  -- YIELD

  -- ORDER BY / LIMIT / SKIP
  highlight(0, 'cypherOrderBy',        { fg = colors.blue,       bg = 'NONE' })  -- ORDER BY
  highlight(0, 'cypherOrder',          { fg = colors.blue,       bg = 'NONE' })  -- ORDER
  highlight(0, 'cypherBy',             { fg = colors.blue,       bg = 'NONE' })  -- BY
  highlight(0, 'cypherLimit',          { fg = colors.blue,       bg = 'NONE' })  -- LIMIT
  highlight(0, 'cypherSkip',           { fg = colors.blue,       bg = 'NONE' })  -- SKIP
  highlight(0, 'cypherAsc',            { fg = colors.blue,       bg = 'NONE' })  -- ASC, ASCENDING
  highlight(0, 'cypherDesc',           { fg = colors.blue,       bg = 'NONE' })  -- DESC, DESCENDING

  -- UNION
  highlight(0, 'cypherUnion',          { fg = colors.blue,       bg = 'NONE' })  -- UNION
  highlight(0, 'cypherUnionAll',       { fg = colors.blue,       bg = 'NONE' })  -- UNION ALL

  -- USE (Multi-database)
  highlight(0, 'cypherUse',            { fg = colors.blue,       bg = 'NONE' })  -- USE


  ---------------------------------------------------------------------------
  -- Writing Clauses
  ---------------------------------------------------------------------------

  -- CREATE
  highlight(0, 'cypherCreate',         { fg = colors.blue,       bg = 'NONE' })  -- CREATE

  -- MERGE
  highlight(0, 'cypherMerge',          { fg = colors.blue,       bg = 'NONE' })  -- MERGE
  highlight(0, 'cypherOnCreate',       { fg = colors.blue,       bg = 'NONE' })  -- ON CREATE
  highlight(0, 'cypherOnMatch',        { fg = colors.blue,       bg = 'NONE' })  -- ON MATCH
  highlight(0, 'cypherOn',             { fg = colors.blue,       bg = 'NONE' })  -- ON

  -- DELETE
  highlight(0, 'cypherDelete',         { fg = colors.blue,       bg = 'NONE' })  -- DELETE
  highlight(0, 'cypherDetach',         { fg = colors.blue,       bg = 'NONE' })  -- DETACH
  highlight(0, 'cypherDetachDelete',   { fg = colors.blue,       bg = 'NONE' })  -- DETACH DELETE

  -- SET
  highlight(0, 'cypherSet',            { fg = colors.blue,       bg = 'NONE' })  -- SET

  -- REMOVE
  highlight(0, 'cypherRemove',         { fg = colors.blue,       bg = 'NONE' })  -- REMOVE

  -- FOREACH
  highlight(0, 'cypherForeach',        { fg = colors.blue,       bg = 'NONE' })  -- FOREACH
  highlight(0, 'cypherIn',             { fg = colors.blue,       bg = 'NONE' })  -- IN


  ---------------------------------------------------------------------------
  -- Data Import
  ---------------------------------------------------------------------------

  highlight(0, 'cypherLoadCsv',        { fg = colors.blue,       bg = 'NONE' })  -- LOAD CSV
  highlight(0, 'cypherLoad',           { fg = colors.blue,       bg = 'NONE' })  -- LOAD
  highlight(0, 'cypherCsv',            { fg = colors.blue,       bg = 'NONE' })  -- CSV
  highlight(0, 'cypherFrom',           { fg = colors.blue,       bg = 'NONE' })  -- FROM
  highlight(0, 'cypherWithHeaders',    { fg = colors.blue,       bg = 'NONE' })  -- WITH HEADERS
  highlight(0, 'cypherHeaders',        { fg = colors.blue,       bg = 'NONE' })  -- HEADERS
  highlight(0, 'cypherFieldterm',      { fg = colors.blue,       bg = 'NONE' })  -- FIELDTERMINATOR


  ---------------------------------------------------------------------------
  -- Schema / Index / Constraint
  ---------------------------------------------------------------------------

  highlight(0, 'cypherIndex',          { fg = colors.blue,       bg = 'NONE' })  -- INDEX
  highlight(0, 'cypherConstraint',     { fg = colors.blue,       bg = 'NONE' })  -- CONSTRAINT
  highlight(0, 'cypherUnique',         { fg = colors.pink,       bg = 'NONE' })  -- UNIQUE
  highlight(0, 'cypherExists',         { fg = colors.blue,       bg = 'NONE' })  -- EXISTS
  highlight(0, 'cypherAssert',         { fg = colors.blue,       bg = 'NONE' })  -- ASSERT
  highlight(0, 'cypherDrop',           { fg = colors.blue,       bg = 'NONE' })  -- DROP
  highlight(0, 'cypherShow',           { fg = colors.blue,       bg = 'NONE' })  -- SHOW


  ---------------------------------------------------------------------------
  -- Conditional
  ---------------------------------------------------------------------------

  highlight(0, 'cypherCase',           { fg = colors.blue,       bg = 'NONE' })  -- CASE
  highlight(0, 'cypherWhen',           { fg = colors.blue,       bg = 'NONE' })  -- WHEN
  highlight(0, 'cypherThen',           { fg = colors.blue,       bg = 'NONE' })  -- THEN
  highlight(0, 'cypherElse',           { fg = colors.blue,       bg = 'NONE' })  -- ELSE
  highlight(0, 'cypherEnd',            { fg = colors.blue,       bg = 'NONE' })  -- END


  ---------------------------------------------------------------------------
  -- Logical Operators
  ---------------------------------------------------------------------------

  highlight(0, 'cypherOperator',       { link = "Operator" })  -- Logical operators
  highlight(0, 'cypherAnd',            { fg = colors.blue,       bg = 'NONE' })  -- AND
  highlight(0, 'cypherOr',             { fg = colors.blue,       bg = 'NONE' })  -- OR
  highlight(0, 'cypherNot',            { fg = colors.blue,       bg = 'NONE' })  -- NOT
  highlight(0, 'cypherXor',            { fg = colors.blue,       bg = 'NONE' })  -- XOR

  -- NULL Checks
  highlight(0, 'cypherIsNull',         { fg = colors.blue,       bg = 'NONE' })  -- IS NULL
  highlight(0, 'cypherIsNotNull',      { fg = colors.blue,       bg = 'NONE' })  -- IS NOT NULL
  highlight(0, 'cypherIs',             { fg = colors.blue,       bg = 'NONE' })  -- IS

  -- String Operators
  highlight(0, 'cypherStartsWith',     { fg = colors.blue,       bg = 'NONE' })  -- STARTS WITH
  highlight(0, 'cypherEndsWith',       { fg = colors.blue,       bg = 'NONE' })  -- ENDS WITH
  highlight(0, 'cypherContains',       { fg = colors.blue,       bg = 'NONE' })  -- CONTAINS
  highlight(0, 'cypherStarts',         { fg = colors.blue,       bg = 'NONE' })  -- STARTS
  highlight(0, 'cypherEnds',           { fg = colors.blue,       bg = 'NONE' })  -- ENDS


  ---------------------------------------------------------------------------
  -- Comparison & Arithmetic Operators
  ---------------------------------------------------------------------------

  highlight(0, 'cypherSymbol',         { fg = colors.white,      bg = 'NONE' })  -- Symbols/Operators
  highlight(0, 'cypherComparison',     { fg = colors.white,      bg = 'NONE' })  -- =, <>, <, >, <=, >=
  highlight(0, 'cypherArithmetic',     { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, %, ^
  highlight(0, 'cypherAssignment',     { fg = colors.white,      bg = 'NONE' })  -- =
  highlight(0, 'cypherPlusEquals',     { fg = colors.white,      bg = 'NONE' })  -- +=
  highlight(0, 'cypherRegex',          { fg = colors.white,      bg = 'NONE' })  -- =~


  ---------------------------------------------------------------------------
  -- Pattern Elements
  ---------------------------------------------------------------------------

  -- Nodes
  highlight(0, 'cypherNode',           { fg = colors.white,      bg = 'NONE' })  -- Node pattern ()
  highlight(0, 'cypherNodeParens',     { fg = colors.white,      bg = 'NONE' })  -- ( )

  -- Relationships
  highlight(0, 'cypherRelationship',   { fg = colors.turquoise,  bg = 'NONE' })  -- Relationship pattern
  highlight(0, 'cypherRelBrackets',    { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'cypherRelArrow',       { fg = colors.white,      bg = 'NONE' })  -- ->, <-, --, -->
  highlight(0, 'cypherRelDash',        { fg = colors.white,      bg = 'NONE' })  -- -

  -- Path Patterns
  highlight(0, 'cypherPath',           { fg = colors.turquoise,  bg = 'NONE' })  -- Path assignments
  highlight(0, 'cypherPathPattern',    { fg = colors.white,      bg = 'NONE' })  -- Path patterns
  highlight(0, 'cypherShortestPath',   { fg = colors.orange,     bg = 'NONE' })  -- shortestPath
  highlight(0, 'cypherAllShortestPaths', { fg = colors.orange,   bg = 'NONE' })  -- allShortestPaths


  ---------------------------------------------------------------------------
  -- Labels & Types
  ---------------------------------------------------------------------------

  highlight(0, 'cypherLabel',          { fg = colors.turquoise,  bg = 'NONE' })  -- :Label
  highlight(0, 'cypherLabelColon',     { fg = colors.white,      bg = 'NONE' })  -- : prefix
  highlight(0, 'cypherRelType',        { link = "Type" })  -- :RELATIONSHIP_TYPE
  highlight(0, 'cypherMultiLabel',     { fg = colors.turquoise,  bg = 'NONE' })  -- :Label1:Label2


  ---------------------------------------------------------------------------
  -- Properties
  ---------------------------------------------------------------------------

  highlight(0, 'cypherProperty',       { fg = colors.redLight,   bg = 'NONE' })  -- Property access
  highlight(0, 'cypherPropertyName',   { fg = colors.orange,     bg = 'NONE' })  -- Property names
  highlight(0, 'cypherPropertyBraces', { fg = colors.white,      bg = 'NONE' })  -- { }
  highlight(0, 'cypherPropertyKey',    { fg = colors.orange,     bg = 'NONE' })  -- Key in {key: value}
  highlight(0, 'cypherPropertyColon',  { fg = colors.white,      bg = 'NONE' })  -- : in properties


  ---------------------------------------------------------------------------
  -- Variables
  ---------------------------------------------------------------------------

  highlight(0, 'cypherVariable',       { link = "Variable" })  -- Variables
  highlight(0, 'cypherNodeVariable',   { link = "Variable" })  -- Node variables
  highlight(0, 'cypherRelVariable',    { link = "Variable" })  -- Relationship variables
  highlight(0, 'cypherPathVariable',   { link = "Variable" })  -- Path variables


  ---------------------------------------------------------------------------
  -- Parameters
  ---------------------------------------------------------------------------

  highlight(0, 'cypherParameter',      { fg = colors.purple,     bg = 'NONE' })  -- $param
  highlight(0, 'cypherParamDollar',    { fg = colors.purple,     bg = 'NONE' })  -- $ prefix
  highlight(0, 'cypherParamName',      { fg = colors.purple,     bg = 'NONE' })  -- Parameter name


  ---------------------------------------------------------------------------
  -- Built-in Functions
  ---------------------------------------------------------------------------

  highlight(0, 'cypherFunction',       { link = "Function" })  -- Functions
  highlight(0, 'cypherFunctionCall',   { link = "Function" })  -- Function calls

  -- Aggregation Functions
  highlight(0, 'cypherAggFunc',        { link = "Function" })  -- Aggregation functions
  highlight(0, 'cypherCount',          { fg = colors.orange,     bg = 'NONE' })  -- count()
  highlight(0, 'cypherSum',            { fg = colors.orange,     bg = 'NONE' })  -- sum()
  highlight(0, 'cypherAvg',            { fg = colors.orange,     bg = 'NONE' })  -- avg()
  highlight(0, 'cypherMin',            { fg = colors.orange,     bg = 'NONE' })  -- min()
  highlight(0, 'cypherMax',            { fg = colors.orange,     bg = 'NONE' })  -- max()
  highlight(0, 'cypherCollect',        { fg = colors.orange,     bg = 'NONE' })  -- collect()
  highlight(0, 'cypherStdev',          { fg = colors.orange,     bg = 'NONE' })  -- stdev(), stdevp()
  highlight(0, 'cypherPercentile',     { fg = colors.orange,     bg = 'NONE' })  -- percentileCont(), percentileDisc()

  -- Scalar Functions
  highlight(0, 'cypherScalarFunc',     { link = "Function" })  -- Scalar functions
  highlight(0, 'cypherSize',           { fg = colors.orange,     bg = 'NONE' })  -- size()
  highlight(0, 'cypherLength',         { fg = colors.orange,     bg = 'NONE' })  -- length()
  highlight(0, 'cypherType',           { link = "Type" })  -- type()
  highlight(0, 'cypherId',             { fg = colors.orange,     bg = 'NONE' })  -- id()
  highlight(0, 'cypherElementId',      { fg = colors.orange,     bg = 'NONE' })  -- elementId()
  highlight(0, 'cypherLabels',         { fg = colors.orange,     bg = 'NONE' })  -- labels()
  highlight(0, 'cypherProperties',     { fg = colors.orange,     bg = 'NONE' })  -- properties()
  highlight(0, 'cypherKeys',           { fg = colors.orange,     bg = 'NONE' })  -- keys()
  highlight(0, 'cypherNodes',          { fg = colors.orange,     bg = 'NONE' })  -- nodes()
  highlight(0, 'cypherRelationships',  { fg = colors.orange,     bg = 'NONE' })  -- relationships()
  highlight(0, 'cypherStartNode',      { fg = colors.orange,     bg = 'NONE' })  -- startNode()
  highlight(0, 'cypherEndNode',        { fg = colors.orange,     bg = 'NONE' })  -- endNode()
  highlight(0, 'cypherCoalesce',       { fg = colors.orange,     bg = 'NONE' })  -- coalesce()
  highlight(0, 'cypherHead',           { fg = colors.orange,     bg = 'NONE' })  -- head()
  highlight(0, 'cypherLast',           { fg = colors.orange,     bg = 'NONE' })  -- last()
  highlight(0, 'cypherTail',           { fg = colors.orange,     bg = 'NONE' })  -- tail()

  -- String Functions
  highlight(0, 'cypherStringFunc',     { link = "String" })  -- String functions
  highlight(0, 'cypherToString',       { link = "String" })  -- toString()
  highlight(0, 'cypherToUpper',        { fg = colors.orange,     bg = 'NONE' })  -- toUpper()
  highlight(0, 'cypherToLower',        { fg = colors.orange,     bg = 'NONE' })  -- toLower()
  highlight(0, 'cypherTrim',           { fg = colors.orange,     bg = 'NONE' })  -- trim(), ltrim(), rtrim()
  highlight(0, 'cypherReplace',        { fg = colors.orange,     bg = 'NONE' })  -- replace()
  highlight(0, 'cypherSubstring',      { link = "String" })  -- substring()
  highlight(0, 'cypherSplit',          { fg = colors.orange,     bg = 'NONE' })  -- split()
  highlight(0, 'cypherReverse',        { fg = colors.orange,     bg = 'NONE' })  -- reverse()
  highlight(0, 'cypherLeft',           { fg = colors.orange,     bg = 'NONE' })  -- left()
  highlight(0, 'cypherRight',          { fg = colors.orange,     bg = 'NONE' })  -- right()

  -- Math Functions
  highlight(0, 'cypherMathFunc',       { link = "Function" })  -- Math functions
  highlight(0, 'cypherAbs',            { fg = colors.orange,     bg = 'NONE' })  -- abs()
  highlight(0, 'cypherCeil',           { fg = colors.orange,     bg = 'NONE' })  -- ceil()
  highlight(0, 'cypherFloor',          { fg = colors.orange,     bg = 'NONE' })  -- floor()
  highlight(0, 'cypherRound',          { fg = colors.orange,     bg = 'NONE' })  -- round()
  highlight(0, 'cypherSign',           { fg = colors.orange,     bg = 'NONE' })  -- sign()
  highlight(0, 'cypherSqrt',           { fg = colors.orange,     bg = 'NONE' })  -- sqrt()
  highlight(0, 'cypherRand',           { fg = colors.orange,     bg = 'NONE' })  -- rand()
  highlight(0, 'cypherLog',            { fg = colors.orange,     bg = 'NONE' })  -- log(), log10()
  highlight(0, 'cypherExp',            { fg = colors.orange,     bg = 'NONE' })  -- exp()
  highlight(0, 'cypherPow',            { fg = colors.orange,     bg = 'NONE' })  -- pow() (same as ^)
  highlight(0, 'cypherSin',            { fg = colors.orange,     bg = 'NONE' })  -- sin(), cos(), tan(), etc.

  -- List Functions
  highlight(0, 'cypherListFunc',       { link = "Function" })  -- List functions
  highlight(0, 'cypherRange',          { fg = colors.orange,     bg = 'NONE' })  -- range()
  highlight(0, 'cypherReduceFunc',     { link = "Function" })  -- reduce()
  highlight(0, 'cypherFilter',         { fg = colors.orange,     bg = 'NONE' })  -- filter() (deprecated)
  highlight(0, 'cypherExtract',        { fg = colors.orange,     bg = 'NONE' })  -- extract() (deprecated)

  -- Predicate Functions
  highlight(0, 'cypherPredicateFunc',  { link = "Function" })  -- Predicate functions
  highlight(0, 'cypherAll',            { fg = colors.orange,     bg = 'NONE' })  -- all()
  highlight(0, 'cypherAny',            { fg = colors.orange,     bg = 'NONE' })  -- any()
  highlight(0, 'cypherNone',           { fg = colors.orange,     bg = 'NONE' })  -- none()
  highlight(0, 'cypherSingle',         { fg = colors.orange,     bg = 'NONE' })  -- single()
  highlight(0, 'cypherExistsFunc',     { link = "Function" })  -- exists()

  -- Temporal Functions
  highlight(0, 'cypherTemporalFunc',   { link = "Function" })  -- Temporal functions
  highlight(0, 'cypherDate',           { fg = colors.orange,     bg = 'NONE' })  -- date()
  highlight(0, 'cypherDatetime',       { fg = colors.orange,     bg = 'NONE' })  -- datetime()
  highlight(0, 'cypherLocalDatetime',  { fg = colors.orange,     bg = 'NONE' })  -- localdatetime()
  highlight(0, 'cypherTime',           { fg = colors.orange,     bg = 'NONE' })  -- time()
  highlight(0, 'cypherLocalTime',      { fg = colors.orange,     bg = 'NONE' })  -- localtime()
  highlight(0, 'cypherDuration',       { fg = colors.orange,     bg = 'NONE' })  -- duration()

  -- Spatial Functions
  highlight(0, 'cypherSpatialFunc',    { link = "Function" })  -- Spatial functions
  highlight(0, 'cypherPoint',          { fg = colors.orange,     bg = 'NONE' })  -- point()
  highlight(0, 'cypherDistance',       { fg = colors.orange,     bg = 'NONE' })  -- distance()


  ---------------------------------------------------------------------------
  -- Data Types
  ---------------------------------------------------------------------------

  highlight(0, 'cypherType',           { link = "Type" })  -- Type names
  highlight(0, 'cypherTypeString',     { link = "String" })  -- STRING
  highlight(0, 'cypherTypeInteger',    { link = "Type" })  -- INTEGER
  highlight(0, 'cypherTypeFloat',      { link = "Type" })  -- FLOAT
  highlight(0, 'cypherTypeBoolean',    { link = "Boolean" })  -- BOOLEAN
  highlight(0, 'cypherTypeDate',       { link = "Type" })  -- DATE
  highlight(0, 'cypherTypeDatetime',   { link = "Type" })  -- DATETIME
  highlight(0, 'cypherTypeDuration',   { link = "Type" })  -- DURATION
  highlight(0, 'cypherTypePoint',      { link = "Type" })  -- POINT
  highlight(0, 'cypherTypeList',       { link = "Type" })  -- LIST
  highlight(0, 'cypherTypeMap',        { link = "Type" })  -- MAP
  highlight(0, 'cypherTypeNode',       { link = "Type" })  -- NODE
  highlight(0, 'cypherTypeRelationship', { link = "Type" })  -- RELATIONSHIP
  highlight(0, 'cypherTypePath',       { link = "Type" })  -- PATH


  ---------------------------------------------------------------------------
  -- Literals
  ---------------------------------------------------------------------------

  -- Strings
  highlight(0, 'cypherString',         { link = "String" })  -- 'string' or "string"
  highlight(0, 'cypherStringContent',  { link = "String" })  -- String content
  highlight(0, 'cypherEscape',         { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences \n, \t

  -- Numbers
  highlight(0, 'cypherNumber',         { link = "Number" })  -- Numeric literals
  highlight(0, 'cypherInteger',        { fg = colors.greenLight, bg = 'NONE' })  -- Integer literals
  highlight(0, 'cypherFloat',          { fg = colors.greenLight, bg = 'NONE' })  -- Float literals
  highlight(0, 'cypherHex',            { fg = colors.greenLight, bg = 'NONE' })  -- Hex numbers 0x...
  highlight(0, 'cypherOctal',          { fg = colors.greenLight, bg = 'NONE' })  -- Octal numbers 0o...

  -- Booleans
  highlight(0, 'cypherBoolean',        { link = "Boolean" })  -- true, false
  highlight(0, 'cypherTrue',           { fg = colors.blue,       bg = 'NONE' })  -- true
  highlight(0, 'cypherFalse',          { fg = colors.blue,       bg = 'NONE' })  -- false

  -- Null
  highlight(0, 'cypherNull',           { fg = colors.pink,       bg = 'NONE' })  -- null


  ---------------------------------------------------------------------------
  -- Comments
  ---------------------------------------------------------------------------

  highlight(0, 'cypherComment',        { link = "Comment" })  -- Comments
  highlight(0, 'cypherLineComment',    { link = "Comment" })  -- // line comment
  highlight(0, 'cypherBlockComment',   { link = "Comment" })  -- /* block comment */
  highlight(0, 'cypherTodo',           { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME


  ---------------------------------------------------------------------------
  -- Punctuation & Delimiters
  ---------------------------------------------------------------------------

  highlight(0, 'cypherPunctuation',    { fg = colors.white,      bg = 'NONE' })  -- Punctuation
  highlight(0, 'cypherParens',         { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'cypherBrackets',       { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'cypherBraces',         { fg = colors.white,      bg = 'NONE' })  -- { }
  highlight(0, 'cypherComma',          { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'cypherSemicolon',      { fg = colors.white,      bg = 'NONE' })  -- ;
  highlight(0, 'cypherDot',            { fg = colors.white,      bg = 'NONE' })  -- .
  highlight(0, 'cypherPipe',           { fg = colors.white,      bg = 'NONE' })  -- |


  ---------------------------------------------------------------------------
  -- List Comprehension
  ---------------------------------------------------------------------------

  highlight(0, 'cypherListComp',       { fg = colors.white,      bg = 'NONE' })  -- List comprehension
  highlight(0, 'cypherListCompIn',     { fg = colors.blue,       bg = 'NONE' })  -- IN (list comp)
  highlight(0, 'cypherListCompWhere',  { fg = colors.blue,       bg = 'NONE' })  -- WHERE (list comp)
  highlight(0, 'cypherListCompPipe',   { fg = colors.white,      bg = 'NONE' })  -- | (list comp)


  ---------------------------------------------------------------------------
  -- Pattern Comprehension
  ---------------------------------------------------------------------------

  highlight(0, 'cypherPatternComp',    { fg = colors.white,      bg = 'NONE' })  -- Pattern comprehension


  ---------------------------------------------------------------------------
  -- Quantified Path Patterns (Neo4j 5+)
  ---------------------------------------------------------------------------

  highlight(0, 'cypherQuantifier',     { fg = colors.pink,       bg = 'NONE' })  -- *, +, ?
  highlight(0, 'cypherQuantRange',     { fg = colors.pink,       bg = 'NONE' })  -- {1,5}, {2,}


  ---------------------------------------------------------------------------
  -- Subquery Keywords
  ---------------------------------------------------------------------------

  highlight(0, 'cypherExistsSubquery', { fg = colors.blue,       bg = 'NONE' })  -- EXISTS { }
  highlight(0, 'cypherCountSubquery',  { fg = colors.blue,       bg = 'NONE' })  -- COUNT { }
  highlight(0, 'cypherCollectSubquery', { fg = colors.blue,      bg = 'NONE' })  -- COLLECT { }


  ---------------------------------------------------------------------------
  -- Transaction Control
  ---------------------------------------------------------------------------

  highlight(0, 'cypherTransaction',    { fg = colors.blue,       bg = 'NONE' })  -- Transaction keywords
  highlight(0, 'cypherBegin',          { fg = colors.blue,       bg = 'NONE' })  -- BEGIN
  highlight(0, 'cypherCommit',         { fg = colors.blue,       bg = 'NONE' })  -- COMMIT
  highlight(0, 'cypherRollback',       { fg = colors.blue,       bg = 'NONE' })  -- ROLLBACK


  ---------------------------------------------------------------------------
  -- Administration Commands
  ---------------------------------------------------------------------------

  highlight(0, 'cypherAdmin',          { fg = colors.blue,       bg = 'NONE' })  -- Admin keywords
  highlight(0, 'cypherShowDatabases',  { fg = colors.blue,       bg = 'NONE' })  -- SHOW DATABASES
  highlight(0, 'cypherCreateDatabase', { fg = colors.blue,       bg = 'NONE' })  -- CREATE DATABASE
  highlight(0, 'cypherDropDatabase',   { fg = colors.blue,       bg = 'NONE' })  -- DROP DATABASE
  highlight(0, 'cypherStartDatabase',  { fg = colors.blue,       bg = 'NONE' })  -- START DATABASE
  highlight(0, 'cypherStopDatabase',   { fg = colors.blue,       bg = 'NONE' })  -- STOP DATABASE

  -- User Management
  highlight(0, 'cypherCreateUser',     { fg = colors.blue,       bg = 'NONE' })  -- CREATE USER
  highlight(0, 'cypherDropUser',       { fg = colors.blue,       bg = 'NONE' })  -- DROP USER
  highlight(0, 'cypherAlterUser',      { fg = colors.blue,       bg = 'NONE' })  -- ALTER USER
  highlight(0, 'cypherShowUsers',      { fg = colors.blue,       bg = 'NONE' })  -- SHOW USERS

  -- Role Management
  highlight(0, 'cypherCreateRole',     { fg = colors.blue,       bg = 'NONE' })  -- CREATE ROLE
  highlight(0, 'cypherDropRole',       { fg = colors.blue,       bg = 'NONE' })  -- DROP ROLE
  highlight(0, 'cypherGrant',          { fg = colors.blue,       bg = 'NONE' })  -- GRANT
  highlight(0, 'cypherRevoke',         { fg = colors.blue,       bg = 'NONE' })  -- REVOKE
  highlight(0, 'cypherDeny',           { fg = colors.blue,       bg = 'NONE' })  -- DENY


  ---------------------------------------------------------------------------
  -- APOC Functions (Common Neo4j Library)
  ---------------------------------------------------------------------------

  highlight(0, 'cypherApoc',           { fg = colors.turquoise,  bg = 'NONE' })  -- apoc namespace
  highlight(0, 'cypherApocFunction',   { link = "Function" })  -- apoc.* functions
  highlight(0, 'cypherApocProcedure',  { fg = colors.orange,     bg = 'NONE' })  -- apoc.* procedures


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.cypher)
  ---------------------------------------------------------------------------

  -- Keywords
  highlight(0, '@keyword.cypher',                { link = "Keyword" })  -- Keywords
  highlight(0, '@keyword.operator.cypher',       { link = "Operator" })  -- AND, OR, NOT
  highlight(0, '@keyword.conditional.cypher',    { link = "Conditional" })  -- CASE, WHEN
  highlight(0, '@keyword.return.cypher',         { link = "Keyword" })  -- RETURN

  -- Variables
  highlight(0, '@variable.cypher',               { link = "Variable" })  -- Variables
  highlight(0, '@variable.parameter.cypher',     { link = "Variable" })  -- $parameters

  -- Types
  highlight(0, '@type.cypher',                   { link = "Type" })  -- Types/Labels

  -- Functions
  highlight(0, '@function.cypher',               { link = "Function" })  -- Functions
  highlight(0, '@function.call.cypher',          { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.cypher',       { link = "Function" })  -- Built-in functions

  -- Properties
  highlight(0, '@property.cypher',               { fg = colors.orange,     bg = 'NONE' })  -- Properties

  -- Strings
  highlight(0, '@string.cypher',                 { link = "String" })  -- Strings
  highlight(0, '@string.escape.cypher',          { link = "String" })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.cypher',                 { link = "Number" })  -- Numbers
  highlight(0, '@number.float.cypher',           { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.cypher',                { link = "Boolean" })  -- true, false

  -- Constants
  highlight(0, '@constant.builtin.cypher',       { link = "Constant" })  -- null

  -- Comments
  highlight(0, '@comment.cypher',                { link = "Comment" })  -- Comments

  -- Operators & Punctuation
  highlight(0, '@operator.cypher',               { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.cypher',    { fg = colors.white,      bg = 'NONE' })  -- ( ) [ ] { }
  highlight(0, '@punctuation.delimiter.cypher',  { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.cypher',    { fg = colors.white,      bg = 'NONE' })  -- -> <- --


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.cypher)
  ---------------------------------------------------------------------------

  highlight(0, '@lsp.type.variable.cypher',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.function.cypher',      { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.parameter.cypher',     { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.type.cypher',          { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.property.cypher',      { fg = colors.orange,     bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.keyword.cypher',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.string.cypher',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.cypher',        { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.comment.cypher',       { link = "Comment" })  -- Comments

end

return cypher
