-------------------------------------------------------------------------------
-- SQL (Standard SQL, MySQL, PostgreSQL, SQLite, Oracle, SQL Server)
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local sql       = {}


-------------------------------------------------------------------------------
-- Settings

sql.setupHighlighting = function()


  ---------------------------------------------------------------------------
  -- Vim Legacy Syntax Groups (Generic SQL)
  ---------------------------------------------------------------------------

  -- Statements (DML/DDL/DCL)
  highlight(0, 'sqlStatement',         { fg = colors.blue,       bg = 'NONE' })  -- SELECT, INSERT, UPDATE, DELETE
  highlight(0, 'sqlKeyword',           { link = "Keyword" })  -- General keywords
  highlight(0, 'sqlKeywordTop',        { link = "Keyword" })  -- TOP keyword

  -- Data Definition
  highlight(0, 'sqlCreate',            { fg = colors.blue,       bg = 'NONE' })  -- CREATE
  highlight(0, 'sqlDrop',              { fg = colors.blue,       bg = 'NONE' })  -- DROP
  highlight(0, 'sqlAlter',             { fg = colors.blue,       bg = 'NONE' })  -- ALTER
  highlight(0, 'sqlTruncate',          { fg = colors.blue,       bg = 'NONE' })  -- TRUNCATE

  -- Data Manipulation
  highlight(0, 'sqlSelect',            { fg = colors.blue,       bg = 'NONE' })  -- SELECT
  highlight(0, 'sqlInsert',            { fg = colors.blue,       bg = 'NONE' })  -- INSERT
  highlight(0, 'sqlUpdate',            { fg = colors.blue,       bg = 'NONE' })  -- UPDATE
  highlight(0, 'sqlDelete',            { fg = colors.blue,       bg = 'NONE' })  -- DELETE
  highlight(0, 'sqlMerge',             { fg = colors.blue,       bg = 'NONE' })  -- MERGE

  -- Clauses
  highlight(0, 'sqlFrom',              { fg = colors.blue,       bg = 'NONE' })  -- FROM
  highlight(0, 'sqlWhere',             { fg = colors.blue,       bg = 'NONE' })  -- WHERE
  highlight(0, 'sqlHaving',            { fg = colors.blue,       bg = 'NONE' })  -- HAVING
  highlight(0, 'sqlGroupBy',           { fg = colors.blue,       bg = 'NONE' })  -- GROUP BY
  highlight(0, 'sqlOrderBy',           { fg = colors.blue,       bg = 'NONE' })  -- ORDER BY
  highlight(0, 'sqlLimit',             { fg = colors.blue,       bg = 'NONE' })  -- LIMIT
  highlight(0, 'sqlOffset',            { fg = colors.blue,       bg = 'NONE' })  -- OFFSET
  highlight(0, 'sqlInto',              { fg = colors.blue,       bg = 'NONE' })  -- INTO
  highlight(0, 'sqlValues',            { fg = colors.blue,       bg = 'NONE' })  -- VALUES
  highlight(0, 'sqlSet',               { fg = colors.blue,       bg = 'NONE' })  -- SET

  -- Joins
  highlight(0, 'sqlJoin',              { fg = colors.blue,       bg = 'NONE' })  -- JOIN
  highlight(0, 'sqlInnerJoin',         { fg = colors.blue,       bg = 'NONE' })  -- INNER JOIN
  highlight(0, 'sqlLeftJoin',          { fg = colors.blue,       bg = 'NONE' })  -- LEFT JOIN
  highlight(0, 'sqlRightJoin',         { fg = colors.blue,       bg = 'NONE' })  -- RIGHT JOIN
  highlight(0, 'sqlOuterJoin',         { fg = colors.blue,       bg = 'NONE' })  -- OUTER JOIN
  highlight(0, 'sqlCrossJoin',         { fg = colors.blue,       bg = 'NONE' })  -- CROSS JOIN
  highlight(0, 'sqlNaturalJoin',       { fg = colors.blue,       bg = 'NONE' })  -- NATURAL JOIN
  highlight(0, 'sqlOn',                { fg = colors.blue,       bg = 'NONE' })  -- ON
  highlight(0, 'sqlUsing',             { fg = colors.blue,       bg = 'NONE' })  -- USING

  -- Subqueries & CTEs
  highlight(0, 'sqlWith',              { fg = colors.blue,       bg = 'NONE' })  -- WITH (CTE)
  highlight(0, 'sqlAs',                { fg = colors.blue,       bg = 'NONE' })  -- AS
  highlight(0, 'sqlRecursive',         { fg = colors.blue,       bg = 'NONE' })  -- RECURSIVE

  -- Set Operations
  highlight(0, 'sqlUnion',             { fg = colors.blue,       bg = 'NONE' })  -- UNION
  highlight(0, 'sqlIntersect',         { fg = colors.blue,       bg = 'NONE' })  -- INTERSECT
  highlight(0, 'sqlExcept',            { fg = colors.blue,       bg = 'NONE' })  -- EXCEPT
  highlight(0, 'sqlMinus',             { fg = colors.blue,       bg = 'NONE' })  -- MINUS (Oracle)

  -- Transaction Control
  highlight(0, 'sqlTransaction',       { fg = colors.blue,       bg = 'NONE' })  -- Transaction keywords
  highlight(0, 'sqlBegin',             { fg = colors.blue,       bg = 'NONE' })  -- BEGIN
  highlight(0, 'sqlCommit',            { fg = colors.blue,       bg = 'NONE' })  -- COMMIT
  highlight(0, 'sqlRollback',          { fg = colors.blue,       bg = 'NONE' })  -- ROLLBACK
  highlight(0, 'sqlSavepoint',         { fg = colors.blue,       bg = 'NONE' })  -- SAVEPOINT

  -- Access Control
  highlight(0, 'sqlGrant',             { fg = colors.blue,       bg = 'NONE' })  -- GRANT
  highlight(0, 'sqlRevoke',            { fg = colors.blue,       bg = 'NONE' })  -- REVOKE

  -- Conditional
  highlight(0, 'sqlConditional',       { link = "Conditional" })  -- CASE, WHEN, THEN, ELSE, END
  highlight(0, 'sqlCase',              { fg = colors.blue,       bg = 'NONE' })  -- CASE
  highlight(0, 'sqlWhen',              { fg = colors.blue,       bg = 'NONE' })  -- WHEN
  highlight(0, 'sqlThen',              { fg = colors.blue,       bg = 'NONE' })  -- THEN
  highlight(0, 'sqlElse',              { fg = colors.blue,       bg = 'NONE' })  -- ELSE
  highlight(0, 'sqlEnd',               { fg = colors.blue,       bg = 'NONE' })  -- END

  -- Operators
  highlight(0, 'sqlOperator',          { link = "Operator" })  -- AND, OR, NOT, IN, LIKE, etc.
  highlight(0, 'sqlLogical',           { fg = colors.blue,       bg = 'NONE' })  -- AND, OR, NOT
  highlight(0, 'sqlAnd',               { fg = colors.blue,       bg = 'NONE' })  -- AND
  highlight(0, 'sqlOr',                { fg = colors.blue,       bg = 'NONE' })  -- OR
  highlight(0, 'sqlNot',               { fg = colors.blue,       bg = 'NONE' })  -- NOT
  highlight(0, 'sqlIn',                { fg = colors.blue,       bg = 'NONE' })  -- IN
  highlight(0, 'sqlBetween',           { fg = colors.blue,       bg = 'NONE' })  -- BETWEEN
  highlight(0, 'sqlLike',              { fg = colors.blue,       bg = 'NONE' })  -- LIKE
  highlight(0, 'sqlIs',                { fg = colors.blue,       bg = 'NONE' })  -- IS
  highlight(0, 'sqlExists',            { fg = colors.blue,       bg = 'NONE' })  -- EXISTS
  highlight(0, 'sqlAll',               { fg = colors.blue,       bg = 'NONE' })  -- ALL
  highlight(0, 'sqlAny',               { fg = colors.blue,       bg = 'NONE' })  -- ANY
  highlight(0, 'sqlSome',              { fg = colors.blue,       bg = 'NONE' })  -- SOME
  highlight(0, 'sqlDistinct',          { fg = colors.blue,       bg = 'NONE' })  -- DISTINCT

  -- Comparison
  highlight(0, 'sqlComparison',        { fg = colors.white,      bg = 'NONE' })  -- =, <>, <, >, <=, >=
  highlight(0, 'sqlArithmetic',        { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, %


  ---------------------------------------------------------------------------
  -- Types
  ---------------------------------------------------------------------------

  highlight(0, 'sqlType',              { link = "Type" })  -- Data types

  -- Numeric Types
  highlight(0, 'sqlTypeInt',           { link = "Type" })  -- INT, INTEGER, SMALLINT, BIGINT
  highlight(0, 'sqlTypeNumeric',       { link = "Type" })  -- NUMERIC, DECIMAL
  highlight(0, 'sqlTypeFloat',         { link = "Type" })  -- FLOAT, REAL, DOUBLE
  highlight(0, 'sqlTypeMoney',         { link = "Type" })  -- MONEY, SMALLMONEY

  -- String Types
  highlight(0, 'sqlTypeChar',          { link = "Type" })  -- CHAR, CHARACTER
  highlight(0, 'sqlTypeVarchar',       { link = "Type" })  -- VARCHAR
  highlight(0, 'sqlTypeText',          { link = "Type" })  -- TEXT
  highlight(0, 'sqlTypeNchar',         { link = "Type" })  -- NCHAR, NVARCHAR

  -- Date/Time Types
  highlight(0, 'sqlTypeDate',          { link = "Type" })  -- DATE
  highlight(0, 'sqlTypeTime',          { link = "Type" })  -- TIME
  highlight(0, 'sqlTypeDatetime',      { link = "Type" })  -- DATETIME, TIMESTAMP
  highlight(0, 'sqlTypeInterval',      { link = "Type" })  -- INTERVAL

  -- Binary Types
  highlight(0, 'sqlTypeBinary',        { link = "Type" })  -- BINARY, VARBINARY
  highlight(0, 'sqlTypeBlob',          { link = "Type" })  -- BLOB
  highlight(0, 'sqlTypeClob',          { link = "Type" })  -- CLOB

  -- Other Types
  highlight(0, 'sqlTypeBool',          { link = "Type" })  -- BOOLEAN, BOOL
  highlight(0, 'sqlTypeUuid',          { link = "Type" })  -- UUID
  highlight(0, 'sqlTypeJson',          { link = "Type" })  -- JSON, JSONB
  highlight(0, 'sqlTypeXml',           { link = "Type" })  -- XML
  highlight(0, 'sqlTypeArray',         { link = "Type" })  -- ARRAY


  ---------------------------------------------------------------------------
  -- Identifiers & Objects
  ---------------------------------------------------------------------------

  -- Tables & Views
  highlight(0, 'sqlTable',             { fg = colors.turquoise,  bg = 'NONE' })  -- Table names
  highlight(0, 'sqlView',              { fg = colors.turquoise,  bg = 'NONE' })  -- View names
  highlight(0, 'sqlAlias',             { fg = colors.white,      bg = 'NONE' })  -- Table/column aliases

  -- Columns & Fields
  highlight(0, 'sqlColumn',            { fg = colors.white,      bg = 'NONE' })  -- Column names
  highlight(0, 'sqlField',             { fg = colors.white,      bg = 'NONE' })  -- Field names

  -- Indexes & Constraints
  highlight(0, 'sqlIndex',             { fg = colors.turquoise,  bg = 'NONE' })  -- Index names
  highlight(0, 'sqlConstraint',        { fg = colors.pink,       bg = 'NONE' })  -- Constraint names

  -- Identifiers
  highlight(0, 'sqlIdentifier',        { fg = colors.white,      bg = 'NONE' })  -- General identifiers
  highlight(0, 'sqlQuotedIdentifier',  { fg = colors.white,      bg = 'NONE' })  -- "quoted_identifiers"
  highlight(0, 'sqlBacktickIdentifier', { fg = colors.white,     bg = 'NONE' })  -- `backtick_identifiers`

  -- Schema Objects
  highlight(0, 'sqlSchema',            { fg = colors.turquoise,  bg = 'NONE' })  -- Schema names
  highlight(0, 'sqlDatabase',          { fg = colors.turquoise,  bg = 'NONE' })  -- Database names
  highlight(0, 'sqlCatalog',           { fg = colors.turquoise,  bg = 'NONE' })  -- Catalog names


  ---------------------------------------------------------------------------
  -- Functions
  ---------------------------------------------------------------------------

  highlight(0, 'sqlFunction',          { link = "Function" })  -- General functions
  highlight(0, 'sqlFunctionCall',      { link = "Function" })  -- Function calls

  -- Aggregate Functions
  highlight(0, 'sqlAggregate',         { fg = colors.orange,     bg = 'NONE' })  -- COUNT, SUM, AVG, MIN, MAX
  highlight(0, 'sqlAggregateCount',    { fg = colors.orange,     bg = 'NONE' })  -- COUNT
  highlight(0, 'sqlAggregateSum',      { fg = colors.orange,     bg = 'NONE' })  -- SUM
  highlight(0, 'sqlAggregateAvg',      { fg = colors.orange,     bg = 'NONE' })  -- AVG
  highlight(0, 'sqlAggregateMin',      { fg = colors.orange,     bg = 'NONE' })  -- MIN
  highlight(0, 'sqlAggregateMax',      { fg = colors.orange,     bg = 'NONE' })  -- MAX

  -- String Functions
  highlight(0, 'sqlStringFunction',    { link = "String" })  -- CONCAT, SUBSTRING, TRIM, etc.

  -- Date Functions
  highlight(0, 'sqlDateFunction',      { link = "Function" })  -- NOW, CURRENT_DATE, etc.

  -- Math Functions
  highlight(0, 'sqlMathFunction',      { link = "Function" })  -- ABS, ROUND, CEILING, FLOOR

  -- Window Functions
  highlight(0, 'sqlWindowFunction',    { link = "Function" })  -- ROW_NUMBER, RANK, LAG, LEAD
  highlight(0, 'sqlWindowKeyword',     { link = "Keyword" })  -- OVER, PARTITION BY, ROWS, RANGE

  -- Conversion Functions
  highlight(0, 'sqlCastFunction',      { link = "Function" })  -- CAST, CONVERT, COALESCE


  ---------------------------------------------------------------------------
  -- Constraints & Modifiers
  ---------------------------------------------------------------------------

  highlight(0, 'sqlConstraintKeyword', { link = "Keyword" })  -- Constraint keywords

  -- Key Constraints
  highlight(0, 'sqlPrimaryKey',        { fg = colors.pink,       bg = 'NONE' })  -- PRIMARY KEY
  highlight(0, 'sqlForeignKey',        { fg = colors.pink,       bg = 'NONE' })  -- FOREIGN KEY
  highlight(0, 'sqlReferences',        { fg = colors.pink,       bg = 'NONE' })  -- REFERENCES
  highlight(0, 'sqlUnique',            { fg = colors.pink,       bg = 'NONE' })  -- UNIQUE

  -- Null Constraints
  highlight(0, 'sqlNull',              { fg = colors.pink,       bg = 'NONE' })  -- NULL
  highlight(0, 'sqlNotNull',           { fg = colors.pink,       bg = 'NONE' })  -- NOT NULL

  -- Default & Check
  highlight(0, 'sqlDefault',           { fg = colors.pink,       bg = 'NONE' })  -- DEFAULT
  highlight(0, 'sqlCheck',             { fg = colors.pink,       bg = 'NONE' })  -- CHECK

  -- Auto Increment
  highlight(0, 'sqlAutoIncrement',     { fg = colors.pink,       bg = 'NONE' })  -- AUTO_INCREMENT, SERIAL, IDENTITY

  -- Cascade Actions
  highlight(0, 'sqlCascade',           { fg = colors.pink,       bg = 'NONE' })  -- CASCADE
  highlight(0, 'sqlOnDelete',          { fg = colors.pink,       bg = 'NONE' })  -- ON DELETE
  highlight(0, 'sqlOnUpdate',          { fg = colors.pink,       bg = 'NONE' })  -- ON UPDATE


  ---------------------------------------------------------------------------
  -- Literals
  ---------------------------------------------------------------------------

  -- Strings
  highlight(0, 'sqlString',            { link = "String" })  -- 'string literals'
  highlight(0, 'sqlStringLiteral',     { link = "String" })  -- String literals

  -- Numbers
  highlight(0, 'sqlNumber',            { link = "Number" })  -- Numeric literals
  highlight(0, 'sqlIntLiteral',        { fg = colors.greenLight, bg = 'NONE' })  -- Integer literals
  highlight(0, 'sqlFloatLiteral',      { fg = colors.greenLight, bg = 'NONE' })  -- Float literals
  highlight(0, 'sqlHexNumber',         { link = "Number" })  -- Hex numbers (0x...)

  -- Booleans
  highlight(0, 'sqlBoolean',           { link = "Boolean" })  -- TRUE, FALSE
  highlight(0, 'sqlBooleanLiteral',    { link = "Boolean" })  -- Boolean literals

  -- Special Values
  highlight(0, 'sqlSpecial',           { fg = colors.pink,       bg = 'NONE' })  -- Special values
  highlight(0, 'sqlNullValue',         { fg = colors.pink,       bg = 'NONE' })  -- NULL value


  ---------------------------------------------------------------------------
  -- Comments
  ---------------------------------------------------------------------------

  highlight(0, 'sqlComment',           { link = "Comment" })  -- General comments
  highlight(0, 'sqlLineComment',       { link = "Comment" })  -- -- line comments
  highlight(0, 'sqlDashComment',       { link = "Comment" })  -- -- dash comments
  highlight(0, 'sqlSlashComment',      { link = "Comment" })  -- // slash comments
  highlight(0, 'sqlHashComment',       { link = "Comment" })  -- # hash comments (MySQL)
  highlight(0, 'sqlMultiComment',      { link = "Comment" })  -- /* */ block comments
  highlight(0, 'sqlBlockComment',      { link = "Comment" })  -- /* */ block comments
  highlight(0, 'sqlTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME


  ---------------------------------------------------------------------------
  -- Punctuation & Delimiters
  ---------------------------------------------------------------------------

  highlight(0, 'sqlPunctuation',       { fg = colors.white,      bg = 'NONE' })  -- General punctuation
  highlight(0, 'sqlDelimiter',         { link = "Delimiter" })  -- , ; .
  highlight(0, 'sqlSemicolon',         { fg = colors.white,      bg = 'NONE' })  -- ;
  highlight(0, 'sqlComma',             { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'sqlDot',               { fg = colors.white,      bg = 'NONE' })  -- .
  highlight(0, 'sqlParens',            { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'sqlBrackets',          { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'sqlBraces',            { fg = colors.white,      bg = 'NONE' })  -- { }
  highlight(0, 'sqlStar',              { fg = colors.white,      bg = 'NONE' })  -- * (SELECT *)


  ---------------------------------------------------------------------------
  -- Errors
  ---------------------------------------------------------------------------

  highlight(0, 'sqlError',             { fg = colors.red,        bg = 'NONE' })  -- Syntax errors
  highlight(0, 'sqlParenError',        { fg = colors.red,        bg = 'NONE' })  -- Paren errors
  highlight(0, 'sqlStringError',       { link = "String" })  -- String errors


  ---------------------------------------------------------------------------
  -- MySQL Specific
  ---------------------------------------------------------------------------

  highlight(0, 'mysqlKeyword',         { link = "Keyword" })  -- MySQL keywords
  highlight(0, 'mysqlSpecial',         { fg = colors.pink,       bg = 'NONE' })  -- MySQL special values
  highlight(0, 'mysqlString',          { link = "String" })  -- MySQL strings
  highlight(0, 'mysqlNumber',          { link = "Number" })  -- MySQL numbers
  highlight(0, 'mysqlVariable',        { link = "Variable" })  -- @variables
  highlight(0, 'mysqlEscaped',         { fg = colors.white,      bg = 'NONE' })  -- `escaped` identifiers
  highlight(0, 'mysqlComment',         { link = "Comment" })  -- MySQL comments
  highlight(0, 'mysqlType',            { link = "Type" })  -- MySQL types
  highlight(0, 'mysqlOperator',        { link = "Operator" })  -- MySQL operators
  highlight(0, 'mysqlOperatorFunction', { link = "Operator" })  -- ISNULL, COALESCE
  highlight(0, 'mysqlFlowFunction',    { link = "Function" })  -- IF, IFNULL, NULLIF
  highlight(0, 'mysqlFlowLabel',       { fg = colors.blue,       bg = 'NONE' })  -- CASE, WHEN, THEN, ELSE
  highlight(0, 'mysqlWindowFunction',  { link = "Function" })  -- ROW_NUMBER, RANK, etc.
  highlight(0, 'mysqlWindowKeyword',   { link = "Keyword" })  -- OVER, PARTITION, WINDOW
  highlight(0, 'mysqlFunction',        { link = "Function" })  -- MySQL functions

  -- MySQL Storage Engines
  highlight(0, 'mysqlEngine',          { fg = colors.pink,       bg = 'NONE' })  -- ENGINE=InnoDB, MyISAM
  highlight(0, 'mysqlCharset',         { fg = colors.pink,       bg = 'NONE' })  -- CHARSET, COLLATE


  ---------------------------------------------------------------------------
  -- PostgreSQL Specific
  ---------------------------------------------------------------------------

  highlight(0, 'pgsqlKeyword',         { link = "Keyword" })  -- PostgreSQL keywords
  highlight(0, 'pgsqlType',            { link = "Type" })  -- PostgreSQL types
  highlight(0, 'pgsqlFunction',        { link = "Function" })  -- PostgreSQL functions
  highlight(0, 'pgsqlOperator',        { link = "Operator" })  -- PostgreSQL operators

  -- PostgreSQL-specific Types
  highlight(0, 'pgsqlTypeSerial',      { link = "Type" })  -- SERIAL, BIGSERIAL
  highlight(0, 'pgsqlTypeJsonb',       { link = "Type" })  -- JSONB
  highlight(0, 'pgsqlTypeArray',       { link = "Type" })  -- Array types
  highlight(0, 'pgsqlTypeTsVector',    { link = "Type" })  -- TSVECTOR
  highlight(0, 'pgsqlTypeInet',        { link = "Type" })  -- INET, CIDR

  -- Dollar Quoting
  highlight(0, 'pgsqlDollarQuote',     { fg = colors.redLight,   bg = 'NONE' })  -- $$strings$$

  -- PostgreSQL pgsql.vim groups
  highlight(0, 'sqlPlpgsqlKeyword',    { link = "Keyword" })  -- PL/pgSQL keywords
  highlight(0, 'sqlPlpgsqlVariable',   { link = "Variable" })  -- PL/pgSQL variables
  highlight(0, 'sqlPlpgsqlOperator',   { link = "Operator" })  -- := assignment

  -- Psql Commands
  highlight(0, 'sqlPsqlCommand',       { fg = colors.pink,       bg = 'NONE' })  -- \d, \dt, \c, etc.
  highlight(0, 'sqlPsqlKeyword',       { link = "Keyword" })  -- Psql keywords

  -- PostgreSQL Extensions
  highlight(0, 'pgsqlExtension',       { fg = colors.pink,       bg = 'NONE' })  -- CREATE EXTENSION
  highlight(0, 'sqlConstant',          { link = "Constant" })  -- Constants
  highlight(0, 'sqlErrorCode',         { fg = colors.red,        bg = 'NONE' })  -- Error codes


  ---------------------------------------------------------------------------
  -- Oracle PL/SQL Specific
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlKeyword',         { link = "Keyword" })  -- PL/SQL keywords
  highlight(0, 'plsqlReserved',        { fg = colors.blue,       bg = 'NONE' })  -- Reserved words
  highlight(0, 'plsqlStorage',         { fg = colors.blue,       bg = 'NONE' })  -- Storage keywords
  highlight(0, 'plsqlFunction',        { link = "Function" })  -- PL/SQL functions
  highlight(0, 'plsqlOperator',        { link = "Operator" })  -- PL/SQL operators
  highlight(0, 'plsqlException',       { fg = colors.pink,       bg = 'NONE' })  -- Exception handling

  -- PL/SQL Control
  highlight(0, 'plsqlRepeat',          { fg = colors.blue,       bg = 'NONE' })  -- LOOP, WHILE, FOR
  highlight(0, 'plsqlConditional',     { link = "Conditional" })  -- IF, THEN, ELSE, ELSIF
  highlight(0, 'plsqlCase',            { fg = colors.blue,       bg = 'NONE' })  -- CASE statement

  -- PL/SQL Blocks
  highlight(0, 'plsqlBlock',           { fg = colors.blue,       bg = 'NONE' })  -- Block keywords
  highlight(0, 'plsqlBEGIN',           { fg = colors.blue,       bg = 'NONE' })  -- BEGIN
  highlight(0, 'plsqlEND',             { fg = colors.blue,       bg = 'NONE' })  -- END
  highlight(0, 'plsqlISAS',            { fg = colors.blue,       bg = 'NONE' })  -- IS, AS
  highlight(0, 'plsqlLoopBlock',       { fg = colors.blue,       bg = 'NONE' })  -- Loop blocks
  highlight(0, 'plsqlConditionalBlock', { link = "Conditional" })  -- Conditional blocks

  -- PL/SQL Identifiers
  highlight(0, 'plsqlIdentifier',      { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, 'plsqlQuotedIdentifier', { fg = colors.white,     bg = 'NONE' })  -- Quoted identifiers
  highlight(0, 'plsqlHostIdentifier',  { fg = colors.purple,     bg = 'NONE' })  -- Host variables

  -- PL/SQL Literals
  highlight(0, 'plsqlStringLiteral',   { link = "String" })  -- Strings
  highlight(0, 'plsqlIntLiteral',      { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, 'plsqlFloatLiteral',    { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'plsqlBooleanLiteral',  { link = "Boolean" })  -- Booleans
  highlight(0, 'plsqlNumbers',         { link = "Number" })  -- Numbers

  -- PL/SQL Attributes
  highlight(0, 'plsqlAttribute',       { fg = colors.pink,       bg = 'NONE' })  -- %TYPE, %ROWTYPE
  highlight(0, 'plsqlTypeAttribute',   { link = "Type" })  -- Type attributes

  -- PL/SQL Comments
  highlight(0, 'plsqlComment',         { link = "Comment" })  -- Comments
  highlight(0, 'plsqlCommentL',        { link = "Comment" })  -- Line comments
  highlight(0, 'plsqlTodo',            { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO

  -- PL/SQL Special
  highlight(0, 'plsqlTrigger',         { fg = colors.pink,       bg = 'NONE' })  -- Trigger keywords
  highlight(0, 'plsqlPseudo',          { fg = colors.pink,       bg = 'NONE' })  -- Pseudo columns
  highlight(0, 'plsqlSymbol',          { fg = colors.white,      bg = 'NONE' })  -- Symbols

  -- SQL*Plus
  highlight(0, 'plsqlSqlPlusCommand',  { fg = colors.pink,       bg = 'NONE' })  -- SQL*Plus commands
  highlight(0, 'plsqlSqlPlusDefine',   { fg = colors.purple,     bg = 'NONE' })  -- &define variables
  highlight(0, 'plsqlSqlPlusRunFile',  { fg = colors.pink,       bg = 'NONE' })  -- @script.sql

  -- PL/SQL Errors
  highlight(0, 'plsqlError',           { fg = colors.red,        bg = 'NONE' })  -- Errors
  highlight(0, 'plsqlGarbage',         { fg = colors.red,        bg = 'NONE' })  -- Invalid syntax


  ---------------------------------------------------------------------------
  -- SQL Server T-SQL Specific
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlKeyword',          { link = "Keyword" })  -- T-SQL keywords
  highlight(0, 'tsqlType',             { link = "Type" })  -- T-SQL types
  highlight(0, 'tsqlFunction',         { link = "Function" })  -- T-SQL functions
  highlight(0, 'tsqlOperator',         { link = "Operator" })  -- T-SQL operators

  -- T-SQL Variables
  highlight(0, 'tsqlVariable',         { link = "Variable" })  -- @variables
  highlight(0, 'tsqlGlobalVariable',   { link = "Variable" })  -- @@global_variables
  highlight(0, 'tsqlTempTable',        { fg = colors.turquoise,  bg = 'NONE' })  -- #temp_tables, ##global_temp

  -- T-SQL Control
  highlight(0, 'tsqlIf',               { fg = colors.blue,       bg = 'NONE' })  -- IF
  highlight(0, 'tsqlWhile',            { fg = colors.blue,       bg = 'NONE' })  -- WHILE
  highlight(0, 'tsqlGoto',             { fg = colors.blue,       bg = 'NONE' })  -- GOTO
  highlight(0, 'tsqlReturn',           { fg = colors.blue,       bg = 'NONE' })  -- RETURN
  highlight(0, 'tsqlBreak',            { fg = colors.blue,       bg = 'NONE' })  -- BREAK
  highlight(0, 'tsqlContinue',         { fg = colors.blue,       bg = 'NONE' })  -- CONTINUE

  -- T-SQL Error Handling
  highlight(0, 'tsqlTry',              { fg = colors.blue,       bg = 'NONE' })  -- TRY
  highlight(0, 'tsqlCatch',            { fg = colors.blue,       bg = 'NONE' })  -- CATCH
  highlight(0, 'tsqlThrow',            { fg = colors.blue,       bg = 'NONE' })  -- THROW
  highlight(0, 'tsqlRaiserror',        { fg = colors.blue,       bg = 'NONE' })  -- RAISERROR

  -- T-SQL Special
  highlight(0, 'tsqlPrint',            { fg = colors.blue,       bg = 'NONE' })  -- PRINT
  highlight(0, 'tsqlExec',             { fg = colors.blue,       bg = 'NONE' })  -- EXEC, EXECUTE
  highlight(0, 'tsqlDeclare',          { fg = colors.blue,       bg = 'NONE' })  -- DECLARE


  ---------------------------------------------------------------------------
  -- SQLite Specific
  ---------------------------------------------------------------------------

  highlight(0, 'sqliteKeyword',        { link = "Keyword" })  -- SQLite keywords
  highlight(0, 'sqliteType',           { link = "Type" })  -- SQLite types
  highlight(0, 'sqliteFunction',       { link = "Function" })  -- SQLite functions
  highlight(0, 'sqliteOperator',       { link = "Operator" })  -- SQLite operators

  -- SQLite Pragmas
  highlight(0, 'sqlitePragma',         { fg = colors.pink,       bg = 'NONE' })  -- PRAGMA statements

  -- SQLite Special
  highlight(0, 'sqliteRowid',          { fg = colors.pink,       bg = 'NONE' })  -- rowid, _rowid_, oid
  highlight(0, 'sqliteAffinity',       { fg = colors.turquoise,  bg = 'NONE' })  -- Type affinity


  ---------------------------------------------------------------------------
  -- Options & Configuration
  ---------------------------------------------------------------------------

  highlight(0, 'sqlOption',            { fg = colors.pink,       bg = 'NONE' })  -- Database options
  highlight(0, 'sqlKeywordLogin',      { link = "Keyword" })  -- Login options


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.sql)
  ---------------------------------------------------------------------------

  -- Keywords
  highlight(0, '@keyword.sql',                   { link = "Keyword" })  -- SQL keywords
  highlight(0, '@keyword.operator.sql',          { link = "Operator" })  -- AND, OR, NOT, IN
  highlight(0, '@keyword.conditional.sql',       { link = "Conditional" })  -- CASE, WHEN, THEN, ELSE
  highlight(0, '@keyword.modifier.sql',          { link = "Keyword" })  -- TEMP, RECURSIVE, constraints

  -- Variables
  highlight(0, '@variable.sql',                  { link = "Variable" })  -- Relations, aliases
  highlight(0, '@variable.member.sql',           { link = "Variable" })  -- Fields, columns
  highlight(0, '@variable.parameter.sql',        { link = "Variable" })  -- Parameters

  -- Types
  highlight(0, '@type.sql',                      { link = "Type" })  -- Object references
  highlight(0, '@type.builtin.sql',              { link = "Type" })  -- int, varchar, etc.

  -- Functions
  highlight(0, '@function.sql',                  { link = "Function" })  -- Functions
  highlight(0, '@function.call.sql',             { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.sql',          { link = "Function" })  -- Built-in functions

  -- Literals
  highlight(0, '@string.sql',                    { link = "String" })  -- String literals
  highlight(0, '@number.sql',                    { link = "Number" })  -- Integer literals
  highlight(0, '@number.float.sql',              { link = "Number" })  -- Float literals
  highlight(0, '@boolean.sql',                   { link = "Boolean" })  -- TRUE, FALSE

  -- Comments
  highlight(0, '@comment.sql',                   { link = "Comment" })  -- Comments

  -- Operators & Punctuation
  highlight(0, '@operator.sql',                  { link = "Operator" })  -- =, <>, <, >, etc.
  highlight(0, '@punctuation.bracket.sql',       { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, '@punctuation.delimiter.sql',     { link = "Delimiter" })  -- ; , .


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.sql)
  ---------------------------------------------------------------------------

  highlight(0, '@lsp.type.variable.sql',         { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.function.sql',         { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.parameter.sql',        { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.type.sql',             { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.sql',            { fg = colors.turquoise,  bg = 'NONE' })  -- Tables
  highlight(0, '@lsp.type.property.sql',         { fg = colors.white,      bg = 'NONE' })  -- Columns
  highlight(0, '@lsp.type.namespace.sql',        { fg = colors.turquoise,  bg = 'NONE' })  -- Schemas
  highlight(0, '@lsp.type.keyword.sql',          { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.string.sql',           { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.sql',           { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.comment.sql',          { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.operator.sql',         { link = "Operator" })  -- Operators


  ---------------------------------------------------------------------------
  -- Embedded SQL (in other languages)
  ---------------------------------------------------------------------------

  highlight(0, 'sqlEmbedded',          { fg = colors.blue,       bg = 'NONE' })  -- Embedded SQL
  highlight(0, 'sqlHostVariable',      { link = "Variable" })  -- :host_variable
  highlight(0, 'sqlPlaceholder',       { fg = colors.purple,     bg = 'NONE' })  -- ? or $1 placeholders


  ---------------------------------------------------------------------------
  -- Database Objects (CREATE statements)
  ---------------------------------------------------------------------------

  highlight(0, 'sqlCreateTable',       { fg = colors.blue,       bg = 'NONE' })  -- CREATE TABLE
  highlight(0, 'sqlCreateView',        { fg = colors.blue,       bg = 'NONE' })  -- CREATE VIEW
  highlight(0, 'sqlCreateIndex',       { fg = colors.blue,       bg = 'NONE' })  -- CREATE INDEX
  highlight(0, 'sqlCreateFunction',    { link = "Function" })  -- CREATE FUNCTION
  highlight(0, 'sqlCreateProcedure',   { fg = colors.blue,       bg = 'NONE' })  -- CREATE PROCEDURE
  highlight(0, 'sqlCreateTrigger',     { fg = colors.blue,       bg = 'NONE' })  -- CREATE TRIGGER
  highlight(0, 'sqlCreateSequence',    { fg = colors.blue,       bg = 'NONE' })  -- CREATE SEQUENCE
  highlight(0, 'sqlCreateSchema',      { fg = colors.blue,       bg = 'NONE' })  -- CREATE SCHEMA
  highlight(0, 'sqlCreateDatabase',    { fg = colors.blue,       bg = 'NONE' })  -- CREATE DATABASE
  highlight(0, 'sqlCreateType',        { link = "Type" })  -- CREATE TYPE
  highlight(0, 'sqlCreateTypeKeyword', { link = "Keyword" })  -- CREATE TYPE clause
  highlight(0, 'sqlCreateOperatorKeyword', { link = "Operator" })  -- CREATE OPERATOR
  highlight(0, 'sqlCreateTextSearchKeyword', { link = "Keyword" })  -- Text search


  ---------------------------------------------------------------------------
  -- Additional SQL Dialects
  ---------------------------------------------------------------------------

  -- Informix
  highlight(0, 'informixKeyword',      { link = "Keyword" })  -- Informix keywords
  highlight(0, 'informixType',         { link = "Type" })  -- Informix types
  highlight(0, 'informixFunction',     { link = "Function" })  -- Informix functions
  highlight(0, 'sqlRepeat',            { fg = colors.blue,       bg = 'NONE' })  -- Loop constructs

  -- SQL Anywhere
  highlight(0, 'sqlaKeyword',          { link = "Keyword" })  -- SQL Anywhere keywords
  highlight(0, 'sqlaType',             { link = "Type" })  -- SQL Anywhere types
  highlight(0, 'sqlaFunction',         { link = "Function" })  -- SQL Anywhere functions

end

return sql
