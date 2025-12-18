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
  highlight(0, 'sqlKeyword',           { fg = colors.blue,       bg = 'NONE' })  -- General keywords
  highlight(0, 'sqlKeywordTop',        { fg = colors.blue,       bg = 'NONE' })  -- TOP keyword

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
  highlight(0, 'sqlConditional',       { fg = colors.blue,       bg = 'NONE' })  -- CASE, WHEN, THEN, ELSE, END
  highlight(0, 'sqlCase',              { fg = colors.blue,       bg = 'NONE' })  -- CASE
  highlight(0, 'sqlWhen',              { fg = colors.blue,       bg = 'NONE' })  -- WHEN
  highlight(0, 'sqlThen',              { fg = colors.blue,       bg = 'NONE' })  -- THEN
  highlight(0, 'sqlElse',              { fg = colors.blue,       bg = 'NONE' })  -- ELSE
  highlight(0, 'sqlEnd',               { fg = colors.blue,       bg = 'NONE' })  -- END

  -- Operators
  highlight(0, 'sqlOperator',          { fg = colors.blue,       bg = 'NONE' })  -- AND, OR, NOT, IN, LIKE, etc.
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

  highlight(0, 'sqlType',              { fg = colors.turquoise,  bg = 'NONE' })  -- Data types

  -- Numeric Types
  highlight(0, 'sqlTypeInt',           { fg = colors.turquoise,  bg = 'NONE' })  -- INT, INTEGER, SMALLINT, BIGINT
  highlight(0, 'sqlTypeNumeric',       { fg = colors.turquoise,  bg = 'NONE' })  -- NUMERIC, DECIMAL
  highlight(0, 'sqlTypeFloat',         { fg = colors.turquoise,  bg = 'NONE' })  -- FLOAT, REAL, DOUBLE
  highlight(0, 'sqlTypeMoney',         { fg = colors.turquoise,  bg = 'NONE' })  -- MONEY, SMALLMONEY

  -- String Types
  highlight(0, 'sqlTypeChar',          { fg = colors.turquoise,  bg = 'NONE' })  -- CHAR, CHARACTER
  highlight(0, 'sqlTypeVarchar',       { fg = colors.turquoise,  bg = 'NONE' })  -- VARCHAR
  highlight(0, 'sqlTypeText',          { fg = colors.turquoise,  bg = 'NONE' })  -- TEXT
  highlight(0, 'sqlTypeNchar',         { fg = colors.turquoise,  bg = 'NONE' })  -- NCHAR, NVARCHAR

  -- Date/Time Types
  highlight(0, 'sqlTypeDate',          { fg = colors.turquoise,  bg = 'NONE' })  -- DATE
  highlight(0, 'sqlTypeTime',          { fg = colors.turquoise,  bg = 'NONE' })  -- TIME
  highlight(0, 'sqlTypeDatetime',      { fg = colors.turquoise,  bg = 'NONE' })  -- DATETIME, TIMESTAMP
  highlight(0, 'sqlTypeInterval',      { fg = colors.turquoise,  bg = 'NONE' })  -- INTERVAL

  -- Binary Types
  highlight(0, 'sqlTypeBinary',        { fg = colors.turquoise,  bg = 'NONE' })  -- BINARY, VARBINARY
  highlight(0, 'sqlTypeBlob',          { fg = colors.turquoise,  bg = 'NONE' })  -- BLOB
  highlight(0, 'sqlTypeClob',          { fg = colors.turquoise,  bg = 'NONE' })  -- CLOB

  -- Other Types
  highlight(0, 'sqlTypeBool',          { fg = colors.turquoise,  bg = 'NONE' })  -- BOOLEAN, BOOL
  highlight(0, 'sqlTypeUuid',          { fg = colors.turquoise,  bg = 'NONE' })  -- UUID
  highlight(0, 'sqlTypeJson',          { fg = colors.turquoise,  bg = 'NONE' })  -- JSON, JSONB
  highlight(0, 'sqlTypeXml',           { fg = colors.turquoise,  bg = 'NONE' })  -- XML
  highlight(0, 'sqlTypeArray',         { fg = colors.turquoise,  bg = 'NONE' })  -- ARRAY


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

  highlight(0, 'sqlFunction',          { fg = colors.orange,     bg = 'NONE' })  -- General functions
  highlight(0, 'sqlFunctionCall',      { fg = colors.orange,     bg = 'NONE' })  -- Function calls

  -- Aggregate Functions
  highlight(0, 'sqlAggregate',         { fg = colors.orange,     bg = 'NONE' })  -- COUNT, SUM, AVG, MIN, MAX
  highlight(0, 'sqlAggregateCount',    { fg = colors.orange,     bg = 'NONE' })  -- COUNT
  highlight(0, 'sqlAggregateSum',      { fg = colors.orange,     bg = 'NONE' })  -- SUM
  highlight(0, 'sqlAggregateAvg',      { fg = colors.orange,     bg = 'NONE' })  -- AVG
  highlight(0, 'sqlAggregateMin',      { fg = colors.orange,     bg = 'NONE' })  -- MIN
  highlight(0, 'sqlAggregateMax',      { fg = colors.orange,     bg = 'NONE' })  -- MAX

  -- String Functions
  highlight(0, 'sqlStringFunction',    { fg = colors.orange,     bg = 'NONE' })  -- CONCAT, SUBSTRING, TRIM, etc.

  -- Date Functions
  highlight(0, 'sqlDateFunction',      { fg = colors.orange,     bg = 'NONE' })  -- NOW, CURRENT_DATE, etc.

  -- Math Functions
  highlight(0, 'sqlMathFunction',      { fg = colors.orange,     bg = 'NONE' })  -- ABS, ROUND, CEILING, FLOOR

  -- Window Functions
  highlight(0, 'sqlWindowFunction',    { fg = colors.orange,     bg = 'NONE' })  -- ROW_NUMBER, RANK, LAG, LEAD
  highlight(0, 'sqlWindowKeyword',     { fg = colors.blue,       bg = 'NONE' })  -- OVER, PARTITION BY, ROWS, RANGE

  -- Conversion Functions
  highlight(0, 'sqlCastFunction',      { fg = colors.orange,     bg = 'NONE' })  -- CAST, CONVERT, COALESCE


  ---------------------------------------------------------------------------
  -- Constraints & Modifiers
  ---------------------------------------------------------------------------

  highlight(0, 'sqlConstraintKeyword', { fg = colors.pink,       bg = 'NONE' })  -- Constraint keywords

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
  highlight(0, 'sqlString',            { fg = colors.redLight,   bg = 'NONE' })  -- 'string literals'
  highlight(0, 'sqlStringLiteral',     { fg = colors.redLight,   bg = 'NONE' })  -- String literals

  -- Numbers
  highlight(0, 'sqlNumber',            { fg = colors.greenLight, bg = 'NONE' })  -- Numeric literals
  highlight(0, 'sqlIntLiteral',        { fg = colors.greenLight, bg = 'NONE' })  -- Integer literals
  highlight(0, 'sqlFloatLiteral',      { fg = colors.greenLight, bg = 'NONE' })  -- Float literals
  highlight(0, 'sqlHexNumber',         { fg = colors.greenLight, bg = 'NONE' })  -- Hex numbers (0x...)

  -- Booleans
  highlight(0, 'sqlBoolean',           { fg = colors.blue,       bg = 'NONE' })  -- TRUE, FALSE
  highlight(0, 'sqlBooleanLiteral',    { fg = colors.blue,       bg = 'NONE' })  -- Boolean literals

  -- Special Values
  highlight(0, 'sqlSpecial',           { fg = colors.pink,       bg = 'NONE' })  -- Special values
  highlight(0, 'sqlNullValue',         { fg = colors.pink,       bg = 'NONE' })  -- NULL value


  ---------------------------------------------------------------------------
  -- Comments
  ---------------------------------------------------------------------------

  highlight(0, 'sqlComment',           { fg = colors.red,        bg = 'NONE' })  -- General comments
  highlight(0, 'sqlLineComment',       { fg = colors.red,        bg = 'NONE' })  -- -- line comments
  highlight(0, 'sqlDashComment',       { fg = colors.red,        bg = 'NONE' })  -- -- dash comments
  highlight(0, 'sqlSlashComment',      { fg = colors.red,        bg = 'NONE' })  -- // slash comments
  highlight(0, 'sqlHashComment',       { fg = colors.red,        bg = 'NONE' })  -- # hash comments (MySQL)
  highlight(0, 'sqlMultiComment',      { fg = colors.red,        bg = 'NONE' })  -- /* */ block comments
  highlight(0, 'sqlBlockComment',      { fg = colors.red,        bg = 'NONE' })  -- /* */ block comments
  highlight(0, 'sqlTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME


  ---------------------------------------------------------------------------
  -- Punctuation & Delimiters
  ---------------------------------------------------------------------------

  highlight(0, 'sqlPunctuation',       { fg = colors.white,      bg = 'NONE' })  -- General punctuation
  highlight(0, 'sqlDelimiter',         { fg = colors.white,      bg = 'NONE' })  -- , ; .
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
  highlight(0, 'sqlStringError',       { fg = colors.red,        bg = 'NONE' })  -- String errors


  ---------------------------------------------------------------------------
  -- MySQL Specific
  ---------------------------------------------------------------------------

  highlight(0, 'mysqlKeyword',         { fg = colors.blue,       bg = 'NONE' })  -- MySQL keywords
  highlight(0, 'mysqlSpecial',         { fg = colors.pink,       bg = 'NONE' })  -- MySQL special values
  highlight(0, 'mysqlString',          { fg = colors.redLight,   bg = 'NONE' })  -- MySQL strings
  highlight(0, 'mysqlNumber',          { fg = colors.greenLight, bg = 'NONE' })  -- MySQL numbers
  highlight(0, 'mysqlVariable',        { fg = colors.purple,     bg = 'NONE' })  -- @variables
  highlight(0, 'mysqlEscaped',         { fg = colors.white,      bg = 'NONE' })  -- `escaped` identifiers
  highlight(0, 'mysqlComment',         { fg = colors.red,        bg = 'NONE' })  -- MySQL comments
  highlight(0, 'mysqlType',            { fg = colors.turquoise,  bg = 'NONE' })  -- MySQL types
  highlight(0, 'mysqlOperator',        { fg = colors.blue,       bg = 'NONE' })  -- MySQL operators
  highlight(0, 'mysqlOperatorFunction', { fg = colors.orange,    bg = 'NONE' })  -- ISNULL, COALESCE
  highlight(0, 'mysqlFlowFunction',    { fg = colors.orange,     bg = 'NONE' })  -- IF, IFNULL, NULLIF
  highlight(0, 'mysqlFlowLabel',       { fg = colors.blue,       bg = 'NONE' })  -- CASE, WHEN, THEN, ELSE
  highlight(0, 'mysqlWindowFunction',  { fg = colors.orange,     bg = 'NONE' })  -- ROW_NUMBER, RANK, etc.
  highlight(0, 'mysqlWindowKeyword',   { fg = colors.blue,       bg = 'NONE' })  -- OVER, PARTITION, WINDOW
  highlight(0, 'mysqlFunction',        { fg = colors.orange,     bg = 'NONE' })  -- MySQL functions

  -- MySQL Storage Engines
  highlight(0, 'mysqlEngine',          { fg = colors.pink,       bg = 'NONE' })  -- ENGINE=InnoDB, MyISAM
  highlight(0, 'mysqlCharset',         { fg = colors.pink,       bg = 'NONE' })  -- CHARSET, COLLATE


  ---------------------------------------------------------------------------
  -- PostgreSQL Specific
  ---------------------------------------------------------------------------

  highlight(0, 'pgsqlKeyword',         { fg = colors.blue,       bg = 'NONE' })  -- PostgreSQL keywords
  highlight(0, 'pgsqlType',            { fg = colors.turquoise,  bg = 'NONE' })  -- PostgreSQL types
  highlight(0, 'pgsqlFunction',        { fg = colors.orange,     bg = 'NONE' })  -- PostgreSQL functions
  highlight(0, 'pgsqlOperator',        { fg = colors.white,      bg = 'NONE' })  -- PostgreSQL operators

  -- PostgreSQL-specific Types
  highlight(0, 'pgsqlTypeSerial',      { fg = colors.turquoise,  bg = 'NONE' })  -- SERIAL, BIGSERIAL
  highlight(0, 'pgsqlTypeJsonb',       { fg = colors.turquoise,  bg = 'NONE' })  -- JSONB
  highlight(0, 'pgsqlTypeArray',       { fg = colors.turquoise,  bg = 'NONE' })  -- Array types
  highlight(0, 'pgsqlTypeTsVector',    { fg = colors.turquoise,  bg = 'NONE' })  -- TSVECTOR
  highlight(0, 'pgsqlTypeInet',        { fg = colors.turquoise,  bg = 'NONE' })  -- INET, CIDR

  -- Dollar Quoting
  highlight(0, 'pgsqlDollarQuote',     { fg = colors.redLight,   bg = 'NONE' })  -- $$strings$$

  -- PostgreSQL pgsql.vim groups
  highlight(0, 'sqlPlpgsqlKeyword',    { fg = colors.blue,       bg = 'NONE' })  -- PL/pgSQL keywords
  highlight(0, 'sqlPlpgsqlVariable',   { fg = colors.purple,     bg = 'NONE' })  -- PL/pgSQL variables
  highlight(0, 'sqlPlpgsqlOperator',   { fg = colors.white,      bg = 'NONE' })  -- := assignment

  -- Psql Commands
  highlight(0, 'sqlPsqlCommand',       { fg = colors.pink,       bg = 'NONE' })  -- \d, \dt, \c, etc.
  highlight(0, 'sqlPsqlKeyword',       { fg = colors.blue,       bg = 'NONE' })  -- Psql keywords

  -- PostgreSQL Extensions
  highlight(0, 'pgsqlExtension',       { fg = colors.pink,       bg = 'NONE' })  -- CREATE EXTENSION
  highlight(0, 'sqlConstant',          { fg = colors.turquoise,  bg = 'NONE' })  -- Constants
  highlight(0, 'sqlErrorCode',         { fg = colors.red,        bg = 'NONE' })  -- Error codes


  ---------------------------------------------------------------------------
  -- Oracle PL/SQL Specific
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlKeyword',         { fg = colors.blue,       bg = 'NONE' })  -- PL/SQL keywords
  highlight(0, 'plsqlReserved',        { fg = colors.blue,       bg = 'NONE' })  -- Reserved words
  highlight(0, 'plsqlStorage',         { fg = colors.blue,       bg = 'NONE' })  -- Storage keywords
  highlight(0, 'plsqlFunction',        { fg = colors.orange,     bg = 'NONE' })  -- PL/SQL functions
  highlight(0, 'plsqlOperator',        { fg = colors.white,      bg = 'NONE' })  -- PL/SQL operators
  highlight(0, 'plsqlException',       { fg = colors.pink,       bg = 'NONE' })  -- Exception handling

  -- PL/SQL Control
  highlight(0, 'plsqlRepeat',          { fg = colors.blue,       bg = 'NONE' })  -- LOOP, WHILE, FOR
  highlight(0, 'plsqlConditional',     { fg = colors.blue,       bg = 'NONE' })  -- IF, THEN, ELSE, ELSIF
  highlight(0, 'plsqlCase',            { fg = colors.blue,       bg = 'NONE' })  -- CASE statement

  -- PL/SQL Blocks
  highlight(0, 'plsqlBlock',           { fg = colors.blue,       bg = 'NONE' })  -- Block keywords
  highlight(0, 'plsqlBEGIN',           { fg = colors.blue,       bg = 'NONE' })  -- BEGIN
  highlight(0, 'plsqlEND',             { fg = colors.blue,       bg = 'NONE' })  -- END
  highlight(0, 'plsqlISAS',            { fg = colors.blue,       bg = 'NONE' })  -- IS, AS
  highlight(0, 'plsqlLoopBlock',       { fg = colors.blue,       bg = 'NONE' })  -- Loop blocks
  highlight(0, 'plsqlConditionalBlock', { fg = colors.blue,      bg = 'NONE' })  -- Conditional blocks

  -- PL/SQL Identifiers
  highlight(0, 'plsqlIdentifier',      { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, 'plsqlQuotedIdentifier', { fg = colors.white,     bg = 'NONE' })  -- Quoted identifiers
  highlight(0, 'plsqlHostIdentifier',  { fg = colors.purple,     bg = 'NONE' })  -- Host variables

  -- PL/SQL Literals
  highlight(0, 'plsqlStringLiteral',   { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, 'plsqlIntLiteral',      { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, 'plsqlFloatLiteral',    { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'plsqlBooleanLiteral',  { fg = colors.blue,       bg = 'NONE' })  -- Booleans
  highlight(0, 'plsqlNumbers',         { fg = colors.greenLight, bg = 'NONE' })  -- Numbers

  -- PL/SQL Attributes
  highlight(0, 'plsqlAttribute',       { fg = colors.pink,       bg = 'NONE' })  -- %TYPE, %ROWTYPE
  highlight(0, 'plsqlTypeAttribute',   { fg = colors.pink,       bg = 'NONE' })  -- Type attributes

  -- PL/SQL Comments
  highlight(0, 'plsqlComment',         { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, 'plsqlCommentL',        { fg = colors.red,        bg = 'NONE' })  -- Line comments
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

  highlight(0, 'tsqlKeyword',          { fg = colors.blue,       bg = 'NONE' })  -- T-SQL keywords
  highlight(0, 'tsqlType',             { fg = colors.turquoise,  bg = 'NONE' })  -- T-SQL types
  highlight(0, 'tsqlFunction',         { fg = colors.orange,     bg = 'NONE' })  -- T-SQL functions
  highlight(0, 'tsqlOperator',         { fg = colors.white,      bg = 'NONE' })  -- T-SQL operators

  -- T-SQL Variables
  highlight(0, 'tsqlVariable',         { fg = colors.purple,     bg = 'NONE' })  -- @variables
  highlight(0, 'tsqlGlobalVariable',   { fg = colors.purple,     bg = 'NONE' })  -- @@global_variables
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

  highlight(0, 'sqliteKeyword',        { fg = colors.blue,       bg = 'NONE' })  -- SQLite keywords
  highlight(0, 'sqliteType',           { fg = colors.turquoise,  bg = 'NONE' })  -- SQLite types
  highlight(0, 'sqliteFunction',       { fg = colors.orange,     bg = 'NONE' })  -- SQLite functions
  highlight(0, 'sqliteOperator',       { fg = colors.white,      bg = 'NONE' })  -- SQLite operators

  -- SQLite Pragmas
  highlight(0, 'sqlitePragma',         { fg = colors.pink,       bg = 'NONE' })  -- PRAGMA statements

  -- SQLite Special
  highlight(0, 'sqliteRowid',          { fg = colors.pink,       bg = 'NONE' })  -- rowid, _rowid_, oid
  highlight(0, 'sqliteAffinity',       { fg = colors.turquoise,  bg = 'NONE' })  -- Type affinity


  ---------------------------------------------------------------------------
  -- Options & Configuration
  ---------------------------------------------------------------------------

  highlight(0, 'sqlOption',            { fg = colors.pink,       bg = 'NONE' })  -- Database options
  highlight(0, 'sqlKeywordLogin',      { fg = colors.blue,       bg = 'NONE' })  -- Login options


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.sql)
  ---------------------------------------------------------------------------

  -- Keywords
  highlight(0, '@keyword.sql',                   { fg = colors.blue,       bg = 'NONE' })  -- SQL keywords
  highlight(0, '@keyword.operator.sql',          { fg = colors.blue,       bg = 'NONE' })  -- AND, OR, NOT, IN
  highlight(0, '@keyword.conditional.sql',       { fg = colors.blue,       bg = 'NONE' })  -- CASE, WHEN, THEN, ELSE
  highlight(0, '@keyword.modifier.sql',          { fg = colors.pink,       bg = 'NONE' })  -- TEMP, RECURSIVE, constraints

  -- Variables
  highlight(0, '@variable.sql',                  { fg = colors.white,      bg = 'NONE' })  -- Relations, aliases
  highlight(0, '@variable.member.sql',           { fg = colors.white,      bg = 'NONE' })  -- Fields, columns
  highlight(0, '@variable.parameter.sql',        { fg = colors.purple,     bg = 'NONE' })  -- Parameters

  -- Types
  highlight(0, '@type.sql',                      { fg = colors.turquoise,  bg = 'NONE' })  -- Object references
  highlight(0, '@type.builtin.sql',              { fg = colors.turquoise,  bg = 'NONE' })  -- int, varchar, etc.

  -- Functions
  highlight(0, '@function.sql',                  { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@function.call.sql',             { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.sql',          { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions

  -- Literals
  highlight(0, '@string.sql',                    { fg = colors.redLight,   bg = 'NONE' })  -- String literals
  highlight(0, '@number.sql',                    { fg = colors.greenLight, bg = 'NONE' })  -- Integer literals
  highlight(0, '@number.float.sql',              { fg = colors.greenLight, bg = 'NONE' })  -- Float literals
  highlight(0, '@boolean.sql',                   { fg = colors.blue,       bg = 'NONE' })  -- TRUE, FALSE

  -- Comments
  highlight(0, '@comment.sql',                   { fg = colors.red,        bg = 'NONE' })  -- Comments

  -- Operators & Punctuation
  highlight(0, '@operator.sql',                  { fg = colors.white,      bg = 'NONE' })  -- =, <>, <, >, etc.
  highlight(0, '@punctuation.bracket.sql',       { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, '@punctuation.delimiter.sql',     { fg = colors.white,      bg = 'NONE' })  -- ; , .


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.sql)
  ---------------------------------------------------------------------------

  highlight(0, '@lsp.type.variable.sql',         { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.function.sql',         { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.parameter.sql',        { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.type.sql',             { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.sql',            { fg = colors.turquoise,  bg = 'NONE' })  -- Tables
  highlight(0, '@lsp.type.property.sql',         { fg = colors.white,      bg = 'NONE' })  -- Columns
  highlight(0, '@lsp.type.namespace.sql',        { fg = colors.turquoise,  bg = 'NONE' })  -- Schemas
  highlight(0, '@lsp.type.keyword.sql',          { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.string.sql',           { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.sql',           { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.comment.sql',          { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.operator.sql',         { fg = colors.white,      bg = 'NONE' })  -- Operators


  ---------------------------------------------------------------------------
  -- Embedded SQL (in other languages)
  ---------------------------------------------------------------------------

  highlight(0, 'sqlEmbedded',          { fg = colors.blue,       bg = 'NONE' })  -- Embedded SQL
  highlight(0, 'sqlHostVariable',      { fg = colors.purple,     bg = 'NONE' })  -- :host_variable
  highlight(0, 'sqlPlaceholder',       { fg = colors.purple,     bg = 'NONE' })  -- ? or $1 placeholders


  ---------------------------------------------------------------------------
  -- Database Objects (CREATE statements)
  ---------------------------------------------------------------------------

  highlight(0, 'sqlCreateTable',       { fg = colors.blue,       bg = 'NONE' })  -- CREATE TABLE
  highlight(0, 'sqlCreateView',        { fg = colors.blue,       bg = 'NONE' })  -- CREATE VIEW
  highlight(0, 'sqlCreateIndex',       { fg = colors.blue,       bg = 'NONE' })  -- CREATE INDEX
  highlight(0, 'sqlCreateFunction',    { fg = colors.blue,       bg = 'NONE' })  -- CREATE FUNCTION
  highlight(0, 'sqlCreateProcedure',   { fg = colors.blue,       bg = 'NONE' })  -- CREATE PROCEDURE
  highlight(0, 'sqlCreateTrigger',     { fg = colors.blue,       bg = 'NONE' })  -- CREATE TRIGGER
  highlight(0, 'sqlCreateSequence',    { fg = colors.blue,       bg = 'NONE' })  -- CREATE SEQUENCE
  highlight(0, 'sqlCreateSchema',      { fg = colors.blue,       bg = 'NONE' })  -- CREATE SCHEMA
  highlight(0, 'sqlCreateDatabase',    { fg = colors.blue,       bg = 'NONE' })  -- CREATE DATABASE
  highlight(0, 'sqlCreateType',        { fg = colors.blue,       bg = 'NONE' })  -- CREATE TYPE
  highlight(0, 'sqlCreateTypeKeyword', { fg = colors.blue,       bg = 'NONE' })  -- CREATE TYPE clause
  highlight(0, 'sqlCreateOperatorKeyword', { fg = colors.blue,   bg = 'NONE' })  -- CREATE OPERATOR
  highlight(0, 'sqlCreateTextSearchKeyword', { fg = colors.blue, bg = 'NONE' })  -- Text search


  ---------------------------------------------------------------------------
  -- Additional SQL Dialects
  ---------------------------------------------------------------------------

  -- Informix
  highlight(0, 'informixKeyword',      { fg = colors.blue,       bg = 'NONE' })  -- Informix keywords
  highlight(0, 'informixType',         { fg = colors.turquoise,  bg = 'NONE' })  -- Informix types
  highlight(0, 'informixFunction',     { fg = colors.orange,     bg = 'NONE' })  -- Informix functions
  highlight(0, 'sqlRepeat',            { fg = colors.blue,       bg = 'NONE' })  -- Loop constructs

  -- SQL Anywhere
  highlight(0, 'sqlaKeyword',          { fg = colors.blue,       bg = 'NONE' })  -- SQL Anywhere keywords
  highlight(0, 'sqlaType',             { fg = colors.turquoise,  bg = 'NONE' })  -- SQL Anywhere types
  highlight(0, 'sqlaFunction',         { fg = colors.orange,     bg = 'NONE' })  -- SQL Anywhere functions

end

return sql
