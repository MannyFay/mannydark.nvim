-------------------------------------------------------------------------------
-- T-SQL (Transact-SQL / Microsoft SQL Server)
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local tsql      = {}


-------------------------------------------------------------------------------
-- Settings

tsql.setupHighlighting = function()


  ---------------------------------------------------------------------------
  -- T-SQL Specific Keywords
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlKeyword',          { fg = colors.blue,       bg = 'NONE' })  -- T-SQL keywords
  highlight(0, 'tsqlGo',               { fg = colors.blue,       bg = 'NONE', bold = true })  -- GO batch separator
  highlight(0, 'tsqlUse',              { fg = colors.blue,       bg = 'NONE' })  -- USE database
  highlight(0, 'tsqlExec',             { fg = colors.blue,       bg = 'NONE' })  -- EXEC, EXECUTE
  highlight(0, 'tsqlPrint',            { fg = colors.blue,       bg = 'NONE' })  -- PRINT
  highlight(0, 'tsqlSet',              { fg = colors.blue,       bg = 'NONE' })  -- SET


  ---------------------------------------------------------------------------
  -- Variables
  ---------------------------------------------------------------------------

  -- Local Variables
  highlight(0, 'tsqlVariable',         { fg = colors.purple,     bg = 'NONE' })  -- @variable
  highlight(0, 'tsqlLocalVariable',    { fg = colors.purple,     bg = 'NONE' })  -- @local_var

  -- System Variables (@@)
  highlight(0, 'tsqlGlobalVariable',   { fg = colors.purple,     bg = 'NONE' })  -- @@system_variable
  highlight(0, 'tsqlRowcount',         { fg = colors.purple,     bg = 'NONE' })  -- @@ROWCOUNT
  highlight(0, 'tsqlError',            { fg = colors.purple,     bg = 'NONE' })  -- @@ERROR
  highlight(0, 'tsqlIdentity',         { fg = colors.purple,     bg = 'NONE' })  -- @@IDENTITY
  highlight(0, 'tsqlScopeIdentity',    { fg = colors.purple,     bg = 'NONE' })  -- SCOPE_IDENTITY()
  highlight(0, 'tsqlTrancount',        { fg = colors.purple,     bg = 'NONE' })  -- @@TRANCOUNT
  highlight(0, 'tsqlFetch_Status',     { fg = colors.purple,     bg = 'NONE' })  -- @@FETCH_STATUS
  highlight(0, 'tsqlVersion',          { fg = colors.purple,     bg = 'NONE' })  -- @@VERSION
  highlight(0, 'tsqlServername',       { fg = colors.purple,     bg = 'NONE' })  -- @@SERVERNAME
  highlight(0, 'tsqlSpid',             { fg = colors.purple,     bg = 'NONE' })  -- @@SPID


  ---------------------------------------------------------------------------
  -- Temp Tables
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlTempTable',        { fg = colors.turquoise,  bg = 'NONE' })  -- #temp_table
  highlight(0, 'tsqlGlobalTempTable',  { fg = colors.turquoise,  bg = 'NONE' })  -- ##global_temp_table
  highlight(0, 'tsqlTableVariable',    { fg = colors.turquoise,  bg = 'NONE' })  -- @table_variable


  ---------------------------------------------------------------------------
  -- DML Statements
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlSelect',           { fg = colors.blue,       bg = 'NONE' })  -- SELECT
  highlight(0, 'tsqlInsert',           { fg = colors.blue,       bg = 'NONE' })  -- INSERT
  highlight(0, 'tsqlUpdate',           { fg = colors.blue,       bg = 'NONE' })  -- UPDATE
  highlight(0, 'tsqlDelete',           { fg = colors.blue,       bg = 'NONE' })  -- DELETE
  highlight(0, 'tsqlMerge',            { fg = colors.blue,       bg = 'NONE' })  -- MERGE
  highlight(0, 'tsqlTruncate',         { fg = colors.blue,       bg = 'NONE' })  -- TRUNCATE

  -- SELECT Clauses
  highlight(0, 'tsqlFrom',             { fg = colors.blue,       bg = 'NONE' })  -- FROM
  highlight(0, 'tsqlWhere',            { fg = colors.blue,       bg = 'NONE' })  -- WHERE
  highlight(0, 'tsqlGroupBy',          { fg = colors.blue,       bg = 'NONE' })  -- GROUP BY
  highlight(0, 'tsqlHaving',           { fg = colors.blue,       bg = 'NONE' })  -- HAVING
  highlight(0, 'tsqlOrderBy',          { fg = colors.blue,       bg = 'NONE' })  -- ORDER BY
  highlight(0, 'tsqlTop',              { fg = colors.blue,       bg = 'NONE' })  -- TOP
  highlight(0, 'tsqlDistinct',         { fg = colors.blue,       bg = 'NONE' })  -- DISTINCT
  highlight(0, 'tsqlInto',             { fg = colors.blue,       bg = 'NONE' })  -- INTO
  highlight(0, 'tsqlValues',           { fg = colors.blue,       bg = 'NONE' })  -- VALUES

  -- Pagination
  highlight(0, 'tsqlOffset',           { fg = colors.blue,       bg = 'NONE' })  -- OFFSET
  highlight(0, 'tsqlFetch',            { fg = colors.blue,       bg = 'NONE' })  -- FETCH NEXT
  highlight(0, 'tsqlRows',             { fg = colors.blue,       bg = 'NONE' })  -- ROWS ONLY

  -- OUTPUT Clause
  highlight(0, 'tsqlOutput',           { fg = colors.blue,       bg = 'NONE' })  -- OUTPUT
  highlight(0, 'tsqlInserted',         { fg = colors.pink,       bg = 'NONE' })  -- inserted pseudo-table
  highlight(0, 'tsqlDeleted',          { fg = colors.pink,       bg = 'NONE' })  -- deleted pseudo-table


  ---------------------------------------------------------------------------
  -- Joins
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlJoin',             { fg = colors.blue,       bg = 'NONE' })  -- JOIN
  highlight(0, 'tsqlInner',            { fg = colors.blue,       bg = 'NONE' })  -- INNER
  highlight(0, 'tsqlLeft',             { fg = colors.blue,       bg = 'NONE' })  -- LEFT
  highlight(0, 'tsqlRight',            { fg = colors.blue,       bg = 'NONE' })  -- RIGHT
  highlight(0, 'tsqlFull',             { fg = colors.blue,       bg = 'NONE' })  -- FULL
  highlight(0, 'tsqlOuter',            { fg = colors.blue,       bg = 'NONE' })  -- OUTER
  highlight(0, 'tsqlCross',            { fg = colors.blue,       bg = 'NONE' })  -- CROSS
  highlight(0, 'tsqlOn',               { fg = colors.blue,       bg = 'NONE' })  -- ON

  -- APPLY
  highlight(0, 'tsqlApply',            { fg = colors.blue,       bg = 'NONE' })  -- APPLY
  highlight(0, 'tsqlCrossApply',       { fg = colors.blue,       bg = 'NONE' })  -- CROSS APPLY
  highlight(0, 'tsqlOuterApply',       { fg = colors.blue,       bg = 'NONE' })  -- OUTER APPLY

  -- PIVOT/UNPIVOT
  highlight(0, 'tsqlPivot',            { fg = colors.blue,       bg = 'NONE' })  -- PIVOT
  highlight(0, 'tsqlUnpivot',          { fg = colors.blue,       bg = 'NONE' })  -- UNPIVOT
  highlight(0, 'tsqlFor',              { fg = colors.blue,       bg = 'NONE' })  -- FOR


  ---------------------------------------------------------------------------
  -- Set Operations
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlUnion',            { fg = colors.blue,       bg = 'NONE' })  -- UNION
  highlight(0, 'tsqlUnionAll',         { fg = colors.blue,       bg = 'NONE' })  -- UNION ALL
  highlight(0, 'tsqlIntersect',        { fg = colors.blue,       bg = 'NONE' })  -- INTERSECT
  highlight(0, 'tsqlExcept',           { fg = colors.blue,       bg = 'NONE' })  -- EXCEPT


  ---------------------------------------------------------------------------
  -- Common Table Expressions (CTE)
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlWith',             { fg = colors.blue,       bg = 'NONE' })  -- WITH
  highlight(0, 'tsqlAs',               { fg = colors.blue,       bg = 'NONE' })  -- AS
  highlight(0, 'tsqlRecursive',        { fg = colors.blue,       bg = 'NONE' })  -- Recursive CTEs


  ---------------------------------------------------------------------------
  -- DDL Statements
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlCreate',           { fg = colors.blue,       bg = 'NONE' })  -- CREATE
  highlight(0, 'tsqlAlter',            { fg = colors.blue,       bg = 'NONE' })  -- ALTER
  highlight(0, 'tsqlDrop',             { fg = colors.blue,       bg = 'NONE' })  -- DROP

  -- Objects
  highlight(0, 'tsqlTable',            { fg = colors.blue,       bg = 'NONE' })  -- TABLE
  highlight(0, 'tsqlView',             { fg = colors.blue,       bg = 'NONE' })  -- VIEW
  highlight(0, 'tsqlIndex',            { fg = colors.blue,       bg = 'NONE' })  -- INDEX
  highlight(0, 'tsqlProcedure',        { fg = colors.blue,       bg = 'NONE' })  -- PROCEDURE, PROC
  highlight(0, 'tsqlFunction',         { fg = colors.blue,       bg = 'NONE' })  -- FUNCTION
  highlight(0, 'tsqlTrigger',          { fg = colors.blue,       bg = 'NONE' })  -- TRIGGER
  highlight(0, 'tsqlSchema',           { fg = colors.blue,       bg = 'NONE' })  -- SCHEMA
  highlight(0, 'tsqlDatabase',         { fg = colors.blue,       bg = 'NONE' })  -- DATABASE
  highlight(0, 'tsqlSequence',         { fg = colors.blue,       bg = 'NONE' })  -- SEQUENCE
  highlight(0, 'tsqlSynonym',          { fg = colors.blue,       bg = 'NONE' })  -- SYNONYM
  highlight(0, 'tsqlType',             { fg = colors.blue,       bg = 'NONE' })  -- TYPE


  ---------------------------------------------------------------------------
  -- Stored Procedures & Functions
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlProc',             { fg = colors.blue,       bg = 'NONE' })  -- PROC keyword
  highlight(0, 'tsqlProcName',         { fg = colors.orange,     bg = 'NONE' })  -- Procedure name
  highlight(0, 'tsqlFuncName',         { fg = colors.orange,     bg = 'NONE' })  -- Function name
  highlight(0, 'tsqlReturns',          { fg = colors.blue,       bg = 'NONE' })  -- RETURNS
  highlight(0, 'tsqlReturn',           { fg = colors.blue,       bg = 'NONE' })  -- RETURN
  highlight(0, 'tsqlBegin',            { fg = colors.blue,       bg = 'NONE' })  -- BEGIN
  highlight(0, 'tsqlEnd',              { fg = colors.blue,       bg = 'NONE' })  -- END
  highlight(0, 'tsqlDeclare',          { fg = colors.blue,       bg = 'NONE' })  -- DECLARE

  -- Parameter Modifiers
  highlight(0, 'tsqlOutput_Param',     { fg = colors.blue,       bg = 'NONE' })  -- OUTPUT parameter
  highlight(0, 'tsqlReadonly',         { fg = colors.blue,       bg = 'NONE' })  -- READONLY


  ---------------------------------------------------------------------------
  -- Triggers
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlTriggerName',      { fg = colors.orange,     bg = 'NONE' })  -- Trigger names
  highlight(0, 'tsqlAfter',            { fg = colors.blue,       bg = 'NONE' })  -- AFTER
  highlight(0, 'tsqlInsteadOf',        { fg = colors.blue,       bg = 'NONE' })  -- INSTEAD OF
  highlight(0, 'tsqlForTrigger',       { fg = colors.blue,       bg = 'NONE' })  -- FOR (trigger)
  highlight(0, 'tsqlEnable',           { fg = colors.blue,       bg = 'NONE' })  -- ENABLE
  highlight(0, 'tsqlDisable',          { fg = colors.blue,       bg = 'NONE' })  -- DISABLE


  ---------------------------------------------------------------------------
  -- Control Flow
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlIf',               { fg = colors.blue,       bg = 'NONE' })  -- IF
  highlight(0, 'tsqlElse',             { fg = colors.blue,       bg = 'NONE' })  -- ELSE
  highlight(0, 'tsqlWhile',            { fg = colors.blue,       bg = 'NONE' })  -- WHILE
  highlight(0, 'tsqlBreak',            { fg = colors.blue,       bg = 'NONE' })  -- BREAK
  highlight(0, 'tsqlContinue',         { fg = colors.blue,       bg = 'NONE' })  -- CONTINUE
  highlight(0, 'tsqlGoto',             { fg = colors.blue,       bg = 'NONE' })  -- GOTO
  highlight(0, 'tsqlLabel',            { fg = colors.pink,       bg = 'NONE' })  -- Label:
  highlight(0, 'tsqlWaitfor',          { fg = colors.blue,       bg = 'NONE' })  -- WAITFOR
  highlight(0, 'tsqlDelay',            { fg = colors.blue,       bg = 'NONE' })  -- DELAY
  highlight(0, 'tsqlTime',             { fg = colors.blue,       bg = 'NONE' })  -- TIME

  -- CASE Expression
  highlight(0, 'tsqlCase',             { fg = colors.blue,       bg = 'NONE' })  -- CASE
  highlight(0, 'tsqlWhen',             { fg = colors.blue,       bg = 'NONE' })  -- WHEN
  highlight(0, 'tsqlThen',             { fg = colors.blue,       bg = 'NONE' })  -- THEN
  highlight(0, 'tsqlEndCase',          { fg = colors.blue,       bg = 'NONE' })  -- END


  ---------------------------------------------------------------------------
  -- Error Handling
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlTry',              { fg = colors.blue,       bg = 'NONE' })  -- BEGIN TRY
  highlight(0, 'tsqlCatch',            { fg = colors.blue,       bg = 'NONE' })  -- BEGIN CATCH
  highlight(0, 'tsqlEndTry',           { fg = colors.blue,       bg = 'NONE' })  -- END TRY
  highlight(0, 'tsqlEndCatch',         { fg = colors.blue,       bg = 'NONE' })  -- END CATCH
  highlight(0, 'tsqlThrow',            { fg = colors.blue,       bg = 'NONE' })  -- THROW
  highlight(0, 'tsqlRaiserror',        { fg = colors.blue,       bg = 'NONE' })  -- RAISERROR

  -- Error Functions
  highlight(0, 'tsqlErrorNumber',      { fg = colors.orange,     bg = 'NONE' })  -- ERROR_NUMBER()
  highlight(0, 'tsqlErrorMessage',     { fg = colors.orange,     bg = 'NONE' })  -- ERROR_MESSAGE()
  highlight(0, 'tsqlErrorSeverity',    { fg = colors.orange,     bg = 'NONE' })  -- ERROR_SEVERITY()
  highlight(0, 'tsqlErrorState',       { fg = colors.orange,     bg = 'NONE' })  -- ERROR_STATE()
  highlight(0, 'tsqlErrorLine',        { fg = colors.orange,     bg = 'NONE' })  -- ERROR_LINE()
  highlight(0, 'tsqlErrorProcedure',   { fg = colors.orange,     bg = 'NONE' })  -- ERROR_PROCEDURE()
  highlight(0, 'tsqlXactState',        { fg = colors.orange,     bg = 'NONE' })  -- XACT_STATE()


  ---------------------------------------------------------------------------
  -- Transaction Control
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlTransaction',      { fg = colors.blue,       bg = 'NONE' })  -- TRANSACTION, TRAN
  highlight(0, 'tsqlBeginTran',        { fg = colors.blue,       bg = 'NONE' })  -- BEGIN TRAN
  highlight(0, 'tsqlCommit',           { fg = colors.blue,       bg = 'NONE' })  -- COMMIT
  highlight(0, 'tsqlRollback',         { fg = colors.blue,       bg = 'NONE' })  -- ROLLBACK
  highlight(0, 'tsqlSave',             { fg = colors.blue,       bg = 'NONE' })  -- SAVE TRANSACTION
  highlight(0, 'tsqlXactAbort',        { fg = colors.blue,       bg = 'NONE' })  -- SET XACT_ABORT


  ---------------------------------------------------------------------------
  -- Cursors
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlCursor',           { fg = colors.blue,       bg = 'NONE' })  -- CURSOR
  highlight(0, 'tsqlOpen',             { fg = colors.blue,       bg = 'NONE' })  -- OPEN
  highlight(0, 'tsqlClose',            { fg = colors.blue,       bg = 'NONE' })  -- CLOSE
  highlight(0, 'tsqlDeallocate',       { fg = colors.blue,       bg = 'NONE' })  -- DEALLOCATE
  highlight(0, 'tsqlFetchCursor',      { fg = colors.blue,       bg = 'NONE' })  -- FETCH
  highlight(0, 'tsqlNext',             { fg = colors.blue,       bg = 'NONE' })  -- NEXT
  highlight(0, 'tsqlPrior',            { fg = colors.blue,       bg = 'NONE' })  -- PRIOR
  highlight(0, 'tsqlFirst',            { fg = colors.blue,       bg = 'NONE' })  -- FIRST
  highlight(0, 'tsqlLast',             { fg = colors.blue,       bg = 'NONE' })  -- LAST
  highlight(0, 'tsqlAbsolute',         { fg = colors.blue,       bg = 'NONE' })  -- ABSOLUTE
  highlight(0, 'tsqlRelative',         { fg = colors.blue,       bg = 'NONE' })  -- RELATIVE

  -- Cursor Options
  highlight(0, 'tsqlLocalCursor',      { fg = colors.blue,       bg = 'NONE' })  -- LOCAL
  highlight(0, 'tsqlGlobalCursor',     { fg = colors.blue,       bg = 'NONE' })  -- GLOBAL
  highlight(0, 'tsqlForwardOnly',      { fg = colors.blue,       bg = 'NONE' })  -- FORWARD_ONLY
  highlight(0, 'tsqlScroll',           { fg = colors.blue,       bg = 'NONE' })  -- SCROLL
  highlight(0, 'tsqlStatic',           { fg = colors.blue,       bg = 'NONE' })  -- STATIC
  highlight(0, 'tsqlKeyset',           { fg = colors.blue,       bg = 'NONE' })  -- KEYSET
  highlight(0, 'tsqlDynamic',          { fg = colors.blue,       bg = 'NONE' })  -- DYNAMIC
  highlight(0, 'tsqlFastForward',      { fg = colors.blue,       bg = 'NONE' })  -- FAST_FORWARD
  highlight(0, 'tsqlReadOnly',         { fg = colors.blue,       bg = 'NONE' })  -- READ_ONLY
  highlight(0, 'tsqlScrollLocks',      { fg = colors.blue,       bg = 'NONE' })  -- SCROLL_LOCKS
  highlight(0, 'tsqlOptimistic',       { fg = colors.blue,       bg = 'NONE' })  -- OPTIMISTIC


  ---------------------------------------------------------------------------
  -- Operators
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlOperator',         { fg = colors.blue,       bg = 'NONE' })  -- Logical operators
  highlight(0, 'tsqlAnd',              { fg = colors.blue,       bg = 'NONE' })  -- AND
  highlight(0, 'tsqlOr',               { fg = colors.blue,       bg = 'NONE' })  -- OR
  highlight(0, 'tsqlNot',              { fg = colors.blue,       bg = 'NONE' })  -- NOT
  highlight(0, 'tsqlIn',               { fg = colors.blue,       bg = 'NONE' })  -- IN
  highlight(0, 'tsqlBetween',          { fg = colors.blue,       bg = 'NONE' })  -- BETWEEN
  highlight(0, 'tsqlLike',             { fg = colors.blue,       bg = 'NONE' })  -- LIKE
  highlight(0, 'tsqlIs',               { fg = colors.blue,       bg = 'NONE' })  -- IS
  highlight(0, 'tsqlNull',             { fg = colors.pink,       bg = 'NONE' })  -- NULL
  highlight(0, 'tsqlExists',           { fg = colors.blue,       bg = 'NONE' })  -- EXISTS
  highlight(0, 'tsqlAll',              { fg = colors.blue,       bg = 'NONE' })  -- ALL
  highlight(0, 'tsqlAny',              { fg = colors.blue,       bg = 'NONE' })  -- ANY
  highlight(0, 'tsqlSome',             { fg = colors.blue,       bg = 'NONE' })  -- SOME

  -- Arithmetic/Comparison
  highlight(0, 'tsqlComparison',       { fg = colors.white,      bg = 'NONE' })  -- =, <>, <, >, <=, >=, !=
  highlight(0, 'tsqlArithmetic',       { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, %

  -- String Operators
  highlight(0, 'tsqlConcat',           { fg = colors.white,      bg = 'NONE' })  -- + (string concat)
  highlight(0, 'tsqlConcatWs',         { fg = colors.orange,     bg = 'NONE' })  -- CONCAT_WS


  ---------------------------------------------------------------------------
  -- Constraints
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlConstraint',       { fg = colors.pink,       bg = 'NONE' })  -- CONSTRAINT
  highlight(0, 'tsqlPrimaryKey',       { fg = colors.pink,       bg = 'NONE' })  -- PRIMARY KEY
  highlight(0, 'tsqlForeignKey',       { fg = colors.pink,       bg = 'NONE' })  -- FOREIGN KEY
  highlight(0, 'tsqlReferences',       { fg = colors.pink,       bg = 'NONE' })  -- REFERENCES
  highlight(0, 'tsqlUnique',           { fg = colors.pink,       bg = 'NONE' })  -- UNIQUE
  highlight(0, 'tsqlCheck',            { fg = colors.pink,       bg = 'NONE' })  -- CHECK
  highlight(0, 'tsqlDefault',          { fg = colors.pink,       bg = 'NONE' })  -- DEFAULT
  highlight(0, 'tsqlNotNull',          { fg = colors.pink,       bg = 'NONE' })  -- NOT NULL
  highlight(0, 'tsqlIdentityCol',      { fg = colors.pink,       bg = 'NONE' })  -- IDENTITY
  highlight(0, 'tsqlClustered',        { fg = colors.pink,       bg = 'NONE' })  -- CLUSTERED
  highlight(0, 'tsqlNonclustered',     { fg = colors.pink,       bg = 'NONE' })  -- NONCLUSTERED

  -- Cascade Actions
  highlight(0, 'tsqlCascade',          { fg = colors.pink,       bg = 'NONE' })  -- CASCADE
  highlight(0, 'tsqlSetNull',          { fg = colors.pink,       bg = 'NONE' })  -- SET NULL
  highlight(0, 'tsqlSetDefault',       { fg = colors.pink,       bg = 'NONE' })  -- SET DEFAULT
  highlight(0, 'tsqlNoAction',         { fg = colors.pink,       bg = 'NONE' })  -- NO ACTION


  ---------------------------------------------------------------------------
  -- Data Types
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlDataType',         { fg = colors.turquoise,  bg = 'NONE' })  -- Data types

  -- Exact Numerics
  highlight(0, 'tsqlBigint',           { fg = colors.turquoise,  bg = 'NONE' })  -- BIGINT
  highlight(0, 'tsqlInt',              { fg = colors.turquoise,  bg = 'NONE' })  -- INT, INTEGER
  highlight(0, 'tsqlSmallint',         { fg = colors.turquoise,  bg = 'NONE' })  -- SMALLINT
  highlight(0, 'tsqlTinyint',          { fg = colors.turquoise,  bg = 'NONE' })  -- TINYINT
  highlight(0, 'tsqlBit',              { fg = colors.turquoise,  bg = 'NONE' })  -- BIT
  highlight(0, 'tsqlDecimal',          { fg = colors.turquoise,  bg = 'NONE' })  -- DECIMAL
  highlight(0, 'tsqlNumeric',          { fg = colors.turquoise,  bg = 'NONE' })  -- NUMERIC
  highlight(0, 'tsqlMoney',            { fg = colors.turquoise,  bg = 'NONE' })  -- MONEY
  highlight(0, 'tsqlSmallmoney',       { fg = colors.turquoise,  bg = 'NONE' })  -- SMALLMONEY

  -- Approximate Numerics
  highlight(0, 'tsqlFloat',            { fg = colors.turquoise,  bg = 'NONE' })  -- FLOAT
  highlight(0, 'tsqlReal',             { fg = colors.turquoise,  bg = 'NONE' })  -- REAL

  -- Character Strings
  highlight(0, 'tsqlChar',             { fg = colors.turquoise,  bg = 'NONE' })  -- CHAR
  highlight(0, 'tsqlVarchar',          { fg = colors.turquoise,  bg = 'NONE' })  -- VARCHAR
  highlight(0, 'tsqlVarcharMax',       { fg = colors.turquoise,  bg = 'NONE' })  -- VARCHAR(MAX)
  highlight(0, 'tsqlText',             { fg = colors.turquoise,  bg = 'NONE' })  -- TEXT (deprecated)

  -- Unicode Character Strings
  highlight(0, 'tsqlNchar',            { fg = colors.turquoise,  bg = 'NONE' })  -- NCHAR
  highlight(0, 'tsqlNvarchar',         { fg = colors.turquoise,  bg = 'NONE' })  -- NVARCHAR
  highlight(0, 'tsqlNvarcharMax',      { fg = colors.turquoise,  bg = 'NONE' })  -- NVARCHAR(MAX)
  highlight(0, 'tsqlNtext',            { fg = colors.turquoise,  bg = 'NONE' })  -- NTEXT (deprecated)

  -- Binary Strings
  highlight(0, 'tsqlBinary',           { fg = colors.turquoise,  bg = 'NONE' })  -- BINARY
  highlight(0, 'tsqlVarbinary',        { fg = colors.turquoise,  bg = 'NONE' })  -- VARBINARY
  highlight(0, 'tsqlVarbinaryMax',     { fg = colors.turquoise,  bg = 'NONE' })  -- VARBINARY(MAX)
  highlight(0, 'tsqlImage',            { fg = colors.turquoise,  bg = 'NONE' })  -- IMAGE (deprecated)

  -- Date and Time
  highlight(0, 'tsqlDate',             { fg = colors.turquoise,  bg = 'NONE' })  -- DATE
  highlight(0, 'tsqlTime',             { fg = colors.turquoise,  bg = 'NONE' })  -- TIME
  highlight(0, 'tsqlDatetime',         { fg = colors.turquoise,  bg = 'NONE' })  -- DATETIME
  highlight(0, 'tsqlDatetime2',        { fg = colors.turquoise,  bg = 'NONE' })  -- DATETIME2
  highlight(0, 'tsqlDatetimeoffset',   { fg = colors.turquoise,  bg = 'NONE' })  -- DATETIMEOFFSET
  highlight(0, 'tsqlSmalldatetime',    { fg = colors.turquoise,  bg = 'NONE' })  -- SMALLDATETIME

  -- Other Types
  highlight(0, 'tsqlUniqueidentifier', { fg = colors.turquoise,  bg = 'NONE' })  -- UNIQUEIDENTIFIER
  highlight(0, 'tsqlXml',              { fg = colors.turquoise,  bg = 'NONE' })  -- XML
  highlight(0, 'tsqlJson',             { fg = colors.turquoise,  bg = 'NONE' })  -- JSON
  highlight(0, 'tsqlSqlVariant',       { fg = colors.turquoise,  bg = 'NONE' })  -- SQL_VARIANT
  highlight(0, 'tsqlHierarchyid',      { fg = colors.turquoise,  bg = 'NONE' })  -- HIERARCHYID
  highlight(0, 'tsqlGeometry',         { fg = colors.turquoise,  bg = 'NONE' })  -- GEOMETRY
  highlight(0, 'tsqlGeography',        { fg = colors.turquoise,  bg = 'NONE' })  -- GEOGRAPHY
  highlight(0, 'tsqlRowversion',       { fg = colors.turquoise,  bg = 'NONE' })  -- ROWVERSION
  highlight(0, 'tsqlTimestamp',        { fg = colors.turquoise,  bg = 'NONE' })  -- TIMESTAMP (deprecated)
  highlight(0, 'tsqlCursor',           { fg = colors.turquoise,  bg = 'NONE' })  -- CURSOR type
  highlight(0, 'tsqlTable',            { fg = colors.turquoise,  bg = 'NONE' })  -- TABLE type


  ---------------------------------------------------------------------------
  -- Built-in Functions
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlBuiltinFunc',      { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions

  -- Aggregate Functions
  highlight(0, 'tsqlCount',            { fg = colors.orange,     bg = 'NONE' })  -- COUNT
  highlight(0, 'tsqlCountBig',         { fg = colors.orange,     bg = 'NONE' })  -- COUNT_BIG
  highlight(0, 'tsqlSum',              { fg = colors.orange,     bg = 'NONE' })  -- SUM
  highlight(0, 'tsqlAvg',              { fg = colors.orange,     bg = 'NONE' })  -- AVG
  highlight(0, 'tsqlMin',              { fg = colors.orange,     bg = 'NONE' })  -- MIN
  highlight(0, 'tsqlMax',              { fg = colors.orange,     bg = 'NONE' })  -- MAX
  highlight(0, 'tsqlStringAgg',        { fg = colors.orange,     bg = 'NONE' })  -- STRING_AGG
  highlight(0, 'tsqlGrouping',         { fg = colors.orange,     bg = 'NONE' })  -- GROUPING
  highlight(0, 'tsqlGroupingSets',     { fg = colors.orange,     bg = 'NONE' })  -- GROUPING SETS
  highlight(0, 'tsqlRollup',           { fg = colors.orange,     bg = 'NONE' })  -- ROLLUP
  highlight(0, 'tsqlCube',             { fg = colors.orange,     bg = 'NONE' })  -- CUBE

  -- String Functions
  highlight(0, 'tsqlLen',              { fg = colors.orange,     bg = 'NONE' })  -- LEN
  highlight(0, 'tsqlDatalength',       { fg = colors.orange,     bg = 'NONE' })  -- DATALENGTH
  highlight(0, 'tsqlSubstring',        { fg = colors.orange,     bg = 'NONE' })  -- SUBSTRING
  highlight(0, 'tsqlLeft',             { fg = colors.orange,     bg = 'NONE' })  -- LEFT
  highlight(0, 'tsqlRight',            { fg = colors.orange,     bg = 'NONE' })  -- RIGHT
  highlight(0, 'tsqlCharindex',        { fg = colors.orange,     bg = 'NONE' })  -- CHARINDEX
  highlight(0, 'tsqlPatindex',         { fg = colors.orange,     bg = 'NONE' })  -- PATINDEX
  highlight(0, 'tsqlReplace',          { fg = colors.orange,     bg = 'NONE' })  -- REPLACE
  highlight(0, 'tsqlStuff',            { fg = colors.orange,     bg = 'NONE' })  -- STUFF
  highlight(0, 'tsqlReverse',          { fg = colors.orange,     bg = 'NONE' })  -- REVERSE
  highlight(0, 'tsqlUpper',            { fg = colors.orange,     bg = 'NONE' })  -- UPPER
  highlight(0, 'tsqlLower',            { fg = colors.orange,     bg = 'NONE' })  -- LOWER
  highlight(0, 'tsqlLtrim',            { fg = colors.orange,     bg = 'NONE' })  -- LTRIM
  highlight(0, 'tsqlRtrim',            { fg = colors.orange,     bg = 'NONE' })  -- RTRIM
  highlight(0, 'tsqlTrimFunc',         { fg = colors.orange,     bg = 'NONE' })  -- TRIM
  highlight(0, 'tsqlConcat',           { fg = colors.orange,     bg = 'NONE' })  -- CONCAT
  highlight(0, 'tsqlFormat',           { fg = colors.orange,     bg = 'NONE' })  -- FORMAT
  highlight(0, 'tsqlReplicate',        { fg = colors.orange,     bg = 'NONE' })  -- REPLICATE
  highlight(0, 'tsqlSpace',            { fg = colors.orange,     bg = 'NONE' })  -- SPACE
  highlight(0, 'tsqlStr',              { fg = colors.orange,     bg = 'NONE' })  -- STR
  highlight(0, 'tsqlTranslate',        { fg = colors.orange,     bg = 'NONE' })  -- TRANSLATE
  highlight(0, 'tsqlStringEscape',     { fg = colors.orange,     bg = 'NONE' })  -- STRING_ESCAPE
  highlight(0, 'tsqlStringSplit',      { fg = colors.orange,     bg = 'NONE' })  -- STRING_SPLIT

  -- Date/Time Functions
  highlight(0, 'tsqlGetdate',          { fg = colors.orange,     bg = 'NONE' })  -- GETDATE
  highlight(0, 'tsqlGetutcdate',       { fg = colors.orange,     bg = 'NONE' })  -- GETUTCDATE
  highlight(0, 'tsqlSysdatetime',      { fg = colors.orange,     bg = 'NONE' })  -- SYSDATETIME
  highlight(0, 'tsqlSysutcdatetime',   { fg = colors.orange,     bg = 'NONE' })  -- SYSUTCDATETIME
  highlight(0, 'tsqlSysdatetimeoffset', { fg = colors.orange,    bg = 'NONE' })  -- SYSDATETIMEOFFSET
  highlight(0, 'tsqlCurrentTimestamp', { fg = colors.orange,     bg = 'NONE' })  -- CURRENT_TIMESTAMP
  highlight(0, 'tsqlDateadd',          { fg = colors.orange,     bg = 'NONE' })  -- DATEADD
  highlight(0, 'tsqlDatediff',         { fg = colors.orange,     bg = 'NONE' })  -- DATEDIFF
  highlight(0, 'tsqlDatediffBig',      { fg = colors.orange,     bg = 'NONE' })  -- DATEDIFF_BIG
  highlight(0, 'tsqlDatename',         { fg = colors.orange,     bg = 'NONE' })  -- DATENAME
  highlight(0, 'tsqlDatepart',         { fg = colors.orange,     bg = 'NONE' })  -- DATEPART
  highlight(0, 'tsqlDatefromparts',    { fg = colors.orange,     bg = 'NONE' })  -- DATEFROMPARTS
  highlight(0, 'tsqlDatetimefromparts', { fg = colors.orange,    bg = 'NONE' })  -- DATETIMEFROMPARTS
  highlight(0, 'tsqlDatetrunc',        { fg = colors.orange,     bg = 'NONE' })  -- DATETRUNC (2022)
  highlight(0, 'tsqlDateBucket',       { fg = colors.orange,     bg = 'NONE' })  -- DATE_BUCKET (2022)
  highlight(0, 'tsqlYear',             { fg = colors.orange,     bg = 'NONE' })  -- YEAR
  highlight(0, 'tsqlMonth',            { fg = colors.orange,     bg = 'NONE' })  -- MONTH
  highlight(0, 'tsqlDay',              { fg = colors.orange,     bg = 'NONE' })  -- DAY
  highlight(0, 'tsqlEomonth',          { fg = colors.orange,     bg = 'NONE' })  -- EOMONTH
  highlight(0, 'tsqlIsdate',           { fg = colors.orange,     bg = 'NONE' })  -- ISDATE

  -- Conversion Functions
  highlight(0, 'tsqlCast',             { fg = colors.orange,     bg = 'NONE' })  -- CAST
  highlight(0, 'tsqlConvert',          { fg = colors.orange,     bg = 'NONE' })  -- CONVERT
  highlight(0, 'tsqlTryCast',          { fg = colors.orange,     bg = 'NONE' })  -- TRY_CAST
  highlight(0, 'tsqlTryConvert',       { fg = colors.orange,     bg = 'NONE' })  -- TRY_CONVERT
  highlight(0, 'tsqlTryParse',         { fg = colors.orange,     bg = 'NONE' })  -- TRY_PARSE
  highlight(0, 'tsqlParse',            { fg = colors.orange,     bg = 'NONE' })  -- PARSE

  -- NULL Functions
  highlight(0, 'tsqlIsnull',           { fg = colors.orange,     bg = 'NONE' })  -- ISNULL
  highlight(0, 'tsqlCoalesce',         { fg = colors.orange,     bg = 'NONE' })  -- COALESCE
  highlight(0, 'tsqlNullif',           { fg = colors.orange,     bg = 'NONE' })  -- NULLIF

  -- Logical Functions
  highlight(0, 'tsqlIif',              { fg = colors.orange,     bg = 'NONE' })  -- IIF
  highlight(0, 'tsqlChoose',           { fg = colors.orange,     bg = 'NONE' })  -- CHOOSE
  highlight(0, 'tsqlGreatest',         { fg = colors.orange,     bg = 'NONE' })  -- GREATEST (2022)
  highlight(0, 'tsqlLeast',            { fg = colors.orange,     bg = 'NONE' })  -- LEAST (2022)

  -- Math Functions
  highlight(0, 'tsqlAbs',              { fg = colors.orange,     bg = 'NONE' })  -- ABS
  highlight(0, 'tsqlCeiling',          { fg = colors.orange,     bg = 'NONE' })  -- CEILING
  highlight(0, 'tsqlFloor',            { fg = colors.orange,     bg = 'NONE' })  -- FLOOR
  highlight(0, 'tsqlRound',            { fg = colors.orange,     bg = 'NONE' })  -- ROUND
  highlight(0, 'tsqlPower',            { fg = colors.orange,     bg = 'NONE' })  -- POWER
  highlight(0, 'tsqlSqrt',             { fg = colors.orange,     bg = 'NONE' })  -- SQRT
  highlight(0, 'tsqlSign',             { fg = colors.orange,     bg = 'NONE' })  -- SIGN
  highlight(0, 'tsqlRand',             { fg = colors.orange,     bg = 'NONE' })  -- RAND
  highlight(0, 'tsqlLog',              { fg = colors.orange,     bg = 'NONE' })  -- LOG, LOG10

  -- Ranking/Window Functions
  highlight(0, 'tsqlRowNumber',        { fg = colors.orange,     bg = 'NONE' })  -- ROW_NUMBER
  highlight(0, 'tsqlRank',             { fg = colors.orange,     bg = 'NONE' })  -- RANK
  highlight(0, 'tsqlDenseRank',        { fg = colors.orange,     bg = 'NONE' })  -- DENSE_RANK
  highlight(0, 'tsqlNtile',            { fg = colors.orange,     bg = 'NONE' })  -- NTILE
  highlight(0, 'tsqlLag',              { fg = colors.orange,     bg = 'NONE' })  -- LAG
  highlight(0, 'tsqlLead',             { fg = colors.orange,     bg = 'NONE' })  -- LEAD
  highlight(0, 'tsqlFirstValue',       { fg = colors.orange,     bg = 'NONE' })  -- FIRST_VALUE
  highlight(0, 'tsqlLastValue',        { fg = colors.orange,     bg = 'NONE' })  -- LAST_VALUE
  highlight(0, 'tsqlPercentileCont',   { fg = colors.orange,     bg = 'NONE' })  -- PERCENTILE_CONT
  highlight(0, 'tsqlPercentileDisc',   { fg = colors.orange,     bg = 'NONE' })  -- PERCENTILE_DISC
  highlight(0, 'tsqlCumeDist',         { fg = colors.orange,     bg = 'NONE' })  -- CUME_DIST
  highlight(0, 'tsqlPercentRank',      { fg = colors.orange,     bg = 'NONE' })  -- PERCENT_RANK

  -- Window Clause
  highlight(0, 'tsqlOver',             { fg = colors.blue,       bg = 'NONE' })  -- OVER
  highlight(0, 'tsqlPartitionBy',      { fg = colors.blue,       bg = 'NONE' })  -- PARTITION BY
  highlight(0, 'tsqlRowsRange',        { fg = colors.blue,       bg = 'NONE' })  -- ROWS, RANGE
  highlight(0, 'tsqlUnbounded',        { fg = colors.blue,       bg = 'NONE' })  -- UNBOUNDED
  highlight(0, 'tsqlPreceding',        { fg = colors.blue,       bg = 'NONE' })  -- PRECEDING
  highlight(0, 'tsqlFollowing',        { fg = colors.blue,       bg = 'NONE' })  -- FOLLOWING
  highlight(0, 'tsqlCurrentRow',       { fg = colors.blue,       bg = 'NONE' })  -- CURRENT ROW

  -- JSON Functions
  highlight(0, 'tsqlJsonValue',        { fg = colors.orange,     bg = 'NONE' })  -- JSON_VALUE
  highlight(0, 'tsqlJsonQuery',        { fg = colors.orange,     bg = 'NONE' })  -- JSON_QUERY
  highlight(0, 'tsqlJsonModify',       { fg = colors.orange,     bg = 'NONE' })  -- JSON_MODIFY
  highlight(0, 'tsqlIsjson',           { fg = colors.orange,     bg = 'NONE' })  -- ISJSON
  highlight(0, 'tsqlOpenjson',         { fg = colors.orange,     bg = 'NONE' })  -- OPENJSON
  highlight(0, 'tsqlForJson',          { fg = colors.blue,       bg = 'NONE' })  -- FOR JSON

  -- XML Functions
  highlight(0, 'tsqlForXml',           { fg = colors.blue,       bg = 'NONE' })  -- FOR XML
  highlight(0, 'tsqlOpenxml',          { fg = colors.orange,     bg = 'NONE' })  -- OPENXML


  ---------------------------------------------------------------------------
  -- System Stored Procedures
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlSp',               { fg = colors.orange,     bg = 'NONE' })  -- sp_* procedures
  highlight(0, 'tsqlSpExecutesql',     { fg = colors.orange,     bg = 'NONE' })  -- sp_executesql
  highlight(0, 'tsqlSpHelp',           { fg = colors.orange,     bg = 'NONE' })  -- sp_help
  highlight(0, 'tsqlSpHelptext',       { fg = colors.orange,     bg = 'NONE' })  -- sp_helptext
  highlight(0, 'tsqlSpWho',            { fg = colors.orange,     bg = 'NONE' })  -- sp_who, sp_who2
  highlight(0, 'tsqlSpRename',         { fg = colors.orange,     bg = 'NONE' })  -- sp_rename
  highlight(0, 'tsqlSpConfigure',      { fg = colors.orange,     bg = 'NONE' })  -- sp_configure
  highlight(0, 'tsqlSpLock',           { fg = colors.orange,     bg = 'NONE' })  -- sp_lock
  highlight(0, 'tsqlSpSpaceused',      { fg = colors.orange,     bg = 'NONE' })  -- sp_spaceused

  -- Extended Stored Procedures
  highlight(0, 'tsqlXp',               { fg = colors.orange,     bg = 'NONE' })  -- xp_* procedures
  highlight(0, 'tsqlXpCmdshell',       { fg = colors.orange,     bg = 'NONE' })  -- xp_cmdshell


  ---------------------------------------------------------------------------
  -- Table Hints
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlHint',             { fg = colors.pink,       bg = 'NONE' })  -- Hints
  highlight(0, 'tsqlNolock',           { fg = colors.pink,       bg = 'NONE' })  -- NOLOCK
  highlight(0, 'tsqlHoldlock',         { fg = colors.pink,       bg = 'NONE' })  -- HOLDLOCK
  highlight(0, 'tsqlRowlock',          { fg = colors.pink,       bg = 'NONE' })  -- ROWLOCK
  highlight(0, 'tsqlPagelock',         { fg = colors.pink,       bg = 'NONE' })  -- PAGELOCK
  highlight(0, 'tsqlTablock',          { fg = colors.pink,       bg = 'NONE' })  -- TABLOCK
  highlight(0, 'tsqlTablockx',         { fg = colors.pink,       bg = 'NONE' })  -- TABLOCKX
  highlight(0, 'tsqlUpdlock',          { fg = colors.pink,       bg = 'NONE' })  -- UPDLOCK
  highlight(0, 'tsqlXlock',            { fg = colors.pink,       bg = 'NONE' })  -- XLOCK
  highlight(0, 'tsqlReadpast',         { fg = colors.pink,       bg = 'NONE' })  -- READPAST
  highlight(0, 'tsqlReadcommitted',    { fg = colors.pink,       bg = 'NONE' })  -- READCOMMITTED
  highlight(0, 'tsqlReaduncommitted',  { fg = colors.pink,       bg = 'NONE' })  -- READUNCOMMITTED
  highlight(0, 'tsqlSerializable',     { fg = colors.pink,       bg = 'NONE' })  -- SERIALIZABLE
  highlight(0, 'tsqlSnapshot',         { fg = colors.pink,       bg = 'NONE' })  -- SNAPSHOT
  highlight(0, 'tsqlForceseek',        { fg = colors.pink,       bg = 'NONE' })  -- FORCESEEK
  highlight(0, 'tsqlForcescan',        { fg = colors.pink,       bg = 'NONE' })  -- FORCESCAN


  ---------------------------------------------------------------------------
  -- Query Hints
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlOption',           { fg = colors.blue,       bg = 'NONE' })  -- OPTION
  highlight(0, 'tsqlRecompile',        { fg = colors.pink,       bg = 'NONE' })  -- RECOMPILE
  highlight(0, 'tsqlOptimizeFor',      { fg = colors.pink,       bg = 'NONE' })  -- OPTIMIZE FOR
  highlight(0, 'tsqlMaxdop',           { fg = colors.pink,       bg = 'NONE' })  -- MAXDOP
  highlight(0, 'tsqlFastN',            { fg = colors.pink,       bg = 'NONE' })  -- FAST n
  highlight(0, 'tsqlForceOrder',       { fg = colors.pink,       bg = 'NONE' })  -- FORCE ORDER
  highlight(0, 'tsqlLoop',             { fg = colors.pink,       bg = 'NONE' })  -- LOOP JOIN hint
  highlight(0, 'tsqlHash',             { fg = colors.pink,       bg = 'NONE' })  -- HASH JOIN hint
  highlight(0, 'tsqlMergeHint',        { fg = colors.pink,       bg = 'NONE' })  -- MERGE JOIN hint


  ---------------------------------------------------------------------------
  -- Literals
  ---------------------------------------------------------------------------

  -- Strings
  highlight(0, 'tsqlString',           { fg = colors.redLight,   bg = 'NONE' })  -- 'string'
  highlight(0, 'tsqlNString',          { fg = colors.redLight,   bg = 'NONE' })  -- N'unicode string'

  -- Numbers
  highlight(0, 'tsqlNumber',           { fg = colors.greenLight, bg = 'NONE' })  -- Numeric literals
  highlight(0, 'tsqlHex',              { fg = colors.greenLight, bg = 'NONE' })  -- 0x hex literals

  -- Booleans
  highlight(0, 'tsqlBoolean',          { fg = colors.blue,       bg = 'NONE' })  -- TRUE, FALSE (in context)


  ---------------------------------------------------------------------------
  -- Comments
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlComment',          { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, 'tsqlLineComment',      { fg = colors.red,        bg = 'NONE' })  -- -- line comment
  highlight(0, 'tsqlBlockComment',     { fg = colors.red,        bg = 'NONE' })  -- /* */ block comment
  highlight(0, 'tsqlTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME


  ---------------------------------------------------------------------------
  -- Punctuation
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlPunctuation',      { fg = colors.white,      bg = 'NONE' })  -- Punctuation
  highlight(0, 'tsqlSemicolon',        { fg = colors.white,      bg = 'NONE' })  -- ;
  highlight(0, 'tsqlComma',            { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'tsqlDot',              { fg = colors.white,      bg = 'NONE' })  -- .
  highlight(0, 'tsqlParens',           { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'tsqlBrackets',         { fg = colors.white,      bg = 'NONE' })  -- [ ]


  ---------------------------------------------------------------------------
  -- Identifiers
  ---------------------------------------------------------------------------

  highlight(0, 'tsqlIdentifier',       { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, 'tsqlQuotedId',         { fg = colors.white,      bg = 'NONE' })  -- [quoted identifier]
  highlight(0, 'tsqlDoubleQuotedId',   { fg = colors.white,      bg = 'NONE' })  -- "quoted identifier"
  highlight(0, 'tsqlSchemaQualified',  { fg = colors.turquoise,  bg = 'NONE' })  -- schema.object


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.tsql / @xxx.sql)
  ---------------------------------------------------------------------------

  highlight(0, '@keyword.tsql',                  { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, '@keyword.operator.tsql',         { fg = colors.blue,       bg = 'NONE' })  -- AND, OR, NOT
  highlight(0, '@keyword.conditional.tsql',      { fg = colors.blue,       bg = 'NONE' })  -- IF, ELSE, CASE
  highlight(0, '@keyword.repeat.tsql',           { fg = colors.blue,       bg = 'NONE' })  -- WHILE
  highlight(0, '@keyword.exception.tsql',        { fg = colors.blue,       bg = 'NONE' })  -- TRY, CATCH
  highlight(0, '@keyword.return.tsql',           { fg = colors.blue,       bg = 'NONE' })  -- RETURN

  highlight(0, '@variable.tsql',                 { fg = colors.purple,     bg = 'NONE' })  -- @variables
  highlight(0, '@variable.builtin.tsql',         { fg = colors.purple,     bg = 'NONE' })  -- @@system_vars
  highlight(0, '@variable.parameter.tsql',       { fg = colors.purple,     bg = 'NONE' })  -- Parameters

  highlight(0, '@type.tsql',                     { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@type.builtin.tsql',             { fg = colors.turquoise,  bg = 'NONE' })  -- Built-in types

  highlight(0, '@function.tsql',                 { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@function.call.tsql',            { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.tsql',         { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions

  highlight(0, '@string.tsql',                   { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@number.tsql',                   { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@boolean.tsql',                  { fg = colors.blue,       bg = 'NONE' })  -- Booleans

  highlight(0, '@comment.tsql',                  { fg = colors.red,        bg = 'NONE' })  -- Comments

  highlight(0, '@operator.tsql',                 { fg = colors.white,      bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.tsql',      { fg = colors.white,      bg = 'NONE' })  -- ( ) [ ]
  highlight(0, '@punctuation.delimiter.tsql',    { fg = colors.white,      bg = 'NONE' })  -- ; , .

  highlight(0, '@constant.builtin.tsql',         { fg = colors.pink,       bg = 'NONE' })  -- NULL


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.sql)
  ---------------------------------------------------------------------------

  highlight(0, '@lsp.type.variable.sql',         { fg = colors.purple,     bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.function.sql',         { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.parameter.sql',        { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.type.sql',             { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.sql',            { fg = colors.turquoise,  bg = 'NONE' })  -- Tables
  highlight(0, '@lsp.type.property.sql',         { fg = colors.white,      bg = 'NONE' })  -- Columns
  highlight(0, '@lsp.type.namespace.sql',        { fg = colors.turquoise,  bg = 'NONE' })  -- Schemas
  highlight(0, '@lsp.type.keyword.sql',          { fg = colors.blue,       bg = 'NONE' })  -- Keywords

end

return tsql
