-------------------------------------------------------------------------------
-- PL/SQL (Oracle Procedural Language/SQL)
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local plsql     = {}


-------------------------------------------------------------------------------
-- Settings

plsql.setupHighlighting = function()


  ---------------------------------------------------------------------------
  -- Block Structure Keywords
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlDeclare',         { fg = colors.blue,       bg = 'NONE' })  -- DECLARE
  highlight(0, 'plsqlBEGIN',           { fg = colors.blue,       bg = 'NONE' })  -- BEGIN
  highlight(0, 'plsqlEND',             { fg = colors.blue,       bg = 'NONE' })  -- END
  highlight(0, 'plsqlException',       { fg = colors.blue,       bg = 'NONE' })  -- EXCEPTION keyword
  highlight(0, 'plsqlBlock',           { fg = colors.blue,       bg = 'NONE' })  -- Block keywords
  highlight(0, 'plsqlISAS',            { fg = colors.blue,       bg = 'NONE' })  -- IS, AS


  ---------------------------------------------------------------------------
  -- Subprogram Keywords
  ---------------------------------------------------------------------------

  -- Procedures
  highlight(0, 'plsqlProcedure',       { fg = colors.blue,       bg = 'NONE' })  -- PROCEDURE keyword
  highlight(0, 'plsqlProcedureName',   { fg = colors.orange,     bg = 'NONE' })  -- Procedure names

  -- Functions
  highlight(0, 'plsqlFunctionKw',      { link = "Function" })  -- FUNCTION keyword
  highlight(0, 'plsqlFunctionName',    { link = "Function" })  -- Function names
  highlight(0, 'plsqlReturn',          { fg = colors.blue,       bg = 'NONE' })  -- RETURN keyword

  -- Parameters
  highlight(0, 'plsqlParameter',       { fg = colors.white,      bg = 'NONE' })  -- Parameter names
  highlight(0, 'plsqlParameterMode',   { fg = colors.blue,       bg = 'NONE' })  -- IN, OUT, IN OUT
  highlight(0, 'plsqlDefault',         { fg = colors.blue,       bg = 'NONE' })  -- DEFAULT keyword
  highlight(0, 'plsqlNocopy',          { fg = colors.blue,       bg = 'NONE' })  -- NOCOPY hint


  ---------------------------------------------------------------------------
  -- Package Keywords
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlPackage',         { fg = colors.blue,       bg = 'NONE' })  -- PACKAGE keyword
  highlight(0, 'plsqlPackageName',     { fg = colors.turquoise,  bg = 'NONE' })  -- Package names
  highlight(0, 'plsqlBody',            { fg = colors.blue,       bg = 'NONE' })  -- BODY keyword
  highlight(0, 'plsqlCreate',          { fg = colors.blue,       bg = 'NONE' })  -- CREATE keyword
  highlight(0, 'plsqlReplace',         { fg = colors.blue,       bg = 'NONE' })  -- OR REPLACE
  highlight(0, 'plsqlEditionable',     { fg = colors.blue,       bg = 'NONE' })  -- EDITIONABLE, NONEDITIONABLE


  ---------------------------------------------------------------------------
  -- Trigger Keywords
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlTrigger',         { fg = colors.blue,       bg = 'NONE' })  -- TRIGGER keyword
  highlight(0, 'plsqlTriggerName',     { fg = colors.orange,     bg = 'NONE' })  -- Trigger names
  highlight(0, 'plsqlTriggerEvent',    { fg = colors.pink,       bg = 'NONE' })  -- INSERTING, UPDATING, DELETING
  highlight(0, 'plsqlBefore',          { fg = colors.blue,       bg = 'NONE' })  -- BEFORE
  highlight(0, 'plsqlAfter',           { fg = colors.blue,       bg = 'NONE' })  -- AFTER
  highlight(0, 'plsqlInsteadOf',       { fg = colors.blue,       bg = 'NONE' })  -- INSTEAD OF
  highlight(0, 'plsqlForEachRow',      { fg = colors.blue,       bg = 'NONE' })  -- FOR EACH ROW
  highlight(0, 'plsqlWhenClause',      { fg = colors.blue,       bg = 'NONE' })  -- WHEN clause
  highlight(0, 'plsqlReferencing',     { fg = colors.blue,       bg = 'NONE' })  -- REFERENCING
  highlight(0, 'plsqlOldNew',          { fg = colors.pink,       bg = 'NONE' })  -- :OLD, :NEW
  highlight(0, 'plsqlCompoundTrigger', { fg = colors.blue,       bg = 'NONE' })  -- COMPOUND TRIGGER
  highlight(0, 'plsqlFollows',         { fg = colors.blue,       bg = 'NONE' })  -- FOLLOWS, PRECEDES
  highlight(0, 'plsqlEnable',          { fg = colors.blue,       bg = 'NONE' })  -- ENABLE, DISABLE


  ---------------------------------------------------------------------------
  -- Cursor Keywords
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlCursor',          { fg = colors.blue,       bg = 'NONE' })  -- CURSOR keyword
  highlight(0, 'plsqlCursorName',      { fg = colors.turquoise,  bg = 'NONE' })  -- Cursor names
  highlight(0, 'plsqlOpen',            { fg = colors.blue,       bg = 'NONE' })  -- OPEN
  highlight(0, 'plsqlFetch',           { fg = colors.blue,       bg = 'NONE' })  -- FETCH
  highlight(0, 'plsqlClose',           { fg = colors.blue,       bg = 'NONE' })  -- CLOSE
  highlight(0, 'plsqlInto',            { fg = colors.blue,       bg = 'NONE' })  -- INTO
  highlight(0, 'plsqlRefCursor',       { fg = colors.turquoise,  bg = 'NONE' })  -- REF CURSOR
  highlight(0, 'plsqlSysCursor',       { fg = colors.turquoise,  bg = 'NONE' })  -- SYS_REFCURSOR

  -- Cursor Attributes
  highlight(0, 'plsqlCursorAttr',      { fg = colors.pink,       bg = 'NONE' })  -- Cursor attributes
  highlight(0, 'plsqlFound',           { fg = colors.pink,       bg = 'NONE' })  -- %FOUND
  highlight(0, 'plsqlNotFound',        { fg = colors.pink,       bg = 'NONE' })  -- %NOTFOUND
  highlight(0, 'plsqlIsOpen',          { fg = colors.pink,       bg = 'NONE' })  -- %ISOPEN
  highlight(0, 'plsqlRowCount',        { fg = colors.pink,       bg = 'NONE' })  -- %ROWCOUNT
  highlight(0, 'plsqlBulkRowCount',    { fg = colors.pink,       bg = 'NONE' })  -- %BULK_ROWCOUNT
  highlight(0, 'plsqlBulkExceptions',  { fg = colors.pink,       bg = 'NONE' })  -- %BULK_EXCEPTIONS


  ---------------------------------------------------------------------------
  -- Type Attributes
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlAttribute',       { fg = colors.pink,       bg = 'NONE' })  -- General attributes
  highlight(0, 'plsqlTypeAttribute',   { link = "Type" })  -- %TYPE
  highlight(0, 'plsqlRowtypeAttr',     { fg = colors.pink,       bg = 'NONE' })  -- %ROWTYPE


  ---------------------------------------------------------------------------
  -- Control Flow - Conditional
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlConditional',     { link = "Conditional" })  -- Conditional keywords
  highlight(0, 'plsqlIf',              { fg = colors.blue,       bg = 'NONE' })  -- IF
  highlight(0, 'plsqlThen',            { fg = colors.blue,       bg = 'NONE' })  -- THEN
  highlight(0, 'plsqlElse',            { fg = colors.blue,       bg = 'NONE' })  -- ELSE
  highlight(0, 'plsqlElsif',           { fg = colors.blue,       bg = 'NONE' })  -- ELSIF
  highlight(0, 'plsqlEndIf',           { fg = colors.blue,       bg = 'NONE' })  -- END IF

  -- CASE
  highlight(0, 'plsqlCase',            { fg = colors.blue,       bg = 'NONE' })  -- CASE
  highlight(0, 'plsqlWhen',            { fg = colors.blue,       bg = 'NONE' })  -- WHEN
  highlight(0, 'plsqlEndCase',         { fg = colors.blue,       bg = 'NONE' })  -- END CASE


  ---------------------------------------------------------------------------
  -- Control Flow - Loops
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlRepeat',          { fg = colors.blue,       bg = 'NONE' })  -- Loop keywords
  highlight(0, 'plsqlLoop',            { fg = colors.blue,       bg = 'NONE' })  -- LOOP
  highlight(0, 'plsqlEndLoop',         { fg = colors.blue,       bg = 'NONE' })  -- END LOOP
  highlight(0, 'plsqlWhile',           { fg = colors.blue,       bg = 'NONE' })  -- WHILE
  highlight(0, 'plsqlFor',             { fg = colors.blue,       bg = 'NONE' })  -- FOR
  highlight(0, 'plsqlIn',              { fg = colors.blue,       bg = 'NONE' })  -- IN
  highlight(0, 'plsqlReverse',         { fg = colors.blue,       bg = 'NONE' })  -- REVERSE
  highlight(0, 'plsqlExit',            { fg = colors.blue,       bg = 'NONE' })  -- EXIT
  highlight(0, 'plsqlContinue',        { fg = colors.blue,       bg = 'NONE' })  -- CONTINUE
  highlight(0, 'plsqlGoto',            { fg = colors.blue,       bg = 'NONE' })  -- GOTO
  highlight(0, 'plsqlLabel',           { fg = colors.pink,       bg = 'NONE' })  -- <<label>>


  ---------------------------------------------------------------------------
  -- Exception Handling
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlExceptionBlock',  { fg = colors.blue,       bg = 'NONE' })  -- EXCEPTION block
  highlight(0, 'plsqlRaise',           { fg = colors.blue,       bg = 'NONE' })  -- RAISE
  highlight(0, 'plsqlRaiseAppError',   { fg = colors.orange,     bg = 'NONE' })  -- RAISE_APPLICATION_ERROR
  highlight(0, 'plsqlWhenOthers',      { fg = colors.blue,       bg = 'NONE' })  -- WHEN OTHERS

  -- Predefined Exceptions
  highlight(0, 'plsqlPredefinedExc',   { fg = colors.turquoise,  bg = 'NONE' })  -- Predefined exceptions
  highlight(0, 'plsqlNoDataFound',     { fg = colors.turquoise,  bg = 'NONE' })  -- NO_DATA_FOUND
  highlight(0, 'plsqlTooManyRows',     { fg = colors.turquoise,  bg = 'NONE' })  -- TOO_MANY_ROWS
  highlight(0, 'plsqlZeroDivide',      { fg = colors.turquoise,  bg = 'NONE' })  -- ZERO_DIVIDE
  highlight(0, 'plsqlValueError',      { fg = colors.turquoise,  bg = 'NONE' })  -- VALUE_ERROR
  highlight(0, 'plsqlInvalidCursor',   { fg = colors.turquoise,  bg = 'NONE' })  -- INVALID_CURSOR
  highlight(0, 'plsqlCursorAlreadyOpen', { fg = colors.turquoise, bg = 'NONE' })  -- CURSOR_ALREADY_OPEN
  highlight(0, 'plsqlDupValOnIndex',   { fg = colors.turquoise,  bg = 'NONE' })  -- DUP_VAL_ON_INDEX
  highlight(0, 'plsqlInvalidNumber',   { link = "Number" })  -- INVALID_NUMBER
  highlight(0, 'plsqlLoginDenied',     { fg = colors.turquoise,  bg = 'NONE' })  -- LOGIN_DENIED
  highlight(0, 'plsqlNotLoggedOn',     { fg = colors.turquoise,  bg = 'NONE' })  -- NOT_LOGGED_ON
  highlight(0, 'plsqlProgramError',    { fg = colors.turquoise,  bg = 'NONE' })  -- PROGRAM_ERROR
  highlight(0, 'plsqlStorageError',    { fg = colors.turquoise,  bg = 'NONE' })  -- STORAGE_ERROR
  highlight(0, 'plsqlTimeoutOnResource', { fg = colors.turquoise, bg = 'NONE' })  -- TIMEOUT_ON_RESOURCE
  highlight(0, 'plsqlAccessIntoNull',  { fg = colors.turquoise,  bg = 'NONE' })  -- ACCESS_INTO_NULL
  highlight(0, 'plsqlCaseNotFound',    { fg = colors.turquoise,  bg = 'NONE' })  -- CASE_NOT_FOUND
  highlight(0, 'plsqlCollIsNull',      { fg = colors.turquoise,  bg = 'NONE' })  -- COLLECTION_IS_NULL
  highlight(0, 'plsqlRowtypeMismatch', { fg = colors.turquoise,  bg = 'NONE' })  -- ROWTYPE_MISMATCH
  highlight(0, 'plsqlSelfIsNull',      { fg = colors.turquoise,  bg = 'NONE' })  -- SELF_IS_NULL
  highlight(0, 'plsqlSubscriptBeyond', { fg = colors.turquoise,  bg = 'NONE' })  -- SUBSCRIPT_BEYOND_COUNT
  highlight(0, 'plsqlSubscriptOutside', { fg = colors.turquoise, bg = 'NONE' })  -- SUBSCRIPT_OUTSIDE_LIMIT
  highlight(0, 'plsqlSysInvalidRowid', { fg = colors.turquoise,  bg = 'NONE' })  -- SYS_INVALID_ROWID

  -- Exception Functions
  highlight(0, 'plsqlSqlcode',         { fg = colors.orange,     bg = 'NONE' })  -- SQLCODE
  highlight(0, 'plsqlSqlerrm',         { fg = colors.orange,     bg = 'NONE' })  -- SQLERRM

  -- User-Defined Exceptions
  highlight(0, 'plsqlExceptionPragma', { fg = colors.pink,       bg = 'NONE' })  -- EXCEPTION_INIT pragma


  ---------------------------------------------------------------------------
  -- Data Types
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlStorage',         { fg = colors.turquoise,  bg = 'NONE' })  -- General data types

  -- Scalar Types - Numeric
  highlight(0, 'plsqlNumber',          { link = "Number" })  -- NUMBER
  highlight(0, 'plsqlInteger',         { fg = colors.turquoise,  bg = 'NONE' })  -- INTEGER, INT
  highlight(0, 'plsqlSmallint',        { fg = colors.turquoise,  bg = 'NONE' })  -- SMALLINT
  highlight(0, 'plsqlPls_integer',     { fg = colors.turquoise,  bg = 'NONE' })  -- PLS_INTEGER
  highlight(0, 'plsqlBinary_integer',  { fg = colors.turquoise,  bg = 'NONE' })  -- BINARY_INTEGER
  highlight(0, 'plsqlPositive',        { fg = colors.turquoise,  bg = 'NONE' })  -- POSITIVE, POSITIVEN
  highlight(0, 'plsqlNatural',         { fg = colors.turquoise,  bg = 'NONE' })  -- NATURAL, NATURALN
  highlight(0, 'plsqlSigntype',        { fg = colors.turquoise,  bg = 'NONE' })  -- SIGNTYPE
  highlight(0, 'plsqlSimple_integer',  { fg = colors.turquoise,  bg = 'NONE' })  -- SIMPLE_INTEGER
  highlight(0, 'plsqlBinaryFloat',     { fg = colors.turquoise,  bg = 'NONE' })  -- BINARY_FLOAT
  highlight(0, 'plsqlBinaryDouble',    { fg = colors.turquoise,  bg = 'NONE' })  -- BINARY_DOUBLE
  highlight(0, 'plsqlDecimal',         { fg = colors.turquoise,  bg = 'NONE' })  -- DECIMAL, DEC
  highlight(0, 'plsqlNumeric',         { fg = colors.turquoise,  bg = 'NONE' })  -- NUMERIC
  highlight(0, 'plsqlFloat',           { fg = colors.turquoise,  bg = 'NONE' })  -- FLOAT
  highlight(0, 'plsqlReal',            { fg = colors.turquoise,  bg = 'NONE' })  -- REAL
  highlight(0, 'plsqlDoublePrecision', { fg = colors.turquoise,  bg = 'NONE' })  -- DOUBLE PRECISION

  -- Scalar Types - Character
  highlight(0, 'plsqlChar',            { fg = colors.turquoise,  bg = 'NONE' })  -- CHAR, CHARACTER
  highlight(0, 'plsqlVarchar2',        { fg = colors.turquoise,  bg = 'NONE' })  -- VARCHAR2
  highlight(0, 'plsqlVarchar',         { fg = colors.turquoise,  bg = 'NONE' })  -- VARCHAR
  highlight(0, 'plsqlNchar',           { fg = colors.turquoise,  bg = 'NONE' })  -- NCHAR
  highlight(0, 'plsqlNvarchar2',       { fg = colors.turquoise,  bg = 'NONE' })  -- NVARCHAR2
  highlight(0, 'plsqlString',          { link = "String" })  -- STRING type
  highlight(0, 'plsqlLong',            { fg = colors.turquoise,  bg = 'NONE' })  -- LONG
  highlight(0, 'plsqlRaw',             { fg = colors.turquoise,  bg = 'NONE' })  -- RAW
  highlight(0, 'plsqlLongRaw',         { fg = colors.turquoise,  bg = 'NONE' })  -- LONG RAW
  highlight(0, 'plsqlRowid',           { fg = colors.turquoise,  bg = 'NONE' })  -- ROWID
  highlight(0, 'plsqlUrowid',          { fg = colors.turquoise,  bg = 'NONE' })  -- UROWID

  -- Scalar Types - Date/Time
  highlight(0, 'plsqlDate',            { fg = colors.turquoise,  bg = 'NONE' })  -- DATE
  highlight(0, 'plsqlTimestamp',       { fg = colors.turquoise,  bg = 'NONE' })  -- TIMESTAMP
  highlight(0, 'plsqlTimestampTz',     { fg = colors.turquoise,  bg = 'NONE' })  -- TIMESTAMP WITH TIME ZONE
  highlight(0, 'plsqlTimestampLtz',    { fg = colors.turquoise,  bg = 'NONE' })  -- TIMESTAMP WITH LOCAL TIME ZONE
  highlight(0, 'plsqlInterval',        { fg = colors.turquoise,  bg = 'NONE' })  -- INTERVAL
  highlight(0, 'plsqlIntervalYM',      { fg = colors.turquoise,  bg = 'NONE' })  -- INTERVAL YEAR TO MONTH
  highlight(0, 'plsqlIntervalDS',      { fg = colors.turquoise,  bg = 'NONE' })  -- INTERVAL DAY TO SECOND

  -- Scalar Types - Boolean
  highlight(0, 'plsqlBoolean',         { link = "Boolean" })  -- BOOLEAN

  -- LOB Types
  highlight(0, 'plsqlClob',            { fg = colors.turquoise,  bg = 'NONE' })  -- CLOB
  highlight(0, 'plsqlNclob',           { fg = colors.turquoise,  bg = 'NONE' })  -- NCLOB
  highlight(0, 'plsqlBlob',            { fg = colors.turquoise,  bg = 'NONE' })  -- BLOB
  highlight(0, 'plsqlBfile',           { fg = colors.turquoise,  bg = 'NONE' })  -- BFILE

  -- XML Types
  highlight(0, 'plsqlXmltype',         { fg = colors.turquoise,  bg = 'NONE' })  -- XMLTYPE

  -- JSON Types (12c+)
  highlight(0, 'plsqlJson',            { fg = colors.turquoise,  bg = 'NONE' })  -- JSON


  ---------------------------------------------------------------------------
  -- Collection Types
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlType',            { link = "Type" })  -- TYPE keyword
  highlight(0, 'plsqlTypeName',        { link = "Type" })  -- Type names

  -- Associative Arrays (Index-By Tables)
  highlight(0, 'plsqlTableOf',         { fg = colors.blue,       bg = 'NONE' })  -- TABLE OF
  highlight(0, 'plsqlIndexBy',         { fg = colors.blue,       bg = 'NONE' })  -- INDEX BY

  -- Nested Tables
  highlight(0, 'plsqlNestedTable',     { fg = colors.turquoise,  bg = 'NONE' })  -- Nested table types

  -- Varrays
  highlight(0, 'plsqlVarray',          { fg = colors.blue,       bg = 'NONE' })  -- VARRAY keyword
  highlight(0, 'plsqlVarrayOf',        { fg = colors.blue,       bg = 'NONE' })  -- VARRAY(...) OF

  -- Collection Methods
  highlight(0, 'plsqlCollMethod',      { link = "Function" })  -- Collection methods
  highlight(0, 'plsqlCount',           { fg = colors.orange,     bg = 'NONE' })  -- COUNT
  highlight(0, 'plsqlFirst',           { fg = colors.orange,     bg = 'NONE' })  -- FIRST
  highlight(0, 'plsqlLast',            { fg = colors.orange,     bg = 'NONE' })  -- LAST
  highlight(0, 'plsqlNext',            { fg = colors.orange,     bg = 'NONE' })  -- NEXT
  highlight(0, 'plsqlPrior',           { fg = colors.orange,     bg = 'NONE' })  -- PRIOR
  highlight(0, 'plsqlExists',          { fg = colors.orange,     bg = 'NONE' })  -- EXISTS
  highlight(0, 'plsqlExtend',          { fg = colors.orange,     bg = 'NONE' })  -- EXTEND
  highlight(0, 'plsqlTrim',            { fg = colors.orange,     bg = 'NONE' })  -- TRIM
  highlight(0, 'plsqlDelete',          { fg = colors.orange,     bg = 'NONE' })  -- DELETE
  highlight(0, 'plsqlLimit',           { fg = colors.orange,     bg = 'NONE' })  -- LIMIT


  ---------------------------------------------------------------------------
  -- Record Types
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlRecord',          { fg = colors.blue,       bg = 'NONE' })  -- RECORD keyword
  highlight(0, 'plsqlRecordName',      { fg = colors.turquoise,  bg = 'NONE' })  -- Record type names
  highlight(0, 'plsqlRecordField',     { fg = colors.orange,     bg = 'NONE' })  -- Record fields


  ---------------------------------------------------------------------------
  -- Object Types
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlObject',          { fg = colors.blue,       bg = 'NONE' })  -- OBJECT keyword
  highlight(0, 'plsqlObjectName',      { fg = colors.turquoise,  bg = 'NONE' })  -- Object type names
  highlight(0, 'plsqlUnder',           { fg = colors.blue,       bg = 'NONE' })  -- UNDER (inheritance)
  highlight(0, 'plsqlMember',          { fg = colors.blue,       bg = 'NONE' })  -- MEMBER keyword
  highlight(0, 'plsqlStatic',          { fg = colors.blue,       bg = 'NONE' })  -- STATIC keyword
  highlight(0, 'plsqlConstructor',     { fg = colors.blue,       bg = 'NONE' })  -- CONSTRUCTOR keyword
  highlight(0, 'plsqlSelf',            { fg = colors.pink,       bg = 'NONE' })  -- SELF
  highlight(0, 'plsqlFinal',           { fg = colors.blue,       bg = 'NONE' })  -- FINAL
  highlight(0, 'plsqlInstantiable',    { fg = colors.blue,       bg = 'NONE' })  -- INSTANTIABLE, NOT INSTANTIABLE
  highlight(0, 'plsqlOverriding',      { fg = colors.blue,       bg = 'NONE' })  -- OVERRIDING
  highlight(0, 'plsqlMap',             { fg = colors.blue,       bg = 'NONE' })  -- MAP
  highlight(0, 'plsqlOrder',           { fg = colors.blue,       bg = 'NONE' })  -- ORDER


  ---------------------------------------------------------------------------
  -- Bulk Operations
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlBulkCollect',     { fg = colors.blue,       bg = 'NONE' })  -- BULK COLLECT
  highlight(0, 'plsqlForall',          { fg = colors.blue,       bg = 'NONE' })  -- FORALL
  highlight(0, 'plsqlIndices',         { fg = colors.blue,       bg = 'NONE' })  -- INDICES OF
  highlight(0, 'plsqlValues',          { fg = colors.blue,       bg = 'NONE' })  -- VALUES OF
  highlight(0, 'plsqlSaveExceptions',  { fg = colors.blue,       bg = 'NONE' })  -- SAVE EXCEPTIONS


  ---------------------------------------------------------------------------
  -- Dynamic SQL
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlExecute',         { fg = colors.blue,       bg = 'NONE' })  -- EXECUTE
  highlight(0, 'plsqlImmediate',       { fg = colors.blue,       bg = 'NONE' })  -- IMMEDIATE
  highlight(0, 'plsqlUsing',           { fg = colors.blue,       bg = 'NONE' })  -- USING
  highlight(0, 'plsqlReturning',       { fg = colors.blue,       bg = 'NONE' })  -- RETURNING


  ---------------------------------------------------------------------------
  -- Transaction Control
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlCommit',          { fg = colors.blue,       bg = 'NONE' })  -- COMMIT
  highlight(0, 'plsqlRollback',        { fg = colors.blue,       bg = 'NONE' })  -- ROLLBACK
  highlight(0, 'plsqlSavepoint',       { fg = colors.blue,       bg = 'NONE' })  -- SAVEPOINT
  highlight(0, 'plsqlSetTransaction',  { fg = colors.blue,       bg = 'NONE' })  -- SET TRANSACTION
  highlight(0, 'plsqlAutonomous',      { fg = colors.pink,       bg = 'NONE' })  -- AUTONOMOUS_TRANSACTION pragma


  ---------------------------------------------------------------------------
  -- SQL Statements (Embedded)
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlSelect',          { fg = colors.blue,       bg = 'NONE' })  -- SELECT
  highlight(0, 'plsqlInsert',          { fg = colors.blue,       bg = 'NONE' })  -- INSERT
  highlight(0, 'plsqlUpdate',          { fg = colors.blue,       bg = 'NONE' })  -- UPDATE
  highlight(0, 'plsqlDeleteSql',       { fg = colors.blue,       bg = 'NONE' })  -- DELETE (SQL)
  highlight(0, 'plsqlMerge',           { fg = colors.blue,       bg = 'NONE' })  -- MERGE

  -- SQL Clauses
  highlight(0, 'plsqlFrom',            { fg = colors.blue,       bg = 'NONE' })  -- FROM
  highlight(0, 'plsqlWhere',           { fg = colors.blue,       bg = 'NONE' })  -- WHERE
  highlight(0, 'plsqlGroupBy',         { fg = colors.blue,       bg = 'NONE' })  -- GROUP BY
  highlight(0, 'plsqlHaving',          { fg = colors.blue,       bg = 'NONE' })  -- HAVING
  highlight(0, 'plsqlOrderBy',         { fg = colors.blue,       bg = 'NONE' })  -- ORDER BY
  highlight(0, 'plsqlConnect',         { fg = colors.blue,       bg = 'NONE' })  -- CONNECT BY
  highlight(0, 'plsqlStartWith',       { fg = colors.blue,       bg = 'NONE' })  -- START WITH

  -- Reserved SQL Keywords
  highlight(0, 'plsqlReserved',        { fg = colors.blue,       bg = 'NONE' })  -- Reserved keywords


  ---------------------------------------------------------------------------
  -- Pragmas
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlPragma',          { fg = colors.pink,       bg = 'NONE' })  -- PRAGMA keyword
  highlight(0, 'plsqlPragmaName',      { fg = colors.pink,       bg = 'NONE' })  -- Pragma names
  highlight(0, 'plsqlRestrictRef',     { fg = colors.pink,       bg = 'NONE' })  -- RESTRICT_REFERENCES
  highlight(0, 'plsqlSeriallyReusable', { fg = colors.pink,      bg = 'NONE' })  -- SERIALLY_REUSABLE
  highlight(0, 'plsqlInline',          { fg = colors.pink,       bg = 'NONE' })  -- INLINE
  highlight(0, 'plsqlUdf',             { fg = colors.pink,       bg = 'NONE' })  -- UDF pragma
  highlight(0, 'plsqlDeprecate',       { fg = colors.pink,       bg = 'NONE' })  -- DEPRECATE


  ---------------------------------------------------------------------------
  -- Compiler Directives (Conditional Compilation)
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlPseudo',          { fg = colors.pink,       bg = 'NONE' })  -- Preprocessor directives
  highlight(0, 'plsqlCondComp',        { fg = colors.pink,       bg = 'NONE' })  -- $IF, $THEN, $ELSE, $END, $ERROR
  highlight(0, 'plsqlPlsqlUnit',       { fg = colors.pink,       bg = 'NONE' })  -- $$PLSQL_UNIT, etc.


  ---------------------------------------------------------------------------
  -- Built-in Functions
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlFunction',        { link = "Function" })  -- Built-in functions

  -- Numeric Functions
  highlight(0, 'plsqlFuncAbs',         { link = "Function" })  -- ABS
  highlight(0, 'plsqlFuncCeil',        { link = "Function" })  -- CEIL
  highlight(0, 'plsqlFuncFloor',       { link = "Function" })  -- FLOOR
  highlight(0, 'plsqlFuncRound',       { link = "Function" })  -- ROUND
  highlight(0, 'plsqlFuncTrunc',       { link = "Function" })  -- TRUNC
  highlight(0, 'plsqlFuncMod',         { link = "Function" })  -- MOD
  highlight(0, 'plsqlFuncPower',       { link = "Function" })  -- POWER
  highlight(0, 'plsqlFuncSqrt',        { link = "Function" })  -- SQRT
  highlight(0, 'plsqlFuncSign',        { link = "Function" })  -- SIGN

  -- String Functions
  highlight(0, 'plsqlFuncConcat',      { link = "Function" })  -- CONCAT
  highlight(0, 'plsqlFuncSubstr',      { link = "Function" })  -- SUBSTR
  highlight(0, 'plsqlFuncLength',      { link = "Function" })  -- LENGTH
  highlight(0, 'plsqlFuncInstr',       { link = "Function" })  -- INSTR
  highlight(0, 'plsqlFuncReplace',     { link = "Function" })  -- REPLACE
  highlight(0, 'plsqlFuncTranslate',   { link = "Function" })  -- TRANSLATE
  highlight(0, 'plsqlFuncUpper',       { link = "Function" })  -- UPPER
  highlight(0, 'plsqlFuncLower',       { link = "Function" })  -- LOWER
  highlight(0, 'plsqlFuncInitcap',     { link = "Function" })  -- INITCAP
  highlight(0, 'plsqlFuncLtrim',       { link = "Function" })  -- LTRIM
  highlight(0, 'plsqlFuncRtrim',       { link = "Function" })  -- RTRIM
  highlight(0, 'plsqlFuncLpad',        { link = "Function" })  -- LPAD
  highlight(0, 'plsqlFuncRpad',        { link = "Function" })  -- RPAD

  -- Date Functions
  highlight(0, 'plsqlFuncSysdate',     { link = "Function" })  -- SYSDATE
  highlight(0, 'plsqlFuncSystimestamp', { link = "Function" })  -- SYSTIMESTAMP
  highlight(0, 'plsqlFuncCurrentDate', { link = "Function" })  -- CURRENT_DATE
  highlight(0, 'plsqlFuncAddMonths',   { link = "Function" })  -- ADD_MONTHS
  highlight(0, 'plsqlFuncMonthsBetween', { link = "Function" })  -- MONTHS_BETWEEN
  highlight(0, 'plsqlFuncLastDay',     { link = "Function" })  -- LAST_DAY
  highlight(0, 'plsqlFuncNextDay',     { link = "Function" })  -- NEXT_DAY
  highlight(0, 'plsqlFuncExtract',     { link = "Function" })  -- EXTRACT
  highlight(0, 'plsqlFuncToDate',      { link = "Function" })  -- TO_DATE
  highlight(0, 'plsqlFuncToTimestamp', { link = "Function" })  -- TO_TIMESTAMP

  -- Conversion Functions
  highlight(0, 'plsqlFuncToChar',      { link = "Function" })  -- TO_CHAR
  highlight(0, 'plsqlFuncToNumber',    { link = "Number" })  -- TO_NUMBER
  highlight(0, 'plsqlFuncCast',        { link = "Function" })  -- CAST
  highlight(0, 'plsqlFuncConvert',     { link = "Function" })  -- CONVERT

  -- Null Functions
  highlight(0, 'plsqlFuncNvl',         { link = "Function" })  -- NVL
  highlight(0, 'plsqlFuncNvl2',        { link = "Function" })  -- NVL2
  highlight(0, 'plsqlFuncCoalesce',    { link = "Function" })  -- COALESCE
  highlight(0, 'plsqlFuncNullif',      { link = "Function" })  -- NULLIF
  highlight(0, 'plsqlFuncDecode',      { link = "Function" })  -- DECODE

  -- Aggregate Functions
  highlight(0, 'plsqlFuncCount',       { link = "Function" })  -- COUNT
  highlight(0, 'plsqlFuncSum',         { link = "Function" })  -- SUM
  highlight(0, 'plsqlFuncAvg',         { link = "Function" })  -- AVG
  highlight(0, 'plsqlFuncMin',         { link = "Function" })  -- MIN
  highlight(0, 'plsqlFuncMax',         { link = "Function" })  -- MAX


  ---------------------------------------------------------------------------
  -- Built-in Packages
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlBuiltinPkg',      { fg = colors.turquoise,  bg = 'NONE' })  -- Built-in package names

  -- DBMS_OUTPUT
  highlight(0, 'plsqlDbmsOutput',      { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_OUTPUT
  highlight(0, 'plsqlPutLine',         { fg = colors.orange,     bg = 'NONE' })  -- PUT_LINE
  highlight(0, 'plsqlPut',             { fg = colors.orange,     bg = 'NONE' })  -- PUT
  highlight(0, 'plsqlNewLine',         { fg = colors.orange,     bg = 'NONE' })  -- NEW_LINE
  highlight(0, 'plsqlGetLine',         { fg = colors.orange,     bg = 'NONE' })  -- GET_LINE
  highlight(0, 'plsqlEnableOutput',    { fg = colors.orange,     bg = 'NONE' })  -- ENABLE

  -- UTL_FILE
  highlight(0, 'plsqlUtlFile',         { fg = colors.turquoise,  bg = 'NONE' })  -- UTL_FILE
  highlight(0, 'plsqlFopen',           { fg = colors.orange,     bg = 'NONE' })  -- FOPEN
  highlight(0, 'plsqlFclose',          { fg = colors.orange,     bg = 'NONE' })  -- FCLOSE
  highlight(0, 'plsqlGetLineUtl',      { fg = colors.orange,     bg = 'NONE' })  -- GET_LINE
  highlight(0, 'plsqlPutLineUtl',      { fg = colors.orange,     bg = 'NONE' })  -- PUT_LINE

  -- DBMS_SQL
  highlight(0, 'plsqlDbmsSql',         { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_SQL
  highlight(0, 'plsqlOpenCursor',      { fg = colors.orange,     bg = 'NONE' })  -- OPEN_CURSOR
  highlight(0, 'plsqlParse',           { fg = colors.orange,     bg = 'NONE' })  -- PARSE
  highlight(0, 'plsqlBindVariable',    { link = "Variable" })  -- BIND_VARIABLE
  highlight(0, 'plsqlExecuteDbms',     { fg = colors.orange,     bg = 'NONE' })  -- EXECUTE

  -- DBMS_LOB
  highlight(0, 'plsqlDbmsLob',         { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_LOB
  highlight(0, 'plsqlGetLength',       { fg = colors.orange,     bg = 'NONE' })  -- GETLENGTH
  highlight(0, 'plsqlReadLob',         { fg = colors.orange,     bg = 'NONE' })  -- READ
  highlight(0, 'plsqlWriteLob',        { fg = colors.orange,     bg = 'NONE' })  -- WRITE
  highlight(0, 'plsqlAppend',          { fg = colors.orange,     bg = 'NONE' })  -- APPEND

  -- DBMS_SCHEDULER
  highlight(0, 'plsqlDbmsScheduler',   { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_SCHEDULER
  highlight(0, 'plsqlCreateJob',       { fg = colors.orange,     bg = 'NONE' })  -- CREATE_JOB
  highlight(0, 'plsqlRunJob',          { fg = colors.orange,     bg = 'NONE' })  -- RUN_JOB
  highlight(0, 'plsqlDropJob',         { fg = colors.orange,     bg = 'NONE' })  -- DROP_JOB

  -- Other Common Packages
  highlight(0, 'plsqlDbmsRandom',      { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_RANDOM
  highlight(0, 'plsqlDbmsUtility',     { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_UTILITY
  highlight(0, 'plsqlDbmsLock',        { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_LOCK
  highlight(0, 'plsqlDbmsMetadata',    { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_METADATA
  highlight(0, 'plsqlDbmsXplan',       { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_XPLAN
  highlight(0, 'plsqlDbmsCrypto',      { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_CRYPTO
  highlight(0, 'plsqlDbmsAq',          { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_AQ
  highlight(0, 'plsqlDbmsAqadm',       { fg = colors.turquoise,  bg = 'NONE' })  -- DBMS_AQADM
  highlight(0, 'plsqlUtlHttp',         { fg = colors.turquoise,  bg = 'NONE' })  -- UTL_HTTP
  highlight(0, 'plsqlUtlMail',         { fg = colors.turquoise,  bg = 'NONE' })  -- UTL_MAIL
  highlight(0, 'plsqlUtlRaw',          { fg = colors.turquoise,  bg = 'NONE' })  -- UTL_RAW
  highlight(0, 'plsqlUtlCompress',     { fg = colors.turquoise,  bg = 'NONE' })  -- UTL_COMPRESS


  ---------------------------------------------------------------------------
  -- Literals
  ---------------------------------------------------------------------------

  -- String Literals
  highlight(0, 'plsqlStringLiteral',   { link = "String" })  -- 'strings'
  highlight(0, 'plsqlQStringLiteral',  { link = "String" })  -- q'[strings]'

  -- Numeric Literals
  highlight(0, 'plsqlIntLiteral',      { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, 'plsqlFloatLiteral',    { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'plsqlNumbers',         { link = "Number" })  -- Numbers

  -- Boolean Literals
  highlight(0, 'plsqlBooleanLiteral',  { link = "Boolean" })  -- TRUE, FALSE

  -- NULL
  highlight(0, 'plsqlNull',            { fg = colors.pink,       bg = 'NONE' })  -- NULL


  ---------------------------------------------------------------------------
  -- Operators
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlOperator',        { link = "Operator" })  -- General operators

  -- Assignment
  highlight(0, 'plsqlAssignment',      { fg = colors.white,      bg = 'NONE' })  -- :=

  -- Comparison
  highlight(0, 'plsqlComparison',      { fg = colors.white,      bg = 'NONE' })  -- =, <>, <, >, <=, >=

  -- Logical
  highlight(0, 'plsqlLogical',         { fg = colors.blue,       bg = 'NONE' })  -- AND, OR, NOT

  -- Arithmetic
  highlight(0, 'plsqlArithmetic',      { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, **

  -- String Concatenation
  highlight(0, 'plsqlConcat',          { fg = colors.white,      bg = 'NONE' })  -- ||

  -- Range
  highlight(0, 'plsqlRange',           { fg = colors.white,      bg = 'NONE' })  -- ..


  ---------------------------------------------------------------------------
  -- Identifiers & Variables
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlIdentifier',      { fg = colors.white,      bg = 'NONE' })  -- General identifiers
  highlight(0, 'plsqlQuotedIdentifier', { fg = colors.white,     bg = 'NONE' })  -- "quoted_id"
  highlight(0, 'plsqlHostIdentifier',  { fg = colors.purple,     bg = 'NONE' })  -- :host_variable
  highlight(0, 'plsqlBindVariable',    { link = "Variable" })  -- :bind


  ---------------------------------------------------------------------------
  -- Comments
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlComment',         { link = "Comment" })  -- /* */ comments
  highlight(0, 'plsqlCommentL',        { link = "Comment" })  -- -- line comments
  highlight(0, 'plsqlCommentString',   { link = "Comment" })  -- Strings in comments
  highlight(0, 'plsqlComment2String',  { link = "Comment" })  -- Strings in comments
  highlight(0, 'plsqlTodo',            { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME


  ---------------------------------------------------------------------------
  -- Punctuation & Symbols
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlSymbol',          { fg = colors.white,      bg = 'NONE' })  -- Symbols
  highlight(0, 'plsqlParens',          { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'plsqlSemicolon',       { fg = colors.white,      bg = 'NONE' })  -- ;
  highlight(0, 'plsqlComma',           { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'plsqlDot',             { fg = colors.white,      bg = 'NONE' })  -- .


  ---------------------------------------------------------------------------
  -- SQL*Plus
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlSqlPlusCommand',  { fg = colors.pink,       bg = 'NONE' })  -- SQL*Plus commands
  highlight(0, 'plsqlSqlPlusSet',      { fg = colors.pink,       bg = 'NONE' })  -- SET commands
  highlight(0, 'plsqlSqlPlusShow',     { fg = colors.pink,       bg = 'NONE' })  -- SHOW commands
  highlight(0, 'plsqlSqlPlusDefine',   { fg = colors.purple,     bg = 'NONE' })  -- &variable, &&variable
  highlight(0, 'plsqlSqlPlusCommentL', { link = "Comment" })  -- REM comments
  highlight(0, 'plsqlSqlPlusRunFile',  { fg = colors.pink,       bg = 'NONE' })  -- @script, @@script
  highlight(0, 'plsqlSqlPlusPrompt',   { fg = colors.pink,       bg = 'NONE' })  -- PROMPT
  highlight(0, 'plsqlSqlPlusSpool',    { fg = colors.pink,       bg = 'NONE' })  -- SPOOL
  highlight(0, 'plsqlSqlPlusAccept',   { fg = colors.pink,       bg = 'NONE' })  -- ACCEPT
  highlight(0, 'plsqlSqlPlusColumn',   { fg = colors.pink,       bg = 'NONE' })  -- COLUMN
  highlight(0, 'plsqlSqlPlusExit',     { fg = colors.pink,       bg = 'NONE' })  -- EXIT, QUIT


  ---------------------------------------------------------------------------
  -- Errors
  ---------------------------------------------------------------------------

  highlight(0, 'plsqlError',           { fg = colors.red,        bg = 'NONE' })  -- Errors
  highlight(0, 'plsqlGarbage',         { fg = colors.red,        bg = 'NONE' })  -- Invalid syntax
  highlight(0, 'plsqlParenError',      { fg = colors.red,        bg = 'NONE' })  -- Paren errors
  highlight(0, 'plsqlSpaceError',      { fg = colors.red,        bg = 'NONE' })  -- Space errors
  highlight(0, 'plsqlIllegalSpace',    { fg = colors.red,        bg = 'NONE' })  -- Illegal spaces
  highlight(0, 'plsqlErrInParen',      { fg = colors.red,        bg = 'NONE' })  -- Error in parens
  highlight(0, 'plsqlErrInBracket',    { fg = colors.red,        bg = 'NONE' })  -- Error in brackets
  highlight(0, 'plsqlStringError',     { link = "String" })  -- String errors


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.plsql)
  ---------------------------------------------------------------------------

  -- Keywords
  highlight(0, '@keyword.plsql',                 { link = "Keyword" })  -- Keywords
  highlight(0, '@keyword.function.plsql',        { link = "Keyword" })  -- FUNCTION, PROCEDURE
  highlight(0, '@keyword.operator.plsql',        { link = "Operator" })  -- AND, OR, NOT
  highlight(0, '@keyword.conditional.plsql',     { link = "Conditional" })  -- IF, ELSIF, ELSE
  highlight(0, '@keyword.repeat.plsql',          { link = "Keyword" })  -- LOOP, WHILE, FOR
  highlight(0, '@keyword.exception.plsql',       { link = "Keyword" })  -- EXCEPTION, RAISE
  highlight(0, '@keyword.return.plsql',          { link = "Keyword" })  -- RETURN

  -- Variables
  highlight(0, '@variable.plsql',                { link = "Variable" })  -- Variables
  highlight(0, '@variable.parameter.plsql',      { link = "Variable" })  -- Parameters
  highlight(0, '@variable.member.plsql',         { link = "Variable" })  -- Record fields

  -- Types
  highlight(0, '@type.plsql',                    { link = "Type" })  -- Types
  highlight(0, '@type.builtin.plsql',            { link = "Type" })  -- Built-in types
  highlight(0, '@type.definition.plsql',         { link = "Type" })  -- Type definitions

  -- Functions
  highlight(0, '@function.plsql',                { link = "Function" })  -- Functions
  highlight(0, '@function.call.plsql',           { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.plsql',        { link = "Function" })  -- Built-in functions

  -- Modules/Packages
  highlight(0, '@module.plsql',                  { fg = colors.turquoise,  bg = 'NONE' })  -- Packages

  -- Literals
  highlight(0, '@string.plsql',                  { link = "String" })  -- Strings
  highlight(0, '@number.plsql',                  { link = "Number" })  -- Numbers
  highlight(0, '@number.float.plsql',            { link = "Number" })  -- Floats
  highlight(0, '@boolean.plsql',                 { link = "Boolean" })  -- TRUE, FALSE
  highlight(0, '@constant.builtin.plsql',        { link = "Constant" })  -- NULL

  -- Comments
  highlight(0, '@comment.plsql',                 { link = "Comment" })  -- Comments

  -- Operators & Punctuation
  highlight(0, '@operator.plsql',                { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.plsql',     { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, '@punctuation.delimiter.plsql',   { link = "Delimiter" })  -- ; , .

  -- Attributes
  highlight(0, '@attribute.plsql',               { fg = colors.pink,       bg = 'NONE' })  -- %TYPE, %ROWTYPE


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.plsql)
  ---------------------------------------------------------------------------

  highlight(0, '@lsp.type.variable.plsql',       { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.function.plsql',       { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.parameter.plsql',      { fg = colors.white,      bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.type.plsql',           { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.plsql',          { fg = colors.turquoise,  bg = 'NONE' })  -- Packages
  highlight(0, '@lsp.type.property.plsql',       { fg = colors.orange,     bg = 'NONE' })  -- Record fields
  highlight(0, '@lsp.type.namespace.plsql',      { fg = colors.turquoise,  bg = 'NONE' })  -- Schemas
  highlight(0, '@lsp.type.keyword.plsql',        { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.string.plsql',         { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.plsql',         { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.comment.plsql',        { link = "Comment" })  -- Comments

end

return plsql
