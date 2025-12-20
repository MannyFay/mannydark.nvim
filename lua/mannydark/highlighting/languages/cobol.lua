-------------------------------------------------------------------------------
-- COBOL Files
-- Highlighting for .cob, .cbl, .cobol files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local cobol   = {}


-------------------------------------------------------------------------------
-- Settings

cobol.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Divisions
  highlight(0, 'cobolDivision',         { fg = colors.blue,       bg = 'NONE', bold = true })  -- IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE DIVISION
  highlight(0, 'cobolDivisionName',     { fg = colors.blue,       bg = 'NONE', bold = true })  -- Division names

  -- Sections
  highlight(0, 'cobolSection',          { fg = colors.blue,       bg = 'NONE'            })  -- SECTION keyword
  highlight(0, 'cobolSectionName',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Section names
  highlight(0, 'cobolFileSection',      { fg = colors.blue,       bg = 'NONE'            })  -- FILE SECTION
  highlight(0, 'cobolWorkingStorage',   { fg = colors.blue,       bg = 'NONE'            })  -- WORKING-STORAGE SECTION
  highlight(0, 'cobolLocalStorage',     { fg = colors.blue,       bg = 'NONE'            })  -- LOCAL-STORAGE SECTION
  highlight(0, 'cobolLinkage',          { fg = colors.blue,       bg = 'NONE'            })  -- LINKAGE SECTION
  highlight(0, 'cobolScreenSection',    { fg = colors.blue,       bg = 'NONE'            })  -- SCREEN SECTION
  highlight(0, 'cobolReportSection',    { fg = colors.blue,       bg = 'NONE'            })  -- REPORT SECTION

  -- Paragraphs
  highlight(0, 'cobolParagraph',        { fg = colors.orange,     bg = 'NONE'            })  -- Paragraph names
  highlight(0, 'cobolParagraphName',    { fg = colors.orange,     bg = 'NONE'            })  -- User-defined paragraph names

  -- Keywords
  highlight(0, 'cobolKeyword',          { link = "Keyword" })  -- General keywords
  highlight(0, 'cobolStatement',        { fg = colors.blue,       bg = 'NONE'            })  -- Statements
  highlight(0, 'cobolConditional',      { link = "Conditional" })  -- IF, ELSE, END-IF, EVALUATE, WHEN, END-EVALUATE
  highlight(0, 'cobolRepeat',           { fg = colors.blue,       bg = 'NONE'            })  -- PERFORM, TIMES, UNTIL, VARYING, END-PERFORM
  highlight(0, 'cobolGoto',             { fg = colors.blue,       bg = 'NONE'            })  -- GO TO
  highlight(0, 'cobolStop',             { fg = colors.blue,       bg = 'NONE'            })  -- STOP RUN, GOBACK, EXIT
  highlight(0, 'cobolCall',             { fg = colors.blue,       bg = 'NONE'            })  -- CALL, USING, BY REFERENCE, BY CONTENT, BY VALUE
  highlight(0, 'cobolReturn',           { fg = colors.blue,       bg = 'NONE'            })  -- RETURN

  -- Data Division Keywords
  highlight(0, 'cobolDataKeyword',      { link = "Keyword" })  -- Data description keywords
  highlight(0, 'cobolPicture',          { fg = colors.blue,       bg = 'NONE'            })  -- PIC, PICTURE
  highlight(0, 'cobolPictureClause',    { fg = colors.pink,       bg = 'NONE'            })  -- Picture strings (9, X, A, V, S, etc.)
  highlight(0, 'cobolUsage',            { fg = colors.blue,       bg = 'NONE'            })  -- USAGE, COMP, COMP-3, BINARY, PACKED-DECIMAL, DISPLAY
  highlight(0, 'cobolValue',            { fg = colors.blue,       bg = 'NONE'            })  -- VALUE, VALUES
  highlight(0, 'cobolRedefines',        { fg = colors.blue,       bg = 'NONE'            })  -- REDEFINES
  highlight(0, 'cobolOccurs',           { fg = colors.blue,       bg = 'NONE'            })  -- OCCURS, DEPENDING ON, INDEXED BY
  highlight(0, 'cobolFiller',           { fg = colors.gray,       bg = 'NONE'            })  -- FILLER

  -- Level Numbers
  highlight(0, 'cobolLevelNumber',      { link = "Number" })  -- 01, 05, 10, 15, etc.
  highlight(0, 'cobolLevel77',          { fg = colors.greenLight, bg = 'NONE'            })  -- 77 level
  highlight(0, 'cobolLevel88',          { fg = colors.greenLight, bg = 'NONE'            })  -- 88 level (condition names)
  highlight(0, 'cobolLevel66',          { fg = colors.greenLight, bg = 'NONE'            })  -- 66 level (RENAMES)

  -- Arithmetic Verbs
  highlight(0, 'cobolArithmetic',       { fg = colors.blue,       bg = 'NONE'            })  -- ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE
  highlight(0, 'cobolMove',             { fg = colors.blue,       bg = 'NONE'            })  -- MOVE, MOVE CORRESPONDING
  highlight(0, 'cobolInitialize',       { fg = colors.blue,       bg = 'NONE'            })  -- INITIALIZE
  highlight(0, 'cobolSet',              { fg = colors.blue,       bg = 'NONE'            })  -- SET

  -- String Handling
  highlight(0, 'cobolStringVerb',       { link = "String" })  -- STRING, UNSTRING, INSPECT
  highlight(0, 'cobolInspect',          { fg = colors.blue,       bg = 'NONE'            })  -- TALLYING, REPLACING, CONVERTING

  -- File I/O
  highlight(0, 'cobolFileIO',           { fg = colors.blue,       bg = 'NONE'            })  -- OPEN, CLOSE, READ, WRITE, REWRITE, DELETE, START
  highlight(0, 'cobolFileControl',      { fg = colors.blue,       bg = 'NONE'            })  -- SELECT, ASSIGN, ORGANIZATION, ACCESS, FILE STATUS
  highlight(0, 'cobolFileOrg',          { fg = colors.blue,       bg = 'NONE'            })  -- SEQUENTIAL, INDEXED, RELATIVE, LINE SEQUENTIAL
  highlight(0, 'cobolAccess',           { fg = colors.blue,       bg = 'NONE'            })  -- SEQUENTIAL, RANDOM, DYNAMIC

  -- Exception Handling
  highlight(0, 'cobolException',        { fg = colors.blue,       bg = 'NONE'            })  -- ON SIZE ERROR, NOT ON SIZE ERROR, AT END, NOT AT END
  highlight(0, 'cobolInvalidKey',       { fg = colors.blue,       bg = 'NONE'            })  -- INVALID KEY, NOT INVALID KEY

  -- Screen/Report
  highlight(0, 'cobolDisplay',          { fg = colors.blue,       bg = 'NONE'            })  -- DISPLAY, ACCEPT
  highlight(0, 'cobolScreen',           { fg = colors.blue,       bg = 'NONE'            })  -- Screen keywords

  -- Compiler Directives
  highlight(0, 'cobolCopy',             { fg = colors.pink,       bg = 'NONE'            })  -- COPY, REPLACING
  highlight(0, 'cobolReplace',          { fg = colors.pink,       bg = 'NONE'            })  -- REPLACE
  highlight(0, 'cobolCompilerDir',      { fg = colors.pink,       bg = 'NONE'            })  -- Compiler directives

  -- Types/Data Names
  highlight(0, 'cobolDataName',         { fg = colors.purple,     bg = 'NONE'            })  -- Data names/identifiers
  highlight(0, 'cobolIdentifier',       { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'cobolFileName',         { fg = colors.turquoise,  bg = 'NONE'            })  -- File names
  highlight(0, 'cobolRecordName',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Record names
  highlight(0, 'cobolConditionName',    { fg = colors.purple,     bg = 'NONE'            })  -- 88-level condition names

  -- Constants
  highlight(0, 'cobolConstant',         { link = "Constant" })  -- Constants
  highlight(0, 'cobolFigurative',       { fg = colors.blue,       bg = 'NONE'            })  -- ZERO, ZEROS, ZEROES, SPACE, SPACES, HIGH-VALUE, LOW-VALUE, QUOTE, ALL

  -- Strings
  highlight(0, 'cobolString',           { link = "String" })  -- "strings" and 'strings'
  highlight(0, 'cobolStringDelim',      { link = "Delimiter" })  -- String delimiters

  -- Numbers
  highlight(0, 'cobolNumber',           { link = "Number" })  -- Numbers
  highlight(0, 'cobolDecimal',          { fg = colors.greenLight, bg = 'NONE'            })  -- Decimal numbers

  -- Operators
  highlight(0, 'cobolOperator',         { link = "Operator" })  -- Operators
  highlight(0, 'cobolArithOp',          { fg = colors.white,      bg = 'NONE'            })  -- +, -, *, /
  highlight(0, 'cobolRelation',         { fg = colors.blue,       bg = 'NONE'            })  -- EQUAL, GREATER, LESS, NOT, AND, OR
  highlight(0, 'cobolRelationSymbol',   { fg = colors.white,      bg = 'NONE'            })  -- =, <, >, >=, <=

  -- Special Registers
  highlight(0, 'cobolSpecialRegister',  { fg = colors.blue,       bg = 'NONE'            })  -- RETURN-CODE, SORT-STATUS, etc.

  -- Comments
  highlight(0, 'cobolComment',          { link = "Comment" })  -- * in column 7 or *> inline
  highlight(0, 'cobolCommentLine',      { link = "Comment" })  -- Full line comments
  highlight(0, 'cobolTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Area Indicators
  highlight(0, 'cobolIndicator',        { fg = colors.gray,       bg = 'NONE'            })  -- Column 7 indicators
  highlight(0, 'cobolSequence',         { fg = colors.gray,       bg = 'NONE'            })  -- Sequence numbers (columns 1-6)
  highlight(0, 'cobolDebug',            { fg = colors.gray,       bg = 'NONE'            })  -- D in column 7 (debug lines)

  -- Periods
  highlight(0, 'cobolPeriod',           { fg = colors.white,      bg = 'NONE'            })  -- Statement-ending periods


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.cobol)

  -- Variables
  highlight(0, '@variable.cobol',              { link = "Variable" })  -- Data names
  highlight(0, '@variable.builtin.cobol',      { link = "Variable" })  -- Special registers
  highlight(0, '@variable.parameter.cobol',    { link = "Variable" })  -- USING parameters
  highlight(0, '@variable.member.cobol',       { link = "Variable" })  -- Group item members

  -- Constants
  highlight(0, '@constant.cobol',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.cobol',      { link = "Constant" })  -- ZERO, SPACE, HIGH-VALUE, etc.

  -- Functions (Paragraphs/Sections)
  highlight(0, '@function.cobol',              { link = "Function" })  -- Paragraph definitions
  highlight(0, '@function.call.cobol',         { link = "Function" })  -- PERFORM paragraph-name
  highlight(0, '@function.builtin.cobol',      { link = "Function" })  -- Intrinsic functions
  highlight(0, '@constructor.cobol',           { fg = colors.turquoise, bg = 'NONE' })  -- Record constructors

  -- Types
  highlight(0, '@type.cobol',                  { link = "Type" })  -- Record/file names
  highlight(0, '@type.builtin.cobol',          { link = "Type" })  -- Built-in types
  highlight(0, '@type.definition.cobol',       { link = "Type" })  -- Type definitions

  -- Keywords
  highlight(0, '@keyword.cobol',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.cobol',      { link = "Keyword" })  -- PERFORM, CALL
  highlight(0, '@keyword.type.cobol',          { link = "Keyword" })  -- PIC, USAGE
  highlight(0, '@keyword.modifier.cobol',      { link = "Keyword" })  -- COMP, BINARY, PACKED-DECIMAL
  highlight(0, '@keyword.return.cobol',        { link = "Keyword" })  -- GOBACK, STOP RUN
  highlight(0, '@keyword.import.cobol',        { link = "Keyword" })  -- COPY
  highlight(0, '@keyword.repeat.cobol',        { link = "Keyword" })  -- PERFORM UNTIL/TIMES/VARYING
  highlight(0, '@keyword.conditional.cobol',   { link = "Conditional" })  -- IF, ELSE, EVALUATE, WHEN
  highlight(0, '@keyword.operator.cobol',      { link = "Operator" })  -- AND, OR, NOT

  -- Strings
  highlight(0, '@string.cobol',                { link = "String" })  -- Strings
  highlight(0, '@string.special.cobol',        { link = "String" })  -- Picture clauses

  -- Numbers
  highlight(0, '@number.cobol',                { link = "Number" })  -- Numbers

  -- Comments
  highlight(0, '@comment.cobol',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.cobol', { link = "Comment" })  -- Doc comments

  -- Modules
  highlight(0, '@module.cobol',                { fg = colors.turquoise, bg = 'NONE' })  -- Program names
  highlight(0, '@label.cobol',                 { fg = colors.orange,    bg = 'NONE' })  -- Paragraph/section labels
  highlight(0, '@property.cobol',              { fg = colors.purple,    bg = 'NONE' })  -- Data item properties

  -- Operators and Punctuation
  highlight(0, '@operator.cobol',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.cobol',   { fg = colors.white,     bg = 'NONE' })  -- ()
  highlight(0, '@punctuation.delimiter.cobol', { link = "Delimiter" })  -- , .


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.cobol)

  highlight(0, '@lsp.type.variable.cobol',      { link = "Variable" })  -- Data names
  highlight(0, '@lsp.type.parameter.cobol',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.cobol',      { fg = colors.purple,    bg = 'NONE' })  -- Data item properties
  highlight(0, '@lsp.type.function.cobol',      { fg = colors.orange,    bg = 'NONE' })  -- Paragraphs
  highlight(0, '@lsp.type.method.cobol',        { fg = colors.orange,    bg = 'NONE' })  -- Sections
  highlight(0, '@lsp.type.type.cobol',          { fg = colors.turquoise, bg = 'NONE' })  -- Record types
  highlight(0, '@lsp.type.class.cobol',         { fg = colors.turquoise, bg = 'NONE' })  -- File/record definitions
  highlight(0, '@lsp.type.namespace.cobol',     { fg = colors.turquoise, bg = 'NONE' })  -- Program ID
  highlight(0, '@lsp.type.keyword.cobol',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.cobol',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.cobol',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.cobol',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.cobol',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.cobol',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.cobol',    { link = "Variable" })  -- Constants
  highlight(0, '@lsp.typemod.function.declaration.cobol', { fg = colors.orange,    bg = 'NONE' })  -- Paragraph definitions
  highlight(0, '@lsp.typemod.type.declaration.cobol',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
end

return cobol
