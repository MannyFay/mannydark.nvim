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
  highlight(0, 'cobolKeyword',          { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'cobolStatement',        { fg = colors.blue,       bg = 'NONE'            })  -- Statements
  highlight(0, 'cobolConditional',      { fg = colors.blue,       bg = 'NONE'            })  -- IF, ELSE, END-IF, EVALUATE, WHEN, END-EVALUATE
  highlight(0, 'cobolRepeat',           { fg = colors.blue,       bg = 'NONE'            })  -- PERFORM, TIMES, UNTIL, VARYING, END-PERFORM
  highlight(0, 'cobolGoto',             { fg = colors.blue,       bg = 'NONE'            })  -- GO TO
  highlight(0, 'cobolStop',             { fg = colors.blue,       bg = 'NONE'            })  -- STOP RUN, GOBACK, EXIT
  highlight(0, 'cobolCall',             { fg = colors.blue,       bg = 'NONE'            })  -- CALL, USING, BY REFERENCE, BY CONTENT, BY VALUE
  highlight(0, 'cobolReturn',           { fg = colors.blue,       bg = 'NONE'            })  -- RETURN

  -- Data Division Keywords
  highlight(0, 'cobolDataKeyword',      { fg = colors.blue,       bg = 'NONE'            })  -- Data description keywords
  highlight(0, 'cobolPicture',          { fg = colors.blue,       bg = 'NONE'            })  -- PIC, PICTURE
  highlight(0, 'cobolPictureClause',    { fg = colors.pink,       bg = 'NONE'            })  -- Picture strings (9, X, A, V, S, etc.)
  highlight(0, 'cobolUsage',            { fg = colors.blue,       bg = 'NONE'            })  -- USAGE, COMP, COMP-3, BINARY, PACKED-DECIMAL, DISPLAY
  highlight(0, 'cobolValue',            { fg = colors.blue,       bg = 'NONE'            })  -- VALUE, VALUES
  highlight(0, 'cobolRedefines',        { fg = colors.blue,       bg = 'NONE'            })  -- REDEFINES
  highlight(0, 'cobolOccurs',           { fg = colors.blue,       bg = 'NONE'            })  -- OCCURS, DEPENDING ON, INDEXED BY
  highlight(0, 'cobolFiller',           { fg = colors.gray,       bg = 'NONE'            })  -- FILLER

  -- Level Numbers
  highlight(0, 'cobolLevelNumber',      { fg = colors.greenLight, bg = 'NONE'            })  -- 01, 05, 10, 15, etc.
  highlight(0, 'cobolLevel77',          { fg = colors.greenLight, bg = 'NONE'            })  -- 77 level
  highlight(0, 'cobolLevel88',          { fg = colors.greenLight, bg = 'NONE'            })  -- 88 level (condition names)
  highlight(0, 'cobolLevel66',          { fg = colors.greenLight, bg = 'NONE'            })  -- 66 level (RENAMES)

  -- Arithmetic Verbs
  highlight(0, 'cobolArithmetic',       { fg = colors.blue,       bg = 'NONE'            })  -- ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE
  highlight(0, 'cobolMove',             { fg = colors.blue,       bg = 'NONE'            })  -- MOVE, MOVE CORRESPONDING
  highlight(0, 'cobolInitialize',       { fg = colors.blue,       bg = 'NONE'            })  -- INITIALIZE
  highlight(0, 'cobolSet',              { fg = colors.blue,       bg = 'NONE'            })  -- SET

  -- String Handling
  highlight(0, 'cobolStringVerb',       { fg = colors.blue,       bg = 'NONE'            })  -- STRING, UNSTRING, INSPECT
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
  highlight(0, 'cobolConstant',         { fg = colors.purple,     bg = 'NONE'            })  -- Constants
  highlight(0, 'cobolFigurative',       { fg = colors.blue,       bg = 'NONE'            })  -- ZERO, ZEROS, ZEROES, SPACE, SPACES, HIGH-VALUE, LOW-VALUE, QUOTE, ALL

  -- Strings
  highlight(0, 'cobolString',           { fg = colors.redLight,   bg = 'NONE'            })  -- "strings" and 'strings'
  highlight(0, 'cobolStringDelim',      { fg = colors.redLight,   bg = 'NONE'            })  -- String delimiters

  -- Numbers
  highlight(0, 'cobolNumber',           { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'cobolDecimal',          { fg = colors.greenLight, bg = 'NONE'            })  -- Decimal numbers

  -- Operators
  highlight(0, 'cobolOperator',         { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'cobolArithOp',          { fg = colors.white,      bg = 'NONE'            })  -- +, -, *, /
  highlight(0, 'cobolRelation',         { fg = colors.blue,       bg = 'NONE'            })  -- EQUAL, GREATER, LESS, NOT, AND, OR
  highlight(0, 'cobolRelationSymbol',   { fg = colors.white,      bg = 'NONE'            })  -- =, <, >, >=, <=

  -- Special Registers
  highlight(0, 'cobolSpecialRegister',  { fg = colors.blue,       bg = 'NONE'            })  -- RETURN-CODE, SORT-STATUS, etc.

  -- Comments
  highlight(0, 'cobolComment',          { fg = colors.red,        bg = 'NONE'            })  -- * in column 7 or *> inline
  highlight(0, 'cobolCommentLine',      { fg = colors.red,        bg = 'NONE'            })  -- Full line comments
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
  highlight(0, '@variable.cobol',              { fg = colors.purple,    bg = 'NONE' })  -- Data names
  highlight(0, '@variable.builtin.cobol',      { fg = colors.blue,      bg = 'NONE' })  -- Special registers
  highlight(0, '@variable.parameter.cobol',    { fg = colors.purple,    bg = 'NONE' })  -- USING parameters
  highlight(0, '@variable.member.cobol',       { fg = colors.purple,    bg = 'NONE' })  -- Group item members

  -- Constants
  highlight(0, '@constant.cobol',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.cobol',      { fg = colors.blue,      bg = 'NONE' })  -- ZERO, SPACE, HIGH-VALUE, etc.

  -- Functions (Paragraphs/Sections)
  highlight(0, '@function.cobol',              { fg = colors.orange,    bg = 'NONE' })  -- Paragraph definitions
  highlight(0, '@function.call.cobol',         { fg = colors.orange,    bg = 'NONE' })  -- PERFORM paragraph-name
  highlight(0, '@function.builtin.cobol',      { fg = colors.orange,    bg = 'NONE' })  -- Intrinsic functions
  highlight(0, '@constructor.cobol',           { fg = colors.turquoise, bg = 'NONE' })  -- Record constructors

  -- Types
  highlight(0, '@type.cobol',                  { fg = colors.turquoise, bg = 'NONE' })  -- Record/file names
  highlight(0, '@type.builtin.cobol',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.cobol',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions

  -- Keywords
  highlight(0, '@keyword.cobol',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.cobol',      { fg = colors.blue,      bg = 'NONE' })  -- PERFORM, CALL
  highlight(0, '@keyword.type.cobol',          { fg = colors.blue,      bg = 'NONE' })  -- PIC, USAGE
  highlight(0, '@keyword.modifier.cobol',      { fg = colors.blue,      bg = 'NONE' })  -- COMP, BINARY, PACKED-DECIMAL
  highlight(0, '@keyword.return.cobol',        { fg = colors.blue,      bg = 'NONE' })  -- GOBACK, STOP RUN
  highlight(0, '@keyword.import.cobol',        { fg = colors.pink,      bg = 'NONE' })  -- COPY
  highlight(0, '@keyword.repeat.cobol',        { fg = colors.blue,      bg = 'NONE' })  -- PERFORM UNTIL/TIMES/VARYING
  highlight(0, '@keyword.conditional.cobol',   { fg = colors.blue,      bg = 'NONE' })  -- IF, ELSE, EVALUATE, WHEN
  highlight(0, '@keyword.operator.cobol',      { fg = colors.blue,      bg = 'NONE' })  -- AND, OR, NOT

  -- Strings
  highlight(0, '@string.cobol',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.special.cobol',        { fg = colors.pink,      bg = 'NONE' })  -- Picture clauses

  -- Numbers
  highlight(0, '@number.cobol',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers

  -- Comments
  highlight(0, '@comment.cobol',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.cobol', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Modules
  highlight(0, '@module.cobol',                { fg = colors.turquoise, bg = 'NONE' })  -- Program names
  highlight(0, '@label.cobol',                 { fg = colors.orange,    bg = 'NONE' })  -- Paragraph/section labels
  highlight(0, '@property.cobol',              { fg = colors.purple,    bg = 'NONE' })  -- Data item properties

  -- Operators and Punctuation
  highlight(0, '@operator.cobol',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.cobol',   { fg = colors.white,     bg = 'NONE' })  -- ()
  highlight(0, '@punctuation.delimiter.cobol', { fg = colors.white,     bg = 'NONE' })  -- , .


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.cobol)

  highlight(0, '@lsp.type.variable.cobol',      { fg = colors.purple,    bg = 'NONE' })  -- Data names
  highlight(0, '@lsp.type.parameter.cobol',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.cobol',      { fg = colors.purple,    bg = 'NONE' })  -- Data item properties
  highlight(0, '@lsp.type.function.cobol',      { fg = colors.orange,    bg = 'NONE' })  -- Paragraphs
  highlight(0, '@lsp.type.method.cobol',        { fg = colors.orange,    bg = 'NONE' })  -- Sections
  highlight(0, '@lsp.type.type.cobol',          { fg = colors.turquoise, bg = 'NONE' })  -- Record types
  highlight(0, '@lsp.type.class.cobol',         { fg = colors.turquoise, bg = 'NONE' })  -- File/record definitions
  highlight(0, '@lsp.type.namespace.cobol',     { fg = colors.turquoise, bg = 'NONE' })  -- Program ID
  highlight(0, '@lsp.type.keyword.cobol',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.cobol',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.cobol',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.cobol',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.cobol',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.cobol',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.cobol',    { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@lsp.typemod.function.declaration.cobol', { fg = colors.orange,    bg = 'NONE' })  -- Paragraph definitions
  highlight(0, '@lsp.typemod.type.declaration.cobol',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
end

return cobol
