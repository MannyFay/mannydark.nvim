-------------------------------------------------------------------------------
-- R Language Files
-- Highlighting for .r and .R files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local r         = {}


-------------------------------------------------------------------------------
-- Settings

r.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords / Control Flow
  highlight(0, 'rStatement',        { fg = colors.blue,       bg = 'NONE' })  -- break, next, return
  highlight(0, 'rConditional',      { link = "Conditional" })  -- if, else
  highlight(0, 'rRepeat',           { fg = colors.blue,       bg = 'NONE' })  -- for, in, repeat, while
  highlight(0, 'rKeyword',          { link = "Keyword" })  -- function, etc.
  highlight(0, 'rFunction',         { link = "Function" })  -- function keyword

  -- Constants
  highlight(0, 'rConstant',         { link = "Constant" })  -- NULL, NA, Inf, NaN, TRUE, FALSE
  highlight(0, 'rBoolean',          { link = "Boolean" })  -- TRUE, FALSE, T, F
  highlight(0, 'rPreProc',          { fg = colors.blue,       bg = 'NONE' })  -- library, require, attach, detach, source

  -- Types
  highlight(0, 'rType',             { link = "Type" })  -- array, character, complex, double, function, etc.

  -- Functions
  highlight(0, 'rFunctionName',     { link = "Function" })  -- Function name definitions
  highlight(0, 'rMethodCall',       { link = "Function" })  -- Method calls

  -- Operators
  highlight(0, 'rOperator',         { link = "Operator" })  -- +, -, *, /, etc.
  highlight(0, 'rAssign',           { fg = colors.white,      bg = 'NONE' })  -- <-, ->, =, <<-, ->>
  highlight(0, 'rPipeOperator',     { link = "Operator" })  -- |>, %>%
  highlight(0, 'rTilde',            { fg = colors.blue,       bg = 'NONE' })  -- ~ formula operator
  highlight(0, 'rInfixOperator',    { link = "Operator" })  -- %...% operators
  highlight(0, 'rDollar',           { fg = colors.white,      bg = 'NONE' })  -- $ for list/data frame access
  highlight(0, 'rAt',               { fg = colors.white,      bg = 'NONE' })  -- @ for S4 slot access
  highlight(0, 'rColon',            { fg = colors.white,      bg = 'NONE' })  -- : :: ::: operators

  -- Strings
  highlight(0, 'rString',           { link = "String" })  -- "string" or 'string'
  highlight(0, 'rRawString',        { link = "String" })  -- r"(...)" raw strings
  highlight(0, 'rRawStrDelim',      { link = "Delimiter" })  -- Raw string delimiters
  highlight(0, 'rSpecial',          { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences \n, \t, etc.
  highlight(0, 'rStrError',         { fg = colors.red,        bg = 'NONE' })  -- String errors

  -- Numbers
  highlight(0, 'rNumber',           { link = "Number" })  -- Numbers
  highlight(0, 'rInteger',          { fg = colors.greenLight, bg = 'NONE' })  -- Integer literals (1L)
  highlight(0, 'rFloat',            { fg = colors.greenLight, bg = 'NONE' })  -- Float literals
  highlight(0, 'rComplex',          { fg = colors.greenLight, bg = 'NONE' })  -- Complex numbers (1+2i)

  -- Comments
  highlight(0, 'rComment',          { link = "Comment" })  -- # comments
  highlight(0, 'rCommentTodo',      { link = "Comment" })  -- TODO, FIXME, etc.
  highlight(0, 'rTodoParen',        { fg = colors.red,        bg = 'NONE' })  -- Parentheses in TODO
  highlight(0, 'rTodoKeyw',         { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO keyword
  highlight(0, 'rTodoInfo',         { fg = colors.red,        bg = 'NONE' })  -- TODO info

  -- Delimiters / Punctuation
  highlight(0, 'rDelimiter',        { link = "Delimiter" })  -- (), {}, []
  highlight(0, 'rLstElmt',          { fg = colors.white,      bg = 'NONE' })  -- List elements
  highlight(0, 'rSeparator',        { fg = colors.white,      bg = 'NONE' })  -- Comma, semicolon

  -- Error
  highlight(0, 'rError',            { fg = colors.red,        bg = 'NONE' })  -- Syntax errors

  -- Roxygen Documentation
  highlight(0, 'rOComment',         { link = "Comment" })  -- #' roxygen comments
  highlight(0, 'rOCommentKey',      { link = "Comment" })  -- @param, @return, etc.
  highlight(0, 'rOExamples',        { fg = colors.redLight,   bg = 'NONE' })  -- @examples code
  highlight(0, 'rOBlock',           { fg = colors.red,        bg = 'NONE' })  -- Roxygen block
  highlight(0, 'rOBlockNoTitle',    { fg = colors.red,        bg = 'NONE' })  -- Block without title
  highlight(0, 'rOTag',             { fg = colors.pink,       bg = 'NONE' })  -- Roxygen tags
  highlight(0, 'rOTitle',           { fg = colors.white,      bg = 'NONE' })  -- Roxygen title
  highlight(0, 'rOTitleBlock',      { fg = colors.white,      bg = 'NONE' })  -- Title block
  highlight(0, 'rOR6Block',         { fg = colors.red,        bg = 'NONE' })  -- R6 block


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.r)

  -- Variables
  highlight(0, '@variable.r',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.r',      { link = "Variable" })  -- Built-in variables
  highlight(0, '@variable.parameter.r',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.r',       { link = "Variable" })  -- List/data frame members

  -- Constants
  highlight(0, '@constant.r',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.r',      { link = "Constant" })  -- NULL, NA, TRUE, FALSE, Inf, NaN

  -- Functions
  highlight(0, '@function.r',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.r',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.r',      { link = "Function" })  -- Built-in functions
  highlight(0, '@function.method.r',       { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.r',  { link = "Function" })  -- Method calls

  -- Types
  highlight(0, '@type.r',                  { link = "Type" })  -- Types
  highlight(0, '@type.builtin.r',          { link = "Type" })  -- Built-in types

  -- Keywords
  highlight(0, '@keyword.r',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.r',      { link = "Keyword" })  -- function
  highlight(0, '@keyword.return.r',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.conditional.r',   { link = "Conditional" })  -- if, else
  highlight(0, '@keyword.repeat.r',        { link = "Keyword" })  -- for, while, repeat
  highlight(0, '@keyword.operator.r',      { link = "Operator" })  -- in
  highlight(0, '@keyword.directive.r',     { link = "Keyword" })  -- library, require

  -- Strings
  highlight(0, '@string.r',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.r',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.special.r',        { link = "String" })  -- Special strings

  -- Numbers
  highlight(0, '@number.r',                { link = "Number" })  -- Numbers
  highlight(0, '@number.float.r',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.r',               { link = "Boolean" })  -- TRUE, FALSE

  -- Comments
  highlight(0, '@comment.r',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.r', { link = "Comment" })  -- Roxygen documentation

  -- Operators
  highlight(0, '@operator.r',              { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.r',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.r', { link = "Delimiter" })  -- , ;
  highlight(0, '@punctuation.special.r',   { fg = colors.blue,      bg = 'NONE' })  -- ~ formula, pipe

  -- Module
  highlight(0, '@module.r',                { fg = colors.turquoise, bg = 'NONE' })  -- Package names

  -- Spell
  highlight(0, '@spell.r',                 { fg = colors.red,       bg = 'NONE' })  -- Spell-checked text
  highlight(0, '@nospell.r',               { fg = colors.white,     bg = 'NONE' })  -- Non-spell-checked


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.r)

  highlight(0, '@lsp.type.variable.r',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.r',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.r',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.r',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.r',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.class.r',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.type.r',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.namespace.r',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces/Packages
  highlight(0, '@lsp.type.keyword.r',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.r',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.comment.r',       { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.string.r',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.r',        { link = "Number" })  -- Numbers

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.r',  { link = "Variable" })  -- Constants
  highlight(0, '@lsp.typemod.function.builtin.r',   { fg = colors.orange,   bg = 'NONE' })  -- Built-in functions


  -----------------------------------------------------------------------------
  -- Base R Functions (Custom highlight groups)

  -- Math Functions
  highlight(0, 'rFuncMath',         { link = "Function" })  -- abs, sqrt, exp, log, sin, cos, tan, etc.

  -- String Functions
  highlight(0, 'rFuncString',       { link = "String" })  -- paste, sprintf, nchar, substr, gsub, etc.

  -- Data Manipulation
  highlight(0, 'rFuncData',         { link = "Function" })  -- c, rep, seq, length, dim, names, etc.

  -- I/O Functions
  highlight(0, 'rFuncIO',           { link = "Function" })  -- read.csv, write.csv, readLines, etc.

  -- Apply Family
  highlight(0, 'rFuncApply',        { link = "Function" })  -- apply, lapply, sapply, mapply, tapply, etc.

  -- Type Checking
  highlight(0, 'rFuncIs',           { link = "Function" })  -- is.null, is.na, is.numeric, etc.

  -- Type Conversion
  highlight(0, 'rFuncAs',           { link = "Function" })  -- as.numeric, as.character, as.data.frame, etc.

  -- Environment Functions
  highlight(0, 'rFuncEnv',          { link = "Function" })  -- environment, assign, get, exists, rm, ls, etc.

  -- OOP Functions
  highlight(0, 'rFuncOOP',          { link = "Function" })  -- class, typeof, mode, setClass, setMethod, etc.


  -----------------------------------------------------------------------------
  -- Tidyverse Functions

  -- dplyr
  highlight(0, 'rDplyrVerb',        { fg = colors.orange,     bg = 'NONE' })  -- select, filter, mutate, arrange, etc.
  highlight(0, 'rDplyrJoin',        { fg = colors.orange,     bg = 'NONE' })  -- left_join, right_join, inner_join, etc.
  highlight(0, 'rDplyrHelper',      { fg = colors.orange,     bg = 'NONE' })  -- everything, starts_with, ends_with, etc.
  highlight(0, 'rDplyrWindow',      { fg = colors.orange,     bg = 'NONE' })  -- lead, lag, row_number, dense_rank, etc.

  -- tidyr
  highlight(0, 'rTidyrFunc',        { link = "Function" })  -- pivot_longer, pivot_wider, separate, etc.

  -- purrr
  highlight(0, 'rPurrrFunc',        { link = "Function" })  -- map, map2, pmap, reduce, etc.

  -- stringr
  highlight(0, 'rStringrFunc',      { link = "String" })  -- str_c, str_detect, str_replace, etc.

  -- readr
  highlight(0, 'rReadrFunc',        { link = "Function" })  -- read_csv, read_tsv, write_csv, etc.


  -----------------------------------------------------------------------------
  -- ggplot2 Functions

  highlight(0, 'rGgplotMain',       { fg = colors.orange,     bg = 'NONE' })  -- ggplot, aes, ggsave
  highlight(0, 'rGgplotGeom',       { fg = colors.orange,     bg = 'NONE' })  -- geom_point, geom_line, geom_bar, etc.
  highlight(0, 'rGgplotScale',      { fg = colors.orange,     bg = 'NONE' })  -- scale_x_continuous, scale_fill_manual, etc.
  highlight(0, 'rGgplotFacet',      { fg = colors.orange,     bg = 'NONE' })  -- facet_wrap, facet_grid
  highlight(0, 'rGgplotTheme',      { fg = colors.orange,     bg = 'NONE' })  -- theme, theme_bw, theme_minimal, etc.
  highlight(0, 'rGgplotCoord',      { fg = colors.orange,     bg = 'NONE' })  -- coord_flip, coord_polar, etc.
  highlight(0, 'rGgplotElement',    { fg = colors.orange,     bg = 'NONE' })  -- element_text, element_line, element_blank, etc.
  highlight(0, 'rGgplotLabs',       { fg = colors.orange,     bg = 'NONE' })  -- labs, xlab, ylab, ggtitle


  -----------------------------------------------------------------------------
  -- Roxygen Tags (in comments)

  highlight(0, 'rRoxygenTag',       { fg = colors.pink,       bg = 'NONE' })  -- @param, @return, @export, etc.
  highlight(0, 'rRoxygenParam',     { fg = colors.purple,     bg = 'NONE' })  -- Parameter names after @param
  highlight(0, 'rRoxygenTitle',     { fg = colors.white,      bg = 'NONE' })  -- Title text
  highlight(0, 'rRoxygenDesc',      { fg = colors.red,        bg = 'NONE' })  -- Description text


  -----------------------------------------------------------------------------
  -- Special R Constructs

  -- Formula
  highlight(0, 'rFormula',          { fg = colors.blue,       bg = 'NONE' })  -- y ~ x formula notation

  -- Pipe Operators
  highlight(0, 'rPipeNative',       { fg = colors.blue,       bg = 'NONE' })  -- |> native pipe
  highlight(0, 'rPipeMagrittr',     { fg = colors.blue,       bg = 'NONE' })  -- %>% magrittr pipe
  highlight(0, 'rPipeAssign',       { fg = colors.blue,       bg = 'NONE' })  -- %<>% assignment pipe
  highlight(0, 'rPipeTee',          { fg = colors.blue,       bg = 'NONE' })  -- %T>% tee pipe
  highlight(0, 'rPipeExpose',       { fg = colors.blue,       bg = 'NONE' })  -- %$% exposition pipe

  -- Assignment Operators
  highlight(0, 'rAssignLeft',       { fg = colors.white,      bg = 'NONE' })  -- <-
  highlight(0, 'rAssignRight',      { fg = colors.white,      bg = 'NONE' })  -- ->
  highlight(0, 'rAssignGlobalLeft', { fg = colors.white,      bg = 'NONE' })  -- <<-
  highlight(0, 'rAssignGlobalRight',{ fg = colors.white,      bg = 'NONE' })  -- ->>
  highlight(0, 'rAssignEqual',      { fg = colors.white,      bg = 'NONE' })  -- = (assignment)

  -- Special Operators
  highlight(0, 'rOpIn',             { fg = colors.blue,       bg = 'NONE' })  -- %in%
  highlight(0, 'rOpOr',             { fg = colors.white,      bg = 'NONE' })  -- %or%
  highlight(0, 'rOpNullCoalesce',   { fg = colors.white,      bg = 'NONE' })  -- %||%
  highlight(0, 'rOpMatMult',        { fg = colors.white,      bg = 'NONE' })  -- %*% matrix multiplication
  highlight(0, 'rOpKronecker',      { fg = colors.white,      bg = 'NONE' })  -- %x% Kronecker product
  highlight(0, 'rOpOuter',          { fg = colors.white,      bg = 'NONE' })  -- %o% outer product
  highlight(0, 'rOpIntDiv',         { fg = colors.white,      bg = 'NONE' })  -- %/% integer division
  highlight(0, 'rOpMod',            { fg = colors.white,      bg = 'NONE' })  -- %% modulo

  -- Namespace Operators
  highlight(0, 'rNamespace',        { fg = colors.turquoise,  bg = 'NONE' })  -- Package name before ::
  highlight(0, 'rNamespaceSep',     { fg = colors.white,      bg = 'NONE' })  -- :: and ::: separators

  -- S4 Classes
  highlight(0, 'rS4Class',          { fg = colors.turquoise,  bg = 'NONE' })  -- setClass
  highlight(0, 'rS4Method',         { link = "Function" })  -- setMethod
  highlight(0, 'rS4Generic',        { fg = colors.orange,     bg = 'NONE' })  -- setGeneric
  highlight(0, 'rS4Slot',           { fg = colors.purple,     bg = 'NONE' })  -- @ slot access

  -- R6 Classes
  highlight(0, 'rR6Class',          { fg = colors.turquoise,  bg = 'NONE' })  -- R6Class
  highlight(0, 'rR6Public',         { fg = colors.blue,       bg = 'NONE' })  -- public
  highlight(0, 'rR6Private',        { fg = colors.blue,       bg = 'NONE' })  -- private
  highlight(0, 'rR6Active',         { fg = colors.blue,       bg = 'NONE' })  -- active
  highlight(0, 'rR6Self',           { fg = colors.blue,       bg = 'NONE' })  -- self
  highlight(0, 'rR6Super',          { fg = colors.blue,       bg = 'NONE' })  -- super

  -- Special NA Values
  highlight(0, 'rNAInteger',        { fg = colors.blue,       bg = 'NONE' })  -- NA_integer_
  highlight(0, 'rNAReal',           { fg = colors.blue,       bg = 'NONE' })  -- NA_real_
  highlight(0, 'rNAComplex',        { fg = colors.blue,       bg = 'NONE' })  -- NA_complex_
  highlight(0, 'rNACharacter',      { fg = colors.blue,       bg = 'NONE' })  -- NA_character_

  -- Built-in Constants
  highlight(0, 'rPi',               { fg = colors.blue,       bg = 'NONE' })  -- pi
  highlight(0, 'rLetters',          { fg = colors.blue,       bg = 'NONE' })  -- LETTERS, letters
  highlight(0, 'rMonths',           { fg = colors.blue,       bg = 'NONE' })  -- month.abb, month.name


  -----------------------------------------------------------------------------
  -- Statistical Functions

  highlight(0, 'rFuncStat',         { link = "Function" })  -- mean, median, sd, var, cor, cov, etc.
  highlight(0, 'rFuncDist',         { link = "Function" })  -- dnorm, pnorm, qnorm, rnorm, dbinom, etc.
  highlight(0, 'rFuncTest',         { link = "Function" })  -- t.test, chisq.test, wilcox.test, etc.
  highlight(0, 'rFuncModel',        { link = "Function" })  -- lm, glm, aov, anova, summary, predict, etc.


  -----------------------------------------------------------------------------
  -- Shiny Functions

  highlight(0, 'rShinyUI',          { fg = colors.orange,     bg = 'NONE' })  -- fluidPage, sidebarLayout, mainPanel, etc.
  highlight(0, 'rShinyInput',       { fg = colors.orange,     bg = 'NONE' })  -- textInput, numericInput, selectInput, etc.
  highlight(0, 'rShinyOutput',      { fg = colors.orange,     bg = 'NONE' })  -- plotOutput, tableOutput, textOutput, etc.
  highlight(0, 'rShinyRender',      { fg = colors.orange,     bg = 'NONE' })  -- renderPlot, renderTable, renderText, etc.
  highlight(0, 'rShinyReactive',    { fg = colors.orange,     bg = 'NONE' })  -- reactive, reactiveVal, observe, etc.
  highlight(0, 'rShinyServer',      { fg = colors.orange,     bg = 'NONE' })  -- shinyServer, shinyApp, runApp


  -----------------------------------------------------------------------------
  -- data.table Functions

  highlight(0, 'rDataTableFunc',    { link = "Function" })  -- data.table, setDT, setkey, setorder, etc.
  highlight(0, 'rDataTableSpecial', { fg = colors.blue,       bg = 'NONE' })  -- .N, .SD, .BY, .I, .GRP, .NGRP, .EACHI


  -----------------------------------------------------------------------------
  -- RMarkdown / Quarto

  highlight(0, 'rmdChunkDelim',     { link = "Delimiter" })  -- ``` delimiters
  highlight(0, 'rmdChunkLabel',     { fg = colors.turquoise,  bg = 'NONE' })  -- Chunk labels
  highlight(0, 'rmdChunkOptions',   { fg = colors.purple,     bg = 'NONE' })  -- Chunk options
  highlight(0, 'rmdInlineCode',     { fg = colors.orange,     bg = 'NONE' })  -- `r code`
  highlight(0, 'rmdYamlKey',        { fg = colors.turquoise,  bg = 'NONE' })  -- YAML front matter keys
  highlight(0, 'rmdYamlValue',      { fg = colors.redLight,   bg = 'NONE' })  -- YAML front matter values
end

return r
