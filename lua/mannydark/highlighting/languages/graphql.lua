-------------------------------------------------------------------------------
-- GraphQL
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local graphql   = {}


-------------------------------------------------------------------------------
-- Settings

graphql.setupHighlighting = function()


  ---------------------------------------------------------------------------
  -- Operations
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlKeyword',           { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, 'graphqlOperationKeyword',  { fg = colors.blue,       bg = 'NONE' })  -- query, mutation, subscription

  -- Query
  highlight(0, 'graphqlQuery',             { fg = colors.blue,       bg = 'NONE' })  -- query keyword
  highlight(0, 'graphqlQueryName',         { fg = colors.orange,     bg = 'NONE' })  -- Query operation name

  -- Mutation
  highlight(0, 'graphqlMutation',          { fg = colors.blue,       bg = 'NONE' })  -- mutation keyword
  highlight(0, 'graphqlMutationName',      { fg = colors.orange,     bg = 'NONE' })  -- Mutation operation name

  -- Subscription
  highlight(0, 'graphqlSubscription',      { fg = colors.blue,       bg = 'NONE' })  -- subscription keyword
  highlight(0, 'graphqlSubscriptionName',  { fg = colors.orange,     bg = 'NONE' })  -- Subscription operation name


  ---------------------------------------------------------------------------
  -- Type System Definitions
  ---------------------------------------------------------------------------

  -- Schema
  highlight(0, 'graphqlSchema',            { fg = colors.blue,       bg = 'NONE' })  -- schema keyword
  highlight(0, 'graphqlSchemaKeyword',     { fg = colors.blue,       bg = 'NONE' })  -- schema keyword

  -- Type Definition
  highlight(0, 'graphqlStructure',         { fg = colors.blue,       bg = 'NONE' })  -- type, input, interface, etc.
  highlight(0, 'graphqlTypeKeyword',       { fg = colors.blue,       bg = 'NONE' })  -- type keyword
  highlight(0, 'graphqlTypeName',          { fg = colors.turquoise,  bg = 'NONE' })  -- Type names

  -- Input Type
  highlight(0, 'graphqlInput',             { fg = colors.blue,       bg = 'NONE' })  -- input keyword
  highlight(0, 'graphqlInputName',         { fg = colors.turquoise,  bg = 'NONE' })  -- Input type names

  -- Interface
  highlight(0, 'graphqlInterface',         { fg = colors.blue,       bg = 'NONE' })  -- interface keyword
  highlight(0, 'graphqlInterfaceName',     { fg = colors.turquoise,  bg = 'NONE' })  -- Interface names
  highlight(0, 'graphqlImplements',        { fg = colors.blue,       bg = 'NONE' })  -- implements keyword

  -- Union
  highlight(0, 'graphqlUnion',             { fg = colors.blue,       bg = 'NONE' })  -- union keyword
  highlight(0, 'graphqlUnionName',         { fg = colors.turquoise,  bg = 'NONE' })  -- Union names
  highlight(0, 'graphqlUnionMember',       { fg = colors.turquoise,  bg = 'NONE' })  -- Union member types

  -- Enum
  highlight(0, 'graphqlEnum',              { fg = colors.blue,       bg = 'NONE' })  -- enum keyword
  highlight(0, 'graphqlEnumName',          { fg = colors.turquoise,  bg = 'NONE' })  -- Enum type names
  highlight(0, 'graphqlEnumValue',         { fg = colors.pink,       bg = 'NONE' })  -- Enum values (CONSTANT_CASE)

  -- Scalar
  highlight(0, 'graphqlScalar',            { fg = colors.blue,       bg = 'NONE' })  -- scalar keyword
  highlight(0, 'graphqlScalarName',        { fg = colors.turquoise,  bg = 'NONE' })  -- Custom scalar names

  -- Extend
  highlight(0, 'graphqlExtend',            { fg = colors.blue,       bg = 'NONE' })  -- extend keyword


  ---------------------------------------------------------------------------
  -- Types
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlType',              { fg = colors.turquoise,  bg = 'NONE' })  -- Type references

  -- Built-in Scalar Types
  highlight(0, 'graphqlBuiltinType',       { fg = colors.turquoise,  bg = 'NONE' })  -- Built-in types
  highlight(0, 'graphqlTypeInt',           { fg = colors.turquoise,  bg = 'NONE' })  -- Int
  highlight(0, 'graphqlTypeFloat',         { fg = colors.turquoise,  bg = 'NONE' })  -- Float
  highlight(0, 'graphqlTypeString',        { fg = colors.turquoise,  bg = 'NONE' })  -- String
  highlight(0, 'graphqlTypeBoolean',       { fg = colors.turquoise,  bg = 'NONE' })  -- Boolean
  highlight(0, 'graphqlTypeID',            { fg = colors.turquoise,  bg = 'NONE' })  -- ID

  -- Type Modifiers
  highlight(0, 'graphqlNonNull',           { fg = colors.white,      bg = 'NONE' })  -- ! (non-null)
  highlight(0, 'graphqlList',              { fg = colors.white,      bg = 'NONE' })  -- [] (list)
  highlight(0, 'graphqlListType',          { fg = colors.turquoise,  bg = 'NONE' })  -- List type


  ---------------------------------------------------------------------------
  -- Fields
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlField',             { fg = colors.white,      bg = 'NONE' })  -- Field names
  highlight(0, 'graphqlFieldName',         { fg = colors.white,      bg = 'NONE' })  -- Field names in queries
  highlight(0, 'graphqlFieldDefinition',   { fg = colors.orange,     bg = 'NONE' })  -- Field definitions in types
  highlight(0, 'graphqlAlias',             { fg = colors.orange,     bg = 'NONE' })  -- Field aliases


  ---------------------------------------------------------------------------
  -- Arguments
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlArgument',          { fg = colors.orange,     bg = 'NONE' })  -- Argument names
  highlight(0, 'graphqlArgumentName',      { fg = colors.orange,     bg = 'NONE' })  -- Argument names
  highlight(0, 'graphqlArgumentValue',     { fg = colors.white,      bg = 'NONE' })  -- Argument values
  highlight(0, 'graphqlInputFieldName',    { fg = colors.orange,     bg = 'NONE' })  -- Input field names


  ---------------------------------------------------------------------------
  -- Variables
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlVariable',          { fg = colors.purple,     bg = 'NONE' })  -- $variable
  highlight(0, 'graphqlVariableName',      { fg = colors.purple,     bg = 'NONE' })  -- Variable names
  highlight(0, 'graphqlVariableDefinition', { fg = colors.purple,    bg = 'NONE' })  -- Variable definitions
  highlight(0, 'graphqlVariableDefault',   { fg = colors.white,      bg = 'NONE' })  -- Default values


  ---------------------------------------------------------------------------
  -- Fragments
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlFragment',          { fg = colors.blue,       bg = 'NONE' })  -- fragment keyword
  highlight(0, 'graphqlFragmentName',      { fg = colors.turquoise,  bg = 'NONE' })  -- Fragment names
  highlight(0, 'graphqlOn',                { fg = colors.blue,       bg = 'NONE' })  -- on keyword
  highlight(0, 'graphqlFragmentSpread',    { fg = colors.turquoise,  bg = 'NONE' })  -- ...FragmentName
  highlight(0, 'graphqlInlineFragment',    { fg = colors.blue,       bg = 'NONE' })  -- ... on Type


  ---------------------------------------------------------------------------
  -- Directives
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlDirective',         { fg = colors.pink,       bg = 'NONE' })  -- @directive
  highlight(0, 'graphqlDirectiveName',     { fg = colors.pink,       bg = 'NONE' })  -- Directive names
  highlight(0, 'graphqlDirectiveLocation', { fg = colors.pink,       bg = 'NONE' })  -- Directive locations

  -- Built-in Directives
  highlight(0, 'graphqlBuiltinDirective',  { fg = colors.pink,       bg = 'NONE' })  -- Built-in directives
  highlight(0, 'graphqlDeprecated',        { fg = colors.pink,       bg = 'NONE' })  -- @deprecated
  highlight(0, 'graphqlSkip',              { fg = colors.pink,       bg = 'NONE' })  -- @skip
  highlight(0, 'graphqlInclude',           { fg = colors.pink,       bg = 'NONE' })  -- @include
  highlight(0, 'graphqlSpecifiedBy',       { fg = colors.pink,       bg = 'NONE' })  -- @specifiedBy

  -- Directive Locations (for directive definitions)
  highlight(0, 'graphqlExecutableLocation', { fg = colors.pink,      bg = 'NONE' })  -- QUERY, MUTATION, etc.
  highlight(0, 'graphqlTypeSystemLocation', { fg = colors.pink,      bg = 'NONE' })  -- SCHEMA, SCALAR, etc.


  ---------------------------------------------------------------------------
  -- Literals
  ---------------------------------------------------------------------------

  -- Strings
  highlight(0, 'graphqlString',            { fg = colors.redLight,   bg = 'NONE' })  -- "string"
  highlight(0, 'graphqlStringContent',     { fg = colors.redLight,   bg = 'NONE' })  -- String content
  highlight(0, 'graphqlBlockString',       { fg = colors.redLight,   bg = 'NONE' })  -- """block string"""
  highlight(0, 'graphqlEscape',            { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences

  -- Numbers
  highlight(0, 'graphqlNumber',            { fg = colors.greenLight, bg = 'NONE' })  -- Numeric literals
  highlight(0, 'graphqlIntValue',          { fg = colors.greenLight, bg = 'NONE' })  -- Integer values
  highlight(0, 'graphqlFloatValue',        { fg = colors.greenLight, bg = 'NONE' })  -- Float values

  -- Booleans
  highlight(0, 'graphqlBoolean',           { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Null
  highlight(0, 'graphqlNull',              { fg = colors.pink,       bg = 'NONE' })  -- null


  ---------------------------------------------------------------------------
  -- Identifiers & Names
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlName',              { fg = colors.white,      bg = 'NONE' })  -- General names
  highlight(0, 'graphqlIdentifier',        { fg = colors.white,      bg = 'NONE' })  -- Identifiers


  ---------------------------------------------------------------------------
  -- Meta Fields
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlMetaFields',        { fg = colors.pink,       bg = 'NONE' })  -- __typename, __schema, __type
  highlight(0, 'graphqlTypename',          { fg = colors.pink,       bg = 'NONE' })  -- __typename
  highlight(0, 'graphqlSchemaIntrospect',  { fg = colors.pink,       bg = 'NONE' })  -- __schema
  highlight(0, 'graphqlTypeIntrospect',    { fg = colors.pink,       bg = 'NONE' })  -- __type


  ---------------------------------------------------------------------------
  -- Operators & Punctuation
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlOperator',          { fg = colors.white,      bg = 'NONE' })  -- Operators
  highlight(0, 'graphqlEquals',            { fg = colors.white,      bg = 'NONE' })  -- =
  highlight(0, 'graphqlColon',             { fg = colors.white,      bg = 'NONE' })  -- :
  highlight(0, 'graphqlPipe',              { fg = colors.white,      bg = 'NONE' })  -- | (union separator)
  highlight(0, 'graphqlAmpersand',         { fg = colors.white,      bg = 'NONE' })  -- & (interface implements)
  highlight(0, 'graphqlAt',                { fg = colors.pink,       bg = 'NONE' })  -- @ (directive prefix)
  highlight(0, 'graphqlDollar',            { fg = colors.purple,     bg = 'NONE' })  -- $ (variable prefix)
  highlight(0, 'graphqlSpread',            { fg = colors.white,      bg = 'NONE' })  -- ... (spread operator)

  -- Delimiters & Braces
  highlight(0, 'graphqlBraces',            { fg = colors.white,      bg = 'NONE' })  -- { }
  highlight(0, 'graphqlParens',            { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'graphqlBrackets',          { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'graphqlDelimiter',         { fg = colors.white,      bg = 'NONE' })  -- General delimiters
  highlight(0, 'graphqlComma',             { fg = colors.white,      bg = 'NONE' })  -- ,


  ---------------------------------------------------------------------------
  -- Comments
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlComment',           { fg = colors.red,        bg = 'NONE' })  -- # comments
  highlight(0, 'graphqlDescription',       { fg = colors.red,        bg = 'NONE' })  -- """descriptions"""
  highlight(0, 'graphqlTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME


  ---------------------------------------------------------------------------
  -- Errors
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlError',             { fg = colors.red,        bg = 'NONE' })  -- Syntax errors


  ---------------------------------------------------------------------------
  -- Template Strings (JS/TS embedded GraphQL)
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlTemplateString',    { fg = colors.redLight,   bg = 'NONE' })  -- Template string content
  highlight(0, 'graphqlTaggedTemplate',    { fg = colors.orange,     bg = 'NONE' })  -- gql`...` tag
  highlight(0, 'graphqlTemplateExpression', { fg = colors.purple,    bg = 'NONE' })  -- ${expression} interpolation


  ---------------------------------------------------------------------------
  -- Fold
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlFold',              { fg = colors.white,      bg = 'NONE' })  -- Foldable regions


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.graphql)
  ---------------------------------------------------------------------------

  -- Keywords
  highlight(0, '@keyword.graphql',                 { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, '@keyword.type.graphql',            { fg = colors.blue,       bg = 'NONE' })  -- type, input, etc.
  highlight(0, '@keyword.operator.graphql',        { fg = colors.blue,       bg = 'NONE' })  -- on, implements

  -- Types
  highlight(0, '@type.graphql',                    { fg = colors.turquoise,  bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.graphql',            { fg = colors.turquoise,  bg = 'NONE' })  -- Int, Float, String, Boolean, ID
  highlight(0, '@type.definition.graphql',         { fg = colors.turquoise,  bg = 'NONE' })  -- Type definitions

  -- Variables
  highlight(0, '@variable.graphql',                { fg = colors.purple,     bg = 'NONE' })  -- $variables
  highlight(0, '@variable.parameter.graphql',      { fg = colors.orange,     bg = 'NONE' })  -- Parameters

  -- Properties (Fields)
  highlight(0, '@property.graphql',                { fg = colors.white,      bg = 'NONE' })  -- Fields
  highlight(0, '@property.definition.graphql',     { fg = colors.orange,     bg = 'NONE' })  -- Field definitions

  -- Attributes (Directives)
  highlight(0, '@attribute.graphql',               { fg = colors.pink,       bg = 'NONE' })  -- @directives

  -- Strings
  highlight(0, '@string.graphql',                  { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@string.documentation.graphql',    { fg = colors.red,        bg = 'NONE' })  -- """descriptions"""

  -- Numbers
  highlight(0, '@number.graphql',                  { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.graphql',            { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.graphql',                 { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Constants
  highlight(0, '@constant.graphql',                { fg = colors.pink,       bg = 'NONE' })  -- Enum values
  highlight(0, '@constant.builtin.graphql',        { fg = colors.pink,       bg = 'NONE' })  -- null

  -- Comments
  highlight(0, '@comment.graphql',                 { fg = colors.red,        bg = 'NONE' })  -- # comments

  -- Operators & Punctuation
  highlight(0, '@operator.graphql',                { fg = colors.white,      bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.graphql',     { fg = colors.white,      bg = 'NONE' })  -- { } ( ) [ ]
  highlight(0, '@punctuation.delimiter.graphql',   { fg = colors.white,      bg = 'NONE' })  -- : , |
  highlight(0, '@punctuation.special.graphql',     { fg = colors.white,      bg = 'NONE' })  -- ... ! @


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.graphql)
  ---------------------------------------------------------------------------

  highlight(0, '@lsp.type.type.graphql',           { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.graphql',          { fg = colors.turquoise,  bg = 'NONE' })  -- Object types
  highlight(0, '@lsp.type.interface.graphql',      { fg = colors.turquoise,  bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.enum.graphql',           { fg = colors.turquoise,  bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.graphql',     { fg = colors.pink,       bg = 'NONE' })  -- Enum values
  highlight(0, '@lsp.type.property.graphql',       { fg = colors.white,      bg = 'NONE' })  -- Fields
  highlight(0, '@lsp.type.variable.graphql',       { fg = colors.purple,     bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.graphql',      { fg = colors.orange,     bg = 'NONE' })  -- Arguments
  highlight(0, '@lsp.type.keyword.graphql',        { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.string.graphql',         { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.graphql',         { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.comment.graphql',        { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.decorator.graphql',      { fg = colors.pink,       bg = 'NONE' })  -- Directives


  ---------------------------------------------------------------------------
  -- Introspection Types
  ---------------------------------------------------------------------------

  highlight(0, 'graphqlIntrospectionType', { fg = colors.pink,       bg = 'NONE' })  -- __Type
  highlight(0, 'graphqlIntrospectionField', { fg = colors.pink,      bg = 'NONE' })  -- __Field
  highlight(0, 'graphqlIntrospectionInputValue', { fg = colors.pink, bg = 'NONE' })  -- __InputValue
  highlight(0, 'graphqlIntrospectionEnumValue', { fg = colors.pink,  bg = 'NONE' })  -- __EnumValue
  highlight(0, 'graphqlIntrospectionDirective', { fg = colors.pink,  bg = 'NONE' })  -- __Directive


  ---------------------------------------------------------------------------
  -- SDL (Schema Definition Language) Specific
  ---------------------------------------------------------------------------

  -- Root Operation Types (in schema definition)
  highlight(0, 'graphqlRootQuery',         { fg = colors.blue,       bg = 'NONE' })  -- query: Query
  highlight(0, 'graphqlRootMutation',      { fg = colors.blue,       bg = 'NONE' })  -- mutation: Mutation
  highlight(0, 'graphqlRootSubscription',  { fg = colors.blue,       bg = 'NONE' })  -- subscription: Subscription

  -- Directive Definition
  highlight(0, 'graphqlDirectiveDefinition', { fg = colors.blue,     bg = 'NONE' })  -- directive keyword
  highlight(0, 'graphqlRepeatable',        { fg = colors.blue,       bg = 'NONE' })  -- repeatable keyword

  -- Directive Locations (Executable)
  highlight(0, 'graphqlLocQuery',          { fg = colors.pink,       bg = 'NONE' })  -- QUERY
  highlight(0, 'graphqlLocMutation',       { fg = colors.pink,       bg = 'NONE' })  -- MUTATION
  highlight(0, 'graphqlLocSubscription',   { fg = colors.pink,       bg = 'NONE' })  -- SUBSCRIPTION
  highlight(0, 'graphqlLocField',          { fg = colors.pink,       bg = 'NONE' })  -- FIELD
  highlight(0, 'graphqlLocFragmentDef',    { fg = colors.pink,       bg = 'NONE' })  -- FRAGMENT_DEFINITION
  highlight(0, 'graphqlLocFragmentSpread', { fg = colors.pink,       bg = 'NONE' })  -- FRAGMENT_SPREAD
  highlight(0, 'graphqlLocInlineFragment', { fg = colors.pink,       bg = 'NONE' })  -- INLINE_FRAGMENT
  highlight(0, 'graphqlLocVariableDef',    { fg = colors.pink,       bg = 'NONE' })  -- VARIABLE_DEFINITION

  -- Directive Locations (Type System)
  highlight(0, 'graphqlLocSchema',         { fg = colors.pink,       bg = 'NONE' })  -- SCHEMA
  highlight(0, 'graphqlLocScalar',         { fg = colors.pink,       bg = 'NONE' })  -- SCALAR
  highlight(0, 'graphqlLocObject',         { fg = colors.pink,       bg = 'NONE' })  -- OBJECT
  highlight(0, 'graphqlLocFieldDef',       { fg = colors.pink,       bg = 'NONE' })  -- FIELD_DEFINITION
  highlight(0, 'graphqlLocArgumentDef',    { fg = colors.pink,       bg = 'NONE' })  -- ARGUMENT_DEFINITION
  highlight(0, 'graphqlLocInterface',      { fg = colors.pink,       bg = 'NONE' })  -- INTERFACE
  highlight(0, 'graphqlLocUnion',          { fg = colors.pink,       bg = 'NONE' })  -- UNION
  highlight(0, 'graphqlLocEnum',           { fg = colors.pink,       bg = 'NONE' })  -- ENUM
  highlight(0, 'graphqlLocEnumValue',      { fg = colors.pink,       bg = 'NONE' })  -- ENUM_VALUE
  highlight(0, 'graphqlLocInputObject',    { fg = colors.pink,       bg = 'NONE' })  -- INPUT_OBJECT
  highlight(0, 'graphqlLocInputFieldDef',  { fg = colors.pink,       bg = 'NONE' })  -- INPUT_FIELD_DEFINITION

end

return graphql
