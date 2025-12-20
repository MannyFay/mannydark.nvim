-------------------------------------------------------------------------------
-- TypeScript Files
-- Highlighting for .ts, .mts, .cts, .d.ts files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local typescript = {}


-------------------------------------------------------------------------------
-- Settings

typescript.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy) - typescript*

  -- Keywords - Declaration
  highlight(0, 'typescriptVariable',            { link = "Variable" })  -- let, const, var
  highlight(0, 'typescriptVariableDeclaration', { link = "Variable" })  -- Variable names

  -- Keywords - Module
  highlight(0, 'typescriptImport',              { fg = colors.blue,       bg = 'NONE'            })  -- import
  highlight(0, 'typescriptExport',              { fg = colors.blue,       bg = 'NONE'            })  -- export
  highlight(0, 'typescriptDefault',             { fg = colors.blue,       bg = 'NONE'            })  -- default
  highlight(0, 'typescriptModule',              { fg = colors.blue,       bg = 'NONE'            })  -- module, namespace
  highlight(0, 'typescriptAmbientDeclaration',  { fg = colors.blue,       bg = 'NONE'            })  -- declare
  highlight(0, 'typescriptImportType',          { link = "Type" })  -- import type
  highlight(0, 'typescriptExportType',          { link = "Type" })  -- export type
  highlight(0, 'typescriptDefaultImportName',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Imported module name

  -- Keywords - Control Flow
  highlight(0, 'typescriptConditional',         { link = "Conditional" })  -- if, else, switch
  highlight(0, 'typescriptConditionalElse',     { link = "Conditional" })  -- else
  highlight(0, 'typescriptRepeat',              { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'typescriptForOperator',         { link = "Operator" })  -- in, of
  highlight(0, 'typescriptAsyncFor',            { fg = colors.blue,       bg = 'NONE'            })  -- for await
  highlight(0, 'typescriptBranch',              { fg = colors.blue,       bg = 'NONE'            })  -- break, continue
  highlight(0, 'typescriptCase',                { fg = colors.blue,       bg = 'NONE'            })  -- case, default
  highlight(0, 'typescriptStatementKeyword',    { link = "Keyword" })  -- return, yield, throw

  -- Keywords - Exception
  highlight(0, 'typescriptTry',                 { fg = colors.blue,       bg = 'NONE'            })  -- try
  highlight(0, 'typescriptExceptions',          { fg = colors.blue,       bg = 'NONE'            })  -- catch, finally, throw

  -- Keywords - Function
  highlight(0, 'typescriptFuncKeyword',         { link = "Keyword" })  -- function
  highlight(0, 'typescriptAsyncFunc',           { link = "Function" })  -- async
  highlight(0, 'typescriptAsyncFuncKeyword',    { link = "Keyword" })  -- async function

  -- Keywords - Class
  highlight(0, 'typescriptClassKeyword',        { link = "Keyword" })  -- class
  highlight(0, 'typescriptClassName',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names
  highlight(0, 'typescriptClassExtends',        { fg = colors.blue,       bg = 'NONE'            })  -- extends
  highlight(0, 'typescriptClassHeritage',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Extended class
  highlight(0, 'typescriptAbstract',            { fg = colors.blue,       bg = 'NONE'            })  -- abstract
  highlight(0, 'typescriptClassStatic',         { fg = colors.blue,       bg = 'NONE'            })  -- static
  highlight(0, 'typescriptConstructor',         { fg = colors.blue,       bg = 'NONE'            })  -- constructor

  -- Keywords - Interface/Type
  highlight(0, 'typescriptInterfaceKeyword',    { link = "Keyword" })  -- interface
  highlight(0, 'typescriptInterfaceName',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Interface names
  highlight(0, 'typescriptInterfaceExtends',    { fg = colors.blue,       bg = 'NONE'            })  -- extends
  highlight(0, 'typescriptInterfaceHeritage',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Extended interface
  highlight(0, 'typescriptAliasKeyword',        { link = "Keyword" })  -- type (alias)
  highlight(0, 'typescriptAliasDeclaration',    { fg = colors.turquoise,  bg = 'NONE'            })  -- Type alias name

  -- Keywords - Enum
  highlight(0, 'typescriptEnumKeyword',         { link = "Keyword" })  -- enum
  highlight(0, 'typescriptEnum',                { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum names

  -- Keywords - Access Modifiers
  highlight(0, 'typescriptAccessibilityModifier', { fg = colors.blue,     bg = 'NONE'            })  -- public, private, protected
  highlight(0, 'typescriptReadonlyModifier',    { fg = colors.blue,       bg = 'NONE'            })  -- readonly

  -- Keywords - Operators
  highlight(0, 'typescriptOperator',            { link = "Operator" })  -- Operators
  highlight(0, 'typescriptKeywordOp',           { link = "Keyword" })  -- typeof, keyof, instanceof, new, delete, in, void
  highlight(0, 'typescriptCastKeyword',         { link = "Keyword" })  -- as
  highlight(0, 'typescriptAssertType',          { link = "Type" })  -- as, satisfies
  highlight(0, 'typescriptTypeQuery',           { link = "Type" })  -- typeof (in type context)
  highlight(0, 'typescriptConstraint',          { fg = colors.blue,       bg = 'NONE'            })  -- extends (in generics)
  highlight(0, 'typescriptMappedIn',            { fg = colors.blue,       bg = 'NONE'            })  -- in (mapped types)

  -- Keywords - Special
  highlight(0, 'typescriptIdentifier',          { fg = colors.blue,       bg = 'NONE'            })  -- this, super
  highlight(0, 'typescriptDebugger',            { fg = colors.blue,       bg = 'NONE'            })  -- debugger

  -- Types - Predefined
  highlight(0, 'typescriptPredefinedType',      { link = "Type" })  -- string, number, boolean, any, void, never, unknown, object, symbol, bigint
  highlight(0, 'typescriptType',                { link = "Type" })  -- Type names
  highlight(0, 'typescriptTypeReference',       { link = "Type" })  -- Type references
  highlight(0, 'typescriptUserDefinedType',     { link = "Type" })  -- User-defined types
  highlight(0, 'typescriptTypeBlock',           { link = "Type" })  -- Type in import
  highlight(0, 'typescriptTypeParameter',       { link = "Type" })  -- Generic type parameters <T>

  -- Types - Special
  highlight(0, 'typescriptStringLiteralType',   { link = "String" })  -- String literal types
  highlight(0, 'typescriptTemplateLiteralType', { link = "Type" })  -- Template literal types
  highlight(0, 'typescriptReadonlyArrayKeyword', { link = "Keyword" })  -- ReadonlyArray
  highlight(0, 'typescriptFuncType',            { link = "Type" })  -- Function types
  highlight(0, 'typescriptFuncTypeArrow',       { link = "Type" })  -- => in function types
  highlight(0, 'typescriptConstructorType',     { link = "Type" })  -- new () => T
  highlight(0, 'typescriptUnion',               { fg = colors.white,      bg = 'NONE'            })  -- | union
  highlight(0, 'typescriptTypeOperator',        { link = "Operator" })  -- keyof, typeof, infer

  -- Functions
  highlight(0, 'typescriptFuncName',            { link = "Function" })  -- Function names
  highlight(0, 'typescriptFuncArg',             { link = "Function" })  -- Function arguments
  highlight(0, 'typescriptArrowFuncArg',        { link = "Function" })  -- Arrow function args
  highlight(0, 'typescriptArrowFunc',           { link = "Function" })  -- =>
  highlight(0, 'typescriptCall',                { fg = colors.purple,     bg = 'NONE'            })  -- Call expressions
  highlight(0, 'typescriptParamImpl',           { fg = colors.purple,     bg = 'NONE'            })  -- Parameter implementation

  -- Members
  highlight(0, 'typescriptMember',              { fg = colors.orange,     bg = 'NONE'            })  -- Class/interface members
  highlight(0, 'typescriptMethodAccessor',      { link = "Function" })  -- get, set
  highlight(0, 'typescriptMemberOptionality',   { fg = colors.pink,       bg = 'NONE'            })  -- ? optional member

  -- Properties
  highlight(0, 'typescriptProp',                { fg = colors.purple,     bg = 'NONE'            })  -- Properties
  highlight(0, 'typescriptProperty',            { fg = colors.purple,     bg = 'NONE'            })  -- Properties
  highlight(0, 'typescriptStringProperty',      { link = "String" })  -- String properties
  highlight(0, 'typescriptComputedPropertyName', { fg = colors.purple,    bg = 'NONE'            })  -- [computed] properties

  -- Global Objects
  highlight(0, 'typescriptGlobal',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Global objects
  highlight(0, 'typescriptGlobalMethod',        { link = "Function" })  -- Global methods

  -- Built-in Object Methods
  highlight(0, 'typescriptObjectStaticMethod',  { link = "Function" })  -- Object.keys, etc.
  highlight(0, 'typescriptObjectMethod',        { link = "Function" })  -- Object methods
  highlight(0, 'typescriptArrayStaticMethod',   { link = "Function" })  -- Array.from, etc.
  highlight(0, 'typescriptArrayMethod',         { link = "Function" })  -- Array methods
  highlight(0, 'typescriptStringStaticMethod',  { link = "String" })  -- String methods
  highlight(0, 'typescriptStringMethod',        { link = "String" })  -- String methods
  highlight(0, 'typescriptNumberStaticMethod',  { link = "Number" })  -- Number methods
  highlight(0, 'typescriptNumberMethod',        { link = "Number" })  -- Number methods
  highlight(0, 'typescriptMathStaticMethod',    { link = "Function" })  -- Math methods
  highlight(0, 'typescriptDateStaticMethod',    { link = "Function" })  -- Date methods
  highlight(0, 'typescriptDateMethod',          { link = "Function" })  -- Date methods
  highlight(0, 'typescriptJSONStaticMethod',    { link = "Function" })  -- JSON methods
  highlight(0, 'typescriptRegExpMethod',        { link = "Function" })  -- RegExp methods
  highlight(0, 'typescriptPromiseStaticMethod', { link = "Function" })  -- Promise methods
  highlight(0, 'typescriptPromiseMethod',       { link = "Function" })  -- Promise methods
  highlight(0, 'typescriptReflectMethod',       { link = "Function" })  -- Reflect methods
  highlight(0, 'typescriptSymbolStaticMethod',  { link = "Function" })  -- Symbol methods
  highlight(0, 'typescriptFunctionMethod',      { link = "Function" })  -- Function methods
  highlight(0, 'typescriptES6MapMethod',        { link = "Function" })  -- Map methods
  highlight(0, 'typescriptES6SetMethod',        { link = "Function" })  -- Set methods
  highlight(0, 'typescriptProxyAPI',            { fg = colors.orange,     bg = 'NONE'            })  -- Proxy methods
  highlight(0, 'typescriptIntlMethod',          { link = "Function" })  -- Intl methods

  -- Static Properties
  highlight(0, 'typescriptMathStaticProp',      { fg = colors.pink,       bg = 'NONE'            })  -- Math.PI, etc.
  highlight(0, 'typescriptNumberStaticProp',    { link = "Number" })  -- Number.MAX_VALUE, etc.
  highlight(0, 'typescriptSymbolStaticProp',    { fg = colors.pink,       bg = 'NONE'            })  -- Symbol.iterator, etc.
  highlight(0, 'typescriptRegExpStaticProp',    { fg = colors.pink,       bg = 'NONE'            })  -- RegExp properties
  highlight(0, 'typescriptRegExpProp',          { fg = colors.pink,       bg = 'NONE'            })  -- RegExp properties
  highlight(0, 'typescriptES6MapProp',          { fg = colors.pink,       bg = 'NONE'            })  -- Map.size
  highlight(0, 'typescriptES6SetProp',          { fg = colors.pink,       bg = 'NONE'            })  -- Set.size

  -- DOM/BOM
  highlight(0, 'typescriptBOM',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- window, document
  highlight(0, 'typescriptBOMWindowProp',       { fg = colors.purple,     bg = 'NONE'            })  -- Window properties
  highlight(0, 'typescriptBOMWindowMethod',     { link = "Function" })  -- Window methods
  highlight(0, 'typescriptBOMWindowCons',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Window constructors
  highlight(0, 'typescriptBOMNavigatorProp',    { fg = colors.purple,     bg = 'NONE'            })  -- Navigator properties
  highlight(0, 'typescriptBOMNavigatorMethod',  { link = "Function" })  -- Navigator methods
  highlight(0, 'typescriptBOMLocationProp',     { fg = colors.purple,     bg = 'NONE'            })  -- Location properties
  highlight(0, 'typescriptBOMLocationMethod',   { link = "Function" })  -- Location methods
  highlight(0, 'typescriptBOMHistoryProp',      { fg = colors.purple,     bg = 'NONE'            })  -- History properties
  highlight(0, 'typescriptBOMHistoryMethod',    { link = "Function" })  -- History methods
  highlight(0, 'typescriptConsoleMethod',       { link = "Function" })  -- console methods
  highlight(0, 'typescriptDOMNodeProp',         { fg = colors.purple,     bg = 'NONE'            })  -- DOM properties
  highlight(0, 'typescriptDOMNodeMethod',       { link = "Function" })  -- DOM methods
  highlight(0, 'typescriptDOMNodeType',         { link = "Type" })  -- DOM types
  highlight(0, 'typescriptDOMDocProp',          { fg = colors.purple,     bg = 'NONE'            })  -- Document properties
  highlight(0, 'typescriptDOMDocMethod',        { link = "Function" })  -- Document methods
  highlight(0, 'typescriptDOMElemAttrs',        { fg = colors.purple,     bg = 'NONE'            })  -- Element attributes
  highlight(0, 'typescriptDOMElemFuncs',        { link = "Function" })  -- Element functions
  highlight(0, 'typescriptDOMEventTargetMethod', { link = "Function" })  -- Event target methods
  highlight(0, 'typescriptDOMEventCons',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Event constructors
  highlight(0, 'typescriptDOMEventProp',        { fg = colors.purple,     bg = 'NONE'            })  -- Event properties
  highlight(0, 'typescriptDOMEventMethod',      { link = "Function" })  -- Event methods
  highlight(0, 'typescriptDOMStorage',          { fg = colors.turquoise,  bg = 'NONE'            })  -- localStorage, sessionStorage
  highlight(0, 'typescriptDOMStorageProp',      { fg = colors.purple,     bg = 'NONE'            })  -- Storage properties
  highlight(0, 'typescriptDOMStorageMethod',    { link = "Function" })  -- Storage methods
  highlight(0, 'typescriptDOMFormProp',         { fg = colors.purple,     bg = 'NONE'            })  -- Form properties
  highlight(0, 'typescriptDOMFormMethod',       { link = "Function" })  -- Form methods
  highlight(0, 'typescriptDOMStyle',            { fg = colors.purple,     bg = 'NONE'            })  -- style property

  -- Fetch/XHR/Network
  highlight(0, 'typescriptXHRGlobal',           { fg = colors.turquoise,  bg = 'NONE'            })  -- XMLHttpRequest
  highlight(0, 'typescriptXHRProp',             { fg = colors.purple,     bg = 'NONE'            })  -- XHR properties
  highlight(0, 'typescriptXHRMethod',           { link = "Function" })  -- XHR methods
  highlight(0, 'typescriptHeadersMethod',       { link = "Function" })  -- Headers methods
  highlight(0, 'typescriptRequestProp',         { fg = colors.purple,     bg = 'NONE'            })  -- Request properties
  highlight(0, 'typescriptRequestMethod',       { link = "Function" })  -- Request methods
  highlight(0, 'typescriptResponseProp',        { fg = colors.purple,     bg = 'NONE'            })  -- Response properties
  highlight(0, 'typescriptResponseMethod',      { link = "Function" })  -- Response methods
  highlight(0, 'typescriptBOMNetworkProp',      { fg = colors.purple,     bg = 'NONE'            })  -- Network properties

  -- Other APIs
  highlight(0, 'typescriptCryptoGlobal',        { fg = colors.turquoise,  bg = 'NONE'            })  -- crypto
  highlight(0, 'typescriptCryptoProp',          { fg = colors.purple,     bg = 'NONE'            })  -- Crypto properties
  highlight(0, 'typescriptCryptoMethod',        { link = "Function" })  -- Crypto methods
  highlight(0, 'typescriptSubtleCryptoMethod',  { link = "Function" })  -- SubtleCrypto methods
  highlight(0, 'typescriptEncodingGlobal',      { fg = colors.turquoise,  bg = 'NONE'            })  -- TextEncoder, etc.
  highlight(0, 'typescriptEncodingProp',        { fg = colors.purple,     bg = 'NONE'            })  -- Encoding properties
  highlight(0, 'typescriptEncodingMethod',      { link = "Function" })  -- Encoding methods
  highlight(0, 'typescriptFileMethod',          { link = "Function" })  -- File methods
  highlight(0, 'typescriptFileReaderProp',      { fg = colors.purple,     bg = 'NONE'            })  -- FileReader properties
  highlight(0, 'typescriptFileReaderMethod',    { link = "Function" })  -- FileReader methods
  highlight(0, 'typescriptFileListMethod',      { link = "Function" })  -- FileList methods
  highlight(0, 'typescriptBlobMethod',          { link = "Function" })  -- Blob methods
  highlight(0, 'typescriptURLUtilsProp',        { fg = colors.purple,     bg = 'NONE'            })  -- URL utilities
  highlight(0, 'typescriptURLStaticMethod',     { link = "Function" })  -- URL static methods
  highlight(0, 'typescriptGeolocationMethod',   { link = "Function" })  -- Geolocation methods
  highlight(0, 'typescriptServiceWorkerProp',   { fg = colors.purple,     bg = 'NONE'            })  -- SW properties
  highlight(0, 'typescriptServiceWorkerMethod', { link = "Function" })  -- SW methods
  highlight(0, 'typescriptCacheMethod',         { link = "Function" })  -- Cache methods

  -- Node.js
  highlight(0, 'typescriptNodeGlobal',          { fg = colors.turquoise,  bg = 'NONE'            })  -- process, Buffer, etc.
  highlight(0, 'typescriptTestGlobal',          { fg = colors.orange,     bg = 'NONE'            })  -- describe, it, etc.

  -- Literals
  highlight(0, 'typescriptBoolean',             { link = "Boolean" })  -- true, false
  highlight(0, 'typescriptNull',                { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'typescriptNumber',              { link = "Number" })  -- Numbers
  highlight(0, 'typescriptString',              { link = "String" })  -- Strings

  -- Template Strings
  highlight(0, 'typescriptTemplate',            { fg = colors.redLight,   bg = 'NONE'            })  -- Template literals
  highlight(0, 'typescriptTemplateSB',          { fg = colors.pink,       bg = 'NONE'            })  -- ${} in templates
  highlight(0, 'typescriptTemplateSubstitution', { link = "Variable" })  -- Expression in ${}

  -- Regular Expressions
  highlight(0, 'typescriptRegexpString',        { link = "String" })  -- /regex/
  highlight(0, 'typescriptRegexpBoundary',      { fg = colors.redLight,   bg = 'NONE'            })  -- / delimiters
  highlight(0, 'typescriptRegexpGroup',         { fg = colors.redLight,   bg = 'NONE'            })  -- Groups
  highlight(0, 'typescriptRegexpCharClass',     { fg = colors.pink,       bg = 'NONE'            })  -- [...] character class
  highlight(0, 'typescriptRegexpBackRef',       { fg = colors.pink,       bg = 'NONE'            })  -- Back references
  highlight(0, 'typescriptRegexpQuantifier',    { fg = colors.pink,       bg = 'NONE'            })  -- +, *, ?, {n,m}
  highlight(0, 'typescriptRegexpMod',           { fg = colors.pink,       bg = 'NONE'            })  -- Flags (g, i, m, etc.)
  highlight(0, 'typescriptRegexpOr',            { fg = colors.white,      bg = 'NONE'            })  -- | alternation

  -- Decorators
  highlight(0, 'typescriptDecorator',           { fg = colors.pink,       bg = 'NONE'            })  -- @decorator

  -- Labels
  highlight(0, 'typescriptLabel',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Labels
  highlight(0, 'typescriptObjectLabel',         { fg = colors.purple,     bg = 'NONE'            })  -- Object labels
  highlight(0, 'typescriptTupleLable',          { fg = colors.purple,     bg = 'NONE'            })  -- Tuple labels

  -- Punctuation/Brackets
  highlight(0, 'typescriptBraces',              { fg = colors.white,      bg = 'NONE'            })  -- {}
  highlight(0, 'typescriptParens',              { fg = colors.white,      bg = 'NONE'            })  -- ()
  highlight(0, 'typescriptTypeBrackets',        { link = "Type" })  -- <> in generics
  highlight(0, 'typescriptTypeBracket',         { link = "Type" })  -- Brackets
  highlight(0, 'typescriptEndColons',           { fg = colors.white,      bg = 'NONE'            })  -- Semicolons
  highlight(0, 'typescriptObjectColon',         { fg = colors.white,      bg = 'NONE'            })  -- : in objects
  highlight(0, 'typescriptFuncComma',           { link = "Function" })  -- , in functions
  highlight(0, 'typescriptDotNotation',         { fg = colors.white,      bg = 'NONE'            })  -- .
  highlight(0, 'typescriptOptionalMark',        { fg = colors.pink,       bg = 'NONE'            })  -- ? optional
  highlight(0, 'typescriptSymbols',             { fg = colors.white,      bg = 'NONE'            })  -- Various symbols
  highlight(0, 'typescriptSpecial',             { fg = colors.pink,       bg = 'NONE'            })  -- Special characters
  highlight(0, 'typescriptASCII',               { fg = colors.pink,       bg = 'NONE'            })  -- ASCII escapes

  -- Spread/Rest
  highlight(0, 'typescriptRestOrSpread',        { fg = colors.pink,       bg = 'NONE'            })  -- ...
  highlight(0, 'typescriptObjectSpread',        { fg = colors.pink,       bg = 'NONE'            })  -- Object spread

  -- Destructuring
  highlight(0, 'typescriptArrayDestructure',    { fg = 'NONE',            bg = 'NONE'            })  -- Array destructuring
  highlight(0, 'typescriptObjectDestructure',   { fg = 'NONE',            bg = 'NONE'            })  -- Object destructuring
  highlight(0, 'typescriptDestructureLabel',    { fg = colors.purple,     bg = 'NONE'            })  -- Destructure label
  highlight(0, 'typescriptDestructureVariable', { link = "Variable" })  -- Destructure variable
  highlight(0, 'typescriptDestructureAs',       { fg = colors.blue,       bg = 'NONE'            })  -- as in destructure
  highlight(0, 'typescriptDestructureString',   { link = "String" })  -- String in destructure
  highlight(0, 'typescriptDestructureComma',    { fg = colors.white,      bg = 'NONE'            })  -- , in destructure
  highlight(0, 'typescriptDefaultParam',        { fg = colors.purple,     bg = 'NONE'            })  -- Default parameters

  -- Ternary
  highlight(0, 'typescriptTernary',             { fg = colors.white,      bg = 'NONE'            })  -- Ternary expression
  highlight(0, 'typescriptTernaryOp',           { fg = colors.white,      bg = 'NONE'            })  -- ? : operators

  -- Blocks (cleared)
  highlight(0, 'typescriptBlock',               { fg = 'NONE',            bg = 'NONE'            })  -- Generic block
  highlight(0, 'typescriptClassBlock',          { fg = 'NONE',            bg = 'NONE'            })  -- Class body
  highlight(0, 'typescriptObjectLiteral',       { fg = 'NONE',            bg = 'NONE'            })  -- Object literal
  highlight(0, 'typescriptFuncCallArg',         { link = "Function" })  -- Function call args
  highlight(0, 'typescriptConditionalParen',    { link = "Conditional" })  -- Conditional paren
  highlight(0, 'typescriptLoopParen',           { fg = 'NONE',            bg = 'NONE'            })  -- Loop paren
  highlight(0, 'typescriptParenExp',            { fg = 'NONE',            bg = 'NONE'            })  -- Paren expression
  highlight(0, 'typescriptIndexExpr',           { fg = 'NONE',            bg = 'NONE'            })  -- Index expression
  highlight(0, 'typescriptTypeAnnotation',      { link = "Type" })  -- Type annotation
  highlight(0, 'typescriptAssign',              { fg = 'NONE',            bg = 'NONE'            })  -- Assignment
  highlight(0, 'typescriptGenericImpl',         { fg = 'NONE',            bg = 'NONE'            })  -- Generic implementation
  highlight(0, 'typescriptGenericFunc',         { link = "Function" })  -- Generic function
  highlight(0, 'typescriptGenericCall',         { fg = 'NONE',            bg = 'NONE'            })  -- Generic call
  highlight(0, 'typescriptGenericDefault',      { fg = 'NONE',            bg = 'NONE'            })  -- Generic default
  highlight(0, 'typescriptTypeArguments',       { link = "Type" })  -- Type arguments
  highlight(0, 'typescriptTypeParameters',      { link = "Type" })  -- Type parameters
  highlight(0, 'typescriptArrowFuncDef',        { link = "Function" })  -- Arrow func definition
  highlight(0, 'typescriptArrowFuncTypeParameter', { link = "Type" })  -- Arrow func type param
  highlight(0, 'typescriptReturnAnnotation',    { fg = 'NONE',            bg = 'NONE'            })  -- Return annotation
  highlight(0, 'typescriptFuncImpl',            { link = "Function" })  -- Function implementation
  highlight(0, 'typescriptFunctionType',        { link = "Type" })  -- Function type
  highlight(0, 'typescriptTypeCast',            { link = "Type" })  -- Type cast
  highlight(0, 'typescriptConditionalType',     { link = "Conditional" })  -- Conditional type
  highlight(0, 'typescriptParenthesizedType',   { link = "Type" })  -- Parenthesized type
  highlight(0, 'typescriptObjectType',          { link = "Type" })  -- Object type
  highlight(0, 'typescriptTupleType',           { link = "Type" })  -- Tuple type
  highlight(0, 'typescriptIndexSignature',      { fg = 'NONE',            bg = 'NONE'            })  -- Index signature
  highlight(0, 'typescriptConstructSignature',  { fg = 'NONE',            bg = 'NONE'            })  -- Construct signature
  highlight(0, 'typescriptInterfaceTypeParameter', { link = "Type" })  -- Interface type param
  highlight(0, 'typescriptInterfaceTypeArguments', { link = "Type" })  -- Interface type args
  highlight(0, 'typescriptInterfaceComma',      { fg = colors.white,      bg = 'NONE'            })  -- , in interface
  highlight(0, 'typescriptClassTypeParameter',  { link = "Type" })  -- Class type param
  highlight(0, 'typescriptClassTypeArguments',  { link = "Type" })  -- Class type args
  highlight(0, 'typescriptMixinComma',          { fg = colors.white,      bg = 'NONE'            })  -- , in mixin

  -- Comments
  highlight(0, 'typescriptComment',             { link = "Comment" })  -- /* */
  highlight(0, 'typescriptLineComment',         { link = "Comment" })  -- //
  highlight(0, 'typescriptCommentTodo',         { link = "Comment" })  -- TODO, FIXME
  highlight(0, 'typescriptMagicComment',        { link = "Comment" })  -- @ts-ignore, etc.
  highlight(0, 'typescriptRef',                 { fg = colors.red,        bg = 'NONE'            })  -- /// <reference>

  -- JSDoc
  highlight(0, 'typescriptDocComment',          { link = "Comment" })  -- /** */
  highlight(0, 'typescriptDocTags',             { fg = colors.pink,       bg = 'NONE'            })  -- @param, @returns
  highlight(0, 'typescriptDocNotation',         { fg = colors.pink,       bg = 'NONE'            })  -- @
  highlight(0, 'typescriptDocParam',            { fg = colors.purple,     bg = 'NONE'            })  -- Param name
  highlight(0, 'typescriptDocParamName',        { fg = colors.purple,     bg = 'NONE'            })  -- Param name
  highlight(0, 'typescriptDocParamType',        { link = "Type" })  -- {Type}
  highlight(0, 'typescriptDocNamedParamType',   { link = "Type" })  -- Named param type
  highlight(0, 'typescriptDocNumParam',         { fg = colors.purple,     bg = 'NONE'            })  -- Numeric param
  highlight(0, 'typescriptDocEventRef',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Event reference

  -- Events
  highlight(0, 'typescriptBOMWindowEvent',      { fg = colors.purple,     bg = 'NONE'            })  -- Window events
  highlight(0, 'typescriptAnimationEvent',      { fg = colors.purple,     bg = 'NONE'            })  -- Animation events
  highlight(0, 'typescriptCSSEvent',            { fg = colors.purple,     bg = 'NONE'            })  -- CSS events
  highlight(0, 'typescriptDatabaseEvent',       { fg = colors.purple,     bg = 'NONE'            })  -- Database events
  highlight(0, 'typescriptDocumentEvent',       { fg = colors.purple,     bg = 'NONE'            })  -- Document events
  highlight(0, 'typescriptDOMMutationEvent',    { fg = colors.purple,     bg = 'NONE'            })  -- DOM mutation events
  highlight(0, 'typescriptDragEvent',           { fg = colors.purple,     bg = 'NONE'            })  -- Drag events
  highlight(0, 'typescriptElementEvent',        { fg = colors.purple,     bg = 'NONE'            })  -- Element events
  highlight(0, 'typescriptFocusEvent',          { fg = colors.purple,     bg = 'NONE'            })  -- Focus events
  highlight(0, 'typescriptFormEvent',           { fg = colors.purple,     bg = 'NONE'            })  -- Form events
  highlight(0, 'typescriptFrameEvent',          { fg = colors.purple,     bg = 'NONE'            })  -- Frame events
  highlight(0, 'typescriptInputDeviceEvent',    { fg = colors.purple,     bg = 'NONE'            })  -- Input events
  highlight(0, 'typescriptMediaEvent',          { fg = colors.purple,     bg = 'NONE'            })  -- Media events
  highlight(0, 'typescriptMenuEvent',           { fg = colors.purple,     bg = 'NONE'            })  -- Menu events
  highlight(0, 'typescriptNetworkEvent',        { fg = colors.purple,     bg = 'NONE'            })  -- Network events
  highlight(0, 'typescriptProgressEvent',       { fg = colors.purple,     bg = 'NONE'            })  -- Progress events
  highlight(0, 'typescriptResourceEvent',       { fg = colors.purple,     bg = 'NONE'            })  -- Resource events
  highlight(0, 'typescriptScriptEvent',         { fg = colors.purple,     bg = 'NONE'            })  -- Script events
  highlight(0, 'typescriptSensorEvent',         { fg = colors.purple,     bg = 'NONE'            })  -- Sensor events
  highlight(0, 'typescriptSessionHistoryEvent', { fg = colors.purple,     bg = 'NONE'            })  -- History events
  highlight(0, 'typescriptStorageEvent',        { fg = colors.purple,     bg = 'NONE'            })  -- Storage events
  highlight(0, 'typescriptSVGEvent',            { fg = colors.purple,     bg = 'NONE'            })  -- SVG events
  highlight(0, 'typescriptTabEvent',            { fg = colors.purple,     bg = 'NONE'            })  -- Tab events
  highlight(0, 'typescriptTextEvent',           { fg = colors.purple,     bg = 'NONE'            })  -- Text events
  highlight(0, 'typescriptTouchEvent',          { fg = colors.purple,     bg = 'NONE'            })  -- Touch events
  highlight(0, 'typescriptUpdateEvent',         { fg = colors.purple,     bg = 'NONE'            })  -- Update events
  highlight(0, 'typescriptValueChangeEvent',    { fg = colors.purple,     bg = 'NONE'            })  -- Value change events
  highlight(0, 'typescriptViewEvent',           { fg = colors.purple,     bg = 'NONE'            })  -- View events
  highlight(0, 'typescriptWebsocketEvent',      { fg = colors.purple,     bg = 'NONE'            })  -- WebSocket events
  highlight(0, 'typescriptWindowEvent',         { fg = colors.purple,     bg = 'NONE'            })  -- Window events
  highlight(0, 'typescriptUncategorizedEvent',  { fg = colors.purple,     bg = 'NONE'            })  -- Other events
  highlight(0, 'typescriptServiceWorkerEvent',  { fg = colors.purple,     bg = 'NONE'            })  -- Service worker events
  highlight(0, 'typescriptPaymentEvent',        { fg = colors.purple,     bg = 'NONE'            })  -- Payment events
  highlight(0, 'typescriptEventString',         { link = "String" })  -- Event string

  -- Payment API
  highlight(0, 'typescriptPaymentMethod',       { link = "Function" })  -- Payment methods
  highlight(0, 'typescriptPaymentProp',         { fg = colors.purple,     bg = 'NONE'            })  -- Payment properties
  highlight(0, 'typescriptPaymentResponseMethod', { link = "Function" })  -- Response methods
  highlight(0, 'typescriptPaymentResponseProp', { fg = colors.purple,     bg = 'NONE'            })  -- Response properties
  highlight(0, 'typescriptPaymentAddressProp',  { fg = colors.purple,     bg = 'NONE'            })  -- Address properties
  highlight(0, 'typescriptPaymentShippingOptionProp', { fg = colors.purple, bg = 'NONE'          })  -- Shipping properties

  -- Errors
  highlight(0, 'typescriptReserved',            { fg = colors.red,        bg = 'NONE'            })  -- Reserved words
  highlight(0, 'typescriptMessage',             { fg = colors.red,        bg = 'NONE'            })  -- Error messages

  -- Misc
  highlight(0, 'typescriptIdentifierName',      { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'typescriptArray',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Array type
  highlight(0, 'typescriptPrototype',           { fg = colors.pink,       bg = 'NONE'            })  -- prototype
  highlight(0, 'typescriptBinaryOp',            { fg = colors.white,      bg = 'NONE'            })  -- Binary operators
  highlight(0, 'typescriptUnaryOp',             { fg = colors.white,      bg = 'NONE'            })  -- Unary operators
  highlight(0, 'typeScript',                    { fg = colors.white,      bg = 'NONE'            })  -- Generic TypeScript


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.typescript)

  -- Variables
  highlight(0, '@variable.typescript',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.typescript',      { link = "Variable" })  -- this, super
  highlight(0, '@variable.member.typescript',       { link = "Variable" })  -- Object members
  highlight(0, '@variable.parameter.typescript',    { link = "Variable" })  -- Function parameters

  -- Constants
  highlight(0, '@constant.typescript',              { link = "Constant" })  -- UPPER_CASE constants
  highlight(0, '@constant.builtin.typescript',      { link = "Constant" })  -- true, false, null, undefined

  -- Modules
  highlight(0, '@module.typescript',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names

  -- Types
  highlight(0, '@type.typescript',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.typescript',          { link = "Type" })  -- Built-in types
  highlight(0, '@type.definition.typescript',       { link = "Type" })  -- Type definitions
  highlight(0, '@type.qualifier.typescript',        { link = "Type" })  -- Type qualifiers

  -- Functions
  highlight(0, '@function.typescript',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.typescript',         { link = "Function" })  -- Function calls
  highlight(0, '@function.method.typescript',       { link = "Function" })  -- Methods
  highlight(0, '@function.method.call.typescript',  { link = "Function" })  -- Method calls
  highlight(0, '@function.builtin.typescript',      { link = "Function" })  -- Built-in functions
  highlight(0, '@constructor.typescript',           { fg = colors.turquoise, bg = 'NONE' })  -- Constructor calls

  -- Keywords
  highlight(0, '@keyword.typescript',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.typescript',      { link = "Keyword" })  -- function, async
  highlight(0, '@keyword.operator.typescript',      { link = "Operator" })  -- typeof, keyof, instanceof, as, satisfies
  highlight(0, '@keyword.return.typescript',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.conditional.typescript',   { link = "Conditional" })  -- if, else, switch
  highlight(0, '@keyword.conditional.ternary.typescript', { link = "Conditional" })  -- ? :
  highlight(0, '@keyword.repeat.typescript',        { link = "Keyword" })  -- for, while, do
  highlight(0, '@keyword.exception.typescript',     { link = "Keyword" })  -- try, catch, finally, throw
  highlight(0, '@keyword.import.typescript',        { link = "Keyword" })  -- import, export, from, require
  highlight(0, '@keyword.coroutine.typescript',     { link = "Keyword" })  -- async, await, yield
  highlight(0, '@keyword.type.typescript',          { link = "Keyword" })  -- type, interface, enum, namespace
  highlight(0, '@keyword.modifier.typescript',      { link = "Keyword" })  -- public, private, protected, readonly, abstract

  -- Attributes (Decorators)
  highlight(0, '@attribute.typescript',             { fg = colors.pink,      bg = 'NONE' })  -- @decorators

  -- Labels
  highlight(0, '@label.typescript',                 { fg = colors.turquoise, bg = 'NONE' })  -- Labels

  -- Strings
  highlight(0, '@string.typescript',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.typescript',         { link = "String" })  -- \n, \t, etc.
  highlight(0, '@string.regexp.typescript',         { link = "String" })  -- /regex/
  highlight(0, '@string.special.url.typescript',    { link = "String" })  -- URLs in imports

  -- Numbers
  highlight(0, '@number.typescript',                { link = "Number" })  -- Numbers
  highlight(0, '@number.float.typescript',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.typescript',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.typescript',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.typescript', { link = "Comment" })  -- JSDoc comments

  -- Operators and Punctuation
  highlight(0, '@operator.typescript',              { link = "Operator" })  -- +, -, =, etc.
  highlight(0, '@punctuation.bracket.typescript',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.typescript', { link = "Delimiter" })  -- , ; : ?.
  highlight(0, '@punctuation.special.typescript',   { fg = colors.pink,      bg = 'NONE' })  -- ${}, ?, !


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.typescript)

  highlight(0, '@lsp.type.class.typescript',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.interface.typescript',     { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.type.typescript',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.typeParameter.typescript', { fg = colors.turquoise, bg = 'NONE' })  -- Type parameters <T>
  highlight(0, '@lsp.type.enum.typescript',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.typescript',    { fg = colors.pink,      bg = 'NONE' })  -- Enum members
  highlight(0, '@lsp.type.function.typescript',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.typescript',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.variable.typescript',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.typescript',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.typescript',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.namespace.typescript',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.keyword.typescript',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.decorator.typescript',     { fg = colors.pink,      bg = 'NONE' })  -- Decorators
  highlight(0, '@lsp.type.string.typescript',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.typescript',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.regexp.typescript',        { fg = colors.redLight,  bg = 'NONE' })  -- RegExp
  highlight(0, '@lsp.type.comment.typescript',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.typescript',     { link = "Variable" })  -- const variables
  highlight(0, '@lsp.typemod.variable.defaultLibrary.typescript', { link = "Variable" })  -- Built-in variables
  highlight(0, '@lsp.typemod.function.declaration.typescript',  { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.defaultLibrary.typescript', { fg = colors.orange,  bg = 'NONE' })  -- Built-in functions
  highlight(0, '@lsp.typemod.class.declaration.typescript',     { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
  highlight(0, '@lsp.typemod.class.defaultLibrary.typescript',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in classes
  highlight(0, '@lsp.typemod.interface.declaration.typescript', { fg = colors.turquoise, bg = 'NONE' })  -- Interface declarations
  highlight(0, '@lsp.typemod.type.declaration.typescript',      { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.enum.declaration.typescript',      { fg = colors.turquoise, bg = 'NONE' })  -- Enum declarations
  highlight(0, '@lsp.typemod.property.declaration.typescript',  { fg = colors.purple,    bg = 'NONE' })  -- Property declarations
  highlight(0, '@lsp.typemod.parameter.declaration.typescript', { fg = colors.purple,    bg = 'NONE' })  -- Parameter declarations
  highlight(0, '@lsp.typemod.property.readonly.typescript',     { fg = colors.pink,      bg = 'NONE' })  -- Readonly properties
  highlight(0, '@lsp.typemod.enumMember.declaration.typescript', { fg = colors.pink,     bg = 'NONE' })  -- Enum member declarations
end

return typescript


