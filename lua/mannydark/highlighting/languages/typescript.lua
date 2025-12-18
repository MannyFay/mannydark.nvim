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
  highlight(0, 'typescriptVariable',            { fg = colors.blue,       bg = 'NONE'            })  -- let, const, var
  highlight(0, 'typescriptVariableDeclaration', { fg = colors.purple,     bg = 'NONE'            })  -- Variable names

  -- Keywords - Module
  highlight(0, 'typescriptImport',              { fg = colors.blue,       bg = 'NONE'            })  -- import
  highlight(0, 'typescriptExport',              { fg = colors.blue,       bg = 'NONE'            })  -- export
  highlight(0, 'typescriptDefault',             { fg = colors.blue,       bg = 'NONE'            })  -- default
  highlight(0, 'typescriptModule',              { fg = colors.blue,       bg = 'NONE'            })  -- module, namespace
  highlight(0, 'typescriptAmbientDeclaration',  { fg = colors.blue,       bg = 'NONE'            })  -- declare
  highlight(0, 'typescriptImportType',          { fg = colors.blue,       bg = 'NONE'            })  -- import type
  highlight(0, 'typescriptExportType',          { fg = colors.blue,       bg = 'NONE'            })  -- export type
  highlight(0, 'typescriptDefaultImportName',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Imported module name

  -- Keywords - Control Flow
  highlight(0, 'typescriptConditional',         { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch
  highlight(0, 'typescriptConditionalElse',     { fg = colors.blue,       bg = 'NONE'            })  -- else
  highlight(0, 'typescriptRepeat',              { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'typescriptForOperator',         { fg = colors.blue,       bg = 'NONE'            })  -- in, of
  highlight(0, 'typescriptAsyncFor',            { fg = colors.blue,       bg = 'NONE'            })  -- for await
  highlight(0, 'typescriptBranch',              { fg = colors.blue,       bg = 'NONE'            })  -- break, continue
  highlight(0, 'typescriptCase',                { fg = colors.blue,       bg = 'NONE'            })  -- case, default
  highlight(0, 'typescriptStatementKeyword',    { fg = colors.blue,       bg = 'NONE'            })  -- return, yield, throw

  -- Keywords - Exception
  highlight(0, 'typescriptTry',                 { fg = colors.blue,       bg = 'NONE'            })  -- try
  highlight(0, 'typescriptExceptions',          { fg = colors.blue,       bg = 'NONE'            })  -- catch, finally, throw

  -- Keywords - Function
  highlight(0, 'typescriptFuncKeyword',         { fg = colors.blue,       bg = 'NONE'            })  -- function
  highlight(0, 'typescriptAsyncFunc',           { fg = colors.blue,       bg = 'NONE'            })  -- async
  highlight(0, 'typescriptAsyncFuncKeyword',    { fg = colors.blue,       bg = 'NONE'            })  -- async function

  -- Keywords - Class
  highlight(0, 'typescriptClassKeyword',        { fg = colors.blue,       bg = 'NONE'            })  -- class
  highlight(0, 'typescriptClassName',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names
  highlight(0, 'typescriptClassExtends',        { fg = colors.blue,       bg = 'NONE'            })  -- extends
  highlight(0, 'typescriptClassHeritage',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Extended class
  highlight(0, 'typescriptAbstract',            { fg = colors.blue,       bg = 'NONE'            })  -- abstract
  highlight(0, 'typescriptClassStatic',         { fg = colors.blue,       bg = 'NONE'            })  -- static
  highlight(0, 'typescriptConstructor',         { fg = colors.blue,       bg = 'NONE'            })  -- constructor

  -- Keywords - Interface/Type
  highlight(0, 'typescriptInterfaceKeyword',    { fg = colors.blue,       bg = 'NONE'            })  -- interface
  highlight(0, 'typescriptInterfaceName',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Interface names
  highlight(0, 'typescriptInterfaceExtends',    { fg = colors.blue,       bg = 'NONE'            })  -- extends
  highlight(0, 'typescriptInterfaceHeritage',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Extended interface
  highlight(0, 'typescriptAliasKeyword',        { fg = colors.blue,       bg = 'NONE'            })  -- type (alias)
  highlight(0, 'typescriptAliasDeclaration',    { fg = colors.turquoise,  bg = 'NONE'            })  -- Type alias name

  -- Keywords - Enum
  highlight(0, 'typescriptEnumKeyword',         { fg = colors.blue,       bg = 'NONE'            })  -- enum
  highlight(0, 'typescriptEnum',                { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum names

  -- Keywords - Access Modifiers
  highlight(0, 'typescriptAccessibilityModifier', { fg = colors.blue,     bg = 'NONE'            })  -- public, private, protected
  highlight(0, 'typescriptReadonlyModifier',    { fg = colors.blue,       bg = 'NONE'            })  -- readonly

  -- Keywords - Operators
  highlight(0, 'typescriptOperator',            { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'typescriptKeywordOp',           { fg = colors.blue,       bg = 'NONE'            })  -- typeof, keyof, instanceof, new, delete, in, void
  highlight(0, 'typescriptCastKeyword',         { fg = colors.blue,       bg = 'NONE'            })  -- as
  highlight(0, 'typescriptAssertType',          { fg = colors.blue,       bg = 'NONE'            })  -- as, satisfies
  highlight(0, 'typescriptTypeQuery',           { fg = colors.blue,       bg = 'NONE'            })  -- typeof (in type context)
  highlight(0, 'typescriptConstraint',          { fg = colors.blue,       bg = 'NONE'            })  -- extends (in generics)
  highlight(0, 'typescriptMappedIn',            { fg = colors.blue,       bg = 'NONE'            })  -- in (mapped types)

  -- Keywords - Special
  highlight(0, 'typescriptIdentifier',          { fg = colors.blue,       bg = 'NONE'            })  -- this, super
  highlight(0, 'typescriptDebugger',            { fg = colors.blue,       bg = 'NONE'            })  -- debugger

  -- Types - Predefined
  highlight(0, 'typescriptPredefinedType',      { fg = colors.turquoise,  bg = 'NONE'            })  -- string, number, boolean, any, void, never, unknown, object, symbol, bigint
  highlight(0, 'typescriptType',                { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'typescriptTypeReference',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Type references
  highlight(0, 'typescriptUserDefinedType',     { fg = colors.turquoise,  bg = 'NONE'            })  -- User-defined types
  highlight(0, 'typescriptTypeBlock',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Type in import
  highlight(0, 'typescriptTypeParameter',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Generic type parameters <T>

  -- Types - Special
  highlight(0, 'typescriptStringLiteralType',   { fg = colors.redLight,   bg = 'NONE'            })  -- String literal types
  highlight(0, 'typescriptTemplateLiteralType', { fg = colors.redLight,   bg = 'NONE'            })  -- Template literal types
  highlight(0, 'typescriptReadonlyArrayKeyword', { fg = colors.blue,      bg = 'NONE'            })  -- ReadonlyArray
  highlight(0, 'typescriptFuncType',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Function types
  highlight(0, 'typescriptFuncTypeArrow',       { fg = colors.white,      bg = 'NONE'            })  -- => in function types
  highlight(0, 'typescriptConstructorType',     { fg = colors.turquoise,  bg = 'NONE'            })  -- new () => T
  highlight(0, 'typescriptUnion',               { fg = colors.white,      bg = 'NONE'            })  -- | union
  highlight(0, 'typescriptTypeOperator',        { fg = colors.blue,       bg = 'NONE'            })  -- keyof, typeof, infer

  -- Functions
  highlight(0, 'typescriptFuncName',            { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'typescriptFuncArg',             { fg = colors.purple,     bg = 'NONE'            })  -- Function arguments
  highlight(0, 'typescriptArrowFuncArg',        { fg = colors.purple,     bg = 'NONE'            })  -- Arrow function args
  highlight(0, 'typescriptArrowFunc',           { fg = colors.white,      bg = 'NONE'            })  -- =>
  highlight(0, 'typescriptCall',                { fg = colors.purple,     bg = 'NONE'            })  -- Call expressions
  highlight(0, 'typescriptParamImpl',           { fg = colors.purple,     bg = 'NONE'            })  -- Parameter implementation

  -- Members
  highlight(0, 'typescriptMember',              { fg = colors.orange,     bg = 'NONE'            })  -- Class/interface members
  highlight(0, 'typescriptMethodAccessor',      { fg = colors.blue,       bg = 'NONE'            })  -- get, set
  highlight(0, 'typescriptMemberOptionality',   { fg = colors.pink,       bg = 'NONE'            })  -- ? optional member

  -- Properties
  highlight(0, 'typescriptProp',                { fg = colors.purple,     bg = 'NONE'            })  -- Properties
  highlight(0, 'typescriptProperty',            { fg = colors.purple,     bg = 'NONE'            })  -- Properties
  highlight(0, 'typescriptStringProperty',      { fg = colors.redLight,   bg = 'NONE'            })  -- String properties
  highlight(0, 'typescriptComputedPropertyName', { fg = colors.purple,    bg = 'NONE'            })  -- [computed] properties

  -- Global Objects
  highlight(0, 'typescriptGlobal',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Global objects
  highlight(0, 'typescriptGlobalMethod',        { fg = colors.orange,     bg = 'NONE'            })  -- Global methods

  -- Built-in Object Methods
  highlight(0, 'typescriptObjectStaticMethod',  { fg = colors.orange,     bg = 'NONE'            })  -- Object.keys, etc.
  highlight(0, 'typescriptObjectMethod',        { fg = colors.orange,     bg = 'NONE'            })  -- Object methods
  highlight(0, 'typescriptArrayStaticMethod',   { fg = colors.orange,     bg = 'NONE'            })  -- Array.from, etc.
  highlight(0, 'typescriptArrayMethod',         { fg = colors.orange,     bg = 'NONE'            })  -- Array methods
  highlight(0, 'typescriptStringStaticMethod',  { fg = colors.orange,     bg = 'NONE'            })  -- String methods
  highlight(0, 'typescriptStringMethod',        { fg = colors.orange,     bg = 'NONE'            })  -- String methods
  highlight(0, 'typescriptNumberStaticMethod',  { fg = colors.orange,     bg = 'NONE'            })  -- Number methods
  highlight(0, 'typescriptNumberMethod',        { fg = colors.orange,     bg = 'NONE'            })  -- Number methods
  highlight(0, 'typescriptMathStaticMethod',    { fg = colors.orange,     bg = 'NONE'            })  -- Math methods
  highlight(0, 'typescriptDateStaticMethod',    { fg = colors.orange,     bg = 'NONE'            })  -- Date methods
  highlight(0, 'typescriptDateMethod',          { fg = colors.orange,     bg = 'NONE'            })  -- Date methods
  highlight(0, 'typescriptJSONStaticMethod',    { fg = colors.orange,     bg = 'NONE'            })  -- JSON methods
  highlight(0, 'typescriptRegExpMethod',        { fg = colors.orange,     bg = 'NONE'            })  -- RegExp methods
  highlight(0, 'typescriptPromiseStaticMethod', { fg = colors.orange,     bg = 'NONE'            })  -- Promise methods
  highlight(0, 'typescriptPromiseMethod',       { fg = colors.orange,     bg = 'NONE'            })  -- Promise methods
  highlight(0, 'typescriptReflectMethod',       { fg = colors.orange,     bg = 'NONE'            })  -- Reflect methods
  highlight(0, 'typescriptSymbolStaticMethod',  { fg = colors.orange,     bg = 'NONE'            })  -- Symbol methods
  highlight(0, 'typescriptFunctionMethod',      { fg = colors.orange,     bg = 'NONE'            })  -- Function methods
  highlight(0, 'typescriptES6MapMethod',        { fg = colors.orange,     bg = 'NONE'            })  -- Map methods
  highlight(0, 'typescriptES6SetMethod',        { fg = colors.orange,     bg = 'NONE'            })  -- Set methods
  highlight(0, 'typescriptProxyAPI',            { fg = colors.orange,     bg = 'NONE'            })  -- Proxy methods
  highlight(0, 'typescriptIntlMethod',          { fg = colors.orange,     bg = 'NONE'            })  -- Intl methods

  -- Static Properties
  highlight(0, 'typescriptMathStaticProp',      { fg = colors.pink,       bg = 'NONE'            })  -- Math.PI, etc.
  highlight(0, 'typescriptNumberStaticProp',    { fg = colors.pink,       bg = 'NONE'            })  -- Number.MAX_VALUE, etc.
  highlight(0, 'typescriptSymbolStaticProp',    { fg = colors.pink,       bg = 'NONE'            })  -- Symbol.iterator, etc.
  highlight(0, 'typescriptRegExpStaticProp',    { fg = colors.pink,       bg = 'NONE'            })  -- RegExp properties
  highlight(0, 'typescriptRegExpProp',          { fg = colors.pink,       bg = 'NONE'            })  -- RegExp properties
  highlight(0, 'typescriptES6MapProp',          { fg = colors.pink,       bg = 'NONE'            })  -- Map.size
  highlight(0, 'typescriptES6SetProp',          { fg = colors.pink,       bg = 'NONE'            })  -- Set.size

  -- DOM/BOM
  highlight(0, 'typescriptBOM',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- window, document
  highlight(0, 'typescriptBOMWindowProp',       { fg = colors.purple,     bg = 'NONE'            })  -- Window properties
  highlight(0, 'typescriptBOMWindowMethod',     { fg = colors.orange,     bg = 'NONE'            })  -- Window methods
  highlight(0, 'typescriptBOMWindowCons',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Window constructors
  highlight(0, 'typescriptBOMNavigatorProp',    { fg = colors.purple,     bg = 'NONE'            })  -- Navigator properties
  highlight(0, 'typescriptBOMNavigatorMethod',  { fg = colors.orange,     bg = 'NONE'            })  -- Navigator methods
  highlight(0, 'typescriptBOMLocationProp',     { fg = colors.purple,     bg = 'NONE'            })  -- Location properties
  highlight(0, 'typescriptBOMLocationMethod',   { fg = colors.orange,     bg = 'NONE'            })  -- Location methods
  highlight(0, 'typescriptBOMHistoryProp',      { fg = colors.purple,     bg = 'NONE'            })  -- History properties
  highlight(0, 'typescriptBOMHistoryMethod',    { fg = colors.orange,     bg = 'NONE'            })  -- History methods
  highlight(0, 'typescriptConsoleMethod',       { fg = colors.orange,     bg = 'NONE'            })  -- console methods
  highlight(0, 'typescriptDOMNodeProp',         { fg = colors.purple,     bg = 'NONE'            })  -- DOM properties
  highlight(0, 'typescriptDOMNodeMethod',       { fg = colors.orange,     bg = 'NONE'            })  -- DOM methods
  highlight(0, 'typescriptDOMNodeType',         { fg = colors.turquoise,  bg = 'NONE'            })  -- DOM types
  highlight(0, 'typescriptDOMDocProp',          { fg = colors.purple,     bg = 'NONE'            })  -- Document properties
  highlight(0, 'typescriptDOMDocMethod',        { fg = colors.orange,     bg = 'NONE'            })  -- Document methods
  highlight(0, 'typescriptDOMElemAttrs',        { fg = colors.purple,     bg = 'NONE'            })  -- Element attributes
  highlight(0, 'typescriptDOMElemFuncs',        { fg = colors.orange,     bg = 'NONE'            })  -- Element functions
  highlight(0, 'typescriptDOMEventTargetMethod', { fg = colors.orange,    bg = 'NONE'            })  -- Event target methods
  highlight(0, 'typescriptDOMEventCons',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Event constructors
  highlight(0, 'typescriptDOMEventProp',        { fg = colors.purple,     bg = 'NONE'            })  -- Event properties
  highlight(0, 'typescriptDOMEventMethod',      { fg = colors.orange,     bg = 'NONE'            })  -- Event methods
  highlight(0, 'typescriptDOMStorage',          { fg = colors.turquoise,  bg = 'NONE'            })  -- localStorage, sessionStorage
  highlight(0, 'typescriptDOMStorageProp',      { fg = colors.purple,     bg = 'NONE'            })  -- Storage properties
  highlight(0, 'typescriptDOMStorageMethod',    { fg = colors.orange,     bg = 'NONE'            })  -- Storage methods
  highlight(0, 'typescriptDOMFormProp',         { fg = colors.purple,     bg = 'NONE'            })  -- Form properties
  highlight(0, 'typescriptDOMFormMethod',       { fg = colors.orange,     bg = 'NONE'            })  -- Form methods
  highlight(0, 'typescriptDOMStyle',            { fg = colors.purple,     bg = 'NONE'            })  -- style property

  -- Fetch/XHR/Network
  highlight(0, 'typescriptXHRGlobal',           { fg = colors.turquoise,  bg = 'NONE'            })  -- XMLHttpRequest
  highlight(0, 'typescriptXHRProp',             { fg = colors.purple,     bg = 'NONE'            })  -- XHR properties
  highlight(0, 'typescriptXHRMethod',           { fg = colors.orange,     bg = 'NONE'            })  -- XHR methods
  highlight(0, 'typescriptHeadersMethod',       { fg = colors.orange,     bg = 'NONE'            })  -- Headers methods
  highlight(0, 'typescriptRequestProp',         { fg = colors.purple,     bg = 'NONE'            })  -- Request properties
  highlight(0, 'typescriptRequestMethod',       { fg = colors.orange,     bg = 'NONE'            })  -- Request methods
  highlight(0, 'typescriptResponseProp',        { fg = colors.purple,     bg = 'NONE'            })  -- Response properties
  highlight(0, 'typescriptResponseMethod',      { fg = colors.orange,     bg = 'NONE'            })  -- Response methods
  highlight(0, 'typescriptBOMNetworkProp',      { fg = colors.purple,     bg = 'NONE'            })  -- Network properties

  -- Other APIs
  highlight(0, 'typescriptCryptoGlobal',        { fg = colors.turquoise,  bg = 'NONE'            })  -- crypto
  highlight(0, 'typescriptCryptoProp',          { fg = colors.purple,     bg = 'NONE'            })  -- Crypto properties
  highlight(0, 'typescriptCryptoMethod',        { fg = colors.orange,     bg = 'NONE'            })  -- Crypto methods
  highlight(0, 'typescriptSubtleCryptoMethod',  { fg = colors.orange,     bg = 'NONE'            })  -- SubtleCrypto methods
  highlight(0, 'typescriptEncodingGlobal',      { fg = colors.turquoise,  bg = 'NONE'            })  -- TextEncoder, etc.
  highlight(0, 'typescriptEncodingProp',        { fg = colors.purple,     bg = 'NONE'            })  -- Encoding properties
  highlight(0, 'typescriptEncodingMethod',      { fg = colors.orange,     bg = 'NONE'            })  -- Encoding methods
  highlight(0, 'typescriptFileMethod',          { fg = colors.orange,     bg = 'NONE'            })  -- File methods
  highlight(0, 'typescriptFileReaderProp',      { fg = colors.purple,     bg = 'NONE'            })  -- FileReader properties
  highlight(0, 'typescriptFileReaderMethod',    { fg = colors.orange,     bg = 'NONE'            })  -- FileReader methods
  highlight(0, 'typescriptFileListMethod',      { fg = colors.orange,     bg = 'NONE'            })  -- FileList methods
  highlight(0, 'typescriptBlobMethod',          { fg = colors.orange,     bg = 'NONE'            })  -- Blob methods
  highlight(0, 'typescriptURLUtilsProp',        { fg = colors.purple,     bg = 'NONE'            })  -- URL utilities
  highlight(0, 'typescriptURLStaticMethod',     { fg = colors.orange,     bg = 'NONE'            })  -- URL static methods
  highlight(0, 'typescriptGeolocationMethod',   { fg = colors.orange,     bg = 'NONE'            })  -- Geolocation methods
  highlight(0, 'typescriptServiceWorkerProp',   { fg = colors.purple,     bg = 'NONE'            })  -- SW properties
  highlight(0, 'typescriptServiceWorkerMethod', { fg = colors.orange,     bg = 'NONE'            })  -- SW methods
  highlight(0, 'typescriptCacheMethod',         { fg = colors.orange,     bg = 'NONE'            })  -- Cache methods

  -- Node.js
  highlight(0, 'typescriptNodeGlobal',          { fg = colors.turquoise,  bg = 'NONE'            })  -- process, Buffer, etc.
  highlight(0, 'typescriptTestGlobal',          { fg = colors.orange,     bg = 'NONE'            })  -- describe, it, etc.

  -- Literals
  highlight(0, 'typescriptBoolean',             { fg = colors.blue,       bg = 'NONE'            })  -- true, false
  highlight(0, 'typescriptNull',                { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'typescriptNumber',              { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'typescriptString',              { fg = colors.redLight,   bg = 'NONE'            })  -- Strings

  -- Template Strings
  highlight(0, 'typescriptTemplate',            { fg = colors.redLight,   bg = 'NONE'            })  -- Template literals
  highlight(0, 'typescriptTemplateSB',          { fg = colors.pink,       bg = 'NONE'            })  -- ${} in templates
  highlight(0, 'typescriptTemplateSubstitution', { fg = 'NONE',           bg = 'NONE'            })  -- Expression in ${}

  -- Regular Expressions
  highlight(0, 'typescriptRegexpString',        { fg = colors.redLight,   bg = 'NONE'            })  -- /regex/
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
  highlight(0, 'typescriptTypeBrackets',        { fg = colors.white,      bg = 'NONE'            })  -- <> in generics
  highlight(0, 'typescriptTypeBracket',         { fg = colors.white,      bg = 'NONE'            })  -- Brackets
  highlight(0, 'typescriptEndColons',           { fg = colors.white,      bg = 'NONE'            })  -- Semicolons
  highlight(0, 'typescriptObjectColon',         { fg = colors.white,      bg = 'NONE'            })  -- : in objects
  highlight(0, 'typescriptFuncComma',           { fg = colors.white,      bg = 'NONE'            })  -- , in functions
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
  highlight(0, 'typescriptDestructureVariable', { fg = colors.purple,     bg = 'NONE'            })  -- Destructure variable
  highlight(0, 'typescriptDestructureAs',       { fg = colors.blue,       bg = 'NONE'            })  -- as in destructure
  highlight(0, 'typescriptDestructureString',   { fg = colors.redLight,   bg = 'NONE'            })  -- String in destructure
  highlight(0, 'typescriptDestructureComma',    { fg = colors.white,      bg = 'NONE'            })  -- , in destructure
  highlight(0, 'typescriptDefaultParam',        { fg = colors.purple,     bg = 'NONE'            })  -- Default parameters

  -- Ternary
  highlight(0, 'typescriptTernary',             { fg = colors.white,      bg = 'NONE'            })  -- Ternary expression
  highlight(0, 'typescriptTernaryOp',           { fg = colors.white,      bg = 'NONE'            })  -- ? : operators

  -- Blocks (cleared)
  highlight(0, 'typescriptBlock',               { fg = 'NONE',            bg = 'NONE'            })  -- Generic block
  highlight(0, 'typescriptClassBlock',          { fg = 'NONE',            bg = 'NONE'            })  -- Class body
  highlight(0, 'typescriptObjectLiteral',       { fg = 'NONE',            bg = 'NONE'            })  -- Object literal
  highlight(0, 'typescriptFuncCallArg',         { fg = 'NONE',            bg = 'NONE'            })  -- Function call args
  highlight(0, 'typescriptConditionalParen',    { fg = 'NONE',            bg = 'NONE'            })  -- Conditional paren
  highlight(0, 'typescriptLoopParen',           { fg = 'NONE',            bg = 'NONE'            })  -- Loop paren
  highlight(0, 'typescriptParenExp',            { fg = 'NONE',            bg = 'NONE'            })  -- Paren expression
  highlight(0, 'typescriptIndexExpr',           { fg = 'NONE',            bg = 'NONE'            })  -- Index expression
  highlight(0, 'typescriptTypeAnnotation',      { fg = 'NONE',            bg = 'NONE'            })  -- Type annotation
  highlight(0, 'typescriptAssign',              { fg = 'NONE',            bg = 'NONE'            })  -- Assignment
  highlight(0, 'typescriptGenericImpl',         { fg = 'NONE',            bg = 'NONE'            })  -- Generic implementation
  highlight(0, 'typescriptGenericFunc',         { fg = 'NONE',            bg = 'NONE'            })  -- Generic function
  highlight(0, 'typescriptGenericCall',         { fg = 'NONE',            bg = 'NONE'            })  -- Generic call
  highlight(0, 'typescriptGenericDefault',      { fg = 'NONE',            bg = 'NONE'            })  -- Generic default
  highlight(0, 'typescriptTypeArguments',       { fg = 'NONE',            bg = 'NONE'            })  -- Type arguments
  highlight(0, 'typescriptTypeParameters',      { fg = 'NONE',            bg = 'NONE'            })  -- Type parameters
  highlight(0, 'typescriptArrowFuncDef',        { fg = 'NONE',            bg = 'NONE'            })  -- Arrow func definition
  highlight(0, 'typescriptArrowFuncTypeParameter', { fg = 'NONE',         bg = 'NONE'            })  -- Arrow func type param
  highlight(0, 'typescriptReturnAnnotation',    { fg = 'NONE',            bg = 'NONE'            })  -- Return annotation
  highlight(0, 'typescriptFuncImpl',            { fg = 'NONE',            bg = 'NONE'            })  -- Function implementation
  highlight(0, 'typescriptFunctionType',        { fg = 'NONE',            bg = 'NONE'            })  -- Function type
  highlight(0, 'typescriptTypeCast',            { fg = 'NONE',            bg = 'NONE'            })  -- Type cast
  highlight(0, 'typescriptConditionalType',     { fg = 'NONE',            bg = 'NONE'            })  -- Conditional type
  highlight(0, 'typescriptParenthesizedType',   { fg = 'NONE',            bg = 'NONE'            })  -- Parenthesized type
  highlight(0, 'typescriptObjectType',          { fg = 'NONE',            bg = 'NONE'            })  -- Object type
  highlight(0, 'typescriptTupleType',           { fg = 'NONE',            bg = 'NONE'            })  -- Tuple type
  highlight(0, 'typescriptIndexSignature',      { fg = 'NONE',            bg = 'NONE'            })  -- Index signature
  highlight(0, 'typescriptConstructSignature',  { fg = 'NONE',            bg = 'NONE'            })  -- Construct signature
  highlight(0, 'typescriptInterfaceTypeParameter', { fg = 'NONE',         bg = 'NONE'            })  -- Interface type param
  highlight(0, 'typescriptInterfaceTypeArguments', { fg = 'NONE',         bg = 'NONE'            })  -- Interface type args
  highlight(0, 'typescriptInterfaceComma',      { fg = colors.white,      bg = 'NONE'            })  -- , in interface
  highlight(0, 'typescriptClassTypeParameter',  { fg = 'NONE',            bg = 'NONE'            })  -- Class type param
  highlight(0, 'typescriptClassTypeArguments',  { fg = 'NONE',            bg = 'NONE'            })  -- Class type args
  highlight(0, 'typescriptMixinComma',          { fg = colors.white,      bg = 'NONE'            })  -- , in mixin

  -- Comments
  highlight(0, 'typescriptComment',             { fg = colors.red,        bg = 'NONE'            })  -- /* */
  highlight(0, 'typescriptLineComment',         { fg = colors.red,        bg = 'NONE'            })  -- //
  highlight(0, 'typescriptCommentTodo',         { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME
  highlight(0, 'typescriptMagicComment',        { fg = colors.red,        bg = 'NONE'            })  -- @ts-ignore, etc.
  highlight(0, 'typescriptRef',                 { fg = colors.red,        bg = 'NONE'            })  -- /// <reference>

  -- JSDoc
  highlight(0, 'typescriptDocComment',          { fg = colors.red,        bg = 'NONE'            })  -- /** */
  highlight(0, 'typescriptDocTags',             { fg = colors.pink,       bg = 'NONE'            })  -- @param, @returns
  highlight(0, 'typescriptDocNotation',         { fg = colors.pink,       bg = 'NONE'            })  -- @
  highlight(0, 'typescriptDocParam',            { fg = colors.purple,     bg = 'NONE'            })  -- Param name
  highlight(0, 'typescriptDocParamName',        { fg = colors.purple,     bg = 'NONE'            })  -- Param name
  highlight(0, 'typescriptDocParamType',        { fg = colors.turquoise,  bg = 'NONE'            })  -- {Type}
  highlight(0, 'typescriptDocNamedParamType',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Named param type
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
  highlight(0, 'typescriptEventString',         { fg = colors.redLight,   bg = 'NONE'            })  -- Event string

  -- Payment API
  highlight(0, 'typescriptPaymentMethod',       { fg = colors.orange,     bg = 'NONE'            })  -- Payment methods
  highlight(0, 'typescriptPaymentProp',         { fg = colors.purple,     bg = 'NONE'            })  -- Payment properties
  highlight(0, 'typescriptPaymentResponseMethod', { fg = colors.orange,   bg = 'NONE'            })  -- Response methods
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
  highlight(0, '@variable.typescript',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.typescript',      { fg = colors.pink,      bg = 'NONE' })  -- this, super
  highlight(0, '@variable.member.typescript',       { fg = colors.purple,    bg = 'NONE' })  -- Object members
  highlight(0, '@variable.parameter.typescript',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters

  -- Constants
  highlight(0, '@constant.typescript',              { fg = colors.pink,      bg = 'NONE' })  -- UPPER_CASE constants
  highlight(0, '@constant.builtin.typescript',      { fg = colors.blue,      bg = 'NONE' })  -- true, false, null, undefined

  -- Modules
  highlight(0, '@module.typescript',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names

  -- Types
  highlight(0, '@type.typescript',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.typescript',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.typescript',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier.typescript',        { fg = colors.blue,      bg = 'NONE' })  -- Type qualifiers

  -- Functions
  highlight(0, '@function.typescript',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.typescript',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.method.typescript',       { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@function.method.call.typescript',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.builtin.typescript',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@constructor.typescript',           { fg = colors.turquoise, bg = 'NONE' })  -- Constructor calls

  -- Keywords
  highlight(0, '@keyword.typescript',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.typescript',      { fg = colors.blue,      bg = 'NONE' })  -- function, async
  highlight(0, '@keyword.operator.typescript',      { fg = colors.blue,      bg = 'NONE' })  -- typeof, keyof, instanceof, as, satisfies
  highlight(0, '@keyword.return.typescript',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.conditional.typescript',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, switch
  highlight(0, '@keyword.conditional.ternary.typescript', { fg = colors.white, bg = 'NONE' })  -- ? :
  highlight(0, '@keyword.repeat.typescript',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, do
  highlight(0, '@keyword.exception.typescript',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, finally, throw
  highlight(0, '@keyword.import.typescript',        { fg = colors.blue,      bg = 'NONE' })  -- import, export, from, require
  highlight(0, '@keyword.coroutine.typescript',     { fg = colors.blue,      bg = 'NONE' })  -- async, await, yield
  highlight(0, '@keyword.type.typescript',          { fg = colors.blue,      bg = 'NONE' })  -- type, interface, enum, namespace
  highlight(0, '@keyword.modifier.typescript',      { fg = colors.blue,      bg = 'NONE' })  -- public, private, protected, readonly, abstract

  -- Attributes (Decorators)
  highlight(0, '@attribute.typescript',             { fg = colors.pink,      bg = 'NONE' })  -- @decorators

  -- Labels
  highlight(0, '@label.typescript',                 { fg = colors.turquoise, bg = 'NONE' })  -- Labels

  -- Strings
  highlight(0, '@string.typescript',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.typescript',         { fg = colors.pink,      bg = 'NONE' })  -- \n, \t, etc.
  highlight(0, '@string.regexp.typescript',         { fg = colors.redLight,  bg = 'NONE' })  -- /regex/
  highlight(0, '@string.special.url.typescript',    { fg = colors.redLight,  bg = 'NONE' })  -- URLs in imports

  -- Numbers
  highlight(0, '@number.typescript',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.float.typescript',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.typescript',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.typescript',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.typescript', { fg = colors.red,       bg = 'NONE' })  -- JSDoc comments

  -- Operators and Punctuation
  highlight(0, '@operator.typescript',              { fg = colors.white,     bg = 'NONE' })  -- +, -, =, etc.
  highlight(0, '@punctuation.bracket.typescript',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.typescript', { fg = colors.white,     bg = 'NONE' })  -- , ; : ?.
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
  highlight(0, '@lsp.type.variable.typescript',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.typescript',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.typescript',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.namespace.typescript',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.keyword.typescript',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.decorator.typescript',     { fg = colors.pink,      bg = 'NONE' })  -- Decorators
  highlight(0, '@lsp.type.string.typescript',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.typescript',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.regexp.typescript',        { fg = colors.redLight,  bg = 'NONE' })  -- RegExp
  highlight(0, '@lsp.type.comment.typescript',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.typescript',     { fg = colors.pink,      bg = 'NONE' })  -- const variables
  highlight(0, '@lsp.typemod.variable.defaultLibrary.typescript', { fg = colors.pink,    bg = 'NONE' })  -- Built-in variables
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


