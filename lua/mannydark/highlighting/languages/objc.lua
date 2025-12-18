-------------------------------------------------------------------------------
-- Objective-C Files
-- Highlighting for .m, .mm, .h files (Objective-C and Objective-C++).
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local objc    = {}


-------------------------------------------------------------------------------
-- Settings

objc.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- @ Compiler Directives - Class/Interface
  highlight(0, 'objcDirective',         { fg = colors.pink,       bg = 'NONE'            })  -- General @ directives
  highlight(0, 'objcInterface',         { fg = colors.pink,       bg = 'NONE'            })  -- @interface
  highlight(0, 'objcImplementation',    { fg = colors.pink,       bg = 'NONE'            })  -- @implementation
  highlight(0, 'objcEnd',               { fg = colors.pink,       bg = 'NONE'            })  -- @end
  highlight(0, 'objcClass',             { fg = colors.pink,       bg = 'NONE'            })  -- @class (forward declaration)

  -- @ Compiler Directives - Protocol
  highlight(0, 'objcProtocol',          { fg = colors.pink,       bg = 'NONE'            })  -- @protocol
  highlight(0, 'objcRequired',          { fg = colors.pink,       bg = 'NONE'            })  -- @required
  highlight(0, 'objcOptional',          { fg = colors.pink,       bg = 'NONE'            })  -- @optional

  -- @ Compiler Directives - Property
  highlight(0, 'objcProperty',          { fg = colors.pink,       bg = 'NONE'            })  -- @property
  highlight(0, 'objcSynthesize',        { fg = colors.pink,       bg = 'NONE'            })  -- @synthesize
  highlight(0, 'objcDynamic',           { fg = colors.pink,       bg = 'NONE'            })  -- @dynamic

  -- @ Compiler Directives - Visibility
  highlight(0, 'objcPublic',            { fg = colors.pink,       bg = 'NONE'            })  -- @public
  highlight(0, 'objcPrivate',           { fg = colors.pink,       bg = 'NONE'            })  -- @private
  highlight(0, 'objcProtected',         { fg = colors.pink,       bg = 'NONE'            })  -- @protected
  highlight(0, 'objcPackage',           { fg = colors.pink,       bg = 'NONE'            })  -- @package

  -- @ Compiler Directives - Exception Handling
  highlight(0, 'objcTry',               { fg = colors.pink,       bg = 'NONE'            })  -- @try
  highlight(0, 'objcCatch',             { fg = colors.pink,       bg = 'NONE'            })  -- @catch
  highlight(0, 'objcFinally',           { fg = colors.pink,       bg = 'NONE'            })  -- @finally
  highlight(0, 'objcThrow',             { fg = colors.pink,       bg = 'NONE'            })  -- @throw

  -- @ Compiler Directives - Memory/Threading
  highlight(0, 'objcAutoreleasepool',   { fg = colors.pink,       bg = 'NONE'            })  -- @autoreleasepool
  highlight(0, 'objcSynchronized',      { fg = colors.pink,       bg = 'NONE'            })  -- @synchronized

  -- @ Compiler Directives - Other
  highlight(0, 'objcSelector',          { fg = colors.pink,       bg = 'NONE'            })  -- @selector()
  highlight(0, 'objcEncode',            { fg = colors.pink,       bg = 'NONE'            })  -- @encode()
  highlight(0, 'objcCompatibilityAlias',{ fg = colors.pink,       bg = 'NONE'            })  -- @compatibility_alias
  highlight(0, 'objcDefs',              { fg = colors.pink,       bg = 'NONE'            })  -- @defs()
  highlight(0, 'objcAvailable',         { fg = colors.pink,       bg = 'NONE'            })  -- @available()

  -- Property Attributes - Memory Management
  highlight(0, 'objcPropertyAttr',      { fg = colors.pink,       bg = 'NONE'            })  -- Property attributes
  highlight(0, 'objcStrong',            { fg = colors.pink,       bg = 'NONE'            })  -- strong
  highlight(0, 'objcWeak',              { fg = colors.pink,       bg = 'NONE'            })  -- weak
  highlight(0, 'objcCopy',              { fg = colors.pink,       bg = 'NONE'            })  -- copy
  highlight(0, 'objcAssign',            { fg = colors.pink,       bg = 'NONE'            })  -- assign
  highlight(0, 'objcRetain',            { fg = colors.pink,       bg = 'NONE'            })  -- retain
  highlight(0, 'objcUnsafeUnretained',  { fg = colors.pink,       bg = 'NONE'            })  -- unsafe_unretained

  -- Property Attributes - Thread Safety
  highlight(0, 'objcAtomic',            { fg = colors.pink,       bg = 'NONE'            })  -- atomic
  highlight(0, 'objcNonatomic',         { fg = colors.pink,       bg = 'NONE'            })  -- nonatomic

  -- Property Attributes - Access
  highlight(0, 'objcReadonly',          { fg = colors.pink,       bg = 'NONE'            })  -- readonly
  highlight(0, 'objcReadwrite',         { fg = colors.pink,       bg = 'NONE'            })  -- readwrite
  highlight(0, 'objcGetter',            { fg = colors.pink,       bg = 'NONE'            })  -- getter=
  highlight(0, 'objcSetter',            { fg = colors.pink,       bg = 'NONE'            })  -- setter=

  -- Property Attributes - Nullability
  highlight(0, 'objcNullable',          { fg = colors.pink,       bg = 'NONE'            })  -- nullable
  highlight(0, 'objcNonnull',           { fg = colors.pink,       bg = 'NONE'            })  -- nonnull
  highlight(0, 'objcNullUnspecified',   { fg = colors.pink,       bg = 'NONE'            })  -- null_unspecified
  highlight(0, 'objcNullResettable',    { fg = colors.pink,       bg = 'NONE'            })  -- null_resettable

  -- Property Attributes - Class
  highlight(0, 'objcClassAttr',         { fg = colors.pink,       bg = 'NONE'            })  -- class (for class properties)

  -- Keywords (C inherited)
  highlight(0, 'objcKeyword',           { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'objcStatement',         { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, goto
  highlight(0, 'objcConditional',       { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch
  highlight(0, 'objcRepeat',            { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'objcLabel',             { fg = colors.blue,       bg = 'NONE'            })  -- case, default
  highlight(0, 'objcStorageClass',      { fg = colors.blue,       bg = 'NONE'            })  -- static, extern, register, auto
  highlight(0, 'objcStructure',         { fg = colors.blue,       bg = 'NONE'            })  -- struct, union, enum, typedef

  -- Objective-C Specific Keywords
  highlight(0, 'objcSelf',              { fg = colors.blue,       bg = 'NONE'            })  -- self
  highlight(0, 'objcSuper',             { fg = colors.blue,       bg = 'NONE'            })  -- super
  highlight(0, 'objcIn',                { fg = colors.blue,       bg = 'NONE'            })  -- in (for fast enumeration)
  highlight(0, 'objcOut',               { fg = colors.blue,       bg = 'NONE'            })  -- out
  highlight(0, 'objcInout',             { fg = colors.blue,       bg = 'NONE'            })  -- inout
  highlight(0, 'objcBycopy',            { fg = colors.blue,       bg = 'NONE'            })  -- bycopy
  highlight(0, 'objcByref',             { fg = colors.blue,       bg = 'NONE'            })  -- byref
  highlight(0, 'objcOneway',            { fg = colors.blue,       bg = 'NONE'            })  -- oneway

  -- Block Keywords
  highlight(0, 'objcBlock',             { fg = colors.blue,       bg = 'NONE'            })  -- __block
  highlight(0, 'objcBlockCaret',        { fg = colors.white,      bg = 'NONE'            })  -- ^ (block start)

  -- Types - Objective-C Specific
  highlight(0, 'objcType',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'objcIdType',            { fg = colors.turquoise,  bg = 'NONE'            })  -- id
  highlight(0, 'objcClassType',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Class
  highlight(0, 'objcSELType',           { fg = colors.turquoise,  bg = 'NONE'            })  -- SEL
  highlight(0, 'objcIMPType',           { fg = colors.turquoise,  bg = 'NONE'            })  -- IMP
  highlight(0, 'objcBOOLType',          { fg = colors.turquoise,  bg = 'NONE'            })  -- BOOL
  highlight(0, 'objcInstancetype',      { fg = colors.turquoise,  bg = 'NONE'            })  -- instancetype

  -- Types - C Inherited
  highlight(0, 'objcBuiltinType',       { fg = colors.turquoise,  bg = 'NONE'            })  -- int, char, float, double, void, etc.
  highlight(0, 'objcSizeType',          { fg = colors.turquoise,  bg = 'NONE'            })  -- NSInteger, NSUInteger, CGFloat

  -- Types - Foundation Classes
  highlight(0, 'objcFoundationType',    { fg = colors.turquoise,  bg = 'NONE'            })  -- NSObject, NSString, NSArray, etc.
  highlight(0, 'objcProtocolType',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Protocol types <NSCopying>

  -- Functions/Methods
  highlight(0, 'objcMethodName',        { fg = colors.orange,     bg = 'NONE'            })  -- Method names
  highlight(0, 'objcMethodCall',        { fg = colors.orange,     bg = 'NONE'            })  -- Method calls [obj method]
  highlight(0, 'objcFunction',          { fg = colors.orange,     bg = 'NONE'            })  -- C functions
  highlight(0, 'objcBuiltinFunc',       { fg = colors.orange,     bg = 'NONE'            })  -- NSLog, etc.
  highlight(0, 'objcInstanceMethod',    { fg = colors.orange,     bg = 'NONE'            })  -- - instance methods
  highlight(0, 'objcClassMethod',       { fg = colors.orange,     bg = 'NONE'            })  -- + class methods
  highlight(0, 'objcMethodIndicator',   { fg = colors.white,      bg = 'NONE'            })  -- - + method indicators

  -- Variables
  highlight(0, 'objcIdentifier',        { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'objcParameter',         { fg = colors.purple,     bg = 'NONE'            })  -- Method parameters
  highlight(0, 'objcIvar',              { fg = colors.purple,     bg = 'NONE'            })  -- Instance variables
  highlight(0, 'objcPropertyName',      { fg = colors.purple,     bg = 'NONE'            })  -- Property names

  -- Constants
  highlight(0, 'objcConstant',          { fg = colors.purple,     bg = 'NONE'            })  -- Constants
  highlight(0, 'objcBoolean',           { fg = colors.blue,       bg = 'NONE'            })  -- YES, NO
  highlight(0, 'objcNil',               { fg = colors.blue,       bg = 'NONE'            })  -- nil, Nil, NULL
  highlight(0, 'objcEnumConstant',      { fg = colors.purple,     bg = 'NONE'            })  -- Enum values

  -- Literals - Strings
  highlight(0, 'objcString',            { fg = colors.redLight,   bg = 'NONE'            })  -- @"strings"
  highlight(0, 'objcCString',           { fg = colors.redLight,   bg = 'NONE'            })  -- "C strings"
  highlight(0, 'objcCharacter',         { fg = colors.redLight,   bg = 'NONE'            })  -- 'c'
  highlight(0, 'objcStringEscape',      { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'objcFormat',            { fg = colors.pink,       bg = 'NONE'            })  -- %@, %d, %s, etc.

  -- Literals - Numbers
  highlight(0, 'objcNumber',            { fg = colors.greenLight, bg = 'NONE'            })  -- @42, numbers
  highlight(0, 'objcInteger',           { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'objcFloat',             { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'objcHex',               { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF hex
  highlight(0, 'objcOctal',             { fg = colors.greenLight, bg = 'NONE'            })  -- 0777 octal

  -- Literals - Collections
  highlight(0, 'objcArrayLiteral',      { fg = colors.redLight,   bg = 'NONE'            })  -- @[]
  highlight(0, 'objcDictLiteral',       { fg = colors.redLight,   bg = 'NONE'            })  -- @{}
  highlight(0, 'objcBoxedExpr',         { fg = colors.redLight,   bg = 'NONE'            })  -- @()

  -- Operators
  highlight(0, 'objcOperator',          { fg = colors.white,      bg = 'NONE'            })  -- + - * / % = < > ! & | ^ ~
  highlight(0, 'objcPointerOp',         { fg = colors.white,      bg = 'NONE'            })  -- * & pointer operators
  highlight(0, 'objcMemberAccess',      { fg = colors.white,      bg = 'NONE'            })  -- . -> member access
  highlight(0, 'objcMsgBracket',        { fg = colors.white,      bg = 'NONE'            })  -- [ ] message brackets

  -- Preprocessor
  highlight(0, 'objcPreProc',           { fg = colors.pink,       bg = 'NONE'            })  -- General preprocessor
  highlight(0, 'objcInclude',           { fg = colors.pink,       bg = 'NONE'            })  -- #import, #include
  highlight(0, 'objcDefine',            { fg = colors.pink,       bg = 'NONE'            })  -- #define
  highlight(0, 'objcMacro',             { fg = colors.pink,       bg = 'NONE'            })  -- Macro names
  highlight(0, 'objcPreCondit',         { fg = colors.pink,       bg = 'NONE'            })  -- #if, #ifdef, #ifndef, etc.
  highlight(0, 'objcPragma',            { fg = colors.pink,       bg = 'NONE'            })  -- #pragma

  -- Comments
  highlight(0, 'objcComment',           { fg = colors.red,        bg = 'NONE'            })  -- /* */ and // comments
  highlight(0, 'objcCommentL',          { fg = colors.red,        bg = 'NONE'            })  -- // line comments
  highlight(0, 'objcDocComment',        { fg = colors.red,        bg = 'NONE'            })  -- Documentation comments
  highlight(0, 'objcTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Special
  highlight(0, 'objcSpecial',           { fg = colors.pink,       bg = 'NONE'            })  -- Special characters
  highlight(0, 'objcError',             { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.objc)

  -- Variables
  highlight(0, '@variable.objc',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.objc',      { fg = colors.blue,      bg = 'NONE' })  -- self, super
  highlight(0, '@variable.parameter.objc',    { fg = colors.purple,    bg = 'NONE' })  -- Method parameters
  highlight(0, '@variable.parameter.builtin.objc', { fg = colors.blue, bg = 'NONE' })  -- _cmd
  highlight(0, '@variable.member.objc',       { fg = colors.purple,    bg = 'NONE' })  -- Instance variables

  -- Constants
  highlight(0, '@constant.objc',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.objc',      { fg = colors.blue,      bg = 'NONE' })  -- YES, NO, nil, Nil, NULL
  highlight(0, '@constant.macro.objc',        { fg = colors.pink,      bg = 'NONE' })  -- Macro constants

  -- Functions/Methods
  highlight(0, '@function.objc',              { fg = colors.orange,    bg = 'NONE' })  -- C functions
  highlight(0, '@function.builtin.objc',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.method.objc',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.objc',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.macro.objc',        { fg = colors.pink,      bg = 'NONE' })  -- Macro functions
  highlight(0, '@constructor.objc',           { fg = colors.orange,    bg = 'NONE' })  -- init methods

  -- Types
  highlight(0, '@type.objc',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.objc',          { fg = colors.turquoise, bg = 'NONE' })  -- id, Class, SEL, BOOL, etc.
  highlight(0, '@type.definition.objc',       { fg = colors.turquoise, bg = 'NONE' })  -- typedef definitions
  highlight(0, '@type.qualifier.objc',        { fg = colors.blue,      bg = 'NONE' })  -- const, volatile, etc.

  -- Modules
  highlight(0, '@module.objc',                { fg = colors.turquoise, bg = 'NONE' })  -- Framework/module names

  -- Keywords
  highlight(0, '@keyword.objc',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.directive.objc',     { fg = colors.pink,      bg = 'NONE' })  -- @ directives
  highlight(0, '@keyword.import.objc',        { fg = colors.pink,      bg = 'NONE' })  -- #import, #include
  highlight(0, '@keyword.modifier.objc',      { fg = colors.pink,      bg = 'NONE' })  -- Property attributes, visibility
  highlight(0, '@keyword.type.objc',          { fg = colors.blue,      bg = 'NONE' })  -- struct, union, enum
  highlight(0, '@keyword.function.objc',      { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.operator.objc',      { fg = colors.blue,      bg = 'NONE' })  -- sizeof, typeof
  highlight(0, '@keyword.coroutine.objc',     { fg = colors.blue,      bg = 'NONE' })  -- @autoreleasepool
  highlight(0, '@keyword.exception.objc',     { fg = colors.blue,      bg = 'NONE' })  -- @try, @catch, @finally, @throw
  highlight(0, '@keyword.conditional.objc',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, switch
  highlight(0, '@keyword.repeat.objc',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, do

  -- Strings
  highlight(0, '@string.objc',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.objc',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.objc',        { fg = colors.pink,      bg = 'NONE' })  -- Format specifiers
  highlight(0, '@character.objc',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.objc',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.objc',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.objc',               { fg = colors.blue,      bg = 'NONE' })  -- YES, NO

  -- Comments
  highlight(0, '@comment.objc',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.objc', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Labels
  highlight(0, '@label.objc',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels

  -- Attributes
  highlight(0, '@attribute.objc',             { fg = colors.pink,      bg = 'NONE' })  -- Attributes/annotations

  -- Properties
  highlight(0, '@property.objc',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Operators and Punctuation
  highlight(0, '@operator.objc',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.objc',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.objc', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.objc',   { fg = colors.pink,      bg = 'NONE' })  -- @ prefix


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.objc)

  highlight(0, '@lsp.type.variable.objc',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.objc',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.objc',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.objc',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.objc',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.macro.objc',         { fg = colors.pink,      bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.type.objc',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.objc',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.protocol.objc',      { fg = colors.turquoise, bg = 'NONE' })  -- Protocols
  highlight(0, '@lsp.type.enum.objc',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.objc',    { fg = colors.purple,    bg = 'NONE' })  -- Enum values
  highlight(0, '@lsp.type.namespace.objc',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces/frameworks
  highlight(0, '@lsp.type.typeParameter.objc', { fg = colors.turquoise, bg = 'NONE' })  -- Generics (<ObjectType>)
  highlight(0, '@lsp.type.keyword.objc',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.objc',      { fg = colors.pink,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.objc',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.objc',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.objc',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.objc',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.decorator.objc',     { fg = colors.pink,      bg = 'NONE' })  -- Attributes

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.objc',    { fg = colors.purple,    bg = 'NONE' })  -- const variables
  highlight(0, '@lsp.typemod.variable.static.objc',      { fg = colors.purple,    bg = 'NONE' })  -- static variables
  highlight(0, '@lsp.typemod.function.declaration.objc', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.method.declaration.objc',   { fg = colors.orange,    bg = 'NONE' })  -- Method declarations
  highlight(0, '@lsp.typemod.method.static.objc',        { fg = colors.orange,    bg = 'NONE' })  -- Class methods (+)
  highlight(0, '@lsp.typemod.type.declaration.objc',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.objc',  { fg = colors.turquoise, bg = 'NONE' })  -- Foundation types
  highlight(0, '@lsp.typemod.class.declaration.objc',    { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
end

return objc
