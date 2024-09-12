-------------------------------------------------------------------------------
-- TypeScript
-------------------------------------------------------------------------------

local colors     = require("mannydark.palette")
local highlight  = vim.api.nvim_set_hl
local typescript = {}

-------------------------------------------------------------------------------
---- Settings

typescript.setupHighlighting = function()
  highlight(0, "typescriptGlobal",              { fg = colors.turquoise, bg = "NONE" })  -- Global objects of global libraries.
  highlight(0, "typescriptExport",              { fg = colors.blue,      bg = "NONE" })  -- 'export' keyword.
  highlight(0, "typescriptDefault",             { fg = colors.blue,      bg = "NONE" })  -- 'default' keyword.
  highlight(0, "typescriptImport",              { fg = colors.blue,      bg = "NONE" })  -- 'from' keyword.
  highlight(0, "typescriptBraces",              { fg = colors.white,     bg = "NONE" })  -- Braces: { }.
  highlight(0, "typescriptString",              { fg = colors.redLight,  bg = "NONE" })  -- Strings.
  highlight(0, "typescriptDefaultImportName",   { fg = colors.turquoise, bg = "NONE" })  -- Imported libraries.
  highlight(0, "typescriptTypeBlock",           { fg = colors.turquoise, bg = "NONE" })  -- Imported types.
  highlight(0, "typescriptPredefinedType",      { fg = colors.blue,      bg = "NONE" })  -- Data types.
  highlight(0, "typescriptTypeParameter",       { fg = colors.purple,    bg = "NONE" })  -- Parameter variables of types.
  highlight(0, "typescriptTemplateSB",          { fg = colors.redLight,  bg = "NONE" })  -- ${} in template strings.
  highlight(0, "typescriptCall",                { fg = colors.blue,      bg = "NONE" })  -- Like 'props' in function parameters.
  highlight(0, "typescriptObjectStaticMethod",  { fg = colors.orange,    bg = "NONE" })  -- Static methods of objects.
  highlight(0, "typescriptInterfaceName",       { fg = colors.turquoise, bg = "NONE" })  -- Name of interfaces.
  highlight(0, "typescriptTypeReference",       { fg = colors.turquoise, bg = "NONE" })  -- Reference to types like classes and interfaces.
  highlight(0, "typescriptVariable",            { fg = colors.blue,      bg = "NONE" })  -- Variable keywords like 'let', 'const' etc.
  highlight(0, "typescriptVariableDeclaration", { fg = colors.purple,    bg = "NONE" })  -- Variable names.
  highlight(0, "typescriptCommentTodo",         { fg = colors.red,       bg = "NONE", bold = true })  -- TODO comments.



----------------------- Not used by now:
-- typescriptArrowFuncTypeParameter xxx cleared
-- typescriptAsyncFunc xxx links to Keyword
-- typescriptFuncName xxx links to Function
-- typescriptArrowFuncArg xxx links to PreProc
-- typescriptReturnAnnotation xxx cleared
-- typescriptGenericImpl xxx cleared
-- typescriptParamImpl xxx links to PreProc
-- typescriptSymbols xxx links to Normal
-- typescriptDocNGParam xxx links to typescriptDocParam
-- typescriptType xxx links to Type
-- typescriptFuncArg xxx links to PreProc
-- typeScript     xxx cleared
-- typescriptWebsocketEvent xxx links to Title
-- typescriptWindowEvent xxx links to Title
-- typescriptUncategorizedEvent xxx links to Title
-- typescriptServiceWorkerEvent xxx links to Title
-- typescriptConstructor xxx links to Keyword
-- typescriptMemberOptionality xxx cleared
-- typescriptMember xxx links to Function
-- typescriptMethodAccessor xxx links to Operator
-- typescriptClassStatic xxx links to StorageClass
-- typescriptStringMember xxx links to String
-- typescriptComputedMember xxx cleared
-- typescriptAsyncFuncKeyword xxx links to Keyword
-- typescriptClassName xxx cleared
-- typescriptClassExtends xxx links to Keyword
-- typescriptClassBlock xxx cleared
-- typescriptClassTypeParameter xxx cleared
-- typescriptClassHeritage xxx cleared
-- typescriptMixinComma xxx cleared
-- typescriptClassTypeArguments xxx cleared
-- typescriptArrowFunc xxx links to Type
-- typescriptInterfaceExtends xxx links to Keyword
-- typescriptInterfaceTypeParameter xxx cleared
-- typescriptInterfaceHeritage xxx cleared
-- typescriptInterfaceComma xxx cleared
-- typescriptInterfaceTypeArguments xxx cleared
-- typescriptTypeCast xxx cleared
-- typescriptArrowFuncDef xxx cleared
-- typescriptFuncImpl xxx cleared
-- typescriptGlobalStringDot xxx cleared
-- typescriptStringStaticMethod xxx links to Keyword
-- typescriptStringMethod xxx links to Keyword
-- typescriptGlobalArrayDot xxx cleared
-- typescriptArrayStaticMethod xxx links to Keyword
-- typescriptArrayMethod xxx links to Keyword
-- typescriptGlobalObjectDot xxx cleared
-- typescriptObjectMethod xxx links to Keyword
-- typescriptGlobalSymbolDot xxx cleared
-- typescriptSymbolStaticProp xxx links to Keyword
-- typescriptSymbolStaticMethod xxx links to Keyword
-- typescriptFunctionMethod xxx links to Keyword
-- typescriptGlobalMathDot xxx cleared
-- typescriptMathStaticProp xxx links to Keyword
-- typescriptMathStaticMethod xxx links to Keyword
-- typescriptGlobalDateDot xxx cleared
-- typescriptDateStaticMethod xxx links to Keyword
-- typescriptDateMethod xxx links to Keyword
-- typescriptGlobalJSONDot xxx cleared
-- typescriptJSONStaticMethod xxx links to Keyword
-- typescriptGlobalRegExpDot xxx cleared
-- typescriptRegExpStaticProp xxx links to Keyword
-- typescriptRegExpProp xxx links to Keyword
-- typescriptRegExpMethod xxx links to Keyword
-- typescriptES6MapProp xxx links to Keyword
-- typescriptES6MapMethod xxx links to Keyword
-- typescriptES6SetProp xxx links to Keyword
-- typescriptES6SetMethod xxx links to Keyword
-- typescriptProxyAPI xxx links to Keyword
-- typescriptGlobalPromiseDot xxx cleared
-- typescriptPromiseStaticMethod xxx links to Keyword
-- typescriptPromiseMethod xxx links to Keyword
-- typescriptReflectMethod xxx links to Keyword
-- typescriptIntlMethod xxx links to Keyword
-- typescriptNodeGlobal xxx links to Structure
-- typescriptTestGlobal xxx links to Function
-- typescriptBOM  xxx links to Structure
-- typescriptBOMWindowProp xxx links to Structure
-- typescriptBOMWindowMethod xxx links to Structure
-- typescriptBOMWindowEvent xxx links to Keyword
-- typescriptBOMWindowCons xxx links to Structure
-- typescriptBOMNavigatorProp xxx links to Keyword
-- typescriptBOMNavigatorMethod xxx links to Keyword
-- typescriptServiceWorkerMethod xxx links to Keyword
-- typescriptBOMLocationProp xxx links to Keyword
-- typescriptBOMLocationMethod xxx links to Keyword
-- typescriptBOMHistoryProp xxx links to Keyword
-- typescriptBOMHistoryMethod xxx links to Keyword
-- typescriptConsoleMethod xxx links to Keyword
-- typescriptXHRGlobal xxx links to Structure
-- typescriptXHRProp xxx links to Keyword
-- typescriptXHRMethod xxx links to Keyword
-- typescriptGlobalURLDot xxx cleared
-- typescriptURLStaticMethod xxx links to Keyword
-- typescriptFileMethod xxx links to Keyword
-- typescriptFileReaderProp xxx links to Keyword
-- typescriptFileReaderMethod xxx links to Keyword
-- typescriptFileListMethod xxx links to Keyword
-- typescriptBlobMethod xxx links to Keyword
-- typescriptURLUtilsProp xxx links to Keyword
-- typescriptCryptoGlobal xxx links to Structure
-- typescriptSubtleCryptoMethod xxx links to Keyword
-- typescriptCryptoProp xxx links to Keyword
-- typescriptCryptoMethod xxx links to Keyword
-- typescriptHeadersMethod xxx links to Keyword
-- typescriptRequestProp xxx links to Keyword
-- typescriptRequestMethod xxx links to Keyword
-- typescriptResponseProp xxx links to Keyword
-- typescriptResponseMethod xxx links to Keyword
-- typescriptServiceWorkerProp xxx links to Keyword
-- typescriptCacheMethod xxx links to Keyword
-- typescriptEncodingGlobal xxx links to Structure
-- typescriptEncodingProp xxx links to Keyword
-- typescriptEncodingMethod xxx links to Keyword
-- typescriptGeolocationMethod xxx links to Keyword
-- typescriptBOMNetworkProp xxx links to Keyword
-- typescriptPaymentMethod xxx links to Keyword
-- typescriptPaymentProp xxx links to Keyword
-- typescriptPaymentEvent xxx links to Keyword
-- typescriptPaymentResponseMethod xxx links to Keyword
-- typescriptPaymentResponseProp xxx links to Keyword
-- typescriptPaymentAddressProp xxx links to Keyword
-- typescriptPaymentShippingOptionProp xxx links to Keyword
-- typescriptDOMNodeProp xxx links to Keyword
-- typescriptDOMNodeMethod xxx links to Keyword
-- typescriptDOMNodeType xxx links to Keyword
-- typescriptDOMElemAttrs xxx links to Keyword
-- typescriptDOMElemFuncs xxx links to Keyword
-- typescriptDOMDocProp xxx links to Keyword
-- typescriptDOMDocMethod xxx links to Keyword
-- typescriptDOMEventTargetMethod xxx links to Keyword
-- typescriptDOMEventCons xxx links to Structure
-- typescriptDOMEventProp xxx links to Keyword
-- typescriptDOMEventMethod xxx links to Keyword
-- typescriptDOMStorage xxx links to Keyword
-- typescriptDOMStorageProp xxx links to Keyword
-- typescriptDOMStorageMethod xxx links to Keyword
-- typescriptDOMFormProp xxx links to Keyword
-- typescriptDOMFormMethod xxx links to Keyword
-- typescriptAnimationEvent xxx links to Title
-- typescriptCSSEvent xxx links to Title
-- typescriptDatabaseEvent xxx links to Title
-- typescriptDocumentEvent xxx links to Title
-- typescriptDOMMutationEvent xxx links to Title
-- typescriptDragEvent xxx links to Title
-- typescriptElementEvent xxx links to Title
-- typescriptFocusEvent xxx links to Title
-- typescriptFormEvent xxx links to Title
-- typescriptFrameEvent xxx links to Title
-- typescriptInputDeviceEvent xxx links to Title
-- typescriptMediaEvent xxx links to Title
-- typescriptMenuEvent xxx links to Title
-- typescriptNetworkEvent xxx links to Title
-- typescriptProgressEvent xxx links to Title
-- typescriptResourceEvent xxx links to Title
-- typescriptScriptEvent xxx links to Title
-- typescriptSensorEvent xxx links to Title
-- typescriptSessionHistoryEvent xxx links to Title
-- typescriptStorageEvent xxx links to Title
-- typescriptSVGEvent xxx links to Title
-- typescriptTabEvent xxx links to Title
-- typescriptTextEvent xxx links to Title
-- typescriptTouchEvent xxx links to Title
-- typescriptUpdateEvent xxx links to Title
-- typescriptValueChangeEvent xxx links to Title
-- typescriptViewEvent xxx links to Title
-- typescriptTypeQuery xxx links to Keyword
-- typescriptStringLiteralType xxx links to String
-- typescriptTemplateLiteralType xxx links to String
-- typescriptReadonlyArrayKeyword xxx links to Keyword
-- typescriptAssertType xxx links to Keyword
-- typescriptTemplateSubstitutionType xxx cleared
-- typescriptTypeOperator xxx cleared
-- typescriptUserDefinedType xxx links to Keyword
-- typescriptAccessibilityModifier xxx links to Keyword
-- typescriptReadonlyModifier xxx links to Keyword
-- typescriptConstructSignature xxx links to Identifier
-- typescriptIndexSignature xxx cleared
-- typescriptTupleLable xxx links to Label
-- typescriptConditionalType xxx cleared
-- typescriptGenericFunc xxx cleared
-- typescriptFuncType xxx links to Special
-- typescriptFunctionType xxx cleared
-- typescriptFuncTypeArrow xxx links to Function
-- typescriptGenericCall xxx cleared
-- typescriptDecorator xxx links to Special
-- typescriptFuncComma xxx links to Operator
-- typescriptMappedIn xxx links to Special
-- typescriptAliasKeyword xxx links to Keyword
-- typescriptAliasDeclaration xxx links to Identifier
-- typescriptGlobalMethod xxx links to Structure
-- typescriptGlobalNumberDot xxx cleared
-- typescriptNumberStaticProp xxx links to Keyword
-- typescriptNumberStaticMethod xxx links to Keyword
-- typescriptNumberMethod xxx links to Keyword
-- typescriptDocComment xxx links to Comment
-- typescriptDocNotation xxx links to SpecialComment
-- typescriptDocTags xxx links to SpecialComment
-- typescriptDocParam xxx links to Function
-- typescriptDocNGDirective xxx cleared
-- typescriptDocNumParam xxx links to Function
-- typescriptDocDesc xxx cleared
-- typescriptDocA xxx cleared
-- typescriptDocNamedParamType xxx links to Type
-- typescriptDocParamName xxx links to Type
-- typescriptDocParamType xxx links to Type
-- typescriptDocRef xxx cleared
-- typescriptDocName xxx cleared
-- typescriptDocEventRef xxx links to Function
-- typescriptDocAs xxx cleared
-- typescriptDocB xxx cleared
-- typescriptDocLinkTag xxx cleared
-- typescriptOptionalMark xxx links to PreProc
-- typescriptGenericDefault xxx cleared
-- typescriptTypeBrackets xxx cleared
-- typescriptTypeParameters xxx cleared
-- typescriptConstraint xxx links to Keyword
-- typescriptUnion xxx links to Operator
-- typescriptConstructorType xxx links to Function
-- typescriptTypeBracket xxx cleared
-- typescriptParenthesizedType xxx cleared
-- typescriptObjectType xxx cleared
-- typescriptTupleType xxx cleared
-- typescriptMagicComment xxx links to SpecialComment
-- typescriptRef  xxx links to Include
-- typescriptLineComment xxx links to Comment
-- typescriptComment xxx links to Comment
-- typescriptEnum xxx cleared
-- typescriptEnumKeyword xxx links to Identifier
-- typescriptOperator xxx links to Identifier
-- typescriptForOperator xxx links to Repeat
-- typescriptBoolean xxx links to Boolean
-- typescriptMessage xxx links to Keyword
-- typescriptConditional xxx links to Conditional
-- typescriptConditionalParen xxx cleared
-- typescriptConditionalElse xxx links to Conditional
-- typescriptRepeat xxx links to Repeat
-- typescriptLoopParen xxx cleared
-- typescriptAsyncFor xxx links to Keyword
-- typescriptBranch xxx links to Conditional
-- typescriptCase xxx links to Conditional
-- typescriptClassKeyword xxx links to Keyword
-- typescriptInterfaceKeyword xxx links to Keyword
-- typescriptStatementKeyword xxx links to Statement
-- typescriptTry  xxx links to Special
-- typescriptExceptions xxx links to Special
-- typescriptDebugger xxx cleared
-- typescriptEndColons xxx links to Exception
-- typescriptAmbientDeclaration xxx links to Special
-- typescriptFuncKeyword xxx links to Keyword
-- typescriptAbstract xxx links to Special
-- typescriptIdentifier xxx links to Structure
-- typescriptNull xxx links to Boolean
-- typescriptPrototype xxx links to Type
-- typescriptTemplateSubstitution xxx cleared
-- typescriptSpecial xxx links to Special
-- typescriptRegexpCharClass xxx cleared
-- typescriptRegexpBoundary xxx cleared
-- typescriptRegexpBackRef xxx cleared
-- typescriptRegexpQuantifier xxx cleared
-- typescriptRegexpOr xxx cleared
-- typescriptRegexpMod xxx cleared
-- typescriptRegexpGroup xxx cleared
-- typescriptRegexpString xxx links to String
-- typescriptArray xxx cleared
-- typescriptNumber xxx links to Number
-- typescriptObjectLabel xxx links to typescriptLabel
-- typescriptStringProperty xxx links to String
-- typescriptComputedPropertyName xxx cleared
-- typescriptObjectAsyncKeyword xxx links to Keyword
-- typescriptTernary xxx cleared
-- typescriptObjectLiteral xxx cleared
-- typescriptObjectColon xxx cleared
-- typescriptUnaryOp xxx cleared
-- typescriptTernaryOp xxx cleared
-- typescriptBinaryOp xxx cleared
-- typescriptKeywordOp xxx links to Identifier
-- typescriptImportType xxx links to Special
-- typescriptExportType xxx links to Special
-- typescriptModule xxx links to Special
-- typescriptBlock xxx cleared
-- typescriptDotNotation xxx cleared
-- typescriptFuncCallArg xxx cleared
-- typescriptTemplate xxx links to String
-- typescriptIndexExpr xxx cleared
-- typescriptTypeArguments xxx cleared
-- typescriptIdentifierName xxx cleared
-- typescriptProp xxx cleared
-- typescriptProperty xxx cleared
-- typescriptCastKeyword xxx links to Special
-- typescriptDOMStyle xxx links to Keyword
-- typescriptDotStyleNotation xxx cleared
-- typescriptParens xxx links to Normal
-- typescriptParenExp xxx cleared
-- typescriptEventFuncCallArg xxx cleared
-- typescriptASCII xxx links to Special
-- typescriptEventString xxx links to String
-- typescriptDestructureAs xxx cleared
-- typescriptDestructureString xxx links to String
-- typescriptTypeAnnotation xxx cleared
-- typescriptAssign xxx cleared
-- typescriptRestOrSpread xxx cleared
-- typescriptDestructureComma xxx cleared
-- typescriptDestructureLabel xxx links to Function
-- typescriptDestructureVariable xxx links to PreProc
-- typescriptDefaultParam xxx cleared
-- typescriptArrayDestructure xxx cleared
-- typescriptObjectDestructure xxx cleared
-- typescriptObjectSpread xxx cleared
-- typescriptReserved xxx links to Error
-- typescriptLabel xxx links to Label





end

return typescript

