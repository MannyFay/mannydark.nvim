-------------------------------------------------------------------------------
-- Starlark Files (Bazel)
-- Highlighting for .star, .bzl, BUILD, BUILD.bazel, WORKSPACE files.
-- Starlark is a Python-like configuration language used in Bazel.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local starlark  = {}


-------------------------------------------------------------------------------
-- Settings

starlark.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Comments
  highlight(0, 'starlarkComment',          { fg = colors.red,        bg = 'NONE' })  -- # comments
  highlight(0, 'starlarkTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Keywords
  highlight(0, 'starlarkKeyword',          { fg = colors.pink,       bg = 'NONE' })  -- def, if, for, etc.
  highlight(0, 'starlarkDef',              { fg = colors.pink,       bg = 'NONE' })  -- def
  highlight(0, 'starlarkLambda',           { fg = colors.pink,       bg = 'NONE' })  -- lambda
  highlight(0, 'starlarkReturn',           { fg = colors.pink,       bg = 'NONE' })  -- return
  highlight(0, 'starlarkPass',             { fg = colors.pink,       bg = 'NONE' })  -- pass

  -- Control Flow
  highlight(0, 'starlarkConditional',      { fg = colors.pink,       bg = 'NONE' })  -- if, elif, else
  highlight(0, 'starlarkIf',               { fg = colors.pink,       bg = 'NONE' })  -- if
  highlight(0, 'starlarkElif',             { fg = colors.pink,       bg = 'NONE' })  -- elif
  highlight(0, 'starlarkElse',             { fg = colors.pink,       bg = 'NONE' })  -- else
  highlight(0, 'starlarkRepeat',           { fg = colors.pink,       bg = 'NONE' })  -- for
  highlight(0, 'starlarkFor',              { fg = colors.pink,       bg = 'NONE' })  -- for
  highlight(0, 'starlarkIn',               { fg = colors.pink,       bg = 'NONE' })  -- in
  highlight(0, 'starlarkBreak',            { fg = colors.pink,       bg = 'NONE' })  -- break
  highlight(0, 'starlarkContinue',         { fg = colors.pink,       bg = 'NONE' })  -- continue

  -- Logical Operators (keywords)
  highlight(0, 'starlarkOperatorKeyword',  { fg = colors.pink,       bg = 'NONE' })  -- and, or, not
  highlight(0, 'starlarkAnd',              { fg = colors.pink,       bg = 'NONE' })  -- and
  highlight(0, 'starlarkOr',               { fg = colors.pink,       bg = 'NONE' })  -- or
  highlight(0, 'starlarkNot',              { fg = colors.pink,       bg = 'NONE' })  -- not

  -- Load Statement
  highlight(0, 'starlarkLoad',             { fg = colors.pink,       bg = 'NONE' })  -- load

  -- Reserved Keywords (not yet implemented in Starlark)
  highlight(0, 'starlarkReserved',         { fg = colors.gray,       bg = 'NONE' })  -- class, try, etc.

  -- Boolean and None
  highlight(0, 'starlarkBoolean',          { fg = colors.blue,       bg = 'NONE' })  -- True, False
  highlight(0, 'starlarkTrue',             { fg = colors.blue,       bg = 'NONE' })  -- True
  highlight(0, 'starlarkFalse',            { fg = colors.blue,       bg = 'NONE' })  -- False
  highlight(0, 'starlarkNone',             { fg = colors.blue,       bg = 'NONE' })  -- None

  -- Numbers
  highlight(0, 'starlarkNumber',           { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, 'starlarkInteger',          { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, 'starlarkFloat',            { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'starlarkHexNumber',        { fg = colors.greenLight, bg = 'NONE' })  -- 0xFF
  highlight(0, 'starlarkOctNumber',        { fg = colors.greenLight, bg = 'NONE' })  -- 0o77
  highlight(0, 'starlarkBinNumber',        { fg = colors.greenLight, bg = 'NONE' })  -- 0b1010

  -- Strings
  highlight(0, 'starlarkString',           { fg = colors.redLight,   bg = 'NONE' })  -- "string"
  highlight(0, 'starlarkStringDouble',     { fg = colors.redLight,   bg = 'NONE' })  -- "double"
  highlight(0, 'starlarkStringSingle',     { fg = colors.redLight,   bg = 'NONE' })  -- 'single'
  highlight(0, 'starlarkStringTriple',     { fg = colors.redLight,   bg = 'NONE' })  -- """triple"""
  highlight(0, 'starlarkRawString',        { fg = colors.redLight,   bg = 'NONE' })  -- r"raw"
  highlight(0, 'starlarkDocstring',        { fg = colors.red,        bg = 'NONE' })  -- Docstrings

  -- Escape Sequences
  highlight(0, 'starlarkEscape',           { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, \\, etc.
  highlight(0, 'starlarkUnicodeEscape',    { fg = colors.pink,       bg = 'NONE' })  -- \uXXXX, \xXX

  -- String Formatting
  highlight(0, 'starlarkFormatSpec',       { fg = colors.pink,       bg = 'NONE' })  -- {} in .format()
  highlight(0, 'starlarkFormatBraces',     { fg = colors.pink,       bg = 'NONE' })  -- { }

  -- Identifiers and Variables
  highlight(0, 'starlarkIdentifier',       { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, 'starlarkVariable',         { fg = colors.white,      bg = 'NONE' })  -- Variables

  -- Functions
  highlight(0, 'starlarkFunction',         { fg = colors.orange,     bg = 'NONE' })  -- Function definitions
  highlight(0, 'starlarkFunctionCall',     { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, 'starlarkParameter',        { fg = colors.purple,     bg = 'NONE' })  -- Function parameters

  -- Attributes/Members
  highlight(0, 'starlarkAttribute',        { fg = colors.blue,       bg = 'NONE' })  -- .attribute
  highlight(0, 'starlarkMember',           { fg = colors.blue,       bg = 'NONE' })  -- obj.member

  -- Operators
  highlight(0, 'starlarkOperator',         { fg = colors.white,      bg = 'NONE' })  -- General operators
  highlight(0, 'starlarkArithmeticOp',     { fg = colors.white,      bg = 'NONE' })  -- + - * / // %
  highlight(0, 'starlarkComparisonOp',     { fg = colors.white,      bg = 'NONE' })  -- == != < > <= >=
  highlight(0, 'starlarkBitwiseOp',        { fg = colors.white,      bg = 'NONE' })  -- & | ^ ~ << >>
  highlight(0, 'starlarkAssignmentOp',     { fg = colors.white,      bg = 'NONE' })  -- = += -= etc.

  -- Delimiters and Punctuation
  highlight(0, 'starlarkParens',           { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'starlarkBrackets',         { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'starlarkBraces',           { fg = colors.white,      bg = 'NONE' })  -- { }
  highlight(0, 'starlarkComma',            { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'starlarkColon',            { fg = colors.white,      bg = 'NONE' })  -- :
  highlight(0, 'starlarkDot',              { fg = colors.white,      bg = 'NONE' })  -- .
  highlight(0, 'starlarkSemicolon',        { fg = colors.white,      bg = 'NONE' })  -- ;


  -----------------------------------------------------------------------------
  -- Built-in Functions

  highlight(0, 'starlarkBuiltin',          { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions

  -- Core Built-ins
  highlight(0, 'starlarkBuiltinAbs',       { fg = colors.orange,     bg = 'NONE' })  -- abs
  highlight(0, 'starlarkBuiltinAll',       { fg = colors.orange,     bg = 'NONE' })  -- all
  highlight(0, 'starlarkBuiltinAny',       { fg = colors.orange,     bg = 'NONE' })  -- any
  highlight(0, 'starlarkBuiltinBool',      { fg = colors.orange,     bg = 'NONE' })  -- bool
  highlight(0, 'starlarkBuiltinBytes',     { fg = colors.orange,     bg = 'NONE' })  -- bytes
  highlight(0, 'starlarkBuiltinDict',      { fg = colors.orange,     bg = 'NONE' })  -- dict
  highlight(0, 'starlarkBuiltinDir',       { fg = colors.orange,     bg = 'NONE' })  -- dir
  highlight(0, 'starlarkBuiltinEnumerate', { fg = colors.orange,     bg = 'NONE' })  -- enumerate
  highlight(0, 'starlarkBuiltinFail',      { fg = colors.orange,     bg = 'NONE' })  -- fail
  highlight(0, 'starlarkBuiltinFloat',     { fg = colors.orange,     bg = 'NONE' })  -- float
  highlight(0, 'starlarkBuiltinGetattr',   { fg = colors.orange,     bg = 'NONE' })  -- getattr
  highlight(0, 'starlarkBuiltinHasattr',   { fg = colors.orange,     bg = 'NONE' })  -- hasattr
  highlight(0, 'starlarkBuiltinHash',      { fg = colors.orange,     bg = 'NONE' })  -- hash
  highlight(0, 'starlarkBuiltinInt',       { fg = colors.orange,     bg = 'NONE' })  -- int
  highlight(0, 'starlarkBuiltinLen',       { fg = colors.orange,     bg = 'NONE' })  -- len
  highlight(0, 'starlarkBuiltinList',      { fg = colors.orange,     bg = 'NONE' })  -- list
  highlight(0, 'starlarkBuiltinMax',       { fg = colors.orange,     bg = 'NONE' })  -- max
  highlight(0, 'starlarkBuiltinMin',       { fg = colors.orange,     bg = 'NONE' })  -- min
  highlight(0, 'starlarkBuiltinPrint',     { fg = colors.orange,     bg = 'NONE' })  -- print
  highlight(0, 'starlarkBuiltinRange',     { fg = colors.orange,     bg = 'NONE' })  -- range
  highlight(0, 'starlarkBuiltinRepr',      { fg = colors.orange,     bg = 'NONE' })  -- repr
  highlight(0, 'starlarkBuiltinReversed',  { fg = colors.orange,     bg = 'NONE' })  -- reversed
  highlight(0, 'starlarkBuiltinSet',       { fg = colors.orange,     bg = 'NONE' })  -- set
  highlight(0, 'starlarkBuiltinSorted',    { fg = colors.orange,     bg = 'NONE' })  -- sorted
  highlight(0, 'starlarkBuiltinStr',       { fg = colors.orange,     bg = 'NONE' })  -- str
  highlight(0, 'starlarkBuiltinTuple',     { fg = colors.orange,     bg = 'NONE' })  -- tuple
  highlight(0, 'starlarkBuiltinType',      { fg = colors.orange,     bg = 'NONE' })  -- type
  highlight(0, 'starlarkBuiltinZip',       { fg = colors.orange,     bg = 'NONE' })  -- zip

  -- Struct
  highlight(0, 'starlarkStruct',           { fg = colors.orange,     bg = 'NONE' })  -- struct


  -----------------------------------------------------------------------------
  -- Bazel-specific Functions (BUILD files)

  -- Global BUILD Functions
  highlight(0, 'bazelFunction',            { fg = colors.orange,     bg = 'NONE' })  -- Bazel functions
  highlight(0, 'bazelGlob',                { fg = colors.orange,     bg = 'NONE' })  -- glob
  highlight(0, 'bazelSelect',              { fg = colors.orange,     bg = 'NONE' })  -- select
  highlight(0, 'bazelPackage',             { fg = colors.orange,     bg = 'NONE' })  -- package
  highlight(0, 'bazelPackageGroup',        { fg = colors.orange,     bg = 'NONE' })  -- package_group
  highlight(0, 'bazelExportsFiles',        { fg = colors.orange,     bg = 'NONE' })  -- exports_files
  highlight(0, 'bazelLicenses',            { fg = colors.orange,     bg = 'NONE' })  -- licenses
  highlight(0, 'bazelDepset',              { fg = colors.orange,     bg = 'NONE' })  -- depset
  highlight(0, 'bazelExistingRule',        { fg = colors.orange,     bg = 'NONE' })  -- existing_rule
  highlight(0, 'bazelExistingRules',       { fg = colors.orange,     bg = 'NONE' })  -- existing_rules
  highlight(0, 'bazelPackageName',         { fg = colors.orange,     bg = 'NONE' })  -- package_name
  highlight(0, 'bazelRepositoryName',      { fg = colors.orange,     bg = 'NONE' })  -- repository_name
  highlight(0, 'bazelRepoName',            { fg = colors.orange,     bg = 'NONE' })  -- repo_name
  highlight(0, 'bazelModuleName',          { fg = colors.orange,     bg = 'NONE' })  -- module_name
  highlight(0, 'bazelModuleVersion',       { fg = colors.orange,     bg = 'NONE' })  -- module_version
  highlight(0, 'bazelSubpackages',         { fg = colors.orange,     bg = 'NONE' })  -- subpackages
  highlight(0, 'bazelPackageRelLabel',     { fg = colors.orange,     bg = 'NONE' })  -- package_relative_label

  -- Native Module
  highlight(0, 'bazelNative',              { fg = colors.purple,     bg = 'NONE' })  -- native
  highlight(0, 'bazelNativeFunc',          { fg = colors.orange,     bg = 'NONE' })  -- native.* functions


  -----------------------------------------------------------------------------
  -- Bazel Rules (Native and Common)

  highlight(0, 'bazelRule',                { fg = colors.turquoise,  bg = 'NONE' })  -- Rule names

  -- General/Core Rules
  highlight(0, 'bazelGenrule',             { fg = colors.turquoise,  bg = 'NONE' })  -- genrule
  highlight(0, 'bazelFilegroup',           { fg = colors.turquoise,  bg = 'NONE' })  -- filegroup
  highlight(0, 'bazelTestSuite',           { fg = colors.turquoise,  bg = 'NONE' })  -- test_suite
  highlight(0, 'bazelAlias',               { fg = colors.turquoise,  bg = 'NONE' })  -- alias
  highlight(0, 'bazelConfigSetting',       { fg = colors.turquoise,  bg = 'NONE' })  -- config_setting
  highlight(0, 'bazelGenquery',            { fg = colors.turquoise,  bg = 'NONE' })  -- genquery

  -- Platform Rules
  highlight(0, 'bazelPlatform',            { fg = colors.turquoise,  bg = 'NONE' })  -- platform
  highlight(0, 'bazelToolchain',           { fg = colors.turquoise,  bg = 'NONE' })  -- toolchain
  highlight(0, 'bazelToolchainType',       { fg = colors.turquoise,  bg = 'NONE' })  -- toolchain_type
  highlight(0, 'bazelConstraintSetting',   { fg = colors.turquoise,  bg = 'NONE' })  -- constraint_setting
  highlight(0, 'bazelConstraintValue',     { fg = colors.turquoise,  bg = 'NONE' })  -- constraint_value

  -- C/C++ Rules
  highlight(0, 'bazelCcLibrary',           { fg = colors.turquoise,  bg = 'NONE' })  -- cc_library
  highlight(0, 'bazelCcBinary',            { fg = colors.turquoise,  bg = 'NONE' })  -- cc_binary
  highlight(0, 'bazelCcTest',              { fg = colors.turquoise,  bg = 'NONE' })  -- cc_test
  highlight(0, 'bazelCcImport',            { fg = colors.turquoise,  bg = 'NONE' })  -- cc_import
  highlight(0, 'bazelCcProtoLibrary',      { fg = colors.turquoise,  bg = 'NONE' })  -- cc_proto_library
  highlight(0, 'bazelCcToolchain',         { fg = colors.turquoise,  bg = 'NONE' })  -- cc_toolchain
  highlight(0, 'bazelCcToolchainSuite',    { fg = colors.turquoise,  bg = 'NONE' })  -- cc_toolchain_suite

  -- Java Rules
  highlight(0, 'bazelJavaLibrary',         { fg = colors.turquoise,  bg = 'NONE' })  -- java_library
  highlight(0, 'bazelJavaBinary',          { fg = colors.turquoise,  bg = 'NONE' })  -- java_binary
  highlight(0, 'bazelJavaTest',            { fg = colors.turquoise,  bg = 'NONE' })  -- java_test
  highlight(0, 'bazelJavaImport',          { fg = colors.turquoise,  bg = 'NONE' })  -- java_import
  highlight(0, 'bazelJavaPlugin',          { fg = colors.turquoise,  bg = 'NONE' })  -- java_plugin
  highlight(0, 'bazelJavaProtoLibrary',    { fg = colors.turquoise,  bg = 'NONE' })  -- java_proto_library
  highlight(0, 'bazelJavaToolchain',       { fg = colors.turquoise,  bg = 'NONE' })  -- java_toolchain

  -- Python Rules
  highlight(0, 'bazelPyLibrary',           { fg = colors.turquoise,  bg = 'NONE' })  -- py_library
  highlight(0, 'bazelPyBinary',            { fg = colors.turquoise,  bg = 'NONE' })  -- py_binary
  highlight(0, 'bazelPyTest',              { fg = colors.turquoise,  bg = 'NONE' })  -- py_test
  highlight(0, 'bazelPyRuntime',           { fg = colors.turquoise,  bg = 'NONE' })  -- py_runtime

  -- Shell Rules
  highlight(0, 'bazelShLibrary',           { fg = colors.turquoise,  bg = 'NONE' })  -- sh_library
  highlight(0, 'bazelShBinary',            { fg = colors.turquoise,  bg = 'NONE' })  -- sh_binary
  highlight(0, 'bazelShTest',              { fg = colors.turquoise,  bg = 'NONE' })  -- sh_test

  -- Proto Rules
  highlight(0, 'bazelProtoLibrary',        { fg = colors.turquoise,  bg = 'NONE' })  -- proto_library

  -- Android Rules
  highlight(0, 'bazelAndroidLibrary',      { fg = colors.turquoise,  bg = 'NONE' })  -- android_library
  highlight(0, 'bazelAndroidBinary',       { fg = colors.turquoise,  bg = 'NONE' })  -- android_binary
  highlight(0, 'bazelAndroidTest',         { fg = colors.turquoise,  bg = 'NONE' })  -- android_test
  highlight(0, 'bazelAarImport',           { fg = colors.turquoise,  bg = 'NONE' })  -- aar_import

  -- iOS/Apple Rules
  highlight(0, 'bazelObjcLibrary',         { fg = colors.turquoise,  bg = 'NONE' })  -- objc_library
  highlight(0, 'bazelObjcImport',          { fg = colors.turquoise,  bg = 'NONE' })  -- objc_import
  highlight(0, 'bazelAppleBinary',         { fg = colors.turquoise,  bg = 'NONE' })  -- apple_binary

  -- Go Rules (rules_go)
  highlight(0, 'bazelGoLibrary',           { fg = colors.turquoise,  bg = 'NONE' })  -- go_library
  highlight(0, 'bazelGoBinary',            { fg = colors.turquoise,  bg = 'NONE' })  -- go_binary
  highlight(0, 'bazelGoTest',              { fg = colors.turquoise,  bg = 'NONE' })  -- go_test

  -- Rust Rules (rules_rust)
  highlight(0, 'bazelRustLibrary',         { fg = colors.turquoise,  bg = 'NONE' })  -- rust_library
  highlight(0, 'bazelRustBinary',          { fg = colors.turquoise,  bg = 'NONE' })  -- rust_binary
  highlight(0, 'bazelRustTest',            { fg = colors.turquoise,  bg = 'NONE' })  -- rust_test


  -----------------------------------------------------------------------------
  -- Common Rule Attributes

  highlight(0, 'bazelAttribute',           { fg = colors.blue,       bg = 'NONE' })  -- Rule attributes

  -- Universal Attributes
  highlight(0, 'bazelAttrName',            { fg = colors.blue,       bg = 'NONE' })  -- name
  highlight(0, 'bazelAttrSrcs',            { fg = colors.blue,       bg = 'NONE' })  -- srcs
  highlight(0, 'bazelAttrDeps',            { fg = colors.blue,       bg = 'NONE' })  -- deps
  highlight(0, 'bazelAttrData',            { fg = colors.blue,       bg = 'NONE' })  -- data
  highlight(0, 'bazelAttrVisibility',      { fg = colors.blue,       bg = 'NONE' })  -- visibility
  highlight(0, 'bazelAttrTags',            { fg = colors.blue,       bg = 'NONE' })  -- tags
  highlight(0, 'bazelAttrTestonly',        { fg = colors.blue,       bg = 'NONE' })  -- testonly
  highlight(0, 'bazelAttrFeatures',        { fg = colors.blue,       bg = 'NONE' })  -- features
  highlight(0, 'bazelAttrCompatibleWith',  { fg = colors.blue,       bg = 'NONE' })  -- compatible_with
  highlight(0, 'bazelAttrRestrictedTo',    { fg = colors.blue,       bg = 'NONE' })  -- restricted_to
  highlight(0, 'bazelAttrDeprecation',     { fg = colors.blue,       bg = 'NONE' })  -- deprecation
  highlight(0, 'bazelAttrExecCompatible',  { fg = colors.blue,       bg = 'NONE' })  -- exec_compatible_with
  highlight(0, 'bazelAttrToolchains',      { fg = colors.blue,       bg = 'NONE' })  -- toolchains

  -- Binary/Test Attributes
  highlight(0, 'bazelAttrMain',            { fg = colors.blue,       bg = 'NONE' })  -- main
  highlight(0, 'bazelAttrArgs',            { fg = colors.blue,       bg = 'NONE' })  -- args
  highlight(0, 'bazelAttrEnv',             { fg = colors.blue,       bg = 'NONE' })  -- env
  highlight(0, 'bazelAttrSize',            { fg = colors.blue,       bg = 'NONE' })  -- size
  highlight(0, 'bazelAttrTimeout',         { fg = colors.blue,       bg = 'NONE' })  -- timeout
  highlight(0, 'bazelAttrFlaky',           { fg = colors.blue,       bg = 'NONE' })  -- flaky
  highlight(0, 'bazelAttrShard',           { fg = colors.blue,       bg = 'NONE' })  -- shard_count

  -- C/C++ Attributes
  highlight(0, 'bazelAttrHdrs',            { fg = colors.blue,       bg = 'NONE' })  -- hdrs
  highlight(0, 'bazelAttrCopts',           { fg = colors.blue,       bg = 'NONE' })  -- copts
  highlight(0, 'bazelAttrLinkopts',        { fg = colors.blue,       bg = 'NONE' })  -- linkopts
  highlight(0, 'bazelAttrDefines',         { fg = colors.blue,       bg = 'NONE' })  -- defines
  highlight(0, 'bazelAttrIncludes',        { fg = colors.blue,       bg = 'NONE' })  -- includes
  highlight(0, 'bazelAttrLinkstatic',      { fg = colors.blue,       bg = 'NONE' })  -- linkstatic
  highlight(0, 'bazelAttrAlwayslink',      { fg = colors.blue,       bg = 'NONE' })  -- alwayslink

  -- Genrule Attributes
  highlight(0, 'bazelAttrOuts',            { fg = colors.blue,       bg = 'NONE' })  -- outs
  highlight(0, 'bazelAttrCmd',             { fg = colors.blue,       bg = 'NONE' })  -- cmd
  highlight(0, 'bazelAttrCmdBash',         { fg = colors.blue,       bg = 'NONE' })  -- cmd_bash
  highlight(0, 'bazelAttrTools',           { fg = colors.blue,       bg = 'NONE' })  -- tools

  -- Package Attributes
  highlight(0, 'bazelAttrDefaultVis',      { fg = colors.blue,       bg = 'NONE' })  -- default_visibility
  highlight(0, 'bazelAttrDefaultTestonly', { fg = colors.blue,       bg = 'NONE' })  -- default_testonly
  highlight(0, 'bazelAttrDefaultDeprecation', { fg = colors.blue,    bg = 'NONE' })  -- default_deprecation


  -----------------------------------------------------------------------------
  -- Labels and Targets

  highlight(0, 'bazelLabel',               { fg = colors.redLight,   bg = 'NONE' })  -- //path:target
  highlight(0, 'bazelLabelRepo',           { fg = colors.turquoise,  bg = 'NONE' })  -- @repo
  highlight(0, 'bazelLabelPackage',        { fg = colors.redLight,   bg = 'NONE' })  -- //path/to/package
  highlight(0, 'bazelLabelTarget',         { fg = colors.redLight,   bg = 'NONE' })  -- :target
  highlight(0, 'bazelLabelSlashes',        { fg = colors.pink,       bg = 'NONE' })  -- //
  highlight(0, 'bazelLabelColon',          { fg = colors.white,      bg = 'NONE' })  -- :
  highlight(0, 'bazelLabelAt',             { fg = colors.pink,       bg = 'NONE' })  -- @


  -----------------------------------------------------------------------------
  -- Visibility Values

  highlight(0, 'bazelVisibility',          { fg = colors.redLight,   bg = 'NONE' })  -- Visibility labels
  highlight(0, 'bazelVisPublic',           { fg = colors.redLight,   bg = 'NONE' })  -- //visibility:public
  highlight(0, 'bazelVisPrivate',          { fg = colors.redLight,   bg = 'NONE' })  -- //visibility:private


  -----------------------------------------------------------------------------
  -- WORKSPACE Functions

  highlight(0, 'bazelWorkspaceFunc',       { fg = colors.orange,     bg = 'NONE' })  -- WORKSPACE functions
  highlight(0, 'bazelWorkspace',           { fg = colors.orange,     bg = 'NONE' })  -- workspace
  highlight(0, 'bazelBind',                { fg = colors.orange,     bg = 'NONE' })  -- bind
  highlight(0, 'bazelRegisterToolchains', { fg = colors.orange,     bg = 'NONE' })  -- register_toolchains
  highlight(0, 'bazelRegisterExecutionPlatforms', { fg = colors.orange, bg = 'NONE' })  -- register_execution_platforms

  -- Repository Rules
  highlight(0, 'bazelRepoRule',            { fg = colors.turquoise,  bg = 'NONE' })  -- Repository rules
  highlight(0, 'bazelHttpArchive',         { fg = colors.turquoise,  bg = 'NONE' })  -- http_archive
  highlight(0, 'bazelHttpFile',            { fg = colors.turquoise,  bg = 'NONE' })  -- http_file
  highlight(0, 'bazelGitRepository',       { fg = colors.turquoise,  bg = 'NONE' })  -- git_repository
  highlight(0, 'bazelNewGitRepository',    { fg = colors.turquoise,  bg = 'NONE' })  -- new_git_repository
  highlight(0, 'bazelLocalRepository',     { fg = colors.turquoise,  bg = 'NONE' })  -- local_repository
  highlight(0, 'bazelNewLocalRepository',  { fg = colors.turquoise,  bg = 'NONE' })  -- new_local_repository


  -----------------------------------------------------------------------------
  -- MODULE.bazel Functions (Bzlmod)

  highlight(0, 'bazelModuleFunc',          { fg = colors.orange,     bg = 'NONE' })  -- MODULE.bazel functions
  highlight(0, 'bazelModule',              { fg = colors.orange,     bg = 'NONE' })  -- module
  highlight(0, 'bazelBazelDep',            { fg = colors.orange,     bg = 'NONE' })  -- bazel_dep
  highlight(0, 'bazelUseRepoRule',         { fg = colors.orange,     bg = 'NONE' })  -- use_repo_rule
  highlight(0, 'bazelUseExtension',        { fg = colors.orange,     bg = 'NONE' })  -- use_extension
  highlight(0, 'bazelUseRepo',             { fg = colors.orange,     bg = 'NONE' })  -- use_repo
  highlight(0, 'bazelRegisterToolchainsBzl', { fg = colors.orange,   bg = 'NONE' })  -- register_toolchains
  highlight(0, 'bazelSingleVersionOverride', { fg = colors.orange,   bg = 'NONE' })  -- single_version_override
  highlight(0, 'bazelMultipleVersionOverride', { fg = colors.orange, bg = 'NONE' })  -- multiple_version_override
  highlight(0, 'bazelArchiveOverride',     { fg = colors.orange,     bg = 'NONE' })  -- archive_override
  highlight(0, 'bazelGitOverride',         { fg = colors.orange,     bg = 'NONE' })  -- git_override
  highlight(0, 'bazelLocalPathOverride',   { fg = colors.orange,     bg = 'NONE' })  -- local_path_override


  -----------------------------------------------------------------------------
  -- Rule Definition (.bzl files)

  highlight(0, 'starlarkRuleDef',          { fg = colors.orange,     bg = 'NONE' })  -- rule
  highlight(0, 'starlarkProviderDef',      { fg = colors.orange,     bg = 'NONE' })  -- provider
  highlight(0, 'starlarkAspectDef',        { fg = colors.orange,     bg = 'NONE' })  -- aspect
  highlight(0, 'starlarkRepositoryRule',   { fg = colors.orange,     bg = 'NONE' })  -- repository_rule
  highlight(0, 'starlarkModuleExtension',  { fg = colors.orange,     bg = 'NONE' })  -- module_extension
  highlight(0, 'starlarkTagClass',         { fg = colors.orange,     bg = 'NONE' })  -- tag_class

  -- attr Module
  highlight(0, 'starlarkAttr',             { fg = colors.purple,     bg = 'NONE' })  -- attr
  highlight(0, 'starlarkAttrBool',         { fg = colors.orange,     bg = 'NONE' })  -- attr.bool
  highlight(0, 'starlarkAttrInt',          { fg = colors.orange,     bg = 'NONE' })  -- attr.int
  highlight(0, 'starlarkAttrIntList',      { fg = colors.orange,     bg = 'NONE' })  -- attr.int_list
  highlight(0, 'starlarkAttrLabel',        { fg = colors.orange,     bg = 'NONE' })  -- attr.label
  highlight(0, 'starlarkAttrLabelList',    { fg = colors.orange,     bg = 'NONE' })  -- attr.label_list
  highlight(0, 'starlarkAttrLabelKeyedStr',{ fg = colors.orange,     bg = 'NONE' })  -- attr.label_keyed_string_dict
  highlight(0, 'starlarkAttrOutput',       { fg = colors.orange,     bg = 'NONE' })  -- attr.output
  highlight(0, 'starlarkAttrOutputList',   { fg = colors.orange,     bg = 'NONE' })  -- attr.output_list
  highlight(0, 'starlarkAttrString',       { fg = colors.orange,     bg = 'NONE' })  -- attr.string
  highlight(0, 'starlarkAttrStringList',   { fg = colors.orange,     bg = 'NONE' })  -- attr.string_list
  highlight(0, 'starlarkAttrStringDict',   { fg = colors.orange,     bg = 'NONE' })  -- attr.string_dict
  highlight(0, 'starlarkAttrStringListDict', { fg = colors.orange,   bg = 'NONE' })  -- attr.string_list_dict

  -- ctx (Context) Object
  highlight(0, 'starlarkCtx',              { fg = colors.purple,     bg = 'NONE' })  -- ctx
  highlight(0, 'starlarkCtxActions',       { fg = colors.orange,     bg = 'NONE' })  -- ctx.actions
  highlight(0, 'starlarkCtxAttr',          { fg = colors.orange,     bg = 'NONE' })  -- ctx.attr
  highlight(0, 'starlarkCtxFile',          { fg = colors.orange,     bg = 'NONE' })  -- ctx.file
  highlight(0, 'starlarkCtxFiles',         { fg = colors.orange,     bg = 'NONE' })  -- ctx.files
  highlight(0, 'starlarkCtxExecutable',    { fg = colors.orange,     bg = 'NONE' })  -- ctx.executable
  highlight(0, 'starlarkCtxOutputs',       { fg = colors.orange,     bg = 'NONE' })  -- ctx.outputs
  highlight(0, 'starlarkCtxBinDir',        { fg = colors.orange,     bg = 'NONE' })  -- ctx.bin_dir
  highlight(0, 'starlarkCtxGenfilesDir',   { fg = colors.orange,     bg = 'NONE' })  -- ctx.genfiles_dir
  highlight(0, 'starlarkCtxLabel',         { fg = colors.orange,     bg = 'NONE' })  -- ctx.label
  highlight(0, 'starlarkCtxWorkspaceName', { fg = colors.orange,     bg = 'NONE' })  -- ctx.workspace_name

  -- actions
  highlight(0, 'starlarkActionsRun',       { fg = colors.orange,     bg = 'NONE' })  -- actions.run
  highlight(0, 'starlarkActionsRunShell',  { fg = colors.orange,     bg = 'NONE' })  -- actions.run_shell
  highlight(0, 'starlarkActionsWrite',     { fg = colors.orange,     bg = 'NONE' })  -- actions.write
  highlight(0, 'starlarkActionsDeclareFile', { fg = colors.orange,   bg = 'NONE' })  -- actions.declare_file
  highlight(0, 'starlarkActionsDeclareDir',{ fg = colors.orange,     bg = 'NONE' })  -- actions.declare_directory
  highlight(0, 'starlarkActionsExpand',    { fg = colors.orange,     bg = 'NONE' })  -- actions.expand_template
  highlight(0, 'starlarkActionsSymlink',   { fg = colors.orange,     bg = 'NONE' })  -- actions.symlink


  -----------------------------------------------------------------------------
  -- String Methods

  highlight(0, 'starlarkStringMethod',     { fg = colors.orange,     bg = 'NONE' })  -- String methods
  highlight(0, 'starlarkStrCapitalize',    { fg = colors.orange,     bg = 'NONE' })  -- capitalize
  highlight(0, 'starlarkStrCount',         { fg = colors.orange,     bg = 'NONE' })  -- count
  highlight(0, 'starlarkStrEndswith',      { fg = colors.orange,     bg = 'NONE' })  -- endswith
  highlight(0, 'starlarkStrFind',          { fg = colors.orange,     bg = 'NONE' })  -- find
  highlight(0, 'starlarkStrFormat',        { fg = colors.orange,     bg = 'NONE' })  -- format
  highlight(0, 'starlarkStrIndex',         { fg = colors.orange,     bg = 'NONE' })  -- index
  highlight(0, 'starlarkStrJoin',          { fg = colors.orange,     bg = 'NONE' })  -- join
  highlight(0, 'starlarkStrLower',         { fg = colors.orange,     bg = 'NONE' })  -- lower
  highlight(0, 'starlarkStrUpper',         { fg = colors.orange,     bg = 'NONE' })  -- upper
  highlight(0, 'starlarkStrReplace',       { fg = colors.orange,     bg = 'NONE' })  -- replace
  highlight(0, 'starlarkStrSplit',         { fg = colors.orange,     bg = 'NONE' })  -- split
  highlight(0, 'starlarkStrStartswith',    { fg = colors.orange,     bg = 'NONE' })  -- startswith
  highlight(0, 'starlarkStrStrip',         { fg = colors.orange,     bg = 'NONE' })  -- strip
  highlight(0, 'starlarkStrLstrip',        { fg = colors.orange,     bg = 'NONE' })  -- lstrip
  highlight(0, 'starlarkStrRstrip',        { fg = colors.orange,     bg = 'NONE' })  -- rstrip
  highlight(0, 'starlarkStrPartition',     { fg = colors.orange,     bg = 'NONE' })  -- partition
  highlight(0, 'starlarkStrRpartition',    { fg = colors.orange,     bg = 'NONE' })  -- rpartition
  highlight(0, 'starlarkStrTitle',         { fg = colors.orange,     bg = 'NONE' })  -- title
  highlight(0, 'starlarkStrRemoveprefix',  { fg = colors.orange,     bg = 'NONE' })  -- removeprefix
  highlight(0, 'starlarkStrRemovesuffix',  { fg = colors.orange,     bg = 'NONE' })  -- removesuffix


  -----------------------------------------------------------------------------
  -- List Methods

  highlight(0, 'starlarkListMethod',       { fg = colors.orange,     bg = 'NONE' })  -- List methods
  highlight(0, 'starlarkListAppend',       { fg = colors.orange,     bg = 'NONE' })  -- append
  highlight(0, 'starlarkListClear',        { fg = colors.orange,     bg = 'NONE' })  -- clear
  highlight(0, 'starlarkListExtend',       { fg = colors.orange,     bg = 'NONE' })  -- extend
  highlight(0, 'starlarkListIndex',        { fg = colors.orange,     bg = 'NONE' })  -- index
  highlight(0, 'starlarkListInsert',       { fg = colors.orange,     bg = 'NONE' })  -- insert
  highlight(0, 'starlarkListPop',          { fg = colors.orange,     bg = 'NONE' })  -- pop
  highlight(0, 'starlarkListRemove',       { fg = colors.orange,     bg = 'NONE' })  -- remove


  -----------------------------------------------------------------------------
  -- Dict Methods

  highlight(0, 'starlarkDictMethod',       { fg = colors.orange,     bg = 'NONE' })  -- Dict methods
  highlight(0, 'starlarkDictClear',        { fg = colors.orange,     bg = 'NONE' })  -- clear
  highlight(0, 'starlarkDictGet',          { fg = colors.orange,     bg = 'NONE' })  -- get
  highlight(0, 'starlarkDictItems',        { fg = colors.orange,     bg = 'NONE' })  -- items
  highlight(0, 'starlarkDictKeys',         { fg = colors.orange,     bg = 'NONE' })  -- keys
  highlight(0, 'starlarkDictPop',          { fg = colors.orange,     bg = 'NONE' })  -- pop
  highlight(0, 'starlarkDictPopitem',      { fg = colors.orange,     bg = 'NONE' })  -- popitem
  highlight(0, 'starlarkDictSetdefault',   { fg = colors.orange,     bg = 'NONE' })  -- setdefault
  highlight(0, 'starlarkDictUpdate',       { fg = colors.orange,     bg = 'NONE' })  -- update
  highlight(0, 'starlarkDictValues',       { fg = colors.orange,     bg = 'NONE' })  -- values


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.starlark)

  -- Comments
  highlight(0, '@comment.starlark',              { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, '@string.documentation.starlark', { fg = colors.red,        bg = 'NONE' })  -- Docstrings

  -- Keywords
  highlight(0, '@keyword.starlark',              { fg = colors.pink,       bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.starlark',     { fg = colors.pink,       bg = 'NONE' })  -- def, lambda
  highlight(0, '@keyword.operator.starlark',     { fg = colors.pink,       bg = 'NONE' })  -- and, or, not, in
  highlight(0, '@keyword.return.starlark',       { fg = colors.pink,       bg = 'NONE' })  -- return
  highlight(0, '@keyword.conditional.starlark',  { fg = colors.pink,       bg = 'NONE' })  -- if, elif, else
  highlight(0, '@keyword.repeat.starlark',       { fg = colors.pink,       bg = 'NONE' })  -- for, break, continue
  highlight(0, '@keyword.import.starlark',       { fg = colors.pink,       bg = 'NONE' })  -- load

  -- Types
  highlight(0, '@type.starlark',                 { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@type.builtin.starlark',         { fg = colors.turquoise,  bg = 'NONE' })  -- Built-in types

  -- Variables
  highlight(0, '@variable.starlark',             { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, '@variable.builtin.starlark',     { fg = colors.purple,     bg = 'NONE' })  -- self, cls
  highlight(0, '@variable.member.starlark',      { fg = colors.blue,       bg = 'NONE' })  -- Attributes
  highlight(0, '@variable.parameter.starlark',   { fg = colors.purple,     bg = 'NONE' })  -- Parameters

  -- Functions
  highlight(0, '@function.starlark',             { fg = colors.orange,     bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.starlark',        { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.starlark',     { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.method.call.starlark', { fg = colors.orange,     bg = 'NONE' })  -- Method calls
  highlight(0, '@function.macro.starlark',       { fg = colors.orange,     bg = 'NONE' })  -- Type conversions
  highlight(0, '@constructor.starlark',          { fg = colors.turquoise,  bg = 'NONE' })  -- Constructors

  -- Decorators
  highlight(0, '@attribute.starlark',            { fg = colors.pink,       bg = 'NONE' })  -- Decorators
  highlight(0, '@attribute.builtin.starlark',    { fg = colors.pink,       bg = 'NONE' })  -- Built-in decorators

  -- Strings
  highlight(0, '@string.starlark',               { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.starlark',        { fg = colors.pink,       bg = 'NONE' })  -- Escapes

  -- Numbers
  highlight(0, '@number.starlark',               { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.starlark',         { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.starlark',              { fg = colors.blue,       bg = 'NONE' })  -- True, False

  -- Constants
  highlight(0, '@constant.starlark',             { fg = colors.blue,       bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.starlark',     { fg = colors.blue,       bg = 'NONE' })  -- None, etc.

  -- None
  highlight(0, '@none.starlark',                 { fg = colors.blue,       bg = 'NONE' })  -- None

  -- Operators
  highlight(0, '@operator.starlark',             { fg = colors.white,      bg = 'NONE' })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.starlark',  { fg = colors.white,      bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.starlark',{ fg = colors.white,      bg = 'NONE' })  -- , . : ;
  highlight(0, '@punctuation.special.starlark',  { fg = colors.pink,       bg = 'NONE' })  -- F-string braces


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.starlark)

  highlight(0, '@lsp.type.variable.starlark',    { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.starlark',   { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.starlark',    { fg = colors.blue,       bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.starlark',    { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.string.starlark',      { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.starlark',      { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.keyword.starlark',     { fg = colors.pink,       bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.comment.starlark',     { fg = colors.red,        bg = 'NONE' })  -- Comments


  -----------------------------------------------------------------------------
  -- Error Highlighting

  highlight(0, 'starlarkError',            { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax errors
  highlight(0, 'bazelError',               { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Bazel errors
end

return starlark
