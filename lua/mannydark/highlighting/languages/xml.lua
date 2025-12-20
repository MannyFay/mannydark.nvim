-------------------------------------------------------------------------------
-- XML Files
-- Highlighting for .xml, .xsd, .xslt, .svg, .plist, .xaml files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local xml       = {}


-------------------------------------------------------------------------------
-- Settings

xml.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Tags
  highlight(0, 'xmlTag',                { fg = colors.blue,       bg = 'NONE' })  -- < > tag delimiters
  highlight(0, 'xmlTagName',            { fg = colors.blue,       bg = 'NONE' })  -- Tag names
  highlight(0, 'xmlEndTag',             { fg = colors.blue,       bg = 'NONE' })  -- </tag> closing tags

  -- Attributes
  highlight(0, 'xmlAttrib',             { fg = colors.turquoise,  bg = 'NONE' })  -- Attribute names
  highlight(0, 'xmlAttribPunct',        { fg = colors.white,      bg = 'NONE' })  -- Punctuation in attributes
  highlight(0, 'xmlEqual',              { fg = colors.white,      bg = 'NONE' })  -- = in attributes

  -- Attribute Values / Strings
  highlight(0, 'xmlString',             { link = "String" })  -- "attribute values"

  -- Namespaces
  highlight(0, 'xmlNamespace',          { fg = colors.pink,       bg = 'NONE' })  -- Namespace prefixes (xmlns:)
  highlight(0, 'xmlNamespaceColon',     { fg = colors.white,      bg = 'NONE' })  -- Colon in namespace

  -- Entity References
  highlight(0, 'xmlEntity',             { fg = colors.pink,       bg = 'NONE' })  -- &entity;
  highlight(0, 'xmlEntityPunct',        { fg = colors.pink,       bg = 'NONE' })  -- & and ; in entities

  -- Processing Instructions
  highlight(0, 'xmlProcessing',         { fg = colors.gray,       bg = 'NONE' })  -- <?...?> content
  highlight(0, 'xmlProcessingDelim',    { link = "Delimiter" })  -- <? and ?>

  -- XML Declaration
  highlight(0, 'xmlDecl',               { fg = colors.gray,       bg = 'NONE' })  -- <?xml ... ?>
  highlight(0, 'xmlDeclVersion',        { fg = colors.turquoise,  bg = 'NONE' })  -- version attribute
  highlight(0, 'xmlDeclEncoding',       { fg = colors.turquoise,  bg = 'NONE' })  -- encoding attribute

  -- DOCTYPE
  highlight(0, 'xmlDocType',            { link = "Type" })  -- DOCTYPE declaration
  highlight(0, 'xmlDocTypeDecl',        { link = "Type" })  -- DOCTYPE content
  highlight(0, 'xmlDocTypeKeyword',     { link = "Keyword" })  -- DOCTYPE, PUBLIC, SYSTEM

  -- DTD (Document Type Definition)
  highlight(0, 'xmlInlineDTD',          { fg = colors.gray,       bg = 'NONE' })  -- Inline DTD content

  -- CDATA Sections
  highlight(0, 'xmlCdata',              { fg = colors.redLight,   bg = 'NONE' })  -- CDATA content
  highlight(0, 'xmlCdataStart',         { fg = colors.gray,       bg = 'NONE' })  -- <![CDATA[
  highlight(0, 'xmlCdataEnd',           { fg = colors.gray,       bg = 'NONE' })  -- ]]>
  highlight(0, 'xmlCdataCdata',         { fg = colors.gray,       bg = 'NONE' })  -- CDATA keyword

  -- Comments
  highlight(0, 'xmlComment',            { link = "Comment" })  -- <!-- comment -->
  highlight(0, 'xmlCommentStart',       { link = "Comment" })  -- <!--
  highlight(0, 'xmlCommentEnd',         { link = "Comment" })  -- -->
  highlight(0, 'xmlCommentPart',        { link = "Comment" })  -- Comment content
  highlight(0, 'xmlCommentError',       { link = "Comment" })  -- Invalid comment chars
  highlight(0, 'xmlTodo',               { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Text Content
  highlight(0, 'xmlRegion',             { fg = colors.white,      bg = 'NONE' })  -- Text between tags

  -- Errors
  highlight(0, 'xmlError',              { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Illegal characters


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.xml)

  -- Tags
  highlight(0, '@tag.xml',                  { fg = colors.blue,       bg = 'NONE' })  -- Tag names
  highlight(0, '@tag.delimiter.xml',        { link = "Delimiter" })  -- < > </ />
  highlight(0, '@tag.attribute.xml',        { fg = colors.turquoise,  bg = 'NONE' })  -- Attribute names

  -- Attributes (alternative captures)
  highlight(0, '@attribute.xml',            { fg = colors.turquoise,  bg = 'NONE' })  -- Attributes

  -- Strings
  highlight(0, '@string.xml',               { link = "String" })  -- Attribute values
  highlight(0, '@string.special.xml',       { link = "String" })  -- Special strings
  highlight(0, '@string.special.url.xml',   { link = "String" })  -- URLs in attributes

  -- Namespaces
  highlight(0, '@module.xml',               { fg = colors.pink,       bg = 'NONE' })  -- Namespace prefixes
  highlight(0, '@label.xml',                { fg = colors.pink,       bg = 'NONE' })  -- Labels

  -- Constants / Entities
  highlight(0, '@constant.xml',             { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.xml',     { link = "Constant" })  -- Built-in entities
  highlight(0, '@character.xml',            { fg = colors.pink,       bg = 'NONE' })  -- Character references
  highlight(0, '@character.special.xml',    { fg = colors.pink,       bg = 'NONE' })  -- &amp; &lt; &gt; etc.

  -- Keywords / Directives
  highlight(0, '@keyword.xml',              { link = "Keyword" })  -- Keywords
  highlight(0, '@keyword.directive.xml',    { link = "Keyword" })  -- Processing instructions
  highlight(0, '@keyword.directive.define.xml', { link = "Keyword" })  -- DTD definitions
  highlight(0, '@keyword.modifier.xml',     { link = "Keyword" })  -- Modifiers

  -- Types
  highlight(0, '@type.xml',                 { link = "Type" })  -- Types
  highlight(0, '@type.builtin.xml',         { link = "Type" })  -- Built-in types
  highlight(0, '@type.definition.xml',      { link = "Type" })  -- Type definitions

  -- Operators
  highlight(0, '@operator.xml',             { link = "Operator" })  -- = operator

  -- Punctuation
  highlight(0, '@punctuation.bracket.xml',  { fg = colors.white,      bg = 'NONE' })  -- [ ] { }
  highlight(0, '@punctuation.delimiter.xml', { link = "Delimiter" })  -- Delimiters

  -- Functions/Macros
  highlight(0, '@function.macro.xml',       { link = "Function" })  -- Macro-like constructs

  -- Numbers / Booleans
  highlight(0, '@number.xml',               { link = "Number" })  -- Numbers
  highlight(0, '@boolean.xml',              { link = "Boolean" })  -- Booleans

  -- Comments
  highlight(0, '@comment.xml',              { link = "Comment" })  -- Comments

  -- Markup
  highlight(0, '@markup.raw.xml',           { fg = colors.redLight,   bg = 'NONE' })  -- CDATA content

  -- None (transparent)
  highlight(0, '@none.xml',                 { fg = 'NONE',            bg = 'NONE' })  -- No highlighting


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.xml)

  highlight(0, '@lsp.type.property.xml',    { fg = colors.turquoise,  bg = 'NONE' })  -- Attributes
  highlight(0, '@lsp.type.string.xml',      { link = "String" })  -- Strings
  highlight(0, '@lsp.type.keyword.xml',     { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.comment.xml',     { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.namespace.xml',   { fg = colors.pink,       bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.type.xml',        { fg = colors.turquoise,  bg = 'NONE' })  -- Types


  -----------------------------------------------------------------------------
  -- Common XML Entities

  highlight(0, 'xmlEntityAmp',          { fg = colors.pink,       bg = 'NONE' })  -- &amp;
  highlight(0, 'xmlEntityLt',           { fg = colors.pink,       bg = 'NONE' })  -- &lt;
  highlight(0, 'xmlEntityGt',           { fg = colors.pink,       bg = 'NONE' })  -- &gt;
  highlight(0, 'xmlEntityQuot',         { fg = colors.pink,       bg = 'NONE' })  -- &quot;
  highlight(0, 'xmlEntityApos',         { fg = colors.pink,       bg = 'NONE' })  -- &apos;
  highlight(0, 'xmlEntityNumeric',      { fg = colors.pink,       bg = 'NONE' })  -- &#123; &#x7B;


  -----------------------------------------------------------------------------
  -- SVG Specific

  highlight(0, 'svgTag',                { fg = colors.blue,       bg = 'NONE' })  -- SVG tags
  highlight(0, 'svgTagName',            { fg = colors.blue,       bg = 'NONE' })  -- svg, path, rect, circle, etc.
  highlight(0, 'svgAttrib',             { fg = colors.turquoise,  bg = 'NONE' })  -- SVG attributes
  highlight(0, 'svgPath',               { fg = colors.redLight,   bg = 'NONE' })  -- Path d="" data
  highlight(0, 'svgTransform',          { fg = colors.orange,     bg = 'NONE' })  -- transform=""
  highlight(0, 'svgStyle',              { fg = colors.redLight,   bg = 'NONE' })  -- style=""
  highlight(0, 'svgColor',              { fg = colors.greenLight, bg = 'NONE' })  -- Color values
  highlight(0, 'svgNumber',             { link = "Number" })  -- Numeric values
  highlight(0, 'svgUnit',               { fg = colors.turquoise,  bg = 'NONE' })  -- px, em, %, etc.


  -----------------------------------------------------------------------------
  -- XSLT Specific

  highlight(0, 'xsltTag',               { fg = colors.blue,       bg = 'NONE' })  -- xsl:* tags
  highlight(0, 'xsltTagName',           { fg = colors.blue,       bg = 'NONE' })  -- template, apply-templates, etc.
  highlight(0, 'xsltNamespace',         { fg = colors.pink,       bg = 'NONE' })  -- xsl: prefix
  highlight(0, 'xsltAttrib',            { fg = colors.turquoise,  bg = 'NONE' })  -- match, select, name
  highlight(0, 'xsltXPath',             { fg = colors.orange,     bg = 'NONE' })  -- XPath expressions
  highlight(0, 'xsltVariable',          { link = "Variable" })  -- Variables $var


  -----------------------------------------------------------------------------
  -- XSD (XML Schema) Specific

  highlight(0, 'xsdTag',                { fg = colors.blue,       bg = 'NONE' })  -- xs:* or xsd:* tags
  highlight(0, 'xsdTagName',            { fg = colors.blue,       bg = 'NONE' })  -- element, complexType, etc.
  highlight(0, 'xsdNamespace',          { fg = colors.pink,       bg = 'NONE' })  -- xs: or xsd: prefix
  highlight(0, 'xsdAttrib',             { fg = colors.turquoise,  bg = 'NONE' })  -- name, type, minOccurs, etc.
  highlight(0, 'xsdType',               { link = "Type" })  -- Type definitions
  highlight(0, 'xsdBuiltinType',        { link = "Type" })  -- xs:string, xs:integer, etc.


  -----------------------------------------------------------------------------
  -- HTML-like XML (XHTML, etc.)

  highlight(0, 'xmlHtmlTag',            { fg = colors.blue,       bg = 'NONE' })  -- HTML tags in XML
  highlight(0, 'xmlHtmlAttrib',         { fg = colors.turquoise,  bg = 'NONE' })  -- HTML attributes


  -----------------------------------------------------------------------------
  -- Android XML (layout, manifest)

  highlight(0, 'androidTag',            { fg = colors.blue,       bg = 'NONE' })  -- Android tags
  highlight(0, 'androidAttrib',         { fg = colors.turquoise,  bg = 'NONE' })  -- android:* attributes
  highlight(0, 'androidNamespace',      { fg = colors.pink,       bg = 'NONE' })  -- android: prefix
  highlight(0, 'androidResource',       { fg = colors.orange,     bg = 'NONE' })  -- @string/, @drawable/, etc.
  highlight(0, 'androidId',             { fg = colors.purple,     bg = 'NONE' })  -- @+id/, @id/


  -----------------------------------------------------------------------------
  -- Maven POM XML

  highlight(0, 'pomTag',                { fg = colors.blue,       bg = 'NONE' })  -- POM tags
  highlight(0, 'pomGroupId',            { fg = colors.turquoise,  bg = 'NONE' })  -- groupId
  highlight(0, 'pomArtifactId',         { fg = colors.turquoise,  bg = 'NONE' })  -- artifactId
  highlight(0, 'pomVersion',            { fg = colors.greenLight, bg = 'NONE' })  -- version
  highlight(0, 'pomDependency',         { fg = colors.blue,       bg = 'NONE' })  -- dependency tags
  highlight(0, 'pomPlugin',             { fg = colors.blue,       bg = 'NONE' })  -- plugin tags


  -----------------------------------------------------------------------------
  -- .csproj / .vbproj (MSBuild XML)

  highlight(0, 'msbuildTag',            { fg = colors.blue,       bg = 'NONE' })  -- MSBuild tags
  highlight(0, 'msbuildProperty',       { fg = colors.turquoise,  bg = 'NONE' })  -- Property names
  highlight(0, 'msbuildCondition',      { fg = colors.orange,     bg = 'NONE' })  -- Condition=""
  highlight(0, 'msbuildVariable',       { link = "Variable" })  -- $(Variable)
  highlight(0, 'msbuildItemGroup',      { fg = colors.blue,       bg = 'NONE' })  -- ItemGroup


  -----------------------------------------------------------------------------
  -- XAML (WPF/UWP/MAUI)

  highlight(0, 'xamlTag',               { fg = colors.blue,       bg = 'NONE' })  -- XAML tags
  highlight(0, 'xamlAttrib',            { fg = colors.turquoise,  bg = 'NONE' })  -- XAML attributes
  highlight(0, 'xamlNamespace',         { fg = colors.pink,       bg = 'NONE' })  -- x:, local:, etc.
  highlight(0, 'xamlBinding',           { fg = colors.orange,     bg = 'NONE' })  -- {Binding ...}
  highlight(0, 'xamlStaticResource',    { fg = colors.orange,     bg = 'NONE' })  -- {StaticResource ...}
  highlight(0, 'xamlMarkupExtension',   { fg = colors.orange,     bg = 'NONE' })  -- Markup extensions


  -----------------------------------------------------------------------------
  -- Plist (Apple Property List)

  highlight(0, 'plistTag',              { fg = colors.blue,       bg = 'NONE' })  -- plist tags
  highlight(0, 'plistKey',              { fg = colors.blue,       bg = 'NONE' })  -- <key>
  highlight(0, 'plistString',           { link = "String" })  -- <string>
  highlight(0, 'plistInteger',          { fg = colors.greenLight, bg = 'NONE' })  -- <integer>
  highlight(0, 'plistReal',             { fg = colors.greenLight, bg = 'NONE' })  -- <real>
  highlight(0, 'plistBoolean',          { link = "Boolean" })  -- <true/>, <false/>
  highlight(0, 'plistDate',             { fg = colors.greenLight, bg = 'NONE' })  -- <date>
  highlight(0, 'plistData',             { fg = colors.redLight,   bg = 'NONE' })  -- <data>
  highlight(0, 'plistDict',             { fg = colors.blue,       bg = 'NONE' })  -- <dict>
  highlight(0, 'plistArray',            { fg = colors.blue,       bg = 'NONE' })  -- <array>
end

return xml
