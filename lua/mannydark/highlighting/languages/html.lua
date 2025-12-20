-------------------------------------------------------------------------------
-- HTML Files
-- Highlighting for .html, .htm, .xhtml, .shtml files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local html      = {}


-------------------------------------------------------------------------------
-- Settings

html.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Tags
  highlight(0, 'htmlTag',              { fg = colors.white,      bg = 'NONE'            })  -- < > delimiters
  highlight(0, 'htmlEndTag',           { fg = colors.white,      bg = 'NONE'            })  -- </tag>
  highlight(0, 'htmlTagN',             { fg = colors.blue,       bg = 'NONE'            })  -- Tag name
  highlight(0, 'htmlTagName',          { fg = colors.blue,       bg = 'NONE'            })  -- Tag name
  highlight(0, 'htmlSpecialTagName',   { fg = colors.blue,       bg = 'NONE'            })  -- script, style, etc.
  highlight(0, 'htmlTagError',         { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Invalid tag

  -- Attributes
  highlight(0, 'htmlArg',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Attribute names
  highlight(0, 'htmlString',           { link = "String" })  -- "attribute values"
  highlight(0, 'htmlValue',            { fg = colors.redLight,   bg = 'NONE'            })  -- Attribute values
  highlight(0, 'htmlEqual',            { fg = colors.white,      bg = 'NONE'            })  -- = in attributes

  -- Event Handlers
  highlight(0, 'htmlEvent',            { fg = colors.orange,     bg = 'NONE'            })  -- onclick, onload, etc.
  highlight(0, 'htmlEventSQ',          { fg = colors.redLight,   bg = 'NONE'            })  -- Event in single quotes
  highlight(0, 'htmlEventDQ',          { fg = colors.redLight,   bg = 'NONE'            })  -- Event in double quotes

  -- DOCTYPE
  highlight(0, 'htmlPreProc',          { fg = colors.gray,       bg = 'NONE'            })  -- <!DOCTYPE>
  highlight(0, 'htmlPreStmt',          { fg = colors.gray,       bg = 'NONE'            })  -- DOCTYPE content
  highlight(0, 'htmlPreError',         { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Invalid DOCTYPE
  highlight(0, 'htmlPreAttr',          { fg = colors.turquoise,  bg = 'NONE'            })  -- DOCTYPE attributes
  highlight(0, 'htmlPreProcAttrName',  { fg = colors.turquoise,  bg = 'NONE'            })  -- Attribute name in DOCTYPE
  highlight(0, 'htmlPreProcAttrError', { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Invalid attribute
  highlight(0, '@spell.html',            { link = "Normal" })  -- Regular text.

  -- Comments
  highlight(0, 'htmlComment',            { link = "Comment" })  -- <!-- comment -->
  highlight(0, 'htmlCommentPart',        { link = "Comment" })  -- Comment content
  highlight(0, 'htmlCommentError',       { link = "Comment" })  -- Invalid -- in comment
  highlight(0, 'htmlCommentNested',      { link = "Comment" })  -- Nested comment
  highlight(0, 'htmlTodo',               { link = "Comment" })  -- TODO, FIXME, XXX
  highlight(0, 'htmlCssStyleComment',    { link = "Comment" })  -- CSS comment in style
  highlight(0, '@comment.html',          { link = "Comment" })  -- <!-- -->
  highlight(0, '@lsp.type.comment.html', { link = "Comment" })  -- Comments

  -- Special Characters / Entities
  highlight(0, 'htmlSpecialChar',      { fg = colors.pink,       bg = 'NONE'            })  -- &nbsp; &amp; etc.
  highlight(0, 'htmlEntity',           { fg = colors.pink,       bg = 'NONE'            })  -- Named entities
  highlight(0, 'htmlEntityNumber',     { link = "Number" })  -- &#123; &#x7B;

  -- Headings
  highlight(0, 'htmlH1',               { fg = colors.white,      bg = 'NONE', bold = true })  -- <h1>
  highlight(0, 'htmlH2',               { fg = colors.white,      bg = 'NONE', bold = true })  -- <h2>
  highlight(0, 'htmlH3',               { fg = colors.white,      bg = 'NONE', bold = true })  -- <h3>
  highlight(0, 'htmlH4',               { fg = colors.white,      bg = 'NONE', bold = true })  -- <h4>
  highlight(0, 'htmlH5',               { fg = colors.white,      bg = 'NONE', bold = true })  -- <h5>
  highlight(0, 'htmlH6',               { fg = colors.white,      bg = 'NONE', bold = true })  -- <h6>
  highlight(0, 'htmlTitle',            { fg = colors.white,      bg = 'NONE', bold = true })  -- <title>

  -- Text Formatting
  highlight(0, 'htmlBold',             { fg = colors.white,      bg = 'NONE', bold = true })  -- <b>, <strong>
  highlight(0, 'htmlItalic',           { fg = colors.white,      bg = 'NONE', italic = true })  -- <i>, <em>
  highlight(0, 'htmlUnderline',        { fg = colors.white,      bg = 'NONE', underline = true })  -- <u>
  highlight(0, 'htmlStrike',           { fg = colors.white,      bg = 'NONE', strikethrough = true })  -- <s>, <strike>, <del>
  highlight(0, 'htmlBoldItalic',       { fg = colors.white,      bg = 'NONE', bold = true, italic = true })
  highlight(0, 'htmlBoldUnderline',    { fg = colors.white,      bg = 'NONE', bold = true, underline = true })
  highlight(0, 'htmlBoldUnderlineItalic', { fg = colors.white,   bg = 'NONE', bold = true, underline = true, italic = true })
  highlight(0, 'htmlItalicBold',       { fg = colors.white,      bg = 'NONE', italic = true, bold = true })
  highlight(0, 'htmlItalicUnderline',  { fg = colors.white,      bg = 'NONE', italic = true, underline = true })
  highlight(0, 'htmlItalicUnderlineBold', { fg = colors.white,   bg = 'NONE', italic = true, underline = true, bold = true })
  highlight(0, 'htmlUnderlineBold',    { fg = colors.white,      bg = 'NONE', underline = true, bold = true })
  highlight(0, 'htmlUnderlineItalic',  { fg = colors.white,      bg = 'NONE', underline = true, italic = true })
  highlight(0, 'htmlUnderlineItalicBold', { fg = colors.white,   bg = 'NONE', underline = true, italic = true, bold = true })

  -- Links
  highlight(0, 'htmlLink',             { fg = colors.blueLink,   bg = 'NONE', underline = true })  -- <a>
  highlight(0, 'htmlAnchor',           { fg = colors.blueLink,   bg = 'NONE'            })  -- Anchor point

  -- Structural Content
  highlight(0, 'htmlHead',             { fg = colors.white,      bg = 'NONE'            })  -- <head> content
  highlight(0, 'htmlBody',             { fg = colors.white,      bg = 'NONE'            })  -- <body> content
  highlight(0, 'htmlLeadingSpace',     { fg = colors.white,      bg = 'NONE'            })  -- Leading whitespace

  -- Script and Style Tags
  highlight(0, 'htmlScriptTag',        { fg = colors.blue,       bg = 'NONE'            })  -- <script>
  highlight(0, 'htmlStyleTag',         { fg = colors.blue,       bg = 'NONE'            })  -- <style>
  highlight(0, 'htmlTemplateTag',      { fg = colors.blue,       bg = 'NONE'            })  -- <template>

  -- Embedded Content
  highlight(0, 'javaScript',           { fg = colors.white,      bg = 'NONE'            })  -- Embedded JavaScript
  highlight(0, 'javaScriptExpression', { fg = colors.white,      bg = 'NONE'            })  -- JS expressions
  highlight(0, 'htmlCssDefinition',    { fg = colors.white,      bg = 'NONE'            })  -- Embedded CSS

  -- Errors
  highlight(0, 'htmlError',            { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax error
  highlight(0, 'htmlFold',             { fg = colors.gray,       bg = 'NONE'            })  -- Folded content


  -----------------------------------------------------------------------------
  -- HTML5 Semantic Elements

  -- Document Structure
  highlight(0, 'htmlTagHeader',        { fg = colors.blue,       bg = 'NONE'            })  -- <header>
  highlight(0, 'htmlTagFooter',        { fg = colors.blue,       bg = 'NONE'            })  -- <footer>
  highlight(0, 'htmlTagMain',          { fg = colors.blue,       bg = 'NONE'            })  -- <main>
  highlight(0, 'htmlTagNav',           { fg = colors.blue,       bg = 'NONE'            })  -- <nav>
  highlight(0, 'htmlTagAside',         { fg = colors.blue,       bg = 'NONE'            })  -- <aside>
  highlight(0, 'htmlTagSection',       { fg = colors.blue,       bg = 'NONE'            })  -- <section>
  highlight(0, 'htmlTagArticle',       { fg = colors.blue,       bg = 'NONE'            })  -- <article>

  -- Media Elements
  highlight(0, 'htmlTagAudio',         { fg = colors.blue,       bg = 'NONE'            })  -- <audio>
  highlight(0, 'htmlTagVideo',         { fg = colors.blue,       bg = 'NONE'            })  -- <video>
  highlight(0, 'htmlTagSource',        { fg = colors.blue,       bg = 'NONE'            })  -- <source>
  highlight(0, 'htmlTagTrack',         { fg = colors.blue,       bg = 'NONE'            })  -- <track>
  highlight(0, 'htmlTagPicture',       { fg = colors.blue,       bg = 'NONE'            })  -- <picture>
  highlight(0, 'htmlTagCanvas',        { fg = colors.blue,       bg = 'NONE'            })  -- <canvas>
  highlight(0, 'htmlTagEmbed',         { fg = colors.blue,       bg = 'NONE'            })  -- <embed>

  -- Interactive Elements
  highlight(0, 'htmlTagDialog',        { fg = colors.blue,       bg = 'NONE'            })  -- <dialog>
  highlight(0, 'htmlTagDetails',       { fg = colors.blue,       bg = 'NONE'            })  -- <details>
  highlight(0, 'htmlTagSummary',       { fg = colors.blue,       bg = 'NONE'            })  -- <summary>

  -- Form Elements
  highlight(0, 'htmlTagDatalist',      { fg = colors.blue,       bg = 'NONE'            })  -- <datalist>
  highlight(0, 'htmlTagOutput',        { fg = colors.blue,       bg = 'NONE'            })  -- <output>
  highlight(0, 'htmlTagProgress',      { fg = colors.blue,       bg = 'NONE'            })  -- <progress>
  highlight(0, 'htmlTagMeter',         { fg = colors.blue,       bg = 'NONE'            })  -- <meter>

  -- Figure Elements
  highlight(0, 'htmlTagFigure',        { fg = colors.blue,       bg = 'NONE'            })  -- <figure>
  highlight(0, 'htmlTagFigcaption',    { fg = colors.blue,       bg = 'NONE'            })  -- <figcaption>

  -- Ruby Annotation (for East Asian typography)
  highlight(0, 'htmlTagRuby',          { fg = colors.blue,       bg = 'NONE'            })  -- <ruby>
  highlight(0, 'htmlTagRt',            { fg = colors.blue,       bg = 'NONE'            })  -- <rt>
  highlight(0, 'htmlTagRp',            { fg = colors.blue,       bg = 'NONE'            })  -- <rp>
  highlight(0, 'htmlTagRb',            { fg = colors.blue,       bg = 'NONE'            })  -- <rb>
  highlight(0, 'htmlTagRtc',           { fg = colors.blue,       bg = 'NONE'            })  -- <rtc>

  -- Other HTML5 Elements
  highlight(0, 'htmlTagMark',          { fg = colors.blue,       bg = 'NONE'            })  -- <mark>
  highlight(0, 'htmlTagTime',          { fg = colors.blue,       bg = 'NONE'            })  -- <time>
  highlight(0, 'htmlTagData',          { fg = colors.blue,       bg = 'NONE'            })  -- <data>
  highlight(0, 'htmlTagWbr',           { fg = colors.blue,       bg = 'NONE'            })  -- <wbr>
  highlight(0, 'htmlTagBdi',           { fg = colors.blue,       bg = 'NONE'            })  -- <bdi>
  highlight(0, 'htmlTagSlot',          { fg = colors.blue,       bg = 'NONE'            })  -- <slot>
  highlight(0, 'htmlTagSearch',        { fg = colors.blue,       bg = 'NONE'            })  -- <search>


  -----------------------------------------------------------------------------
  -- Deprecated/Legacy Tags

  highlight(0, 'htmlTagDeprecated',    { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- Deprecated
  highlight(0, 'htmlTagBlink',         { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- <blink>
  highlight(0, 'htmlTagMarquee',       { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- <marquee>
  highlight(0, 'htmlTagCenter',        { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- <center>
  highlight(0, 'htmlTagFont',          { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- <font>
  highlight(0, 'htmlTagNobr',          { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- <nobr>
  highlight(0, 'htmlTagSpacer',        { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- <spacer>
  highlight(0, 'htmlTagFrame',         { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- <frame>
  highlight(0, 'htmlTagFrameset',      { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- <frameset>
  highlight(0, 'htmlTagNoframes',      { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- <noframes>


  -----------------------------------------------------------------------------
  -- Special Tags (MathML, SVG)

  highlight(0, 'htmlMathTagName',      { fg = colors.blue,       bg = 'NONE'            })  -- MathML tags
  highlight(0, 'htmlSvgTagName',       { fg = colors.blue,       bg = 'NONE'            })  -- SVG tags in HTML


  -----------------------------------------------------------------------------
  -- Global Attributes

  highlight(0, 'htmlArgId',            { fg = colors.turquoise,  bg = 'NONE'            })  -- id=""
  highlight(0, 'htmlArgClass',         { fg = colors.turquoise,  bg = 'NONE'            })  -- class=""
  highlight(0, 'htmlArgStyle',         { fg = colors.turquoise,  bg = 'NONE'            })  -- style=""
  highlight(0, 'htmlArgTitle',         { fg = colors.turquoise,  bg = 'NONE'            })  -- title=""
  highlight(0, 'htmlArgLang',          { fg = colors.turquoise,  bg = 'NONE'            })  -- lang=""
  highlight(0, 'htmlArgDir',           { fg = colors.turquoise,  bg = 'NONE'            })  -- dir=""
  highlight(0, 'htmlArgHidden',        { fg = colors.turquoise,  bg = 'NONE'            })  -- hidden
  highlight(0, 'htmlArgTabindex',      { fg = colors.turquoise,  bg = 'NONE'            })  -- tabindex=""


  -----------------------------------------------------------------------------
  -- Data Attributes

  highlight(0, 'htmlArgData',          { fg = colors.purple,     bg = 'NONE'            })  -- data-* attributes


  -----------------------------------------------------------------------------
  -- ARIA Attributes

  highlight(0, 'htmlArgAria',          { fg = colors.purple,     bg = 'NONE'            })  -- aria-* attributes
  highlight(0, 'htmlArgRole',          { fg = colors.purple,     bg = 'NONE'            })  -- role=""


  -----------------------------------------------------------------------------
  -- Event Attributes

  highlight(0, 'htmlArgOnclick',       { fg = colors.orange,     bg = 'NONE'            })  -- onclick=""
  highlight(0, 'htmlArgOnload',        { fg = colors.orange,     bg = 'NONE'            })  -- onload=""
  highlight(0, 'htmlArgOnsubmit',      { fg = colors.orange,     bg = 'NONE'            })  -- onsubmit=""
  highlight(0, 'htmlArgOnchange',      { fg = colors.orange,     bg = 'NONE'            })  -- onchange=""
  highlight(0, 'htmlArgOnmouseover',   { fg = colors.orange,     bg = 'NONE'            })  -- onmouseover=""
  highlight(0, 'htmlArgOnkeydown',     { fg = colors.orange,     bg = 'NONE'            })  -- onkeydown=""
  highlight(0, 'htmlArgOnerror',       { fg = colors.orange,     bg = 'NONE'            })  -- onerror=""


  -----------------------------------------------------------------------------
  -- Form-specific Attributes

  highlight(0, 'htmlArgAction',        { fg = colors.turquoise,  bg = 'NONE'            })  -- action=""
  highlight(0, 'htmlArgMethod',        { link = "Function" })  -- method=""
  highlight(0, 'htmlArgName',          { fg = colors.turquoise,  bg = 'NONE'            })  -- name=""
  highlight(0, 'htmlArgValue',         { fg = colors.turquoise,  bg = 'NONE'            })  -- value=""
  highlight(0, 'htmlArgType',          { link = "Type" })  -- type=""
  highlight(0, 'htmlArgPlaceholder',   { fg = colors.turquoise,  bg = 'NONE'            })  -- placeholder=""
  highlight(0, 'htmlArgRequired',      { fg = colors.turquoise,  bg = 'NONE'            })  -- required
  highlight(0, 'htmlArgDisabled',      { fg = colors.turquoise,  bg = 'NONE'            })  -- disabled
  highlight(0, 'htmlArgReadonly',      { fg = colors.turquoise,  bg = 'NONE'            })  -- readonly
  highlight(0, 'htmlArgAutofocus',     { fg = colors.turquoise,  bg = 'NONE'            })  -- autofocus
  highlight(0, 'htmlArgAutocomplete',  { fg = colors.turquoise,  bg = 'NONE'            })  -- autocomplete=""


  -----------------------------------------------------------------------------
  -- Link/Resource Attributes

  highlight(0, 'htmlArgHref',          { fg = colors.turquoise,  bg = 'NONE'            })  -- href=""
  highlight(0, 'htmlArgSrc',           { fg = colors.turquoise,  bg = 'NONE'            })  -- src=""
  highlight(0, 'htmlArgAlt',           { fg = colors.turquoise,  bg = 'NONE'            })  -- alt=""
  highlight(0, 'htmlArgRel',           { fg = colors.turquoise,  bg = 'NONE'            })  -- rel=""
  highlight(0, 'htmlArgTarget',        { fg = colors.turquoise,  bg = 'NONE'            })  -- target=""
  highlight(0, 'htmlArgDownload',      { fg = colors.turquoise,  bg = 'NONE'            })  -- download
  highlight(0, 'htmlArgAsync',         { fg = colors.turquoise,  bg = 'NONE'            })  -- async
  highlight(0, 'htmlArgDefer',         { fg = colors.turquoise,  bg = 'NONE'            })  -- defer
  highlight(0, 'htmlArgIntegrity',     { fg = colors.turquoise,  bg = 'NONE'            })  -- integrity=""
  highlight(0, 'htmlArgCrossorigin',   { fg = colors.turquoise,  bg = 'NONE'            })  -- crossorigin=""


  -----------------------------------------------------------------------------
  -- Media Attributes

  highlight(0, 'htmlArgControls',      { fg = colors.turquoise,  bg = 'NONE'            })  -- controls
  highlight(0, 'htmlArgAutoplay',      { fg = colors.turquoise,  bg = 'NONE'            })  -- autoplay
  highlight(0, 'htmlArgLoop',          { fg = colors.turquoise,  bg = 'NONE'            })  -- loop
  highlight(0, 'htmlArgMuted',         { fg = colors.turquoise,  bg = 'NONE'            })  -- muted
  highlight(0, 'htmlArgPreload',       { fg = colors.turquoise,  bg = 'NONE'            })  -- preload=""
  highlight(0, 'htmlArgPoster',        { fg = colors.turquoise,  bg = 'NONE'            })  -- poster=""


  -----------------------------------------------------------------------------
  -- Dimension Attributes

  highlight(0, 'htmlArgWidth',         { fg = colors.turquoise,  bg = 'NONE'            })  -- width=""
  highlight(0, 'htmlArgHeight',        { fg = colors.turquoise,  bg = 'NONE'            })  -- height=""


  -----------------------------------------------------------------------------
  -- Microdata Attributes

  highlight(0, 'htmlArgItemscope',     { fg = colors.purple,     bg = 'NONE'            })  -- itemscope
  highlight(0, 'htmlArgItemtype',      { fg = colors.purple,     bg = 'NONE'            })  -- itemtype=""
  highlight(0, 'htmlArgItemprop',      { fg = colors.purple,     bg = 'NONE'            })  -- itemprop=""


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.html)

  -- Tags
  highlight(0, '@tag.html',                    { link = "Keyword" })  -- Tag names
  highlight(0, '@tag.builtin.html',            { link = "Keyword" })  -- Built-in tags
  highlight(0, '@tag.delimiter.html',          { link = "Ignore" })  -- < > </ />
  highlight(0, '@tag.attribute.html',          { link = "Type"})  -- Attribute names
  highlight(0, '@tag.error.html',              { link = "Error"})  -- Invalid tag

  -- Attributes (alternative captures)
  highlight(0, '@attribute.html',              { link = "Type" })  -- Attributes
  highlight(0, '@attribute.builtin.html',      { link = "Type" })  -- Built-in attributes

  -- Strings
  highlight(0, '@string.html',                 { link = "String" })  -- Attribute values
  highlight(0, '@string.special.html',         { link = "String" })  -- Special strings
  highlight(0, '@string.special.url.html',     { link = "String" })  -- URLs

  -- Constants / Entities
  highlight(0, '@constant.html',               { link = "Constant" })  -- DOCTYPE
  highlight(0, '@constant.builtin.html',       { link = "Constant" })  -- Built-in constants
  highlight(0, '@character.html',              { fg = colors.pink,       bg = 'NONE' })  -- Characters
  highlight(0, '@character.special.html',      { link = "keyword"})  -- &amp; &lt; etc.

  -- Keywords
  highlight(0, '@keyword.html',                { link = "Keyword" })  -- Keywords
  highlight(0, '@keyword.directive.html',      { link = "Keyword" })  -- DOCTYPE directive

  -- Operators
  highlight(0, '@operator.html',               { link = "Operator" })  -- = operator

  -- Punctuation
  highlight(0, '@punctuation.bracket.html',    { fg = colors.white,      bg = 'NONE' })  -- < >
  highlight(0, '@punctuation.delimiter.html',  { link = "Delimiter" })  -- Delimiters
  highlight(0, '@punctuation.special.html',    { fg = colors.pink,       bg = 'NONE' })  -- & ; in entities


  -- Text Content
  highlight(0, '@text.html',                   { fg = colors.white,      bg = 'NONE' })  -- Text content
  highlight(0, '@text.strong.html',            { fg = colors.white,      bg = 'NONE', bold = true })  -- <strong>
  highlight(0, '@text.emphasis.html',          { fg = colors.white,      bg = 'NONE', italic = true })  -- <em>
  highlight(0, '@text.underline.html',         { fg = colors.white,      bg = 'NONE', underline = true })  -- <u>
  highlight(0, '@text.strike.html',            { fg = colors.white,      bg = 'NONE', strikethrough = true })  -- <s>
  highlight(0, '@text.title.html',             { fg = colors.white,      bg = 'NONE', bold = true })  -- <h1>-<h6>
  highlight(0, '@text.uri.html',               { fg = colors.blueLink,   bg = 'NONE', underline = true })  -- URLs

  -- Markup (for markdown-like rendering)
  highlight(0, '@markup.heading.html',         { link = "Normal" })  -- Headings
  highlight(0, '@markup.heading.1.html',       { link = "Normal" })  -- h1
  highlight(0, '@markup.heading.2.html',       { link = "Normal" })  -- h2
  highlight(0, '@markup.heading.3.html',       { link = "Normal" })  -- h3
  highlight(0, '@markup.heading.4.html',       { link = "Normal" })  -- h4
  highlight(0, '@markup.heading.5.html',       { link = "Normal" })  -- h5
  highlight(0, '@markup.heading.6.html',       { link = "Normal" })  -- h6
  highlight(0, '@markup.strong.html',          { link = "Normal" })  -- Bold
  highlight(0, '@markup.italic.html',          { link = "Normal" })  -- Italic
  highlight(0, '@markup.underline.html',       { link = "Normal" })  -- Underline
  highlight(0, '@markup.strikethrough.html',   { link = "Normal" })  -- Strike
  highlight(0, '@markup.link.html',            { link = "Comment"})  -- Links
  highlight(0, '@markup.link.label.html',      { link = "Normal" })  -- Link text
  highlight(0, '@markup.link.url.html',        { fg = colors.blueLink,   bg = 'NONE' })  -- Link URL
  highlight(0, '@markup.raw.html',             { fg = colors.redLight,   bg = 'NONE' })  -- <pre>/<code>
  highlight(0, '@markup.list.html',            { fg = colors.white,      bg = 'NONE' })  -- <ul>/<ol>
  highlight(0, '@markup.quote.html',           { fg = colors.gray,       bg = 'NONE', italic = true })  -- <blockquote>

  -- None (transparent)
  highlight(0, '@none.html',                   { fg = 'NONE',            bg = 'NONE' })  -- No highlighting


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.html)

  highlight(0, '@lsp.type.property.html',      { fg = colors.turquoise,  bg = 'NONE' })  -- Attributes
  highlight(0, '@lsp.type.string.html',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.keyword.html',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.class.html',         { fg = colors.turquoise,  bg = 'NONE' })  -- CSS classes
  highlight(0, '@lsp.type.type.html',          { fg = colors.blue,       bg = 'NONE' })  -- Tag types
  highlight(0, '@lsp.type.namespace.html',     { fg = colors.pink,       bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.event.html',         { fg = colors.orange,     bg = 'NONE' })  -- Events
  highlight(0, '@lsp.type.method.html',        { fg = colors.orange,     bg = 'NONE' })  -- Methods (events)


  -----------------------------------------------------------------------------
  -- Common HTML Entities

  highlight(0, 'htmlEntityAmp',        { fg = colors.pink,       bg = 'NONE'            })  -- &amp;
  highlight(0, 'htmlEntityLt',         { fg = colors.pink,       bg = 'NONE'            })  -- &lt;
  highlight(0, 'htmlEntityGt',         { fg = colors.pink,       bg = 'NONE'            })  -- &gt;
  highlight(0, 'htmlEntityQuot',       { fg = colors.pink,       bg = 'NONE'            })  -- &quot;
  highlight(0, 'htmlEntityApos',       { fg = colors.pink,       bg = 'NONE'            })  -- &apos;
  highlight(0, 'htmlEntityNbsp',       { fg = colors.pink,       bg = 'NONE'            })  -- &nbsp;
  highlight(0, 'htmlEntityCopy',       { fg = colors.pink,       bg = 'NONE'            })  -- &copy;
  highlight(0, 'htmlEntityReg',        { fg = colors.pink,       bg = 'NONE'            })  -- &reg;
  highlight(0, 'htmlEntityTrade',      { fg = colors.pink,       bg = 'NONE'            })  -- &trade;
  highlight(0, 'htmlEntityNumeric',    { fg = colors.pink,       bg = 'NONE'            })  -- &#123;
  highlight(0, 'htmlEntityHex',        { fg = colors.pink,       bg = 'NONE'            })  -- &#x7B;


  -----------------------------------------------------------------------------
  -- Web Components / Custom Elements

  highlight(0, 'htmlCustomElement',    { fg = colors.turquoise,  bg = 'NONE'            })  -- <my-component>
  highlight(0, 'htmlSlotAttr',         { fg = colors.purple,     bg = 'NONE'            })  -- slot=""
  highlight(0, 'htmlIsAttr',           { fg = colors.purple,     bg = 'NONE'            })  -- is=""


  -----------------------------------------------------------------------------
  -- Template Syntax (for server-side templating indicators)

  highlight(0, 'htmlTemplateDelim',    { link = "Delimiter" })  -- {{ }} <% %>
  highlight(0, 'htmlTemplateVar',      { link = "Variable" })  -- Template variables
  highlight(0, 'htmlTemplateExpr',     { fg = colors.white,      bg = 'NONE'            })  -- Template expressions


  -----------------------------------------------------------------------------
  -- Embedded SVG in HTML

  highlight(0, 'htmlSvgTag',           { fg = colors.blue,       bg = 'NONE'            })  -- <svg>
  highlight(0, 'htmlSvgPath',          { fg = colors.blue,       bg = 'NONE'            })  -- <path>
  highlight(0, 'htmlSvgRect',          { fg = colors.blue,       bg = 'NONE'            })  -- <rect>
  highlight(0, 'htmlSvgCircle',        { fg = colors.blue,       bg = 'NONE'            })  -- <circle>
  highlight(0, 'htmlSvgUse',           { fg = colors.blue,       bg = 'NONE'            })  -- <use>


  -----------------------------------------------------------------------------
  -- Embedded MathML in HTML

  highlight(0, 'htmlMathTag',          { fg = colors.blue,       bg = 'NONE'            })  -- <math>
  highlight(0, 'htmlMathMrow',         { fg = colors.blue,       bg = 'NONE'            })  -- <mrow>
  highlight(0, 'htmlMathMi',           { fg = colors.blue,       bg = 'NONE'            })  -- <mi>
  highlight(0, 'htmlMathMo',           { fg = colors.blue,       bg = 'NONE'            })  -- <mo>


  -----------------------------------------------------------------------------
  -- Meta Information

  highlight(0, 'htmlMetaTag',          { fg = colors.blue,       bg = 'NONE'            })  -- <meta>
  highlight(0, 'htmlMetaName',         { fg = colors.turquoise,  bg = 'NONE'            })  -- name=""
  highlight(0, 'htmlMetaContent',      { fg = colors.redLight,   bg = 'NONE'            })  -- content=""
  highlight(0, 'htmlMetaCharset',      { fg = colors.turquoise,  bg = 'NONE'            })  -- charset=""
  highlight(0, 'htmlMetaHttpEquiv',    { fg = colors.turquoise,  bg = 'NONE'            })  -- http-equiv=""


  -----------------------------------------------------------------------------
  -- HTMX Attributes (popular HTML extension)

  highlight(0, 'htmlArgHxGet',         { fg = colors.orange,     bg = 'NONE'            })  -- hx-get=""
  highlight(0, 'htmlArgHxPost',        { fg = colors.orange,     bg = 'NONE'            })  -- hx-post=""
  highlight(0, 'htmlArgHxTarget',      { fg = colors.orange,     bg = 'NONE'            })  -- hx-target=""
  highlight(0, 'htmlArgHxTrigger',     { fg = colors.orange,     bg = 'NONE'            })  -- hx-trigger=""
  highlight(0, 'htmlArgHxSwap',        { fg = colors.orange,     bg = 'NONE'            })  -- hx-swap=""
  highlight(0, 'htmlArgHxPush',        { fg = colors.orange,     bg = 'NONE'            })  -- hx-push-url=""


  -----------------------------------------------------------------------------
  -- Alpine.js Attributes (popular HTML extension)

  highlight(0, 'htmlArgXData',         { fg = colors.purple,     bg = 'NONE'            })  -- x-data=""
  highlight(0, 'htmlArgXBind',         { fg = colors.purple,     bg = 'NONE'            })  -- x-bind:=""
  highlight(0, 'htmlArgXOn',           { fg = colors.purple,     bg = 'NONE'            })  -- x-on:=""
  highlight(0, 'htmlArgXText',         { fg = colors.purple,     bg = 'NONE'            })  -- x-text=""
  highlight(0, 'htmlArgXShow',         { fg = colors.purple,     bg = 'NONE'            })  -- x-show=""
  highlight(0, 'htmlArgXIf',           { fg = colors.purple,     bg = 'NONE'            })  -- x-if=""
  highlight(0, 'htmlArgXFor',          { fg = colors.purple,     bg = 'NONE'            })  -- x-for=""
end

return html
