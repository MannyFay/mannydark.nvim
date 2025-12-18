-------------------------------------------------------------------------------
-- Noice.nvim - UI replacement for messages, cmdline, popupmenu
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local noice     = {}


-------------------------------------------------------------------------------
-- Settings

noice.setupHighlighting = function()
  -- Cmdline
  highlight(0, 'NoiceCmdline',       { fg = colors.white,     bg = 'NONE'           })  -- Cmdline text
  highlight(0, 'NoiceCmdlinePrompt', { fg = colors.turquoise, bg = 'NONE'           })  -- Cmdline prompt

  -- Cmdline icons
  highlight(0, 'NoiceCmdlineIcon',          { fg = colors.blue,      bg = 'NONE' })  -- Default icon
  highlight(0, 'NoiceCmdlineIconCmdline',   { fg = colors.blue,      bg = 'NONE' })  -- : command icon
  highlight(0, 'NoiceCmdlineIconFilter',    { fg = colors.orange,    bg = 'NONE' })  -- Filter icon
  highlight(0, 'NoiceCmdlineIconHelp',      { fg = colors.green,     bg = 'NONE' })  -- Help icon
  highlight(0, 'NoiceCmdlineIconIncRename', { fg = colors.purple,    bg = 'NONE' })  -- Inc rename icon
  highlight(0, 'NoiceCmdlineIconInput',     { fg = colors.turquoise, bg = 'NONE' })  -- Input icon
  highlight(0, 'NoiceCmdlineIconLua',       { fg = colors.blue,      bg = 'NONE' })  -- Lua icon
  highlight(0, 'NoiceCmdlineIconSearch',    { fg = colors.orange,    bg = 'NONE' })  -- Search icon

  -- Cmdline popup
  highlight(0, 'NoiceCmdlinePopup',       { fg = colors.white,     bg = 'NONE' })  -- Popup text
  highlight(0, 'NoiceCmdlinePopupBorder', { fg = colors.turquoise, bg = 'NONE' })  -- Popup border

  -- Cmdline popup borders (specific types)
  highlight(0, 'NoiceCmdlinePopupBorderCmdline',   { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'NoiceCmdlinePopupBorderFilter',    { fg = colors.orange,    bg = 'NONE' })
  highlight(0, 'NoiceCmdlinePopupBorderHelp',      { fg = colors.green,     bg = 'NONE' })
  highlight(0, 'NoiceCmdlinePopupBorderIncRename', { fg = colors.purple,    bg = 'NONE' })
  highlight(0, 'NoiceCmdlinePopupBorderInput',     { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'NoiceCmdlinePopupBorderLua',       { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'NoiceCmdlinePopupBorderSearch',    { fg = colors.orange,    bg = 'NONE' })

  -- Cmdline popup titles
  highlight(0, 'NoiceCmdlinePopupTitle',         { fg = colors.turquoise, bg = 'NONE', bold = true })
  highlight(0, 'NoiceCmdlinePopupTitleCmdline',  { fg = colors.blue,      bg = 'NONE', bold = true })
  highlight(0, 'NoiceCmdlinePopupTitleFilter',   { fg = colors.orange,    bg = 'NONE', bold = true })
  highlight(0, 'NoiceCmdlinePopupTitleInput',    { fg = colors.turquoise, bg = 'NONE', bold = true })
  highlight(0, 'NoiceCmdlinePopupTitleLua',      { fg = colors.blue,      bg = 'NONE', bold = true })
  highlight(0, 'NoiceCmdlinePopupTitleSearch',   { fg = colors.orange,    bg = 'NONE', bold = true })

  -- Popup/Split windows
  highlight(0, 'NoicePopup',       { fg = colors.white,     bg = 'NONE' })  -- Popup content
  highlight(0, 'NoicePopupBorder', { fg = colors.turquoise, bg = 'NONE' })  -- Popup border
  highlight(0, 'NoiceSplit',       { fg = colors.white,     bg = 'NONE' })  -- Split content
  highlight(0, 'NoiceSplitBorder', { fg = colors.turquoise, bg = 'NONE' })  -- Split border

  -- Popupmenu (completion)
  highlight(0, 'NoicePopupmenu',         { fg = colors.white, bg = 'NONE'           })  -- Menu items
  highlight(0, 'NoicePopupmenuBorder',   { fg = colors.turquoise, bg = 'NONE'       })  -- Menu border
  highlight(0, 'NoicePopupmenuMatch',    { fg = colors.turquoise, bg = 'NONE', bold = true })  -- Matching text
  highlight(0, 'NoicePopupmenuSelected', { fg = colors.white, bg = colors.grayDark  })  -- Selected item

  -- Confirm dialog
  highlight(0, 'NoiceConfirm',       { fg = colors.white,     bg = 'NONE' })  -- Confirm dialog
  highlight(0, 'NoiceConfirmBorder', { fg = colors.turquoise, bg = 'NONE' })  -- Confirm border

  -- Cursor
  highlight(0, 'NoiceCursor', { fg = colors.black, bg = colors.white })  -- Fake cursor

  -- Scrollbar
  highlight(0, 'NoiceScrollbar',      { fg = colors.grayDark, bg = 'NONE'       })  -- Scrollbar track
  highlight(0, 'NoiceScrollbarThumb', { fg = colors.gray,     bg = colors.gray  })  -- Scrollbar thumb

  -- Mini (bottom right notifications)
  highlight(0, 'NoiceMini', { fg = colors.white, bg = 'NONE' })  -- Mini message

  -- Virtual text
  highlight(0, 'NoiceVirtualText', { fg = colors.gray, bg = 'NONE' })  -- Virtual text hints

  -- Format highlights
  highlight(0, 'NoiceFormatTitle',          { fg = colors.turquoise, bg = 'NONE', bold = true })  -- Title
  highlight(0, 'NoiceFormatDate',           { fg = colors.gray,      bg = 'NONE'              })  -- Date
  highlight(0, 'NoiceFormatEvent',          { fg = colors.gray,      bg = 'NONE'              })  -- Event
  highlight(0, 'NoiceFormatKind',           { fg = colors.turquoise, bg = 'NONE'              })  -- Kind
  highlight(0, 'NoiceFormatConfirm',        { fg = colors.green,     bg = 'NONE'              })  -- Confirm
  highlight(0, 'NoiceFormatConfirmDefault', { fg = colors.green,     bg = 'NONE', bold = true })  -- Confirm default

  -- Format levels (notification severity)
  highlight(0, 'NoiceFormatLevelDebug', { fg = colors.gray,      bg = 'NONE' })  -- Debug
  highlight(0, 'NoiceFormatLevelTrace', { fg = colors.gray,      bg = 'NONE' })  -- Trace
  highlight(0, 'NoiceFormatLevelOff',   { fg = colors.gray,      bg = 'NONE' })  -- Off
  highlight(0, 'NoiceFormatLevelInfo',  { fg = colors.blue,      bg = 'NONE' })  -- Info
  highlight(0, 'NoiceFormatLevelWarn',  { fg = colors.orange,    bg = 'NONE' })  -- Warning
  highlight(0, 'NoiceFormatLevelError', { fg = colors.red,       bg = 'NONE' })  -- Error

  -- Progress indicators
  highlight(0, 'NoiceFormatProgressDone', { fg = colors.green,    bg = 'NONE' })  -- Progress done
  highlight(0, 'NoiceFormatProgressTodo', { fg = colors.grayDark, bg = 'NONE' })  -- Progress todo

  -- LSP progress
  highlight(0, 'NoiceLspProgressClient',  { fg = colors.turquoise, bg = 'NONE' })  -- LSP client name
  highlight(0, 'NoiceLspProgressSpinner', { fg = colors.turquoise, bg = 'NONE' })  -- Spinner
  highlight(0, 'NoiceLspProgressTitle',   { fg = colors.white,     bg = 'NONE' })  -- Progress title

  -- Completion item kinds (same colors as nvim-cmp)
  highlight(0, 'NoiceCompletionItemKindDefault',     { fg = colors.white,     bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindClass',       { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindColor',       { fg = colors.pink,      bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindConstant',    { fg = colors.orange,    bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindConstructor', { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindEnum',        { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindEnumMember',  { fg = colors.purple,    bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindField',       { fg = colors.purple,    bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindFile',        { fg = colors.white,     bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindFolder',      { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindFunction',    { fg = colors.orange,    bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindInterface',   { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindKeyword',     { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindMethod',      { fg = colors.orange,    bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindModule',      { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindOperator',    { fg = colors.white,     bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindProperty',    { fg = colors.purple,    bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindReference',   { fg = colors.purple,    bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindSnippet',     { fg = colors.green,     bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindStruct',      { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindText',        { fg = colors.white,     bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindTypeParameter', { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindUnit',        { fg = colors.turquoise, bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindValue',       { fg = colors.orange,    bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemKindVariable',    { fg = colors.purple,    bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemMenu',            { fg = colors.gray,      bg = 'NONE' })
  highlight(0, 'NoiceCompletionItemWord',            { fg = colors.white,     bg = 'NONE' })
end

return noice
