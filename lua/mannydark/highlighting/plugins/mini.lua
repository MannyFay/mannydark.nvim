-------------------------------------------------------------------------------
-- mini.nvim - Library of 40+ independent Lua modules
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local mini      = {}


-------------------------------------------------------------------------------
-- Settings

mini.setupHighlighting = function()

  -----------------------------------------------------------------------------
  -- mini.statusline
  highlight(0, "MiniStatuslineModeNormal",  { fg = colors.black, bg = colors.blue, bold = true }     )  -- Normal mode
  highlight(0, "MiniStatuslineModeInsert",  { fg = colors.black, bg = colors.green, bold = true }    )  -- Insert mode
  highlight(0, "MiniStatuslineModeVisual",  { fg = colors.black, bg = colors.purple, bold = true }   )  -- Visual mode
  highlight(0, "MiniStatuslineModeReplace", { fg = colors.black, bg = colors.red, bold = true }      )  -- Replace mode
  highlight(0, "MiniStatuslineModeCommand", { fg = colors.black, bg = colors.orange, bold = true }   )  -- Command mode
  highlight(0, "MiniStatuslineModeOther",   { fg = colors.black, bg = colors.turquoise, bold = true })  -- Other modes
  highlight(0, "MiniStatuslineDevinfo",     { fg = colors.white, bg = colors.grayDark }              )  -- Dev info section
  highlight(0, "MiniStatuslineFilename",    { fg = colors.white, bg = "NONE" }                       )  -- Filename section
  highlight(0, "MiniStatuslineFileinfo",    { fg = colors.white, bg = colors.grayDark }              )  -- File info section
  highlight(0, "MiniStatuslineInactive",    { fg = colors.gray, bg = "NONE" }                        )  -- Inactive window

  -----------------------------------------------------------------------------
  -- mini.tabline
  highlight(0, "MiniTablineCurrent",         { fg = colors.white, bg = colors.grayDark, bold = true } )  -- Active buffer
  highlight(0, "MiniTablineVisible",         { fg = colors.white, bg = "NONE" }                       )  -- Visible buffer
  highlight(0, "MiniTablineHidden",          { fg = colors.gray, bg = "NONE" }                        )  -- Hidden buffer
  highlight(0, "MiniTablineModifiedCurrent", { fg = colors.orange, bg = colors.grayDark, bold = true })  -- Modified active
  highlight(0, "MiniTablineModifiedVisible", { fg = colors.orange, bg = "NONE" }                      )  -- Modified visible
  highlight(0, "MiniTablineModifiedHidden",  { fg = colors.orange, bg = "NONE" }                      )  -- Modified hidden
  highlight(0, "MiniTablineFill",            { fg = "NONE", bg = "NONE" }                             )  -- Empty space
  highlight(0, "MiniTablineTabpagesection",  { fg = colors.white, bg = colors.gray, bold = true }     )  -- Tabpage section
  highlight(0, "MiniTablineTrunc",           { fg = colors.gray, bg = "NONE" }                        )  -- Truncation indicator

  -----------------------------------------------------------------------------
  -- mini.pick
  highlight(0, "MiniPickBorder",        { fg = colors.turquoise, bg = "NONE" }             )  -- Window border
  highlight(0, "MiniPickBorderBusy",    { fg = colors.orange, bg = "NONE" }                )  -- Border while busy
  highlight(0, "MiniPickBorderText",    { fg = colors.turquoise, bg = "NONE", bold = true })  -- Non-prompt border text
  highlight(0, "MiniPickCursor",        { fg = "NONE", bg = "NONE", blend = 100 }          )  -- Hidden cursor
  highlight(0, "MiniPickIconDirectory", { fg = colors.blue, bg = "NONE" }                  )  -- Directory icon
  highlight(0, "MiniPickIconFile",      { fg = colors.white, bg = "NONE" }                 )  -- File icon
  highlight(0, "MiniPickHeader",        { fg = colors.turquoise, bg = "NONE", bold = true })  -- Headers
  highlight(0, "MiniPickMatchCurrent",  { fg = "NONE", bg = colors.grayDark }              )  -- Current match
  highlight(0, "MiniPickMatchMarked",   { fg = colors.orange, bg = "NONE", bold = true }   )  -- Marked items
  highlight(0, "MiniPickMatchRanges",   { fg = colors.turquoise, bg = "NONE", bold = true })  -- Matching ranges
  highlight(0, "MiniPickNormal",        { fg = colors.white, bg = "NONE" }                 )  -- Normal text
  highlight(0, "MiniPickPreviewLine",   { fg = "NONE", bg = colors.grayDark }              )  -- Preview target line
  highlight(0, "MiniPickPreviewRegion", { fg = "NONE", bg = colors.gray }                  )  -- Preview target region
  highlight(0, "MiniPickPrompt",        { fg = colors.turquoise, bg = "NONE", bold = true })  -- Prompt
  highlight(0, "MiniPickPromptCaret",   { fg = colors.white, bg = "NONE" }                 )  -- Prompt caret
  highlight(0, "MiniPickPromptPrefix",  { fg = colors.blue, bg = "NONE" }                  )  -- Prompt prefix

  -----------------------------------------------------------------------------
  -- mini.files
  highlight(0, "MiniFilesBorder",         { fg = colors.turquoise, bg = "NONE" }             )  -- Window border
  highlight(0, "MiniFilesBorderModified", { fg = colors.orange, bg = "NONE" }                )  -- Modified buffer border
  highlight(0, "MiniFilesCursorLine",     { fg = "NONE", bg = colors.grayDark }              )  -- Cursor line
  highlight(0, "MiniFilesDirectory",      { fg = colors.blue, bg = "NONE" }                  )  -- Directory
  highlight(0, "MiniFilesFile",           { fg = colors.white, bg = "NONE" }                 )  -- File
  highlight(0, "MiniFilesNormal",         { fg = colors.white, bg = "NONE" }                 )  -- Normal text
  highlight(0, "MiniFilesTitle",          { fg = colors.turquoise, bg = "NONE", bold = true })  -- Window title
  highlight(0, "MiniFilesTitleFocused",   { fg = colors.turquoise, bg = "NONE", bold = true })  -- Focused window title

  -----------------------------------------------------------------------------
  -- mini.diff
  highlight(0, "MiniDiffSignAdd",        { fg = colors.green, bg = "NONE" } )  -- Added lines sign
  highlight(0, "MiniDiffSignChange",     { fg = colors.blue, bg = "NONE" }  )  -- Changed lines sign
  highlight(0, "MiniDiffSignDelete",     { fg = colors.red, bg = "NONE" }   )  -- Deleted lines sign
  highlight(0, "MiniDiffOverAdd",        { fg = colors.green, bg = "NONE" } )  -- Added text overlay
  highlight(0, "MiniDiffOverChange",     { fg = colors.blue, bg = "NONE" }  )  -- Changed ref overlay
  highlight(0, "MiniDiffOverChangeBuf",  { fg = colors.orange, bg = "NONE" })  -- Changed buf overlay
  highlight(0, "MiniDiffOverContext",    { fg = colors.gray, bg = "NONE" }  )  -- Context in ref overlay
  highlight(0, "MiniDiffOverContextBuf", { fg = colors.gray, bg = "NONE" }  )  -- Context in buf overlay
  highlight(0, "MiniDiffOverDelete",     { fg = colors.red, bg = "NONE" }   )  -- Deleted text overlay

  -----------------------------------------------------------------------------
  -- mini.hipatterns
  highlight(0, "MiniHipatternsFixme", { fg = colors.black, bg = colors.red, bold = true }      )  -- FIXME
  highlight(0, "MiniHipatternsHack",  { fg = colors.black, bg = colors.orange, bold = true }   )  -- HACK
  highlight(0, "MiniHipatternsTodo",  { fg = colors.black, bg = colors.turquoise, bold = true })  -- TODO
  highlight(0, "MiniHipatternsNote",  { fg = colors.black, bg = colors.green, bold = true }    )  -- NOTE

  -----------------------------------------------------------------------------
  -- mini.indentscope
  highlight(0, "MiniIndentscopeSymbol",    { fg = colors.gray, bg = "NONE" }    )  -- Scope symbol
  highlight(0, "MiniIndentscopeSymbolOff", { fg = colors.grayDark, bg = "NONE" })  -- Off-shiftwidth symbol

  -----------------------------------------------------------------------------
  -- mini.jump / mini.jump2d
  highlight(0, "MiniJump",             { fg = colors.black, bg = colors.pink, bold = true }     )  -- Jump positions
  highlight(0, "MiniJump2dSpot",       { fg = colors.black, bg = colors.pink, bold = true }     )  -- Jump2d spots
  highlight(0, "MiniJump2dSpotAhead",  { fg = colors.black, bg = colors.turquoise, bold = true })  -- Spots ahead
  highlight(0, "MiniJump2dSpotUnique", { fg = colors.black, bg = colors.orange, bold = true }   )  -- Unique spots
  highlight(0, "MiniJump2dDim",        { fg = colors.gray, bg = "NONE" }                        )  -- Dimmed text

  -----------------------------------------------------------------------------
  -- mini.cursorword
  highlight(0, "MiniCursorword",        { fg = "NONE", bg = "NONE", underline = true })  -- Word under cursor
  highlight(0, "MiniCursorwordCurrent", { fg = "NONE", bg = "NONE" }                  )  -- Current word (no highlight)

  -----------------------------------------------------------------------------
  -- mini.notify
  highlight(0, "MiniNotifyBorder", { fg = colors.turquoise, bg = "NONE" }             )  -- Notification border
  highlight(0, "MiniNotifyNormal", { fg = colors.white, bg = "NONE" }                 )  -- Normal notification
  highlight(0, "MiniNotifyTitle",  { fg = colors.turquoise, bg = "NONE", bold = true })  -- Notification title

  -----------------------------------------------------------------------------
  -- mini.starter
  highlight(0, "MiniStarterCurrent",    { fg = "NONE", bg = "NONE" }                       )  -- Current item
  highlight(0, "MiniStarterFooter",     { fg = colors.gray, bg = "NONE", italic = true }   )  -- Footer
  highlight(0, "MiniStarterHeader",     { fg = colors.turquoise, bg = "NONE", bold = true })  -- Header
  highlight(0, "MiniStarterInactive",   { fg = colors.gray, bg = "NONE" }                  )  -- Inactive items
  highlight(0, "MiniStarterItem",       { fg = colors.white, bg = "NONE" }                 )  -- Item
  highlight(0, "MiniStarterItemBullet", { fg = colors.turquoise, bg = "NONE" }             )  -- Item bullet
  highlight(0, "MiniStarterItemPrefix", { fg = colors.orange, bg = "NONE" }                )  -- Item prefix
  highlight(0, "MiniStarterSection",    { fg = colors.blue, bg = "NONE", bold = true }     )  -- Section
  highlight(0, "MiniStarterQuery",      { fg = colors.turquoise, bg = "NONE", bold = true })  -- Query match

  -----------------------------------------------------------------------------
  -- mini.surround
  highlight(0, "MiniSurround", { fg = colors.black, bg = colors.orange })  -- Surround highlight

  -----------------------------------------------------------------------------
  -- mini.trailspace
  highlight(0, "MiniTrailspace", { fg = "NONE", bg = colors.red })  -- Trailing whitespace

  -----------------------------------------------------------------------------
  -- mini.completion
  highlight(0, "MiniCompletionActiveParameter", { fg = "NONE", bg = "NONE", underline = true })  -- Active parameter

  -----------------------------------------------------------------------------
  -- mini.clue
  highlight(0, "MiniClueBorder",              { fg = colors.turquoise, bg = "NONE" }             )  -- Clue window border
  highlight(0, "MiniClueDescGroup",           { fg = colors.purple, bg = "NONE" }                )  -- Group description
  highlight(0, "MiniClueDescSingle",          { fg = colors.white, bg = "NONE" }                 )  -- Single key description
  highlight(0, "MiniClueNextKey",             { fg = colors.turquoise, bg = "NONE" }             )  -- Next key
  highlight(0, "MiniClueNextKeyWithPostkeys", { fg = colors.orange, bg = "NONE" }                )  -- Next key with postkeys
  highlight(0, "MiniClueSeparator",           { fg = colors.gray, bg = "NONE" }                  )  -- Separator
  highlight(0, "MiniClueTitle",               { fg = colors.turquoise, bg = "NONE", bold = true })  -- Window title

  -----------------------------------------------------------------------------
  -- mini.map
  highlight(0, "MiniMapNormal",      { fg = colors.gray, bg = "NONE" }     )  -- Normal map text
  highlight(0, "MiniMapSymbolCount", { fg = colors.turquoise, bg = "NONE" })  -- Symbol count
  highlight(0, "MiniMapSymbolLine",  { fg = colors.turquoise, bg = "NONE" })  -- Line symbol
  highlight(0, "MiniMapSymbolView",  { fg = colors.blue, bg = "NONE" }     )  -- View symbol

  -----------------------------------------------------------------------------
  -- mini.operators
  highlight(0, "MiniOperatorsExchangeFrom", { fg = "NONE", bg = colors.grayDark })  -- Exchange source

  -----------------------------------------------------------------------------
  -- mini.deps
  highlight(0, "MiniDepsChangeAdded",   { fg = colors.green, bg = "NONE" }                 )  -- Added dependency
  highlight(0, "MiniDepsChangeRemoved", { fg = colors.red, bg = "NONE" }                   )  -- Removed dependency
  highlight(0, "MiniDepsHint",          { fg = colors.turquoise, bg = "NONE" }             )  -- Hint
  highlight(0, "MiniDepsInfo",          { fg = colors.blue, bg = "NONE" }                  )  -- Info
  highlight(0, "MiniDepsMsgBreaking",   { fg = colors.red, bg = "NONE", bold = true }      )  -- Breaking change
  highlight(0, "MiniDepsPlaceholder",   { fg = colors.gray, bg = "NONE" }                  )  -- Placeholder
  highlight(0, "MiniDepsTitle",         { fg = colors.turquoise, bg = "NONE", bold = true })  -- Title
  highlight(0, "MiniDepsTitleError",    { fg = colors.red, bg = "NONE", bold = true }      )  -- Error title
  highlight(0, "MiniDepsTitleSame",     { fg = colors.gray, bg = "NONE" }                  )  -- Same version title
  highlight(0, "MiniDepsTitleUpdate",   { fg = colors.green, bg = "NONE", bold = true }    )  -- Update title

  -----------------------------------------------------------------------------
  -- mini.icons
  highlight(0, "MiniIconsAzure",  { fg = "#007FFF", bg = "NONE" }       )  -- Azure color
  highlight(0, "MiniIconsBlue",   { fg = colors.blue, bg = "NONE" }     )  -- Blue
  highlight(0, "MiniIconsCyan",   { fg = colors.turquoise, bg = "NONE" })  -- Cyan
  highlight(0, "MiniIconsGreen",  { fg = colors.green, bg = "NONE" }    )  -- Green
  highlight(0, "MiniIconsGrey",   { fg = colors.gray, bg = "NONE" }     )  -- Grey
  highlight(0, "MiniIconsOrange", { fg = colors.orange, bg = "NONE" }   )  -- Orange
  highlight(0, "MiniIconsPurple", { fg = colors.purple, bg = "NONE" }   )  -- Purple
  highlight(0, "MiniIconsRed",    { fg = colors.red, bg = "NONE" }      )  -- Red
  highlight(0, "MiniIconsYellow", { fg = colors.yellow, bg = "NONE" }   )  -- Yellow
end

return mini
