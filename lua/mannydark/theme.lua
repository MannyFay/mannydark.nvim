local c = require('mannydark.palette')

local hl = vim.api.nvim_set_hl
local theme = {}

theme.set_highlights = function()

------------------------------------------------------------------------------
-- Editor
------------------------------------------------------------------------------
  hl(0, 'FloatBorder', {
      fg = vim.api.nvim_get_hl_by_name('NormalFloat', true).background,
      bg = vim.api.nvim_get_hl_by_name('NormalFloat', true).background,
  })    

  --hl(0, 'IndentBlanklineChar', { fg=#252525]])
      hl(0, 'IndentBlanklineChar', { fg = c.gray_dark, bg = 'NONE' })
  
  hl(0, "SignColumn", { fg = 'NONE', bg = c.black })

  -- Vertical visual guide line:
  hl(0, "ColorColumn", { fg = 'NONE', bg = c.gray_dark })

  hl(0, "LineNr", { fg = c.gray, bg = 'NONE' })
  hl(0, "CursorLineNr", { fg = c.white, bg = 'NONE' })
  -- Vertical split separator line:
  hl(0, "VertSplit", { fg = c.gray_dark, bg = c.gray_dark })
  -- Color of actual line:
  hl(0, "CursorLine", { fg = 'NONE', bg = c.gray_dark })
  -- Highlighting of search patterns:
  hl(0, "Search", { fg = c.black, bg = c.pink })
  -- Highlighting of first result from search pattern:
  hl(0, "IncSearch", { fg = c.black, bg = c.white })
  -- Empty end of buffer:
  hl(0, "EndOfBuffer", { fg = c.black, bg = 'NONE' })
  -- Error message:
  hl(0, "ErrorMsg", { fg = c.white, bg = c.red, bold = true, })
  hl(0, "Visual", { fg = 'NONE', bg = c.gray })
  hl(0, "Pmenu", { fg = c.white, bg = c.gray_dark })
  hl(0, "PmenuSel", { fg = c.black, bg = c.gray })

----------------------------
  hl(0, "Cursor", { fg = c.red, bg = c.green })
  hl(0, "lCursor", { fg = c.red, bg = c.green })
  hl(0, "CursorIM", { fg = c.red, bg = c.green })

  hl(0, "MsgArea", { fg = c.fg, bg = c.bg })
  hl(0, "ModeMsg", { fg = c.fg, bg = c.alt_bg })
  hl(0, "MsgSeparator", { fg = c.fg, bg = c.bg })
  hl(0, "SpellBad", { fg = 'NONE', bg = 'NONE', sp = c.red, undercurl = true, })
  hl(0, "SpellCap", { fg = 'NONE', bg = 'NONE', sp = c.yellow, undercurl = true, })
  hl(0, "SpellLocal", { fg = 'NONE', bg = 'NONE', sp = c.green, underline = true, })
  hl(0, "SpellRare", { fg = 'NONE', bg = 'NONE', sp = c.purple, underline = true, })
  hl(0, "NormalNC", { fg = c.fg, bg = c.bg })

  hl(0, "WildMenu", { fg = c.red, bg = c.purple })

  hl(0, "Folded", { fg = c.gray, bg = c.alt_bg })
  hl(0, "FoldColumn", { fg = c.gray, bg = c.alt_bg })
  hl(0, "FloatBorder", { fg = c.gray, bg = c.alt_bg })
  hl(0, "Whitespace", { fg = c.dark_gray, bg = 'NONE' })

  hl(0, "CursorColumn", { fg = c.blue, bg = c.red })
  --hl(0, "CursorColumn", { fg = 'NONE', bg = c.alt_bg })

  hl(0, "NormalFloat", { fg = 'NONE', bg = c.alt_bg })
  hl(0, "VisualNOS", { fg = 'NONE', bg = c.alt_bg })
  hl(0, "WarningMsg", { fg = c.ui_orange, bg = c.bg })
  hl(0, "QuickFixLine", { fg = 'NONE', bg = c.ui2_blue })
  hl(0, "PmenuSbar", { fg = 'NONE', bg = c.alt_bg })
  hl(0, "PmenuThumb", { fg = 'NONE', bg = c.gray })
  hl(0, "MatchWord", { fg = 'NONE', bg = c.reference })
  hl(0, "MatchWordCur", { fg = 'NONE', bg = c.reference })
  hl(0, "MatchParenCur", { fg = 'NONE', bg = c.reference })
  hl(0, "TermCursor", { fg = c.cursor_fg, bg = c.cursor_bg })
  hl(0, "TermCursorNC", { fg = c.cursor_fg, bg = c.cursor_bg })
  hl(0, "Conceal", { fg = c.gray, bg = 'NONE' })
  hl(0, "SpecialKey", { fg = c.blue, bg = 'NONE', bold = true, })
  hl(0, "Substitute", { fg = 'NONE', bg = c.ui2_orange })
  hl(0, "MoreMsg", { fg = c.orange, bg = 'NONE' })
  hl(0, "Question", { fg = c.orange, bg = 'NONE' })
  hl(0, "NonText", { fg = c.dark_gray, bg = 'NONE' })

  hl(0, "TabLine", { fg = c.red, bg = c.green })

  hl(0, "TabLineSel", { fg = c.fg, bg = c.line })
  hl(0, "TabLineFill", { fg = c.line, bg = c.line })


--------------------------------------------------------------
-- Buffer

  hl(0, "BufferCurrent", { fg = c.fg, bg = c.bg })
  hl(0, "BufferCurrentIndex", { fg = c.fg, bg = c.bg })
  hl(0, "BufferCurrentMod", { fg = c.info, bg = c.bg })
  hl(0, "BufferCurrentSign", { fg = c.hint, bg = c.bg })
  hl(0, "BufferCurrentTarget", { fg = c.red, bg = c.bg, bold = true, })
  hl(0, "BufferVisible", { fg = c.fg, bg = c.bg })
  hl(0, "BufferVisibleIndex", { fg = c.fg, bg = c.bg })
  hl(0, "BufferVisibleMod", { fg = c.info, bg = c.bg })
  hl(0, "BufferVisibleSign", { fg = c.gray, bg = c.bg })
  hl(0, "BufferVisibleTarget", { fg = c.red, bg = c.bg, bold = true, })
  hl(0, "BufferInactive", { fg = c.gray, bg = c.alt_bg })
  hl(0, "BufferInactiveIndex", { fg = c.gray, bg = c.alt_bg })
  hl(0, "BufferInactiveMod", { fg = c.info, bg = c.alt_bg })
  hl(0, "BufferInactiveSign", { fg = c.gray, bg = c.alt_bg })
  hl(0, "BufferInactiveTarget", { fg = c.red, bg = c.alt_bg, bold = true, })


--------------------------------------------------------------
-- Status Line

  hl(0, "StatusLine", { fg = c.white, bg = c.gray_dark })
  hl(0, "StatusLineNC", { fg = c.black, bg = c.gray_dark })

-----------------------

  hl(0, "StatusLineSeparator", { fg = c.green, bg = c.red })
  hl(0, "StatusLineTerm", { fg = c.red, bg = c.black })
  hl(0, "StatusLineTermNC", { fg = c.white, bg = c.purple })


--------------------------------------------------------------
-- Hop

  hl(0, "HopNextKey", { fg = c.blue, bg = 'NONE' })
  hl(0, "HopNextKey1", { fg = c.purple, bg = 'NONE' })
  hl(0, "HopNextKey2", { fg = c.purple, bg = 'NONE' })
  hl(0, "HopUnmatched", { fg = c.gray, bg = 'NONE' })
  hl(0, "HopPreview", { fg = '#c7ba7d', bg = 'NONE' })
  hl(0, "HopCursor", { fg = c.white, bg = 'NONE' })


--------------------------------------------------------------
-- NvimTree

  hl(0, "NvimTreeRootFolder"               , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "Directory"                        , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeFolderIcon"               , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeIndentMarker"             , { fg = c.gray     , bg = 'NONE'      })
  hl(0, "NvimTreeWinSeparator"             , { fg = c.gray_dark, bg = c.gray_dark })
  hl(0, "NvimTreeNormal"                   , { fg = c.white    , bg = 'NONE'      })
  hl(0, "NvimTreeFolderName"               , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeOpenedFolderName"         , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeEmptyFolderName"          , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeEndOfBuffer"              , { fg = c.black    , bg = 'NONE'      })
  hl(0, "NvimTreeCursorLine"               , { fg = 'NONE'     , bg = c.gray_dark })
  hl(0, "NvimTreeImageFile"                , { fg = c.white    , bg = 'NONE'      })
  hl(0, "NvimTreeGitIgnored"               , { fg = c.gray     , bg = 'NONE'      })
  hl(0, "NvimTreeGitDeleted"               , { fg = c.gray     , bg = 'NONE'      })
  hl(0, "NvimTreeGitRenamed"               , { fg = c.red      , bg = 'NONE'      })
  hl(0, "NvimTreeGitNew"                   , { fg = c.red      , bg = 'NONE'      })
  hl(0, "NvimTreeGitDirty"                 , { fg = c.red      , bg = 'NONE'      })
  hl(0, "NvimTreeGitStaged"                , { fg = c.orange   , bg = 'NONE'      })
  hl(0, "NvimTreeGitMerge"                 , { fg = c.orange   , bg = 'NONE'      })
  hl(0, "NvimTreeLspDiagnosticsError"      , { fg = c.red      , bg = 'NONE'      })
  hl(0, "NvimTreeLspDiagnosticsWarning"    , { fg = c.orange   , bg = 'NONE'      })
  hl(0, "NvimTreeLspDiagnosticsInformation", { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeLspDiagnosticsHint"       , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeFileIgnored"              , { fg = '#606366'  , bg = 'NONE'      })
  hl(0, "NvimTreeFileDeleted"              , { fg = '#606366'  , bg = 'NONE'      })
  hl(0, "nvimtreefilerenamed"              , { fg = '#ff0000'  , bg = 'NONE'      })
  hl(0, "NvimTreeFileNew"                  , { fg = '#FF0000'  , bg = 'NONE'      })
  hl(0, "NvimTreeFileDirty"                , { fg = '#FF0000'  , bg = 'NONE'      })
  hl(0, "NvimTreeFileStaged"               , { fg = '#E8BF6A'  , bg = 'NONE'      })
  hl(0, "NvimTreeFileMerge"                , { fg = '#E8BF6A'  , bg = 'NONE'      })
  hl(0, "NvimTreeModifiedFile"             , { fg = '#E8BF6A'  , bg = 'NONE'      })
  hl(0, "NvimTreeOpenedFile"               , { fg = '#E8BF6A'  , bg = 'NONE'      })
  hl(0, "NvimTreeNormalFloat"              , { fg = '#606366'  , bg = 'NONE'      })
  hl(0, "NvimTreeExecFile"                 , { fg = c.white    , bg = 'NONE'      })
  
  
  hl(0, "NvimTreeSpecialFile", { fg = c.white, bg = 'NONE' })

----------------------------

  hl(0, "NvimTreeVertSplit", { fg = c.red, bg = c.green })

  hl(0, "NvimTreeSymlink", { fg = c.cyan, bg = 'NONE' })

  
  --[[ NvimTreeSymlinkIcon ]]
--[[ NvimTreeSymlinkFolderName   (Directory) ]]
--[[ NvimTreeOpenedFolderIcon    (NvimTreeFolderIcon) ]]
--[[ NvimTreeClosedFolderIcon    (NvimTreeFolderIcon) ]]
--[[ NvimTreeFileIcon ]]
--[[ NvimTreeWindowPicker ]]

--[[ There are also links to normal bindings to style the tree itself. ]]

--[[ NvimTreeNormalFloat ]]
--[[ NvimTreeCursorLineNr    (CursorLineNr) ]]
--[[ NvimTreeLineNr          (LineNr) ]]
--[[ NvimTreeCursorColumn    (CursorColumn) ]]

--[[ There are 2 highlight groups for the live filter feature ]]
--[[ NvimTreeLiveFilterPrefix ]]
--[[ NvimTreeLiveFilterValue ]]

--[[ Color of the bookmark icon ]]
--[[ NvimTreeBookmark ]]

  
--------------------------------------------------------------
-- Lualine

  hl(0, "lualine_b_insert", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_a_insert", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_b_inactive", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_c_inactive", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_a_inactive", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_b_visual", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_a_visual", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_b_normal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_c_normal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_a_normal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_b_command", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_a_command", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_b_replace", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_a_replace", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_normal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_insert", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_visual", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_replace", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_command", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_terminal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_inactive", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_normal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_insert", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_visual", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_replace", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_command", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_terminal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_inactive", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_normal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_insert", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_visual", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_replace", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_command", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_terminal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_inactive", { fg = c.red, bg = c.green, italic = true, })



------------------------------------------------------------------------------
-- Language Defaults
------------------------------------------------------------------------------

  hl(0, "Comment", { fg = c.red, bg = 'NONE' })
  hl(0, "Variable", { fg = c.purple, bg = 'NONE' })
  hl(0, "String", { fg = c.red_light, bg = 'NONE' })
  hl(0, "Number", { fg = c.green_light, bg = 'NONE' })
  hl(0, "Float", { fg = c.green_light, bg = 'NONE' })
  hl(0, "Boolean", { fg = c.blue, bg = 'NONE' })
  hl(0, "Constant", { fg = c.purple, bg = 'NONE' })
  hl(0, "Type", { fg = c.turquoise, bg = 'NONE' })
  hl(0, "Function", { fg = c.orange, bg = 'NONE' })
  hl(0, "Keyword", { fg = c.blue, bg = 'NONE' })
  hl(0, "Character", { fg = c.white, bg = 'NONE' })
  hl(0, "Conditional", { fg = c.blue, bg = 'NONE' })

  hl(0, "Delimiter", { fg = c.blue, bg = 'NONE' })

  hl(0, "Exception", { fg = c.blue, bg = 'NONE' })
  hl(0, "Identifier", { fg = c.purple, bg = 'NONE' })
  hl(0, "Include", { fg = c.blue, bg = 'NONE' })
  hl(0, "MatchParen", { fg = c.white, bg = c.gray })
  hl(0, "Normal", { fg = c.white, bg = c.black })
  hl(0, "Operator", { fg = c.white, bg = 'NONE' })
  hl(0, "PreProc", { fg = c.blue, bg = 'NONE' })
  hl(0, "Special", { fg = c.white, bg = 'NONE' })
  hl(0, "Todo", { fg = c.red, bg = 'NONE', bold = true, })
  hl(0, "Title", { fg = c.white, bg = 'NONE' })
  hl(0, "Error", { fg = c.red, bg = 'NONE', bold = true, })
  hl(0, "Statement", { fg = c.blue, bg = 'NONE' })
  hl(0, "Structure", { fg = c.blue, bg = 'NONE' })

-------------------------------------------------------------

  hl(0, "SpecialChar", { fg = c.white, bg = 'NONE' })
  hl(0, "Repeat", { fg = c.purple, bg = 'NONE' })
  hl(0, "StorageClass", { fg = c.cyan, bg = 'NONE' })
  hl(0, "Typedef", { fg = c.purple, bg = 'NONE' })
  hl(0, "Define", { fg = c.purple, bg = 'NONE' })
  hl(0, "Macro", { fg = c.purple, bg = 'NONE' })
  hl(0, "Debug", { fg = c.red, bg = 'NONE' })
  hl(0, "Label", { fg = c.blue, bg = 'NONE' })
  hl(0, "SpecialComment", { fg = c.fg, bg = 'NONE' })
  hl(0, "Tag", { fg = c.blue, bg = 'NONE' })
  hl(0, "Bold", { fg = 'NONE', bg = 'NONE', bold = true, })
  hl(0, "Italic", { fg = 'NONE', bg = 'NONE', italic = true, })
  hl(0, "Underlined", { fg = 'NONE', bg = 'NONE', underline = true, })
  hl(0, "Ignore", { fg = c.magenta, bg = 'NONE', bold = true, })
  hl(0, "PreCondit", { fg = c.purple, bg = 'NONE' })


--------------------------------------------------------------
-- Treesitter

  hl(0, "@comment", { fg = c.red, bg = 'NONE' })
  hl(0, "@variable", { fg = c.purple, bg = 'NONE'})
  -- $this keyword:
  hl(0, "@variable.builtin", { fg = c.blue, bg = 'NONE' })
  hl(0, "@string", { link = 'String' })
  hl(0, "@number", { link = 'Number' })
  hl(0, "@float", { link = 'Float' })
  hl(0, "@boolean", { link = 'Boolean' })
  hl(0, "@constant", { link = 'Constant' })
  -- Data type (maybe better in turquoise?):
  hl(0, "@type", { link = 'Type' })
  hl(0, "@function", { link = 'Function' })
  hl(0, "@keyword", { link = 'Keyword' })
  hl(0, "@character", { link = 'Character' })
  hl(0, "@conditional", { link = 'Conditional' })
  hl(0, "@exception", { link = 'Exception' })
  hl(0, "@include", { link = 'Include' })
  hl(0, "@operator", { link = 'Operator' })
  hl(0, "@preproc", { link = 'PreProc' })
  hl(0, "@keyword.return", { link = 'Keyword' })
  hl(0, "@method", { link = 'Function' })
  hl(0, "@method.call", { link = 'Function' })
  hl(0, "@keyword.function", { link = 'Keyword' })
  hl(0, "@function.call", { link = 'Function' })
  hl(0, "@text.todo", { link = 'Todo' })
  hl(0, "@text.title", { link = 'Title' })
  -- Open/close bracket of tags:
  hl(0, "@tag.delimiter", { link = 'Tag' })
  hl(0, "@punctuation.delimiter", { fg = c.white, bg = 'NONE' })
  hl(0, "@punctuation.bracket", { fg = c.white, bg = 'NONE' })
  hl(0, "@punctuation.special", { fg = c.white, bg = 'NONE' })
  hl(0, "@constant.builtin", { link = 'Constant' })
  -- Return types:
  hl(0, "@type.builtin", { fg = c.blue, bg = 'NONE' })
  -- In PHP, every parameter with it's data type:
  hl(0, "@parameter", { fg = 'NONE', bg = 'NONE' })
  hl(0, "@constructor", { fg = c.orange, bg = 'NONE' })
  hl(0, "@type.qualifier", { fg = c.blue, bg = 'NONE' })
  hl(0, "@storageclass", { fg = c.blue, bg = 'NONE' })
  hl(0, "@none", { fg = 'NONE', bg = 'NONE' })
  hl(0, "@tag.attribute", { fg = c.turquoise, bg = 'NONE' })
  -- Path of namespaces:
  hl(0, "@namespace", { fg = c.white, bg = 'NONE' })
  hl(0, "@function.builtin", { fg = c.orange, bg = 'NONE' })
  -- In PHP the @stuff in a doc block:
  hl(0, "@attribute", { fg = c.blue, bg = 'NONE' })
  -- All properties (css classes too):
  hl(0, "@property", { fg = c.purple, bg = 'NONE' })


  hl(0, "@field", { fg = c.purple, bg = 'NONE' })
  hl(0, "@keyword.operator", { fg = c.blue, bg = 'NONE' })
---------------------------------

  hl(0, "@define", { fg = c.orange, bg = c.red })
  hl(0, "@string.regex", { fg = c.blue, bg = c.red })
  hl(0, "@string.escape", { fg = c.green, bg = c.blue })
  hl(0, "@string.special", { fg = c.red, bg = c.orange })
  hl(0, "@character.special", { fg = c.white, bg = c.purple })


  hl(0, "@function.macro", { fg = c.black, bg = c.white })


  hl(0, "@repeat", { link = 'Repeat' })
  hl(0, "@debug", { link = 'Debug' })

  hl(0, "@label", { fg = c.blue, bg = 'NONE' })


  hl(0, "@type.definition", { fg = c.purple, bg = 'NONE' })







  hl(0, "@constant.macro", { fg = c.red, bg = c.green })
  hl(0, "@symbol", { fg = c.blue, bg = c.orange })
  hl(0, "@text", { link = 'None' })
  hl(0, "@text.strong", { link = 'Bold' })
  hl(0, "@text.emphasis", { link = 'Italic' })
  hl(0, "@text.underline", { link = 'Underlined' })
  hl(0, "@text.strike", { fg = 'NONE', bg = 'NONE', strikethrough = true, })
  hl(0, "@text.literal", { link = 'String' })
  hl(0, "@text.uri", { link = 'Underlined' })
  hl(0, "@text.math", { link = 'Special' })
  hl(0, "@text.environment", { link = 'Macro' })
  hl(0, "@text.environment.name", { link = 'Type' })
  hl(0, "@text.reference", { link = 'Constant' })
  hl(0, "@text.note", { link = 'SpecialComment' })
  hl(0, "@text.warning", { link = 'Todo' })
  hl(0, "@text.danger", { link = 'WarningMsg' })
  hl(0, "@tag", { link = 'Tag' })



------------------------------------------------------------------------------
-- CSS
------------------------------------------------------------------------------

  hl(0, "cssAtKeyword", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssAtRule", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssAttrComma", { fg = c.white, bg = 'NONE' })
  hl(0, "cssAttributeSelector", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssAttrRegion", { fg = c.white, bg = 'NONE' })
  hl(0, "cssBackgroundAttr", { fg = c.orange, bg = 'NONE' })
  hl(0, "cssBackgroundProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssBoxAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssBoxProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssBraceError", { fg = 'NONE', bg = c.red })
  hl(0, "cssBraces", { fg = c.white, bg = 'NONE' })
  hl(0, "cssClassName", { fg = c.turquoise, bg = 'NONE' })
  hl(0, "cssClassNameDot", { fg = c.white, bg = 'NONE' })
  hl(0, "cssColor", { fg = c.white, bg = 'NONE' })
  hl(0, "cssComment", { fg = c.red, bg = 'NONE' })
  hl(0, "cssCommonAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssCustomProp", { fg = c.purple, bg = 'NONE' })
  hl(0, "cssError", { fg = 'NONE', bg = c.red })
  hl(0, "cssFlexibleBoxAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssFlexibleBoxProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssFontAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssFontDescriptorProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssFontProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssFunction", { fg = c.orange, bg = 'NONE' })
  hl(0, "cssFunctionComma", { fg = c.white, bg = 'NONE' })
  hl(0, "cssFunctionName", { fg = c.orange, bg = 'NONE' })
  hl(0, "cssGeneratedContentProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssGridProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssIdentifier", { fg = c.turquoise, bg = 'NONE' })
  hl(0, "cssImportant", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssKeyFrameProp", { fg = c.white, bg = 'NONE' })
  hl(0, "cssListProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssMediaProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssMultiColumnProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssNoise", { fg = c.white, bg = 'NONE' })
  hl(0, "cssPageProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssPositioningAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssPositioningProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssPseudoClass", { fg = c.turquoise, bg = 'NONE' })
  hl(0, "cssPseudoClassId", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssSelectorOp", { fg = c.white, bg = 'NONE' })
  hl(0, "cssSelectorOp2", { fg = c.white, bg = 'NONE' })
  hl(0, "cssStringQQ", { fg = c.red_light, bg = 'NONE' })
  hl(0, "cssTableAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssTagName", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssTextAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssTextProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssTransformProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssTransitionAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssUIAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssUIProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssUnitDecorators", { fg = c.white, bg = 'NONE' })
  hl(0, "cssValueAngle", { fg = c.green_light, bg = 'NONE' })
  hl(0, "cssValueLength", { fg = c.green_light, bg = 'NONE' })
  hl(0, "cssValueNumber", { fg = c.green_light, bg = 'NONE' })
  hl(0, "cssBorderProp", { fg = c.white, bg = 'NONE' })

----------------------------------

  hl(0, "cssInclude", { fg = c.purple, bg = 'NONE' })
  hl(0, "cssPseudoClassLang", { fg = c.yellow, bg = 'NONE' })
  hl(0, "cssDefinition", { fg = c.fg, bg = 'NONE' })
  hl(0, "cssVendor", { fg = c.orange, bg = 'NONE' })
  hl(0, "cssStyle", { fg = c.fg, bg = 'NONE' })



------------------------------------------------------------------------------
-- HTML
------------------------------------------------------------------------------

  hl(0, "htmlArg", { fg = c.turquoise, bg = 'NONE' })
  hl(0, "htmlEndTag", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlH1", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlH2", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlH3", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlH4", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlH5", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlH6", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlLink", { fg = c.white, bg = 'NONE', underline = true, })
  hl(0, "htmlSpecialChar", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlSpecialTagName", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlTag", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlTagN", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlTagName", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlTitle", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlString", { fg = c.red_light, bg = 'NONE' })
  hl(0, "htmlComment", { fg = c.red, bg = 'NONE' })
  hl(0, "htmlLeadingSpace", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlHead", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlScriptTag", { fg = c.blue, bg = 'NONE' })



------------------------------------------------------------------------------
-- Git
------------------------------------------------------------------------------

  hl(0, "SignAdd", { fg = c.green, bg = 'NONE' })
  hl(0, "SignChange", { fg = c.blue, bg = 'NONE' })
  hl(0, "SignDelete", { fg = c.red, bg = 'NONE' })
  hl(0, "GitSignsAdd", { fg = c.green, bg = 'NONE' })
  hl(0, "GitSignsChange", { fg = c.blue, bg = 'NONE' })
  hl(0, "GitSignsDelete", { fg = c.red, bg = 'NONE' })

-----------------------

  hl(0, "DiffText", { fg = c.alt_bg, bg = c.sign_delete })
  hl(0, "DiffAdd", { fg = c.alt_bg, bg = c.sign_add })
  hl(0, "DiffChange", { fg = c.alt_bg, bg = c.sign_change, underline = true, })
  hl(0, "DiffDelete", { fg = c.alt_bg, bg = c.sign_delete })



------------------------------------------------------------------------------
-- JSON
------------------------------------------------------------------------------

  hl(0, "jsonNoise", { fg = c.white, bg = 'NONE' })
  hl(0, "jsonKeyword", { fg = c.blue, bg = 'NONE' })
  hl(0, "jsonKeywordMatch", { fg = c.white, bg = 'NONE' })
  hl(0, "jsonQuote", { fg = c.white, bg = 'NONE' })
  hl(0, "jsonString", { fg = c.red_light, bg = 'NONE' })
  hl(0, "jsonEscape", { fg = c.blue, bg = 'NONE' })
  hl(0, "jsonBoolean", { fg = c.blue, bg = 'NONE' })
  hl(0, "jsonBraces", { fg = c.white, bg = 'NONE' })



------------------------------------------------------------------------------
-- Lua
------------------------------------------------------------------------------

  hl(0, "luaFunction", { fg = c.blue, bg = 'NONE' })
  hl(0, "luaCond", { fg = c.blue, bg = 'NONE' })
  hl(0, "luaStatement", { fg = c.blue, bg = 'NONE' })
  hl(0, "luaOperator", { fg = c.blue, bg = 'NONE' })
  hl(0, "luaSymbolOperator", { fg = c.white, bg = 'NONE' })
  hl(0, "luaComment", { fg = c.red, bg = 'NONE' })
  hl(0, "luaConstant", { fg = c.purple, bg = 'NONE' })
  hl(0, "luaString2", { fg = c.red_light, bg = 'NONE' })
  hl(0, "luaStringDelimiter", { fg = c.red_light, bg = 'NONE' })
  hl(0, "luaString", { fg = c.red_light, bg = 'NONE' })
  hl(0, "luaNumber", { fg = c.green_light, bg = 'NONE' })
  hl(0, "luaTable", { fg = c.white, bg = 'NONE' })
  hl(0, "luaFunc", { fg = c.orange, bg = 'NONE' })



------------------------------------------------------------------------------
-- Markdown
------------------------------------------------------------------------------

  hl(0, "markdownBlockquote", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownBold", { fg = c.white, bg = 'NONE', bold = true, })
  hl(0, "markdownBoldDelimiter", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownCode", { fg = c.green, bg = 'NONE' })
  hl(0, "markdownCodeBlock", { fg = c.green, bg = 'NONE' })
  hl(0, "markdownCodeDelimiter", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownH1", { fg = c.white, bg = 'NONE' , bold = true })
  hl(0, "markdownH2", { fg = c.white, bg = 'NONE' , bold = true })
  hl(0, "markdownH3", { fg = c.white, bg = 'NONE' , bold = true })
  hl(0, "markdownH4", { fg = c.white, bg = 'NONE' , bold = true })
  hl(0, "markdownH5", { fg = c.white, bg = 'NONE' , bold = true })
  hl(0, "markdownH6", { fg = c.white, bg = 'NONE' , bold = true })
  hl(0, "markdownHeadingDelimiter", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownItalic", { fg = c.white, bg = 'NONE', italic = true })
  hl(0, "markdownLinkDelimiter", { fg = c.white, bg = 'NONE' })
  hl(0, "markdownLinkText", { fg = c.white, bg = 'NONE' })
  hl(0, "markdownListMarker", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownOrderedListMarker", { fg = c.blue, bg = 'NONE' })
  if vim.fn.has("nvim-0.7.3") == 1 then
    hl(0, "markdownUrl", { fg = c.blue_link, bg = 'NONE', underdotted = true, })
  else
    hl(0, "markdownUrl", { fg = c.blue_link, bg = 'NONE', underdot = true, })
  end

--------------------------------

  hl(0, "markdownHeadingRule", { fg = c.fg, bg = 'NONE', bold = true, })
  hl(0, "markdownId", { link = 'Identifier' })
  hl(0, "markdownIdDeclaration", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownIdDelimiter", { fg = c.light_gray, bg = 'NONE' })
  hl(0, "markdownBoldItalic", { fg = c.yellow, bg = 'NONE', bold = true, italic = true, })
  hl(0, "markdownRule", { fg = c.gray, bg = 'NONE' })
  hl(0, "markdownFootnote", { fg = c.orange, bg = 'NONE' })
  hl(0, "markdownFootnoteDefinition", { fg = c.orange, bg = 'NONE' })
  hl(0, "markdownEscape", { fg = c.yellow, bg = 'NONE' })





  -- Whichkey
  --hl(0, "WhichKey", { fg = c.purple, bg = 'NONE' })
  --hl(0, "WhichKeySeperator", { fg = c.green, bg = 'NONE' })
  --hl(0, "WhichKeyGroup", { fg = c.blue, bg = 'NONE' })
  --hl(0, "WhichKeyDesc", { fg = c.fg, bg = 'NONE' })
  --hl(0, "WhichKeyFloat", { fg = 'NONE', bg = c.alt_bg })



------------------------------------------------------------------------------
-- Language Server Protocol (LSP)
------------------------------------------------------------------------------

  hl(0, "DiagnosticInfo", { fg = c.blue, bg = 'NONE' })
  hl(0, "DiagnosticError", { fg = c.red, bg = 'NONE' })
  hl(0, "DiagnosticHint", { fg = c.blue, bg = 'NONE' })
  hl(0, "DiagnosticWarn", { fg = c.orange, bg = 'NONE' })
  hl(0, "DiagnosticVirtualTextHint", { fg = c.blue, bg = c.hint_bg })
  hl(0, "DiagnosticVirtualTextInfo", { fg = c.blue, bg = c.info_bg })
  hl(0, "DiagnosticVirtualTextWarn", { fg = c.orange, bg = c.warn_bg })
  hl(0, "DiagnosticVirtualTextError", { fg = c.red, bg = c.error_bg })
  hl(0, "LspDiagnosticsError", { fg = c.red, bg = 'NONE' })
  hl(0, "LspDiagnosticsWarning", { fg = c.orange, bg = 'NONE' })
  hl(0, "LspDiagnosticsInfo", { fg = c.blue, bg = 'NONE' })
  hl(0, "LspDiagnosticsHint", { fg = c.blue, bg = 'NONE' })
  hl(0, "LspCodeLens", { fg = c.purple, bg = 'NONE', italic = true, })
  hl(0, "LspCodeLensSeparator", { fg = c.purple, bg = 'NONE', italic = true, })
  hl(0, "DiagnosticUnderlineHint", { fg = 'NONE', bg = 'NONE', sp = c.blue, undercurl = true, })
  hl(0, "DiagnosticUnderlineInfo", { fg = 'NONE', bg = 'NONE', sp = c.blue, undercurl = true, })
  hl(0, "DiagnosticUnderlineWarn", { fg = 'NONE', bg = 'NONE', sp = c.orange, undercurl = true, })
  hl(0, "DiagnosticUnderlineError", { fg = 'NONE', bg = 'NONE', sp = c.red, undercurl = true, })

----------------

  hl(0, "DiagnosticOther", { fg = c.purple, bg = 'NONE' })
  hl(0, "DiagnosticSignHint", { link = 'DiagnosticHint' })
  hl(0, "DiagnosticSignInfo", { link = 'DiagnosticInfo' })
  hl(0, "DiagnosticSignWarn", { link = 'DiagnosticWarn' })
  hl(0, "DiagnosticSignError", { link = 'DiagnosticError' })
  hl(0, "DiagnosticSignOther", { link = 'DiagnosticOther' })
  hl(0, "DiagnosticSignWarning", { link = 'DiagnosticWarn' })
  hl(0, "DiagnosticFloatingHint", { link = 'DiagnosticHint' })
  hl(0, "DiagnosticFloatingInfo", { link = 'DiagnosticInfo' })
  hl(0, "DiagnosticFloatingWarn", { link = 'DiagnosticWarn' })
  hl(0, "DiagnosticFloatingError", { link = 'DiagnosticError' })
  hl(0, "DiagnosticSignInformation", { link = 'DiagnosticInfo' })
  hl(0, "LspDiagnosticsInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsDefaultError", { link = 'LspDiagnosticsError' })
  hl(0, "LspDiagnosticsDefaultWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "LspDiagnosticsDefaultInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsDefaultInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsDefaultHint", { link = 'LspDiagnosticsHint' })
  hl(0, "LspDiagnosticsVirtualTextError", { link = 'DiagnosticVirtualTextError' })
  hl(0, "LspDiagnosticsVirtualTextWarning", { link = 'DiagnosticVirtualTextWarn' })
  hl(0, "LspDiagnosticsVirtualTextInformation", { link = 'DiagnosticVirtualTextInfo' })
  hl(0, "LspDiagnosticsVirtualTextInfo", { link = 'DiagnosticVirtualTextInfo' })
  hl(0, "LspDiagnosticsVirtualTextHint", { link = 'DiagnosticVirtualTextHint' })
  hl(0, "LspDiagnosticsFloatingError", { link = 'LspDiagnosticsError' })
  hl(0, "LspDiagnosticsFloatingWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "LspDiagnosticsFloatingInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsFloatingInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsFloatingHint", { link = 'LspDiagnosticsHint' })
  hl(0, "LspDiagnosticsSignError", { link = 'LspDiagnosticsError' })
  hl(0, "LspDiagnosticsSignWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "LspDiagnosticsSignInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsSignInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsSignHint", { link = 'LspDiagnosticsHint' })
  hl(0, "NvimTreeLspDiagnosticsError", { link = 'LspDiagnosticsError' })
  hl(0, "NvimTreeLspDiagnosticsWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "NvimTreeLspDiagnosticsInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "NvimTreeLspDiagnosticsInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "NvimTreeLspDiagnosticsHint", { link = 'LspDiagnosticsHint' })
  hl(0, "LspDiagnosticsUnderlineError", { link = 'DiagnosticUnderlineError' })
  hl(0, "LspDiagnosticsUnderlineWarning", { link = 'DiagnosticUnderlineWarn' })
  hl(0, "LspDiagnosticsUnderlineInformation", { link = 'DiagnosticUnderlineInfo' })
  hl(0, "LspDiagnosticsUnderlineInfo", { link = 'DiagnosticUnderlineInfo' })
  hl(0, "LspDiagnosticsUnderlineHint", { link = 'DiagnosticUnderlineHint' })
  hl(0, "LspReferenceRead", { fg = 'NONE', bg = c.reference })
  hl(0, "LspReferenceText", { fg = 'NONE', bg = c.reference })
  hl(0, "LspReferenceWrite", { fg = 'NONE', bg = c.reference })
  hl(0, "IlluminatedWordRead", { link = 'LspReferenceRead' })
  hl(0, "IlluminatedWordText", { link = 'LspReferenceText' })
  hl(0, "IlluminatedWordWrite", { link = 'LspReferenceWrite' })

  -- Quickscope
  hl(0, "QuickScopePrimary", { fg = '#ff007c', bg = 'NONE', underline = true, })
  hl(0, "QuickScopeSecondary", { fg = '#00dfff', bg = 'NONE', underline = true, })

  -- Telescope
  hl(0, "TelescopeSelection", { fg = 'NONE', bg = c.ui2_blue })
  hl(0, "TelescopeSelectionCaret", { fg = c.red, bg = c.ui2_blue })
  hl(0, "TelescopeMatching", { fg = c.info, bg = 'NONE', bold = true, italic = true, })
  hl(0, "TelescopeBorder", { fg = c.alt_fg, bg = 'NONE' })
  hl(0, "TelescopeNormal", { fg = c.fg, bg = c.menu_bg })
  hl(0, "TelescopePromptPrefix", { fg = c.hint, bg = 'NONE' })
  hl(0, "TelescopePromptTitle", { fg = c.ui_orange, bg = 'NONE', bold = true, })
  hl(0, "TelescopeResultsTitle", { fg = c.ui_orange, bg = 'NONE', bold = true, })
  hl(0, "TelescopePreviewTitle", { fg = c.ui_orange, bg = 'NONE', bold = true, })
  hl(0, "TelescopePromptCounter", { fg = c.red, bg = 'NONE' })
  hl(0, "TelescopePreviewHyphen", { fg = c.red, bg = 'NONE' })

  -- Lir
  hl(0, "LirFloatNormal", { fg = c.fg, bg = c.alt_bg })
  hl(0, "LirDir", { link = 'Directory' })
  hl(0, "LirSymLink", { fg = c.cyan, bg = 'NONE' })
  hl(0, "LirEmptyDirText", { fg = c.gray, bg = 'NONE', italic = true, })

  -- IndentBlankline
  --hl(0, "IndentBlanklineContextChar", { fg = c.context, bg = 'NONE' })
  --hl(0, "IndentBlanklineContextStart", { fg = 'NONE', bg = 'NONE', underline = true, })
  --hl(0, "IndentBlanklineChar", { fg = c.dark_gray, bg = 'NONE' })

  -- Dashboard
  --hl(0, "DashboardHeader", { fg = c.blue, bg = 'NONE' })
  --hl(0, "DashboardCenter", { fg = c.purple, bg = 'NONE' })
  --hl(0, "DashboardFooter", { fg = c.cyan, bg = 'NONE' })

  -- DiffView
  hl(0, "DiffViewNormal", { fg = c.gray, bg = c.alt_bg })
  hl(0, "DiffviewStatusAdded", { fg = c.sign_add, bg = 'NONE' })
  hl(0, "DiffviewStatusModified", { fg = c.sign_change, bg = 'NONE' })
  hl(0, "DiffviewStatusRenamed", { fg = c.sign_change, bg = 'NONE' })
  hl(0, "DiffviewStatusDeleted", { fg = c.sign_delete, bg = 'NONE' })
  hl(0, "DiffviewFilePanelInsertion", { fg = c.sign_add, bg = 'NONE' })
  hl(0, "DiffviewFilePanelDeletion", { fg = c.sign_delete, bg = 'NONE' })
  hl(0, "DiffviewVertSplit", { fg = 'NONE', bg = c.bg })

  -- Bookmarks
  hl(0, "BookmarkSign", { fg = c.sign_change, bg = 'NONE' })
  hl(0, "BookmarkAnnotationSign", { fg = c.yellow, bg = 'NONE' })
  hl(0, "BookmarkLine", { fg = c.ui2_blue, bg = 'NONE' })
  hl(0, "BookmarkAnnotationLine", { fg = c.ui2_blue, bg = 'NONE' })

  -- Bqf
  hl(0, "BqfPreviewBorder", { fg = c.fg, bg = 'NONE' })
  hl(0, "BqfPreviewRange", { fg = 'NONE', bg = c.ui2_blue })
  hl(0, "BqfSign", { fg = c.ui_orange, bg = 'NONE' })



------------------------------------------------------------------------------
-- CMP
------------------------------------------------------------------------------

  hl(0, "CmpItemAbbrMatch", { fg = c.black, bg = c.gray })
  hl(0, "CmpItemAbbrDeprecated", { fg = c.gray, bg = 'NONE', strikethrough = true, })

---------------------

  hl(0, "CmpItemAbbrMatchFuzzy", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindFunction", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindMethod", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindConstructor", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindClass", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindEnum", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindEvent", { fg = c.info, bg = 'NONE' })
  hl(0, "CmpItemKindInterface", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindStruct", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindVariable", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindField", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindProperty", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindEnumMember", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindConstant", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindKeyword", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindModule", { fg = c.cyan, bg = 'NONE' })
  hl(0, "CmpItemKindValue", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindUnit", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindText", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindSnippet", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindFile", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindFolder", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindColor", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindReference", { fg = c.light_blue, bg = 'NONE' })
  hl(0, "CmpItemKindOperator", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindTypeParameter", { fg = c.blue, bg = 'NONE' })



------------------------------------------------------------------------------
-- PHP
------------------------------------------------------------------------------

  hl(0, "phpTodo", { fg = c.red, bg = 'NONE', bold = true })
  hl(0, "phpComment", { fg = c.red, bg = 'NONE' })
  hl(0, "phpRegion", { fg = c.turquoise, bg = 'NONE' })
  hl(0, "phpInclude", { fg = c.blue, bg = 'NONE' })
  hl(0, "phpClass", { fg = c.blue, bg = 'NONE' })
  hl(0, "phpClasses", { fg = c.blue, bg = 'NONE' })
  hl(0, "phpFunction", { fg = c.orange, bg = 'NONE' })
  hl(0, "phpType", { fg = c.blue, bg = 'NONE' })
  hl(0, "phpKeyword", { fg = c.blue, bg = 'NONE' })
  hl(0, "phpVarSelector", { fg = c.purple, bg = 'NONE' })
  hl(0, "phpIdentifier", { fg = c.purple, bg = 'NONE' })
  hl(0, "phpMethod", { fg = c.orange, bg = 'NONE' })
  hl(0, "phpMethodsVar", { fg = c.orange, bg = 'NONE' })
  hl(0, "phpMemberSelector", { fg = c.white, bg = 'NONE' })
  hl(0, "phpStorageClass", { fg = c.blue, bg = 'NONE' })
  hl(0, "phpDefine", { fg = c.blue, bg = 'NONE' })
  hl(0, "phpSpecialFunction", { fg = c.orange, bg = 'NONE' })
  hl(0, "phpParent", { fg = c.white, bg = 'NONE' })










  -- Navic
  hl(0, "NavicIconsFile", { link = 'CmpItemKindFile' })
  hl(0, "NavicIconsModule", { link = 'CmpItemKindModule' })
  hl(0, "NavicIconsNamespace", { link = 'CmpItemKindModule' })
  hl(0, "NavicIconsPackage", { link = 'CmpItemKindModule' })
  hl(0, "NavicIconsClass", { link = 'CmpItemKindClass' })
  hl(0, "NavicIconsMethod", { link = 'CmpItemKindMethod' })
  hl(0, "NavicIconsProperty", { link = 'CmpItemKindProperty' })
  hl(0, "NavicIconsField", { link = 'CmpItemKindField' })
  hl(0, "NavicIconsConstructor", { link = 'CmpItemKindConstructor' })
  hl(0, "NavicIconsEnum", { link = 'CmpItemKindEnum' })
  hl(0, "NavicIconsInterface", { link = 'CmpItemKindInterface' })
  hl(0, "NavicIconsFunction", { link = 'CmpItemKindFunction' })
  hl(0, "NavicIconsVariable", { link = 'CmpItemKindVariable' })
  hl(0, "NavicIconsConstant", { link = 'CmpItemKindConstant' })
  hl(0, "NavicIconsString", { link = 'String' })
  hl(0, "NavicIconsNumber", { link = 'Number' })
  hl(0, "NavicIconsBoolean", { link = 'Boolean' })
  hl(0, "NavicIconsArray", { link = 'CmpItemKindClass' })
  hl(0, "NavicIconsObject", { link = 'CmpItemKindClass' })
  hl(0, "NavicIconsKey", { link = 'CmpItemKindKeyword' })
  hl(0, "NavicIconsKeyword", { link = 'CmpItemKindKeyword' })
  hl(0, "NavicIconsNull", { fg = c.blue, bg = 'NONE' })
  hl(0, "NavicIconsEnumMember", { link = 'CmpItemKindEnumMember' })
  hl(0, "NavicIconsStruct", { link = 'CmpItemKindStruct' })
  hl(0, "NavicIconsEvent", { link = 'CmpItemKindEvent' })
  hl(0, "NavicIconsOperator", { link = 'CmpItemKindOperator' })
  hl(0, "NavicIconsTypeParameter", { link = 'CmpItemKindTypeParameter' })
  hl(0, "NavicText", { fg = c.gray, bg = 'NONE' })
  hl(0, "NavicSeparator", { fg = c.context, bg = 'NONE' })

  -- Packer
  hl(0, "packerString", { fg = c.ui_orange, bg = 'NONE' })
  hl(0, "packerHash", { fg = c.ui4_blue, bg = 'NONE' })
  hl(0, "packerOutput", { fg = c.ui_purple, bg = 'NONE' })
  hl(0, "packerRelDate", { fg = c.gray, bg = 'NONE' })
  hl(0, "packerSuccess", { fg = c.success_green, bg = 'NONE' })
  hl(0, "packerStatusSuccess", { fg = c.ui4_blue, bg = 'NONE' })

  -- SymbolOutline
  hl(0, "SymbolsOutlineConnector", { fg = c.gray, bg = 'NONE' })
  hl(0, "FocusedSymbol", { fg = 'NONE', bg = '#36383F' })

  -- Notify
  hl(0, "NotifyERRORBorder", { fg = '#8A1F1F', bg = 'NONE' })
  hl(0, "NotifyWARNBorder", { fg = '#79491D', bg = 'NONE' })
  hl(0, "NotifyINFOBorder", { fg = c.ui_blue, bg = 'NONE' })
  hl(0, "NotifyDEBUGBorder", { fg = c.gray, bg = 'NONE' })
  hl(0, "NotifyTRACEBorder", { fg = '#4F3552', bg = 'NONE' })
  hl(0, "NotifyERRORIcon", { fg = c.error, bg = 'NONE' })
  hl(0, "NotifyWARNIcon", { fg = c.warn, bg = 'NONE' })
  hl(0, "NotifyINFOIcon", { fg = c.ui4_blue, bg = 'NONE' })
  hl(0, "NotifyDEBUGIcon", { fg = c.gray, bg = 'NONE' })
  hl(0, "NotifyTRACEIcon", { fg = c.ui_purple, bg = 'NONE' })
  --[[ hl(0, "NotifyERRORTitle", { fg = c.error, bg = 'NONE' })
  hl(0, "NotifyWARNTitle", { fg = c.warn, bg = 'NONE' })
  hl(0, "NotifyINFOTitle", { fg = c.ui4_blue, bg = 'NONE' })
  hl(0, "NotifyDEBUGTitle", { fg = c.gray, bg = 'NONE' })
  hl(0, "NotifyTRACETitle", { fg = c.ui_purple, bg = 'NONE' }) ]]
  -- hl(0, "NeogitFold", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitStash", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitDiffAdd", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitObjectId", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitRebasing", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitDiffDelete", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitRebaseDone", { fg = c.ui_purple, bg = 'NONE' })
  hl(0, "NeogitBranch", { fg = c.ui_purple, bg = 'NONE' })
  hl(0, "NeogitRemote", { fg = c.yellow_orange, bg = 'NONE' })
  hl(0, "NeogitStashes", { fg = c.ui_purple, bg = 'NONE' })
  hl(0, "NeogitUnmergedInto", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnpulledFrom", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitRecentcommits", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitStagedchanges", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUntrackedfiles", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnmergedchanges", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnpulledchanges", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnstagedchanges", { fg = c.blue, bg = 'NONE' })
  -- hl(0, "NoiceCmdline", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIcon", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconCmdline", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconFilter", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconHelp", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconIncRename", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconInput", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconLua", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconSearch", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopup", { fg = c.hint, bg = c.hint })
hl(0, "NoiceCmdlinePopupBorder", { fg = c.hint, bg = "NONE" })
  -- hl(0, "NoiceCmdlinePopupBorderCmdline", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderFilter", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderHelp", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderIncRename", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderInput", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderLua", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderSearch", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePrompt", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindClass", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindColor", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindConstant", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindConstructor", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindDefault", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindEnum", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindEnumMember ", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindField", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindFile", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindFolder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindFunction", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindInterface", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindKeyword", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindMethod", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindModule", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindProperty", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindSnippet", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindStruct", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindText", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindUnit", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindValue", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindVariable", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemMenu", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemWord", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceConfirm", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceConfirmBorder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCursor", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatConfirm", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatConfirmDefault", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatDate", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatEvent", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatKind", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelDebug", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelError", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelInfo", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelOff", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelTrace", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelWarn", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatProgressDone", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatProgressTodo", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatTitle", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceLspProgressClient", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceLspProgressSpinner", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceLspProgressTitle", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceMini", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopup", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupBorder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenu", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenuBorder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenuMatch", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenuSelected", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceScrollbar", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceScrollbarThumb", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceSplit", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceSplitBorder", { fg = c.hint, bg = c.hint })
  hl(0, "NoiceVirtualText", { fg = c.hint, bg = c.hint_bg })
  -- Noice

  -- TreesitterContext
  hl(0, "TreesitterContext", { fg = 'NONE', bg = c.alt_bg })

  -- Crates
  hl(0, "CratesNvimLoading", { fg = c.hint, bg = 'NONE' })
  hl(0, "CratesNvimVersion", { fg = c.hint, bg = 'NONE' })

  -- Misc
  hl(0, "diffAdded", { fg = c.sign_add, bg = 'NONE' })
  hl(0, "diffRemoved", { fg = c.sign_delete, bg = 'NONE' })
  hl(0, "diffFileId", { fg = c.blue, bg = 'NONE', bold = true, reverse = true, })
  hl(0, "diffFile", { fg = c.alt_bg, bg = 'NONE' })
  hl(0, "diffNewFile", { fg = c.green, bg = 'NONE' })
  hl(0, "diffOldFile", { fg = c.red, bg = 'NONE' })
  hl(0, "debugPc", { fg = 'NONE', bg = c.ui5_blue })
  hl(0, "debugBreakpoint", { fg = c.red, bg = 'NONE', reverse = true, })
  hl(0, "CodiVirtualText", { fg = c.hint, bg = 'NONE' })
  hl(0, "SniprunVirtualTextOk", { fg = c.hint, bg = 'NONE' })
  hl(0, "SniprunFloatingWinOk", { fg = c.hint, bg = 'NONE' })
  hl(0, "SniprunVirtualTextErr", { fg = c.error, bg = 'NONE' })
  hl(0, "SniprunFloatingWinErr", { fg = c.error, bg = 'NONE' })
  hl(0, "DapBreakpoint", { fg = c.error, bg = 'NONE' })

  -- Language
  hl(0, "xmlTag", { fg = c.cyan, bg = 'NONE' })
  hl(0, "xmlTagName", { fg = c.cyan, bg = 'NONE' })
  hl(0, "xmlEndTag", { fg = c.cyan, bg = 'NONE' })
  -- hl(0, "yamlPlainScalar", { fg = c.orange, bg = 'NONE' })
  -- hl(0, "yamlTSField", { fg = c.blue, bg = 'NONE' })
  hl(0, "hclTSPunctSpecial", { fg = c.alt_fg, bg = 'NONE' })
  hl(0, "yamlBlockMappingKey", { fg = c.blue, bg = 'NONE' })
  hl(0, "tomlTSProperty", { fg = c.blue, bg = 'NONE' })
  hl(0, "zshKSHFunction", { link = "Function" })
  hl(0, "zshVariableDef", { link = "Constant" })
end

return theme
