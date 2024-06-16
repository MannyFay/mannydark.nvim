-------------------------------------------------------------------------------
-- Stuff that has to be refactored
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local todoStuff = {}

-- Insert the following syntax highlighting too:
  -- Typescript
  -- React (jsx)
  -- graphQL
  -- SQL

--------------------------------------------------------------
-- Settings

todoStuff.setupHighlighting = function()
  ----------------------- Not used by now (maybe?!):

  -- Quickscope
  highlight(0, 'QuickScopePrimary',   { fg = '#ff007c', bg = 'NONE', underline = true, })
  highlight(0, 'QuickScopeSecondary', { fg = '#00dfff', bg = 'NONE', underline = true, })

  -- Lir
  highlight(0, 'LirFloatNormal',  { fg = colors.black, bg = colors.whithe                 })
  highlight(0, 'LirDir',          { fg = colors.black, bg = colors.whithe                 })
  highlight(0, 'LirSymLink',      { fg = colors.black, bg = colors.whithe                 })
  highlight(0, 'LirEmptyDirText', { fg = colors.black, bg = colors.whithe, italic = true, })

  -- IndentBlankline
  --highlight(0, 'IndentBlanklineContextChar',  { fg = colors.green, bg = colors.orange                   })
  --highlight(0, 'IndentBlanklineContextStart', { fg = colors.green, bg = colors.orange, underline = true })
  -- highlight(0, 'IndentBlanklineChar',         { fg = colors.green, bg = colors.orange                   })

  -- Dashboard
  --highlight(0, 'DashboardHeader', { fg = colors.blue, bg = colors.gray })
  --highlight(0, 'DashboardCenter', { fg = colors.blue, bg = colors.gray })
  --highlight(0, 'DashboardFooter', { fg = colors.blue, bg = colors.gray })


  -- Bookmarks
  highlight(0, 'BookmarkSign',           { fg = colors.orange, bg = colors.blue })
  highlight(0, 'BookmarkAnnotationSign', { fg = colors.orange, bg = colors.blue })
  highlight(0, 'BookmarkLine',           { fg = colors.orange, bg = colors.blue })
  highlight(0, 'BookmarkAnnotationLine', { fg = colors.orange, bg = colors.blue })

  -- Bqf
  highlight(0, 'BqfPreviewBorder', { fg = colors.red, bg = colors.orange })
  highlight(0, 'BqfPreviewRange',  { fg = colors.red, bg = colors.orange })
  highlight(0, 'BqfSign',          { fg = colors.red, bg = colors.orange })

  -- SymbolOutline
  highlight(0, 'SymbolsOutlineConnector', { fg = colors.orange, bg = colors.gray })
  highlight(0, 'FocusedSymbol',           { fg = colors.orange, bg = colors.gray })

  -- TreesitterContext
  --highlight(0, 'TreesitterContext', { fg = colors.white, bg = colors.purple })

  -- Crates
  highlight(0, 'CratesNvimLoading', { fg = colors.blue, bg = colors.gray })
  highlight(0, 'CratesNvimVersion', { fg = colors.blue, bg = colors.gray })

  -- Misc
  highlight(0, 'diffAdded',             { fg = colors.green, bg = "NONE"                               })  -- Lines/chars added in files in diff view.
  highlight(0, 'diffRemoved',           { fg = colors.red, bg = "NONE"                               })  -- Lines/chars removed in files in diff view.
  highlight(0, 'diffNewFile',           { fg = colors.white, bg = "NONE"                               })  -- Name of file with changes.
  highlight(0, 'diffOldFile',           { fg = colors.white, bg = "NONE"                             })  -- Name of of file before changes.

  highlight(0, 'diffFileId',            { fg = colors.green, bg = colors.white, bold = true, reverse = true, })
  highlight(0, 'diffFile',              { fg = colors.red, bg = colors.white                               })

  highlight(0, 'debugPc',               { fg = colors.grayDark, bg = colors.white                               })
  highlight(0, 'debugBreakpoint',       { fg = colors.grayDark, bg = colors.white                               })
  highlight(0, 'CodiVirtualText',       { fg = colors.grayDark, bg = colors.white                               })
  highlight(0, 'SniprunVirtualTextOk',  { fg = colors.grayDark, bg = colors.white                               })
  highlight(0, 'SniprunFloatingWinOk',  { fg = colors.grayDark, bg = colors.white                               })
  highlight(0, 'SniprunVirtualTextErr', { fg = colors.grayDark, bg = colors.white                               })
  highlight(0, 'SniprunFloatingWinErr', { fg = colors.grayDark, bg = colors.white                               })
  highlight(0, 'DapBreakpoint',         { fg = colors.grayDark, bg = colors.white                               })

  -- Language
  highlight(0, 'hclTSPunctSpecial',   { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'tomlTSProperty',      { fg = colors.pink, bg = colors.greenLight })
end

return todoStuff

