------------------------------------------------------------------------------
-- NvimTree Neovim Plugin
------------------------------------------------------------------------------

local colors      = require('mannydark.palette')
local highlight   = vim.api.nvim_set_hl
local nvimTree    = {}


--------------------------------------------------------------
-- Settings

nvimTree.setupHighlighting = function()
  highlight(0, 'NvimTreeRootFolder',                { fg = colors.blue,     bg = 'NONE'           })
  highlight(0, 'Directory',                         { fg = colors.blue,     bg = 'NONE'           })
  highlight(0, 'NvimTreeFolderIcon',                { fg = colors.blue,     bg = 'NONE'           })
  highlight(0, 'NvimTreeIndentMarker',              { fg = colors.grayDark, bg = 'NONE'           })  -- Vertical indent lines.
  highlight(0, 'NvimTreeWinSeparator',              { fg = colors.grayDark, bg = colors.grayDark  })
  highlight(0, 'NvimTreeNormal',                    { fg = colors.white,    bg = 'NONE'           })
  highlight(0, 'NvimTreeFolderName',                { fg = colors.blue,     bg = 'NONE'           })
  highlight(0, 'NvimTreeOpenedFolderName',          { fg = colors.blue,     bg = 'NONE'           })
  highlight(0, 'NvimTreeEmptyFolderName',           { fg = colors.blue,     bg = 'NONE'           })
  highlight(0, 'NvimTreeEndOfBuffer',               { fg = colors.black,    bg = 'NONE'           })
  highlight(0, 'NvimTreeCursorLine',                { fg = 'NONE',          bg = colors.grayDark  })  -- Current line.
  highlight(0, 'NvimTreeImageFile',                 { fg = colors.white,    bg = 'NONE'           })
  highlight(0, 'NvimTreeGitIgnored',                { fg = colors.gray,     bg = 'NONE'           })
  highlight(0, 'NvimTreeGitDeleted',                { fg = colors.gray,     bg = 'NONE'           })
  highlight(0, 'NvimTreeGitRenamed',                { fg = colors.red,      bg = 'NONE'           })
  highlight(0, 'NvimTreeGitNew',                    { fg = colors.red,      bg = 'NONE'           })
  highlight(0, 'NvimTreeGitDirty',                  { fg = colors.red,      bg = 'NONE'           })
  highlight(0, 'NvimTreeGitStaged',                 { fg = colors.orange,   bg = 'NONE'           })
  highlight(0, 'NvimTreeGitMerge',                  { fg = colors.orange,   bg = 'NONE'           })
  highlight(0, 'NvimTreeLspDiagnosticsError',       { fg = colors.red,      bg = 'NONE'           })
  highlight(0, 'NvimTreeLspDiagnosticsWarning',     { fg = colors.orange,   bg = 'NONE'           })
  highlight(0, 'NvimTreeLspDiagnosticsInformation', { fg = colors.blue,     bg = 'NONE'           })
  highlight(0, 'NvimTreeLspDiagnosticsHint',        { fg = colors.blue,     bg = 'NONE'           })
  highlight(0, 'NvimTreeFileIgnored',               { fg = colors.gray,     bg = 'NONE'           })
  highlight(0, 'NvimTreeFileDeleted',               { fg = colors.gray,     bg = 'NONE'           })
  highlight(0, 'nvimtreefilerenamed',               { fg = colors.red,      bg = 'NONE'           })
  highlight(0, 'NvimTreeFileNew',                   { fg = colors.red,      bg = 'NONE'           })
  highlight(0, 'NvimTreeFileDirty',                 { fg = colors.red,      bg = 'NONE'           })
  highlight(0, 'NvimTreeFileStaged',                { fg = colors.orange,   bg = 'NONE'           })
  highlight(0, 'NvimTreeFileMerge',                 { fg = colors.orange,   bg = 'NONE'           })
  highlight(0, 'NvimTreeModifiedFile',              { fg = colors.orange,   bg = 'NONE'           })
  highlight(0, 'NvimTreeOpenedFile',                { fg = colors.orange,   bg = 'NONE'           })
  highlight(0, 'NvimTreeNormalFloat',               { fg = colors.gray,     bg = 'NONE'           })
  highlight(0, 'NvimTreeExecFile',                  { fg = colors.white,    bg = 'NONE'           })
  highlight(0, 'NvimTreeSpecialFile',               { fg = colors.white,    bg = 'NONE'           })
  highlight(0, 'NvimTreeClosedFolderIcon',          { fg = colors.blue,     bg = 'NONE'           })  -- Icons of closed directories.
  highlight(0, 'NvimTreeOpenedFolderIcon',          { fg = colors.blue,     bg = 'NONE'           })  -- Icons of open directories.
  highlight(0, 'NvimTreeFileIcon',                  { fg = colors.white,    bg = 'NONE'           })  -- Icons not belong to WebDevIcons.
  highlight(0, 'NvimTreeSymlinkIcon',               { fg = colors.orange,   bg = 'NONE'           })  -- Icons of symlinks before filename.
  highlight(0, 'NvimTreeSymlink',                   { fg = colors.white,    bg = 'NONE'           })  -- Name and destination of symlinks.
  highlight(0, 'NvimTreeSymlinkFolderName',         { fg = colors.blue,     bg = 'NONE'           })  -- Name and destination of symlink directories.
  highlight(0, 'NvimTreeLineNr',                    { fg = colors.gray,     bg = 'NONE'           })  -- Line numbers.
  highlight(0, 'NvimTreeCursorLineNr',              { fg = colors.white,    bg = 'NONE'           })  -- Line number of the current line.

  ----------------------- Not used by now:
  highlight(0, 'NvimTreeVertSplit',                 { fg = colors.red,      bg = colors.green     })
  highlight(0, 'NvimTreeWindowPicker',              { fg = colors.green,    bg = colors.orange    })
  highlight(0, 'NvimTreeNormalFloat',               { fg = colors.turquoise,bg = colors.orange    })
  highlight(0, 'NvimTreeCursorColumn',              { fg = colors.orange,   bg = colors.blue      })
  highlight(0, 'NvimTreeLiveFilterPrefix',          { fg = colors.green,    bg = colors.blue      })
  highlight(0, 'NvimTreeLiveFilterValue',           { fg = colors.turquoise,bg = colors.blue      })
  highlight(0, 'NvimTreeBookmark',                  { fg = colors.orange,   bg = colors.blue      })
end

return nvimTree
