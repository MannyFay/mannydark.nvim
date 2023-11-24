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
  highlight(0, 'NvimTreeIndentMarker',              { fg = colors.gray,     bg = 'NONE'           })
  highlight(0, 'NvimTreeWinSeparator',              { fg = colors.grayDark, bg = colors.grayDark  })
  highlight(0, 'NvimTreeNormal',                    { fg = colors.white,    bg = 'NONE'           })
  highlight(0, 'NvimTreeFolderName',                { fg = colors.blue,     bg = 'NONE'           })
  highlight(0, 'NvimTreeOpenedFolderName',          { fg = colors.blue,     bg = 'NONE'           })
  highlight(0, 'NvimTreeEmptyFolderName',           { fg = colors.blue,     bg = 'NONE'           })
  highlight(0, 'NvimTreeEndOfBuffer',               { fg = colors.black,    bg = 'NONE'           })
  highlight(0, 'NvimTreeCursorLine',                { fg = 'NONE',          bg = colors.grayDark  })
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

  ----------------------- Not used by now:
  highlight(0, 'NvimTreeVertSplit',                 { fg = colors.red,      bg = colors.green     })
  highlight(0, 'NvimTreeSymlink',                   { fg = colors.pink,     bg = colors.orange    })
  highlight(0, 'NvimTreeSymlinkIcon',               { fg = colors.pink,     bg = colors.orange    })
  highlight(0, 'NvimTreeSymlinkFolderName',         { fg = colors.red,      bg = colors.green     })
  highlight(0, 'NvimTreeOpenedFolderIcon',          { fg = colors.pink,     bg = colors.orange    })
  highlight(0, 'NvimTreeClosedFolderIcon',          { fg = colors.pink,     bg = colors.orange    })
  highlight(0, 'NvimTreeFileIcon',                  { fg = colors.pink,     bg = colors.orange    })
  highlight(0, 'NvimTreeWindowPicker',              { fg = colors.pink,     bg = colors.orange    })
  highlight(0, 'NvimTreeNormalFloat',               { fg = colors.pink,     bg = colors.orange    })
  highlight(0, 'NvimTreeCursorLineNr',              { fg = colors.red,      bg = colors.green     })
  highlight(0, 'NvimTreeLineNr',                    { fg = colors.pink,     bg = colors.orange    })
  highlight(0, 'NvimTreeCursorColumn',              { fg = colors.pink,     bg = colors.orange    })
  highlight(0, 'NvimTreeLiveFilterPrefix',          { fg = colors.pink,     bg = colors.orange    })
  highlight(0, 'NvimTreeLiveFilterValue',           { fg = colors.pink,     bg = colors.orange    })
  highlight(0, 'NvimTreeBookmark',                  { fg = colors.pink,     bg = colors.orange    })
end

return nvimTree
