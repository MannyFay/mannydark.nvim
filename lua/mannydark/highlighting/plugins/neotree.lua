-------------------------------------------------------------------------------
-- Neo-tree.nvim - File explorer
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local neotree   = {}


-------------------------------------------------------------------------------
-- Settings

neotree.setupHighlighting = function()
  -- Base window highlights
  highlight(0, 'NeoTreeNormal',           { fg = colors.white,     bg = 'NONE'                          })  -- Normal text
  highlight(0, 'NeoTreeNormalNC',         { fg = colors.white,     bg = 'NONE'                          })  -- Inactive window
  highlight(0, 'NeoTreeEndOfBuffer',      { fg = colors.black,     bg = 'NONE'                          })  -- End of buffer tildes
  highlight(0, 'NeoTreeSignColumn',       { fg = colors.white,     bg = 'NONE'                          })  -- Sign column
  highlight(0, 'NeoTreeCursorLine',       { fg = 'NONE',           bg = colors.grayDark                 })  -- Current line
  highlight(0, 'NeoTreeWinSeparator',     { fg = colors.grayDark,  bg = 'NONE'                          })  -- Window separator
  highlight(0, 'NeoTreeStatusLine',       { fg = colors.white,     bg = 'NONE'                          })  -- Status line
  highlight(0, 'NeoTreeStatusLineNC',     { fg = colors.gray,      bg = 'NONE'                          })  -- Inactive status line

  -- File and directory names
  highlight(0, 'NeoTreeFileName',         { fg = colors.white,     bg = 'NONE'                          })  -- Regular file names
  highlight(0, 'NeoTreeFileNameOpened',   { fg = colors.white,     bg = 'NONE',          bold = true    })  -- Currently opened files
  highlight(0, 'NeoTreeDirectoryName',    { fg = colors.blue,      bg = 'NONE'                          })  -- Directory names
  highlight(0, 'NeoTreeRootName',         { fg = colors.blue,      bg = 'NONE',          bold = true    })  -- Root directory name
  highlight(0, 'NeoTreeDotfile',          { fg = colors.gray,      bg = 'NONE'                          })  -- Dotfiles (hidden files)
  highlight(0, 'NeoTreeHiddenByName',     { fg = colors.gray,      bg = 'NONE'                          })  -- Files hidden by name filter

  -- Icons
  highlight(0, 'NeoTreeFileIcon',         { fg = colors.white,     bg = 'NONE'                          })  -- File icons
  highlight(0, 'NeoTreeDirectoryIcon',    { fg = colors.blue,      bg = 'NONE'                          })  -- Directory icons
  highlight(0, 'NeoTreeClosedIcon',       { fg = colors.blue,      bg = 'NONE'                          })  -- Closed directory icon
  highlight(0, 'NeoTreeOpenedIcon',       { fg = colors.blue,      bg = 'NONE'                          })  -- Opened directory icon

  -- Symbolic links
  highlight(0, 'NeoTreeSymbolicLinkTarget', { fg = colors.turquoise, bg = 'NONE'                        })  -- Symlink target path
  highlight(0, 'NeoTreeSymlink',          { fg = colors.turquoise, bg = 'NONE'                          })  -- Symlink name

  -- Tree structure
  highlight(0, 'NeoTreeIndentMarker',     { fg = colors.gray,      bg = 'NONE'                          })  -- Vertical indent lines
  highlight(0, 'NeoTreeExpander',         { fg = colors.gray,      bg = 'NONE'                          })  -- Expand/collapse arrows
  highlight(0, 'NeoTreeDimText',          { fg = colors.gray,      bg = 'NONE'                          })  -- Dimmed/faded text

  -- Git status highlights
  highlight(0, 'NeoTreeGitAdded',         { fg = colors.green,     bg = 'NONE'                          })  -- Added files
  highlight(0, 'NeoTreeGitDeleted',       { fg = colors.red,       bg = 'NONE'                          })  -- Deleted files
  highlight(0, 'NeoTreeGitModified',      { fg = colors.orange,    bg = 'NONE'                          })  -- Modified files
  highlight(0, 'NeoTreeGitConflict',      { fg = colors.red,       bg = 'NONE',          bold = true, italic = true })  -- Merge conflicts
  highlight(0, 'NeoTreeGitUntracked',     { fg = colors.red,       bg = 'NONE'                          })  -- Untracked files
  highlight(0, 'NeoTreeGitUnstaged',      { fg = colors.orange,    bg = 'NONE'                          })  -- Unstaged changes
  highlight(0, 'NeoTreeGitStaged',        { fg = colors.green,     bg = 'NONE'                          })  -- Staged changes
  highlight(0, 'NeoTreeGitIgnored',       { fg = colors.gray,      bg = 'NONE'                          })  -- Ignored files
  highlight(0, 'NeoTreeGitRenamed',       { fg = colors.purple,    bg = 'NONE'                          })  -- Renamed files

  -- Modified indicator
  highlight(0, 'NeoTreeModified',         { fg = colors.orange,    bg = 'NONE'                          })  -- Unsaved file indicator

  -- Floating window
  highlight(0, 'NeoTreeFloatBorder',      { fg = colors.turquoise, bg = 'NONE'                          })  -- Float border
  highlight(0, 'NeoTreeFloatNormal',      { fg = colors.white,     bg = 'NONE'                          })  -- Float normal text
  highlight(0, 'NeoTreeFloatTitle',       { fg = colors.turquoise, bg = 'NONE',          bold = true    })  -- Float title

  -- Title bar
  highlight(0, 'NeoTreeTitleBar',         { fg = colors.black,     bg = colors.turquoise, bold = true   })  -- Title bar

  -- Tabs
  highlight(0, 'NeoTreeTabActive',        { fg = colors.white,     bg = colors.grayDark, bold = true    })  -- Active tab
  highlight(0, 'NeoTreeTabInactive',      { fg = colors.gray,      bg = 'NONE'                          })  -- Inactive tab
  highlight(0, 'NeoTreeTabSeparatorActive',   { fg = colors.turquoise, bg = colors.grayDark             })  -- Active tab separator
  highlight(0, 'NeoTreeTabSeparatorInactive', { fg = colors.gray,      bg = 'NONE'                      })  -- Inactive tab separator

  -- Preview
  highlight(0, 'NeoTreePreview',          { fg = 'NONE',           bg = colors.grayDark                 })  -- Preview window highlight

  -- Filter/search
  highlight(0, 'NeoTreeFilterTerm',       { fg = colors.turquoise, bg = 'NONE',          bold = true    })  -- Filter search term

  -- Diagnostics (LSP)
  highlight(0, 'NeoTreeDiagnosticError',  { fg = colors.red,       bg = 'NONE'                          })  -- Error diagnostic
  highlight(0, 'NeoTreeDiagnosticWarn',   { fg = colors.orange,    bg = 'NONE'                          })  -- Warning diagnostic
  highlight(0, 'NeoTreeDiagnosticInfo',   { fg = colors.blue,      bg = 'NONE'                          })  -- Info diagnostic
  highlight(0, 'NeoTreeDiagnosticHint',   { fg = colors.turquoise, bg = 'NONE'                          })  -- Hint diagnostic

  -- Buffers source
  highlight(0, 'NeoTreeBufferNumber',     { fg = colors.gray,      bg = 'NONE'                          })  -- Buffer number

  -- Message/notifications
  highlight(0, 'NeoTreeMessage',          { fg = colors.gray,      bg = 'NONE',          italic = true  })  -- Messages
end

return neotree
