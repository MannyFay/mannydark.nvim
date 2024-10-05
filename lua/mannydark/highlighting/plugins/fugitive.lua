-------------------------------------------------------------------------------
-- Fugitive Plugin
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local fugitive  = {}


-------------------------------------------------------------------------------
-- Settings

fugitive.setupHighlighting = function()
  highlight(0, 'FugitiveblameUncommitted',     { fg = colors.red,    bg = 'NONE'                   })  -- Commit hash for uncommitted changes.
  highlight(0, 'FugitiveblameNotCommittedYet', { fg = colors.red,    bg = 'NONE'                   })  -- Text for lines not committed yet.
  highlight(0, 'FugitiveblameDelimiter',       { fg = colors.white,  bg = 'NONE'                   })  -- ( in blame window.
  highlight(0, 'FugitiveblameHashb2856e',      { fg = colors.blue,   bg = 'NONE'                   })  -- Commit hash.
  highlight(0, 'FugitiveblameHash7a6ec8',      { fg = colors.blue,   bg = 'NONE', underline = true })  -- Commit hash of a block.
  highlight(0, 'FugitiveblameAnnotation',      { fg = colors.purple, bg = 'NONE'                   })  -- Name of the author.
  highlight(0, 'FugitiveblameTime',            { fg = colors.green,  bg = 'NONE'                   })  -- Time of the commit.

  -- highlight(0, '',            { fg = colors.white,  bg = 'NONE'                           })  -- .


  ----------------------- Not used by now:

end

return fugitive













