-------------------------------------------------------------------------------
-- NeoGit Neovim Plugin
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local neoGit    = {}


--------------------------------------------------------------
-- Settings

neoGit.setupHighlighting = function()
  highlight(0, 'NeogitSubtleText',      { fg = colors.green,  bg = 'NONE' })  -- Heading of command legend.
  highlight(0, 'NeogitStatusHEAD',      { fg = colors.blue,   bg = 'NONE' })  -- 'HEAD' word.
  highlight(0, 'NeogitObjectId',        { fg = colors.blue,   bg = 'NONE' })  -- Commit hash.
  highlight(0, 'NeogitBranch',          { fg = colors.purple, bg = 'NONE' })  -- Branch name.
  highlight(0, 'NeogitStagedchanges',   { fg = colors.red,    bg = 'NONE' })  -- Staged changes.
  highlight(0, 'NeogitUnstagedchanges', { fg = colors.red,    bg = 'NONE' })  -- Un-staged changes.
  highlight(0, 'NeogitUntrackedfiles',  { fg = colors.red,    bg = 'NONE' })  -- Untracked files.
  highlight(0, 'NeogitFold',            { fg = colors.blue,   bg = 'NONE' })

  ----------------------- Not used by now:
  highlight(0, 'NeogitStash',           { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitDiffAdd',         { fg = colors.green, bg = colors.blue })
  highlight(0, 'NeogitRebasing',        { fg = colors.green, bg = colors.red })
  highlight(0, 'NeogitDiffDelete',      { fg = colors.green, bg = colors.redLight })
  highlight(0, 'NeogitRebaseDone',      { fg = colors.green, bg = colors.orange })
  highlight(0, 'NeogitRemote',          { fg = colors.green, bg = colors.purple })
  highlight(0, 'NeogitStashes',         { fg = colors.green, bg = colors.pink })
  highlight(0, 'NeogitUnmergedInto',    { fg = colors.green, bg = colors.gray })
  highlight(0, 'NeogitUnpulledFrom',    { fg = colors.green, bg = colors.white })
  highlight(0, 'NeogitRecentcommits',   { fg = colors.green, bg = colors.turquoise })
  highlight(0, 'NeogitUnmergedchanges', { fg = colors.pink, bg = colors.greenLignt })
  highlight(0, 'NeogitUnpulledchanges', { fg = colors.pink, bg = colors.blue })
  highlight(0, 'NeogitUnstagedchanges', { fg = colors.pink, bg = colors.turquoise })



end

return neoGit

