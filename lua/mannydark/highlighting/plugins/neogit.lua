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


  ----------------------- Not used by now:
  highlight(0, 'NeogitFold',            { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitStash',           { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitDiffAdd',         { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitRebasing',        { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitDiffDelete',      { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitRebaseDone',      { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitRemote',          { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitStashes',         { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitUnmergedInto',    { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitUnpulledFrom',    { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitRecentcommits',   { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitStagedchanges',   { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitUntrackedfiles',  { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitUnmergedchanges', { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitUnpulledchanges', { fg = colors.green, bg = colors.greenLignt })
  highlight(0, 'NeogitUnstagedchanges', { fg = colors.green, bg = colors.greenLignt })



end

return neoGit

