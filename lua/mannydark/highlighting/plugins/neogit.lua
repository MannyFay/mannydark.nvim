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
  highlight(0, 'NeogitUnstagedchanges', { fg = colors.red,    bg = 'NONE', underline = true })  -- Un-staged changes fold heading.
  highlight(0, 'NeogitChangeMunstaged', { fg = colors.red,    bg = 'NONE' })  -- 'Modified' flag.
  highlight(0, 'NeogitChangeDunstaged', { fg = colors.red,    bg = 'NONE' })  -- 'Deleted' flag.
  highlight(0, 'NeogitUntrackedfiles',  { fg = colors.red,    bg = 'NONE', underline = true })  -- Untracked files fold heading.
  highlight(0, 'NeogitFold',            { fg = colors.blue,   bg = 'NONE' })  -- Heading of file folds.
  highlight(0, 'NeogitStashes',         { fg = colors.red,    bg = 'NONE', underline = true })  -- Stashes fold heading.
  highlight(0, 'NeogitRemote',          { fg = colors.purple, bg = 'NONE' })  -- Remote name.
  highlight(0, 'NeogitPopupSectionTitle', { fg = colors.blue, bg = 'NONE', underline = true })  -- Title of Neogit window.


  -- highlight(0, '',          { fg = colors.purple, bg = 'NONE' })  -- .

  highlight(0, 'NeogitFilePath',          { fg = colors.orange, bg = colors.purple })  -- Remote name.
  highlight(0, 'NeogitDiffAdditions',          { fg = colors.orange, bg = colors.blue })  -- Remote name.
  highlight(0, 'NeogitDiffDeletions',          { fg = colors.orange, bg = colors.turquoise })  -- Remote name.
  highlight(0, 'NeogitFloatHeaderHighlight',          { fg = colors.orange, bg = colors.green })  -- Remote name.

  highlight(0, 'NeogitCommitViewDescription',          { fg = colors.green, bg = colors.yellow })  -- Remote name.







-- diff:
-- NeogitFilePath
-- NeogitDiffAdditions
-- NeogitDiffDeletions
-- NeogitFloatHeaderHighlight

-- commit-view:
-- NeogitCommitViewHeader
-- NeogitCommitViewDescription


  ----------------------- Not used by now:
  -- highlight(0, 'NeogitStash',           { fg = colors.green, bg = colors.greenLignt })
  -- highlight(0, 'NeogitDiffAdd',         { fg = colors.green, bg = colors.blue })
  -- highlight(0, 'NeogitRebasing',        { fg = colors.green, bg = colors.red })
  -- highlight(0, 'NeogitDiffDelete',      { fg = colors.green, bg = colors.redLight })
  -- highlight(0, 'NeogitRebaseDone',      { fg = colors.green, bg = colors.orange })
  -- highlight(0, 'NeogitUnmergedInto',    { fg = colors.green, bg = colors.gray })
  -- highlight(0, 'NeogitUnpulledFrom',    { fg = colors.green, bg = colors.white })
  -- highlight(0, 'NeogitUnmergedchanges', { fg = colors.pink, bg = colors.greenLignt })
  -- highlight(0, 'NeogitUnpulledchanges', { fg = colors.pink, bg = colors.blue })



end

return neoGit

