-------------------------------------------------------------------------------
-- Obsidian Neovim Plugin
-- https://github.com/obsidian-nvim/obsidian.nvim
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local obsidian  = {}


-------------------------------------------------------------------------------
-- Settings

obsidian.setupHighlighting = function()
  -- Checkboxes
  highlight(0, "ObsidianTodo",       { bold = true, fg = colors.orange }    )  -- [ ] unchecked
  highlight(0, "ObsidianDone",       { bold = true, fg = colors.green }     )  -- [x] checked
  highlight(0, "ObsidianRightArrow", { bold = true, fg = colors.blue }      )  -- [>] forwarded
  highlight(0, "ObsidianTilde",      { bold = true, fg = colors.pink }      )  -- [~] cancelled
  highlight(0, "ObsidianImportant",  { bold = true, fg = colors.red }       )  -- [!] important

  -- Text elements
  highlight(0, "ObsidianBullet",        { bold = true, fg = colors.blue }      )  -- Bullet points
  highlight(0, "ObsidianRefText",       { underline = true, fg = colors.purple })  -- [[references]]
  highlight(0, "ObsidianExtLinkIcon",   { fg = colors.blueLink }               )  -- External link icon
  highlight(0, "ObsidianTag",           { italic = true, fg = colors.turquoise })  -- #tags
  highlight(0, "ObsidianBlockID",       { italic = true, fg = colors.gray }    )  -- ^block-ids
  highlight(0, "ObsidianHighlightText", { bg = colors.orangeDark }             )  -- ==highlighted==
end

return obsidian
