-------------------------------------------------------------------------------
-- Lazy Plugin Manager
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local lazy      = {}


-------------------------------------------------------------------------------
-- Settings

lazy.setupHighlighting = function()
  highlight(0, "LazyH1",            { fg = colors.purple, bg = "NONE", underline = true })  -- Lazy title in window H1.
  highlight(0, "LazyH2",            { fg = colors.purple, bg = "NONE", underline = true })  -- Lazy subtitle H2.
  highlight(0, "LazyButtonActive",  { fg = colors.black, bg = colors.blue }              )  -- Active element.
  highlight(0, "LazyButton",        { fg = colors.black, bg = colors.gray }              )  -- Inactive element.
  highlight(0, "LazyComment",       { fg = colors.green, bg = "NONE" }                   )  -- Comments of Lazy.
  highlight(0, "LazySpecial",       { fg = colors.green, bg = "NONE" }                   )  -- Signs in first column.
  highlight(0, "LazyReasonEvent",   { fg = colors.green, bg = "NONE" }                   )  -- Why something was changed.
  highlight(0, "LazyReasonPlugin",  { fg = colors.green, bg = "NONE" }                   )  -- Because of what something has changed.
  highlight(0, "LazyReasonStart",   { fg = colors.green, bg = "NONE" }                   )  -- Start has changed.
  highlight(0, "LazyReasonRequire", { fg = colors.green, bg = "NONE" }                   )  -- Requirement has changed.
  highlight(0, "LazyReasonCmd",     { fg = colors.green, bg = "NONE" }                   )  -- Command has changed.
  highlight(0, "LazyReasonFt",      { fg = colors.green, bg = "NONE" }                   )  -- File type has changed.
  highlight(0, "LazyReasonKeys",    { fg = colors.green, bg = "NONE" }                   )  -- Key mappings has changed.
  highlight(0, "LazyReasonSource",  { fg = colors.green, bg = "NONE" }                   )  -- Source has changed.
  highlight(0, "LazyDimmed",        { fg = colors.white, bg = "NONE" }                   )  -- Commit message of updated plugin.
  highlight(0, "LazyCommitIssue",   { fg = colors.purple, bg = "NONE" }                  )  -- Issue number that was solved on updated plugin.
  highlight(0, "LazyProp",          { fg = colors.purple, bg = "NONE" }                  )  -- Properties in Lazy overview.
  highlight(0, "LazyCommitScope",   { fg = colors.white, bg = "NONE" }                   )  -- Scope of the commit message.
end

return lazy
