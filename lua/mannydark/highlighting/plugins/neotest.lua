-------------------------------------------------------------------------------
-- Neotest - Test runner framework
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local neotest   = {}


-------------------------------------------------------------------------------
-- Settings

neotest.setupHighlighting = function()
  -- Test status highlights
  highlight(0, "NeotestPassed",  { fg = colors.green, bg = "NONE" }    )  -- Passed tests
  highlight(0, "NeotestFailed",  { fg = colors.red, bg = "NONE" }      )  -- Failed tests
  highlight(0, "NeotestRunning", { fg = colors.orange, bg = "NONE" }   )  -- Running tests
  highlight(0, "NeotestSkipped", { fg = colors.turquoise, bg = "NONE" })  -- Skipped tests
  highlight(0, "NeotestUnknown", { fg = colors.gray, bg = "NONE" }     )  -- Unknown status

  -- Tree structure highlights
  highlight(0, "NeotestTest",         { fg = colors.white, bg = "NONE" }    )  -- Test name
  highlight(0, "NeotestNamespace",    { fg = colors.purple, bg = "NONE" }   )  -- Namespace/describe block
  highlight(0, "NeotestFile",         { fg = colors.turquoise, bg = "NONE" })  -- File name
  highlight(0, "NeotestDir",          { fg = colors.blue, bg = "NONE" }     )  -- Directory name
  highlight(0, "NeotestIndent",       { fg = colors.gray, bg = "NONE" }     )  -- Indent guides
  highlight(0, "NeotestExpandMarker", { fg = colors.gray, bg = "NONE" }     )  -- Expand/collapse marker

  -- UI highlights
  highlight(0, "NeotestFocused",     { fg = "NONE", bg = "NONE", bold = true, underline = true })  -- Focused item
  highlight(0, "NeotestAdapterName", { fg = colors.pink, bg = "NONE" }                          )  -- Adapter name
  highlight(0, "NeotestWinSelect",   { fg = colors.turquoise, bg = "NONE", bold = true }        )  -- Window selection
  highlight(0, "NeotestMarked",      { fg = colors.orange, bg = "NONE", bold = true }           )  -- Marked items
  highlight(0, "NeotestTarget",      { fg = colors.red, bg = "NONE" }                           )  -- Target position
  highlight(0, "NeotestWatching",    { fg = colors.orange, bg = "NONE" }                        )  -- Watched tests

  -- Summary window highlights
  highlight(0, "NeotestSummary", { fg = colors.white, bg = "NONE" })  -- Summary text

  -- Output panel highlights
  highlight(0, "NeotestOutput",            { fg = colors.white, bg = "NONE" }                 )  -- Output text
  highlight(0, "NeotestOutputPanelHeader", { fg = colors.turquoise, bg = "NONE", bold = true })  -- Output panel header

  -- Border highlights
  highlight(0, "NeotestBorder", { fg = colors.turquoise, bg = "NONE" })  -- Window border
end

return neotest
