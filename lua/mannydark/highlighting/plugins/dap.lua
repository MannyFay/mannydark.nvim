-------------------------------------------------------------------------------
-- DAP (Debug Adapter Protocol) - nvim-dap & nvim-dap-ui
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local dap       = {}


-------------------------------------------------------------------------------
-- Settings

dap.setupHighlighting = function()
  -- nvim-dap signs (used with sign_define texthl, linehl, numhl)
  highlight(0, 'DapBreakpoint',          { fg = colors.red,        bg = 'NONE'           })  -- Breakpoint sign
  highlight(0, 'DapBreakpointCondition', { fg = colors.orange,     bg = 'NONE'           })  -- Conditional breakpoint sign
  highlight(0, 'DapBreakpointRejected',  { fg = colors.gray,       bg = 'NONE'           })  -- Rejected breakpoint sign
  highlight(0, 'DapLogPoint',            { fg = colors.blue,       bg = 'NONE'           })  -- Log point sign
  highlight(0, 'DapStopped',             { fg = colors.green,      bg = colors.grayDark  })  -- Current stopped line
  highlight(0, 'DapStoppedLine',         { fg = 'NONE',            bg = colors.grayDark  })  -- Background for stopped line

  -- nvim-dap-ui elements
  highlight(0, 'DapUIScope',             { fg = colors.turquoise,  bg = 'NONE'           })  -- Scope headers
  highlight(0, 'DapUIType',              { fg = colors.purple,     bg = 'NONE'           })  -- Type annotations
  highlight(0, 'DapUIValue',             { fg = colors.white,      bg = 'NONE'           })  -- Variable values
  highlight(0, 'DapUIModifiedValue',     { fg = colors.orange,     bg = 'NONE', bold = true })  -- Modified values
  highlight(0, 'DapUIDecoration',        { fg = colors.turquoise,  bg = 'NONE'           })  -- Tree decorations
  highlight(0, 'DapUIThread',            { fg = colors.green,      bg = 'NONE'           })  -- Thread names
  highlight(0, 'DapUIStoppedThread',     { fg = colors.turquoise,  bg = 'NONE'           })  -- Stopped thread
  highlight(0, 'DapUISource',            { fg = colors.purple,     bg = 'NONE'           })  -- Source file names
  highlight(0, 'DapUILineNumber',        { fg = colors.turquoise,  bg = 'NONE'           })  -- Line numbers in stack
  highlight(0, 'DapUIFloatBorder',       { fg = colors.turquoise,  bg = 'NONE'           })  -- Float window borders
  highlight(0, 'DapUIWatchesEmpty',      { fg = colors.gray,       bg = 'NONE'           })  -- Empty watches
  highlight(0, 'DapUIWatchesValue',      { fg = colors.green,      bg = 'NONE'           })  -- Watch values
  highlight(0, 'DapUIWatchesError',      { fg = colors.red,        bg = 'NONE'           })  -- Watch errors
  highlight(0, 'DapUIBreakpointsPath',   { fg = colors.turquoise,  bg = 'NONE'           })  -- Breakpoint file paths
  highlight(0, 'DapUIBreakpointsInfo',   { fg = colors.green,      bg = 'NONE'           })  -- Breakpoint info
  highlight(0, 'DapUIBreakpointsCurrentLine', { fg = colors.green, bg = 'NONE', bold = true })  -- Current breakpoint line
  highlight(0, 'DapUIBreakpointsLine',   { fg = colors.turquoise,  bg = 'NONE'           })  -- Breakpoint line numbers
  highlight(0, 'DapUIBreakpointsDisabledLine', { fg = colors.gray, bg = 'NONE'           })  -- Disabled breakpoints
  highlight(0, 'DapUICurrentFrameName',  { fg = colors.green,      bg = 'NONE', bold = true })  -- Current stack frame
  highlight(0, 'DapUIStepOver',          { fg = colors.blue,       bg = 'NONE'           })  -- Step over button
  highlight(0, 'DapUIStepInto',          { fg = colors.blue,       bg = 'NONE'           })  -- Step into button
  highlight(0, 'DapUIStepBack',          { fg = colors.blue,       bg = 'NONE'           })  -- Step back button
  highlight(0, 'DapUIStepOut',           { fg = colors.blue,       bg = 'NONE'           })  -- Step out button
  highlight(0, 'DapUIStop',              { fg = colors.red,        bg = 'NONE'           })  -- Stop button
  highlight(0, 'DapUIPlayPause',         { fg = colors.green,      bg = 'NONE'           })  -- Play/pause button
  highlight(0, 'DapUIRestart',           { fg = colors.green,      bg = 'NONE'           })  -- Restart button
  highlight(0, 'DapUIUnavailable',       { fg = colors.gray,       bg = 'NONE'           })  -- Unavailable elements
  highlight(0, 'DapUIWinSelect',         { fg = colors.turquoise,  bg = 'NONE', bold = true })  -- Window selection
  highlight(0, 'DapUIEndofBuffer',       { fg = colors.grayDark,   bg = 'NONE'           })  -- End of buffer
  highlight(0, 'DapUINormalFloat',       { fg = colors.white,      bg = 'NONE'           })  -- Normal float text
  highlight(0, 'DapUIPlayPauseNC',       { fg = colors.green,      bg = 'NONE'           })  -- Play/pause (inactive)
  highlight(0, 'DapUIRestartNC',         { fg = colors.green,      bg = 'NONE'           })  -- Restart (inactive)
  highlight(0, 'DapUIStopNC',            { fg = colors.red,        bg = 'NONE'           })  -- Stop (inactive)
  highlight(0, 'DapUIUnavailableNC',     { fg = colors.gray,       bg = 'NONE'           })  -- Unavailable (inactive)
  highlight(0, 'DapUIStepOverNC',        { fg = colors.blue,       bg = 'NONE'           })  -- Step over (inactive)
  highlight(0, 'DapUIStepIntoNC',        { fg = colors.blue,       bg = 'NONE'           })  -- Step into (inactive)
  highlight(0, 'DapUIStepBackNC',        { fg = colors.blue,       bg = 'NONE'           })  -- Step back (inactive)
  highlight(0, 'DapUIStepOutNC',         { fg = colors.blue,       bg = 'NONE'           })  -- Step out (inactive)
end

return dap
