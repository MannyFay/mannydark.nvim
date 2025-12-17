-------------------------------------------------------------------------------
-- Mannydark Colorscheme
-------------------------------------------------------------------------------

local M = {}
local colors = require('mannydark.palette')

-- Default configuration
M.config = {
  transparent = false,     -- Transparent background.
  italic_comments = true,  -- Italic comments.
  dim_inactive = false,    -- Dim inactive windows.
}

--- Setup the colorscheme with optional configuration.
--- @param opts table|nil Configuration options
M.setup = function(opts)
  -- Merge user options with defaults
  M.config = vim.tbl_deep_extend('force', M.config, opts or {})

  vim.cmd('hi clear')

  vim.o.background = 'dark'
  if vim.fn.exists('syntax_on') then
    vim.cmd('syntax reset')
  end

  vim.o.termguicolors = true
  vim.g.colors_name = 'mannydark'

  -- Build theme (passes config internally via require)
  local theme = require('mannydark.theme')
  theme.buildTheme()

  -- Apply config-dependent highlights
  M.applyConfig()

  -- Terminal colors (for :terminal)
  vim.g.terminal_color_0  = colors.black      -- Black
  vim.g.terminal_color_1  = colors.red        -- Red
  vim.g.terminal_color_2  = colors.green      -- Green
  vim.g.terminal_color_3  = colors.orange     -- Yellow
  vim.g.terminal_color_4  = colors.blue       -- Blue
  vim.g.terminal_color_5  = colors.purple     -- Magenta
  vim.g.terminal_color_6  = colors.turquoise  -- Cyan
  vim.g.terminal_color_7  = colors.white      -- White
  -- Bright variants (same as normal)
  vim.g.terminal_color_8  = colors.black
  vim.g.terminal_color_9  = colors.red
  vim.g.terminal_color_10 = colors.green
  vim.g.terminal_color_11 = colors.orange
  vim.g.terminal_color_12 = colors.blue
  vim.g.terminal_color_13 = colors.purple
  vim.g.terminal_color_14 = colors.turquoise
  vim.g.terminal_color_15 = colors.white
end

--- Apply configuration-dependent highlights.
--- Called after buildTheme() to override based on user config.
M.applyConfig = function()
  local highlight = vim.api.nvim_set_hl

  -- Transparent background
  if M.config.transparent then
    highlight(0, 'Normal', { fg = colors.white, bg = 'NONE' })
    highlight(0, 'NormalNC', { fg = colors.white, bg = 'NONE' })
    highlight(0, 'NormalFloat', { fg = colors.white, bg = 'NONE' })
    highlight(0, 'SignColumn', { fg = 'NONE', bg = 'NONE' })
    highlight(0, 'EndOfBuffer', { fg = 'NONE', bg = 'NONE' })
    highlight(0, 'CursorLine', { fg = 'NONE', bg = colors.grayDark })
    highlight(0, 'CursorLineNr', { fg = colors.white, bg = 'NONE' })
  end

  -- Dim inactive windows
  if M.config.dim_inactive then
    highlight(0, 'NormalNC', { fg = colors.gray, bg = colors.black })
  end

  -- Italic comments
  if M.config.italic_comments then
    highlight(0, 'Comment', { fg = colors.red, bg = 'NONE', italic = true })
    highlight(0, '@comment', { fg = colors.red, bg = 'NONE', italic = true })
    highlight(0, '@lsp.type.comment', { fg = colors.red, bg = 'NONE', italic = true })
  else
    highlight(0, 'Comment', { fg = colors.red, bg = 'NONE', italic = false })
    highlight(0, '@comment', { fg = colors.red, bg = 'NONE', italic = false })
    highlight(0, '@lsp.type.comment', { fg = colors.red, bg = 'NONE', italic = false })
  end
end

return M
