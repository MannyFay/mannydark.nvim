local M = {}
local theme = require('mannydark.theme')
local colors = require('mannydark.palette')

M.setup = function()
  vim.cmd('hi clear')

  vim.o.background = 'dark'
  if vim.fn.exists('syntax_on') then
    vim.cmd('syntax reset')
  end

  vim.o.termguicolors = true
  vim.g.colors_name = 'mannydark'

  theme.buildTheme()

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

return M

