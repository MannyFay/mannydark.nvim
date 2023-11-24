------------------------------------------------------------------------------
-- Lualine Neovim Plugin Highlighting
------------------------------------------------------------------------------

local colors        = require('mannydark.palette')
local highlight     = vim.api.nvim_set_hl
local lualinePlugin = {}


--------------------------------------------------------------
-- Settings

lualinePlugin.setupHighlighting = function()
  -- Normal Mode:
  highlight(0, 'lualine_a_normal', { fg = colors.blue, bg = colors.grayDark })
  highlight(0, 'lualine_b_normal', { fg = colors.blue, bg = colors.grayDark })
  highlight(0, 'lualine_c_normal', { fg = colors.blue, bg = colors.grayDark })
  highlight(0, 'lualine_x_normal', { fg = colors.blue, bg = colors.grayDark })
  highlight(0, 'lualine_y_normal', { fg = colors.blue, bg = colors.grayDark })
  highlight(0, 'lualine_z_normal', { fg = colors.blue, bg = colors.grayDark })

  ----------------------- Not used by now:
  -- Insert Mode:
  highlight(0, 'lualine_a_insert', { fg = colors.blue, bg = colors.gray    })
  highlight(0, 'lualine_b_insert', { fg = colors.red,  bg = colors.green   })

  -- Visual Mode:
  highlight(0, 'lualine_a_visual', { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_b_visual', { fg = colors.red, bg = colors.green })

  -- Command Mode:
  highlight(0, 'lualine_a_command', { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_b_command', { fg = colors.red, bg = colors.green })

  -- Replace Mode:
  highlight(0, 'lualine_a_replace', { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_b_replace', { fg = colors.red, bg = colors.green })

  -- Inactive Mode:
  highlight(0, 'lualine_a_inactive', { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_b_inactive', { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_c_inactive', { fg = colors.red, bg = colors.green })

  highlight(0, 'lualine_x_diff_added_normal',      { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_added_insert',      { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_added_visual',      { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_added_replace',     { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_added_command',     { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_added_terminal',    { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_added_inactive',    { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_modified_normal',   { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_modified_insert',   { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_modified_visual',   { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_modified_replace',  { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_modified_command',  { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_modified_terminal', { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_modified_inactive', { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_removed_normal',    { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_removed_insert',    { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_removed_visual',    { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_removed_replace',   { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_removed_command',   { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_removed_terminal',  { fg = colors.red, bg = colors.green })
  highlight(0, 'lualine_x_diff_removed_inactive',  { fg = colors.red, bg = colors.green })
end

return lualinePlugin
