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
  highlight(0, 'lualine_a_normal',   { fg = colors.blue,      bg = colors.grayDark })
  highlight(0, 'lualine_b_normal',   { fg = colors.blue,      bg = colors.grayDark })
  highlight(0, 'lualine_c_normal',   { fg = colors.blue,      bg = colors.grayDark })
  highlight(0, 'lualine_x_normal',   { fg = colors.blue,      bg = colors.grayDark })
  highlight(0, 'lualine_y_normal',   { fg = colors.blue,      bg = colors.grayDark })
  highlight(0, 'lualine_z_normal',   { fg = colors.blue,      bg = colors.grayDark })

  -- Insert Mode:
  highlight(0, 'lualine_a_insert',   { fg = colors.purple,    bg = colors.grayDark })
  highlight(0, 'lualine_b_insert',   { fg = colors.purple,    bg = colors.grayDark })
  highlight(0, 'lualine_c_insert',   { fg = colors.purple,    bg = colors.grayDark })
  highlight(0, 'lualine_x_insert',   { fg = colors.purple,    bg = colors.grayDark })
  highlight(0, 'lualine_y_insert',   { fg = colors.purple,    bg = colors.grayDark })
  highlight(0, 'lualine_z_insert',   { fg = colors.purple,    bg = colors.grayDark })

  -- Visual Mode:
  highlight(0, 'lualine_a_visual',   { fg = colors.orange,    bg = colors.grayDark })
  highlight(0, 'lualine_b_visual',   { fg = colors.orange,    bg = colors.grayDark })
  highlight(0, 'lualine_c_visual',   { fg = colors.orange,    bg = colors.grayDark })
  highlight(0, 'lualine_x_visual',   { fg = colors.orange,    bg = colors.grayDark })
  highlight(0, 'lualine_y_visual',   { fg = colors.orange,    bg = colors.grayDark })
  highlight(0, 'lualine_z_visual',   { fg = colors.orange,    bg = colors.grayDark })

  -- Command Mode:
  highlight(0, 'lualine_a_command',  { fg = colors.turquoise, bg = colors.grayDark })
  highlight(0, 'lualine_b_command',  { fg = colors.turquoise, bg = colors.grayDark })
  highlight(0, 'lualine_c_command',  { fg = colors.turquoise, bg = colors.grayDark })
  highlight(0, 'lualine_x_command',  { fg = colors.turquoise, bg = colors.grayDark })
  highlight(0, 'lualine_y_command',  { fg = colors.turquoise, bg = colors.grayDark })
  highlight(0, 'lualine_z_command',  { fg = colors.turquoise, bg = colors.grayDark })

  -- Inactive Mode:
  highlight(0, 'lualine_a_inactive', { fg = colors.gray,      bg = colors.grayDark })
  highlight(0, 'lualine_b_inactive', { fg = colors.gray,      bg = colors.grayDark })
  highlight(0, 'lualine_c_inactive', { fg = colors.gray,      bg = colors.grayDark })
  highlight(0, 'lualine_x_inactive', { fg = colors.gray,      bg = colors.grayDark })
  highlight(0, 'lualine_y_inactive', { fg = colors.gray,      bg = colors.grayDark })
  highlight(0, 'lualine_z_inactive', { fg = colors.gray,      bg = colors.grayDark })

  ----------------------- Not used by now:
  -- Replace Mode:
  highlight(0, 'lualine_a_replace', { fg = colors.red, bg = colors.greenLight })
  highlight(0, 'lualine_b_replace', { fg = colors.red, bg = colors.green })

  highlight(0, 'lualine_x_diff_added_normal',      { fg = colors.green, bg = colors.redLight })
  highlight(0, 'lualine_x_diff_added_insert',      { fg = colors.green, bg = colors.orange })
  highlight(0, 'lualine_x_diff_added_visual',      { fg = colors.green, bg = colors.pink })
  highlight(0, 'lualine_x_diff_added_replace',     { fg = colors.green, bg = colors.blue })
  highlight(0, 'lualine_x_diff_added_command',     { fg = colors.green, bg = colors.greenLight })
  highlight(0, 'lualine_x_diff_added_terminal',    { fg = colors.green, bg = colors.turquoise })
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

