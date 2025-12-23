-------------------------------------------------------------------------------
-- Lualine Neovim Plugin Highlighting
-------------------------------------------------------------------------------

local colors        = require("mannydark.palette")
local highlight     = vim.api.nvim_set_hl
local lualinePlugin = {}


--------------------------------------------------------------
-- Settings

lualinePlugin.setupHighlighting = function()
  -- Normal Mode:
  highlight(0, "lualine_a_normal", { fg = colors.blue, bg = colors.grayDark })
  highlight(0, "lualine_b_normal", { fg = colors.blue, bg = colors.grayDark })
  highlight(0, "lualine_c_normal", { fg = colors.blue, bg = colors.grayDark })
  highlight(0, "lualine_x_normal", { fg = colors.blue, bg = colors.grayDark })
  highlight(0, "lualine_y_normal", { fg = colors.blue, bg = colors.grayDark })
  highlight(0, "lualine_z_normal", { fg = colors.blue, bg = colors.grayDark })

  -- Insert Mode:
  highlight(0, "lualine_a_insert", { fg = colors.purple, bg = colors.grayDark })
  highlight(0, "lualine_b_insert", { fg = colors.purple, bg = colors.grayDark })
  highlight(0, "lualine_c_insert", { fg = colors.purple, bg = colors.grayDark })
  highlight(0, "lualine_x_insert", { fg = colors.purple, bg = colors.grayDark })
  highlight(0, "lualine_y_insert", { fg = colors.purple, bg = colors.grayDark })
  highlight(0, "lualine_z_insert", { fg = colors.purple, bg = colors.grayDark })

  -- Visual Mode:
  highlight(0, "lualine_a_visual", { fg = colors.orange, bg = colors.grayDark })
  highlight(0, "lualine_b_visual", { fg = colors.orange, bg = colors.grayDark })
  highlight(0, "lualine_c_visual", { fg = colors.orange, bg = colors.grayDark })
  highlight(0, "lualine_x_visual", { fg = colors.orange, bg = colors.grayDark })
  highlight(0, "lualine_y_visual", { fg = colors.orange, bg = colors.grayDark })
  highlight(0, "lualine_z_visual", { fg = colors.orange, bg = colors.grayDark })

  -- Command Mode:
  highlight(0, "lualine_a_command", { fg = colors.turquoise, bg = colors.grayDark })
  highlight(0, "lualine_b_command", { fg = colors.turquoise, bg = colors.grayDark })
  highlight(0, "lualine_c_command", { fg = colors.turquoise, bg = colors.grayDark })
  highlight(0, "lualine_x_command", { fg = colors.turquoise, bg = colors.grayDark })
  highlight(0, "lualine_y_command", { fg = colors.turquoise, bg = colors.grayDark })
  highlight(0, "lualine_z_command", { fg = colors.turquoise, bg = colors.grayDark })

  -- Inactive Mode:
  highlight(0, "lualine_a_inactive", { fg = colors.gray, bg = colors.grayDark })
  highlight(0, "lualine_b_inactive", { fg = colors.gray, bg = colors.grayDark })
  highlight(0, "lualine_c_inactive", { fg = colors.gray, bg = colors.grayDark })
  highlight(0, "lualine_x_inactive", { fg = colors.gray, bg = colors.grayDark })
  highlight(0, "lualine_y_inactive", { fg = colors.gray, bg = colors.grayDark })
  highlight(0, "lualine_z_inactive", { fg = colors.gray, bg = colors.grayDark })

  -- Replace Mode:
  highlight(0, "lualine_a_replace", { fg = colors.red, bg = colors.grayDark })
  highlight(0, "lualine_b_replace", { fg = colors.red, bg = colors.grayDark })

  -- Git Diff (added = grün, modified/removed = rot):
  highlight(0, "lualine_x_diff_added_normal",   { link = "DiagnosticOk" })
  highlight(0, "lualine_x_diff_added_insert",   { link = "DiagnosticOk" })
  highlight(0, "lualine_x_diff_added_visual",   { link = "DiagnosticOk" })
  highlight(0, "lualine_x_diff_added_replace",  { link = "DiagnosticOk" })
  highlight(0, "lualine_x_diff_added_command",  { link = "DiagnosticOk" })
  highlight(0, "lualine_x_diff_added_terminal", { link = "DiagnosticOk" })
  highlight(0, "lualine_x_diff_added_inactive", { link = "DiagnosticOk" })

  highlight(0, "lualine_x_diff_modified_normal",   { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_modified_insert",   { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_modified_visual",   { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_modified_replace",  { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_modified_command",  { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_modified_terminal", { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_modified_inactive", { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_removed_normal",    { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_removed_insert",    { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_removed_visual",    { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_removed_replace",   { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_removed_command",   { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_removed_terminal",  { link = "DiagnosticError" })
  highlight(0, "lualine_x_diff_removed_inactive",  { link = "DiagnosticError" })
end

return lualinePlugin

