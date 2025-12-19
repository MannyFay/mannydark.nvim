-- Mannydark colorscheme (dark variant)
-- This file allows using :colorscheme mannydark

local mannydark = require('mannydark')

-- Preserve existing user config, only ensure style is 'dark'
mannydark.setup(vim.tbl_deep_extend('force', mannydark.config, { style = 'dark' }))

-- Auto-enable dev mode in multiple scenarios:
-- 1. When CWD is the colorscheme directory
-- 2. When the current buffer is a mannydark file
-- 3. When vim.g.mannydark_debug is set
local function should_enable_dev_mode()
  -- Check CWD
  local cwd = vim.fn.getcwd()
  if cwd:match('mannydark') then
    return true
  end

  -- Check current buffer path
  local bufpath = vim.fn.expand('%:p')
  if bufpath:match('mannydark') then
    return true
  end

  -- Check debug flag
  if vim.g.mannydark_debug then
    return true
  end

  return false
end

if should_enable_dev_mode() and not mannydark.is_dev_mode() then
  mannydark.dev_mode()
end
