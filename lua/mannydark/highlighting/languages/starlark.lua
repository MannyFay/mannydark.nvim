-------------------------------------------------------------------------------
-- Starlark Files (Bazel)
-- Highlighting for .star, .bzl, BUILD, BUILD.bazel, WORKSPACE files.
-- Starlark is a Python-like configuration language used in Bazel.
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local starlark  = {}


-------------------------------------------------------------------------------
-- Settings

starlark.setupHighlighting = function()

end

return starlark