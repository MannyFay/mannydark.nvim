-------------------------------------------------------------------------------
-- Rust Files
-- Highlighting for .rs files.
-------------------------------------------------------------------------------

local colors  = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local rust    = {}


-------------------------------------------------------------------------------
-- Settings

rust.setupHighlighting = function()
  -- Doc comments (///, //!, /** */, /*! */) should be green like in JS/TS
  -- Treesitter captures
  highlight(0, "@comment.documentation.rust", { link = "MannydarkFgGreen" })

  -- LSP semantic tokens (higher priority than Treesitter, so must override)
  highlight(0, "@lsp.typemod.comment.documentation.rust", { link = "MannydarkFgGreen" })

  -- Glob import (*) should be operator color
  highlight(0, "@character.special.rust", { link = "Operator" })
end

return rust