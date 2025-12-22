-------------------------------------------------------------------------------
-- TypeScript Files
-- Highlighting for .ts, .mts, .cts, .d.ts files.
-------------------------------------------------------------------------------

-- local colors    = require("mannydark.palette")
local highlight  = vim.api.nvim_set_hl
local typescript = {}


-------------------------------------------------------------------------------
-- Settings

typescript.setupHighlighting = function()
  highlight(0, "@comment.documentation.typescript", { link = "MannydarkFgGreen" })  -- Doc Blocks.
end

return typescript
