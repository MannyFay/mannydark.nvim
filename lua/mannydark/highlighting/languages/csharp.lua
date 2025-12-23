-------------------------------------------------------------------------------
-- C# Files
-- Highlighting for .cs, .csx files.
-------------------------------------------------------------------------------

-- local colors  = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local csharp  = {}


-------------------------------------------------------------------------------
-- Settings

csharp.setupHighlighting = function()
  highlight(0, "@comment.documentation.c_sharp", { link = "MannydarkFgGreen" })  -- Doc blocks (/** ... */ or ///)
  highlight(0, "@type.builtin.c_sharp",          { link = "Keyword" })           -- Built-in types (int, bool, string, etc.)
  highlight(0, "@character.special.c_sharp",     { link = "@text" })             -- _
end

return csharp
