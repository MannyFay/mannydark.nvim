--------------------------------------------------------------------------------------------------------------
-- CSS
--------------------------------------------------------------------------------------------------------------

-- Uncomment if you need to use custom colors from the palette:
-- local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local css       = {}


-------------------------------------------------------------------------------
-- Settings

css.setupHighlighting = function()
  highlight(0, "@comment.documentation.css", { link = "MannydarkFgGreen" })  -- Doc blocks (/** ... */)
  -- highlight(0, "@operator.css",  { link = "Keyword" })
  highlight(0, "@attribute.css", { link = "Keyword" })
  -- highlight(0, "@punctuation.delimiter.css", { link = "Keyword" })




end

return css
