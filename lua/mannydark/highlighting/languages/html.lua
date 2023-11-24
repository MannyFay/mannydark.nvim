------------------------------------------------------------------------------
-- HTML
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local html      = {}


--------------------------------------------------------------
-- Settings

html.setupHighlighting = function()
  highlight(0, 'htmlArg',            { fg = colors.turquoise, bg = 'NONE'                    })
  highlight(0, 'htmlEndTag',         { fg = colors.blue,      bg = 'NONE'                    })
  highlight(0, 'htmlH1',             { fg = colors.white,     bg = 'NONE'                    })
  highlight(0, 'htmlH2',             { fg = colors.white,     bg = 'NONE'                    })
  highlight(0, 'htmlH3',             { fg = colors.white,     bg = 'NONE'                    })
  highlight(0, 'htmlH4',             { fg = colors.white,     bg = 'NONE'                    })
  highlight(0, 'htmlH5',             { fg = colors.white,     bg = 'NONE'                    })
  highlight(0, 'htmlH6',             { fg = colors.white,     bg = 'NONE'                    })
  highlight(0, 'htmlLink',           { fg = colors.white,     bg = 'NONE', underline = true, })
  highlight(0, 'htmlSpecialChar',    { fg = colors.blue,      bg = 'NONE'                    })
  highlight(0, 'htmlSpecialTagName', { fg = colors.blue,      bg = 'NONE'                    })
  highlight(0, 'htmlTag',            { fg = colors.blue,      bg = 'NONE'                    })
  highlight(0, 'htmlTagN',           { fg = colors.blue,      bg = 'NONE'                    })
  highlight(0, 'htmlTagName',        { fg = colors.blue,      bg = 'NONE'                    })
  highlight(0, 'htmlTitle',          { fg = colors.white,     bg = 'NONE'                    })
  highlight(0, 'htmlString',         { fg = colors.redLight,  bg = 'NONE'                    })
  highlight(0, 'htmlComment',        { fg = colors.red,       bg = 'NONE'                    })
  highlight(0, 'htmlLeadingSpace',   { fg = colors.white,     bg = 'NONE'                    })
  highlight(0, 'htmlHead',           { fg = colors.white,     bg = 'NONE'                    })
  highlight(0, 'htmlScriptTag',      { fg = colors.blue,      bg = 'NONE'                    })
end

return html
