local colors = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local ini = {}

ini.setupHighlighting = function()
------------------------------------------------------------------------------
-- Configuration Files
------------------------------------------------------------------------------
  highlight(0, 'dosiniLabel', { fg = colors.purple, bg = 'NONE'                                  })  -- Settings variable of config files (.editorconfig).
  highlight(0, 'dosiniValue', { fg = colors.blue, bg = 'NONE'                                  })
  highlight(0, 'dosiniComment', { fg = colors.red, bg = 'NONE'                                  })
  highlight(0, 'dosiniHeader', { fg = colors.white, bg = 'NONE'                                  })
  highlight(0, 'dosiniNumber', { fg = colors.greenLight, bg = 'NONE'                                  })

end

return ini
