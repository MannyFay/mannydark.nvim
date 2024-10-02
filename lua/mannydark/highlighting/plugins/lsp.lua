-------------------------------------------------------------------------------
-- LSP (Language Server Protocol) Neovim Plugin
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local lsp       = {}


--------------------------------------------------------------
-- Settings

lsp.setupHighlighting = function()
  highlight(0, 'DiagnosticSignError',                 { fg = colors.red,    bg = 'NONE'                                        })  -- Error icon (x) in sign column.
  highlight(0, 'DiagnosticUnderlineError',            { fg = 'NONE',    bg = 'NONE', sp = colors.red,    underline = true })  -- Error highlighting in project tree on words.
  highlight(0, 'DiagnosticVirtualTextError',          { fg = colors.red,    bg = 'NONE', sp = colors.red,    underline = true })  -- Virtual text at the end of the line.

  highlight(0, 'DiagnosticError',                     { fg = colors.purple,    bg = colors.turquoise                              })  -- ??? Code error indicator bar and text at end of line.

  -- warnings
  highlight(0, 'DiagnosticSignWarn',                  { fg = colors.orange, bg = 'NONE'                                       })  -- Warning icon in sign column.
  highlight(0, 'DiagnosticUnderlineWarn',             { fg = colors.orange, bg = colors.orange, sp = colors.orange, undercurl = true })  -- Warning highlighting in project tree on words.
  highlight(0, 'DiagnosticVirtualTextWarn',           { fg = colors.orange, bg = 'NONE', sp = colors.orange, underline = true })  -- Virtual text at the end of the line.

  highlight(0, 'DiagnosticWarn',                      { fg = colors.turquoise, bg = colors.orange                                       })

  -- hints are more important than information!
  highlight(0, 'DiagnosticSignHint',                  { fg = colors.green, bg = 'NONE'                                      })  -- Hint icon (bulb) in sign column.
  highlight(0, 'DiagnosticUnderlineHint',             { fg = colors.green,       bg = colors.green, sp = colors.green, undercurl = true })  -- Hints in project tree on words.
  highlight(0, 'DiagnosticVirtualTextHint',           { fg = colors.green, bg = 'NONE', sp = colors.green, underline = true })  -- Virtual text at the end of the line.
  highlight(0, 'DiagnosticUnnecessary',               { fg = 'NONE',       bg = 'NONE', sp = colors.green, undercurl = true })  -- POV of hint itself (unnecessary item in code).
--DiagnosticUnnecessary
  highlight(0, 'LspDiagnosticsHint',                  { fg = colors.turquoise,   bg = colors.blue                                        }) -- ???
  highlight(0, 'DiagnosticHint',                      { fg = colors.purple,   bg = colors.blueLink                                       })  -- ??? idk - this is not true -> Code hint indicator bar and text at end of line.

  ----- infos
  highlight(0, 'DiagnosticSignInfo',                  { fg = colors.gray,   bg = 'NONE'                                       })  -- Info icon in sign column.
  highlight(0, 'DiagnosticUnderlineInfo',             { fg = colors.gray,        bg = colors.gray, sp = colors.gray,   underline = true })  -- Info highlighting in project tree on words.
  highlight(0, 'DiagnosticVirtualTextInfo',           { fg = colors.gray,   bg = 'NONE', sp = colors.gray,   underline = true })  -- Horizontal info indicator bar and text at end of line.


  highlight(0, 'DiagnosticInfo',                      { fg = colors.blue,   bg = 'NONE'                                        })




  highlight(0, 'LspDiagnosticsError',                 { fg = colors.green,    bg = 'NONE'                                        })
  highlight(0, 'LspDiagnosticsWarning',               { fg = colors.white, bg = 'NONE'                                        })
  highlight(0, 'LspDiagnosticsInfo',                  { fg = colors.redLight,   bg = 'NONE'                                        })
  highlight(0, 'LspCodeLens',                         { fg = colors.purple, bg = 'NONE', italic = true,                        })
  highlight(0, 'LspCodeLensSeparator',                { fg = colors.purple, bg = 'NONE', italic = true,                        })
  highlight(0, 'IlluminatedWordRead',                  { fg = "NONE", bg = colors.grayDark                                   })  -- References of word under cursor (used by illuminate).
  highlight(0, 'IlluminatedWordText',                  { fg = "NONE", bg = colors.grayDark                                   })  -- String highlighting used by illuminate.
  highlight(0, 'IlluminatedWordWrite',                 { fg = "NONE", bg = colors.grayDark                                   })  -- Word under cursor (used by illuminate).



  ----------------------- Not used by now:
  highlight(0, 'LspReferenceRead',                     { fg = "NONE", bg = colors.gray                                    })  -- Matching words to the one under cursor, like HTML tag names.

  highlight(0, 'DiagnosticFloatingHint',               { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'DiagnosticOther',                      { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'DiagnosticSignOther',                  { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'DiagnosticSignWarning',                { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'DiagnosticFloatingInfo',               { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'DiagnosticFloatingWarn',               { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'DiagnosticFloatingError',              { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'DiagnosticSignInformation',            { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsInformation',            { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsDefaultError',           { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsDefaultWarning',         { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsDefaultInformation',     { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsDefaultInfo',            { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsDefaultHint',            { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsVirtualTextError',       { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsVirtualTextWarning',     { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsVirtualTextInformation', { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsVirtualTextInfo',        { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsVirtualTextHint',        { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsFloatingError',          { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsFloatingWarning',        { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsFloatingInformation',    { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsFloatingInfo',           { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsFloatingHint',           { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsSignError',              { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsSignWarning',            { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsSignInformation',        { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsSignInfo',               { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsSignHint',               { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'NvimTreeLspDiagnosticsError',          { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'NvimTreeLspDiagnosticsWarning',        { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'NvimTreeLspDiagnosticsInformation',    { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'NvimTreeLspDiagnosticsInfo',           { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'NvimTreeLspDiagnosticsHint',           { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsUnderlineError',         { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsUnderlineWarning',       { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsUnderlineInformation',   { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsUnderlineInfo',          { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspDiagnosticsUnderlineHint',          { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspReferenceText',                     { fg = colors.pink, bg = colors.green                                    }) -- This is text inside of strings including the quotes.
  highlight(0, 'LspReferenceWrite',                    { fg = colors.pink, bg = colors.pink   })

  


  highlight(0, 'LspInfoFiletype',                 { fg = colors.pink, bg = colors.blue                                    })
  highlight(0, 'LspInfoTitle',                 { fg = colors.pink, bg = colors.green                                    })
  highlight(0, 'LspInfoTip',                 { fg = colors.pink, bg = colors.orange                                    })
  highlight(0, 'LspInfoList',                 { fg = colors.pink, bg = colors.greenLight                                    })
  highlight(0, 'LspInfoBorder',                 { fg = colors.pink, bg = colors.blueLink                                    })

end

return lsp

