------------------------------------------------------------------------------
-- Markdown
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local markdown  = {}


--------------------------------------------------------------
-- Settings

markdown.setupHighlighting = function()
  highlight(0, 'markdownBlockquote',         { fg = colors.blue,     bg = 'NONE'                      })
  highlight(0, 'markdownBold',               { fg = colors.white,    bg = 'NONE', bold = true,        })
  highlight(0, 'markdownBoldDelimiter',      { fg = colors.blue,     bg = 'NONE'                      })
  highlight(0, 'markdownCode',               { fg = colors.green,    bg = 'NONE'                      })
  highlight(0, 'markdownCodeBlock',          { fg = colors.green,    bg = 'NONE'                      })
  highlight(0, 'markdownCodeDelimiter',      { fg = colors.blue,     bg = 'NONE'                      })
  highlight(0, 'markdownH1',                 { fg = colors.pink,     bg = 'NONE', bold = true         })
  highlight(0, 'markdownH2',                 { fg = colors.pink,     bg = 'NONE', bold = true         })
  highlight(0, 'markdownH3',                 { fg = colors.pink,     bg = 'NONE', bold = true         })
  highlight(0, 'markdownH4',                 { fg = colors.pink,     bg = 'NONE', bold = true         })
  highlight(0, 'markdownH5',                 { fg = colors.pink,     bg = 'NONE', bold = true         })
  highlight(0, 'markdownH6',                 { fg = colors.pink,     bg = 'NONE', bold = true         })
  highlight(0, 'markdownHeadingDelimiter',   { fg = colors.blue,     bg = 'NONE'                      })
  highlight(0, 'markdownItalic',             { fg = colors.white,    bg = 'NONE', italic = true       })
  highlight(0, 'markdownLinkDelimiter',      { fg = colors.white,    bg = 'NONE'                      })
  highlight(0, 'markdownLinkText',           { fg = colors.white,    bg = 'NONE'                      })
  highlight(0, 'markdownListMarker',         { fg = colors.blue,     bg = 'NONE'                      })
  highlight(0, 'markdownOrderedListMarker',  { fg = colors.blue,     bg = 'NONE'                      })
  if vim.fn.has('nvim-0.7.3') == 1 then
    highlight(0, 'markdownUrl',              { fg = colors.blueLink, bg = 'NONE', underdotted = true, })
  else
    highlight(0, 'markdownUrl',              { fg = colors.blueLink, bg = 'NONE', underdot = true,    })
  end
  highlight(0, 'mkdHeading',                 { fg = colors.blue,     bg = 'NONE'                      })
  highlight(0, 'mkdHeading',                 { fg = colors.blue,     bg = 'NONE'                      })  -- Hash symbols before heading text.
  highlight(0, 'mkdCode',                    { fg = colors.green,    bg = 'NONE'                      })  -- Inline code content.
  highlight(0, 'mkdCodeDelimiter',           { fg = colors.blue,     bg = 'NONE'                      })  -- Inline code backticks.
  highlight(0, 'mkdCodeStart',               { fg = colors.blue,     bg = 'NONE'                      })  -- Code block start delimiter.
  highlight(0, 'mkdCodeEnd',                 { fg = colors.blue,     bg = 'NONE'                      })  -- Code block end delimiter.
  highlight(0, 'mkdNonListItemBlock',        { fg = colors.white,    bg = 'NONE'                      })  -- Regular text.
  highlight(0, 'mkdLineBreak',               { fg = colors.blue,     bg = 'NONE'                      })  -- Line break dots (two spaces at end of line).
  highlight(0, 'mkdRule',                    { fg = colors.blue,     bg = 'NONE'                      })  -- Horizontal line indicator.
  highlight(0, 'mkdLink',                    { fg = colors.purple,   bg = 'NONE'                      })  -- Link text.
  highlight(0, 'mkdURL',                     { fg = colors.blueLink, bg = 'NONE', underline = true    })  -- Link URL.
  highlight(0, 'mkdListItem',                { fg = colors.blue,     bg = 'NONE'                      })  -- List item bullet.
  highlight(0, 'mkdListItemLine',            { fg = colors.white,    bg = 'NONE'                      })  -- List item text.
  highlight(0, 'mkdBlockquote',              { fg = colors.orange,   bg = 'NONE'                      })  -- Whole block quotes.
  highlight(0, 'mkdInlineURL',               { fg = colors.blueLink, bg = 'NONE'                      })  -- Link in regular text.
  highlight(0, 'mkdBold',                    { fg = colors.blue,     bg = 'NONE'                      })  -- Bold delimiter.

  ----------------------- Not used by now:
  highlight(0, 'mkdItalic',                  { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdBoldItalic',              { fg = colors.green, bg = colors.blue                    })
  highlight(0, 'mkdDelimiter',               { fg = colors.green, bg = colors.blueLight               })
  highlight(0, 'mkdFootnotes',               { fg = colors.green, bg = colors.purple                  })
  highlight(0, 'mkdID',                      { fg = colors.green, bg = colors.red                     })
  highlight(0, 'mkdLinkDefTarget',           { fg = colors.green, bg = colors.white                   })
  highlight(0, 'mkdLinkDef',                 { fg = colors.green, bg = colors.gray                    })
  highlight(0, 'mkdLinkTitle',               { fg = colors.pink, bg = colors.orange                   })
  highlight(0, 'mkdFootnote',                { fg = colors.pink, bg = colors.orange                   })
  highlight(0, 'mkdMath',                    { fg = colors.blue, bg = colors.greenLight               })
  highlight(0, 'mkdStrike',                  { fg = colors.turquoise, bg = colors.gray                })
  highlight(0, 'mkdString',                  { fg = colors.turquoise, bg = colors.white               })
  highlight(0, 'markdownHeadingRule',        { fg = colors.turquoise, bg = colors.pink                })
  highlight(0, 'markdownId',                 { fg = colors.turquoise, bg = colors.red                 })
  highlight(0, 'markdownIdDeclaration',      { fg = colors.turquoise, bg = colors.orange              })
  highlight(0, 'markdownIdDelimiter',        { fg = colors.turquoise, bg = colors.blue                })
  highlight(0, 'markdownBoldItalic',         { fg = colors.greenLight, bg = colors.orange             })
  highlight(0, 'markdownRule',               { fg = colors.turquoise, bg = colors.orange              })
  highlight(0, 'markdownFootnote',           { fg = colors.blue, bg = colors.orange                   })
  highlight(0, 'markdownFootnoteDefinition', { fg = colors.red, bg = colors.orange                    })
  highlight(0, 'markdownEscape',             { fg = colors.pink, bg = colors.orange                   })
end

return markdown


