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
  highlight(0, 'mkdListItem',                { fg = colors.blue,     bg = 'NONE'                      })

  ----------------------- Not used by now:
  highlight(0, 'mkdItalic',                  { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdBold',                    { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdBoldItalic',              { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdDelimiter',               { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdFootnotes',               { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdID',                      { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdURL',                     { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdLink',                    { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdInlineURL',               { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdLinkDefTarget',           { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdLinkDef',                 { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdLinkTitle',               { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdHeading',                 { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdLineBreak',               { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdBlockquote',              { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdCodeDelimiter',           { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdCode',                    { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdFootnote',                { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdListItem',                { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdListItemLine',            { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdNonListItemBlock',        { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdRule',                    { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdMath',                    { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdStrike',                  { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdString',                  { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdCodeStart',               { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'mkdCodeEnd',                 { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'markdownHeadingRule',        { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'markdownId',                 { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'markdownIdDeclaration',      { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'markdownIdDelimiter',        { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'markdownBoldItalic',         { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'markdownRule',               { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'markdownFootnote',           { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'markdownFootnoteDefinition', { fg = colors.green, bg = colors.orange                  })
  highlight(0, 'markdownEscape',             { fg = colors.green, bg = colors.orange                  })
end

return markdown
