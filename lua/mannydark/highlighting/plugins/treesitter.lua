-------------------------------------------------------------------------------
-- Treesitter Neovim Plugin
-------------------------------------------------------------------------------

local colors     = require('mannydark.palette')
local highlight  = vim.api.nvim_set_hl
local treesitter = {}


--------------------------------------------------------------
-- Settings

treesitter.setupHighlighting = function()
  highlight(0, '@comment',               { fg = colors.red,       bg = 'NONE'                        })
  highlight(0, '@variable',              { fg = colors.purple,    bg = 'NONE'                        })
  highlight(0, '@variable.builtin',      { fg = colors.blue,      bg = 'NONE'                        })  -- $this keyword.
  highlight(0, '@string',                { link = 'String'                                           })
  highlight(0, '@number',                { link = 'Number'                                           })
  highlight(0, '@float',                 { link = 'Float'                                            })
  highlight(0, '@boolean',               { link = 'Boolean'                                          })
  highlight(0, '@constant',              { link = 'Constant'                                         })
  highlight(0, '@type',                  { link = 'Type'                                             })  -- Data type (maybe better in turquoise?).
  highlight(0, '@function',              { link = 'Function'                                         })
  highlight(0, '@keyword',               { link = 'Keyword'                                          })
  highlight(0, '@character',             { link = 'Character'                                        })
  highlight(0, '@conditional',           { link = 'Conditional'                                      })
  highlight(0, '@exception',             { link = 'Exception'                                        })
  highlight(0, '@include',               { link = 'Include'                                          })
  highlight(0, '@operator',              { link = 'Operator'                                         })
  highlight(0, '@preproc',               { link = 'PreProc'                                          })
  highlight(0, '@keyword.return',        { link = 'Keyword'                                          })
  highlight(0, '@method',                { link = 'Function'                                         })
  highlight(0, '@method.call',           { link = 'Function'                                         })
  highlight(0, '@keyword.function',      { link = 'Keyword'                                          })
  highlight(0, '@function.call',         { link = 'Function'                                         })
  highlight(0, '@text.todo',             { link = 'Todo'                                             })
  highlight(0, '@text.title',            { link = 'Title'                                            })
  highlight(0, '@tag.delimiter',         { link = 'Tag'                                              })  -- Open/close bracket of tags.
  highlight(0, '@punctuation.delimiter', { fg = colors.white,     bg = 'NONE'                        })
  highlight(0, '@punctuation.bracket',   { fg = colors.white,     bg = 'NONE'                        })
  highlight(0, '@punctuation.special',   { fg = colors.white,     bg = 'NONE'                        })
  highlight(0, '@constant.builtin',      { link = 'Constant'                                         })
  highlight(0, '@type.builtin',          { fg = colors.blue,      bg = 'NONE'                        })  -- Return types:
  highlight(0, '@parameter',             { fg = 'NONE',           bg = 'NONE'                        })  -- In PHP, every parameter with it's data type.
  highlight(0, '@constructor',           { fg = colors.orange,    bg = 'NONE'                        })
  highlight(0, '@type.qualifier',        { fg = colors.blue,      bg = 'NONE'                        })
  highlight(0, '@storageclass',          { fg = colors.blue,      bg = 'NONE'                        })
  highlight(0, '@none',                  { fg = 'NONE',           bg = 'NONE'                        })
  highlight(0, '@tag.attribute',         { fg = colors.turquoise, bg = 'NONE'                        })
  highlight(0, '@namespace',             { fg = colors.white,     bg = 'NONE'                        })  -- Path of namespaces.
  highlight(0, '@function.builtin',      { fg = colors.orange,    bg = 'NONE'                        })
  highlight(0, '@attribute',             { fg = colors.blue,      bg = 'NONE'                        })  -- In PHP the @stuff in a doc block:
  highlight(0, '@property',              { fg = colors.purple,    bg = 'NONE'                        })  -- All properties (css classes too):
  highlight(0, '@field',                 { fg = colors.purple,    bg = 'NONE'                        })
  highlight(0, '@keyword.operator',      { fg = colors.blue,      bg = 'NONE'                        })
  highlight(0, '@string.escape',         { fg = colors.pink,      bg = 'NONE'                        })
  highlight(0, '@lsp.type.comment',      { fg = colors.gray,      bg = 'NONE'                        })


  ----------------------- Not used by now:
  highlight(0, '@define',                { fg = colors.orange,    bg = colors.red                    })
  highlight(0, '@string.regex',          { fg = colors.blue,      bg = colors.red                    })
  highlight(0, '@string.special',        { fg = colors.red,       bg = colors.orange                 })
  highlight(0, '@character.special',     { fg = colors.white,     bg = colors.purple                 })
  highlight(0, '@function.macro',        { fg = colors.black,     bg = colors.white                  })
  highlight(0, '@repeat',                { link = 'Repeat'                                           })
  highlight(0, '@debug',                 { link = 'Debug'                                            })
  highlight(0, '@label',                 { fg = colors.blue,      bg = 'NONE'                        })
  highlight(0, '@type.definition',       { fg = colors.purple,    bg = 'NONE'                        })
  highlight(0, '@constant.macro',        { fg = colors.red,       bg = colors.green                  })
  highlight(0, '@symbol',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@text',                  { link = 'None'                                             })
  highlight(0, '@text.strong',           { link = 'Bold'                                             })
  highlight(0, '@text.emphasis',         { link = 'Italic'                                           })
  highlight(0, '@text.underline',        { link = 'Underlined'                                       })
  highlight(0, '@text.strike',           { fg = 'NONE',           bg = 'NONE',  strikethrough = true })
  highlight(0, '@text.literal',          { link = 'String'                                           })
  highlight(0, '@text.uri',              { link = 'Underlined'                                       })
  highlight(0, '@text.math',             { link = 'Special'                                          })
  highlight(0, '@text.environment',      { link = 'Macro'                                            })
  highlight(0, '@text.environment.name', { link = 'Type'                                             })
  highlight(0, '@text.reference',        { link = 'Constant'                                         })
  highlight(0, '@text.note',             { link = 'SpecialComment'                                   })
  highlight(0, '@text.warning',          { link = 'Todo'                                             })
  highlight(0, '@text.danger',           { link = 'WarningMsg'                                       })
  highlight(0, '@tag',                   { link = 'Tag'                                              })

  highlight(0, '@lsp.type.class',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.comment',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.decorator',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.enum',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.enumMember',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.event',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.function',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.interface',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.keyword',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.macro',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.method',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.modifier',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.namespace',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.number',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.operator',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.parameter',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.property',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.regexp',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.string',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.struct',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.type',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.typeParameter',                { fg = colors.blue,      bg = colors.orange                 })
  highlight(0, '@lsp.type.variable',                { fg = colors.blue,      bg = colors.orange                 })

end

return treesitter

