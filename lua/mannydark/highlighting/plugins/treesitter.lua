-------------------------------------------------------------------------------
-- Treesitter Neovim Plugin
-------------------------------------------------------------------------------

local colors     = require('mannydark.palette')
local highlight  = vim.api.nvim_set_hl
local treesitter = {}


--------------------------------------------------------------
-- Settings

treesitter.setupHighlighting = function()
    highlight(0, '@comment', { fg = colors.red, bg = 'NONE' })
    highlight(0, '@variable', { fg = colors.purple, bg = 'NONE' })
    highlight(0, '@variable.builtin', { fg = colors.blue, bg = 'NONE' }) -- $this keyword.
    highlight(0, '@string', { link = 'String' })
    highlight(0, '@number', { link = 'Number' })
    highlight(0, '@float', { link = 'Float' })
    highlight(0, '@boolean', { link = 'Boolean' })
    highlight(0, '@constant', { link = 'Constant' })
    highlight(0, '@type', { link = 'Type' }) -- Data type (maybe better in turquoise?).
    highlight(0, '@function', { link = 'Function' })
    highlight(0, '@keyword', { link = 'Keyword' })
    highlight(0, '@character', { link = 'Character' })
    highlight(0, '@conditional', { link = 'Conditional' })
    highlight(0, '@exception', { link = 'Exception' })
    highlight(0, '@include', { link = 'Include' })
    highlight(0, '@operator', { link = 'Operator' })
    highlight(0, '@preproc', { link = 'PreProc' })
    highlight(0, '@keyword.return', { link = 'Keyword' })
    highlight(0, '@method', { link = 'Function' })
    highlight(0, '@method.call', { link = 'Function' })
    highlight(0, '@keyword.function', { link = 'Keyword' })
    highlight(0, '@function.call', { link = 'Function' })
    highlight(0, '@text.todo', { fg = colors.red, bg = 'NONE', bold = true }) -- TODO comments.
    highlight(0, '@text.title', { link = 'Title' })
    highlight(0, '@tag.delimiter', { link = 'Tag' })                          -- Open/close bracket of tags.
    highlight(0, '@punctuation.delimiter', { fg = colors.white, bg = 'NONE' })
    highlight(0, '@punctuation.bracket', { fg = colors.white, bg = 'NONE' })
    highlight(0, '@punctuation.special', { fg = colors.white, bg = 'NONE' })
    highlight(0, '@constant.builtin', { fg = colors.purple, bg = 'NONE' }) -- Constants like __FILE__.          })
    highlight(0, '@type.builtin', { fg = colors.blue, bg = 'NONE' })       -- Return types:
    highlight(0, '@parameter', { fg = 'NONE', bg = 'NONE' })               -- In PHP, every parameter with it's data type.
    highlight(0, '@constructor', { fg = colors.orange, bg = 'NONE' })
    highlight(0, '@type.qualifier', { fg = colors.blue, bg = 'NONE' })
    highlight(0, '@storageclass', { fg = colors.blue, bg = 'NONE' })
    highlight(0, '@none', { fg = 'NONE', bg = 'NONE' })
    highlight(0, '@tag.attribute', { fg = colors.turquoise, bg = 'NONE' })
    highlight(0, '@namespace', { fg = colors.white, bg = 'NONE' }) -- Path of namespaces.
    highlight(0, '@function.builtin', { fg = colors.orange, bg = 'NONE' })
    highlight(0, '@attribute', { fg = colors.blue, bg = 'NONE' })  -- In PHP the @stuff in a doc block:
    highlight(0, '@property', { fg = colors.purple, bg = 'NONE' }) -- All properties (css classes too):
    highlight(0, '@field', { fg = colors.purple, bg = 'NONE' })
    highlight(0, '@keyword.operator', { fg = colors.blue, bg = 'NONE' })
    highlight(0, '@string.escape', { fg = colors.pink, bg = 'NONE' })

    highlight(0, '@lsp.type.variable', { fg = colors.purple, bg = "NONE" })     -- Variables marked by LSP.
    highlight(0, '@lsp.type.type', { fg = colors.blue, bg = "NONE" })           -- Keywords marked by LSP.
    highlight(0, '@lsp.type.function', { fg = colors.orange, bg = "NONE" })     -- Functions marked by LSP.
    highlight(0, '@lsp.type.parameter', { fg = colors.blue, bg = "NONE" })      -- Props in React functions marked by LSP.
    highlight(0, '@lsp.type.interface', { fg = colors.turquoise, bg = "NONE" }) -- Interface names marked by LSP.
    highlight(0, '@lsp.type.class', { fg = colors.turquoise, bg = "NONE" })     -- Class names marked by LSP.
    highlight(0, '@lsp.type.property', { fg = colors.purple, bg = "NONE" })     -- Properties/Attributes marked by LSP.
    highlight(0, '@lsp.type.comment', { fg = colors.red, bg = 'NONE' })         -- Comments marked by LSP.
    highlight(0, '@lsp.type.method', { fg = colors.orange, bg = 'NONE' })       -- Name of method.
    highlight(0, '@tag', { fg = colors.blue, bg = 'NONE' })                     -- Tags like HTML tags.
    highlight(0, '@label.vimdoc',
        { fg = colors.blue, bg = 'NONE', underline = true, bold = true })       -- Labels/Headings in vimdoc.

    highlight(0, '@string.regexp.tsx', { fg = colors.redLight, bg = 'NONE' })   -- Regular expression strings.


    -------------------------------------------------------------------------------
    --- C

    highlight(0, '@operator.c', { fg = colors.white, bg = 'NONE' })   --
    highlight(0, '@lsp.type.operator.c', { fg = colors.white, bg = 'NONE' })   --
    highlight(0, '@lsp.type.parameter.c', { fg = colors.purple, bg = 'NONE' })   --


    -------------------------------------------------------------------------------
    --- TypeScript/React

    highlight(0, '@tag.tsx', { fg = colors.turquoise, bg = 'NONE' })                            -- Tags.
    highlight(0, '@tag.builtin.tsx', { fg = colors.blue, bg = 'NONE' })                         -- HTML tags.
    highlight(0, '@tag.delimiter.tsx', { fg = colors.white, bg = 'NONE' })                      -- < > of tags.
    highlight(0, '@type.builtin.typescript', { fg = colors.blue, bg = 'NONE' })                 -- Data types.
    highlight(0, '@lsp.type.namespace.typescriptreact', { fg = colors.turquoise, bg = 'NONE' }) -- Namespace of types like 'React'.
    highlight(0, '@lsp.type.parameter.typescript', { fg = colors.purple, bg = 'NONE' })         -- Variables as parameters.
    highlight(0, '@lsp.type.parameter.typescriptreact', { fg = colors.purple, bg = 'NONE' })    -- Variables as parameters.
    highlight(0, '@lsp.type.type.typescriptreact', { fg = colors.turquoise, bg = 'NONE' })      -- .
    highlight(0, '@lsp.type.namespace.typescript', { fg = colors.turquoise, bg = 'NONE' })      -- Namespaces.
    highlight(0, '@character.special.tsx', { fg = colors.blue, bg = 'NONE' })                   -- HTML entities.
    highlight(0, '@character.special.typescript', { fg = colors.white, bg = 'NONE' })           -- Characters like *.
    highlight(0, '@string.regexp.typescript', { fg = colors.white, bg = 'NONE' })               -- Strings in regex.
    highlight(0, '@lsp.type.typeParameter.typescript', { fg = colors.turquoise, bg = 'NONE' })               -- Types as parameters.
    highlight(0, '@lsp.type.type.typescript', { fg = colors.turquoise, bg = 'NONE' })               -- Self made types.
    highlight(0, '@lsp.type.typeParameter.typescriptreact', { fg = colors.turquoise, bg = 'NONE' })               -- Types as parameters.
    highlight(0, '@lsp.type.enum.typescript', { fg = colors.turquoise, bg = 'NONE' })               -- Enum types.
    highlight(0, '@keyword.conditional.ternary.tsx', { fg = colors.white, bg = 'NONE' })             -- Ternary operator signs.


    -------------------------------------------------------------------------------
    --- Docker

    highlight(0, '@lsp.type.string.dockerfile', { fg = colors.redLight, bg = 'NONE' })             -- Strings.
    highlight(0, '@lsp.type.parameter.dockerfile', { fg = colors.white, bg = 'NONE' })             -- Regular text.
    highlight(0, '@lsp.type.variable.dockerfile', { fg = colors.purple, bg = 'NONE' })             -- Variables.
    highlight(0, '@lsp.type.keyword.dockerfile', { fg = colors.blue, bg = 'NONE' })             -- Variables.
    highlight(0, '@operator.dockerfile',         { fg = colors.white, bg = 'NONE' })             -- Operator (like :).
    highlight(0, '@lsp.type.operator.dockerfile', { fg = colors.white, bg = 'NONE' })             -- Operator (like =).
    highlight(0, '@lsp.type.class.dockerfile', { fg = colors.turquoise, bg = 'NONE' })             -- Image.
    highlight(0, '@lsp.type.property.dockerfile', { fg = colors.purple, bg = 'NONE' })             -- Image properties.
    highlight(0, '@lsp.type.namespace.dockerfile', { fg = colors.turquoise, bg = 'NONE' })             -- Namespaces.


    -------------------------------------------------------------------------------
    --- YAML

    highlight(0, '@string.yaml', { fg = colors.redLight, bg = 'NONE' })             -- Regular strings.
    highlight(0, '@string.special.path.bash', { fg = colors.redLight, bg = 'NONE' })             -- Path strings.



    -------------------------------------------------------------------------------
    --- tmux

    highlight(0, '@string.special.path.tmux', { fg = colors.redLight, bg = 'NONE' }) -- Strings (without ").


    -------------------------------------------------------------------------------
    --- Editorconfig

    highlight(0, '@string.special.path.editorconfig', { fg = colors.white, bg = 'NONE' }) -- * symbol.
    highlight(0, '@string.special.editorconfig', { fg = colors.redLight, bg = 'NONE' })   -- Strings (without ").
    highlight(0, '@character.special.editorconfig', { fg = colors.blue, bg = 'NONE' })    -- * symbol (...too?).

    -------------------------------------------------------------------------------
    --- JavaScript

    highlight(0, '@tag.delimiter.javascript', { fg = colors.white, bg = 'NONE' }) -- Tag delimiters like < >.
    highlight(0, '@tag.builtin.javascript', { fg = colors.blue, bg = 'NONE' })    -- Tag names.
    highlight(0, '@lsp.type.namespace.javascript', { fg = colors.turquoise, bg = 'NONE' })    -- Tag names.

    -------------------------------------------------------------------------------
    --- CSS

    highlight(0, '@character.special.css', { fg = colors.white, bg = 'NONE' }) -- * symbol.


    -------------------------------------------------------------------------------
    --- Git

    highlight(0, '@string.special.path.gitignore', { fg = colors.white, bg = 'NONE' }) -- Text.
    highlight(0, '@character.special.gitignore', { fg = colors.white, bg = 'NONE' })   -- * symbol.
    highlight(0, '@constant.gitignore', { fg = colors.purple, bg = 'NONE' })           -- Constants like chars in Regex.
    highlight(0, '@markup.heading.gitcommit', { fg = colors.blue, bg = 'NONE' })       -- Commit message in commit window.
    highlight(0, '@comment.gitcommit', { fg = colors.red, bg = 'NONE' })               -- Comments in commit window.
    highlight(0, '@markup.link.gitcommit', { fg = colors.purple, bg = 'NONE' })        -- Branch name in commit window.
    highlight(0, '@string.special.path.gitcommit', { fg = colors.blue, bg = 'NONE' })  -- Path of files in commit window.
    highlight(0, '@keyword.gitcommit', { fg = colors.red, bg = 'NONE' })               -- Words like 'modified'.
    highlight(0, '@string.special.git_config', { fg = colors.redLight, bg = 'NONE' })               -- Strings in git config files.


    -- highlight(0, '',              { fg = colors.,     bg = 'NONE'              })  -- .


    -----------------------------------------------------------------------------
    -- Lua
    highlight(0, '@constructor.lua', { fg = colors.white, bg = 'NONE' }) -- {} brackets.
    -- highlight(0, '',              { fg = colors.white,     bg = 'NONE'              })  -- .


    -----------------------------------------------------------------------------
    -- Vim
    highlight(0, '@function.macro.vim', { fg = colors.orange, bg = 'NONE' })   -- Function macros (in strings too).
    highlight(0, '@string.special.vim', { fg = colors.redLight, bg = 'NONE' }) -- Like * symbol in strings.
    -- highlight(0, '',              { fg = colors.white,     bg = 'NONE'              })  -- .


    -------------------------------------------------------------------------------
    --- Prisma

    highlight(0, '@type.prisma', { fg = colors.blue, bg = 'NONE' }) -- Constants like chars in Regex.

    -------------------------------------------------------------------------------
    --- Shell

    highlight(0, '@string.regexp.bash', { fg = colors.redLight, bg = 'NONE' }) -- Regular expressions in bash scripts.
    highlight(0, '@variable.parameter.bash', { fg = colors.white, bg = 'NONE' }) -- Parameters of commands.



    -------------------------------------------------------------------------------
    --- Rust

    highlight(0, '@lsp.type.macro.rust', { fg = colors.orange, bg = 'NONE' })
    highlight(0, '@lsp.type.namespace.rust', { fg = colors.blue, bg = 'NONE' })
    highlight(0, '@lsp.type.struct.rust', { fg = colors.turquoise, bg = 'NONE' })
    highlight(0, '@lsp.type.decorator.rust', { fg = colors.pink, bg = 'NONE' })
    highlight(0, '@function.macro.rust', { fg = colors.orange, bg = 'NONE' })
    highlight(0, '@lsp.type.enum.rust', { fg = colors.turquoise, bg = 'NONE' })
    highlight(0, '@lsp.type.string.rust', { fg = colors.redLight, bg = 'NONE' })
    highlight(0, '@lsp.type.operator.rust', { fg = colors.white, bg = 'NONE' })
    highlight(0, '@lsp.type.enumMember.rust', { fg = colors.purple, bg = 'NONE' })



    -------------------------------------------------------------------------------
    --- Markdown

    highlight(0, '@label.markdown', { fg = colors.green, bg = 'NONE' }) -- Labels of code blocks like 'bash'.



    highlight(0, '@text.reference', { fg = colors.pink, bg = colors.green }) -- Maybe that is the word reference highlighter?

    ----------------------- Not used by now:
    highlight(0, '@define', { fg = colors.green, bg = colors.red })
    highlight(0, '@string.regex', { fg = colors.blue, bg = colors.red })
    highlight(0, '@string.special', { fg = colors.red, bg = colors.orange })
    highlight(0, '@character.special', { fg = colors.white, bg = colors.purple })
    highlight(0, '@function.macro', { fg = colors.black, bg = colors.white })
    highlight(0, '@repeat', { fg = colors.black, bg = colors.white })
    highlight(0, '@debug', { fg = colors.black, bg = colors.white })
    highlight(0, '@label', { fg = colors.blue, bg = colors.white })
    highlight(0, '@type.definition', { fg = colors.purple, bg = colors.white })
    highlight(0, '@constant.macro', { fg = colors.red, bg = colors.green })
    highlight(0, '@symbol', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text.strong', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text.emphasis', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text.underline', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text.strike', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text.literal', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text.uri', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text.math', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text.environment', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text.environment.name', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text.note', { fg = colors.blue, bg = colors.red })
    highlight(0, '@text.warning', { fg = "NONE", bg = "NONE", sp = colors.orange, undercurl = true })
    highlight(0, '@text.danger', { fg = "NONE", bg = "NONE", sp = colors.red, undercurl = true })

    highlight(0, '@lsp.type.decorator', { fg = colors.blue, bg = colors.green })
    highlight(0, '@lsp.type.enum', { fg = colors.blue, bg = colors.greenLight })
    highlight(0, '@lsp.type.enumMember', { fg = colors.blue, bg = colors.orange })
    highlight(0, '@lsp.type.event', { fg = colors.blue, bg = colors.yellow })
    highlight(0, '@lsp.type.keyword', { fg = colors.blue, bg = colors.grayDark })
    highlight(0, '@lsp.type.macro', { fg = colors.blue, bg = colors.purple })
    highlight(0, '@lsp.type.modifier', { fg = colors.orange, bg = colors.orange })
    highlight(0, '@lsp.type.namespace', { fg = colors.orange, bg = colors.pink })
    highlight(0, '@lsp.type.number', { fg = colors.orange, bg = colors.red })
    highlight(0, '@lsp.type.operator', { fg = colors.orange, bg = colors.white })
    highlight(0, '@lsp.type.regexp', { fg = colors.purple, bg = colors.gray })
    highlight(0, '@lsp.type.string', { fg = colors.purple, bg = colors.red })
    highlight(0, '@lsp.type.struct', { fg = colors.purple, bg = colors.blue })
    highlight(0, '@lsp.type.typeParameter', { fg = colors.purple, bg = colors.orange })
end

return treesitter
