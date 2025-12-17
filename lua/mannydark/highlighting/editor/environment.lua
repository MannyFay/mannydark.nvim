-------------------------------------------------------------------------------
-- Editor Environment
-------------------------------------------------------------------------------

local colors                  = require('mannydark.palette')
local highlight               = vim.api.nvim_set_hl
local environment             = {}

--------------------------------------------------------------
-- Settings

environment.setupHighlighting = function()
    -- highlight(0, 'FloatBorder', {
    --     fg = vim.api.nvim_get_hl_by_name('NormalFloat', true).background,
    --     bg = vim.api.nvim_get_hl_by_name('NormalFloat', true).background,
    -- })

    highlight(0, 'IndentBlanklineChar',
        { fg = colors.grayDark, bg = 'NONE' })                                                   -- Indent Blank Line plugin vertical lines.
    highlight(0, 'SignColumn', { fg = 'NONE', bg = colors.black })                               -- Sign column left of the line numbers.
    highlight(0, 'ColorColumn', { fg = 'NONE', bg = colors.grayDark })                           -- Vertical visual guide line.
    highlight(0, 'LineNr', { fg = colors.gray, bg = 'NONE', underline = false })                 -- Line numbers.
    highlight(0, 'CursorLineNr', { fg = colors.white, bg = colors.grayDark })                    -- Line number of current line.
    highlight(0, 'VertSplit', { fg = colors.grayDark, bg = colors.grayDark })                    -- Vertical split separator line.
    highlight(0, 'WinSeparator', { fg = colors.grayDark, bg = colors.grayDark })                 -- Vertical split separator line.
    highlight(0, 'CursorLine', { fg = 'NONE', bg = colors.grayDark })                            -- Color of actual line.
    highlight(0, 'Search', { fg = colors.black, bg = colors.gray })                              -- Highlighting of search patterns.
    highlight(0, 'IncSearch', { fg = colors.black, bg = colors.white })                          -- Highlighting of first result from search pattern.
    highlight(0, 'EndOfBuffer', { fg = colors.black, bg = 'NONE' })                              -- Empty end of buffer.
    highlight(0, 'Visual', { fg = 'NONE', bg = colors.gray })                                    -- Selected text in visual mode.
    highlight(0, 'Pmenu', { fg = colors.white, bg = colors.grayDark })                           -- Context menus.
    highlight(0, 'PmenuSel', { fg = colors.black, bg = colors.gray })                            -- Selected item in context menus.
    highlight(0, 'MsgArea', { fg = colors.white, bg = 'NONE' })                                  -- Area below status line.
    highlight(0, 'ErrorMsg', { fg = colors.red, bg = 'NONE' })                                   -- Error messages below status line.
    highlight(0, 'WarningMsg', { fg = colors.orange, bg = 'NONE' })                              -- Warning messages below status line.
    highlight(0, 'Question', { fg = colors.green, bg = 'NONE' })                                 -- Questions that Neovim asks the user.
    highlight(0, 'NormalFloat', { fg = colors.white, bg = colors.grayDark })                     -- Neovims regular floating window.
    highlight(0, 'FloatBorder', { fg = colors.grayDark, bg = colors.grayDark })                  -- Border of Neovims regular floating window.
    highlight(0, 'Whitespace', { fg = colors.red, bg = 'NONE' })                                 -- Trailing white spaces in buffer.
    highlight(0, 'PmenuSbar', { fg = colors.grayDark, bg = colors.grayDark })                    -- Scroll bar background in context menus.
    highlight(0, 'PmenuThumb', { fg = colors.gray, bg = colors.gray })                           -- Scroll bar in context menus.
    highlight(0, 'MsgSeparator', { fg = 'NONE', bg = colors.black })                             -- Separator line above messages under status line.
    highlight(0, 'SpellBad', { fg = 'NONE', bg = 'NONE', sp = colors.gray, undercurl = true })   -- Spelling mistakes of a not recognized word.
    highlight(0, 'SpellCap', { fg = 'NONE', bg = 'NONE', sp = colors.gray, undercurl = true })   -- Spelling mistakes of not capitalized word.
    highlight(0, 'SpellLocal', { fg = 'NONE', bg = 'NONE', sp = colors.gray, undercurl = true }) -- Spelling mistakes in another language.
    highlight(0, 'SpellRare', { fg = 'NONE', bg = 'NONE', sp = colors.gray, undercurl = true })  -- Spelling mistakes of a rare word.
    highlight(0, 'ModeMsg', { fg = colors.green, bg = 'NONE' })                                  -- Messages of Neovim in specific modes.
    highlight(0, 'MoreMsg', { fg = colors.green, bg = 'NONE' })                                  -- Message of Neovim if it asks for more.
    highlight(0, 'NormalNC', { fg = 'NONE', bg = 'NONE' })                                       -- Inactive new buffer.
    highlight(0, 'SpecialKey', { fg = 'NONE', bg = 'NONE' })                                     -- Left sign column in floating window.
    highlight(0, 'TabLine', { fg = colors.white, bg = 'NONE' })                                  -- Tabs.
    highlight(0, 'TabLineSel', { fg = colors.blue, bg = 'NONE' })                                -- Current selected tab.
    highlight(0, 'TabLineFill', { fg = 'NONE', bg = 'NONE' })                                    -- Color of the tab line itself.
    highlight(0, 'qfFileName', { fg = colors.white, bg = 'NONE' })                               -- File name in quick fix list.

    -- Diff (for :diffsplit, git diffs, etc.)
    highlight(0, 'DiffAdd',    { fg = 'NONE', bg = '#2E4A3E' })                                  -- Added lines.
    highlight(0, 'DiffChange', { fg = 'NONE', bg = colors.orangeDark })                         -- Changed lines.
    highlight(0, 'DiffDelete', { fg = colors.red, bg = colors.redDark })                        -- Deleted lines.
    highlight(0, 'DiffText',   { fg = colors.white, bg = colors.orange, bold = true })          -- Changed text within a line.

    -- Window bar (Neovim 0.8+)
    highlight(0, 'WinBar',     { fg = colors.white, bg = 'NONE', bold = true })                 -- Window bar (active).
    highlight(0, 'WinBarNC',   { fg = colors.gray, bg = 'NONE' })                               -- Window bar (inactive).

    -- Floating windows
    highlight(0, 'FloatTitle', { fg = colors.blue, bg = colors.grayDark, bold = true })         -- Title of floating windows.

    -- Search (Neovim 0.10+)
    highlight(0, 'CurSearch',  { fg = colors.black, bg = colors.orange })                       -- Current search match under cursor.

    -- Folding
    highlight(0, 'Folded',     { fg = colors.gray, bg = colors.grayDark })                      -- Folded lines.
    highlight(0, 'FoldColumn', { fg = colors.gray, bg = 'NONE' })                               -- Fold column on the left.

    -- Match
    highlight(0, 'MatchParen', { fg = colors.white, bg = colors.gray, bold = true })            -- Matching parenthesis/bracket.

    ----------------------- Not used by now:
    highlight(0, 'Cursor', { fg = colors.red, bg = colors.green })
    highlight(0, 'lCursor', { fg = colors.red, bg = colors.green })
    highlight(0, 'CursorIM', { fg = colors.red, bg = colors.green })
    highlight(0, 'WildMenu', { fg = colors.red, bg = colors.purple })
    highlight(0, 'Folded', { fg = colors.green, bg = colors.orange })
    highlight(0, 'FoldColumn', { fg = colors.red, bg = colors.blue })
    highlight(0, 'CursorColumn', { fg = colors.blue, bg = colors.red })
    highlight(0, 'VisualNOS', { fg = colors.blue, bg = colors.green })
    highlight(0, 'QuickFixLine', { fg = colors.green, bg = colors.blue })
    highlight(0, 'MatchWord', { fg = colors.pink, bg = colors.purple })
    highlight(0, 'MatchWordCur', { fg = colors.orange, bg = colors.green })
    highlight(0, 'MatchParenCur', { fg = colors.purple, bg = colors.pink })
    highlight(0, 'TermCursor', { fg = colors.pink, bg = colors.purple })
    highlight(0, 'TermCursorNC', { fg = colors.white, bg = colors.pink })
    highlight(0, 'Conceal', { fg = colors.orange, bg = colors.purple })
    highlight(0, 'Substitute', { fg = colors.orange, bg = colors.purple })
    highlight(0, 'NonText', { fg = colors.grayDark, bg = colors.pink })
end

return environment
