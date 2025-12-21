# Customization

Here you can find information on how to customize the colorscheme to suit your preferences.

---

<br>

## Configuration
These are all the available configuration options you can set by now:
```lua
config = function()
  local colorscheme = require('mannydark')

  colorscheme.setup({
    -- Set the theme style you like.
    -- This is the initial style every time you start Neovim.
    -- While using the editor, you can change it with :MannydarkToggle <style>
    style = "dark",  -- dark | bright | red-green

    -- UI options you can change (true = enable, false = disable):
    transparent     = false,  -- Transparent background.
    dim_inactive    = false,  -- Dim inactive windows.
    terminal_colors = true,  -- Apply colors to :terminal.

    -- Per-element styling (e.g., { italic = true }, { bold = true }):
    styles = {
      comments  = { italic = true, bold = true },
      keywords  = {},
      functions = {},
      variables = {},
      strings   = {},
      types     = {},
    },

    on_colors = function(colors)
      -- Here you can override the colors of the scheme.
      -- Just add another hex value to the colors table in the desired theme style.
      -- If changed, activate your changes with :MannydarkReload command.

      local style = require('mannydark').config.style

      -------------------------------------------------------------------------
      -- Dark Theme Styles (dark background)
      -------------------------------------------------------------------------

      if style == "dark" then
        -- Base:
        colors.black       = "#191B1C"   -- Main background.
        colors.white       = "#ABABAB"   -- Main foreground (text, operators, etc.).

        -- Grays
        colors.gray        = "#606366"   -- Muted text, line numbers, etc.
        colors.grayDark    = "#252525"   -- Secondary backgrounds, vertical lines, etc.
        colors.grayLight   = "#404040"   -- Borders, dividers, etc.

        -- Syntax
        colors.blue        = "#569CD6"   -- Keywords, control flow.
        colors.blueLink    = "#287BDE"   -- Links, references.
        colors.green       = "#4FA544"   -- Success, Git added, doc block comments.
        colors.greenLight  = "#A5C25C"   -- Numbers.
        colors.orange      = "#E8BF6A"   -- Functions, methods, etc.
        colors.orangeDark  = "#3E372A"   -- Warning backgrounds.
        colors.pink        = "#ED3276"   -- Decorators, special keywords, escape sequences.
        colors.purple      = "#C064C7"   -- Variables, parameters, constants, properties, etc.
        colors.red         = "#FF0000"   -- Comments (because Uncle Bob says: "Make them big fat red...").
        colors.redDark     = "#553939"   -- Error backgrounds.
        colors.redLight    = "#CE9178"   -- Strings.
        colors.turquoise   = "#45C8B0"   -- Types, classes, namespaces, etc.
        colors.yellow      = "#DCDCAA"   -- Warnings, deprecated.

        -- UI
        colors.cursorLine  = "#252525"   -- Current line highlight.
        colors.selection   = "#264F78"   -- Selection background.
        colors.search      = "#613315"   -- Search highlight.
        colors.matchParen  = "#0D3A58"   -- Matching brackets.


      -------------------------------------------------------------------------
      -- Bright Theme Styles (light background)
      -------------------------------------------------------------------------

      elseif style == "bright" then

        -- Base
        colors.black       = "#FEFEFE"   -- Main background (white).
        colors.white       = "#404040"   -- Main foreground (text, operators, etc.).

        -- Grays
        colors.gray        = "#A0A1A7"   -- Muted text, line numbers, etc.
        colors.grayDark    = "#F0F0F0"   -- Secondary backgrounds, vertical lines, etc.
        colors.grayLight   = "#D3D3D3"   -- Borders, dividers, etc.

        -- Syntax
        colors.blue        = "#569CD6"   -- Keywords, control flow.
        colors.blueLink    = "#1A5FB4"   -- Links, references.
        colors.green       = "#2E8B57"   -- Success, Git added, doc block comments.
        colors.greenLight  = "#A5C25C"   -- Numbers.
        colors.orange      = "#E8BF6A"   -- Functions, methods, etc.
        colors.orangeDark  = "#FFF3CD"   -- Warning backgrounds.
        colors.pink        = "#D63384"   -- Decorators, special keywords, escape sequences.
        colors.purple      = "#C064C7"   -- Variables, parameters, constants, properties, etc.
        colors.red         = "#FF0000"   -- Comments (because Uncle Bob says: "Make them big fat red...").
        colors.redDark     = "#FFE5E5"   -- Error backgrounds.
        colors.redLight    = "#CE9178"   -- Strings.
        colors.turquoise   = "#0997B3"   -- Types, classes, namespaces, etc.
        colors.yellow      = "#B8860B"   -- Warnings, deprecated.

        -- UI
        colors.cursorLine  = "#F5F5F5"   -- Current line highlight.
        colors.selection   = "#ADD6FF"   -- Selection background.
        colors.search      = "#FFECB3"   -- Search highlight.
        colors.matchParen  = "#B4D7FF"   -- Matching brackets.


      -------------------------------------------------------------------------
      -- Red-Green Colorblind Friendly Theme
      -- Optimized for Deuteranopia and Protanopia
      -------------------------------------------------------------------------

      elseif style == "red-green" then

        -- Base
        colors.black       = "#191B1C"   -- Main background.
        colors.white       = "#ABABAB"   -- Main foreground (text, operators, etc.).

        -- Grays
        colors.gray        = "#606366"   -- Muted text, line numbers, etc.
        colors.grayDark    = "#252525"   -- Secondary backgrounds, vertical lines, etc.
        colors.grayLight   = "#404040"   -- Borders, dividers, etc.

        -- Syntax (avoids red/green confusion)
        colors.blue        = "#569CD6"   -- Keywords, control flow.
        colors.blueLink    = "#287BDE"   -- Links, references.
        colors.green       = "#3498DB"   -- Success, Git added, doc block comments (blue instead of green).
        colors.greenLight  = "#F1C40F"   -- Numbers (yellow instead of green).
        colors.orange      = "#E8BF6A"   -- Functions, methods, etc.
        colors.orangeDark  = "#3E372A"   -- Warning backgrounds.
        colors.pink        = "#9B59B6"   -- Decorators, special keywords, escape sequences (purple instead of pink).
        colors.purple      = "#C064C7"   -- Variables, parameters, constants, properties, etc.
        colors.red         = "#E67E22"   -- Comments (orange instead of red).
        colors.redDark     = "#4A3728"   -- Error backgrounds.
        colors.redLight    = "#CE9178"   -- Strings.
        colors.turquoise   = "#45C8B0"   -- Types, classes, namespaces, etc.
        colors.yellow      = "#DCDCAA"   -- Warnings, deprecated.

        -- UI
        colors.cursorLine  = "#252525"   -- Current line highlight.
        colors.selection   = "#264F78"   -- Selection background.
        colors.search      = "#613315"   -- Search highlight.
        colors.matchParen  = "#0D3A58"   -- Matching brackets.

      end
    end,

    on_highlights = function(hl, colors)
      -- Here you can override the highlights of the specific UI partials of the scheme.
      -- If changed, activate your changes with :MannydarkReload command.

      local style = require('mannydark').config.style

      -------------------------------------------------------------------------
      -- Editor UI
      -------------------------------------------------------------------------

      hl.Normal        = { fg = colors.white,     bg = colors.black }
      hl.NormalNC      = { fg = colors.white,     bg = colors.black }
      hl.NormalFloat   = { fg = colors.white,     bg = colors.grayDark }
      hl.FloatBorder   = { fg = colors.gray,      bg = colors.grayDark }
      hl.FloatTitle    = { fg = colors.white,     bg = colors.grayDark }
      hl.Cursor        = { fg = colors.black,     bg = colors.white }
      hl.CursorLine    = { bg = colors.cursorLine }
      hl.CursorLineNr  = { fg = colors.white,     bg = "NONE" }
      hl.LineNr        = { fg = colors.gray,      bg = "NONE" }
      hl.SignColumn    = { fg = "NONE",           bg = "NONE" }
      hl.ColorColumn   = { bg = colors.grayDark }
      hl.Visual        = { bg = colors.selection }
      hl.VisualNOS     = { bg = colors.selection }
      hl.Search        = { bg = colors.search }
      hl.IncSearch     = { fg = colors.black,     bg = colors.orange }
      hl.CurSearch     = { fg = colors.black,     bg = colors.orange }
      hl.MatchParen    = { fg = colors.white,     bg = colors.gray }
      hl.Folded        = { fg = colors.gray,      bg = colors.grayDark }
      hl.FoldColumn    = { fg = colors.gray,      bg = "NONE" }
      hl.VertSplit     = { fg = colors.grayLight, bg = "NONE" }
      hl.WinSeparator  = { fg = colors.grayLight, bg = "NONE" }
      hl.StatusLine    = { fg = colors.white,     bg = colors.grayDark }
      hl.StatusLineNC  = { fg = colors.gray,      bg = colors.grayDark }
      hl.TabLine       = { fg = colors.gray,      bg = colors.grayDark }
      hl.TabLineFill   = { bg = colors.grayDark }
      hl.TabLineSel    = { fg = colors.white,     bg = colors.black }
      hl.WildMenu      = { fg = colors.black,     bg = colors.blue }
      hl.Pmenu         = { fg = colors.white,     bg = colors.grayDark }
      hl.PmenuSel      = { fg = colors.white,     bg = colors.selection }
      hl.PmenuSbar     = { bg = colors.grayDark }
      hl.PmenuThumb    = { bg = colors.gray }
      hl.EndOfBuffer   = { fg = colors.gray }
      hl.NonText       = { fg = colors.gray }
      hl.SpecialKey    = { fg = colors.gray }
      hl.Whitespace    = { fg = colors.grayLight }
      hl.Directory     = { fg = colors.blue }
      hl.Title         = { fg = colors.white }
      hl.Question      = { fg = colors.green }
      hl.MoreMsg       = { fg = colors.green }
      hl.ModeMsg       = { fg = colors.white }
      hl.ErrorMsg      = { fg = colors.red }
      hl.WarningMsg    = { fg = colors.orange }

      -------------------------------------------------------------------------
      -- Syntax (Vim Legacy Groups)
      -------------------------------------------------------------------------

      hl.Comment        = { fg = colors.red }
      hl.Constant       = { fg = colors.purple }
      hl.String         = { fg = colors.redLight }
      hl.Character      = { fg = colors.redLight }
      hl.Number         = { fg = colors.greenLight }
      hl.Float          = { fg = colors.greenLight }
      hl.Boolean        = { fg = colors.blue }
      hl.Identifier     = { fg = colors.purple }
      hl.Variable       = { fg = colors.purple }
      hl.Function       = { fg = colors.orange }
      hl.Statement      = { fg = colors.blue }
      hl.Conditional    = { fg = colors.blue }
      hl.Repeat         = { fg = colors.blue }
      hl.Label          = { fg = colors.blue }
      hl.Operator       = { fg = colors.white }
      hl.Keyword        = { fg = colors.blue }
      hl.Exception      = { fg = colors.blue }
      hl.PreProc        = { fg = colors.blue }
      hl.Include        = { fg = colors.blue }
      hl.Define         = { fg = colors.blue }
      hl.Macro          = { fg = colors.pink }
      hl.PreCondit      = { fg = colors.blue }
      hl.Type           = { fg = colors.turquoise }
      hl.StorageClass   = { fg = colors.turquoise }
      hl.Structure      = { fg = colors.turquoise }
      hl.Typedef        = { fg = colors.turquoise }
      hl.Special        = { fg = colors.pink }
      hl.SpecialChar    = { fg = colors.pink }
      hl.SpecialComment = { fg = colors.red }
      hl.Tag            = { fg = colors.blue }
      hl.Delimiter      = { fg = colors.white }
      hl.Debug          = { fg = colors.orange }
      hl.Underlined     = { fg = colors.blueLink, underline = true }
      hl.Ignore         = { fg = colors.gray }
      hl.Error          = { fg = colors.red, undercurl = true }
      hl.Todo           = { fg = colors.red }

      -------------------------------------------------------------------------
      -- Treesitter Highlights (@xxx)
      -------------------------------------------------------------------------

      -- Variables
      hl["@variable"]                    = { fg = colors.purple }
      hl["@variable.builtin"]            = { fg = colors.purple }
      hl["@variable.parameter"]          = { fg = colors.purple }
      hl["@variable.parameter.builtin"]  = { fg = colors.purple }
      hl["@variable.member"]             = { fg = colors.purple }

      -- Constants
      hl["@constant"]                    = { fg = colors.purple }
      hl["@constant.builtin"]            = { fg = colors.purple }
      hl["@constant.macro"]              = { fg = colors.purple }

      -- Modules
      hl["@module"]                      = { fg = colors.turquoise }
      hl["@module.builtin"]              = { fg = colors.turquoise }

      -- Labels
      hl["@label"]                       = { fg = colors.blue }

      -- Strings
      hl["@string"]                      = { fg = colors.redLight }
      hl["@string.documentation"]        = { fg = colors.redLight }
      hl["@string.regexp"]               = { fg = colors.orange }
      hl["@string.escape"]               = { fg = colors.pink }
      hl["@string.special"]              = { fg = colors.pink }
      hl["@string.special.symbol"]       = { fg = colors.purple }
      hl["@string.special.path"]         = { fg = colors.redLight }
      hl["@string.special.url"]          = { fg = colors.blueLink, underline = true }

      -- Characters
      hl["@character"]                   = { fg = colors.redLight }
      hl["@character.special"]           = { fg = colors.pink }

      -- Numbers
      hl["@number"]                      = { fg = colors.greenLight }
      hl["@number.float"]                = { fg = colors.greenLight }

      -- Booleans
      hl["@boolean"]                     = { fg = colors.blue }

      -- Types
      hl["@type"]                        = { fg = colors.turquoise }
      hl["@type.builtin"]                = { fg = colors.turquoise }
      hl["@type.definition"]             = { fg = colors.turquoise }
      hl["@type.qualifier"]              = { fg = colors.blue }

      -- Attributes
      hl["@attribute"]                   = { fg = colors.purple }
      hl["@attribute.builtin"]           = { fg = colors.purple }

      -- Properties
      hl["@property"]                    = { fg = colors.white }

      -- Functions
      hl["@function"]                    = { fg = colors.orange }
      hl["@function.builtin"]            = { fg = colors.orange }
      hl["@function.call"]               = { fg = colors.orange }
      hl["@function.macro"]              = { fg = colors.orange }
      hl["@function.method"]             = { fg = colors.orange }
      hl["@function.method.call"]        = { fg = colors.orange }

      -- Constructors
      hl["@constructor"]                 = { fg = colors.turquoise }

      -- Operators
      hl["@operator"]                    = { fg = colors.white }

      -- Keywords
      hl["@keyword"]                     = { fg = colors.blue }
      hl["@keyword.coroutine"]           = { fg = colors.blue }
      hl["@keyword.function"]            = { fg = colors.blue }
      hl["@keyword.operator"]            = { fg = colors.blue }
      hl["@keyword.import"]              = { fg = colors.blue }
      hl["@keyword.type"]                = { fg = colors.blue }
      hl["@keyword.modifier"]            = { fg = colors.blue }
      hl["@keyword.repeat"]              = { fg = colors.blue }
      hl["@keyword.return"]              = { fg = colors.blue }
      hl["@keyword.exception"]           = { fg = colors.blue }
      hl["@keyword.conditional"]         = { fg = colors.blue }
      hl["@keyword.directive"]           = { fg = colors.blue }
      hl["@keyword.storage"]             = { fg = colors.blue }
      hl["@keyword.debug"]               = { fg = colors.orange }
      hl["@keyword.directive.define"]    = { fg = colors.pink }
      hl["@keyword.conditional.ternary"] = { fg = colors.white }

      -- Punctuation
      hl["@punctuation.delimiter"]       = { fg = colors.white }
      hl["@punctuation.bracket"]         = { fg = colors.white }
      hl["@punctuation.special"]         = { fg = colors.white }

      -- Comments
      hl["@comment"]                     = { fg = colors.red }
      hl["@comment.documentation"]       = { fg = colors.red }
      hl["@comment.error"]               = { fg = colors.red }
      hl["@comment.warning"]             = { fg = colors.orange, bold = true }
      hl["@comment.todo"]                = { fg = colors.red }
      hl["@comment.note"]                = { fg = colors.turquoise, bold = true }

      -- Markup (Markdown, etc.)
      hl["@markup.strong"]               = { bold = true }
      hl["@markup.italic"]               = { italic = true }
      hl["@markup.strikethrough"]        = { fg = colors.gray, strikethrough = true }
      hl["@markup.underline"]            = { underline = true }
      hl["@markup.heading"]              = { fg = colors.blue, bold = true }
      hl["@markup.heading.1"]            = { fg = colors.blue, bold = true }
      hl["@markup.heading.2"]            = { fg = colors.blue, bold = true }
      hl["@markup.heading.3"]            = { fg = colors.blue, bold = true }
      hl["@markup.heading.4"]            = { fg = colors.blue, bold = true }
      hl["@markup.heading.5"]            = { fg = colors.blue, bold = true }
      hl["@markup.heading.6"]            = { fg = colors.blue, bold = true }
      hl["@markup.quote"]                = { fg = colors.gray, italic = true }
      hl["@markup.math"]                 = { fg = colors.greenLight }
      hl["@markup.link"]                 = { fg = colors.blueLink, underline = true }
      hl["@markup.link.label"]           = { fg = colors.turquoise }
      hl["@markup.link.url"]             = { fg = colors.blueLink }
      hl["@markup.raw"]                  = { fg = colors.redLight }
      hl["@markup.raw.block"]            = { fg = colors.redLight }
      hl["@markup.list"]                 = { fg = colors.blue }
      hl["@markup.list.checked"]         = { fg = colors.green }
      hl["@markup.list.unchecked"]       = { fg = colors.gray }

      -- Diff
      hl["@diff.plus"]                   = { fg = colors.green }
      hl["@diff.minus"]                  = { fg = colors.red }
      hl["@diff.delta"]                  = { fg = colors.orange }

      -- Tags (HTML, XML, JSX)
      hl["@tag"]                         = { fg = colors.blue }
      hl["@tag.builtin"]                 = { fg = colors.blue }
      hl["@tag.attribute"]               = { fg = colors.turquoise }
      hl["@tag.delimiter"]               = { fg = colors.white }

      -------------------------------------------------------------------------
      -- LSP Semantic Tokens (@lsp.type.xxx)
      -------------------------------------------------------------------------

      hl["@lsp.type.class"]              = { fg = colors.turquoise }
      hl["@lsp.type.comment"]            = { fg = colors.red }
      hl["@lsp.type.decorator"]          = { fg = colors.purple }
      hl["@lsp.type.enum"]               = { fg = colors.turquoise }
      hl["@lsp.type.enumMember"]         = { fg = colors.purple }
      hl["@lsp.type.event"]              = { fg = colors.orange }
      hl["@lsp.type.function"]           = { fg = colors.orange }
      hl["@lsp.type.interface"]          = { fg = colors.turquoise }
      hl["@lsp.type.keyword"]            = { fg = colors.blue }
      hl["@lsp.type.macro"]              = { fg = colors.pink }
      hl["@lsp.type.method"]             = { fg = colors.orange }
      hl["@lsp.type.modifier"]           = { fg = colors.blue }
      hl["@lsp.type.namespace"]          = { fg = colors.blue }
      hl["@lsp.type.number"]             = { fg = colors.greenLight }
      hl["@lsp.type.operator"]           = { fg = colors.white }
      hl["@lsp.type.parameter"]          = { fg = colors.purple }
      hl["@lsp.type.property"]           = { fg = colors.purple }
      hl["@lsp.type.regexp"]             = { fg = colors.orange }
      hl["@lsp.type.string"]             = { fg = colors.redLight }
      hl["@lsp.type.struct"]             = { fg = colors.turquoise }
      hl["@lsp.type.type"]               = { fg = colors.turquoise }
      hl["@lsp.type.typeParameter"]      = { fg = colors.purple }
      hl["@lsp.type.variable"]           = { fg = colors.purple }

      -- LSP Modifiers (@lsp.mod.xxx)
      hl["@lsp.mod.abstract"]            = { italic = true }
      hl["@lsp.mod.async"]               = { italic = true }
      hl["@lsp.mod.declaration"]         = {}
      hl["@lsp.mod.defaultLibrary"]      = {}
      hl["@lsp.mod.definition"]          = {}
      hl["@lsp.mod.deprecated"]          = { strikethrough = true }
      hl["@lsp.mod.documentation"]       = {}
      hl["@lsp.mod.modification"]        = {}
      hl["@lsp.mod.readonly"]            = {}
      hl["@lsp.mod.static"]              = { italic = true }

      -------------------------------------------------------------------------
      -- Diagnostics
      -------------------------------------------------------------------------

      -- Base
      hl.DiagnosticError            = { fg = colors.red }
      hl.DiagnosticWarn             = { fg = colors.yellow }
      hl.DiagnosticInfo             = { fg = colors.blue }
      hl.DiagnosticHint             = { fg = colors.turquoise }
      hl.DiagnosticOk               = { fg = colors.green }

      -- Virtual Text
      hl.DiagnosticVirtualTextError = { fg = colors.red }
      hl.DiagnosticVirtualTextWarn  = { fg = colors.yellow }
      hl.DiagnosticVirtualTextInfo  = { fg = colors.blue }
      hl.DiagnosticVirtualTextHint  = { fg = colors.turquoise }
      hl.DiagnosticVirtualTextOk    = { fg = colors.green }

      -- Underlines
      hl.DiagnosticUnderlineError   = { undercurl = true, sp = colors.red }
      hl.DiagnosticUnderlineWarn    = { undercurl = true, sp = colors.yellow }
      hl.DiagnosticUnderlineInfo    = { undercurl = true, sp = colors.blue }
      hl.DiagnosticUnderlineHint    = { undercurl = true, sp = colors.turquoise }
      hl.DiagnosticUnderlineOk      = { undercurl = true, sp = colors.green }

      -- Floating Windows
      hl.DiagnosticFloatingError    = { fg = colors.red }
      hl.DiagnosticFloatingWarn     = { fg = colors.yellow }
      hl.DiagnosticFloatingInfo     = { fg = colors.blue }
      hl.DiagnosticFloatingHint     = { fg = colors.turquoise }
      hl.DiagnosticFloatingOk       = { fg = colors.green }

      -- Signs
      hl.DiagnosticSignError        = { fg = colors.red }
      hl.DiagnosticSignWarn         = { fg = colors.orange }
      hl.DiagnosticSignInfo         = { fg = colors.blue }
      hl.DiagnosticSignHint         = { fg = colors.turquoise }
      hl.DiagnosticSignOk           = { fg = colors.green }

      -------------------------------------------------------------------------
      -- Git / Diff
      -------------------------------------------------------------------------

      hl.DiffAdd       = { fg = colors.green }
      hl.DiffChange    = { fg = colors.orange }
      hl.DiffDelete    = { fg = colors.red }
      hl.DiffText      = { fg = colors.blue }
      hl.diffAdded     = { fg = colors.green }
      hl.diffChanged   = { fg = colors.orange }
      hl.diffRemoved   = { fg = colors.red }
      hl.diffFile      = { fg = colors.blue }
      hl.diffIndexLine = { fg = colors.purple }
      hl.diffLine      = { fg = colors.gray }
      hl.diffNewFile   = { fg = colors.green }
      hl.diffOldFile   = { fg = colors.red }

      -------------------------------------------------------------------------
      -- LSP
      -------------------------------------------------------------------------

      hl.LspReferenceText            = { fg = "NONE", bg = colors.blue }
      hl.LspReferenceRead            = { fg = "NONE", bg = colors.blue }
      hl.LspReferenceWrite           = { fg = "NONE", bg = colors.blue }
      hl.LspCodeLens                 = { fg = colors.gray }
      hl.LspCodeLensSeparator        = { fg = colors.gray }
      hl.LspSignatureActiveParameter = { fg = colors.orange, bold = true }
      hl.LspInlayHint                = { fg = colors.gray, italic = true }

      -------------------------------------------------------------------------
      -- Spell
      -------------------------------------------------------------------------

      hl.SpellBad   = { undercurl = true, sp = colors.red }
      hl.SpellCap   = { undercurl = true, sp = colors.yellow }
      hl.SpellLocal = { undercurl = true, sp = colors.turquoise }
      hl.SpellRare  = { undercurl = true, sp = colors.purple }

    end,
  })

  vim.cmd.colorscheme('mannydark')
end,
```

---

<br>

## Key Mappings

You can set your own key mappings to use the available commands.
Add the following to your Neovim configuration (e.g., `init.lua` or your plugin config):

```lua
-- Toggle between theme styles (dark -> bright -> red-green -> dark):
vim.keymap.set("n", "<leader>ut", "<cmd>MannydarkToggle<cr>", { desc = "Toggle Mannydark theme style" })

-- Reload the colorscheme after changes:
vim.keymap.set("n", "<leader>ur", "<cmd>MannydarkReload<cr>", { desc = "Reload Mannydark colorscheme" })
```

### Available Commands

| Command            | Description                                       |
|--------------------|---------------------------------------------------|
| `:MannydarkToggle` | Toggle between dark, bright, and red-green style. |
| `:MannydarkReload` | Reload the colorscheme after changes.             |

