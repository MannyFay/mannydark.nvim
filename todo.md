⏺ Jetzt habe ich eine umfassende Analyse durchgeführt. Hier ist mein detaillierter Bericht:

  ---
  Analyse: mannydark.nvim Colorscheme

  Übersicht

  Das Colorscheme ist funktional und gut strukturiert, aber im Vergleich zu modernen High-Quality-Colorschemes wie https://github.com/catppuccin/nvim (~7k Stars) oder https://github.com/folke
/tokyonight.nvim (~7.7k Stars) gibt es erhebliches Verbesserungspotenzial.

  ---
  1. KRITISCHE BUGS & PROBLEME

  1.1 Fehlerhafte Highlight-Definitionen ("Not used by now" Sections)

  Viele Dateien enthalten Highlights mit unsinnigen Farbkombinationen, die wahrscheinlich als Marker dienen sollten:

  -- treesitter.lua:221-259
  highlight(0, '@define', { fg = colors.green, bg = colors.red })
  highlight(0, '@string.regex', { fg = colors.blue, bg = colors.red })
  highlight(0, '@lsp.type.decorator', { fg = colors.blue, bg = colors.green })

  Diese grellen Kombinationen (grün auf rot, blau auf grün) sind visuell störend und sollten entweder:
  - Korrekt definiert werden (ohne Hintergrund, bg = 'NONE')
  - Entfernt oder auskommentiert werden

  Betroffene Dateien:
  - treesitter.lua:220-259
  - lsp.lua:67-106
  - environment.lua:56-73
  - todos.lua:22-84
  - noice.lua:14-89 (alle mit { fg = colors.blue, bg = colors.purple })

  1.2 Undefinierte Farbe

  -- languagedefaults.lua:45
  highlight(0, 'StorageClass', { fg = colors.cyan, bg = 'NONE' })

  colors.cyan existiert nicht in palette.lua - dies verursacht einen Fehler!

  1.3 Typo in todos.lua

  -- todos.lua:26-29
  highlight(0, 'LirFloatNormal',  { fg = colors.black, bg = colors.whithe })
  --                                                             ^^^^^^^ typo

  colors.whithe existiert nicht (sollte colors.white sein).

  1.4 Lualine Replace-Mode leer

  -- lualine/themes/mannydark.lua:44-49
  replace = {
    a = {  },  -- Komplett leer!
    b = {  },
    ...
  }

  ---
  2. ARCHITEKTUR-VERBESSERUNGEN

  2.1 Fehlende Konfigurationsmöglichkeiten

  Aktuell: setup({}) akzeptiert keine Optionen.

  Best Practice (Catppuccin/TokyoNight):
  require('mannydark').setup({
    transparent = false,
    italic_comments = true,
    dim_inactive = false,
    integrations = {
      telescope = true,
      nvim_cmp = true,
      -- ...
    }
  })

  2.2 Entry Point modernisieren

  Aktuell: colors/mannydark.vim (VimScript wrapper)
  lua << EOF
  local mannydark = require('mannydark')
  mannydark.setup({})
  EOF

  Modern (Pure Lua): colors/mannydark.lua
  require('mannydark').load()

  2.3 Fehlende Lazy-Loading Integration

  Die Highlights werden alle sofort geladen. Moderne Colorschemes unterstützen:
  - Compiled themes (Catppuccin) für ~1ms Startup
  - Lazy loading für Plugin-Highlights

  2.4 Duplizierte Farbdefinitionen

  -- palette.lua: Zentrale Farben
  local colors = { blue = '#569CD6', ... }

  -- lualine/themes/mannydark.lua: Eigene Kopie!
  local colors = { blue = '#569CD6', ... }

  Lösung: Lualine-Theme sollte require('mannydark.palette') verwenden.

  ---
  3. FEHLENDE MODERNE FEATURES

  3.1 Keine Terminal-Farben

  Moderne Colorschemes setzen vim.g.terminal_color_0 bis vim.g.terminal_color_15.

  3.2 Fehlende Treesitter Captures (Neovim 0.9+)

  Die neue nvim-treesitter verwendet hierarchische Namen. Fehlend:
  -- Neu in Neovim 0.9+
  '@variable.parameter'     -- statt @parameter
  '@function.method'        -- statt @method
  '@keyword.conditional'    -- statt @conditional
  '@keyword.repeat'         -- statt @repeat
  '@markup.heading'         -- für Markdown
  '@markup.list'
  '@markup.quote'

  3.3 Fehlende Plugin-Integrationen

  Nicht unterstützt (aber sehr populär 2025):
  - blink.cmp (moderner nvim-cmp Ersatz)
  - mini.nvim (Suite von Plugins)
  - flash.nvim (moderner hop.nvim Ersatz)
  - neo-tree.nvim (moderner nvim-tree Ersatz)
  - trouble.nvim (Diagnostics UI)
  - indent-blankline.nvim v3 (neue Highlight-Namen)
  - render-markdown.nvim
  - oil.nvim
  - snacks.nvim

  3.4 Fehlende Semantic Token Unterstützung

  LSP Semantic Tokens sind nur teilweise implementiert. Fehlend:
  '@lsp.typemod.variable.readonly'
  '@lsp.typemod.function.async'
  '@lsp.mod.deprecated'
  '@lsp.mod.defaultLibrary'

  ---
  4. CODE-QUALITÄT

  4.1 Redundante Legacy-Gruppen

  lsp.lua definiert viele veraltete Gruppen die seit Neovim 0.6+ nicht mehr verwendet werden:
  -- Veraltet:
  'LspDiagnosticsSignError'
  'LspDiagnosticsError'
  'LspDiagnosticsDefaultError'

  -- Modern:
  'DiagnosticSignError'
  'DiagnosticError'

  4.2 Ineffiziente Highlight-Aufrufe

  -- Aktuell: Viele einzelne Calls
  highlight(0, '@string', { link = 'String' })
  highlight(0, '@number', { link = 'Number' })
  highlight(0, '@float', { link = 'Float' })

  Besser: Loop über Table
  local links = {
    ['@string'] = 'String',
    ['@number'] = 'Number',
    ['@float'] = 'Float',
  }
  for group, link in pairs(links) do
    highlight(0, group, { link = link })
  end

  4.3 Fehlende Dokumentation

  - Kein doc/mannydark.txt (Vim Help)
  - README fehlt: Lazy.nvim Installation, Screenshots, Feature-Liste
  - Keine CHANGELOG.md

  ---
  5. PALETTE-ANALYSE

  5.1 Kontrast-Probleme

  white = '#ABABAB'  -- Zu dunkel für "white" (Kontrastverhältnis gegen black ~5:1)
  gray = '#606366'   -- Kommentare könnten schwer lesbar sein

  5.2 Fehlende Farben für moderne UIs

  -- Empfohlen hinzuzufügen:
  cyan       = '#45C8B0',  -- Für TypeParameter etc.
  greenDark  = '#2E4A3E',  -- Für Diff-Hintergründe
  blueDark   = '#2A3A4A',  -- Für Info-Hintergründe

  ---
  6. KONKRETE EMPFEHLUNGEN (Priorisiert)

  HOCH (Bugs/Funktionalität)

  1. Undefinierte colors.cyan fixen in languagedefaults.lua:45
  2. Typo colors.whithe fixen in todos.lua
  3. Lualine replace-mode füllen mit Farben
  4. "Not used by now" Sections bereinigen - entweder korrekt definieren oder entfernen
  5. Noice.lua aktivieren und fixen - aktuell alle Gruppen mit unsinnigen Farben

  MITTEL (Modernisierung)

  6. Terminal-Farben hinzufügen
  7. Neue Treesitter Captures unterstützen (@variable.parameter, etc.)
  8. Lualine Theme: Palette importieren statt duplizieren
  9. Entry Point auf Lua umstellen (colors/mannydark.lua)
  10. Setup-Optionen implementieren (transparent, italic_comments, etc.)

  NIEDRIG (Nice-to-have)

  11. Mehr Plugin-Integrationen (flash.nvim, trouble.nvim, etc.)
  12. Performance: Compiled themes
  13. Vim Help-Dokumentation
  14. Extras für andere Tools (Kitty, Alacritty, etc.)
  15. Light-Theme Variante

  ---
  7. BEISPIEL: Modernisierte init.lua

  local M = {}

  M.config = {
    transparent = false,
    italic_comments = true,
    dim_inactive = false,
  }

  M.setup = function(opts)
    M.config = vim.tbl_deep_extend('force', M.config, opts or {})
  end

  M.load = function()
    if vim.g.colors_name then
      vim.cmd('hi clear')
    end

    vim.o.termguicolors = true
    vim.o.background = 'dark'
    vim.g.colors_name = 'mannydark'

    local theme = require('mannydark.theme')
    theme.buildTheme(M.config)

    -- Terminal colors
    local colors = require('mannydark.palette')
    vim.g.terminal_color_0 = colors.black
    vim.g.terminal_color_1 = colors.red
    -- ... etc
  end

  return M

  ---
  Quellen:
  - https://github.com/catppuccin/nvim
  - https://github.com/folke/tokyonight.nvim
  - https://neovim.io/doc/user/treesitter.html
  - https://github.com/nvim-treesitter/nvim-treesitter/discussions/1633
  - https://dotfyle.com/neovim/colorscheme/top

  Soll ich mit den Korrekturen beginnen?
