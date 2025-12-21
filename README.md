# Mannydark

A quiet, accessible colorscheme for Neovim.

Available in **4 styles**:
- `dark` – Dark background
- `bright` – Light background
- `red-green-dark` – Colorblind-friendly (dark)
- `red-green-bright` – Colorblind-friendly (light)

Designed for long coding sessions, reducing eye strain with balanced palettes.

---

<br>

## Drive

Many colorschemes are beautiful but overstimulating for long coding sessions, making it hard to focus. Mannydark is designed to be easy on the eyes.

This colorscheme includes themes for people with red-green color blindness (Deuteranopia/Protanopia), available in both dark and light variants.

It also adds syntax highlighting for languages not supported by Neovim or Treesitter by default (e.g. Brainfuck).

Accessible for everybody, no matter what language you code in or how you see
colors!

---

<br>

## Requirements

- Neovim >= 0.8.0
- `termguicolors` enabled
- A terminal with true color support

---

<br>

## Installation

### Lazy.nvim

```lua
{
  'MannyFay/mannydark.nvim',
  lazy = false,
  priority = 1000,
  config = function()
    local colorscheme = require('mannydark')

    colorscheme.setup({ })

    vim.cmd.colorscheme('mannydark')
  end,
}
```

### Packer

```lua
use({
  'MannyFay/mannydark.nvim',
  config = function()
    local colorscheme = require('mannydark')

    colorscheme.setup({ })

    vim.cmd.colorscheme('mannydark')
  end,
})
```

---

<br>

## Commands

| Command                            | Action                              |
|------------------------------------|-------------------------------------|
| `:MannydarkToggle`                 | Cycle through all styles.           |
| `:MannydarkToggle dark`            | Switch to dark style.               |
| `:MannydarkToggle bright`          | Switch to bright style.             |
| `:MannydarkToggle red-green-dark`  | Switch to colorblind-friendly dark. |
| `:MannydarkToggle red-green-bright`| Switch to colorblind-friendly light.|
| `:MannydarkReload`                 | Reload the colorscheme.             |
| `:MannydarkDebug`                  | Show debug information.             |

---

<br>

## Lualine
The colorscheme includes a Lualine theme. To use it:

```lua
require('lualine').setup({
  options = {
    theme = 'mannydark',
  },
})
```

---

<br>

## Further Documentation
- [Customization](/docs/CUSTOMIZATION.md)
- [Supported Languages](/docs/SUPPORTED-LANGUAGES.md)
- [Supported Plugins](/docs/SUPPORTED-PLUGINS.md)
- [Development](/docs/DEVELOPMENT.md)
- [Troubleshooting](/docs/TROUBLESHOOTING.md)

