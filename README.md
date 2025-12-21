# Mannydark

Dark Quiet Theme
(can be bright too...)
for Neovim!

This colorscheme is designed for long coding sessions, reducing eye strain with a balanced dark palette.

---

<br>

## Drive
Lots of colorschemes are very beautiful, but too overstimulating for long coding
sessions (too me) what makes it hard to focus. Mannydark is designed to be easy on the eyes.

I will try to create a dark and a bright theme for people who have vision
problems such as color blindness or red-green deficiency.

Also I want to try to add syntax highlighting for languages that are not supported by Neovim or Treesitter
by default (e.g. Brainfuck).

It has to be accessible for everybody, no matter what kind of language people code in!

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

| Command                      | Action                    |
|------------------------------|---------------------------|
| `:MannydarkReload`           | Reload Scheme.            |
| `:MannydarkToggle red-green` | Change style (red-green). |
| `:MannydarkToggle bright`    | Change style (bright).    |
| `:MannydarkToggle dark`      | Change style (dark).      |
| `:MannydarkDebug`            | Show debug information.   |

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

