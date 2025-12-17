# Mannydark

A dark colorscheme for Neovim written in Lua.

## Features

- Dark theme optimized for long coding sessions
- Treesitter support with modern capture names (Neovim 0.9+)
- LSP semantic token highlighting
- Terminal colors for embedded terminal
- Lualine theme included
- Configurable options (transparent background, italic comments, dim inactive windows)

## Supported Plugins

- Treesitter
- LSP Diagnostics
- Telescope
- nvim-tree
- nvim-cmp
- Gitsigns
- Neogit
- Fugitive
- Lualine
- Mason
- Lazy.nvim
- Which-key
- Hop
- Diffview
- nvim-notify
- Copilot

## Requirements

- Neovim >= 0.8.0
- `termguicolors` enabled
- A terminal with true color support

## Installation

### Lazy.nvim

```lua
{
  'MannyFay/mannydark.nvim',
  lazy = false,
  priority = 1000,
  config = function()
    require('mannydark').setup({})
    vim.cmd.colorscheme('mannydark')
  end,
}
```

### Packer

```lua
use({
  'MannyFay/mannydark.nvim',
  config = function()
    require('mannydark').setup({})
    vim.cmd.colorscheme('mannydark')
  end,
})
```

## Configuration

The setup function accepts the following options:

```lua
require('mannydark').setup({
  transparent = false,     -- Enable transparent background.
  italic_comments = true,  -- Render comments in italic.
  dim_inactive = false,    -- Dim inactive windows.
})
```

### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `transparent` | boolean | `false` | Removes background color for transparent terminals. |
| `italic_comments` | boolean | `true` | Displays comments in italic style. |
| `dim_inactive` | boolean | `false` | Dims inactive split windows. |

## Lualine

The colorscheme includes a Lualine theme. To use it:

```lua
require('lualine').setup({
  options = {
    theme = 'mannydark',
  },
})
```

## Customization

### Changing Colors

Edit the color palette in `lua/mannydark/palette.lua`:

```lua
local colors = {
  black      = '#191B1C',
  blue       = '#569CD6',
  -- ...
}
```

### Finding Highlight Groups

To find the highlight group under your cursor, use the `:Inspect` command (Neovim 0.9+).

For older versions:

```vim
:echo synIDattr(synID(line("."), col("."), 1), "name")
```

### File Structure

```
mannydark.nvim/
├── colors/
│   └── mannydark.vim           -- Entry point
├── lua/
│   ├── mannydark/
│   │   ├── init.lua            -- Setup and configuration
│   │   ├── palette.lua         -- Color definitions
│   │   ├── theme.lua           -- Theme builder
│   │   └── highlighting/
│   │       ├── editor/         -- Editor UI highlights
│   │       ├── languages/      -- Language-specific highlights
│   │       └── plugins/        -- Plugin highlights
│   └── lualine/
│       └── themes/
│           └── mannydark.lua   -- Lualine theme
└── README.md
```

## License

MIT License. See [LICENSE](LICENSE) for details.
