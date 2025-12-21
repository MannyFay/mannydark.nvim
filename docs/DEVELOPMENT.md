# Development

This guide explains how to set up your Neovim configuration for developing and testing the Mannydark colorscheme locally.

---

<br>

## Prerequisites

- Neovim 0.9+
- [lazy.nvim](https://github.com/folke/lazy.nvim) as plugin manager
- Git

---

<br>

## Setup Local Development

### 1. Clone the Repository

Clone the repository to your local development path:

```bash
cd ~/personal/github  # Or your preferred development directory
git clone https://github.com/MannyFay/mannydark.nvim.git
```

### 2. Configure lazy.nvim

In your lazy.nvim setup (e.g., `~/.config/nvim/lua/core/lazy.lua`), add the `dev` configuration:

```lua
require("lazy").setup({
  -- ... your plugins ...
}, {
  -- Enable local plugin development:
  dev = {
    path = "~/personal/github",  -- Path where your local plugins are stored.
    patterns = { "MannyFay" },   -- Load all MannyFay/* plugins from local path.
    fallback = true,             -- Fallback to Git if local plugin doesn't exist.
  },
})
```

### 3. Configure the Colorscheme Plugin

In your colorscheme plugin config (e.g., `~/.config/nvim/lua/plugins/colorscheme.lua`):

```lua
-- Enable debug mode for automatic hot reload:
vim.g.mannydark_debug = true

return {
  "MannyFay/mannydark.nvim",
  dev      = true,   -- Load from local dev.path instead of Git remote.
  lazy     = false,
  priority = 1000,
  config = function()
    require('mannydark').setup({
      style = 'dark',
      -- ... your configuration ...
    })
  end,
}
```

---

<br>

## Development Keybindings

Add these keybindings to quickly reload and test your changes:

```lua
-- Reload the colorscheme after changes:
vim.keymap.set("n", "<leader>ur", "<cmd>MannydarkReload<cr>", { desc = "Reload Mannydark colorscheme" })

-- Toggle between theme styles (dark -> bright -> red-green -> dark):
vim.keymap.set("n", "<leader>ut", "<cmd>MannydarkToggle<cr>", { desc = "Toggle Mannydark theme style" })

-- Toggle dev mode (auto-reload on save):
vim.keymap.set("n", "<leader>ud", function()
  local mannydark = require("mannydark")
  if mannydark.is_dev_mode() then
    mannydark.dev_mode_off()
    vim.notify("Mannydark: Dev mode OFF", vim.log.levels.INFO)
  else
    mannydark.dev_mode()
    vim.notify("Mannydark: Dev mode ON", vim.log.levels.INFO)
  end
end, { desc = "Toggle Mannydark dev mode" })
```

---

<br>

## Available Development Commands

| Command            | Description                                       |
|--------------------|---------------------------------------------------|
| `:MannydarkReload` | Reload the colorscheme after changes.             |
| `:MannydarkToggle` | Toggle between dark, bright, and red-green style. |

---

<br>

## Development Workflow

1. **Edit** the colorscheme files in `lua/mannydark/`
2. **Reload** with `<leader>ur` or `:MannydarkReload`
3. **Test** different styles with `<leader>ut` or `:MannydarkToggle`

### Auto-Reload (Dev Mode)

When `vim.g.mannydark_debug = true` is set, changes to the colorscheme files will automatically reload when you save them.

You can also toggle dev mode at runtime with `<leader>ud`.

---

<br>

## Switching Back to Production

To switch back to the Git remote version:

1. In your colorscheme config, change `dev = true` to `dev = false` (or remove the line)
2. Optionally remove `vim.g.mannydark_debug = true`
3. Restart Neovim
