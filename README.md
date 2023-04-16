# Mannydark Neovim Color Scheme
A very dark color scheme.

## Installation
### macOS
Open your plugin file and paste the plugin.
Packer:
```lua
use "MannyFay/mannydark.nvim"
```
After that, you can run the following commands in your Neovim command line to
be up to date. Enter the command line from normal mode with `:`.
Sync packages:
```shell
PackerSync
```
Update packages:
```shell
PackerUpdate
```
You will find the repository on your machine in:
```shell
~/.local/share/nvim/site/pack/packer/start/mannydark.nvim
```

## Activate Color Scheme
To activate the color scheme, put this in your `init.lua` (or another
configuration file of Neovim):
```lua
vim.cmd("colorscheme mannydark")
```

## Change Color Scheme
If there are colors you would like to change, go into visual mode and mark the charakter or the word with the color you like to change. Then press `:`
to switch into the Neovim command line and enter:
```shell
echo synIDattr(synID(line("."), col("."), 1), "name")
```
The color variable will be displayed in the command line.

The colors itself you can change in `/lua/mannydark/palette.lua`.
If you want to change the color of a part in your document, enter
`/lua/mannydark/theme.lua`.
