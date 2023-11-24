# Mannydark Neovim Color Scheme
A very dark color scheme - by default :)  
This is a highly customizable color scheme for Neovim written in Lua.
So is anything annoying for your eyes, you can change it very easy.

This project has a MIT license so please feel free to use it wherever you want and change it as you like.  
Fork it, contribute to it or donate to it - everything is possible!

---
<br>

# Installation
## macOS
Open your Neovim plugin file and paste the plugin.  
Packer:
```lua
-- Set color scheme:
use({
  'MannyFay/mannydark.nvim',
  vim.cmd [[
    try
      colorscheme mannydark
    catch /^Vim\%((\a\+)\)\=:E185/
      colorscheme default
      set background=dark
    endtry
  ]],
})
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

---
<br>

# Change Color Scheme
If there are colors you would like to change, go into visual mode and select the character or the word with the color you like to change, then press `:`.  
In command line the Neovim range indicator `'<'>` will be displayed by default.  
Delete it and enter the following command after `:`:
```shell
echo synIDattr(synID(line("."), col("."), 1), "name")
```
The color variable will be displayed in the command line.

Do your changes it `~/.local/share/nvim/site/pack/packer/start/mannydark.nvim`.  
Save your changes and restart Neovim. You should see them immediately after open the specific file again.  
The colors itself you can change in `/lua/mannydark/palette.lua`.  
If you want to change the color of a part in your document, enter `/lua/mannydark/theme.lua` file. There you will see a list of all highlighting files and their places.

After your changes, save the whole `mannydark.nvim` directory somewhere else on your machine, because if you do `PackerUpdate` or `PackerSync` your changes will be lost in `~/.local/share/nvim/site/pack/packer/start/mannydark.nvim`. Now you can set your scheme.  
I recommend to put it into a GitHb remote repository, so it never gets lost.

---
<br>

Have fun and a great time ;)
