local theme = {}

theme.buildTheme = function()
  -- Editor:
  require('mannydark.highlighting.editor.buffer'             ).setupHighlighting()
  require('mannydark.highlighting.editor.environment'        ).setupHighlighting()
  require('mannydark.highlighting.editor.statusline'         ).setupHighlighting()
  -- Languages:
  require('mannydark.highlighting.languages.css'             ).setupHighlighting()
  require('mannydark.highlighting.languages.dockercompose'   ).setupHighlighting()
  require('mannydark.highlighting.languages.html'            ).setupHighlighting()
  require('mannydark.highlighting.languages.inifiles'        ).setupHighlighting()
  require('mannydark.highlighting.languages.javascript'      ).setupHighlighting()
  require('mannydark.highlighting.languages.json'            ).setupHighlighting()
  require('mannydark.highlighting.languages.languagedefaults').setupHighlighting()
  require('mannydark.highlighting.languages.laravelblade'    ).setupHighlighting()
  require('mannydark.highlighting.languages.lua'             ).setupHighlighting()
  require('mannydark.highlighting.languages.markdown'        ).setupHighlighting()
  require('mannydark.highlighting.languages.php'             ).setupHighlighting()
  require('mannydark.highlighting.languages.shell'           ).setupHighlighting()  -- Here is a bug (inside the file?!), but where?
  require('mannydark.highlighting.languages.xml'             ).setupHighlighting()
  require('mannydark.highlighting.languages.yml'             ).setupHighlighting()
  require('mannydark.highlighting.languages.zsh'             ).setupHighlighting()
  -- Plugins:
  require('mannydark.highlighting.plugins.copilot'           ).setupHighlighting()
  require('mannydark.highlighting.plugins.diffview'          ).setupHighlighting()
  require('mannydark.highlighting.plugins.gitsigns'          ).setupHighlighting()
  require('mannydark.highlighting.plugins.hop'               ).setupHighlighting()
  require('mannydark.highlighting.plugins.lsp'               ).setupHighlighting()
  require('mannydark.highlighting.plugins.lualine'           ).setupHighlighting()
  require('mannydark.highlighting.plugins.navic'             ).setupHighlighting()
  require('mannydark.highlighting.plugins.neogit'            ).setupHighlighting()
  --require('mannydark.highlighting.plugins.noice'           ).setupHighlighting()  -- Here is a bug (inside the file?!), but where?
  require('mannydark.highlighting.plugins.notify'            ).setupHighlighting()
  require('mannydark.highlighting.plugins.nvimcmp'           ).setupHighlighting()
  require('mannydark.highlighting.plugins.nvimtree'          ).setupHighlighting()
  require('mannydark.highlighting.plugins.packer'            ).setupHighlighting()
  require('mannydark.highlighting.plugins.telescope'         ).setupHighlighting()
  require('mannydark.highlighting.plugins.treesitter'        ).setupHighlighting()
  require('mannydark.highlighting.plugins.whichkey'          ).setupHighlighting()
  -- Todo:
  require('mannydark.highlighting.todos'                     ).setupHighlighting()
end

return theme

