local c     = require('mannydark.palette')
local hl    = vim.api.nvim_set_hl
local theme = {}

-- Editor:
-- local environment      = require('mannydark.highlighting.editor.environment')
local buffer           = require('mannydark.highlighting.editor.buffer')
local statusLine       = require('mannydark.highlighting.editor.statusline')
-- Languages:
local css              = require('mannydark.highlighting.languages.css')
local javaScript       = require('mannydark.highlighting.javascript')
local languageDefaults = require('mannydark.highlighting.languages.languagedefaults')
local laravelBlade     = require('mannydark.highlighting.laravelblade')
local ini              = require('mannydark.highlighting.languages.inifiles')
-- local php              = require('mannydark.highlighting.languages.php').setupHighlighting()
local php              = require('mannydark.highlighting.languages.php')
local xml              = require('mannydark.highlighting.languages.xml')
local html             = require('mannydark.highlighting.languages.html')
local json             = require('mannydark.highlighting.languages.json')
local lua              = require('mannydark.highlighting.languages.lua')
local markdown        = require('mannydark.highlighting.languages.markdown')
-- Plugins:
local hopPlugin        = require('mannydark.highlighting.hopplugin')
local lualinePlugin    = require('mannydark.highlighting.lualineplugin')
local packer           = require('mannydark.highlighting.plugins.packer')
local nvimTree         = require('mannydark.highlighting.plugins.nvimtree')
local treesitter       = require('mannydark.highlighting.plugins.treesitter')
local gitSigns         = require('mannydark.highlighting.plugins.gitsigns')
local telescope        = require('mannydark.highlighting.plugins.telescope')

-- Todo:
  -- Typescript
  -- React (jsx)
  -- graphQL
  -- SQL

theme.buildTheme = function()
  require('mannydark.highlighting.editor.environment').setupHighlighting()
  -- Editor:
  -- environment.setupHighlighting()
  buffer.setupHighlighting()
  statusLine.setupHighlighting()
  -- Languages:
  css.setupHighlighting()
  javaScript.setupHighlighting()
  languageDefaults.setupHighlighting()
  laravelBlade.setupHighlighting()
  php.setupHighlighting()
  xml.setupHighlighting()
  html.setupHighlighting()
  json.setupHighlighting()
  lua.setupHighlighting()
  markdown.setupHighlighting()
  ini.setupHighlighting()
  -- Plugins:
  hopPlugin.setupHighlighting()
  lualinePlugin.setupHighlighting()
  packer.setupHighlighting()
  nvimTree.setupHighlighting()
  treesitter.setupHighlighting()
  gitSigns.setupHighlighting()
  telescope.setupHighlighting()




  -- Whichkey
  --hl(0, "WhichKey", { fg = c.purple, bg = 'NONE' })
  --hl(0, "WhichKeySeperator", { fg = c.green, bg = 'NONE' })
  --hl(0, "WhichKeyGroup", { fg = c.blue, bg = 'NONE' })
  --hl(0, "WhichKeyDesc", { fg = c.fg, bg = 'NONE' })
  --hl(0, "WhichKeyFloat", { fg = 'NONE', bg = c.alt_bg })



------------------------------------------------------------------------------
-- Language Server Protocol (LSP)
------------------------------------------------------------------------------

  hl(0, "DiagnosticInfo", { fg = c.blue, bg = 'NONE' })
  hl(0, "DiagnosticError", { fg = c.red, bg = 'NONE' })
  hl(0, "DiagnosticHint", { fg = c.blue, bg = 'NONE' })
  hl(0, "DiagnosticWarn", { fg = c.orange, bg = 'NONE' })
  hl(0, "DiagnosticVirtualTextHint", { fg = c.blue, bg = c.hint_bg })
  hl(0, "DiagnosticVirtualTextInfo", { fg = c.blue, bg = c.info_bg })
  hl(0, "DiagnosticVirtualTextWarn", { fg = c.orange, bg = c.warn_bg })
  hl(0, "DiagnosticVirtualTextError", { fg = c.red, bg = c.error_bg })
  hl(0, "LspDiagnosticsError", { fg = c.red, bg = 'NONE' })
  hl(0, "LspDiagnosticsWarning", { fg = c.orange, bg = 'NONE' })
  hl(0, "LspDiagnosticsInfo", { fg = c.blue, bg = 'NONE' })
  hl(0, "LspDiagnosticsHint", { fg = c.blue, bg = 'NONE' })
  hl(0, "LspCodeLens", { fg = c.purple, bg = 'NONE', italic = true, })
  hl(0, "LspCodeLensSeparator", { fg = c.purple, bg = 'NONE', italic = true, })
  hl(0, "DiagnosticUnderlineHint", { fg = 'NONE', bg = 'NONE', sp = c.blue, undercurl = true, })
  hl(0, "DiagnosticUnderlineInfo", { fg = 'NONE', bg = 'NONE', sp = c.blue, undercurl = true, })
  hl(0, "DiagnosticUnderlineWarn", { fg = 'NONE', bg = 'NONE', sp = c.orange, undercurl = true, })
  hl(0, "DiagnosticUnderlineError", { fg = 'NONE', bg = 'NONE', sp = c.red, undercurl = true, })

----------------

  hl(0, "DiagnosticOther", { fg = c.purple, bg = 'NONE' })
  hl(0, "DiagnosticSignHint", { link = 'DiagnosticHint' })
  hl(0, "DiagnosticSignInfo", { link = 'DiagnosticInfo' })
  hl(0, "DiagnosticSignWarn", { link = 'DiagnosticWarn' })
  hl(0, "DiagnosticSignError", { link = 'DiagnosticError' })
  hl(0, "DiagnosticSignOther", { link = 'DiagnosticOther' })
  hl(0, "DiagnosticSignWarning", { link = 'DiagnosticWarn' })
  hl(0, "DiagnosticFloatingHint", { link = 'DiagnosticHint' })
  hl(0, "DiagnosticFloatingInfo", { link = 'DiagnosticInfo' })
  hl(0, "DiagnosticFloatingWarn", { link = 'DiagnosticWarn' })
  hl(0, "DiagnosticFloatingError", { link = 'DiagnosticError' })
  hl(0, "DiagnosticSignInformation", { link = 'DiagnosticInfo' })
  hl(0, "LspDiagnosticsInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsDefaultError", { link = 'LspDiagnosticsError' })
  hl(0, "LspDiagnosticsDefaultWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "LspDiagnosticsDefaultInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsDefaultInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsDefaultHint", { link = 'LspDiagnosticsHint' })
  hl(0, "LspDiagnosticsVirtualTextError", { link = 'DiagnosticVirtualTextError' })
  hl(0, "LspDiagnosticsVirtualTextWarning", { link = 'DiagnosticVirtualTextWarn' })
  hl(0, "LspDiagnosticsVirtualTextInformation", { link = 'DiagnosticVirtualTextInfo' })
  hl(0, "LspDiagnosticsVirtualTextInfo", { link = 'DiagnosticVirtualTextInfo' })
  hl(0, "LspDiagnosticsVirtualTextHint", { link = 'DiagnosticVirtualTextHint' })
  hl(0, "LspDiagnosticsFloatingError", { link = 'LspDiagnosticsError' })
  hl(0, "LspDiagnosticsFloatingWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "LspDiagnosticsFloatingInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsFloatingInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsFloatingHint", { link = 'LspDiagnosticsHint' })
  hl(0, "LspDiagnosticsSignError", { link = 'LspDiagnosticsError' })
  hl(0, "LspDiagnosticsSignWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "LspDiagnosticsSignInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsSignInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsSignHint", { link = 'LspDiagnosticsHint' })
  hl(0, "NvimTreeLspDiagnosticsError", { link = 'LspDiagnosticsError' })
  hl(0, "NvimTreeLspDiagnosticsWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "NvimTreeLspDiagnosticsInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "NvimTreeLspDiagnosticsInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "NvimTreeLspDiagnosticsHint", { link = 'LspDiagnosticsHint' })
  hl(0, "LspDiagnosticsUnderlineError", { link = 'DiagnosticUnderlineError' })
  hl(0, "LspDiagnosticsUnderlineWarning", { link = 'DiagnosticUnderlineWarn' })
  hl(0, "LspDiagnosticsUnderlineInformation", { link = 'DiagnosticUnderlineInfo' })
  hl(0, "LspDiagnosticsUnderlineInfo", { link = 'DiagnosticUnderlineInfo' })
  hl(0, "LspDiagnosticsUnderlineHint", { link = 'DiagnosticUnderlineHint' })
  hl(0, "LspReferenceRead", { fg = 'NONE', bg = c.reference })
  hl(0, "LspReferenceText", { fg = 'NONE', bg = c.reference })
  hl(0, "LspReferenceWrite", { fg = 'NONE', bg = c.reference })
  hl(0, "IlluminatedWordRead", { link = 'LspReferenceRead' })
  hl(0, "IlluminatedWordText", { link = 'LspReferenceText' })
  hl(0, "IlluminatedWordWrite", { link = 'LspReferenceWrite' })



  -- Quickscope
  hl(0, "QuickScopePrimary", { fg = '#ff007c', bg = 'NONE', underline = true, })
  hl(0, "QuickScopeSecondary", { fg = '#00dfff', bg = 'NONE', underline = true, })


  -- Lir
  hl(0, "LirFloatNormal", { fg = c.fg, bg = c.alt_bg })
  hl(0, "LirDir", { link = 'Directory' })
  hl(0, "LirSymLink", { fg = c.cyan, bg = 'NONE' })
  hl(0, "LirEmptyDirText", { fg = c.gray, bg = 'NONE', italic = true, })

  -- IndentBlankline
  --hl(0, "IndentBlanklineContextChar", { fg = c.context, bg = 'NONE' })
  --hl(0, "IndentBlanklineContextStart", { fg = 'NONE', bg = 'NONE', underline = true, })
  --hl(0, "IndentBlanklineChar", { fg = c.dark_gray, bg = 'NONE' })

  -- Dashboard
  --hl(0, "DashboardHeader", { fg = c.blue, bg = 'NONE' })
  --hl(0, "DashboardCenter", { fg = c.purple, bg = 'NONE' })
  --hl(0, "DashboardFooter", { fg = c.cyan, bg = 'NONE' })

  -- DiffView
  hl(0, "DiffViewNormal", { fg = c.gray, bg = c.alt_bg })
  hl(0, "DiffviewStatusAdded", { fg = c.sign_add, bg = 'NONE' })
  hl(0, "DiffviewStatusModified", { fg = c.sign_change, bg = 'NONE' })
  hl(0, "DiffviewStatusRenamed", { fg = c.sign_change, bg = 'NONE' })
  hl(0, "DiffviewStatusDeleted", { fg = c.sign_delete, bg = 'NONE' })
  hl(0, "DiffviewFilePanelInsertion", { fg = c.sign_add, bg = 'NONE' })
  hl(0, "DiffviewFilePanelDeletion", { fg = c.sign_delete, bg = 'NONE' })
  hl(0, "DiffviewVertSplit", { fg = 'NONE', bg = c.bg })

  -- Bookmarks
  hl(0, "BookmarkSign", { fg = c.sign_change, bg = 'NONE' })
  hl(0, "BookmarkAnnotationSign", { fg = c.yellow, bg = 'NONE' })
  hl(0, "BookmarkLine", { fg = c.ui2_blue, bg = 'NONE' })
  hl(0, "BookmarkAnnotationLine", { fg = c.ui2_blue, bg = 'NONE' })

  -- Bqf
  hl(0, "BqfPreviewBorder", { fg = c.fg, bg = 'NONE' })
  hl(0, "BqfPreviewRange", { fg = 'NONE', bg = c.ui2_blue })
  hl(0, "BqfSign", { fg = c.ui_orange, bg = 'NONE' })



------------------------------------------------------------------------------
-- CMP
------------------------------------------------------------------------------

  hl(0, "CmpItemAbbrMatch", { fg = c.black, bg = c.gray })
  hl(0, "CmpItemAbbrDeprecated", { fg = c.gray, bg = 'NONE', strikethrough = true, })

---------------------

  hl(0, "CmpItemAbbrMatchFuzzy", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindFunction", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindMethod", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindConstructor", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindClass", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindEnum", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindEvent", { fg = c.info, bg = 'NONE' })
  hl(0, "CmpItemKindInterface", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindStruct", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindVariable", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindField", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindProperty", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindEnumMember", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindConstant", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindKeyword", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindModule", { fg = c.cyan, bg = 'NONE' })
  hl(0, "CmpItemKindValue", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindUnit", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindText", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindSnippet", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindFile", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindFolder", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindColor", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindReference", { fg = c.light_blue, bg = 'NONE' })
  hl(0, "CmpItemKindOperator", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindTypeParameter", { fg = c.blue, bg = 'NONE' })



------------------------------------------------------------------------------
-- Navic
------------------------------------------------------------------------------

  hl(0, "NavicIconsFile", { link = 'CmpItemKindFile' })
  hl(0, "NavicIconsModule", { link = 'CmpItemKindModule' })
  hl(0, "NavicIconsNamespace", { link = 'CmpItemKindModule' })
  hl(0, "NavicIconsPackage", { link = 'CmpItemKindModule' })
  hl(0, "NavicIconsClass", { link = 'CmpItemKindClass' })
  hl(0, "NavicIconsMethod", { link = 'CmpItemKindMethod' })
  hl(0, "NavicIconsProperty", { link = 'CmpItemKindProperty' })
  hl(0, "NavicIconsField", { link = 'CmpItemKindField' })
  hl(0, "NavicIconsConstructor", { link = 'CmpItemKindConstructor' })
  hl(0, "NavicIconsEnum", { link = 'CmpItemKindEnum' })
  hl(0, "NavicIconsInterface", { link = 'CmpItemKindInterface' })
  hl(0, "NavicIconsFunction", { link = 'CmpItemKindFunction' })
  hl(0, "NavicIconsVariable", { link = 'CmpItemKindVariable' })
  hl(0, "NavicIconsConstant", { link = 'CmpItemKindConstant' })
  hl(0, "NavicIconsString", { link = 'String' })
  hl(0, "NavicIconsNumber", { link = 'Number' })
  hl(0, "NavicIconsBoolean", { link = 'Boolean' })
  hl(0, "NavicIconsArray", { link = 'CmpItemKindClass' })
  hl(0, "NavicIconsObject", { link = 'CmpItemKindClass' })
  hl(0, "NavicIconsKey", { link = 'CmpItemKindKeyword' })
  hl(0, "NavicIconsKeyword", { link = 'CmpItemKindKeyword' })
  hl(0, "NavicIconsNull", { fg = c.blue, bg = 'NONE' })
  hl(0, "NavicIconsEnumMember", { link = 'CmpItemKindEnumMember' })
  hl(0, "NavicIconsStruct", { link = 'CmpItemKindStruct' })
  hl(0, "NavicIconsEvent", { link = 'CmpItemKindEvent' })
  hl(0, "NavicIconsOperator", { link = 'CmpItemKindOperator' })
  hl(0, "NavicIconsTypeParameter", { link = 'CmpItemKindTypeParameter' })
  hl(0, "NavicText", { fg = c.gray, bg = 'NONE' })
  hl(0, "NavicSeparator", { fg = c.context, bg = 'NONE' })



------------------------------------------------------------------------------
-- Notify
------------------------------------------------------------------------------

  hl(0, "NotifyERRORBorder", { fg = '#8A1F1F', bg = 'NONE' })
  hl(0, "NotifyWARNBorder", { fg = '#79491D', bg = 'NONE' })
  hl(0, "NotifyINFOBorder", { fg = c.ui_blue, bg = 'NONE' })
  hl(0, "NotifyDEBUGBorder", { fg = c.gray, bg = 'NONE' })
  hl(0, "NotifyTRACEBorder", { fg = '#4F3552', bg = 'NONE' })
  hl(0, "NotifyERRORIcon", { fg = c.error, bg = 'NONE' })
  hl(0, "NotifyWARNIcon", { fg = c.warn, bg = 'NONE' })
  hl(0, "NotifyINFOIcon", { fg = c.ui4_blue, bg = 'NONE' })
  hl(0, "NotifyDEBUGIcon", { fg = c.gray, bg = 'NONE' })
  hl(0, "NotifyTRACEIcon", { fg = c.ui_purple, bg = 'NONE' })
  --[[ hl(0, "NotifyERRORTitle", { fg = c.error, bg = 'NONE' })
  hl(0, "NotifyWARNTitle", { fg = c.warn, bg = 'NONE' })
  hl(0, "NotifyINFOTitle", { fg = c.ui4_blue, bg = 'NONE' })
  hl(0, "NotifyDEBUGTitle", { fg = c.gray, bg = 'NONE' })
  hl(0, "NotifyTRACETitle", { fg = c.ui_purple, bg = 'NONE' }) ]]




  -- SymbolOutline
  hl(0, "SymbolsOutlineConnector", { fg = c.gray, bg = 'NONE' })
  hl(0, "FocusedSymbol", { fg = 'NONE', bg = '#36383F' })

  -- hl(0, "NeogitFold", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitStash", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitDiffAdd", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitObjectId", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitRebasing", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitDiffDelete", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitRebaseDone", { fg = c.ui_purple, bg = 'NONE' })
  hl(0, "NeogitBranch", { fg = c.ui_purple, bg = 'NONE' })
  hl(0, "NeogitRemote", { fg = c.yellow_orange, bg = 'NONE' })
  hl(0, "NeogitStashes", { fg = c.ui_purple, bg = 'NONE' })
  hl(0, "NeogitUnmergedInto", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnpulledFrom", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitRecentcommits", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitStagedchanges", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUntrackedfiles", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnmergedchanges", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnpulledchanges", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnstagedchanges", { fg = c.blue, bg = 'NONE' })
  -- hl(0, "NoiceCmdline", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIcon", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconCmdline", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconFilter", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconHelp", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconIncRename", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconInput", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconLua", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconSearch", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopup", { fg = c.hint, bg = c.hint })
hl(0, "NoiceCmdlinePopupBorder", { fg = c.hint, bg = "NONE" })
  -- hl(0, "NoiceCmdlinePopupBorderCmdline", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderFilter", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderHelp", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderIncRename", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderInput", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderLua", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderSearch", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePrompt", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindClass", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindColor", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindConstant", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindConstructor", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindDefault", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindEnum", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindEnumMember ", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindField", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindFile", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindFolder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindFunction", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindInterface", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindKeyword", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindMethod", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindModule", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindProperty", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindSnippet", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindStruct", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindText", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindUnit", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindValue", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindVariable", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemMenu", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemWord", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceConfirm", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceConfirmBorder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCursor", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatConfirm", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatConfirmDefault", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatDate", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatEvent", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatKind", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelDebug", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelError", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelInfo", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelOff", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelTrace", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelWarn", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatProgressDone", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatProgressTodo", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatTitle", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceLspProgressClient", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceLspProgressSpinner", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceLspProgressTitle", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceMini", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopup", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupBorder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenu", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenuBorder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenuMatch", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenuSelected", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceScrollbar", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceScrollbarThumb", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceSplit", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceSplitBorder", { fg = c.hint, bg = c.hint })
  hl(0, "NoiceVirtualText", { fg = c.hint, bg = c.hint_bg })
  -- Noice

  -- TreesitterContext
  hl(0, "TreesitterContext", { fg = 'NONE', bg = c.alt_bg })

  -- Crates
  hl(0, "CratesNvimLoading", { fg = c.hint, bg = 'NONE' })
  hl(0, "CratesNvimVersion", { fg = c.hint, bg = 'NONE' })

  -- Misc
  hl(0, "diffAdded", { fg = c.sign_add, bg = 'NONE' })
  hl(0, "diffRemoved", { fg = c.sign_delete, bg = 'NONE' })
  hl(0, "diffFileId", { fg = c.blue, bg = 'NONE', bold = true, reverse = true, })
  hl(0, "diffFile", { fg = c.alt_bg, bg = 'NONE' })
  hl(0, "diffNewFile", { fg = c.green, bg = 'NONE' })
  hl(0, "diffOldFile", { fg = c.red, bg = 'NONE' })
  hl(0, "debugPc", { fg = 'NONE', bg = c.ui5_blue })
  hl(0, "debugBreakpoint", { fg = c.red, bg = 'NONE', reverse = true, })
  hl(0, "CodiVirtualText", { fg = c.hint, bg = 'NONE' })
  hl(0, "SniprunVirtualTextOk", { fg = c.hint, bg = 'NONE' })
  hl(0, "SniprunFloatingWinOk", { fg = c.hint, bg = 'NONE' })
  hl(0, "SniprunVirtualTextErr", { fg = c.error, bg = 'NONE' })
  hl(0, "SniprunFloatingWinErr", { fg = c.error, bg = 'NONE' })
  hl(0, "DapBreakpoint", { fg = c.error, bg = 'NONE' })

  -- Language

  -- hl(0, "yamlPlainScalar", { fg = c.orange, bg = 'NONE' })
  -- hl(0, "yamlTSField", { fg = c.blue, bg = 'NONE' })
  hl(0, "hclTSPunctSpecial", { fg = c.alt_fg, bg = 'NONE' })
  hl(0, "yamlBlockMappingKey", { fg = c.blue, bg = 'NONE' })
  hl(0, "tomlTSProperty", { fg = c.blue, bg = 'NONE' })
  hl(0, "zshKSHFunction", { link = "Function" })
  hl(0, "zshVariableDef", { link = "Constant" })





end

return theme
