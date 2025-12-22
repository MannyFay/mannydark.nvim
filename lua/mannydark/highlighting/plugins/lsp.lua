-------------------------------------------------------------------------------
-- LSP (Language Server Protocol) Neovim Plugin
-- Note: Core diagnostic highlights (DiagnosticError, DiagnosticVirtualText*, etc.)
-- are defined in languagedefaults.lua to avoid redundancy.
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local lsp       = {}


-------------------------------------------------------------------------------
-- Settings

lsp.setupHighlighting = function()

  -- CodeLens (inline hints from LSP):
  highlight(0, "LspCodeLens",          { fg = colors.gray, bg = "NONE", italic = true })
  highlight(0, "LspCodeLensSeparator", { fg = colors.gray, bg = "NONE" })

  -- vim-illuminate (highlights references of word under cursor):
  highlight(0, "IlluminatedWordRead",  { fg = "NONE", bg = colors.grayDark })
  highlight(0, "IlluminatedWordText",  { fg = "NONE", bg = colors.grayDark })
  highlight(0, "IlluminatedWordWrite", { fg = "NONE", bg = colors.grayDark })

  -- LSP References (built-in reference highlighting):
  highlight(0, "LspReferenceRead",  { fg = "NONE", bg = colors.grayDark })
  highlight(0, "LspReferenceText",  { fg = "NONE", bg = colors.grayDark })
  highlight(0, "LspReferenceWrite", { fg = "NONE", bg = colors.grayDark })

  -- LSP Signature Help:
  highlight(0, "LspSignatureActiveParameter", { fg = colors.orange, bg = "NONE", bold = true })

  -- LspInfo window:
  highlight(0, "LspInfoBorder",   { fg = colors.gray,  bg = "NONE" })
  highlight(0, "LspInfoFiletype", { fg = colors.blue,  bg = "NONE" })
  highlight(0, "LspInfoTitle",    { fg = colors.white, bg = "NONE", bold = true })
  highlight(0, "LspInfoTip",      { fg = colors.gray,  bg = "NONE", italic = true })
  highlight(0, "LspInfoList",     { fg = colors.white, bg = "NONE" })

  -- Inlay Hints (Neovim 0.10+):
  highlight(0, "LspInlayHint", { fg = colors.gray, bg = "NONE", italic = true })

end

return lsp
