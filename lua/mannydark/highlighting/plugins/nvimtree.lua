-------------------------------------------------------------------------------
-- NvimTree Neovim Plugin
-------------------------------------------------------------------------------

local colors      = require("mannydark.palette")
local highlight   = vim.api.nvim_set_hl
local nvimTree    = {}


--------------------------------------------------------------
-- Settings

nvimTree.setupHighlighting = function()
  highlight(0, "NvimTreeRootFolder",                { link = "Label" }                            )
  highlight(0, "Directory",                         { link = "Label" }                            )
  highlight(0, "NvimTreeFolderIcon",                { link = "Label" }                            )
  highlight(0, "NvimTreeIndentMarker",              { link = "Ignore" }                           )  -- Vertical indent lines.
  highlight(0, "NvimTreeWinSeparator",              { fg = colors.grayDark, bg = colors.grayDark })
  highlight(0, "NvimTreeNormal",                    { link = "Operator" }                         )
  highlight(0, "NvimTreeFolderName",                { link = "Label" }                            )
  highlight(0, "NvimTreeOpenedFolderName",          { link = "Label" }                            )
  highlight(0, "NvimTreeEmptyFolderName",           { link = "Label" }                            )
  highlight(0, "NvimTreeEndOfBuffer",               { fg = colors.black, bg = "NONE" }            )
  highlight(0, "NvimTreeCursorLine",                { fg = "NONE", bg = colors.grayDark }         )  -- Current line.
  highlight(0, "NvimTreeImageFile",                 { link = "Operator" }                         )
  highlight(0, "NvimTreeGitIgnored",                { link = "Ignore" }                           )
  highlight(0, "NvimTreeGitDeleted",                { link = "Ignore" }                           )
  highlight(0, "NvimTreeGitRenamed",                { fg = colors.red, bg = "NONE" }              )
  highlight(0, "NvimTreeGitNew",                    { link = "DiagnosticOk" }                     )
  highlight(0, "NvimTreeGitDirty",                  { link = "Ignore" }                           )
  highlight(0, "NvimTreeGitStaged",                 { fg = colors.orange, bg = "NONE" }           )
  highlight(0, "NvimTreeGitMerge",                  { fg = colors.orange, bg = "NONE" }           )
  -- Legacy diagnostic groups (for older nvim-tree versions)
  highlight(0, "NvimTreeLspDiagnosticsError",       { link = "DiagnosticError" })
  highlight(0, "NvimTreeLspDiagnosticsWarning",     { link = "DiagnosticWarn" })
  highlight(0, "NvimTreeLspDiagnosticsInformation", { link = "DiagnosticInfo" })
  highlight(0, "NvimTreeLspDiagnosticsHint",        { link = "DiagnosticHint" })

  -- Current diagnostic groups (nvim-tree 1.0+)
  highlight(0, "NvimTreeDiagnosticErrorHL",         { link = "DiagnosticError" })
  highlight(0, "NvimTreeDiagnosticWarnHL",          { link = "DiagnosticWarn" })
  highlight(0, "NvimTreeDiagnosticInfoHL",          { link = "DiagnosticInfo" })
  highlight(0, "NvimTreeDiagnosticHintHL",          { link = "DiagnosticHint" })

  highlight(0, "NvimTreeDiagnosticErrorIcon",       { link = "DiagnosticError" })
  highlight(0, "NvimTreeDiagnosticWarnIcon",        { link = "DiagnosticWarn" })
  highlight(0, "NvimTreeDiagnosticInfoIcon",        { link = "DiagnosticInfo" })
  highlight(0, "NvimTreeDiagnosticHintIcon",        { link = "DiagnosticHint" })

  highlight(0, "NvimTreeDiagnosticErrorFolderHL",   { link = "DiagnosticError" })
  highlight(0, "NvimTreeDiagnosticWarnFolderHL",    { link = "DiagnosticWarn" })
  highlight(0, "NvimTreeDiagnosticInfoFolderHL",    { link = "DiagnosticInfo" })
  highlight(0, "NvimTreeDiagnosticHintFolderHL",    { link = "DiagnosticHint" })
  highlight(0, "NvimTreeFileIgnored",               { link = "Ignore" }                           )
  highlight(0, "NvimTreeFileDeleted",               { fg = colors.gray, bg = "NONE" }             )
  highlight(0, "nvimtreefilerenamed",               { fg = colors.red, bg = "NONE" }              )
  highlight(0, "NvimTreeFileNew",                   { fg = colors.red, bg = "NONE" }              )
  highlight(0, "NvimTreeFileDirty",                 { fg = colors.red, bg = "NONE" }              )
  highlight(0, "NvimTreeFileStaged",                { fg = colors.orange, bg = "NONE" }           )
  highlight(0, "NvimTreeFileMerge",                 { fg = colors.orange, bg = "NONE" }           )
  highlight(0, "NvimTreeModifiedFile",              { fg = colors.orange, bg = "NONE" }           )
  highlight(0, "NvimTreeOpenedFile",                { link = "Operator", underline = true }       )
  highlight(0, "NvimTreeNormalFloat",               { fg = colors.gray, bg = "NONE" }             )
  highlight(0, "NvimTreeExecFile",                  { link = "Number" }                           )  -- Executable files.
  highlight(0, "NvimTreeSpecialFile",               { link = "Operator" }                         )
  highlight(0, "NvimTreeClosedFolderIcon",          { link = "Label" }                            )  -- Icons of closed directories.
  highlight(0, "NvimTreeOpenedFolderIcon",          { link = "Label" }                            )  -- Icons of open directories.
  highlight(0, "NvimTreeFileIcon",                  { link = "Operator" }                         )  -- Icons not belong to WebDevIcons.
  highlight(0, "NvimTreeSymlinkIcon",               { link = "Function" }                         )  -- Icons of symlinks before filename.
  highlight(0, "NvimTreeSymlink",                   { fg = colors.white, bg = "NONE" }            )  -- Name and destination of symlinks.
  highlight(0, "NvimTreeSymlinkFolderName",         { link = "Label" }                            )  -- Name and destination of symlink directories.
  highlight(0, "NvimTreeLineNr",                    { link = "Ignore" }                           )  -- Line numbers.
  highlight(0, "NvimTreeCursorLineNr",              { link = "Operator" }                         )  -- Line number of the current line.

  -----------------------------------------------------------------------------
  -- Git status highlighting

  -- New:
  highlight(0, "NvimTreeGitNewIcon",     { link = "Comment" } )  -- Icon before new Git files.
  highlight(0, "NvimTreeGitFileNewHL",   { link = "Operator" })  -- Text label of new Git files.
  highlight(0, "NvimTreeGitFolderNewHL", { link = "Label" }   )  -- Text label of new Git directories.

  -- Dirty:
  highlight(0, "NvimTreeGitDirtyIcon",     { link = "Comment" } )  -- Icon before modified Git files.
  highlight(0, "NvimTreeGitFileDirtyHL",   { link = "Operator" })  -- Text label of modified Git files.
  highlight(0, "NvimTreeGitFolderDirtyHL", { link = "Label" }   )  -- Text label of modified Git directories.
  highlight(0, "NvimTreeGitRenamedIcon",   { link = "Comment" } )  -- Text label of renamed Git files.
  highlight(0, "NvimTreeGitFileRenamedHL", { link = "Operator" })  -- Text label of renamed Git files.

  -- Ignored:
  highlight(0, "NvimTreeGitIgnoredIcon",     { link = "Ignore" })  -- Icon before ignored Git files.
  highlight(0, "NvimTreeGitFolderIgnoredHL", { link = "Ignore" })  -- Text label of ignored Git files.




  ----------------------- Not used by now:
  highlight(0, "NvimTreeVertSplit",        { fg = colors.red, bg = colors.green }       )
  highlight(0, "NvimTreeWindowPicker",     { fg = colors.green, bg = colors.orange }    )
  highlight(0, "NvimTreeNormalFloat",      { fg = colors.turquoise, bg = colors.orange })
  highlight(0, "NvimTreeCursorColumn",     { fg = colors.orange, bg = colors.blue }     )
  highlight(0, "NvimTreeLiveFilterPrefix", { fg = colors.green, bg = colors.blue }      )
  highlight(0, "NvimTreeLiveFilterValue",  { fg = colors.turquoise, bg = colors.blue }  )
  highlight(0, "NvimTreeBookmark",         { fg = colors.orange, bg = colors.blue }     )
end

return nvimTree

