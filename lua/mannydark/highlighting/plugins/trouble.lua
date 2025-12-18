-------------------------------------------------------------------------------
-- Trouble.nvim - Diagnostics, references, quickfix list
-- Supports both v2 (legacy) and v3 highlight groups
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local trouble   = {}


-------------------------------------------------------------------------------
-- Settings

trouble.setupHighlighting = function()
  -- Trouble v3 base groups
  highlight(0, 'TroubleNormal',        { fg = colors.white,     bg = 'NONE'           })  -- Normal text in trouble window
  highlight(0, 'TroubleNormalNC',      { fg = colors.white,     bg = 'NONE'           })  -- Normal text (inactive)
  highlight(0, 'TroubleText',          { fg = colors.white,     bg = 'NONE'           })  -- Default text
  highlight(0, 'TroublePreview',       { fg = 'NONE',           bg = colors.grayDark  })  -- Preview highlighting
  highlight(0, 'TroubleSource',        { fg = colors.gray,      bg = 'NONE'           })  -- Source of diagnostic
  highlight(0, 'TroubleCode',          { fg = colors.gray,      bg = 'NONE'           })  -- Diagnostic code
  highlight(0, 'TroublePos',           { fg = colors.gray,      bg = 'NONE'           })  -- Position (line:col)
  highlight(0, 'TroubleCount',         { fg = colors.purple,    bg = 'NONE', bold = true })  -- Item count
  highlight(0, 'TroubleFilename',      { fg = colors.turquoise, bg = 'NONE'           })  -- File name
  highlight(0, 'TroubleBasename',      { fg = colors.turquoise, bg = 'NONE'           })  -- Base name of file
  highlight(0, 'TroubleDirectory',     { fg = colors.blue,      bg = 'NONE'           })  -- Directory path
  highlight(0, 'TroubleIconDirectory', { fg = colors.blue,      bg = 'NONE'           })  -- Directory icon

  -- Trouble v3 indent guides
  highlight(0, 'TroubleIndent',        { fg = colors.grayDark,  bg = 'NONE'           })  -- Indent guides
  highlight(0, 'TroubleIndentMiddle',  { fg = colors.grayDark,  bg = 'NONE'           })  -- Middle indent
  highlight(0, 'TroubleIndentLast',    { fg = colors.grayDark,  bg = 'NONE'           })  -- Last indent
  highlight(0, 'TroubleIndentTop',     { fg = colors.grayDark,  bg = 'NONE'           })  -- Top indent
  highlight(0, 'TroubleIndentWs',      { fg = 'NONE',           bg = 'NONE'           })  -- Whitespace indent
  highlight(0, 'TroubleIndentFoldOpen',  { fg = colors.gray,    bg = 'NONE'           })  -- Open fold icon
  highlight(0, 'TroubleIndentFoldClosed', { fg = colors.gray,   bg = 'NONE'           })  -- Closed fold icon

  -- Diagnostic severity highlights
  highlight(0, 'TroubleError',         { fg = colors.red,       bg = 'NONE'           })  -- Error severity
  highlight(0, 'TroubleWarning',       { fg = colors.orange,    bg = 'NONE'           })  -- Warning severity
  highlight(0, 'TroubleHint',          { fg = colors.turquoise, bg = 'NONE'           })  -- Hint severity
  highlight(0, 'TroubleInformation',   { fg = colors.blue,      bg = 'NONE'           })  -- Info severity
  highlight(0, 'TroubleOther',         { fg = colors.purple,    bg = 'NONE'           })  -- Other severity

  -- Diagnostic text (message content)
  highlight(0, 'TroubleTextError',       { fg = colors.red,       bg = 'NONE'         })  -- Error message text
  highlight(0, 'TroubleTextWarning',     { fg = colors.orange,    bg = 'NONE'         })  -- Warning message text
  highlight(0, 'TroubleTextHint',        { fg = colors.turquoise, bg = 'NONE'         })  -- Hint message text
  highlight(0, 'TroubleTextInformation', { fg = colors.blue,      bg = 'NONE'         })  -- Info message text

  -- Diagnostic signs
  highlight(0, 'TroubleSignError',       { fg = colors.red,       bg = 'NONE'         })  -- Error sign
  highlight(0, 'TroubleSignWarning',     { fg = colors.orange,    bg = 'NONE'         })  -- Warning sign
  highlight(0, 'TroubleSignHint',        { fg = colors.turquoise, bg = 'NONE'         })  -- Hint sign
  highlight(0, 'TroubleSignInformation', { fg = colors.blue,      bg = 'NONE'         })  -- Info sign
  highlight(0, 'TroubleSignOther',       { fg = colors.purple,    bg = 'NONE'         })  -- Other sign

  -- Icons (linked to LSP/Treesitter kinds)
  highlight(0, 'TroubleIconArray',         { fg = colors.orange,    bg = 'NONE'       })  -- Array icon
  highlight(0, 'TroubleIconBoolean',       { fg = colors.blue,      bg = 'NONE'       })  -- Boolean icon
  highlight(0, 'TroubleIconClass',         { fg = colors.turquoise, bg = 'NONE'       })  -- Class icon
  highlight(0, 'TroubleIconConstant',      { fg = colors.orange,    bg = 'NONE'       })  -- Constant icon
  highlight(0, 'TroubleIconConstructor',   { fg = colors.turquoise, bg = 'NONE'       })  -- Constructor icon
  highlight(0, 'TroubleIconEnum',          { fg = colors.turquoise, bg = 'NONE'       })  -- Enum icon
  highlight(0, 'TroubleIconEnumMember',    { fg = colors.purple,    bg = 'NONE'       })  -- Enum member icon
  highlight(0, 'TroubleIconEvent',         { fg = colors.orange,    bg = 'NONE'       })  -- Event icon
  highlight(0, 'TroubleIconField',         { fg = colors.purple,    bg = 'NONE'       })  -- Field icon
  highlight(0, 'TroubleIconFile',          { fg = colors.white,     bg = 'NONE'       })  -- File icon
  highlight(0, 'TroubleIconFunction',      { fg = colors.orange,    bg = 'NONE'       })  -- Function icon
  highlight(0, 'TroubleIconInterface',     { fg = colors.turquoise, bg = 'NONE'       })  -- Interface icon
  highlight(0, 'TroubleIconKey',           { fg = colors.purple,    bg = 'NONE'       })  -- Key icon
  highlight(0, 'TroubleIconMethod',        { fg = colors.orange,    bg = 'NONE'       })  -- Method icon
  highlight(0, 'TroubleIconModule',        { fg = colors.turquoise, bg = 'NONE'       })  -- Module icon
  highlight(0, 'TroubleIconNamespace',     { fg = colors.turquoise, bg = 'NONE'       })  -- Namespace icon
  highlight(0, 'TroubleIconNull',          { fg = colors.blue,      bg = 'NONE'       })  -- Null icon
  highlight(0, 'TroubleIconNumber',        { fg = colors.greenLight, bg = 'NONE'      })  -- Number icon
  highlight(0, 'TroubleIconObject',        { fg = colors.turquoise, bg = 'NONE'       })  -- Object icon
  highlight(0, 'TroubleIconOperator',      { fg = colors.white,     bg = 'NONE'       })  -- Operator icon
  highlight(0, 'TroubleIconPackage',       { fg = colors.turquoise, bg = 'NONE'       })  -- Package icon
  highlight(0, 'TroubleIconProperty',      { fg = colors.purple,    bg = 'NONE'       })  -- Property icon
  highlight(0, 'TroubleIconString',        { fg = colors.redLight,  bg = 'NONE'       })  -- String icon
  highlight(0, 'TroubleIconStruct',        { fg = colors.turquoise, bg = 'NONE'       })  -- Struct icon
  highlight(0, 'TroubleIconTypeParameter', { fg = colors.turquoise, bg = 'NONE'       })  -- Type parameter icon
  highlight(0, 'TroubleIconVariable',      { fg = colors.purple,    bg = 'NONE'       })  -- Variable icon

  -- Trouble v2 legacy groups (for backwards compatibility)
  highlight(0, 'TroubleFile',          { fg = colors.turquoise, bg = 'NONE'           })  -- File header
  highlight(0, 'TroubleFoldIcon',      { fg = colors.gray,      bg = 'NONE'           })  -- Fold icon
  highlight(0, 'TroubleLocation',      { fg = colors.gray,      bg = 'NONE'           })  -- Location info
  highlight(0, 'TroubleIndentAlt',     { fg = colors.grayDark,  bg = 'NONE'           })  -- Alternative indent
end

return trouble
