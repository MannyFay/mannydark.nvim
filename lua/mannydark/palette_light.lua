-------------------------------------------------------------------------------
-- Mannydark Color Palette (Light Theme)
-------------------------------------------------------------------------------

local colors = {
  ------------------------------------------------------------------------------
  -- Base Colors (inverted for light theme)

  black      = '#FFFFFF',  -- Main background (white)
  white      = '#383A42',  -- Main foreground text (dark gray)

  ------------------------------------------------------------------------------
  -- Grays (adjusted for light background)

  gray       = '#A0A1A7',  -- Muted text, line numbers, inactive elements
  grayDark   = '#F0F0F0',  -- Secondary backgrounds (sidebars, panels)
  grayLight  = '#D3D3D3',  -- Borders, subtle dividers

  ------------------------------------------------------------------------------
  -- Syntax Highlighting (adjusted for readability on light background)

  -- Keywords, control flow (darker blue)
  blue       = '#4078F2',

  -- LSP references, subtle blue backgrounds
  blueLight  = '#D6E4FF',

  -- Links, references
  blueLink   = '#1A5FB4',

  -- Success states, git added
  green      = '#2E8B57',

  -- Numbers, numeric literals
  greenLight = '#50A14F',

  -- Functions, method names
  orange     = '#C18401',

  -- Warning backgrounds, subtle highlights
  orangeDark = '#FFF3CD',

  -- Special keywords, decorators
  pink       = '#D63384',

  -- Variables, parameters
  purple     = '#A626A4',

  -- Comments (mannydark signature red comments)
  red        = '#E60000',

  -- Error backgrounds, diff removed backgrounds
  redDark    = '#FFE5E5',

  -- Strings
  redLight   = '#986801',

  -- Types, classes, interfaces
  turquoise  = '#0997B3',

  -- Warnings, deprecated
  yellow     = '#B8860B',

  ------------------------------------------------------------------------------
  -- Additional UI Colors

  -- Cursor line, current line highlight
  cursorLine = '#F5F5F5',

  -- Selection background
  selection  = '#ADD6FF',

  -- Search highlight
  search     = '#FFECB3',

  -- Matching brackets
  matchParen = '#B4D7FF',
}

return colors
