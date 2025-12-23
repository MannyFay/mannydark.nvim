-------------------------------------------------------------------------------
-- Mannydark Color Palette (Bright Theme)
-------------------------------------------------------------------------------

local colors = {
  ------------------------------------------------------------------------------
  -- Base Colors (inverted for bright theme)

  black      = "#FEFEFE",  -- Main background (white)
  white      = "#404040",  -- Main foreground text (dark gray)

  ------------------------------------------------------------------------------
  -- Grays (adjusted for light background)

  gray       = "#A0A1A7",  -- Muted text, line numbers, inactive elements
  grayDark   = "#F0F0F0",  -- Secondary backgrounds (sidebars, panels)
  grayLight  = "#D3D3D3",  -- Borders, subtle dividers

  ------------------------------------------------------------------------------
  -- Syntax Highlighting (adjusted for readability on light background)

  -- Keywords, control flow (darker blue)
  blue       = "#569CD6",

  -- Links, references
  blueLink   = "#1A5FB4",

  -- Success states, git added
  green      = "#2E8B57",

  -- Numbers, numeric literals
  -- greenLight = "#50A14F",
  greenLight = "#A5C25C",

  -- Functions, method names
  orange     = "#E8BF6A",

  -- Warning backgrounds, subtle highlights
  orangeDark = "#FFF3CD",

  -- Special keywords, decorators
  pink       = "#D63384",

  -- Variables, parameters
  purple     = "#C064C7",

  -- Comments (mannydark signature red comments)
  red        = "#FF0000",

  -- Error backgrounds, diff removed backgrounds
  redDark    = "#FFE5E5",

  -- Strings
  redLight   = "#CE9178",

  -- Types, classes, interfaces
  turquoise  = "#0997B3",

  -- Warnings, deprecated
  yellow     = "#B8860B",

  ------------------------------------------------------------------------------
  -- Additional UI Colors

  -- Cursor line, current line highlight
  cursorLine = "#F5F5F5",

  -- Selection background
  selection  = "#ADD6FF",

  -- Search highlight
  search     = "#FFECB3",

  -- Matching brackets
  matchParen = "#B4D7FF",
}

return colors
