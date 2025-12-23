-------------------------------------------------------------------------------
-- Mannydark Color Palette (Red-Green Colorblind Friendly - Bright)
-- Optimized for Deuteranopia and Protanopia (red-green color blindness)
-- Light background variant for better color discrimination
-------------------------------------------------------------------------------

local colors = {
  ------------------------------------------------------------------------------
  -- Base Colors (light background for better color perception)

  black      = "#FEFEFE",  -- Main background (white)
  white      = "#404040",  -- Main foreground text (dark)

  ------------------------------------------------------------------------------
  -- Grays (adjusted for light background)

  gray       = "#A0A1A7",  -- Muted text, line numbers, inactive elements
  grayDark   = "#F0F0F0",  -- Secondary backgrounds (sidebars, panels)
  grayLight  = "#D3D3D3",  -- Borders, subtle dividers

  ------------------------------------------------------------------------------
  -- Syntax Highlighting (colorblind-friendly palette)
  -- Avoids red/green confusion by using blue/orange/yellow/purple
  -- Higher contrast colors for light background

  -- Keywords, control flow
  blue       = "#0550AE",

  -- LSP references, subtle blue backgrounds
  blueLight  = "#D6E4FF",

  -- Links, references
  blueLink   = "#1A5FB4",

  -- Success states, git added (blue instead of green)
  green      = "#0969DA",

  -- Numbers, numeric literals (darker yellow/gold for light bg)
  greenLight = "#9A6700",

  -- Functions, method names (darker orange for light bg)
  orange     = "#B35900",

  -- Warning backgrounds, subtle highlights
  orangeDark = "#FFF3CD",

  -- Special keywords, decorators (purple)
  pink       = "#7C3AED",

  -- Variables, parameters
  purple     = "#8250DF",

  -- Comments (burnt orange instead of red, darker for light bg)
  red        = "#BC4C00",

  -- Error backgrounds, diff removed backgrounds
  redDark    = "#FFE5E5",

  -- Strings (darker for light bg)
  redLight   = "#A04000",

  -- Types, classes, interfaces (darker cyan for light bg)
  turquoise  = "#0E7C6B",

  -- Warnings, deprecated
  yellow     = "#7C6400",

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
