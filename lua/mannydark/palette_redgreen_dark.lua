-------------------------------------------------------------------------------
-- Mannydark Color Palette (Red-Green Colorblind Friendly - Dark)
-- Optimized for Deuteranopia and Protanopia (red-green color blindness)
-------------------------------------------------------------------------------

local colors = {
  ------------------------------------------------------------------------------
  -- Base Colors

  black      = "#191B1C",  -- Main background
  white      = "#ABABAB",  -- Main foreground text

  ------------------------------------------------------------------------------
  -- Grays

  gray       = "#606366",  -- Muted text, line numbers, inactive elements
  grayDark   = "#252525",  -- Secondary backgrounds (sidebars, panels)
  grayLight  = "#404040",  -- Borders, subtle dividers

  ------------------------------------------------------------------------------
  -- Syntax Highlighting (colorblind-friendly palette)
  -- Avoids red/green confusion by using blue/orange/yellow/purple

  -- Keywords, control flow
  blue       = "#569CD6",

  -- LSP references, subtle blue backgrounds
  blueLight  = "#2D3E50",

  -- Links, references
  blueLink   = "#287BDE",

  -- Success states, git added (blue instead of green)
  green      = "#3498DB",

  -- Numbers, numeric literals (yellow instead of green)
  greenLight = "#F1C40F",

  -- Functions, method names
  orange     = "#E8BF6A",

  -- Warning backgrounds, subtle highlights
  orangeDark = "#3E372A",

  -- Special keywords, decorators (magenta instead of pink)
  pink       = "#9B59B6",

  -- Variables, parameters
  purple     = "#C064C7",

  -- Comments (orange instead of red for better visibility)
  red        = "#E67E22",

  -- Error backgrounds, diff removed backgrounds
  redDark    = "#4A3728",

  -- Strings
  redLight   = "#CE9178",

  -- Types, classes, interfaces
  turquoise  = "#45C8B0",

  -- Warnings, deprecated
  yellow     = "#DCDCAA",

  ------------------------------------------------------------------------------
  -- Additional UI Colors

  -- Cursor line, current line highlight
  cursorLine = "#252525",

  -- Selection background
  selection  = "#264F78",

  -- Search highlight
  search     = "#613315",

  -- Matching brackets
  matchParen = "#0D3A58",
}

return colors
