-------------------------------------------------------------------------------
-- Mannydark Color Palette (Dark Theme)
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
  -- Syntax Highlighting

  -- Keywords, control flow
  blue       = "#569CD6",

  -- Links, references
  blueLink   = "#287BDE",

  -- Success states, git added
  green      = "#4FA544",

  -- Numbers, numeric literals
  greenLight = "#A5C25C",

  -- Functions, method names
  orange     = "#E8BF6A",

  -- Warning backgrounds, subtle highlights
  orangeDark = "#3E372A",

  -- Special keywords, decorators
  pink       = "#ED3276",

  -- Variables, parameters
  purple     = "#C064C7",

  -- Comments (mannydark signature red comments)
  red        = "#FF0000",

  -- Error backgrounds, diff removed backgrounds
  redDark    = "#553939",

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

