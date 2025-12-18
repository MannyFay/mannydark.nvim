-------------------------------------------------------------------------------
-- Mannybright Color Palette (Light Theme)
-- Companion to mannydark - same semantic colors, optimized for light backgrounds
-------------------------------------------------------------------------------

local colors = {
  ------------------------------------------------------------------------------
  -- Base Colors (inverted from dark theme)

  black      = '#FAFAFA',  -- Main background (light)
  white      = '#333333',  -- Main foreground text (dark)

  ------------------------------------------------------------------------------
  -- Grays (adjusted for light theme)

  gray       = '#6E7781',  -- Muted text, line numbers, inactive elements
  grayDark   = '#F0F0F0',  -- Secondary backgrounds (sidebars, panels)
  grayLight  = '#E1E4E8',  -- Borders, subtle dividers (NEW - for light theme)

  ------------------------------------------------------------------------------
  -- Syntax Highlighting (adjusted for contrast on light backgrounds)

  -- Keywords, control flow (darker blue for light bg)
  blue       = '#0550AE',

  -- Links, references
  blueLink   = '#0969DA',

  -- Success states, git added, diff added
  green      = '#1A7F37',

  -- Numbers, numeric literals (teal, good contrast on light)
  greenLight = '#098658',

  -- Functions, method names (brown/amber - VS Code style)
  orange     = '#795E26',

  -- Warning backgrounds, subtle highlights
  orangeDark = '#FFF8C5',

  -- Special keywords, decorators, important markers
  pink       = '#BF3989',

  -- Variables, parameters
  purple     = '#6F42C1',

  -- Comments (keeping red as mannydark signature, but darker for light bg)
  red        = '#C41E3A',

  -- Error backgrounds, diff removed backgrounds
  redDark    = '#FFEBE9',

  -- Strings (classic maroon/red for light themes)
  redLight   = '#A31515',

  -- Types, classes, interfaces
  turquoise  = '#0969DA',

  -- Warnings, deprecated (darker yellow/gold for light bg)
  yellow     = '#9A6700',

  ------------------------------------------------------------------------------
  -- Additional Light-Theme Specific Colors

  -- Cursor line, current line highlight (subtle)
  cursorLine = '#F3F4F6',

  -- Selection background
  selection  = '#B4D7FF',

  -- Search highlight
  search     = '#FFDF5D',

  -- Matching brackets
  matchParen = '#C9DEF1',
}

return colors
