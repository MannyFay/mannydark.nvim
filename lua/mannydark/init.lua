-------------------------------------------------------------------------------
-- Mannydark Colorscheme
-- A dark (and light) colorscheme for Neovim
-------------------------------------------------------------------------------

---@class MannydarkStyles
---@field comments? table Style for comments (e.g., { italic = true })
---@field keywords? table Style for keywords
---@field functions? table Style for functions
---@field variables? table Style for variables
---@field strings? table Style for strings
---@field types? table Style for types

---@class MannydarkConfig
---@field style string Theme style: "dark" | "light"
---@field transparent boolean Enable transparent background
---@field dim_inactive boolean Dim inactive windows
---@field terminal_colors boolean Apply colors to terminal
---@field styles MannydarkStyles Per-element styling options
---@field on_colors? fun(colors: table) Callback to override colors
---@field on_highlights? fun(highlights: table, colors: table) Callback to override highlights

local M = {}

-------------------------------------------------------------------------------
-- Default Configuration

---@type MannydarkConfig
M.config = {
  style = 'dark',            -- "dark" | "light"
  transparent = false,       -- Transparent background
  dim_inactive = false,      -- Dim inactive windows
  terminal_colors = true,    -- Apply colors to :terminal

  styles = {
    comments  = { italic = true },
    keywords  = {},
    functions = {},
    variables = {},
    strings   = {},
    types     = {},
  },

  on_colors = nil,           -- function(colors) end
  on_highlights = nil,       -- function(highlights, colors) end
}

-------------------------------------------------------------------------------
-- Color Palette (loaded based on style)

M.colors = nil

-------------------------------------------------------------------------------
-- Load colors based on current style

local function load_colors()
  local style = M.config.style

  -- Load the appropriate palette
  local palette
  if style == 'light' then
    palette = require('mannydark.palette_light')
  else
    palette = require('mannydark.palette')
  end

  -- Create a copy so we don't modify the original
  local colors = vim.tbl_deep_extend('force', {}, palette)

  -- Apply user color overrides
  if M.config.on_colors then
    M.config.on_colors(colors)
  end

  return colors
end

-------------------------------------------------------------------------------
-- Apply Configuration-Dependent Highlights

local function apply_config(colors)
  local highlight = vim.api.nvim_set_hl
  local styles = M.config.styles

  -- Transparent background
  if M.config.transparent then
    highlight(0, 'Normal', { fg = colors.white, bg = 'NONE' })
    highlight(0, 'NormalNC', { fg = colors.white, bg = 'NONE' })
    highlight(0, 'NormalFloat', { fg = colors.white, bg = 'NONE' })
    highlight(0, 'SignColumn', { fg = 'NONE', bg = 'NONE' })
    highlight(0, 'EndOfBuffer', { fg = 'NONE', bg = 'NONE' })
    highlight(0, 'CursorLine', { fg = 'NONE', bg = colors.grayDark })
    highlight(0, 'CursorLineNr', { fg = colors.white, bg = 'NONE' })
  end

  -- Dim inactive windows
  if M.config.dim_inactive then
    highlight(0, 'NormalNC', { fg = colors.gray, bg = colors.black })
  end

  -- Apply comment style
  local comment_style = vim.tbl_extend('force', { fg = colors.red, bg = 'NONE' }, styles.comments or {})
  highlight(0, 'Comment', comment_style)
  highlight(0, '@comment', comment_style)
  highlight(0, '@lsp.type.comment', comment_style)

  -- Apply keyword style
  if styles.keywords and next(styles.keywords) then
    local keyword_style = vim.tbl_extend('force', { fg = colors.blue, bg = 'NONE' }, styles.keywords)
    highlight(0, 'Keyword', keyword_style)
    highlight(0, '@keyword', keyword_style)
  end

  -- Apply function style
  if styles.functions and next(styles.functions) then
    local function_style = vim.tbl_extend('force', { fg = colors.orange, bg = 'NONE' }, styles.functions)
    highlight(0, 'Function', function_style)
    highlight(0, '@function', function_style)
  end

  -- Apply variable style
  if styles.variables and next(styles.variables) then
    local variable_style = vim.tbl_extend('force', { fg = colors.white, bg = 'NONE' }, styles.variables)
    highlight(0, 'Identifier', variable_style)
    highlight(0, '@variable', variable_style)
  end

  -- Apply string style
  if styles.strings and next(styles.strings) then
    local string_style = vim.tbl_extend('force', { fg = colors.redLight, bg = 'NONE' }, styles.strings)
    highlight(0, 'String', string_style)
    highlight(0, '@string', string_style)
  end

  -- Apply type style
  if styles.types and next(styles.types) then
    local type_style = vim.tbl_extend('force', { fg = colors.turquoise, bg = 'NONE' }, styles.types)
    highlight(0, 'Type', type_style)
    highlight(0, '@type', type_style)
  end
end

-------------------------------------------------------------------------------
-- Apply Terminal Colors

local function apply_terminal_colors(colors)
  if not M.config.terminal_colors then
    return
  end

  vim.g.terminal_color_0  = colors.black      -- Black
  vim.g.terminal_color_1  = colors.red        -- Red
  vim.g.terminal_color_2  = colors.green      -- Green
  vim.g.terminal_color_3  = colors.orange     -- Yellow
  vim.g.terminal_color_4  = colors.blue       -- Blue
  vim.g.terminal_color_5  = colors.purple     -- Magenta
  vim.g.terminal_color_6  = colors.turquoise  -- Cyan
  vim.g.terminal_color_7  = colors.white      -- White
  -- Bright variants
  vim.g.terminal_color_8  = colors.gray
  vim.g.terminal_color_9  = colors.pink
  vim.g.terminal_color_10 = colors.greenLight
  vim.g.terminal_color_11 = colors.yellow
  vim.g.terminal_color_12 = colors.blueLink
  vim.g.terminal_color_13 = colors.purple
  vim.g.terminal_color_14 = colors.turquoise
  vim.g.terminal_color_15 = colors.white
end

-------------------------------------------------------------------------------
-- Setup

--- Setup the colorscheme with optional configuration.
---@param opts? MannydarkConfig Configuration options.
M.setup = function(opts)
  -- Merge user options with defaults
  M.config = vim.tbl_deep_extend('force', M.config, opts or {})

  -- Clear highlights
  vim.cmd('hi clear')

  -- Set background based on style
  if M.config.style == 'light' then
    vim.o.background = 'light'
  else
    vim.o.background = 'dark'
  end

  -- Reset syntax
  if vim.fn.exists('syntax_on') == 1 then
    vim.cmd('syntax reset')
  end

  vim.o.termguicolors = true
  vim.g.colors_name = M.config.style == 'light' and 'mannybright' or 'mannydark'

  -- Load colors for current style
  M.colors = load_colors()

  -- Set the palette module to our loaded colors BEFORE loading theme
  -- This makes require('mannydark.palette') return the correct colors
  package.loaded['mannydark.palette'] = M.colors

  -- Build theme
  local theme = require('mannydark.theme')
  theme.buildTheme()

  -- Apply config-dependent highlights (after theme, so they override)
  apply_config(M.colors)

  -- Apply terminal colors
  apply_terminal_colors(M.colors)

  -- Apply user highlight overrides (last, so they have final say)
  if M.config.on_highlights then
    local hl = setmetatable({}, {
      __newindex = function(_, name, val)
        vim.api.nvim_set_hl(0, name, val)
      end
    })
    M.config.on_highlights(hl, M.colors)
  end
end

-------------------------------------------------------------------------------
-- Convenience Functions

--- Get the current color palette
---@return table colors The current color palette
M.get_colors = function()
  return M.colors or load_colors()
end

--- Switch between dark and light style
---@param style? string "dark" | "light" | nil (toggles if nil)
M.toggle = function(style)
  if style then
    M.config.style = style
  else
    M.config.style = M.config.style == 'dark' and 'light' or 'dark'
  end

  -- Clear highlighting module caches so they reload with new colors
  for name, _ in pairs(package.loaded) do
    if name:match('^mannydark%.highlighting%.') then
      package.loaded[name] = nil
    end
  end
  package.loaded['mannydark.theme'] = nil
  package.loaded['mannydark.palette'] = nil
  package.loaded['mannydark.palette_light'] = nil

  M.setup(M.config)
end

return M
