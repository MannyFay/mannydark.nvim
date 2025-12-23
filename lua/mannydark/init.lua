-------------------------------------------------------------------------------
-- Mannydark Colorscheme
-- A dark, bright, and colorblind-friendly colorscheme for Neovim
-------------------------------------------------------------------------------

---@class MannydarkStyles
---@field comments? table Style for comments (e.g., { italic = true })
---@field keywords? table Style for keywords
---@field functions? table Style for functions
---@field variables? table Style for variables
---@field strings? table Style for strings
---@field types? table Style for types

---@class MannydarkConfig
---@field style string Theme style: "dark" | "bright" | "red-green-dark" | "red-green-bright"
---@field transparent boolean Enable transparent background
---@field dim_inactive boolean Dim inactive windows
---@field terminal_colors boolean Apply colors to terminal
---@field styles MannydarkStyles Per-element styling options
---@field on_colors? fun(colors: table) Callback to override colors
---@field on_highlights? fun(highlights: table, colors: table) Callback to override highlights

local M = {}

-------------------------------------------------------------------------------
-- Early Filetype Detection
-- Register filetypes for languages without built-in Neovim support
-- This must happen at module load time, before any files are opened

vim.filetype.add({
  extension = {
    bf = "brainfuck",
    b = "brainfuck",
    brainfuck = "brainfuck",
  },
})

-------------------------------------------------------------------------------
-- Default Configuration

-- Store user config in _G (global table) so it survives module cache clearing
-- This is CRITICAL for hot reload to work correctly
-- Using _G (not vim.g) because vim.g can"t store functions (on_colors, on_highlights)
-- _G persists for the entire Neovim session and survives package.loaded clears
local function get_user_config()
  return _G._mannydark_user_config
end

local function set_user_config(config)
  _G._mannydark_user_config = config
end

---@type MannydarkConfig
M.config = {
  style = "dark",            -- "dark" | "bright" | "red-green-dark" | "red-green-bright"
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
  if style == "bright" then
    palette = require("mannydark.palette_bright")
  elseif style == "red-green-dark" then
    palette = require("mannydark.palette_redgreen_dark")
  elseif style == "red-green-bright" then
    palette = require("mannydark.palette_redgreen_bright")
  else
    palette = require("mannydark.palette")
  end

  -- Create a copy so we don"t modify the original
  local colors = vim.tbl_deep_extend("force", {}, palette)

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
    highlight(0, "Normal", { fg = colors.white, bg = "NONE" })
    highlight(0, "NormalNC", { fg = colors.white, bg = "NONE" })
    highlight(0, "NormalFloat", { fg = colors.white, bg = "NONE" })
    highlight(0, "SignColumn", { fg = "NONE", bg = "NONE" })
    highlight(0, "EndOfBuffer", { fg = "NONE", bg = "NONE" })
    highlight(0, "CursorLine", { fg = "NONE", bg = colors.grayDark })
    highlight(0, "CursorLineNr", { fg = colors.white, bg = "NONE" })
  end

  -- Dim inactive windows
  if M.config.dim_inactive then
    highlight(0, "NormalNC", { fg = colors.gray, bg = colors.black })
  end

  -- Apply comment style
  local comment_style = vim.tbl_extend("force", { fg = colors.red, bg = "NONE" }, styles.comments or {})
  highlight(0, "Comment", comment_style)
  highlight(0, "@comment", comment_style)
  highlight(0, "@lsp.type.comment", comment_style)

  -- Apply keyword style
  if styles.keywords and next(styles.keywords) then
    local keyword_style = vim.tbl_extend("force", { fg = colors.blue, bg = "NONE" }, styles.keywords)
    highlight(0, "Keyword", keyword_style)
    highlight(0, "@keyword", keyword_style)
  end

  -- Apply function style
  if styles.functions and next(styles.functions) then
    local function_style = vim.tbl_extend("force", { fg = colors.orange, bg = "NONE" }, styles.functions)
    highlight(0, "Function", function_style)
    highlight(0, "@function", function_style)
  end

  -- Apply variable style
  if styles.variables and next(styles.variables) then
    local variable_style = vim.tbl_extend("force", { fg = colors.white, bg = "NONE" }, styles.variables)
    highlight(0, "Identifier", variable_style)
    highlight(0, "@variable", variable_style)
  end

  -- Apply string style
  if styles.strings and next(styles.strings) then
    local string_style = vim.tbl_extend("force", { fg = colors.redLight, bg = "NONE" }, styles.strings)
    highlight(0, "String", string_style)
    highlight(0, "@string", string_style)
  end

  -- Apply type style
  if styles.types and next(styles.types) then
    local type_style = vim.tbl_extend("force", { fg = colors.turquoise, bg = "NONE" }, styles.types)
    highlight(0, "Type", type_style)
    highlight(0, "@type", type_style)
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
  -- CRITICAL: Save user config before any cache clearing
  -- This allows hot reload to preserve user settings
  if opts then
    set_user_config(vim.deepcopy(opts))
  end

  -- Use saved user config if available (for hot reload scenarios)
  local effective_opts = opts or get_user_config() or {}

  -- Merge user options with defaults
  M.config = vim.tbl_deep_extend("force", {
    style = "dark",
    transparent = false,
    dim_inactive = false,
    terminal_colors = true,
    styles = {
      comments  = { italic = true },
      keywords  = {},
      functions = {},
      variables = {},
      strings   = {},
      types     = {},
    },
    on_colors = nil,
    on_highlights = nil,
  }, effective_opts)

  -- Set termguicolors first (required for true color)
  vim.o.termguicolors = true

  -- Temporarily clear colors_name to prevent Neovim from auto-reloading
  -- a colorscheme when we change vim.o.background
  local old_colors_name = vim.g.colors_name
  vim.g.colors_name = nil

  -- Set background based on style BEFORE clearing (Neovim uses this for hi clear behavior)
  if M.config.style == "bright" or M.config.style == "red-green-bright" then
    vim.o.background = "light"
  else
    vim.o.background = "dark"
  end

  -- Clear highlighting module caches so they reload with new colors
  for name, _ in pairs(package.loaded) do
    if name:match("^mannydark%.") and name ~= "mannydark" then
      package.loaded[name] = nil
    end
  end

  -- Load colors for current style
  M.colors = load_colors()

  -- Pre-populate the palette module so require("mannydark.palette") returns correct colors
  package.loaded["mannydark.palette"] = M.colors

  -- Only clear highlights if a colorscheme was previously active
  -- This prevents flash on initial load and follows TokyoNight/Gruvbox pattern
  if old_colors_name then
    vim.cmd("hi clear")
  end

  -- Reset syntax if enabled
  if vim.fn.exists("syntax_on") == 1 then
    vim.cmd("syntax reset")
  end

  -- Set the colorscheme name
  local style_names = {
    dark = "mannydark",
    bright = "mannybright",
    ["red-green-dark"] = "mannydark-rg",
    ["red-green-bright"] = "mannybright-rg",
  }
  vim.g.colors_name = style_names[M.config.style] or "mannydark"

  -- Apply Normal highlight IMMEDIATELY after clearing to prevent default colorscheme flash
  -- This is critical for Neovim 0.10+ where hi clear loads the default colorscheme
  local bg = M.config.transparent and "NONE" or M.colors.black
  vim.api.nvim_set_hl(0, "Normal", { fg = M.colors.white, bg = bg })
  vim.api.nvim_set_hl(0, "NormalNC", { fg = M.colors.white, bg = bg })
  vim.api.nvim_set_hl(0, "NormalFloat", { fg = M.colors.white, bg = M.colors.grayDark })

  -- Build theme (loads all highlight modules)
  local theme = require("mannydark.theme")
  theme.buildTheme()

  -- Apply config-dependent highlights (after theme, so they override)
  apply_config(M.colors)

  -- Apply terminal colors
  apply_terminal_colors(M.colors)

  -- Re-apply LSP semantic highlights after LspAttach
  -- This ensures our highlights take precedence over plugins (like lazydev) that set their own
  vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("MannydarkLspHighlights", { clear = true }),
    callback = function()
      vim.schedule(function()
        require("mannydark.highlighting.languages.languagedefaults").applyLspSemanticHighlights()
        -- Re-apply config-dependent highlights (styles.comments, etc.) after LSP highlights
        apply_config(M.colors)
      end)
    end,
  })

  -- Apply user highlight overrides (last, so they have final say)
  if M.config.on_highlights then
    local hl = setmetatable({}, {
      __newindex = function(_, name, val)
        vim.api.nvim_set_hl(0, name, val)
      end
    })
    M.config.on_highlights(hl, M.colors)
  end

  -- Refresh nvim-web-devicons if installed (hi clear wipes their highlights)
  local ok, devicons = pcall(require, "nvim-web-devicons")
  if ok and devicons.refresh then
    devicons.refresh()
  end
end

-------------------------------------------------------------------------------
-- Convenience Functions

--- Get the current color palette
---@return table colors The current color palette
M.get_colors = function()
  return M.colors or load_colors()
end

--- Switch between styles
---@param style? string "dark" | "bright" | "red-green-dark" | "red-green-bright" | nil (cycles if nil)
M.toggle = function(style)
  if style then
    M.config.style = style
  else
    -- Cycle through: dark -> bright -> red-green-dark -> red-green-bright -> dark
    local cycle = {
      dark = "bright",
      bright = "red-green-dark",
      ["red-green-dark"] = "red-green-bright",
      ["red-green-bright"] = "dark",
    }
    M.config.style = cycle[M.config.style] or "dark"
  end

  -- setup() handles all cache clearing now
  M.setup(M.config)
end

-------------------------------------------------------------------------------
-- Development Mode & Commands

local dev_mode_enabled = false

--- Reload the colorscheme (clears all caches and re-applies)
--- This is the main function for hot reload
M.reload = function()
  -- Config is saved in _G._mannydark_user_config, which survives module reload
  -- So we don"t need to save it here - it persists automatically

  -- Clear ALL mannydark modules from cache
  for name, _ in pairs(package.loaded) do
    if name:match("^mannydark") then
      package.loaded[name] = nil
    end
  end

  -- Clear treesitter query cache for languages with custom queries in after/queries/
  -- This ensures .scm file changes take effect without restart
  if vim.treesitter and vim.treesitter.query then
    local langs_with_custom_queries = { "lua", "ada", "javascript", "tsx", "typescript", "css", "html", "html_tags", "php", "jsonnet", "c_sharp", "c" }
    for _, lang in ipairs(langs_with_custom_queries) do
      -- Neovim 0.9+: invalidate() clears the query cache for a language
      if vim.treesitter.query.invalidate then
        pcall(vim.treesitter.query.invalidate, lang)
      end
    end
  end

  -- Collect buffers that need treesitter refresh BEFORE clearing modules
  local buffers_to_refresh = {}
  for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(bufnr) and vim.bo[bufnr].buftype == "" then
      local ft = vim.bo[bufnr].filetype
      if ft and ft ~= "" then
        -- Stop treesitter and invalidate parser
        pcall(function()
          vim.treesitter.stop(bufnr)
          local parser = vim.treesitter.get_parser(bufnr)
          if parser then
            parser:invalidate(true)
          end
        end)
        table.insert(buffers_to_refresh, bufnr)
      end
    end
  end

  -- Re-require mannydark (fresh module)
  local ok, mannydark = pcall(require, "mannydark")
  if ok then
    -- setup() will automatically use vim.g.mannydark_user_config
    mannydark.setup()

    -- Restart treesitter on all collected buffers
    for _, bufnr in ipairs(buffers_to_refresh) do
      pcall(function()
        vim.treesitter.start(bufnr)
      end)
    end

    return true
  else
    vim.notify("Mannydark reload failed: " .. tostring(mannydark), vim.log.levels.ERROR)
    return false
  end
end

--- Enable development mode with auto-reload on save
--- Call this when working on the colorscheme
M.dev_mode = function()
  if dev_mode_enabled then
    vim.notify("Mannydark dev mode already enabled", vim.log.levels.WARN)
    return
  end

  dev_mode_enabled = true

  -- Auto-reload when any mannydark lua file is saved
  vim.api.nvim_create_autocmd("BufWritePost", {
    group = vim.api.nvim_create_augroup("MannydarkDevMode", { clear = true }),
    pattern = { "*/mannydark.nvim/**.lua", "*/mannydark/**.lua" },
    callback = function(ev)
      local filepath = ev.file or vim.fn.expand("%:p")

      -- Double-check this is a mannydark file
      if not filepath:match("mannydark") then
        return
      end

      -- Only reload if mannydark colorscheme is active
      local current = vim.g.colors_name
      if not current or not current:match("^manny") then
        return
      end

      -- Use vim.schedule to avoid issues with autocmd timing
      vim.schedule(function()
        if M.reload() then
          local filename = vim.fn.fnamemodify(filepath, ":t")
          vim.notify("↻ " .. filename, vim.log.levels.INFO)
        end
      end)
    end,
  })

  vim.notify("Mannydark dev mode enabled - auto-reload on save", vim.log.levels.INFO)
end

--- Disable development mode
M.dev_mode_off = function()
  if not dev_mode_enabled then
    return
  end

  dev_mode_enabled = false
  pcall(vim.api.nvim_del_augroup_by_name, "MannydarkDevMode")
  vim.notify("Mannydark dev mode disabled", vim.log.levels.INFO)
end

--- Check if dev mode is enabled
M.is_dev_mode = function()
  return dev_mode_enabled
end

--- Debug function to show current state
M.debug = function()
  local user_config = get_user_config()
  local info = {
    "Mannydark Debug Info:",
    "─────────────────────",
    "colors_name: " .. tostring(vim.g.colors_name),
    "dev_mode: " .. tostring(dev_mode_enabled),
    "style: " .. tostring(M.config.style),
    "transparent: " .. tostring(M.config.transparent),
    "user_config exists: " .. tostring(user_config ~= nil),
    "user_config.style: " .. tostring(user_config and user_config.style or "N/A"),
    "",
    "Loaded mannydark modules:",
  }

  local count = 0
  for name, _ in pairs(package.loaded) do
    if name:match("^mannydark") then
      table.insert(info, "  • " .. name)
      count = count + 1
    end
  end
  table.insert(info, "Total: " .. count .. " modules")

  vim.notify(table.concat(info, "\n"), vim.log.levels.INFO)
end

-------------------------------------------------------------------------------
-- User Commands

-- Create user commands on first load
local function create_commands()
  -- :MannydarkReload - Manual reload
  vim.api.nvim_create_user_command("MannydarkReload", function()
    if M.reload() then
      vim.notify("Mannydark reloaded!", vim.log.levels.INFO)
    end
  end, { desc = "Reload mannydark colorscheme" })

  -- :MannydarkDevMode - Toggle dev mode
  vim.api.nvim_create_user_command("MannydarkDevMode", function()
    if dev_mode_enabled then
      M.dev_mode_off()
    else
      M.dev_mode()
    end
  end, { desc = "Toggle mannydark dev mode" })

  -- :MannydarkDebug - Show debug info
  vim.api.nvim_create_user_command("MannydarkDebug", function()
    M.debug()
  end, { desc = "Show mannydark debug info" })

  -- :MannydarkToggle [style] - Toggle or set style
  vim.api.nvim_create_user_command("MannydarkToggle", function(opts)
    if opts.args and opts.args ~= "" then
      M.toggle(opts.args)
    else
      M.toggle()
    end
  end, {
    desc = "Toggle mannydark style (dark/bright/red-green-dark/red-green-bright)",
    nargs = "?",
    complete = function()
      return { "dark", "bright", "red-green-dark", "red-green-bright" }
    end,
  })
end

-- Create commands immediately
create_commands()

-------------------------------------------------------------------------------
-- Auto Dev Mode (like Catppuccin"s debug mode)
-- Set vim.g.mannydark_debug = true BEFORE loading to enable

if vim.g.mannydark_debug then
  vim.api.nvim_create_autocmd("BufWritePost", {
    group = vim.api.nvim_create_augroup("MannydarkDebugMode", { clear = true }),
    pattern = { "*/mannydark.nvim/**.lua", "*/mannydark/**.lua" },
    callback = function()
      vim.schedule(function()
        if M.reload() then
          vim.notify("Mannydark (debug): reloaded", vim.log.levels.INFO)
        end
      end)
    end,
  })
end

return M
