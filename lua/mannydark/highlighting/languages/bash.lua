-------------------------------------------------------------------------------
-- Bash Files
-- Highlighting for .bash, .bashrc, .bash_profile, .bash_aliases files.
-------------------------------------------------------------------------------

-- local colors  = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local bash    = {}


-------------------------------------------------------------------------------
-- Settings

bash.setupHighlighting = function()
  -- Commands/executables - green
  highlight(0, "@function.call.bash",    { link = "MannydarkFgGreen" })  -- All commands
  highlight(0, "@function.builtin.bash", { link = "MannydarkFgGreen" })  -- Builtins (echo, cd, printf, etc.)

  -- Array subscript special chars (@ and *) - white
  highlight(0, "@character.special.bash", { link = "MannydarkFgWhite" })

  -- Variables - purple
  highlight(0, "@variable.bash", { link = "MannydarkFgPurple" })
  highlight(0, "@constant.bash",  { link = "MannydarkFgPurple" })  -- Uppercase vars like COUNTER, PATH

  -- Array subscript keys: [key]="value"
  -- [, ] = white, key = purple, = = white
  local ns = vim.api.nvim_create_namespace("mannydark_bash_arrays")

  local function highlight_array_keys(bufnr)
    vim.api.nvim_buf_clear_namespace(bufnr, ns, 0, -1)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    for lnum, line in ipairs(lines) do
      local start_pos = 1
      -- Match escape sequences: \n, \t, \', \", \\, \r, \0, etc.
      while true do
        local s, e = line:find("\\[ntrv0'\"\\abef]", start_pos)
        if not s then break end
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, s - 1, {
          end_col = e,
          hl_group = "MannydarkFgPink",
          priority = 300,
        })
        start_pos = e + 1
      end
      start_pos = 1
      -- Match variables inside ((...)) arithmetic expressions
      while true do
        local s, e = line:find("%(%(([%w_]+)", start_pos)
        if not s then break end
        local var_start = s + 2  -- after ((
        local var_end = e
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, var_start - 1, {
          end_col = var_end,
          hl_group = "MannydarkFgPurple",
          priority = 300,
        })
        start_pos = e + 1
      end
      start_pos = 1
      while true do
        -- Match [key]= pattern
        local s, e, key_start, key_end = line:find("%[()%w+()%]=", start_pos)
        if not s then break end
        -- [ bracket - white
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, s - 1, {
          end_col = s,
          hl_group = "MannydarkFgWhite",
          priority = 300,
        })
        -- key content - purple
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, s, {
          end_col = e - 2,
          hl_group = "MannydarkFgPurple",
          priority = 300,
        })
        -- ] bracket - white
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, e - 2, {
          end_col = e - 1,
          hl_group = "MannydarkFgWhite",
          priority = 300,
        })
        -- = sign - white
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, e - 1, {
          end_col = e,
          hl_group = "MannydarkFgWhite",
          priority = 300,
        })
        start_pos = e + 1
      end
    end
  end

  vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "TextChanged", "TextChangedI" }, {
    group = vim.api.nvim_create_augroup("MannydarkBashArrays", { clear = true }),
    pattern = { "*.bash", "*.sh", ".bashrc", ".bash_profile", ".bash_aliases" },
    callback = function(args)
      highlight_array_keys(args.buf)
    end,
  })
end

return bash
