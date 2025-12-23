-------------------------------------------------------------------------------
-- YAML Files
-- Highlighting for .yml and .yaml files.
-------------------------------------------------------------------------------

-- local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local yml       = {}


-------------------------------------------------------------------------------
-- Settings

yml.setupHighlighting = function()
  highlight(0, "@punctuation.special.yaml", { link = "Keyword" })      -- ---
  highlight(0, "@string.special.yaml",      { link = "MannydarkFgGreen" })  -- Terminal commands (run:, command:)

  -- GitHub Actions template expressions (${{ ... }}) - purple via extmarks
  local ns = vim.api.nvim_create_namespace("mannydark_yaml_templates")

  local function highlight_templates(bufnr)
    vim.api.nvim_buf_clear_namespace(bufnr, ns, 0, -1)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    for lnum, line in ipairs(lines) do
      local start_pos = 1
      while true do
        local s, e = line:find("%${{.-}}", start_pos)
        if not s then break end
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, s - 1, {
          end_col = e,
          hl_group = "MannydarkFgRedLight",
          priority = 300,  -- Higher than treesitter (100-200)
        })
        start_pos = e + 1
      end
    end
  end

  vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "TextChanged", "TextChangedI" }, {
    group = vim.api.nvim_create_augroup("MannydarkYamlTemplates", { clear = true }),
    pattern = { "*.yaml", "*.yml" },
    callback = function(args)
      highlight_templates(args.buf)
    end,
  })
end

return yml
