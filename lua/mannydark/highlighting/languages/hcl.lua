-------------------------------------------------------------------------------
-- HCL Files (HashiCorp Configuration Language)
-- Highlighting for .hcl, .tf, .tfvars files (Terraform, Packer, Vault, etc.)
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local hcl       = {}


-------------------------------------------------------------------------------
-- Settings

hcl.setupHighlighting = function()

end

return hcl