-------------------------------------------------------------------------------
-- INI and Configuration Files
-- Highlighting for .ini, .cfg, .conf, .editorconfig, .gitconfig, .desktop,
-- .properties, .npmrc, .pip.conf, systemd units, ssh_config, and similar files.
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local ini       = {}


-------------------------------------------------------------------------------
-- Settings

ini.setupHighlighting = function()

end

return ini