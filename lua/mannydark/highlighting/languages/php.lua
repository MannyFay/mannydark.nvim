-------------------------------------------------------------------------------
-- PHP
-------------------------------------------------------------------------------

-- local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local php       = {}


-------------------------------------------------------------------------------
-- Settings

php.setupHighlighting = function()
  highlight(0, "@comment.documentation.php", { link = "MannydarkFgGreen" })  -- Doc blocks (/** ... */)
  highlight(0, "@type.builtin.php",          { link = "Keyword"          })  -- int, string, etc.

  -- PHP in Blade (php_only injection)
  highlight(0, "@function.call.php_only",    { link = "MannydarkFgOrange"  })  -- Function calls (config, route, etc.)
  highlight(0, "@function.php_only",         { link = "MannydarkFgOrange"  })  -- Functions
  highlight(0, "@string.php_only",           { link = "MannydarkFgRedLight"})  -- Strings
  highlight(0, "@variable.php_only",         { link = "MannydarkFgPurple"  })  -- Variables ($user, $post, etc.)
  highlight(0, "@variable.member.php_only",  { link = "MannydarkFgPurple"  })  -- Object properties ($user->name)
  highlight(0, "@operator.php_only",              { link = "MannydarkFgWhite" })  -- Operators (->, etc.)
  highlight(0, "@keyword.conditional.php_only",  { link = "MannydarkFgWhite" })  -- ?? operator (captured as conditional)
  highlight(0, "@punctuation.bracket.php_only",   { link = "MannydarkFgWhite" })  -- Brackets
  highlight(0, "@punctuation.delimiter.php_only", { link = "MannydarkFgWhite" })  -- Delimiters

  -- Custom mannydark captures for php_only (from after/queries/php_only/highlights.scm)
  highlight(0, "@keyword.mannydark.php_only",         { link = "Keyword"            })  -- as, etc.
  highlight(0, "@function.call.mannydark.php_only",   { link = "MannydarkFgOrange"  })
  highlight(0, "@variable.member.mannydark.php_only", { link = "MannydarkFgPurple"  })
  highlight(0, "@string.mannydark.php_only",          { link = "MannydarkFgRedLight"})
end

return php
