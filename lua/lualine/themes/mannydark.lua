local colors = {
  blue       = '#569CD6',
  turquoise  = '#45C8B0',
  purple     = '#C064C7',
  red1   = '#CE9178',
  --[[ yellow = '#dcdcaa', ]]
  --[[ orange = '#D7BA7D', ]]
  orange = '#E8BF6A',
  fg     = '#ababab',
  -- bg     = '#007acc',
  -- bg     = '#68217a',
  bg     = '#252525',
  gray  = '#252525',
  --gray  = '#333333',
  -- light_gray  = '#5c6370',
  --[[ gray3  = '#3e4452', ]]
  gray3  = '#252525',
}

return {
  normal = {
    a = { fg = colors.blue, bg = colors.gray },
    b = { fg = colors.blue, bg = colors.gray },
    c = { fg = colors.blue, bg = colors.gray },
    x = {},
    y = {},
    z = {},
  },
  insert = {
    a = { fg = colors.gray     , bg = colors.turquoise },
    b = { fg = colors.turquoise, bg = colors.gray      },
    c = { fg = colors.turquoise, bg = colors.gray      },
    x = {},
    y = {},
    z = {},
  },
	visual = { a = { fg = colors.bg, bg = colors.purple }, b = { fg = colors.purple, bg = colors.gray } },
	command = { a = { fg = colors.bg, bg = colors.orange }, b = { fg = colors.orange, bg = colors.gray } },
	replace = { a = { fg = colors.bg, bg = colors.red }, b = { fg = colors.red, bg = colors.gray } },

	inactive = {
		a = { bg = colors.bg, fg = colors.blue },
		b = { bg = colors.bg, fg = colors.gray, gui = "bold" },
		c = { bg = colors.bg, fg = colors.gray },
	},
}
