-- [[ MannyDark theme for LuaLine ]]
-- [[ License: MIT                ]]
-- [[ mannythefay@gmail.com       ]]
-- [[ https://github.com/MannyFay ]]

local colors = {
  blue       = '#569CD6',
  turquoise  = '#45C8B0',
  purple     = '#C064C7',
  orange     = '#E8BF6A',
  gray       = '#252525',
}

return {
  normal = {
    a = { fg = colors.blue, bg = colors.gray },
    b = { fg = colors.blue, bg = colors.gray },
    c = { fg = colors.blue, bg = colors.gray },
    x = { fg = colors.blue, bg = colors.gray },
    y = { fg = colors.blue, bg = colors.gray },
    z = { fg = colors.blue, bg = colors.gray },
  },
  insert = {
    a = { fg = colors.purple, bg = colors.gray },
    b = { fg = colors.purple, bg = colors.gray },
    c = { fg = colors.purple, bg = colors.gray },
    x = { fg = colors.purple, bg = colors.gray },
    y = { fg = colors.purple, bg = colors.gray },
    z = { fg = colors.purple, bg = colors.gray },
  },
  visual = {
    a = { fg = colors.orange, bg = colors.gray }, 
    b = { fg = colors.orange, bg = colors.gray },
    c = { fg = colors.orange, bg = colors.gray },
    x = { fg = colors.orange, bg = colors.gray },
    y = { fg = colors.orange, bg = colors.gray },
    z = { fg = colors.orange, bg = colors.gray },
  },
  command = {
    a = { fg = colors.turquoise, bg = colors.gray },
    b = { fg = colors.turquoise, bg = colors.gray },
    c = { fg = colors.turquoise, bg = colors.gray },
    x = { fg = colors.turquoise, bg = colors.gray },
    y = { fg = colors.turquoise, bg = colors.gray },
    z = { fg = colors.turquoise, bg = colors.gray },
  },
	replace = {
    a = {  },
    b = {  },
    c = {  },
    x = {  },
    y = {  },
    z = {  },
  },
	inactive = {
		a = {  },
    b = {  },
    c = {  },
    x = {  },
    y = {  },
    z = {  },
	},
}
