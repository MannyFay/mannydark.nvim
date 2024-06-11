-------------------------------------------------------------------------------
-- GraphQL
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local graphql   = {}

-------------------------------------------------------------------------------
---- Settings

graphql.setupHighlighting = function()
  -- highlight(0, "phpTodo", { fg = colors.red, bg = "NONE" })

----------------------- Not used by now:
-- graphqlComment xxx links to Comment
-- graphqlOperator xxx links to Operator
-- graphqlBoolean xxx links to Boolean
-- graphqlNull    xxx links to Keyword
-- graphqlNumber  xxx links to Number
-- graphqlString  xxx links to String
-- graphqlKeyword xxx links to Keyword
-- graphqlType    xxx links to Type
-- graphqlDirectiveLocation xxx links to Special
-- graphqlStructure xxx links to Structure
-- graphqlName    xxx links to Identifier
-- graphqlDirective xxx links to PreProc
-- graphqlFold    xxx cleared
-- graphqlVariable xxx links to Identifier
-- graphqlMetaFields xxx links to Special
-- graphqlBraces  xxx links to Delimiter
-- graphqlList    xxx cleared
-- graphqlTemplateString xxx links to typescriptTemplate
-- graphqlTaggedTemplate xxx cleared
-- graphqlTemplateExpression xxx links to typescriptTemplateSubstitution


end

return graphql

