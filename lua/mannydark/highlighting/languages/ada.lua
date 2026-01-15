-------------------------------------------------------------------------------
-- Ada Files
-- Highlighting for .ada, .adb, .ads files.
--
-- Note: This colorscheme includes queries/ada/highlights.scm which provides
-- comprehensive tree-sitter captures for Ada. This file only needs to style
-- the highlight groups.
-------------------------------------------------------------------------------

local highlight = vim.api.nvim_set_hl
local ada       = {}


-------------------------------------------------------------------------------
-- Settings

ada.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Tree-sitter Captures (styled via queries/ada/highlights.scm)

  -- Keywords
  highlight(0, "@keyword.ada",              { link = "Keyword"              })
  highlight(0, "@keyword.type.ada",         { link = "Keyword"              })
  highlight(0, "@keyword.function.ada",     { link = "Keyword"              })
  highlight(0, "@keyword.operator.ada",     { link = "Keyword"              })
  highlight(0, "@keyword.return.ada",       { link = "Keyword"              })
  highlight(0, "@keyword.directive.ada",    { link = "Keyword"              })
  highlight(0, "@storageclass.ada",         { link = "Keyword"              })
  highlight(0, "@include.ada",              { link = "Keyword"              })
  highlight(0, "@exception.ada",            { link = "Keyword"              })
  highlight(0, "@repeat.ada",               { link = "Keyword"              })
  highlight(0, "@conditional.ada",          { link = "Keyword"              })

  -- Types
  highlight(0, "@type.ada",                 { link = "Type"                 })
  highlight(0, "@type.builtin.ada",         { link = "Type"                 })
  highlight(0, "@type.definition.ada",      { link = "Type"                 })

  -- Modules/Packages
  highlight(0, "@module.ada",               { link = "Type"                 })
  highlight(0, "@namespace.ada",            { link = "Type"                 })

  -- Functions & Procedures
  highlight(0, "@function.ada",             { link = "Function"             })
  highlight(0, "@function.call.ada",        { link = "Function"             })

  -- Variables & Parameters
  highlight(0, "@variable.ada",             { link = "Variable"             })
  highlight(0, "@variable.parameter.ada",   { link = "Variable"             })
  highlight(0, "@variable.member.ada",      { link = "Variable"             })

  -- Constants
  highlight(0, "@constant.ada",             { link = "Constant"             })
  highlight(0, "@constant.builtin.ada",     { link = "Constant"             })

  -- Literals
  highlight(0, "@string.ada",               { link = "String"               })
  highlight(0, "@character.ada",            { link = "String"               })
  highlight(0, "@number.ada",               { link = "Number"               })
  highlight(0, "@boolean.ada",              { link = "Boolean"              })

  -- Operators & Punctuation
  highlight(0, "@operator.ada",             { link = "Operator"             })
  highlight(0, "@punctuation.delimiter.ada",{ link = "Delimiter"            })
  highlight(0, "@punctuation.bracket.ada",  { link = "Delimiter"            })

  -- Labels
  highlight(0, "@label.ada",                { link = "Label"                })

  -- Attributes ('Access, 'Range, etc.)
  highlight(0, "@attribute.ada",            { link = "MannydarkFgPurple"    })

  -- Comments
  highlight(0, "@comment.ada",              { link = "Comment"              })
  highlight(0, "@comment.documentation.ada",{ link = "MannydarkFgGreen"     })

  -- Errors
  highlight(0, "@error.ada",                { link = "Error"                })


  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (Fallback for non-tree-sitter users)

  highlight(0, "adaKeyword",                { link = "Keyword"              })
  highlight(0, "adaStatement",              { link = "Keyword"              })
  highlight(0, "adaConditional",            { link = "Keyword"              })
  highlight(0, "adaRepeat",                 { link = "Keyword"              })
  highlight(0, "adaException",              { link = "Keyword"              })
  highlight(0, "adaStorageClass",           { link = "Keyword"              })
  highlight(0, "adaStructure",              { link = "Keyword"              })
  highlight(0, "adaRecord",                 { link = "Keyword"              })
  highlight(0, "adaBegin",                  { link = "Keyword"              })
  highlight(0, "adaEnd",                    { link = "Keyword"              })
  highlight(0, "adaInc",                    { link = "Keyword"              })

  highlight(0, "adaType",                   { link = "Type"                 })
  highlight(0, "adaBuiltinType",            { link = "Type"                 })
  highlight(0, "adaTypedef",                { link = "Type"                 })

  highlight(0, "adaFunction",               { link = "Function"             })
  highlight(0, "adaProcedure",              { link = "Function"             })
  highlight(0, "adaPackage",                { link = "Type"                 })

  highlight(0, "adaString",                 { link = "String"               })
  highlight(0, "adaCharacter",              { link = "String"               })
  highlight(0, "adaNumber",                 { link = "Number"               })
  highlight(0, "adaBoolean",                { link = "Boolean"              })

  highlight(0, "adaOperator",               { link = "Operator"             })
  highlight(0, "adaAssignment",             { link = "Operator"             })
  highlight(0, "adaSign",                   { link = "Operator"             })
  highlight(0, "adaSpecial",                { link = "Special"              })
  highlight(0, "adaAttribute",              { link = "MannydarkFgPurple"    })
  highlight(0, "adaLabel",                  { link = "Label"                })

  highlight(0, "adaComment",                { link = "Comment"              })
  highlight(0, "adaTodo",                   { link = "Todo"                 })
  highlight(0, "adaError",                  { link = "Error"                })
  highlight(0, "adaSpaceError",             { link = "Error"                })
  highlight(0, "adaLineError",              { link = "Error"                })
  highlight(0, "adaPreproc",                { link = "PreProc"              })


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (ada-language-server)

  highlight(0, "@lsp.type.type.ada",        { link = "Type"                 })
  highlight(0, "@lsp.type.class.ada",       { link = "Type"                 })
  highlight(0, "@lsp.type.enum.ada",        { link = "Type"                 })
  highlight(0, "@lsp.type.struct.ada",      { link = "Type"                 })
  highlight(0, "@lsp.type.namespace.ada",   { link = "Type"                 })
  highlight(0, "@lsp.type.enumMember.ada",  { link = "Constant"             })
  highlight(0, "@lsp.type.variable.ada",    { link = "Variable"             })
  highlight(0, "@lsp.type.parameter.ada",   { link = "Variable"             })
  highlight(0, "@lsp.type.property.ada",    { link = "Variable"             })
  highlight(0, "@lsp.type.function.ada",    { link = "Function"             })
  highlight(0, "@lsp.type.method.ada",      { link = "Function"             })
end

return ada
