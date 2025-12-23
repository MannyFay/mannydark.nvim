-------------------------------------------------------------------------------
-- Brainfuck Files
-- Highlighting for .bf, .b, .brainfuck files.
--
-- Brainfuck is an esoteric language with only 8 commands:
--   +  Increment the byte at the data pointer
--   -  Decrement the byte at the data pointer
--   >  Move data pointer right
--   <  Move data pointer left
--   .  Output the byte at the data pointer
--   ,  Input a byte and store at data pointer
--   [  Jump forward to ] if byte at pointer is zero
--   ]  Jump back to [ if byte at pointer is nonzero
--
-- Since Neovim has no built-in Brainfuck support, we provide both
-- filetype detection and syntax highlighting here.
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local brainfuck = {}


-------------------------------------------------------------------------------
-- Syntax Definition
-- Note: Filetype detection is in init.lua (must happen at module load time)

-- Apply syntax to a single buffer:
local function apply_brainfuck_syntax(bufnr)
  vim.api.nvim_buf_call(bufnr, function()
    -- Stop treesitter highlighting if active (no brainfuck parser exists):
    if vim.treesitter.highlighter.active[bufnr] then
      vim.treesitter.stop(bufnr)
    end

    -- Ensure vim syntax is enabled for this buffer
    vim.bo.syntax = "brainfuck"

    vim.cmd("syntax clear")
    vim.cmd("syntax match brainfuckIncrement /+/")
    vim.cmd("syntax match brainfuckDecrement /-/")
    vim.cmd("syntax match brainfuckMoveRight />/")
    vim.cmd("syntax match brainfuckMoveLeft /</")
    vim.cmd("syntax match brainfuckOutput /\\./")
    vim.cmd("syntax match brainfuckInput /,/")
    vim.cmd("syntax match brainfuckLoopStart /\\[/")
    vim.cmd("syntax match brainfuckLoopEnd /\\]/")
    vim.cmd("syntax match brainfuckComment /[^+\\-><.,\\[\\]]/")

    -- Mark syntax as loaded:
    vim.b.current_syntax = "brainfuck"
  end)
end

brainfuck.setupSyntax = function()
  -- Register autocmd for future brainfuck files:
  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("MannydarkBrainfuckSyntax", { clear = true }),
    pattern = "brainfuck",
    callback = function(ev)
      apply_brainfuck_syntax(ev.buf)
    end,
  })

  -- Apply to any already-open brainfuck buffers (delayed to ensure filetype is set):
  vim.defer_fn(function()
    for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_loaded(bufnr) then
        local ft = vim.bo[bufnr].filetype
        if ft == "brainfuck" then
          apply_brainfuck_syntax(bufnr)
        end
      end
    end
  end, 10)
end


-------------------------------------------------------------------------------
-- Highlighting

brainfuck.setupHighlighting = function()
  highlight(0, "brainfuckIncrement", { link = "Delimiter" })  -- +
  highlight(0, "brainfuckDecrement", { link = "Delimiter" })  -- -
  highlight(0, "brainfuckMoveRight", { link = "Function"  })  -- >
  highlight(0, "brainfuckMoveLeft",  { link = "Function"  })  -- <
  highlight(0, "brainfuckOutput",    { link = "String"    })  -- .
  highlight(0, "brainfuckInput",     { link = "String"    })  -- ,
  highlight(0, "brainfuckLoopStart", { link = "Keyword"   })  -- [
  highlight(0, "brainfuckLoopEnd",   { link = "Keyword"   })  -- ]
  highlight(0, "brainfuckComment",   { link = "Comment"   })
end


-------------------------------------------------------------------------------
-- Setup (called from theme.lua)

brainfuck.setup = function()
  brainfuck.setupSyntax()
  brainfuck.setupHighlighting()
end

return brainfuck

