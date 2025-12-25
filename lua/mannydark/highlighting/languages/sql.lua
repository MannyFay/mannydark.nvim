-------------------------------------------------------------------------------
-- SQL (Standard SQL, MySQL, PostgreSQL, SQLite, Oracle, SQL Server)
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local sql       = {}


-------------------------------------------------------------------------------
-- Settings

sql.setupHighlighting = function()
  -- Custom mannydark captures
  highlight(0, "@type.mannydark.sql",            { link = "MannydarkFgTurquoise" })  -- Table/database names
  highlight(0, "@variable.member.mannydark.sql", { link = "MannydarkFgPurple"    })  -- Column names / field references
  highlight(0, "@keyword.mannydark.sql",         { link = "Keyword"              })  -- Keywords (OWNER, ENCODING, etc.)
  highlight(0, "@number.mannydark.sql",          { link = "MannydarkFgGreenLight"})  -- Numbers in ERROR nodes
  highlight(0, "@function.call.mannydark.sql",   { link = "MannydarkFgOrange"    })  -- Function calls

  -- Standard SQL captures
  highlight(0, "@type.sql",                      { link = "MannydarkFgTurquoise" })  -- Object references
  highlight(0, "@type.builtin.sql",              { link = "Keyword"              })  -- Built-in types (SERIAL, BIGINT, UUID, etc.)
  highlight(0, "@attribute.sql",                 { link = "Keyword"              })  -- GENERATED, ALWAYS, etc.
  highlight(0, "@keyword.modifier.sql",          { link = "Keyword"              })  -- CHECK, etc.
  highlight(0, "@variable.member.sql",           { link = "MannydarkFgPurple"    })  -- Fields/columns
  highlight(0, "@function.call.sql",             { link = "MannydarkFgOrange"    })  -- Functions
  highlight(0, "@string.sql",                    { link = "MannydarkFgRedLight"  })  -- Strings
  highlight(0, "@number.sql",                    { link = "MannydarkFgGreenLight"})  -- Numbers
  highlight(0, "@boolean.sql",                   { link = "Keyword"              })  -- TRUE/FALSE
  highlight(0, "@keyword.sql",                   { link = "Keyword"              })  -- Keywords
  highlight(0, "@comment.sql",                   { link = "MannydarkFgRed"       })  -- Comments
  highlight(0, "@operator.sql",                  { link = "MannydarkFgWhite"     })  -- Operators
  highlight(0, "@variable.parameter.sql",        { link = "MannydarkFgPurple"    })  -- Parameters

  -----------------------------------------------------------------------------
  -- Extmarks for elements not captured by treesitter (parser errors)

  local ns = vim.api.nvim_create_namespace("mannydark_sql")

  local function highlight_sql_extmarks(bufnr)
    vim.api.nvim_buf_clear_namespace(bufnr, ns, 0, -1)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

    for lnum, line in ipairs(lines) do
      -- Skip comment lines
      if line:match("^%s*%-%-") then
        goto continue
      end

      -------------------------------------------------------------------------
      -- Helper: check if a column position is inside a quoted string
      local function is_inside_string(str, pos)
        local before = str:sub(1, pos - 1)
        local quote_count = select(2, before:gsub("'", ""))
        return (quote_count % 2) == 1
      end

      -------------------------------------------------------------------------
      -- General SQL keywords (for DML statements)
      local sql_keywords = {
        "INSERT", "INTO", "VALUES", "RETURNING", "SELECT", "FROM", "WHERE", "AND", "OR", "NOT",
        "UPDATE", "SET", "DELETE", "JOIN", "LEFT", "RIGHT", "INNER", "OUTER", "FULL", "CROSS",
        "ON", "CONFLICT", "DO", "NOTHING", "EXCLUDED", "CONSTRAINT", "AS", "DISTINCT", "ALL",
        "ORDER", "BY", "GROUP", "HAVING", "LIMIT", "OFFSET", "UNION", "INTERSECT", "EXCEPT",
        "CASE", "WHEN", "THEN", "ELSE", "END", "IS", "NULL", "IN", "BETWEEN", "LIKE", "ILIKE",
        "EXISTS", "ANY", "SOME", "TRUE", "FALSE", "CURRENT_TIMESTAMP", "CURRENT_DATE", "CURRENT_TIME",
        "WITH", "RECURSIVE", "LATERAL", "ONLY", "ASC", "DESC", "NULLS", "FIRST", "LAST",
        "FOR", "SHARE", "NOWAIT", "SKIP", "LOCKED", "ESCAPE", "USING",
        "TRUNCATE", "TABLE", "RESTART", "IDENTITY", "CASCADE", "RESTRICT", "CONTINUE",
        "RETURNS", "LANGUAGE", "DEFAULT", "FUNCTION", "PROCEDURE", "REPLACE", "BEGIN", "END",
        "RETURN", "DECLARE", "EXCEPTION", "RAISE", "NOTICE", "PERFORM", "EXECUTE", "IMMUTABLE",
        "STABLE", "VOLATILE", "STRICT", "SECURITY", "DEFINER", "INVOKER", "COST", "ROWS",
        "CALL", "IF", "THEN", "ELSIF", "LOOP", "WHILE", "EXIT", "FOREACH",
        "OUT", "INOUT", "VARIADIC", "NEW", "OLD", "TG_OP", "TG_NAME", "TG_TABLE_NAME", "TRIGGER",
        "DROP", "ALTER", "RENAME", "TO", "ADD", "COLUMN", "BEFORE", "AFTER", "INSTEAD", "EACH", "ROW",
        "TRANSACTION", "WORK", "START", "ISOLATION", "LEVEL", "SERIALIZABLE", "REPEATABLE", "READ",
        "COMMITTED", "UNCOMMITTED", "SAVEPOINT", "ROLLBACK", "RELEASE", "COMMIT", "DEFERRABLE",
        "ROLE", "LOGIN", "NOLOGIN", "PASSWORD", "SUPERUSER", "CREATEDB", "CREATEROLE", "INHERIT",
        "NOINHERIT", "REPLICATION", "BYPASSRLS", "CONNECTION", "VALID", "UNTIL", "GRANT", "REVOKE",
        "PRIVILEGES", "USAGE", "REFERENCES", "SCHEMA", "DATABASE", "SEQUENCE", "TABLES", "SEQUENCES",
        "FUNCTIONS", "TYPES", "ENABLE", "DISABLE", "FORCE", "POLICY", "PUBLIC", "PERMISSIVE",
        "RESTRICTIVE", "CHECK",
        "EXPLAIN", "ANALYZE", "BUFFERS", "FORMAT", "JSON", "TEXT", "XML", "YAML", "VERBOSE",
        "COSTS", "SETTINGS", "WAL", "TIMING", "SUMMARY",
        "CLUSTER", "REINDEX", "VACUUM", "INDEX",
        "DEFERRABLE", "INITIALLY", "DEFERRED", "IMMEDIATE", "CONSTRAINTS", "KEY", "FOREIGN",
        "PRIMARY", "UNIQUE", "TYPE", "EXTENSION",
        "PREPARE", "EXECUTE", "DEALLOCATE",
        "CURSOR", "FETCH", "NEXT", "PRIOR", "ABSOLUTE", "RELATIVE", "FORWARD", "BACKWARD",
        "MOVE", "CLOSE", "SCROLL", "NO",
        "COPY", "STDOUT", "STDIN", "CSV", "HEADER", "DELIMITER", "QUOTE", "ENCODING", "FREEZE",
        "BINARY", "OIDS", "FORCE_QUOTE", "FORCE_NOT_NULL", "FORCE_NULL",
        "LOCK", "MODE", "ACCESS", "EXCLUSIVE",
        "COMMENT"
      }
      for _, kw in ipairs(sql_keywords) do
        local pos = 1
        while true do
          local ks, ke = line:find(kw, pos)
          if not ks then break end
          local before = ks > 1 and line:sub(ks - 1, ks - 1) or " "
          local after = ke < #line and line:sub(ke + 1, ke + 1) or " "
          if not before:match("[%w_]") and not after:match("[%w_]") then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ks - 1, {
              end_col = ke, hl_group = "Keyword", priority = 500,
            })
          end
          pos = ke + 1
        end
      end

      -------------------------------------------------------------------------
      -- INSERT INTO table (columns) - table turquoise, columns purple
      if line:match("INSERT%s+INTO") then
        local tbl = line:match("INTO%s+([%w_]+)")
        if tbl then
          local ts, te = line:find(tbl, line:find("INTO"), true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- TRUNCATE TABLE table1, table2 - table names turquoise
      if line:match("TRUNCATE%s+TABLE") then
        local after_table = line:match("TABLE%s+(.-)%s+RESTART") or line:match("TABLE%s+(.-)%s+CASCADE")
          or line:match("TABLE%s+(.-)%s+RESTRICT") or line:match("TABLE%s+(.-)%;?$")
        if after_table then
          local search_pos = line:find("TABLE") + 5
          for tbl in after_table:gmatch("([%w_]+)") do
            local ts, te = line:find(tbl, search_pos, true)
            if ts then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
                end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
              search_pos = te + 1
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- FROM/JOIN table - table name turquoise (INTO handled above, UPDATE has its own logic)
      for _, kw in ipairs({"FROM", "JOIN"}) do
        local pattern = kw .. "%s+([%w_]+)"
        local search_start = 1
        for tbl in line:gmatch(pattern) do
          if tbl and not tbl:match("^%u+$") then -- skip all-uppercase (likely keywords)
            local kw_pos = line:find(kw, search_start)
            if kw_pos then
              local ts, te = line:find(tbl, kw_pos, true)
              if ts and not is_inside_string(line, ts) then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
                  end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
                })
                search_start = te + 1
              end
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- Strings in single quotes
      local str_pos = 1
      while true do
        local ss, se = line:find("'[^']*'", str_pos)
        if not ss then break end
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ss - 1, {
          end_col = se, hl_group = "MannydarkFgRedLight", priority = 500,
        })
        str_pos = se + 1
      end

      -------------------------------------------------------------------------
      -- Strings/identifiers in double quotes
      local dq_pos = 1
      while true do
        local ds, de = line:find('"[^"]*"', dq_pos)
        if not ds then break end
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ds - 1, {
          end_col = de, hl_group = "MannydarkFgRedLight", priority = 500,
        })
        dq_pos = de + 1
      end

      -------------------------------------------------------------------------
      -- Column names in parentheses after INSERT INTO table (not VALUES line)
      -- Pattern: only match simple column lists like (col1, col2) without quotes
      if line:match("INSERT") and line:match("%([%w_,%s]+%)") then
        local ps, pe = line:find("%b()")
        if ps then
          local paren_content = line:sub(ps + 1, pe - 1)
          -- Skip if content contains quotes (it's a VALUES clause)
          if not paren_content:match("'") then
            local search_pos = ps
            for col in paren_content:gmatch("([%w_]+)") do
              local skip = false
              for _, kw in ipairs(sql_keywords) do
                if col:upper() == kw then skip = true break end
              end
              if not skip and not col:match("^%d") then
                local cs, ce = line:find(col, search_pos, true)
                if cs and cs < pe then
                  vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                    end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
                  })
                  search_pos = ce + 1
                end
              end
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- Column names after SELECT, RETURNING (comma-separated)
      -- Handle both "SELECT cols FROM" on same line and "SELECT cols" with FROM on next line
      local select_cols = line:match("SELECT%s+(.-)%s+FROM")
        or line:match("SELECT%s+(.-)$")  -- SELECT without FROM (continues on next line)
        or line:match("RETURNING%s+(.-)%;?$")
      if select_cols and not line:match("^%s*FROM") then
        local start_pos = line:find("SELECT") or line:find("RETURNING") or 1
        for col in select_cols:gmatch("([%w_]+)") do
          local skip = false
          for _, kw in ipairs(sql_keywords) do
            if col:upper() == kw then skip = true break end
          end
          if not skip then
            local cs, ce = line:find(col, start_pos, true)
            if cs and not is_inside_string(line, cs) then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
              })
              start_pos = ce + 1
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- column = value patterns (column names purple)
      local eq_search = 1
      for col in line:gmatch("([%w_]+)%s*=") do
        local skip = false
        for _, kw in ipairs(sql_keywords) do
          if col:upper() == kw then skip = true break end
        end
        if not skip then
          local cs, ce = line:find(col, eq_search, true)
          if cs and not is_inside_string(line, cs) then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
              end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
            })
            eq_search = ce + 1
          end
        end
      end

      -------------------------------------------------------------------------
      -- ORDER BY col1, col2 [ASC|DESC] - columns are purple
      if line:match("ORDER%s+BY") then
        local by_pos = line:find("BY", line:find("ORDER"))
        local after_by = line:match("ORDER%s+BY%s+(.+)$")
        if after_by and by_pos then
          local search_pos = by_pos + 2
          for col in after_by:gmatch("([%w_]+)") do
            local skip = false
            for _, kw in ipairs(sql_keywords) do
              if col:upper() == kw then skip = true break end
            end
            -- Also skip numbers
            if col:match("^%d+$") then skip = true end
            if not skip then
              local cs, ce = line:find(col, search_pos, true)
              if cs then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                  end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
                })
                search_pos = ce + 1
              end
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- GROUP BY col1, col2 - columns are purple
      if line:match("GROUP%s+BY") then
        local after_by = line:match("GROUP%s+BY%s+(.+)$")
        if after_by then
          local search_pos = line:find("BY", line:find("GROUP")) + 2
          for col in after_by:gmatch("([%w_]+)") do
            local skip = false
            for _, kw in ipairs(sql_keywords) do
              if col:upper() == kw then skip = true break end
            end
            if not skip then
              local cs, ce = line:find(col, search_pos, true)
              if cs then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                  end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
                })
                search_pos = ce + 1
              end
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- CREATE FUNCTION/PROCEDURE name(params) - function name turquoise, params purple, types keyword
      if line:match("CREATE%s+.-FUNCTION") or line:match("CREATE%s+.-PROCEDURE") then
        -- Function/procedure name
        local func_name = line:match("FUNCTION%s+([%w_]+)") or line:match("PROCEDURE%s+([%w_]+)")
        if func_name then
          local fn_pos = line:find("FUNCTION") or line:find("PROCEDURE")
          local fs, fe = line:find(func_name, fn_pos, true)
          if fs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, fs - 1, {
              end_col = fe, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
        -- Parameters in parentheses
        local paren_start, paren_end = line:find("%b()")
        if paren_start then
          local params = line:sub(paren_start + 1, paren_end - 1)
          local search_pos = paren_start
          -- Skip keywords and types that shouldn't be treated as param names
          local skip_words = {
            OUT = true, IN = true, INOUT = true, VARIADIC = true, DEFAULT = true,
            DECIMAL = true, INTEGER = true, INT = true, BIGINT = true, SMALLINT = true,
            TEXT = true, VARCHAR = true, CHAR = true, BOOLEAN = true, BOOL = true,
            NUMERIC = true, REAL = true, FLOAT = true, DOUBLE = true, DATE = true,
            TIME = true, TIMESTAMP = true, INTERVAL = true, UUID = true, JSON = true,
            JSONB = true, BYTEA = true, ARRAY = true, VOID = true, TRIGGER = true,
            RECORD = true, SETOF = true, TABLE = true
          }
          -- Match "name TYPE" patterns
          for param_name, param_type in params:gmatch("([%w_]+)%s+([%w_]+)") do
            if not skip_words[param_name:upper()] then
              local ps, pe = line:find(param_name, search_pos, true)
              if ps and ps < paren_end then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ps - 1, {
                  end_col = pe, hl_group = "MannydarkFgPurple", priority = 500,
                })
                search_pos = pe + 1
                -- Type
                local ts, te = line:find(param_type, search_pos, true)
                if ts and ts < paren_end then
                  vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
                    end_col = te, hl_group = "Keyword", priority = 500,
                  })
                  search_pos = te + 1
                end
              end
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- Function/procedure parameter lines: IN/OUT/INOUT name TYPE
      local param_prefix = line:match("^%s*(IN)%s+") or line:match("^%s*(OUT)%s+") or line:match("^%s*(INOUT)%s+")
      if param_prefix then
        -- Highlight IN/OUT/INOUT
        local ps, pe = line:find(param_prefix)
        if ps then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ps - 1, {
            end_col = pe, hl_group = "Keyword", priority = 500,
          })
        end
        -- Match parameter name and type
        local param_name, param_type = line:match(param_prefix .. "%s+([%w_]+)%s+([%w_%(%)]+)")
        if param_name and param_type then
          local ns_s, ns_e = line:find(param_name, pe, true)
          if ns_s then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ns_s - 1, {
              end_col = ns_e, hl_group = "MannydarkFgPurple", priority = 500,
            })
            -- Type (handle VARCHAR(20) style)
            local base_type = param_type:match("([%w_]+)")
            local ts, te = line:find(base_type, ns_e, true)
            if ts then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
                end_col = te, hl_group = "Keyword", priority = 500,
              })
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- CALL procedure_name(...) - procedure name is turquoise
      if line:match("CALL%s+") then
        local proc_name = line:match("CALL%s+([%w_]+)")
        if proc_name then
          local cs = line:find("CALL")
          local ps, pe = line:find(proc_name, cs, true)
          if ps then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ps - 1, {
              end_col = pe, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- CREATE ROLE role_name / ALTER ROLE role_name
      if line:match("CREATE%s+ROLE") or line:match("ALTER%s+ROLE") or line:match("DROP%s+ROLE") then
        local role_name = line:match("ROLE%s+([%w_]+)")
        if role_name then
          local rp = line:find("ROLE")
          local rs, re = line:find(role_name, rp, true)
          if rs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, rs - 1, {
              end_col = re, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- IN SCHEMA schema_name
      local in_schema = line:match("IN%s+SCHEMA%s+([%w_]+)")
      if in_schema then
        local schema_pos = line:find("SCHEMA", line:find("IN"))
        local ss, se = line:find(in_schema, schema_pos, true)
        if ss then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ss - 1, {
            end_col = se, hl_group = "MannydarkFgTurquoise", priority = 500,
          })
        end
      end

      -------------------------------------------------------------------------
      -- ALTER ROLE role SET setting TO value1, value2
      if line:match("ALTER%s+ROLE") and line:match("SET%s+") then
        -- Setting name (like search_path)
        local setting = line:match("SET%s+([%w_]+)")
        if setting and setting ~= "TRANSACTION" then
          local set_pos = line:find("SET")
          local sts, ste = line:find(setting, set_pos, true)
          if sts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, sts - 1, {
              end_col = ste, hl_group = "MannydarkFgPurple", priority = 500,
            })
          end
        end
        -- Values after TO
        local to_values = line:match("TO%s+(.+)%;?$")
        if to_values then
          local to_pos = line:find("%sTO%s")
          if to_pos then
            local search_pos = to_pos + 3
            for val in to_values:gmatch("([%w_]+)") do
              local vs, ve = line:find(val, search_pos, true)
              if vs then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, vs - 1, {
                  end_col = ve, hl_group = "MannydarkFgTurquoise", priority = 500,
                })
                search_pos = ve + 1
              end
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- GRANT ... TO role_name / REVOKE ... FROM role_name
      if line:match("GRANT%s+") or line:match("REVOKE%s+") then
        -- Role name after TO/FROM (but not inside CREATE POLICY which has its own handling)
        if not line:match("POLICY") then
          local to_match_start = 1
          while true do
            local to_s, to_e = line:find("%sTO%s", to_match_start)
            if not to_s then break end
            local to_role = line:match("([%w_]+)", to_e + 1)
            if to_role and to_role ~= "PUBLIC" then
              local rs, re = line:find(to_role, to_e, true)
              if rs then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, rs - 1, {
                  end_col = re, hl_group = "MannydarkFgTurquoise", priority = 500,
                })
              end
            end
            to_match_start = to_e + 1
          end
        end
        local from_role = line:match("FROM%s+([%w_]+)")
        if from_role and from_role ~= "PUBLIC" then
          local from_pos = line:find("%sFROM%s")
          if from_pos then
            local rs, re = line:find(from_role, from_pos, true)
            if rs then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, rs - 1, {
                end_col = re, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
            end
          end
        end
        -- Object name after ON (table, schema, function, database)
        local obj_name = line:match("ON%s+SCHEMA%s+([%w_]+)") or line:match("ON%s+DATABASE%s+([%w_]+)")
          or line:match("ON%s+FUNCTION%s+([%w_]+)") or line:match("ON%s+SEQUENCE%s+([%w_]+)")
          or line:match("ON%s+([%w_]+)") -- simple table name
        if obj_name and not obj_name:match("^ALL$") then
          local on_pos = line:find("%sON%s")
          if on_pos then
            local os, oe = line:find(obj_name, on_pos, true)
            if os then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, os - 1, {
                end_col = oe, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- CREATE POLICY policy_name ON table_name ... TO role_name
      if line:match("CREATE%s+POLICY") or line:match("ALTER%s+POLICY") or line:match("DROP%s+POLICY") then
        local policy_name = line:match("POLICY%s+([%w_]+)")
        if policy_name then
          local pp = line:find("POLICY")
          local ps, pe = line:find(policy_name, pp, true)
          if ps then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ps - 1, {
              end_col = pe, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
        -- Table name after ON
        local table_name = line:match("ON%s+([%w_]+)")
        if table_name then
          local on_pos = line:find("%sON%s")
          if on_pos then
            local ts, te = line:find(table_name, on_pos, true)
            if ts then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
                end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
            end
          end
        end
        -- Role name after TO
        local to_role = line:match("%sTO%s+([%w_]+)")
        if to_role and to_role ~= "PUBLIC" then
          local to_pos = line:find("%sTO%s")
          if to_pos then
            local rs, re = line:find(to_role, to_pos, true)
            if rs then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, rs - 1, {
                end_col = re, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- Standalone TO role_name line (multi-line POLICY/GRANT statements)
      -- Pattern: line starts with whitespace, then TO, then role name
      local standalone_to_role = line:match("^%s+TO%s+([%w_]+)")
      if standalone_to_role then
        local skip = false
        for _, kw in ipairs(sql_keywords) do
          if standalone_to_role:upper() == kw then skip = true break end
        end
        if not skip then
          local to_pos = line:find("TO")
          local rs, re = line:find(standalone_to_role, to_pos, true)
          if rs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, rs - 1, {
              end_col = re, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- ANALYZE table_name [(columns)] / VACUUM table_name
      if line:match("^%s*ANALYZE%s+") or line:match("^%s*VACUUM%s+") then
        local cmd = line:match("^%s*(ANALYZE)") or line:match("^%s*(VACUUM)")
        if cmd then
          local after_cmd = line:match(cmd .. "%s+([%w_]+)")
          if after_cmd then
            -- Skip if it's a keyword like VERBOSE
            local skip = false
            for _, kw in ipairs(sql_keywords) do
              if after_cmd:upper() == kw then skip = true break end
            end
            if not skip then
              local cmd_pos = line:find(cmd)
              local ts, te = line:find(after_cmd, cmd_pos + #cmd, true)
              if ts then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
                  end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
                })
              end
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- CLUSTER table_name USING index_name
      if line:match("^%s*CLUSTER%s+") then
        local table_name = line:match("CLUSTER%s+([%w_]+)")
        if table_name and table_name ~= "VERBOSE" then
          local cp = line:find("CLUSTER")
          local ts, te = line:find(table_name, cp, true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
          -- Index name after USING
          local idx_name = line:match("USING%s+([%w_]+)")
          if idx_name then
            local up = line:find("USING")
            local is, ie = line:find(idx_name, up, true)
            if is then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, is - 1, {
                end_col = ie, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- REINDEX TABLE/INDEX/DATABASE/SCHEMA name
      if line:match("^%s*REINDEX%s+") then
        local obj_type, obj_name = line:match("REINDEX%s+(%u+)%s+([%w_]+)")
        if obj_name then
          local rp = line:find("REINDEX")
          local ts, te = line:find(obj_name, rp, true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- ALTER TABLE table ADD/DROP CONSTRAINT constraint_name
      if line:match("ALTER%s+TABLE") and line:match("CONSTRAINT") then
        -- Table name after ALTER TABLE
        local table_name = line:match("ALTER%s+TABLE%s+([%w_]+)")
        if table_name then
          local atp = line:find("TABLE")
          local ts, te = line:find(table_name, atp, true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
        -- Constraint name after CONSTRAINT (skip IF, EXISTS)
        local constraint_name = line:match("CONSTRAINT%s+IF%s+EXISTS%s+([%w_]+)")
          or line:match("CONSTRAINT%s+([%w_]+)")
        if constraint_name and constraint_name ~= "IF" then
          local cp = line:find("CONSTRAINT")
          local cs, ce = line:find(constraint_name, cp, true)
          if cs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
              end_col = ce, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
        -- Column names in CHECK (col ...) or UNIQUE (col) or FOREIGN KEY (col)
        local paren_start, paren_end = line:find("%b()")
        if paren_start then
          local paren_content = line:sub(paren_start + 1, paren_end - 1)
          local search_pos = paren_start
          for col in paren_content:gmatch("([%w_]+)") do
            local skip = false
            for _, kw in ipairs(sql_keywords) do
              if col:upper() == kw then skip = true break end
            end
            -- Skip numbers
            if col:match("^%d") then skip = true end
            if not skip then
              local cols, cole = line:find(col, search_pos, true)
              if cols and cols < paren_end then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cols - 1, {
                  end_col = cole, hl_group = "MannydarkFgPurple", priority = 500,
                })
                search_pos = cole + 1
              end
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- Standalone FOREIGN KEY (col) line (multi-line constraint)
      if line:match("^%s+FOREIGN%s+KEY%s*%(") then
        local paren_start, paren_end = line:find("%b()")
        if paren_start then
          local paren_content = line:sub(paren_start + 1, paren_end - 1)
          local search_pos = paren_start
          for col in paren_content:gmatch("([%w_]+)") do
            local cols, cole = line:find(col, search_pos, true)
            if cols and cols < paren_end then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cols - 1, {
                end_col = cole, hl_group = "MannydarkFgPurple", priority = 500,
              })
              search_pos = cole + 1
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- SET CONSTRAINTS constraint_name IMMEDIATE/DEFERRED
      if line:match("^%s*SET%s+CONSTRAINTS%s+") then
        local constraint_name = line:match("CONSTRAINTS%s+([%w_]+)")
        if constraint_name and constraint_name ~= "ALL" then
          local cp = line:find("CONSTRAINTS")
          local cs, ce = line:find(constraint_name, cp, true)
          if cs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
              end_col = ce, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- ALTER TABLE ... ADD/DROP/ALTER/RENAME COLUMN
      if line:match("ALTER%s+TABLE") and line:match("COLUMN") then
        -- Table name after ALTER TABLE
        local table_name = line:match("ALTER%s+TABLE%s+([%w_]+)")
        if table_name then
          local atp = line:find("TABLE")
          local ts, te = line:find(table_name, atp, true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
        -- Column name after COLUMN
        local col_name = line:match("COLUMN%s+([%w_]+)")
        if col_name then
          local colp = line:find("COLUMN")
          local cs, ce = line:find(col_name, colp, true)
          if cs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
              end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
            })
          end
        end
        -- New column name after TO (for RENAME COLUMN old TO new)
        if line:match("RENAME%s+COLUMN") then
          local new_name = line:match("%sTO%s+([%w_]+)")
          if new_name then
            local top = line:find("%sTO%s")
            local ns_s, ns_e = line:find(new_name, top, true)
            if ns_s then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ns_s - 1, {
                end_col = ns_e, hl_group = "MannydarkFgPurple", priority = 500,
              })
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- ALTER TABLE ... RENAME TO new_table_name (without COLUMN)
      if line:match("ALTER%s+TABLE") and line:match("RENAME%s+TO") and not line:match("COLUMN") then
        -- Table name after ALTER TABLE
        local table_name = line:match("ALTER%s+TABLE%s+([%w_]+)")
        if table_name then
          local atp = line:find("TABLE")
          local ts, te = line:find(table_name, atp, true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
        -- New table name after RENAME TO
        local new_name = line:match("RENAME%s+TO%s+([%w_]+)")
        if new_name then
          local rp = line:find("RENAME")
          local ns_s, ns_e = line:find(new_name, rp, true)
          if ns_s then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ns_s - 1, {
              end_col = ns_e, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- PREPARE statement_name (types) AS ...
      if line:match("^%s*PREPARE%s+") then
        local stmt_name = line:match("PREPARE%s+([%w_]+)")
        if stmt_name then
          local pp = line:find("PREPARE")
          local ss, se = line:find(stmt_name, pp, true)
          if ss then
            -- Use orange (function) since prepared statements are function-like
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ss - 1, {
              end_col = se, hl_group = "MannydarkFgOrange", priority = 500,
            })
          end
        end
        -- Parameter types in parentheses
        local paren_start, paren_end = line:find("%b()")
        if paren_start then
          local types_content = line:sub(paren_start + 1, paren_end - 1)
          local search_pos = paren_start
          for typ in types_content:gmatch("([%w_]+)") do
            local ts, te = line:find(typ, search_pos, true)
            if ts and ts < paren_end then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
                end_col = te, hl_group = "Keyword", priority = 500,
              })
              search_pos = te + 1
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- EXECUTE statement_name(args) / DEALLOCATE statement_name
      if line:match("^%s*EXECUTE%s+") and not line:match("^%s*EXECUTE%s+PROCEDURE") then
        local stmt_name = line:match("EXECUTE%s+([%w_]+)")
        if stmt_name then
          local ep = line:find("EXECUTE")
          local ss, se = line:find(stmt_name, ep, true)
          if ss then
            -- Use orange (function) since it's called like a function
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ss - 1, {
              end_col = se, hl_group = "MannydarkFgOrange", priority = 500,
            })
          end
        end
      end

      if line:match("^%s*DEALLOCATE%s+") then
        local stmt_name = line:match("DEALLOCATE%s+([%w_]+)")
        if stmt_name and stmt_name ~= "ALL" then
          local dp = line:find("DEALLOCATE")
          local ss, se = line:find(stmt_name, dp, true)
          if ss then
            -- Use orange (function) since prepared statements are function-like
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ss - 1, {
              end_col = se, hl_group = "MannydarkFgOrange", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- Parameter placeholders $1, $2, etc.
      local param_pos = 1
      while true do
        local ps, pe = line:find("%$%d+", param_pos)
        if not ps then break end
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ps - 1, {
          end_col = pe, hl_group = "MannydarkFgPurple", priority = 500,
        })
        param_pos = pe + 1
      end

      -------------------------------------------------------------------------
      -- DECLARE cursor_name CURSOR FOR ...
      if line:match("DECLARE%s+[%w_]+%s+CURSOR") then
        local cursor_name = line:match("DECLARE%s+([%w_]+)%s+CURSOR")
        if cursor_name then
          local dp = line:find("DECLARE")
          local cs, ce = line:find(cursor_name, dp, true)
          if cs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
              end_col = ce, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- FETCH ... FROM cursor_name / MOVE ... IN cursor_name / CLOSE cursor_name
      if line:match("^%s*FETCH%s+") then
        local cursor_name = line:match("FROM%s+([%w_]+)")
        if cursor_name then
          local fp = line:find("FROM")
          local cs, ce = line:find(cursor_name, fp, true)
          if cs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
              end_col = ce, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      if line:match("^%s*MOVE%s+") then
        local cursor_name = line:match("IN%s+([%w_]+)")
        if cursor_name then
          local ip = line:find("%sIN%s")
          if ip then
            local cs, ce = line:find(cursor_name, ip, true)
            if cs then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                end_col = ce, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
            end
          end
        end
      end

      if line:match("^%s*CLOSE%s+") then
        local cursor_name = line:match("CLOSE%s+([%w_]+)")
        if cursor_name then
          local cp = line:find("CLOSE")
          local cs, ce = line:find(cursor_name, cp, true)
          if cs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
              end_col = ce, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- COPY table_name TO/FROM ... / \copy table_name TO/FROM ...
      if line:match("^%s*COPY%s+") or line:match("^%s*\\copy%s+") then
        -- Highlight \copy as keyword (psql meta-command)
        local bcopy_s, bcopy_e = line:find("\\copy")
        if bcopy_s then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, bcopy_s - 1, {
            end_col = bcopy_e, hl_group = "Keyword", priority = 500,
          })
        end
        local table_name = line:match("COPY%s+([%w_]+)") or line:match("\\copy%s+([%w_]+)")
        if table_name then
          local cp = line:find("COPY") or line:find("\\copy")
          if cp then
            local ts, te = line:find(table_name, cp, true)
            if ts then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
                end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- LOCK TABLE table_name IN ... MODE
      if line:match("^%s*LOCK%s+TABLE%s+") then
        local table_name = line:match("TABLE%s+([%w_]+)")
        if table_name then
          local tp = line:find("TABLE")
          local ts, te = line:find(table_name, tp, true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- COMMENT ON object_type object_name IS 'comment'
      if line:match("^%s*COMMENT%s+ON%s+") then
        -- COMMENT ON COLUMN table.column
        if line:match("ON%s+COLUMN%s+") then
          local tbl, col = line:match("COLUMN%s+([%w_]+)%.([%w_]+)")
          if tbl and col then
            local cp = line:find("COLUMN")
            local ts, te = line:find(tbl, cp, true)
            if ts then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
                end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
              local cs, ce = line:find(col, te, true)
              if cs then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                  end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
                })
              end
            end
          end
        -- COMMENT ON FUNCTION/PROCEDURE name
        elseif line:match("ON%s+FUNCTION%s+") or line:match("ON%s+PROCEDURE%s+") then
          local func_name = line:match("FUNCTION%s+([%w_]+)") or line:match("PROCEDURE%s+([%w_]+)")
          if func_name then
            local fp = line:find("FUNCTION") or line:find("PROCEDURE")
            local fs, fe = line:find(func_name, fp, true)
            if fs then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, fs - 1, {
                end_col = fe, hl_group = "MannydarkFgOrange", priority = 500,
              })
            end
          end
        -- COMMENT ON TABLE/INDEX/SCHEMA/DATABASE/etc. name
        else
          local obj_type, obj_name = line:match("ON%s+(%u+)%s+([%w_]+)")
          if obj_name then
            local op = line:find(obj_type, line:find("ON"))
            local os, oe = line:find(obj_name, op, true)
            if os then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, os - 1, {
                end_col = oe, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- ALTER TABLE table_name ENABLE/DISABLE ...
      if line:match("ALTER%s+TABLE") and (line:match("ENABLE") or line:match("DISABLE")) then
        local table_name = line:match("TABLE%s+([%w_]+)")
        if table_name then
          local tp = line:find("TABLE")
          local ts, te = line:find(table_name, tp, true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- SAVEPOINT name / ROLLBACK TO SAVEPOINT name / RELEASE SAVEPOINT name
      if line:match("SAVEPOINT%s+") then
        local sp_name = line:match("SAVEPOINT%s+([%w_]+)")
        if sp_name then
          local sp_pos = line:find("SAVEPOINT")
          local ns_s, ns_e = line:find(sp_name, sp_pos, true)
          if ns_s then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ns_s - 1, {
              end_col = ns_e, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- DROP TRIGGER [IF EXISTS] trigger_name ON table_name
      if line:match("DROP%s+TRIGGER") then
        local trigger_name = line:match("EXISTS%s+([%w_]+)") or line:match("TRIGGER%s+([%w_]+)")
        if trigger_name and trigger_name ~= "IF" then
          local ts, te = line:find(trigger_name, line:find("TRIGGER"), true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
        local table_name = line:match("ON%s+([%w_]+)")
        if table_name then
          local on_pos = line:find("ON%s+[%w_]")
          if on_pos then
            local tbs, tbe = line:find(table_name, on_pos, true)
            if tbs then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, tbs - 1, {
                end_col = tbe, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- CREATE TRIGGER trigger_name ... ON table_name
      if line:match("CREATE%s+.-TRIGGER") then
        local trigger_name = line:match("TRIGGER%s+([%w_]+)")
        if trigger_name then
          local ts, te = line:find(trigger_name, line:find("TRIGGER"), true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
        local table_name = line:match("ON%s+([%w_]+)")
        if table_name then
          local on_pos = line:find("ON%s+[%w_]")
          if on_pos then
            local tbs, tbe = line:find(table_name, on_pos, true)
            if tbs then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, tbs - 1, {
                end_col = tbe, hl_group = "MannydarkFgTurquoise", priority = 500,
              })
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- NEW.field and OLD.field - field names are purple
      for prefix in line:gmatch("(NEW)%.") do
        local ps, pe = line:find(prefix .. "%.")
        if ps then
          local field = line:match("NEW%.([%w_]+)", ps)
          if field then
            local fs, fe = line:find(field, pe, true)
            if fs then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, fs - 1, {
                end_col = fe, hl_group = "MannydarkFgPurple", priority = 500,
              })
            end
          end
        end
      end
      for prefix in line:gmatch("(OLD)%.") do
        local ps, pe = line:find(prefix .. "%.")
        if ps then
          local field = line:match("OLD%.([%w_]+)", ps)
          if field then
            local fs, fe = line:find(field, pe, true)
            if fs then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, fs - 1, {
                end_col = fe, hl_group = "MannydarkFgPurple", priority = 500,
              })
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- PL/pgSQL variable assignment: variable := value
      local assign_var = line:match("^%s*([%w_]+)%s*:=")
      if assign_var then
        local skip = false
        for _, kw in ipairs(sql_keywords) do
          if assign_var:upper() == kw then skip = true break end
        end
        if not skip then
          local vs, ve = line:find(assign_var)
          if vs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, vs - 1, {
              end_col = ve, hl_group = "MannydarkFgPurple", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- IF variable IS / IF variable = / IF NOT variable
      if line:match("^%s*IF%s+") then
        local if_var = line:match("IF%s+([%w_]+)%s+IS") or line:match("IF%s+([%w_]+)%s*[=<>]")
          or line:match("IF%s+NOT%s+([%w_]+)")
        if if_var then
          local skip = false
          for _, kw in ipairs(sql_keywords) do
            if if_var:upper() == kw then skip = true break end
          end
          if not skip then
            local is, ie = line:find(if_var, line:find("IF"), true)
            if is then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, is - 1, {
                end_col = ie, hl_group = "MannydarkFgPurple", priority = 500,
              })
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- RETURNS type - type is keyword
      if line:match("RETURNS%s+") then
        local ret_type = line:match("RETURNS%s+([%w_]+)")
        if ret_type then
          local rs = line:find("RETURNS")
          local ts, te = line:find(ret_type, rs, true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "Keyword", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- LANGUAGE name - language name is keyword (plpgsql, sql, etc.)
      if line:match("LANGUAGE%s+") then
        local lang = line:match("LANGUAGE%s+([%w_]+)")
        if lang then
          local ls = line:find("LANGUAGE")
          local ts, te = line:find(lang, ls, true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "Keyword", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- CTE names: WITH [RECURSIVE] name AS ( or , name AS ( - turquoise
      local cte_name = line:match("WITH%s+([%w_]+)%s+AS%s*%(")
        or line:match("WITH%s+RECURSIVE%s+([%w_]+)%s+AS%s*%(")
        or line:match("^%s*([%w_]+)%s+AS%s*%($")
        or line:match("%),%s*([%w_]+)%s+AS%s*%(")
        or line:match("^([%w_]+)%s+AS%s*%($")
      if cte_name and cte_name ~= "RECURSIVE" then
        local search_start = line:find("WITH") or 1
        local as, ae = line:find(cte_name, search_start, true)
        if as then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, as - 1, {
            end_col = ae, hl_group = "MannydarkFgTurquoise", priority = 500,
          })
        end
      end

      -------------------------------------------------------------------------
      -- Subquery alias: ) alias_name or ) AS alias_name - turquoise
      -- Also handles ) AS alias(col1 TYPE, col2 TYPE, ...) for CROSSTAB etc.
      local subquery_alias = line:match("%)%s+AS%s+([%w_]+)") or line:match("%)%s+([%w_]+)%;?$") or line:match("%)%s+([%w_]+)%s*,")
      if subquery_alias and not subquery_alias:match("^%u+$") then -- skip keywords
        local paren_pos = line:find("%)")
        if paren_pos then
          local as, ae = line:find(subquery_alias, paren_pos, true)
          if as then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, as - 1, {
              end_col = ae, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
            -- Check for column definitions: alias(col1 TYPE, col2 TYPE, ...)
            local col_def_start, col_def_end = line:find("%b()", ae)
            if col_def_start then
              local col_defs = line:sub(col_def_start + 1, col_def_end - 1)
              local search_pos = col_def_start
              -- Match pairs of "name TYPE"
              for col_name, col_type in col_defs:gmatch("([%w_]+)%s+([%w_]+)") do
                -- Column name - purple
                local cs, ce = line:find(col_name, search_pos, true)
                if cs and cs < col_def_end then
                  vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                    end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
                  })
                  search_pos = ce + 1
                  -- Type - keyword
                  local ts, te = line:find(col_type, search_pos, true)
                  if ts and ts < col_def_end then
                    vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
                      end_col = te, hl_group = "Keyword", priority = 500,
                    })
                    search_pos = te + 1
                  end
                end
              end
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- Table alias: FROM table alias, JOIN table alias - turquoise
      for _, kw in ipairs({"FROM", "JOIN"}) do
        -- Pattern: FROM/JOIN table_name alias_name (where alias is not a keyword)
        local pattern = kw .. "%s+([%w_]+)%s+([%w_]+)"
        local tbl, alias = line:match(pattern)
        if tbl and alias and not alias:match("^%u+$") then
          -- Make sure alias is not a keyword like ON, WHERE, etc.
          local is_keyword = false
          for _, k in ipairs(sql_keywords) do
            if alias:upper() == k then is_keyword = true break end
          end
          if not is_keyword then
            local kw_pos = line:find(kw)
            local tbl_pos = line:find(tbl, kw_pos, true)
            if tbl_pos then
              local as, ae = line:find(alias, tbl_pos + #tbl, true)
              if as then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, as - 1, {
                  end_col = ae, hl_group = "MannydarkFgTurquoise", priority = 500,
                })
              end
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- USING (column) - column is purple (for JOINs)
      -- But NOT for CREATE INDEX ... USING method
      if line:match("USING%s*%(") and not line:match("CREATE%s+.-INDEX") then
        local using_pos = line:find("USING")
        local paren_start, paren_end = line:find("%b()", using_pos)
        if paren_start then
          local paren_content = line:sub(paren_start + 1, paren_end - 1)
          local search_pos = paren_start
          for col in paren_content:gmatch("([%w_]+)") do
            local cs, ce = line:find(col, search_pos, true)
            if cs and cs < paren_end then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
              })
              search_pos = ce + 1
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- DISTINCT ON (column) - column is purple
      if line:match("DISTINCT%s+ON%s*%(") then
        local paren_start, paren_end = line:find("%b()", line:find("DISTINCT"))
        if paren_start then
          local paren_content = line:sub(paren_start + 1, paren_end - 1)
          local search_pos = paren_start
          for col in paren_content:gmatch("([%w_]+)") do
            local cs, ce = line:find(col, search_pos, true)
            if cs and cs < paren_end then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
              })
              search_pos = ce + 1
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- ON CONFLICT (column) - column is purple
      -- ON CONFLICT ON CONSTRAINT constraint_name - constraint name is turquoise
      if line:match("ON%s+CONFLICT") then
        -- Check for ON CONSTRAINT pattern first
        local constraint_name = line:match("ON%s+CONSTRAINT%s+([%w_]+)")
        if constraint_name then
          local cs, ce = line:find(constraint_name, line:find("CONSTRAINT"), true)
          if cs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
              end_col = ce, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
        -- Check for (column) pattern
        local paren_start, paren_end = line:find("%b()")
        if paren_start then
          local paren_content = line:sub(paren_start + 1, paren_end - 1)
          local search_pos = paren_start
          for col in paren_content:gmatch("([%w_]+)") do
            local cs, ce = line:find(col, search_pos, true)
            if cs and cs < paren_end then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
              })
              search_pos = ce + 1
            end
          end
        end
      end


      -------------------------------------------------------------------------
      -- CREATE [GLOBAL] [TEMPORARY] TABLE name - when treesitter fails
      local create_table_pattern = "CREATE%s+(.-)TABLE%s+([%w_]+)"
      local modifiers, table_name = line:match(create_table_pattern)
      if table_name and not line:match("PARTITION%s+OF") then
        local cs, ce = line:find("CREATE")
        if cs then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
            end_col = ce, hl_group = "Keyword", priority = 500,
          })
        end
        -- Handle modifiers like GLOBAL, TEMPORARY, UNLOGGED, etc.
        if modifiers and #modifiers > 0 then
          for mod in modifiers:gmatch("(%w+)") do
            local ms, me = line:find(mod, ce, true)
            if ms then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ms - 1, {
                end_col = me, hl_group = "Keyword", priority = 500,
              })
              ce = me
            end
          end
        end
        local ts, te = line:find("TABLE", ce)
        if ts then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
            end_col = te, hl_group = "Keyword", priority = 500,
          })
        end
        local ns_s, ns_e = line:find(table_name, te, true)
        if ns_s then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ns_s - 1, {
            end_col = ns_e, hl_group = "MannydarkFgTurquoise", priority = 500,
          })
        end
      end

      -------------------------------------------------------------------------
      -- CREATE [UNIQUE] INDEX [CONCURRENTLY] name ON table(columns) [USING method] [WHERE ...]
      if line:match("CREATE%s+.-INDEX") then
        -- Keywords: CREATE, UNIQUE, INDEX, CONCURRENTLY, ON, USING, WHERE, INCLUDE, etc.
        local keywords_to_find = {
          "CREATE", "UNIQUE", "INDEX", "CONCURRENTLY", "ON", "USING", "WHERE", "INCLUDE",
          "GIN", "GIST", "HASH", "BTREE", "BRIN", "DESC", "ASC", "NULLS", "LAST", "FIRST",
          -- Built-in SQL functions (simple transformations, treated as keywords)
          "LOWER", "UPPER", "COALESCE", "NULLIF", "GREATEST", "LEAST", "TRIM", "LTRIM", "RTRIM",
          "LENGTH", "SUBSTRING", "REPLACE", "CONCAT", "CAST", "CONVERT"
        }
        for _, kw in ipairs(keywords_to_find) do
          local pos = 1
          while true do
            local ks, ke = line:find(kw, pos)
            if not ks then break end
            -- Make sure it's a whole word
            local before = ks > 1 and line:sub(ks - 1, ks - 1) or " "
            local after = ke < #line and line:sub(ke + 1, ke + 1) or " "
            if not before:match("[%w_]") and not after:match("[%w_]") then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ks - 1, {
                end_col = ke, hl_group = "Keyword", priority = 500,
              })
            end
            pos = ke + 1
          end
        end

        -- Index name (after INDEX keyword, before ON)
        local idx_name = line:match("INDEX%s+CONCURRENTLY%s+([%w_]+)") or line:match("INDEX%s+([%w_]+)")
        if idx_name and idx_name ~= "ON" and idx_name ~= "CONCURRENTLY" then
          local is, ie = line:find(idx_name, 1, true)
          if is then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, is - 1, {
              end_col = ie, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end

        -- Table name (after ON, before parenthesis or USING)
        local tbl_name = line:match("ON%s+([%w_]+)")
        if tbl_name then
          local ts, te = line:find(tbl_name, line:find("ON"), true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end

        -- Column names in all parentheses
        local skip_words = {
          "LOWER", "UPPER", "COALESCE", "to_tsvector", "english", "DESC", "ASC", "NULLS", "LAST", "FIRST",
          "GIN", "GIST", "HASH", "BTREE", "BRIN", "INCLUDE", "USING", "WHERE"
        }
        local paren_pos = 1
        while true do
          local ps, pe = line:find("%b()", paren_pos)
          if not ps then break end
          local paren_content = line:sub(ps + 1, pe - 1)
          for col in paren_content:gmatch("([%w_]+)") do
            local should_skip = false
            for _, s in ipairs(skip_words) do
              if col:upper() == s:upper() then should_skip = true break end
            end
            if not should_skip then
              local cs, ce = line:find(col, ps, true)
              if cs then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                  end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
                })
              end
            end
          end
          paren_pos = pe + 1
        end

        -- Complex functions (orange)
        local complex_funcs = {"to_tsvector", "to_tsquery", "plainto_tsquery", "gen_random_uuid", "uuid_generate_v4", "now", "current_timestamp"}
        for _, func in ipairs(complex_funcs) do
          local fs, fe = line:find(func, 1, true)
          if fs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, fs - 1, {
              end_col = fe, hl_group = "MannydarkFgOrange", priority = 510,
            })
          end
        end

        -- Strings
        for str in line:gmatch("('[^']*')") do
          local ss, se = line:find(str, 1, true)
          if ss then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ss - 1, {
              end_col = se, hl_group = "MannydarkFgRedLight", priority = 500,
            })
          end
        end

        -- TRUE/FALSE
        for bool in line:gmatch("(TRUE)") do
          local bs, be = line:find(bool)
          if bs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, bs - 1, {
              end_col = be, hl_group = "Keyword", priority = 500,
            })
          end
        end
        for bool in line:gmatch("(FALSE)") do
          local bs, be = line:find(bool)
          if bs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, bs - 1, {
              end_col = be, hl_group = "Keyword", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- CREATE [RECURSIVE] VIEW name (columns) AS
      if line:match("CREATE%s+.-VIEW") and not line:match("REFRESH") then
        for _, kw in ipairs({"CREATE", "RECURSIVE", "MATERIALIZED", "VIEW", "AS", "OR", "REPLACE"}) do
          local pos = 1
          while true do
            local ks, ke = line:find(kw, pos)
            if not ks then break end
            local before = ks > 1 and line:sub(ks - 1, ks - 1) or " "
            local after = ke < #line and line:sub(ke + 1, ke + 1) or " "
            if not before:match("[%w_]") and not after:match("[%w_]") then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ks - 1, {
                end_col = ke, hl_group = "Keyword", priority = 500,
              })
            end
            pos = ke + 1
          end
        end
        -- View name
        local view_name = line:match("VIEW%s+([%w_]+)")
        if view_name then
          local vs, ve = line:find(view_name, line:find("VIEW"), true)
          if vs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, vs - 1, {
              end_col = ve, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
        -- Column names in parentheses
        local paren_start = line:find("%(")
        if paren_start then
          local paren_end = line:find("%)", paren_start)
          if paren_end then
            local cols = line:sub(paren_start + 1, paren_end - 1)
            local search_pos = paren_start
            for col in cols:gmatch("([%w_]+)") do
              local cs, ce = line:find(col, search_pos, true)
              if cs then
                vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
                  end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
                })
                search_pos = ce + 1
              end
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- REFRESH MATERIALIZED VIEW [CONCURRENTLY] name
      if line:match("REFRESH%s+MATERIALIZED") then
        for _, kw in ipairs({"REFRESH", "MATERIALIZED", "VIEW", "CONCURRENTLY"}) do
          local ks, ke = line:find(kw)
          if ks then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ks - 1, {
              end_col = ke, hl_group = "Keyword", priority = 500,
            })
          end
        end
        local view_name = line:match("VIEW%s+CONCURRENTLY%s+([%w_]+)") or line:match("VIEW%s+([%w_]+)")
        if view_name then
          local vs, ve = line:find(view_name, line:find("VIEW"), true)
          if vs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, vs - 1, {
              end_col = ve, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- DROP INDEX [IF EXISTS] name
      if line:match("DROP%s+INDEX") then
        for _, kw in ipairs({"DROP", "INDEX", "IF", "EXISTS"}) do
          local ks, ke = line:find(kw)
          if ks then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ks - 1, {
              end_col = ke, hl_group = "Keyword", priority = 500,
            })
          end
        end
        local idx_name = line:match("EXISTS%s+([%w_]+)") or line:match("INDEX%s+([%w_]+)")
        if idx_name and idx_name ~= "IF" then
          local is, ie = line:find(idx_name, line:find("INDEX"), true)
          if is then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, is - 1, {
              end_col = ie, hl_group = "MannydarkFgTurquoise", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- ON COMMIT PRESERVE/DELETE ROWS
      local on_commit_pattern = "ON%s+COMMIT%s+(%w+)%s+ROWS"
      local commit_action = line:match(on_commit_pattern)
      if commit_action then
        local os, oe = line:find("ON")
        if os then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, os - 1, {
            end_col = oe, hl_group = "Keyword", priority = 500,
          })
        end
        local cs, ce = line:find("COMMIT", oe)
        if cs then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
            end_col = ce, hl_group = "Keyword", priority = 500,
          })
        end
        local as, ae = line:find(commit_action, ce, true)
        if as then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, as - 1, {
            end_col = ae, hl_group = "Keyword", priority = 500,
          })
        end
        local rs, re = line:find("ROWS", ae)
        if rs then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, rs - 1, {
            end_col = re, hl_group = "Keyword", priority = 500,
          })
        end
      end

      -------------------------------------------------------------------------
      -- Column definitions: name TYPE - for lines inside CREATE TABLE
      local col_def_pattern = "^%s+([%w_]+)%s+([%w_]+)"
      local col_name, col_type = line:match(col_def_pattern)
      if col_name and col_type and not line:match("^%s*%-%-") and not line:match("CONSTRAINT") then
        -- Skip if it's a keyword line like PRIMARY KEY, FOREIGN KEY, LIMIT, ORDER BY, etc.
        local is_keyword = false
        for _, kw in ipairs(sql_keywords) do
          if col_name:upper() == kw then
            is_keyword = true
            break
          end
        end
        if not is_keyword then
          local ns_s, ns_e = line:find(col_name)
          if ns_s then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ns_s - 1, {
              end_col = ns_e, hl_group = "MannydarkFgPurple", priority = 500,
            })
          end
          local ts, te = line:find(col_type, ns_e, true)
          if ts then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
              end_col = te, hl_group = "Keyword", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- FOR VALUES FROM (...) TO (...)
      local for_values_pattern = "FOR%s+VALUES%s+FROM%s*%("
      if line:match(for_values_pattern) then
        local fs, fe = line:find("FOR")
        if fs then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, fs - 1, {
            end_col = fe, hl_group = "Keyword", priority = 500,
          })
        end
        local vs, ve = line:find("VALUES", fe)
        if vs then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, vs - 1, {
            end_col = ve, hl_group = "Keyword", priority = 500,
          })
        end
        local frs, fre = line:find("FROM", ve)
        if frs then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, frs - 1, {
            end_col = fre, hl_group = "Keyword", priority = 500,
          })
        end
        local tos, toe = line:find("TO", fre)
        if tos then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, tos - 1, {
            end_col = toe, hl_group = "Keyword", priority = 500,
          })
        end
        -- Highlight strings in this line
        local str_pos = 1
        while true do
          local ss, se = line:find("'[^']*'", str_pos)
          if not ss then break end
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ss - 1, {
            end_col = se, hl_group = "MannydarkFgRedLight", priority = 500,
          })
          str_pos = se + 1
        end
      end

      -------------------------------------------------------------------------
      -- CREATE TABLE name PARTITION OF parent_table
      local partition_of_pattern = "CREATE%s+TABLE%s+([%w_]+)%s+PARTITION%s+OF%s+([%w_]+)"
      local new_table, parent_table = line:match(partition_of_pattern)
      if new_table and parent_table then
        local cs, ce = line:find("CREATE")
        if cs then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
            end_col = ce, hl_group = "Keyword", priority = 500,
          })
        end
        local ts, te = line:find("TABLE", ce)
        if ts then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
            end_col = te, hl_group = "Keyword", priority = 500,
          })
        end
        local nts, nte = line:find(new_table, te, true)
        if nts then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, nts - 1, {
            end_col = nte, hl_group = "MannydarkFgTurquoise", priority = 500,
          })
        end
        local ps, pe = line:find("PARTITION", nte)
        if ps then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ps - 1, {
            end_col = pe, hl_group = "Keyword", priority = 500,
          })
        end
        local os, oe = line:find("OF", pe)
        if os then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, os - 1, {
            end_col = oe, hl_group = "Keyword", priority = 500,
          })
        end
        local pts, pte = line:find(parent_table, oe, true)
        if pts then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, pts - 1, {
            end_col = pte, hl_group = "MannydarkFgTurquoise", priority = 500,
          })
        end
      end

      -------------------------------------------------------------------------
      -- PARTITION BY RANGE/LIST/HASH (column)
      local partition_pattern = "PARTITION%s+BY%s+(%w+)%s*%(([^)]+)%)"
      local partition_type, partition_cols = line:match(partition_pattern)
      if partition_type then
        local ps, pe = line:find("PARTITION")
        if ps then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ps - 1, {
            end_col = pe, hl_group = "Keyword", priority = 500,
          })
        end
        local bs, be = line:find("BY", pe)
        if bs then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, bs - 1, {
            end_col = be, hl_group = "Keyword", priority = 500,
          })
        end
        local ts, te = line:find(partition_type, be, true)
        if ts then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
            end_col = te, hl_group = "Keyword", priority = 500,
          })
        end
        local paren_start = line:find("%(", te)
        if paren_start then
          for col in partition_cols:gmatch("([%w_]+)") do
            local col_s, col_e = line:find(col, paren_start, true)
            if col_s then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, col_s - 1, {
                end_col = col_e, hl_group = "MannydarkFgPurple", priority = 500,
              })
              paren_start = col_e + 1
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- PRIMARY KEY (col1, col2) - standalone without CONSTRAINT name
      local pk_pattern = "PRIMARY%s+KEY%s*%(([^)]+)%)"
      local pk_cols = line:match(pk_pattern)
      if pk_cols and not line:match("CONSTRAINT%s+[%w_]+%s+PRIMARY") then
        local ps, pe = line:find("PRIMARY")
        if ps then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ps - 1, {
            end_col = pe, hl_group = "Keyword", priority = 500,
          })
        end
        local ks, ke = line:find("KEY", pe)
        if ks then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ks - 1, {
            end_col = ke, hl_group = "Keyword", priority = 500,
          })
        end
        local paren_start = line:find("%(", ke)
        if paren_start then
          for col in pk_cols:gmatch("([%w_]+)") do
            local col_s, col_e = line:find(col, paren_start, true)
            if col_s then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, col_s - 1, {
                end_col = col_e, hl_group = "MannydarkFgPurple", priority = 500,
              })
              paren_start = col_e + 1
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- CONSTRAINT name UNIQUE/FOREIGN KEY/PRIMARY KEY (col1, col2) - often in ERROR nodes
      local unique_pattern = "CONSTRAINT%s+([%w_]+)%s+UNIQUE%s*%(([^)]+)%)"
      local fk_pattern = "CONSTRAINT%s+([%w_]+)%s+FOREIGN%s+KEY%s*%(([^)]+)%)"
      local pk_constraint_pattern = "CONSTRAINT%s+([%w_]+)%s+PRIMARY%s+KEY%s*%(([^)]+)%)"

      local cname, cols = line:match(unique_pattern)
      local is_fk = false
      local is_pk = false
      if not cname then
        cname, cols = line:match(fk_pattern)
        is_fk = cname ~= nil
      end
      if not cname then
        cname, cols = line:match(pk_constraint_pattern)
        is_pk = cname ~= nil
      end

      if cname then
        -- Find positions and highlight
        local cs, ce = line:find("CONSTRAINT")
        if cs then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
            end_col = ce, hl_group = "Keyword", priority = 500,
          })
        end

        local ns_start, ns_end = line:find(cname, ce)
        if ns_start then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ns_start - 1, {
            end_col = ns_end, hl_group = "MannydarkFgTurquoise", priority = 500,
          })
        end

        local keyword_end = ns_end
        if is_fk then
          local fs, fe = line:find("FOREIGN", ns_end)
          if fs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, fs - 1, {
              end_col = fe, hl_group = "Keyword", priority = 500,
            })
          end
          local ks, ke = line:find("KEY", fe)
          if ks then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ks - 1, {
              end_col = ke, hl_group = "Keyword", priority = 500,
            })
            keyword_end = ke
          end
        elseif is_pk then
          local ps, pe = line:find("PRIMARY", ns_end)
          if ps then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ps - 1, {
              end_col = pe, hl_group = "Keyword", priority = 500,
            })
          end
          local ks, ke = line:find("KEY", pe)
          if ks then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ks - 1, {
              end_col = ke, hl_group = "Keyword", priority = 500,
            })
            keyword_end = ke
          end
        else
          local us, ue = line:find("UNIQUE", ns_end)
          if us then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, us - 1, {
              end_col = ue, hl_group = "Keyword", priority = 500,
            })
            keyword_end = ue
          end
        end

        -- Highlight column names inside parentheses
        local paren_start = line:find("%(", keyword_end)
        if paren_start then
          for col in cols:gmatch("([%w_]+)") do
            local col_s, col_e = line:find(col, paren_start, true)
            if col_s then
              vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, col_s - 1, {
                end_col = col_e, hl_group = "MannydarkFgPurple", priority = 500,
              })
              paren_start = col_e + 1
            end
          end
        end
      end

      -------------------------------------------------------------------------
      -- REFERENCES table(column) - column should be purple
      local ref_pattern = "REFERENCES%s+([%w_]+)%s*%(([%w_]+)%)"
      local ref_table, ref_col = line:match(ref_pattern)
      if ref_table and ref_col then
        local rs, re = line:find("REFERENCES")
        if rs then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, rs - 1, {
            end_col = re, hl_group = "Keyword", priority = 500,
          })
        end

        local ts, te = line:find(ref_table, re, true)
        if ts then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, ts - 1, {
            end_col = te, hl_group = "MannydarkFgTurquoise", priority = 500,
          })
        end

        local paren = line:find("%(", te)
        if paren then
          local cs, ce = line:find(ref_col, paren, true)
          if cs then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, cs - 1, {
              end_col = ce, hl_group = "MannydarkFgPurple", priority = 500,
            })
          end
        end
      end

      -------------------------------------------------------------------------
      -- Numbers not captured by treesitter
      local pos = 1
      while true do
        local s, e = line:find("%-?%d+%.?%d*", pos)
        if not s then break end

        -- Skip if number is part of an identifier (has word chars before/after)
        local char_before = s > 1 and line:sub(s - 1, s - 1) or ""
        local char_after = e < #line and line:sub(e + 1, e + 1) or ""
        local part_of_identifier = char_before:match("[%w_]") or char_after:match("[%w_]")

        -- Skip if number is inside a string (check for surrounding quotes)
        local before_substr = line:sub(1, s - 1)
        local quote_count = select(2, before_substr:gsub("'", ""))
        local inside_string = (quote_count % 2) == 1

        if not part_of_identifier and not inside_string then
          local caps = vim.treesitter.get_captures_at_pos(bufnr, lnum - 1, s - 1)
          local dominated = false
          for _, cap in ipairs(caps) do
            if cap.capture == "number" or cap.capture == "number.float" then
              dominated = true
              break
            end
            if cap.capture == "string" then
              dominated = true
              break
            end
          end

          if not dominated then
            vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, s - 1, {
              end_col = e,
              hl_group = "MannydarkFgGreenLight",
              priority = 500,
            })
          end
        end

        pos = e + 1
      end

      ::continue::
    end
  end

  vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "TextChanged", "TextChangedI" }, {
    group = vim.api.nvim_create_augroup("MannydarkSQL", { clear = true }),
    pattern = { "*.sql" },
    callback = function(args)
      -- Defer to let treesitter finish parsing first
      vim.defer_fn(function()
        if vim.api.nvim_buf_is_valid(args.buf) then
          highlight_sql_extmarks(args.buf)
        end
      end, 50)
    end,
  })
end

return sql