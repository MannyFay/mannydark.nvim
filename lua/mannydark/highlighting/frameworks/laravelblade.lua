-------------------------------------------------------------------------------
-- Laravel Blade Files
-- Highlighting for Laravel Blade templates (*.blade.php).
-------------------------------------------------------------------------------

local colors = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local laravelBlade = {}


-------------------------------------------------------------------------------
-- Settings

laravelBlade.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Delimiters
  highlight(0, "bladeDelimiter",       { fg = colors.orange,     bg = "NONE"            })  -- {{ }} {!! !!} {-- --}
  highlight(0, "bladeEchoDelim",       { fg = colors.orange,     bg = "NONE"            })  -- {{ }}
  highlight(0, "bladeRawEchoDelim",    { fg = colors.orange,     bg = "NONE"            })  -- {!! !!}
  highlight(0, "bladeCommentDelim",    { fg = colors.red,        bg = "NONE"            })  -- {{-- --}}

  -- Echo Content
  highlight(0, "bladeEcho",            { fg = colors.white,      bg = "NONE"            })  -- Content inside {{ }}
  highlight(0, "bladeRawEcho",         { fg = colors.white,      bg = "NONE"            })  -- Content inside {!! !!}
  highlight(0, "bladeEscape",          { fg = colors.pink,       bg = "NONE"            })  -- @{{ escaped }}

  -- Comments
  highlight(0, "bladeComment",         { fg = colors.red,        bg = "NONE"            })  -- {{-- comment --}}
  highlight(0, "bladeTodo",            { fg = colors.red,        bg = "NONE", bold = true })  -- TODO, FIXME, XXX

  -- Directives (generic)
  highlight(0, "bladeKeyword",         { fg = colors.blue,       bg = "NONE"            })  -- @directive
  highlight(0, "bladeDirective",       { fg = colors.blue,       bg = "NONE"            })  -- @directive

  -- PHP Regions
  highlight(0, "bladePhpRegion",       { fg = colors.white,      bg = "NONE"            })  -- @php ... @endphp
  highlight(0, "bladePhpParenBlock",   { fg = colors.white,      bg = "NONE"            })  -- PHP in parentheses


  -----------------------------------------------------------------------------
  -- Conditional Directives

  -- If/Else
  highlight(0, "bladeIf",              { fg = colors.blue,       bg = "NONE"            })  -- @if
  highlight(0, "bladeElseif",          { fg = colors.blue,       bg = "NONE"            })  -- @elseif
  highlight(0, "bladeElse",            { fg = colors.blue,       bg = "NONE"            })  -- @else
  highlight(0, "bladeEndif",           { fg = colors.blue,       bg = "NONE"            })  -- @endif

  -- Unless
  highlight(0, "bladeUnless",          { fg = colors.blue,       bg = "NONE"            })  -- @unless
  highlight(0, "bladeEndunless",       { fg = colors.blue,       bg = "NONE"            })  -- @endunless

  -- Isset/Empty
  highlight(0, "bladeIsset",           { fg = colors.blue,       bg = "NONE"            })  -- @isset
  highlight(0, "bladeEndisset",        { fg = colors.blue,       bg = "NONE"            })  -- @endisset
  highlight(0, "bladeEmpty",           { fg = colors.blue,       bg = "NONE"            })  -- @empty
  highlight(0, "bladeEndempty",        { fg = colors.blue,       bg = "NONE"            })  -- @endempty

  -- Switch
  highlight(0, "bladeSwitch",          { fg = colors.blue,       bg = "NONE"            })  -- @switch
  highlight(0, "bladeCase",            { fg = colors.blue,       bg = "NONE"            })  -- @case
  highlight(0, "bladeDefault",         { fg = colors.blue,       bg = "NONE"            })  -- @default
  highlight(0, "bladeEndswitch",       { fg = colors.blue,       bg = "NONE"            })  -- @endswitch


  -----------------------------------------------------------------------------
  -- Authentication Directives

  highlight(0, "bladeAuth",            { fg = colors.blue,       bg = "NONE"            })  -- @auth
  highlight(0, "bladeEndauth",         { fg = colors.blue,       bg = "NONE"            })  -- @endauth
  highlight(0, "bladeGuest",           { fg = colors.blue,       bg = "NONE"            })  -- @guest
  highlight(0, "bladeEndguest",        { fg = colors.blue,       bg = "NONE"            })  -- @endguest

  -- Authorization
  highlight(0, "bladeCan",             { fg = colors.blue,       bg = "NONE"            })  -- @can
  highlight(0, "bladeEndcan",          { fg = colors.blue,       bg = "NONE"            })  -- @endcan
  highlight(0, "bladeCannot",          { fg = colors.blue,       bg = "NONE"            })  -- @cannot
  highlight(0, "bladeEndcannot",       { fg = colors.blue,       bg = "NONE"            })  -- @endcannot
  highlight(0, "bladeCanany",          { fg = colors.blue,       bg = "NONE"            })  -- @canany
  highlight(0, "bladeEndcanany",       { fg = colors.blue,       bg = "NONE"            })  -- @endcanany
  highlight(0, "bladeElsecan",         { fg = colors.blue,       bg = "NONE"            })  -- @elsecan
  highlight(0, "bladeElsecannot",      { fg = colors.blue,       bg = "NONE"            })  -- @elsecannot


  -----------------------------------------------------------------------------
  -- Environment Directives

  highlight(0, "bladeProduction",      { fg = colors.blue,       bg = "NONE"            })  -- @production
  highlight(0, "bladeEndproduction",   { fg = colors.blue,       bg = "NONE"            })  -- @endproduction
  highlight(0, "bladeEnv",             { fg = colors.blue,       bg = "NONE"            })  -- @env
  highlight(0, "bladeEndenv",          { fg = colors.blue,       bg = "NONE"            })  -- @endenv


  -----------------------------------------------------------------------------
  -- Loop Directives

  -- For
  highlight(0, "bladeFor",             { fg = colors.blue,       bg = "NONE"            })  -- @for
  highlight(0, "bladeEndfor",          { fg = colors.blue,       bg = "NONE"            })  -- @endfor

  -- Foreach
  highlight(0, "bladeForeach",         { fg = colors.blue,       bg = "NONE"            })  -- @foreach
  highlight(0, "bladeEndforeach",      { fg = colors.blue,       bg = "NONE"            })  -- @endforeach

  -- Forelse
  highlight(0, "bladeForelse",         { fg = colors.blue,       bg = "NONE"            })  -- @forelse
  highlight(0, "bladeEndforelse",      { fg = colors.blue,       bg = "NONE"            })  -- @endforelse

  -- While
  highlight(0, "bladeWhile",           { fg = colors.blue,       bg = "NONE"            })  -- @while
  highlight(0, "bladeEndwhile",        { fg = colors.blue,       bg = "NONE"            })  -- @endwhile

  -- Loop Control
  highlight(0, "bladeContinue",        { fg = colors.blue,       bg = "NONE"            })  -- @continue
  highlight(0, "bladeBreak",           { fg = colors.blue,       bg = "NONE"            })  -- @break

  -- Loop Variable
  highlight(0, "bladeLoopVar",         { fg = colors.purple,     bg = "NONE"            })  -- $loop


  -----------------------------------------------------------------------------
  -- Template Inheritance Directives

  -- Extends/Layouts
  highlight(0, "bladeExtends",         { fg = colors.blue,       bg = "NONE"            })  -- @extends

  -- Sections
  highlight(0, "bladeSection",         { fg = colors.blue,       bg = "NONE"            })  -- @section
  highlight(0, "bladeEndsection",      { fg = colors.blue,       bg = "NONE"            })  -- @endsection
  highlight(0, "bladeShow",            { fg = colors.blue,       bg = "NONE"            })  -- @show
  highlight(0, "bladeStop",            { fg = colors.blue,       bg = "NONE"            })  -- @stop
  highlight(0, "bladeAppend",          { fg = colors.blue,       bg = "NONE"            })  -- @append
  highlight(0, "bladeOverwrite",       { fg = colors.blue,       bg = "NONE"            })  -- @overwrite

  -- Yield
  highlight(0, "bladeYield",           { fg = colors.blue,       bg = "NONE"            })  -- @yield
  highlight(0, "bladeParent",          { fg = colors.blue,       bg = "NONE"            })  -- @parent

  -- HasSection
  highlight(0, "bladeHasSection",      { fg = colors.blue,       bg = "NONE"            })  -- @hasSection
  highlight(0, "bladeSectionMissing",  { fg = colors.blue,       bg = "NONE"            })  -- @sectionMissing


  -----------------------------------------------------------------------------
  -- Stack Directives

  highlight(0, "bladeStack",           { fg = colors.blue,       bg = "NONE"            })  -- @stack
  highlight(0, "bladePush",            { fg = colors.blue,       bg = "NONE"            })  -- @push
  highlight(0, "bladeEndpush",         { fg = colors.blue,       bg = "NONE"            })  -- @endpush
  highlight(0, "bladePushIf",          { fg = colors.blue,       bg = "NONE"            })  -- @pushIf
  highlight(0, "bladeEndPushIf",       { fg = colors.blue,       bg = "NONE"            })  -- @endPushIf
  highlight(0, "bladePrepend",         { fg = colors.blue,       bg = "NONE"            })  -- @prepend
  highlight(0, "bladeEndprepend",      { fg = colors.blue,       bg = "NONE"            })  -- @endprepend
  highlight(0, "bladePushOnce",        { fg = colors.blue,       bg = "NONE"            })  -- @pushOnce
  highlight(0, "bladeEndPushOnce",     { fg = colors.blue,       bg = "NONE"            })  -- @endPushOnce
  highlight(0, "bladePrependOnce",     { fg = colors.blue,       bg = "NONE"            })  -- @prependOnce
  highlight(0, "bladeEndPrependOnce",  { fg = colors.blue,       bg = "NONE"            })  -- @endPrependOnce
  highlight(0, "bladeHasstack",        { fg = colors.blue,       bg = "NONE"            })  -- @hasstack


  -----------------------------------------------------------------------------
  -- Include Directives

  highlight(0, "bladeInclude",         { fg = colors.blue,       bg = "NONE"            })  -- @include
  highlight(0, "bladeIncludeIf",       { fg = colors.blue,       bg = "NONE"            })  -- @includeIf
  highlight(0, "bladeIncludeWhen",     { fg = colors.blue,       bg = "NONE"            })  -- @includeWhen
  highlight(0, "bladeIncludeUnless",   { fg = colors.blue,       bg = "NONE"            })  -- @includeUnless
  highlight(0, "bladeIncludeFirst",    { fg = colors.blue,       bg = "NONE"            })  -- @includeFirst
  highlight(0, "bladeEach",            { fg = colors.blue,       bg = "NONE"            })  -- @each


  -----------------------------------------------------------------------------
  -- Component Directives

  highlight(0, "bladeComponent",       { fg = colors.blue,       bg = "NONE"            })  -- @component
  highlight(0, "bladeEndcomponent",    { fg = colors.blue,       bg = "NONE"            })  -- @endcomponent
  highlight(0, "bladeSlot",            { fg = colors.blue,       bg = "NONE"            })  -- @slot
  highlight(0, "bladeEndslot",         { fg = colors.blue,       bg = "NONE"            })  -- @endslot
  highlight(0, "bladeProps",           { fg = colors.blue,       bg = "NONE"            })  -- @props
  highlight(0, "bladeAware",           { fg = colors.blue,       bg = "NONE"            })  -- @aware


  -----------------------------------------------------------------------------
  -- Form Directives

  highlight(0, "bladeCsrf",            { fg = colors.blue,       bg = "NONE"            })  -- @csrf
  highlight(0, "bladeMethod",          { fg = colors.blue,       bg = "NONE"            })  -- @method
  highlight(0, "bladeError",           { fg = colors.blue,       bg = "NONE"            })  -- @error
  highlight(0, "bladeEnderror",        { fg = colors.blue,       bg = "NONE"            })  -- @enderror
  highlight(0, "bladeOld",             { fg = colors.blue,       bg = "NONE"            })  -- old()


  -----------------------------------------------------------------------------
  -- HTML Attribute Directives

  highlight(0, "bladeClass",           { fg = colors.blue,       bg = "NONE"            })  -- @class
  highlight(0, "bladeStyle",           { fg = colors.blue,       bg = "NONE"            })  -- @style
  highlight(0, "bladeChecked",         { fg = colors.blue,       bg = "NONE"            })  -- @checked
  highlight(0, "bladeSelected",        { fg = colors.blue,       bg = "NONE"            })  -- @selected
  highlight(0, "bladeDisabled",        { fg = colors.blue,       bg = "NONE"            })  -- @disabled
  highlight(0, "bladeReadonly",        { fg = colors.blue,       bg = "NONE"            })  -- @readonly
  highlight(0, "bladeRequired",        { fg = colors.blue,       bg = "NONE"            })  -- @required


  -----------------------------------------------------------------------------
  -- Session/Context Directives

  highlight(0, "bladeSession",         { fg = colors.blue,       bg = "NONE"            })  -- @session
  highlight(0, "bladeEndsession",      { fg = colors.blue,       bg = "NONE"            })  -- @endsession
  highlight(0, "bladeContext",         { fg = colors.blue,       bg = "NONE"            })  -- @context
  highlight(0, "bladeEndcontext",      { fg = colors.blue,       bg = "NONE"            })  -- @endcontext


  -----------------------------------------------------------------------------
  -- Special Directives

  -- Once
  highlight(0, "bladeOnce",            { fg = colors.blue,       bg = "NONE"            })  -- @once
  highlight(0, "bladeEndonce",         { fg = colors.blue,       bg = "NONE"            })  -- @endonce

  -- Verbatim
  highlight(0, "bladeVerbatim",        { fg = colors.blue,       bg = "NONE"            })  -- @verbatim
  highlight(0, "bladeEndverbatim",     { fg = colors.blue,       bg = "NONE"            })  -- @endverbatim

  -- Fragment
  highlight(0, "bladeFragment",        { fg = colors.blue,       bg = "NONE"            })  -- @fragment
  highlight(0, "bladeEndfragment",     { fg = colors.blue,       bg = "NONE"            })  -- @endfragment
  highlight(0, "bladeFragmentIf",      { fg = colors.blue,       bg = "NONE"            })  -- @fragmentIf

  -- PHP/Raw Code
  highlight(0, "bladePhp",             { fg = colors.blue,       bg = "NONE"            })  -- @php
  highlight(0, "bladeEndphp",          { fg = colors.blue,       bg = "NONE"            })  -- @endphp
  highlight(0, "bladeUse",             { fg = colors.blue,       bg = "NONE"            })  -- @use

  -- Service Injection
  highlight(0, "bladeInject",          { fg = colors.blue,       bg = "NONE"            })  -- @inject

  -- JSON
  highlight(0, "bladeJson",            { fg = colors.blue,       bg = "NONE"            })  -- @json

  -- JS
  highlight(0, "bladeJs",              { fg = colors.blue,       bg = "NONE"            })  -- @js

  -- Lang/Translation
  highlight(0, "bladeLang",            { fg = colors.blue,       bg = "NONE"            })  -- @lang
  highlight(0, "bladeChoice",          { fg = colors.blue,       bg = "NONE"            })  -- @choice


  -----------------------------------------------------------------------------
  -- Livewire Directives

  highlight(0, "bladeLivewire",        { fg = colors.blue,       bg = "NONE"            })  -- @livewire
  highlight(0, "bladeLivewireStyles",  { fg = colors.blue,       bg = "NONE"            })  -- @livewireStyles
  highlight(0, "bladeLivewireScripts", { fg = colors.blue,       bg = "NONE"            })  -- @livewireScripts
  highlight(0, "bladeThis",            { fg = colors.blue,       bg = "NONE"            })  -- @this
  highlight(0, "bladeEntangle",        { fg = colors.blue,       bg = "NONE"            })  -- @entangle
  highlight(0, "bladePersist",         { fg = colors.blue,       bg = "NONE"            })  -- @persist
  highlight(0, "bladeEndpersist",      { fg = colors.blue,       bg = "NONE"            })  -- @endpersist
  highlight(0, "bladeTeleport",        { fg = colors.blue,       bg = "NONE"            })  -- @teleport
  highlight(0, "bladeEndteleport",     { fg = colors.blue,       bg = "NONE"            })  -- @endteleport


  -----------------------------------------------------------------------------
  -- Inertia Directives

  highlight(0, "bladeInertia",         { fg = colors.blue,       bg = "NONE"            })  -- @inertia
  highlight(0, "bladeInertiaHead",     { fg = colors.blue,       bg = "NONE"            })  -- @inertiaHead


  -----------------------------------------------------------------------------
  -- Vite/Asset Directives

  highlight(0, "bladeVite",            { fg = colors.blue,       bg = "NONE"            })  -- @vite
  highlight(0, "bladeViteReactRefresh", { fg = colors.blue,      bg = "NONE"            })  -- @viteReactRefresh


  -----------------------------------------------------------------------------
  -- Alpine.js Integration (x-data, x-bind, etc. in Blade)

  highlight(0, "bladeAlpineDirective", { fg = colors.purple,     bg = "NONE"            })  -- x-data, x-bind, etc.
  highlight(0, "bladeAlpineAtSign",    { fg = colors.purple,     bg = "NONE"            })  -- @click, @submit (Alpine)


  -----------------------------------------------------------------------------
  -- Component Tags (x-slot, x-component style)

  highlight(0, "bladeXComponent",      { fg = colors.green,      bg = "NONE"            })  -- <x-component>
  highlight(0, "bladeXSlot",           { fg = colors.green,      bg = "NONE"            })  -- <x-slot>
  highlight(0, "bladeXDynamic",        { fg = colors.green,      bg = "NONE"            })  -- <x-dynamic-component>
  highlight(0, "bladeComponentTag",    { fg = colors.green,      bg = "NONE"            })  -- Component tag name
  highlight(0, "bladeComponentAttr",   { fg = colors.turquoise,  bg = "NONE"            })  -- Component attribute
  highlight(0, "bladeComponentBind",   { fg = colors.orange,     bg = "NONE"            })  -- :attribute (bound)
  highlight(0, "bladeWireModel",       { fg = colors.pink,       bg = "NONE"            })  -- wire:model
  highlight(0, "bladeWireClick",       { fg = colors.pink,       bg = "NONE"            })  -- wire:click


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.blade)

  -- Directives
  highlight(0, "@keyword.blade",               { fg = colors.blue,       bg = "NONE" })  -- Directives
  highlight(0, "@keyword.directive.blade",     { fg = colors.blue,       bg = "NONE" })  -- @directive
  highlight(0, "@keyword.conditional.blade",   { fg = colors.blue,       bg = "NONE" })  -- @if, @unless
  highlight(0, "@keyword.repeat.blade",        { fg = colors.blue,       bg = "NONE" })  -- @for, @foreach
  highlight(0, "@keyword.import.blade",        { fg = colors.blue,       bg = "NONE" })  -- @include, @extends
  highlight(0, "@keyword.function.blade",      { fg = colors.blue,       bg = "NONE" })  -- @component

  -- Tags
  highlight(0, "@tag.blade",                   { fg = colors.green,      bg = "NONE" })  -- x-component tags
  highlight(0, "@tag.attribute.blade",         { fg = colors.turquoise,  bg = "NONE" })  -- Component attributes
  highlight(0, "@tag.delimiter.blade",         { fg = colors.white,      bg = "NONE" })  -- < > </ />

  -- Echo/Output
  highlight(0, "@punctuation.bracket.blade",   { fg = colors.orange,     bg = "NONE" })  -- {{ }} {!! !!}
  highlight(0, "@punctuation.delimiter.blade", { fg = colors.white,      bg = "NONE" })  -- Delimiters

  -- Strings
  highlight(0, "@string.blade",                { fg = colors.redLight,   bg = "NONE" })  -- String content

  -- Comments
  highlight(0, "@comment.blade",               { fg = colors.red,        bg = "NONE" })  -- {{-- --}}

  -- Variables
  highlight(0, "@variable.blade",              { fg = colors.white,      bg = "NONE" })  -- $variable
  highlight(0, "@variable.builtin.blade",      { fg = colors.purple,     bg = "NONE" })  -- $loop, $slot

  -- Special
  highlight(0, "@constant.blade",              { fg = colors.purple,     bg = "NONE" })  -- Constants
  highlight(0, "@function.blade",              { fg = colors.orange,     bg = "NONE" })  -- Functions


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.blade)

  highlight(0, "@lsp.type.keyword.blade",      { fg = colors.blue,       bg = "NONE" })  -- Directives
  highlight(0, "@lsp.type.function.blade",     { fg = colors.orange,     bg = "NONE" })  -- Functions
  highlight(0, "@lsp.type.variable.blade",     { fg = colors.white,      bg = "NONE" })  -- Variables
  highlight(0, "@lsp.type.string.blade",       { fg = colors.redLight,   bg = "NONE" })  -- Strings
  highlight(0, "@lsp.type.comment.blade",      { fg = colors.red,        bg = "NONE" })  -- Comments
  highlight(0, "@lsp.type.class.blade",        { fg = colors.green,      bg = "NONE" })  -- Components


  -----------------------------------------------------------------------------
  -- Directive Categories (for semantic coloring)

  -- Control Flow → blue
  highlight(0, "bladeControlFlow",     { fg = colors.blue,       bg = "NONE"            })

  -- Auth/Authorization → purple
  highlight(0, "bladeAuthorization",   { fg = colors.purple,     bg = "NONE"            })

  -- Layout/Template → turquoise
  highlight(0, "bladeLayout",          { fg = colors.turquoise,  bg = "NONE"            })

  -- Components → green
  highlight(0, "bladeComponentDir",    { fg = colors.green,      bg = "NONE"            })

  -- Includes → orange
  highlight(0, "bladeIncludeDir",      { fg = colors.orange,     bg = "NONE"            })

  -- Livewire → pink
  highlight(0, "bladeLivewireDir",     { fg = colors.pink,       bg = "NONE", bold = true })


  -----------------------------------------------------------------------------
  -- Error Highlighting

  highlight(0, "bladeError",           { fg = colors.red,        bg = "NONE", undercurl = true })  -- Syntax error
  highlight(0, "bladeUnmatchedDir",    { fg = colors.red,        bg = "NONE", undercurl = true })  -- Unmatched directive


  -----------------------------------------------------------------------------
  -- Links

  highlight(0, "bladePhpBlock",        { link = "bladePhpRegion" })
  highlight(0, "bladePhpInline",       { link = "bladePhpRegion" })


  -----------------------------------------------------------------------------
  -- Extmarks for escaped Blade/Vue templates

  local ns = vim.api.nvim_create_namespace("mannydark_blade_vue")

  local function highlight_js_expression(bufnr, lnum, content, start_col)
    -- Split by dots and highlight each part as purple
    local parts = {}
    for part in content:gmatch("[^%.]+") do
      table.insert(parts, part)
    end

    local line = vim.api.nvim_buf_get_lines(bufnr, lnum, lnum + 1, false)[1]
    local current_col = start_col

    for _, part in ipairs(parts) do
      local part_trimmed = part:match("^%s*(.-)%s*$")
      if #part_trimmed > 0 then
        local part_start = line:find(part_trimmed, current_col + 1, true)
        if part_start then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum, part_start - 1, {
            end_col = part_start - 1 + #part_trimmed,
            hl_group = "MannydarkFgPurple",
            priority = 300,
          })
          current_col = part_start + #part_trimmed
        end
      end
    end
  end

  local function highlight_vue_templates(bufnr)
    vim.api.nvim_buf_clear_namespace(bufnr, ns, 0, -1)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    local in_verbatim = false

    for lnum, line in ipairs(lines) do
      -- Track @verbatim blocks
      if line:match("@verbatim") then
        in_verbatim = true
      elseif line:match("@endverbatim") then
        in_verbatim = false
      end

      -- Pattern 1: @{{ ... }} (escaped blade for Vue/JS) - anywhere
      local start_pos = 1
      while true do
        local s, e, content = line:find("@{{%s*(.-)%s*}}", start_pos)
        if not s then break end
        highlight_js_expression(bufnr, lnum - 1, content, s + 2)
        start_pos = e + 1
      end

      -- Pattern 2: {{ ... }} inside @verbatim blocks (React/Vue)
      if in_verbatim then
        start_pos = 1
        while true do
          local s, e, content = line:find("{{%s*(.-)%s*}}", start_pos)
          if not s then break end
          -- Skip if it's actually @{{ (already handled above)
          if s == 1 or line:sub(s - 1, s - 1) ~= "@" then
            highlight_js_expression(bufnr, lnum - 1, content, s + 1)
          end
          start_pos = e + 1
        end
      end

      -- Pattern 3: "as" keyword in @foreach, @forelse, @for (e.g., @foreach($users as $user))
      local as_start, as_end = line:find("%s+as%s+")
      if as_start and line:match("@for") then
        -- Find the actual "as" position
        local as_pos = line:find("as", as_start)
        if as_pos then
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, as_pos - 1, {
            end_col = as_pos + 1,
            hl_group = "Keyword",
            priority = 300,
          })
        end
      end

      -- Pattern 4: Blade component tags (<x-..., </x-...) - keyword blue
      local pos = 1
      while true do
        local s, e = line:find("</?x%-[%w%-]+", pos)
        if not s then break end
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, s - 1, {
          end_col = e,
          hl_group = "Keyword",
          priority = 500,
        })
        pos = e + 1
      end

      -- Pattern 5: Member access ($var->property) - property name should be purple
      pos = 1
      while true do
        local s, e, prop = line:find("%->([%w_]+)", pos)
        if not s then break end
        -- s is position of '-', property starts at s+2
        vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, s + 1, {
          end_col = e,
          hl_group = "MannydarkFgPurple",
          priority = 500,
        })
        pos = e + 1
      end

      -- Pattern 6: Boolean literals (true, false) - keyword blue
      for _, bool in ipairs({ "true", "false" }) do
        pos = 1
        while true do
          local s, e = line:find("%f[%w]" .. bool .. "%f[%W]", pos)
          if not s then break end
          vim.api.nvim_buf_set_extmark(bufnr, ns, lnum - 1, s - 1, {
            end_col = e,
            hl_group = "Keyword",
            priority = 500,
          })
          pos = e + 1
        end
      end
    end
  end

  vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "TextChanged", "TextChangedI" }, {
    group = vim.api.nvim_create_augroup("MannydarkBladeVue", { clear = true }),
    pattern = { "*.blade.php" },
    callback = function(args)
      highlight_vue_templates(args.buf)
    end,
  })
end

return laravelBlade
