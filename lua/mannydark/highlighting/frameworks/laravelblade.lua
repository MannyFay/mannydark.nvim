-------------------------------------------------------------------------------
-- Laravel Blade Files
-- Highlighting for Laravel Blade templates (*.blade.php).
-------------------------------------------------------------------------------

local colors = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local laravelBlade = {}


-------------------------------------------------------------------------------
-- Settings

laravelBlade.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Delimiters
  highlight(0, 'bladeDelimiter',       { fg = colors.orange,     bg = 'NONE'            })  -- {{ }} {!! !!} {-- --}
  highlight(0, 'bladeEchoDelim',       { fg = colors.orange,     bg = 'NONE'            })  -- {{ }}
  highlight(0, 'bladeRawEchoDelim',    { fg = colors.orange,     bg = 'NONE'            })  -- {!! !!}
  highlight(0, 'bladeCommentDelim',    { fg = colors.red,        bg = 'NONE'            })  -- {{-- --}}

  -- Echo Content
  highlight(0, 'bladeEcho',            { fg = colors.white,      bg = 'NONE'            })  -- Content inside {{ }}
  highlight(0, 'bladeRawEcho',         { fg = colors.white,      bg = 'NONE'            })  -- Content inside {!! !!}
  highlight(0, 'bladeEscape',          { fg = colors.pink,       bg = 'NONE'            })  -- @{{ escaped }}

  -- Comments
  highlight(0, 'bladeComment',         { fg = colors.red,        bg = 'NONE'            })  -- {{-- comment --}}
  highlight(0, 'bladeTodo',            { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Directives (generic)
  highlight(0, 'bladeKeyword',         { fg = colors.blue,       bg = 'NONE'            })  -- @directive
  highlight(0, 'bladeDirective',       { fg = colors.blue,       bg = 'NONE'            })  -- @directive

  -- PHP Regions
  highlight(0, 'bladePhpRegion',       { fg = colors.white,      bg = 'NONE'            })  -- @php ... @endphp
  highlight(0, 'bladePhpParenBlock',   { fg = colors.white,      bg = 'NONE'            })  -- PHP in parentheses


  -----------------------------------------------------------------------------
  -- Conditional Directives

  -- If/Else
  highlight(0, 'bladeIf',              { fg = colors.blue,       bg = 'NONE'            })  -- @if
  highlight(0, 'bladeElseif',          { fg = colors.blue,       bg = 'NONE'            })  -- @elseif
  highlight(0, 'bladeElse',            { fg = colors.blue,       bg = 'NONE'            })  -- @else
  highlight(0, 'bladeEndif',           { fg = colors.blue,       bg = 'NONE'            })  -- @endif

  -- Unless
  highlight(0, 'bladeUnless',          { fg = colors.blue,       bg = 'NONE'            })  -- @unless
  highlight(0, 'bladeEndunless',       { fg = colors.blue,       bg = 'NONE'            })  -- @endunless

  -- Isset/Empty
  highlight(0, 'bladeIsset',           { fg = colors.blue,       bg = 'NONE'            })  -- @isset
  highlight(0, 'bladeEndisset',        { fg = colors.blue,       bg = 'NONE'            })  -- @endisset
  highlight(0, 'bladeEmpty',           { fg = colors.blue,       bg = 'NONE'            })  -- @empty
  highlight(0, 'bladeEndempty',        { fg = colors.blue,       bg = 'NONE'            })  -- @endempty

  -- Switch
  highlight(0, 'bladeSwitch',          { fg = colors.blue,       bg = 'NONE'            })  -- @switch
  highlight(0, 'bladeCase',            { fg = colors.blue,       bg = 'NONE'            })  -- @case
  highlight(0, 'bladeDefault',         { fg = colors.blue,       bg = 'NONE'            })  -- @default
  highlight(0, 'bladeEndswitch',       { fg = colors.blue,       bg = 'NONE'            })  -- @endswitch


  -----------------------------------------------------------------------------
  -- Authentication Directives

  highlight(0, 'bladeAuth',            { fg = colors.purple,     bg = 'NONE'            })  -- @auth
  highlight(0, 'bladeEndauth',         { fg = colors.purple,     bg = 'NONE'            })  -- @endauth
  highlight(0, 'bladeGuest',           { fg = colors.purple,     bg = 'NONE'            })  -- @guest
  highlight(0, 'bladeEndguest',        { fg = colors.purple,     bg = 'NONE'            })  -- @endguest

  -- Authorization
  highlight(0, 'bladeCan',             { fg = colors.purple,     bg = 'NONE'            })  -- @can
  highlight(0, 'bladeEndcan',          { fg = colors.purple,     bg = 'NONE'            })  -- @endcan
  highlight(0, 'bladeCannot',          { fg = colors.purple,     bg = 'NONE'            })  -- @cannot
  highlight(0, 'bladeEndcannot',       { fg = colors.purple,     bg = 'NONE'            })  -- @endcannot
  highlight(0, 'bladeCanany',          { fg = colors.purple,     bg = 'NONE'            })  -- @canany
  highlight(0, 'bladeEndcanany',       { fg = colors.purple,     bg = 'NONE'            })  -- @endcanany
  highlight(0, 'bladeElsecan',         { fg = colors.purple,     bg = 'NONE'            })  -- @elsecan
  highlight(0, 'bladeElsecannot',      { fg = colors.purple,     bg = 'NONE'            })  -- @elsecannot


  -----------------------------------------------------------------------------
  -- Environment Directives

  highlight(0, 'bladeProduction',      { fg = colors.orange,     bg = 'NONE'            })  -- @production
  highlight(0, 'bladeEndproduction',   { fg = colors.orange,     bg = 'NONE'            })  -- @endproduction
  highlight(0, 'bladeEnv',             { fg = colors.orange,     bg = 'NONE'            })  -- @env
  highlight(0, 'bladeEndenv',          { fg = colors.orange,     bg = 'NONE'            })  -- @endenv


  -----------------------------------------------------------------------------
  -- Loop Directives

  -- For
  highlight(0, 'bladeFor',             { fg = colors.blue,       bg = 'NONE'            })  -- @for
  highlight(0, 'bladeEndfor',          { fg = colors.blue,       bg = 'NONE'            })  -- @endfor

  -- Foreach
  highlight(0, 'bladeForeach',         { fg = colors.blue,       bg = 'NONE'            })  -- @foreach
  highlight(0, 'bladeEndforeach',      { fg = colors.blue,       bg = 'NONE'            })  -- @endforeach

  -- Forelse
  highlight(0, 'bladeForelse',         { fg = colors.blue,       bg = 'NONE'            })  -- @forelse
  highlight(0, 'bladeEndforelse',      { fg = colors.blue,       bg = 'NONE'            })  -- @endforelse

  -- While
  highlight(0, 'bladeWhile',           { fg = colors.blue,       bg = 'NONE'            })  -- @while
  highlight(0, 'bladeEndwhile',        { fg = colors.blue,       bg = 'NONE'            })  -- @endwhile

  -- Loop Control
  highlight(0, 'bladeContinue',        { fg = colors.pink,       bg = 'NONE'            })  -- @continue
  highlight(0, 'bladeBreak',           { fg = colors.pink,       bg = 'NONE'            })  -- @break

  -- Loop Variable
  highlight(0, 'bladeLoopVar',         { fg = colors.purple,     bg = 'NONE'            })  -- $loop


  -----------------------------------------------------------------------------
  -- Template Inheritance Directives

  -- Extends/Layouts
  highlight(0, 'bladeExtends',         { fg = colors.turquoise,  bg = 'NONE', bold = true })  -- @extends

  -- Sections
  highlight(0, 'bladeSection',         { fg = colors.turquoise,  bg = 'NONE'            })  -- @section
  highlight(0, 'bladeEndsection',      { fg = colors.turquoise,  bg = 'NONE'            })  -- @endsection
  highlight(0, 'bladeShow',            { fg = colors.turquoise,  bg = 'NONE'            })  -- @show
  highlight(0, 'bladeStop',            { fg = colors.turquoise,  bg = 'NONE'            })  -- @stop
  highlight(0, 'bladeAppend',          { fg = colors.turquoise,  bg = 'NONE'            })  -- @append
  highlight(0, 'bladeOverwrite',       { fg = colors.turquoise,  bg = 'NONE'            })  -- @overwrite

  -- Yield
  highlight(0, 'bladeYield',           { fg = colors.turquoise,  bg = 'NONE'            })  -- @yield
  highlight(0, 'bladeParent',          { fg = colors.turquoise,  bg = 'NONE'            })  -- @parent

  -- HasSection
  highlight(0, 'bladeHasSection',      { fg = colors.turquoise,  bg = 'NONE'            })  -- @hasSection
  highlight(0, 'bladeSectionMissing',  { fg = colors.turquoise,  bg = 'NONE'            })  -- @sectionMissing


  -----------------------------------------------------------------------------
  -- Stack Directives

  highlight(0, 'bladeStack',           { fg = colors.turquoise,  bg = 'NONE'            })  -- @stack
  highlight(0, 'bladePush',            { fg = colors.turquoise,  bg = 'NONE'            })  -- @push
  highlight(0, 'bladeEndpush',         { fg = colors.turquoise,  bg = 'NONE'            })  -- @endpush
  highlight(0, 'bladePushIf',          { fg = colors.turquoise,  bg = 'NONE'            })  -- @pushIf
  highlight(0, 'bladeEndPushIf',       { fg = colors.turquoise,  bg = 'NONE'            })  -- @endPushIf
  highlight(0, 'bladePrepend',         { fg = colors.turquoise,  bg = 'NONE'            })  -- @prepend
  highlight(0, 'bladeEndprepend',      { fg = colors.turquoise,  bg = 'NONE'            })  -- @endprepend
  highlight(0, 'bladePushOnce',        { fg = colors.turquoise,  bg = 'NONE'            })  -- @pushOnce
  highlight(0, 'bladeEndPushOnce',     { fg = colors.turquoise,  bg = 'NONE'            })  -- @endPushOnce
  highlight(0, 'bladePrependOnce',     { fg = colors.turquoise,  bg = 'NONE'            })  -- @prependOnce
  highlight(0, 'bladeEndPrependOnce',  { fg = colors.turquoise,  bg = 'NONE'            })  -- @endPrependOnce
  highlight(0, 'bladeHasstack',        { fg = colors.turquoise,  bg = 'NONE'            })  -- @hasstack


  -----------------------------------------------------------------------------
  -- Include Directives

  highlight(0, 'bladeInclude',         { fg = colors.orange,     bg = 'NONE'            })  -- @include
  highlight(0, 'bladeIncludeIf',       { fg = colors.orange,     bg = 'NONE'            })  -- @includeIf
  highlight(0, 'bladeIncludeWhen',     { fg = colors.orange,     bg = 'NONE'            })  -- @includeWhen
  highlight(0, 'bladeIncludeUnless',   { fg = colors.orange,     bg = 'NONE'            })  -- @includeUnless
  highlight(0, 'bladeIncludeFirst',    { fg = colors.orange,     bg = 'NONE'            })  -- @includeFirst
  highlight(0, 'bladeEach',            { fg = colors.orange,     bg = 'NONE'            })  -- @each


  -----------------------------------------------------------------------------
  -- Component Directives

  highlight(0, 'bladeComponent',       { fg = colors.green,      bg = 'NONE'            })  -- @component
  highlight(0, 'bladeEndcomponent',    { fg = colors.green,      bg = 'NONE'            })  -- @endcomponent
  highlight(0, 'bladeSlot',            { fg = colors.green,      bg = 'NONE'            })  -- @slot
  highlight(0, 'bladeEndslot',         { fg = colors.green,      bg = 'NONE'            })  -- @endslot
  highlight(0, 'bladeProps',           { fg = colors.green,      bg = 'NONE'            })  -- @props
  highlight(0, 'bladeAware',           { fg = colors.green,      bg = 'NONE'            })  -- @aware


  -----------------------------------------------------------------------------
  -- Form Directives

  highlight(0, 'bladeCsrf',            { fg = colors.pink,       bg = 'NONE', bold = true })  -- @csrf
  highlight(0, 'bladeMethod',          { fg = colors.pink,       bg = 'NONE'            })  -- @method
  highlight(0, 'bladeError',           { fg = colors.pink,       bg = 'NONE'            })  -- @error
  highlight(0, 'bladeEnderror',        { fg = colors.pink,       bg = 'NONE'            })  -- @enderror
  highlight(0, 'bladeOld',             { fg = colors.pink,       bg = 'NONE'            })  -- old()


  -----------------------------------------------------------------------------
  -- HTML Attribute Directives

  highlight(0, 'bladeClass',           { fg = colors.turquoise,  bg = 'NONE'            })  -- @class
  highlight(0, 'bladeStyle',           { fg = colors.turquoise,  bg = 'NONE'            })  -- @style
  highlight(0, 'bladeChecked',         { fg = colors.turquoise,  bg = 'NONE'            })  -- @checked
  highlight(0, 'bladeSelected',        { fg = colors.turquoise,  bg = 'NONE'            })  -- @selected
  highlight(0, 'bladeDisabled',        { fg = colors.turquoise,  bg = 'NONE'            })  -- @disabled
  highlight(0, 'bladeReadonly',        { fg = colors.turquoise,  bg = 'NONE'            })  -- @readonly
  highlight(0, 'bladeRequired',        { fg = colors.turquoise,  bg = 'NONE'            })  -- @required


  -----------------------------------------------------------------------------
  -- Session/Context Directives

  highlight(0, 'bladeSession',         { fg = colors.purple,     bg = 'NONE'            })  -- @session
  highlight(0, 'bladeEndsession',      { fg = colors.purple,     bg = 'NONE'            })  -- @endsession
  highlight(0, 'bladeContext',         { fg = colors.purple,     bg = 'NONE'            })  -- @context
  highlight(0, 'bladeEndcontext',      { fg = colors.purple,     bg = 'NONE'            })  -- @endcontext


  -----------------------------------------------------------------------------
  -- Special Directives

  -- Once
  highlight(0, 'bladeOnce',            { fg = colors.blue,       bg = 'NONE'            })  -- @once
  highlight(0, 'bladeEndonce',         { fg = colors.blue,       bg = 'NONE'            })  -- @endonce

  -- Verbatim
  highlight(0, 'bladeVerbatim',        { fg = colors.gray,       bg = 'NONE'            })  -- @verbatim
  highlight(0, 'bladeEndverbatim',     { fg = colors.gray,       bg = 'NONE'            })  -- @endverbatim

  -- Fragment
  highlight(0, 'bladeFragment',        { fg = colors.orange,     bg = 'NONE'            })  -- @fragment
  highlight(0, 'bladeEndfragment',     { fg = colors.orange,     bg = 'NONE'            })  -- @endfragment
  highlight(0, 'bladeFragmentIf',      { fg = colors.orange,     bg = 'NONE'            })  -- @fragmentIf

  -- PHP/Raw Code
  highlight(0, 'bladePhp',             { fg = colors.blue,       bg = 'NONE'            })  -- @php
  highlight(0, 'bladeEndphp',          { fg = colors.blue,       bg = 'NONE'            })  -- @endphp
  highlight(0, 'bladeUse',             { fg = colors.blue,       bg = 'NONE'            })  -- @use

  -- Service Injection
  highlight(0, 'bladeInject',          { fg = colors.purple,     bg = 'NONE'            })  -- @inject

  -- JSON
  highlight(0, 'bladeJson',            { fg = colors.orange,     bg = 'NONE'            })  -- @json

  -- JS
  highlight(0, 'bladeJs',              { fg = colors.orange,     bg = 'NONE'            })  -- @js

  -- Lang/Translation
  highlight(0, 'bladeLang',            { fg = colors.purple,     bg = 'NONE'            })  -- @lang
  highlight(0, 'bladeChoice',          { fg = colors.purple,     bg = 'NONE'            })  -- @choice


  -----------------------------------------------------------------------------
  -- Livewire Directives

  highlight(0, 'bladeLivewire',        { fg = colors.pink,       bg = 'NONE', bold = true })  -- @livewire
  highlight(0, 'bladeLivewireStyles',  { fg = colors.pink,       bg = 'NONE'            })  -- @livewireStyles
  highlight(0, 'bladeLivewireScripts', { fg = colors.pink,       bg = 'NONE'            })  -- @livewireScripts
  highlight(0, 'bladeThis',            { fg = colors.pink,       bg = 'NONE'            })  -- @this
  highlight(0, 'bladeEntangle',        { fg = colors.pink,       bg = 'NONE'            })  -- @entangle
  highlight(0, 'bladePersist',         { fg = colors.pink,       bg = 'NONE'            })  -- @persist
  highlight(0, 'bladeEndpersist',      { fg = colors.pink,       bg = 'NONE'            })  -- @endpersist
  highlight(0, 'bladeTeleport',        { fg = colors.pink,       bg = 'NONE'            })  -- @teleport
  highlight(0, 'bladeEndteleport',     { fg = colors.pink,       bg = 'NONE'            })  -- @endteleport


  -----------------------------------------------------------------------------
  -- Inertia Directives

  highlight(0, 'bladeInertia',         { fg = colors.turquoise,  bg = 'NONE'            })  -- @inertia
  highlight(0, 'bladeInertiaHead',     { fg = colors.turquoise,  bg = 'NONE'            })  -- @inertiaHead


  -----------------------------------------------------------------------------
  -- Vite/Asset Directives

  highlight(0, 'bladeVite',            { fg = colors.green,      bg = 'NONE'            })  -- @vite
  highlight(0, 'bladeViteReactRefresh', { fg = colors.green,     bg = 'NONE'            })  -- @viteReactRefresh


  -----------------------------------------------------------------------------
  -- Alpine.js Integration (x-data, x-bind, etc. in Blade)

  highlight(0, 'bladeAlpineDirective', { fg = colors.purple,     bg = 'NONE'            })  -- x-data, x-bind, etc.
  highlight(0, 'bladeAlpineAtSign',    { fg = colors.purple,     bg = 'NONE'            })  -- @click, @submit (Alpine)


  -----------------------------------------------------------------------------
  -- Component Tags (x-slot, x-component style)

  highlight(0, 'bladeXComponent',      { fg = colors.green,      bg = 'NONE'            })  -- <x-component>
  highlight(0, 'bladeXSlot',           { fg = colors.green,      bg = 'NONE'            })  -- <x-slot>
  highlight(0, 'bladeXDynamic',        { fg = colors.green,      bg = 'NONE'            })  -- <x-dynamic-component>
  highlight(0, 'bladeComponentTag',    { fg = colors.green,      bg = 'NONE'            })  -- Component tag name
  highlight(0, 'bladeComponentAttr',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Component attribute
  highlight(0, 'bladeComponentBind',   { fg = colors.orange,     bg = 'NONE'            })  -- :attribute (bound)
  highlight(0, 'bladeWireModel',       { fg = colors.pink,       bg = 'NONE'            })  -- wire:model
  highlight(0, 'bladeWireClick',       { fg = colors.pink,       bg = 'NONE'            })  -- wire:click


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.blade)

  -- Directives
  highlight(0, '@keyword.blade',               { fg = colors.blue,       bg = 'NONE' })  -- Directives
  highlight(0, '@keyword.directive.blade',     { fg = colors.blue,       bg = 'NONE' })  -- @directive
  highlight(0, '@keyword.conditional.blade',   { fg = colors.blue,       bg = 'NONE' })  -- @if, @unless
  highlight(0, '@keyword.repeat.blade',        { fg = colors.blue,       bg = 'NONE' })  -- @for, @foreach
  highlight(0, '@keyword.import.blade',        { fg = colors.orange,     bg = 'NONE' })  -- @include, @extends
  highlight(0, '@keyword.function.blade',      { fg = colors.green,      bg = 'NONE' })  -- @component

  -- Tags
  highlight(0, '@tag.blade',                   { fg = colors.green,      bg = 'NONE' })  -- x-component tags
  highlight(0, '@tag.attribute.blade',         { fg = colors.turquoise,  bg = 'NONE' })  -- Component attributes
  highlight(0, '@tag.delimiter.blade',         { fg = colors.white,      bg = 'NONE' })  -- < > </ />

  -- Echo/Output
  highlight(0, '@punctuation.bracket.blade',   { fg = colors.orange,     bg = 'NONE' })  -- {{ }} {!! !!}
  highlight(0, '@punctuation.delimiter.blade', { fg = colors.white,      bg = 'NONE' })  -- Delimiters

  -- Strings
  highlight(0, '@string.blade',                { fg = colors.redLight,   bg = 'NONE' })  -- String content

  -- Comments
  highlight(0, '@comment.blade',               { fg = colors.red,        bg = 'NONE' })  -- {{-- --}}

  -- Variables
  highlight(0, '@variable.blade',              { fg = colors.white,      bg = 'NONE' })  -- $variable
  highlight(0, '@variable.builtin.blade',      { fg = colors.purple,     bg = 'NONE' })  -- $loop, $slot

  -- Special
  highlight(0, '@constant.blade',              { fg = colors.purple,     bg = 'NONE' })  -- Constants
  highlight(0, '@function.blade',              { fg = colors.orange,     bg = 'NONE' })  -- Functions


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.blade)

  highlight(0, '@lsp.type.keyword.blade',      { fg = colors.blue,       bg = 'NONE' })  -- Directives
  highlight(0, '@lsp.type.function.blade',     { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.variable.blade',     { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.string.blade',       { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.comment.blade',      { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.class.blade',        { fg = colors.green,      bg = 'NONE' })  -- Components


  -----------------------------------------------------------------------------
  -- Directive Categories (for semantic coloring)

  -- Control Flow → blue
  highlight(0, 'bladeControlFlow',     { fg = colors.blue,       bg = 'NONE'            })

  -- Auth/Authorization → purple
  highlight(0, 'bladeAuthorization',   { fg = colors.purple,     bg = 'NONE'            })

  -- Layout/Template → turquoise
  highlight(0, 'bladeLayout',          { fg = colors.turquoise,  bg = 'NONE'            })

  -- Components → green
  highlight(0, 'bladeComponentDir',    { fg = colors.green,      bg = 'NONE'            })

  -- Includes → orange
  highlight(0, 'bladeIncludeDir',      { fg = colors.orange,     bg = 'NONE'            })

  -- Livewire → pink
  highlight(0, 'bladeLivewireDir',     { fg = colors.pink,       bg = 'NONE', bold = true })


  -----------------------------------------------------------------------------
  -- Error Highlighting

  highlight(0, 'bladeError',           { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax error
  highlight(0, 'bladeUnmatchedDir',    { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Unmatched directive


  -----------------------------------------------------------------------------
  -- Links

  highlight(0, 'bladePhpBlock',        { link = 'bladePhpRegion' })
  highlight(0, 'bladePhpInline',       { link = 'bladePhpRegion' })
end

return laravelBlade
