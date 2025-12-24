; extends

; Blade comments ({{-- ... --}}) - red with very high priority to override HTML injection
((comment) @comment
  (#set! priority 300))

; Blade directives (@if, @foreach, @include, @vite, etc.) - keyword blue
([
  (directive)
  (directive_start)
  (directive_end)
] @keyword
  (#set! priority 200))
