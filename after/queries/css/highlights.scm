; extends

; Override ID selectors to use turquoise (Type) instead of purple (Constant)
((id_name) @type (#set! priority 200))

; Universal selector '*' in blue (Keyword)
((universal_selector) @keyword (#set! priority 200))

; CSS keyword values (flex, row, wrap, center, space-between, block, border-box, etc.)
((plain_value) @keyword (#set! priority 150))

; CSS custom properties (--variables) stay purple
((plain_value) @variable
  (#lua-match? @variable "^[-][-]")
  (#set! priority 200))
