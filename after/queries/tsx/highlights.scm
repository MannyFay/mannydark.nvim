; extends

; JSX tag delimiters - gray (higher priority)
(jsx_element
  open_tag: (jsx_opening_element
    ["<" ">"] @tag.delimiter)
  (#set! priority 200))

(jsx_element
  close_tag: (jsx_closing_element
    ["</" ">"] @tag.delimiter)
  (#set! priority 200))

(jsx_self_closing_element
  ["<" "/>"] @tag.delimiter
  (#set! priority 200))

; bigint type as keyword (blue)
((_) @keyword
  (#eq? @keyword "bigint")
  (#set! priority 200))

; null, undefined as keyword (blue)
((_) @keyword
  (#eq? @keyword "null")
  (#set! priority 200))

((_) @keyword
  (#eq? @keyword "undefined")
  (#set! priority 200))

; Error as type (turquoise) - override the blue @type.builtin
((identifier) @type
  (#eq? @type "Error")
  (#set! priority 200))

; JSX component names (capitalized) - turquoise
; Opening tag: <Component>
(jsx_opening_element
  name: (identifier) @type
  (#lua-match? @type "^[A-Z]")
  (#set! priority 200))

; Closing tag: </Component>
(jsx_closing_element
  name: (identifier) @type
  (#lua-match? @type "^[A-Z]")
  (#set! priority 200))

; Self-closing: <Component />
(jsx_self_closing_element
  name: (identifier) @type
  (#lua-match? @type "^[A-Z]")
  (#set! priority 200))

; Compound components: <Tabs.List> - the member expression part
(jsx_opening_element
  name: (member_expression) @type
  (#set! priority 200))

(jsx_closing_element
  name: (member_expression) @type
  (#set! priority 200))

(jsx_self_closing_element
  name: (member_expression) @type
  (#set! priority 200))
