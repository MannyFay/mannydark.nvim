; extends

; JSX tag delimiters - gray
; JavaScript/JSX uses jsx_element nodes (NOT simple literals!)
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

; null, undefined as keyword (blue)
((null) @keyword
  (#set! priority 200))

((undefined) @keyword
  (#set! priority 200))

; Built-in types (Object, Array, String, etc.) - turquoise
((identifier) @type.builtin
  (#any-of? @type.builtin "Object" "Array" "String" "Number" "Boolean" "Function" "Symbol" "BigInt" "Map" "Set" "WeakMap" "WeakSet" "Promise" "Date" "RegExp" "Error" "JSON" "Math" "Intl" "Proxy" "Reflect")
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
