; extends

; Built-in type tags (!!str, !!int, !!float, etc.) - keyword blue
((tag) @keyword
  (#lua-match? @keyword "^!!")
  (#set! priority 200))

; Custom tags (!custom/date, !myapp/user, etc.) - purple
((tag) @constant
  (#lua-match? @constant "^![^!]")
  (#set! priority 200))

; Terminal commands (run:, command: values) - green
; Plain scalar value
(block_mapping_pair
  key: (flow_node
    (plain_scalar
      (string_scalar) @_key))
  value: (flow_node
    (plain_scalar
      (string_scalar) @string.special.yaml))
  (#any-of? @_key "run" "command")
  (#set! priority 200))

; Double-quoted value
(block_mapping_pair
  key: (flow_node
    (plain_scalar
      (string_scalar) @_key))
  value: (flow_node
    (double_quote_scalar) @string.special.yaml)
  (#any-of? @_key "run" "command")
  (#set! priority 200))

; Single-quoted value
(block_mapping_pair
  key: (flow_node
    (plain_scalar
      (string_scalar) @_key))
  value: (flow_node
    (single_quote_scalar) @string.special.yaml)
  (#any-of? @_key "run" "command")
  (#set! priority 200))

; Block scalar value (multiline |, >)
(block_mapping_pair
  key: (flow_node
    (plain_scalar
      (string_scalar) @_key))
  value: (block_node
    (block_scalar) @string.special.yaml)
  (#any-of? @_key "run" "command")
  (#set! priority 200))
