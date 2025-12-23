; extends

; Doc blocks (/** ... */ or ///) - green
((comment) @comment.documentation
  (#lua-match? @comment.documentation "^/%*%*")
  (#set! priority 200))

((comment) @comment.documentation
  (#lua-match? @comment.documentation "^///")
  (#set! priority 200))

; inline keyword - blue
("inline" @keyword
  (#set! priority 200))

; Primitive/built-in types - keyword blue
((primitive_type) @keyword
  (#set! priority 200))

((sized_type_specifier) @keyword
  (#set! priority 200))

; Attributes - brackets white, name keyword blue
(attribute_declaration
  "[[" @punctuation.bracket
  "]]" @punctuation.bracket
  (#set! priority 200))

((attribute) @keyword
  (#set! priority 200))
