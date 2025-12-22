; extends

; PHP opening tag <?php - blue
((php_tag) @keyword
  (#set! priority 200))

; Doc blocks (/** ... */) - green
((comment) @comment.documentation
  (#lua-match? @comment.documentation "^/%*%*"))

; Class constants and enum cases after :: with relative_scope (self::Red, static::Value)
(class_constant_access_expression
  (relative_scope)
  (name) @constant)

; Enum case definitions (case Red = 0xFF0000)
(enum_case
  name: (name) @constant)
