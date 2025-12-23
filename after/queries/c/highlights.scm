; extends

; Doc blocks (/** ... */ or ///) - green
((comment) @comment.documentation
  (#lua-match? @comment.documentation "^/%*%*")
  (#set! priority 200))

((comment) @comment.documentation
  (#lua-match? @comment.documentation "^///")
  (#set! priority 200))
