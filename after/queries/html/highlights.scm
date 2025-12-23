; extends

; HTML tag delimiters - gray
; HTML uses simple literal matching (NOT jsx_element nodes!)
(["<" ">" "</" "/>"] @tag.delimiter
  (#set! priority 200))
