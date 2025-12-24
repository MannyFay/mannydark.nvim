; extends

; HTML tag names - keyword blue
((tag_name) @tag.mannydark
  (#set! priority 200))

; HTML tag delimiters - gray
; HTML uses simple literal matching (NOT jsx_element nodes!)
(["<" ">" "</" "/>"] @tag.delimiter
  (#set! priority 200))
