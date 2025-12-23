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
