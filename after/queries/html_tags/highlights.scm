; extends

; Override strikethrough - disable spell checking which may add underline
((element
  (start_tag
    (tag_name) @_tag)
  (text) @markup.strikethrough @nospell)
  (#any-of? @_tag "s" "del")
  (#set! priority 200))

; Override underline - disable spell checking
((element
  (start_tag
    (tag_name) @_tag)
  (text) @markup.underline @nospell)
  (#eq? @_tag "u")
  (#set! priority 200))
