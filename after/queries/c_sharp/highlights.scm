; extends

; Parameter modifiers (out, ref, in) - blue keywords
((_) @keyword.modifier
  (#any-of? @keyword.modifier "out" "ref" "in")
  (#set! priority 200))
