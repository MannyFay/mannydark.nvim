; extends

; Parameter modifiers (out, ref, in) - blue keywords
((_) @keyword.modifier
  (#any-of? @keyword.modifier "out" "ref" "in")
  (#set! priority 200))

; LINQ join clause keywords
(join_clause
  "join" @keyword
  (#set! priority 200))

(join_clause
  "in" @keyword
  (#set! priority 200))

(join_clause
  "on" @keyword
  (#set! priority 200))

(join_clause
  "equals" @keyword
  (#set! priority 200))

(join_into_clause
  "into" @keyword
  (#set! priority 200))
