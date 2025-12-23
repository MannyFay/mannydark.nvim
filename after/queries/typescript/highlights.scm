; extends

; bigint type as keyword (blue)
((_) @keyword
  (#eq? @keyword "bigint")
  (#set! priority 200))

; null, undefined as keyword (blue)
((_) @keyword
  (#eq? @keyword "null")
  (#set! priority 200))

((_) @keyword
  (#eq? @keyword "undefined")
  (#set! priority 200))

; Error as type (turquoise) - override the blue @type.builtin.typescript
((identifier) @type
  (#eq? @type "Error")
  (#set! priority 200))
