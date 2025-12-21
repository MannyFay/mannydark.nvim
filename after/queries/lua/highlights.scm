; extends
; Prioritize bracket highlighting over Normal
(["(" ")" "[" "]" "{" "}"] @punctuation.bracket (#set! priority 150))
