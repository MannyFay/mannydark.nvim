; extends
; Prioritize bracket highlighting over Normal, but below diagnostics
(["(" ")" "[" "]" "{" "}"] @punctuation.bracket (#set! priority 110))
