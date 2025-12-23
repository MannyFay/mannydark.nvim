; extends

; declare/typeset flags (-r, -i, -a, -A, -x, -l, -u, etc.) - keyword blue
(declaration_command
  (word) @keyword
  (#lua-match? @keyword "^-")
  (#set! priority 200))

; Loop control keywords - blue
((command_name
  (word) @keyword)
  (#any-of? @keyword "break" "continue")
  (#set! priority 200))

; Command flags/options (-r, +e, --verbose, etc.) - green
((command
  argument: (word) @function.call)
  (#lua-match? @function.call "^[%-+]")
  (#set! priority 200))

; Flags in concatenations (--include="*.txt") - green
((command
  argument: (concatenation
    (word) @function.call))
  (#lua-match? @function.call "^[%-+]")
  (#set! priority 200))

; Paths (/, ./, ../, ~/) - green
((command
  argument: (word) @function.call)
  (#lua-match? @function.call "^[/~%.]")
  (#set! priority 200))

; Booleans (true, false) - keyword blue
((word) @keyword
  (#any-of? @keyword "true" "false")
  (#set! priority 200))

; Test operators in conditionals [[ ]] - keyword blue
((test_operator) @keyword
  (#set! priority 200))

; Case patterns - string color (redLight)
(case_item
  value: (word) @string
  (#set! priority 200))
