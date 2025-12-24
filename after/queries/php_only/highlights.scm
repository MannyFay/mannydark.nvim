; extends

; "as" keyword in foreach - blue
("as" @keyword.mannydark
  (#set! priority 200))

; Function calls - orange
(function_call_expression
  function: [
    (name) @function.call.mannydark
    (qualified_name (name) @function.call.mannydark)
  ]
  (#set! priority 200))

; Member/property access ($user->name) - purple
(member_access_expression
  name: (name) @variable.member.mannydark
  (#set! priority 200))

; Member names inside ERROR nodes (blade template parsing issues) - purple
(member_access_expression
  (ERROR
    (name) @variable.member.mannydark)
  (#set! priority 200))

; Strings - redLight
([
  (string)
  (encapsed_string)
] @string.mannydark
  (#set! priority 200))
