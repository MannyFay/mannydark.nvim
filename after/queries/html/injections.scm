; extends

; JSON-LD: <script type="application/ld+json">
(script_element
  (start_tag
    (attribute
      (attribute_name) @_attr
      (quoted_attribute_value
        (attribute_value) @_type)))
  (raw_text) @injection.content
  (#eq? @_attr "type")
  (#any-of? @_type "application/ld+json" "application/json")
  (#set! injection.language "json"))
