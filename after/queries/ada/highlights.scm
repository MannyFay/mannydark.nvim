; extends

; Constant declarations (number_declaration) - e.g., Max_Size : constant := 100;
(number_declaration
  (identifier) @constant)

; Object declarations with constant modifier - e.g., Version : constant String := "1.0.0";
(object_declaration
  "constant"
  .
  (identifier) @constant)

; Regular object declarations (variables) - e.g., Counter : Integer := 0;
(object_declaration
  (identifier) @variable)

; Type names in declarations
(full_type_declaration
  (identifier) @type.definition)

(subtype_declaration
  (identifier) @type.definition)

; Private type declarations - e.g., type Stack is private;
(private_type_declaration
  (identifier) @type.definition)

; Exception declarations - e.g., My_Error : exception;
(exception_declaration
  (identifier) @type.definition)

; Parameter names in function/procedure signatures
(parameter_specification
  (identifier) @variable.parameter)

; Discriminant names (e.g., Size in "type Buffer (Size : Positive)")
(discriminant_specification
  (identifier) @constant)

; Discriminant/constant references in index constraints (e.g., "String (1 .. Size)")
(index_constraint
  (range_g
    (term
      (identifier) @constant
      (#not-any-of? @constant
        "Integer" "Float" "Boolean" "Character" "String"
        "Natural" "Positive" "Duration" "Long_Integer" "Short_Integer"
        "Long_Float" "Short_Float" "Long_Long_Integer" "Long_Long_Float"
        "Wide_Character" "Wide_String" "Wide_Wide_Character" "Wide_Wide_String"
        "Unbounded_String" "Bounded_String"
        "Time" "Year_Number" "Month_Number" "Day_Number" "Day_Duration"))))

; Record component names
(component_declaration
  (identifier) @variable.member)

; Enumeration values
(enumeration_type_definition
  (identifier) @constant)

; Enumeration representation clause - the type name after "for"
(enumeration_representation_clause
  "for"
  (identifier) @type)

; Enumeration representation clause - the enum values
(enumeration_representation_clause
  (enumeration_aggregate
    (named_array_aggregate
      (array_component_association
        (discrete_choice_list
          (discrete_choice
            (expression
              (term
                (identifier) @constant))))))))

; Subtype range constraint - enum values as range bounds (e.g., Monday .. Friday)
; Excludes built-in type names which are handled by @type.builtin
(subtype_declaration
  (range_constraint
    (range_g
      (term
        (identifier) @constant
        (#not-any-of? @constant
          "Integer" "Float" "Boolean" "Character" "String"
          "Natural" "Positive" "Duration" "Long_Integer" "Short_Integer"
          "Long_Float" "Short_Float" "Long_Long_Integer" "Long_Long_Float"
          "Wide_Character" "Wide_String" "Wide_Wide_Character" "Wide_Wide_String")))))

; Built-in types (Float, Integer, Boolean, etc.)
((identifier) @type.builtin
  (#any-of? @type.builtin
    "Integer" "Float" "Boolean" "Character" "String"
    "Natural" "Positive" "Duration" "Long_Integer" "Short_Integer"
    "Long_Float" "Short_Float" "Long_Long_Integer" "Long_Long_Float"
    "Wide_Character" "Wide_String" "Wide_Wide_Character" "Wide_Wide_String"
    "Unbounded_String" "Bounded_String"
    "Time" "Year_Number" "Month_Number" "Day_Number" "Day_Duration"))

; Boolean literals (True, False) - colored like keywords (blue)
((identifier) @boolean
  (#any-of? @boolean "True" "False"))

; Attributes (e.g., 'Last, 'First, 'Range, 'Image, etc.) - colored like functions
(attribute_designator
  (identifier) @function.builtin)
