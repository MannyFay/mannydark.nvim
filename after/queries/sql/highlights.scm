; extends

; Database name in CREATE DATABASE - turquoise (first identifier after keyword_database)
(create_database
  (keyword_database)
  .
  (identifier) @type.mannydark
  (#set! priority 200))

; Schema name in CREATE SCHEMA - turquoise (works with IF NOT EXISTS too)
(create_schema
  (identifier) @type.mannydark
  (#set! priority 200))

; Table names - turquoise
(create_table
  (object_reference
    name: (identifier) @type.mannydark)
  (#set! priority 200))

(object_reference
  name: (identifier) @type.mannydark
  (#set! priority 200))

; Column names - purple
(column_definition
  name: (identifier) @variable.member.mannydark
  (#set! priority 200))

(field
  name: (identifier) @variable.member.mannydark
  (#set! priority 205))

; Constraint names - turquoise (like table names)
(constraint
  name: (identifier) @type.mannydark
  (#set! priority 200))

; PostgreSQL CREATE DATABASE options that should be keywords (parsed as identifiers by treesitter)
((identifier) @keyword.mannydark
  (#any-of? @keyword.mannydark
    "OWNER" "ENCODING" "LC_COLLATE" "LC_CTYPE" "TABLESPACE" "CONNECTION" "LIMIT"
    "TEMPLATE" "ALLOW_CONNECTIONS" "IS_TEMPLATE" "OID"
    "LOCALE" "LOCALE_PROVIDER" "COLLATION_VERSION" "ICU_LOCALE" "ICU_RULES"
    "STRATEGY" "BUILTIN" "LIBC" "ICU"
    "IDENTITY"
    "BLOB" "BYTEA" "CLOB" "BINARY" "VARBINARY"
    "MACADDR" "MACADDR8" "INET" "CIDR"
    "POINT" "LINE" "LSEG" "BOX" "POLYGON" "CIRCLE" "PATH"
    "JSONB" "JSON" "TSVECTOR" "TSQUERY" "XML"
    "MONEY" "BIT" "VARBIT" "INTERVAL" "ARRAY"
    "DATERANGE" "TSRANGE" "TSTZRANGE" "INT4RANGE" "INT8RANGE" "NUMRANGE")
  (#set! priority 210))

; PostgreSQL/MySQL common option keywords
((identifier) @keyword.mannydark
  (#any-of? @keyword.mannydark
    "ENGINE" "CHARSET" "COLLATE" "AUTO_INCREMENT" "ROW_FORMAT"
    "COMMENT" "COMPRESSION" "ENCRYPTION" "TABLESPACE"
    "STORAGE" "DISK" "MEMORY" "COLUMN_FORMAT" "FIXED" "DYNAMIC"
    "STATS_AUTO_RECALC" "STATS_PERSISTENT" "STATS_SAMPLE_PAGES")
  (#set! priority 210))

; ARRAY constructor - keyword
(array
  (keyword_array) @keyword.mannydark
  (#set! priority 220))

; Function calls - orange (higher priority than @type)
(invocation
  (object_reference
    name: (identifier) @function.call.mannydark)
  (#set! priority 210))

; Numbers inside ERROR nodes (parser failures)
(ERROR
  (literal) @number.mannydark
  (#set! priority 200))
