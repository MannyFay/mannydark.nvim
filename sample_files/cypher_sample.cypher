// ==============================================================================
// Comprehensive Cypher Sample - Syntax Highlighting Demonstration
// ==============================================================================

// This file demonstrates all major Neo4j Cypher language features
// for syntax highlighting purposes.

// ==============================================================================
// Comments
// ==============================================================================

// Single line comment
/* Multi-line
   comment */

// ==============================================================================
// Node Patterns
// ==============================================================================

// Simple node pattern
MATCH (n)
RETURN n;

// Node with label
MATCH (p:Person)
RETURN p;

// Node with multiple labels
MATCH (e:Employee:Manager:Person)
RETURN e;

// Node with properties
MATCH (p:Person {name: 'Alice', age: 30})
RETURN p;

// Node with variable
MATCH (person:Person)
RETURN person.name, person.age;

// Anonymous node
MATCH (:Person)-[:KNOWS]->(:Person)
RETURN count(*);

// ==============================================================================
// Relationship Patterns
// ==============================================================================

// Undirected relationship
MATCH (a)-[r]-(b)
RETURN a, r, b;

// Directed relationships
MATCH (a)-[r]->(b)
RETURN a, r, b;

MATCH (a)<-[r]-(b)
RETURN a, r, b;

// Relationship with type
MATCH (a)-[:KNOWS]->(b)
RETURN a, b;

// Relationship with variable
MATCH (a)-[rel:KNOWS]->(b)
RETURN rel;

// Multiple relationship types
MATCH (a)-[:KNOWS|:FRIENDS_WITH|:WORKS_WITH]->(b)
RETURN a, b;

// Relationship with properties
MATCH (a)-[r:KNOWS {since: 2020}]->(b)
RETURN a, r, b;

// Variable-length relationships
MATCH (a)-[*]->(b)
RETURN a, b;

MATCH (a)-[*2]->(b)      // Exactly 2 hops
RETURN a, b;

MATCH (a)-[*1..3]->(b)   // 1 to 3 hops
RETURN a, b;

MATCH (a)-[*..5]->(b)    // Up to 5 hops
RETURN a, b;

MATCH (a)-[*3..]->(b)    // 3 or more hops
RETURN a, b;

// Variable-length with type and variable
MATCH (a)-[r:KNOWS*1..5]->(b)
RETURN a, relationships(r), b;

// ==============================================================================
// Path Patterns
// ==============================================================================

// Named path
MATCH path = (a:Person)-[:KNOWS*]->(b:Person)
RETURN path, length(path);

// Shortest path
MATCH (a:Person {name: 'Alice'}), (b:Person {name: 'Bob'})
MATCH path = shortestPath((a)-[*..10]-(b))
RETURN path;

// All shortest paths
MATCH (a:Person {name: 'Alice'}), (b:Person {name: 'Bob'})
MATCH paths = allShortestPaths((a)-[*..10]-(b))
RETURN paths;

// Complex path pattern
MATCH (a:Person)-[:WORKS_AT]->(c:Company)<-[:WORKS_AT]-(b:Person),
      (a)-[:KNOWS]-(b)
RETURN a, b, c;

// ==============================================================================
// CREATE Statements
// ==============================================================================

// Create node
CREATE (n:Person {name: 'Alice', age: 30});

// Create multiple nodes
CREATE (a:Person {name: 'Alice'}),
       (b:Person {name: 'Bob'}),
       (c:Company {name: 'TechCorp'});

// Create node and return
CREATE (p:Person {name: 'Charlie', born: 1990})
RETURN p;

// Create relationship
MATCH (a:Person {name: 'Alice'}), (b:Person {name: 'Bob'})
CREATE (a)-[:KNOWS {since: 2020}]->(b);

// Create path
CREATE path = (a:Person {name: 'Alice'})-[:KNOWS]->(b:Person {name: 'Bob'})-[:KNOWS]->(c:Person {name: 'Charlie'})
RETURN path;

// Create with MERGE pattern
CREATE (p:Person {name: 'David'})
CREATE (p)-[:HAS_ADDRESS]->(:Address {city: 'Seattle', state: 'WA'});

// ==============================================================================
// MERGE Statements
// ==============================================================================

// Merge node
MERGE (p:Person {name: 'Alice'})
RETURN p;

// Merge with ON CREATE
MERGE (p:Person {name: 'Alice'})
ON CREATE SET p.created = timestamp()
RETURN p;

// Merge with ON MATCH
MERGE (p:Person {name: 'Alice'})
ON MATCH SET p.lastSeen = timestamp()
RETURN p;

// Merge with both
MERGE (p:Person {name: 'Alice'})
ON CREATE SET p.created = timestamp(), p.updated = timestamp()
ON MATCH SET p.updated = timestamp()
RETURN p;

// Merge relationship
MATCH (a:Person {name: 'Alice'}), (b:Person {name: 'Bob'})
MERGE (a)-[r:KNOWS]->(b)
ON CREATE SET r.since = date()
RETURN r;

// Merge entire path
MERGE (a:Person {name: 'Alice'})-[:KNOWS]->(b:Person {name: 'Bob'})
RETURN a, b;

// ==============================================================================
// SET and REMOVE
// ==============================================================================

// Set property
MATCH (p:Person {name: 'Alice'})
SET p.age = 31;

// Set multiple properties
MATCH (p:Person {name: 'Alice'})
SET p.age = 31, p.city = 'Seattle', p.updated = timestamp();

// Set properties from map
MATCH (p:Person {name: 'Alice'})
SET p += {age: 31, city: 'Seattle'};

// Replace all properties
MATCH (p:Person {name: 'Alice'})
SET p = {name: 'Alice', age: 31, city: 'Seattle'};

// Set label
MATCH (p:Person {name: 'Alice'})
SET p:Employee;

// Set multiple labels
MATCH (p:Person {name: 'Alice'})
SET p:Employee:Manager;

// Remove property
MATCH (p:Person {name: 'Alice'})
REMOVE p.age;

// Remove property with SET null
MATCH (p:Person {name: 'Alice'})
SET p.temporary = null;

// Remove label
MATCH (p:Person:Employee {name: 'Alice'})
REMOVE p:Employee;

// ==============================================================================
// DELETE
// ==============================================================================

// Delete node
MATCH (p:Person {name: 'Temporary'})
DELETE p;

// Delete relationship
MATCH (a:Person)-[r:KNOWS]->(b:Person)
WHERE a.name = 'Alice' AND b.name = 'Bob'
DELETE r;

// Delete node and relationships
MATCH (p:Person {name: 'Alice'})
DETACH DELETE p;

// Delete all
MATCH (n)
DETACH DELETE n;

// ==============================================================================
// WHERE Clause
// ==============================================================================

// Equality
MATCH (p:Person)
WHERE p.name = 'Alice'
RETURN p;

// Comparison operators
MATCH (p:Person)
WHERE p.age > 18 AND p.age <= 65
RETURN p;

MATCH (p:Person)
WHERE p.age <> 30
RETURN p;

// Boolean operators
MATCH (p:Person)
WHERE p.age >= 21 AND (p.city = 'Seattle' OR p.city = 'Portland')
RETURN p;

MATCH (p:Person)
WHERE NOT p.retired
RETURN p;

// XOR
MATCH (p:Person)
WHERE p.employed XOR p.retired
RETURN p;

// NULL checks
MATCH (p:Person)
WHERE p.email IS NOT NULL
RETURN p;

MATCH (p:Person)
WHERE p.middleName IS NULL
RETURN p;

// String matching
MATCH (p:Person)
WHERE p.name STARTS WITH 'Al'
RETURN p;

MATCH (p:Person)
WHERE p.name ENDS WITH 'ice'
RETURN p;

MATCH (p:Person)
WHERE p.name CONTAINS 'lic'
RETURN p;

// Regular expressions
MATCH (p:Person)
WHERE p.email =~ '.*@gmail\\.com'
RETURN p;

// Case insensitive regex
MATCH (p:Person)
WHERE p.name =~ '(?i)alice'
RETURN p;

// IN list
MATCH (p:Person)
WHERE p.name IN ['Alice', 'Bob', 'Charlie']
RETURN p;

// Property existence
MATCH (p:Person)
WHERE exists(p.email)
RETURN p;

// Pattern in WHERE (exists pattern)
MATCH (p:Person)
WHERE (p)-[:KNOWS]->(:Person)
RETURN p;

// NOT exists pattern
MATCH (p:Person)
WHERE NOT (p)-[:WORKS_AT]->(:Company)
RETURN p;

// Path predicate
MATCH (p:Person), (c:Company)
WHERE (p)-[:WORKS_AT]->(c)
RETURN p, c;

// ==============================================================================
// RETURN Clause
// ==============================================================================

// Return all
MATCH (p:Person)
RETURN *;

// Return specific properties
MATCH (p:Person)
RETURN p.name, p.age;

// Return with alias
MATCH (p:Person)
RETURN p.name AS name, p.age AS age;

// Return node
MATCH (p:Person)
RETURN p;

// Return relationship
MATCH (a)-[r:KNOWS]->(b)
RETURN r;

// Return path
MATCH path = (a)-[:KNOWS*]->(b)
RETURN path;

// Return map projection
MATCH (p:Person)
RETURN p {.name, .age, .city};

// Return map with additional properties
MATCH (p:Person)
RETURN p {.name, .age, friendCount: size((p)-[:KNOWS]->())};

// Return expression
MATCH (p:Person)
RETURN p.firstName + ' ' + p.lastName AS fullName;

// DISTINCT
MATCH (p:Person)-[:WORKS_AT]->(c:Company)
RETURN DISTINCT c.name;

// ==============================================================================
// ORDER BY, SKIP, LIMIT
// ==============================================================================

// Order by ascending
MATCH (p:Person)
RETURN p.name, p.age
ORDER BY p.age;

// Order by descending
MATCH (p:Person)
RETURN p.name, p.age
ORDER BY p.age DESC;

// Multiple order by
MATCH (p:Person)
RETURN p.name, p.age
ORDER BY p.city, p.age DESC;

// Order by expression
MATCH (p:Person)
RETURN p.name, p.age
ORDER BY toLower(p.name);

// Skip
MATCH (p:Person)
RETURN p
ORDER BY p.name
SKIP 10;

// Limit
MATCH (p:Person)
RETURN p
ORDER BY p.name
LIMIT 10;

// Skip and Limit (pagination)
MATCH (p:Person)
RETURN p
ORDER BY p.name
SKIP 20
LIMIT 10;

// ==============================================================================
// Aggregation Functions
// ==============================================================================

// Count
MATCH (p:Person)
RETURN count(p) AS personCount;

// Count distinct
MATCH (p:Person)-[:WORKS_AT]->(c:Company)
RETURN count(DISTINCT c) AS companyCount;

// Sum
MATCH (p:Person)-[r:PURCHASED]->(product)
RETURN sum(r.amount) AS totalSpent;

// Average
MATCH (p:Person)
RETURN avg(p.age) AS averageAge;

// Min and Max
MATCH (p:Person)
RETURN min(p.age) AS youngest, max(p.age) AS oldest;

// Collect into list
MATCH (p:Person)-[:WORKS_AT]->(c:Company)
RETURN c.name, collect(p.name) AS employees;

// Percentile
MATCH (p:Person)
RETURN percentileDisc(p.age, 0.5) AS medianAge,
       percentileCont(p.age, 0.9) AS p90Age;

// Standard deviation
MATCH (p:Person)
RETURN stDev(p.age) AS stdDevAge;

// ==============================================================================
// WITH Clause
// ==============================================================================

// Chain queries
MATCH (p:Person)
WITH p
ORDER BY p.age DESC
LIMIT 10
RETURN p.name;

// Aggregate then filter
MATCH (p:Person)-[:KNOWS]->(friend)
WITH p, count(friend) AS friendCount
WHERE friendCount > 5
RETURN p.name, friendCount;

// Multiple WITH
MATCH (p:Person)
WITH p
WHERE p.age > 21
WITH p
ORDER BY p.name
RETURN p;

// WITH for subquery-like behavior
MATCH (c:Company)
WITH c, size((c)<-[:WORKS_AT]-()) AS employeeCount
WHERE employeeCount > 100
RETURN c.name, employeeCount;

// ==============================================================================
// UNWIND
// ==============================================================================

// Unwind list
UNWIND [1, 2, 3, 4, 5] AS number
RETURN number;

// Unwind and create
UNWIND ['Alice', 'Bob', 'Charlie'] AS name
CREATE (p:Person {name: name});

// Unwind with index
WITH ['a', 'b', 'c'] AS letters
UNWIND range(0, size(letters) - 1) AS idx
RETURN idx, letters[idx];

// Unwind from property
MATCH (p:Person)
WHERE p.tags IS NOT NULL
UNWIND p.tags AS tag
RETURN DISTINCT tag;

// ==============================================================================
// OPTIONAL MATCH
// ==============================================================================

// Optional match
MATCH (p:Person {name: 'Alice'})
OPTIONAL MATCH (p)-[:WORKS_AT]->(c:Company)
RETURN p.name, c.name;

// Multiple optional matches
MATCH (p:Person)
OPTIONAL MATCH (p)-[:HAS_ADDRESS]->(a:Address)
OPTIONAL MATCH (p)-[:HAS_PHONE]->(ph:Phone)
RETURN p.name, a.city, ph.number;

// ==============================================================================
// CASE Expression
// ==============================================================================

// Simple CASE
MATCH (p:Person)
RETURN p.name,
       CASE p.status
         WHEN 'active' THEN 'Active User'
         WHEN 'inactive' THEN 'Inactive User'
         ELSE 'Unknown'
       END AS statusLabel;

// Searched CASE
MATCH (p:Person)
RETURN p.name,
       CASE
         WHEN p.age < 18 THEN 'Minor'
         WHEN p.age < 65 THEN 'Adult'
         ELSE 'Senior'
       END AS ageGroup;

// CASE with null handling
MATCH (p:Person)
RETURN p.name,
       CASE
         WHEN p.email IS NULL THEN 'No email'
         ELSE p.email
       END AS email;

// ==============================================================================
// Subqueries
// ==============================================================================

// EXISTS subquery
MATCH (p:Person)
WHERE EXISTS {
  MATCH (p)-[:WORKS_AT]->(:Company {name: 'TechCorp'})
}
RETURN p;

// COUNT subquery
MATCH (p:Person)
WHERE COUNT {
  MATCH (p)-[:KNOWS]->()
} > 5
RETURN p;

// CALL subquery
MATCH (p:Person)
CALL {
  WITH p
  MATCH (p)-[:KNOWS]->(friend)
  RETURN count(friend) AS friendCount
}
RETURN p.name, friendCount;

// CALL subquery with UNION
MATCH (p:Person)
CALL {
  WITH p
  MATCH (p)-[:KNOWS]->(friend:Person)
  RETURN friend.name AS connection
  UNION
  WITH p
  MATCH (p)-[:WORKS_WITH]->(colleague:Person)
  RETURN colleague.name AS connection
}
RETURN p.name, collect(DISTINCT connection) AS connections;

// ==============================================================================
// UNION
// ==============================================================================

// Union
MATCH (p:Person)
RETURN p.name AS name, 'Person' AS type
UNION
MATCH (c:Company)
RETURN c.name AS name, 'Company' AS type;

// Union All
MATCH (a)-[:KNOWS]->(b)
RETURN a.name, b.name
UNION ALL
MATCH (a)-[:WORKS_WITH]->(b)
RETURN a.name, b.name;

// ==============================================================================
// Functions - String
// ==============================================================================

RETURN
  // Case conversion
  toLower('HELLO') AS lower,
  toUpper('hello') AS upper,

  // Trimming
  trim('  hello  ') AS trimmed,
  ltrim('  hello') AS leftTrimmed,
  rtrim('hello  ') AS rightTrimmed,

  // Substring
  substring('hello', 0, 3) AS substr,
  left('hello', 3) AS leftChars,
  right('hello', 3) AS rightChars,

  // Replace
  replace('hello world', 'world', 'cypher') AS replaced,

  // Split and join
  split('a,b,c', ',') AS splitResult,

  // Length
  size('hello') AS stringLength,

  // Reverse
  reverse('hello') AS reversed,

  // String representation
  toString(123) AS numToString,
  toInteger('123') AS stringToInt,
  toFloat('3.14') AS stringToFloat;

// ==============================================================================
// Functions - Numeric
// ==============================================================================

RETURN
  // Rounding
  round(3.7) AS rounded,
  floor(3.7) AS floored,
  ceil(3.2) AS ceiled,

  // Absolute
  abs(-42) AS absolute,

  // Sign
  sign(-10) AS signNeg,
  sign(10) AS signPos,

  // Mathematical
  sqrt(16) AS squareRoot,
  exp(1) AS exponential,
  log(10) AS naturalLog,
  log10(100) AS log10,

  // Trigonometric
  sin(0) AS sine,
  cos(0) AS cosine,
  tan(0) AS tangent,
  asin(0) AS arcSine,
  acos(1) AS arcCosine,
  atan(0) AS arcTangent,

  // Random
  rand() AS random,

  // Range
  range(0, 10) AS rangeList,
  range(0, 10, 2) AS rangeWithStep;

// ==============================================================================
// Functions - List
// ==============================================================================

WITH [1, 2, 3, 4, 5] AS numbers
RETURN
  // Size
  size(numbers) AS listSize,

  // Access
  head(numbers) AS first,
  last(numbers) AS lastItem,
  numbers[0] AS firstByIndex,
  numbers[-1] AS lastByIndex,
  numbers[1..3] AS slice,

  // Modification
  tail(numbers) AS withoutFirst,
  reverse(numbers) AS reversed,

  // Check
  1 IN numbers AS containsOne,
  single(x IN numbers WHERE x > 4) AS singleMatch,
  any(x IN numbers WHERE x > 3) AS anyGreaterThan3,
  all(x IN numbers WHERE x > 0) AS allPositive,
  none(x IN numbers WHERE x < 0) AS noneNegative;

// List comprehension
WITH [1, 2, 3, 4, 5] AS numbers
RETURN
  [x IN numbers | x * 2] AS doubled,
  [x IN numbers WHERE x > 2] AS filtered,
  [x IN numbers WHERE x > 2 | x * 2] AS filteredAndDoubled;

// Reduce
WITH [1, 2, 3, 4, 5] AS numbers
RETURN reduce(total = 0, x IN numbers | total + x) AS sum;

// ==============================================================================
// Functions - Map
// ==============================================================================

WITH {name: 'Alice', age: 30, city: 'Seattle'} AS person
RETURN
  keys(person) AS mapKeys,
  person.name AS nameValue,
  person['age'] AS ageValue;

// ==============================================================================
// Functions - Date and Time
// ==============================================================================

RETURN
  // Current
  date() AS today,
  datetime() AS now,
  time() AS currentTime,
  localdatetime() AS localNow,

  // From components
  date({year: 2024, month: 1, day: 15}) AS specificDate,
  datetime({year: 2024, month: 1, day: 15, hour: 12, minute: 30}) AS specificDateTime,
  time({hour: 12, minute: 30, second: 45}) AS specificTime,

  // From string
  date('2024-01-15') AS parsedDate,
  datetime('2024-01-15T12:30:00Z') AS parsedDateTime,

  // Components
  date().year AS year,
  date().month AS month,
  date().day AS day,
  datetime().hour AS hour,
  datetime().minute AS minute,

  // Duration
  duration({days: 7}) AS oneWeek,
  duration({hours: 24}) AS oneDay,
  duration('P1Y2M3D') AS iso8601Duration,

  // Arithmetic
  date() + duration({days: 7}) AS nextWeek,
  datetime() - duration({hours: 1}) AS oneHourAgo;

// ==============================================================================
// Functions - Graph
// ==============================================================================

MATCH (a:Person)-[r:KNOWS]->(b:Person)
RETURN
  // Node info
  id(a) AS nodeId,
  labels(a) AS nodeLabels,
  properties(a) AS nodeProps,

  // Relationship info
  id(r) AS relId,
  type(r) AS relType,
  startNode(r) AS startNode,
  endNode(r) AS endNode,
  properties(r) AS relProps;

// Path functions
MATCH path = (a:Person)-[:KNOWS*1..3]->(b:Person)
RETURN
  length(path) AS pathLength,
  nodes(path) AS pathNodes,
  relationships(path) AS pathRels;

// ==============================================================================
// Functions - APOC (Common Procedures)
// ==============================================================================

// Note: These require APOC plugin

// String utilities
// RETURN apoc.text.join(['a', 'b', 'c'], ', ') AS joined;

// Date utilities
// RETURN apoc.date.format(timestamp(), 'ms', 'yyyy-MM-dd') AS formatted;

// Collection utilities
// RETURN apoc.coll.sort([3, 1, 4, 1, 5]) AS sorted;
// RETURN apoc.coll.flatten([[1, 2], [3, 4]]) AS flattened;

// Map utilities
// RETURN apoc.map.merge({a: 1}, {b: 2}) AS merged;

// ==============================================================================
// Index and Constraint Management
// ==============================================================================

// Create index
CREATE INDEX person_name_index FOR (p:Person) ON (p.name);

// Create composite index
CREATE INDEX person_composite_index FOR (p:Person) ON (p.firstName, p.lastName);

// Create text index
CREATE TEXT INDEX person_bio_text_index FOR (p:Person) ON (p.bio);

// Create range index
CREATE RANGE INDEX person_age_range_index FOR (p:Person) ON (p.age);

// Create point index
CREATE POINT INDEX location_point_index FOR (l:Location) ON (l.coordinates);

// Create full-text index
CREATE FULLTEXT INDEX person_fulltext_index FOR (p:Person) ON EACH [p.name, p.bio];

// Create uniqueness constraint
CREATE CONSTRAINT person_email_unique FOR (p:Person) REQUIRE p.email IS UNIQUE;

// Create node key constraint
CREATE CONSTRAINT person_node_key FOR (p:Person) REQUIRE (p.firstName, p.lastName) IS NODE KEY;

// Create existence constraint
CREATE CONSTRAINT person_name_exists FOR (p:Person) REQUIRE p.name IS NOT NULL;

// Create relationship property existence constraint
CREATE CONSTRAINT knows_since_exists FOR ()-[r:KNOWS]-() REQUIRE r.since IS NOT NULL;

// Drop index
DROP INDEX person_name_index;

// Drop constraint
DROP CONSTRAINT person_email_unique;

// Show indexes
SHOW INDEXES;

// Show constraints
SHOW CONSTRAINTS;

// ==============================================================================
// CALL Procedures
// ==============================================================================

// Database info
CALL db.labels();
CALL db.relationshipTypes();
CALL db.propertyKeys();

// Schema info
CALL db.indexes();
CALL db.constraints();

// Query explanation
CALL db.schema.visualization();

// Run query with parameters
CALL db.stats.retrieve('GRAPH COUNTS');

// ==============================================================================
// Transaction Control
// ==============================================================================

// Explicit transaction (in applications)
// BEGIN
// ... queries ...
// COMMIT or ROLLBACK

// ==============================================================================
// Import and Export
// ==============================================================================

// Load CSV
LOAD CSV FROM 'file:///people.csv' AS row
CREATE (p:Person {name: row[0], age: toInteger(row[1])});

// Load CSV with headers
LOAD CSV WITH HEADERS FROM 'file:///people.csv' AS row
CREATE (p:Person {name: row.name, age: toInteger(row.age)});

// Load CSV with field terminator
LOAD CSV FROM 'file:///people.csv' AS row FIELDTERMINATOR ';'
CREATE (p:Person {name: row[0]});

// Periodic commit for large imports
USING PERIODIC COMMIT 1000
LOAD CSV WITH HEADERS FROM 'file:///large_file.csv' AS row
CREATE (p:Person {name: row.name});

// ==============================================================================
// Query Hints
// ==============================================================================

// Use index
MATCH (p:Person)
USING INDEX p:Person(name)
WHERE p.name = 'Alice'
RETURN p;

// Use scan
MATCH (p:Person)
USING SCAN p:Person
WHERE p.age > 30
RETURN p;

// Join hint
MATCH (a:Person), (b:Person)
USING JOIN ON a
WHERE a.name = b.name
RETURN a, b;

// ==============================================================================
// FOREACH
// ==============================================================================

// Update all in path
MATCH path = (a:Person)-[:KNOWS*]->(b:Person)
WHERE a.name = 'Alice' AND b.name = 'Bob'
FOREACH (node IN nodes(path) | SET node.visited = true);

// Create multiple from list
FOREACH (name IN ['Alice', 'Bob', 'Charlie'] |
  MERGE (p:Person {name: name})
);

// ==============================================================================
// Conditional Execution
// ==============================================================================

// CASE in SET
MATCH (p:Person)
SET p.status = CASE
  WHEN p.age < 18 THEN 'minor'
  WHEN p.age < 65 THEN 'adult'
  ELSE 'senior'
END;

// Conditional create with FOREACH and CASE
MATCH (p:Person)
FOREACH (x IN CASE WHEN p.needsUpdate THEN [1] ELSE [] END |
  SET p.updated = timestamp()
);

// ==============================================================================
// Pattern Comprehension
// ==============================================================================

MATCH (p:Person)
RETURN p.name,
       [(p)-[:KNOWS]->(friend) | friend.name] AS friends,
       [(p)-[:WORKS_AT]->(c) | c.name] AS companies;

// Pattern comprehension with filter
MATCH (p:Person)
RETURN p.name,
       [(p)-[r:KNOWS]->(friend) WHERE r.since > 2020 | friend.name] AS recentFriends;

// ==============================================================================
// Complex Query Example
// ==============================================================================

// Find mutual friends recommendation
MATCH (me:Person {name: 'Alice'})-[:KNOWS]->(friend)-[:KNOWS]->(recommended)
WHERE NOT (me)-[:KNOWS]->(recommended)
  AND me <> recommended
WITH recommended, count(friend) AS mutualFriends
ORDER BY mutualFriends DESC
LIMIT 10
MATCH (recommended)-[:WORKS_AT]->(company)
RETURN recommended.name,
       mutualFriends,
       collect(DISTINCT company.name) AS companies;

// Graph algorithm: Find all paths between two nodes
MATCH (start:Person {name: 'Alice'}), (end:Person {name: 'Bob'})
MATCH path = allShortestPaths((start)-[*..6]-(end))
WITH path, [rel IN relationships(path) | type(rel)] AS relTypes
RETURN path,
       relTypes,
       length(path) AS pathLength
ORDER BY pathLength;

// Centrality-like query
MATCH (p:Person)
WITH p, size((p)-[:KNOWS]->()) AS outgoing,
        size((p)<-[:KNOWS]-()) AS incoming
RETURN p.name,
       outgoing,
       incoming,
       outgoing + incoming AS totalConnections
ORDER BY totalConnections DESC
LIMIT 10;

// ==============================================================================
// End of Cypher Sample
// ==============================================================================
