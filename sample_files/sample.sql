-- ==============================================================================
-- Comprehensive SQL Sample - Syntax Highlighting Demonstration
-- ==============================================================================

-- This file demonstrates all major SQL language features
-- for syntax highlighting purposes (ANSI SQL with common extensions).

-- ==============================================================================
-- Comments
-- ==============================================================================

-- Single line comment
/* Multi-line
   comment */

/* Nested /* comments */ might not work in all databases */

-- ==============================================================================
-- Data Definition Language (DDL)
-- ==============================================================================

-- Create Database
CREATE DATABASE sample_database
    WITH OWNER = postgres
    ENCODING = 'UTF8'
    LC_COLLATE = 'en_US.UTF-8'
    LC_CTYPE = 'en_US.UTF-8'
    TABLESPACE = pg_default
    CONNECTION LIMIT = -1;

-- Create Schema
CREATE SCHEMA IF NOT EXISTS myapp;
CREATE SCHEMA sales AUTHORIZATION admin;

-- Create Table with all column types and constraints
CREATE TABLE IF NOT EXISTS users (
    -- Primary key
    id SERIAL PRIMARY KEY,
    user_id BIGINT GENERATED ALWAYS AS IDENTITY,
    uuid UUID DEFAULT gen_random_uuid() NOT NULL,

    -- String types
    username VARCHAR(50) NOT NULL UNIQUE,
    email VARCHAR(255) NOT NULL,
    password_hash CHAR(60) NOT NULL,
    bio TEXT,
    short_code CHAR(6),

    -- Numeric types
    age SMALLINT CHECK (age >= 0 AND age <= 150),
    balance DECIMAL(15, 2) DEFAULT 0.00,
    score NUMERIC(10, 4),
    rating REAL,
    multiplier DOUBLE PRECISION,

    -- Boolean
    is_active BOOLEAN DEFAULT TRUE,
    is_verified BOOLEAN DEFAULT FALSE NOT NULL,

    -- Date/Time types
    birth_date DATE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE,
    login_time TIME,
    duration INTERVAL,

    -- Binary types
    avatar BYTEA,
    profile_image BLOB,

    -- JSON types
    preferences JSON,
    metadata JSONB DEFAULT '{}',

    -- Array types (PostgreSQL)
    tags TEXT[],
    scores INTEGER[],

    -- Network types (PostgreSQL)
    ip_address INET,
    mac_address MACADDR,

    -- Geometric types (PostgreSQL)
    location POINT,
    area POLYGON,

    -- Range types (PostgreSQL)
    valid_range DATERANGE,

    -- Constraints
    CONSTRAINT email_format CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$'),
    CONSTRAINT unique_user UNIQUE (username, email)
);

-- Table with foreign keys
CREATE TABLE orders (
    order_id BIGSERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE ON UPDATE CASCADE,
    product_id INTEGER NOT NULL,
    quantity INTEGER NOT NULL DEFAULT 1 CHECK (quantity > 0),
    unit_price DECIMAL(10, 2) NOT NULL,
    total_amount DECIMAL(12, 2) GENERATED ALWAYS AS (quantity * unit_price) STORED,
    status VARCHAR(20) DEFAULT 'pending',
    order_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    shipped_date TIMESTAMP,
    notes TEXT,

    CONSTRAINT fk_product FOREIGN KEY (product_id)
        REFERENCES products(id)
        ON DELETE RESTRICT
        ON UPDATE CASCADE,
    CONSTRAINT valid_status CHECK (status IN ('pending', 'processing', 'shipped', 'delivered', 'cancelled'))
);

-- Partitioned table
CREATE TABLE measurements (
    id BIGSERIAL,
    sensor_id INTEGER NOT NULL,
    measurement_time TIMESTAMP NOT NULL,
    value DOUBLE PRECISION,
    PRIMARY KEY (id, measurement_time)
) PARTITION BY RANGE (measurement_time);

CREATE TABLE measurements_2024_q1 PARTITION OF measurements
    FOR VALUES FROM ('2024-01-01') TO ('2024-04-01');

CREATE TABLE measurements_2024_q2 PARTITION OF measurements
    FOR VALUES FROM ('2024-04-01') TO ('2024-07-01');

-- Temporary table
CREATE TEMPORARY TABLE temp_results (
    id INTEGER,
    value TEXT
);

CREATE GLOBAL TEMPORARY TABLE global_temp (
    session_id INTEGER,
    data TEXT
) ON COMMIT PRESERVE ROWS;

-- Create Table As
CREATE TABLE users_backup AS
SELECT * FROM users WHERE created_at < '2024-01-01';

-- ==============================================================================
-- Indexes
-- ==============================================================================

-- Simple index
CREATE INDEX idx_users_email ON users(email);

-- Unique index
CREATE UNIQUE INDEX idx_users_username ON users(username);

-- Composite index
CREATE INDEX idx_orders_user_date ON orders(user_id, order_date);

-- Partial index
CREATE INDEX idx_active_users ON users(username) WHERE is_active = TRUE;

-- Expression index
CREATE INDEX idx_users_lower_email ON users(LOWER(email));

-- Covering index (include columns)
CREATE INDEX idx_orders_covering ON orders(user_id) INCLUDE (order_date, status);

-- Full-text search index
CREATE INDEX idx_users_bio_fts ON users USING GIN (to_tsvector('english', bio));

-- Hash index
CREATE INDEX idx_users_uuid_hash ON users USING HASH (uuid);

-- B-tree index with options
CREATE INDEX CONCURRENTLY idx_orders_date ON orders(order_date DESC NULLS LAST);

-- Drop index
DROP INDEX IF EXISTS idx_users_email;

-- ==============================================================================
-- Views
-- ==============================================================================

-- Simple view
CREATE VIEW active_users AS
SELECT id, username, email, created_at
FROM users
WHERE is_active = TRUE;

-- View with join
CREATE OR REPLACE VIEW user_orders AS
SELECT
    u.id AS user_id,
    u.username,
    u.email,
    o.order_id,
    o.total_amount,
    o.status,
    o.order_date
FROM users u
LEFT JOIN orders o ON u.id = o.user_id;

-- Materialized view
CREATE MATERIALIZED VIEW monthly_sales AS
SELECT
    DATE_TRUNC('month', order_date) AS month,
    COUNT(*) AS order_count,
    SUM(total_amount) AS total_revenue,
    AVG(total_amount) AS avg_order_value
FROM orders
WHERE status = 'delivered'
GROUP BY DATE_TRUNC('month', order_date)
WITH DATA;

-- Refresh materialized view
REFRESH MATERIALIZED VIEW CONCURRENTLY monthly_sales;

-- View with check option
CREATE VIEW pending_orders AS
SELECT * FROM orders WHERE status = 'pending'
WITH CHECK OPTION;

-- Recursive view
CREATE RECURSIVE VIEW category_tree (id, name, parent_id, level, path) AS
    SELECT id, name, parent_id, 0, ARRAY[id]
    FROM categories WHERE parent_id IS NULL
    UNION ALL
    SELECT c.id, c.name, c.parent_id, ct.level + 1, ct.path || c.id
    FROM categories c
    JOIN category_tree ct ON c.parent_id = ct.id;

-- ==============================================================================
-- Data Manipulation Language (DML)
-- ==============================================================================

-- INSERT statements
INSERT INTO users (username, email, password_hash)
VALUES ('alice', 'alice@example.com', 'hash123');

INSERT INTO users (username, email, password_hash, is_active)
VALUES
    ('bob', 'bob@example.com', 'hash456', TRUE),
    ('charlie', 'charlie@example.com', 'hash789', FALSE),
    ('diana', 'diana@example.com', 'hash012', TRUE);

-- Insert with returning
INSERT INTO users (username, email, password_hash)
VALUES ('eve', 'eve@example.com', 'hash345')
RETURNING id, username, created_at;

-- Insert from select
INSERT INTO users_archive (id, username, email, archived_at)
SELECT id, username, email, CURRENT_TIMESTAMP
FROM users
WHERE is_active = FALSE;

-- Insert with conflict handling (upsert)
INSERT INTO users (username, email, password_hash)
VALUES ('alice', 'alice_new@example.com', 'newhash')
ON CONFLICT (username)
DO UPDATE SET
    email = EXCLUDED.email,
    updated_at = CURRENT_TIMESTAMP;

INSERT INTO products (sku, name, price)
VALUES ('SKU-001', 'Widget', 9.99)
ON CONFLICT ON CONSTRAINT products_sku_key
DO NOTHING;

-- UPDATE statements
UPDATE users
SET is_verified = TRUE
WHERE email LIKE '%@verified.com';

UPDATE users
SET
    balance = balance + 100.00,
    updated_at = CURRENT_TIMESTAMP
WHERE id = 1;

-- Update with join
UPDATE orders o
SET status = 'cancelled'
FROM users u
WHERE o.user_id = u.id
AND u.is_active = FALSE
AND o.status = 'pending';

-- Update with subquery
UPDATE products
SET price = price * 1.10
WHERE category_id IN (
    SELECT id FROM categories WHERE name = 'Electronics'
);

-- Update with returning
UPDATE users
SET is_active = FALSE
WHERE last_login < CURRENT_DATE - INTERVAL '1 year'
RETURNING id, username, email;

-- DELETE statements
DELETE FROM users WHERE id = 1;

DELETE FROM orders
WHERE order_date < CURRENT_DATE - INTERVAL '5 years';

-- Delete with join
DELETE FROM orders o
USING users u
WHERE o.user_id = u.id
AND u.username = 'deleted_user';

-- Delete with returning
DELETE FROM temp_results
WHERE id > 1000
RETURNING *;

-- TRUNCATE
TRUNCATE TABLE temp_results;
TRUNCATE TABLE orders, order_items RESTART IDENTITY CASCADE;

-- MERGE statement (SQL:2003)
MERGE INTO target_table t
USING source_table s
ON t.id = s.id
WHEN MATCHED AND s.deleted = TRUE THEN
    DELETE
WHEN MATCHED THEN
    UPDATE SET t.value = s.value, t.updated_at = CURRENT_TIMESTAMP
WHEN NOT MATCHED THEN
    INSERT (id, value, created_at) VALUES (s.id, s.value, CURRENT_TIMESTAMP);

-- ==============================================================================
-- SELECT Statements - Basic
-- ==============================================================================

-- Simple select
SELECT * FROM users;
SELECT id, username, email FROM users;

-- Distinct
SELECT DISTINCT status FROM orders;
SELECT DISTINCT ON (user_id) * FROM orders ORDER BY user_id, order_date DESC;

-- Aliases
SELECT
    u.id AS user_id,
    u.username AS "User Name",
    o.total_amount amount
FROM users u
JOIN orders o ON u.id = o.user_id;

-- Literals and expressions
SELECT
    'Hello, ' || username AS greeting,
    balance * 1.1 AS projected_balance,
    42 AS magic_number,
    3.14159 AS pi,
    TRUE AS is_true,
    NULL AS nothing,
    CURRENT_DATE AS today,
    CURRENT_TIMESTAMP AS now
FROM users;

-- ==============================================================================
-- SELECT Statements - WHERE Clause
-- ==============================================================================

-- Comparison operators
SELECT * FROM users WHERE age > 18;
SELECT * FROM users WHERE age >= 21 AND age <= 65;
SELECT * FROM users WHERE balance <> 0;
SELECT * FROM users WHERE balance != 0;
SELECT * FROM orders WHERE status = 'pending';

-- NULL handling
SELECT * FROM users WHERE bio IS NULL;
SELECT * FROM users WHERE bio IS NOT NULL;
SELECT * FROM users WHERE bio IS DISTINCT FROM '';

-- Pattern matching
SELECT * FROM users WHERE email LIKE '%@gmail.com';
SELECT * FROM users WHERE email NOT LIKE '%spam%';
SELECT * FROM users WHERE username LIKE 'admin_%' ESCAPE '\\';
SELECT * FROM users WHERE email ILIKE '%@EXAMPLE.COM';  -- Case insensitive
SELECT * FROM users WHERE email SIMILAR TO '%@(gmail|yahoo|hotmail)\.com';

-- Regular expressions
SELECT * FROM users WHERE email ~ '^[a-z]+@';
SELECT * FROM users WHERE email ~* '^[A-Z]+@';  -- Case insensitive
SELECT * FROM users WHERE username !~ '[0-9]';

-- Range operators
SELECT * FROM users WHERE age BETWEEN 18 AND 65;
SELECT * FROM users WHERE age NOT BETWEEN 0 AND 17;
SELECT * FROM orders WHERE order_date BETWEEN '2024-01-01' AND '2024-12-31';

-- IN operator
SELECT * FROM users WHERE status IN ('active', 'pending', 'verified');
SELECT * FROM orders WHERE user_id NOT IN (SELECT id FROM banned_users);

-- EXISTS
SELECT * FROM users u WHERE EXISTS (
    SELECT 1 FROM orders o WHERE o.user_id = u.id
);

SELECT * FROM users u WHERE NOT EXISTS (
    SELECT 1 FROM orders o WHERE o.user_id = u.id AND o.status = 'delivered'
);

-- ANY/ALL/SOME
SELECT * FROM products WHERE price > ANY (SELECT price FROM competitor_products);
SELECT * FROM products WHERE price > ALL (SELECT avg_price FROM market_data);
SELECT * FROM users WHERE age > SOME (ARRAY[18, 21, 25]);

-- Boolean logic
SELECT * FROM users
WHERE (is_active = TRUE AND is_verified = TRUE)
   OR (created_at > '2024-01-01' AND balance > 1000);

-- ==============================================================================
-- SELECT Statements - JOINs
-- ==============================================================================

-- Inner join
SELECT u.username, o.order_id, o.total_amount
FROM users u
INNER JOIN orders o ON u.id = o.user_id;

-- Left outer join
SELECT u.username, o.order_id
FROM users u
LEFT JOIN orders o ON u.id = o.user_id;

SELECT u.username, o.order_id
FROM users u
LEFT OUTER JOIN orders o ON u.id = o.user_id;

-- Right outer join
SELECT u.username, o.order_id
FROM users u
RIGHT JOIN orders o ON u.id = o.user_id;

-- Full outer join
SELECT u.username, o.order_id
FROM users u
FULL OUTER JOIN orders o ON u.id = o.user_id;

-- Cross join
SELECT u.username, p.product_name
FROM users u
CROSS JOIN products p;

-- Natural join
SELECT * FROM users NATURAL JOIN user_profiles;

-- Self join
SELECT e.name AS employee, m.name AS manager
FROM employees e
LEFT JOIN employees m ON e.manager_id = m.id;

-- Multiple joins
SELECT
    u.username,
    o.order_id,
    p.product_name,
    oi.quantity
FROM users u
JOIN orders o ON u.id = o.user_id
JOIN order_items oi ON o.order_id = oi.order_id
JOIN products p ON oi.product_id = p.id;

-- Join with USING
SELECT * FROM orders JOIN users USING (user_id);

-- Lateral join
SELECT u.*, recent_orders.*
FROM users u
CROSS JOIN LATERAL (
    SELECT order_id, total_amount, order_date
    FROM orders
    WHERE user_id = u.id
    ORDER BY order_date DESC
    LIMIT 3
) recent_orders;

-- ==============================================================================
-- SELECT Statements - Aggregation
-- ==============================================================================

-- Aggregate functions
SELECT
    COUNT(*) AS total_users,
    COUNT(DISTINCT status) AS unique_statuses,
    COUNT(bio) AS users_with_bio,
    SUM(balance) AS total_balance,
    AVG(balance) AS average_balance,
    MIN(balance) AS min_balance,
    MAX(balance) AS max_balance,
    STDDEV(balance) AS balance_stddev,
    VARIANCE(balance) AS balance_variance
FROM users;

-- GROUP BY
SELECT
    status,
    COUNT(*) AS count,
    SUM(total_amount) AS revenue
FROM orders
GROUP BY status;

-- GROUP BY with expressions
SELECT
    DATE_TRUNC('month', order_date) AS month,
    COUNT(*) AS orders
FROM orders
GROUP BY DATE_TRUNC('month', order_date)
ORDER BY month;

-- GROUP BY ROLLUP
SELECT
    COALESCE(category, 'ALL CATEGORIES') AS category,
    COALESCE(subcategory, 'ALL SUBCATEGORIES') AS subcategory,
    SUM(amount) AS total
FROM sales
GROUP BY ROLLUP (category, subcategory);

-- GROUP BY CUBE
SELECT
    category,
    region,
    SUM(amount) AS total
FROM sales
GROUP BY CUBE (category, region);

-- GROUP BY GROUPING SETS
SELECT
    category,
    region,
    SUM(amount) AS total
FROM sales
GROUP BY GROUPING SETS (
    (category, region),
    (category),
    (region),
    ()
);

-- HAVING clause
SELECT
    user_id,
    COUNT(*) AS order_count,
    SUM(total_amount) AS total_spent
FROM orders
GROUP BY user_id
HAVING COUNT(*) > 5 AND SUM(total_amount) > 1000;

-- FILTER clause
SELECT
    COUNT(*) AS total,
    COUNT(*) FILTER (WHERE status = 'active') AS active,
    COUNT(*) FILTER (WHERE status = 'inactive') AS inactive,
    SUM(balance) FILTER (WHERE is_verified = TRUE) AS verified_balance
FROM users;

-- ==============================================================================
-- SELECT Statements - Window Functions
-- ==============================================================================

-- Basic window functions
SELECT
    username,
    balance,
    ROW_NUMBER() OVER (ORDER BY balance DESC) AS rank,
    RANK() OVER (ORDER BY balance DESC) AS rank_with_gaps,
    DENSE_RANK() OVER (ORDER BY balance DESC) AS dense_rank,
    NTILE(4) OVER (ORDER BY balance DESC) AS quartile,
    PERCENT_RANK() OVER (ORDER BY balance DESC) AS percent_rank,
    CUME_DIST() OVER (ORDER BY balance DESC) AS cumulative_dist
FROM users;

-- Partition by
SELECT
    category,
    product_name,
    price,
    ROW_NUMBER() OVER (PARTITION BY category ORDER BY price DESC) AS category_rank,
    AVG(price) OVER (PARTITION BY category) AS category_avg_price,
    price - AVG(price) OVER (PARTITION BY category) AS diff_from_avg
FROM products;

-- Frame specification
SELECT
    order_date,
    total_amount,
    SUM(total_amount) OVER (ORDER BY order_date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
    AVG(total_amount) OVER (ORDER BY order_date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS moving_avg_7day,
    SUM(total_amount) OVER (ORDER BY order_date RANGE BETWEEN INTERVAL '1 month' PRECEDING AND CURRENT ROW) AS monthly_total
FROM orders;

-- Lead and lag
SELECT
    order_date,
    total_amount,
    LAG(total_amount, 1) OVER (ORDER BY order_date) AS prev_amount,
    LEAD(total_amount, 1) OVER (ORDER BY order_date) AS next_amount,
    total_amount - LAG(total_amount, 1) OVER (ORDER BY order_date) AS diff_from_prev,
    FIRST_VALUE(total_amount) OVER (PARTITION BY user_id ORDER BY order_date) AS first_order,
    LAST_VALUE(total_amount) OVER (PARTITION BY user_id ORDER BY order_date
        ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS last_order,
    NTH_VALUE(total_amount, 2) OVER (PARTITION BY user_id ORDER BY order_date) AS second_order
FROM orders;

-- Named window
SELECT
    username,
    balance,
    ROW_NUMBER() OVER w AS row_num,
    SUM(balance) OVER w AS running_sum
FROM users
WINDOW w AS (ORDER BY created_at);

-- ==============================================================================
-- SELECT Statements - Subqueries
-- ==============================================================================

-- Scalar subquery
SELECT
    username,
    balance,
    (SELECT AVG(balance) FROM users) AS avg_balance,
    balance - (SELECT AVG(balance) FROM users) AS diff_from_avg
FROM users;

-- Subquery in FROM
SELECT
    user_stats.username,
    user_stats.order_count,
    user_stats.total_spent
FROM (
    SELECT
        u.username,
        COUNT(o.order_id) AS order_count,
        COALESCE(SUM(o.total_amount), 0) AS total_spent
    FROM users u
    LEFT JOIN orders o ON u.id = o.user_id
    GROUP BY u.username
) user_stats
WHERE user_stats.order_count > 0;

-- Correlated subquery
SELECT
    u.username,
    u.balance,
    (SELECT MAX(o.total_amount)
     FROM orders o
     WHERE o.user_id = u.id) AS max_order
FROM users u;

-- Subquery in WHERE
SELECT * FROM products
WHERE price > (SELECT AVG(price) FROM products);

SELECT * FROM users
WHERE id IN (SELECT DISTINCT user_id FROM orders WHERE total_amount > 100);

-- ==============================================================================
-- SELECT Statements - Set Operations
-- ==============================================================================

-- UNION
SELECT username, email FROM users
UNION
SELECT name, contact_email FROM customers;

-- UNION ALL (keeps duplicates)
SELECT product_id FROM orders_2023
UNION ALL
SELECT product_id FROM orders_2024;

-- INTERSECT
SELECT user_id FROM premium_users
INTERSECT
SELECT user_id FROM active_users;

-- EXCEPT (MINUS in Oracle)
SELECT user_id FROM all_users
EXCEPT
SELECT user_id FROM banned_users;

-- Combined set operations
(SELECT id, name FROM table_a
 UNION
 SELECT id, name FROM table_b)
INTERSECT
SELECT id, name FROM table_c;

-- ==============================================================================
-- SELECT Statements - CTEs and Recursive Queries
-- ==============================================================================

-- Common Table Expression (CTE)
WITH high_value_orders AS (
    SELECT * FROM orders WHERE total_amount > 1000
),
vip_users AS (
    SELECT DISTINCT user_id FROM high_value_orders
)
SELECT u.username, u.email
FROM users u
JOIN vip_users v ON u.id = v.user_id;

-- Multiple CTEs
WITH
monthly_revenue AS (
    SELECT
        DATE_TRUNC('month', order_date) AS month,
        SUM(total_amount) AS revenue
    FROM orders
    GROUP BY DATE_TRUNC('month', order_date)
),
revenue_growth AS (
    SELECT
        month,
        revenue,
        LAG(revenue) OVER (ORDER BY month) AS prev_revenue,
        revenue - LAG(revenue) OVER (ORDER BY month) AS growth
    FROM monthly_revenue
)
SELECT * FROM revenue_growth WHERE growth > 0;

-- Recursive CTE
WITH RECURSIVE subordinates AS (
    -- Base case: direct reports
    SELECT id, name, manager_id, 1 AS level
    FROM employees
    WHERE manager_id = 1

    UNION ALL

    -- Recursive case
    SELECT e.id, e.name, e.manager_id, s.level + 1
    FROM employees e
    JOIN subordinates s ON e.manager_id = s.id
)
SELECT * FROM subordinates ORDER BY level, name;

-- Recursive CTE for tree structure
WITH RECURSIVE category_path AS (
    SELECT id, name, parent_id, name::TEXT AS path
    FROM categories
    WHERE parent_id IS NULL

    UNION ALL

    SELECT c.id, c.name, c.parent_id, cp.path || ' > ' || c.name
    FROM categories c
    JOIN category_path cp ON c.parent_id = cp.id
)
SELECT * FROM category_path;

-- CTE with DML (PostgreSQL)
WITH deleted_orders AS (
    DELETE FROM orders
    WHERE status = 'cancelled' AND order_date < CURRENT_DATE - INTERVAL '1 year'
    RETURNING *
)
INSERT INTO orders_archive
SELECT * FROM deleted_orders;

-- ==============================================================================
-- SELECT Statements - Advanced Features
-- ==============================================================================

-- CASE expression
SELECT
    username,
    balance,
    CASE
        WHEN balance > 10000 THEN 'Premium'
        WHEN balance > 1000 THEN 'Standard'
        WHEN balance > 0 THEN 'Basic'
        ELSE 'Inactive'
    END AS tier,
    CASE status
        WHEN 'active' THEN 'Active User'
        WHEN 'inactive' THEN 'Inactive User'
        WHEN 'pending' THEN 'Pending Verification'
        ELSE 'Unknown'
    END AS status_label
FROM users;

-- COALESCE and NULLIF
SELECT
    COALESCE(nickname, username, 'Anonymous') AS display_name,
    NULLIF(balance, 0) AS non_zero_balance,
    COALESCE(NULLIF(bio, ''), 'No bio provided') AS bio_text
FROM users;

-- GREATEST and LEAST
SELECT
    GREATEST(price, min_price, competitor_price) AS max_price,
    LEAST(price, min_price, competitor_price) AS min_price
FROM products;

-- Array operations (PostgreSQL)
SELECT
    tags,
    ARRAY_LENGTH(tags, 1) AS tag_count,
    tags[1] AS first_tag,
    'featured' = ANY(tags) AS is_featured,
    tags || ARRAY['new'] AS tags_with_new,
    ARRAY_AGG(DISTINCT tag) AS all_tags
FROM products, UNNEST(tags) AS tag
GROUP BY tags;

-- JSON operations
SELECT
    metadata->>'name' AS name,
    metadata->'address'->>'city' AS city,
    metadata#>>'{contact,email}' AS email,
    jsonb_typeof(metadata->'tags') AS tags_type,
    jsonb_array_length(metadata->'tags') AS tag_count,
    metadata ? 'verified' AS has_verified_key,
    metadata @> '{"active": true}' AS is_active
FROM users
WHERE metadata @? '$.tags[*] ? (@ == "premium")';

-- Full-text search
SELECT
    title,
    ts_rank(search_vector, query) AS rank
FROM articles, plainto_tsquery('english', 'database performance') query
WHERE search_vector @@ query
ORDER BY rank DESC;

-- Pivot/Crosstab
SELECT *
FROM crosstab(
    'SELECT department, quarter, revenue FROM sales ORDER BY 1, 2',
    'SELECT DISTINCT quarter FROM sales ORDER BY 1'
) AS ct(department TEXT, q1 NUMERIC, q2 NUMERIC, q3 NUMERIC, q4 NUMERIC);

-- ==============================================================================
-- Functions - Built-in
-- ==============================================================================

-- String functions
SELECT
    CONCAT('Hello', ' ', 'World') AS concatenated,
    CONCAT_WS(', ', 'a', 'b', 'c') AS concat_with_sep,
    LENGTH('Hello') AS len,
    CHAR_LENGTH('Hello') AS char_len,
    UPPER('hello') AS upper_case,
    LOWER('HELLO') AS lower_case,
    INITCAP('hello world') AS init_cap,
    TRIM('  hello  ') AS trimmed,
    LTRIM('  hello') AS left_trimmed,
    RTRIM('hello  ') AS right_trimmed,
    LPAD('42', 5, '0') AS left_padded,
    RPAD('hi', 5, '!') AS right_padded,
    SUBSTRING('Hello World' FROM 1 FOR 5) AS substring,
    LEFT('Hello', 3) AS left_chars,
    RIGHT('Hello', 3) AS right_chars,
    POSITION('World' IN 'Hello World') AS position,
    REPLACE('Hello World', 'World', 'SQL') AS replaced,
    TRANSLATE('hello', 'el', '31') AS translated,
    REVERSE('Hello') AS reversed,
    REPEAT('ab', 3) AS repeated,
    SPLIT_PART('a,b,c', ',', 2) AS split_part,
    STRING_AGG(name, ', ' ORDER BY name) AS names;

-- Numeric functions
SELECT
    ABS(-42) AS absolute,
    CEIL(4.2) AS ceiling,
    FLOOR(4.8) AS floor,
    ROUND(3.14159, 2) AS rounded,
    TRUNC(3.14159, 2) AS truncated,
    MOD(17, 5) AS modulo,
    POWER(2, 10) AS power,
    SQRT(16) AS square_root,
    EXP(1) AS exponential,
    LN(2.71828) AS natural_log,
    LOG(10, 100) AS log_base_10,
    SIGN(-42) AS sign,
    RANDOM() AS random_value,
    GREATEST(1, 2, 3) AS greatest,
    LEAST(1, 2, 3) AS least;

-- Date/Time functions
SELECT
    CURRENT_DATE AS today,
    CURRENT_TIME AS current_time,
    CURRENT_TIMESTAMP AS now,
    NOW() AS now_func,
    LOCALTIME AS local_time,
    LOCALTIMESTAMP AS local_timestamp,
    EXTRACT(YEAR FROM CURRENT_DATE) AS year,
    EXTRACT(MONTH FROM CURRENT_DATE) AS month,
    EXTRACT(DAY FROM CURRENT_DATE) AS day,
    EXTRACT(DOW FROM CURRENT_DATE) AS day_of_week,
    EXTRACT(EPOCH FROM CURRENT_TIMESTAMP) AS epoch,
    DATE_PART('hour', CURRENT_TIMESTAMP) AS hour,
    DATE_TRUNC('month', CURRENT_TIMESTAMP) AS month_start,
    AGE(CURRENT_DATE, '1990-01-01') AS age,
    CURRENT_DATE + INTERVAL '1 month' AS next_month,
    CURRENT_DATE - INTERVAL '1 week' AS last_week,
    MAKE_DATE(2024, 1, 15) AS make_date,
    MAKE_TIMESTAMP(2024, 1, 15, 12, 30, 0) AS make_timestamp,
    TO_CHAR(CURRENT_TIMESTAMP, 'YYYY-MM-DD HH24:MI:SS') AS formatted;

-- Conditional functions
SELECT
    CASE WHEN x > 0 THEN 'positive' ELSE 'non-positive' END,
    COALESCE(nullable_col, 'default'),
    NULLIF(a, b),
    GREATEST(a, b, c),
    LEAST(a, b, c);

-- ==============================================================================
-- Stored Procedures and Functions
-- ==============================================================================

-- Create function (PostgreSQL)
CREATE OR REPLACE FUNCTION calculate_tax(amount DECIMAL, rate DECIMAL DEFAULT 0.1)
RETURNS DECIMAL
LANGUAGE plpgsql
AS $$
BEGIN
    RETURN amount * rate;
END;
$$;

-- Function with multiple return values
CREATE OR REPLACE FUNCTION get_user_stats(p_user_id INTEGER)
RETURNS TABLE(order_count BIGINT, total_spent DECIMAL, avg_order DECIMAL)
LANGUAGE plpgsql
AS $$
BEGIN
    RETURN QUERY
    SELECT
        COUNT(*)::BIGINT,
        COALESCE(SUM(total_amount), 0),
        COALESCE(AVG(total_amount), 0)
    FROM orders
    WHERE user_id = p_user_id;
END;
$$;

-- Stored procedure
CREATE OR REPLACE PROCEDURE process_order(
    IN p_order_id INTEGER,
    IN p_status VARCHAR(20),
    OUT p_success BOOLEAN
)
LANGUAGE plpgsql
AS $$
DECLARE
    v_current_status VARCHAR(20);
BEGIN
    SELECT status INTO v_current_status FROM orders WHERE order_id = p_order_id;

    IF v_current_status IS NULL THEN
        p_success := FALSE;
        RETURN;
    END IF;

    UPDATE orders SET status = p_status, updated_at = NOW() WHERE order_id = p_order_id;
    p_success := TRUE;
END;
$$;

-- Call procedure
CALL process_order(123, 'shipped', NULL);

-- ==============================================================================
-- Triggers
-- ==============================================================================

-- Create trigger function
CREATE OR REPLACE FUNCTION update_timestamp()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
    NEW.updated_at := CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$;

-- Create trigger
CREATE TRIGGER trg_users_update
    BEFORE UPDATE ON users
    FOR EACH ROW
    EXECUTE FUNCTION update_timestamp();

-- Trigger with condition
CREATE TRIGGER trg_audit_changes
    AFTER INSERT OR UPDATE OR DELETE ON orders
    FOR EACH ROW
    WHEN (pg_trigger_depth() = 0)
    EXECUTE FUNCTION audit_log_changes();

-- Drop trigger
DROP TRIGGER IF EXISTS trg_users_update ON users;

-- ==============================================================================
-- Transactions
-- ==============================================================================

-- Begin transaction
BEGIN;
BEGIN TRANSACTION;
BEGIN WORK;
START TRANSACTION;

-- Transaction with isolation level
BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE;
BEGIN TRANSACTION READ ONLY;
BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ;
BEGIN TRANSACTION ISOLATION LEVEL READ COMMITTED;

-- Set transaction characteristics
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;
SET TRANSACTION READ ONLY;

-- Savepoints
SAVEPOINT my_savepoint;
ROLLBACK TO SAVEPOINT my_savepoint;
RELEASE SAVEPOINT my_savepoint;

-- Commit and rollback
COMMIT;
COMMIT WORK;
ROLLBACK;
ROLLBACK WORK;

-- ==============================================================================
-- Access Control
-- ==============================================================================

-- Create role
CREATE ROLE app_user LOGIN PASSWORD 'secure_password';
CREATE ROLE admin_role SUPERUSER CREATEDB CREATEROLE;
CREATE ROLE readonly NOLOGIN;

-- Grant privileges
GRANT SELECT ON ALL TABLES IN SCHEMA public TO readonly;
GRANT SELECT, INSERT, UPDATE, DELETE ON users TO app_user;
GRANT USAGE ON SCHEMA myapp TO app_user;
GRANT EXECUTE ON FUNCTION calculate_tax TO app_user;
GRANT ALL PRIVILEGES ON DATABASE mydb TO admin_role;

-- Revoke privileges
REVOKE DELETE ON users FROM app_user;
REVOKE ALL PRIVILEGES ON orders FROM PUBLIC;

-- Alter role
ALTER ROLE app_user SET search_path TO myapp, public;
ALTER ROLE app_user CONNECTION LIMIT 10;

-- Row-level security
ALTER TABLE users ENABLE ROW LEVEL SECURITY;

CREATE POLICY user_isolation ON users
    FOR ALL
    TO app_user
    USING (user_id = current_user_id());

-- ==============================================================================
-- Performance and Optimization
-- ==============================================================================

-- Explain query plan
EXPLAIN SELECT * FROM users WHERE email = 'test@example.com';
EXPLAIN ANALYZE SELECT * FROM users WHERE email = 'test@example.com';
EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON) SELECT * FROM orders WHERE user_id = 1;

-- Vacuum and analyze
VACUUM users;
VACUUM FULL users;
VACUUM ANALYZE users;
ANALYZE users;

-- Cluster
CLUSTER users USING idx_users_email;

-- Reindex
REINDEX TABLE users;
REINDEX INDEX idx_users_email;
REINDEX DATABASE mydb;

-- Statistics
SELECT * FROM pg_stat_user_tables;
SELECT * FROM pg_stat_user_indexes;

-- ==============================================================================
-- Constraints
-- ==============================================================================

-- Add constraints
ALTER TABLE users ADD CONSTRAINT chk_age CHECK (age >= 0);
ALTER TABLE users ADD CONSTRAINT uq_email UNIQUE (email);
ALTER TABLE orders ADD CONSTRAINT fk_user
    FOREIGN KEY (user_id) REFERENCES users(id);

-- Drop constraints
ALTER TABLE users DROP CONSTRAINT chk_age;
ALTER TABLE users DROP CONSTRAINT IF EXISTS uq_email CASCADE;

-- Deferrable constraints
ALTER TABLE orders ADD CONSTRAINT fk_user
    FOREIGN KEY (user_id) REFERENCES users(id)
    DEFERRABLE INITIALLY DEFERRED;

SET CONSTRAINTS fk_user IMMEDIATE;
SET CONSTRAINTS ALL DEFERRED;

-- ==============================================================================
-- Schema Changes
-- ==============================================================================

-- Alter table
ALTER TABLE users ADD COLUMN phone VARCHAR(20);
ALTER TABLE users DROP COLUMN phone;
ALTER TABLE users ALTER COLUMN username TYPE VARCHAR(100);
ALTER TABLE users ALTER COLUMN balance SET DEFAULT 0;
ALTER TABLE users ALTER COLUMN email SET NOT NULL;
ALTER TABLE users RENAME COLUMN username TO user_name;
ALTER TABLE users RENAME TO app_users;

-- Alter column
ALTER TABLE users ALTER COLUMN created_at SET DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE users ALTER COLUMN is_active DROP DEFAULT;

-- ==============================================================================
-- Data Types Casting
-- ==============================================================================

SELECT
    CAST('42' AS INTEGER) AS cast_int,
    CAST(42 AS VARCHAR) AS cast_varchar,
    CAST('2024-01-15' AS DATE) AS cast_date,
    '42'::INTEGER AS pg_cast_int,
    42::TEXT AS pg_cast_text,
    '3.14'::NUMERIC(10,2) AS pg_cast_numeric,
    ARRAY[1,2,3]::TEXT[] AS cast_array;

-- ==============================================================================
-- Sequences
-- ==============================================================================

CREATE SEQUENCE order_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

SELECT nextval('order_seq');
SELECT currval('order_seq');
SELECT setval('order_seq', 1000);
SELECT lastval();

ALTER SEQUENCE order_seq RESTART WITH 1;
DROP SEQUENCE order_seq;

-- ==============================================================================
-- Extensions (PostgreSQL)
-- ==============================================================================

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";
CREATE EXTENSION IF NOT EXISTS "pg_trgm";
CREATE EXTENSION IF NOT EXISTS "hstore";

-- Using extensions
SELECT uuid_generate_v4();
SELECT gen_random_uuid();
SELECT crypt('password', gen_salt('bf'));
SELECT similarity('hello', 'hallo');

-- ==============================================================================
-- Prepared Statements
-- ==============================================================================

PREPARE user_by_email (TEXT) AS
    SELECT * FROM users WHERE email = $1;

EXECUTE user_by_email('test@example.com');

DEALLOCATE user_by_email;
DEALLOCATE ALL;

-- ==============================================================================
-- Cursors
-- ==============================================================================

DECLARE user_cursor CURSOR FOR SELECT * FROM users;
FETCH NEXT FROM user_cursor;
FETCH FORWARD 10 FROM user_cursor;
FETCH BACKWARD 5 FROM user_cursor;
MOVE FORWARD 10 IN user_cursor;
CLOSE user_cursor;

-- ==============================================================================
-- Copy Data
-- ==============================================================================

COPY users TO '/tmp/users.csv' WITH (FORMAT CSV, HEADER);
COPY users FROM '/tmp/users.csv' WITH (FORMAT CSV, HEADER);
COPY users TO STDOUT WITH (FORMAT CSV, DELIMITER '|');

\copy users TO 'users.csv' CSV HEADER

-- ==============================================================================
-- Locking
-- ==============================================================================

LOCK TABLE users IN ACCESS SHARE MODE;
LOCK TABLE orders IN ROW EXCLUSIVE MODE;
LOCK TABLE products IN SHARE UPDATE EXCLUSIVE MODE;
LOCK TABLE inventory IN EXCLUSIVE MODE;
LOCK TABLE config IN ACCESS EXCLUSIVE MODE NOWAIT;

SELECT * FROM users WHERE id = 1 FOR UPDATE;
SELECT * FROM users WHERE id = 1 FOR NO KEY UPDATE;
SELECT * FROM users WHERE id = 1 FOR SHARE;
SELECT * FROM users WHERE id = 1 FOR KEY SHARE;
SELECT * FROM users WHERE id = 1 FOR UPDATE SKIP LOCKED;
SELECT * FROM users WHERE id = 1 FOR UPDATE NOWAIT;

-- ==============================================================================
-- Comments on Database Objects
-- ==============================================================================

COMMENT ON TABLE users IS 'Main user accounts table';
COMMENT ON COLUMN users.email IS 'User email address, must be unique';
COMMENT ON INDEX idx_users_email IS 'Index for email lookups';
COMMENT ON FUNCTION calculate_tax IS 'Calculates tax based on amount and rate';

-- ==============================================================================
-- End of SQL Sample
-- ==============================================================================
