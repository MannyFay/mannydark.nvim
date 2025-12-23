-- ==============================================================================
-- Comprehensive PL/SQL Sample - Syntax Highlighting Demonstration
-- ==============================================================================

-- This file demonstrates all major Oracle PL/SQL language features
-- for syntax highlighting purposes.

-- ==============================================================================
-- Anonymous Block
-- ==============================================================================

DECLARE
    -- Variable declarations
    v_message VARCHAR2(100) := 'Hello, PL/SQL!';
    v_count NUMBER := 0;
    v_price NUMBER(10,2) DEFAULT 99.99;
    v_active BOOLEAN := TRUE;
    v_date DATE := SYSDATE;
    v_timestamp TIMESTAMP := SYSTIMESTAMP;

    -- Constants
    c_pi CONSTANT NUMBER := 3.14159;
    c_max_retries CONSTANT PLS_INTEGER := 3;

    -- Subtypes
    SUBTYPE t_name IS VARCHAR2(50);
    SUBTYPE t_positive IS NUMBER(10) NOT NULL;

    -- Using subtypes
    v_username t_name := 'alice';

    -- Anchored types
    v_emp_name employees.first_name%TYPE;
    v_emp_row employees%ROWTYPE;

    -- Collections
    TYPE t_number_array IS VARRAY(10) OF NUMBER;
    TYPE t_name_table IS TABLE OF VARCHAR2(100);
    TYPE t_emp_table IS TABLE OF employees%ROWTYPE INDEX BY PLS_INTEGER;
    TYPE t_assoc_array IS TABLE OF NUMBER INDEX BY VARCHAR2(50);

    v_numbers t_number_array := t_number_array(1, 2, 3, 4, 5);
    v_names t_name_table := t_name_table('Alice', 'Bob', 'Charlie');
    v_employees t_emp_table;
    v_lookup t_assoc_array;

    -- Record type
    TYPE t_address IS RECORD (
        street VARCHAR2(100),
        city VARCHAR2(50),
        state CHAR(2),
        zip VARCHAR2(10)
    );

    v_address t_address;

    -- Cursor variable
    TYPE t_emp_cursor IS REF CURSOR RETURN employees%ROWTYPE;
    v_emp_cursor t_emp_cursor;

    -- Object type variable
    v_point point_type;

BEGIN
    -- Output
    DBMS_OUTPUT.PUT_LINE(v_message);
    DBMS_OUTPUT.PUT_LINE('Count: ' || TO_CHAR(v_count));

    -- Assignments
    v_count := v_count + 1;
    v_message := 'Updated message';

    -- Conditional statements
    IF v_count > 0 THEN
        DBMS_OUTPUT.PUT_LINE('Positive count');
    ELSIF v_count = 0 THEN
        DBMS_OUTPUT.PUT_LINE('Zero count');
    ELSE
        DBMS_OUTPUT.PUT_LINE('Negative count');
    END IF;

    -- CASE statement
    CASE v_count
        WHEN 1 THEN DBMS_OUTPUT.PUT_LINE('One');
        WHEN 2 THEN DBMS_OUTPUT.PUT_LINE('Two');
        ELSE DBMS_OUTPUT.PUT_LINE('Other');
    END CASE;

    -- Searched CASE
    CASE
        WHEN v_count < 0 THEN DBMS_OUTPUT.PUT_LINE('Negative');
        WHEN v_count = 0 THEN DBMS_OUTPUT.PUT_LINE('Zero');
        WHEN v_count > 0 THEN DBMS_OUTPUT.PUT_LINE('Positive');
    END CASE;

    -- CASE expression
    v_message := CASE v_count
        WHEN 1 THEN 'Single'
        WHEN 2 THEN 'Double'
        ELSE 'Multiple'
    END;

    -- Loops
    -- Simple loop
    LOOP
        v_count := v_count + 1;
        EXIT WHEN v_count >= 10;
    END LOOP;

    -- WHILE loop
    WHILE v_count < 20 LOOP
        v_count := v_count + 1;
    END LOOP;

    -- FOR loop
    FOR i IN 1..10 LOOP
        DBMS_OUTPUT.PUT_LINE('Iteration: ' || i);
    END LOOP;

    -- Reverse FOR loop
    FOR i IN REVERSE 1..10 LOOP
        DBMS_OUTPUT.PUT_LINE('Reverse: ' || i);
    END LOOP;

    -- Loop labels
    <<outer_loop>>
    FOR i IN 1..5 LOOP
        <<inner_loop>>
        FOR j IN 1..5 LOOP
            CONTINUE outer_loop WHEN j = 3;
            EXIT outer_loop WHEN i * j > 15;
            DBMS_OUTPUT.PUT_LINE(i || ',' || j);
        END LOOP inner_loop;
    END LOOP outer_loop;

    -- Forall loop
    FORALL i IN 1..v_numbers.COUNT
        INSERT INTO numbers_table VALUES (v_numbers(i));

    -- Collection operations
    v_numbers.EXTEND(2);
    v_numbers(6) := 6;
    v_numbers(7) := 7;
    DBMS_OUTPUT.PUT_LINE('Count: ' || v_numbers.COUNT);
    DBMS_OUTPUT.PUT_LINE('First: ' || v_numbers.FIRST);
    DBMS_OUTPUT.PUT_LINE('Last: ' || v_numbers.LAST);
    v_numbers.DELETE(7);
    v_numbers.TRIM(1);

    -- NULL handling
    IF v_emp_name IS NULL THEN
        v_emp_name := 'Unknown';
    END IF;

    v_emp_name := NVL(v_emp_name, 'Default');
    v_emp_name := COALESCE(v_emp_name, 'Fallback', 'Default');
    v_price := NVL2(v_emp_name, 100, 0);
    v_emp_name := NULLIF(v_emp_name, 'Unknown');

    -- Dynamic SQL
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM employees' INTO v_count;

    EXECUTE IMMEDIATE 'UPDATE employees SET salary = salary * 1.1 WHERE department_id = :dept'
        USING 10;

    EXECUTE IMMEDIATE 'SELECT first_name FROM employees WHERE employee_id = :id'
        INTO v_emp_name
        USING 100;

    -- COMMIT and ROLLBACK
    COMMIT;
    ROLLBACK;
    SAVEPOINT my_savepoint;
    ROLLBACK TO my_savepoint;

    -- Autonomous transaction
    NULL; -- Placeholder

EXCEPTION
    WHEN NO_DATA_FOUND THEN
        DBMS_OUTPUT.PUT_LINE('No data found');
    WHEN TOO_MANY_ROWS THEN
        DBMS_OUTPUT.PUT_LINE('Too many rows');
    WHEN DUP_VAL_ON_INDEX THEN
        DBMS_OUTPUT.PUT_LINE('Duplicate value');
    WHEN VALUE_ERROR THEN
        DBMS_OUTPUT.PUT_LINE('Value error');
    WHEN ZERO_DIVIDE THEN
        DBMS_OUTPUT.PUT_LINE('Division by zero');
    WHEN OTHERS THEN
        DBMS_OUTPUT.PUT_LINE('Error: ' || SQLERRM);
        DBMS_OUTPUT.PUT_LINE('Code: ' || SQLCODE);
        RAISE;
END;
/

-- ==============================================================================
-- Cursors
-- ==============================================================================

DECLARE
    -- Explicit cursor
    CURSOR c_employees IS
        SELECT employee_id, first_name, last_name, salary
        FROM employees
        WHERE department_id = 10
        ORDER BY last_name;

    -- Cursor with parameters
    CURSOR c_emp_by_dept(p_dept_id NUMBER) IS
        SELECT * FROM employees WHERE department_id = p_dept_id;

    -- Cursor with FOR UPDATE
    CURSOR c_emp_update IS
        SELECT employee_id, salary
        FROM employees
        WHERE department_id = 20
        FOR UPDATE OF salary NOWAIT;

    -- Cursor with RETURN
    CURSOR c_emp_return RETURN employees%ROWTYPE IS
        SELECT * FROM employees;

    v_emp_rec c_employees%ROWTYPE;
    v_found BOOLEAN;

BEGIN
    -- Explicit cursor operations
    OPEN c_employees;

    LOOP
        FETCH c_employees INTO v_emp_rec;
        EXIT WHEN c_employees%NOTFOUND;
        DBMS_OUTPUT.PUT_LINE(v_emp_rec.first_name || ' ' || v_emp_rec.last_name);
    END LOOP;

    DBMS_OUTPUT.PUT_LINE('Rows fetched: ' || c_employees%ROWCOUNT);

    CLOSE c_employees;

    -- Cursor FOR loop (implicit open/fetch/close)
    FOR emp_rec IN c_employees LOOP
        DBMS_OUTPUT.PUT_LINE(emp_rec.salary);
    END LOOP;

    -- Cursor FOR loop with subquery
    FOR emp_rec IN (SELECT * FROM employees WHERE salary > 50000) LOOP
        DBMS_OUTPUT.PUT_LINE(emp_rec.first_name);
    END LOOP;

    -- Parameterized cursor
    FOR emp_rec IN c_emp_by_dept(30) LOOP
        DBMS_OUTPUT.PUT_LINE(emp_rec.employee_id);
    END LOOP;

    -- Cursor with FOR UPDATE and WHERE CURRENT OF
    OPEN c_emp_update;
    LOOP
        FETCH c_emp_update INTO v_emp_rec.employee_id, v_emp_rec.salary;
        EXIT WHEN c_emp_update%NOTFOUND;

        UPDATE employees
        SET salary = salary * 1.05
        WHERE CURRENT OF c_emp_update;
    END LOOP;
    CLOSE c_emp_update;

    COMMIT;

END;
/

-- ==============================================================================
-- REF CURSOR
-- ==============================================================================

DECLARE
    TYPE t_ref_cursor IS REF CURSOR;
    TYPE t_emp_cursor IS REF CURSOR RETURN employees%ROWTYPE;

    v_cursor t_ref_cursor;
    v_emp_cursor t_emp_cursor;
    v_emp_rec employees%ROWTYPE;
    v_dept_name departments.department_name%TYPE;

BEGIN
    -- Weak REF CURSOR
    OPEN v_cursor FOR
        SELECT * FROM employees WHERE department_id = 10;

    LOOP
        FETCH v_cursor INTO v_emp_rec;
        EXIT WHEN v_cursor%NOTFOUND;
        DBMS_OUTPUT.PUT_LINE(v_emp_rec.first_name);
    END LOOP;
    CLOSE v_cursor;

    -- Using with different query
    OPEN v_cursor FOR
        SELECT department_name FROM departments WHERE department_id = 10;

    FETCH v_cursor INTO v_dept_name;
    CLOSE v_cursor;

    -- Strong REF CURSOR
    OPEN v_emp_cursor FOR
        SELECT * FROM employees;

    FETCH v_emp_cursor INTO v_emp_rec;
    CLOSE v_emp_cursor;

END;
/

-- ==============================================================================
-- Procedures
-- ==============================================================================

CREATE OR REPLACE PROCEDURE greet_user(
    p_name IN VARCHAR2,
    p_greeting OUT VARCHAR2
) AS
BEGIN
    p_greeting := 'Hello, ' || p_name || '!';
END greet_user;
/

CREATE OR REPLACE PROCEDURE update_salary(
    p_employee_id IN employees.employee_id%TYPE,
    p_percentage IN NUMBER DEFAULT 5,
    p_new_salary OUT NUMBER
) AS
    v_current_salary NUMBER;
BEGIN
    SELECT salary INTO v_current_salary
    FROM employees
    WHERE employee_id = p_employee_id;

    p_new_salary := v_current_salary * (1 + p_percentage / 100);

    UPDATE employees
    SET salary = p_new_salary
    WHERE employee_id = p_employee_id;

    COMMIT;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20001, 'Employee not found: ' || p_employee_id);
    WHEN OTHERS THEN
        ROLLBACK;
        RAISE;
END update_salary;
/

-- Procedure with IN OUT parameter
CREATE OR REPLACE PROCEDURE swap_values(
    p_value1 IN OUT NUMBER,
    p_value2 IN OUT NUMBER
) AS
    v_temp NUMBER;
BEGIN
    v_temp := p_value1;
    p_value1 := p_value2;
    p_value2 := v_temp;
END swap_values;
/

-- Procedure with NOCOPY hint
CREATE OR REPLACE PROCEDURE process_large_data(
    p_data IN OUT NOCOPY CLOB
) AS
BEGIN
    -- Process large data without copying
    NULL;
END process_large_data;
/

-- Procedure with AUTHID
CREATE OR REPLACE PROCEDURE check_access
AUTHID CURRENT_USER
AS
BEGIN
    -- Runs with caller's privileges
    NULL;
END check_access;
/

-- Procedure with DETERMINISTIC and PARALLEL_ENABLE
CREATE OR REPLACE PROCEDURE parallel_proc
PARALLEL_ENABLE
AS
BEGIN
    NULL;
END parallel_proc;
/

-- ==============================================================================
-- Functions
-- ==============================================================================

CREATE OR REPLACE FUNCTION calculate_tax(
    p_amount IN NUMBER,
    p_rate IN NUMBER DEFAULT 0.1
) RETURN NUMBER
DETERMINISTIC
AS
BEGIN
    RETURN p_amount * p_rate;
END calculate_tax;
/

CREATE OR REPLACE FUNCTION get_employee_name(
    p_employee_id IN employees.employee_id%TYPE
) RETURN VARCHAR2
AS
    v_name VARCHAR2(100);
BEGIN
    SELECT first_name || ' ' || last_name
    INTO v_name
    FROM employees
    WHERE employee_id = p_employee_id;

    RETURN v_name;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
        RETURN NULL;
END get_employee_name;
/

-- Function returning REF CURSOR
CREATE OR REPLACE FUNCTION get_employees_by_dept(
    p_dept_id IN NUMBER
) RETURN SYS_REFCURSOR
AS
    v_cursor SYS_REFCURSOR;
BEGIN
    OPEN v_cursor FOR
        SELECT * FROM employees WHERE department_id = p_dept_id;
    RETURN v_cursor;
END get_employees_by_dept;
/

-- Pipelined function
CREATE OR REPLACE FUNCTION get_numbers(p_count IN NUMBER)
RETURN number_table PIPELINED
AS
BEGIN
    FOR i IN 1..p_count LOOP
        PIPE ROW(i);
    END LOOP;
    RETURN;
END get_numbers;
/

-- Table function
CREATE OR REPLACE FUNCTION get_employees_table
RETURN employee_table_type
AS
    v_result employee_table_type := employee_table_type();
BEGIN
    FOR emp_rec IN (SELECT * FROM employees) LOOP
        v_result.EXTEND;
        v_result(v_result.COUNT) := employee_type(
            emp_rec.employee_id,
            emp_rec.first_name,
            emp_rec.last_name
        );
    END LOOP;
    RETURN v_result;
END get_employees_table;
/

-- Result cache function
CREATE OR REPLACE FUNCTION get_department_name(p_dept_id NUMBER)
RETURN VARCHAR2
RESULT_CACHE RELIES_ON(departments)
AS
    v_name VARCHAR2(100);
BEGIN
    SELECT department_name INTO v_name
    FROM departments WHERE department_id = p_dept_id;
    RETURN v_name;
END get_department_name;
/

-- ==============================================================================
-- Packages
-- ==============================================================================

-- Package specification
CREATE OR REPLACE PACKAGE employee_pkg AS

    -- Constants
    c_max_salary CONSTANT NUMBER := 999999.99;
    c_min_salary CONSTANT NUMBER := 0;

    -- Types
    TYPE t_emp_record IS RECORD (
        employee_id employees.employee_id%TYPE,
        full_name VARCHAR2(100),
        salary NUMBER
    );

    TYPE t_emp_table IS TABLE OF t_emp_record INDEX BY PLS_INTEGER;

    -- Cursors
    CURSOR c_all_employees RETURN employees%ROWTYPE;

    -- Variables (public)
    g_last_error VARCHAR2(500);

    -- Exceptions
    e_salary_too_high EXCEPTION;
    e_employee_not_found EXCEPTION;
    PRAGMA EXCEPTION_INIT(e_salary_too_high, -20001);

    -- Procedures
    PROCEDURE hire_employee(
        p_first_name IN VARCHAR2,
        p_last_name IN VARCHAR2,
        p_email IN VARCHAR2,
        p_department_id IN NUMBER,
        p_salary IN NUMBER DEFAULT 50000,
        p_employee_id OUT NUMBER
    );

    PROCEDURE fire_employee(p_employee_id IN NUMBER);

    PROCEDURE update_salary(
        p_employee_id IN NUMBER,
        p_new_salary IN NUMBER
    );

    -- Functions
    FUNCTION get_salary(p_employee_id IN NUMBER) RETURN NUMBER;

    FUNCTION get_employee_count(p_department_id IN NUMBER DEFAULT NULL) RETURN NUMBER;

    FUNCTION employee_exists(p_employee_id IN NUMBER) RETURN BOOLEAN;

END employee_pkg;
/

-- Package body
CREATE OR REPLACE PACKAGE BODY employee_pkg AS

    -- Private variables
    g_hire_date DATE := SYSDATE;

    -- Private procedures/functions
    FUNCTION validate_salary(p_salary IN NUMBER) RETURN BOOLEAN IS
    BEGIN
        RETURN p_salary BETWEEN c_min_salary AND c_max_salary;
    END validate_salary;

    PROCEDURE log_action(p_action IN VARCHAR2, p_details IN VARCHAR2) IS
        PRAGMA AUTONOMOUS_TRANSACTION;
    BEGIN
        INSERT INTO action_log (action, details, log_date)
        VALUES (p_action, p_details, SYSDATE);
        COMMIT;
    END log_action;

    -- Cursor implementation
    CURSOR c_all_employees RETURN employees%ROWTYPE IS
        SELECT * FROM employees ORDER BY last_name, first_name;

    -- Public implementations
    PROCEDURE hire_employee(
        p_first_name IN VARCHAR2,
        p_last_name IN VARCHAR2,
        p_email IN VARCHAR2,
        p_department_id IN NUMBER,
        p_salary IN NUMBER DEFAULT 50000,
        p_employee_id OUT NUMBER
    ) IS
    BEGIN
        IF NOT validate_salary(p_salary) THEN
            RAISE e_salary_too_high;
        END IF;

        INSERT INTO employees (
            employee_id, first_name, last_name, email,
            department_id, salary, hire_date
        ) VALUES (
            employees_seq.NEXTVAL, p_first_name, p_last_name, p_email,
            p_department_id, p_salary, g_hire_date
        ) RETURNING employee_id INTO p_employee_id;

        log_action('HIRE', 'Hired employee: ' || p_employee_id);
        COMMIT;

    EXCEPTION
        WHEN DUP_VAL_ON_INDEX THEN
            g_last_error := 'Email already exists: ' || p_email;
            RAISE_APPLICATION_ERROR(-20002, g_last_error);
    END hire_employee;

    PROCEDURE fire_employee(p_employee_id IN NUMBER) IS
    BEGIN
        DELETE FROM employees WHERE employee_id = p_employee_id;

        IF SQL%ROWCOUNT = 0 THEN
            RAISE e_employee_not_found;
        END IF;

        log_action('FIRE', 'Fired employee: ' || p_employee_id);
        COMMIT;
    END fire_employee;

    PROCEDURE update_salary(
        p_employee_id IN NUMBER,
        p_new_salary IN NUMBER
    ) IS
    BEGIN
        IF NOT validate_salary(p_new_salary) THEN
            RAISE e_salary_too_high;
        END IF;

        UPDATE employees
        SET salary = p_new_salary
        WHERE employee_id = p_employee_id;

        IF SQL%ROWCOUNT = 0 THEN
            RAISE e_employee_not_found;
        END IF;

        COMMIT;
    END update_salary;

    FUNCTION get_salary(p_employee_id IN NUMBER) RETURN NUMBER IS
        v_salary NUMBER;
    BEGIN
        SELECT salary INTO v_salary
        FROM employees
        WHERE employee_id = p_employee_id;

        RETURN v_salary;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            RETURN NULL;
    END get_salary;

    FUNCTION get_employee_count(p_department_id IN NUMBER DEFAULT NULL) RETURN NUMBER IS
        v_count NUMBER;
    BEGIN
        SELECT COUNT(*)
        INTO v_count
        FROM employees
        WHERE department_id = NVL(p_department_id, department_id);

        RETURN v_count;
    END get_employee_count;

    FUNCTION employee_exists(p_employee_id IN NUMBER) RETURN BOOLEAN IS
        v_dummy NUMBER;
    BEGIN
        SELECT 1 INTO v_dummy
        FROM employees
        WHERE employee_id = p_employee_id;

        RETURN TRUE;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            RETURN FALSE;
    END employee_exists;

    -- Package initialization
BEGIN
    g_hire_date := TRUNC(SYSDATE);
    DBMS_OUTPUT.PUT_LINE('Package initialized');
END employee_pkg;
/

-- ==============================================================================
-- Object Types
-- ==============================================================================

-- Object type specification
CREATE OR REPLACE TYPE address_type AS OBJECT (
    street VARCHAR2(100),
    city VARCHAR2(50),
    state CHAR(2),
    zip VARCHAR2(10),

    -- Member functions
    MEMBER FUNCTION get_full_address RETURN VARCHAR2,
    MEMBER FUNCTION is_valid RETURN BOOLEAN,

    -- Static function
    STATIC FUNCTION create_address(
        p_street VARCHAR2,
        p_city VARCHAR2,
        p_state CHAR,
        p_zip VARCHAR2
    ) RETURN address_type,

    -- Constructor
    CONSTRUCTOR FUNCTION address_type(
        p_street VARCHAR2,
        p_city VARCHAR2
    ) RETURN SELF AS RESULT,

    -- Map function for ordering
    MAP MEMBER FUNCTION address_key RETURN VARCHAR2
);
/

-- Object type body
CREATE OR REPLACE TYPE BODY address_type AS

    MEMBER FUNCTION get_full_address RETURN VARCHAR2 IS
    BEGIN
        RETURN street || CHR(10) ||
               city || ', ' || state || ' ' || zip;
    END get_full_address;

    MEMBER FUNCTION is_valid RETURN BOOLEAN IS
    BEGIN
        RETURN street IS NOT NULL
           AND city IS NOT NULL
           AND state IS NOT NULL
           AND LENGTH(zip) >= 5;
    END is_valid;

    STATIC FUNCTION create_address(
        p_street VARCHAR2,
        p_city VARCHAR2,
        p_state CHAR,
        p_zip VARCHAR2
    ) RETURN address_type IS
    BEGIN
        RETURN address_type(p_street, p_city, p_state, p_zip);
    END create_address;

    CONSTRUCTOR FUNCTION address_type(
        p_street VARCHAR2,
        p_city VARCHAR2
    ) RETURN SELF AS RESULT IS
    BEGIN
        self.street := p_street;
        self.city := p_city;
        self.state := 'CA';
        self.zip := '00000';
        RETURN;
    END;

    MAP MEMBER FUNCTION address_key RETURN VARCHAR2 IS
    BEGIN
        RETURN state || zip || city || street;
    END address_key;

END;
/

-- Inheritance
CREATE OR REPLACE TYPE person_type AS OBJECT (
    person_id NUMBER,
    first_name VARCHAR2(50),
    last_name VARCHAR2(50),
    birth_date DATE,

    MEMBER FUNCTION get_age RETURN NUMBER,
    MEMBER FUNCTION get_full_name RETURN VARCHAR2
) NOT FINAL NOT INSTANTIABLE;
/

CREATE OR REPLACE TYPE employee_type UNDER person_type (
    employee_id NUMBER,
    hire_date DATE,
    salary NUMBER,
    department_id NUMBER,

    OVERRIDING MEMBER FUNCTION get_full_name RETURN VARCHAR2,
    MEMBER FUNCTION get_annual_salary RETURN NUMBER
);
/

-- Collection types
CREATE OR REPLACE TYPE number_table AS TABLE OF NUMBER;
/

CREATE OR REPLACE TYPE varchar_varray AS VARRAY(100) OF VARCHAR2(100);
/

CREATE OR REPLACE TYPE address_table AS TABLE OF address_type;
/

-- ==============================================================================
-- Triggers
-- ==============================================================================

-- Row-level trigger
CREATE OR REPLACE TRIGGER trg_employees_audit
    BEFORE INSERT OR UPDATE OR DELETE ON employees
    FOR EACH ROW
DECLARE
    v_operation VARCHAR2(10);
BEGIN
    IF INSERTING THEN
        v_operation := 'INSERT';
        :NEW.created_date := SYSDATE;
        :NEW.created_by := USER;
    ELSIF UPDATING THEN
        v_operation := 'UPDATE';
        :NEW.modified_date := SYSDATE;
        :NEW.modified_by := USER;
    ELSIF DELETING THEN
        v_operation := 'DELETE';
    END IF;

    INSERT INTO employees_audit (
        audit_id, employee_id, operation,
        old_salary, new_salary, audit_date, audit_user
    ) VALUES (
        audit_seq.NEXTVAL,
        NVL(:NEW.employee_id, :OLD.employee_id),
        v_operation,
        :OLD.salary, :NEW.salary,
        SYSDATE, USER
    );
END trg_employees_audit;
/

-- Statement-level trigger
CREATE OR REPLACE TRIGGER trg_employees_statement
    AFTER INSERT OR UPDATE OR DELETE ON employees
DECLARE
    v_count NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_count FROM employees;
    DBMS_OUTPUT.PUT_LINE('Total employees: ' || v_count);
END trg_employees_statement;
/

-- INSTEAD OF trigger (for views)
CREATE OR REPLACE TRIGGER trg_emp_view_insert
    INSTEAD OF INSERT ON employee_view
    FOR EACH ROW
BEGIN
    INSERT INTO employees (
        employee_id, first_name, last_name, email
    ) VALUES (
        employees_seq.NEXTVAL, :NEW.first_name,
        :NEW.last_name, :NEW.email
    );
END trg_emp_view_insert;
/

-- Compound trigger
CREATE OR REPLACE TRIGGER trg_employees_compound
    FOR INSERT OR UPDATE ON employees
    COMPOUND TRIGGER

    TYPE t_emp_table IS TABLE OF employees.employee_id%TYPE;
    g_emp_ids t_emp_table := t_emp_table();

    BEFORE STATEMENT IS
    BEGIN
        g_emp_ids.DELETE;
    END BEFORE STATEMENT;

    BEFORE EACH ROW IS
    BEGIN
        :NEW.modified_date := SYSDATE;
    END BEFORE EACH ROW;

    AFTER EACH ROW IS
    BEGIN
        g_emp_ids.EXTEND;
        g_emp_ids(g_emp_ids.COUNT) := :NEW.employee_id;
    END AFTER EACH ROW;

    AFTER STATEMENT IS
    BEGIN
        FORALL i IN 1..g_emp_ids.COUNT
            INSERT INTO employee_changes (employee_id, change_date)
            VALUES (g_emp_ids(i), SYSDATE);
    END AFTER STATEMENT;

END trg_employees_compound;
/

-- Database event trigger
CREATE OR REPLACE TRIGGER trg_logon
    AFTER LOGON ON DATABASE
BEGIN
    INSERT INTO login_log (username, login_time, ip_address)
    VALUES (USER, SYSDATE, SYS_CONTEXT('USERENV', 'IP_ADDRESS'));
END trg_logon;
/

CREATE OR REPLACE TRIGGER trg_ddl_audit
    AFTER DDL ON SCHEMA
BEGIN
    INSERT INTO ddl_log (
        event_type, object_type, object_name,
        sql_text, event_date, username
    ) VALUES (
        ORA_SYSEVENT, ORA_DICT_OBJ_TYPE, ORA_DICT_OBJ_NAME,
        ORA_SQL_TXT(1), SYSDATE, USER
    );
END trg_ddl_audit;
/

-- ==============================================================================
-- Exception Handling
-- ==============================================================================

DECLARE
    -- User-defined exceptions
    e_invalid_input EXCEPTION;
    e_custom_error EXCEPTION;
    PRAGMA EXCEPTION_INIT(e_custom_error, -20100);

    v_value NUMBER;
BEGIN
    -- Raising exceptions
    RAISE e_invalid_input;

    -- Using RAISE_APPLICATION_ERROR
    RAISE_APPLICATION_ERROR(-20100, 'Custom error message');
    RAISE_APPLICATION_ERROR(-20100, 'Error with backtrace', TRUE);

EXCEPTION
    -- Named exceptions
    WHEN NO_DATA_FOUND THEN
        DBMS_OUTPUT.PUT_LINE('No data found');

    WHEN TOO_MANY_ROWS THEN
        DBMS_OUTPUT.PUT_LINE('Too many rows');

    WHEN DUP_VAL_ON_INDEX THEN
        DBMS_OUTPUT.PUT_LINE('Duplicate value');

    WHEN VALUE_ERROR OR INVALID_NUMBER THEN
        DBMS_OUTPUT.PUT_LINE('Value/Number error');

    WHEN ZERO_DIVIDE THEN
        DBMS_OUTPUT.PUT_LINE('Division by zero');

    WHEN CURSOR_ALREADY_OPEN THEN
        DBMS_OUTPUT.PUT_LINE('Cursor already open');

    WHEN INVALID_CURSOR THEN
        DBMS_OUTPUT.PUT_LINE('Invalid cursor');

    WHEN LOGIN_DENIED THEN
        DBMS_OUTPUT.PUT_LINE('Login denied');

    WHEN NOT_LOGGED_ON THEN
        DBMS_OUTPUT.PUT_LINE('Not logged on');

    WHEN PROGRAM_ERROR THEN
        DBMS_OUTPUT.PUT_LINE('Program error');

    WHEN STORAGE_ERROR THEN
        DBMS_OUTPUT.PUT_LINE('Storage error');

    WHEN TIMEOUT_ON_RESOURCE THEN
        DBMS_OUTPUT.PUT_LINE('Timeout');

    WHEN e_invalid_input THEN
        DBMS_OUTPUT.PUT_LINE('Invalid input');

    WHEN e_custom_error THEN
        DBMS_OUTPUT.PUT_LINE('Custom error');

    WHEN OTHERS THEN
        DBMS_OUTPUT.PUT_LINE('Error code: ' || SQLCODE);
        DBMS_OUTPUT.PUT_LINE('Error message: ' || SQLERRM);
        DBMS_OUTPUT.PUT_LINE('Error stack: ' || DBMS_UTILITY.FORMAT_ERROR_STACK);
        DBMS_OUTPUT.PUT_LINE('Call stack: ' || DBMS_UTILITY.FORMAT_CALL_STACK);
        DBMS_OUTPUT.PUT_LINE('Error backtrace: ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
        RAISE;
END;
/

-- ==============================================================================
-- Bulk Operations
-- ==============================================================================

DECLARE
    TYPE t_emp_ids IS TABLE OF employees.employee_id%TYPE;
    TYPE t_salaries IS TABLE OF employees.salary%TYPE;

    v_emp_ids t_emp_ids;
    v_salaries t_salaries;
    v_emp_records t_emp_table;

BEGIN
    -- BULK COLLECT
    SELECT employee_id, salary
    BULK COLLECT INTO v_emp_ids, v_salaries
    FROM employees
    WHERE department_id = 10;

    -- BULK COLLECT with LIMIT
    DECLARE
        CURSOR c_emp IS SELECT * FROM employees;
    BEGIN
        OPEN c_emp;
        LOOP
            FETCH c_emp BULK COLLECT INTO v_emp_records LIMIT 100;
            EXIT WHEN v_emp_records.COUNT = 0;

            -- Process batch
            FOR i IN 1..v_emp_records.COUNT LOOP
                DBMS_OUTPUT.PUT_LINE(v_emp_records(i).first_name);
            END LOOP;
        END LOOP;
        CLOSE c_emp;
    END;

    -- FORALL with INSERT
    FORALL i IN 1..v_emp_ids.COUNT
        INSERT INTO emp_backup (employee_id, salary)
        VALUES (v_emp_ids(i), v_salaries(i));

    -- FORALL with UPDATE
    FORALL i IN 1..v_emp_ids.COUNT
        UPDATE employees
        SET salary = v_salaries(i) * 1.1
        WHERE employee_id = v_emp_ids(i);

    -- FORALL with DELETE
    FORALL i IN 1..v_emp_ids.COUNT
        DELETE FROM temp_employees
        WHERE employee_id = v_emp_ids(i);

    -- FORALL with INDICES OF
    v_emp_ids.DELETE(2);
    v_emp_ids.DELETE(5);

    FORALL i IN INDICES OF v_emp_ids
        INSERT INTO log_table VALUES (v_emp_ids(i));

    -- FORALL with VALUES OF
    DECLARE
        TYPE t_indices IS TABLE OF PLS_INTEGER;
        v_indices t_indices := t_indices(1, 3, 5, 7);
    BEGIN
        FORALL i IN VALUES OF v_indices
            UPDATE employees SET processed = 'Y'
            WHERE employee_id = v_emp_ids(i);
    END;

    -- FORALL with SAVE EXCEPTIONS
    BEGIN
        FORALL i IN 1..v_emp_ids.COUNT SAVE EXCEPTIONS
            INSERT INTO some_table VALUES (v_emp_ids(i));
    EXCEPTION
        WHEN OTHERS THEN
            IF SQLCODE = -24381 THEN
                FOR j IN 1..SQL%BULK_EXCEPTIONS.COUNT LOOP
                    DBMS_OUTPUT.PUT_LINE(
                        'Error ' || j || ': Index=' ||
                        SQL%BULK_EXCEPTIONS(j).ERROR_INDEX ||
                        ' Code=' || SQL%BULK_EXCEPTIONS(j).ERROR_CODE
                    );
                END LOOP;
            ELSE
                RAISE;
            END IF;
    END;

    COMMIT;
END;
/

-- ==============================================================================
-- Dynamic SQL
-- ==============================================================================

DECLARE
    v_sql VARCHAR2(1000);
    v_count NUMBER;
    v_cursor INTEGER;
    v_result INTEGER;

    TYPE t_emp_table IS TABLE OF employees%ROWTYPE;
    v_employees t_emp_table;

BEGIN
    -- EXECUTE IMMEDIATE - simple
    EXECUTE IMMEDIATE 'TRUNCATE TABLE temp_table';

    -- EXECUTE IMMEDIATE with INTO
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM employees'
        INTO v_count;

    -- EXECUTE IMMEDIATE with USING (bind variables)
    v_sql := 'SELECT COUNT(*) FROM employees WHERE department_id = :dept';
    EXECUTE IMMEDIATE v_sql INTO v_count USING 10;

    -- EXECUTE IMMEDIATE with RETURNING
    v_sql := 'UPDATE employees SET salary = salary * 1.1 ' ||
             'WHERE employee_id = :id RETURNING salary INTO :new_sal';
    EXECUTE IMMEDIATE v_sql USING 100, OUT v_count;

    -- EXECUTE IMMEDIATE with BULK COLLECT
    v_sql := 'SELECT * FROM employees WHERE department_id = :dept';
    EXECUTE IMMEDIATE v_sql
        BULK COLLECT INTO v_employees
        USING 10;

    -- DBMS_SQL package
    v_cursor := DBMS_SQL.OPEN_CURSOR;
    DBMS_SQL.PARSE(v_cursor, 'SELECT * FROM employees', DBMS_SQL.NATIVE);
    v_result := DBMS_SQL.EXECUTE(v_cursor);
    DBMS_SQL.CLOSE_CURSOR(v_cursor);

    -- Open cursor for dynamic SQL
    DECLARE
        v_ref_cursor SYS_REFCURSOR;
        v_emp_rec employees%ROWTYPE;
    BEGIN
        OPEN v_ref_cursor FOR
            'SELECT * FROM employees WHERE department_id = :dept'
            USING 10;

        LOOP
            FETCH v_ref_cursor INTO v_emp_rec;
            EXIT WHEN v_ref_cursor%NOTFOUND;
            DBMS_OUTPUT.PUT_LINE(v_emp_rec.first_name);
        END LOOP;

        CLOSE v_ref_cursor;
    END;

END;
/

-- ==============================================================================
-- Built-in Packages
-- ==============================================================================

BEGIN
    -- DBMS_OUTPUT
    DBMS_OUTPUT.ENABLE(1000000);
    DBMS_OUTPUT.PUT('No newline');
    DBMS_OUTPUT.PUT_LINE('With newline');
    DBMS_OUTPUT.NEW_LINE;

    -- DBMS_LOB
    DECLARE
        v_clob CLOB;
        v_amount INTEGER;
    BEGIN
        DBMS_LOB.CREATETEMPORARY(v_clob, TRUE);
        DBMS_LOB.WRITEAPPEND(v_clob, 5, 'Hello');
        v_amount := DBMS_LOB.GETLENGTH(v_clob);
        DBMS_LOB.FREETEMPORARY(v_clob);
    END;

    -- DBMS_JOB
    DECLARE
        v_job NUMBER;
    BEGIN
        DBMS_JOB.SUBMIT(
            job => v_job,
            what => 'my_procedure;',
            next_date => SYSDATE + 1,
            interval => 'SYSDATE + 1'
        );
        COMMIT;
    END;

    -- DBMS_SCHEDULER
    DBMS_SCHEDULER.CREATE_JOB(
        job_name => 'MY_JOB',
        job_type => 'PLSQL_BLOCK',
        job_action => 'BEGIN my_procedure; END;',
        start_date => SYSTIMESTAMP,
        repeat_interval => 'FREQ=DAILY; BYHOUR=2',
        enabled => TRUE
    );

    -- DBMS_LOCK
    DECLARE
        v_lockhandle VARCHAR2(128);
        v_result INTEGER;
    BEGIN
        DBMS_LOCK.ALLOCATE_UNIQUE('MY_LOCK', v_lockhandle);
        v_result := DBMS_LOCK.REQUEST(v_lockhandle, DBMS_LOCK.X_MODE);
        v_result := DBMS_LOCK.RELEASE(v_lockhandle);
    END;

    -- UTL_FILE
    DECLARE
        v_file UTL_FILE.FILE_TYPE;
        v_line VARCHAR2(1000);
    BEGIN
        v_file := UTL_FILE.FOPEN('MY_DIR', 'file.txt', 'W');
        UTL_FILE.PUT_LINE(v_file, 'Hello, World!');
        UTL_FILE.FCLOSE(v_file);

        v_file := UTL_FILE.FOPEN('MY_DIR', 'file.txt', 'R');
        UTL_FILE.GET_LINE(v_file, v_line);
        UTL_FILE.FCLOSE(v_file);
    END;

    -- UTL_HTTP
    DECLARE
        v_request UTL_HTTP.REQ;
        v_response UTL_HTTP.RESP;
        v_text VARCHAR2(32767);
    BEGIN
        v_request := UTL_HTTP.BEGIN_REQUEST('http://example.com');
        v_response := UTL_HTTP.GET_RESPONSE(v_request);
        UTL_HTTP.READ_TEXT(v_response, v_text);
        UTL_HTTP.END_RESPONSE(v_response);
    END;

    -- DBMS_UTILITY
    DBMS_UTILITY.EXEC_DDL_STATEMENT('CREATE TABLE temp (id NUMBER)');
    DBMS_OUTPUT.PUT_LINE(DBMS_UTILITY.GET_TIME);
    DBMS_OUTPUT.PUT_LINE(DBMS_UTILITY.FORMAT_CALL_STACK);

    -- DBMS_RANDOM
    DBMS_OUTPUT.PUT_LINE(DBMS_RANDOM.VALUE);
    DBMS_OUTPUT.PUT_LINE(DBMS_RANDOM.VALUE(1, 100));
    DBMS_OUTPUT.PUT_LINE(DBMS_RANDOM.STRING('U', 10));

    -- DBMS_CRYPTO
    DECLARE
        v_raw RAW(2000);
        v_encrypted RAW(2000);
        v_key RAW(16) := UTL_RAW.CAST_TO_RAW('0123456789ABCDEF');
    BEGIN
        v_raw := UTL_RAW.CAST_TO_RAW('Secret message');
        v_encrypted := DBMS_CRYPTO.ENCRYPT(
            src => v_raw,
            typ => DBMS_CRYPTO.AES_CBC_PKCS5,
            key => v_key
        );
    END;

END;
/

-- ==============================================================================
-- Conditional Compilation
-- ==============================================================================

DECLARE
    $IF DBMS_DB_VERSION.VERSION >= 12 $THEN
        v_new_feature VARCHAR2(100) := 'Available in 12c+';
    $ELSE
        v_old_way VARCHAR2(100) := 'Legacy approach';
    $END

    $IF $$DEBUG $THEN
        v_debug BOOLEAN := TRUE;
    $END
BEGIN
    $IF $$TRACE $THEN
        DBMS_OUTPUT.PUT_LINE('Trace enabled');
    $END

    NULL;
END;
/

-- Setting conditional compilation flags
ALTER SESSION SET PLSQL_CCFLAGS = 'DEBUG:TRUE, TRACE:FALSE';

-- ==============================================================================
-- Invoker Rights and Definer Rights
-- ==============================================================================

CREATE OR REPLACE PROCEDURE invoker_proc
AUTHID CURRENT_USER
AS
BEGIN
    -- Executes with caller's privileges
    NULL;
END invoker_proc;
/

CREATE OR REPLACE PROCEDURE definer_proc
AUTHID DEFINER
AS
BEGIN
    -- Executes with owner's privileges (default)
    NULL;
END definer_proc;
/

-- ==============================================================================
-- Edition-Based Redefinition
-- ==============================================================================

CREATE EDITION new_edition AS CHILD OF ora$base;

ALTER SESSION SET EDITION = new_edition;

CREATE OR REPLACE EDITIONABLE PACKAGE my_pkg AS
    -- Package available in this edition
    PROCEDURE my_proc;
END my_pkg;
/

CREATE OR REPLACE NONEDITIONABLE PROCEDURE static_proc AS
BEGIN
    -- Procedure same across all editions
    NULL;
END static_proc;
/

-- ==============================================================================
-- PL/SQL Optimization
-- ==============================================================================

-- Set optimization level
ALTER SESSION SET PLSQL_OPTIMIZE_LEVEL = 2;

-- Enable native compilation
ALTER SESSION SET PLSQL_CODE_TYPE = 'NATIVE';

-- Interpreted (default)
ALTER SESSION SET PLSQL_CODE_TYPE = 'INTERPRETED';

-- Inlining
CREATE OR REPLACE PROCEDURE optimized_proc AS
    PRAGMA INLINE(helper_function, 'YES');
BEGIN
    NULL;
END;
/

-- ==============================================================================
-- End of PL/SQL Sample
-- ==============================================================================
