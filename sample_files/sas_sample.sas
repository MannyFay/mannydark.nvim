/* ==============================================================================
   Comprehensive SAS Sample - Syntax Highlighting Demonstration
   ==============================================================================

   This file demonstrates all major SAS language features
   for syntax highlighting purposes.
*/

/* ==============================================================================
   Comments
   ============================================================================== */

* This is a statement-style comment;
/* This is a block comment */

/*
   Multi-line
   block comment
*/

/* ==============================================================================
   Basic Data Step
   ============================================================================== */

/* Create dataset with inline data */
DATA work.employees;
    INPUT id name $ age salary department $;
    DATALINES;
1 Alice 30 75000 IT
2 Bob 25 60000 HR
3 Charlie 35 80000 IT
4 Diana 28 65000 Finance
5 Eve 32 70000 HR
;
RUN;

/* Create dataset from existing data */
DATA work.employees_copy;
    SET work.employees;
RUN;

/* Variable assignment and transformations */
DATA work.employees_enhanced;
    SET work.employees;

    /* Numeric operations */
    annual_bonus = salary * 0.1;
    tax = salary * 0.25;
    net_salary = salary - tax + annual_bonus;

    /* Length statement */
    LENGTH full_name $50 status $10;

    /* Character operations */
    full_name = CATX(' ', 'Employee:', name);
    status = 'Active';

    /* Date operations */
    hire_date = TODAY();
    FORMAT hire_date DATE9.;

    /* Conditional assignment */
    IF age < 30 THEN age_group = 'Young';
    ELSE IF age < 40 THEN age_group = 'Middle';
    ELSE age_group = 'Senior';

    /* RETAIN and LAG */
    RETAIN running_total 0;
    running_total = running_total + salary;
    prev_salary = LAG(salary);

    /* DROP and KEEP */
    DROP prev_salary;
RUN;

/* ==============================================================================
   Variable Attributes
   ============================================================================== */

DATA work.formatted_data;
    SET work.employees;

    /* Length */
    LENGTH new_var $20;

    /* Label */
    LABEL id = 'Employee ID'
          name = 'Employee Name'
          salary = 'Annual Salary (USD)';

    /* Format */
    FORMAT salary DOLLAR12.2
           hire_date DATE9.;

    /* Informat */
    INFORMAT birth_date DATE9.;
RUN;

/* ==============================================================================
   Conditional Logic
   ============================================================================== */

DATA work.categorized;
    SET work.employees;

    /* IF-THEN-ELSE */
    IF salary > 70000 THEN
        salary_level = 'High';
    ELSE IF salary > 60000 THEN
        salary_level = 'Medium';
    ELSE
        salary_level = 'Low';

    /* SELECT-WHEN */
    SELECT (department);
        WHEN ('IT') dept_code = 100;
        WHEN ('HR') dept_code = 200;
        WHEN ('Finance') dept_code = 300;
        OTHERWISE dept_code = 999;
    END;

    /* Subsetting IF */
    IF age >= 25;

    /* DELETE statement */
    IF salary < 50000 THEN DELETE;

    /* OUTPUT statement */
    OUTPUT;
RUN;

/* WHERE clause */
DATA work.it_employees;
    SET work.employees;
    WHERE department = 'IT' AND age > 25;
RUN;

/* ==============================================================================
   Loops
   ============================================================================== */

DATA work.loop_examples;
    /* DO loop */
    DO i = 1 TO 10;
        square = i ** 2;
        OUTPUT;
    END;
RUN;

DATA work.loop_examples2;
    /* DO-WHILE loop */
    n = 1;
    total = 0;
    DO WHILE (total < 100);
        total = total + n;
        n = n + 1;
        OUTPUT;
    END;
RUN;

DATA work.loop_examples3;
    /* DO-UNTIL loop */
    n = 1;
    factorial = 1;
    DO UNTIL (n > 10);
        factorial = factorial * n;
        OUTPUT;
        n = n + 1;
    END;
RUN;

DATA work.loop_examples4;
    /* DO loop with BY */
    DO x = 0 TO 10 BY 0.5;
        y = SIN(x);
        OUTPUT;
    END;
RUN;

DATA work.nested_loops;
    /* Nested loops */
    DO i = 1 TO 3;
        DO j = 1 TO 3;
            product = i * j;
            OUTPUT;
        END;
    END;
RUN;

/* ==============================================================================
   Arrays
   ============================================================================== */

DATA work.array_example;
    /* Define array */
    ARRAY scores[5] score1-score5;
    ARRAY grades[5] $ grade1-grade5;
    ARRAY temps[12] jan feb mar apr may jun jul aug sep oct nov dec;

    /* Array with initial values */
    ARRAY weights[4] _TEMPORARY_ (1 2 3 4);

    /* Two-dimensional array */
    ARRAY matrix[3,3] m1-m9;

    /* Process array */
    INPUT score1-score5;
    total = 0;
    DO i = 1 TO 5;
        total = total + scores[i];
        IF scores[i] >= 90 THEN grades[i] = 'A';
        ELSE IF scores[i] >= 80 THEN grades[i] = 'B';
        ELSE IF scores[i] >= 70 THEN grades[i] = 'C';
        ELSE grades[i] = 'F';
    END;
    average = total / 5;

    /* DIM function */
    DO j = 1 TO DIM(scores);
        PUT scores[j]=;
    END;

    DATALINES;
85 90 78 92 88
;
RUN;

/* ==============================================================================
   Functions
   ============================================================================== */

DATA work.functions_demo;
    /* Numeric functions */
    a = ABS(-5);
    b = CEIL(3.2);
    c = FLOOR(3.8);
    d = ROUND(3.14159, 0.01);
    e = INT(5.9);
    f = MOD(17, 5);
    g = SQRT(16);
    h = LOG(10);
    i = LOG10(100);
    j = EXP(1);
    k = MIN(1, 2, 3);
    l = MAX(1, 2, 3);
    m = SUM(1, 2, 3, 4, 5);
    n = MEAN(1, 2, 3, 4, 5);
    o = STD(1, 2, 3, 4, 5);
    p = RANGE(1, 2, 3, 4, 5);
    q = N(1, 2, ., 4, 5);
    r = NMISS(1, 2, ., 4, .);

    /* Trigonometric functions */
    s = SIN(0);
    t = COS(0);
    u = TAN(0);
    v = ATAN(1);

    /* Random functions */
    w = RANUNI(0);
    x = RANNOR(0);

    /* Character functions */
    str = '  Hello, World!  ';
    len = LENGTH(str);
    trimmed = STRIP(str);
    upper_str = UPCASE(str);
    lower_str = LOWCASE(str);
    proper_str = PROPCASE(str);
    substr_str = SUBSTR(str, 3, 5);
    index_pos = INDEX(str, 'World');
    scan_word = SCAN('a,b,c', 2, ',');
    compress_str = COMPRESS(str);
    translate_str = TRANSLATE('abc', 'ABC', 'abc');
    tranwrd_str = TRANWRD(str, 'World', 'SAS');
    cat_str = CAT('Hello', ' ', 'World');
    cats_str = CATS('  a  ', '  b  ');
    catx_str = CATX(',', 'a', 'b', 'c');
    left_str = LEFT(str);
    right_str = RIGHT(str);
    reverse_str = REVERSE('abc');
    repeat_str = REPEAT('ab', 3);

    /* Date functions */
    today_date = TODAY();
    now_datetime = DATETIME();
    year_val = YEAR(today_date);
    month_val = MONTH(today_date);
    day_val = DAY(today_date);
    weekday_val = WEEKDAY(today_date);
    qtr_val = QTR(today_date);
    intck_val = INTCK('MONTH', '01JAN2024'D, '01JUL2024'D);
    intnx_val = INTNX('MONTH', today_date, 3);
    mdy_date = MDY(1, 15, 2024);
    yrdif_val = YRDIF('01JAN2000'D, today_date, 'ACTUAL');

    /* Format dates */
    FORMAT today_date intnx_val DATE9.
           now_datetime DATETIME19.;

    /* Type conversion */
    num_to_char = PUT(123, 8.);
    char_to_num = INPUT('123', 8.);

    /* Missing value functions */
    miss_test = MISSING(.);
    coalesce_val = COALESCE(., ., 5);
    coalescec_val = COALESCEC('', '', 'default');

    /* Logical functions */
    verify_val = VERIFY('abc123', 'abcdefghijklmnopqrstuvwxyz');
    any_digit = ANYDIGIT('abc123');
    not_digit = NOTDIGIT('abc123');
RUN;

/* ==============================================================================
   Merging and Combining Datasets
   ============================================================================== */

/* Create sample datasets */
DATA work.dataset1;
    INPUT id value1;
    DATALINES;
1 100
2 200
3 300
;
RUN;

DATA work.dataset2;
    INPUT id value2 $;
    DATALINES;
1 A
2 B
4 D
;
RUN;

/* One-to-one merge */
DATA work.merged;
    MERGE work.dataset1 work.dataset2;
    BY id;
RUN;

/* Left join equivalent */
DATA work.left_join;
    MERGE work.dataset1 (IN=a) work.dataset2 (IN=b);
    BY id;
    IF a;
RUN;

/* Inner join equivalent */
DATA work.inner_join;
    MERGE work.dataset1 (IN=a) work.dataset2 (IN=b);
    BY id;
    IF a AND b;
RUN;

/* Full outer join equivalent */
DATA work.full_join;
    MERGE work.dataset1 (IN=a) work.dataset2 (IN=b);
    BY id;
RUN;

/* Concatenate datasets */
DATA work.concatenated;
    SET work.dataset1 work.dataset2;
RUN;

/* Interleave datasets */
DATA work.interleaved;
    SET work.dataset1 work.dataset2;
    BY id;
RUN;

/* Update dataset */
DATA work.master;
    UPDATE work.dataset1 work.dataset2;
    BY id;
RUN;

/* ==============================================================================
   PROC SQL
   ============================================================================== */

PROC SQL;
    /* Select all */
    SELECT * FROM work.employees;

    /* Select specific columns with aliases */
    SELECT
        id AS employee_id,
        name AS employee_name,
        salary FORMAT=DOLLAR12.2
    FROM work.employees;

    /* Filter with WHERE */
    SELECT * FROM work.employees
    WHERE department = 'IT' AND salary > 65000;

    /* Aggregations */
    SELECT
        department,
        COUNT(*) AS employee_count,
        SUM(salary) AS total_salary FORMAT=DOLLAR15.2,
        AVG(salary) AS avg_salary FORMAT=DOLLAR12.2,
        MIN(salary) AS min_salary FORMAT=DOLLAR12.2,
        MAX(salary) AS max_salary FORMAT=DOLLAR12.2
    FROM work.employees
    GROUP BY department
    HAVING COUNT(*) > 1
    ORDER BY total_salary DESC;

    /* Joins */
    SELECT a.*, b.value2
    FROM work.dataset1 AS a
    LEFT JOIN work.dataset2 AS b
    ON a.id = b.id;

    /* Inner join */
    SELECT a.*, b.value2
    FROM work.dataset1 AS a
    INNER JOIN work.dataset2 AS b
    ON a.id = b.id;

    /* Full outer join */
    SELECT COALESCE(a.id, b.id) AS id, a.value1, b.value2
    FROM work.dataset1 AS a
    FULL JOIN work.dataset2 AS b
    ON a.id = b.id;

    /* Subquery */
    SELECT * FROM work.employees
    WHERE salary > (SELECT AVG(salary) FROM work.employees);

    /* Create table */
    CREATE TABLE work.new_table AS
    SELECT * FROM work.employees
    WHERE department = 'IT';

    /* Insert rows */
    INSERT INTO work.new_table
    VALUES (6, 'Frank', 29, 72000, 'IT');

    /* Update rows */
    UPDATE work.new_table
    SET salary = salary * 1.1
    WHERE age > 30;

    /* Delete rows */
    DELETE FROM work.new_table
    WHERE id = 6;

    /* Case expression */
    SELECT
        name,
        salary,
        CASE
            WHEN salary > 70000 THEN 'High'
            WHEN salary > 60000 THEN 'Medium'
            ELSE 'Low'
        END AS salary_level
    FROM work.employees;

    /* Union */
    SELECT id, name FROM work.employees WHERE department = 'IT'
    UNION
    SELECT id, name FROM work.employees WHERE department = 'HR';

    /* Except */
    SELECT id FROM work.dataset1
    EXCEPT
    SELECT id FROM work.dataset2;

    /* Intersect */
    SELECT id FROM work.dataset1
    INTERSECT
    SELECT id FROM work.dataset2;
QUIT;

/* ==============================================================================
   Procedures
   ============================================================================== */

/* PROC PRINT */
PROC PRINT DATA=work.employees NOOBS LABEL;
    VAR name age salary;
    WHERE department = 'IT';
    TITLE 'IT Department Employees';
    FOOTNOTE 'Source: HR Database';
RUN;

/* PROC SORT */
PROC SORT DATA=work.employees OUT=work.employees_sorted;
    BY department DESCENDING salary;
RUN;

/* PROC SORT with NODUPKEY */
PROC SORT DATA=work.employees OUT=work.unique_depts NODUPKEY;
    BY department;
RUN;

/* PROC CONTENTS */
PROC CONTENTS DATA=work.employees;
RUN;

/* PROC MEANS */
PROC MEANS DATA=work.employees N MEAN STD MIN MAX;
    VAR salary age;
    CLASS department;
    OUTPUT OUT=work.summary_stats
           MEAN=mean_salary mean_age
           STD=std_salary std_age;
RUN;

/* PROC SUMMARY */
PROC SUMMARY DATA=work.employees NWAY;
    CLASS department;
    VAR salary;
    OUTPUT OUT=work.dept_summary SUM= MEAN= / AUTONAME;
RUN;

/* PROC FREQ */
PROC FREQ DATA=work.employees;
    TABLES department * age_group / CHISQ;
    TABLES salary_level;
RUN;

/* PROC UNIVARIATE */
PROC UNIVARIATE DATA=work.employees;
    VAR salary;
    HISTOGRAM salary / NORMAL;
    QQPLOT salary;
RUN;

/* PROC CORR */
PROC CORR DATA=work.employees;
    VAR age salary;
RUN;

/* PROC REG */
PROC REG DATA=work.employees;
    MODEL salary = age;
    OUTPUT OUT=work.reg_out PREDICTED=predicted RESIDUAL=residual;
RUN;
QUIT;

/* PROC GLM */
PROC GLM DATA=work.employees;
    CLASS department;
    MODEL salary = department age;
    LSMEANS department / PDIFF;
RUN;
QUIT;

/* PROC LOGISTIC */
/*
PROC LOGISTIC DATA=work.employees;
    CLASS department;
    MODEL high_salary(EVENT='1') = age department;
RUN;
*/

/* PROC TTEST */
PROC TTEST DATA=work.employees;
    CLASS department;
    VAR salary;
    WHERE department IN ('IT', 'HR');
RUN;

/* PROC ANOVA */
PROC ANOVA DATA=work.employees;
    CLASS department;
    MODEL salary = department;
    MEANS department / TUKEY;
RUN;
QUIT;

/* PROC TRANSPOSE */
PROC TRANSPOSE DATA=work.employees OUT=work.transposed;
    BY id;
    VAR salary age;
RUN;

/* PROC DATASETS */
PROC DATASETS LIBRARY=work NOLIST;
    DELETE temp_*;
    CHANGE old_name = new_name;
    MODIFY employees;
        RENAME salary = annual_salary;
        LABEL annual_salary = 'Annual Salary (USD)';
        FORMAT annual_salary DOLLAR12.2;
RUN;
QUIT;

/* PROC EXPORT */
PROC EXPORT DATA=work.employees
    OUTFILE='/path/to/output.csv'
    DBMS=CSV
    REPLACE;
RUN;

/* PROC IMPORT */
PROC IMPORT DATAFILE='/path/to/input.csv'
    OUT=work.imported
    DBMS=CSV
    REPLACE;
    GETNAMES=YES;
    GUESSINGROWS=MAX;
RUN;

/* ==============================================================================
   Graphics Procedures
   ============================================================================== */

/* PROC SGPLOT */
PROC SGPLOT DATA=work.employees;
    SCATTER X=age Y=salary / GROUP=department;
    REG X=age Y=salary / NOMARKERS;
    TITLE 'Age vs Salary by Department';
    XAXIS LABEL='Age (Years)';
    YAXIS LABEL='Salary (USD)' GRID;
RUN;

PROC SGPLOT DATA=work.employees;
    VBAR department / RESPONSE=salary STAT=MEAN;
    TITLE 'Average Salary by Department';
RUN;

PROC SGPLOT DATA=work.employees;
    HISTOGRAM salary;
    DENSITY salary / TYPE=KERNEL;
    TITLE 'Distribution of Salaries';
RUN;

PROC SGPLOT DATA=work.employees;
    VBOX salary / CATEGORY=department;
    TITLE 'Salary Distribution by Department';
RUN;

/* PROC SGPANEL */
PROC SGPANEL DATA=work.employees;
    PANELBY department;
    HISTOGRAM salary;
    TITLE 'Salary Distribution by Department';
RUN;

/* ==============================================================================
   Macro Language
   ============================================================================== */

/* Macro variables */
%LET dataset = work.employees;
%LET var = salary;
%LET threshold = 65000;

/* Using macro variables */
PROC PRINT DATA=&dataset;
    VAR name &var;
    WHERE &var > &threshold;
RUN;

/* Macro variable functions */
%LET text = Hello World;
%LET upper_text = %UPCASE(&text);
%LET lower_text = %LOWCASE(&text);
%LET length = %LENGTH(&text);
%LET substr = %SUBSTR(&text, 1, 5);
%LET index = %INDEX(&text, World);
%LET scan = %SCAN(&text, 2, %STR( ));
%LET trim = %TRIM(&text);
%LET left = %LEFT(&text);
%LET compress = %COMPRESS(&text);
%LET translate = %TRANSLATE(&text, abc, ABC);
%LET eval_result = %EVAL(1 + 2);
%LET sysevalf_result = %SYSEVALF(3.14 * 2);

/* System macro variables */
%PUT &SYSDATE;
%PUT &SYSDATE9;
%PUT &SYSTIME;
%PUT &SYSDAY;
%PUT &SYSUSERID;
%PUT &SYSVER;
%PUT &SYSJOBID;
%PUT &SYSLAST;
%PUT &SYSERR;
%PUT &SQLRC;
%PUT &SQLOBS;

/* Define macro */
%MACRO print_dataset(ds, vars=_ALL_);
    PROC PRINT DATA=&ds NOOBS;
        VAR &vars;
    RUN;
%MEND print_dataset;

/* Call macro */
%print_dataset(work.employees, vars=name salary);

/* Macro with conditional logic */
%MACRO analyze(ds, type=summary);
    %IF &type = summary %THEN %DO;
        PROC MEANS DATA=&ds;
        RUN;
    %END;
    %ELSE %IF &type = freq %THEN %DO;
        PROC FREQ DATA=&ds;
            TABLES _CHARACTER_;
        RUN;
    %END;
    %ELSE %DO;
        %PUT ERROR: Invalid type specified;
    %END;
%MEND analyze;

%analyze(work.employees, type=summary);

/* Macro with loops */
%MACRO create_datasets(n);
    %DO i = 1 %TO &n;
        DATA work.dataset_&i;
            x = &i;
        RUN;
    %END;
%MEND create_datasets;

%create_datasets(5);

/* Macro with SYMPUT and SYMGET */
DATA _NULL_;
    SET work.employees END=last;
    IF last THEN DO;
        CALL SYMPUTX('total_rows', _N_);
        CALL SYMPUTX('max_salary', salary);
    END;
RUN;

%PUT Total rows: &total_rows;
%PUT Max salary: &max_salary;

/* Macro quoting functions */
%LET special = %STR(;);
%LET with_amp = %NRSTR(&var);
%LET quoted = %BQUOTE(It's working);
%LET unquoted = %UNQUOTE(&quoted);

/* ==============================================================================
   Formats and Informats
   ============================================================================== */

/* User-defined format */
PROC FORMAT;
    VALUE salary_fmt
        LOW-50000 = 'Low'
        50000<-70000 = 'Medium'
        70000<-HIGH = 'High';

    VALUE $dept_fmt
        'IT' = 'Information Technology'
        'HR' = 'Human Resources'
        'Finance' = 'Finance Department'
        OTHER = 'Other';

    VALUE age_fmt
        LOW-<25 = 'Junior'
        25-<35 = 'Mid-Level'
        35-<45 = 'Senior'
        45-HIGH = 'Executive';

    PICTURE phone_fmt
        LOW-HIGH = '(999) 999-9999' (PREFIX='(');
RUN;

/* Use custom format */
DATA work.formatted;
    SET work.employees;
    FORMAT salary salary_fmt.
           department $dept_fmt.
           age age_fmt.;
RUN;

/* ==============================================================================
   ODS (Output Delivery System)
   ============================================================================== */

/* ODS destinations */
ODS HTML FILE='/path/to/output.html';
ODS PDF FILE='/path/to/output.pdf';
ODS RTF FILE='/path/to/output.rtf';
ODS EXCEL FILE='/path/to/output.xlsx';

PROC PRINT DATA=work.employees;
RUN;

ODS HTML CLOSE;
ODS PDF CLOSE;
ODS RTF CLOSE;
ODS EXCEL CLOSE;

/* ODS options */
ODS HTML FILE='/path/to/output.html' STYLE=HTMLBlue;
ODS PDF FILE='/path/to/output.pdf' STYLE=Journal;

/* ODS TRACE */
ODS TRACE ON;
PROC MEANS DATA=work.employees;
    VAR salary;
RUN;
ODS TRACE OFF;

/* ODS OUTPUT */
ODS OUTPUT Summary=work.summary_output;
PROC MEANS DATA=work.employees;
    VAR salary age;
RUN;

/* ODS SELECT/EXCLUDE */
ODS SELECT Summary;
PROC MEANS DATA=work.employees;
    VAR salary;
RUN;

/* ==============================================================================
   Hash Objects
   ============================================================================== */

DATA work.hash_example;
    /* Declare hash object */
    IF _N_ = 1 THEN DO;
        DECLARE HASH h(DATASET: 'work.dataset2');
        h.DEFINEKEY('id');
        h.DEFINEDATA('value2');
        h.DEFINEDONE();
    END;

    /* Lookup using hash */
    SET work.dataset1;
    CALL MISSING(value2);
    rc = h.FIND();
RUN;

/* Hash table with ordered output */
DATA work.hash_ordered;
    IF _N_ = 1 THEN DO;
        DECLARE HASH h(ORDERED: 'A');
        h.DEFINEKEY('id');
        h.DEFINEDATA('id', 'value');
        h.DEFINEDONE();
    END;

    /* Add data to hash */
    DO id = 5, 3, 1, 4, 2;
        value = id * 10;
        h.ADD();
    END;

    /* Output sorted data */
    h.OUTPUT(DATASET: 'work.sorted_output');
    STOP;
RUN;

/* ==============================================================================
   Error Handling
   ============================================================================== */

/* Check for errors */
DATA work.error_check;
    SET work.employees;

    /* Input validation */
    IF salary < 0 THEN DO;
        PUT 'ERROR: Negative salary for ' name;
        DELETE;
    END;

    /* Missing value handling */
    IF MISSING(department) THEN DO;
        PUT 'WARNING: Missing department for ' name;
        department = 'Unknown';
    END;
RUN;

/* OPTIONS for error handling */
OPTIONS ERRORS=20;
OPTIONS NOTES;
OPTIONS NONOTES;
OPTIONS SOURCE;
OPTIONS NOSOURCE;

/* ABORT statement */
/*
DATA work.critical;
    SET work.employees;
    IF _ERROR_ THEN ABORT;
RUN;
*/

/* ==============================================================================
   Performance Options
   ============================================================================== */

/* Data step options */
DATA work.performance_example;
    LENGTH name $50;
    SET work.employees (KEEP=id name salary
                        WHERE=(salary > 50000)
                        OBS=1000);
RUN;

/* Index creation */
PROC DATASETS LIBRARY=work NOLIST;
    MODIFY employees;
    INDEX CREATE id / UNIQUE;
    INDEX CREATE dept_age = (department age);
RUN;
QUIT;

/* Compression */
DATA work.compressed (COMPRESS=YES);
    SET work.employees;
RUN;

/* Buffer and threading options */
OPTIONS BUFSIZE=32K;
OPTIONS THREADS;
OPTIONS CPUCOUNT=4;

/* ==============================================================================
   Clean Up
   ============================================================================== */

/* Delete datasets */
PROC DELETE DATA=work.temp1 work.temp2;
RUN;

/* Clear formats */
PROC CATALOG CATALOG=work.formats;
    DELETE salary_fmt.FORMAT;
RUN;
QUIT;

/* Clear titles and footnotes */
TITLE;
FOOTNOTE;

/* ==============================================================================
   End of SAS Sample
   ============================================================================== */
