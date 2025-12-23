      ******************************************************************
      * COMPREHENSIVE COBOL SAMPLE - SYNTAX HIGHLIGHTING DEMONSTRATION
      ******************************************************************
      * This program demonstrates all major COBOL language features
      * for syntax highlighting purposes.
      ******************************************************************

      ******************************************************************
      * IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE-PROGRAM.
       AUTHOR. SYNTAX-HIGHLIGHTER.
       INSTALLATION. DEVELOPMENT-ENVIRONMENT.
       DATE-WRITTEN. 2024-01-15.
       DATE-COMPILED.
       SECURITY. NONE.
       REMARKS.
           This is a comprehensive COBOL sample file demonstrating
           various language features for syntax highlighting.

      ******************************************************************
      * ENVIRONMENT DIVISION
      ******************************************************************
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "$"
           DECIMAL-POINT IS COMMA
           CLASS ALPHANUMERIC-CLASS IS "A" THRU "Z", "a" THRU "z"
                                       "0" THRU "9"
           SYMBOLIC CHARACTERS TAB-CHAR IS 10.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO "CUSTFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               ALTERNATE RECORD KEY IS CUST-NAME
                   WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.

           SELECT TRANSACTION-FILE
               ASSIGN TO "TRANSACT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO "REPORT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORT-FILE
               ASSIGN TO "SORTWORK".

       I-O-CONTROL.
           APPLY WRITE-ONLY ON REPORT-FILE.

      ******************************************************************
      * DATA DIVISION
      ******************************************************************
       DATA DIVISION.

      ******************************************************************
       FILE SECTION.
      ******************************************************************
       FD  CUSTOMER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS
           BLOCK CONTAINS 10 RECORDS
           DATA RECORD IS CUSTOMER-RECORD.

       01  CUSTOMER-RECORD.
           05  CUST-ID                 PIC 9(8).
           05  CUST-NAME               PIC X(30).
           05  CUST-ADDRESS.
               10  CUST-STREET         PIC X(40).
               10  CUST-CITY           PIC X(20).
               10  CUST-STATE          PIC X(2).
               10  CUST-ZIP            PIC 9(5).
           05  CUST-PHONE              PIC 9(10).
           05  CUST-EMAIL              PIC X(50).
           05  CUST-BALANCE            PIC S9(9)V99 COMP-3.
           05  CUST-CREDIT-LIMIT       PIC 9(7)V99 COMP.
           05  CUST-STATUS             PIC X(1).
               88  CUST-ACTIVE         VALUE "A".
               88  CUST-INACTIVE       VALUE "I".
               88  CUST-SUSPENDED      VALUE "S".
           05  CUST-CREATED-DATE       PIC 9(8).
           05  FILLER                  PIC X(19).

       FD  TRANSACTION-FILE
           RECORD CONTAINS 100 CHARACTERS.

       01  TRANSACTION-RECORD.
           05  TRANS-ID                PIC 9(10).
           05  TRANS-CUST-ID           PIC 9(8).
           05  TRANS-TYPE              PIC X(1).
               88  TRANS-PURCHASE      VALUE "P".
               88  TRANS-PAYMENT       VALUE "Y".
               88  TRANS-REFUND        VALUE "R".
           05  TRANS-AMOUNT            PIC S9(7)V99.
           05  TRANS-DATE              PIC 9(8).
           05  TRANS-TIME              PIC 9(6).
           05  TRANS-DESC              PIC X(50).
           05  FILLER                  PIC X(8).

       FD  REPORT-FILE
           RECORD CONTAINS 132 CHARACTERS.

       01  REPORT-LINE                 PIC X(132).

       SD  SORT-FILE
           RECORD CONTAINS 100 CHARACTERS.

       01  SORT-RECORD.
           05  SORT-KEY                PIC 9(8).
           05  SORT-DATA               PIC X(92).

      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
       01  WS-FILE-STATUS              PIC XX.
           88  WS-FILE-OK              VALUE "00".
           88  WS-END-OF-FILE          VALUE "10".
           88  WS-KEY-NOT-FOUND        VALUE "23".

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X VALUE "N".
               88  WS-EOF              VALUE "Y".
               88  WS-NOT-EOF          VALUE "N".
           05  WS-ERROR-SW             PIC X VALUE "N".
               88  WS-ERROR            VALUE "Y".
               88  WS-NO-ERROR         VALUE "N".

       01  WS-COUNTERS.
           05  WS-RECORD-COUNT         PIC 9(7) VALUE ZERO.
           05  WS-PAGE-COUNT           PIC 9(3) VALUE ZERO.
           05  WS-LINE-COUNT           PIC 9(2) VALUE ZERO.
           05  WS-ERROR-COUNT          PIC 9(5) VALUE ZERO.

       01  WS-TOTALS.
           05  WS-TOTAL-AMOUNT         PIC S9(11)V99 VALUE ZERO.
           05  WS-TOTAL-CREDITS        PIC S9(11)V99 VALUE ZERO.
           05  WS-TOTAL-DEBITS         PIC S9(11)V99 VALUE ZERO.
           05  WS-AVERAGE-AMOUNT       PIC S9(9)V99 VALUE ZERO.

       01  WS-NUMERIC-FIELDS.
           05  WS-INTEGER              PIC 9(9) VALUE 12345.
           05  WS-SIGNED-INT           PIC S9(9) VALUE -9876.
           05  WS-DECIMAL              PIC 9(5)V99 VALUE 123.45.
           05  WS-SIGNED-DEC           PIC S9(5)V99 VALUE -987.65.
           05  WS-PACKED               PIC S9(7)V99 COMP-3 VALUE 0.
           05  WS-BINARY               PIC S9(9) COMP VALUE 0.
           05  WS-BINARY-2             PIC S9(4) COMP-4 VALUE 0.
           05  WS-FLOAT                COMP-1 VALUE 3.14159.
           05  WS-DOUBLE               COMP-2 VALUE 2.71828.

       01  WS-TEXT-FIELDS.
           05  WS-NAME                 PIC X(30) VALUE SPACES.
           05  WS-MESSAGE              PIC X(80) VALUE SPACES.
           05  WS-LITERAL              PIC X(20) VALUE "HELLO WORLD".
           05  WS-JUSTIFIED            PIC X(30) JUSTIFIED RIGHT.

       01  WS-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR     PIC 9(4).
               10  WS-CURRENT-MONTH    PIC 9(2).
               10  WS-CURRENT-DAY      PIC 9(2).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOUR     PIC 9(2).
               10  WS-CURRENT-MIN      PIC 9(2).
               10  WS-CURRENT-SEC      PIC 9(2).
               10  WS-CURRENT-HUND     PIC 9(2).
           05  WS-DATE-INTEGER         PIC 9(7).

       01  WS-EDITED-FIELDS.
           05  WS-EDIT-AMOUNT          PIC $$$,$$$,$$9.99-.
           05  WS-EDIT-DATE            PIC 99/99/9999.
           05  WS-EDIT-PHONE           PIC (999) 999-9999.
           05  WS-EDIT-SSN             PIC 999-99-9999.
           05  WS-EDIT-ZERO-SUP        PIC ZZZZZ9.99.
           05  WS-EDIT-STARS           PIC *****9.99.
           05  WS-EDIT-FLOAT-SIGN      PIC ++++9.99.
           05  WS-EDIT-DB-CR           PIC $$$,$$9.99DB.

       01  WS-ARRAYS.
           05  WS-MONTH-TABLE.
               10  FILLER              PIC X(9) VALUE "JANUARY".
               10  FILLER              PIC X(9) VALUE "FEBRUARY".
               10  FILLER              PIC X(9) VALUE "MARCH".
               10  FILLER              PIC X(9) VALUE "APRIL".
               10  FILLER              PIC X(9) VALUE "MAY".
               10  FILLER              PIC X(9) VALUE "JUNE".
               10  FILLER              PIC X(9) VALUE "JULY".
               10  FILLER              PIC X(9) VALUE "AUGUST".
               10  FILLER              PIC X(9) VALUE "SEPTEMBER".
               10  FILLER              PIC X(9) VALUE "OCTOBER".
               10  FILLER              PIC X(9) VALUE "NOVEMBER".
               10  FILLER              PIC X(9) VALUE "DECEMBER".
           05  WS-MONTH-NAME REDEFINES WS-MONTH-TABLE
                                       PIC X(9) OCCURS 12 TIMES.

       01  WS-MULTI-DIM-ARRAY.
           05  WS-REGION               OCCURS 4 TIMES
                                       INDEXED BY WS-REGION-IDX.
               10  WS-BRANCH           OCCURS 10 TIMES
                                       INDEXED BY WS-BRANCH-IDX.
                   15  WS-BRANCH-NAME  PIC X(20).
                   15  WS-BRANCH-SALES PIC S9(9)V99.

       01  WS-VARIABLE-ARRAY.
           05  WS-ITEM-COUNT           PIC 9(3) VALUE 0.
           05  WS-ITEM OCCURS 1 TO 100 TIMES
                       DEPENDING ON WS-ITEM-COUNT
                       ASCENDING KEY IS WS-ITEM-KEY
                       INDEXED BY WS-ITEM-IDX.
               10  WS-ITEM-KEY         PIC 9(5).
               10  WS-ITEM-DESC        PIC X(30).
               10  WS-ITEM-PRICE       PIC 9(5)V99.

       01  WS-REFERENCE-MOD.
           05  WS-FULL-STRING          PIC X(50).
           05  WS-SUBSTRING            PIC X(10).

       01  WS-POINTERS.
           05  WS-DATA-PTR             POINTER.
           05  WS-PROC-PTR             PROCEDURE-POINTER.

      ******************************************************************
       LOCAL-STORAGE SECTION.
      ******************************************************************
       01  LS-WORK-AREA                PIC X(100).

      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
       01  LS-PARAMETER.
           05  LS-ACTION               PIC X(10).
           05  LS-CUSTOMER-ID          PIC 9(8).
           05  LS-RETURN-CODE          PIC S9(4) COMP.

      ******************************************************************
       REPORT SECTION.
      ******************************************************************
       RD  CUSTOMER-REPORT
           PAGE LIMIT IS 60
           HEADING 1
           FIRST DETAIL 5
           LAST DETAIL 55.

       01  TYPE PAGE HEADING.
           05  LINE 1.
               10  COLUMN 1            PIC X(30)
                   VALUE "CUSTOMER REPORT".
               10  COLUMN 50           PIC X(10)
                   VALUE "PAGE:".
               10  COLUMN 61           PIC Z(3)9
                   SOURCE WS-PAGE-COUNT.

       01  CUSTOMER-DETAIL TYPE DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1            PIC 9(8)
                   SOURCE CUST-ID.
               10  COLUMN 12           PIC X(30)
                   SOURCE CUST-NAME.
               10  COLUMN 45           PIC $$$,$$$,$$9.99
                   SOURCE CUST-BALANCE.

      ******************************************************************
      * PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION USING LS-PARAMETER.

      ******************************************************************
       0000-MAIN-PARAGRAPH.
      ******************************************************************
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-FILES
           PERFORM 9000-TERMINATE
           STOP RUN.

      ******************************************************************
       1000-INITIALIZE.
      ******************************************************************
           DISPLAY "PROGRAM STARTING..."

           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE

           INITIALIZE WS-COUNTERS
           INITIALIZE WS-TOTALS

           OPEN INPUT  CUSTOMER-FILE
                INPUT  TRANSACTION-FILE
                OUTPUT REPORT-FILE

           IF NOT WS-FILE-OK
               DISPLAY "FILE OPEN ERROR: " WS-FILE-STATUS
               MOVE 1 TO LS-RETURN-CODE
               STOP RUN
           END-IF.

      ******************************************************************
       2000-PROCESS-FILES.
      ******************************************************************
           PERFORM UNTIL WS-EOF
               READ CUSTOMER-FILE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       PERFORM 2100-PROCESS-CUSTOMER
               END-READ
           END-PERFORM.

      ******************************************************************
       2100-PROCESS-CUSTOMER.
      ******************************************************************
           ADD 1 TO WS-RECORD-COUNT

           EVALUATE TRUE
               WHEN CUST-ACTIVE
                   PERFORM 2110-ACTIVE-CUSTOMER
               WHEN CUST-INACTIVE
                   PERFORM 2120-INACTIVE-CUSTOMER
               WHEN CUST-SUSPENDED
                   PERFORM 2130-SUSPENDED-CUSTOMER
               WHEN OTHER
                   DISPLAY "UNKNOWN STATUS: " CUST-STATUS
           END-EVALUATE

           IF CUST-BALANCE > CUST-CREDIT-LIMIT
               DISPLAY "OVER LIMIT: " CUST-ID
               ADD 1 TO WS-ERROR-COUNT
           END-IF.

      ******************************************************************
       2110-ACTIVE-CUSTOMER.
      ******************************************************************
           ADD CUST-BALANCE TO WS-TOTAL-AMOUNT

           MOVE CUST-BALANCE TO WS-EDIT-AMOUNT
           DISPLAY "ACTIVE CUSTOMER: " CUST-NAME
                   " BALANCE: " WS-EDIT-AMOUNT.

      ******************************************************************
       2120-INACTIVE-CUSTOMER.
      ******************************************************************
           DISPLAY "INACTIVE CUSTOMER: " CUST-NAME.

      ******************************************************************
       2130-SUSPENDED-CUSTOMER.
      ******************************************************************
           DISPLAY "SUSPENDED CUSTOMER: " CUST-NAME.

      ******************************************************************
       3000-ARITHMETIC-EXAMPLES.
      ******************************************************************
      * Basic arithmetic
           ADD 1 TO WS-RECORD-COUNT
           ADD WS-AMOUNT-1 WS-AMOUNT-2 GIVING WS-TOTAL-AMOUNT
           ADD CORR WS-INPUT-GROUP TO WS-OUTPUT-GROUP

           SUBTRACT 1 FROM WS-RECORD-COUNT
           SUBTRACT WS-DISCOUNT FROM WS-TOTAL GIVING WS-NET-AMOUNT

           MULTIPLY WS-QUANTITY BY WS-PRICE
           MULTIPLY WS-RATE BY WS-HOURS GIVING WS-PAY

           DIVIDE WS-TOTAL BY WS-COUNT GIVING WS-AVERAGE
           DIVIDE WS-TOTAL BY WS-COUNT GIVING WS-QUOTIENT
                   REMAINDER WS-REMAINDER

           COMPUTE WS-RESULT = (WS-A + WS-B) * WS-C / WS-D
           COMPUTE WS-RESULT ROUNDED = WS-A ** 2 + WS-B ** 0.5

      * On size error handling
           ADD WS-LARGE-NUM TO WS-TOTAL
               ON SIZE ERROR
                   DISPLAY "OVERFLOW OCCURRED"
                   MOVE 999999999 TO WS-TOTAL
               NOT ON SIZE ERROR
                   CONTINUE
           END-ADD.

      ******************************************************************
       4000-STRING-EXAMPLES.
      ******************************************************************
      * String operations
           STRING WS-FIRST-NAME DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  WS-LAST-NAME DELIMITED BY SPACE
                  INTO WS-FULL-NAME
                  WITH POINTER WS-STRING-PTR
               ON OVERFLOW
                   DISPLAY "STRING OVERFLOW"
           END-STRING

           UNSTRING WS-INPUT-LINE DELIMITED BY "," OR SPACE
               INTO WS-FIELD-1
                    WS-FIELD-2
                    WS-FIELD-3
               WITH POINTER WS-UNSTR-PTR
               TALLYING IN WS-FIELD-COUNT
               ON OVERFLOW
                   DISPLAY "UNSTRING OVERFLOW"
           END-UNSTRING

           INSPECT WS-TEXT-FIELD
               TALLYING WS-CHAR-COUNT FOR ALL "A"
               REPLACING ALL "X" BY "Y"

           INSPECT WS-TEXT-FIELD
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

      * Reference modification
           MOVE WS-FULL-STRING(1:10) TO WS-SUBSTRING
           MOVE WS-FULL-STRING(WS-START-POS:WS-LENGTH)
               TO WS-SUBSTRING.

      ******************************************************************
       5000-CONDITIONAL-EXAMPLES.
      ******************************************************************
      * IF statement
           IF WS-AMOUNT > 1000
               DISPLAY "LARGE AMOUNT"
           ELSE IF WS-AMOUNT > 100
               DISPLAY "MEDIUM AMOUNT"
           ELSE
               DISPLAY "SMALL AMOUNT"
           END-IF

      * Nested IF
           IF WS-STATUS = "A"
               IF WS-BALANCE > 0
                   DISPLAY "ACTIVE WITH BALANCE"
               END-IF
           END-IF

      * Complex conditions
           IF (WS-A > WS-B AND WS-C > WS-D)
              OR (WS-E = WS-F AND NOT WS-G = WS-H)
               DISPLAY "COMPLEX CONDITION MET"
           END-IF

      * Class conditions
           IF WS-FIELD IS NUMERIC
               DISPLAY "NUMERIC"
           END-IF

           IF WS-FIELD IS ALPHABETIC
               DISPLAY "ALPHABETIC"
           END-IF

           IF WS-FIELD IS ALPHANUMERIC-CLASS
               DISPLAY "CUSTOM CLASS"
           END-IF

      * Sign conditions
           IF WS-AMOUNT IS POSITIVE
               DISPLAY "POSITIVE"
           ELSE IF WS-AMOUNT IS NEGATIVE
               DISPLAY "NEGATIVE"
           ELSE
               DISPLAY "ZERO"
           END-IF

      * EVALUATE statement
           EVALUATE WS-CODE ALSO WS-FLAG
               WHEN 1 ALSO "Y"
                   DISPLAY "CODE 1 WITH Y"
               WHEN 2 THRU 5 ALSO "N"
                   DISPLAY "CODE 2-5 WITH N"
               WHEN 6 ALSO ANY
                   DISPLAY "CODE 6 WITH ANY FLAG"
               WHEN OTHER
                   DISPLAY "OTHER COMBINATION"
           END-EVALUATE.

      ******************************************************************
       6000-LOOP-EXAMPLES.
      ******************************************************************
      * PERFORM variations
           PERFORM 6100-SIMPLE-PARA

           PERFORM 6100-SIMPLE-PARA 5 TIMES

           PERFORM 6100-SIMPLE-PARA
               UNTIL WS-EOF

           PERFORM 6100-SIMPLE-PARA
               WITH TEST AFTER
               UNTIL WS-EOF

           PERFORM 6100-SIMPLE-PARA
               VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 10

           PERFORM 6100-SIMPLE-PARA
               VARYING WS-REGION-IDX FROM 1 BY 1
               UNTIL WS-REGION-IDX > 4
               AFTER WS-BRANCH-IDX FROM 1 BY 1
               UNTIL WS-BRANCH-IDX > 10

      * Inline PERFORM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               DISPLAY "ITERATION: " WS-I
               ADD WS-I TO WS-TOTAL
           END-PERFORM.

      ******************************************************************
       6100-SIMPLE-PARA.
      ******************************************************************
           ADD 1 TO WS-COUNTER.

      ******************************************************************
       7000-TABLE-EXAMPLES.
      ******************************************************************
      * Table access with index
           SET WS-ITEM-IDX TO 1
           SEARCH WS-ITEM
               AT END
                   DISPLAY "NOT FOUND"
               WHEN WS-ITEM-KEY(WS-ITEM-IDX) = WS-SEARCH-KEY
                   DISPLAY "FOUND AT: " WS-ITEM-IDX
           END-SEARCH

      * Binary search (table must be sorted)
           SEARCH ALL WS-ITEM
               AT END
                   DISPLAY "NOT FOUND"
               WHEN WS-ITEM-KEY(WS-ITEM-IDX) = WS-SEARCH-KEY
                   MOVE WS-ITEM-DESC(WS-ITEM-IDX) TO WS-RESULT
           END-SEARCH

      * SET statement
           SET WS-ITEM-IDX TO 1
           SET WS-ITEM-IDX UP BY 1
           SET WS-ITEM-IDX DOWN BY 2
           SET WS-ITEM-IDX TO WS-SAVE-IDX

      * SORT statement
           SORT SORT-FILE
               ON ASCENDING KEY SORT-KEY
               INPUT PROCEDURE IS 7100-SORT-INPUT
               OUTPUT PROCEDURE IS 7200-SORT-OUTPUT.

      ******************************************************************
       7100-SORT-INPUT.
      ******************************************************************
           PERFORM UNTIL WS-EOF
               READ CUSTOMER-FILE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       MOVE CUST-ID TO SORT-KEY
                       MOVE CUSTOMER-RECORD TO SORT-DATA
                       RELEASE SORT-RECORD
               END-READ
           END-PERFORM.

      ******************************************************************
       7200-SORT-OUTPUT.
      ******************************************************************
           PERFORM UNTIL WS-EOF
               RETURN SORT-FILE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       WRITE REPORT-LINE FROM SORT-DATA
               END-RETURN
           END-PERFORM.

      ******************************************************************
       8000-INTRINSIC-FUNCTIONS.
      ******************************************************************
      * Numeric functions
           COMPUTE WS-RESULT = FUNCTION ABS(WS-SIGNED-VALUE)
           COMPUTE WS-RESULT = FUNCTION MAX(WS-A WS-B WS-C)
           COMPUTE WS-RESULT = FUNCTION MIN(WS-A WS-B WS-C)
           COMPUTE WS-RESULT = FUNCTION SUM(WS-A WS-B WS-C)
           COMPUTE WS-RESULT = FUNCTION MEAN(WS-A WS-B WS-C)
           COMPUTE WS-RESULT = FUNCTION MEDIAN(WS-A WS-B WS-C)
           COMPUTE WS-RESULT = FUNCTION SQRT(WS-VALUE)
           COMPUTE WS-RESULT = FUNCTION INTEGER(WS-DECIMAL)
           COMPUTE WS-RESULT = FUNCTION MOD(WS-A WS-B)
           COMPUTE WS-RESULT = FUNCTION REM(WS-A WS-B)
           COMPUTE WS-RESULT = FUNCTION RANDOM
           COMPUTE WS-RESULT = FUNCTION LOG(WS-VALUE)
           COMPUTE WS-RESULT = FUNCTION LOG10(WS-VALUE)
           COMPUTE WS-RESULT = FUNCTION EXP(WS-VALUE)
           COMPUTE WS-RESULT = FUNCTION FACTORIAL(5)

      * String functions
           MOVE FUNCTION UPPER-CASE(WS-TEXT) TO WS-UPPER
           MOVE FUNCTION LOWER-CASE(WS-TEXT) TO WS-LOWER
           MOVE FUNCTION REVERSE(WS-TEXT) TO WS-REVERSED
           COMPUTE WS-LEN = FUNCTION LENGTH(WS-TEXT)
           COMPUTE WS-LEN = FUNCTION BYTE-LENGTH(WS-TEXT)
           MOVE FUNCTION TRIM(WS-TEXT) TO WS-TRIMMED
           MOVE FUNCTION TRIM(WS-TEXT LEADING) TO WS-TRIMMED
           MOVE FUNCTION TRIM(WS-TEXT TRAILING) TO WS-TRIMMED
           MOVE FUNCTION SUBSTITUTE(WS-TEXT "OLD" "NEW")
               TO WS-RESULT

      * Date/time functions
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME
           COMPUTE WS-INTEGER = FUNCTION INTEGER-OF-DATE(WS-DATE)
           MOVE FUNCTION DATE-OF-INTEGER(WS-INTEGER) TO WS-DATE
           COMPUTE WS-DAY-OF-WEEK =
               FUNCTION DAY-OF-INTEGER(WS-INTEGER)
           MOVE FUNCTION WHEN-COMPILED TO WS-COMPILE-DATE

      * Conversion functions
           COMPUTE WS-ORD = FUNCTION ORD("A")
           MOVE FUNCTION CHAR(65) TO WS-CHAR
           MOVE FUNCTION NUMVAL(WS-NUMERIC-STRING) TO WS-NUMBER
           MOVE FUNCTION NUMVAL-C(WS-CURRENCY-STRING) TO WS-NUMBER.

      ******************************************************************
       9000-TERMINATE.
      ******************************************************************
           DISPLAY "RECORDS PROCESSED: " WS-RECORD-COUNT
           DISPLAY "ERRORS FOUND: " WS-ERROR-COUNT

           CLOSE CUSTOMER-FILE
                 TRANSACTION-FILE
                 REPORT-FILE

           IF WS-ERROR-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF

           DISPLAY "PROGRAM COMPLETE"
           GOBACK.

      ******************************************************************
      * COPY and REPLACE
      ******************************************************************
      *    COPY CUSTOMER-LAYOUT.
      *    COPY COMMON-ROUTINES REPLACING ==:PREFIX:== BY ==WS-==.

      ******************************************************************
      * END OF PROGRAM
      ******************************************************************
