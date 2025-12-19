@echo off
REM ==============================================================================
REM Comprehensive Windows Batch Sample - Syntax Highlighting Demonstration
REM ==============================================================================

REM This script demonstrates all major Windows Batch language features
REM for syntax highlighting purposes.

REM Enable delayed expansion for variable manipulation
setlocal EnableDelayedExpansion

REM ==============================================================================
REM Comments
REM ==============================================================================

REM This is a REM comment
:: This is also a comment (label-style)

REM Multi-line comment simulation
goto :skip_comment
This is a block of text
that will be skipped.
It acts like a multi-line comment.
:skip_comment

REM ==============================================================================
REM Variable Declaration and Assignment
REM ==============================================================================

REM Simple variables
set NAME=Alice
set AGE=30
set MESSAGE=Hello, World!

REM Variable with spaces (quoted)
set "GREETING=Hello, World!"
set "PATH_WITH_SPACES=C:\Program Files\MyApp"

REM Numeric variables
set /a NUMBER=42
set /a RESULT=5+3
set /a PRODUCT=4*5
set /a QUOTIENT=20/4
set /a MODULO=17%%5
set /a POWER=2*2*2*2

REM Arithmetic expressions
set /a "SUM=10+20"
set /a "EXPR=(5+3)*2"
set /a "INC=NUMBER+1"

REM Increment/decrement
set /a COUNT+=1
set /a COUNT-=1

REM Bitwise operations
set /a "AND=0xFF & 0x0F"
set /a "OR=0xF0 | 0x0F"
set /a "XOR=0xFF ^ 0xF0"
set /a "NOT=~0"
set /a "SHIFT=1<<4"

REM ==============================================================================
REM Variable Expansion
REM ==============================================================================

REM Normal expansion
echo Name: %NAME%
echo Age: %AGE%

REM Delayed expansion (requires EnableDelayedExpansion)
echo Delayed: !NAME!

REM Substring
set STRING=Hello, World!
echo First 5 chars: %STRING:~0,5%
echo Last 6 chars: %STRING:~-6%
echo Skip first 7: %STRING:~7%
echo Middle: %STRING:~7,5%

REM String replacement
echo Replace: %STRING:World=Batch%
echo Remove: %STRING: =%

REM Environment variables
echo User: %USERNAME%
echo Computer: %COMPUTERNAME%
echo OS: %OS%
echo Path: %PATH%
echo Home: %USERPROFILE%
echo Temp: %TEMP%
echo Date: %DATE%
echo Time: %TIME%

REM Special variables
echo Script: %0
echo First arg: %1
echo All args: %*
echo Error level: %ERRORLEVEL%
echo Random: %RANDOM%
echo CD: %CD%

REM ==============================================================================
REM Control Flow
REM ==============================================================================

REM If statement
if "%NAME%"=="Alice" (
    echo Hello, Alice!
) else (
    echo Hello, stranger!
)

REM If with comparison operators
if %AGE% EQU 30 echo Age is 30
if %AGE% NEQ 25 echo Age is not 25
if %AGE% GTR 18 echo Adult
if %AGE% GEQ 18 echo 18 or older
if %AGE% LSS 65 echo Not retired
if %AGE% LEQ 100 echo Valid age

REM If exist
if exist "file.txt" (
    echo File exists
) else (
    echo File not found
)

REM If not
if not exist "missing.txt" echo File is missing

REM If defined
if defined NAME echo NAME is defined
if not defined UNDEFINED echo UNDEFINED is not defined

REM If errorlevel
if errorlevel 1 echo Error occurred
if %ERRORLEVEL% EQU 0 echo Success

REM If with string comparison
if /i "%NAME%"=="alice" echo Case insensitive match

REM ==============================================================================
REM Loops
REM ==============================================================================

REM For loop - iterate over set
for %%i in (1 2 3 4 5) do (
    echo Number: %%i
)

REM For loop - iterate over files
for %%f in (*.txt) do (
    echo File: %%f
)

REM For /L - numeric range
for /l %%i in (1,1,10) do (
    echo Count: %%i
)

REM For /L - step by 2
for /l %%i in (0,2,10) do (
    echo Even: %%i
)

REM For /L - countdown
for /l %%i in (10,-1,1) do (
    echo Countdown: %%i
)

REM For /D - directories only
for /d %%d in (*) do (
    echo Directory: %%d
)

REM For /R - recursive
for /r "C:\Temp" %%f in (*.txt) do (
    echo Found: %%f
)

REM For /F - parse file content
for /f "tokens=1,2 delims=," %%a in (data.csv) do (
    echo Column1: %%a, Column2: %%b
)

REM For /F - parse command output
for /f "tokens=*" %%i in ('dir /b *.txt') do (
    echo Found file: %%i
)

REM For /F - parse string
for /f "tokens=1-3 delims=-" %%a in ("2024-01-15") do (
    echo Year: %%a, Month: %%b, Day: %%c
)

REM For /F - skip lines and use eol
for /f "skip=1 eol=# tokens=*" %%i in (config.txt) do (
    echo Config: %%i
)

REM Nested loops
for %%i in (A B C) do (
    for %%j in (1 2 3) do (
        echo %%i%%j
    )
)

REM ==============================================================================
REM Labels and Goto
REM ==============================================================================

goto :main

:subroutine1
echo In subroutine 1
goto :eof

:subroutine2
echo In subroutine 2
goto :eof

:error_handler
echo Error occurred!
exit /b 1

:main
echo Starting main
call :subroutine1
call :subroutine2
echo Done

REM ==============================================================================
REM Functions (Subroutines)
REM ==============================================================================

REM Function definition
:greet
echo Hello, %~1!
goto :eof

REM Function with return value
:add
set /a result=%~1+%~2
goto :eof

REM Function with multiple parameters
:create_file
set filename=%~1
set content=%~2
echo %content% > %filename%
goto :eof

REM Calling functions
call :greet "World"
call :add 5 3
echo Result: %result%

REM Function with local variables
:local_demo
setlocal
set LOCAL_VAR=I'm local
echo %LOCAL_VAR%
endlocal
goto :eof

REM ==============================================================================
REM Input and Output
REM ==============================================================================

REM User input
set /p USER_INPUT=Enter something:
echo You entered: %USER_INPUT%

REM Echo variations
echo Normal echo
echo.
echo Above was a blank line
echo:This also works for blank lines
echo(And this too

REM Suppress echo
@echo This line's echo is suppressed

REM Output to file
echo Hello > output.txt
echo World >> output.txt

REM Redirect stderr
dir nonexistent 2> errors.txt
dir nonexistent 2>> errors.txt

REM Redirect both stdout and stderr
dir 2>&1 > all_output.txt

REM Null device
dir > nul 2>&1

REM Piping
type file.txt | find "pattern"
dir | sort | more

REM ==============================================================================
REM File and Directory Operations
REM ==============================================================================

REM Create directory
mkdir newdir
md "path with spaces"

REM Remove directory
rmdir /s /q olddir
rd /s /q "path with spaces"

REM Copy files
copy source.txt dest.txt
copy /y source.txt dest.txt
xcopy /s /e /i source dest

REM Move files
move source.txt dest.txt
move /y source.txt newdir\

REM Delete files
del file.txt
del /q /f *.tmp
erase oldfile.txt

REM Rename
ren oldname.txt newname.txt
rename "old file.txt" "new file.txt"

REM File attributes
attrib +r readonly.txt
attrib -r readonly.txt
attrib +h hidden.txt

REM ==============================================================================
REM Path Manipulation
REM ==============================================================================

set FILEPATH=C:\Users\Alice\Documents\file.txt

REM Extract drive
for %%i in (%FILEPATH%) do echo Drive: %%~di

REM Extract path
for %%i in (%FILEPATH%) do echo Path: %%~pi

REM Extract filename
for %%i in (%FILEPATH%) do echo Name: %%~ni

REM Extract extension
for %%i in (%FILEPATH%) do echo Ext: %%~xi

REM Full path
for %%i in (%FILEPATH%) do echo Full: %%~fi

REM File size
for %%i in (%FILEPATH%) do echo Size: %%~zi

REM File date/time
for %%i in (%FILEPATH%) do echo Time: %%~ti

REM File attributes
for %%i in (%FILEPATH%) do echo Attr: %%~ai

REM Combine modifiers
for %%i in (%FILEPATH%) do echo Drive+Path: %%~dpi

REM ==============================================================================
REM String Operations
REM ==============================================================================

set STR=Hello, World!

REM Length (workaround)
:strlen
setlocal EnableDelayedExpansion
set "s=%STR%"
set len=0
:strlen_loop
if defined s (
    set s=!s:~1!
    set /a len+=1
    goto :strlen_loop
)
echo Length: %len%
endlocal

REM Check if string contains substring
echo %STR% | find "World" > nul
if %ERRORLEVEL% EQU 0 echo Contains "World"

REM ==============================================================================
REM Date and Time
REM ==============================================================================

echo Current date: %DATE%
echo Current time: %TIME%

REM Parse date (depends on locale)
for /f "tokens=1-3 delims=/" %%a in ("%DATE%") do (
    set month=%%a
    set day=%%b
    set year=%%c
)

REM Parse time
for /f "tokens=1-3 delims=:." %%a in ("%TIME%") do (
    set hour=%%a
    set minute=%%b
    set second=%%c
)

REM Formatted timestamp
set TIMESTAMP=%DATE:~-4%%DATE:~-10,2%%DATE:~-7,2%_%TIME:~0,2%%TIME:~3,2%%TIME:~6,2%

REM ==============================================================================
REM Process Management
REM ==============================================================================

REM Start new process
start notepad.exe
start "" "C:\Program Files\App\app.exe"

REM Start minimized/maximized
start /min notepad.exe
start /max notepad.exe

REM Wait for process
start /wait notepad.exe

REM Run in new window
start cmd /c "echo Hello & pause"

REM Tasklist
tasklist | find "notepad"

REM Taskkill
taskkill /im notepad.exe /f
taskkill /pid 1234 /f

REM ==============================================================================
REM Network Commands
REM ==============================================================================

REM Ping
ping localhost -n 1 > nul 2>&1
if %ERRORLEVEL% EQU 0 echo Host is reachable

REM Check network share
net use Z: \\server\share
net use Z: /delete

REM ==============================================================================
REM Registry Operations
REM ==============================================================================

REM Query registry
reg query "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion" /v ProductName

REM Add registry value
reg add "HKEY_CURRENT_USER\Software\MyApp" /v Setting /t REG_SZ /d "Value" /f

REM Delete registry value
reg delete "HKEY_CURRENT_USER\Software\MyApp" /v Setting /f

REM ==============================================================================
REM Error Handling
REM ==============================================================================

REM Check errorlevel
dir nonexistent 2> nul
if %ERRORLEVEL% NEQ 0 (
    echo Command failed with error: %ERRORLEVEL%
    goto :error_handler
)

REM Using || for error handling
dir nonexistent 2> nul || echo Failed

REM Using && for success chaining
dir 2> nul && echo Success

REM Exit with error code
exit /b 1

REM ==============================================================================
REM Advanced Features
REM ==============================================================================

REM Delayed expansion example
setlocal EnableDelayedExpansion
set counter=0
for %%i in (a b c d e) do (
    set /a counter+=1
    echo !counter!: %%i
)
endlocal

REM Choice command
choice /c YN /m "Continue?"
if %ERRORLEVEL% EQU 1 echo You chose Yes
if %ERRORLEVEL% EQU 2 echo You chose No

REM Timeout
timeout /t 5 /nobreak > nul

REM Color
color 0A

REM Title
title My Batch Script

REM Window mode
mode con: cols=100 lines=50

REM ==============================================================================
REM Main Script
REM ==============================================================================

:main_script
echo ==== Batch Script Sample ====
echo.

REM Display variables
echo Name: %NAME%
echo Age: %AGE%

REM Arithmetic
set /a RESULT=5+3
echo 5 + 3 = %RESULT%

REM Loop example
echo Counting to 5:
for /l %%i in (1,1,5) do echo   %%i

REM Function call
call :greet "Batch User"

echo.
echo Done!

REM Clean exit
exit /b 0
