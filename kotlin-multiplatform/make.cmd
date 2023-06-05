@echo off
setlocal
set "PROMPT=$$ "
call :"%~1"
endlocal

:exit
	exit

:errexit
	echo ERROR %ERRORLEVEL%
	exit /b %ERRORLEVEL%

:""
	goto "help"

:"help"
	findstr /r /c:"^:\"" make.cmd
	goto exit

:"ready"
	set ready=yes
	goto "fmt"

:"docker-build"
	for %%d in ("%CD%") do set curdir=%%~nxd
	docker build --no-cache . -t deez_%curdir% || goto err
	goto exit

:"docker-ready"
	for %%d in ("%CD%") do set curdir=%%~nxd
	docker run -v %cd%:/app -t deez_%curdir% || goto err
	goto exit

:"clean"
	echo ^=^=^=^> Cleaning
	call .\gradlew.bat clean
	goto exit

:"fmt"
	echo ^=^=^=^> Formatting
	call .\gradlew.bat formatKotlin
	if %ready%=="yes" goto "lint" else goto exit

:"lint"
	echo ^=^=^=^> Linting
	call .\gradlew.bat lintKotlin
	if %ready%=="yes" goto "test" else goto exit

:"test"
	echo ^=^=^=^> Testing EVERYTHING
	set test_all=yes
	goto "test-lexer"

:"test-lexer"
	echo ^=^=^=^> Testing lexer
	call .\gradlew.bat test --tests dev.hermannm.monkeylang.LexerTest
	if %test_all%=="yes" goto "test-ast" else goto exit

:"test-ast"
	echo ^=^=^=^> Testing AST
	echo [Not yet implemented]
	if %test_all%=="yes" goto "test-parser" else goto exit

:"test-parser"
	echo ^=^=^=^> Testing parser
	echo [Not yet implemented]
	goto exit

:"repl"
	echo ^=^=^=^> Running REPL
	call .\gradlew.bat :jvm:run --quiet --console=plain
	goto exit

:"repl-browser":
	echo ^=^=^=^> Running REPL (Kotlin-JS in browser)
	call .\gradlew.bat :web:jsBrowserRun
	goto exit

:"repl-browser-dev":
	echo ^=^=^=^> Running REPL (Kotlin-JS in browser) [Development mode with hot reload]
	call .\gradlew.bat :web:jsBrowserDevelopmentRun --continuous
	goto exit
