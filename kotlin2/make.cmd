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
	call .\gradlew.bat ktlintFormat
	if %ready%=="yes" goto "lint" else goto exit

:"lint"
	echo ^=^=^=^> Linting
	call .\gradlew.bat ktlintCheck
	if %ready%=="yes" goto "test" else goto exit

:"test"
	echo ^=^=^=^> Testing
	call .\gradlew.bat :monkey-common:allTests
	goto exit

:"repl"
	echo ^=^=^=^> Running REPL
	call .\gradlew.bat :monkey-jvm:run --quiet --console=plain
	goto exit

:"repl-browser":
	echo ^=^=^=^> Running REPL (JS in browser)
	call .\gradlew.bat :monkey-js:jsBrowserRun
	goto exit

:"repl-browser-dev":
    echo ^=^=^=^> Running REPL (JS in browser) [Development mode with hot reload]
    call .\gradlew.bat :monkey-js:jsBrowserDevelopmentRun --continuous
    goto exit