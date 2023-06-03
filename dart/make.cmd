@rem
@rem Static part, don't change these:
@rem

@setlocal
@set "PROMPT=$$ "
@call :"%~1"
@endlocal

:exit
	@exit

:errexit
	@echo ERROR %ERRORLEVEL%
	@exit /b %ERRORLEVEL%

:""
	@goto "help"

:"help"
	@findstr /r /c:"^:\"" make.cmd
	@goto exit

:"ready"
	@set ready=yes
	@goto "fmt"

:"docker-build"
	@for %%d in ("%CD%") do @set curdir=%%~nxd
	docker build --no-cache . -t deez_dart
	@goto exit

:"docker-ready"
	@for %%d in ("%CD%") do @set curdir=%%~nxd
	docker run -v %cd%:/deez -t deez_dart
	@goto exit

@rem
@rem Update those:
@rem

:"fmt"
	@echo "===> Formatting"
	dart format .
	@if %ready%=="yes" goto "lint" else goto exit

:"lint"
	@echo "===> Linting"
	dart analyze .
	@if %ready%=="yes" goto "test" else goto exit

:"test"
	@echo "===> Testing"
	dart test
	@goto exit

