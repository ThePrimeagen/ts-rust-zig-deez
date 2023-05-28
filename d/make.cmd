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
	@rem docker build --no-cache . -t deez_%curdir% || goto err
	@goto exit

:"docker-ready"
	@for %%d in ("%CD%") do @set curdir=%%~nxd
	@rem docker run -v %cd%:/deez -t deez_%curdir% || goto err
	@goto exit

@rem
@rem Update those:
@rem

:"fmt"
	@echo "===> Formatting"
	dfmt -i source/*
	@if %ready%=="yes" goto "lint" else goto exit

:"lint"
	@echo "===> Linting"
	dscanner --syntaxCheck source/*
	dscanner --styleCheck source/*
	@if %ready%=="yes" goto "test" else goto exit

:"test"
	@echo "===> Testing"
	dub test
	@goto exit

