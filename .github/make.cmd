@rem DISCLAIMER:
@rem  Using this is completely optional.
@rem  Instead, it's usually better to create a well-written Makefile for *NIX systems,
@rem  tailored to the needs of your implementation, but based on the template in LANG_TEMPLATE

@rem
@rem Static part, don't change these:
@rem

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
	docker run -v %cd%:/deez -t deez_%curdir% || goto err
	goto exit

@rem
@rem Update those:
@rem

:"clean"
	echo "===> Cleaning"
	@rem TODO: replace this line with your cleaning commands

	rem But DON'T replace this one:
	goto exit

:"fmt"
	echo "===> Formatting"
	@rem TODO: replace this line with your formatting commands

	rem But DON'T replace this one:
	if %ready%=="yes" goto "lint" else goto exit

:"lint"
	echo "===> Linting"
	@rem TODO: replace this line with your linting commands
	
	rem But DON'T replace this one:
	if %ready%=="yes" goto "test" else goto exit

:"test"
	echo "===> Testing"
	@rem TODO: replace this line with your testing commands

	rem But DON'T replace this one:
	goto exit

