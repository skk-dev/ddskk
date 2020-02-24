rem MAKE1.BAT for SKK.
rem Copyright (C) 1999 Yuh Ohmura, mailto:yutopia@y6.dion.ne.jp
rem
rem Author: Yuh Ohmura, mailto:yutopia@y6.dion.ne.jp
rem Maintainer: SKK Development Team mailto:skk@ring.gr.jp
rem Created: March 23, 1999

rem --- check calling from makeit.bat
if not "%SUBMAKEOK%"=="OK" goto prnusage
set SUBMAKEOK=

rem argument check

set arg1=%1

if "%arg1%"=="elc" goto compile
if "%arg1%"=="all" goto install
if "%arg1%"=="install" goto install
if "%arg1%"=="info" goto info
if "%arg1%"=="install-info" goto installinfo
if "%arg1%"=="uninstall" goto uninstall
if "%arg1%"=="what-where" goto listing
if "%arg1%"=="clean" goto clean
if "%arg1%"=="test" goto test
if "%arg1%"=="get" goto get
echo Unrecognized argument: specify either
echo   elc          : byte compile
echo   all, install : install
echo   info         : generate info file
echo   install-info : install info file
echo   uninstall    : uninstall
echo   what-where   : print where to install
echo   clean        : cleaning garbage file
echo   test         : load test/all-tests.el and execute ert-run-tests-batch-and-exit
echo   get          : download jisyo files from skk-dev.github.io/dict/
goto pauseend

:compile
%EMACS% --batch --no-init-file --quick --load SKK-MK --funcall SKK-MK-generate-autoloads-el
FOR %%f IN (*.el) do (
  echo %%f
  %EMACS% --batch --no-init-file --quick --directory ./ --funcall batch-byte-compile %%f
)
goto end

:install
%EMACS% --batch --no-init-file --quick --load SKK-MK --funcall SKK-MK-install
goto end

:info
%EMACS% --batch --no-init-file --quick --load SKK-MK --funcall SKK-MK-compile-info
goto end

:installinfo
%EMACS% --batch --no-init-file --quick --load SKK-MK --funcall SKK-MK-install-info
goto end

:uninstall
%EMACS% --batch --no-init-file --quick --load SKK-MK --funcall SKK-MK-uninstall
goto end

:listing
%EMACS% --batch --no-init-file --quick --load SKK-MK --funcall SKK-MK-what-where
goto end

:clean
del leim-list.el skk-autoloads.el skk-setup.el auto-autoloads.el custom-load.el *.elc doc\skk.info* *~
goto end

:test
%EMACS% -batch -Q -L . -L test -l test/all-tests.el -f ert-run-tests-batch-and-exit
goto end

:get
%EMACS% --batch --no-init-file --quick --load SKK-MK --funcall SKK-MK-generate-autoloads-el
%EMACS% --batch --no-init-file --quick --directory ./ --load tar-util.el --load skk-develop.el --eval "(skk-get \"./dic\")"
goto end

rem --- This file should not be executed by itself. Use makeit.bat.
:prnusage
echo This file should not be executed by itself. Use makeit.bat.

rem --- If error occurs, stay display until any key is typed.
:pauseend
echo Type any key when you're done reading the error message.
pause

:end

