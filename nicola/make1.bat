echo off
rem MAKE1.BAT for SKK.
rem Copyright (C) 1999 Yuh Ohmura, mailto:yutopia@y6.dion.ne.jp
rem
rem Author: Yuh Ohmura, mailto:yutopia@y6.dion.ne.jp
rem Maintainer: SKK Development Team mailto:skk@ring.gr.jp
rem Created: March 23, 1999

rem ---  modified 2017/03/12, Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>

rem --- check calling from makeit.bat
if not "%SUBMAKEOK%"=="OK" goto prnusage
set SUBMAKEOK=

rem argument check

set arg1=%1

if "%arg1%"=="elc" goto compile
if "%arg1%"=="install" goto install
if "%arg1%"=="clean" goto clean

echo Unrecognized argument: specify either
echo   elc          : byte compile
echo   install      : install
echo   clean        : cleaning garbage file
goto pauseend

:compile
%EMACS% -batch -q -no-site-file -l NICOLA-DDSKK-MK -f compile-nicola-ddskk
goto end

:install
%EMACS% -batch -q -no-site-file -l NICOLA-DDSKK-MK -f install-nicola-ddskk
goto end

:clean
del *.elc auto-autoloads.el custom-load.el nicola-ddskk-autoloads.el

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
