echo off
rem MAKE.BAT for SKK.
rem Copyright (C) 1999 Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem
rem Author: Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem Maintainer: Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem Version: $Id: make.bat,v 1.15 2000/09/21 10:46:36 akiho Exp $
rem Created: March 23, 1999
rem Last Modified: $Date: 2000/09/21 10:46:36 $

rem ********************************************************************
rem *                                                                  *
rem * Please modify following five lines according to your environment *
rem *   If you use Meadow and Windows NT, use meadowNT.exe insted      *
rem *  of meadow95.exe                                                 *
rem *                                                                  *
rem ********************************************************************
set EMACS=c:\usr\meadow\1.10\bin\meadow95.exe
set PREFIX=c:\usr\meadow\1.10
set LISPDIR=c:\usr\meadow\site-lisp
rem ********************************************************************

set arg1=%1

if "%arg1%"=="install" goto install
if "%arg1%"=="what-where" goto listing
if "%arg1%"=="clean" goto clean
if "%arg1%"=="info-mule-for-win" goto info

:install
%EMACS% -batch -q -no-site-file -l SKK-MK -f SKK-MK-install
goto end

:listing
%EMACS% -batch -q -no-site-file -l SKK-MK -f SKK-MK-what-where
goto end

:info
%EMACS% -q -no-site-file -l SKK-MK -f SKK-MK-compile-info-for-mule4win32
goto end


:clean
del skk-autoloads.el 
del *.elc 
del doc\skk.info* 
del *~

:end

