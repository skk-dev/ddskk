echo off
rem MAKE.BAT for SKK.
rem Copyright (C) 1999 Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem
rem Author: Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem Maintainer: Mikio Nakajima, mailto:minakaji@osaka.email.ne.jp
rem Version: $Id: make.bat,v 1.12 1999/12/16 01:13:51 yutopia Exp $
rem Created: March 23, 1999
rem Last Modified: $Date: 1999/12/16 01:13:51 $

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

:install
%EMACS% -batch -q -no-site-file -l SKK-MK -f SKK-MK-install
goto end

:listing
%EMACS% -batch -q -no-site-file -l SKK-MK -f SKK-MK-what-where
goto end

:clean
del skk-autoloads.el *.elc doc\skk.info* *~'

:end

