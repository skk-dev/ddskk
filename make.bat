echo off
rem MAKE.BAT for SKK.
rem Copyright (C) 1999 Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem
rem Author: Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem Maintainer: Mikio Nakajima, mailto:minakaji@osaka.email.ne.jp
rem Version: $Id: make.bat,v 1.6 1999/09/16 14:06:41 minakaji Exp $
rem Created: March 23, 1999
rem Last Modified: $Date: 1999/09/16 14:06:41 $

rem ********************************************************************
rem *                                                                  *
rem * Please modify following five lines according to your environment *
rem *                                                                  *
rem ********************************************************************
set EMACS=c:\usr\meadow\1.00\bin\meadow95.exe
set PREFIX=NONE
set LISPDIR=c:\usr\meadow\site-lisp
set PACKAGEDIR=NONE
set V_S_LISPDIR=NONE
rem ********************************************************************

%EMACS% -batch -q -no-site-file -l SKK-MK -f SKK-MK-install %PREFIX% %LISPDIR% %V_S_LISPDIR%

