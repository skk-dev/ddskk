echo off
rem MAKE.BAT for SKK.
rem Copyright (C) 1999 Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem
rem Author: Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem Maintainer: Mikio Nakajima, mailto:minakaji@osaka.email.ne.jp
rem Version: $Id: make.bat,v 1.10 1999/10/23 04:42:19 minakaji Exp $
rem Created: March 23, 1999
rem Last Modified: $Date: 1999/10/23 04:42:19 $

rem ********************************************************************
rem *                                                                  *
rem * Please modify following five lines according to your environment *
rem *                                                                  *
rem ********************************************************************
set EMACS=c:\usr\meadow\1.10\bin\meadow95.exe
set PREFIX=c:\usr\meadow\1.10
set LISPDIR=c:\usr\meadow\site-lisp
rem ********************************************************************

%EMACS% -batch -q -no-site-file -l SKK-MK -f SKK-MK-install

