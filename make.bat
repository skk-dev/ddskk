echo off
rem MAKE.BAT for SKK.
rem Copyright (C) 1999 Yuh Ohmura <yutopia@t3.rim.or.jp>
rem
rem Author: Yuh Ohmura <yutopia@t3.rim.or.jp>
rem Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
rem Version: $Id: make.bat,v 1.1 1999/08/19 12:56:00 minakaji Exp $
rem Created: March 23, 1999
rem Last Modified: $Date: 1999/08/19 12:56:00 $

rem ********************************************************************
rem *                                                                  *
rem * Please modify following five lines according to your environment *
rem *                                                                  *
rem ********************************************************************
set EMACS=c:\usr\meadow\1.00\bin\meadow.exe
set PREFIX=NONE
set LISPDIR=c:\usr\meadow\site-lisp
set PACKAGEDIR=NONE
set V_S_LISPDIR=NONE
rem ********************************************************************

rem *********************************************
rem set FLAGS="-batch -q -no-site-file -l SKK-MK"
rem *********************************************

%EMACS% -batch -q -no-site-file -l SKK-MK -f install-skk %PREFIX% %LISPDIR% %V_S_LISPDIR%
rem copy default.el %LISPDIR%

