@echo off
rem ---
rem --- cmail install batch file for Meadow & NTEmacs
rem ---  1999/07/07, Masaki YATSU mailto:yatsu@aurora.dti.ne.jp
rem ---              cmail ML member
rem ---  modified 1999/12/01, Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem ---  date $Date: 2001/01/16 02:17:20 $
rem ---  version $Id: make1.bat,v 1.1 2001/01/16 02:17:20 yutopia Exp $


rem --- 引数
rem ---   elc : バイトコンパイルのみ。
rem ---   all, install またはなし : インストール。
rem ---   elc-no-options : バイトコンパイルのみ(オプションなし)
rem ---   install-no-option : インストール(オプションなし)
rem ---   clean : お掃除
rem ---   what-where : インストール場所をサーチ
rem ---


rem --- 最初に make.bat から実行されているかチェックする
if not "%SUBMAKEOK%"=="OK" goto prnusage
set SUBMAKEOK=

rem --- 引数判定
set arg1=%1
if "%arg1%"=="" goto install
if "%arg1%"=="elc" goto compile
if "%arg1%"=="elc-no-options" goto compilenoopt
if "%arg1%"=="all" goto install
if "%arg1%"=="install" goto install
if "%arg1%"=="install-no-options" goto installnoopt
if "%arg1%"=="clean" goto clean
if "%arg1%"=="what-where" goto listing
echo Unrecognized argument: specify either 'elc', 'elc-no-options', 'all',
echo 'install', 'install-no-options', 'clean' or 'what-where'. Default is
echo 'install' (when no argument is specified.)
goto end

rem --- インストール
:install
%EMACS% -batch -q -no-site-file -l CMAIL-MK -f install-cmail %PREFIX% NONE %LISPDIR% %INFODIR% %VERSION_SPECIFIC_LISPDIR%
goto end

rem --- インストール (options を除外)
:installnoopt
%EMACS% -batch -q -no-site-file -l CMAIL-MK -f install-cmail-no-options %PREFIX% NONE %LISPDIR% %INFODIR% %VERSION_SPECIFIC_LISPDIR%
goto end

rem --- *.elcの作成
:compile
%EMACS%  -batch -q -no-site-file -l CMAIL-MK -f compile-cmail %PREFIX% NONE %LISPDIR% %INFODIR% %VERSION_SPECIFIC_LISPDIR%
goto end

rem --- *.elcの作成 (options を除外)
:compilenoopt
%EMACS%  -batch -q -no-site-file -l CMAIL-MK -f compile-cmail-no-options %PREFIX% NONE %LISPDIR% %INFODIR% %VERSION_SPECIFIC_LISPDIR%
goto end

rem --- 掃除
:clean
del *.elc
del doc\*.info
del *~
del apel\*~
del alel\*.elc
del doc\*~
rem del icon\*~
goto end

rem --- インストール先のリスト
:listing
%EMACS% -batch -q -no-site-file -l CMAIL-MK -f what-where-cmail %PREFIX% NONE %LISPDIR% %INFODIR% %VERSION_SPECIFIC_LISPDIR%
goto end

rem --- このファイルは単体で実行するものではない旨表示する
:prnusage
echo This file should not be executed by itself. Use make.bat.

:end
