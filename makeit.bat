@echo off
rem ---
rem --- common install batch file for Meadow & NTEmacs
rem ---  1999/07/07, Masaki YATSU mailto:yatsu@aurora.dti.ne.jp
rem ---              cmail ML member
rem ---  modified 1999/12/01, Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem ---  modified 2000/12/26, Takeshi Morishima mailto:tm@interaccess.com
rem ---  date $Date: 2013/01/13 09:30:21 $
rem ---  version $Id: makeit.bat,v 1.5 2013/01/13 09:30:21 skk-cvs Exp $

set ELISPMK_APP=skk

rem --- Japanese Comments:
rem ---
rem --- 引数
rem ---   引数については make1.bat のコメントを参照してください．
rem ---   makeit.bat は、環境変数を設定した後に make1.bat を呼び出します。
rem ---
rem --- 変数設定
rem ---   このコメントのあとにある PREFIX, EMACS, EXEC_PREFIX, LISPDIR,
rem ---   INFODIR, VERSION_SPECIFIC_LISPDIR の各変数を，お使いの環境に
rem ---   適当に合せて設定してください．
rem ---   特に，EMACS の値を，
rem ---     Windows95/98 を利用されている方は meadow95.exe
rem ---     WindowsNT4.0 を利用されている方は meadownt.exe
rem ---     NTEmacs を利用されている方は emacs.exe
rem ---   を指定するのを忘れないように．
rem ---
rem ---   適宜指定が終った makeit.bat は下のいずれかのファイルとして
rem ---   コピーしておくとそちらを優先して実行します。(アップグレード
rem ---   の際に makeit.bat を再編集する必要がありません.) 優先順に:
rem ---
rem ---     1-1. %HOME%\.elispmk.%ELISPMK_APP%.bat
rem ---     1-2. %HOME%\elisp\elispmk.%ELISPMK_APP%.bat
rem ---     1-3. %HOME%\config\elispmk.%ELISPMK_APP%.bat
rem ---     1-4. c:\Program Files\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---     1-5. c:\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---     1-6. d:\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---
rem ---     2-1. %HOME%\.elispmk.bat
rem ---     2-2. %HOME%\elisp\elispmk.bat
rem ---     2-3. %HOME%\config\elispmk.bat
rem ---     2-4. c:\Program Files\Meadow\elispmk.bat
rem ---     2-5. c:\Meadow\elispmk.bat
rem ---     2-6. d:\Meadow\elispmk.bat
rem ---
rem ---   となります。
rem ---
rem --- English Comments:
rem ---
rem --- Arguments
rem ---   Please refer to comment section of make1.bat. Makeit.bat
rem ---   will perform installation procedure by executing make1.bat.
rem ---
rem --- Specifying variables
rem ---   After this comment section, PREFIX, EMACS, EXEC_PREFIX,
rem ---   LISPDIR, INFODIR, VERSION_SPECIFIC_LISPDIR is defined using
rem ---   'set' batch command. Please specify them appropriately
rem ---   according to your Emacs environment. Especially remember to set
rem ---   the EMACS variable to meadow95.exe if you use Meadow on
rem ---   Windows95/98, or to meadownt.exe if you use Meadow on
rem ---   WindowsNT4.0, or to emacs.exe if you use NTEmacs.
rem ---
rem ---   After modification, you may make a copy of makeit.bat as a pre-
rem ---   configured file as one of the following name. Any future
rem ---   execution of makeit.bat will automatically use this pre-
rem ---   configured batch file instead of makeit.bat itself. (When
rem ---   upgrading new distribution file for example, you do not have to
rem ---   make modification to makeit.bat again.) A pre-configured batch
rem ---   file is searched in order listed below:
rem ---
rem ---     1-1. %HOME%\.elispmk.%ELISPMK_APP%.bat
rem ---     1-2. %HOME%\elisp\elispmk.%ELISPMK_APP%.bat
rem ---     1-3. %HOME%\config\elispmk.%ELISPMK_APP%.bat
rem ---     1-4. c:\Program Files\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---     1-5. c:\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---     1-6. d:\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---
rem ---     2-1. %HOME%\.elispmk.bat
rem ---     2-2. %HOME%\elisp\elispmk.bat
rem ---     2-3. %HOME%\config\elispmk.bat
rem ---     2-4. c:\Program Files\Meadow\elispmk.bat
rem ---     2-5. c:\Meadow\elispmk.bat
rem ---     2-6. d:\Meadow\elispmk.bat

rem --- 変数設定の例 (Example of variable definition)
rem --- c:\usr\Meadow にインストールされている 1.10 の Meadow を使用
rem --- している場合の設定例. (An example of variable definition. In
rem --- this example, Meadow 1.10 installed in c:\usr\Meadow directory
rem --- is used.)
rem ---   set PREFIX=c:\usr\Meadow
rem ---   set EMACS=%PREFIX%\1.10\bin\meadow95.exe
rem ---   set EXEC_PREFIX=
rem ---   set LISPDIR=%PREFIX%\site-lisp
rem ---   set VERSION_SPECIFIC_LISPDIR=%PREFIX%\1.10\site-lisp
rem ---   set DEFAULT_MAKE_ARG=elc
rem --- 安全のためデフォルトの値はすべて空文字列になっています。お使い
rem --- のシステムにあわせてこれらの変数を指定してください。(To take a
rem --- safe side, default values are all set to null strings. Please
rem --- specify these variables accordingly for your system.)
rem --- なお、DEFAULT_MAKE_ARG に可能な値は make1.bat を御覧ください。
rem --- (Please see make1.bat for possible values of DEFAULT_MAKE_ARG.)

set PREFIX=
set EMACS=
set LISPDIR=
set DEFAULT_MAKE_ARG=


rem --- makeit.bat 内から呼ばれている場合は再帰呼び出しをせず make1 を実行
if not "%ELISPMK%"=="" goto execsubmk

rem ---
set ELISPMK=%HOME%\.elispmk.%ELISPMK_APP%.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=%HOME%\elisp\elispmk.%ELISPMK_APP%.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=%HOME%\config\elispmk.%ELISPMK_APP%.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK="c:\Program Files\Meadow\elispmk.%ELISPMK_APP%.bat"
if exist %ELISPMK% goto execelmkb
set ELISPMK=c:\Meadow\elispmk.%ELISPMK_APP%.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=d:\Meadow\elispmk.%ELISPMK_APP%.bat
if exist %ELISPMK% goto execelmkb
rem ---
set ELISPMK=%HOME%\.elispmk.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=%HOME%\elisp\elispmk.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=%HOME%\config\elispmk.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK="c:\Program Files\Meadow\elispmk.bat"
if exist %ELISPMK% goto execelmkb
set ELISPMK=c:\Meadow\elispmk.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=d:\Meadow\elispmk.bat
if exist %ELISPMK% goto execelmkb

echo ----
echo INFORMATIVE: No pre-configured batch (e.g. ~/.elispmk.bat
echo INFORMATIVE: or ~/.elispmk.%ELISPMK_APP%.bat) found.
echo INFORMATIVE: You may create one for your convenience.
echo INFORMATIVE: See comments in makeit.bat.
echo ----

:execsubmk
set ELISPMK=
rem --- %EMACS% が場合はエラー終了する
if "%EMACS%"=="" goto errnotspecified
if not exist "%EMACS%" goto errnonexistent

rem --- MAKE1.BAT Control
set SUBMAKEOK=OK

echo ----
echo Executing make1.bat in the current directory using the following env.
echo HOME=%HOME%
echo PREFIX=%PREFIX%
echo EMACS=%EMACS%
echo EXEC_PREFIX=%EXEC_PREFIX%
echo LISPDIR=%LISPDIR%
echo INFODIR=%INFODIR%
echo VERSION_SPECIFIC_LISPDIR=%VERSION_SPECIFIC_LISPDIR%
echo ----

set ARG=%1
if "%ARG%"=="" set ARG=%DEFAULT_MAKE_ARG%

echo Executing .\make1.bat with argument=%ARG%
.\make1.bat %ARG%

echo Error: for some reason .\make1.bat could not be executed.
echo Please check if .\make1.bat exists and correct.
goto pauseend

:execelmkb
echo ----
echo Found %ELISPMK%. Executing it...
echo ----
%ELISPMK% %1
echo Error: for some reason %ELISPMK% could not be executed.
echo Please check if ELISPMK=%ELISPMK% exists and correct.
goto printenv

rem --- %EMACS% が設定されていない
:errnotspecified
echo Error: Environment variable EMACS is not specified.
goto printenv

rem --- %EMACS% に設定されているファイルが存在しない
:errnonexistent
echo Error: EMACS=%EMACS% does not exist.

:printenv
echo ----
echo Check correctness of the following environment variables.
echo HOME=%HOME%
echo PREFIX=%PREFIX%
echo EMACS=%EMACS%
echo EXEC_PREFIX=%EXEC_PREFIX%
echo LISPDIR=%LISPDIR%
echo INFODIR=%INFODIR%
echo VERSION_SPECIFIC_LISPDIR=%VERSION_SPECIFIC_LISPDIR%
echo DEFAULT_MAKE_ARG=%DEFAULT_MAKE_ARG%
echo See comments in makeit.bat and make1.bat for setup instruction.
echo ----

:pauseend
echo Type any key when you're done reading the error message.
pause

rem --- end of makeit.bat
:end
