@echo off

REM このスクリプトは Texinfo マニュアルの
REM   19. Formatting and Printing Hardcopy
REM     19.3 Format with tex/texindex
REM に記載された手順を実行するものです。
REM   skk.texi --(ptex)--> skk.dvi --(dvipdfmx)--> skk.pdf
REM サイズの小さい (約 824 Kbyte) PDF が完成しますが、
REM その pdf は参照 ( リファレンス, @ref{} ) が clickable ではありません。

ptex skk.texi

for %%f in (skk.??) do texindex %%f

ptex skk.texi

for %%f in (skk.??) do texindex %%f

ptex skk.texi
dvipdfmx skk.dvi

del skk.cp*  skk.fn*  skk.ky*  skk.pg*  skk.tp*  skk.vr*
del skk.aux  skk.dvi  skk.toc

