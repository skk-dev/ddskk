@echo off

ptex skk.texi

for %%f in (skk.??) do texindex %%f

ptex skk.texi

for %%f in (skk.??) do texindex %%f

ptex skk.texi
dvipdfmx skk.dvi

del skk.cp*  skk.fn*  skk.ky*  skk.pg*  skk.tp*  skk.vr*
del skk.aux  skk.dvi  skk.toc

