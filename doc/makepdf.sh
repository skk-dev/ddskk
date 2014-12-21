#!/bin/sh
set -e

ptex skk.texi
for f in skk.??; do texindex "$f"; done
ptex skk.texi
for f in skk.??; do texindex "$f"; done
ptex skk.texi
dvipdfmx skk.dvi
/bin/rm -f skk.cp* skk.fn* skk.ky* skk.pg* skk.tp* skk.vr*
/bin/rm -f skk.aux skk.dvi skk.toc skk.log
