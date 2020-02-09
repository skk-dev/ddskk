#!/bin/sh

# このスクリプトは Texinfo マニュアルの
#   19. Formatting and Printing Hardcopy
#     19.3 Format with tex/texindex
# に記載された手順を実行するものです。
#   skk.texi --(ptex)--> skk.dvi --(dvipdfmx)--> skk.pdf
# サイズの小さい (約 824 Kbyte) PDF が完成しますが、
# その pdf は参照 ( リファレンス, @ref{} ) が clickable ではありません。

set -e

ptex skk.texi
for f in skk.??; do texindex "$f"; done
ptex skk.texi
for f in skk.??; do texindex "$f"; done
ptex skk.texi
dvipdfmx skk.dvi
/bin/rm -f skk.cp* skk.fn* skk.ky* skk.pg* skk.tp* skk.vr*
/bin/rm -f skk.aux skk.dvi skk.toc skk.log
