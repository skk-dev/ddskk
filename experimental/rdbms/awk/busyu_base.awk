# busyu_base.awk -- 
#
# Copyright (C) 1998 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
#
# Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
# Created: Sep 1, 1998
# Last Modified: $Date: 2001/02/03 00:22:59 $
# Version: $Id: busyu_base.awk,v 1.2 2001/02/03 00:22:59 minakaji Exp $
#
# This file is not part of SKK yet.
#
# SKK is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either versions 2, or (at your option)
# any later version.
#
# SKK is distributed in the hope that it will be useful
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with SKK, see the file COPYING.  If not, write to the Free
# Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.
#
# Commentary:
# The format of output file is (delimiter is TAB);
#
#        busyuID	kanji	yomi1	yomi2	yomi3	yomin...
#
# and how to convert;
#
#        % gawk -f busyu.awk kanjidic | sort -n > temp4.txt
#
# Code

/^[^#]/{
if (match($0, /T2 [ ¤¡-¤ó]+/) != 0) {
  temp = substr($0, RSTART + 4, RLENGTH - 4);
  entries = split(temp, busyu, " ");

  if (match($0, /B[0-9]+/) != 0) {
    busyuID = substr($0, RSTART + 1, RLENGTH - 1);
    printf("%s\t", busyuID); # busyuID
  } else {
    printf("\t");
  }

  if (match($0, /C[0-9]+/) != 0) {
    busyuID2 = substr($0, RSTART + 1, RLENGTH - 1);
    printf("%s\t", busyuID2); # classic busyuID
  } else {
    printf("\t");
  }

  printf("%s", $1); # kanji

  for (i = 1; i <= entries; i++)
    printf("\t%s", busyu[i]); # yomi1, yomi2, yomiN...

  printf("\n");
}
}
# end of busyu_base.awk
