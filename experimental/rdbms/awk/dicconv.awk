# dicconv.awk -- SKK 辞書をリレーショナルデータベースのテーブルに登録しやすい形に変換する awk スクリプト。
#
# Copyright (C) 1998 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
#
# Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
# Created: Apr 18, 1998
# Last Modified: $Date: 2001/02/03 00:22:59 $
# Version: $Id: dicconv.awk,v 1.2 2001/02/03 00:22:59 minakaji Exp $
# This file is not part of SKK yet.

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
# SKK 辞書をリレーショナルデータベースのテーブルに登録しやすい形に変換する awk スクリプト。
#
# yomi /kouho1/kouho2/.../kouhoN/
# ->
#
# yomi kouho1 okuriari
# yomi kouho2 okuriari
# ...
# yomi kouhoN okuriari
#
# OKURIARI は 0 か 1。
#

# Code
BEGIN {
  okuriari = 1;
  ctime = myctime(0);
  private = 1;
}
{
  if (NR == 1) {
    if (((match(FILENAME, "SKK-JISYO.L") != 0) ||
	 (match(FILENAME, "SKK-JISYO.M") != 0) ||
	 (match(FILENAME, "SKK-JISYO.S") != 0) ||
	 (match(FILENAME, "SKK-JISYO.JIS2") != 0) ) ||

	((match($0, /^;; Kakutei dictionary for SKK system/) != 0) ||
	 (match($0, /^;; Initial search dictionary for SKK system/) != 0) ||
	 (match($0, /^;; JIS LEVEL 2 Kanji dictionary for SKK system/) != 0) ||
	 (match($0, /^;; Large size dictionary for SKK system/) != 0) ||
	 (match($0, /^;; Medium size dictionary for SKK system/) != 0) ||
	 (match($0, /^;; Small size dictionary for SKK system/) != 0) )) {
      private = 0;
    }
  }
  else if (match($0, /^;; okuri-ari entries\.$/) != 0)
    okuriari = 1;
  else if (match($0, /^;; okuri-nasi entries\.$/) != 0)
    okuriari = 0;
  else if (match($0, /^;;/) != 0)
      ; # Comment line.  do nothing.
  else if (match($0, /^[^ ][^ ]* \//) != 0) {
    yomi = substr($0, 1, RLENGTH - 2);
    temp = substr($0, RLENGTH + 1);
    if (okuriari && match($0, /\/\[/) != 0) { # okuriari && okuri strictly
      temp = substr($0, RSTART, length($0) - 3);
      fn1 = split(temp, entry, "/]");
      for (i = 1; i <= fn1; i++) {
	fn2 = split(substr(entry[i], 3), parts, "/");
	for (j = 2; j <= fn2; j++) {
	  # why "\\\\\\" instead of "\\\\"?
	  gsub(/\\/, "\\\\\\", yomi);
	  gsub(/\\/, "\\\\\\", parts[j]);
	  printf("1\t%s\t%s\t%s", yomi, parts[j], parts[1]);
	  if (private) printf("\t%s", ctime);
	  printf("\n");
	}
      }
    }
    else {
      kouhos = substr(temp, 1, length(temp) - 1);
      fn1 = split(kouhos, kouho, "/");
      for (i = 1; i <= fn1; i++) {
	if (okuriari)
	  printf("1");
	else printf("0");
	gsub(/\\/, "\\\\\\", yomi);
	gsub(/\\/, "\\\\\\", kouho[i]);
	printf("\t%s\t%s", yomi, kouho[i]);
	if (private) printf("\t\\N\t%s", ctime); #`\N' is null string representation.
	printf("\n");
      }
    }
  }
}

function myctime(ts,    format) {
  format = "%a %b %e %H:%M:%S %Y";
  if (ts == 0)
    ts = systime();         # use current time as default
  return strftime(format, ts);
}

# end of dicconv.awk
