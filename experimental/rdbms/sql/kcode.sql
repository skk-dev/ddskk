-- kcode.sql
--
-- Copyright (C) 1998 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
--
-- Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
-- Created: Sep 1, 1998
-- Last Modified: $Date: 2001/02/03 00:23:00 $
-- Version: $Id: kcode.sql,v 1.2 2001/02/03 00:23:00 minakaji Exp $
--
-- This file is not part of SKK yet.
--
-- SKK is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either versions 2, or (at your option)
-- any later version.
--
-- SKK is distributed in the hope that it will be useful
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with SKK, see the file COPYING.  If not, write to the Free
-- Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
-- MA 02111-1307, USA.
--
-- Commentary:
--
-- Code

DROP TABLE kcode;
CREATE TABLE kcode (
	kanji varchar(2) NOT NULL PRIMARY KEY,
	JIScode char(4) NOT NULL UNIQUE,
        Unicode char(4) NOT NULL UNIQUE
	);
COPY kcode FROM '/usr/local/share/emacs/site-lisp/skk/tmp/kcode.txt';
VACUUM kcode;
SELECT COUNT(*) FROM kcode;

-- end of kcode.sql
