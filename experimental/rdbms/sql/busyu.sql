-- busyu.sql
--
-- Copyright (C) 1998 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
--
-- Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
-- Created: Sep 1, 1998
-- Last Modified: $Date: 2001/02/03 00:23:00 $
-- Version: $Id: busyu.sql,v 1.2 2001/02/03 00:23:00 minakaji Exp $
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

DROP TABLE busyu_base;
CREATE TABLE busyu_base (
	busyuID int2 NOT NULL, -- radical (Busyu) number.  why aren't they unique?
	busyuID2 int2, -- classic radical number
	kanji varchar(2), -- 部首のみの標記が不能な文字もあるので、NOT NULL, UNIQUE 条件を付けない。
	yomi1 varchar(16) NOT NULL,
	yomi2 varchar(16),
	yomi3 varchar(16),
	yomi4 varchar(16) 
	);
COPY busyu_base FROM '/usr/local/share/emacs/site-lisp/skk/tmp/busyu_base.txt';
CREATE INDEX busyuID_index ON busyu_base USING BTREE (busyuID int2_ops);
CREATE INDEX yomi1_index ON busyu_base USING BTREE (yomi1 varchar_ops);
CREATE INDEX yomi2_index ON busyu_base USING BTREE (yomi2 varchar_ops);
CREATE INDEX yomi3_index ON busyu_base USING BTREE (yomi3 varchar_ops);
CREATE INDEX yomi4_index ON busyu_base USING BTREE (yomi4 varchar_ops);

DROP TABLE busyu_data;
CREATE TABLE busyu_data (
	busyuID int2 NOT NULL, -- radical (Busyu) number.  why aren't they unique?
	busyuID2 int2, -- classic radical number
	kanji varchar(2) NOT NULL UNIQUE
	);
COPY busyu_data FROM '/usr/local/share/emacs/site-lisp/skk/tmp/busyu_data.txt';
CREATE INDEX busyuID_index ON busyu USING BTREE (busyuID int2_ops);

VACUUM busyu_base;
VACUUM busyu_data;
SELECT COUNT(*) FROM busyu_base;
SELECT COUNT(*) FROM busyu_data;


-- end of busyu.sql
