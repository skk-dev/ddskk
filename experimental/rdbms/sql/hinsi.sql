-- hinsi.sql
--
-- Copyright (C) 1998 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
--
-- Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
-- Created: Sep 6, 1998
-- Last Modified: $Date: 2001/02/03 00:23:00 $
-- Version: $Id: hinsi.sql,v 1.2 2001/02/03 00:23:00 minakaji Exp $
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

DROP TABLE hinsi; -- old name
DROP TABLE hinsi_base;
CREATE TABLE hinsi_base (
	hinsi varchar(23) NOT NULL, -- longest is `サ行(する)&名詞化接尾語'
	hinsiID int2 NOT NULL,
	hinsiID2 int2
);

COPY hinsi_base FROM '/usr/local/share/emacs/site-lisp/skk/tmp/hinsi_base.txt';
CREATE INDEX hinsiB_hinsi_index ON hinsi_base USING BTREE (hinsi varchar_ops);
CREATE INDEX hinsiB_hinsiID_index ON hinsi_base USING BTREE (hinsiID int2_ops);
CREATE INDEX hinsiB_hinsiID2_index ON hinsi_base USING BTREE (hinsiID2 int2_ops);
VACUUM hinsi_base;
SELECT COUNT(*) FROM hinsi_base;

DROP TABLE hinsi_data;
CREATE TABLE hinsi_data (
	yomi varchar(26) NOT NULL, -- longest is `ばっくぷろぱげーしょんほう'
	kanji varchar(24) NOT NULL, -- longest is `オペレーティングシステム'
	hinsiID int2 NOT NULL,
	hinsiID2 int2
);
COPY hinsi_data FROM '/usr/local/share/emacs/site-lisp/skk/tmp/hinsi_data.txt';
CREATE INDEX hinsiD_yomi_index ON hinsi_data USING BTREE (yomi varchar_ops);
CREATE INDEX hinsiD_hinsiID_index ON hinsi_data USING BTREE (hinsiID int2_ops);
CREATE INDEX hinsiD_hinsiID2_index ON hinsi_data USING BTREE (hinsiID2 int2_ops);
VACUUM hinsi_data;
SELECT COUNT(*) FROM hinsi_data;

-- end of hinsi.sql
