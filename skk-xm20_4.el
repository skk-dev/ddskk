;;; skk-xm20_4.el --- dummy file for XEmacs 20.4.
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-xm20_4.el,v 1.3 2000/12/01 09:15:48 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/12/01 09:15:48 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; Daredevil SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;;  XEmacs 20.4 dumps certain files of SKK 10.38.  This is because
;;  there was not SKK package when 10.38 was developing.  It requires
;;  skk-vars.el (obsolete name of skk-autoloads.el or
;;  auto-autoloads.el).
;;  This file is just dummy only for XEmacs 20.4.  If you make SKK for
;;  XEmacs 20.4, this file will be installed automatically.
;;
;;  Thanks to Naoki Wakamatsu <naoki-w@ht-net21.ne.jp> for giving me
;;  information in detail about XEmacs 20.4.

;;; Code
(or (and (boundp 'preloaded-file-list) (member "skk-leim" preloaded-file-list))
    (error "You don't have to load skk-xm20_4.el"))

;; reset autoloads.
(load "skk-autoloads")

;; reload new skk-leim.el.
(load "skk-leim")

(require 'product)
(product-provide (provide 'skk-xm20_4) (require 'skk-version))
;;; end of skk-xm20_4.el.
