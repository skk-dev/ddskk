;;; skk-vars.el --- dummy file for XEmacs 20.4.
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-vars.el,v 1.1 1999/09/17 11:13:01 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/09/17 11:13:01 $

;; This file is not part of SKK yet.

;; SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
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

;;; Code:
(or (and (boundp 'preloaded-file-list) (member "skk-leim" preloaded-file-list))
    (error "You don't have to load skk-vars.el") )

(provide 'skk-vars)
;;; skk-vars.el ends here
