;;; skk-jisx0213.el --- SKK 用 JISX0213 文字コード関連プログラム

;; Copyright (C) 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-jisx0213.el,v 1.8 2001/11/19 15:54:01 czkmt Exp $
;; Keywords: japanese, mule, input method
;; Created: Sep. 30, 2000.
;; Last Modified: $Date: 2001/11/19 15:54:01 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either versions 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))
(require 'jisx0213)
;;(require 'un-define)

;;;###autoload
(defun skk-jisx0213-henkan-list-filter ()
  ;; remove candidate that contains jisx0213 characters from
  ;; SKK-HENKAN-LIST.
  (let ((henkan-list (nthcdr skk-henkan-count skk-henkan-list))
	e charset)
    (while (setq e (car henkan-list))
      (setq charset (find-charset-string (if (consp e) (cdr e) e)))
      (if (or (memq 'japanese-jisx0213-1 charset)
	      (memq 'japanese-jisx0213-2 charset))
	  (setq skk-henkan-list (delq e skk-henkan-list)
		henkan-list (delq e henkan-list))
	(setq henkan-list (cdr henkan-list))))))

(require 'product)
(product-provide
    (provide 'skk-jisx0213)
  (require 'skk-version))

;;; skk-jisx0213.el ends here
