;;; skk-version.el --- version information for SKK

;; Copyright (C) 2000, 2001, 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-version.el,v 1.39 2010/09/10 14:02:41 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2010/09/10 14:02:41 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(put 'skk-version 'product-name "Daredevil SKK")
(put 'skk-version 'version-string "14.1.50")
(put 'skk-version 'codename "Mikuni")

;;;###autoload
(defun skk-version (&optional without-codename)
  "Return SKK version with its codename.
If WITHOUT-CODENAME is non-nil, simply return SKK version without
the codename."
  (interactive "P")
  (if (interactive-p)
      (message "%s" (skk-version without-codename))
    (if without-codename
	(format "%s/%s"
		(get 'skk-version 'product-name)
		(get 'skk-version 'version-string))
      (format "%s/%s (%s)"
	      (get 'skk-version 'product-name)
	      (get 'skk-version 'version-string)
	      (get 'skk-version 'codename)))))

;;; 
(defun skk-startup-screen-message ()
  ""
  (insert "\n"
	  (format "%s is available for use. move to the `KANA mode' typing %s"
		  (skk-version)
		  (mapconcat '(lambda (x)
				(format "`%s'"
					(mapconcat 'key-description
						   (list x)
						   "")))
			     (where-is-internal 'skk-mode)
			     " or "))))

;;;###autoload
(defun skk-startup-screen ()
  ;; ~/.emacs に (skk-startup-screen) と追記してください。
  ;; TODO
  ;;  1) skk-setup.el(.in) への組み込みも試しましたが、
  ;;     cannot open load file: advice とエラーが発生して
  ;;     emacs が起動すらしませんでした。
  ;;  2) GNU Emacs 23 でのみテストしています。
  "Emacs 起動時の startup screen に skk のバージョンを追加表示する。"
  (if (fboundp 'skk-mode)
      (progn
	;; for GUI
	(defadvice fancy-startup-tail (after insert-skk-version activate)
	  "docstring."
	  (skk-startup-screen-message))
	;; for TTY
	(defadvice normal-mouse-startup-screen (after insert-skk-version activate)
	  "docstring."
	  (skk-startup-screen-message)))))

;;;

(provide 'skk-version)

;;; skk-version.el ends here
