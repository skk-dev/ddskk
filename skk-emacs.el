;;; skk-emacs.el --- GNU Emacs support for SKK -*- coding: iso-2022-jp -*-

;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method

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

;; skk-kcode.el より。
;; XEmacs でのエラー回避のためにこの関数を一時退避している。
;; 2面
;;;###autoload
(defun skk-jis2sjis2 (char1 char2)
  (let* ((ch2 (if (eq (* (/ char1 2) 2) char1)
		  (+ char2 125) (+ char2 31)))
	 (c2 (if (>= ch2 127)
		 (+ ch2 1) ch2))
         (ku (- char1 32))
         (c1 (if (<= ku 15)
		 (- (/ (+ ku ?\x1df) 2) (* (/ ku 8) 3))
	       (/ (+ ku ?\x19b) 2))))
    (list c1 c2)))

(provide 'skk-emacs)

;;; skk-emacs.el ends here
