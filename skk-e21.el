;;; skk-e21.el -- GNU Emacs 21 support for SKK.
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; Daredevil SKK, see the file COPYING.  If not, write to the Free Software
;; Foundation Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (autoload 'Info-goto-node "info")
  (autoload 'browse-url "browse-url"))

;; Variables.
(defvar skk-e21-modeline-menu-items
  '("Daredevil SKK Menu"
    ["Hiragana"
     (call-interactively
      (lambda ()
	(interactive)
	(skk-j-mode-on)
	(when skk-use-color-cursor
	  (set-buffer-local-cursor-color (skk-cursor-current-color)))))
     :selected (and skk-j-mode (not skk-katakana))
     :style radio
     :key-sequence nil]
    ["Katakana"
     (call-interactively
      (lambda ()
	(interactive)
	(skk-j-mode-on t)
	(when skk-use-color-cursor
	  (set-buffer-local-cursor-color (skk-cursor-current-color)))))
     :selected (and skk-j-mode skk-katakana)
     :style radio
     :key-sequence nil]
    ["Hankaku alphabet"
     skk-latin-mode
     :selected skk-latin-mode
     :style radio
     :key-sequence nil]
    ["Zenkaku alphabet"
     skk-jisx0208-latin-mode
     :selected skk-jisx0208-latin-mode
     :style radio
     :key-sequence nil]
    "--"
    ["Read Manual" skk-e21-info t]
    ["Start Tutorial" skk-tutorial t]
    ["Customize Daredevil SKK" skk-e21-customize t]
    "--"
    ["About Daredevil SKK..." skk-version t]
    ["Visit Daredevil SKK Home..." skk-e21-visit-openlab t]))

(defvar skk-e21-modeline-property
  (and window-system
       (list 'local-map (purecopy
			 (make-mode-line-mouse2-map
			  #'skk-e21-modeline-menu))
	     'help-echo "マウスの button 2 -> Daredevil SKK のメニュ−")))

(defvar skk-e21-property-alist
  (list
   (cons 'default nil)
   (cons 'latin skk-e21-modeline-property)))


;; Functions.

(defun skk-e21-modeline-menu ()
  (interactive)
  ;; Find keys
  (aset (nth 1 skk-e21-modeline-menu-items)
	0
	(format "Hiragana %s"
		(if skk-j-mode
		    (if skk-katakana
			(skk-e21-find-func-keys 'skk-toggle-kana)
		      "")
		  (skk-e21-find-func-keys 'skk-kakutei))))
  (aset (nth 2 skk-e21-modeline-menu-items)
	0
	(format "Katakana %s"
		(if skk-j-mode
		    (if skk-katakana
			""
		      (skk-e21-find-func-keys 'skk-toggle-kana))
		  "")))
  (aset (nth 3 skk-e21-modeline-menu-items)
	0
	(format "Hankaku alphabet %s"
		(if skk-j-mode
		    (skk-e21-find-func-keys 'skk-latin-mode)
		  "")))
  (aset (nth 4 skk-e21-modeline-menu-items)
	0
	(format "Zenkaku alphabet %s"
		(if skk-j-mode
		    (skk-e21-find-func-keys 'skk-jisx0208-latin-mode)
		  "")))
  ;;
  (popup-menu skk-e21-modeline-menu-items))

(defun skk-e21-info ()
  (interactive)
  (Info-goto-node "(skk)"))

(defun skk-e21-customize ()
  (interactive)
  (customize-group "skk"))

(defun skk-e21-visit-openlab ()
  (interactive)
  (browse-url "http://openlab.ring.gr.jp/skk/index-j.html"))

;;;###autoload
(defun skk-e21-prepare-modeline-properties ()
  (let (face-sym)
    (dolist (mode '(hiragana katakana jisx0208-latin jisx0201 abbrev))
      (setq face-sym (intern (format "skk-e21-%s-face" mode)))
      (make-face face-sym)
      (when window-system
	(set-face-foreground
	 face-sym
	 (symbol-value (intern (format "skk-cursor-%s-color" mode)))))
      (set-face-bold-p face-sym t)
      (setq skk-e21-property-alist
	    (cons
	     (cons mode (append skk-e21-modeline-property
				(list 'face face-sym)))
	     skk-e21-property-alist)))))

(defun skk-e21-find-func-keys (func)
  (let ((keys
	 (or (do ((spec (nth 4 skk-rule-tree) (cdr spec))
		  (list nil (car spec))
		  (str nil (when (eq (nth 3 list)
				     func)
			     (nth 1 list))))
		 ((or str (null spec))
		  (when (stringp str)
		    str)))
	     (car (where-is-internal func skk-j-mode-map)))))
    (if keys
	(format "(%s)" (key-description keys))
      "")))

(require 'product)
(product-provide (provide 'skk-e21) (require 'skk-version))

;; skk-e21.el ends here
