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
  (require 'cl)
  (require 'static))

(eval-and-compile
  (autoload 'Info-goto-node "info")
  (autoload 'browse-url "browse-url"))

;; Variables.
(defvar skk-e21-modeline-menu-items
  (when window-system
    '("Daredevil SKK Menu"
      ["Hiragana"
       (skk-j-mode-on)
       :selected (and skk-j-mode (not skk-katakana))
       :style radio
       :keys nil
       :key-sequence nil]
      ["Katakana"
       (skk-j-mode-on t)
       :selected (and skk-j-mode skk-katakana)
       :style radio
       :keys nil
       :key-sequence nil]
      ["Hankaku alphabet"
       skk-latin-mode
       :selected skk-latin-mode
       :style radio
       :keys nil
       :key-sequence nil]
      ["Zenkaku alphabet"
       skk-jisx0208-latin-mode
       :selected skk-jisx0208-latin-mode
       :style radio
       :keys nil
       :key-sequence nil]
      "--"
      ["Read Manual" skk-e21-info t]
      ["Start Tutorial" skk-tutorial t]
      ["Customize Daredevil SKK" skk-e21-customize t]
      ["Send a Bug Report"
       (let (skk-japanese-message-and-error)
	 (skk-submit-bug-report)) t]
      "--"
      ["About Daredevil SKK..." skk-version t]
      ["Visit Daredevil SKK Home..." skk-e21-visit-openlab t])))

(defvar skk-e21-modeline-property
  (if window-system
      (list 'local-map (static-if
			   (fboundp
			    'make-mode-line-mouse-map)
			   (make-mode-line-mouse-map
			    'mouse-2 #'skk-e21-modeline-menu)
			 (make-mode-line-mouse2-map
			  #'skk-e21-modeline-menu))
	    'help-echo
	    "マウスの button 2 -> Daredevil SKK のメニュ−")
    nil))

(defvar skk-e21-property-alist
  (if window-system
      (list
       (cons 'latin skk-e21-modeline-property))
    nil))

;; Functions.

(defun skk-e21-modeline-menu ()
  (interactive)
  ;; Find keys
  (aset (nth 1 skk-e21-modeline-menu-items)
	7
	(cond (skk-katakana
	       (skk-e21-find-func-keys 'skk-toggle-kana))
	      ((not skk-j-mode)
	       (skk-e21-find-func-keys 'skk-kakutei))
	      (t
	       nil)))
  (aset (nth 2 skk-e21-modeline-menu-items)
	7
	(if (and skk-j-mode
		 (not skk-katakana))
	    (skk-e21-find-func-keys 'skk-toggle-kana)
	  nil))
  (aset (nth 3 skk-e21-modeline-menu-items)
	7
	(if skk-j-mode
	    (skk-e21-find-func-keys 'skk-latin-mode)
	  nil))
  (aset (nth 4 skk-e21-modeline-menu-items)
	7
	(if skk-j-mode
	    (skk-e21-find-func-keys 'skk-jisx0208-latin-mode)
	  nil))
  ;;
  (let ((easy-menu-converted-items-table
	 (make-hash-table :test 'equal)))
    (popup-menu skk-e21-modeline-menu-items)))

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
  (when window-system
    (let (face)
      (dolist (mode '(hiragana
		      katakana
		      jisx0208-latin
		      jisx0201
		      abbrev))
	(setq face (intern (format "skk-e21-%s-face" mode)))
	(make-face face)
	(set-face-foreground face
			     (symbol-value
			      (intern
			       (format "skk-cursor-%s-color"
				       mode))))
	(push (cons mode (append skk-e21-modeline-property
				 (list 'face face)))
	       skk-e21-property-alist)))))

(defun skk-e21-find-func-keys (func)
  (let ((keys
	 (or (do ((spec (nth 4 skk-rule-tree) (cdr spec))
		  (list nil (car spec))
		  (str nil (when (eq (nth 3 list)
				     func)
			     (nth 1 list))))
		 ((or str (null spec))
		  (if (stringp str)
		      str
		    nil)))
	     (car (where-is-internal func skk-j-mode-map)))))
    (if keys
	(format "%s" (key-description keys))
      nil)))

;;

(require 'product)
(product-provide
    (provide 'skk-e21)
  (require 'skk-version))

;; skk-e21.el ends here
