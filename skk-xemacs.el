;;; skk-xemacs.el -- XEmacs support for SKK.
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

(eval-and-compile
  (autoload 'Info-goto-node "info")
  (autoload 'browse-url "browse-url"))

;; Variables.

(defvar skk-xemacs-hiragana-extent (make-extent nil nil))
(defvar skk-xemacs-katakana-extent (make-extent nil nil))
(defvar skk-xemacs-jisx0208-latin-extent (make-extent nil nil))
(defvar skk-xemacs-latin-extent (make-extent nil nil))
(defvar skk-xemacs-jisx0201-extent (make-extent nil nil))

(defvar skk-xemacs-modeline-menu-items
  '("Daredevil SKK Menu"
    ["Read Manual" skk-xemacs-info t]
    ["Start Tutorial" skk-tutorial t]
    ["Customize Daredevil SKK" skk-xemacs-customize t]
    "--"
    ["About Daredevil SKK..." skk-version t]
    ["Visit Daredevil SKK Home..." skk-xemacs-visit-openlab t]))

(when (featurep 'window-system)
  ;;
  (defvar skk-xemacs-modeline-map
    (let ((map (make-sparse-keymap)))
      (define-key
	map
	[button2]
	(eval '(make-modeline-command-wrapper 'skk-xemacs-modeline-menu)))
      map))
  ;;
  (dolist (sym '(skk-xemacs-hiragana-extent
		    skk-xemacs-katakana-extent
		    skk-xemacs-jisx0208-latin-extent
		    skk-xemacs-latin-extent
		    skk-xemacs-jisx0201-extent))
    (let ((extent (symbol-value sym)))
    (set-extent-keymap extent skk-xemacs-modeline-map)
    (set-extent-property extent 'help-echo
			 "マウスの button 2 -> Daredevil SKK のメニュ−"))))

;; Functions.

(defun skk-xemacs-modeline-menu ()
  (interactive)
  (popup-menu skk-xemacs-modeline-menu-items))

(defun skk-xemacs-info ()
  (interactive)
  (Info-goto-node "(skk)"))

(defun skk-xemacs-customize ()
  (interactive)
  (customize-group "skk"))

(defun skk-xemacs-visit-openlab ()
  (interactive)
  (browse-url "http://openlab.ring.gr.jp/skk/index-j.html"))

;;;###autoload
(defun skk-xemacs-prepare-modeline-properties ()
  (make-face 'skk-xemacs-hiragana-face)
  (set-face-parent 'skk-xemacs-hiragana-face 'modeline nil '(default))
  (when (featurep 'window-system)
    (set-face-foreground 'skk-xemacs-hiragana-face
			 skk-cursor-hiragana-color nil
			 '(default color win))
    (set-face-font 'skk-xemacs-hiragana-face [bold] nil
		   '(default mono win))
    (set-face-font 'skk-xemacs-hiragana-face [bold] nil
		   '(default grayscale win)))
  (set-extent-face skk-xemacs-hiragana-extent 'skk-xemacs-hiragana-face)
  ;;
  (make-face 'skk-xemacs-katakana-face)
  (set-face-parent 'skk-xemacs-katakana-face 'modeline nil '(default))
  (when (featurep 'window-system)
    (set-face-foreground 'skk-xemacs-katakana-face
			 skk-cursor-katakana-color nil
			 '(default color win))
    (set-face-font 'skk-xemacs-katakana-face [bold] nil
		   '(default mono win))
    (set-face-font 'skk-xemacs-katakana-face [bold] nil
		   '(default grayscale win)))
  (set-extent-face skk-xemacs-katakana-extent 'skk-xemacs-katakana-face)
  ;;
  (make-face 'skk-xemacs-jisx0208-latin-face)
  (set-face-parent 'skk-xemacs-jisx0208-latin-face 'modeline
		   nil '(default))
  (when (featurep 'window-system)
    (set-face-foreground 'skk-xemacs-jisx0208-latin-face
			 skk-cursor-jisx0208-latin-color nil
			 '(default color win))
    (set-face-font 'skk-xemacs-jisx0208-latin-face [bold] nil
		   '(default mono win))
    (set-face-font 'skk-xemacs-jisx0208-latin-face [bold] nil
		   '(default grayscale win)))
  (set-extent-face skk-xemacs-jisx0208-latin-extent
		   'skk-xemacs-jisx0208-latin-face)
  ;;
  (make-face 'skk-xemacs-latin-face)
  (set-face-parent 'skk-xemacs-latin-face 'modeline nil '(default))
  (when (featurep 'window-system)
    (set-face-foreground 'skk-xemacs-latin-face
			 skk-cursor-latin-color nil
			 '(default color win))
    (set-face-font 'skk-xemacs-latin-face [bold] nil
		   '(default mono win))
    (set-face-font 'skk-xemacs-latin-face [bold] nil
		   '(default grayscale win)))
  (set-extent-face skk-xemacs-latin-extent 'skk-xemacs-latin-face)
  ;;
  (defconst skk-xemacs-abbrev-extent (make-extent nil nil))
  (make-face 'skk-xemacs-abbrev-face)
  (set-face-parent 'skk-xemacs-abbrev-face 'modeline nil '(default))
  (when (featurep 'window-system)
    (set-face-foreground 'skk-xemacs-abbrev-face
			 skk-cursor-abbrev-color nil
			 '(default color win))
    (set-face-font 'skk-xemacs-abbrev-face [bold] nil
		   '(default mono win))
    (set-face-font 'skk-xemacs-abbrev-face [bold] nil
		   '(default grayscale win)))
  (set-extent-face skk-xemacs-abbrev-extent 'skk-xemacs-abbrev-face)
  ;;
  (make-face 'skk-xemacs-jisx0201-face)
  (set-face-parent 'skk-xemacs-jisx0201-face 'modeline nil '(default))
  (when (featurep 'window-system)
    (set-face-foreground 'skk-xemacs-jisx0201-face
			 skk-cursor-jisx0201-color nil
			 '(default color win))
    (set-face-font 'skk-xemacs-jisx0201-face [bold] nil
		   '(default mono win))
    (set-face-font 'skk-xemacs-jisx0201-face [bold] nil
		   '(default grayscale win)))
  (set-extent-face skk-xemacs-jisx0201-extent 'skk-xemacs-jisx0201-face)
  ;;
  (setq skk-default-indicator
	(cons (make-extent nil nil) "")
	skk-latin-mode-indicator
	(cons skk-xemacs-latin-extent skk-latin-mode-string)
	skk-hiragana-mode-indicator
	(cons skk-xemacs-hiragana-extent skk-hiragana-mode-string)
	skk-katakana-mode-indicator
	(cons skk-xemacs-katakana-extent skk-katakana-mode-string)
	skk-jisx0208-latin-mode-indicator
	(cons skk-xemacs-jisx0208-latin-extent skk-jisx0208-latin-mode-string)
	skk-jisx0201-mode-indicator
	(cons skk-xemacs-jisx0201-extent skk-jisx0201-mode-string)
	skk-abbrev-mode-indicator
	(cons skk-xemacs-abbrev-extent skk-abbrev-mode-string)))

;; Hooks.

;;; Not necessary, but...
;;;###autoload
(add-hook 'before-init-hook
	  '(lambda ()
	     ;; Don't give dired this!
	     (define-key ctl-x-map [(control j)] 'skk-mode)))

;;

(require 'product)
(product-provide (provide 'skk-xemacs) (require 'skk-version))

;; skk-xemacs.el ends here.

