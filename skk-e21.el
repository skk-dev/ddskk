
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

(eval-and-compile
  (autoload 'Info-goto-node "info")
  (autoload 'browse-url "browse-url"))

;; Variables.
(defvar skk-xemacs-modeline-menu-items
  '("Daredevil SKK Menu"
    ["Read Manual" skk-e21-info t]
    ["Start Tutorial" skk-tutorial t]
    ["Customize Daredevil SKK" skk-e21-customize t]
    "--"
    ["About Daredevil SKK..." skk-version t]
    ["Visit Daredevil SKK Home..." skk-e21-visit-openlab t]))

(defvar skk-e21-modeline-property
  (list 'local-map (purecopy
		    (make-mode-line-mouse2-map
		     #'skk-e21-modeline-menu))
	'help-echo "マウスの button 2 -> Daredevil SKK のメニュ−")
  (let ((map (make-sparse-keymap)))
    (define-key
      map
      [button2]
      (eval '(make-modeline-command-wrapper 'skk-xemacs-modeline-menu)))
    map))

;; Functions.

(defun skk-e21-modeline-menu ()
  (interactive)
  (popup-menu skk-xemacs-modeline-menu-items))

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
  (setq skk-default-indicator ""
	skk-latin-mode-indicator
	(apply 'propertize skk-latin-mode-string skk-e21-modeline-property)
	skk-hiragana-mode-indicator
	(apply 'propertize skk-hiragana-mode-string skk-e21-modeline-property)
	skk-katakana-mode-indicator
	(apply 'propertize skk-katakana-mode-string skk-e21-modeline-property)
	skk-jisx0208-latin-mode-indicator
	(apply 'propertize skk-jisx0208-latin-mode-string
	       skk-e21-modeline-property)
	skk-jisx0201-mode-indicator
	(apply 'propertize skk-jisx0201-mode-string skk-e21-modeline-property)
	skk-abbrev-mode-indicator
	(apply 'propertize skk-abbrev-mode-string skk-e21-modeline-property)))

;;

(require 'product)
(product-provide (provide 'skk-e21) (require 'skk-version))

;; skk-xemacs.el ends here.

