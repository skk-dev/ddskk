;;; skk-jisyo-edit-mode.el --- major mode for editing SKK dictionaries

;; Copyright (C) 2001 SKK Development Team

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
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a major mode for editing SKK dictionaries.

;;; Code:

(eval-when-compile
  (require 'poe)
  (require 'static)
  (defvar font-lock-defaults))

(defvar skk-jisyo-edit-map nil
  "Keymap for SKK JISYO Edit mode.")

(defvar skk-jisyo-edit-mode-hook nil)

(defvar skk-jisyo-edit-syntax-table nil)

(unless skk-jisyo-edit-map
  (setq skk-jisyo-edit-map (make-sparse-keymap 'skk-jisyo-edit-map)))

(defconst skk-jisyo-edit-font-lock-keywords
 '(("^\\(;; okuri-ari entries\\.\\)$" 1 font-lock-keyword-face)
   ("^\\(;; okuri-nasi entries\\.\\)$" 1 font-lock-keyword-face)
   ("^\\(;.+\\)$" 1 font-lock-comment-face)
   ("^\\([^; ]+ \\)/" 1 font-lock-function-name-face)
   ("\\(;[^/]*\\)/" 1 font-lock-type-face)
   ("/\\([^/\n]+\\)$" 1 highlight)
   ("\\(/\\)" 1 font-lock-warning-face))
 "Additional expressions to highlight in SKK JISYO edit mode.")

(put 'skk-jisyo-edit-mode
     'font-lock-defaults
     '(skk-jisyo-edit-font-lock-keywords))

;;;###autoload
(defun skk-jisyo-edit-mode ()
  "Major mode for editing SKK JISYO."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "SKK JISYO Edit")
  (setq major-mode 'skk-jisyo-edit)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(skk-jisyo-edit-font-lock-keywords))
  (make-local-variable 'skk-jisyo-edit-syntax-table)
  (setq skk-jisyo-edit-syntax-table (make-syntax-table))
  (set-syntax-table skk-jisyo-edit-syntax-table)
  (let ((map (make-sparse-keymap)))
    (static-cond
     ((featurep 'xemacs)
      (set-keymap-parents map (list skk-jisyo-edit-map))
      (use-local-map map))
     (t
      (use-local-map (nconc map skk-jisyo-edit-map)))))
  (modify-syntax-entry ?\" "w" skk-jisyo-edit-syntax-table)
  (modify-syntax-entry ?/ "w" skk-jisyo-edit-syntax-table)
  (run-hooks 'skk-jisyo-edit-mode-hook))

;;;###autoload
(unless (member '("SKK-JISYO" . skk-jisyo-edit-mode)
		auto-mode-alist)
  (setq auto-mode-alist
	(append
	 '(("SKK-JISYO" . skk-jisyo-edit-mode)
	   ("\\.skk-jisyo\\(\\.BAK\\|\\.bak\\|~\\)?$"
	    . skk-jisyo-edit-mode)
	   ("\\..*skk/jisyo\\(\\.BAK\\|\\.bak\\|~\\)?$"
	    . skk-jisyo-edit-mode))
	 auto-mode-alist)))

(require 'product)
(product-provide
    (provide 'skk-jisyo-edit)
  (require 'skk-version))

;;; skk-jisyo-edit.el ends here
