;;; skk-setup.el --- initial setup for SKK  -*- emacs-lisp -*-

;; Copyright (C) 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-setup.el.in,v 1.36 2011/12/14 22:32:48 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2011/12/14 22:32:48 $

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
;; the Free Software Foundation Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

;;; Autoloads.
(unless (featurep 'xemacs)
  (require 'skk-autoloads))

;;; Key bindings.
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)

;;; Dictionaries.
;;(defvar skk-large-jisyo "@DIC@")
;;(defvar skk-aux-large-jisyo "@AUXDIC@")
;;(defvar skk-tut-file "@TUT@")

;;; Isearch setting.
(defun skk-isearch-setup-maybe ()
  (require 'skk-vars)
  (when (or (eq skk-isearch-mode-enable 'always)
	    (and (boundp 'skk-mode)
		 skk-mode
		 skk-isearch-mode-enable))
    (skk-isearch-mode-setup)))

(defun skk-isearch-cleanup-maybe ()
  (require 'skk-vars)
  (when (and (featurep 'skk-isearch)
	     skk-isearch-mode-enable)
    (skk-isearch-mode-cleanup)))

(add-hook 'isearch-mode-hook #'skk-isearch-setup-maybe)
(add-hook 'isearch-mode-end-hook #'skk-isearch-cleanup-maybe)

(provide 'skk-setup)

;;; skk-setup.el ends here
