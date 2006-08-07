;;; tinyinstall.el --- Emacs Lisp package install utility

;; Copyright (C) 1996,1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Created: 1996/08/18
;; Keywords: install, byte-compile, directory detection
;; Version: $Id: tinyinstall.el,v 1.12 2006/08/07 15:27:12 skk-cvs Exp $
;; Last Modified: $Date: 2006/08/07 15:27:12 $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defvar install-prefix
  (cond ((featurep 'xemacs)		; running-xemacs
	 (expand-file-name "../../.." exec-directory))
	((memq system-type '(ms-dos windows-nt))
	 (expand-file-name ".." exec-directory))
	(t
	 (expand-file-name "../../../.." data-directory))))

(defvar install-elisp-prefix "site-lisp")

;; from path-util.el
(defvar default-load-path load-path
  "*Base of `load-path'.
It is used as default value of target path to search file or
subdirectory under load-path.")

(defun install-detect-elisp-directory (&optional prefix elisp-prefix
						 allow-version-specific)
  (unless prefix
    (setq prefix install-prefix))
  (unless elisp-prefix
    (setq elisp-prefix install-elisp-prefix))
  (or
   (catch 'tag
     (let ((rest (delq nil (copy-sequence default-load-path)))
	   (pat (concat "^"
			(expand-file-name (concat ".*/" elisp-prefix) prefix)
			"/?$")))
       (while rest
	 (when (and (string-match pat (car rest))
		    (or allow-version-specific
			(not (string-match (format "/%d\\.%d"
						   emacs-major-version
						   emacs-minor-version)
					   (car rest)))))
	   (throw 'tag (car rest)))
	 (setq rest (cdr rest)))))
   (expand-file-name (concat
		      (if (not (featurep 'xemacs))
			  "share/"
			"lib/")
		      (cond ((featurep 'xemacs)
			     ;; running-xemacs
			     "xemacs/")
			    (t
			     "emacs/"))
		      elisp-prefix)
		     prefix)))

(defun tinyinstall-add-load-path (directory path)
  (setq directory (expand-file-name directory))
  (if (and (file-exists-p directory)
	   (null (member directory path)))
      (cons directory path)
    ;; original path
    path))

(provide 'tinyinstall)

;;; tinyinstall.el ends here
