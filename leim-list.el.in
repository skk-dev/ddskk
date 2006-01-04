;;; skk/leim-list.el --- list of LEIM for SKK  -*- emacs-lisp -*-

;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
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
;; the Free Software Foundation Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file registers SKK as input methods for Emacs.
;; Currently this file is for FSF Emacs only (not for XEmacs).

;;; Code:

(unless noninteractive
  ;; No need to load this file when Emacs is called for `make' or most of other
  ;; batch processings.

  ;; `register-input-method' is called in skk-autoloads.el. `skk-activate' is
  ;; also autoloaded there.
  (require 'skk-autoloads)

  ;; Doubt. This is a matter of preference.
  (require 'skk-setup)

;;; skk/leim-list.el should end here but some works are needed for Emacs 20.

  ;; For Emacs 20. Load all "leim-list" files.
  (cond

   ((string-lessp "5.0" mule-version)
    ;; Emacs 21 loads all "leim-list" files in load-path.
    nil)

   ((fboundp 'load-leim-list-except-this)
    ;; "egg/leim-list.el" is already loaded.
    nil)

   ((locate-library "egg/leim-list.el" 'nosuffix)
    ;; Try loading egg/leim-list.el.
    (load "egg/leim-list.el" 'noerror 'nomessage 'nosuffix))

   (t
    ;; Shamelessly ripped off from "egg/leim-list.el" and "egg-util.el" in
    ;; Tamago 4.
    (defun locate-libraries (library &optional nosuffix path interactive-call)
      (let ((lpath (or path load-path))
	    (result nil))
	(while lpath
	  (let ((path
		 (locate-library library nosuffix lpath interactive-call)))
	    (if path
		(progn
		  (setq lpath (cdr-safe
			       (member (directory-file-name
					(file-name-directory path))
				       lpath))
			result (cons path result)))
	      (progn
		(setq lpath nil
		      result (reverse result))))))
	result))

    (defun load-libraries (library &optional path)
      (let ((files (locate-libraries library nil (or path load-path) nil)))
	(while files
	  (load-file (car files))
	  (setq files (cdr files)))))

    (defun load-leim-list-except-this ()
      (load-libraries
       "leim-list"
       (cdr-safe (member (directory-file-name
			  (file-name-directory load-file-name))
			 load-path))))

    (message "Finished loading %s \n   and load others..." load-file-name)
    (load-leim-list-except-this))))

;;; skk/leim-list.el ends here
