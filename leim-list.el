;;; skk/leim-list.el -- list of LEIM (Library of Emacs Input Method) for SKK
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

;; You should have received a copy of the GNU General Public License along
;; with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; Loading this file registers SKK as input methods for Emacs.
;; Currently this file is for FSF Emacs only (not for XEmacs).

;;; Code:

(require 'skk-autoloads)

(if (string-lessp "5.0" mule-version)
    ;; Emacs 21 or later.
    (add-hook 'before-init-hook
	      (function
	       (lambda ()
		 (require 'skk-setup))))
  (require 'skk-setup))

;; For Emacs 20. Load all "leim-list" files.
(when (and (not (featurep 'xemacs))
	   site-run-file)
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
    ;; Shamelessly ripped off from "egg/leim-list.el" in Tamago 4.
    (defun load-leim-list-except-this ()
      (load-libraries
       "leim-list"
       (cdr-safe (member (directory-file-name
			  (file-name-directory load-file-name))
			 load-path))))
    (message "Finished loading %s \n   and load others..." load-file-name)
    (load-leim-list-except-this))))

;;; skk/leim-list.el ends here
