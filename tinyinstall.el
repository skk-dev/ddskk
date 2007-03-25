;;; tinyinstall.el --- Emacs Lisp package install utility

;; Copyright (C) 1996,1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Created: 1996/08/18
;; Keywords: install, byte-compile, directory detection
;; Version: $Id: tinyinstall.el,v 1.13 2007/03/25 20:41:49 skk-cvs Exp $
;; Last Modified: $Date: 2007/03/25 20:41:49 $

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

(when (and (stringp VERSION_SPECIFIC_LISPDIR)
	   (stringp EMU_PREFIX))
  (let ((dir (expand-file-name EMU_PREFIX VERSION_SPECIFIC_LISPDIR)))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(require 'install)

(setq install-prefix
  (cond ((featurep 'xemacs)		; running-xemacs
	 (expand-file-name "../../.." exec-directory))
	((memq system-type '(ms-dos windows-nt))
	 (expand-file-name ".." exec-directory))
	(t
	 (expand-file-name "../../../.." data-directory))))

(provide 'tinyinstall)

;;; tinyinstall.el ends here
