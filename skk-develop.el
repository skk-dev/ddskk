;;; skk-edebug.el --- support SKK developper.
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-develop.el,v 1.1 1999/08/29 13:26:18 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/08/29 13:26:18 $

;; This file is not part of SKK yet.

;; SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;; Following people contributed to skk-develop.el (Alphabetical order):
;;; Code:
(require 'skk)

(eval-after-load "edebug"
  '(progn
     (def-edebug-spec skk-save-point t)
     (def-edebug-spec skk-with-point-move t)
     ;; よう分かりません...
     ;;(def-edebug-spec skk-defun-cond
     ;;  (&define name lambda-list
     ;;           [&optional stringp]
     ;;           [&rest form def-body] ))
     ;;skk-defsubst-cond
     ))

(eval-after-load "hilit19"
  '(mapcar (function
            (lambda (pattern)
              (hilit-add-pattern
               (car pattern) (cdr pattern)
               (cond ((eq skk-background-mode 'mono)
                      'bold )
                     ((eq skk-background-mode 'light)
                      'RoyalBlue )
                     (t 'cyan) )
               'emacs-lisp-mode )))
           '(("^\\s *(skk-deflocalvar\\s +\\S +" . "")
	     ("^\\s *(skk-defun-cond\\s +\\S +" . "")
	     ("^\\s *(skk-defsubst-cond\\s +\\S +" . "") )))

(eval-after-load "font-lock"
  '(setq lisp-font-lock-keywords-2
	 (nconc
	  '(("^(\\(skk-defun-cond\\)[ \t'\(]*\\(\\sw+\\)?"
	     (1 font-lock-keyword-face)
	     (2 font-lock-variable-name-face) )
	    ("^(\\(skk-defsubst-cond\\)[ \t'\(]*\\(\\sw+\\)?"
	     (1 font-lock-keyword-face)
	     (2 font-lock-variable-name-face) )
	    ("^(\\(skk-deflocalvar\\)[ \t'\(]*\\(\\sw+\\)?"
	     (1 font-lock-keyword-face)
	     (2 font-lock-variable-name-face) ))
	  lisp-font-lock-keywords-2 )))

(provide 'skk-develop)
;;; Local Variables:
;;; End:
;;; skk-develop.el ends here
