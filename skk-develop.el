;;; skk-develop.el --- support SKK developper

;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-develop.el,v 1.20 2001/12/16 05:03:09 czkmt Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2001/12/16 05:03:09 $

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

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

(eval-when-compile
  (defvar skk-exserv-list))

;;;###autoload
(defun skk-submit-bug-report ()
  "SKK のバグレポートを書くメールバッファを用意する。
mail-user-agent を設定することにより好みのメールインターフェイスを使用すること
ができる。例えば、Wanderlust を使用したい場合は下記のように設定する。

    \(setq mail-user-agent 'wl-user-agent\) "
  (interactive)
  (require 'reporter)
  (when (skk-y-or-n-p
	 "SKK についてのバグレポートを書きますか？ "
	 "Do you really want to write a bug report on SKK? ")
    (reporter-submit-bug-report
     skk-ml-address
     (concat (skk-version 'with-codename)
	     ", "
	     (cond
	      ((or (and (boundp 'skk-servers-list)
			skk-servers-list)
		   (or (and (boundp 'skk-server-host)
			    skk-server-host)
		       (getenv "SKKSERVER"))
		   ;; refer to DEFAULT_JISYO when skk-server-jisyo is nil.
		   ;;(or (and (boundp 'skk-server-jisyo) skk-server-jisyo)
		   ;;    (getenv "SKK_JISYO"))))
		   )
	       (require 'skk-server)
	       (concat "skkserv; "
		       (skk-server-version)
		       (when (getenv "SKKSERVER")
			 (concat ",\nSKKSERVER; "
				 (getenv "SKKSERVER")))
		       (when (getenv "SKKSERV")
			 (concat ", SKKSERV; "
				 (getenv "SKKSERV")))))
	      ((and (boundp 'skk-exserv-list)
		    skk-exserv-list)
	       (require 'skk-exserv)
	       (skk-server-version))))
     (let ((base (list 'window-system
		       'isearch-mode-hook
		       'isearch-mode-end-hook
		       'skk-auto-okuri-process
		       'skk-auto-start-henkan
		       'skk-egg-like-newline
		       'skk-henkan-okuri-strictly
		       'skk-henkan-strict-okuri-precedence
		       'skk-kakutei-early
		       'skk-process-okuri-early
		       'skk-search-prog-list
		       'skk-share-private-jisyo
		       'skk-use-viper)))
       (when (boundp 'skk-server-host)
	 (setq base (append base '(skk-server-host))))
       (when (boundp 'skk-server-prog)
	 (setq base (append base '(skk-server-prog))))
       (when (boundp 'skk-servers-list)
	 (setq base (append base '(skk-servers-list))))
       (when (boundp 'skk-exserv-list)
	 (setq base (append base '(skk-exserv-list))))
       base))))

(eval-after-load "hilit19"
  '(mapcar (function
	    (lambda (pattern)
	      (hilit-add-pattern
	       (car pattern) (cdr pattern)
	       (cond ((eq skk-background-mode 'mono)
		      'bold)
		     ((eq skk-background-mode 'light)
		      'RoyalBlue)
		     (t 'cyan))
	       'emacs-lisp-mode)))
	   '(("^\\s *(skk-deflocalvar\\s +\\S +" . "")
	     ("^\\s *(skk-defun-cond\\s +\\S +" . "")
	     ("^\\s *(skk-defadvice\\s +\\S +" . "")
	     ("^\\s *(skk-defsubst-cond\\s +\\S +" . ""))))

;;;###autoload
(eval-after-load "font-lock"
  '(setq lisp-font-lock-keywords-2
	 (nconc
	  (list
	   (list
	    (concat "(\\(skk-def\\("
		    ;; Function declarations.
		    "\\(un-cond\\|subst-cond\\|advice\\)\\|"
		    ;; Variable declarations.
		    "\\(var\\|localvar\\)"
		    "\\)\\)\\>"
		    ;; Any whitespace and defined object.
		    "[ \t'\(]*"
		    "\\(\\sw+\\)?")
	    '(1 font-lock-keyword-face)
	    '(5 (cond ((match-beginning 3) font-lock-function-name-face)
		      ((match-beginning 5) font-lock-variable-name-face))
		nil t)))
	  lisp-font-lock-keywords-2)))

(require 'product)
(product-provide
    (provide 'skk-develop)
  (require 'skk-version))

;;; skk-develop.el ends here
