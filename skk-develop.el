;;; skk-develop.el --- support SKK developper.
;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-develop.el,v 1.11 2001/02/03 00:22:58 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/02/03 00:22:58 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; Daredevil SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

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
  (if (not (skk-y-or-n-p
	    "SKK についてのバグレポートを書きますか？ "
	    "Do you really want to write a bug report on SKK? "))
      nil
    (reporter-submit-bug-report
     skk-ml-address
     (concat (skk-version 'with-codename)
	     ", "
	     (cond ((or (and (boundp 'skk-servers-list) skk-servers-list)
			(or (and (boundp 'skk-server-host) skk-server-host)
			    (getenv "SKKSERVER"))
			;; refer to DEFAULT_JISYO when skk-server-jisyo is nil.
			;;(or (and (boundp 'skk-server-jisyo) skk-server-jisyo)
			;;    (getenv "SKK_JISYO"))))
			)
		    (require 'skk-server)
		    (concat "skkserv; " (skk-server-version)
			    (if (getenv "SKKSERVER")
				(concat ",\nSKKSERVER; "
					(getenv "SKKSERVER")))
			    (if (getenv "SKKSERV")
				(concat ", SKKSERV; "
					(getenv "SKKSERV")))))
		   ((and (boundp 'skk-exserv-list) skk-exserv-list)
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
       (and (boundp 'skk-server-host)
	    (setq base (append base '(skk-server-host))))
       (and (boundp 'skk-server-prog)
	    (setq base (append base '(skk-server-prog))))
       (and (boundp 'skk-servers-list)
	    (setq base (append base '(skk-servers-list))))
       (and (boundp 'skk-exserv-list) 
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

(eval-after-load "font-lock"
  '(setq lisp-font-lock-keywords-2
	 (nconc
	  '(("^(\\(skk-defun-cond\\)[ \t'\(]*\\(\\sw+\\)?"
	     (1 font-lock-keyword-face)
	     (2 font-lock-variable-name-face))
	    ("^(\\(skk-defsubst-cond\\)[ \t'\(]*\\(\\sw+\\)?"
	     (1 font-lock-keyword-face)
	     (2 font-lock-variable-name-face))
	    ("^(\\(skk-defavice\\)[ \t'\(]*\\(\\sw+\\)?"
	     (1 font-lock-keyword-face)
	     (2 font-lock-variable-name-face))
	    ("^(\\(skk-deflocalvar\\)[ \t'\(]*\\(\\sw+\\)?"
	     (1 font-lock-keyword-face)
	     (2 font-lock-variable-name-face)))
	  lisp-font-lock-keywords-2)))

(require 'product)
(product-provide (provide 'skk-develop) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-develop.el ends here
