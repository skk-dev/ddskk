;;; skk-develop.el --- support SKK developper.
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-develop.el,v 1.3 1999/09/25 05:26:58 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/09/25 05:26:58 $

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
	    "Do you really want to write a bug report on SKK? " ))
      nil
    (if (boundp 'mail-user-agent)
	(cond ((eq mail-user-agent 'wl-user-agent)
	       (require 'wl-user-agent)
	       (define-mail-user-agent
		 'wl-user-agent 'wl-user-agent-compose
		 'wl-draft-send 'wl-draft-kill
		 'mail-send-hook ))
	      ((eq mail-user-agent 'mew-user-agent)
	       ;; Mew 1.93 works well.
	       (require 'mew)
	       (define-mail-user-agent 'mew-user-agent
		 'mew-send 'mew-draft-send-letter 'mew-draft-kill ))))
    (reporter-submit-bug-report
     skk-ml-address
     (concat "skk.el " (skk-version)
	     (if (or (and (boundp 'skk-servers-list) skk-servers-list)
		     (or (and (boundp 'skk-server-host) skk-server-host)
			 (getenv "SKKSERVER") )
		     ;; refer to DEFAULT_JISYO when skk-server-jisyo is nil.
		     ;;(or (and (boundp 'skk-server-jisyo) skk-server-jisyo)
		     ;;    (getenv "SKK_JISYO") )))
		     )
		 (progn
		   (require 'skk-server)
		   (concat ", skkserv; " (skk-server-version)
			   (if (getenv "SKKSERVER")
			       (concat ",\nSKKSERVER; "
				       (getenv "SKKSERVER") ))
			   (if (getenv "SKKSERV")
			       (concat ", SKKSERV; "
				       (getenv "SKKSERV") ))))))
     (let ((base (list 'window-system
		       'skk-auto-okuri-process
		       'skk-auto-start-henkan
		       'skk-egg-like-newline
		       'skk-henkan-okuri-strictly
		       'skk-henkan-strict-okuri-precedence
		       'skk-kakutei-early
		       'skk-process-okuri-early
		       'skk-search-prog-list
		       'skk-use-face
		       'skk-use-viper )))
       (and (boundp 'skk-henkan-face)
	    (setq base (append base '(skk-henkan-face))) )
       (and (boundp 'skk-server-host)
	    (setq base (append base '(skk-server-host))) )
       (and (boundp 'skk-server-prog)
	    (setq base (append base '(skk-server-prog))) )
       (and (boundp 'skk-servers-list)
	    (setq base (append base '(skk-servers-list))) )
       base ))))

(eval-after-load "edebug"
  '(progn
     (def-edebug-spec skk-save-point t)
     (def-edebug-spec skk-with-point-move t)
     (def-edebug-spec skk-defun-cond
       (&define name lambda-list
		[&optional stringp]
		[&rest ([&not eval] [&rest sexp])]
		[&optional (eval [&optional ("interactive" interactive)] def-body)]
		&rest (&rest sexp) ))
     (def-edebug-spec skk-defmacro-cond
       (&define name lambda-list
		[&rest ([&not eval] [&rest sexp])]
		[&optional (eval def-body)]
		&rest (&rest sexp) ))
     (def-edebug-spec skk-defmacro-cond skk-defsubst-cond)
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
