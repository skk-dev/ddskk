;;; ccc.el --- cursor color control

;; Copyright (C) 2000 Masatake YAMATO <masata-y@is.aist-nara.ac.jp>

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: ccc.el,v 1.10 2001/12/02 03:28:17 czkmt Exp $
;; Keywords: cursor
;; Last Modified: $Date: 2001/12/02 03:28:17 $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs, see the file COPYING.  If not, write to the
;; Free Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Buffer local frame parameters
;; --- cursor, foreground, background
;; --- TODO: support other frame parameters
;;           should use uni prefix for functions and variables?

;;; Code:

(eval-when-compile
  (require 'advice)
  (require 'poe)
  (require 'static))

;; Frame parameters.
(defsubst get-apparent-cursor-color ()
  (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))

(defsubst get-apparent-foreground-color ()
  (cdr (assq 'foreground-color (frame-parameters (selected-frame)))))

(defsubst get-apparent-background-color ()
  (cdr (assq 'background-color (frame-parameters (selected-frame)))))

;; Internal variables.
(defvar frame-cursor-color (get-apparent-cursor-color))

(defvar buffer-local-cursor-color nil)
(make-variable-buffer-local 'buffer-local-cursor-color)

(defvar frame-foreground-color (get-apparent-foreground-color))

(defvar buffer-local-foreground-color nil)
(make-variable-buffer-local 'buffer-local-foreground-color)

(defvar frame-background-color (get-apparent-background-color))

(defvar buffer-local-background-color nil)
(make-variable-buffer-local 'buffer-local-background-color)

;; Macros.
(defmacro ccc-defadvice (function &rest everything-else)
  (let ((origfunc (and (fboundp function)
		       (if (ad-is-advised function)
			   (ad-get-orig-definition function)
			 (symbol-function function))))
	interactive)
    (unless
	(or (not origfunc)
	    (not (subrp origfunc))
	    (memq function ; XXX possibilly Emacs version dependent
		  ;; built-in commands which do not have interactive specs.
		  '(abort-recursive-edit
		    bury-buffer
		    delete-frame
		    delete-window
		    exit-minibuffer)))
      ;; check if advice definition has a interactive call or not.
      (setq interactive
	    (cond
	     ((and (stringp (nth 1 everything-else)) ; have document
		   (eq 'interactive (car-safe (nth 2 everything-else))))
	      (nth 2 everything-else))
	     ((eq 'interactive (car-safe (nth 1 everything-else)))
	      (nth 1 everything-else))))
      (cond
       ((and (commandp origfunc)
	     (not interactive))
	(message "%s"
		 "\
*** WARNING: Adding advice to subr %s\
 without mirroring its interactive spec ***"
		 function))
       ((and (not (commandp origfunc))
	     interactive)
	(setq everything-else (delq interactive everything-else))
	(message
	 "\
*** WARNING: Deleted interactive call from %s advice\
 as %s is not a subr command ***"
	 function function))))
    (` (defadvice (, function) (,@ everything-else)))))

;;;###autoload
(put 'ccc-defadvice 'lisp-indent-function 'defun)
(def-edebug-spec ccc-defadvice defadvice)

;; Functions.
(defsubst ccc-read-color (prompt)
  (static-cond
   ((string-lessp "20.5" emacs-version)
    (list (facemenu-read-color prompt)))
   (t
    (list (format "s%s" prompt)))))

;;;###autoload
(defun update-buffer-local-frame-params ()
  (update-buffer-local-cursor-color)
  (update-buffer-local-foreground-color)
  (update-buffer-local-background-color))

;;
;; buffer-local-cursor
;;
(defun set-buffer-local-cursor-color (color-name)
  (interactive (ccc-read-color "Cursor color: "))
  (let ((local buffer-local-cursor-color))
    (setq buffer-local-cursor-color
	  (if (and (stringp color-name)
		   (> (length color-name) 0))
	      color-name
	    frame-cursor-color))
    (condition-case nil
	(update-buffer-local-cursor-color)
      (error
       (setq buffer-local-cursor-color local)))))

(defun update-buffer-local-cursor-color ()
  (set-cursor-color
   (cond ((stringp buffer-local-cursor-color)
	  buffer-local-cursor-color)
	 (t
	  frame-cursor-color))))

(defun set-cursor-color-buffer-local (arg)
  (if arg
      (setq buffer-local-cursor-color (get-apparent-cursor-color))
    (set-cursor-color frame-cursor-color)
    (setq buffer-local-cursor-color nil)))

;;
;; buffer-local-foreground-color
;;
(defun set-buffer-local-foreground-color (color-name)
  (interactive (ccc-read-color "Foreground color: "))
  (let ((local buffer-local-foreground-color))
    (setq buffer-local-foreground-color
	  (if (and (stringp color-name)
		   (> (length color-name) 0))
	      color-name
	    frame-foreground-color))
    (condition-case nil
	(update-buffer-local-foreground-color)
      (error
       (setq buffer-local-foreground-color local)))))

(defun update-buffer-local-foreground-color ()
  (set-foreground-color
   (cond ((stringp buffer-local-foreground-color)
	  buffer-local-foreground-color)
	 (t
	  frame-foreground-color))))

(defun set-foreground-color-buffer-local (arg)
  (if arg
      (setq buffer-local-foreground-color (get-apparent-foreground-color))
    (set-foreground-color frame-foreground-color)
    (setq buffer-local-foreground-color nil)))

;;
;; buffer-local-background-color
;;
(defun set-buffer-local-background-color (color-name)
  (interactive (ccc-read-color "Background color: "))
  (let ((local buffer-local-background-color))
    (setq buffer-local-background-color
	  (if (and (stringp color-name)
		   (> (length color-name) 0))
	      color-name
	    frame-background-color))
    (condition-case nil
	(update-buffer-local-background-color)
      (error
       (setq buffer-local-background-color local)))))

(defun update-buffer-local-background-color ()
  (set-background-color
   (cond ((stringp buffer-local-background-color)
	  buffer-local-background-color)
	 (t
	  frame-background-color))))

(defun set-background-color-buffer-local (arg)
  (if arg
      (setq buffer-local-background-color (get-apparent-background-color))
    (set-background-color frame-background-color)
    (setq buffer-local-background-color nil)))

;; Advices.
(let ((funcs '(;; cover to original Emacs functions.
	       ;; subr, but no argument.
	       bury-buffer
	       delete-frame
	       delete-window

	       overwrite-mode
	       ;; subr, but non-command.
	       pop-to-buffer
	       select-window

	       ;; subrs possibly with interactive specs.
	       (execute-extended-command . "P")
	       (kill-buffer . "bKill buffer: ")
	       (other-window . "p")
	       (select-frame . "e")
	       (switch-to-buffer . "BSwitch to buffer: ")))
      func)
  (while (setq func (car funcs))
    (if (consp func)
	;; command that has an interactive spec.
	(eval
	 (`
	  (ccc-defadvice (, (intern (symbol-name (car func)))) (after ccc-ad
								      activate)
	    "Update frame parameters if `buffer-local-*-color's are given."
	    (interactive (, (cdr func)))
	    (update-buffer-local-frame-params))))
      ;; non-command or command that has not an interactive spec.
      (eval
       (`
	(ccc-defadvice (, (intern (symbol-name func))) (after ccc-ad activate)
	  "Update frame parameters if `buffer-local-*-color's are given."
	  (update-buffer-local-frame-params)))))
    (setq funcs (cdr funcs))))

(defadvice modify-frame-parameters (after ccc-ad activate)
  (when (and (assq 'cursor-color (ad-get-arg 1))
	     (null buffer-local-cursor-color))
    (setq frame-cursor-color (get-apparent-cursor-color)))
  (when (and (assq 'foreground-color (ad-get-arg 1))
	     (null buffer-local-foreground-color))
    (setq frame-foreground-color (get-apparent-foreground-color)))
  (when (and (assq 'background-color (ad-get-arg 1))
	     (null buffer-local-background-color))
    (setq frame-background-color (get-apparent-background-color))))

;; Hooks
(add-hook 'isearch-mode-end-hook 'update-buffer-local-frame-params 'append)
(add-hook 'minibuffer-setup-hook 'update-buffer-local-frame-params 'append)
(add-hook 'minibuffer-exit-hook
	  (lambda ()
	    (with-current-buffer (nth 1 (buffer-list))
	      (update-buffer-local-frame-params)))
	  'append)

(provide 'ccc)

;;; ccc.el ends here
