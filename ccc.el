;;; ccc.el --- cursor color control

;; Copyright (C) 2000 Masatake YAMATO <masata-y@is.aist-nara.ac.jp>

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: ccc.el,v 1.7 2001/11/19 15:54:00 czkmt Exp $
;; Keywords: cursor
;; Last Modified: $Date: 2001/11/19 15:54:00 $

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
  (require 'static)
  ;; shut up compiler warnings.
  (defvar buffer-local-cursor-color)
  (defvar buffer-local-background-color)
  (defvar buffer-local-background-color-default)
  (defvar buffer-local-cursor-color-default)
  (defvar buffer-local-foreground-color)
  (defvar buffer-local-foreground-color-default))

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

(put 'ccc-defadvice 'lisp-indent-function 'defun)
(def-edebug-spec ccc-defadvice defadvice)

;; functions.
;;;###autoload
(defun update-buffer-local-frame-params ()
  (update-buffer-local-cursor-color)
  (update-buffer-local-foreground-color)
  (update-buffer-local-background-color))

;;
;; buffer-local-cursor
;;
(defun buffer-local-cursor-color-default ()
  (static-if (featurep 'xemacs)
      (frame-property (selected-frame) 'cursor-color)
    (cdr (assq 'cursor-color
	       (frame-parameters (selected-frame))))))

(defun set-buffer-local-cursor-color (color-name)
  (interactive "sColor: ")
  (setq buffer-local-cursor-color
	(if (and (stringp color-name)
		 (> (length color-name) 0))
	    color-name
	  buffer-local-cursor-color-default))
  (update-buffer-local-cursor-color))

(defun update-buffer-local-cursor-color ()
  (when (and buffer-local-cursor-color
	     (stringp buffer-local-cursor-color)
	     (not (string= (buffer-local-cursor-color-default)
			   buffer-local-cursor-color)))
    (condition-case error
	(set-cursor-color buffer-local-cursor-color)
      (error
       (setq buffer-local-cursor-color nil)))))

;;
;; buffer-local-foreground-color
;;
(defun buffer-local-foreground-color-default ()
  (static-if (featurep 'xemacs)
      (frame-property (selected-frame) 'foreground-color)
    (cdr (assq 'foreground-color
	       (frame-parameters (selected-frame))))))

(defun set-buffer-local-foreground-color (color-name)
  (interactive "sColor: ")
  (setq buffer-local-foreground-color
	(if (and (stringp color-name)
		 (> (length color-name) 0))
	    color-name
	  buffer-local-foreground-color-default))
  (update-buffer-local-foreground-color))

(defun update-buffer-local-foreground-color ()
  (when (and buffer-local-foreground-color
	     (stringp buffer-local-foreground-color)
	     (not (string= (buffer-local-foreground-color-default)
			   buffer-local-foreground-color)))
    (condition-case error
	(set-foreground-color buffer-local-foreground-color)
      (error
       (setq buffer-local-foreground-color nil)))))

;;
;; buffer-local-background-color
;;
(defun buffer-local-background-color-default ()
  (static-if (featurep 'xemacs)
      (frame-property (selected-frame) 'background-color)
    (cdr (assq 'background-color
	       (frame-parameters (selected-frame))))))

(defun set-buffer-local-background-color (color-name)
  (interactive "sColor: ")
  (setq buffer-local-background-color
      (if (and (stringp color-name)
	       (> (length color-name) 0))
	  color-name
	buffer-local-background-color-default))
  (update-buffer-local-background-color))

(defun update-buffer-local-background-color ()
  (when (and buffer-local-background-color
	     (stringp buffer-local-background-color)
	     (not (string= (buffer-local-background-color-default)
			   buffer-local-background-color)))
    (condition-case error
	(set-background-color buffer-local-background-color)
      (error
       (setq buffer-local-background-color nil)))))

;; internal variables.
(defvar buffer-local-cursor-color-default
  (buffer-local-cursor-color-default))
(defvar buffer-local-cursor-color
  (buffer-local-cursor-color-default))
(make-variable-buffer-local 'buffer-local-cursor-color)

(defvar buffer-local-foreground-color-default
  (buffer-local-foreground-color-default))
(defvar buffer-local-foreground-color
  (buffer-local-foreground-color-default))
(make-variable-buffer-local 'buffer-local-foreground-color)

(defvar buffer-local-background-color-default
  (buffer-local-background-color-default))
(defvar buffer-local-background-color
  (buffer-local-background-color-default))
(make-variable-buffer-local 'buffer-local-background-color)

;; advices.
(let ((funcs '(
	       ;; cover to original Emacs functions.
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
	       (switch-to-buffer . "BSwitch to buffer: ")

	       ;;goto-line
	       ;;insert-file
	       ;;recenter
	       ;;yank
	       ;;yank-pop
	       ))
      func)
  (while (setq func (car funcs))
    (if (consp func)
	;; command that has an interactive spec.
	(eval
	 (`
	  (ccc-defadvice (, (intern (symbol-name (car func))))
	    (after buffer-local-frame-params-ad activate)
	    "Update frame parameters if `buffer-local-*-color's are given."
	    (interactive (, (cdr func)))
	    (update-buffer-local-frame-params))))
      ;; non-command or command that has not an interactive spec.
      (eval
       (`
	(ccc-defadvice (, (intern (symbol-name func)))
	  (after buffer-local-frame-params-ad activate)
	  "Update frame parameters if `buffer-local-*-color's are given."
	  (update-buffer-local-frame-params)))))
    (setq funcs (cdr funcs))))

;; Hooks
(add-hook 'isearch-mode-end-hook 'update-buffer-local-frame-params 'append)
(add-hook 'minibuffer-setup-hook 'update-buffer-local-frame-params 'append)
(add-hook 'minibuffer-exit-hook
	  (lambda ()
	    (with-current-buffer (nth 1 (buffer-list))
	      (update-buffer-local-frame-params))) 'append)

(provide 'ccc)

;;; ccc.el ends here
