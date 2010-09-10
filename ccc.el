;;; ccc.el --- cursor color control

;; Copyright (C) 2000 Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Copyright (C) 2001, 2002, 2004, 2005,
;;   2007, 2008 SKK Development Team <skk@ring.gr.jp>

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: ccc.el,v 1.38 2010/09/10 14:22:11 skk-cvs Exp $
;; Keywords: cursor
;; Last Modified: $Date: 2010/09/10 14:22:11 $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs, see the file COPYING.  If not, write to the
;; Free Software Foundation Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Buffer local frame parameters
;; --- cursor, foreground, background
;; --- TODO: support other frame parameters
;;           should use uni prefix for functions and variables?

;;; Code:

(eval-when-compile
  (require 'advice))

;; Internal variables.
(defvar buffer-local-cursor-color nil)
(make-variable-buffer-local 'buffer-local-cursor-color)

(defvar buffer-local-foreground-color nil)
(make-variable-buffer-local 'buffer-local-foreground-color)

(defvar buffer-local-background-color nil)
(make-variable-buffer-local 'buffer-local-background-color)

(defvar default-cursor-color nil)
(defvar default-foreground-color nil)
(defvar default-background-color nil)

;; Frame parameters.
(defsubst current-cursor-color ()
  (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))
(defsubst initial-cursor-color ()
  (cdr (assq 'cursor-color initial-frame-alist)))
(defsubst default-cursor-color ()
  (or default-cursor-color
      (cdr (assq 'cursor-color default-frame-alist))))
(defsubst fallback-cursor-color ()
  (if (eq frame-background-mode 'dark)
      "white"
    "black"))

(defsubst current-foreground-color ()
  (cdr (assq 'foreground-color (frame-parameters (selected-frame)))))
(defsubst initial-foreground-color ()
  (cdr (assq 'foreground-color initial-frame-alist)))
(defsubst default-foreground-color ()
  (or default-foreground-color
      (cdr (assq 'foreground-color default-frame-alist))))
(defsubst fallback-foreground-color ()
  (if (eq frame-background-mode 'dark)
      "white"
    "black"))

(defsubst current-background-color ()
  (cdr (assq 'background-color (frame-parameters (selected-frame)))))
(defsubst initial-background-color ()
  (cdr (assq 'background-color initial-frame-alist)))
(defsubst default-background-color ()
  (or default-background-color
      (cdr (assq 'background-color default-frame-alist))))
(defsubst fallback-background-color ()
  (if (eq frame-background-mode 'dark)
      "black"
    "white"))

(defsubst frame-cursor-color (&optional frame)
  (frame-parameter (or frame (selected-frame)) 'frame-cursor-color))
(defsubst set-frame-cursor-color (frame color)
  (modify-frame-parameters frame (list (cons 'frame-cursor-color color))))

(defsubst frame-foreground-color (&optional frame)
  (frame-parameter (or frame (selected-frame)) 'frame-foreground-color))
(defsubst set-frame-foreground-color (frame color)
  (modify-frame-parameters frame (list (cons 'frame-foreground-color color))))

(defsubst frame-background-color (&optional frame)
  (frame-parameter (or frame (selected-frame)) 'frame-background-color))
(defsubst set-frame-background-color (frame color)
  (modify-frame-parameters frame (list (cons 'frame-background-color color))))

;; Functions.
(defsubst ccc-read-color (prompt)
  (list (facemenu-read-color prompt)))

(defsubst ccc-color-equal (a b)
  (facemenu-color-equal a b))

(defun ccc-setup-new-frame (frame)
  (set-frame-cursor-color frame (or (default-cursor-color)
				    (fallback-cursor-color)))
  (set-frame-foreground-color frame (or (default-foreground-color)
					(fallback-foreground-color)))
  (set-frame-background-color frame (or (default-background-color)
					(fallback-background-color))))

;;;###autoload
(defun ccc-setup ()
  ;; Determine default colors for frames other than the initial frame.
  (setq default-cursor-color (or (default-cursor-color)
				 (current-cursor-color))
	default-foreground-color (or (default-foreground-color)
				     (current-foreground-color))
	default-background-color (or (default-background-color)
				     (current-background-color)))
  ;; Set up colors for the initial frame.
  (let ((frame (selected-frame)))
    (set-frame-cursor-color frame (or (initial-cursor-color)
				      (default-cursor-color)
				      (fallback-cursor-color)))
    (set-frame-foreground-color frame (or (initial-foreground-color)
					  (default-foreground-color)
					  (fallback-background-color)))
    (set-frame-background-color frame (or (initial-background-color)
					  (default-background-color)
					  (fallback-background-color)))))

;;;###autoload
(defun update-buffer-local-frame-params (&optional buffer)
  (with-current-buffer (if (buffer-live-p buffer)
			   buffer
			 (window-buffer (selected-window)))
    (update-buffer-local-cursor-color)
    (update-buffer-local-foreground-color)
    (update-buffer-local-background-color)))

;;
;; buffer-local-cursor
;;
(defun set-buffer-local-cursor-color (color-name)
  (interactive (ccc-read-color "Cursor color: "))
  (let ((local buffer-local-cursor-color))
    (setq buffer-local-cursor-color
	  (or color-name
	      (frame-cursor-color)))
    (condition-case nil
	(update-buffer-local-cursor-color)
      (error
       (setq buffer-local-cursor-color local)))))

(defun update-buffer-local-cursor-color ()
  (let ((color (if (stringp buffer-local-cursor-color)
		   buffer-local-cursor-color
		 (frame-cursor-color))))
    (when (and (stringp color)
	       (x-color-defined-p color)
	       (not (ccc-color-equal color (current-cursor-color))))
	(set-cursor-color color))))

(defun set-cursor-color-buffer-local (arg)
  (if arg
      (setq buffer-local-cursor-color (current-cursor-color))
    (set-cursor-color (frame-cursor-color))
    (setq buffer-local-cursor-color nil)))

;;
;; buffer-local-foreground-color
;;
(defun set-buffer-local-foreground-color (color-name)
  (interactive (ccc-read-color "Foreground color: "))
  (let ((local buffer-local-foreground-color))
    (setq buffer-local-foreground-color
	  (or color-name
	      (frame-foreground-color)))
    (condition-case nil
	(update-buffer-local-foreground-color)
      (error
       (setq buffer-local-foreground-color local)))))

(defun update-buffer-local-foreground-color ()
  (let ((color (if (stringp buffer-local-foreground-color)
		   buffer-local-foreground-color
		 (frame-foreground-color))))
    (when (and (stringp color)
	       (x-color-defined-p color)
	       (not (ccc-color-equal color (current-foreground-color))))
      (set-foreground-color color))))

(defun set-foreground-color-buffer-local (arg)
  (if arg
      (setq buffer-local-foreground-color (current-foreground-color))
    (set-foreground-color (frame-foreground-color))
    (setq buffer-local-foreground-color nil)))

;;
;; buffer-local-background-color
;;
(defun set-buffer-local-background-color (color-name)
  (interactive (ccc-read-color "Background color: "))
  (let ((local buffer-local-background-color))
    (setq buffer-local-background-color
	  (or color-name
	      (frame-background-color)))
    (condition-case nil
	(update-buffer-local-background-color)
      (error
       (setq buffer-local-background-color local)))))

(defun update-buffer-local-background-color ()
  (let ((color (if (stringp buffer-local-background-color)
		   buffer-local-background-color
		 (frame-background-color))))
    (when (and (stringp color)
	       (x-color-defined-p color)
	       (not (ccc-color-equal color (current-background-color))))
	(set-background-color color))))

(defun set-background-color-buffer-local (arg)
  (if arg
      (setq buffer-local-background-color (current-background-color))
    (set-background-color (frame-background-color))
    (setq buffer-local-background-color nil)))

;; Advices.
(defadvice modify-frame-parameters (after ccc-ad activate)
  (when (and (assq 'cursor-color (ad-get-arg 1))
	     (null buffer-local-cursor-color))
    (set-frame-cursor-color (ad-get-arg 0)
			    (cdr (assq 'cursor-color (ad-get-arg 1)))))
  (when (and (assq 'foreground-color (ad-get-arg 1))
	     (null buffer-local-foreground-color))
    (set-frame-foreground-color (ad-get-arg 0)
				(cdr (assq 'foreground-color (ad-get-arg 1)))))
  (when (and (assq 'background-color (ad-get-arg 1))
	     (null buffer-local-background-color))
    (set-frame-background-color (ad-get-arg 0)
				(cdr (assq 'background-color
					   (ad-get-arg 1))))))

;; Hooks
(add-hook 'post-command-hook 'update-buffer-local-frame-params)
(add-hook 'after-make-frame-functions 'ccc-setup-new-frame)
;;;###autoload
(add-hook 'after-init-hook
	  (lambda ()
	    (when window-system
	      (ccc-setup))))

(provide 'ccc)

;;; ccc.el ends here
