;;; ccc.el --- cursor color control

;; Copyright (C) 2000 Masatake YAMATO <masata-y@is.aist-nara.ac.jp>

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: ccc.el,v 1.30 2007/07/31 06:44:36 skk-cvs Exp $
;; Keywords: cursor
;; Last Modified: $Date: 2007/07/31 06:44:36 $

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
  (require 'advice)
  (require 'poe))

;; Frame parameters.
(defsubst get-apparent-cursor-color ()
  (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))

(defsubst get-apparent-foreground-color ()
  (cdr (assq 'foreground-color (frame-parameters (selected-frame)))))

(defsubst get-apparent-background-color ()
  (cdr (assq 'background-color (frame-parameters (selected-frame)))))

(defsubst set-frame-cursor-color (frame color)
  (modify-frame-parameters frame (list (cons 'frame-cursor-color color))))

(defsubst set-frame-foreground-color (frame color)
  (modify-frame-parameters frame (list (cons 'frame-foreground-color color))))

(defsubst set-frame-background-color (frame color)
  (modify-frame-parameters frame (list (cons 'frame-background-color color))))

;; Internal variables.
(defvar frame-cursor-color (get-apparent-cursor-color))
(make-variable-frame-local 'frame-cursor-color)

(defvar buffer-local-cursor-color nil)
(make-variable-buffer-local 'buffer-local-cursor-color)

(defvar frame-foreground-color (get-apparent-foreground-color))
(make-variable-frame-local 'frame-foreground-color)

(defvar buffer-local-foreground-color nil)
(make-variable-buffer-local 'buffer-local-foreground-color)

(defvar frame-background-color (get-apparent-background-color))
(make-variable-frame-local 'frame-background-color)

(defvar buffer-local-background-color nil)
(make-variable-buffer-local 'buffer-local-background-color)

;; Functions.
(defsubst ccc-read-color (prompt)
  (list (facemenu-read-color prompt)))

(defsubst ccc-color-equal (a b)
  (facemenu-color-equal a b))

(defun ccc-setup-new-frame (frame)
  (set-frame-cursor-color frame frame-cursor-color)
  (set-frame-foreground-color frame frame-foreground-color)
  (set-frame-background-color frame frame-background-color))

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
	      frame-cursor-color))
    (condition-case nil
	(update-buffer-local-cursor-color)
      (error
       (setq buffer-local-cursor-color local)))))

(defun update-buffer-local-cursor-color ()
  (let ((color (if (stringp buffer-local-cursor-color)
		   buffer-local-cursor-color
		 frame-cursor-color)))
    (unless (ccc-color-equal color (get-apparent-cursor-color))
      (set-cursor-color color))))

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
	  (or color-name
	      frame-foreground-color))
    (condition-case nil
	(update-buffer-local-foreground-color)
      (error
       (setq buffer-local-foreground-color local)))))

(defun update-buffer-local-foreground-color ()
  (let ((color (if (stringp buffer-local-foreground-color)
		   buffer-local-foreground-color
		 frame-foreground-color)))
    (unless (ccc-color-equal color (get-apparent-foreground-color))
      (set-foreground-color color))))

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
	  (or color-name
	      frame-background-color))
    (condition-case nil
	(update-buffer-local-background-color)
      (error
       (setq buffer-local-background-color local)))))

(defun update-buffer-local-background-color ()
  (let ((color (if (stringp buffer-local-background-color)
		   buffer-local-background-color
		 frame-background-color)))
    (unless (ccc-color-equal color (get-apparent-background-color))
      (set-background-color color))))

(defun set-background-color-buffer-local (arg)
  (if arg
      (setq buffer-local-background-color (get-apparent-background-color))
    (set-background-color frame-background-color)
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

(provide 'ccc)

;;; ccc.el ends here
