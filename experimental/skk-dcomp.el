;;; skk-dcomp.el --- SKK dynamic completion
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-dcomp.el,v 1.3 1999/09/21 21:48:21 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/09/21 21:48:21 $

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

;;; Commentary
;; 
;; Inspired by POBox developed by 増井俊之.

;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-foreword)
(require 'skk-comp)

(defface skk-dcomp-face
  '(
    ;;(((class color) (background light)) (:foreground ""))
    ;; Is there something good?
    ;;(((class color) (background dark)) (:foreground ""))
    (((class grayscale) (background light))
     (:foreground "LightGray" :bold t :underline t) )
    (((class grayscale) (background dark))
     (:foreground "Gray50" :bold t :underline t) )
    (t (:foreground "DarkKhaki")) )
  "*Face used to highlight region dynamically completed." )

(defvar skk-dcomp-face-priority 700
  "*Overlay/extent priority of `skk-dcomp-face'." )

(skk-deflocalvar skk-dcomp-start-point nil)
(defvar skk-dcomp-extent nil)

;; to be moved to skk.el...
(skk-defun-cond skk-face-on
  (object start end face &optional priority)
  ((eq skk-emacs-type 'xemacs)
   (let ((inhibit-quit t))
     (if (extentp object)
	 nil
       (setq object (make-extent start end))
       (set-extent-properties
	object
	(if priority (list 'face face 'priority priority) (list 'face face)) ))
     (insert-extent object start end) ))
  (t
   (let ((inhibit-quit t))
     (if (overlayp object)
	 nil
       (setq object (make-overlay start end))
       (and priority (overlay-put object 'priority priority)) )
     (move-overlay object start end)
     (overlay-put object 'face face) )))

;; to be moved to skk.el...
(skk-defun-cond skk-detach-extent (object)
  ((eq skk-emacs-type 'xemacs)
   (and (extentp object) (detach-extent object)) )
  (t
   (and (overlayp object) (delete-overlay object)) ))

(defun skk-dcomp-face-on (start end)
  (skk-face-on skk-dcomp-extent start end skk-dcomp-face
	       skk-dcomp-face-priority ))

(defun skk-dcomp-face-off ()
  (skk-detach-extent skk-dcomp-extent) )

;; main dynamic completion engine.
(defadvice skk-kana-input (around skk-dcomp-ad activate)
  (if (not skk-henkan-on)
      ad-do-it
    (if (or skk-henkan-active (skk-get-prefix skk-current-rule-tree)
	    (not skk-completion-stack) )
	(setq skk-dcomp-start-point nil)
      (when skk-dcomp-start-point
	(skk-dcomp-face-off)
	(condition-case nil
	    (delete-region skk-dcomp-start-point (point))
	  (error) )))
    ad-do-it
    (if (and (not (skk-get-prefix skk-current-rule-tree)) (not skk-okurigana))
	(progn
	  (setq skk-dcomp-start-point (point))
	  (condition-case nil
	      (skk-completion 'first)
	    (error
	     (setq skk-completion-stack nil)
	     (message nil) ))
	  (skk-dcomp-face-on skk-dcomp-start-point (point)) ))))

(defadvice skk-kakutei (after skk-dcomp-ad activate)
  (skk-dcomp-face-off)
  (setq skk-dcomp-start-point nil
	skk-completion-stack nil ))

(provide 'skk-dcomp)
;;; Local Variables:
;;; End:
;;; skk-dcomp.el ends here
