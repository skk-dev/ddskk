;;; skk-dcomp.el --- SKK dynamic completion
;; Copyright (C) 1999, 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-dcomp.el,v 1.9 2000/11/28 13:06:34 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/11/28 13:06:34 $

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
;; along with Daredevil SKK, see the file COPYING.  If not, write to the
;; Free Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary
;; 
;; Inspired by POBox developed by 増井俊之.

;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-comp)

(defgroup skk-dcomp nil "SKK dynamic completion related customization."
  :prefix "skk-dcomp-"
  :group 'skk)

(defface skk-dcomp-face
  '((((class color)) (:foreground "DarkKhaki"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t)))
  "*Face used to highlight region dynamically completed."
  :group 'skk-faces)

(defcustom skk-dcomp-face-priority 700
  "*Overlay/extent priority of `skk-dcomp-face'."
  :type 'integer
  :group 'skk-dcomp)

(skk-deflocalvar skk-dcomp-start-point nil)
(skk-deflocalvar skk-dcomp-end-point nil)
(skk-deflocalvar skk-dcomp-extent nil)
;; why is it necessary?
(defvar skk-dcomp-face 'skk-dcomp-face)
(defvar skk-dcomp-toggle-key
  (car-safe
   (or (rassoc (list nil 'skk-toggle-kana) skk-rom-kana-rule-list)
       (rassoc (list nil 'skk-toggle-characters) skk-rom-kana-rule-list)
       (rassoc (list nil 'skk-toggle-kana) skk-rom-kana-base-rule-list)
       (rassoc (list nil 'skk-toggle-characters) skk-rom-kana-base-rule-list))))

(defun skk-dcomp-face-on (start end)
  (skk-face-on skk-dcomp-extent start end skk-dcomp-face
	       skk-dcomp-face-priority))

(defun skk-dcomp-face-off ()
  (skk-detach-extent skk-dcomp-extent))

;; main dynamic completion engine.
(defadvice skk-kana-input (around skk-dcomp-ad activate)
  (if (not skk-henkan-on)
      ad-do-it
    (let (pos)
      (if (or skk-henkan-active (skk-get-prefix skk-current-rule-tree)
	      (not skk-completion-stack))
	  (skk-set-marker skk-dcomp-start-point nil)
	(when (marker-position skk-dcomp-start-point)
	  (skk-dcomp-face-off)
	  (or (equal skk-dcomp-toggle-key (this-command-keys))
	      (condition-case nil
		  (delete-region skk-dcomp-start-point (point))
		(error)))))
      ad-do-it
      (if (and (not (skk-get-prefix skk-current-rule-tree)) (not skk-okurigana))
	  (progn
	    (setq pos (point))
	    (condition-case nil
		(skk-completion 'first 'silent)
	      (error
	       (setq skk-completion-stack nil)
	       (message nil)))
	    (skk-set-marker skk-dcomp-start-point pos)
	    (skk-set-marker skk-dcomp-end-point (point))
	    (skk-dcomp-face-on skk-dcomp-start-point skk-dcomp-end-point))))))

(defadvice skk-kakutei (after skk-dcomp-ad activate)
  (skk-dcomp-face-off)
  (skk-set-marker skk-dcomp-start-point nil)
  (skk-set-marker skk-dcomp-end-point nil)
  (setq skk-completion-stack nil))

(defadvice skk-start-henkan (before skk-dcomp-ad activate)
  (skk-dcomp-face-off)
  (delete-region skk-dcomp-end-point (point))
  (skk-set-marker skk-dcomp-end-point (point)))
  
(defadvice keyboard-quit (after skk-dcomp-ad activate)
  (if skk-henkan-on
      (progn
	(skk-set-marker skk-dcomp-start-point nil)
	(skk-set-marker skk-dcomp-end-point nil)
	(setq skk-completion-stack nil))))

(require 'product)
(product-provide (provide 'skk-dcomp) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-dcomp.el ends here
