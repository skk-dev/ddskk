;;; skk-dcomp.el --- SKK dynamic completion
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-dcomp.el,v 1.5 1999/09/23 13:52:18 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/09/23 13:52:18 $

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

(defgroup skk-dcomp nil "SKK dynamic completion related customization."
  :prefix "skk-dcomp-"
  :group 'skk )

(defface skk-dcomp-face
  '((((class color)) (:foreground "DarkKhaki"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t)) )
  "*Face used to highlight region dynamically completed."
  :group 'skk-faces )

(defcustom skk-dcomp-face-priority 700
  "*Overlay/extent priority of `skk-dcomp-face'."
  :type 'integer
  :group 'skk-dcomp )

(skk-deflocalvar skk-dcomp-start-point nil)
(defvar skk-dcomp-extent nil)
;; why is it necessary?
(defvar skk-dcomp-face 'skk-dcomp-face)

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
