;;; skk-nicola-dcomp.el
;; Copyright (C) 2001 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-macs)
  (require 'static))

;;;###autoload
(add-hook 'skk-mode-hook
	  (lambda ()
	    (if (and (featurep 'skk-dcomp)
		     (featurep 'skk-nicola))
		(require 'skk-nicola-dcomp))))

(defadvice skk-nicola-self-insert-lshift (around skk-nicola-dcomp activate)
  (cond
   ((or (not skk-dcomp-activate)
	(eq skk-henkan-mode 'active))
    ad-do-it)
   (t
    (let (pos)
      (cond
       ((or (eq skk-henkan-mode 'active)
	    (skk-get-prefix skk-current-rule-tree)
	    (not skk-comp-stack))
	(skk-set-marker skk-dcomp-start-point nil)
	(skk-set-marker skk-dcomp-end-point nil))
       ((skk-dcomp-marked-p)
	(skk-dcomp-face-off)
	(unless (member (this-command-keys)
			skk-dcomp-keep-completion-keys)
	  ;;
	  (if (eq this-command 'skk-nicola-self-insert-rshift)
	      (setq pos (point))
	    (ignore-errors
	      (delete-region skk-dcomp-start-point
			     skk-dcomp-end-point))))))
      ad-do-it
      ;;
      (when (and (eq this-command
		     'skk-nicola-self-insert-rshift)
		 (eq skk-henkan-mode 'on))
	(when (and (markerp skk-dcomp-start-point)
		   (marker-position skk-dcomp-start-point)
		   (< (marker-position skk-dcomp-start-point)
		      pos))
	  (delete-region skk-dcomp-start-point
			 pos))
	(when (and (markerp skk-dcomp-end-point)
		   (marker-position skk-dcomp-end-point)
		   (< (point)
		      (marker-position skk-dcomp-end-point)))
	  (delete-region skk-dcomp-end-point
			 (point))))
      ;;
      (when (and (eq skk-henkan-mode 'on)
		 (not (skk-get-prefix skk-current-rule-tree))
		 (not skk-okurigana))
	(let ((pos (point)))
	  (condition-case nil
	      (progn
		(skk-comp-do 'first 'silent)
		(skk-set-marker skk-dcomp-start-point
				pos)
		(skk-set-marker skk-dcomp-end-point
				(point))
		(skk-dcomp-face-on skk-dcomp-start-point
				   skk-dcomp-end-point)
		(goto-char skk-dcomp-start-point))
	    (error
	     (setq skk-comp-stack nil)
	     (message nil)))))))))

;;

(require 'product)
(product-provide
    (provide 'skk-nicola-dcomp)
  (require 'skk-version))

;;; skk-kanagaki-util.el ends here
