;; skk-e18.el --- emacs 18 specific functions for skk.el
;; Copyright (C) 2000 Tsukamoto Tetsuo

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-e18.el,v 1.4 2000/11/25 17:27:18 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/11/25 17:27:18 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK  is free software;  you  can redistribute it  and/or modify it
;; under the terms  of the GNU General Public License  as published by the Free
;; Software  Foundation;  either versions  2,  or  (at your option)  any  later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful  but WITHOUT
;; ANY  WARRANTY;  without  even  the implied  warranty  of MERCHANTABILITY  or
;; FITNESS  FOR  A PARTICULAR PURPOSE.  See the GNU General Public License  for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; Daredevil SKK,  see the file COPYING.  If not,  write  to  the Free Software
;; Foundation Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

;;; Commentary:

;; Daredevil SKK を Emacs 18 ベースで利用するための work around です。
;; 基本的な機能しか動きません。現在動作確認できる環境は
;;
;;     o Nemacs 3.3.2 based on Emacs 18.59
;;
;; に限られています。
;;
;; Daredevil SKK  は advice.el を必要とします。 Emacs 18 で利用できる advice.el
;; は Daredevil SKK のアーカイブの  patch/e18/ というディレクトリに収録されてい
;; ます。

;;; Code:
(condition-case nil
    (require 'advice)
  (error
   (error "advice.el is required for this version of SKK.
Install patch/e18/advice.el in load-path and try again.")))

(defvar-maybe minibuffer-setup-hook nil)
(defvar-maybe minibuffer-exit-hook nil)

;; skk-vars.el で default variable を nil にしておきましたが、念のた
;; め、defconst しておきましょう。
(defconst skk-use-color-cursor nil)
(defconst skk-cursor-change-width nil)

(require 'skk-macs)
(require 'skk-vars)

;; Variables.
(defvar auto-fill-function nil)

;; Macros.
(defmacro-maybe save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list 'let (list (list original '(match-data)))
          (list 'unwind-protect
                (cons 'progn body)
                (list 'store-match-data original)))))

;; Inline functions.
;; Pieces of advice.
(defadvice byte-code-function-p (around skk-e18-ad activate)
  (cond ((and (consp (ad-get-arg 0)) (consp (cdr (ad-get-arg 0))))
	 ad-do-it)
	(t
	 nil)))

(defadvice search-forward (after skk-e18-ad activate)
  (when ad-return-value
    (setq ad-return-value (match-end 0))))

(defadvice search-backward (after skk-e18-ad activate)
  (when ad-return-value
    (setq ad-return-value (match-beginning 0))))

(defadvice re-search-forward (after skk-e18-ad activate)
  (when ad-return-value
    (setq ad-return-value (match-end 0))))

(defadvice re-search-backward (after skk-e18-ad activate)
  (when ad-return-value
    (setq ad-return-value (match-beginning 0))))

(when (< emacs-minor-version 59)
  (defadvice call-process (after skk-e18-ad activate)
    (when (and (not (eq 0 (ad-get-arg 2)))
	       (null ad-return-value))
      (setq ad-return-value 0))))

(defadvice read-from-minibuffer (before skk-e18-ad activate)
  ;;
  (when (and minibuffer-exit-hook
	     (skk-in-minibuffer-p))
    (condition-case nil
	(run-hooks 'minibuffer-exit-hook)
      (error)))
  ;;
  (when minibuffer-setup-hook
    (with-current-buffer
	(get-buffer-create
	 (format " *Minibuf-%d*" (minibuffer-depth)))
      (run-hooks 'minibuffer-setup-hook))))

(defadvice exit-minibuffer (around skk-e18-ad activate)
  (let ((no-nl (and skk-egg-like-newline skk-henkan-on)))
    (progn
      ;; なぜか 2 回 skk-kautei を呼ばないとうまくいかない。
      ;; 原因を考え中。
      (skk-kakutei)
      (skk-kakutei))
    (if no-nl
	nil
      (setq skk-mode nil)
      ad-do-it)))

;; Other functions.
(defun-maybe window-minibuffer-p (&optional window)
"Return non-nil if WINDOW is a minibuffer window."
  (eq (or window (selected-window)) (minibuffer-window)))

(defun-maybe overlayp (object))

(defun-maybe float (arg)
  arg)

(defun-maybe frame-width (&optional frame)
  "Return number of columns available for display on FRAME.
If FRAME is omitted, describe the currently selected frame."
  (screen-width))

(defalias-maybe 'insert-and-inherit 'insert)
(defalias-maybe 'number-to-string 'int-to-string)

(when (eq skk-emacs-type 'mule1)
  (defun insert-file-contents-as-coding-system
    (coding-system filename &optional visit beg end replace)
    "Like `insert-file-contents', q.v., but CODING-SYSTEM the first arg will
be applied to `file-coding-system-for-read'."
    (let ((file-coding-system-for-read coding-system))
      (insert-file-contents filename visit))))

(defun skk-e18-make-local-map (map1 map2)
  (let ((alist1 (cdr (copy-sequence map1)))
	(alist2 (cdr (copy-sequence map2)))
	alist cell1 cell2 cell)
    (while alist1
      (setq cell nil)
      (setq cell1 (car alist1))
      (cond ((and (keymapp (cdr cell1))
		  (setq cell2 (assq (car cell1) alist2))
		  (keymapp (cdr cell2)))
	     (setq cell (cons (car cell1)
			      (skk-e18-make-local-map
			       (cdr cell1)
			       (cdr cell2))))
	     (setq alist2 (delete cell2 alist2)))
	    (t
	     (setq cell cell1)))
      (when cell
	(setq alist (nconc alist (list cell))))
      (setq alist1 (cdr alist1)))
    (while alist2
      (setq alist (nconc alist (list (car alist2))))
      (setq alist2 (cdr alist2)))
    (cons 'keymap alist)))

;; Hooks.

;;(add-hook 'skk-load-hook
;;	  (function
;;	   (lambda ()
;;
;;	       ;; end case nemacs
;;	       ))))

(require 'product)
(product-provide (provide 'skk-e18) (require 'skk-version))
;;; skk-e18.el ends here
