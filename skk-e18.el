;; skk-e18.el --- emacs 18 specific functions for skk.el
;; Copyright (C) 2000 Tsukamoto Tetsuo

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese

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

;; Although v18 original compiler cannot expand APEL specific macro such as
;; `defmacro-maybe' or `defun-maybe', but jwz's bytecompiler can do.
;; so require pces to expand such macros.
(require 'pces)
(condition-case nil
    (require 'advice)
  (error
   (error "advice.el is required for this version of SKK.
Install patch/e18/advice.el in load-path and try again.")))

;; for safety.
(defconst skk-use-color-cursor nil)
(defconst skk-cursor-change-width nil)
(defconst skk-use-face nil)

(require 'skk-macs)
(require 'skk-vars)

;; Variables.
(defvar-maybe auto-fill-function nil)
(defvar skk-e18-self-insert-keys
  (append (where-is-internal 'self-insert-command global-map)
	  (where-is-internal 'canna-self-insert-command global-map)
	  (where-is-internal 'canna-henkan-region-or-self-insert global-map)
	  (where-is-internal 'egg-self-insert-command global-map)
	  '("\t")))

;; Can v18 original compiler expand `skk-deflocalvar'?
;; I'm not sure...
(defvar skk-current-local-map nil)
(make-variable-buffer-local 'skk-current-local-map)

(defvar-maybe minibuffer-setup-hook nil)
(defvar-maybe minibuffer-exit-hook nil)
(defvar-maybe minor-mode-map-alist nil)

(let ((i 0) e list)
  (setq list '(skk-latin-mode-map skk-j-mode-map skk-jisx0208-latin-mode-map
				  skk-abbrev-mode-map))
  (while (setq e (nth i list))
    (set e (make-sparse-keymap))
    (setq i (1+ i)))
  ;; Defined in skk-mode.
  ;; (define-key skk-latin-mode-map skk-kakutei-key 'skk-kakutei)
  (setq i 0 list skk-e18-self-insert-keys)
  (while (setq e (nth i list))
    (define-key skk-j-mode-map e 'skk-insert)
    (setq i (1+ i)))
  ;; Defined in skk-mode.
  ;; (define-key skk-jisx0208-latin-mode-map skk-kakutei-key 'skk-kakutei)
  (setq i 0)
  (while (< i 128)
    (and (aref skk-jisx0208-latin-vector i)
	 (define-key skk-jisx0208-latin-mode-map
	   (char-to-string i) 'skk-jisx0208-latin-insert))
    (setq i (1+ i)))
  (define-key skk-jisx0208-latin-mode-map "\C-q" 'skk-latin-henkan))

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

(if (product-version>= 'apel-ver '(10 3))
    nil
  (defadvice read-from-minibuffer (around skk-e18-ad activate)
    ;;
    (when minibuffer-setup-hook
      (with-current-buffer
	  (get-buffer-create
	   (format " *Minibuf-%d*" (minibuffer-depth)))
	(run-hooks 'minibuffer-setup-hook)))
    ;;
    ad-do-it
    ;;
    (when minibuffer-exit-hook
      (with-current-buffer
	  (get-buffer-create
	   (format " *Minibuf-%d*" (minibuffer-depth)))
	(condition-case nil
	    (run-hooks 'minibuffer-exit-hook)
	  (error))))))

(defadvice exit-minibuffer (around skk-e18-ad activate)
  (let ((no-nl (and skk-egg-like-newline skk-henkan-on)))
    (when skk-henkan-on
      (unless skk-mode
	(skk-mode 1))
      (skk-kakutei))
    (if no-nl
	nil
      (setq skk-mode nil)
      ad-do-it)))

(defadvice skk-kakutei (around skk-e18-ad activate)
  (let ((skk-jisyo skk-jisyo))
    (when skk-henkan-on
      (unless skk-mode
	(skk-mode 1)))
    ad-do-it))

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
  (defun-maybe insert-file-contents-as-coding-system
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

(defun skk-e18-setup ()
  (let ((keymap (if (skk-in-minibuffer-p)
		    minibuffer-local-map
		  (current-local-map))))
    (if (and keymap (eq (lookup-key keymap "a") 'skk-insert))
	nil
      (setq skk-current-local-map keymap))))

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
