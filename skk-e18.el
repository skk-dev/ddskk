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
(require
 (condition-case nil
     (require 'advice)
   (error
    (error "%s"
	   "advice.el is required for this version of SKK.
Install patch/e18/advice.el in load-path and try again."))))

;; for safety.
(defconst skk-use-color-cursor nil)
(defconst skk-cursor-change-width nil)
(defconst skk-use-face nil)

(require 'skk-macs)
(require 'skk-vars)

;; Variables.
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

(defvar-maybe pre-command-hook nil)
(defvar-maybe post-command-hook nil)
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

;; Pieces of advice.
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

(defadvice byte-code-function-p (around skk-e18-ad activate)
  ;; これは一時の APEL のバグに対して work around したものだから、最新の
  ;; APEL に対しては不要。
  (cond ((and (consp (ad-get-arg 0)) (consp (cdr (ad-get-arg 0))))
	 ad-do-it)
	(t
	 nil)))

;; 時折、検索系の関数が数値を返すことを期待しているコードがあるため、
;; それらが動くように以下の 4 関数への advice をする。
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
    ;; Emacs 18.55 ではプロセス周りのコードが Emacs 18.59 と違い、
    ;; `call-process' は常に nil を返すため、数値 (0 または 1) を返すことを期
    ;; 待しているコードはうまく動作しない。以下の対処は正しくないが、どうしよ
    ;; うもない。
    (when (and (not (eq 0 (ad-get-arg 2)))
	       (null ad-return-value))
      (setq ad-return-value 0))))

;; Emacs 18 において、`defadvice' は関数の定義後に呼ばれる必要があるらしいの
;; で、以下の関数を定義しておいて、これを `skk-mode-invoke' から呼び出す。
(defun skk-e18-advise-skk-functions ()
  ;; It is impossible to take advantage of `pre-command-hook' and
  ;; `post-command-hook'.
  (defadvice skk-insert (after skk-e18-ad activate compile)
    (skk-e18-pre-command))

  (defadvice skk-previous-candidate (after skk-e18-ad activate compile)
    (skk-e18-pre-command))

  (defadvice skk-kana-input (before skk-e18-ad activate compile)
    ;; デバッグしていないバグのために work around する。
    (when (and skk-henkan-active
	       skk-kakutei-early
	       (not skk-process-okuri-early))
      (skk-kakutei)))

  (defadvice skk-kakutei (around skk-e18-ad activate compile)
    ;; skk-tut を利用しているときなど、`skk-kakutei' の前後で `skk-jisyo' の
    ;; 値が変わってしまうことがある。まだデバッグしていないため、work around
    ;; する。
    (let ((skk-jisyo skk-jisyo))
      (when skk-henkan-on
	(unless skk-mode
	  (skk-mode 1)))
      ad-do-it)
    (skk-after-point-move)))

;; Other functions.
(defun-maybe frame-width (&optional frame)
  "Return number of columns available for display on FRAME.
If FRAME is omitted, describe the currently selected frame."
  ;; Note that this function will be defined in APEL 10.3.
  (screen-width))

(defun read-from-minibuffer (prompt &optional
				    initial-contents keymap read hist)
  "Read a string from the minibuffer, prompting with string PROMPT.
If optional second arg INITIAL-CONTENTS is non-nil, it is a string
  to be inserted into the minibuffer before reading input.
  If INITIAL-CONTENTS is (STRING . POSITION), the initial input
  is STRING, but point is placed at position POSITION in the minibuffer.
Third arg KEYMAP is a keymap to use whilst reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HIST is ignored in this implementatin."
  ;; This re-definition of `read-from-minibuffer' is intended to enable
  ;; `minibuffer-setup-hook' and `minibuffer-exit-hook'.  Not well tested.
  (let ((minibuf (get-minibuffer (minibuffer-depth)))
	map)
    (with-current-buffer minibuf
      ;; Note that `current-local-map' inside `minibuffer-setup-hook' should
      ;; return the 3rd arg KEYMAP.
      (use-local-map (or keymap minibuffer-local-map))
      ;;
      (when minibuffer-setup-hook
	(save-window-excursion
	  ;; Note that `(window-buffer (minibuffer-window))' should return
	  ;; the new minibuffer.
	  (set-window-buffer (minibuffer-window) (current-buffer))
	  (run-hooks 'minibuffer-setup-hook)))
      ;; The local keymap here will be passed to `si:read-from-minibuffer'.
      ;; if the 3rd arg KEYMAP is nil.
      (setq map (current-local-map)))
    ;; `minibuffer-exit-hook' should be called even on abnormai exits.
    (unwind-protect
	(si:read-from-minibuffer prompt
				 initial-contents
				 (or keymap map)
				 read)
      ;;
      (when minibuffer-exit-hook
	(with-current-buffer (if (buffer-live-p minibuf)
				 minibuf
			       (get-minibuffer (minibuffer-depth)))
	  (save-window-excursion
	    ;; Note that `(window-buffer (minibuffer-window))' should return
	    ;; the new minibuffer.
	    (set-window-buffer (minibuffer-window) (current-buffer))
	    (safe-run-hooks 'minibuffer-exit-hook)))))))

(defun safe-run-hooks (hook)
  ;; /* If we get an error while running the hook, cause the hook variable
  ;;    to be nil.  Also inhibit quits, so that C-g won't cause the hook
  ;;    to mysteriously evaporate. */
  (let ((inhibit-quit hook))
    (condition-case nil
	(run-hooks hook)
      (error
       (when (symbolp hook)
	 (set hook nil))))))

(defun get-minibuffer (depth)
  ;; /* Return a buffer to be used as the minibuffer at depth `depth'.
  ;;  depth = 0 is the lowest allowed argument, and that is the value
  ;;  used for nonrecursive minibuffer invocations */
  (let* ((name (format " *Minibuf-%d*" depth))
	 (buf (get-buffer name)))
    (cond
     ((not (buffer-live-p buf))
      (setq buf (get-buffer-create name))
      ;; Emulate Emacs 19.28.
      ;; /* Although the buffer's name starts with a space, undo should be
      ;;    enabled in it.  */
      (buffer-enable-undo buf))
     (t
      ;; reset_buffer() is called in get_minibuffer() also under Emacs 18.
      (save-current-buffer
	;; Emulate Emacs 19.34.
	(set-buffer buf)
	(kill-all-local-variables))))
    buf))

(defun skk-e18-make-local-map (map1 map2)
  ;; MAP1 と MAP2 の両方のキー定義が使えるキーマップを返す。
  ;; `set-keymap-parent' と同様の手法ではうまくいかないため、このような小細工
  ;; を弄している。
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

(defun skk-e18-pre-command ()
  ;; この関数は SKK 8.6 の `j-kana-input' のコードを流用している。
  ;;
  ;; Emacs 18 においては `pre-command-hook' を利用する手立てが無いため、旧来
  ;; の手法 (`read-char' で次の入力を捕まえる) によらざるを得ない。
  (condition-case nil
      (let ((char (if (and (setq char (read-char))
			   skk-henkan-on
			   (not skk-henkan-active)
			   ;; we must distinguish the two cases where
			   ;; SKK-ECHO is on and off
			   (= skk-henkan-start-point
			      (if skk-echo (1- (point)) (point)))
			   (< 64 char) (< char 91))
		      ;; this case takes care of the rare case where
		      ;; one types two characters in upper case
		      ;; consequtively.  For example, one sometimes
		      ;; types "TE" when one should type "Te"
		      (+ 32 char)
		    char)))
	(unless (memq (key-binding (char-to-string char))
		      skk-kana-cleanup-command-list)
	  (skk-kana-cleanup t))
	(setq unread-command-char char))
    (quit
     (if (skk-in-minibuffer-p)
	 (abort-recursive-edit)
       (keyboard-quit)))))

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
