;;; skk-comp.el --- 補完のためのプログラム
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;;               1999, 2000
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-comp.el,v 1.18 2001/09/15 01:12:04 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/09/15 01:12:04 $

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
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;; ▽さ (TAB) -> ▽さとう (.) -> ▽さいとう (,) -> ▽さとう(.) -> ▽さいとう

;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars))

;;;###autoload
(defun skk-comp-start-henkan (arg)
  "▽モードで読みの補完を行なった後、変換する。
それ以外のモードではオリジナルのキーマップに割り付けられたコマンドをエミュレー
トする。"
  (interactive "*P")
  (if (and skk-henkan-on (not skk-henkan-active))
      (progn
	(skk-comp-do (not (eq last-command 'skk-comp-do)))
	(skk-start-henkan arg))
    (skk-emulate-original-map arg)))

;;;###autoload
(defun skk-comp (first &optional silent)
  (setq this-command 'skk-comp-do)
  (skk-comp-do first silent))

;;;###autoload
(defun skk-comp-do (first &optional silent)
  ;; main completion engine.
  (let ((inhibit-quit t)
	;; skk-num が require されてないと buffer-local 値を壊す恐れあり。
	skk-num-list c-word)
    (skk-kana-cleanup 'force)
    (and first (setq skk-comp-stack nil skk-comp-depth 0))
    (and (or first skk-dabbrev-like-completion)
	 (setq skk-comp-key
	       (buffer-substring-no-properties skk-henkan-start-point (point))))
    (if (> skk-comp-depth 0)
	;; (過去に探索済みの読みをアクセス中)
	(setq skk-comp-depth (1- skk-comp-depth)
	      c-word (nth skk-comp-depth skk-comp-stack))
      ;; (新規の読みを辞書バッファから探索)
      ;; skk-comp-key はバッファローカル値なので、辞書バッファに移る前に
      ;; 一時変数に移し変えておく。
      (and (setq c-word
		 (or (let ((word (skk-comp-do-1 skk-comp-key first)))
		       (if (member word skk-comp-stack)
			   (skk-comp-do-1 skk-comp-key first)
			 word))
		     (and skk-abbrev-mode skk-use-look (skk-look-completion))))
	   ;; 新規に見つけたときだけ cons する。
	   (setq skk-comp-stack (cons c-word skk-comp-stack))))
    ;; 辞書バッファの外。
    (if (not c-word)
	(progn
	  (setq skk-comp-depth (1+ skk-comp-depth))
	  (if silent
	      nil
	  (ding)
	  (if (string= skk-comp-key "")
	      (skk-message
	       "これ以上の履歴はありません"
	       "No more words on history")
	    (if skk-japanese-message-and-error
		(message "\"%s\" で補完すべき見出し語は%sありません"
			 skk-comp-key (if first "" "他に"))
	      (message "No %scompletions for \"%s\""
		       (if first "" "more ") skk-comp-key)))))
      (delete-region skk-henkan-start-point (point))
      (insert c-word))))

(defun skk-comp-do-1 (key first)
  ;; skk-comp-1 のサブルーチン。
  (when first
    (setq skk-dic-comp-first t))
  (cond
   ((string= key "")
    (skk-comp-by-history))
   (t
    (or (skk-comp-do-1-in-buf (skk-get-jisyo-buffer skk-jisyo)
			      key
			      first)
	(prog1
	    (skk-comp-do-1-in-buf (skk-dic-setup-buffer)
				  key
				  skk-dic-comp-first)
	  (setq skk-dic-comp-first nil))))))

(defun skk-comp-do-1-in-buf (buffer key first)
  (when (buffer-live-p buffer)
    (let (c-word)
      (with-current-buffer buffer
	(if first
	    (goto-char skk-okuri-nasi-min))
	(save-match-data
	  ;; case-fold-search は、辞書バッファでは常に nil。
	  (while (and (not c-word)
		      (search-forward
		       (concat "\n"
			       (if skk-use-numeric-conversion
				   (skk-num-compute-henkan-key key)
				 key))
		       nil t))
	    (unless (eq (following-char)
			?\040) ;SPC
	      (setq c-word
		    (concat key
			    (buffer-substring-no-properties
			     ;; 見出し語に空白は含まれない。
			     ;; " /" をサーチする必要はない。
			     (point)
			     (1- (search-forward " ")))))))
	  c-word)))))

;;;###autoload
(defun skk-comp-previous ()
  ;; skk-abbrev-comma, skk-insert-comma のサブルーチン。直前に補完を行った見
  ;; 出しを挿入する。
  (let ((inhibit-quit t)
	(c-word
	 (progn
	   (setq skk-comp-depth (1+ skk-comp-depth))
	   (nth skk-comp-depth skk-comp-stack))))
    (if c-word
	(progn
	  (delete-region skk-henkan-start-point (point))
	  (insert c-word))
      (setq skk-comp-depth (1- skk-comp-depth))
      (ding)
      (skk-message "\"%s\"で補完すべき見出し語は他にありません"
		   "No more previous completions for \"%s\""
		   skk-comp-key))))

;;;###autoload
(defun skk-comp-previous/next (ch)
  (setq this-command 'skk-comp-do)
  (cond ((eq ch skk-next-completion-char)
	 (skk-comp-do nil))
	((eq ch skk-previous-completion-char)
	 (skk-previous-completion))))

(defun skk-comp-by-history ()
  (unless skk-comp-stack
    (let ((hist skk-kakutei-history)
	  list el)
      (while hist
	(setq el (caar hist))
	(unless (member el list)
	  (setq list (cons el list)))
	(setq hist (cdr hist)))
      (setq skk-comp-kakutei-midasi-list (nreverse list))))
  (prog1
      (car skk-comp-kakutei-midasi-list)
    (setq skk-comp-kakutei-midasi-list (cdr skk-comp-kakutei-midasi-list))))

(defalias 'skk-previous-completion 'skk-comp-previous)
(defalias 'skk-start-henkan-with-completion 'skk-comp-start-henkan)

(run-hooks 'skk-comp-load-hook)
(require 'product)
(product-provide (provide 'skk-comp) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-comp.el ends here
