;;; skk-comp.el --- 補完のためのプログラム

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;;               1999, 2000
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-comp.el,v 1.47 2006/01/04 10:10:45 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2006/01/04 10:10:45 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ▽さ (TAB) -> ▽さとう (.) -> ▽さいとう (,) -> ▽さとう(.) -> ▽さいとう

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

;;;###autoload
(defun skk-comp-start-henkan (arg)
  "▽モードで読みの補完を行なった後、変換する。
それ以外のモードではオリジナルのキーマップに割り付けられたコマンドをエミュレー
トする。"
  (interactive "*P")
  (cond
   ((eq skk-henkan-mode 'on)
    (skk-comp-do (not (eq last-command 'skk-comp-do)))
    (skk-start-henkan arg))
   (t
    (skk-emulate-original-map arg))))

;;;###autoload
(defun skk-comp (first &optional silent)
  (setq this-command 'skk-comp-do)
  (skk-comp-do first silent))

;;;###autoload
(defun skk-comp-do (first &optional silent)
  ;; main completion engine.
  (let ((inhibit-quit t)
	;; skk-num が require されてないと
	;; buffer-local 値を壊す恐れあり。
	skk-num-list
	c-word)
    (skk-kana-cleanup 'force)
    (when first
      (setq skk-comp-search-done nil
	    skk-comp-stack nil
	    skk-comp-depth 0))
    (when first
      (setq skk-comp-key (buffer-substring-no-properties
			  skk-henkan-start-point (point))))
    (when (and (not (memq skk-use-look '(nil conversion)))
	       skk-abbrev-mode
	       (not (memq skk-look-ignore-case '(nil conversion))))
      (setq skk-comp-key (downcase skk-comp-key)))
    (cond
     ;; (過去に探索済みの読みをアクセス中)
     (skk-comp-search-done
      (if (= skk-comp-depth 0)
	  ;; circulate ならば c-word = skk-comp-key なので c-word = nil
	  ;; non-circulate ならば これ以上候補がないので c-word = nil
	  (if skk-comp-circulate
	      (setq skk-comp-depth (length skk-comp-stack)))
	(setq skk-comp-depth (1- skk-comp-depth))
	(setq c-word (nth skk-comp-depth skk-comp-stack))))
     ;; (新規の読みを辞書バッファから探索)
     ;; skk-comp-key はバッファローカル値なので、辞書バッファに移る前に
     ;; 一時変数に移し変えておく。
     (t
      (setq c-word
	    (or (let ((word (skk-comp-do-1 skk-comp-key first)))
		  (while (member word skk-comp-stack)
		    (setq word (skk-comp-do-1 skk-comp-key first)))
		  word)
		;;
		(when (and skk-abbrev-mode
			   (not (memq skk-use-look '(nil conversion))))
		  (skk-look-completion))))
      (if c-word
	  ;; 新規に見つけたときだけ push する。
	  (push c-word skk-comp-stack)
	(setq skk-comp-search-done t)
	(if skk-comp-circulate
	    (setq skk-comp-depth (length skk-comp-stack))))))
    ;; 辞書バッファの外。
    (cond
     (c-word
      (delete-region skk-henkan-start-point (point))
      (insert c-word))
     (t
      ;; When skk-comp-circulate, return to the keyword.
      (when (or skk-comp-circulate
		(and (not (memq skk-use-look '(nil conversion)))
		     skk-abbrev-mode
		     (not (memq skk-look-ignore-case '(nil conversion)))))
	(delete-region skk-henkan-start-point (point))
	(insert skk-comp-key))
      (unless silent
	(ding)
	(cond
	 ((string= skk-comp-key "")
	  (skk-message "これ以上の履歴はありません"
		       "No more words in history"))
	 (t
	  (if skk-japanese-message-and-error
	      (message "\"%s\" で補完すべき見出し語は%sありません"
		       skk-comp-key
		       (if first "" "他に"))
	    (message "No %scompletions for \"%s\""
		     (if first "" "more ")
		     skk-comp-key)))))))))

;;;###autoload
(defun skk-comp-do-1 (key first)
  ;; skk-comp-1 のサブルーチン。
  (cond
   ((string= key "")
    (skk-comp-by-history))
   (t
    (let ((buffers (list (skk-get-jisyo-buffer skk-jisyo 'nomsg)
			 (skk-dic-setup-buffer)))
	  (abbrev skk-abbrev-mode)
	  word)
      (when first
	(skk-loop-for-buffers buffers
	  (goto-char skk-okuri-nasi-min)))
      (catch 'word
	(skk-loop-for-buffers buffers
	  (setq word (skk-comp-search-current-buffer key abbrev))
	  (if word
	      (throw 'word word)
	    nil)))))))

;;;###autoload
(defun skk-comp-search-current-buffer (key &optional abbrev)
  (let (c-word)
    (save-match-data
      ;; `case-fold-search' は、辞書バッファでは常に nil。
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
			 (1- (search-forward " ")))))
	  (when (and abbrev
		     (string-match "\\Ca" c-word))
	    ;; abbrev モードで「3ねん」などの補完はしない
	    (setq c-word nil))))
      c-word)))

;;;###autoload
(defun skk-comp-previous ()
  ;; skk-abbrev-comma, skk-insert-comma のサブルーチン。
  ;; 直前に補完を行った見出しを挿入する。
  (let ((inhibit-quit t)
	(stack-length (length skk-comp-stack))
	c-word)
    (if (and skk-comp-circulate (= skk-comp-depth stack-length))
	(setq skk-comp-depth 0)
      (setq skk-comp-depth (1+ skk-comp-depth)))
    (setq c-word (nth skk-comp-depth skk-comp-stack))
    (cond
     (c-word
      (delete-region skk-henkan-start-point (point))
      (insert c-word))
     (t
      (if (null skk-comp-circulate)
	  ;; non-circulate ならば skk-comp-depth が範囲外なので 1 戻す
	  (setq skk-comp-depth (1- skk-comp-depth))
	(delete-region skk-henkan-start-point (point))
	(insert skk-comp-key))
      ;;(setq skk-comp-depth (1- skk-comp-depth))
      (ding)
      (skk-message "\"%s\"で補完すべき見出し語は他にありません"
		   "No more previous completions for \"%s\""
		   skk-comp-key)))))

;;;###autoload
(defun skk-comp-previous/next (ch)
  (setq this-command 'skk-comp-do)
  (cond ((eq ch skk-next-completion-char)
	 (skk-comp-do nil))
	((eq ch skk-previous-completion-char)
	 (skk-previous-completion))))

;;;###autoload
(defun skk-comp-by-history ()
  (unless skk-comp-stack
    (let (list
	  el)
      (dolist (cell skk-kakutei-history)
	(setq el (car cell))
	(unless (member el list)
	  (push el list)))
      (setq skk-comp-kakutei-midasi-list
	    (nreverse list))))
  (pop skk-comp-kakutei-midasi-list))

(defalias 'skk-previous-completion 'skk-comp-previous)
(defalias 'skk-start-henkan-with-completion 'skk-comp-start-henkan)

(run-hooks 'skk-comp-load-hook)

(require 'product)
(product-provide
    (provide 'skk-comp)
  (require 'skk-version))

;;; skk-comp.el ends here
