;;; skk-comp.el --- 補完のためのプログラム
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;;               1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-comp.el,v 1.7 2000/10/30 22:10:14 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/10/30 22:10:14 $

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
(defun skk-start-henkan-with-completion (arg)
  "▽モードで読みの補完を行なった後、変換する。
それ以外のモードではオリジナルのキーマップに割り付けられたコマンドをエミュレー
トする。"
  (interactive "*P")
  (if (and skk-henkan-on (not skk-henkan-active))
      (progn
        (skk-completion (not (eq last-command 'skk-completion)))
        (skk-start-henkan arg))
    (skk-emulate-original-map arg)))

;;;###autoload
(defun skk-completion (first)
  ;; skk-try-completion のサブルーチン。
  (let ((inhibit-quit t)
	;; skk-num が require されてないと buffer-local 値を壊す恐れあり。
        skk-num-list c-word)
    (skk-kana-cleanup 'force)
    (and first (setq skk-completion-stack nil skk-completion-depth 0))
    (and (or first skk-dabbrev-like-completion)
	 (setq skk-completion-word
	       (buffer-substring-no-properties skk-henkan-start-point (point))))
    (and (string= skk-completion-word "")
	 (skk-error "空文字から補完することはできません！"
		    "Cannot complete an empty string!"))
    (if (> skk-completion-depth 0)
	;; (過去に探索済みの読みをアクセス中)
	(setq skk-completion-depth (1- skk-completion-depth)
	      c-word (nth skk-completion-depth skk-completion-stack))
      ;; (新規の読みを辞書バッファから探索)
      ;; skk-completion-word はバッファローカル値なので、辞書バッファに移る前に
      ;; 一時変数に移し変えておく。
      (and (setq c-word
		 (or (skk-completion-1 skk-completion-word first)
		     (and skk-abbrev-mode skk-use-look (skk-look-completion))))
	   ;; 新規に見つけたときだけ cons する。
	   (setq skk-completion-stack (cons c-word skk-completion-stack))))
    ;; 辞書バッファの外。
    (if (not c-word)
	(progn
	  (setq skk-completion-depth (1+ skk-completion-depth))
	  (if skk-japanese-message-and-error
	      (error "\"%s\" で補完すべき見出し語は%sありません"
		     skk-completion-word (if first "" "他に"))
	    (error "No %scompletions for \"%s\""
		   (if first "" "more ") skk-completion-word)))
      (delete-region skk-henkan-start-point (point))
      (insert c-word))))

(defun skk-completion-1 (key first)
  (let (c-word)
    (with-current-buffer (skk-get-jisyo-buffer skk-jisyo)
      (if first (goto-char skk-okuri-nasi-min))
      (save-match-data
	;; case-fold-search は、辞書バッファでは常に nil。
	(while (and (not c-word)
		    (search-forward (concat "\n"
					    (if skk-use-numeric-conversion
						(skk-num-compute-henkan-key key)
					      key))
				    nil t))
	  (or (eq (following-char) ?\040) ;SPC
	      (setq c-word (concat key
				   (buffer-substring-no-properties
				    ;; 見出し語に空白は含まれない。" /" をサー
				    ;; チする必要はない。
				    (point) (1- (search-forward " ")))))))
	c-word))))

;;;###autoload
(defun skk-previous-completion ()
  ;; skk-abbrev-comma, skk-insert-comma のサブルーチン。直前に補完を行った見
  ;; 出しを挿入する。
  (let ((inhibit-quit t)
        (c-word 
	 (progn
	   (setq skk-completion-depth (1+ skk-completion-depth))
	   (nth skk-completion-depth skk-completion-stack))))
    (if c-word
	(progn
	  (delete-region skk-henkan-start-point (point))
	  (insert c-word))
      (setq skk-completion-depth (1- skk-completion-depth))
      (skk-error "\"%s\"で補完すべき見出し語は他にありません"
                 "No more previous completions for \"%s\""
                 skk-completion-word))))

(run-hooks 'skk-comp-load-hook)

(require 'product)
(product-provide (provide 'skk-comp) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-comp.el ends here
