;;; skk-comp.el --- 補完のためのプログラム
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;;               1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-comp.el,v 1.3 1999/10/03 05:36:20 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/10/03 05:36:20 $

;; This file is part of SKK.

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

;;; Commentary:

;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-foreword)
;; Elib version 1.0 required.
(require 'stack-m)

(defgroup skk-comp nil "SKK completion related customization."
  :prefix "skk-"
  :group 'skk )

;;; -- user variables
(defcustom skk-dabbrev-like-completion nil
  "*Non-nil であれば、見出し語の補完において、最後に補完された語について更に補完が行われる。
例えば、

  \"さ\" (,) -> \"さとう\" (,) -> \"さとうせんせい\"

nil であれば、先頭の文字を共通にする文字列について補完が行なわれる。
例えば、

  \"さ\" (,) -> \"さとう\" (,) -> \"さいとう\" (,) -> \"さくら\""
  :type 'boolean
  :group 'skk-comp )

(defcustom skk-completion-function 'skk-completion-original
  "*skk-completion で使用する関数。"
  :type 'function
  :group 'skk-comp )

(defcustom skk-previous-completion-function 'skk-previous-completion-original
  "*skk-previous-completion で使用する関数。"
  :type 'function
  :group 'skk-comp )

(defcustom skk-comp-load-hook nil
  "*skk-comp.el をロードした後にコールされるフック。"
  :type 'hook
  :group 'skk-comp )

;;; -- internal variables
;; ---- buffer local variables
;; 空文字列に対して skk-completion を呼ぶこともありうるので、"" を nil では代
;; 用できない。
(skk-deflocalvar skk-completion-word ""
  "補完すべき見出し語。
skk-dabbrev-like-completion が non-nil の場合は、常に最後に補完した見出し語が
代入される。" )
;; 辞書登録時ミニバッファで補完した場合、元のバッファに戻ったときに
;; skk-completion-word の値が破壊されていない方がベター。

;; skk-completion-stack はバッファローカル値であり、しかも stack-m.el では破壊
;; 的にリストを操作するので初期値は nil にしておく必要がある。
(skk-deflocalvar skk-completion-stack nil
  "補完した語を保存しておくスタック。
skk-previous-completion では、スタックからポップして以前に補完した語に戻る。" )

;;;###autoload
(defun skk-start-henkan-with-completion (arg)
  "▽モードで読みの補完を行なった後、変換する。
それ以外のモードではオリジナルのキーマップに割り付けられたコマンドをエミュレー
トする。"
  (interactive "*P")
  (if (and skk-henkan-on (not skk-henkan-active))
      (progn
        (skk-completion (not (eq last-command 'skk-completion)))
        (skk-start-henkan arg) )
    (skk-emulate-original-map arg) ))

;;;###autoload
(defun skk-completion (first)
  ;; skk-try-completion のサブルーチン。
  (funcall skk-completion-function first) )

(defun skk-completion-original (first)
  (let ((inhibit-quit t)
        skk-num-list
        completion-word c-word )
    (skk-kana-cleanup 'force)
    (and first (setq skk-completion-stack (stack-create)))
    (and (or first skk-dabbrev-like-completion)
	 (setq skk-completion-word
	       (buffer-substring-no-properties skk-henkan-start-point (point)) ))
    (and (string= skk-completion-word "")
	 (skk-error "空文字から補完することはできません！"
		    "Cannot complete an empty string!" ))
    ;; skk-completion-word はバッファローカル値なので、辞書バッファに移る前に
    ;; 一時変数に移し変えておく。
    (setq completion-word skk-completion-word)
    (with-current-buffer (skk-get-jisyo-buffer skk-jisyo)
      (if first (goto-char skk-okuri-nasi-min))
      (save-match-data
        ;; case-fold-search は、辞書バッファでは常に nil。
	(while
	    (and (not c-word)
		 (search-forward
		  (concat "\n"
			  (if skk-use-numeric-conversion
			      (skk-num-compute-henkan-key completion-word)
			    completion-word ))
		  nil t ))
	  (if (eq (following-char) ?\040) ;SPC
	      nil
	    (setq c-word (concat completion-word
				 (buffer-substring-no-properties
				  ;; 見出し語に空白は含まれない。" /" をサー
				  ;; チする必要はない。
				  (point) (1- (search-forward " ")) )))))))
    (and (not c-word) skk-abbrev-mode skk-use-look
	 (setq c-word (skk-look-completion)) )
    ;; 辞書バッファの外。
    (if (not c-word)
	(if skk-japanese-message-and-error
	    (error "\"%s\" で補完すべき見出し語は%sありません"
		   skk-completion-word (if first "" "他に") )
	  (error "No %scompletions for \"%s\""
		 (if first "" "more ") skk-completion-word ))
      (stack-push skk-completion-stack c-word)
      (delete-region skk-henkan-start-point (point))
      (insert c-word) )))

;;;###autoload
(defun skk-previous-completion ()
  ;; skk-abbrev-comma, skk-insert-comma のサブルーチン。直前に補完を行った見
  ;; 出しを挿入する。
  (funcall skk-previous-completion-function) )

(defun skk-previous-completion-original ()
  (let ((inhibit-quit t)
        c-word )
    (setq c-word (stack-pop skk-completion-stack))
    (if (string= c-word
                 (buffer-substring-no-properties skk-henkan-start-point (point)) )
        ;; ポップした語がバッファのポイント直前にある文字列と同じだったら 1 つ
        ;; 捨てる。
        (setq c-word (stack-pop skk-completion-stack)) )
    (if c-word
	(progn
	  (delete-region skk-henkan-start-point (point))
	  (insert c-word) )
      ;;(insert skk-completion-word)
      (skk-error "\"%s\"で補完すべき見出し語は他にありません"
                 "No more previous completions for \"%s\""
                 skk-completion-word ))
    (setq this-command 'skk-completion) ))

(run-hooks 'skk-comp-load-hook)

(provide 'skk-comp)
;;; Local Variables:
;;; End:
;;; skk-comp.el ends here
