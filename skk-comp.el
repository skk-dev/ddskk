;;; skk-comp.el --- 補完のためのプログラム

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;;               1999, 2000
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-comp.el,v 1.51 2006/01/11 20:12:04 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2006/01/11 20:12:04 $

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
    (when first
      (setq skk-comp-search-done nil
	    skk-comp-stack nil
	    skk-comp-depth 0
	    skk-comp-prefix skk-prefix)
      (when (skk-comp-prefix-unnecessary-p skk-comp-prefix)
	(setq skk-comp-prefix "")))
    (skk-kana-cleanup 'force)
    (when first
      (setq skk-comp-key (buffer-substring-no-properties
			  skk-henkan-start-point (point))))
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
	    (let ((word (skk-comp-get-candidate first)))
	      (while (member word skk-comp-stack)
		(setq word (skk-comp-get-candidate)))
	      word))
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
      (when skk-comp-circulate
	(delete-region skk-henkan-start-point (point))
	(insert skk-comp-key))
      (unless silent
	(ding)
	(cond
	 ((and (string= skk-comp-key "")
	       (or (not skk-comp-use-prefix)
		   (string= skk-comp-prefix "")))
	  (skk-message "これ以上の履歴はありません"
		       "No more words in history"))
	 (t
	  (if skk-japanese-message-and-error
	      (message "\"%s\" で補完すべき見出し語は%sありません"
		       (if skk-comp-use-prefix
			   (concat skk-comp-key skk-comp-prefix)
			 skk-comp-key)
		       (if first "" "他に"))
	    (message "No %scompletions for \"%s\""
		     (if first "" "more ")
		     (if skk-comp-use-prefix
			 (concat skk-comp-key skk-comp-prefix)
		       skk-comp-key))))))))))

(defun skk-comp-get-candidate (&optional first)
  (when first
    (setq skk-comp-first t
	  skk-current-completion-prog-list skk-completion-prog-list))
  (let (cand prog)
    (while (and (null cand)
		skk-current-completion-prog-list)
      (setq prog (car skk-current-completion-prog-list))
      (setq cand (eval prog)
	    skk-comp-first nil)
      (unless cand
	(setq skk-current-completion-prog-list
	      (cdr skk-current-completion-prog-list))
	(setq skk-comp-first t)))
    cand))

;; for test or backend use
;;;###autoload
(defun skk-comp-get-all-candidates (key prefix prog-list)
  (let ((skk-comp-key key)
	(skk-comp-prefix prefix)
	(skk-completion-prog-list prog-list)
	skk-current-completion-prog-list
	skk-comp-first
	ret)
    (setq ret (list (skk-comp-get-candidate 'first)))
    (while skk-current-completion-prog-list
      (setq ret (cons (skk-comp-get-candidate)
		      ret)))
    (nreverse (cdr ret))))

(defun skk-comp-prefix-unnecessary-p (prefix)
  ;; skk-kana-input-search-function は多分考慮しなくていいと思うが確信は無い
  (let ((tree skk-rule-tree))
    (dolist (c (string-to-char-list prefix))
      (setq tree (skk-select-branch tree c)))
    (skk-get-kana tree)))

;;;###autoload
(defun skk-comp-from-jisyo (file)
  ;; skk-comp-prefix は使わない版
  "SKK 辞書フォーマットの FILE から補完候補を得る。"
  (let ((buffer (skk-get-jisyo-buffer file 'nomsg))
	(abbrev skk-abbrev-mode)
	(key skk-comp-key)
	(first skk-comp-first)
	word)
    (with-current-buffer buffer
      (when first
	(goto-char skk-okuri-nasi-min))
      ;; 従来の挙動に合わせておくが、確定履歴だけでなく
      ;; 個人辞書も使えたほうがいいのかどうか
      (unless (string= key "")
	;; 中身をごっそり取り込もうかと思ったけど
	;; とりあえず利用することに
	(skk-comp-search-current-buffer key abbrev)))))

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
		   (if skk-comp-use-prefix
		       (concat skk-comp-key skk-comp-prefix)
		     skk-comp-key))))))

;;;###autoload
(defun skk-comp-previous/next (ch)
  (setq this-command 'skk-comp-do)
  (cond ((eq ch skk-next-completion-char)
	 (skk-comp-do nil))
	((eq ch skk-previous-completion-char)
	 (skk-previous-completion))))

;;;###autoload
(defun skk-comp-by-history ()
  ;; skk-comp-prefix を考慮
  "入力が空の時に履歴から補完する。
対象は現在の Emacs のセッションにおいて行なった送り無し変換のうち、
`skk-kakutei-history-limit' で指定される最近のものである。"
  (when (and (string= skk-comp-key "")
	     (or (not skk-comp-use-prefix)
		 (string= skk-comp-prefix "")))
    (when skk-comp-first
      (let (list
	    el)
	(dolist (cell skk-kakutei-history)
	  (setq el (car cell))
	  (unless (member el list)
	    (push el list)))
	(setq skk-comp-kakutei-midasi-list
	      (nreverse list))))
    (pop skk-comp-kakutei-midasi-list)))

;;;###autoload
(defun skk-completion-search (comp-prog-list &optional search-prog-list without-midasi)
  "変換キーで補完を行い、得られた各見出しでさらに検索する。
COMP-PROG-LIST は `skk-completion-prog-list' と同じ形式で、
これに含まれる補完関数によって、まず変換キーから見出しのリストを得る。
SEARCH-PROG-LIST は `skk-search-prog-list' と同じ形式で、
補完関数によって得た見出しをこれに含まれる検索関数により変換候補を得る。
デフォルトでは、補完によって得られた見出しと対応する候補はセットであるが、
WITHOUT-MIDASI を指定すると見出しは省かれる。"
  (when (eq (aref skk-henkan-key (1- (length skk-henkan-key)))
	    skk-completion-search-char)
    (let* ((key (substring skk-henkan-key 0 (1- (length skk-henkan-key))))
	   (midasi-list (skk-comp-get-all-candidates key "" comp-prog-list))
	   tmp words)
      (dolist (midasi midasi-list)
	(setq tmp (skk-search-progs midasi
				    (or search-prog-list
					skk-search-prog-list)))
	(when tmp	; 補完対象と検索対象は独立なので存在しない事も
	  (unless without-midasi
	    (setq words (nconc words (list midasi))))
	  ;; SKK 本体で skk-nunion してるのでここでは高速性重視
	  (setq words (nconc words tmp))))
      words)))

(defalias 'skk-previous-completion 'skk-comp-previous)
(defalias 'skk-start-henkan-with-completion 'skk-comp-start-henkan)

(run-hooks 'skk-comp-load-hook)

(require 'product)
(product-provide
    (provide 'skk-comp)
  (require 'skk-version))

;;; skk-comp.el ends here
