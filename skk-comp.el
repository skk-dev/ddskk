;;; skk-comp.el --- 補完のためのプログラム

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;;               1999, 2000
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-comp.el,v 1.59 2006/02/05 19:22:51 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2006/02/05 19:22:51 $

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
        tmp-key data
	c-word)
    (when first
      (setq skk-comp-search-done nil
	    skk-comp-stack nil
	    skk-comp-depth 0
	    skk-comp-prefix skk-prefix)
      ;;  key  \ use-prefix    nil	  kakutei-first	  non-nil	 # data
      ;; "かk"		     "か"  , ""	   "か"	 , "k"	  "か", "k"	 #    t
      ;; "かn"		     "かん", ""	   "かん", ""	  "か", "n"	 # non-t
      (setq tmp-key (buffer-substring-no-properties
		     skk-henkan-start-point (point)))
      ;; skk-kana-cleanup() を呼ぶ前の key を取得
      (unless (or skk-abbrev-mode
		  (memq skk-comp-use-prefix '(nil kakutei-first)))
	(save-match-data
	  (if (string-match "^\\([^a-z]*\\)[a-z]*$" tmp-key)
	      (setq skk-comp-key (match-string 1 tmp-key))
	    ;; 送り無しで見出しにアルファベットを含むような変則的な候補は、
	    ;; skk-echo も考えるとまともな対処が面倒なので、
	    ;; 害が無い範囲で適当に処理。 nil か kakutei-first を使ってもらう。
	    (setq skk-comp-key tmp-key))))
      ;; prefix に対応する「かな」etc. があれば non-t
      ;; 副作用を伴なうルールが指定されているかもしれないので、
      ;; データがあるかどうかのチェックのみに使う。
      (setq data
	    (skk-kana-cleanup 'force))
      (when (or skk-abbrev-mode
		(memq skk-comp-use-prefix '(nil kakutei-first)))
	(setq skk-comp-key (buffer-substring-no-properties
			    skk-henkan-start-point (point)))
	(unless (and skk-comp-use-prefix
		     (eq data t))
	  (setq skk-comp-prefix ""))))
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
     ;; (新規の読みを探索)
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
	cand ret)
    (setq cand (skk-comp-get-candidate 'first))
    (when cand
      (setq ret (list cand))
      (while (setq cand (skk-comp-get-candidate))
	(unless (member cand ret)
	  (setq ret (cons cand ret)))))
    (nreverse ret)))

(defun skk-comp-get-regexp (prefix)
  ;; プレフィクスに対応する正規表現を返す。
  ;; 一度生成した正規表現は skk-comp-prefix-regexp-alist に保存しておく。
  (or (cdr (assoc prefix skk-comp-prefix-regexp-alist))
      (let ((regexp
	     (if (string= prefix "")
		 ""
	       (let ((tree skk-rule-tree)
		     kana-list)
		 (dolist (c (string-to-char-list prefix))
		   (setq tree (skk-select-branch tree c)))
		 (setq kana-list
		       (skk-comp-arrange-kana-list
			(skk-comp-collect-kana tree)
			prefix))
		 (condition-case nil
		     (regexp-opt kana-list 'with-paren)
		   (error
		    (if kana-list
			(concat "\\("
				(mapconcat #'regexp-quote kana-list "\\|")
				"\\)")
		      "")))))))
	(add-to-list 'skk-comp-prefix-regexp-alist (cons prefix regexp))
	regexp)))

(defun skk-comp-collect-kana (tree)
  ;; skk-rule-tree の部分木に属する "かな" を集める
  (let ((data (skk-get-kana tree))
	(branches (skk-get-branch-list tree))
	kana kana-list)
    (when data
      (setq kana (if (consp data)
		     (cdr data)
		   data))
      (when (stringp kana)
	(setq kana-list (list kana))))
    (nconc kana-list (apply #'nconc
			    (mapcar #'skk-comp-collect-kana
				    branches)))))

(defun skk-comp-arrange-kana-list (kana-list prefix)
  ;; skk-comp-collect-kana から得た "かな" のリストを元に
  ;; プレフィクスに対応した調整をする
  (let (short-list long-list tmp)
    (dolist (kana kana-list)
      (if (= (length kana) 1)
	  (add-to-list 'short-list kana)
	(add-to-list 'long-list kana)))
    ;; "に" がある時に "にゃ" とかはいらない
    (dolist (s-kana short-list)
      (dolist (l-kana long-list)
	(when (string= s-kana
		       (substring l-kana 0 1))
	  (setq long-list (delete l-kana long-list)))))
    (setq tmp (nconc short-list long-list))
    (if skk-comp-kana-list-filter-function
	(funcall skk-comp-kana-list-filter-function tmp prefix)
      tmp)))

;;;###autoload
(defun skk-comp-from-jisyo (file)
  ;; skk-comp-prefix を使える
  "SKK 辞書フォーマットの FILE から補完候補を得る。"
  (let ((buffer (skk-get-jisyo-buffer file 'nomsg))
	(abbrev skk-abbrev-mode)
	(key skk-comp-key)
	(prefix skk-comp-prefix)
	(first skk-comp-first)
	(use-prefix skk-comp-use-prefix))
    (with-current-buffer buffer
      (when first
	(goto-char skk-okuri-nasi-min))
      (if use-prefix
	  (unless (and (string= key "")
		       (string= prefix ""))
	    (skk-comp-re-search-current-buffer key prefix abbrev))
	(unless (string= key "")
	  (skk-comp-search-current-buffer key abbrev))))))

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

(defun skk-comp-re-search-current-buffer (key prefix &optional abbrev)
  ;; 問題のあるケースがあるかもしれないので
  ;; skk-comp-search-current-buffer との一本化はとりあえず保留
  (let (c-word regexp-key)
    (setq regexp-key (concat (regexp-quote
			      (if skk-use-numeric-conversion
				  (skk-num-compute-henkan-key key)
				key))
			     (skk-comp-get-regexp prefix)))
    (save-match-data
      ;; `case-fold-search' は、辞書バッファでは常に nil。
      (while (and (not c-word)
		  (re-search-forward
		   (concat "\n" regexp-key)
		   nil t))
	(beginning-of-line)
	(search-forward (if skk-use-numeric-conversion
			    (skk-num-compute-henkan-key key)
			  key))
	(unless (eq (following-char)
		    ?\040)		;SPC
	  (setq c-word
		(concat key
			(buffer-substring-no-properties
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

(defun skk-comp-restrict-by-prefix (comp-prog)
  "補完プログラムにより得られた候補を `skk-comp-prefix' で絞り込む。

  (skk-comp-restrict-by-prefix '(skk-comp-by-server-completion))
のようなものを `skk-completion-prog-list' に指定する。"
  (save-match-data
    (let ((regexp-key (concat "^"
			      (regexp-quote skk-comp-key)
			      (skk-comp-get-regexp skk-comp-prefix)))
	  cand)
      (setq cand (eval comp-prog))
      (when skk-comp-use-prefix
	(while (and cand
		    (not (string-match regexp-key cand)))
	  (let (skk-comp-first)
	    (setq cand (eval comp-prog)))))
      cand)))

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
	   (skk-comp-use-prefix nil)
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
