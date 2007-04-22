;;; skk-dinsert.el --- SKK dynamic insert -*- coding: iso-2022-jp -*-

;; Copyright (C) 2002 Eiji Obata <obata@suzuki.kuee.kyoto-u.ac.jp>

;; Author: Eiji Obata <obata@suzuki.kuee.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-dinsert.el,v 1.10 2007/04/22 02:38:28 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2007/04/22 02:38:28 $

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
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; カーソルの前後の文字列や、任意の elisp の評価値によってバッファに挿
;; 入する文字を動的に決定するプログラムです。
;;
;; skk-dinsert-rule-list に、キー入力とそれに対応する条件のリストを書
;; いてください。数字の直後でのみ [-,.]を[ー、。]でなくそのまま入力し
;; たい場合には次のように .skk に書いてください。
;;
;;   (setq skk-dinsert-rule-list
;;         '(("." nil
;;            (("[0-9]" . ".")
;;             (t . skk-current-kuten)))
;;           ("," nil
;;            (("[0-9]" . ",")
;;             (t . skk-current-touten)))
;;           ("-" nil
;;            (("[0-9]" . "-")
;;             (t . "ー")))))
;;
;; また、SKK 本体に統合されるまでは、skk-dinsert-rule-list の設定より
;; も下の方に以下のコードを追加して下さい。
;;
;;   (when (locate-library "skk-dinsert")
;;     (require 'skk-dinsert)
;;     (setq skk-rom-kana-rule-list
;;           (append skk-rom-kana-rule-list
;;                   (let ((count -1))
;;                     (mapcar #'(lambda (r)
;;                                 (setq count (1+ count))
;;                                 (list (car r)
;;                                       (cadr r)
;;                                       `(lambda (arg)
;;                                          (skk-dinsert arg ,count))))
;;                             skk-dinsert-rule-list)))))
;;
;; さらに、このファイルを load-path の通ったディレクトリに置いて下さい。
;;
;;
;; 動的な入力を無効にしたい時は、M-x skk-toggle-dinsert して下さい。
;; 又、一時的に無効にしたい時は、Q の入力により▽モードに入って下さい。
;; 続く一文字目については無効になります。

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

(defvar skk-dinsert-mode t
  "*Non-nil であれば、`skk-dinsert' による動的な入力を有効にする。
nil であれば `skk-dinsert-rule-list' での t に対応する値を用いる。

isearch の際には、この変数の値によらず、動的な入力は無効になる。又、
Q (skk-set-henkan-point-subr) の入力によって▽モードに入ると、直後の
入力に限り無効になる。")

(defvar skk-dinsert-ignore-lf t
  "*Non-nil であれば、カーソル以前の文字列による条件判定を行頭で行なった場合、改行を無視する。
`skk-dinsert-rule-list' にて対応するオプションが指定してあると、そちらが優先される。")

(defvar skk-dinsert-rule-list nil
  "*`skk-dinsert' による動的な入力の条件リスト。

リストの各要素は、それぞれが一つの規則であり、下記の形式を満たしていなければ
ならない。

 (INPUT-STATE NEXT-STATE RULE-ALIST)

INPUT-STATE, NEXT-STATE の意味は `skk-rom-kana-base-rule-list' と同じであり、
SKK は INPUT-STATE を検出すると、RULE-ALIST に基づいてバッファに文字を挿入し、
続いて NEXT-STATE に状態を移したうえで、入力待ち状態となる。

RULE-ALIST は条件と、それが成立した時に出力される値の連想リストである。
それぞれのルールは

  (REGEXP looking-at b-regexp/ignore-lf limit s-exp . VAL)

又は

  (S-EXP . VAL)

の形式をとる。(REGEXP, S-EXP, VAL 以外は省略可能)

初めに、最初の要素が文字列である場合について説明する。
リストの各要素の意味は下記の通り。

  0th: 文字列を正規表現として扱い、カーソル周囲の文字列がこれにマッチするか判定
       する。
  1th: Non-nil であれば `looking-at' を用いてカーソル以後の文字列が REGEXP にマッ
       チするか判定する。nil であれば `re-search-backward' によりカーソル直前の
       文字列が REGEXP にマッチするか判定する。省略すると nil として扱う。
  2th: 1th の looking-at の値により異なる意味を持つ。
         looking-at の指定が Non-nil の時:
           nil であれば、カーソル直後の文字列が REGEXP にマッチするか判定する。
           Non-nil であれば、これを正規表現として扱い、`re-search-backward' によっ
           てカーソル移動をする。その後 `looking-at' を行ない REGEXP が文字列に
           マッチするか判定する。これはコンテキストに応じた入力をする場合に有効
           である(と思われる)。
         looking-at の指定が nil の時(`re-search-backward' によって判定する時):
           Non-nil であれば、行頭での入力において、その直前の文字(すなわち改行コー
           ド)を無視して REGEXP とマッチするか判定する。指定が無ければ
           `skk-dinsert-ignore-lf' の値を用いる。
  3th: `re-search-backward' を行なう際の検索範囲を指定する。これはマッチングの条
       件を絞ったり、パフォーマンスの低下を抑える目的に利用される。
       数値が指定されると、現在のカーソル位置より limit 文字前までを範囲として検
       索する。具体的には (- (point) limit) を `re-search-backward' に渡す。
       それ以外であると、S 式として評価した値を用いる。
         note:
           * 数値をそのまま渡したい場合には (quote 1) 等とする必要がある。
           * 評価した値がカーソル位置の point より大きいとエラーになる。
  4th: REGEXP によるマッチングが成功した際、この s-exp を評価する。
       S 式の中では arg と m-d を利用する事ができ、arg には `skk-dinsert' の引数
       が、m-d には REGEXP によるマッチングの際の (match-data) の内容が入ってい
       る。s-exp の評価値が nil であると、このルールは適用されない。
  5th: REGEXP によるマッチングが成立し、かつ s-exp(指定されていれば)の
       評価値が Non-nil であった時、この VAL がバッファに挿入される。

次に最初の要素が文字列以外の場合であるが、これは S 式として評価される。
評価値が Non-nil であればルールが適用され、対応する VAL がバッファに挿入される。

ルールは上から順に試され、最初に成立したものが適用される。

VAL には、以下の 3つの形式を指定できる。

文字列 -- これがバッファに挿入される。
関数名シンボル
       -- `skk-rom-kana-rule-list' で指定した場合と同様、引数付きで呼ばれる。
          返り値が文字列であればそれがバッファに挿入される。
変数名シンボル
       -- (`format' \"%S\" VAL) した値がバッファに挿入される。

特別な場合として nil を指定する事ができる。この時には、S-EXP 又は s-exp の評価値
が用いられ、これは上の 3つの形式の何れかである必要がある。

`skk-rom-kana-rule-list' とは異なり、アトムでない要素は指定できない。このため
\(\"カナ\" . \"かな\") とは指定できない。かなモード、カナモードによって条件分け
したい場合には、変数 `skk-hiragana', `skk-katakana' によって調べる事ができる。
  note: `skk-hiragana' は `skk-dinsert' の中においてのみ有効なローカル変数である。")


(defun skk-toggle-dinsert (&optional arg)
  "動的な入力の有効/無効を切り替える。"
  (interactive "P")
  (setq skk-dinsert-mode (cond ((null arg)
				(not skk-dinsert-mode))
			       ((> (prefix-numeric-value arg) 0)
				t)
			       (t
				nil))))

(defun skk-dinsert (arg idx)
  (let ((rule-alist (nth 2 (nth idx skk-dinsert-rule-list)))
	;; VAL に ("カナ" . "かな") の形式が書けないので
	;; せめて変数を提供してみる
	(skk-hiragana (and (not skk-katakana) skk-j-mode))
	val cnd)
    (if (or (not skk-dinsert-mode)
	    (and skk-henkan-mode
		 (= skk-henkan-start-point skk-kana-start-point))
	    (and skk-isearch-switch
		 (buffer-live-p skk-isearch-current-buffer)))
	;; isearch 又は 動的な入力をしないなら t に対応する値を使う
	(setq val (cdr (assq t rule-alist)))
      (catch 'return
	(dolist (cur-rule rule-alist)
	  (setq cnd (car cur-rule))
	  (cond
	   ((stringp cnd)		; REGEXP
	    (let (found s-exp m-d)
	      (setq val
		    (save-match-data
		      (save-excursion
			(let* ((i 0)
			       (regexp cnd)
			       (r cur-rule)
			       (v (progn
				    (while (not (atom r))
				      (setq i (1+ i)
					    r (cdr r)))
				    r))
			       l-a i-lf b-regexp lim pos)
			  ;; (0 1 2 3 4 . 5) の形式を採用しているので
			  ;; (nth n LIST) をするには i > n である事が必要
			  (ignore-errors
			    (setq l-a (nth 1 cur-rule)
				  i-lf (nth 2 cur-rule)
				  b-regexp i-lf
				  lim (nth 3 cur-rule)
				  s-exp (nth 4 cur-rule)))
			  ;; re-search-backward の limit 調整
			  (when lim
			    (setq lim
				  (if (numberp lim)
				      (- (point) lim) ; 負になっても ok
				    (eval lim))))
			  (cond
			   (l-a		; looking-at
			    (when b-regexp
			      (re-search-backward b-regexp lim t))
			    (when (looking-at regexp)
			      (setq found t
				    m-d (match-data))
			      v))
			   (t		; re-search-backward
			    (when (and (not (bobp))
				       (bolp)
				       (if (> i 2)
					   i-lf
					 skk-dinsert-ignore-lf))
			      (backward-char))
			    (setq pos (point))
			    (when (and (re-search-backward regexp lim t)
				       (= pos
					  (match-end 0)))
			      (setq found t
				    m-d (match-data))
			      v)))))))
	      ;; match-data を用いて出力を生成 or 条件判定
	      ;; skk-dinsert 自身の引数は arg
	      ;; match-data の内容は m-d
	      (when (and found
			 s-exp)
		(setq val
		      (let ((retval (eval s-exp)))
			(when retval
			  (or val
			      retval))))))
	    (when val
	      (throw 'return nil)))
	   (t				; S-EXP
	    (let ((retval (eval cnd))
		  (v (cdr cur-rule)))
	      (setq val
		    (when retval
		      (or v
			  retval))))
	    (when val
	      (throw 'return nil)))))))
    (cond ((stringp val)
	   val)
	  ((functionp val)
	   (funcall val arg))
	  (t
	   (format "%S" val)))))


(provide 'skk-dinsert)
