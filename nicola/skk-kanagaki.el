;;; skk-kanagaki.el --- SKK の仮名入力サポート -*- coding: iso-2022-jp -*-

;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese, mule, input method

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

;; {てっとり早い使いかた (暫定バージョン)}
;;
;; ~/.skk に
;;
;; (setq skk-use-kana-keyboard t)
;; (setq skk-kanagaki-keyboard-type '106-jis)
;;
;; と書く。
;;
;;
;; {説明}
;;
;; このプログラムは SKK においてローマ字入力ならぬ仮名入力をサポートすること
;; を目的とします。 NICOLA や旧 JIS 配列に対応します。
;;
;; なお、以下は「親指シフト入力」以外の例です。親指シフト入力の例については、
;; README.NICOLA.ja と skk-nicola.el を御覧ください。
;;
;;  -*- 問題点 -*-
;;
;; 1. Emacs Lisp のレベルでの問題
;;
;; 仮名入力においては SHIFT キーを利用して入力される仮名もあるため、 SKK 本来
;; のSHIFT の使い方ができません。その他いろいろ SKK らしくないのですが、 とり
;; あえず、
;;
;;   o 変換開始点の指定は仮名入力とは別に行う。
;;   o 変換の開始は通常通り、 [SPC] で指示する。 ただし、送りありの変換のとき
;;     は 送り開始点を指定するための特殊な操作をする。
;;
;; してあります。例えば、「嬉しい」を入力するためには
;;
;; [fj] うれし [fj] い
;;
;; のように入力します。[fj] とは f と j を 同時に打鍵することです。
;;
;; 2. システムレベルでの問題
;;
;; 第 2 の問題点として、 キー配列の設定により刻印通りの入力ができない場合があ
;; ります。例えば日本語 106 キーボード使用時、XFree86 上では
;;
;; o 「￥」キー (仮想キーコード 133)
;; o 「＼」キー (仮想キーコード 123)
;;
;; はいずれも backslash として扱われます。しかし仮名入力において前者は 「ー」、
;; 後者は「ろ」 となることが望まれます。この場合の対応策として、例えば
;;
;; % cat >> ~/.Xmodmap
;;
;; keycode 123 = underscore underscore
;; % xmodmap ~/.Xmodmap
;;
;; などとしておいてから、~/.skk に
;;
;; (setq skk-kanagaki-rule-list
;;       '(("\\" nil "ー")))
;;
;; と書くことなどが考えられます。
;; (同様のアイデアは Canna で仮名入力する際にも有効であるようです。)
;;
;; もしあなたが XEmacs のベータテスターならば
;;
;; keycode 123 = kana_RO underscore
;; keycode 19 = 0 kana_WO
;;
;; なんて設定でとても幸せになれるかもしれません。 (Mr. XEmacs のしわざかな?)
;;
;; さらに、もしあなたが PC-98 ユーザ で XEmacs のベータテスターならば、おもむ
;; ろに「かな」キーをロックしてみてください。 ;')
;;
;;  -*- 使い方 -*-
;;
;; 1. 変換開始点の指定
;;
;; 通常の SKK においては、 SHIFT を押しながら入力することで変換開始位置を明示
;; していましたが、仮名入力ではこれができません。そこで、別の方法で変換開始点
;; を指定しなければなりません。そこで、「f と j を同時に押す」という手法を使い
;; ます。以下の [fj] は、同時打鍵を意味します。
;;
;; [fj] はる ⇒ ▽はる [SPC] ⇒ ▼春
;;
;; または
;;
;; はる ^B^B [fj] ⇒ ▽はる ^F^F [SPC] ⇒ ▼春
;;
;; 2. 送りありの変換のしかた
;;
;; 通常の SKK においては、 SHIFT を押しながら入力することで送り仮名の位置を明
;; 示していました。仮名入力 SKK においてはそれはできません。そこで
;;
;; o [fj] が押されたときに、 直前の 1 文字を送り仮名と見倣して変換を開始する。
;;
;; という手法を使います。 例えば、「達す」と入力したい場合は
;;
;; ▽たっす [fj]  ⇒ ▼達す
;;
;; のようになります。「待って」と入力したい場合は
;;
;; ▽まっ [fj] ⇒ ▼待っ
;;
;; としてから「て」を入力します。
;;
;; 3. いくつかの重要なキー定義について
;;
;; カナ入力が 「q」、 abbrev モードが 「/」、latin モードが 「l」などは定番で
;; すが、仮名入力ではこれも使えません。そのため、これらのうち重要と思われるも
;; のを別のキー定義にしてあります。C-h 3 と入力すると、現在のモードにおける特
;; 殊なキー定義を確認できます。
;; なお、これらは同時にファンクションキーに退避してあります。
;;
;; [f2]  … 変換開始点の指定
;; [f3]  … 接頭辞または接尾辞変換
;; [f5]  … コード入力
;; [f6]  … abbrev モード
;; [f7]  … カナモードまたはカナ変換
;; [f8]  … 全英モード
;; [f9]  … 半角カナモードまたは半角カナ変換
;; [f10] … latin モード
;; [f12] … ローマ字入力 ⇔ 仮名入力 の切り替え

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-dcomp)
  (require 'skk-kanagaki-util)
  (require 'skk-macs)
  (require 'skk-vars))

(require 'nicola-ddskk-autoloads)

(when window-system
  (require 'skk-kanagaki-menu))

;; Variables.

(defcustom skk-kanagaki-keyboard-type '106-jis "\
*仮名入力に使用するキーボードの種別。
値は任意のシンボル。 ただし `skk-kanagaki-{シンボル名}-base-rule-list' という
変数を用意しなければならない。何も設定しなければ日本語 106 キーボード用の設定
を用意し、これを使用する。"
  :type '(radio (const 106-jis)
		(const nicola-jis)
		(const nicola-us)
		(const nicola-dvorak)
		(const omelet-jis)
		(const omelet-us)
		(const omelet-dvorak)
		(const oasys)
		(symbol :tag "Another Keyboard Type"))
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-set-henkan-point-key [f2] "\
*このキーを押すことで変換開始位置を設定する。
変換開始位置の設定は仮名を入力する前におこなっても、 入力し終わった後でおこなっ
ても構わない。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-abbrev-mode-key [f6] "\
*このキーを押すことで abbrev モードに入る。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-katakana-mode-key [f7] "\
*このキーを押すことでカナモードとかなモードを切りかえる。
変換開始位置の設定後に押すことで対象文字列をカナに変換することもできる。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-jisx0208-mode-key [f8] "\
*このキーを押すことで全角英数モードに入る。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-hankaku-mode-key [f9] "\
*このキーを押すことで半角カナモードに切りかえる。
変換開始位置の設定後に押すことで対象文字列を半角カナに変換することもできる。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-mode-key [f10] "\
*このキーを押すことで latin モードに入る。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-toggle-rom-kana-key [f12] "\
*このキーを押すことで ローマ字入力 ⇔ 仮名入力の切り替えができる。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-code-input-key [f5] "\
*このキーを押すことでコード入力ができる。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-midashi-henkan-key [f3] "\
*このキーを押すことで接頭辞または接尾辞変換をする。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-help-key "1" "\
*\\[help] においてヘルプを表示するキー。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-previous-candidate-key "\C-p"
  "*前候補を表示するためのキー。
XFree86 上で使用する場合、 例えばこの値を [henkan]  (XEmacs では [henkan-mode])
にすれば、日本語キーボードの [前候補] キーに割り当てることができる。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-start-henkan-key " " "\
*変換を開始するためのキー。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-rule-list
  '((skk-kakutei-key nil skk-kakutei)) "\
*キー入力に対する変換文字の規則で、使用者の追加の設定を行なうもの。
例えば、 キー配列を独自に設定している場合などは、この変数を用いてそれに対応し
た設定をすることができる。"
  :type '(repeat
	  (list :tag "ルールリスト"
		(radio :tag "1 (キー入力)"
		       (string :tag "文字列")
		       (symbol :tag "変数名"))
		(radio :tag "2 (促音の場合「文字列」を選びます)"
		       (string :tag "文字列")
		       (const nil))
		(radio :tag "3 (いずれかを選ぶ)"
		       (symbol :tag "関数")
		       (string :tag "文字列 (カナ/かな共通)")
		       (cons :tag "文字列の組 (カナ . かな)"
			     (string :tag "3-1 (カナ)")
			     (string :tag "3-2 (かな)")))))
  :group 'skk-kanagaki)

;; Internal constants and variables.

(defvar skk-kanagaki-base-rule-list nil)
(defvar skk-kanagaki-rule-tree nil)
(defvar skk-kanagaki-rom-kana-rule-tree nil)

(defvar skk-kanagaki-state 'kana)

;; Hooks.

;; Functions.

(unless (fboundp 'help-mode)
  (defalias 'help-mode 'fundamental-mode))

;;;###autoload
(defun skk-kanagaki-midashi-henkan (&optional arg)
  "接頭辞または接尾辞変換をする。"
  (interactive "*p")
  (cond ((eq skk-henkan-mode 'active)
	 (skk-kakutei)
	 (let (skk-kakutei-history)
	   (skk-set-henkan-point-subr))
	 (insert-and-inherit ?>))
	((eq skk-henkan-mode 'on)
	 ;; 接頭語の処理
	 (skk-kana-cleanup 'force)
	 (insert-and-inherit ?>)
	 (skk-set-marker skk-henkan-end-point (point))
	 (setq skk-henkan-count 0
	       skk-henkan-key (buffer-substring-no-properties
			       skk-henkan-start-point
			       (point))
	       skk-prefix "")
	 (skk-henkan))))

;;;###autoload
(defun skk-kanagaki-help ()
  (interactive)
  (skk-kanagaki-help-1
   "* SKK 仮名入力 ヘルプ*"
   "現在の仮名入力モードの主なキー定義:"
   (append
    '((skk-kanagaki-set-henkan-point-key . "変換開始点をセット")
      (skk-kanagaki-midashi-henkan-key . "接頭辞 or 接尾辞変換")
      (skk-kanagaki-code-input-key . "コード入力")
      (skk-kanagaki-abbrev-mode-key . "abbrev モード")
      (skk-kanagaki-katakana-mode-key . "カナモード or カナ変換")
      (skk-kanagaki-latin-jisx0208-mode-key . "全英モード")
      (skk-kanagaki-hankaku-mode-key . "半角カナモード or 半角カナ変換")
      (skk-kanagaki-latin-mode-key . "latin モード")
      (skk-kanagaki-toggle-rom-kana-key . "ローマ字入力 ⇔ 仮名入力")
      (skk-kanagaki-previous-candidate-key . "前候補表示")
      (skk-kanagaki-start-henkan-key . "変換・次候補表示"))
    (list
     (cons (format "M-x help %s" skk-kanagaki-help-key)
	   "このヘルプを表示"))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree)
		(cdr spec))
	  (list nil (car spec))
	  (str nil (when (memq
			  (nth 3 list)
			  '(skk-kanagaki-set-okurigana
			    skk-kanagaki-set-okurigana-no-sokuon))
		     (nth 1 list))))
	 ((or str (null spec))
	  (when (stringp str)
	    (cons str "送りあり変換開始"))))))))

(defun skk-kanagaki-adjust-rule-tree ()
  (unless skk-kanagaki-rule-tree
    (setq skk-kanagaki-rule-tree
	  (skk-compile-rule-list
	   skk-kanagaki-base-rule-list
	   skk-kanagaki-rule-list)))
  (unless skk-kanagaki-rom-kana-rule-tree
    (setq skk-kanagaki-rom-kana-rule-tree
	  (or skk-rule-tree
	      (skk-compile-rule-list
	       skk-rom-kana-base-rule-list
	       skk-rom-kana-rule-list))))
  (let ((rule
	 (case skk-kanagaki-state
	   (kana
	    skk-kanagaki-rule-tree)
	   (t
	    skk-kanagaki-rom-kana-rule-tree))))
    (setq skk-rule-tree rule)
    (when (skk-local-variable-p 'skk-rule-tree)
      (setq-default skk-rule-tree rule))))

;;;###autoload
(defun skk-kanagaki-insert (&optional arg parg)
  "SPC キーだけこれを `skk-insert' の代わりに使う。"
  (interactive "*p")
  (cond
   ((or (integerp parg)
	;; C-u ではない場合
	(not (and parg (listp parg))))
    (skk-bind-last-command-char ?\ 
      (skk-insert arg parg)))
   (t
    ;; C-u [SPC] で送りあり変換をする。
    (when (featurep 'skk-dcomp)
      (skk-dcomp-cleanup-buffer))
    (skk-kanagaki-set-okurigana-no-sokuon t))))

;;;###autoload
(defalias 'skk-kanagaki-set-okurigana 'skk-set-char-before-as-okurigana)

;;;###autoload
(defun skk-kanagaki-set-okurigana-no-sokuon (&optional arg)
  "ポイントの直前の文字を送り仮名と見倣して、変換を開始する。"
  (interactive "*p")
  (skk-kanagaki-set-okurigana
   (not (eq 4 (prefix-numeric-value arg)))))

;;;###autoload
(defun skk-kanagaki-initialize ()
  "SKK 起動時の適当なタイミングで仮名入力用の設定を行う。"
  ;; 実際には `skk-regularize' の実行後、SKK の基本ルールが compile された後
  ;; に呼ばれる。

  ;; 必要なモジュールをロード。
  (when skk-kanagaki-keyboard-type
    (require (intern
	      (format "skk-%s"
		      skk-kanagaki-keyboard-type))))
  (unless skk-kanagaki-base-rule-list
    (setq skk-kanagaki-base-rule-list
	  (symbol-value (intern
			 (format
			  "skk-kanagaki-%s-base-rule-list"
			  skk-kanagaki-keyboard-type)))))
  ;;
  (add-hook 'skk-mode-hook
	    (function skk-kanagaki-adjust-rule-tree)
	    t)
  ;; 句読点入力時の問題を回避。 日本語 106 キーボードでは "<" と ">" による接
  ;; 尾辞の入力はできなくなる。 "?" による接尾辞の入力はできる。
  (dolist (char skk-special-midashi-char-list)
    (when (and skk-use-kana-keyboard
	       (memq (nth 2 (assoc
			     (skk-char-to-unibyte-string char)
			     (symbol-value
			      (intern
			       (format
				"skk-kanagaki-%s-base-rule-list"
				skk-kanagaki-keyboard-type)))))
		     '(skk-current-kuten skk-current-touten)))
      (setq skk-special-midashi-char-list
	    (delq char skk-special-midashi-char-list)))))

;; Pieces of advice.

(defadvice skk-setup-keymap (after skk-kanagaki-keys activate preactivate)
  ;; キーバインド。ただしこれは、より適切なキー定義を見つけるまでの暫定的処置。
  ;; ここで言う「より適切なキー定義」とは、入力方式に依存するため、SKK の重要
  ;; なキー定義をファンクションキーに残しておくことは、実用のためよりもむしろ
  ;; 参考のため。
  (dolist (cell '((skk-kanagaki-set-henkan-point-key
		   . skk-set-henkan-point-subr)
		  (skk-kanagaki-abbrev-mode-key
		   . skk-abbrev-mode)
		  (skk-kanagaki-katakana-mode-key
		   . skk-toggle-kana)
		  (skk-kanagaki-latin-jisx0208-mode-key
		   . skk-jisx0208-latin-mode)
		  (skk-kanagaki-latin-mode-key
		   . skk-latin-mode)
		  (skk-kanagaki-code-input-key
		   . skk-input-by-code-or-menu)
		  (skk-kanagaki-toggle-rom-kana-key
		   . skk-kanagaki-toggle-rom-kana)
		  (skk-kanagaki-hankaku-mode-key
		   . skk-toggle-katakana)
		  (skk-kanagaki-midashi-henkan-key
		   . skk-kanagaki-midashi-henkan)
		  (skk-kanagaki-previous-candidate-key
		   . skk-previous-candidate)))
    (when (and (symbol-value (car cell))
	       (commandp (cdr cell))
	       (or (eq (car cell) 'skk-kanagaki-previous-candidate-key)
		   (string-match "\\[f[1-9][1-9]\\]"
				 (format "%s" (symbol-value (car cell))))
		   (eq skk-j-mode-function-key-usage 'kanagaki)))
      (define-key skk-j-mode-map (symbol-value (car cell)) (cdr cell))))
  (define-key help-map skk-kanagaki-help-key 'skk-kanagaki-help))

(defadvice skk-insert (around skk-kanagaki-workaround activate compile)
  "仮名入力用の work around 。"
  ;;
  (when (and skk-process-okuri-early
	     (eq skk-kanagaki-state 'kana))
    ;; `skk-process-okuri-early' が副作用を持つかも知れない。仮名入力ではそも
    ;; そも意味のないオプションなので強制的に off にする。
    (setq skk-process-okuri-early nil))
  ;;
  (let ((skk-set-henkan-point-key
	 (cond
	  ((and (eq skk-kanagaki-state 'kana)
		(not skk-jisx0201-mode))
	   nil)
	  (t
	   skk-set-henkan-point-key))))
    ad-do-it))

(defadvice skk-compute-henkan-lists-sub-adjust-okuri (around
						      skk-kanagaki-adjust-okuri
						      activate compile)
  (cond
   (skk-use-kana-keyboard
    ;; 仮名入力用の特殊処理
    (let ((item (ad-get-arg 0))
	  (okuri-key (ad-get-arg 1)))
      (setq ad-return-value
	    (cond
	     ((or (and (eq skk-kanagaki-state 'kana)
		       ;; okuri-key が "っ" で item が "って" などだった場合。
		       (string-match (concat "^" (regexp-quote okuri-key))
				     item))
		  (and (eq skk-kanagaki-state 'rom)
		       ;; okuri-key が "って" で item が "っ" などだった場合。
		       (string-match (concat "^" (regexp-quote item))
				     okuri-key)))
	      okuri-key)
	     (t
	      item)))))
   (t
    ad-do-it)))

(provide 'skk-kanagaki)

;;; skk-kanagaki.el ends here
