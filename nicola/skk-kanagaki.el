;;; skk-kanagaki.el --- SKK の仮名入力サポート
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese

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

;; {てっとり早い使いかた (暫定バージョン)}
;;
;; ~/.skk に
;;
;; (setq skk-use-kana-keyboard)
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
;; のように入力します。[fj] とは f と j を 同時に打鍵することです。詳しくは
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
  (require 'skk-kanagaki-util)
  (require 'skk-macs)
  (require 'skk-vars)
  (require 'static))

(require 'nicola-ddskk-autoloads)

(static-unless (memq skk-emacs-type '(nemacs mule1))
  (when window-system
    (require 'skk-kanagaki-menu)))

(defgroup skk-kanagaki nil "SKK kanagaki related customization."
  :prefix "skk-kanagaki-"
  :group 'skk)

;; Variables.

(defcustom skk-use-kana-keyboard t "\
*Non-nil なら仮名入力用の設定を読み込む。
SKK 使用中にこの変数の値を切り替えることで  ローマ字入力 ⇔ 仮名入力 の切り替
えができる。"
  :type 'boolean
  :group 'skk
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-keyboard-type '106-jis "\
*仮名入力に使用するキーボードの種別。
値は任意のシンボル。 ただし `skk-kanagaki-{シンボル名}-base-rule-list' という
変数を用意しなければならない。何も設定しなければ日本語 106 キーボード用の設定
を用意し、これを使用する。"
  :type '(choice (const 106-jis)
		 (const nicola-jis)
		 (const nicola-us)
		 (const nicola-dvorak)
		 (const omelet-jis)
		 (const omelet-us)
		 (const omelet-dvorak)
		 (const oasys)
		 (symbol :tag "Another Keyboard Type"))
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-set-henkan-point-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[12~")
	(t
	 [f2])) "\
*このキーを押すことで変換開始位置を設定する。
変換開始位置の設定は仮名を入力する前におこなっても、 入力し終わった後でおこなっ
ても構わない。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-abbrev-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[17~")
	(t
	 [f6])) "\
*このキーを押すことで abbrev モードに入る。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-katakana-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[18~")
	(t
	 [f7])) "\
*このキーを押すことでカナモードとかなモードを切りかえる。
変換開始位置の設定後に押すことで対象文字列をカナに変換することもできる。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-jisx0208-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[19~")
	(t
	 [f8])) "\
*このキーを押すことで全角英数モードに入る。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-hankaku-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[20~")
	(t
	 [f9])) "\
*このキーを押すことで半角カナモードに切りかえる。
変換開始位置の設定後に押すことで対象文字列を半角カナに変換することもできる。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-mode-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[21~")
	(t
	 [f10])) "\
*このキーを押すことで latin モードに入る。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-toggle-rom-kana-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[24~")
	(t
	 [f12])) "\
*このキーを押すことで ローマ字入力 ⇔ 仮名入力の切り替えができる。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-code-input-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[15~")
	(t
	 [f5])) "\
*このキーを押すことでコード入力ができる。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-midashi-henkan-key
  (cond ((memq skk-emacs-type '(nemacs mule1))
	 "\e[13~")
	(t
	 [f3])) "\
*このキーを押すことで接頭辞または接尾辞変換をする。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-help-key "1" "\
*`help' においてヘルプを表示するキー。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-previous-candidate-key "\C-p" "\
*前候補を表示するためのキー。
XFree86 上で使用する場合、 例えばこの値を [henkan]  (XEmacs では
[henkan-mode]) にすれば、日本語キーボードの [前候補] キーに割り当てることがで
きる。 同キーは、Mule2.3@19.28 では [key-35]、Mule2.3@19.34 では [numbersign]
(なぜ ??) となるらしい。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-start-henkan-key " " "\
*変換を開始するためのキー。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-rule-list nil "\
*キー入力に対する変換文字の規則で、使用者の追加の設定を行なうもの。
例えば、 キー配列を独自に設定している場合などは、この変数を用いてそれに対応し
た設定をすることができる。"
  :type '(repeat
	  (list :tag "Rule"
		(string :tag "1 (keyboard input)")
		(choice :tag "2 (choose string if sokuon)"
			string
			(const nil))
		(choice :tag "3 (choice)"
			(symbol :tag "Function")
			(string :tag "String (common)")
			(cons :tag "Strings (katakana & hiragana)"
			 (string :tag "3-1 (katakana string)")
			 (string :tag "3-2 (hiragana string)")))))
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-jidou-keymap-kakikae-service nil "\
*Non-nil なら仮名入力のために勝手にキー配列を書換える。
X 上で xmodmap が実行可能な場合だけ有効。動作が改善される代わりに、他のモード
やアプリケーションにも キー配列の変更が影響するという副作用があるので、十分注
意して使ってください。"
  :type '(choice (const 106-jis)
		 (const 106-jis-kodawari)
		 (const nicola-jis)
		 (const oasys)
		 (const nil))
  :group 'skk-kanagaki)

;; Internal constants and variables.

(defvar skk-kanagaki-base-rule-list nil)
(defvar skk-kanagaki-rule-tree nil)
(defvar skk-kanagaki-rom-kana-rule-tree nil)

(defvar skk-kanagaki-state 'kana)

;; Functions.

(defalias-maybe 'help-mode 'fundamental-mode)

;;;###autoload
(defun skk-kanagaki-midashi-henkan (&optional arg)
  "接頭辞または接尾辞変換をする。"
  (interactive "*p")
  (cond (skk-henkan-active
	 (skk-kakutei)
	 (skk-set-henkan-point-subr)
	 (insert-and-inherit ?>))
	(skk-henkan-on
	 ;; 接頭語の処理
	 (skk-kana-cleanup 'force)
	 (insert-and-inherit ?>)
	 (skk-set-marker skk-henkan-end-point (point))
	 (setq skk-henkan-count 0
	       skk-henkan-key (buffer-substring-no-properties
			       skk-henkan-start-point (point))
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
     (cons (format "M-x help %s" skk-kanagaki-help-key) "このヘルプを表示"))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree) (cdr spec))
	  (list nil (car spec))
	  (str nil (when (memq
			  (nth 3 list)
			  '(skk-kanagaki-set-okurigana
			    skk-kanagaki-set-okurigana-no-sokuon))
		     (nth 1 list))))
	 ((or str (null spec))
	  (when (stringp str)
	    (cons str "送りあり変換開始"))))))))

;;;###autoload
(defun skk-kanagaki-insert (&optional arg)
  "SPC キーだけこれを `skk-insert' の代わりに使う。"
  (interactive "*p")
  (cond ((eq arg 1)
	 (let ((last-command-char ?\ ))
	   (skk-insert arg)))
	(t
	 ;; C-u [SPC] で送りあり変換をする。
	 (skk-kanagaki-set-okurigana-no-sokuon t))))

;;;###autoload
(defun skk-kanagaki-set-okurigana (&optional no-sokuon)
  "ポイントの直前の文字を送り仮名と見倣して、変換を開始する。
ただし、 もうひとつ前の文字が促音だった場合には、 それ以降を送り仮名と見倣す。"
  (interactive)
  (let ((pt1 (point))
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	pt2 okuri sokuon)
    (setq okuri
	  (save-excursion
	    ;; うう、こんなことをしなければならないのか...
	    (backward-char (* len 1))
	    (buffer-substring-no-properties (setq pt2 (point)) pt1)))
    (when okuri
      (unless no-sokuon
	(setq sokuon
	      (save-excursion
		(backward-char (* len 2))
		(buffer-substring-no-properties (point) pt2)))
	(unless (member sokuon '("っ" "ッ"))
	  (setq sokuon nil)))
      ;;
      (save-excursion
	(backward-char (* len (if sokuon 2 1)))
	(skk-set-marker skk-okurigana-start-point (point)))
      (setq skk-okuri-char (skk-okurigana-prefix okuri))
      (skk-set-okurigana))))

;;;###autoload
(defun skk-kanagaki-set-okurigana-no-sokuon (&optional arg)
  "ポイントの直前の文字を送り仮名と見倣して、変換を開始する。"
  (interactive "*p")
  (skk-kanagaki-set-okurigana (if (eq (prefix-numeric-value arg) 4) nil t)))

(defun skk-kanagaki-initialize ()
  "SKK 起動時の適当なタイミングで仮名入力用の設定を行う。"
  ;; 実際には `skk-regularize' の実行後、SKK の基本ルールが compile された後
  ;; に呼ばれる。
  (static-when (memq skk-emacs-type '(nemacs mule1))
    ;; Nemacs の canna.el より引用。
    (if (not (keymapp (global-key-binding "\e[")))
	(global-unset-key "\e[")))
  ;; 必要なモジュールをロード。
  (when skk-kanagaki-keyboard-type
    (require (intern (format "skk-%s" skk-kanagaki-keyboard-type))))
  ;; キーバインド。ただしこれは、より適切なキー定義を見つけるまでの暫定的処置。
  ;; ここで言う「より適切なキー定義」とは、入力方式に依存するため、SKK の重要
  ;; なキー定義をファンクションキーに残しておくことは、実用のためよりもむしろ
  ;; 参考のため。
  (dolist
      (cell
       '((skk-kanagaki-set-henkan-point-key . skk-set-henkan-point-subr)
	 (skk-kanagaki-abbrev-mode-key . skk-abbrev-mode)
	 (skk-kanagaki-katakana-mode-key . skk-toggle-kana)
	 (skk-kanagaki-latin-jisx0208-mode-key . skk-jisx0208-latin-mode)
	 (skk-kanagaki-hankaku-mode-key . skk-toggle-katakana)
	 (skk-kanagaki-latin-mode-key . skk-latin-mode)
	 (skk-kanagaki-code-input-key . skk-input-by-code-or-menu)
	 (skk-kanagaki-toggle-rom-kana-key . skk-kanagaki-toggle-rom-kana)
	 (skk-kanagaki-midashi-henkan-key . skk-kanagaki-midashi-henkan)
	 (skk-kanagaki-previous-candidate-key . skk-previous-candidate)))
    (when (and (symbol-value (car cell)) (commandp (cdr cell)))
      (define-key skk-j-mode-map
	(symbol-value (car cell)) (cdr cell))))
  ;;
  (let ((char (and (stringp skk-kanagaki-previous-candidate-key)
		   (string-to-char skk-kanagaki-previous-candidate-key))))
    (when (eq skk-previous-candidate-char ?x)
      ;; 既定値のままであるとき、適切に設定する。
      (setq skk-previous-candidate-char (or char
					    ;; C-p
					    (int-char 16)))))
  ;;
  (define-key help-map skk-kanagaki-help-key 'skk-kanagaki-help)
  ;;
  (static-unless (memq skk-emacs-type '(nemacs mule1))
    (eval-after-load "skk-jisx0201"
      '(when skk-kanagaki-hankaku-mode-key
	 (define-key skk-jisx0201-mode-map skk-kanagaki-hankaku-mode-key
	   'skk-toggle-katakana))))
  ;;
  (define-key skk-j-mode-map skk-kanagaki-start-henkan-key
    'skk-kanagaki-insert)
  ;;
  (unless skk-kanagaki-base-rule-list
    (setq skk-kanagaki-base-rule-list
	  (symbol-value (intern (format "skk-kanagaki-%s-base-rule-list"
				       skk-kanagaki-keyboard-type)))))
  (setq skk-kanagaki-rule-tree
	(skk-compile-rule-list
	 skk-kanagaki-base-rule-list skk-kanagaki-rule-list))
  (setq skk-kanagaki-rom-kana-rule-tree skk-rule-tree)
  ;;
  (add-hook 'skk-mode-hook
	    (function
	     (lambda ()
	       (skk-kanagaki-adjust-rule-tree))))
  ;; 句読点入力時の問題を回避。 日本語 106 キーボードでは "<" と ">" による接
  ;; 尾辞の入力はできなくなる。 "?" による接尾辞の入力はできる。
  (dolist (char skk-special-midashi-char-list)
    (when (and skk-use-kana-keyboard
	       (memq
		(nth 2 (assoc (skk-char-to-string char)
			      (symbol-value
			       (intern (format "skk-kanagaki-%s-base-rule-list"
					       skk-kanagaki-keyboard-type)))))
		'(skk-current-kuten skk-current-touten)))
      (setq skk-special-midashi-char-list
	    (delq char skk-special-midashi-char-list)))))

;; Pieces of advice.

(defadvice skk-insert (around skk-kanagaki-ad activate compile)
  "仮名入力用の work around 。"
  ;;
  (when (and skk-process-okuri-early
	     (eq skk-kanagaki-state 'kana))
    ;; `skk-process-okuri-early' が副作用を持つかも知れない。仮名入力ではそも
    ;; そも意味のないオプションなので強制的に off にする。
    (setq skk-process-okuri-early nil))
  ;;
  (if (eq  skk-kanagaki-state 'kana)
      (let (skk-set-henkan-point-key)
	ad-do-it)
    ad-do-it))

(defadvice skk-compute-henkan-lists (around skk-kanagaki-ad activate)
  (let ((okurigana (ad-get-arg 0)))
    (if (not okurigana)
	(setq ad-return-value
	      (list (split-string (buffer-substring-no-properties
				   (point) (progn (end-of-line) (1- (point))))
				  "/") nil nil nil))
      (save-match-data
	(let ((stage 1) (q1 (queue-create)) (q2 (queue-create))
	      (q3 (queue-create)) (q4 (queue-create))
	      (okuri-key (concat "\[" okurigana)) item headchar)
	  (catch 'exit
	    (while (not (eolp))
	      (setq item (buffer-substring-no-properties
			  (point)
			  (1- (search-forward "/")))
		    headchar (if (string= item "")
				 (int-char 0)
			       (skk-str-ref item 0)))
	      (cond ((and (eq headchar ?\[) (<= stage 2))
		     ;;
		     (when (and skk-use-kana-keyboard
				skk-henkan-okuri-strictly)
		       ;; 仮名入力用の特殊処理
		       (cond
			((eq skk-kanagaki-state 'kana)
			 ;; okuri-key が "っ" で item が "って" などだった
			 ;; 場合、item を okuri-key に置き換える。
			 (when (and
				(not (string= okuri-key item))
				(string-match
				 (concat "^" (regexp-quote okuri-key)) item))
			   (setq item okuri-key)))
			((eq skk-kanagaki-state 'rom)
			 ;; okuri-key が "って" で item が "っ" などだった
			 ;; 場合、item を okuri-key に置き換える。
			 (when (and
				(not (string= okuri-key item))
				(string-match
				 (concat "^" (regexp-quote item)) okuri-key))
			   (setq item okuri-key)))))
		     ;;
		     (if (string= item okuri-key)
			 (progn (queue-enqueue q2 item)
				(setq stage 3))
		       (setq stage 2)
		       (queue-enqueue q2 item)))
		    ((= stage 1)
		     (queue-enqueue q1 item))
		    ((= stage 2)
		     (queue-enqueue q2 item))
		    ((= stage 3)
		     (if (eq headchar ?\]) ; ?\]
			 (progn (setq stage 4)
				(queue-enqueue q4 item))
		       (queue-enqueue q3 item)))
		    ((= stage 4)
		     (queue-enqueue q4 item)))))
	  (setq ad-return-value
		(list (queue-all q1)
		      (queue-all q2)
		      (queue-all q3)
		      (queue-all q4))))))))

;;

(require 'product)
(product-provide (provide 'skk-kanagaki) (require 'skk-version))

;;; skk-kanagaki.el ends here
