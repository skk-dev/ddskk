;;; skk-kanagaki.el --- SKK の仮名入力サポート
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese

;; This file is part of SKK (Simple Kana to Kanji conversion program).

;; SKK  is free software;  you  can redistribute it  and/or modify it under the
;; terms  of the GNU General Public License  as published  by the Free Software
;; Foundation;  either versions  2,  or  (at your option)  any  later version.

;; SKK  is distributed  in the hope  that  it will  be useful  but  WITHOUT ANY
;; WARRANTY;  without even the implied  warranty  of MERCHANTABILITY or FITNESS
;; FOR  A  PARTICULAR  PURPOSE.  See  the  GNU General Public License  for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; SKK,  see the file COPYING.  If not,  write  to the Free Software Foundation
;; Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

;;; Commentary:
;;
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
;; このプログラムは  SKK においてローマ字入力ならぬ仮名入力をサポートすることを
;; 目的とします。 AT 互換機用の日本語 106 キーボードは普通 JIS 配列の刻印があり
;; ますが、まずはこれに対応する予定です。PC-98 への対応はこれを少し変更すればで
;; きると思います。
;;
;;  -*- 問題点 -*-
;;
;; 1. Emacs Lisp のレベルでの問題
;;
;; 仮名入力においては SHIFT キーを利用して入力される仮名もあるため、 SKK 本来の
;; SHIFT の使い方ができません。その他いろいろ SKK らしくないのですが、 とりあえ
;; ず、
;;
;;   o 変換開始点の指定は仮名入力とは別に行う。
;;   o 変換の開始は通常通り、 [SPC] で指示する。 ただし、送りありの変換のときは
;;     C-u [SPC] として明示する。
;;
;; のようにしてあります。例えば、「嬉しい」を入力するためには、
;;
;; `Q' うれし C-u [SPC] い
;;
;; のように打ちます。
;; (改善の余地があると思いますが、とりあえずアイデアがここで尽きています。)
;;
;; 2. システムレベルでの問題
;;
;; 第 2 の問題点として、 キー配列の設定により刻印通りの入力ができない場合があり
;; ます。例えば日本語 106 キーボード使用時、XFree86 上では
;;
;; o 「￥」キー (仮想キーコード 133)
;; o 「＼」キー (仮想キーコード 123)
;;
;; はいずれも backslash として扱われます。 しかし仮名入力において前者は 「ー」、
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
;; さらに、もしあなたが PC-98 ユーザ で XEmacs のベータテスターならば、おもむろ
;; に「かな」キーをロックしてみてください。 ;')
;;
;;  -*- 使い方 -*-
;;
;; 送りなしの変換については、通常の SKK と同様なので省略します。
;;
;; 1. 変換開始点の指定
;;
;; 通常の SKK においては、 SHIFT を押しながら入力することで変換開始位置を明示し
;; ていましたが、仮名入力ではこれができません。そこで、"Q" を押すことで変換開始
;; 位置を指定する必要があります。 要は、abbrev モードと同様のやり方で変換を開始
;; することになります。例えば「春」の入力は
;;
;; `Q' はる ⇒ ▽はる [SPC] ⇒ ▼春
;;
;; または
;;
;; はる ^B^B `Q' ⇒ ▽はる ^F^F [SPC] ⇒ ▼春
;;
;; のいずれかでできます。
;;
;; 2. 送りありの変換のしかた
;;
;; 通常の SKK においては、 SHIFT を押しながら入力することで送り仮名の位置を明示
;; していました。仮名入力 SKK においてはそれはできません。そこで
;;
;; o `S' が押されたときに、 直前の 1 文字を送り仮名と見倣して変換を開始する。
;;
;; という風にしてみました。 例えば、「達す」と入力したい場合は
;;
;; ▽たっす `S'  ⇒ ▼達す
;;
;; のようになります。「待って」と入力したい場合は
;;
;; ▽まっ `S' ⇒ ▼待っ
;;
;; としてから「て」を入力します。
;;
;; (追記)
;;
;; `S' の代わりに C-u [SPC] でも送りあり変換ができます。
;;
;; 3. いくつかの重要なキー定義について
;;
;; カナ入力が 「q」、 abbrev モードが 「/」、latin モードが 「l」などは定番です
;; が、仮名入力ではこれも使えません。仮名入力できるキーボードでファンクションキ
;; ーが使えないことはあまり無いだろうと思い、デフォルトではこれらをファンクショ
;; ンキーに退避してみました。デフォルトでは以下のようになっています。ユーザオプ
;; ションなので自由に変更できます。
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
;;
;; これらのキー定義は一時的なものです。よりよいキー定義を考えて修正する予定であ
;; り、ユーザのご意見に期待しています。
;;
;; {TODO}
;;
;; o 個別のキーボード配列への対応コードはそれぞれ別ファイルにして、モジュール的
;;   にロードすることにした。そこで、今後はできるだけモジュールを追加する。
;; o とりあえず、NICOLA 配列と omelet 独自配列への対応を完了したい。

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
*Non-nil なら仮名入力用の設定をロードする。
SKK 使用中にこの変数の値を切り替えることで  ローマ字入力 ⇔ 仮名入力 のトグルが
できる。"
  :type 'boolean
  :group 'skk
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-keyboard-type '106-jis "\
*仮名入力に使用するキーボードのタイプ。
値は任意のシンボル。 ただし  `skk-kanagaki-{シンボル名}-base-rule-list'  という
変数を用意しなければならない。デフォルトでは日本語 106 キーボード用の設定を用意
し、これを使用する。"
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
*このキーを押すことで ローマ字入力 ⇔ 仮名入力のトグルができる。"
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

(defcustom skk-kanagaki-previous-candidate-key "x" "\
*前候補を表示するためのキー。
XFree86 上で使用する場合、 例えばこの値を [henkan]  (XEmacs では [henkan-mode])
にすれば、日本語キーボードの [前候補] キーに割り当てることができる。 同キーは、
Mule2.3@19.28 では [key-35]、Mule2.3@19.34 では [numbersign] (なぜ ??) となるら
しい。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-start-henkan-key " " "\
*変換を開始するためのキー。"
  :type 'sexp
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-rule-list nil "\
*キー入力に対する変換文字の規則で、ユーザーの追加の設定を行なうもの。
例えば、 仮想キーコードに対するシンボルを独自に設定している場合などは、この変数
を用いてそれに対応した設定をすることができる。"
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

(defcustom skk-kanagaki-jidou-key-symbol-kakikae-service nil "\
*Non-nil なら仮名入力のために勝手にキーテーブルを書換える。
X 上で xmodmap がインストールされている場合だけ有効。動作が改善される代わりに、
他のモードやアプリケーションにも キーテーブルの変更が影響するという副作用がある
ので、十分注意して使ってください。"
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

(defvar skk-kanagaki-temp-dir
  (static-cond
   ((fboundp 'temp-directory)
    (temp-directory))
   ((and (boundp 'temporary-file-directory) temporary-file-directory)
    temporary-file-directory)
   (t
    (or (getenv "TMP") "/tmp"))))

(defvar skk-kanagaki-isearch-buffer nil)

(skk-deflocalvar skk-kanagaki-state 'kana)

;; Functions.

(defalias-maybe 'help-mode 'fundamental-mode)

;;;###autoload
(defun skk-kanagaki-toggle-rom-kana (&optional arg)
  "ローマ字入力 ⇔ 仮名入力 を切り替える。"
  (interactive)
  (setq skk-kanagaki-state
	(if (memq arg '(kana rom))
	    arg
	  (case skk-kanagaki-state
	    (kana 'rom)
	    (rom 'kana)
	    ;; とりあえず。
	    (t 'kana)))))

;;;###autoload
(defun skk-kanagaki-midashi-henkan (&optional arg)
  "接頭辞または接尾辞変換をする。"
  (interactive "*p")
  (cond (skk-henkan-active
	 (skk-kakutei)
	 (skk-set-henkan-point-subr)	
	 (insert ?>))
	((and skk-henkan-on (not skk-henkan-active))
	 (insert ?>)
	 (skk-set-marker skk-henkan-end-point (point))
	 (setq skk-henkan-count 0
	       skk-henkan-key (buffer-substring
			       skk-henkan-start-point (point)))
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
  (cond ((eq arg 1) (skk-insert arg))
	;; C-u [SPC] で送りあり変換をする。
	(t (skk-kanagaki-set-okurigana-no-sokuon t))))

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
      (setq skk-okuri-char
	    (cond ((or sokuon (member okuri '("っ" "ッ")))
		   "t")
		  (t
		   (let ((skk-henkan-okurigana okuri))
		     (skk-okurigana-prefix okuri)))))
      (skk-set-okurigana))))

;;;###autoload
(defun skk-kanagaki-set-okurigana-no-sokuon (&optional arg)
  "ポイントの直前の文字を送り仮名と見倣して、変換を開始する。"
  (interactive "*p")
  (skk-kanagaki-set-okurigana (if (eq (prefix-numeric-value arg) 4) nil t)))

;; Pieces of advice.

(defadvice skk-adjust-user-option (before skk-kanagaki-ad activate compile)
  "SKK 起動時の適当なタイミングで仮名入力用の設定を行う。"
  ;;
  (static-when (memq skk-emacs-type '(nemacs mule1))
    ;; Nemacs の canna.el より引用。
    (if (not (keymapp (global-key-binding "\e[")))
	(global-unset-key "\e[")))
  ;; 必要なモジュールをロード。
  (when skk-kanagaki-keyboard-type
    (require (intern (format "skk-%s" skk-kanagaki-keyboard-type))))
  ;; キーバインド。ただしこれは、より適切なキー定義を見つけるまでの暫定的処置。
  (let ((list
	 '((skk-kanagaki-set-henkan-point-key . skk-set-henkan-point-subr)
	   (skk-kanagaki-abbrev-mode-key . skk-abbrev-mode)
	   (skk-kanagaki-katakana-mode-key . skk-toggle-kana)
	   (skk-kanagaki-latin-jisx0208-mode-key . skk-jisx0208-latin-mode)
	   (skk-kanagaki-hankaku-mode-key . skk-toggle-katakana)
	   (skk-kanagaki-latin-mode-key . skk-latin-mode)
	   (skk-kanagaki-code-input-key . skk-input-by-code-or-menu)
	   (skk-kanagaki-toggle-rom-kana-key . skk-kanagaki-toggle-rom-kana)
	   (skk-kanagaki-midashi-henkan-key . skk-kanagaki-midashi-henkan)
	   (skk-kanagaki-previous-candidate-key . skk-previous-candidate))))
    (while list
      (let ((cons (car list)))
	(when (and (symbol-value (car cons)) (commandp (cdr cons)))
	  (define-key skk-j-mode-map
	    (symbol-value (car cons)) (cdr cons))))
      (setq list (cdr list))))
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
  (setq skk-kanagaki-rom-kana-rule-tree skk-rule-tree))

(defadvice skk-insert (around skk-kanagaki-ad activate compile)
  "仮名入力用の work around 。"
  (when (and (skk-local-variable-p 'skk-jisyo (current-buffer))
	     (equal skk-jisyo "~/skk-tut-jisyo")
	     (not (eq skk-kanagaki-state 'rom)))
    (skk-kanagaki-toggle-rom-kana 'rom))
  (let* ((list (copy-sequence skk-special-midashi-char-list))
	 (skk-special-midashi-char-list
	  ;; 句読点入力時の問題を回避。 日本語 106 キーボードでは "<" と ">" に
	  ;; よる接尾辞の入力はできなくなる。 "?" による接尾辞の入力はできる。
	  (cond
	   ((and
	     skk-use-kana-keyboard
	     (memq last-command-char list)
	     (memq
	      (nth 2 (assoc (skk-char-to-string last-command-char)
			    (symbol-value
			     (intern (format "skk-kanagaki-%s-base-rule-list"
					     skk-kanagaki-keyboard-type)))))
	      '(skk-current-kuten skk-current-touten)))
	    (delq last-command-char list))
	   (t
	    list))))
    (case skk-kanagaki-state
      (kana
       (unless (equal skk-rule-tree skk-kanagaki-rule-tree)
	 (make-local-variable 'skk-rule-tree)
	 (setq skk-rule-tree skk-kanagaki-rule-tree))
       (let (skk-set-henkan-point-key)
	 ad-do-it))
      (rom
       (unless (equal skk-rule-tree skk-kanagaki-rom-kana-rule-tree)
	 (make-local-variable 'skk-rule-tree)
	 (setq skk-rule-tree skk-kanagaki-rom-kana-rule-tree))
       ad-do-it)
      (t nil))))

(defadvice skk-okurigana-prefix (around skk-knagaki-ad activate compile)
  (if (eq skk-kanagaki-state 'kana)
      (if (member (ad-get-arg 0) '("っ" "ッ"))
	  "t"
	(let ((skk-henkan-okurigana (ad-get-arg 0)))
	  ad-do-it))
    ad-do-it))

(defadvice skk-isearch-wrapper (around skk-kanagaki-ad activate)
  (setq skk-kanagaki-isearch-buffer (current-buffer))
  ad-do-it
  (setq skk-kanagaki-isearch-buffer nil))

;;

(require 'product)
(product-provide (provide 'skk-kanagaki) (require 'skk-version))

;;; skk-kanagaki.el ends here
