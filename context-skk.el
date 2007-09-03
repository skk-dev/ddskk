;;; context-skk.el --- turning off skk when the point enters where skk is unnecessary -*- coding: iso-2022-jp -*-
;;
;; Copyright (C) 2003, 2005 Masatake YAMATO
;;
;; Author: Masatake YAMATO <jet@gyve.org>
;; Created: Tue May 13 19:12:23 2003
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;
;;; Commentary:
;; このプログラムはskkの動作、振舞いに関して2つの機能を提供します。
;;
;; (1) 編集の文脈に応じて自動的にskkのモードをlatinに切り替えます。
;; 明かにskkによる日本語入力が必要ない個所で、skkをオンにしたまま
;; キー操作を行なったためにemacsからエラーの報告を受けたり、わざわざ
;; skkをオフにしてテキストを修正するのは不快です。これを抑制するこ
;; とが、この機能の目的です。
;;
;; 文脈の判定はemacs lispによって記述できます。このプログラムには、次の3つ
;; の文脈に対する判定関数が含まれています。
;;
;; 1A. read-onlyかどうか
;; --------------------
;;    read-onlyバッファでは、日本語入力の必要はないし、できないので、日
;;    本語入力をオフにします。またread-onlyの領域でも同様に日本語入力を
;;    offにします。エラーの報告を受けるかわりにskkによってシャドウされ
;;    た元のキーに割当てられたコマンドを実行できます。
;;
;; 1B. プログラムコード中でのコメントや文字列の内側にいるか
;; -------------------------------------------------------
;;    あるプログラミング言語でプログラムを書いているとき、日本語入力の必要が
;;    あるのは一般に、そのプログラミング言語の文字列中かコメント中に限られま
;;    す。文字列、コメントの「外」を編集するときは、多くの場合日本語入力は必
;;    要ありません。
;;    たとえば emacs lispでは、
;;
;;    "〜" や ;; 〜
;;
;;    といった個所でだけ日本語入力が必要となります。
;; 
;;    現在の文字列とコメントの「外」で編集開始と同時に
;;    (skkがオンであれば)skkの入力モードをlatinに切り替えます。「外」での編集
;;    を開始するにあたって、日本語入力がonになっていたために発生する入力誤り
;;    とその修正操作を回避することができます。
;;    
;; 1C. キーマップが登録されているかどうかを判定
;; -------------------------------------------
;;    ポイント下に`keymap'あるいは`local-map'の属性を持つ文字あるいはオーバレイが
;;    あるかどうかを調べます。キーマップが設定されている場合、さらにskkで母音
;;    の入力に使う ?a, ?i, ?u, ?e, ?oのキーがキーマップ中に定義されているか調
;;    べます。定義されている場合、キーマップ中のキーに割当てられた機能を実行
;;    できるよう日本語入力をオフにします。
;;
;; 自身でskkをオフにする文脈を定義するには、
;; `context-skk-context-check-hook'
;; 変数を使います。skkの文字入力関数`skk-insert'の実行直前に引数無しで呼び出
;; され、skkをオフにする文脈にあるときnon-nilを返す関数を定義して、この変数
;; に`add-hook'して下さい。
;;
;; (2) 編集の文脈に応じてskkの設定を変更します。
;; skkの文字入力関数`skk-insert'のまわりに`let'を配置して、文字入力中に一時的
;; に変数の束縛を変更して、文字入力のたびにskkの設定を変更できます。このプログ
;; ラムには、skkによるテキストの入力先のバッファをスキャンし、(句読点の種類を
;; 表す)`skk-kutouten-type'を変更する関数が含まれています。
;;
;; 独自に変数を設定したい場合、関数を書く必要があります。
;; `context-skk-custumize-functions'のドキュメントに従い、関数を書き、
;;
;; (add-to-list 'context-skk-custumize-functions 
;;	        'your-on-the-fly-customize-func)
;;
;; として登録します。M-x context-skk-dump-customize による現在のポイントに対して、
;; context-skkによって一次的に束縛される変数とその値の組を確認できます。デバッグ
;; に活用して下さい。
;;
;; 上述した2つの機能はcontext-skk-modeというマイナーモードとして実装してあります。
;; M-x context-skk-mode
;; で オン/オフをできます。モードラインに ";▽" が表示されている
;; 場合、このマイナーモードがonになっていることを意味します。
;;
;; - インストール - .emacs に以下を記述します。
;;
;; (add-hook 'skk-load-hook
;;	  (lambda ()
;;	    (require 'context-skk)))
;;
;; - todo 
;; Handling the prefix arguments

;;; Code: 

;;
;; Custom
;;
;;;###autoload
(defgroup context-skk nil
  "Context-skk minor mode related customization."
  :group 'skk
  :prefix "context-skk-")

;;;###autoload
(defcustom context-skk-context-check-hook
  '(context-skk-out-of-string-or-comment-in-programming-mode-p
    context-skk-on-keymap-defined-area-p
    context-skk-in-read-only-p)
  "*日本語入力を自動的にoffにしたい「コンテキスト」にいればtを返す関数を登録する。"
  :type 'hook
  :group 'context-skk)

;;;###autoload
(defcustom context-skk-custumize-functions 
  '(context-skk-customize-kutouten)
  "*skkによる入力開始直前に、入力のカスタマイズを行うための関数を登録する。
関数は以下の形式のデータを要素とするリストを返すものとする: 

  \(VARIABLE VALUE\)

`skk-insert'をかこむ`let'によってVARIABLEはVALUEに束縛される。
特にその場でカスタマイズすべき変数がない場合 `nil'を返せば良い。
関数には何も引数が渡されない。"
  :type 'hook				; hook? list of function?
  :group 'context-skk)

;;;###autoload
(defcustom context-skk-programming-mode
  '(ada-mode antlr-mode asm-mode autoconf-mode awk-mode
    c-mode objc-mode java-mode idl-mode pike-mode cperl-mode
    ;;?? dcl-mode
    delphi-mode f90-mode fortran-mode
    icon-mode idlwave-mode inferior-lisp-mode lisp-mode m4-mode makefile-mode
    metafont-mode modula-2-mode octave-mode pascal-mode perl-mode
    prolog-mode ps-mode postscript-mode ruby-mode scheme-mode sh-mode simula-mode
    ;; sql-mode
    tcl-mode vhdl-mode emacs-lisp-mode)
  "*context-skkにて「プログラミングモード」とみなすモードのリスト"
  :type '(repeat (symbol))
  :group 'context-skk)

;;
;; Minor mode definition
;;
;; Change autoload cookie for XEmacs.
;;;###autoload (autoload 'context-skk-mode "context-skk" "文脈に応じて自動的にskkの入力モードをlatinに切り換えるマイナーモード。" t)
(define-minor-mode context-skk-mode
  "文脈に応じて自動的にskkの入力モードをlatinに切り換えるマイナーモード。"
  t 
  :lighter " ;▽")

;;
;; Advices
;;
(defadvice skk-insert (around skk-insert-ctx-switch activate)
  "文脈に応じて自動的にskkの入力モードをlatinにする。"
  (if (and context-skk-mode (context-skk-context-check))
      (context-skk-insert) 
    (eval `(let ,(context-skk-custumize)
	     ad-do-it))))

(defadvice skk-jisx0208-latin-insert (around skk-jisx0208-latin-insert-ctx-switch activate)
  "文脈に応じて自動的にskkの入力モードをlatinにする。"
  (if (and context-skk-mode (context-skk-context-check))
      (context-skk-insert) 
    (eval `(let ,(context-skk-custumize)
	     ad-do-it))))

;;
;; Helper
;;
(defun context-skk-context-check ()
  "日本語入力を自動的にoffにしたい「コンテキスト」にいればtを返す"
  (run-hook-with-args-until-success 'context-skk-context-check-hook))

(defun context-skk-custumize ()
  "カスタマイズしたい変数と値の組を得る。"
  (let (customized-pairs)
    (dolist (func context-skk-custumize-functions)
      (setq customized-pairs
	    (append 
	     (save-excursion (funcall func))
	     customized-pairs)))
    customized-pairs))

(defun context-skk-dump-customize ()
  "現在のポイントの位置における (context-skk-custumize) の結果を表示する。"
  (interactive)
  (let ((customized-pairs (context-skk-custumize)))
    (with-output-to-temp-buffer "*context-skk customize result*"
      (pp customized-pairs))))

(defun context-skk-insert ()
  "skk-latin-modeをonにした上`this-command-keys'に対する関数を呼び出し直す。"
  (message "[context-skk] 日本語入力 off")
  (skk-latin-mode t)
  (let* ((keys (this-command-keys))
	 ;; `this-command-keys' が tab を返したときなど function-key-map や
	 ;; key-translation-map に依存している場合はそれらの keymap を参照する
	 (binding (or (key-binding keys)
		      (key-binding (lookup-key function-key-map keys))
		      (key-binding (lookup-key key-translation-map keys)))))
    (when binding
      (call-interactively binding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Predicators
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; リードオンリーでないか？
;;
(defun context-skk-in-read-only-p ()
  (or (context-skk-in-read-only-buffer-p)
      (context-skk-in-read-only-area-p)))

(defun context-skk-in-read-only-buffer-p ()
  buffer-read-only)

(defun context-skk-in-read-only-area-p ()
  (or 
   (and (get-char-property (point) 'read-only)
	(get-char-property (point) 'front-sticky))
   (and 
    (< (point-min) (point))
    (get-char-property (1- (point)) 'read-only)
    (not (get-char-property (1- (point)) 'rear-nonsticky)))))

;;
;; 通常日本語入力を必要としないプログラミングのモードにいるかどうか
;; 文字列を編集中かどうか
;; コメントを編集中かどうか
;;
(defun context-skk-out-of-string-or-comment-in-programming-mode-p ()
  "プログラミングモードにあって文字列あるいはコメントの外にいればnon-nilを返す。
プログラミングモードにいない場合はnilを返す。
プログラミングモードにあって文字列あるいはコメントの中にいる場合nilを返す。"
  (and (context-skk-in-programming-mode-p) 
       (not (or (context-skk-in-string-p)
		(context-skk-in-comment-p)))))

(defun context-skk-in-programming-mode-p ()
  (memq major-mode
	context-skk-programming-mode))

(defun context-skk-in-string-p ()
  (nth 3 (parse-partial-sexp (point) (point-min))))
(defun context-skk-in-comment-p ()
  (nth 4 (parse-partial-sexp (point) (point-min))))

;;
;; 現在のポイント下にkeymapが定義されているかどうか？
;;
(defun context-skk-on-keymap-defined-area-p ()
  (or (context-skk-on-vowel-key-reserved-p 'keymap)
      (context-skk-on-vowel-key-reserved-p 'local-map)))

(defun context-skk-on-vowel-key-reserved-p (map-symbol)
  (let ((map (get-char-property (point) map-symbol)))
    (when map
      ;; "あいうえお"を入力することを想定してチェックする。
      (or (lookup-key map "a")
	  (lookup-key map "i")
	  (lookup-key map "u")
	  (lookup-key map "e")
	  (lookup-key map "o")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customize function
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; 句読点(skk-kutouten-type)
;;
;; Based on a post to skk ml by 
;; Kenichi Kurihara (kenichi_kurihara at nifty dot com)
;; Message-ID: <m2y85qctw6.wl%kurihara@mi.cs.titech.ac.jp>
;;
(defun context-skk-customize-kutouten ()
  (let ((kuten-jp  (context-skk-customize-regexp-scan "。" 'forward 0 nil))
	(kuten-en  (context-skk-customize-regexp-scan "．" 'forward 0 nil))
	(touten-jp (context-skk-customize-regexp-scan "、" 'forward 0 nil))
	(touten-en (context-skk-customize-regexp-scan "，" 'forward 0 nil)))
    (if (or (eq kuten-jp kuten-en)
	    (eq touten-jp touten-en))
	nil ;; Nothing to customize
      `((skk-kutouten-type 
	 ',(if kuten-jp
	      (if touten-jp 
		  'jp
		'jp-en)
	    (if touten-jp 
		'en-jp
	      'en)))))))
      
(defun context-skk-customize-regexp-scan (regexp direction from limit)
  (let ((func (if (eq direction 'forward)
		  're-search-forward
		're-search-backward)))
    (save-excursion
      (goto-char from)
      (if (funcall func regexp limit t) 
	  t
	nil))))

(provide 'context-skk)
;; context-skk.el ends here
