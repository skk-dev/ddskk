;;; skk-nicola.el --- SKK に親指シフト入力インタフェイスを提供 -*- coding: iso-2022-jp -*-

;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;;   Itsushi Minoura <minoura@eva.hi-ho.ne.jp>

;; Author: Itsushi Minoura <minoura@eva.hi-ho.ne.jp>
;;      Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
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

;; このプログラムは箕浦逸史さん作の NICOLA-SKK 0.39 を基に、 Daredevil SKK に
;; 対応させたものです。原作のアイデアに基いて実装していく予定です。
;;
;; キー配列のルールは別ファイルに細分化しています。これらは、同じく箕浦さん作
;; の omelet (たまご用の親指シフト入力インターフェイス) および同氏の web site
;; の文章を基につくりました。
;;
;; 同氏のアイデアと親指シフト入力に関するご尽力に敬意を表し、また感謝いたしま
;; す。

;; 詳細については、同梱の README.NICOLA.ja をご覧下さい。

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-kanagaki-util)
  (require 'skk-macs)
  (require 'skk-vars))

(eval-and-compile
  (require 'skk-kanagaki))

(when (eval-when-compile (featurep 'emacs))
  (require 'skk-emacs))

(eval-and-compile
  (autoload 'skk-dcomp-marked-p "skk-dcomp")
  (autoload 'skk-dcomp-face-off "skk-dcomp")
  (autoload 'skk-dcomp-face-on "skk-dcomp"))

;; Variables.

(defcustom skk-nicola-interval 0.1
  "*この時間以内に打鍵されたものを同時打鍵と判定する。
単位は秒。デフォルトは 0.1 秒。"
  :type 'number
  :group 'skk-nicola)

(defcustom skk-nicola-latin-interval 0.1
  "*この時間以内に打鍵されたものを同時打鍵と判定する。
単位は秒。デフォルトは 0.1 秒。"
  :type 'number
  :group 'skk-nicola)

(defcustom skk-nicola-lshift-keys
  (list (cond
	 ((eq system-type 'windows-nt)
	  [noconvert])
	 (t
	  ;; XEmacs, Emacs 19 or later
	  ;; (except Emacs 20.1 & 20.2)
	  [muhenkan])))
  "*左親指キーとして使うキー。"
  :type (if (get 'key-sequence 'widget-type)
	    '(repeat key-sequence)
	  '(repeat sexp))
  :group 'skk-nicola)

(defcustom skk-nicola-rshift-keys
  (append '(" ")
	  (list (cond
		 ((eq system-type 'windows-nt)
		  [convert])
		 ((featurep 'xemacs)
		  [henkan-mode])
		 (t
		  ;; Emacs 20.3 or later
		  [henkan]))))
  "*右親指キーとして使うキー。"
  :type (if (get 'key-sequence 'widget-type)
	    '(repeat key-sequence)
	  '(repeat sexp))
  :group 'skk-nicola)

(defcustom skk-nicola-use-lshift-as-space nil
  "*Non-nil であれば左親指キーもスペースキーとして利用する。"
  :type 'boolean
  :group 'skk-nicola)

(defcustom skk-nicola-lshift-function nil
  "*Non-nil であれば左親指キーを押したときにこの関数を実行する。"
  :type 'function
  :group 'skk-nicola)

(defcustom skk-nicola-set-henkan-point-chars
  (cond ((memq skk-kanagaki-keyboard-type
	       '(nicola-dvorak
		 omelet-dvorak))
	 '(?u ?h))
	(t
	 '(?f ?j)))
  "*変換開始位置もしくは送り開始位置の指定をする文字。
これらの文字に当たるキーの同時打鍵を検出すると、 実行される。"
  :type '(repeat character)
  :group 'skk-nicola)

(defcustom skk-nicola-toggle-kana-chars
  (cond ((memq skk-kanagaki-keyboard-type
	       '(nicola-dvorak
		 omelet-dvorak))
	 '(?e ?t))
	(t
	 '(?d ?k)))
  "*カナ変換または カナ ⇔ かな 切り替えをする文字。
これらの文字に当たるキーの同時打鍵を検出すると、 実行される。"
  :type '(repeat character)
  :group 'skk-nicola)

(defcustom skk-nicola-use-koyubi-functions
  (cond ((eq skk-kanagaki-keyboard-type
	     'oasys)
	 t)
	(t
	 nil))
  "*Non-nil なら OASYS 風の BS キーと取り消しキーを用意する。
これは、JIS キーボードでは \":\" と \"]\" の位置に相当する。"
  :type 'boolean
  :group 'skk-nicola)

(defcustom skk-nicola-prefix-suffix-abbrev-chars
  (cond ((memq skk-kanagaki-keyboard-type
	       '(nicola-dvorak
		 omelet-dvorak))
	 '(?i ?d))
	(t
	 '(?g ?h)))
  "*接頭・接尾語入力をしたり、 abbrev モードに入る文字。
これらの文字に当たるキーの同時打鍵を検出すると、 実行される。"
  :type '(repeat character)
  :group 'skk-nicola)

(defcustom skk-nicola-prefix-suffix-chars
  (cond ((memq skk-kanagaki-keyboard-type
	       '(nicola-dvorak
		 omelet-dvorak))
	 '(?o ?n))
	(t
	 '(?s ?l)))
  "*接頭・接尾語入力をする。
これらの文字に当たるキーの同時打鍵を検出すると、 実行される。
abbrev と同じキーにする場合は skk-nicola-prefix-suffix-abbrev-chars を使う。"
  :type '(repeat character)
  :group 'skk-nicola)

(defcustom skk-nicola-abbrev-chars
  (cond ((memq skk-kanagaki-keyboard-type
	       '(nicola-dvorak
		 omelet-dvorak))
	 '(?i ?d))
	(t
	 '(?g ?h)))
  "abbrev モードに入る文字。
これらの文字に当たるキーの同時打鍵を検出すると、 実行される。
接頭・接尾語入力と同じキーにする場合は skk-nicola-prefix-suffix-abbrev-chars を使う。"
  :type '(repeat character)
  :group 'skk-nicola)

(defcustom skk-nicola-okuri-style 'nicola-skk
  "*送り仮名のスタイル。
`nicola-skk' を選ぶと、「▽し*っ ⇒ ▼知っ」のように変換する。
`skk' を選ぶと、「▽し*って ⇒ ▼知って」のように変換する。"
  :type '(choice (const nicola-skk)
		 (const skk))
  :group 'skk-nicola)

(defcustom skk-nicola-help-key "2"
  "* \\[help] においてヘルプを表示するキー。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-nicola)

(defcustom skk-nicola-2nd-help-key "3"
  "* \\[help] においてもうひとつのヘルプを表示するキー。"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-nicola)

(defcustom skk-nicola-hiragana-mode-string
  (cond ((eq skk-status-indicator 'left) "にこら:")
	(t " にこら"))
  "*ひらがなモードのインジケータ。"
  :type 'string
  :group 'skk-nicola)

(defcustom skk-nicola-katakana-mode-string
  (cond ((eq skk-status-indicator 'left) "ニコラ:")
	(t " ニコラ"))
  "*カタカナモードのインジケータ。"
  :type 'string
  :group 'skk-nicola)

;; Internal constants and variables.

(defconst skk-nicola-hiragana-rom-string skk-hiragana-mode-string)
(defconst skk-nicola-katakana-rom-string skk-katakana-mode-string)

(defvar skk-nicola-plain-rule nil)
(defvar skk-nicola-lshift-rule nil)
(defvar skk-nicola-rshift-rule nil)

(defvar skk-nicola-temp-data nil)

(skk-deflocalvar skk-nicola-okuri-flag nil)

;; Shut up compiler.
(defvar skktut-j-mode-map)
(defvar skktut-latin-mode-map)

;; Hooks.

(add-hook 'skk-mode-hook 'skk-nicola-setup)
(add-hook 'skk-mode-hook 'skk-nicola-setup-modeline)

;; Functions.

(defun skk-nicola-setup ()
  "NICOLA の初期設定をする。"
  ;; SKK の初回起動時のみ実行されるべきものはこの関数に入れる。
  (dolist (key skk-nicola-lshift-keys)
    (define-key skk-j-mode-map
      key
      'skk-nicola-self-insert-lshift)
    (define-key skk-latin-mode-map
      key
      'skk-nicola-turn-on-j-mode))
  (dolist (key skk-nicola-rshift-keys)
    (define-key skk-j-mode-map
      key
      'skk-nicola-self-insert-rshift)
    (define-key skk-latin-mode-map
      key
      'skk-nicola-turn-on-j-mode))
  ;;
  (when skk-nicola-help-key
    (define-key help-map
      skk-nicola-help-key
      'skk-nicola-help))
  (when skk-nicola-2nd-help-key
    (define-key help-map
      skk-nicola-2nd-help-key
      'skk-nicola-2nd-help))
  ;;
  (unless skk-nicola-plain-rule
    (setq skk-nicola-plain-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-plain-rule-list"
		    skk-kanagaki-keyboard-type)))))
  (unless skk-nicola-lshift-rule
    (setq skk-nicola-lshift-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-lshift-rule-list"
		    skk-kanagaki-keyboard-type)))))
  (unless skk-nicola-rshift-rule
    (setq skk-nicola-rshift-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-rshift-rule-list"
		    skk-kanagaki-keyboard-type)))))
  ;;
  (remove-hook 'skk-mode-hook 'skk-nicola-setup))

(defun skk-nicola-setup-modeline ()
  (case skk-kanagaki-state
    (kana
     (setq skk-hiragana-mode-string skk-nicola-hiragana-mode-string
	   skk-katakana-mode-string skk-nicola-katakana-mode-string))
    (rom
     (setq skk-hiragana-mode-string skk-nicola-hiragana-rom-string
	   skk-katakana-mode-string skk-nicola-katakana-rom-string)))
  ;;
  (skk-modify-indicator-alist 'katakana
			      skk-katakana-mode-string)
  (skk-modify-indicator-alist 'hiragana
			      skk-hiragana-mode-string)
  ;;
  (skk-update-modeline (if skk-katakana
			   'katakana
			 'hiragana)))

(defun skk-nicola-setup-tutorial ()
  (dolist (key skk-nicola-lshift-keys)
    (define-key skktut-j-mode-map
      key
      'skk-nicola-self-insert-lshift)
    (define-key skktut-latin-mode-map
      key
      'skk-nicola-turn-on-j-mode))
  (dolist (key skk-nicola-rshift-keys)
    (define-key skktut-j-mode-map
      key
      'skk-nicola-self-insert-rshift)
    (define-key skktut-latin-mode-map
      key
      'skk-nicola-turn-on-j-mode)))

;;;###autoload
(defun skk-nicola-help (&optional arg)
  "現在使われている親指シフトキー配列を表示する。"
  (interactive "p")
  (describe-variable
   (intern (format "skk-%s-keymap-display"
		   skk-kanagaki-keyboard-type))))

;;;###autoload
(defun skk-nicola-2nd-help ()
  "skk-nicola.el 独自のキー定義一覧を表示する。"
  (interactive)
  (skk-kanagaki-help-1
   "* SKK 親指シフト入力 ヘルプ*"
   "親指シフト入力モードの独自キー定義:"
   (nconc
    ;;
    (mapcar
     #'(lambda (key)
	 (cons (key-description key)
	       "左親指シフトキー"))
     skk-nicola-lshift-keys)
    ;;
    (mapcar
     #'(lambda (key)
	 (cons (key-description key)
	       "右親指シフトキー"))
     skk-nicola-rshift-keys)
    ;;
    (list (cons "SPC"
		"送りなし変換開始"))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree)
		(cdr spec))
	  (list nil (car spec))
	  (str nil
	       (when (memq
		      (nth 3 list)
		      '(skk-input-by-code-or-menu))
		 (nth 1 list))))
	 ((or str
	      (null spec))
	  (when (stringp str)
	    (cons str
		  "コードまたはメニューによる入力")))))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree)
		(cdr spec))
	  (list nil
		(car spec))
	  (str nil
	       (when (memq
		      (nth 3 list)
		      '(skk-today))
		 (nth 1 list))))
	 ((or str
	      (null spec))
	  (when (stringp str)
	    (cons str "今日の日付けを挿入")))))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree)
		(cdr spec))
	  (list nil
		(car spec))
	  (str nil
	       (when (memq
		      (nth 3 list)
		      '(skk-jisx0208-latin-mode))
		 (nth 1 list))))
	 ((or str
	      (null spec))
	  (when (stringp str)
	    (cons str "全英モード")))))
    ;;
    (list
     (cons (format
	    "%c + %c"
	    (car skk-nicola-set-henkan-point-chars)
	    (cadr skk-nicola-set-henkan-point-chars))
	   "変換開始点をセット、送り開始点指定")
     (cons (format
	    "%c + %c"
	    (car skk-nicola-prefix-suffix-abbrev-chars)
	    (cadr skk-nicola-prefix-suffix-abbrev-chars))
	   "接頭辞 or 接尾辞変換 (▽モード or ▼モード)、abbrev モード")
     (cons (format
	    "%c + %c"
	    (car skk-nicola-prefix-suffix-chars)
	    (cadr skk-nicola-prefix-suffix-chars))
	   "接頭辞 or 接尾辞変換 (▽モード or ▼モード)")
     (cons (format
	    "%c + %c"
	    (car skk-nicola-abbrev-chars)
	    (cadr skk-nicola-abbrev-chars))
	   "abbrev モード")
     (cons (format
	    "%c + %c"
	    (car skk-nicola-toggle-kana-chars)
	    (cadr skk-nicola-toggle-kana-chars))
	   "カナモード or カナ変換")
     (cons "左親指シフト + 右親指シフト"
	   "latin モード ⇔ かなモード切り替え")
     (cons (format "M-x help %s" skk-nicola-help-key)
	   "現在の入力方式のキー配列を表示")
     (cons (format "M-x help %s" skk-nicola-2nd-help-key)
	   "このヘルプを表示")))))

;;;###autoload
(defalias 'skk-nicola-self-insert-rshift 'skk-nicola-self-insert-lshift)

;;;###autoload
(defun skk-nicola-self-insert-lshift (&optional arg parg)
  "右または左シフトに割り付ける関数。"
  (interactive "p")
  (unless parg
    (setq parg current-prefix-arg))
  (skk-nicola-self-insert-lshift-1 arg parg)
  ;; verbose message
  (skk-henkan-on-message))

;;;###autoload
(let ((property (if (featurep 'xemacs)
		    'pending-del
		  'delete-selection)))
  (put 'skk-nicola-self-insert-rshift property t)
  (put 'skk-nicola-self-insert-lshift property t))

(defun skk-nicola-self-insert-lshift-1 (arg parg)
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point)
		     (marker-position skk-nicola-okuri-flag)))
	    (not (eq skk-henkan-mode 'on)))
    (setq skk-nicola-okuri-flag nil))
  ;;
  (cond
   ((and (eq skk-kanagaki-state 'kana)
	 (not skk-jisx0201-mode))
    (skk-nicola-insert arg parg))
   (t
    (unless (skk-last-command-char)
      (skk-set-last-command-char ?\ ))
    (call-interactively 'skk-insert))))

;;;###autoload
(defun skk-nicola-turn-on-j-mode (&optional arg)
  "`skk-latin-mode' において、`skk-j-mode' に入るためのコマンド。
左右親指キーの同時打鍵を検出した場合に `skk-j-mode' に入る。"
  (interactive "*p")
  (if (sit-for skk-nicola-latin-interval t)
      ;; then
      (skk-bind-last-command-char
	  (if (characterp (event-to-character
			   last-command-event))
	      (event-to-character last-command-event)
	    ?\ )
	(call-interactively 'self-insert-command t))
    ;; else
    (let ((last (cond
		 ((eval-when-compile (featurep 'xemacs))
		  (event-key last-command-event))
		 (t
		  last-command-event)))
	  (next (cond
		 ((eval-when-compile (featurep 'xemacs))
		  (event-key (next-command-event)))
		 (t
		  (next-command-event))))
	  char)
      (if (eq last next)
	  ;; then
	  (skk-bind-last-command-char
	      (if (characterp (event-to-character
			       last-command-event))
		  (event-to-character last-command-event)
		?\ )
	    (call-interactively 'self-insert-command t)
	    (call-interactively 'self-insert-command t))
	;; else
	(when (characterp next)
	  (setq char next)
	  (setq next (key-description
		      (skk-char-to-unibyte-string char))))
	(when (eq next 'space)
	  (setq next (key-description " ")))
	(when (symbolp next)
	  (setq next (key-description
		      (vector next))))
	;;
	(unless (stringp next)
	  (setq next (format "%s" next)))
	;;
	(cond ((member next
		       (mapcar
			#'(lambda (key)
			    (key-description key))
			(append
			 skk-nicola-rshift-keys
			 skk-nicola-lshift-keys)))
	       ;;
	       (skk-j-mode-on)
	       (skk-cursor-set skk-cursor-hiragana-color))
	      (char
	       (skk-bind-last-command-char
		   (if (characterp (event-to-character
				    last-command-event))
		       (event-to-character last-command-event)
		     ?\ )
		 (call-interactively 'self-insert-command t))
	       (skk-bind-last-command-char char
		 (call-interactively 'self-insert-command
				     t)))))))
  nil)

;;;###autoload
(defun skk-nicola-insert (&optional arg parg)
  "同時打鍵を認識して、NICOLA かな入力をする。"
  (interactive "*p")
  (let (time1
	time2
	next-event
	next)
    ;;
    (setq time1 (skk-nicola-format-time
		 (current-time)))
    ;;
    (unless (eq skk-henkan-mode 'on)
      (setq skk-nicola-okuri-flag nil))
    ;;
    (cond
     ((sit-for skk-nicola-interval t)
      ;; No input in the interval.
      (skk-nicola-insert-single this-command arg parg))
     (t
      ;; Some key's pressed.
      (setq time2 (skk-nicola-format-time (current-time)))
      ;;
      (setq next-event (next-command-event)
	    next (skk-nicola-event-to-key next-event))
      (cond
       ((skk-nicola-maybe-double-p this-command next)
	(skk-nicola-treat-triple this-command next
				 time1 time2
				 arg))
       (t
	;; 最初の入力は単独打鍵でしかありえないと確定。
	(skk-nicola-insert-single this-command arg)
	(skk-unread-event next-event)))))
    ;; 統計的価値があるかな...？
;    (setq skk-nicola-temp-data
;	  (cons
;	   (list (or (skk-last-command-char) this-command)
;		 period1
;		 next
;		 period2
;		 third)
;	   skk-nicola-temp-data))
    )
  ;; `skk-kana-input' が何も入力しないように、nil を返しておく。
  nil)

(defun skk-nicola-format-time (time)
  "`current-time' の返す結果を変換して評価できるようにする。"
  (let ((time1 (* (float 65536) ;; 2^16
		  (car time)))
	(time2 (cadr time))
	(time3 (/ (caddr time)
		  (float 1000000))))
    (+ time1 time2 time3)))

(defun skk-nicola-event-to-key (event)
  "EVENT を発生するキーを取得する。"
  (cond
   ((eval-when-compile (featurep 'xemacs))
    (let ((char (event-to-character event)))
      (if (characterp char)
	  char
	(event-key event))))
   (t
    (if (symbolp event)
	(vector event)
      event))))

;; 〜 NICOLA 規格書より 〜
;; 7.4.2　打鍵順序だけでは決定できない同時打鍵
;;
;;        文字キーa、親指キーs、文字キーbの３つのキーが、判定時間以内
;;        の間隔で重複して押された場合は、中央に挟まれた親指キーsが文
;;        字キーaを修飾するものか、文字キー bを修飾するものかを決定し
;;        なければならない。（図６）
;;
;;        基本的には、押下時刻が、 より親指キーに近い文字キーとの間に
;;        同時打鍵が成立すると判断する。
;;
;;              図6　　　「文字キーON→親指キーON→文字キーON」の例
;;
;;              　　文字キーa 　　　　　　　|￣￣￣|
;;              　　　　　　　　　　…………　　　　……………………
;;
;;              　　親指キーs 　　　　　　　　　|￣￣￣|
;;              　　　　　　　　　　………………　　　　………………
;;
;;              　　文字キーb 　　　　　　　　　　　　|￣￣￣|
;;              　　　　　　　　　　………………………　　　　………
;;
;;             　　　　　　　　　　　　　　|-t1-|-t2-|
;;                                         (t1、t2は共に判定時間以内)
;;
;;   t1=t2ならば、文字キーaと親指キーsが同時打鍵、文字キーbは単独打鍵。
(defun skk-nicola-treat-triple (first next time1 time2 arg)
  "3 つの打鍵のうち、どの 2 打鍵が同時打鍵か判定してバッファに挿入する。"
  (let ((period1 (- time2 time1))
	time3
	period2
	str
	third-event
	third)
  (cond
   ((sit-for period1 t)
    ;; 3 つめの打鍵は制限時間内になかった。同時打鍵と確定。(< t1 t2)
    (skk-nicola-insert-double first next arg))
   (t
    ;; 3 つめの打鍵が制限時間内にあった。その event を調べる。
    (setq period2 (- (setq time3 (skk-nicola-format-time
				  (current-time)))
		     time2)
	  str (if (characterp next)
		  (skk-char-to-unibyte-string next))
	  third-event (next-command-event)
	  third (skk-nicola-event-to-key third-event))
    (cond
     ((and
       (skk-nicola-maybe-double-p next third)
       ;; (要らないかも知らないが、多少 `sit-for' の返ってくる時
       ;; 間と `current-time' が返す時間との間にズレが生じること
       ;; もあるので、一応比較しておく)
       (> period1 period2))
      ;; 前の 2 打鍵は同時打鍵ではないと確定。
      ;; 後の 2 打鍵が同時打鍵かどうかは、更に次の入力を調べないと
      ;; 確定しない。
      (skk-nicola-insert-single this-command arg)
      (skk-nicola-treat-triple
       (lookup-key skk-j-mode-map (or str next))
       third
       time2
       time3
       arg))
     (t
      ;; 前の 2 打鍵が同時打鍵と確定。(< t1 t2)
      (skk-nicola-insert-double this-command next arg)
      (skk-unread-event third-event)))))))

(defun skk-nicola-insert-single (command arg &optional parg)
  "単独打鍵を処理する。"
  (let ((char (skk-last-command-char)))
    (case command
      (skk-nicola-self-insert-rshift
       ;; (変換・スペース)
       (skk-nicola-space-function arg parg))
      (skk-nicola-self-insert-lshift
       ;; 左シフト
       (skk-nicola-lshift-function arg))
      (t
       ;; 文字
       (skk-nicola-insert-kana char
			       skk-nicola-plain-rule
			       arg)))))

(defun skk-nicola-insert-double (first next arg)
  "同時打鍵を処理する。"
  (let ((command (cond
		  ((commandp first)
		   first)
		  ((characterp first)
		   (lookup-key skk-j-mode-map
			       (skk-char-to-unibyte-string first)))
		  (t
		   (lookup-key skk-j-mode-map first))))
	(char (if (characterp first)
		  first
		(skk-last-command-char)))
	(str (when (characterp next)
	       (skk-char-to-unibyte-string next))))
    ;;
    (case (lookup-key skk-j-mode-map (or str next))
      (skk-nicola-self-insert-rshift
       ;; 右シフト
       (case command
	 (skk-nicola-self-insert-rshift
	  ;; [右 右]
	  (skk-bind-last-command-char ?\ 
	    (cond (skk-henkan-mode
		   ;;
		   (skk-kanagaki-insert arg)
		   (unless (>= skk-nicola-interval
			       1)
		     ;; 単独打鍵を同一キー連続打鍵で代用する。
		     (skk-kanagaki-insert arg)))
		  (t
		   (self-insert-command
		    (if (< 1 skk-nicola-interval)
			;; 単独打鍵を同一キー連続打鍵で代用する。
			arg
		      (1+ arg)))))))
	 (skk-nicola-self-insert-lshift
	  ;; [左 右]
	  (skk-nicola-double-shift))
	 (t
	  ;; [文字 右]
	  (skk-nicola-insert-kana char
				  skk-nicola-rshift-rule
				  arg))))
      (skk-nicola-self-insert-lshift
       ;; 左シフト
       (case command
	 (skk-nicola-self-insert-lshift
	  ;;[左 左]
	  (cond ((skk-in-minibuffer-p)
		 (exit-minibuffer))
		(t
		 (skk-nicola-lshift-function arg)
		 (unless (< 1 skk-nicola-interval)
		   ;; 単独打鍵を同一キー連続打鍵で代用する。
		   (skk-nicola-lshift-function 1)))))
	 (skk-nicola-self-insert-rshift
	  ;; [右 左]
	  (skk-nicola-double-shift))
	 (t
	  ;; [文字 左]
	  (skk-nicola-insert-kana char
				  skk-nicola-lshift-rule
				  arg))))
      (t
       ;; 文字
       (cond
	((eq command 'skk-nicola-self-insert-rshift)
	 ;;  [右 文字]
	 (skk-nicola-insert-kana next
				 skk-nicola-rshift-rule
				 arg))
	((eq command 'skk-nicola-self-insert-lshift)
	 ;; [左 文字]
	 (skk-nicola-insert-kana next
				 skk-nicola-lshift-rule
				 arg))
	((and (not (eq char next))
	      (memq (skk-last-command-char)
		    skk-nicola-set-henkan-point-chars)
	      (memq next
		    skk-nicola-set-henkan-point-chars))
	 ;; [fj]
	 (cond
	  ((not (eq skk-henkan-mode 'on))
	   (skk-set-henkan-point-subr 1))
	  ((eq (point) (marker-position skk-henkan-start-point))
	   nil)
	  (t
	   (skk-nicola-set-okuri-flag))))
	((and (not (eq char next))
	      (memq char
		    skk-nicola-prefix-suffix-abbrev-chars)
	      (memq next
		    skk-nicola-prefix-suffix-abbrev-chars))
	 ;; [gh] suffix の 入力
	 (cond
	  ((eq skk-henkan-mode 'active)
	   ;; 接尾語の処理
	   (skk-kakutei)
	   (let (skk-kakutei-history)
	     (skk-set-henkan-point-subr))
	   (insert-and-inherit ?>))
	  ((eq skk-henkan-mode 'on)
	   ;; 接頭語の処理
	   (skk-kana-cleanup 'force)
	   (insert-and-inherit ?>)
	   (skk-set-marker skk-henkan-end-point
			   (point))
	   (setq skk-henkan-count 0
		 skk-henkan-key   (buffer-substring-no-properties
				   skk-henkan-start-point
				   (point))
		 skk-prefix       "")
	   (skk-henkan))
	  (t
	   ;;
	   (skk-abbrev-mode 1))))
	((and (not (eq char next))
	      (memq char
		    skk-nicola-prefix-suffix-chars)
	      (memq next
		    skk-nicola-prefix-suffix-chars))
	 ;; [sl] suffix の 入力
	 (cond
	  ((eq skk-henkan-mode 'active)
	   ;; 接尾語の処理
	   (skk-kakutei)
	   (let (skk-kakutei-history)
	     (skk-set-henkan-point-subr))
	   (insert-and-inherit ?>))
	  ((eq skk-henkan-mode 'on)
	   ;; 接頭語の処理
	   (skk-kana-cleanup 'force)
	   (insert-and-inherit ?>)
	   (skk-set-marker skk-henkan-end-point
			   (point))
	   (setq skk-henkan-count 0
		 skk-henkan-key   (buffer-substring-no-properties
				   skk-henkan-start-point
				   (point))
		 skk-prefix       "")
	   (skk-henkan))))
	((and (not (eq char next))
	      (memq char
		    skk-nicola-abbrev-chars)
	      (memq next
		    skk-nicola-abbrev-chars))
	 ;; [gh]
	 (skk-kakutei)
	 (skk-abbrev-mode 1))
	((and (not (eq char next))
	      (memq char
		    skk-nicola-toggle-kana-chars)
	      (memq next
		    skk-nicola-toggle-kana-chars))
	 ;; [dk]
	 (skk-toggle-kana 1))
	(t
	 ;; [文字 文字]
	 (let ((str (skk-nicola-insert-kana
		     char
		     skk-nicola-plain-rule
		     arg)))
	   (when (and skk-isearch-switch
		      (not skk-henkan-mode))
	     (setq isearch-cmds
		   (cons
		    (cond
		     ((vectorp (car isearch-cmds))
		      (let ((cmds (copy-sequence (car isearch-cmds))))
			(aset cmds 0 (concat (aref (car isearch-cmds) 0)
					     str))
			(aset cmds 1 (concat (aref (car isearch-cmds) 1)
					     str))
			cmds))
		     (t
		      (nconc
		       (list (concat (caar isearch-cmds)
				     str)
			     (concat (cadar isearch-cmds)
				     str))
		       (cddar isearch-cmds))))
		    isearch-cmds))))
	 (unless (and (< 1 skk-nicola-interval)
		      (eq next char))
	   ;; 単独打鍵を同一キー連続打鍵で代用できるように。
	   (skk-nicola-insert-kana
	    next
	    skk-nicola-plain-rule))))))))

(defun skk-nicola-double-shift ()
  "親指右キーと親指左キーの同時打鍵を処理する。"
  (cond
   ((and skk-j-mode
	 (not skk-katakana))
    (skk-latin-mode 1))
   (t
    (skk-toggle-kana 1)))
  nil)

(defun skk-nicola-maybe-double-p (first next)
  "FIRST と NEXT が同時打鍵だったら non-nil を返す。"
  (let ((command (cond
		  ((commandp first)
		   first)
		  ((characterp first)
		   (lookup-key skk-j-mode-map
			       (skk-char-to-unibyte-string first)))
		  (t
		   (lookup-key skk-j-mode-map first))))
	(char (if (characterp first)
		  first
		(skk-last-command-char)))
	(str (when (characterp next)
	       (skk-char-to-unibyte-string next)))
	(shifts '(skk-nicola-self-insert-lshift
		  skk-nicola-self-insert-rshift)))
  (or
   ;; * どちらか一方が親指
   (or (memq command shifts)
       (memq (lookup-key skk-j-mode-map (or str
					    next))
	     shifts))
   ;; * skk-nicola に於ける特殊同時打鍵キー
   (and (not (eq char next))
	(or
	 ;; [fj]
	 (and (memq (skk-last-command-char)
		    skk-nicola-set-henkan-point-chars)
	      (memq next
		    skk-nicola-set-henkan-point-chars))
	 ;; [gh]
	 (and (memq char
		    skk-nicola-prefix-suffix-abbrev-chars)
	      (memq next
		    skk-nicola-prefix-suffix-abbrev-chars))
	 ;; [sl]
	 (and (memq char
		    skk-nicola-prefix-suffix-chars)
	      (memq next
		    skk-nicola-prefix-suffix-chars))
	 ;; [gh]
	 (and (memq char
		    skk-nicola-abbrev-chars)
	      (memq next
		    skk-nicola-abbrev-chars))
	 ;; [dk]
	 (and  (memq char
		     skk-nicola-toggle-kana-chars)
	       (memq next
		     skk-nicola-toggle-kana-chars)))))))

(defun skk-nicola-insert-kana (char rule &optional arg)
  "CHAR を RULE の中から探して入力すべき文字列を決定する。
ARG を与えられた場合はその数だけ文字列を連結して入力する。"
  (let* ((el (cadr (assq char rule)))
	 (str (when el
		(cond ((stringp el)
		       el)
		      ((not (listp el))
		       nil)
		      (skk-katakana
		       (car el))
		      (t
		       (cdr el)))))
	 (fun (when (and el
			 (symbolp el))
		el))
	 (arg (prefix-numeric-value arg)))
    ;;
    (when str
      (if (symbolp str)
	  (setq fun str
		str nil)
	(skk-cancel-undo-boundary)
	(skk-insert-str
	 (setq str (make-string arg (string-to-char str))))))
    ;;
    (when fun
      (funcall fun arg))
    ;;
    (cond (skk-nicola-okuri-flag
	   (skk-nicola-process-okuri))
	  ((eq skk-henkan-mode 'active)
	   (skk-kakutei)))
    ;; 何かに使うことがあるかもしれないので、
    ;; STR を返しておく。
    str))

(defun skk-nicola-process-okuri ()
  "送り開始の標識により送り開始点を認識し、送りあり変換を開始する。"
  (let ((okuri (buffer-substring-no-properties
		(1+ skk-nicola-okuri-flag)
		(point)))
	tag)
    (unless (and (not (eq skk-nicola-okuri-style
			  'nicola-skk))
		 (member okuri '("っ" "ッ")))
      (skk-save-point
	(goto-char skk-nicola-okuri-flag)
	(when (eq (following-char)
		  ?*)
	  (delete-char 1))
	(backward-char 1)
	(when (member (buffer-substring-no-properties
		       (point)
		       (marker-position skk-nicola-okuri-flag))
		      '("っ" "ッ"))
	  (setq tag 'no-sokuon)))
      (skk-kanagaki-set-okurigana tag))))

(defun skk-nicola-set-okuri-flag ()
  "送り開始点を marker で標識し、送りあり変換の待ち状態に入る。
`*' を挿入することで標識する。"
  (interactive)
  (when (eq skk-henkan-mode 'on)
    ;; ▽モードのときだけ機能する。
    (let ((pt (point)))
      (unless (and (string= "*"
			    (buffer-substring-no-properties
			     (1- pt)
			     pt))
		   (markerp skk-nicola-okuri-flag))
	;; 既に標識済みなら何もしない。
	(skk-set-marker skk-nicola-okuri-flag pt)
	(insert-and-inherit "*")))))

(defun skk-nicola-space-function (&optional arg parg)
  "親指右キー単独打鍵時の挙動を決める関数。"
  (skk-bind-last-command-char ?\ 
    (cond
     ((eq skk-henkan-mode 'active)
      (call-interactively 'skk-insert))
     ((eq skk-henkan-mode 'on)
      (skk-kanagaki-insert arg parg))
     (t
      (self-insert-command arg)))))

(defun skk-nicola-lshift-function (&optional arg)
  "親指左キー単独打鍵時の挙動を決める関数。"
  (cond (skk-henkan-mode
	 ;; 確定に使う。
	 (skk-kakutei))
	(skk-nicola-use-lshift-as-space
	 ;;
	 (skk-nicola-space-function arg))
	(skk-nicola-lshift-function
	 (if (commandp skk-nicola-lshift-function)
	     (call-interactively skk-nicola-lshift-function)
	   (funcall skk-nicola-lshift-function arg)))
	(t
	 ;; 改行に使う。
	 (if (skk-in-minibuffer-p)
	     (exit-minibuffer)
	   (newline arg)))))

;; Pieces of Advice.

(defadvice skk-kanagaki-initialize (after skk-nicols-setup activate)
  ;; M-x skk-restart 対策として
  (add-hook 'skk-mode-hook 'skk-nicola-setup)
  (add-hook 'skk-mode-hook 'skk-nicola-setup-modeline))

(defadvice skk-insert (before skk-nicola-update-flag activate)
  "送り待ち状態を管理する。"
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point)
		     (marker-position
		      skk-nicola-okuri-flag)))
	    (not (eq skk-henkan-mode 'on)))
    (setq skk-nicola-okuri-flag nil)))

(defadvice skk-kakutei (before skk-nicola-update-flag activate)
  "送り待ち状態を管理する。"
  (when (and skk-j-mode
	     (eq skk-henkan-mode 'on)
	     (markerp skk-nicola-okuri-flag))
    ;; 確定するときは送り開始の標識を消す。
    (skk-save-point
      (goto-char skk-nicola-okuri-flag)
      (when (eq ?* (following-char))
	(delete-char 1))))
  ;;
  (setq skk-nicola-okuri-flag nil))

(defadvice skk-previous-candidate (before skk-nicola-update-flag activate)
  "送り待ち状態を管理する。"
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point)
		     (marker-position
		      skk-nicola-okuri-flag)))
	    (not (eq skk-henkan-mode 'on)))
    (setq skk-nicola-okuri-flag nil)))

(defadvice skk-insert (around skk-nicola-workaround activate)
  ;;
  (let* ((list (symbol-value
		(intern (format "skk-%s-plain-rule-list"
				skk-kanagaki-keyboard-type))))
	 (cell1 (rassoc '("、") list))
	 (cell2 (rassoc '("。") list))
	 marker)
    (cond
     ((and (eq skk-kanagaki-state 'kana)
	   skk-j-mode
	   (or (eq (skk-last-command-char)
		   (car cell1))
	       (eq (skk-last-command-char)
		   (car cell2)))
	   skk-henkan-mode)
      ;; なぜかこける。原因解明中。
      (cond
       ((not (eq skk-henkan-mode 'active))
	(setq marker skk-henkan-start-point)
	(skk-kakutei)
	ad-do-it
	(unless (or (string= (char-to-string (char-before))
			     (cadr cell1))
		    (string= (char-to-string (char-before))
			     (cadr cell2)))
	  (skk-save-point
	   (goto-char marker)
	   (skk-set-henkan-point-subr))))
       (t
	(skk-kakutei)
	ad-do-it)))
     (t
      ad-do-it))))

(defadvice skk-isearch-setup-keymap (before skk-nicola-workaround activate)
  "親指キーでサーチが終了してしまわないようにする。"
  (let ((keys (append skk-nicola-lshift-keys
		      skk-nicola-rshift-keys)))
    (while keys
      (define-key (ad-get-arg 0)
	(car keys)
	'skk-isearch-wrapper)
      (setq keys (cdr keys)))))

(defadvice isearch-char-to-string (around skk-nicola-workaround activate)
  "エラーが出ると検索が中断して使い辛いので、黙らせる。"
  (cond ((and skk-use-kana-keyboard
	      (featurep 'skk-isearch)
	      (with-current-buffer
		  (get-buffer-create
		   skk-isearch-working-buffer)
		skk-mode))
	 (ignore-errors
	   ad-do-it))
	(t
	 ad-do-it)))

(put 'skk-nicola-insert 'isearch-command t)
(put 'skk-nicola-self-insert-lshift 'isearch-command t)
(put 'skk-nicola-self-insert-rshift 'isearch-command t)

(provide 'skk-nicola)

;;; skk-nicola.el ends here
