;;; skk-nicola.el -- SKK に親指シフト入力インタフェイスを提供
;; Copyright (C) 1996 - 2000 Itsushi Minoura <minoura@eva.hi-ho.ne.jp>

;; Author: Itsushi Minoura <minoura@eva.hi-ho.ne.jp>
;;      Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
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
  (require 'skk-vars)
  (require 'static))

(eval-and-compile
  (require 'skk-kanagaki))

(static-when (eq skk-emacs-type 'mule5)
  (eval-and-compile
    (require 'skk-e21)))

(eval-when-compile
  (defvar skk-dcomp-start-point)
  (defvar skk-dcomp-end-point)
  (defvar skk-dcomp-keep-completion-keys)
  (autoload 'skk-dcomp-face-off "skk-dcomp")
  (autoload 'skk-dcomp-face-on "skk-dcomp"))

(defgroup skk-nicola nil "SKK NICOLA related customization."
  :prefix "skk-nicola-"
  :group 'skk
  :group 'skk-kanagaki)

;; Variables.

(defcustom skk-nicola-interval (if (featurep 'lisp-float-type)
				   (/ (float 1) (float 10))
				 1) "\
*この時間以内に打鍵されたものを同時打鍵と判定する。
単位は秒。デフォルトは 0.1 秒。ただし、 Emacs 18 の場合は浮動小数点数を扱えな
いため、仮に 1 秒としておくが、実用的には厳しいと思われる。"
  :type 'number
  :group 'skk-nicola)

(defcustom skk-nicola-latin-interval (if (featurep 'lisp-float-type)
					 (/ (float 1) (float 10))
				       1) "\
*この時間以内に打鍵されたものを同時打鍵と判定する。
単位は秒。デフォルトは 0.1 秒。ただし、 Emacs 18 の場合は浮動小数点数を扱えな
いため、仮に 1 秒としておくが、実用的には厳しいと思われる。"
  :type 'number
  :group 'skk-nicola)

(defcustom skk-nicola-lshift-keys
  (list (cond
	 ((eq system-type 'windows-nt)
	  [noconvert])
	 ((memq skk-emacs-type '(nemacs mule1 mule3))
	  ;; Don't know what is good..
	  "\\")
	 (t
	  ;; XEmacs, Emacs 19 or later (except Emacs 20.1 & 20.2)
	  [muhenkan]))) "\
*左親指キーとして使うキー。"
  :type '(repeat sexp)
  :group 'skk-nicola)

(defcustom skk-nicola-rshift-keys
  (if (memq skk-emacs-type '(nemacs mule1 mule3))
      '(" ")
    (append '(" ")
	    (list (cond
		   ((eq system-type 'windows-nt)
		    [convert])
		   ((eq skk-emacs-type 'xemacs)
		    [henkan-mode])
		   ((string-match "^19\\.\\(29\\|3[0-4]\\)" emacs-version)
		    [numbersign])
		   ((string-match "^19\\.2" emacs-version)
		    ;; Mule 2.3@19.28 or earlier (?)
		    [key-35])
		   (t
		    ;; Emacs 20.3 or later
		    [henkan]))))) "\
*右親指キーとして使うキー。"
  :type 'sexp
  :group 'skk-nicola)

(defcustom skk-nicola-use-lshift-as-space nil "\
*Non-nil であれば左親指キーもスペースキーとして利用する。"
  :type 'boolean
  :group 'skk-nicola)

(defcustom skk-nicola-set-henkan-point-chars
  (cond ((memq skk-kanagaki-keyboard-type '(nicola-dvorak omelet-dvorak))
	 '(?u ?h))
	(t
	 '(?f ?j)))
  "\
*変換開始位置もしくは送り開始位置の指定をする文字。
これらの文字に当たるキーの同時打鍵を検出すると、 実行される。"
  :type '(repeat character)
  :group 'skk-nicola)

(defcustom skk-nicola-toggle-kana-chars
  (cond ((memq skk-kanagaki-keyboard-type '(nicola-dvorak omelet-dvorak))
	 '(?e ?t))
	(t
	 '(?d ?k)))
  "\
*カナ変換または カナ ⇔ かな 切り替えをする文字。
これらの文字に当たるキーの同時打鍵を検出すると、 実行される。"
  :type '(repeat character)
  :group 'skk-nicola)

(defcustom skk-nicola-use-koyubi-functions
  (cond ((eq skk-kanagaki-keyboard-type 'oasys)
	 t)
	(t
	 nil))
  "\
*Non-nil なら OASYS 風の BS キーと取り消しキーを用意する。
これは、JIS キーボードでは \":\" と \"]\" の位置に相当する。"
  :type 'boolean
  :group 'skk-nicola)

(defcustom skk-nicola-prefix-suffix-abbrev-chars
  (cond ((memq skk-kanagaki-keyboard-type '(nicola-dvorak omelet-dvorak))
	 '(?i ?d))
	(t
	 '(?g ?h)))
  "\
*接頭・接尾語入力をしたり、 abbrev モードに入る文字。
これらの文字に当たるキーの同時打鍵を検出すると、 実行される。"
  :type '(repeat character)
  :group 'skk-nicola)

(defcustom skk-nicola-okuri-style 'nicola-skk "\
*送り仮名のスタイル。
`nicola-skk' を選ぶと、「▽し*っ ⇒ ▼知っ」のように変換する。
`skk' を選ぶと、「▽し*って ⇒ ▼知って」のように変換する。"
  :type '(choice (const nicola-skk)
		 (const skk))
  :group 'skk-nicola)

(defcustom skk-nicola-help-key "2" "\
* \\[help] においてヘルプを表示するキー。"
  :type 'sexp
  :group 'skk-nicola)

(defcustom skk-nicola-2nd-help-key "3" "\
* \\[help] においてもうひとつのヘルプを表示するキー。"
  :type 'sexp
  :group 'skk-nicola)

(defcustom skk-nicola-hiragana-mode-string
  (cond ((eq skk-status-indicator 'left) "にこら:")
	(t " にこら"))
  "\
*ひらがなモードのインジケータ。"
  :type 'string
  :group 'skk-nicola)

(defcustom skk-nicola-katakana-mode-string
  (cond ((eq skk-status-indicator 'left) "ニコラ:")
	(t " ニコラ"))
  "\
*カタカナモードのインジケータ。"
  :type 'string
  :group 'skk-nicola)

;; Internal constants and variables.

(defconst skk-nicola-hiragana-rom-string skk-hiragana-mode-string)
(defconst skk-nicola-katakana-rom-string skk-katakana-mode-string)

(defvar skk-nicola-plain-rule nil)
(defvar skk-nicola-lshift-rule nil)
(defvar skk-nicola-rshift-rule nil)

(skk-deflocalvar skk-nicola-okuri-flag nil)

(skk-deflocalvar skk-current-local-map nil "\
Emacs 18 用の変数。")

(static-when (memq skk-emacs-type '(nemacs mule1 mule3))
  (case skk-kanagaki-jidou-keymap-kakikae-service
    ;;
    (nicola-jis
     ;; Emacs 18 では Muhenkan, Henkan は使えないようなので、使えるキーに書換
     ;; える。
     (skk-kanagaki-call-xmodmap
	 (case skk-emacs-type
	   (mule3
	    "keycode 129 = F19 Mode_switch
keycode 131 = F18\n")
	   (t
	    "keycode 129 = space Mode_switch
keycode 131 = underscore\n"))
       (case skk-emacs-type
	 (mule3
	  (setq skk-nicola-lshift-keys (nconc skk-nicola-lshift-keys
					      '([f18]))
		skk-nicola-rshift-keys (nconc skk-nicola-rshift-keys
					      '([f19]))))
	 (t
	  (setq skk-nicola-lshift-keys (nconc skk-nicola-lshift-keys
					      '("_"))
		skk-nicola-rshift-keys (nconc skk-nicola-rshift-keys
					      '(" ")))))))))

;; Shut up compiler.
(defvar skktut-j-mode-map)
(defvar skktut-latin-mode-map)

;; Hooks.

(add-hook 'skk-mode-hook 'skk-nicola-setup)

(add-hook
 'skk-mode-hook
 (function
  (lambda ()
    ;;
    (case skk-kanagaki-state
      (kana
       (setq skk-hiragana-mode-string skk-nicola-hiragana-mode-string
	     skk-katakana-mode-string skk-nicola-katakana-mode-string))
      (rom
       (setq skk-hiragana-mode-string skk-nicola-hiragana-rom-string
	     skk-katakana-mode-string skk-nicola-katakana-rom-string)))
    ;;
    (skk-modify-indicator-alist 'katakana skk-katakana-mode-string)
    (skk-modify-indicator-alist 'hiragana skk-hiragana-mode-string)
    ;;
    (skk-update-modeline (if skk-katakana
			     'katakana
			   'hiragana)))))

;; Functions.

(defun skk-nicola-setup ()
  ;; SKK の初回起動時のみ実行されるべきものはこの関数に入れる。
  (dolist (key skk-nicola-lshift-keys)
    (define-key skk-j-mode-map key 'skk-nicola-self-insert-lshift)
    (define-key skk-latin-mode-map key 'skk-nicola-turn-on-j-mode))
  (dolist (key skk-nicola-rshift-keys)
    (define-key skk-j-mode-map key 'skk-nicola-self-insert-rshift)
    (define-key skk-latin-mode-map key 'skk-nicola-turn-on-j-mode))
  ;;
  (when skk-nicola-help-key
    (define-key help-map skk-nicola-help-key 'skk-nicola-help))
  (when skk-nicola-2nd-help-key
    (define-key help-map skk-nicola-2nd-help-key 'skk-nicola-2nd-help))
  ;;
  (unless skk-nicola-plain-rule
    (setq skk-nicola-plain-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-plain-rule-list" skk-kanagaki-keyboard-type)))))
  (unless skk-nicola-lshift-rule
    (setq skk-nicola-lshift-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-lshift-rule-list" skk-kanagaki-keyboard-type)))))
  (unless skk-nicola-rshift-rule
    (setq skk-nicola-rshift-rule
	  (symbol-value
	   (intern
	    (format "skk-%s-rshift-rule-list" skk-kanagaki-keyboard-type)))))
  ;;
  (remove-hook 'skk-mode-hook 'skk-niola-setup))

(defun skk-nicola-setup-tutorial ()
  (static-unless (memq skk-emacs-type '(nemacs mule1))
    (dolist (key skk-nicola-lshift-keys)
      (define-key skktut-j-mode-map key 'skk-nicola-self-insert-lshift)
      (define-key skktut-latin-mode-map key 'skk-nicola-turn-on-j-mode))
    (dolist (key skk-nicola-rshift-keys)
      (define-key skktut-j-mode-map key 'skk-nicola-self-insert-rshift)
      (define-key skktut-latin-mode-map key 'skk-nicola-turn-on-j-mode))))

;;;###autoload
(defun skk-nicola-help (&optional arg)
  ;; キー配列を表示する。
  (interactive "p")
  (describe-variable
   (intern (format "skk-%s-keymap-display" skk-kanagaki-keyboard-type))))

;;;###autoload
(defun skk-nicola-2nd-help ()
  ;; skk-nicola.el 独自のキー定義一覧を表示する。
  (interactive)
  (skk-kanagaki-help-1
   "* SKK 親指シフト入力 ヘルプ*"
   "親指シフト入力モードの独自キー定義:"
   (nconc
    ;;
    (mapcar (function
	     (lambda (key)
	       (cons (key-description key) "左親指シフトキー")))
	    skk-nicola-lshift-keys)
    ;;
    (mapcar (function
	     (lambda (key)
	       (cons (key-description key) "右親指シフトキー")))
	    skk-nicola-rshift-keys)
    ;;
    (list (cons "SPC" "送りなし変換開始"))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree) (cdr spec))
	  (list nil (car spec))
	  (str nil (when (memq
			  (nth 3 list)
			  '(skk-input-by-code-or-menu))
		     (nth 1 list))))
	 ((or str (null spec))
	  (when (stringp str)
	    (cons str "コードまたはメニューによる入力")))))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree) (cdr spec))
	  (list nil (car spec))
	  (str nil (when (memq
			  (nth 3 list)
			  '(skk-today))
		     (nth 1 list))))
	 ((or str (null spec))
	  (when (stringp str)
	    (cons str "今日の日付けを挿入")))))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree) (cdr spec))
	  (list nil (car spec))
	  (str nil (when (memq
			  (nth 3 list)
			  '(skk-jisx0208-latin-mode))
		     (nth 1 list))))
	 ((or str (null spec))
	  (when (stringp str)
	    (cons str "全英モード")))))
    ;;
    (list
     (cons (format "%c + %c"
		   (car skk-nicola-set-henkan-point-chars)
		   (cadr skk-nicola-set-henkan-point-chars))
	   "変換開始点をセット、送り開始点指定")
     (cons (format "%c + %c"
		   (car skk-nicola-prefix-suffix-abbrev-chars)
		   (cadr skk-nicola-prefix-suffix-abbrev-chars))
	   "接頭辞 or 接尾辞変換 (▽モード or ▼モード)、abbrev モード")
     (cons (format "%c + %c"
		   (car skk-nicola-toggle-kana-chars)
		   (cadr skk-nicola-toggle-kana-chars))
	   "カナモード or カナ変換")
     (cons "左親指シフト + 右親指シフト" "latin モード ⇔ かなモード切り替え")
     (cons (format "M-x help %s" skk-nicola-help-key)
	   "現在の入力方式のキー配列を表示")
     (cons (format "M-x help %s" skk-nicola-2nd-help-key)
	   "このヘルプを表示")))))

;;;###autoload
(defun skk-nicola-self-insert-rshift (&optional arg)
  "右シフトに割り付ける関数。"
  (interactive "p")
  (skk-nicola-self-insert-lshift arg))

;;;###autoload
(defun skk-nicola-self-insert-lshift (&optional arg)
  "左シフトに割り付ける関数。"
  (interactive "p")
  ;;
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point) (marker-position skk-nicola-okuri-flag)))
	    (or (not skk-henkan-on) skk-henkan-active))
    (setq skk-nicola-okuri-flag nil))
  ;;
  (skk-nicola-insert arg))

;;;###autoload
(defun skk-nicola-turn-on-j-mode (&optional arg)
  ;; `skk-latin-mode' において、 左右親指キーの同時打鍵によって 'skk-j-mode'
  ;; に入る。
  (interactive "*p")
  (if (skk-sit-for skk-nicola-latin-interval t)
      ;; then
      (let ((last-command-char
	     (if (characterp (event-to-character last-command-event))
		 (event-to-character last-command-event)
	       ?\ )))
	(call-interactively 'self-insert-command t))
    ;; else
    (let ((last (static-cond
		 ((eq skk-emacs-type 'xemacs)
		  (event-key last-command-event))
		 ((memq skk-emacs-type '(nemacs mule1))
		  last-command-char)
		 (t last-command-event)))
	  (next (static-cond
		 ((eq skk-emacs-type 'xemacs)
		  (event-key (next-command-event)))
		 (t (next-command-event))))
	  char)
      (if (eq last next)
	  ;; then
	  (let ((last-command-char
		 (if (characterp (event-to-character last-command-event))
		     (event-to-character last-command-event)
		   ?\ )))
	    (call-interactively 'self-insert-command t)
	    (call-interactively 'self-insert-command t))
	;; else
	(when (characterp next)
	  (setq char next)
	  (setq next (key-description (char-to-string char))))
	(when (eq next 'space)
	  (setq next (key-description " ")))
	(when (symbolp next)
	  (setq next (key-description (vector next))))
	;;
	(unless (stringp next)
	  (setq next (format "%s" next)))
	;;
	(cond ((member next
		       (mapcar (function
				(lambda (key)
				  (key-description key)))
			       (append
				skk-nicola-rshift-keys
				skk-nicola-lshift-keys)))
	       (skk-j-mode-on)
	       (when (and skk-use-color-cursor (skk-color-display-p))
		 ;; 新しい skk-cursor 対策
		 (static-cond
		  ((eq skk-emacs-type 'xemacs)
		   (set-face-property
		    'text-cursor 'background skk-cursor-hiragana-color
		    (current-buffer)))
		  (t
		   (set-buffer-local-cursor-color
		    skk-cursor-hiragana-color)))))
	      (char
	       (let ((last-command-char
		      (if (characterp (event-to-character last-command-event))
			  (event-to-character last-command-event)
			?\ )))
		 (call-interactively 'self-insert-command t))
	       (let ((last-command-char char))
		 (call-interactively 'self-insert-command t))))))))

;;;###autoload
(defun skk-nicola-insert (&optional arg)
  ;; 同時打鍵を認識して、NICOLA かな入力をする。
  (interactive "*p")
  (unless (and skk-henkan-on (not skk-henkan-active))
    (setq skk-nicola-okuri-flag nil))
  ;;
  (cond ((skk-sit-for skk-nicola-interval t)
	 ;; No input in the interval.
	 (case this-command
	   (skk-nicola-self-insert-rshift
	    ;; (変換・スペース)
	    (skk-nicola-space-function arg))
	   (skk-nicola-self-insert-lshift
	    ;; 左シフト
	    (skk-nicola-lshift-function arg))
	   (t
	    ;; 文字
	    (skk-nicola-insert-kana last-command-char skk-nicola-plain-rule
				    arg))))
	(t
	 ;; Some key's pressed.
	 (let ((next (next-command-event))
	       nextasstr)
	   ;;
	   (static-cond
	    ((eq skk-emacs-type 'xemacs)
	     (let ((char (event-to-character next)))
	       (setq next (cond ((characterp char) char)
				(t (event-key next))))))
	    (t
	     (cond ((symbolp next)
		    (setq next (vector next)))
		   ((characterp next)
		    (setq nextasstr (char-to-string next))))))
	   ;;
	   (case (lookup-key skk-j-mode-map (or nextasstr next))
	     (skk-nicola-self-insert-rshift
	      ;; 右シフト
	      (case this-command
		(skk-nicola-self-insert-rshift
		 ;; [右 右]
		 (let ((last-command-char ?\ ))
		   (cond ((or skk-henkan-on skk-henkan-active)
			  (skk-kanagaki-insert arg)
			  (unless (>= skk-nicola-interval 1)
			    ;; Emacs 18  で単独打鍵を同一キー連続打鍵で代用で
			    ;; きるように。
			    (skk-kanagaki-insert arg)))
			 (t
			  (self-insert-command
			   (if (>= skk-nicola-interval 1)
			       ;; Emacs 18 で単独打鍵を同一キー連続打鍵で代用
			       ;; できるように。
			       arg
			     (1+ arg)))))))
		(skk-nicola-self-insert-lshift
		 ;; [左 右]
		 (cond ((and skk-j-mode (not skk-katakana))
			(skk-latin-mode 1))
		       (t
			(skk-toggle-kana 1))))
		(t
		 ;; [文字 右]
		 (skk-nicola-insert-kana last-command-char
					 skk-nicola-rshift-rule arg))))
	     (skk-nicola-self-insert-lshift
	      ;; 左シフト
	      (case this-command
		(skk-nicola-self-insert-lshift
		 ;;[左 左]
		 (cond ((skk-in-minibuffer-p)
			(exit-minibuffer))
		       (t
			(skk-nicola-lshift-function arg)
			(unless (>= skk-nicola-interval 1)
			  ;; Emacs 18 で単独打鍵を同一キー連続打鍵で代用でき
			  ;; るように。
			  (skk-nicola-lshift-function 1)))))
		(skk-nicola-self-insert-rshift
		 ;; [右 左]
		 (if (and skk-j-mode (not skk-katakana))
		     (skk-latin-mode 1)
		   (skk-toggle-kana 1)))
		(t
		 ;; [文字 左]
		 (skk-nicola-insert-kana last-command-char
					 skk-nicola-lshift-rule arg))))
	     (t
	      ;; 文字
	      (cond
	       ((eq this-command 'skk-nicola-self-insert-rshift)
		;;  [右 文字]
		(skk-nicola-insert-kana next skk-nicola-rshift-rule arg))
	       ((eq this-command 'skk-nicola-self-insert-lshift)
		;; [左 文字]
		(skk-nicola-insert-kana next skk-nicola-lshift-rule arg))
	       ((and
		 (not (eq last-command-char next))
		 (memq last-command-char skk-nicola-set-henkan-point-chars)
		 (memq next skk-nicola-set-henkan-point-chars))
		;; [fj]
		(cond ((and skk-henkan-on (not skk-henkan-active))
		       (if (memq skk-kanagaki-keyboard-type '(106-jis))
			   (skk-kanagaki-set-okurigana-no-sokuon arg)
			 (skk-nicola-set-okuri-flag)))
		      (t
		       (skk-set-henkan-point-subr 1))))
	       ((and
		 (not (eq last-command-char next))
		 (memq last-command-char skk-nicola-prefix-suffix-abbrev-chars)
		 (memq next skk-nicola-prefix-suffix-abbrev-chars))
		;; [gh] suffix の 入力
		(cond (skk-henkan-active
		       ;; 接尾語の処理
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
		       (skk-henkan))
		      (t
		       ;;
		       (skk-abbrev-mode 1))))
	       ((and
		 (not (eq last-command-char next))
		 (memq last-command-char skk-nicola-toggle-kana-chars)
		 (memq next skk-nicola-toggle-kana-chars))
		;; [dk]
		(skk-toggle-kana 1))
	       (t
		;; [文字 文字]
		(let ((str (skk-nicola-insert-kana
			    last-command-char skk-nicola-plain-rule arg)))
		  (when (and skk-isearch-switch
			     (not (or skk-henkan-on skk-henkan-active)))
		    (setq isearch-cmds
			  (cons
			   (nconc
			    (list (concat (caar isearch-cmds) str)
				  (concat (cadar isearch-cmds) str))
			    (cddar isearch-cmds))
			   isearch-cmds))))
		(unless (and (>= skk-nicola-interval 1)
			     (eq next last-command-char))
		  ;; Emacs 18 で単独打鍵を同一キー連続打鍵で代用できるように。
		  (skk-nicola-insert-kana next skk-nicola-plain-rule)))))))))
  ;; `skk-kana-input' が何も入力しないように、nil を返しておく。
  nil)

(defun skk-nicola-insert-kana (char rule &optional arg)
  ;; CHAR を RULE の中から探して入力すべき文字列を決定する。
  ;; ARG を与えられた場合はその数だけ文字列を連結して入力する。
  (let* ((el (cadr (assq char rule)))
	 (str (when el (cond ((stringp el) el)
			     (skk-katakana (car el))
			     (t (cdr el)))))
	 (arg (prefix-numeric-value arg)))
    ;;
    (when str
      (skk-insert-str (setq str (skk-kanagaki-make-string arg str))))
    ;;
    (cond (skk-nicola-okuri-flag
	   (skk-nicola-process-okuri))
	  (t
	   ;;
	   (when skk-henkan-active (skk-kakutei))))
    ;; 何かに使うことがあるかもしれないので、STR を返しておく。
    str))

(defun skk-nicola-process-okuri ()
  ;; 送り開始の標識により送り開始点を認識し、送りあり変換を開始する。
  (let ((okuri (buffer-substring-no-properties
                              (1+ skk-nicola-okuri-flag) (point)))
	(len (if (eq skk-emacs-type 'nemacs) 2 1)) tag)
    (cond ((and (not (eq skk-nicola-okuri-style 'nicola-skk))
		(member okuri '("っ" "ッ")))
	   ;; 何もしない。
	   )
	  (t
	   (save-excursion
	    (goto-char skk-nicola-okuri-flag)
	    (when (eq (following-char) ?*)
	      (delete-char 1))
	    (backward-char (* len 1))
	    (if (member
		 (buffer-substring-no-properties
		  (point) (marker-position skk-nicola-okuri-flag))
		 '("っ" "ッ"))
		(setq tag 'no-sokuon)))
	   (static-cond
	    ((memq skk-emacs-type '(nemacs mule1))
	     ;; 理由がよく分からないが、point がズレてしまう。`skk-insert' を
	     ;; 抜けてから変換するとうまくいく。(??)
	     (throw 'okuri (or tag 'ari)))
	    (t
	     (skk-kanagaki-set-okurigana tag)))))))

(defun skk-nicola-set-okuri-flag ()
  ;; 送り開始点を marker で標識するとともに、`*' を挿入することで送りあり変換
  ;; の待ち状態であることを明示する。
  (interactive)
  (when (and skk-henkan-on (not skk-henkan-active))
    ;; ▽モードのときだけ機能する。
    (let ((pt (point)))
      (unless (and (string= "*" (buffer-substring-no-properties (1- pt) pt))
		   (markerp skk-nicola-okuri-flag))
	;; 既に標識済みなら何もしない。
	(skk-set-marker skk-nicola-okuri-flag pt)
	(insert-and-inherit "*")))))

(defun skk-nicola-space-function (&optional arg)
  (let ((last-command-char ?\ ))
    (cond
     (skk-henkan-active
      (call-interactively 'skk-insert))
     (skk-henkan-on
      (skk-kanagaki-insert arg))
     (t
      (self-insert-command arg)))))

(defun skk-nicola-lshift-function (&optional arg)
  (cond ((or skk-henkan-active skk-henkan-on)
	 ;; 確定に使う。
	 (skk-kakutei))
	(skk-nicola-use-lshift-as-space
	 ;;
	 (skk-nicola-space-function arg))
	(t
	 ;; 改行に使う。
	 (if (skk-in-minibuffer-p)
	     (exit-minibuffer)
	   (newline arg)))))

;; Pieces of Advice.

(defadvice skk-insert (before skk-nicola-ad activate compile)
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point) (marker-position skk-nicola-okuri-flag)))
	    (or (not skk-henkan-on) skk-henkan-active))
    (setq skk-nicola-okuri-flag nil)))

(defadvice skk-kakutei (before skk-nicola-ad activate compile)
  ;;
  (when (and skk-j-mode skk-henkan-on (not skk-henkan-active)
	     (markerp skk-nicola-okuri-flag))
    ;; 確定するときは送り開始の標識を消す。
    (save-excursion
      (goto-char skk-nicola-okuri-flag)
      (when (eq (following-char) ?*)
	(delete-char 1))))
  ;;
  (setq skk-nicola-okuri-flag nil))

(defadvice skk-previous-candidate (before skk-nicola-ad activate compile)
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point) (marker-position skk-nicola-okuri-flag)))
	    (or (not skk-henkan-on) skk-henkan-active))
    (setq skk-nicola-okuri-flag nil)))

(defadvice skk-nicola-self-insert-lshift (around skk-nicola-ad-for-dcomp
						 activate compile)
  (eval-when-compile
    (defvar skk-dcomp-start-point)
    (defvar skk-dcomp-end-point)
    (defvar skk-dcomp-keep-completion-keys)
    (autoload 'skk-dcomp-face-off "skk-dcomp")
    (autoload 'skk-dcomp-face-on "skk-dcomp"))
  (cond
   ((featurep 'skk-dcomp)
    (if (or skk-henkan-active (not skk-henkan-on))
	ad-do-it
      (let (pos)
	(if (or skk-henkan-active (skk-get-prefix skk-current-rule-tree)
		(not skk-comp-stack))
	    (progn
	      (skk-set-marker skk-dcomp-start-point nil)
	      (skk-set-marker skk-dcomp-end-point nil))
	  (when (and (marker-position skk-dcomp-start-point)
		     (marker-position skk-dcomp-end-point))
	    (skk-dcomp-face-off)
	    (or (member (this-command-keys) skk-dcomp-keep-completion-keys)
		;;
		(if (eq this-command 'skk-nicola-self-insert-rshift)
		    (setq pos (point))
		  ;;
		  (condition-case nil
		      (delete-region skk-dcomp-start-point skk-dcomp-end-point)
		    (error))))))
	ad-do-it
	;;
	(when (and (eq this-command 'skk-nicola-self-insert-rshift)
		   skk-henkan-on
		   (not skk-henkan-active))
	  (when (and (markerp skk-dcomp-start-point)
		     (marker-position skk-dcomp-start-point)
		     (< (marker-position skk-dcomp-start-point) pos))
	    (delete-region skk-dcomp-start-point pos))
	  (when (and (markerp skk-dcomp-end-point)
		     (marker-position skk-dcomp-end-point)
		     (< (point) (marker-position skk-dcomp-end-point)))
	    (delete-region skk-dcomp-end-point (point))))
	;;
	(if (and (not (skk-get-prefix skk-current-rule-tree))
		 (not skk-okurigana))
	    (let ((pos (point)))
	      (condition-case nil
		  (progn
		    (skk-comp-do 'first 'silent)
		    (skk-set-marker skk-dcomp-start-point pos)
		    (skk-set-marker skk-dcomp-end-point (point))
		    (skk-dcomp-face-on skk-dcomp-start-point skk-dcomp-end-point)
		    (goto-char skk-dcomp-start-point))
		(error
		 (setq skk-comp-stack nil)
		 (message nil))))))))
   (t
    ad-do-it)))

(static-unless (memq skk-emacs-type '(nemacs mule1))
  ;;
  (defadvice skk-isearch-setup-keymap (before skk-nicola-ad activate compile)
    ;; 親指キーでサーチが終了してしまわないように。
    (let ((keys (append skk-nicola-lshift-keys skk-nicola-rshift-keys)))
      (while keys
	(define-key (ad-get-arg 0) (car keys) 'skk-isearch-wrapper)
	(setq keys (cdr keys)))))
  ;;
  (defadvice isearch-char-to-string (around skk-nicola-ad activate compile)
    ;; エラーが出ると検索が中断して使い辛いので、黙らせる。
    (cond ((and skk-use-kana-keyboard (featurep 'skk-isearch)
		(with-current-buffer
		    (get-buffer-create skk-isearch-working-buffer)
		  skk-mode))
	   (condition-case nil
	       ad-do-it
	     (error)))
	  (t
	   ad-do-it)))
  ;;
  (defadvice isearch-text-char-description (around skk-nicola-ad activate
						   compile)
    ;; エラーが出ると検索が中断して使い辛いので、黙らせる。
    (cond ((and skk-use-kana-keyboard (featurep 'skk-isearch)
		(with-current-buffer
		    (get-buffer-create skk-isearch-working-buffer)
		  skk-mode))
	   (condition-case nil
	       ad-do-it
	     (error)))
	  (t
	   ad-do-it))))

(static-when (eq skk-emacs-type 'mule2)
  ;;
  (defadvice isearch-char-to-string (after skk-nicola-ad activate compile)
    ;; この関数が日本語をちゃんと扱えないことに対策。
    (when (integerp (ad-get-arg 0))
      (setq ad-return-value (skk-char-to-string (ad-get-arg 0))))))

(static-when (memq skk-emacs-type '(nemacs mule1))
  ;; バグの原因が明らかになるまでの work around。
  (defadvice skk-insert (around skk-nicola-ad-e18 activate)
    (cond (skk-nicola-okuri-flag
	   (let ((tag (catch 'okuri (skk-nicola-insert (ad-get-arg 0)))))
	     (when (memq tag '(ari no-sokuon))
	       (skk-kanagaki-set-okurigana (eq tag 'no-sokuon)))))
	  (t
	   ad-do-it)))
  ;;
  (defadvice skk-previous-candidate (around skk-nicola-ad-e18 activate)
    (cond (skk-nicola-okuri-flag
	   (let ((tag (catch 'okuri (skk-nicola-insert (ad-get-arg 0)))))
	     (when (memq tag '(ari no-sokuon))
	       (skk-kanagaki-set-okurigana (eq tag 'no-sokuon)))))
	  (t
	   ad-do-it)))
  ;;
  (defadvice skk-nicola-self-insert-lshift (around skk-nicola-ad-e18 activate)
    (cond (skk-nicola-okuri-flag
	   (let ((tag (catch 'okuri (skk-nicola-insert (ad-get-arg 0)))))
	     (when (memq tag '(ari no-sokuon))
	       (skk-kanagaki-set-okurigana (eq tag 'no-sokuon)))))
	  (t
	   ad-do-it))))

;;

(put 'skk-nicola-insert 'isearch-command t)
(put 'skk-nicola-self-insert-lshift 'isearch-command t)
(put 'skk-nicola-self-insert-rshift 'isearch-command t)

;;

(require 'product)
(product-provide (provide 'skk-nicola) (require 'skk-version))

;; skk-nicola.el ends here
