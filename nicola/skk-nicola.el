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

;; このプログラムは箕浦逸史さん作の  NICOLA-SKK version 0.39 を基に、 Daredevil
;; SKK に対応させたものです。原作のアイデアに基いて実装していく予定です。
;;
;; キー配列のルールは別ファイルに細分化しています。これらは、同じく箕浦さん作の
;; omelet (たまご用の親指シフト入力インターフェイス) および同氏の web site の文
;; 章を基につくりました。
;;
;; 同氏のアイデアと親指シフト入力に関するご尽力に敬意を表します。

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

(defgroup skk-nicola nil "SKK NICOLA related customization."
  :prefix "skk-nicola-"
  :group 'skk
  :group 'skk-kanagaki)

;; Variables.

(defcustom skk-nicola-interval
  (if (featurep 'lisp-float-type) (/ (float 1) (float 10)) 1) "\
*この時間以内に打鍵されたものを同時打鍵と判定する。
単位は秒。デフォルトは 0.1 秒。ただし、 Emacs 18 の場合は浮動小数点数を扱えない
ため、仮に 1 秒としておくが、実用的には厳しいと思われる。"
  :type 'number
  :group 'skk-nicola)

(defcustom skk-nicola-latin-interval
  (if (featurep 'lisp-float-type) (/ (float 1) (float 10)) 1) "\
*この時間以内に打鍵されたものを同時打鍵と判定する。
単位は秒。デフォルトは 0.1 秒。ただし、 Emacs 18 の場合は浮動小数点数を扱えない
ため、仮に 1 秒としておくが、実用的には厳しいと思われる。"
  :type 'number
  :group 'skk-nicola)

(defcustom skk-nicola-lshift-key
  (cond ((eq system-type 'windows-nt) [noconvert])
	((memq skk-emacs-type '(nemacs mule1 mule3)) "\\")
	(t [muhenkan])) "\
*左親指キーとして使うキー。"
  :type 'sexp
  :group 'skk-nicola)

(defcustom skk-nicola-rshift-key
  (cond ((eq system-type 'windows-nt) [convert])
	((eq skk-emacs-type 'xemacs) [henkan-mode])
	((string-match "^19.28" emacs-version) [key-35])
	((string-match "^19.\\(29\\|3[0-4]\\)" emacs-version) [numbersign])
	((memq skk-emacs-type '(nemacs mule1 mule3)) " ")
	(t [henkan])) "\
*右親指キーとして使うキー。"
  :type 'sexp
  :group 'skk-nicola)

(defcustom skk-nicola-use-lshift-as-space nil "\
*Non-nil であれば左親指キーもスペースキーとして利用する。"
  :type 'boolean
  :group 'skk-nicola)

(defcustom skk-nicola-use-space-as-rshift t "\
*Non-nil であればスペースキーも右親指キーとして利用する。"
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

(skk-deflocalvar skk-nicola-lshift-original nil "\
skk-mode に入る際に、左親指キーの本来の定義を調べるための変数。")

(skk-deflocalvar skk-nicola-okuri-flag nil)

(skk-deflocalvar skk-current-local-map nil "\
Emacs 18 用の変数。")

(static-when (memq skk-emacs-type '(nemacs mule1 mule3))
  (case skk-kanagaki-jidou-key-symbol-kakikae-service
    ;;
    (nicola-jis
     ;; Emacs 18 では Muhenkan, Henkan は使えないようなので、使えるキーに書換え
     ;; る。
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
	  (setq skk-nicola-lshift-key [f18]
		skk-nicola-rshift-key [f19]))
	 (t
	  (setq skk-nicola-lshift-key "_"
		skk-nicola-rshift-key " ")))))))

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
    (setq skk-nicola-lshift-original
	  (static-cond
	   ((memq skk-emacs-type '(nemacs mule1))
	    (or (lookup-key (or skk-current-local-map
				(make-sparse-keymap))
			    skk-nicola-lshift-key)
		(lookup-key (current-global-map)
			    skk-nicola-lshift-key)))
	   (t
	    (let (skk-mode skk-j-mode)
	      (key-binding skk-nicola-lshift-key)))))
    ;;
    (case skk-kanagaki-state
      (kana
       (setq skk-hiragana-mode-string skk-nicola-hiragana-mode-string
	     skk-katakana-mode-string skk-nicola-katakana-mode-string))
      (rom
       (setq skk-hiragana-mode-string skk-nicola-hiragana-rom-string
	     skk-katakana-mode-string skk-nicola-katakana-rom-string)))
    (setq skk-hiragana-mode-indicator
	  (skk-mode-string-to-indicator 'hiragana
					skk-hiragana-mode-string))
    (setq skk-katakana-mode-indicator
	  (skk-mode-string-to-indicator 'katakana
					skk-katakana-mode-string))
    (skk-update-modeline (if skk-katakana
			     skk-katakana-mode-indicator
			   skk-hiragana-mode-indicator)))))

;; Functions.

(defun skk-nicola-setup ()
  ;; SKK の初回起動時のみ実行されるべきものはこの関数に入れる。
  (when skk-nicola-use-space-as-rshift
    (define-key skk-j-mode-map " " 'skk-nicola-self-insert-rshift))
  ;;
  (define-key skk-j-mode-map skk-nicola-lshift-key
    'skk-nicola-self-insert-lshift)
  (define-key skk-j-mode-map skk-nicola-rshift-key
    'skk-nicola-self-insert-rshift)
  ;;
  (when skk-nicola-use-space-as-rshift
    (define-key skk-latin-mode-map " "
      'skk-nicola-turn-on-j-mode))
  ;;
  (define-key skk-latin-mode-map skk-nicola-lshift-key
    'skk-nicola-turn-on-j-mode)
  (define-key skk-latin-mode-map skk-nicola-rshift-key
    'skk-nicola-turn-on-j-mode)
  ;;
  (when skk-nicola-help-key
    (define-key help-map skk-nicola-help-key
      'skk-nicola-help))
  (when skk-nicola-2nd-help-key
    (define-key help-map skk-nicola-2nd-help-key
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
  (remove-hook 'skk-mode-hook 'skk-niola-setup))

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
   (append
    '((skk-nicola-lshift-key . "左親指シフトキー")
      (skk-nicola-rshift-key . "右親指シフトキー"))
    ;;
    (list
     (when (and skk-nicola-use-space-as-rshift
		(not (member (key-description skk-nicola-rshift-key)
			     '("SPC" "space"))))
       '("space" . "右親指シフトキー、送りなし変換開始")))
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
	    (cons str "送りあり変換開始")))))
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
  ;; `skk-latin-mode' において、 左右親指キーの同時打鍵によって 'skk-j-mode' に
  ;; 入る。
  (interactive "*p")
  (if (skk-sit-for skk-nicola-latin-interval t)
      ;; then
      (let ((last-command-char ?\ ))
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
	  (let ((last-command-char ?\ ))
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
				(list skk-nicola-rshift-key
				      skk-nicola-lshift-key)
				(when skk-nicola-use-space-as-rshift
				  (list " ")))))
	       (skk-j-mode-on)
	       (when (and skk-use-color-cursor (skk-color-display-p))
		 ;; 新しい skk-cursor 対策
		 (static-cond
		  ((eq skk-emacs-type 'xemacs)
		   (set-face-property
		    'text-cursor 'background skk-cursor-hiragana-color
		    (current-buffer)))
		  (t
		   (set-buffer-local-cursor-color skk-cursor-hiragana-color)))))
	      (char
	       (let ((last-command-char ?\ ))
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
			    ;; Emacs 18  で単独打鍵を同一キー連続打鍵で代用でき
			    ;; るように。
			    (skk-kanagaki-insert arg)))
			 (t
			  (self-insert-command
			   (if (>= skk-nicola-interval 1)
			       ;; Emacs 18 で単独打鍵を同一キー連続打鍵で代用で
			       ;; きるように。
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
			  ;; Emacs 18  で単独打鍵を同一キー連続打鍵で代用できる
			  ;; ように。
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
		       (skk-nicola-set-okuri-flag))
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
		       (insert ?>))
		      ((and skk-henkan-on (not skk-henkan-active))
		       ;; 接頭語の処理
		       (insert ?>)
		       (skk-set-marker skk-henkan-end-point (point))
		       (setq skk-henkan-count 0
			     skk-henkan-key (buffer-substring
					     skk-henkan-start-point (point)))
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
			   (append
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
	     ;; 理由がよく分からないが、point がズレてしまう。`skk-insert' を抜
	     ;; けてから変換するとうまくいく。(??)
	     (throw 'okuri (or tag 'ari)))
	    (t
	     (skk-kanagaki-set-okurigana tag)))))))

(defun skk-nicola-set-okuri-flag ()
  ;; 送り開始点を marker で標識するとともに、`*' を挿入することで送りあり変換の
  ;; 待ち状態であることを明示する。
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
    (if (or skk-henkan-on skk-henkan-active)
	;; 変換する。
	(skk-kanagaki-insert arg)
      (self-insert-command arg))))

(defun skk-nicola-lshift-function (&optional arg)
  (cond ((or skk-henkan-active skk-henkan-on)
	 ;; 確定に使う。
	 (let ((skk-egg-like-newline t))
	   (newline arg)))
	(skk-nicola-use-lshift-as-space
	 ;;
	 (skk-nicola-space-function arg))
	(t
	 ;; 改行に使う。
	 (if (skk-in-minibuffer-p)
	     (call-interactively 'exit-minibuffer)
	   (newline arg)))))

(defun skk-nicola-setup-tutorial ()
  (when skk-nicola-use-space-as-rshift
    (define-key skktut-j-mode-map " " 'skk-nicola-self-insert-rshift))
  ;;
  (define-key skktut-j-mode-map skk-nicola-lshift-key
    'skk-nicola-self-insert-lshift)
  (define-key skktut-j-mode-map skk-nicola-rshift-key
    'skk-nicola-self-insert-rshift)
  ;;
  (when skk-nicola-use-space-as-rshift
    (define-key skktut-latin-mode-map " "
      'skk-nicola-turn-on-j-mode))
  ;;
  (define-key skktut-latin-mode-map skk-nicola-lshift-key
    'skk-nicola-turn-on-j-mode)
  (define-key skktut-latin-mode-map skk-nicola-rshift-key
    'skk-nicola-turn-on-j-mode))

;; Pieces of Advice.

(defadvice skk-insert (before skk-nicola-ad activate compile)
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point) (marker-position skk-nicola-okuri-flag)))
	    (or (not skk-henkan-on) skk-henkan-active))
    (setq skk-nicola-okuri-flag nil)))

(defadvice skk-latin-mode (before skk-nicola-ad activate compile)
  (setq skk-nicola-okuri-flag nil))

(defadvice skk-toggle-kana (before skk-nicola-ad activate compile)
  (setq skk-nicola-okuri-flag nil))

(defadvice skk-abbrev-mode (before skk-nicola-ad activate compile)
  (setq skk-nicola-okuri-flag nil))

(defadvice skk-jisx0208-latin-mode (before skk-nicola-ad activate compile)
  (setq skk-nicola-okuri-flag nil))

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

(defadvice skk-kanagaki-toggle-rom-kana (around skk-nicola-ad activate
						preactivate)
  (setq skk-nicola-okuri-flag nil)
  ad-do-it
  ;; モード行の表示の調節。
  (case skk-kanagaki-state
    (kana
     (setq skk-hiragana-mode-string skk-nicola-hiragana-mode-string
	   skk-katakana-mode-string skk-nicola-katakana-mode-string))
    (rom
     (setq skk-hiragana-mode-string skk-nicola-hiragana-rom-string
	   skk-katakana-mode-string skk-nicola-katakana-rom-string)))
  (setq skk-hiragana-mode-indicator
	(skk-mode-string-to-indicator 'hiragana
				      skk-hiragana-mode-string))
  (setq skk-katakana-mode-indicator
	(skk-mode-string-to-indicator 'katakana
				      skk-katakana-mode-string))
  (let ((list (buffer-list))
	buf)
    (while list
      (when (buffer-live-p (setq buf (car list)))
	(with-current-buffer buf
	  (when skk-j-mode
	    (skk-update-modeline (if skk-katakana
				     skk-katakana-mode-indicator
				   skk-hiragana-mode-indicator)))))
      (setq list (cdr list))))
  (force-mode-line-update t))

(defadvice skk-kanagaki-start-henkn-okuriari (before skk-nicola-ad activate
						     compile)
  (setq skk-nicola-okuri-flag nil))

(defadvice skk-previous-candidate (before skk-nicola-ad activate compile)
  (when (or (and (markerp skk-nicola-okuri-flag)
		 (<= (point) (marker-position skk-nicola-okuri-flag)))
	    (or (not skk-henkan-on) skk-henkan-active))
    (setq skk-nicola-okuri-flag nil)))

(static-unless (memq skk-emacs-type '(nemacs mule1))
  ;;
  (defadvice skk-isearch-setup-keymap (before skk-nicola-ad activate compile)
    ;; 親指キーでサーチが終了してしまわないように。
    (define-key (ad-get-arg 0) skk-nicola-lshift-key 'skk-isearch-wrapper)
    (define-key (ad-get-arg 0) skk-nicola-rshift-key 'skk-isearch-wrapper))
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
  ;;
  (defadvice skk-insert (around skk-nicola-ad-e18 activate)
    ;; バグの原因が明らかになるまでの work around。
    (cond (skk-nicola-okuri-flag
	   (let ((tag (catch 'okuri (skk-nicola-insert (ad-get-arg 0)))))
	     (when (memq tag '(ari no-sokuon))
	       (skk-kanagaki-set-okurigana (eq tag 'no-sokuon)))))
	  (t
	   ad-do-it)))
  ;;
  (defadvice skk-nicola-self-insert-lshift (around skk-nicola-ad-e18 activate)
    ;; バグの原因が明らかになるまでの work around。
    (cond (skk-nicola-okuri-flag
	   (let ((tag (catch 'okuri (skk-nicola-insert (ad-get-arg 0)))))
	     (when (memq tag '(ari no-sokuon))
	       (skk-kanagaki-set-okurigana (eq tag 'no-sokuon)))))
	  (t
	   ad-do-it))))

(if (featurep 'skk-tut)
    (skk-nicola-setup-tutorial)
  ;;
  (defadvice skk-tutorial (after skk-nicola-advice-to-skk-tutorial activate)
    (skk-nicola-setup-tutorial)
    (ad-deactivate-regexp "^skk-nicola-advice-for skk-tutorial$")))

;;

(put 'skk-nicola-insert 'isearch-command t)
(put 'skk-nicola-self-insert-lshift 'isearch-command t)
(put 'skk-nicola-self-insert-rshift 'isearch-command t)

;;

(require 'product)
(product-provide (provide 'skk-nicola) (require 'skk-version))

;; skk-nicola.el ends here
