;;; skk.el --- Daredevil SKK (Simple Kana to Kanji conversion program)

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999, 2000, 2001
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk.el,v 1.209 2001/12/09 12:29:31 czkmt Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2001/12/09 12:29:31 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either versions 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; SKK-MODE is a mode for inputting Japanese to a current buffer which is
;; composed of four minor modes described below.
;;
;;      +---------------+-------- skk-mode -----+--------------------+
;;      |               |                       |                    |
;;      |               |                       |                    |
;;  skk-j-mode   skk-latin-mode   skk-jisx0208-latin-mode   skk-abbrev-mode
;;                  ASCII               JISX0208 LATIN         ABBREVIATION
;; (C-j wakes up skk-j-mode)      (ZEN'KAKU EIMOJI)
;;
;; skk-j-mode-map               skk-jisx0208-latin-mode-map
;;              skk-latin-mode-map                        skk-abbrev-mode-map
;;
;; skk-katakana: nil
;;   HIRAKANA
;;
;; skk-katakana: t
;;   KATAKANA

;;; Code:

(eval-when-compile ; shut up compiler warning.
  (defvar enable-character-translation)
  (defvar enable-character-unification)
  (defvar epoch::version)
  (defvar message-log-max)
  (defvar minibuffer-local-ns-map)
  (defvar self-insert-after-hook)
  (defvar skk-rdbms-private-jisyo-table)
  (defvar this-command-char))

;; APEL 10.2 or higher is required.
(eval-when-compile
  (require 'static))
(require 'poe)
(require 'poem) ; requires pces.
(require 'pces)
(require 'pcustom)
(require 'alist)

;; Elib 1.0 is required.
(require 'queue-m)

;; Emacs standard library.
(require 'advice)
(require 'easymenu)

(eval-and-compile
  ;; SKK common.
  (require 'skk-autoloads)
  (require 'skk-vars)
  (require 'skk-macs)
  ;; SKK version dependent.
  (static-cond
   ((eq skk-emacs-type 'mule5)
    (require 'skk-e21))
   ((eq skk-emacs-type 'xemacs)
    (require 'skk-xemacs)))
  ;; Shut up, compiler.
  (autoload 'skk-kanagaki-initialize "skk-kanagaki")
  (autoload 'skk-rdbms-count-jisyo-candidates "skk-rdbms"))

;; aliases.
(defalias 'skk-toggle-kana 'skk-toggle-characters)

;;;###autoload
(defun skk-mode (&optional arg)
  "日本語入力モード。
マイナーモードの一種で、オリジナルのモードには影響を与えない。
負の引数を与えると SKK モードから抜ける。

An input mode for Japanese, converting romanized phonetic strings to kanji.

A minor mode, it should not affect the use of any major mode or
orthogonal minor modes.

In the initial SKK mode, hiragana submode, the mode line indicator is
\"かな\".  Lowercase romaji inputs are automatically converted to
hiragana where possible.  The lowercase characters `q' and `l' change
submodes of SKK, and `x' is used as a prefix indicating a small kana.

`q' is used to toggle between hiragana and katakana \(mode line
indicator \"カナ\"\) input submodes.

`l' is used to enter ASCII submode \(mode line indicator \"SKK\"\).
Uppercase `L' enters JISX0208 latin \(wide ASCII\) submode \(mode line
indicator \"全英\"\).  `\C-j' returns to hiragana submode from either
ASCII submode.

Kanji conversion is complex, but the basic principle is that the user
signals the appropriate stem to be matched against dictionary keys by
the use of uppercase letters.  Because SKK does not use grammatical
information, both the beginning and the end of the stem must be marked.

For non-inflected words \(eg, nouns\) consisting entirely of kanji, the
simplest way to invoke conversion is to enter the reading of the kanji,
the first character only in uppercase.  A leading \"▽\" indicates that
kanji conversion is in progress.  After entering the reading, press
space.  This invokes dictionary lookup, and the hiragana reading will be
redisplayed in kanji as the first candidate.  Pressing space again gives
the next candidate.  Further presses of space produce further candidates,
as well as a list of the next few candidates in the minibuffer.  Eg,
\"Benri\" => \"▽べんり\", and pressing space produces \"▼便利\" \(the
solid triangle indicates that conversion is in progress\).  Backspace
steps through the candidate list in reverse.

A candidate can be accepted by pressing `\C-j', or by entering a
self-inserting character.  \(Unlike other common Japanese input methods,
RET not only accepts the current candidate, but also inserts a line
break.\)

Inflected words \(verbs and adjectives\), like non-inflected words, begin
input with a capital letter.  However, for these words the end of the
kanji string is signaled by capitalizing the next mora.  Eg, \"TuyoI\"
=> \"▼強い\".  If no candidate is available at that point, the inflection
point will be indicated with an asterisk \"*\", and trailing characters
will be displayed until a candidate is recognized.  It will be
immediately displayed \(pressing space is not necessary\).  Space and
backspace are used to step forward and backward through the list of
candidates.

For more information, see the `skk' topic in Info.  \(Japanese only.\)

A tutorial is available in Japanese or English via \"M-x skk-tutorial\".
Use a prefix argument to choose the language.  The default is system-
dependent."
  (interactive "P")
  (setq skk-mode (cond ((null arg)
			(not skk-mode))
		       ;; - は -1 に変換される。
		       ((> (prefix-numeric-value arg) 0)
			t)
		       (t
			nil)))
  (if (not skk-mode)
      ;; exit skk-mode
      (skk-mode-exit)
    ;; enter into skk-mode.
    (unless skk-mode-invoked
      ;; enter `skk-mode' for the first time in this session.
      (skk-mode-invoke))
    ;; 以下は skk-mode に入るたびに毎度コールされるコード。
    (skk-create-file skk-jisyo
		     "SKK の空辞書を作りました"
		     "I have created an empty SKK Jisyo file for you")
    (static-when (eq skk-emacs-type 'xemacs)
      (easy-menu-add skk-menu))
    (skk-require-module)
    ;; To terminate kana input.
    (static-unless (memq skk-emacs-type '(mule5))
      (make-local-hook 'pre-command-hook))
    (add-hook 'pre-command-hook 'skk-pre-command nil 'local)
    (static-unless (memq skk-emacs-type '(mule5))
      (make-local-hook 'post-command-hook))
    (add-hook 'post-command-hook 'skk-after-point-move nil 'local)
    (skk-j-mode-on)
    (run-hooks 'skk-mode-hook)))

;;;###autoload
(defun skk-auto-fill-mode (&optional arg)
  "日本語入力モード。自動折り返し機能付き。
マイナーモードの一種で、オリジナルのモードには影響を与えない。
正の引数を与えると、強制的に auto-fill-mode 及び SKK モードに入る。
負の引数を与えると auto-fill-mode 及び SKK モードから抜ける。"
  (interactive "P")
  (let ((auto-fill
	 (cond ((null arg)
		(not auto-fill-function))
	       ((> (prefix-numeric-value arg) 0)
		t))))
    (auto-fill-mode (if auto-fill 1 -1))
    (skk-mode arg)
    (run-hooks 'skk-auto-fill-mode-hook)))

(defun skk-kill-emacs-without-saving-jisyo (&optional query)
  "SKK 辞書をセーブしないで、Emacs を終了する。"
  (interactive "P")
  ;; format を引数に持たせた場合は、skk-yes-or-no-p を使うとかえって冗長にな
  ;; る。
  (when (yes-or-no-p
	 (format
	  (if skk-japanese-message-and-error
	      "辞書の保存をせずに %s を終了します。良いですか？"
	    "Do you really wish to kill %s without saving Jisyo? ")
	  (static-cond
	   ((eq skk-emacs-type 'xemacs) "XEmacs")
	   ((eq skk-emacs-type 'mule2) "Mule")
	   (t "Emacs"))))
    (let ((buff (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
      (ad-disable-advice 'save-buffers-kill-emacs 'before 'skk-ad)
      (ad-activate 'save-buffers-kill-emacs)
      (remove-hook 'skk-before-kill-emacs-hook 'skk-save-jisyo) ; fail safe.
      (when buff
	(set-buffer buff)
	(set-buffer-modified-p nil)
	(kill-buffer buff))
      (save-buffers-kill-emacs query))))

(defun skk-restart ()
  "skk-init-file の再ロード及び各種再セットアップの後、SKK モードを起動する。"
  (interactive)
  (let (skk-mode-invoked)
    (skk-mode 1)))

(defun skk-require-module ()
  (when skk-use-viper
    (require 'skk-viper))
  (when (or skk-servers-list
	    skk-server-host
	    (getenv "SKKSERVER"))
    (require 'skk-server))
  (when (featurep 'skk-server)
    (skk-adjust-search-prog-list-for-server-search))
  (when skk-auto-okuri-process
    (require 'skk-auto)
    (skk-adjust-search-prog-list-for-auto-okuri))
  (when skk-use-look
    (require 'skk-look))
  (when (featurep 'skk-jisx0201)
    (setq skk-use-jisx0201-input-method t))
  (when skk-dcomp-activate
    (require 'skk-dcomp)))

(defun skk-mode-exit ()
  (let ((skk-mode t))
    (skk-kakutei))
  (skk-mode-off)
  (remove-hook 'pre-command-hook
	       'skk-pre-command
	       'local)
  (remove-hook 'post-command-hook
	       'skk-after-point-move
	       'local)
  (skk-update-modeline)
  (static-when (eq skk-emacs-type 'xemacs)
    (easy-menu-remove skk-menu)))

(defun skk-mode-invoke ()
  (skk-setup-init-file)
  (load skk-init-file t)
  (unless (featurep 'tinycustom)
    (skk-cus-setup))
  (skk-setup-modeline)
  (when skk-share-private-jisyo
    (skk-setup-shared-private-jisyo))
  (when skk-keep-record
    (skk-create-file skk-record-file
		     "SKK の記録用ファイルを作りました"
		     "I have created an SKK record file for you"))
  (skk-setup-auto-paren) ; necessary to call before compiling skk-rule-tree.
  (when skk-use-kana-keyboard
    ;; 仮名入力を行う場合の初期設定。
    (skk-kanagaki-initialize))
  (skk-setup-delete-selection-mode)
  (skk-adjust-user-option)
  (setq skk-mode-invoked t))

;;; setup
(defun skk-setup-shared-private-jisyo ()
  (setq skk-jisyo-update-vector (make-vector skk-jisyo-save-count nil))
  (setq skk-emacs-id
	(make-temp-name
	 (concat (system-name) ":"
		 (mapconcat 'int-to-string (current-time) "")
		 ":")))
  (skk-create-file skk-emacs-id-file nil nil 384) ; 0600
  (with-temp-buffer
    (insert-file-contents skk-emacs-id-file)
    (insert skk-emacs-id "\n")
    (write-region 1 (point-max) skk-emacs-id-file nil 'nomsg)))

(defun skk-setup-keymap ()
  (cond
   (skk-j-mode
    (skk-define-j-mode-map)
    (unless (memq (lookup-key skk-j-mode-map skk-kakutei-key)
		  '(skk-insert skk-kakutei))
      (when (vectorp skk-kakutei-key)
	(define-key skk-j-mode-map skk-kakutei-key 'skk-kakutei))
      (define-key skk-j-mode-map (char-to-string skk-try-completion-char)
	'skk-insert)
      (unless (featurep 'skk-kanagaki)
	(define-key skk-j-mode-map (char-to-string skk-previous-candidate-char)
	  'skk-previous-candidate))
      (when skk-use-jisx0201-input-method
	;; This command is autoloaded.
	(define-key skk-j-mode-map "\C-q" 'skk-toggle-katakana))
      (unless skk-use-viper
	(define-key skk-j-mode-map
	  (char-to-string skk-start-henkan-with-completion-char)
	  'skk-start-henkan-with-completion)
	(define-key skk-j-mode-map
	  (char-to-string skk-backward-and-set-henkan-point-char)
	  'skk-backward-and-set-henkan-point))
      (skk-setup-delete-backward-char)
      (skk-setup-undo)))
   ;;
   (skk-latin-mode
    (skk-define-latin-mode-map)
    (unless (eq (lookup-key skk-latin-mode-map skk-kakutei-key)
		'skk-kakutei)
      (define-key skk-latin-mode-map skk-kakutei-key 'skk-kakutei)))
   ;;
   (skk-jisx0208-latin-mode
    (skk-define-jisx0208-latin-mode-map)
    (unless (eq (lookup-key skk-jisx0208-latin-mode-map skk-kakutei-key)
		'skk-kakutei)
      (define-key skk-jisx0208-latin-mode-map skk-kakutei-key 'skk-kakutei)
      (unless skk-use-viper
	(define-key skk-jisx0208-latin-mode-map
	  (char-to-string skk-backward-and-set-henkan-point-char)
	  'skk-backward-and-set-henkan-point))))
   ;;
   (skk-abbrev-mode-map
    (skk-define-abbrev-mode-map)
    (unless (eq (lookup-key skk-abbrev-mode-map skk-kakutei-key)
		'skk-kakutei)
      (define-key skk-abbrev-mode-map skk-kakutei-key 'skk-kakutei)
      (define-key skk-abbrev-mode-map (char-to-string skk-start-henkan-char)
	'skk-start-henkan)
      (define-key skk-abbrev-mode-map (char-to-string skk-try-completion-char)
	'skk-try-completion)
      (unless skk-use-viper
	(define-key skk-abbrev-mode-map
	  (char-to-string skk-start-henkan-with-completion-char)
	  'skk-start-henkan-with-completion)))))
  ;;
  (unless (eq (lookup-key minibuffer-local-map skk-kakutei-key)
	      'skk-kakutei)
    (define-key minibuffer-local-map skk-kakutei-key 'skk-kakutei)
    (define-key minibuffer-local-completion-map skk-kakutei-key 'skk-kakutei)
    ;; XEmacs doesn't have minibuffer-local-ns-map
    (when (and (boundp 'minibuffer-local-ns-map)
	       (keymapp (symbol-value 'minibuffer-local-ns-map)))
      (define-key minibuffer-local-ns-map skk-kakutei-key 'skk-kakutei)))
  ;;
  (unless skk-rule-tree
    (setq skk-rule-tree (skk-compile-rule-list
			 skk-rom-kana-base-rule-list
			 skk-rom-kana-rule-list))))

(defun skk-define-menu (map)
  (easy-menu-define skk-menu
		    map
		    "Menu used in SKK mode."
		    skk-menu-items))

(defun skk-define-j-mode-map ()
  (unless (keymapp skk-j-mode-map)
    (setq skk-j-mode-map (make-sparse-keymap))
    (set-modified-alist
     'minor-mode-map-alist
     (list (cons 'skk-j-mode skk-j-mode-map)
	   (cons 'skk-jisx0201-mode skk-j-mode-map))))
  (unless (eq (lookup-key skk-j-mode-map "a")
	      'skk-insert)
    (let ((i 32))
      (while (< i 127)
	(define-key skk-j-mode-map (char-to-string i) 'skk-insert)
	(setq i (1+ i))))
    (skk-define-menu skk-j-mode-map)))

(defun skk-define-latin-mode-map ()
  (unless (keymapp skk-latin-mode-map)
    (setq skk-latin-mode-map (make-sparse-keymap))
    (set-modified-alist
     'minor-mode-map-alist
     (list (cons 'skk-latin-mode skk-latin-mode-map)))
    (skk-define-menu skk-latin-mode-map)))

(defun skk-define-jisx0208-latin-mode-map ()
  (unless (keymapp skk-jisx0208-latin-mode-map)
    (setq skk-jisx0208-latin-mode-map (make-sparse-keymap))
    (set-modified-alist
     'minor-mode-map-alist
     (list (cons 'skk-jisx0208-latin-mode skk-jisx0208-latin-mode-map))))
  (unless (eq (lookup-key skk-jisx0208-latin-mode-map "a")
	      'skk-jisx0208-latin-insert)
    (let ((i 0))
      (while (< i 128)
	(when (aref skk-jisx0208-latin-vector i)
	  (define-key skk-jisx0208-latin-mode-map (char-to-string i)
	    'skk-jisx0208-latin-insert))
	(setq i (1+ i))))
    (define-key skk-jisx0208-latin-mode-map "\C-q" 'skk-latin-henkan)
    (skk-define-menu skk-jisx0208-latin-mode-map)))

(defun skk-define-abbrev-mode-map ()
  (unless (keymapp skk-abbrev-mode-map)
    (setq skk-abbrev-mode-map (make-sparse-keymap))
    (set-modified-alist
     'minor-mode-map-alist
     (list (cons 'skk-abbrev-mode skk-abbrev-mode-map)))
    (define-key skk-abbrev-mode-map "," 'skk-abbrev-comma)
    (define-key skk-abbrev-mode-map "." 'skk-abbrev-period)
    (define-key skk-abbrev-mode-map "\C-q" 'skk-jisx0208-latin-henkan)
    (skk-define-menu skk-abbrev-mode-map)))

(skk-define-abbrev-mode-map)
(skk-define-latin-mode-map)
(skk-define-jisx0208-latin-mode-map)
(skk-define-j-mode-map)

(defun skk-make-indicator-alist ()
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (skk-xemacs-prepare-modeline-properties))
   ((eq skk-emacs-type 'mule5)
    (skk-e21-prepare-modeline-properties)))
  ;;
  (let ((mode-string-list '(skk-latin-mode-string
			    skk-hiragana-mode-string
			    skk-katakana-mode-string
			    skk-jisx0208-latin-mode-string
			    skk-abbrev-mode-string
			    skk-jisx0201-mode-string))
	mode string base)
    (save-match-data
      (cons
       (cons 'default
	     (cons "" (skk-mode-string-to-indicator 'default "")))
       (mapcar (lambda (symbol)
		 (setq mode (prin1-to-string symbol))
		 (string-match "skk-\\([-a-z0-9]+\\)-mode-string" mode)
		 (setq mode (intern (match-string-no-properties 1 mode)))
		 (setq string (symbol-value symbol))
		 ;; 本来ならこのようにユーザ変数を加工するのはおかしいが、
		 ;; 移行期の処置として暫定的に行なう。
		 (cond
		  ((string-match "^ +" string)
		   ;; minor-mode setting
		   (setq base (substring string (match-end 0))))
		  ((string-match "^--" string)
		   ;; mode-line left setting
		   (setq base (substring string (match-end 0)))
		   (when (string-match "::*$" base)
		     (setq base (substring base 0 (match-beginning 0)))))
		  (t
		   (setq base string)))
		 (cons mode
		       (cons (concat " " base)
			     (skk-make-indicator-alist-1 mode base))))
	       mode-string-list)))))

(defun skk-make-indicator-alist-1 (mode base)
  (let ((string
	 (concat "--" base
		 (cond
		  ((skk-face-proportional-p 'modeline)
		   ":")
		  ((memq mode '(latin abbrev))
		   "::")
		  (t
		   ":")))))
    (skk-mode-string-to-indicator mode string)))

(defun skk-setup-modeline ()
  "モード行へのステータス表示を準備する。"
  (setq skk-indicator-alist (skk-make-indicator-alist))
  (cond
   ((not (eq skk-status-indicator 'left))
    (when (and (listp mode-line-format)
	       (equal (car mode-line-format)
		      "")
	       (eq 'skk-modeline-input-mode
		   (nth 1 mode-line-format)))
      ;; for skk-restart.
      (setq-default mode-line-format
		    (nthcdr 2 mode-line-format)))

    (skk-loop-for-buffers (buffer-list)
      (when (and (listp mode-line-format)
		 (equal (car mode-line-format)
			"")
		 (eq 'skk-modeline-input-mode
		     (nth 1 mode-line-format)))
	;; for skk-restart.
	(setq mode-line-format (nthcdr 2 mode-line-format))))

    (setq-default skk-modeline-input-mode "")

    (static-if
	(memq skk-emacs-type '(xemacs mule5))
	(add-minor-mode 'skk-mode 'skk-modeline-input-mode)
      (setq minor-mode-alist
	    ;; each element of minor-mode-alist is not cons cell.
	    (put-alist 'skk-mode
		       '(skk-modeline-input-mode)
		       minor-mode-alist))))
   (t
    (unless (memq 'skk-modeline-input-mode
		  (default-value 'mode-line-format))
      (setq-default mode-line-format
		    (append '("" skk-modeline-input-mode)
			    (default-value 'mode-line-format))))
    (skk-loop-for-buffers (buffer-list)
      (when (and (listp mode-line-format)
		 (skk-local-variable-p 'mode-line-format)
		 (null (memq 'skk-modeline-input-mode
			     mode-line-format)))
	(setq mode-line-format
	      (append '("" skk-modeline-input-mode)
		      mode-line-format))))
    (force-mode-line-update t))))

(defun skk-setup-emulation-commands (commands emulation)
  (let ((map (if (and (boundp 'overriding-local-map)
		      (keymapp 'overriding-local-map))
		 overriding-local-map
	       (current-global-map))))
    (dolist (command commands)
      (dolist (key (where-is-internal command map))
	(define-key skk-abbrev-mode-map key emulation)
	(define-key skk-j-mode-map key emulation)))))

(defun skk-setup-delete-backward-char ()
  (skk-setup-emulation-commands
   '(backward-delete-char-untabify
     backward-delete-char
     backward-or-forward-delete-char
     delete-backward-char
     picture-backward-clear-column
     ;; following two are SKK adviced.
     ;;viper-del-backward-char-in-insert
     ;;vip-del-backward-char-in-insert
     )
   'skk-delete-backward-char))

(defun skk-setup-undo ()
  (skk-setup-emulation-commands
  '(undo
    advertised-undo)
  'skk-undo))

(defun skk-setup-init-file ()
  ;; skk-byte-compile-init-file が non-nil の場合で、skk-init-file をバイトコ
  ;; ンパイルしたファイルが存在しないか、そのバイトコンパイル済ファイルより
  ;; skk-init-file の方が新しいときは、skk-init-file をバイトコンパイルする。
  ;;
  ;; skk-byte-compile-init-file が nil の場合で、skk-init-file をバイトコンパ
  ;; イルしたファイルより skk-init-file の方が新しいときは、そのバイトコンパイ
  ;; ル済ファイルを消す。
  (save-match-data
    (let* ((init-file (expand-file-name skk-init-file))
	   (elc (concat init-file
			(if (string-match "\\.el$" init-file)
			    "c"
			  ".elc"))))
      (if skk-byte-compile-init-file
	  (when (and (file-exists-p init-file)
		     (or (not (file-exists-p elc))
			 (file-newer-than-file-p init-file elc)))
	    (save-window-excursion ; for keep window configuration.
	      (skk-message "%s をバイトコンパイルします"
			   "Byte-compile %s"
			   skk-init-file)
	      (sit-for 2)
	      (byte-compile-file init-file)))
	(when (and (file-exists-p init-file)
		   (file-exists-p elc)
		   (file-newer-than-file-p init-file elc))
	  (delete-file elc))))))

(defun skk-setup-delete-selection-mode ()
  ;; Delete Selection モードが SKK を使った日本語入力に対しても機能するように
  ;; セットアップする。
  (let ((feature (static-cond
		  ((eq skk-emacs-type 'xemacs)
		   'pending-del)
		  (t
		   'delsel)))
	(property (static-cond
		  ((eq skk-emacs-type 'xemacs)
		   'pending-delete)
		  (t
		   'delete-selection)))
	(funcs '(skk-current-kuten
		 skk-current-touten
		 skk-input-by-code-or-menu
		 skk-insert
		 skk-today)))
    (when (and (featurep feature)
	       (not (get 'skk-insert property)))
      (dolist (func funcs)
	(put func property t)))))

(defun skk-setup-auto-paren ()
  (when (and skk-auto-insert-paren
	     skk-auto-paren-string-alist)
    ;;
    (let ((strlst (mapcar 'char-to-string
			  skk-special-midashi-char-list))
	  rulealst str alist)
      (while strlst
	;; skk-auto-paren-string-alist の中から、
	;; skk-special-midashi-char-list の要素に
	;; 関連するものを取り除く。
	(remove-alist 'skk-auto-paren-string-alist (car strlst))
	(setq strlst (cdr strlst)))
      (when (memq t (mapcar
		     (function
		      (lambda (e)
			(skk-ascii-char-p (string-to-char (car e)))))
		     skk-auto-paren-string-alist))
	;;
	(setq alist skk-auto-paren-string-alist
	      rulealst (nconc (mapcar (function
				       (lambda (e)
					 (nth 2 e)))
				      skk-rom-kana-rule-list)
			      (mapcar (function
				       (lambda (e)
					 (nth 2 e)))
				      skk-rom-kana-base-rule-list)))
	(dolist (cell alist)
	  (setq str (car cell))
	  (when (and (skk-ascii-char-p (string-to-char str))
		     ;; 出力文字が入っているセルを調べて、いずれかの
		     ;; キーにバインドされていないかどうかを確認する。
		     (null (assoc str rulealst))
		     (null (rassoc str rulealst))
		     ;; 割り付けようとしているキーが、何か他の出力文字に
		     ;; バインドされていないかどうかを確認する。
		     (null (assoc str skk-rom-kana-base-rule-list))
		     (null (assoc str skk-rom-kana-rule-list)))
	    ;; skk-auto-paren-string-alist の各要素の car の文字が
	    ;; ascii char である場合は、skk-rom-kana-rule-list,
	    ;; skk-rom-kana-base-rule-list にその文字を書き込む (本
	    ;; 来は ascii char は skk-rom-kana-rule-list,
	    ;; skk-rom-kana-base-rule-list に書く必要がない ---
	    ;; skk-emulate-original-mapによる入力が行なわれる ---
	    ;; が、skk-auto-paren-string-alist に指定された対になる
	    ;; 文字の挿入のためには、キーとなる文字を書いておく必要が
	    ;; ある)。
	    (setq skk-rom-kana-rule-list
		  (cons (list str nil str)
			skk-rom-kana-rule-list))))))))

(defun skk-setup-minibuffer ()
  ;; カレントバッファの入力モードに従いミニバッファの入力モードを設定する。
  (cond ((eq skk-minibuffer-origin-mode 'hiragana)
	 (skk-j-mode-on))
	((eq skk-minibuffer-origin-mode 'katakana)
	 (skk-j-mode-on t))
	((eq skk-minibuffer-origin-mode 'abbrev)
	 (skk-abbrev-mode-on))
	((eq skk-minibuffer-origin-mode 'latin)
	 (skk-latin-mode-on))
	((eq skk-minibuffer-origin-mode 'jisx0208-latin)
	 (skk-jisx0208-latin-mode-on))))

(defun skk-setup-jisyo-buffer ()
  ;; skk-jisyo の辞書バッファで、
  ;; (1)空バッファであれば、新しくヘッダーを作り、
  ;; (2)辞書エントリがある既存の辞書バッファならば、ヘッダーが正しいかどうかを
  ;;    チェックする。
  ;;
  ;; skk-okuri-ari-min と skk-okuri-nasi-min の位置を変更した。
  ;;                       ↓ 新しい skk-okuri-ari-min
  ;;   ;; okuri-ari entries.
  ;;   ← 以前の skk-okuri-ari-min
  ;;
  ;;   ↓ skk-okuri-ari-max ↓ 新しい skk-okuri-nasi-min
  ;;   ;; okuri-nasi entries.
  ;;   ← 以前の skk-okuri-nasi-min
  ;;
  ;;
  ;; 変更前の位置であれば、下記のような空辞書の場合、
  ;;
  ;;   ;; okuri-ari entries.
  ;;   ;; okuri-nasi entries.
  ;;
  ;; skk-okuri-ari-min と skk-okuri-ari-max のマーカーが重なってしまい、
  ;; skk-okuri-ari-min の位置に挿入した候補が skk-okuri-ari-max のマーカー
  ;; を後方に押しやらない。
  ;;
  ;; この関数のオリジナルの名称は、j-check-jisyo だったが、skk-check-jisyo と
  ;; いう名前にすると skk-tools.el 内の関数名と重複する。
  ;; case-fold-search は、辞書バッファでは常に nil。
  (save-match-data
    (when (= (buffer-size) 0)
      ;; 空バッファだったら、ヘッダーのみ挿入。
      (insert ";; okuri-ari entries.\n" ";; okuri-nasi entries.\n"))
    (goto-char (point-min))
    (unless (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
      (skk-error "送りありエントリのヘッダーがありません！"
		 "Header line for okuri-ari entries is missing!"))
    ;; 固定ポイントなので、(point) で十分。
    (setq skk-okuri-ari-min (point))
    (unless (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
      (skk-error "送りなしエントリのヘッダーがありません！"
		 "Header line for okuri-nasi entries is missing!"))
    (beginning-of-line)
    ;; 共有辞書なら固定ポイントでも良いのだが、辞書バッファで編集を行
    ;; なったときのことを配慮してマーカーにしておく。
    (setq skk-okuri-ari-max (point-marker))
    (forward-line 1)
    (backward-char 1)
    (setq skk-okuri-nasi-min (point-marker))))

(defun skk-emulate-original-map (arg)
  ;; キー入力に対して、SKK のモードではなく、Emacs のオリジナルのキー割り付けで
  ;; コマンドを実行する。
  (let ((prefix-arg arg)
	(keys (skk-command-key-sequence (this-command-keys) this-command)))
    (when keys ; If key is nil, the command may have been invoked by M-x.
      (let (skk-mode
	    skk-latin-mode
	    skk-j-mode
	    skk-abbrev-mode
	    skk-jisx0208-latin-mode
	    skk-jisx0201-mode
	    command)
	;; have to search key binding after binding 4 minor mode flags to nil.
	(setq command (key-binding keys))
	(unless (eq command this-command)
	  ;; avoid recursive calling of skk-emulate-original-map.

	  ;; if no bindings are found, call `undefined'.  it's
	  ;; original behaviour.
	  ;;(skk-cancel-undo-boundary)
	  (command-execute (or command
			       (function undefined))))))))

(defun skk-command-key-sequence (key command)
  ;; KEY から universal arguments を取り除き、COMMAND を実行するキーを返す。
  ;; `execute-extended-command' によってコマンドが実行された場合は、nil を
  ;; 返す。
  (while (not (or (zerop (length key))
		  (eq command (key-binding key))))
    (setq key (vconcat (cdr (append key nil)))))
  (unless (zerop (length key))
    key))

(defun skk-adjust-user-option ()
  ;; 両立できないオプションの調整を行なう。
  (when skk-process-okuri-early
    ;; skk-process-okuri-early の値が non-nil であるときに下記の値が non-nil
    ;; であれば正常に動かないのでこの変数の優先順位を高くした。
    (setq skk-kakutei-early nil
	  skk-auto-okuri-process nil
	  skk-henkan-okuri-strictly nil
	  skk-henkan-strict-okuri-precedence nil)))

(defun skk-try-completion (arg)
  "▽モードで見出し語の補完を行う。
それ以外のモードでは、オリジナルのキー割り付けのコマンドをエミュレートする。"
  (interactive "P")
  (skk-with-point-move
   (if (eq skk-henkan-mode 'on)
       (skk-comp (not (eq last-command 'skk-comp-do)))
     (skk-emulate-original-map arg))))

(defun skk-latin-mode (arg)
  "SKK のモードを latin (ascii) モードに変更する。"
  (interactive "P")
  (skk-kakutei)
  (skk-latin-mode-on)
  nil)

(defun skk-jisx0208-latin-mode (arg)
  "SKK のモードを全角英字入力モードに変更する。"
  (interactive "P")
  (skk-kakutei)
  (skk-jisx0208-latin-mode-on)
  nil)

(defun skk-abbrev-mode (arg)
  "ascii 文字をキーにした変換を行うための入力モード。"
  (interactive "*P")
  (cond ((eq skk-henkan-mode 'active)
	 (skk-kakutei))
	((eq skk-henkan-mode 'on)
	 (skk-error "既に▽モードに入っています" "Already in ▽ mode")))
  (let (skk-dcomp-activate)
    (skk-set-henkan-point-subr))
  (skk-abbrev-mode-on)
  nil)

(defun skk-toggle-characters (arg)
  "■モード、▼モードで、ひらがなモードとカタカナモードをトグルで切り替える。
▽モードでは skk-henkan-start-point (▽の直後) とカーソルの間の文字列につい
て、ひらがなとカタカナを入れ替える。"
  (interactive "P")
  (cond
   ((eq skk-henkan-mode 'on)
    (let (char)
      (skk-set-marker skk-henkan-end-point (point))
      (skk-save-point
       (goto-char skk-henkan-start-point)
       (while (and
	       ;;(not (eobp))
	       (>= skk-henkan-end-point (point))
	       (or
		;; "ー" では文字種別が判別できないので、ポイントを進める。
		(looking-at "ー")
		(eq 'unknown (setq char (skk-what-char-type)))))
	 (forward-char 1)))
      (cond ((eq char 'hiragana)
	     (skk-katakana-region
	      skk-henkan-start-point skk-henkan-end-point
	      'vcontract))
	    ((eq char 'katakana)
	     (skk-hiragana-region
	      skk-henkan-start-point skk-henkan-end-point))
	    ((eq char 'jisx0208-latin)
	     (skk-latin-region
	      skk-henkan-start-point skk-henkan-end-point))
	    ((eq char 'ascii)
	     (skk-jisx0208-latin-region
	      skk-henkan-start-point skk-henkan-end-point))
	    )))
   ((and (skk-in-minibuffer-p)
	 (not skk-j-mode))
    ;; ミニバッファへの初突入時。
    (skk-j-mode-on))
   (t
    (setq skk-katakana (not skk-katakana))))
  (skk-kakutei)
  (when skk-j-mode
    (skk-j-mode-on skk-katakana))
  nil)

(defun skk-misc-for-picture ()
  ;; picture-mode へ入ったときに SKK 起動前の状態に戻す。
  ;; edit-picture-hook に add-hook して使用する。
  ;;
  ;; 既存のバッファを picture mode にしたとき、picture-mode 関数は
  ;; kill-all-local-variables 関数を呼ばないので、SKK 関連のバッファローカル
  ;; 変数が元のバッファの値のままになってしまう。そこで、picture mode に入った
  ;; ときにフックを利用してこれらのバッファローカル変数を kill する。
  ;; RMS は picture-mode で kill-all-local-variables 関数を呼ばないのは、バグ
  ;; ではない、と言っていた。
  ;;
  ;; picture-mode で SKK を使用し漢字入力をした場合に、BS で全角文字が消せない
  ;; のは、SKK の不具合ではなく、picture.el の問題 (move-to-column-force 関数
  ;; の中で使用している move-to-column で全角文字を無視したカラム数が与えられ
  ;; たときにカーソル移動ができないから) である。消したい文字にポイントを合わ
  ;; せ、C-c C-d で一文字づつ消すしか方法はない。
  (when skk-mode
    (skk-kill-local-variables)))

(defun skk-kill-local-variables ()
  ;; SKK 関連のバッファローカル変数を無効にする。
  (skk-mode -1)
  (let ((lv (buffer-local-variables))
	v vstr)
    (while lv
      (setq v (car (car lv))
	    lv (cdr lv)
	    vstr (prin1-to-string v))
      (when (and (> (length vstr) 3)
		 (string= "skk-" (substring vstr 0 4)))
	(kill-local-variable v)))))

;;;; kana inputting functions
(defun skk-insert (&optional arg)
  "SKK の文字入力を行なう。"
  (interactive "*p")
  (skk-with-point-move
   (let ((ch last-command-char))
     (cond ((and skk-henkan-mode
		 (memq ch skk-special-midashi-char-list))
	    ;; 接頭辞・接尾辞の処理。
	    (skk-process-prefix-or-suffix arg))
	   (;; start writing a midasi key.
	    (and (memq ch skk-set-henkan-point-key)
		 (or skk-okurigana
		     (not (skk-get-prefix skk-current-rule-tree))
		     (not (skk-select-branch skk-current-rule-tree ch))))
	    ;; normal pattern
	    ;; skk-set-henkan-point -> skk-kana-input.
	    (skk-set-henkan-point arg))
	   ;; start conversion.
	   ((and skk-henkan-mode
		 (eq ch skk-start-henkan-char))
	    (skk-start-henkan arg))
	   ;; just input kana.
	   ((not (eq skk-henkan-mode 'on))
	    (skk-kana-input arg))
	   ;; for completion.
	   ;; コンプリーション関連の関数は skk-rom-kana-base-rule-list の中に押
	   ;; し込め、skk-kana-input の中から制御すべき。
	   ;; 但し、TAB は self-insert-command ではないので、skk-j-mode-map の
	   ;; キーマップで substitute-key-definition しても skk-insert にバイン
	   ;; ドできない。skk-j-mode-map で 直接 "\t" を skk-insert にバインド
	   ;; して、completion と skk-current-kuten/skk-current-touten をコント
	   ;; ロールするコマンド名を skk-rom-kana-base-rule-list に書けば良いか
	   ;; も。
	   ;; でも、skk-comp と skk-current-kuten/skk-current-touten のコントロ
	   ;; ールがハードコーディングされるのはまずいかも (skk-comp は使っても
	   ;; skk-current-kuten/skk-current-touten は使わない、という人がいるか
	   ;; も)。
	   ((and (eq skk-henkan-mode 'on)
		 (eq ch skk-try-completion-char))
	    (skk-comp (not (eq last-command 'skk-comp-do))))
	   ((and (eq skk-henkan-mode 'on)
		 (memq ch (list skk-next-completion-char
				skk-previous-completion-char))
		 (eq last-command 'skk-comp-do))
	    (skk-comp-previous/next ch))
	   (t
	   ;; just input Kana.
	    (skk-kana-input arg))))))

(defun skk-process-prefix-or-suffix (&optional arg)
  "接頭辞または接尾辞の入力を開始する。
これは、普通 `skk-special-midashi-char-list' に指定された文字の入力があった場
合に非対話的に呼び出されるが、対話的に呼出すことも可能である。"
  ;; SKK 10 までは、> < ? の 3 つについて扱いが平等でなかった。Daredevil SKK
  ;; 11 以降では、辞書における表現を > で統一することにより、3 者の扱いを平等
  ;; にし、なおかつ、このコマンドが文字キーでない入力により呼ばれたときにも接
  ;; 尾辞・ 接頭辞入力ができるようにする。
  (interactive "*p")
  (cond ((eq skk-henkan-mode 'active)
	 ;; 接尾辞のための処理。
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
			       skk-henkan-start-point (point))
	       skk-prefix "")
	 (skk-henkan))
	(last-command-char
	 ;; `skk-insert' から呼ばれる場合には、このケースはない。
	 (let ((i (prefix-numeric-value arg))
	       (str (skk-char-to-string last-command-char)))
	   (while (> i 0)
	     (skk-insert-str str)
	     (setq i (1- i)))))
	(t
	 ;; どうするべきかまだ決まっていない。
	 ;; (skk-emulate-original-map arg)
	 )))

(defun skk-kana-input (&optional arg)
  ;;"かな文字の入力を行うルーチン。"
  ;; Message-Id: <19980610190611B.sakurada@kuis.kyoto-u.ac.jp>
  ;; From: Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
  ;; Date: Wed, 10 Jun 1998 19:06:11 +0900 (JST)
  ;;
  ;; 新しい skk-kana-input は, 簡単に説明すると,
  ;; あらかじめルールを木の形に表現しておいて, 入力を見
  ;; て木を辿り, それ以上辿れなくなったらその節に登録さ
  ;; れている仮名を入力する. というしくみです.
  ;;
  ;; 例えば, n a t のみからなる以下のルール
  ;;
  ;;     a  → あ
  ;;     n  → ん
  ;;     nn → ん
  ;;     na → な
  ;;     ta → た
  ;;     tt → っ (次状態 t)
  ;;
  ;; をルール木に変換すると,
  ;;
  ;;                 ／/＼
  ;;               ／ /   ＼
  ;;           a ／  / t    ＼ n
  ;;           ／   /         ＼
  ;;          あ   ・           ん
  ;;             ／|           / ＼
  ;;         a ／  | t      a /    ＼ n
  ;;         ／    |         /       ＼
  ;;       た     っ        な        ん
  ;;          (次状態 "t")
  ;;
  ;; という形になります.
  ;;
  ;; 初期状態(木の根)で `a' を入力すると, 木の根から
  ;; 「あ」に移動します. 次にどのような入力が来ても,
  ;; それより下にたどれないので, 「あ」を出力して根に戻ります.
  ;; ルールに次状態が設定されている場合は, 設定されてい
  ;; る文字列をキューに戻してから根に戻ります.
  ;;
  ;; 初期状態で `n' を入力すると, 「ん」に移動します.
  ;; 次に `a' または `n' が入力されればそれより下にたどれる
  ;; ので次の入力を見るまでまだ出力しません.
  ;; 次に `t' が入力された場合は, `t' では下にたどれないので,
  ;; 「ん」を出力して `t' をキューに戻します.
  ;;
  ;; ここで, 初期状態, 現状態をそれぞれ skk-rule-tree,
  ;; skk-current-rule-tree で表し.
  ;; 木を下にたどる, という操作は, skk-select-branch を
  ;; 用いて,
  ;;
  ;;   (skk-select-branch rule-tree ?a)
  ;;
  ;; のようにします. 現状態に設定されているかな(("ア". "あ")など),
  ;; 次状態("t" など)は, それぞれ skk-get-kana,
  ;; skk-get-nextstate で取ります.
  ;; don't echo key strokes in the minibuffer.
  (let ((echo-keystrokes 0)
	(queue (list last-command-char)))
    (while queue
      (if (not (skk-get-prefix skk-current-rule-tree))
	  (progn
	    (skk-set-marker skk-kana-start-point (point))
	    (setq skk-current-rule-tree skk-rule-tree))
	(skk-erase-prefix))
      (setq skk-prefix (concat (skk-get-prefix skk-current-rule-tree)
			       (char-to-string last-command-char)))
      (let ((next (skk-select-branch
		   skk-current-rule-tree
		   (car queue)))
	    data)
	(cond
	 (next
	  ;; can go down SKK-CURRENT-RULE-TREE
	  (cond
	   ((skk-get-branch-list next)
	    ;; NEXT have at least one branch
	    (when (and (eq skk-henkan-mode 'active)
		       skk-kakutei-early
		       (not skk-process-okuri-early))
	      (skk-kakutei)
	      (skk-set-marker skk-kana-start-point (point)))
	    (setq queue (cdr queue)
		  skk-current-rule-tree next))
	   (t
	    ;; NEXT does not have any branch (i.e. NEXT is a leaf)
	    (setq data (skk-get-kana next)
		  queue (nconc (string-to-char-list
				(skk-get-nextstate next))
			       (cdr queue))
		  skk-current-rule-tree nil))))
	 (t
	  ;; can not go down SKK-CURRENT-RULE-TREE
	  (let ((d (skk-get-kana skk-current-rule-tree)))
	    (cond
	     (d
	      ;; SKK-CURRENT-RULE-TREE have a roma->kana rule
	      (setq data d
		    queue (nconc (string-to-char-list
				  (skk-get-nextstate
				   skk-current-rule-tree))
				 queue)
		    skk-current-rule-tree nil))
	     (t
	      ;; SKK-CURRENT-RULE-TREE does not have any roma->kana rule
	      (let ((dd (when skk-kana-input-search-function
			  (funcall skk-kana-input-search-function))))
		(cond
		 (dd
		  (setq data (car dd)
			queue (nconc (string-to-char-list (cdr dd))
				     (cdr queue))
			skk-current-rule-tree nil))
		 ((eq skk-current-rule-tree skk-rule-tree)
		  ;; typo on the root of tree
		  (setq queue nil
			skk-current-rule-tree nil))
		 (t
		  ;; otherwise move to root of the tree, and redo
		  (setq skk-current-rule-tree nil)))))))))
	(cond
	 ((not data)
	  (if skk-current-rule-tree
	      (progn
		;;(digit-argument arg)
		;; う〜ん、よう分からん。とりあえず。
		(unless skk-isearch-message
		  (setq prefix-arg arg))
		(setq skk-prefix (skk-get-prefix skk-current-rule-tree))
		(skk-insert-prefix skk-prefix))
	    ;;(skk-kana-cleanup 'force)
	    (when (eq skk-henkan-mode 'active)
	      (skk-kakutei))
	    (setq skk-prefix "")
	    (unless (or queue
			(and (not (eq this-command 'skk-insert))
			     skk-henkan-mode))
	      (skk-emulate-original-map (skk-make-raw-arg arg)))))
	 (t
	  ;;(skk-cancel-undo-boundary)
	  (setq skk-prefix "")
	  (when (functionp data)
	    (setq data (funcall data (skk-make-raw-arg arg))))
	  (when (stringp (if (consp data)
			     (car data)
			   data))
	    (let* ((str (if (consp data)
			    (if skk-katakana
				(car data)
			      (cdr data))
			  data))
		   (pair (when skk-auto-insert-paren
			   (cdr (assoc
				 str
				 skk-auto-paren-string-alist))))
		   (count0 arg)
		   (count1 arg)
		   (inserted 0))
	      (when (and (eq skk-henkan-mode 'active)
			 skk-kakutei-early
			 (not skk-process-okuri-early))
		(skk-kakutei))
	      ;; arg は保存しておかないと、0 になってしまい、queue
	      ;; がたまっていて再度ここへやって来たときに文字入力が
	      ;; できなくなる。
	      (skk-cancel-undo-boundary)
	      (while (> count0 0)
		(skk-insert-str str)
		(setq count0 (1- count0)))
	      (when pair
		(while (> count1 0)
		  (if (not (string= pair (char-to-string (following-char))))
		      (progn
			(setq inserted (1+ inserted))
			(skk-insert-str pair)))
		  (setq count1 (1- count1)))
		(unless (= inserted 0)
		  (backward-char inserted)))
	      (when (and skk-okurigana
			 (null queue))
		(skk-set-okurigana)))))))
      ;; XXX I don't know how skk-isearch-message works....
      (when skk-isearch-message
	(skk-isearch-message)))))

;;; tree procedure (ツリーにアクセスするためのインターフェース)
(defun skk-search-tree (tree char-list)
  ;; TREE の根から先端へ CHAR-LIST に従ってたどる。
  ;; 成功した場合は nil と 結果の木の組を返し,
  ;; 失敗した場合はたどれなかった CHAR-LIST の残りとたどれなくなった
  ;; 節点の木の組を返す。
  (catch 'return
    (let (next char rest)
      (while char-list
	(setq char (car char-list)
	      rest (cdr char-list)
	      next (skk-select-branch tree char))
	(if next
	    (setq tree next
		  char-list rest)
	  (throw 'return (cons char-list tree))))
      (cons nil tree))))

(defun skk-add-rule (tree rule)
  (let* ((prefix (nth 0 rule))
	 (l (length prefix))
	 (result (skk-search-tree tree (string-to-char-list prefix)))
	 (rest (car result))
	 (addpoint (cdr result)))
    (while rest
      (let ((addtree
	     (skk-make-rule-tree (car rest)
				 (substring prefix 0 (1+ (- l (length rest))))
				 nil nil nil)))
	(skk-add-branch addpoint addtree)
	(setq addpoint addtree
	      rest (cdr rest))))
    (skk-set-nextstate addpoint (nth 1 rule))
    (skk-set-kana addpoint (nth 2 rule))))

(defun skk-delete-rule (tree string)
  ;; 入力 STRING に対するルールをルール木 TREE から削除
  (catch 'return
    (let ((char-list (string-to-char-list string))
	  (cutpoint tree)
	  (cuttree (car (skk-get-branch-list tree)))
					; TREE の根から出る枝が1本しかない場合
					; のために一応初期化しておく
	  next)
      (while char-list
	(setq next (skk-select-branch tree (car char-list))
	      char-list (cdr char-list))
	(if next
	    (if (> (length (skk-get-branch-list tree)) 1)
		(setq cutpoint tree	; 枝が2本以上の時 cutpoint cuttree
		      cuttree next	; を update
		      tree next)
	      (setq tree next))
	  (throw 'return nil)))
      (skk-set-branch-list cutpoint
			   (delq cuttree (skk-get-branch-list cutpoint))))))

;; convert skk-rom-kana-rule-list to skk-rule-tree.
;; The rule tree follows the following syntax:
;; <branch-list>  := nil | (<tree> . <branch-list>)
;; <tree>         := (<char> <prefix> <nextstate> <kana> <branch-list>)
;; <kana>         := (<ひらがな文字列> . <カタカナ文字列>) | nil
;; <char>         := <英小文字>
;; <nextstate>    := <英小文字文字列> | nil
;;;###autoload
(defun skk-compile-rule-list (&rest l)
  ;; rule-list を木の形にコンパイルする。
  (let ((tree (skk-make-rule-tree nil "" nil nil nil))
	rule key ll)
    (while l
      (setq ll (car l)
	    l (cdr l))
      (while ll
	(setq rule (car ll)
	      key (car rule)
	      ll (cdr ll))
	(ignore-errors
	  (when (symbolp key)
	    (setq key (eval key))
	    (setcar rule key))
	  (unless (or (string-match "\\w" key)
		      (eq (key-binding key)
			  'self-insert-command))
	    (define-key skk-j-mode-map key 'skk-insert)))
	(skk-add-rule tree rule)))
    tree))

(defun skk-insert-str (str)
  ;; STR を挿入する。必要であれば self-insert-after-hook をコ
  ;; ールする。overwrite-mode であれば、適切に上書きを行う。
  (insert-and-inherit str)
  (if (eq skk-henkan-mode 'on)
      ;;
      (when (and skk-auto-start-henkan
		 (not skk-okurigana))
	(skk-auto-start-henkan str))
    ;;
    (when (and (boundp 'self-insert-after-hook)
	       self-insert-after-hook)
      (funcall self-insert-after-hook
	       (- (point) (length str))
	       (point)))
    (when overwrite-mode
      (skk-del-char-with-pad (skk-ovwrt-len (string-width str)))))
  ;; SKK 9.6 ではこのタイミングで fill が行われていたが、SKK 10 では行われてい
  ;; なかった。
  (when (and skk-j-mode
	     (not skk-henkan-mode))
    (skk-do-auto-fill)))

(defun skk-ovwrt-len (len)
  ;; 上書きして良い長さを返す。
  (min (string-width
	(buffer-substring-no-properties
	 (point) (skk-save-point
		  (end-of-line)
		  (point))))
       len))

(defun skk-del-char-with-pad (length)
  ;; 長さ LENGTH の文字を消去する。調整のため、必要であれば、末尾にスペースを
  ;; 挿入する。
  (let ((p (point)) (len 0))
    (while (< len length)
      (forward-char 1)
      (setq len (string-width (buffer-substring-no-properties (point) p))))
    (delete-region p (point))
    (unless (= length len)
      (insert-and-inherit " ")
      (backward-char 1))))

(defun skk-cancel-undo-boundary ()
  ;; skk-insert, skk-jisx0208-latin-insert で連続して入力さ
  ;; れた 20 文字を 1 回のアンドゥの対象とする。`20' は
  ;; keyboard.c に定められたマジックナンバー。Mule-2.3 添付
  ;; の egg.el を参考にした。
  (cond
   ((and (< skk-self-insert-non-undo-count 20)
	 (memq last-command
	       '(skk-insert
		 skk-jisx0208-latin-insert
		 ;; SKK abbrev モードでは、アスキー文字入力が Emacs オリジナ
		 ;; ルの self-insert-command により行なわれているので、
		 ;; skk-self-insert-non-undo-count をインクリメントすること
		 ;; ができないので、アンドゥをエミュレートできない。
		 ;; しかも、カンマやピリオドを挿入した時点で、
		 ;; skk-abbrev-comma や skk-abbrev-period を使うことになるの
		 ;; で (self-insert-command 以外のコマンドを使ってしまうので)、
		 ;; オリジナルのアンドゥの機能も損なってしまう。
		 ;; しかし現実問題としては、SKK abbrev モードは省略形としての
		 ;; 見出し語を挿入するためのモードであるので、長い見出し語を
		 ;; 挿入することはあまりなく、問題も小さいと考えられる。
		 ;;skk-abbrev-comma
		 ;;skk-abbrev-period
		 )))
    (cancel-undo-boundary)
    (when (null skk-current-rule-tree)
      ;; まだかな文字が完成していないときは、undo count をインクリメント
      ;; しない。
      (setq skk-self-insert-non-undo-count
	    (1+ skk-self-insert-non-undo-count))))
   (t
    (setq skk-self-insert-non-undo-count 1))))

(defun skk-set-okurigana ()
  ;; 見出し語から skk-henkan-okurigana, skk-henkan-key の各値をセットする。
  (cancel-undo-boundary)
  ;;(and skk-katakana (skk-hiragana-region skk-henkan-start-point (point)))
  (skk-set-marker skk-henkan-end-point skk-okurigana-start-point)
  ;; just in case
  (skk-save-point
    (goto-char skk-okurigana-start-point)
    (unless (eq (following-char) ?*)
      (insert-and-inherit "*")))
  (setq skk-henkan-okurigana (buffer-substring-no-properties
			      (1+ skk-okurigana-start-point)
			      (point)))
  (setq skk-henkan-key (concat (buffer-substring-no-properties
				skk-henkan-start-point
				skk-henkan-end-point)
			       (or (skk-okurigana-prefix skk-henkan-okurigana)
				   skk-okuri-char))
	skk-prefix "")
  (when skk-katakana
    (setq skk-henkan-key (skk-katakana-to-hiragana skk-henkan-key)
	  skk-henkan-okurigana
	  (skk-katakana-to-hiragana skk-henkan-okurigana)))
  (delete-region skk-okurigana-start-point (1+ skk-okurigana-start-point))
  (setq skk-henkan-count 0)
  (skk-henkan)
  (setq skk-okurigana nil))

;;; other inputting functions
(defun skk-toggle-kutouten ()
  "句読点の種類をトグルで変更する。"
  (interactive)
  (setq skk-kutouten-type (if (eq skk-kutouten-type 'jp)
			      'en
			    'jp))
  (when (interactive-p)
    (skk-message "句点: `%s'  読点: `%s'"
		 "Kuten: `%s'  Touten: `%s'"
		 (skk-current-kuten nil)
		 (skk-current-touten nil))))

(defun skk-current-kuten (arg)
  ;; just ignore arg.
  (car (cdr (assq skk-kutouten-type skk-kuten-touten-alist))))

(defun skk-current-touten (arg)
  ;; just ignore arg.
  (cdr (cdr (assq skk-kutouten-type skk-kuten-touten-alist))))

(defun skk-abbrev-period (arg)
  "SKK abbrev モードで見出しの補完中であれば、次の候補を表示する。
補完の直後でなければ、オリジナルのキー割り付けのコマンドをエミュレートする。
SKK abbrev モード以外では、skk-insert-period 関数を使用すること。"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-comp-do)
       (progn
	 (setq this-command 'skk-comp-do)
	 (skk-comp-do nil))
     (skk-emulate-original-map arg))))

(defun skk-abbrev-comma (arg)
  "SKK abbrev モードで見出しの補完中であれば、直前の候補を表示する。
補完の直後でなければ、オリジナルのキー割り付けのコマンドをエミュレートする。
SKK abbrev モード以外では、skk-insert-comma 関数を使用すること。"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-comp-do)
       (progn
	 (setq this-command 'skk-comp-do)
	 (skk-comp-previous))
     (skk-emulate-original-map arg))))

(defun skk-jisx0208-latin-insert (arg)
  "全英文字をカレントバッファに挿入する。
skk-jisx0208-latin-vector をテーブルとして、最後に入力されたキーに対応する文
字を挿入する。
skk-auto-insert-paren の値が non-nil の場合で、skk-auto-paren-string-alist に
対応する文字列があるときは、その対応する文字列 (かっこ類) を自動的に挿入する。"
  (interactive "*p")
  (skk-with-point-move
   (let* ((str (aref skk-jisx0208-latin-vector last-command-char))
	  (arg2 arg)
	  (pair-str
	   (and skk-auto-insert-paren
		(cdr (assoc str skk-auto-paren-string-alist))))
	  (pair-str-inserted 0))
     (if (not str)
	 (skk-emulate-original-map arg)
       (skk-cancel-undo-boundary)
       (while (> arg 0)
	 (skk-insert-str str)
	 (setq arg (1- arg)))
       (when pair-str
	 (while (> arg2 0)
	   (unless (string= pair-str (char-to-string (following-char)))
	     (setq pair-str-inserted (1+ pair-str-inserted))
	     (skk-insert-str pair-str))
	   (setq arg2 (1- arg2)))
	 (unless (= pair-str-inserted 0)
	   (backward-char pair-str-inserted)))))))

(defun skk-delete-backward-char (arg)
  "▼モードで `skk-delete-implies-kakutei' なら直前の文字を消して確定する。
▼モードで `skk-delete-implies-kakutei' が nil だったら前候補を表示する。
▽モードで`▽'よりも前のポイントで実行すると確定する。
確定入力モードで、かなプレフィックスの入力中ならば、かなプレフィックスを消す。"
  (interactive "*P")
  (skk-with-point-move
   (let ((count (prefix-numeric-value arg)))
     (cond
      ((eq skk-henkan-mode 'active)
       (if (and (not skk-delete-implies-kakutei)
		(= skk-henkan-end-point (point)))
	   (skk-previous-candidate)
	 ;; overwrite-mode で、ポイントが全角文字に囲まれていると
	 ;; きに delete-backward-char を使うと、全角文字は消すが半
	 ;; 角文字分しか backward 方向にポイントが戻らない (Emacs
	 ;; 19.31 にて確認)。変換中の候補に対しては
	 ;; delete-backward-char で必ず全角文字 1 文字分 backward
	 ;; 方向に戻った方が良い。
	 (if overwrite-mode
	     (progn
	       (backward-char count)
	       (delete-char count arg))
	   (skk-emulate-original-map arg))
	 ;; XXX assume skk-prefix has no multibyte chars.
	 (if (> (length skk-prefix) count)
	     (setq skk-prefix (substring skk-prefix
					 0 (- (length skk-prefix) count)))
	   (setq skk-prefix ""))
	 (when (>= skk-henkan-end-point (point))
	   (skk-kakutei))))
      ((and skk-henkan-mode
	    (>= skk-henkan-start-point (point)))
       (setq skk-henkan-count 0)
       (skk-kakutei))
      ;; 入力中の見出し語に対しては delete-backward-char で
      ;; 必ず全角文字 1文字分 backward 方向に戻った方が良い。
      ((and skk-henkan-mode
	    overwrite-mode)
       (backward-char count)
       (delete-char count arg))
      (t
       (skk-delete-okuri-mark)
       (if (skk-get-prefix skk-current-rule-tree)
	   (skk-erase-prefix 'clean)
	 (skk-set-marker skk-kana-start-point nil)
	 (skk-emulate-original-map arg)))))))

;;; henkan routines
(defun skk-henkan ()
  ;; カナを漢字変換するメインルーチン。
  (let (mark
	prototype
	new-word
	kakutei-henkan)
    (if (string= skk-henkan-key "")
	(skk-kakutei)
      ;; we use mark to go back to the correct position after henkan
      (unless (eobp)
	(setq mark (skk-save-point
		    (forward-char 1)
		    (point-marker))))
      (unless (eq skk-henkan-mode 'active)
	(skk-change-marker)
	(setq skk-current-search-prog-list skk-search-prog-list))
      ;; skk-henkan-1 の中からコールされる skk-henkan-show-candidate から throw
      ;; される。ここでキャッチした場合は、?x がストリームに戻されているので、
      ;; この関数を出て、skk-previous-candidates へゆく。
      (catch 'unread
	(cond
	 ((setq prototype (skk-henkan-1))
	  (setq new-word prototype))
	 ((setq prototype (skk-henkan-in-minibuff))
	  (setq new-word (skk-quote-semicolon prototype))))
	(setq kakutei-henkan skk-kakutei-flag)
	(when new-word
	  (skk-insert-new-word new-word)))
      ;;
      (when (and new-word
		 (string= new-word prototype)
		 (skk-numeric-p))
	(setq new-word (skk-get-current-candidate 'noconv)))
      ;;
      (if mark
	  (progn
	    (goto-char mark)
	    ;; 参照されていないマーカーは、Garbage Collection がコールされたと
	    ;; きに回収されるが、それまでの間、テキストのどこかを指していると、
	    ;; テキストのアップデートの際にそのマーカー値を更新する必要がある
	    ;; ので、どこも指さないようにする。
	    (skk-set-marker mark nil)
	    (backward-char 1))
	(goto-char (point-max)))
      ;;
      (when kakutei-henkan
	(skk-kakutei new-word)))))

(defun skk-henkan-1 ()
  ;; skk-henkan のサブルーチン。
  (let (new-word)
    (cond
     ((= skk-henkan-count 0)
      (when (and (eq last-command 'skk-undo-kakutei-henkan)
		 (eq (car (car skk-current-search-prog-list))
		     'skk-search-kakutei-jisyo-file))
	;; in this case, we should not search kakutei jisyo.
	(setq skk-current-search-prog-list
	      (cdr skk-current-search-prog-list)))
      (while (and skk-current-search-prog-list
		  (not new-word))
	(setq skk-henkan-list (skk-nunion skk-henkan-list
					  (skk-search)))
	(skk-henkan-list-filter)
	(setq new-word (skk-get-current-candidate)))
      (when (and new-word
		 skk-kakutei-flag)
	;; found the unique candidate in kakutei jisyo
	(setq this-command 'skk-kakutei-henkan)))
     (t
      ;; 変換回数が 1 以上のとき。
      (setq new-word (skk-get-current-candidate))
      (unless new-word
	;; 新しい候補を見つけるか、skk-current-search-prog-list が空にな
	;; るまで skk-search を連続してコールする。
	(while (and skk-current-search-prog-list (not new-word))
	  (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search)))
	  (skk-henkan-list-filter)
	  (setq new-word (skk-get-current-candidate))))
      (when (and new-word
		 (> skk-henkan-count 3))
	;; show candidates in minibuffer
	(setq new-word (skk-henkan-show-candidates)))))
    new-word))

(defun skk-get-current-candidate (&optional noconv)
  (cond ((not (skk-numeric-p))
	 (skk-get-current-candidate-1))
	(noconv
	 (car (skk-get-current-candidate-1)))
	(t
	 (cdr (skk-get-current-candidate-1)))))

(defun skk-henkan-list-filter ()
  (when (skk-numeric-p)
    (skk-num-uniq)
    (skk-num-multiple-convert))
  (when (and (featurep 'jisx0213)
	     skk-jisx0213-prohibit)
    (skk-jisx0213-henkan-list-filter)))

(defun skk-henkan-show-candidates ()
  ;; ミニバッファで変換した候補群を表示する。
  (skk-save-point
   (let* ((candidate-keys ; 表示用のキーリスト
	   (mapcar
	    (function
	     (lambda (c)
	       (when (memq c '(?\C-g ?\040 ?x)) ; ?\040 is SPC.
		 (skk-error "`%s' に無効なキーが指定されています"
			    "Illegal key in `%s'"
			    "skk-henkan-show-candidates-keys"))
	       (char-to-string (upcase c))))
	    skk-henkan-show-candidates-keys))
	  key-num-alist	; 候補選択用の連想リスト
	  (key-num-alist1 ; key-num-alist を組み立てるための作業用連想リスト。
	   (let ((count 6))
	     (mapcar
	      (function
	       (lambda (key)
		 (prog1
		     (cons key count)
		   (setq count (1- count)))))
	      ;; 逆さまにしておいて、表示する候補の数が少なかったら先
	      ;; 頭から幾つか削る。
	      (reverse skk-henkan-show-candidates-keys))))
	  (loop 0)
	  inhibit-quit
	  henkan-list
	  new-one
	  reverse
	  n)
     ;; Emacs 19.28 だと Overlay を消しておかないと、次に insert される
     ;; skk-henkan-key に何故か Overlay がかかってしまう。
     (when skk-use-face
       (skk-henkan-face-off))
     (delete-region skk-henkan-start-point
		    skk-henkan-end-point)
     (while loop
       (cond
	(reverse
	 (setq loop (1- loop)
	       henkan-list (nthcdr (+ 4 (* loop 7))
				   skk-henkan-list)
	       reverse nil))
	(skk-exit-show-candidates
	 ;; 候補が尽きてしまって、skk-henkan-show-candidates ->
	 ;; skk-henkan-in-minibuff -> skk-henkan
	 ;; -> skk-henkan-show-candidates の順で、再びこの関数が呼ばれ
	 ;; たときは、ここで henkan-list と loop を計算する。
	 (setq henkan-list (nthcdr skk-henkan-count skk-henkan-list)
	       loop (car skk-exit-show-candidates)
	       skk-exit-show-candidates nil))
	(t
	 ;; skk-henkan-show-candidates-keys の最終のキーに対応する候補
	 ;; が出てくるまでサーチを続ける。
	 (skk-henkan-list-filter)
	 (while (and skk-current-search-prog-list
		     (null (nthcdr (+ 11 (* loop 7))
				   skk-henkan-list)))
	   (setq skk-henkan-list
		 (skk-nunion skk-henkan-list
			     (skk-search)))
	   (skk-henkan-list-filter))
	 (setq henkan-list (nthcdr (+ 4 (* loop 7))
				   skk-henkan-list))))
       (save-window-excursion
	 (setq n (skk-henkan-show-candidate-subr
		  candidate-keys
		  henkan-list))
	 (when (> n 0)
	   (condition-case nil
	       (let* ((event (next-command-event))
		      (char (event-to-character event))
		      (key (skk-event-key event))
		      num)
		 (static-when (eq skk-emacs-type 'xemacs)
		   ;; clear out candidates in echo area
		   (message ""))
		 (if (and (null char)
			  (null key))
		     (skk-unread-event event)
		   (setq key-num-alist (nthcdr (- 7 n)
					       key-num-alist1))
		   (when (and key-num-alist
			      char)
		     (setq num (cdr (or (assq char
					      key-num-alist)
					(assq (if (skk-lower-case-p char)
						  (upcase char)
						(downcase char))
					      key-num-alist)))))
		   (cond
		    (num
		     (setq new-one (nth num henkan-list)
			   skk-henkan-count (+ 4 (* loop 7) num)
			   skk-kakutei-flag t
			   loop nil))
		    ((or (eq char ?\040) ; SPC
			 (skk-key-binding-member
			  key
			  '(skk-nicola-self-insert-rshift)
			  skk-j-mode-map))
		     ;;
		     (if (or skk-current-search-prog-list
			     (nthcdr 7 henkan-list))
			 (setq loop (1+ loop))
		       ;; 候補が尽きた。この関数から抜ける。
		       (let ((last-showed-index (+ 4 (* loop 7))))
			 (setq skk-exit-show-candidates
			       ;; cdr 部は、辞書登録に入る前に最後に表示し
			       ;; た候補群の中で最初の候補を指すインデクス
			       (cons loop last-showed-index))
			 ;; 辞書登録に入る。skk-henkan-count は
			 ;; skk-henkan-list の最後の候補の次 (存在しない
			 ;; --- nil)を指す。
			 (setq skk-henkan-count (+ last-showed-index n)
			       loop nil))))
		    ((or (eq char skk-previous-candidate-char) ; ?x
			 (skk-key-binding-member
			  key
			  '(skk-previous-candidate
			    skk-delete-backward-char
			    skk-undo)
			  skk-j-mode-map))
		     (cond
		      ((= loop 0)
		       ;; skk-henkan-show-candidates を呼ぶ前の
		       ;; 状態に戻す。
		       (setq skk-henkan-count 4)
		       (skk-unread-event
			(character-to-event
			 (aref (car (where-is-internal
				     'skk-previous-candidate
				     skk-j-mode-map))
			       0)))
		       ;; skk-henkan まで一気に throw する。
		       (throw 'unread nil))
		      (t
		       ;; 一つ前の候補群をエコーエリアに表示する。
		       (setq reverse t))))
		    ((skk-key-binding-member
		      key
		      '(keyboard-quit
			skk-kanagaki-bs
			skk-kanagaki-esc)
		      skk-j-mode-map)
		     ;;
		     (signal 'quit nil))
		    (t
		     (skk-message "`%s' は無効なキーです！"
				  "`%s' is not valid here!"
				  (or (key-description key)
				      (key-description char)))
		     (sit-for 1)))))
	     (quit
	      ;; skk-previous-candidate へ
	      (setq skk-henkan-count 0)
	      (skk-unread-event
	       (character-to-event
		(aref (car (where-is-internal
			    'skk-previous-candidate
			    skk-j-mode-map))
		      0)))
	      ;; skk-henkan まで一気に throw する。
	      (throw 'unread nil)))))) ; end of while loop
     ;;
     (or (cdr-safe new-one)
	 new-one))))

(defun skk-henkan-show-candidate-subr (keys candidates)
  ;; key と candidates を組み合わせて 7 つの候補群 (候補数が 7 に満たなかっ
  ;; たらそこで打ち切る) の文字列を作り、ミニバッファに表示する。
  (let ((workinglst
	 ;; CANDIDATES の先頭の 7 つのみのリスト。
	 (let ((count 0) e v)
	   (while (> 7 count)
	     (setq e (nth count candidates))
	     (if e
		 (setq v (cons (cond
				((and (skk-numeric-p) (consp e))
				 (cdr e))
				((not (skk-lisp-prog-p e))
				 e)
				((skk-eval-string e))
				(t e))
			       v)
		       count (1+ count))
	       (setq count 7)))
	   (nreverse v)))
	(n 0)
	str cand message-log-max)
    (when (car workinglst)
      ;;(setq workinglst (skk-truncate-message workinglst))
      (setq n 1
	    ;; 最初の候補の前に空白をくっつけないように最初の候補だけ先に取り
	    ;; 出す。
	    str (concat (car keys) ":" (if (consp (car workinglst))
					   (cdr (car workinglst))
					 (car workinglst))))
      ;; 残りの 6 つを取り出す。候補と候補の間を空白でつなぐ。
      (while (and (< n 7) (setq cand (nth n workinglst)))
	(setq cand (if (consp cand) (cdr cand) cand)
	      str (concat str "  " (nth n keys) ":" cand)
	      n (1+ n)))
      (setq str (format
		 "%s  [残り %d%s]"
		 str (length (nthcdr n candidates))
		 (make-string (length skk-current-search-prog-list) ?+)))
      (if (> (frame-width) (string-width str))
	  (message "%s" str)
	(let ((buff (get-buffer-create "*候補*"))
	      (case-fold-search t))
	  (with-current-buffer buff
	    (erase-buffer)
	    (insert str)
	    (goto-char (point-min))
	    ;; 1 候補に 1 行をわりあてる。
	    (forward-char 2)
	    (while (re-search-forward
		    (concat "  "
			    (mapconcat 'identity keys ":\\|  ") ":\\|"
			    "  \\[残り [0-9]+\\(\\++\\)?\\]") nil t)
	      (goto-char (match-beginning 0))
	      (delete-char 2)
	      (insert "\n"))
	    (goto-char (point-min))
	    (while (and (move-to-column (- (frame-width) 2))
			(not (eobp))
			(>= (frame-width) (current-column)))
	      (when (not (eolp))
		(backward-char 1)
		(insert "\n  "))
	      (forward-line 1)))
	  (let ((minibuf-p (skk-in-minibuffer-p))
		(window (get-buffer-window
			 (skk-minibuffer-origin))))
	    (when minibuf-p
	      (if window
		  (select-window window)
		(other-window 1)))
	    (unless (eq (next-window) (selected-window))
	      ;; *候補* バッファを見易くする。
	      ;; (save-window-excursion の中なので大丈夫なはず)
	      (delete-other-windows))
	    (display-buffer buff)
	    (unless (pos-visible-in-window-p)
	      (recenter '(1)))
	    (when minibuf-p
	      (select-window (minibuffer-window)))))))
    ;; 表示する候補数を返す。
    n))

(defun skk-henkan-in-minibuff ()
  ;; 辞書登録モードに入り、登録した単語の文字列を返す。
  (save-match-data
    (let ((enable-recursive-minibuffers t)
	  ;; XEmacs では次の変数が再帰的ミニバッファの可否に影響する。
	  minibuffer-max-depth
	  ;; 変換中に isearch message が出ないようにする。
	  skk-isearch-message orglen new-one)
      (add-hook 'minibuffer-setup-hook 'skk-j-mode-on)
      (add-hook
       'minibuffer-setup-hook
       '(lambda ()
	  (add-hook 'pre-command-hook 'skk-pre-command nil 'local)))
      (condition-case nil
	  (setq new-one
		(read-from-minibuffer
		 (concat (or (and (skk-numeric-p)
				  (skk-num-henkan-key))
			     (if skk-okuri-char
				 (skk-compute-henkan-key2)
			       skk-henkan-key))
			 " ")
		 (when (and (not skk-okuri-char)
			    skk-read-from-minibuffer-function)
		   (funcall skk-read-from-minibuffer-function))))
	(quit
	 (setq new-one "")))
      (when (and skk-check-okurigana-on-touroku
		 ;; 送りあり変換でも skk-okuri-char だけだと判断できない。
		 skk-henkan-okurigana new-one)
	(setq new-one (skk-remove-redundant-okurgana new-one)))
      (cond
       ((string= new-one "")
	(if skk-exit-show-candidates
	    ;; ミニバッファに表示した候補が尽きて辞書登録に入ったが、空文字
	    ;; 列が登録された場合。最後にミニバッファに表示した候補群を再表
	    ;; 示する。
	    (progn
	      (setq skk-henkan-count (cdr skk-exit-show-candidates))
	      (skk-henkan))
	  ;; skk-henkan-show-candidates に入る前に候補が尽きた場合
	  (setq skk-henkan-count (1- skk-henkan-count))
	  (when (= skk-henkan-count -1)
	    ;; 送りありの変換で辞書登録に入り、空文字を登録した後、その
	    ;; まま再度送りなしとして変換した場合は
	    ;; skk-henkan-okurigana, skk-okuri-char の値を nil にしなけ
	    ;; れば、それぞれの値に古い送り仮名が入ったままで検索に失敗
	    ;; する。
	    (setq skk-henkan-okurigana nil
		  skk-okurigana nil
		  skk-okuri-char nil)
	    (skk-change-marker-to-white)
	    ;; skk-henkan-count が -1 でなければ、カレントバッファでは最後の
	    ;; 候補を表示したままなので (表示関連では何もしなくても、もう既
	    ;; に望みの状態になっている) 何もしない。
	    )))
       (t
	(when (string-match "[ 　]+$" new-one)
	  (setq new-one (substring new-one 0 (match-beginning 0))))
	(setq skk-henkan-list (nconc skk-henkan-list
				     (list new-one)))
	(when (skk-numeric-p)
	  (setq orglen (length skk-henkan-list))
	  (skk-num-convert)
	  (setq new-one (cdr (skk-get-current-candidate-1))))
	(when (or (not orglen)
		  (= orglen (length skk-henkan-list)))
	  (setq skk-kakutei-flag t))
	(setq skk-henkan-in-minibuff-flag t
	      skk-touroku-count (1+ skk-touroku-count))))
      ;; (nth skk-henkan-count skk-henkan-list) が nil だから辞書登録に
      ;; 入っている。skk-henkan-count をインクリメントする必要はない。
      ;; new-one が空文字列だったら nil を返す。
      (unless (string= new-one "")
	new-one))))

(defun skk-compute-henkan-key2 ()
  ;; skk-henkan-okurigana が non-nil なら skk-henkan-key から、かつて
  ;; skk-henkan-key2 と呼ばれていたものを作る。
  ;; skk-henkan-key2 とは、「漢字部分の読み + "*" + 送り仮名」の形式の文字列を
  ;; 言う。
  (when skk-henkan-okurigana
    (save-match-data
      (string-match "[a-z]+$" skk-henkan-key)
      (concat (substring skk-henkan-key 0 (match-beginning 0))
	      "*"
	      skk-henkan-okurigana))))

(defun skk-remove-redundant-okurgana (word)
  ;; 送りありの登録をするとき、送り仮名を消してから [RET] を押さなけ
  ;; れば正しく登録できない。 そこで、ユーザが間違えて送り仮名を消し
  ;; 忘れていないかどうか、 SKK の側でチェックできる範囲についてはユ
  ;; ーザの確認を取る。この部分は`skk-check-okurigana-on-touroku' を
  ;;  non-nil に設定している場合のみ有効。変換が行われたバッファで実
  ;; 行される。ミニバッファ、辞書バッファではない。
  (save-match-data
    (let* ((len (skk-str-length word))
	   (str1 (when (< 0 len)
		   (skk-substring word (1- len) len)))
	   (str2 (when (< 1 len)
		   (skk-substring word (- len 2) (1- len))))
	   (str (if (and str2
			 (string-match "^[ぁ-ん]$" str2))
		    (concat str2 str1)
		  str1)))
      (when (and str
		 (string-match "^[ぁ-ん]$" str1)
		 (or (eq skk-check-okurigana-on-touroku
			 'auto)
		     (skk-y-or-n-p
		      (format
		       "今入力した `%s' の `%s' は送り仮名ですか？"
		       word str)
		      (format
		       "You mean `%s' in `%s' you've given is okurigana?"
		       str word))))
	;; ユーザの指示に従い送り仮名を取り除く。
	(message "")
	(setq word (skk-substring
		    word 0
		    (if (string-match "^[ぁ-ん]$" str2)
			(- len 2)
		      (1- len)))))))
  ;;
  word)

(defun skk-previous-candidate (&optional arg)
  "▼モードであれば、一つ前の候補を表示する。
▼モード以外ではカレントバッファに \"x\" を挿入する。
確定辞書による確定の直後に呼ぶと確定がアンドゥされて、確定前の状態で
直前の見出し語がカレントバッファに挿入される。"
  (interactive "*p")
  (skk-with-point-move
   (cond
    ((not (eq skk-henkan-mode 'active))
     (if (not (eq last-command 'skk-kakutei-henkan))
	 (when (and last-command-char
		    (characterp last-command-char))
	   (skk-kana-input arg))
       ;; restore the state just before the last kakutei henkan.
       (delete-region skk-henkan-start-point (point))
       (skk-set-henkan-point-subr)
       (insert-and-inherit
	(if (not skk-katakana)
	    (skk-get-last-henkan-datum 'henkan-key)
	  (skk-hiragana-to-katakana
	   (skk-get-last-henkan-datum 'henkan-key))))
       (setq this-command 'skk-undo-kakutei-henkan)))
    ((string= skk-henkan-key "")
     nil)
    (t
     (let ((mark (unless (eobp)
		   (skk-save-point
		    (forward-char 1)
		    (point-marker)))))
       (skk-save-point
	(cond
	 ((= skk-henkan-count 0)
	  (when skk-okuri-char
	    ;; roman prefix for okurigana should be removed.
	    (setq skk-henkan-key (substring skk-henkan-key 0 -1)))
	  (when skk-katakana
	    (setq skk-henkan-key
		  (skk-hiragana-to-katakana skk-henkan-key)))
	  (setq skk-henkan-count -1
		skk-henkan-in-minibuff-flag nil
		skk-henkan-list nil
		skk-henkan-okurigana nil
		skk-okuri-char nil
		skk-okuri-index-min -1
		skk-okuri-index-max -1
		skk-okurigana nil
		skk-prefix "")
	  (when (skk-numeric-p)
	    (skk-num-initialize))
	  ;; Emacs 19.28 だと Overlay を消しておかないと、次に insert され
	  ;; る skk-henkan-key に何故か Overlay がかかってしまう。
	  (when skk-use-face
	    (skk-henkan-face-off))
	  (delete-region skk-henkan-start-point skk-henkan-end-point)
	  (goto-char skk-henkan-end-point)
	  (insert-and-inherit skk-henkan-key)
	  (skk-change-marker-to-white))
	 (t
	  (setq skk-henkan-count (1- skk-henkan-count))
	  (skk-insert-new-word (skk-get-current-candidate)))))
       (if mark
	   (progn
	     (goto-char mark)
	     (skk-set-marker mark nil)
	     (backward-char 1))
	 (goto-char (point-max)))
       (when (and skk-abbrev-mode
		  (= skk-henkan-count -1))
	 (skk-abbrev-mode-on)))))))

(defun skk-undo (&optional arg)
  (interactive "*P")
  (cond ((skk-get-prefix skk-current-rule-tree)
	 (skk-kana-cleanup 'force))
	((eq skk-henkan-mode 'active)
	 (skk-previous-candidate))
	((eq skk-henkan-mode 'on)
	 (if (= (point)
		(marker-position skk-henkan-start-point))
	     (skk-kakutei arg)
	   (forward-char -1)
	   (delete-char 1)))
	(t
	 (skk-emulate-original-map arg))))

(defun skk-insert-new-word (word)
  ;; 見出し語を消し、その場所へ変換結果の文字列を挿入する。
  ;; Emacs 19.28 だと Overlay を消しておかないと、次に insert される
  ;; skk-henkan-key に何故か Overlay がかかってしまう。
  (save-match-data
    (let (note)
      (when (string-match ";" word)
	(setq note (substring word (match-end 0))
	      word (substring word 0 (match-beginning 0))))
      (when (skk-lisp-prog-p word)
	(let ((res (skk-eval-string word)))
	  (if (consp res)
	      (setq word (car res)
		    note (cdr res))
	    (setq word res))))
      (when skk-use-face
	(skk-henkan-face-off))
      (delete-region skk-henkan-start-point skk-henkan-end-point)
      (goto-char skk-henkan-start-point)
      (insert-and-inherit word)
      (skk-set-marker skk-henkan-end-point (point))
      (when skk-use-face
	(skk-henkan-face-on))
      (when (and skk-show-annotation
		 note)
	(skk-annotation-show note))
      (when skk-insert-new-word-function
	(funcall skk-insert-new-word-function)))))

(defun skk-kakutei (&optional word)
  "現在表示されている語で確定し、辞書の更新を行う。
カレントバッファで SKK モードになっていなかったら SKK モードに入る。
オプショナル引数の WORD を渡すと、現在表示されている候補とは無関係に
WORD で確定する。"
  ;; read only でエラーになるようにすると read only バッファで SKK が起動でき
  ;; なくなる。
  (interactive)
  (let ((inhibit-quit t)
	converted kakutei-word)
    (when skk-henkan-mode
      (cond
       ((eq skk-henkan-mode 'active)
	(setq kakutei-word
	      ;; 確定辞書の語で確定したときは、辞書にその語を書き込む必要もな
	      ;; いし、更新する必要もないと思っていたが、補完を行なうときは、
	      ;; 個人辞書を参照する (確定辞書は参照しない) ので、多少資源と時
	      ;; 間を無駄にしても、個人辞書に確定辞書のエントリを書き込んで更
	      ;; 新もしておく。
	      (or word (skk-get-current-candidate 'noconv)))
	(when (or (and (not skk-search-excluding-word-pattern-function)
		       kakutei-word)
		  (and kakutei-word
		       skk-search-excluding-word-pattern-function
		       (not (funcall
			     skk-search-excluding-word-pattern-function
			     kakutei-word))))
	  (skk-update-jisyo kakutei-word)
	  (when (skk-numeric-p)
	    (setq converted (skk-get-current-candidate))
	    (skk-num-update-jisyo kakutei-word converted))))
       (t
	;; ▽モードで確定した場合。便宜的に現在のポイントまでを見出し語を扱い
	;; して履歴を更新する。
	(when (and (> skk-kakutei-history-limit 0)
		   (< skk-henkan-start-point (point)))
	  (skk-update-kakutei-history
	   (buffer-substring-no-properties
	    skk-henkan-start-point (point))))))
      (when skk-mode
	(skk-kakutei-cleanup-buffer)
	;; KAKUTEI-WORD などの情報が必要であれば、skk-last-henkan-data
	;; から得られる。必要なデータがそれらの変数に限定されないので、
	;; 引数にしない。
	(when skk-kakutei-end-function
	  (funcall skk-kakutei-end-function))
	(skk-kakutei-initialize
	 (if (skk-numeric-p)
	     (cons kakutei-word converted)
	   kakutei-word))))
    (skk-do-auto-fill)
    (if skk-mode
	(unless (or skk-j-mode
		    skk-jisx0201-mode)
	  (skk-j-mode-on skk-katakana))
      ;; カレントバッファでまだ skk-mode が
      ;; コールされていなかったら、コールする。
      (skk-mode 1)))
  nil)

(defun skk-kakutei-cleanup-buffer ()
  ;; 確定直後のバッファの整形を行なう。
  (when skk-okurigana
    (skk-delete-okuri-mark))
  (skk-delete-henkan-markers)
  (when skk-undo-kakutei-word-only
    (cond
     ((> (point) (marker-position skk-henkan-start-point))
      (let ((word (buffer-substring-no-properties
		   skk-henkan-start-point (point))))
	(delete-region skk-henkan-start-point (point))
	(setq buffer-undo-list skk-last-buffer-undo-list)
	(setq skk-last-buffer-undo-list t)
	(set-buffer-modified-p skk-last-buffer-modified)
	(skk-insert-str word)
	(skk-set-marker skk-henkan-end-point (point))))
     (t
      (setq buffer-undo-list skk-last-buffer-undo-list)
      (setq skk-last-buffer-undo-list t)
      (set-buffer-modified-p skk-last-buffer-modified))))
  (when (and (boundp 'self-insert-after-hook)
	     self-insert-after-hook)
    (funcall self-insert-after-hook
	     skk-henkan-start-point (point)))
  (when overwrite-mode
    (skk-del-char-with-pad
     (skk-ovwrt-len
      (string-width
       (buffer-substring-no-properties
	skk-henkan-start-point (point)))))))

(defun skk-kakutei-initialize (&optional kakutei-word)
  ;; 確定時に変数の初期化とアンドゥのための変数の保存を行なう。
  (when (and kakutei-word
	     (or (consp kakutei-word)
		 (not (string= kakutei-word ""))))
    (setq skk-kakutei-count (1+ skk-kakutei-count))
    ;; skk-undo-kakutei のために最後の変換のデータを保存する。
    (skk-put-last-henkan-data
     (list (cons 'henkan-key skk-henkan-key)
	   (cons 'okuri-char skk-okuri-char)
	   (cons 'henkan-okurigana skk-henkan-okurigana)
	   (cons 'henkan-list
		 ;; 確定した語を先頭にする。
		 (cons kakutei-word
		       (delete kakutei-word skk-henkan-list)))
	   ;; (eq last-command 'skk-kakutei-henkan) でポータブルに確認でき
	   ;; るのであえていらないか。
	   ;; (cons 'kakutei-henkan (eq this-command 'skk-kakutei-henkan))
	   ;; 上記以外の henkan data を skk-last-henkan-data に残したかったら、
	   ;; skk-kakutei-end-function を利用する。
	   )))
  (setq skk-abbrev-mode nil
	skk-exit-show-candidates nil
	skk-henkan-count -1
	skk-henkan-in-minibuff-flag nil
	skk-henkan-key nil
	skk-henkan-list nil
	skk-henkan-okurigana nil
	skk-henkan-mode nil
	skk-kakutei-flag nil
	skk-okuri-char nil
	skk-okuri-index-min -1
	skk-okuri-index-max -1
	;; skk-prefix ""
	))

(defun skk-undo-kakutei ()
  "一番最後の確定をアンドゥし、見出しに対する候補を表示する。
最後に確定したときの候補はスキップされる。
候補が他にないときは、ミニバッファでの辞書登録に入る。"
  (interactive)
  (skk-with-point-move
   (cond ((eq last-command 'skk-undo-kakutei)
	  (skk-error "確定アンドゥは連続使用できません"
		     "Cannot undo kakutei repeatedly"))
	 ((eq skk-henkan-mode 'active)
	  (skk-error "▼モードでは確定アンドゥできません"
		     "Cannot undo kakutei in ▼ mode"))
	 ( ; skk-henkan-key may be nil or "".
	  (or (not (skk-get-last-henkan-datum 'henkan-key))
	      (string= (skk-get-last-henkan-datum 'henkan-key) ""))
	  (skk-error "アンドゥデータがありません"
		     "Lost undo data")))
   (condition-case nil
       (let ((end
	      (if (skk-get-last-henkan-datum 'henkan-okurigana)
		  (+ (length (skk-get-last-henkan-datum
			      'henkan-okurigana))
		     skk-henkan-end-point)
		skk-henkan-end-point)))
	 (setq skk-henkan-mode 'active
	       skk-current-search-prog-list
	       (if (eq (car (car skk-search-prog-list))
		       'skk-search-kakutei-jisyo-file)
		   ;; 確定辞書は探しても無意味。
		   (cdr skk-search-prog-list)
		 skk-search-prog-list))
	 ;; get henkan data back from skk-last-henkan-data.
	 (setq skk-henkan-key (skk-get-last-henkan-datum 'henkan-key)
	       skk-henkan-list (skk-get-last-henkan-datum 'henkan-list)
	       skk-henkan-okurigana (skk-get-last-henkan-datum
				     'henkan-okurigana)
	       skk-okuri-char (skk-get-last-henkan-datum 'okuri-char))
	 (when skk-use-numeric-conversion
	   (setq skk-num-list (skk-get-last-henkan-datum 'skk-num-list)))
	 (when (>= (point-max) end)
	   ;; 最後の変換部分のテキストを消す。送り仮名を把握しているのなら
	   ;; (skk-process-okuri-early が non-nil なら送り仮名を把握できない)、
	   ;; 送り仮名を含めた部分までを消す。
	   (delete-region skk-henkan-start-point end))
	 (when skk-undo-kakutei-word-only
	   (setq skk-last-buffer-undo-list buffer-undo-list
		 buffer-undo-list t
		 skk-last-buffer-modified (buffer-modified-p)))
	 (goto-char skk-henkan-start-point)
	 (insert-and-inherit "▼")
	 (skk-set-marker skk-henkan-start-point (point))
	 (cond
	  (skk-okuri-char
	   ;; 送りあり
	   (insert-and-inherit (substring skk-henkan-key 0
					  (1- (length skk-henkan-key))))
	   (skk-set-marker skk-henkan-end-point (point))
	   (when skk-henkan-okurigana
	     (insert-and-inherit skk-henkan-okurigana)))
	  (t
	   (insert-and-inherit skk-henkan-key)
	   (skk-set-marker skk-henkan-end-point (point))))
	 (skk-message "確定アンドゥ！"
		      "Undo kakutei!")
	 (setq skk-henkan-count 1)
	 (skk-henkan))
     ;; skk-kakutei-undo から途中で抜けた場合は、各種フラグを初期化しておかない
     ;; と次の動作をしようとしたときにエラーになる。
     ((error quit)
      (skk-kakutei)))))

(defun skk-set-henkan-point (&optional arg)
  ;;"変換を開始するポイントをマークし、対応する skk-prefix か母音を入力する。"
  (let* ((last-char (skk-downcase last-command-char))
	 (normal (not (eq last-char last-command-char)))
	 (sokuon (if (string= skk-prefix (char-to-string last-char))
		     (/= last-char ?o)
		   nil))
	 (henkan-active (eq skk-henkan-mode 'active)))
    (cond
     ((not (eq skk-henkan-mode 'on))
      (if normal
	  (skk-set-henkan-point-subr)
	(when skk-henkan-mode
	  (skk-set-henkan-point-subr))
	(if henkan-active
	    (skk-emulate-original-map arg)
	  ;; What's to be here?
	  ;;(skk-insert arg)
	  )))
      ((not normal)
       ;; special char
       (insert-and-inherit last-char)
       (skk-set-marker skk-henkan-end-point (point))
       (setq skk-henkan-count 0
	     skk-henkan-key (buffer-substring-no-properties
			     skk-henkan-start-point (point))
	     skk-prefix "")
       (skk-henkan))
      ;; prepare for the processing of okurigana if not skk-okurigana
      ;; and the preceding character is not a numeric character.
      ;; if the previous char is a special midashi char or a
      ;; numeric character, we assume that the user intended to type the
      ;; last-command-char in lower case.
      ((and (or
	     ;; for KAnji, KanJIru
	     (not (skk-get-prefix skk-current-rule-tree))
	     (if (/= skk-kana-start-point skk-henkan-start-point)
		 (prog1
		     t
		   (unless sokuon ; for TaSSi or TasSi
		     (skk-kana-cleanup))) ; for NEko
	       nil))
	    (not skk-okurigana)
	    (or (= skk-henkan-start-point (point))
		(let ((p (char-before)))
		  (not (or
			;; previous char is a special midashi char
			(memq p skk-special-midashi-char-list)
			;; previous char is an ascii numeric char
			(and (<= ?0 p)
			     (<= p ?9))
			;; previous char is a JIS X 0208 numeric char
			(and (skk-jisx0208-p p)
			     (= (skk-char-octet p 0) 35) ;?#
			     (<= 48 (skk-char-octet p 1)) ; ?0
			     (<= (skk-char-octet p 1) 57))  ; ?9
			)))))
       (cond
	(skk-process-okuri-early
	 (skk-set-marker skk-henkan-end-point (point))
	 (let ((char (char-to-string last-char)))
	   (setq skk-okuri-char
		 (or (cdr (assoc char skk-okuri-char-alist))
		     char)))
	 (cond
	  (sokuon
	   (setq skk-henkan-key
		 (concat (buffer-substring-no-properties
			  skk-henkan-start-point
			  skk-kana-start-point)
			 (if skk-katakana "ッ" "っ")
			 skk-henkan-okurigana))
	   (skk-erase-prefix)
	   (insert-and-inherit (if skk-katakana "ッ " "っ "))
	   (setq skk-prefix ""
		 skk-henkan-count 0)
	   (skk-henkan)
	   (delete-backward-char 2))
	  (t
	   (setq skk-henkan-key (concat
				 (buffer-substring-no-properties
				  skk-henkan-start-point
				  (point))
				 skk-okuri-char))
	   (insert-and-inherit " ")
	   (setq skk-prefix ""
		 skk-henkan-count 0)
	   (skk-henkan)
	   (delete-backward-char 1)))
	 ;; we set skk-kana-start-point here, since the marker may no
	 ;; longer point at the correct position after skk-henkan.
	 (skk-set-marker skk-kana-start-point (point)))
	((/= skk-henkan-start-point (point))
	 (when sokuon
	   (skk-erase-prefix 'clean)
	   (insert-and-inherit (if skk-katakana "ッ" "っ")))
	 (skk-set-marker skk-okurigana-start-point (point))
	 (insert-and-inherit "*")
	 (skk-set-marker skk-kana-start-point (point))
	 (setq skk-okuri-char (char-to-string last-char)
	       skk-okurigana t)))))
    (when normal
      (setq last-command-char last-char)
      (skk-kana-input arg))))

(defun skk-start-henkan (arg)
  "▽モードでは漢字変換を開始する。▼モードでは次の候補を表示する。
▽モードで、カタカナモードのまま漢字変換を開始すると、見出し語を平仮名に
変換後、漢字変換を開始する。
見出し語の変換せずにそのまま漢字変換を行ないたければ、C-u SPC \(arg が 4
になる\) とタイプする。"
  (interactive "*p")
  (skk-with-point-move
   (cancel-undo-boundary)
   (if (eq skk-henkan-mode 'active)
       (progn
	 (setq skk-henkan-count (1+ skk-henkan-count))
	 (skk-henkan))
     (save-match-data
       (let (pos)
	 (skk-kana-cleanup 'force)
	 (when (skk-get-prefix skk-current-rule-tree)
	   ;; Never.  `skk-erase-prefix' called by `skk-kana-cleanup'
	   ;; initializes `skk-prefix'.
	   (skk-error "フィックスされていない skk-prefix があります"
		      "Have unfixed skk-prefix"))
	 (setq pos (point))
	 (when (< pos skk-henkan-start-point)
	   (skk-error
	    "カーソルが変換開始地点より前にあります"
	    "Henkan end point must be after henkan start point"))
	 (setq skk-henkan-key (buffer-substring-no-properties
			       skk-henkan-start-point pos))
	 (when (and skk-katakana
		    (= arg 1))
	   (setq skk-henkan-key (skk-katakana-to-hiragana skk-henkan-key)))
	 (when (and skk-okurigana
		    (string-match "\\* *$" skk-henkan-key))
	   (skk-error
	    "空の送り仮名で漢字を登録しようとしています"
	    "No okurigana!"))
	 (if skk-allow-spaces-newlines-and-tabs
	     ;; skk-henkan-key の中の "[ \n\t]+" を完全に取り除く。
	     (while (string-match "[ \n\t]+" skk-henkan-key)
	       (setq skk-henkan-key
		     (concat (substring skk-henkan-key 0 (match-beginning 0))
			     (substring skk-henkan-key (match-end 0)))))
	   (skk-save-point
	    (beginning-of-line)
	    (when (> (point) skk-henkan-start-point)
	      (skk-error
	       "変換キーに改行が含まれています"
	       "Henkan key may not contain a new line character")))
	   ;; 最初のスペースで skk-henkan-key をちょん切るだけ。
	   (setq skk-henkan-key (substring skk-henkan-key
					   0
					   (string-match " "
							 skk-henkan-key))))
	 (skk-set-marker skk-henkan-end-point pos)
	 (setq skk-henkan-count 0)
	 (skk-henkan)
	 (when (and skk-abbrev-mode
		    (eq skk-henkan-mode 'active))
	   ;; こうしておかないと変換後、次に入力される文字もまた
	   ;; SKK abbrev-mode 入力になってしまう。
	   (skk-j-mode-on skk-katakana)
	   (setq skk-abbrev-mode t)))))))

(defun skk-auto-start-henkan (str)
  ;; skk-auto-start-henkan-keyword-list の要素の文字列を挿入したときに自動的に
  ;; (スペースを打鍵しなくとも) 変換を開始する。エー×イソフト社の MSDOS 用 の
  ;; FEP、WX2+ 風。
  (when (member str skk-auto-start-henkan-keyword-list)
    (skk-save-point
     (backward-char 1)
     (when (> (point) skk-henkan-start-point)
       (let ((skk-prefix ""))
	 (skk-start-henkan (prefix-numeric-value current-prefix-arg)))))))

(defun skk-backward-and-set-henkan-point (arg)
  "ポイントの直前にある文字列の先頭に変換開始ポイントを示す \"▽\" を付ける。
カーソルの直前にある文字 \(スペース文字、タブ文字、長音を表わす「ー」 は無条件
にスキップされる\) を skk-what-char-type にて判別し、同種の文字列をひとかたま
りとして後方へスキップする。
但し、ひらかなの場合は「を」の直前で、カタカナの場合は「ヲ」の直前で止まる。
C-u ARG で ARG を与えると、その文字分だけ戻って同じ動作を行なう。"
  (interactive "*P")
  (if (not skk-mode)
      (skk-emulate-original-map arg)
    (catch 'exit1
      (skk-save-point
       ;; とりあえず最初の SPC, TAB, 全角 SPC だけジャンプする。
       (skip-chars-backward " \t　")
       ;; 引数あり。
       (cond
	(arg
	 (if (not skk-allow-spaces-newlines-and-tabs)
	     (backward-char (prefix-numeric-value arg))
	   (setq arg (prefix-numeric-value arg))
	   (while (> arg 0)
	     (skip-chars-backward " \t　")
	     (if (bolp)
		 ;; 行頭だったら一行前の行末まで戻るが、arg は減らさない。
		 (backward-char 1)
	       (backward-char 1)
	       (setq arg (1- arg))))))
	(t
	 ;; 引数なし。
	 (let ((limit
		(if (not skk-allow-spaces-newlines-and-tabs)
		    (skk-save-point (beginning-of-line) (point))
		  (point-min)))
	       ;; ＿￣＾¨｀´゜゛！？；：・．，。
	       (unknown-chars-regexp
		(if skk-allow-spaces-newlines-and-tabs
		    "[ 　\n\tー〃ゞゝヾヽ]"
		  "[　ー〃ゞゝヾヽ]"))
	       type p)
	   (save-match-data
	     (skk-save-point
	      (backward-char 1)
	      (while (and (> (point) limit)
			  ;; unknown-chars-regexp では文字種別が判別できないの
			  ;; で、その文字列が続く限りポイントをバッファの先頭
			  ;; 方向へ戻す。
			  (looking-at unknown-chars-regexp))
		(backward-char 1))
	      (setq type (skk-what-char-type))
	      (when (eq type 'unknown)
		(throw 'exit1 nil))
	      (skk-backward-and-set-henkan-point-1 type)
	      (setq p (point))
	      (when skk-allow-spaces-newlines-and-tabs
		(while (and (> (point) limit) (bolp))
		  ;; 1 行上の行末へ。
		  (backward-char 1)
		  ;; ポイントが判別できない文字種別の上にある間は
		  ;; backward 方向へポイントを戻す。
		  ;;(while (and (> (point) limit)
		  ;;            (looking-at unknown-chars-regexp))
		  ;;  (backward-char 1))
		  (when ;;(or
		      (> 0 (skk-backward-and-set-henkan-point-1 type))
		    ;;(eq (skk-what-char-type) type))
		    (setq p (point)))))))
	   (goto-char p)
	   (skip-chars-forward unknown-chars-regexp))))
       (skk-set-henkan-point-subr)))))

(defun skk-backward-and-set-henkan-point-1 (type)
  ;; skk-backward-and-set-henkan-point のサブルーチン。CHAR の種類に応じた文字
  ;; をスキップしてバッファの先頭方向へ戻る。
  (cond ((eq type 'hiragana)
	 ;; "を" の前で止まった方が便利？
	 (skip-chars-backward "ヽヾゝゞ〃ーんぁ-ゑ"))
	((eq type 'katakana)
	 ;; "ヲ" の前で止まった方が便利？
	 (skip-chars-backward "ヽヾゝゞ〃ーンァ-ヱ"))
	((eq type 'jisx0208-latin)
	 (skip-chars-backward "　-ｚ"))
	((eq type 'ascii)
	 (skip-chars-backward " -~"))))

(defun skk-what-char-type ()
  ;; 現在のポイントにある文字がどんな種類かを判別する。
  (save-match-data
    (cond ((looking-at "[ぁ-ん]")
	   'hiragana)
	  ((looking-at "[ァ-ン]")
	   'katakana)
	  ;; "ー" を除外している ("ー" は "〇" と "―" の間に入っている)。
	  ((looking-at "[　-〇―-ｚ]")
	   'jisx0208-latin)
	  ((looking-at "[ -~]")
	   'ascii)
	  (t
	   'unknown))))

(defun skk-set-henkan-point-subr (&optional arg)
  "かなを入力した後で、ポイントに変換開始のマーク \(▽\) を付ける。
この関数は skk-set-henkan-point の内部関数としても使用されている。"
  (interactive "*P")
  (skk-with-point-move
   (unless skk-undo-kakutei-word-only
       (cancel-undo-boundary))
   (if skk-henkan-mode
       (skk-kakutei)
     (skk-kana-cleanup));; XXX
   (when skk-undo-kakutei-word-only
     (setq skk-last-buffer-undo-list buffer-undo-list
	   buffer-undo-list t
	   skk-last-buffer-modified (buffer-modified-p)))
   (if (not (skk-get-prefix skk-current-rule-tree))
       (insert-and-inherit "▽")
     (skk-erase-prefix)
     (insert-and-inherit "▽")
     (skk-set-marker skk-kana-start-point (point))
     (skk-insert-prefix))
   (setq skk-henkan-mode 'on)
   (skk-set-marker skk-henkan-start-point (point)))
   nil)

(defun skk-change-marker ()
  ;; "▽"を"▼"に変える。`skk-henkan-mode' を active にする。
  (skk-save-point
   (goto-char (- skk-henkan-start-point skk-kanji-len))
   (unless (looking-at "▽")
     (skk-kakutei)
     (skk-error "▽がありません"
		"It seems that you have deleted ▽"))
   (cancel-undo-boundary)
   (let ((buffer-undo-list t))
     (insert-and-inherit "▼")
     (delete-char 1))
   (setq skk-henkan-mode 'active)))

(defun skk-change-marker-to-white ()
  ;; "▼"を"▽"に変える。`skk-henkan-mode' を on にする。
  (skk-save-point
   (goto-char (- skk-henkan-start-point skk-kanji-len))
   (cancel-undo-boundary)
   (if (looking-at "▼")
       (let ((buffer-undo-list t))
	 (insert-and-inherit "▽")
	 (delete-char 1))
     (goto-char skk-henkan-start-point)
     (insert-and-inherit "▽")
     (skk-set-marker skk-henkan-start-point (point))
     (skk-message "▼がありません"
		  "It seems that you have deleted ▼"))
   (setq skk-henkan-mode 'on)))

(defun skk-delete-henkan-markers (&optional nomesg)
  ;; 変換時にカレントバッファに表われる `▽', `▼' マークを消す。
  (when (marker-position skk-henkan-start-point)
    (save-match-data
      (skk-save-point
       (goto-char (- skk-henkan-start-point skk-kanji-len))
       (cond
	((eq skk-henkan-mode 'active)
	 (when skk-use-face
	   (skk-henkan-face-off))
	 (if (looking-at "▼")
	     (delete-char 1)
	   (unless nomesg
	     (skk-message "▼がありません"
			  "It seems that you have deleted ▼"))))
	((looking-at "▽")
	 (delete-char 1))
	((not nomesg)
	 (skk-message "▽がありません"
		      "It seems that you have deleted ▽")))))))

(defun skk-delete-okuri-mark ()
  ;; 送り仮名入力中にカレントバッファに表われる `*' マークを消し、
  ;; 送り仮名関連フラグを nil にセットする。
  (when (and skk-okurigana
	     skk-okurigana-start-point
	     (markerp skk-okurigana-start-point)
	     (marker-position skk-okurigana-start-point))
    (skk-save-point
     (when (eq ?* (char-after skk-okurigana-start-point))
       (delete-region skk-okurigana-start-point
		      (1+ skk-okurigana-start-point))))
    (setq skk-okurigana nil
	  skk-okuri-char nil
	  skk-henkan-okurigana nil)))

;;; jisyo related functions
(defun skk-purge-from-jisyo (&optional arg)
  "▼モードで現在の候補を辞書バッファから消去する。"
  (interactive "*P")
  (skk-with-point-move
   (when (and (eq skk-henkan-mode 'active)
	      (not (string= skk-henkan-key ""))
	      (yes-or-no-p
	       (format
		(if skk-japanese-message-and-error
		    "%s /%s/%sを辞書から削除します。良いですか？"
		  "Really purge \"%s /%s/%s\"?")
		skk-henkan-key
		(skk-get-current-candidate)
		(cond
		 ((not (and skk-henkan-okurigana
			    (or skk-henkan-okuri-strictly
				skk-henkan-strict-okuri-precedence)))
		  " ")
		 (skk-japanese-message-and-error
		  (format " (送り仮名: %s) " skk-henkan-okurigana))
		 (t
		  (format " (okurigana: %s) " skk-henkan-okurigana))))))
     ;; skk-henkan-start-point から point まで削除してしまっても、変換直後
     ;; に (カーソルを動かすことなく) skk-purge-from-jisyo を呼べば問題ない
     ;; が、カーソルが違う場所へ移動していた場合は、削除すべきでないものま
     ;; で削除してしまう可能性がある。そこで、送り仮名があればその長さを含
     ;; めた end を求め、今回の変換に関連した個所だけを正確に切り取るように
     ;; する。
     (let ((end (if skk-henkan-okurigana
		    (+ (length skk-henkan-okurigana)
		       skk-henkan-end-point)
		  skk-henkan-end-point))
	   (word (skk-get-current-candidate)))
       (skk-update-jisyo word 'purge)
       ;; Emacs 19.28 だと Overlay を消しておかないと、次に insert される
       ;; skk-henkan-key に何故か Overlay がかかってしまう。
       (when skk-use-face
	 (skk-henkan-face-off))
       (delete-region skk-henkan-start-point end)
       (skk-change-marker-to-white)
       (skk-kakutei))))
  nil)

(defun skk-save-jisyo (&optional quiet)
  "SKK の辞書バッファをセーブする。
オプショナル引数の QUIET が non-nil であれば、辞書セーブ時のメッセージを
出さない。"
  (interactive "P")
  ;; skk.el 以外で提供される辞書セーブ機能を利用できるように関数を funcall する
  ;; 形にしておく。
  (funcall skk-save-jisyo-function quiet))

(defun skk-save-jisyo-original (&optional quiet)
  ;;"SKK の辞書バッファをセーブする。
  ;;オプショナル引数の QUIET が non-nil であれば、辞書セーブ時のメッセージを
  ;;出さない。"
  (let ((jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (if (not (and jisyo-buffer
		  (buffer-modified-p jisyo-buffer)))
	(unless quiet
	  (skk-message "SKK 辞書を保存する必要はありません"
		       "No need to save SKK jisyo")
	  (sit-for 1))
      ;;
      (with-current-buffer jisyo-buffer
	(when (and skk-share-private-jisyo
		 (skk-jisyo-is-shared-p))
	  (lock-buffer skk-jisyo)
	  (skk-update-shared-jisyo))
	(let ((inhibit-quit t)
	      (tempo-file (skk-make-temp-jisyo)))
	  (unless quiet
	    (skk-message "SKK 辞書を保存しています..."
			 "Saving SKK jisyo..."))
	  (skk-save-jisyo-as tempo-file)
	  (skk-check-size-and-do-save-jisyo tempo-file)
	  ;; 辞書のセーブに成功して初めて modified フラッグを nil にする。
	  (set-buffer-modified-p nil)
	  (setq skk-update-jisyo-count 0)
	  (unless quiet
	    (skk-message "SKK 辞書を保存しています...完了！"
			 "Saving SKK jisyo...done")
	    (sit-for 1)))
	(when skk-share-private-jisyo
	  (skk-init-shared-jisyo)
	  (unlock-buffer))))))

(defun skk-init-shared-jisyo ()
  (fillarray skk-jisyo-update-vector nil)
  (with-temp-buffer
    (insert skk-emacs-id "\n")
    (write-region 1 (point-max) skk-emacs-id-file nil 'nomsg)))

(defun skk-jisyo-is-shared-p ()
  (and (file-exists-p skk-emacs-id-file)
       (with-temp-buffer
	 (insert-file-contents skk-emacs-id-file)
	 (goto-char (point-min))
	 ;; 個人辞書が他の emacs 上の skk により更新されたかをチェック
	 (not (search-forward skk-emacs-id nil t)))))

(defun skk-update-shared-jisyo ()
  ;; 現在の jisyo-buffer の内容を消去して、他の emacs 上の skk が
  ;; 更新した skk-jisyo を読み込む。
  (erase-buffer)
  (insert-file-contents skk-jisyo)
  (skk-setup-jisyo-buffer)
  ;; skk-jisyo-update-vector にしたがってバッファを更新する。
  (let ((index 0) list skk-henkan-key)
    (while (and (< index skk-jisyo-save-count)
		(setq list (aref skk-jisyo-update-vector index)))
      ;; skk-update-jisyo-1, skk-search-jisyo
      ;; で参照される skk-henkan-key をセットする
      (setq skk-henkan-key (car list))
      (skk-update-jisyo-1
       ;; okurigana    word
       (nth 1 list) (nth 2 list)
       (skk-search-jisyo (nth 1 list) 0 'delete)
       ;; purge
       (nth 3 list))
      (setq index (1+ index)))))

(defun skk-save-jisyo-as (file)
  (save-match-data
    (let (buffer-read-only)
      (goto-char (point-min))
      (unless (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
	(skk-error
	 "\
送りありエントリのヘッダーがありません！ SKK 辞書のセーブを中止します"
	 "\
Header line for okuri-ari entries is missing!  Stop saving SKK jisyo"))
      ;; おっ、コメントフェイスが $ で終わらないぞ > hilit19.el
      (unless (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
	(skk-error
	 "\
送りなしエントリのヘッダーがありません ！ SKK 辞書のセーブを中止します"
	 "\
Header line for okuri-nasi entries is missing!  Stop saving SKK jisyo")))
    (write-region-as-coding-system
     (skk-find-coding-system skk-jisyo-code)
     1 (point-max) file nil 'nomsg)))

(defun skk-check-size-and-do-save-jisyo (new-file)
  (let ((new-size (nth 7 (file-attributes new-file)))
	old-size
	;; yes-or-no-p に回答し、newline すると、this-command が変ってしまう。
	this-command this-command-char last-command last-command-char)
    (when (= new-size 0)
      (delete-file new-file)
      (skk-error "SKK 辞書が空になっています！ 辞書のセーブを中止します"
		 "Null SKK jisyo!  Stop saving jisyo"))
    (cond
     ((or (not skk-compare-jisyo-size-when-saving)
	  ;; 旧辞書とのサイズ比較を行なわない。
	  (progn
	    ;; (1)skk-jisyo がないか、
	    ;; (2)new-file と skk-jisyo が同一のサイズか
	    ;;    (skk-(aux-)large-jisyo から新規の単語を読み込まなかったり、
	    ;;    新規単語の登録を行なわなかった場合はサイズが同じ)、
	    ;; (3)new-file の方が大きい
	    ;; 場合 (上記の 3 通りであればいずれも正常)。
	    (setq old-size (nth 7 (file-attributes skk-jisyo)))
	    (or (not old-size)
		(>= new-size old-size))))
      (skk-make-new-jisyo new-file))
     ((skk-yes-or-no-p
       (format
	"skk-jisyo が %dbytes 小さくなりますが、セーブして良いですか？"
	(- old-size new-size))
       (format
	"New %s will be %dbytes smaller.  Save anyway?"
	skk-jisyo (- old-size new-size)))
      ;; とにかくセーブ。
      (skk-make-new-jisyo new-file))
     (t
      ;; セーブとり止め。
      (delete-file new-file)
      (with-output-to-temp-buffer "*SKK warning*"
	(if skk-japanese-message-and-error
	    (princ "\
セーブしようとする辞書のサイズが元のものよりも小さくなってしまうので、
セーブを中止しました。辞書のサイズが小さくなった原因には例えば、

    (a) M-x skk-purge-from-jisyo を実行した。

    (b) ~/.skk-jisyo の漢字コードと、違う漢字コードで \" *.skk-jisyo*\"
       バッファが保存されようとしている。

    (c) \" *.skk-jisyo*\" バッファを自分で編集した。

などがあります。a と b の場合は、異常ではありません。c の場合は、編集の
内容によります。原因を確認後、慎重に辞書を保存することをお勧めします。

元の辞書を再度読み込むには、

    M-x skk-reread-private-jisyo

を実行して下さい。")
	  (princ "\
Saving your private dictionary has been canceled, since the size of the
dictionary will be smaller.  The following cases should be considered:

   (a) You executed M-x skk-purge-from-jisyo,

   (b) The coding system SKK tried to save \" *.skk-jisyo*\" buffer in
       is different from that of ~/.skk-jisyo.

   (c) You have edited \" *.skk-jisyo*\" buffer manually.

Either the case (a) or (b) is not strange.  Probability of the case (c)
depends on how you edited the buffer.  Anyway, it is strongly recommended
that you check each of the cases above and save the dictionary carefully.

If you want to restore the dictionary from the disc, try

    M-x skk-reread-private-jisyo
")))
      (skk-error "SKK 辞書のセーブを中止しました！"
		 "Stop saving SKK jisyo!")))))

(defun skk-make-temp-jisyo ()
  ;; SKK 個人辞書保存のための作業用のファイルを作り、ファイルのモードを
  ;; skk-jisyo のものと同じに設定する。作った作業用ファイルの名前を返す。
  (let* ((dir (static-cond
	       ((fboundp 'temp-directory)
		(temp-directory))
	       (t
		(cond
		 ((skk-file-exists-and-writable-p temporary-file-directory)
		  temporary-file-directory)
		 (t
		  (unless (file-exists-p "~/tmp")
		    (make-directory "~/tmp"))
		  (unless (file-writable-p "~/tmp")
		    (set-file-modes "~/tmp" 1023))
		  "~/tmp/")))))
	 (temp-name (make-temp-name
		     (expand-file-name
		      (concat (user-login-name) "-skk")
		      (expand-file-name dir)))))
    (skk-create-file temp-name nil nil 384) ; 0600
    temp-name))

(defun skk-make-new-jisyo (tempo-file)
  ;; TEMPO-FILE を新規の skk-jisyo にする。skk-backup-jisyo が non-nil だった
  ;; らバックアップ辞書を作る。
  (if skk-backup-jisyo
      (progn
	(when (file-exists-p skk-backup-jisyo)
	  (delete-file skk-backup-jisyo))
	(rename-file skk-jisyo skk-backup-jisyo))
    (delete-file skk-jisyo))
  (rename-file tempo-file skk-jisyo 'ok-if-already-exists))

(defun skk-reread-private-jisyo (&optional force)
  "バッファに読み込んだ個人辞書を破棄し、ファイルからバッファへ再読み込みする。
オプショナル引数の FORCE が non-nil であれば、破棄の確認をしない。"
  (interactive "P")
  (let ((buf (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (when (and buf
	       (or force
		   (skk-yes-or-no-p
		    "編集中の個人辞書を破棄しますか？"
		    "Discard your editing private JISYO?")))
      (with-current-buffer buf
	(set-buffer-modified-p nil)
	(kill-buffer buf))
      (unless (skk-get-jisyo-buffer skk-jisyo 'nomsg)
	(skk-error "個人辞書を再読み込みすることができません！"
		   "Cannot reread private JISYO!")))))

(defun skk-record-jisyo-data ()
  "辞書データを skk-record-file にセーブする。"
  (unless (or (not skk-keep-record)
	      (> 1 skk-kakutei-count))
    (with-temp-file skk-record-file
      (insert-file-contents skk-record-file)
      (goto-char (point-min))
      (insert (format
	       "%s  登録: %3d  確定: %4d  確定率: %3d%%  語数:%6d\n"
	       (current-time-string)
	       skk-touroku-count
	       skk-kakutei-count
	       (/ (* 100 (- skk-kakutei-count skk-touroku-count))
		  skk-kakutei-count)
	       (cond
		((featurep 'skk-rdbms)
		 ;; RDBMS を使えばもっと興味深い統計が取れるかもしれない
		 ;; が、とりあえず語数だけ数えて入れておく。
		 (skk-rdbms-count-jisyo-candidates
		  skk-rdbms-private-jisyo-table))
		(skk-count-private-jisyo-candidates-exactly
		 (skk-count-jisyo-candidates
		  (expand-file-name (if (consp skk-jisyo)
					(car skk-jisyo)
				      skk-jisyo))))
		;; 1 行 1 候補とみなす。
	      (t
	       (with-current-buffer (skk-get-jisyo-buffer
				     skk-jisyo 'nomsg)
		 (- (count-lines (point-min) (point-max))
		    2))))))
      (when (integerp skk-keep-record)
	(setq selective-display nil)
	(widen)
	(goto-char (point-min))
	(forward-line skk-keep-record)
	(delete-region (point) (point-max))))
    (setq skk-touroku-count 0
	  skk-kakutei-count 0)))

(defun skk-count-jisyo-candidates (file-or-table)
  "SKK 辞書の候補数を数える。"
  (interactive
   (list (cond
	  ((eq skk-count-jisyo-candidates-function
	       'skk-count-jisyo-candidates-original)
	   (read-file-name
	    (format "Jisyo file: (default: %s) " skk-jisyo)
	    default-directory skk-jisyo 'confirm))
	  ((eq skk-count-jisyo-candidates-function
	       'skk-rdbms-count-jisyo-candidates)
	   ;; データベースファイルを直接ファイル名で指定できる
	   ;; permission がない場合が多いよね...。
	   ;;(read-file-name
	   ;; (format "Jisyo table: (default: %s) "
	   ;;	 skk-rdbms-private-jisyo-table))
	   skk-rdbms-private-jisyo-table))))
  ;; mule@emacs19.31 だと下記のようにすると (`ァ' が原因のよう) 何故か
  ;; default-directory の末尾に改行が付く。
  ;; 通常は気が付かないが、rsz-mini.el を使って resize-minibuffer-mode を
  ;; non-nil にしていると不要な 2 行目が出現する。
  ;; (interactive "f辞書ファイル: ")
  (let ((count (funcall skk-count-jisyo-candidates-function
			file-or-table)))
    (if (interactive-p)
	(message (if (= count 1)
		     "%d candidate"
		   "%d candidates")
		 count)
      count)))

(defun skk-count-jisyo-candidates-original (file)
  ;;"SKK 辞書の候補数を数える。
  ;;`[' と `]' に囲まれた送り仮名毎のブロック内は数えない。"
  (with-current-buffer (find-file-noselect file)
    (save-match-data
      (let ((count 0)
	    (min (point-min))
	    (max (and (interactive-p) (point-max)))
	    (interactive-p (interactive-p)))
	(goto-char min)
	(unless (and
		 ;; こちらは skk-save-point を使わず、ポイントを移動させる。
		 (re-search-forward "^;; okuri-ari entries.$" nil t nil)
		 (skk-save-point
		  (re-search-forward "^;; okuri-nasi entries.$" nil t nil)))
	  (skk-error "このファイルは SKK 辞書ではありません"
		     "This file is not a SKK dictionary"))
	(beginning-of-line)
	(while (looking-at ";")
	  (forward-line 1)
	  (beginning-of-line))
	(search-forward " " nil t)
	(while (search-forward "/" nil t)
	  (cond ((or (eolp)
		     (looking-at "\\["))
		 (forward-line 1)
		 (beginning-of-line)
		 (while (looking-at ";")
		   (forward-line 1)
		   (beginning-of-line))
		 (search-forward " " nil t))
		(t
		 (setq count (1+ count))))
	  (when interactive-p
	    (message "Counting jisyo candidates...%3d%% done"
		     (/ (* 100 (- (point) min)) max))))
	count))))

(defun skk-create-file (file &optional japanese english modes)
  ;; FILE がなければ、FILE という名前の空ファイルを作る。
  ;; オプショナル引数の JAPANESE/ENGLISH を指定すると、ファイル作成後そのメッセ
  ;; ージをミニバッファに表示する。
  (let ((file (expand-file-name file)))
    (if (file-exists-p file)
	(when modes
	  (set-file-modes file modes))
      (write-region 1 1 file nil 0)
      (when modes
	(set-file-modes file modes))
      (when (or japanese english)
	(message "%s"
		 (if skk-japanese-message-and-error
		     japanese
		   english))
	(sit-for 3)))))

(defun skk-get-jisyo-buffer (file &optional nomsg)
  ;; FILE を開いて SKK 辞書バッファを作り、バッファを返す。
  ;; オプショナル引数の NOMSG を指定するとファイル読み込みの
  ;; 際のメッセージを表示しない。
  (when file
    (let* ((inhibit-quit t)
	   (code (skk-find-coding-system (if (consp file)
					     (cdr file)
					   skk-jisyo-code)))
	   (file (if (consp file)
		     (car file)
		   file))
	   (enable-character-translation
	    (not (memq code '(euc-japan shift_jis junet))))
	   (enable-character-unification enable-character-translation)
	   (buf-name (concat " *"
			     (file-name-nondirectory file)
			     "*"))
	   (buf (get-buffer buf-name)))
      ;; 辞書バッファとしてオープンされているなら、何もしない。
      (unless (buffer-live-p buf)
	(setq buf (get-buffer-create buf-name))
	(setq file (expand-file-name file))
	(with-current-buffer buf
	  (buffer-disable-undo)
	  (auto-save-mode -1)
	  ;; ワーキングバッファのモードラインはアップデートされない？
	  ;;(make-local-variable 'line-number-mode)
	  ;;(make-local-variable 'column-number-mode)
	  ;;(setq column-number-mode nil
	  ;;      line-number-mode nil)
	  (setq buffer-read-only nil
		case-fold-search nil
		;; buffer-file-name を nil にしておくと M-x compile など
		;; 内部で save-some-buffers をコールしているコマンドを
		;; 使ったときでもセーブするかどうかを尋ねてこなくなる。
		;; buffer-file-name file
		;; cache-long-line-scans nil
		;; dabbrev のサーチとなるバッファにならないように存在し
		;; ないモード名にしておく。実害のある副作用はないはず。
		major-mode 'skk-jisyo-mode
		mode-name "SKK dic")
	  (unless nomsg
	    (skk-message "SKK 辞書 %s をバッファに読み込んでいます..."
			 "Inserting contents of %s ..."
			 (file-name-nondirectory file)))
	  (insert-file-contents-as-coding-system code file)
	  (unless nomsg
	    (skk-message
	     "SKK 辞書 %s をバッファに読み込んでいます...完了！"
	     "Inserting contents of %s ...done"
	     (file-name-nondirectory file)))
	  (skk-setup-jisyo-buffer)
	  (set-buffer-modified-p nil)))
      buf)))

(defun skk-search ()
  ;; skk-current-search-prog-list の要素になっているプログラムを評価して、
  ;; skk-henkan-keyをキーにして検索を行う。
  (let (l)
    (while (and (null l) skk-current-search-prog-list)
      (setq l (eval (car skk-current-search-prog-list))
	    skk-current-search-prog-list (cdr skk-current-search-prog-list)))
    l))

(defun skk-search-jisyo-file (file limit &optional nomsg)
  ;; SKK 辞書フォーマットの FILE で skk-henkan-key をキーにして検索を行う。
  ;; 検索領域が LIMIT 以下になるまでバイナリサーチを行い、その後リニ
  ;; アサーチを行う。
  ;; LIMIT が 0 であれば、リニアサーチのみを行う。
  ;; 辞書がソートされていないのであれば、LIMIT を 0 する必要がある。
  ;; オプショナル引数の NOMSG が non-nil であれば skk-get-jisyo-buffer の
  ;; メッセージを出力しないようにする。
  (skk-search-jisyo-buf (skk-get-jisyo-buffer file nomsg)
			limit))

(defun skk-search-server (file limit &optional nomsg)
  ;; SKK 辞書フォーマットの FILE で SKK サーバーを使用して
  ;; `skk-henkan-key' をキーにして検索を行う。
  ;; SKK サーバーが使用できないときは、FILE をバッファに
  ;; 読み込んでサーチを行う。
  ;; LIMIT と NOMSG は SKK サーバーを使用しないときのみ使う。
  ;; これらの引数については `skk-search-jisyo-file' の
  ;; コメントを参照。
  (if (or skk-server-host
	  skk-servers-list)
      (skk-search-server-1 file limit)
    (skk-search-jisyo-file file limit nomsg)))

(defun skk-okuri-search ()
  ;; skk-auto-okuri-process が non-nil ならば "Uresii" のように
  ;; 送り仮名も含めてタイプしても送りありの "嬉しい" を探し出す。
  (when skk-auto-okuri-process
    (skk-okuri-search-1)))

(defun skk-search-jisyo-buf (buf limit)
  ;; バッファを BUF に移動して、そこを辞書として検索する。
  (when (buffer-live-p buf)
    ;; skk-henkan-key と skk-henkan-okurigana はカレントバッファの
    ;; ローカル値なので、あらかじめ取得。
    (let ((okurigana (or skk-henkan-okurigana
			 skk-okuri-char))
	  (midasi (if skk-use-numeric-conversion
		      (skk-num-compute-henkan-key skk-henkan-key)
		    skk-henkan-key))
	  (henkan-buffer (current-buffer))
	  words-list)
      (with-current-buffer buf
	(setq skk-henkan-key midasi
	      words-list (skk-search-jisyo okurigana limit))
	(skk-select-words-from-list words-list
				    henkan-buffer
				    midasi
				    okurigana)))))

(defun skk-search-jisyo (okurigana limit &optional delete)
  ;; カレントバッファを辞書として検索する。
  ;; `skk-compute-henkan-lists' を使用し、見出し語についての候補の情報を
  ;; 返す。DELETE が non-nil であれば、MIDASI にマッチするエントリを削除
  ;; する。
  (let ((key (concat "\n" skk-henkan-key " /"))
	min max size p)
    (save-match-data
      ;; skk-okuri-ari-min と skk-okuri-ari-max は辞書バッファのローカル値。
      (if okurigana
	  (setq min skk-okuri-ari-min
		max skk-okuri-ari-max)
	(setq min skk-okuri-nasi-min
	      max (point-max)))
      (when (> limit 0)
	(while (progn
		 (setq size (- max min))
		 (> size limit))
	  (goto-char (+ min (/ size 2)))
	  (beginning-of-line)
	  (setq p (point))
	  ;; 送りありなら逆順に比較を行なう。
	  (let ((p-is-further
		 (if okurigana
		     (string< (buffer-substring-no-properties
			       p (1- (search-forward  " ")))
			      skk-henkan-key)
		   (string< skk-henkan-key
			    (buffer-substring-no-properties
			     p (1- (search-forward " ")))))))
	    (if p-is-further
		(setq max p)
	      (setq min p)))))
      (goto-char min)
      ;; key が検索開始地点にあった場合でも検索可能なように一文字戻る。key が
      ;; その先頭部分に "\n" を含んでいることに注意。
      (unless (bobp)
	(backward-char 1))
      ;; case-fold-search は、辞書バッファでは常に nil。
      (when (search-forward key max 'noerror)
	(prog1
	    (skk-compute-henkan-lists okurigana)
	  (when delete
	    (beginning-of-line)
	    (delete-region (point)
			   (progn
			     (forward-line 1)
			     (point)))))))))

(defun skk-select-words-from-list (list buffer midasi okurigana)
  ;; `skk-search-jisyo' が返した候補リストから、現在要求されている
  ;; 候補を選びだす。
  (when list
    (let ((words
	   (cond
	    ((and okurigana
		  skk-henkan-okuri-strictly)
	     ;; 送り仮名が同一の候補のみを返す。
	     (nth 2 list))
	    ((and okurigana
		  skk-henkan-strict-okuri-precedence)
	     ;; 送り仮名が同一の候補のうしろに、
	     ;; その他の候補をつけてかえす。
	     (skk-nunion (nth 2 list)
			 (car list)))
	    (t
	     (car list)))))
      (when skk-search-end-function
	(setq words (funcall skk-search-end-function
			     buffer
			     midasi
			     okurigana
			     words)))
      words)))

(defun skk-compute-henkan-lists (okurigana)
  ;; 辞書候補群を 4 つのリストに分解する。
  ;;
  ;; 送りなし (例えば、辞書エントリ "てんさい /転載/天災/天才/" の処理)
  ;; words1 := ("転載" "天災" "天才") == 全候補群
  ;; words2 := nil
  ;; words3 := nil
  ;; words4 := nil
  ;;
  ;; 送りあり (例えば、「泣く」の変換を行った場合の、辞書エントリ
  ;;           "なk /亡/無/鳴/泣/[く/無/鳴/泣/]/[き/亡/]/" の処理)
  ;; words1 := ("亡" "無" "鳴" "泣")  == 漢字部分の全候補群
  ;; words2 := ("[く")                == 他の送り仮名を使う漢字候補群 (あれ
  ;;                                     ば) + 今回の変換の送り仮名部分
  ;; words3 := ("無" "鳴" "泣")       == 今回の変換の送り仮名を使う可能性の
  ;;                                     ある全漢字候補群
  ;; words4 := ("]" "[き" "亡" "]")   == 他の送り仮名を使う漢字候補群 (残
  ;;                                     り。あれば)
  ;;
  ;;   * "[" は直後に続くひらがなを送り仮名に持つ漢字の候補群の初まりを表し、
  ;;     "]" は、該当の送り仮名グループの終りを示す。
  ;;
  ;; この関数は、変換時と、確定直後の辞書のアップデート時の 2 度呼ばれる
  ;; (変換時に検索を行った辞書が、skk-jisyo とは限らないので、2 度計算せざる
  ;; を得ない)。
  ;;
  ;; 変換時は、skk-henkan-okuri-strictly が non-nil であれば、
  ;; 計算結果の words3を、skk-henkan-okuri-strictly が nil であって
  ;; かつ skk-henkan-strict-okuri-precedence が non-nil あれば
  ;; (skk-nunion words3 words1) を取り出す。
  ;; ふたつの変数がともに nil の場合は words1 を取り出す。
  (cond
   ((not okurigana)
    (list (split-string (buffer-substring-no-properties
			 (point) (progn
				   (end-of-line)
				   (1- (point))))
			"/")
	  nil nil nil))
   (t
    (save-match-data
      (let ((stage 1)
	    (q1 (queue-create))
	    (q2 (queue-create))
	    (q3 (queue-create))
	    (q4 (queue-create))
	    (okuri-key (concat "\[" okurigana))
	    item
	    headchar)
	(while (not (eolp))
	  (setq item (buffer-substring-no-properties
		      (point)
		      (1- (search-forward "/")))
		headchar (if (string= item "")
			     (int-char 0)
			   (skk-str-ref item 0)))
	  (cond
	   ((and (eq headchar ?\[)
		 (<= stage 2))
	    (setq item (skk-compute-henkan-lists-sub-adjust-okuri
			item
			okuri-key))
	    (if (string= item okuri-key)
		(progn
		  (queue-enqueue q2 item)
		  (setq stage 3))
	      (setq stage 2)
	      (queue-enqueue q2 item)))
	   ((= stage 1)
	    (queue-enqueue q1 item))
	   ((= stage 2)
	    (queue-enqueue q2 item))
	   ((= stage 3)
	    (if (eq headchar ?\]) ; ?\]
		(progn
		  (setq stage 4)
		  (queue-enqueue q4 item))
	      (queue-enqueue q3 item)))
	   ((= stage 4)
	    (queue-enqueue q4 item))))
	;;
	(list (queue-all q1)       ; words1
	      (queue-all q2)       ; words2
	      (queue-all q3)       ; words3
	      (queue-all q4))))))) ; words4

(defun skk-compute-henkan-lists-sub-adjust-okuri (item &optional okuri-key)
  ;; Yet to be elucidated.
  item)

(defun skk-nunion (x y)
  ;; X と Y の和集合を作る。等しいかどうかの比較は、`equal' で行われる。
  ;; X に Y を破壊的に連接する。
  (cond
   ((null x)
    y)
   ((null y)
    x)
   (t
    (save-match-data
      (let ((list2 y) list1 origlist1 e1 e2)
	(while list2
	  (setq list1 (cons nil x)
		e2 (car list2)
		origlist1 list1)
	  (catch 'found
	    (while (setq e1 (car (cdr list1)))
	      (cond
	       ((equal e1 e2)
		(throw 'found nil))
	       ((and (stringp e1)
		     (stringp e2)
		     (string-match ";" e1))
		(setq e1 (substring e1 0 (match-beginning 0)))
		(when (or (equal e1 e2)
			  (and
			   (string-match ";" e2)
			   (equal (substring e2 0 (match-beginning 0))
				  e1)))
		  (throw 'found nil))))
	      (setq list1 (cdr list1)))
	    (setcdr list1 (list e2))
	    (setq x (cdr origlist1)))
	  (setq list2 (cdr list2)))
	x)))))

(defun skk-search-kakutei-jisyo-file (file limit &optional nomsg)
  ;; 辞書ファイルを探し、候補をリストで返す。
  ;; 候補を見つけた場合は、大域変数 skk-kakutei-flag に non-nil を代入する。
  ;; 候補が見つからなかった場合は、nil を返す。
  (setq skk-kakutei-flag (skk-search-jisyo-file file limit nomsg)))

(defun skk-update-jisyo (word &optional purge)
  (funcall skk-update-jisyo-function word purge))

(defun skk-update-jisyo-original (word &optional purge)
  ;; WORD が次の変換時に最初の候補になるように、プライベート辞書を更新する。
  ;; PURGE が non-nil で WORD が共有辞書にある候補なら skk-ignore-dic-word
  ;; 関数でクォートした候補をプライベート辞書に作り、次の変換から出力しな
  ;; いようにする。
  ;; WORD が共有辞書になければ、プライベート辞書の辞書エントリから削除する。
  ;;
  ;; SKK 9.x より、プライベート辞書のエントリの挿入の方法を変更した (9.3 のみ
  ;; は例外)。
  ;;
  ;; 【変更前】
  ;;         ;; okuri-ari entries.
  ;;  見キ   わるk /悪/[か/悪/]/[く/悪/]/
  ;;  出ー   わるi /悪/[い/悪/]/
  ;;  しに   わたs /渡/[さ/渡/]/[せ/渡/]/
  ;;  語降   わすr /忘/[れ/忘/]/
  ;;  を順   わかt /分/判/[った/分/判/]/[って/分/]/
  ;;   ↓     .....
  ;;         あi /合/[い/合/]/
  ;;         ;; okuri-nasi entries.
  ;;  変で   じょうたい /状態/
  ;;  換昇   そうにゅう /挿入/
  ;;  順順   かな /仮名/
  ;;   ↓    ...
  ;;         ...
  ;;
  ;; 【変更後】
  ;;         ;; okuri-ari entries.
  ;;  変で   でt /出/[て/出/]/[た/出/]/
  ;;  換昇   つi /付/[い/付/]/
  ;;  順順   けs /消/[す/消/]/[し/消/]/[せ/消/]/[さ/消/]/
  ;;   ↓    かえs /返/[し/返/]/[す/返/]/[さ/返/]/[せ/返/]/
  ;;         ...
  ;;         ...
  ;;         ながs /長/流/[し/流/]/[さ/長/]/[そ/流/]/
  ;;         ;; okuri-nasi entries.
  ;;  変で   じょうたい /状態/
  ;;  換昇   そうにゅう /挿入/
  ;;  順順   かな /仮名/
  ;;   ↓    ...
  ;;         ...
  ;;
  ;; skk-auto-okuri-process が non-nil のときに、(j-okuri-search 改め)
  ;; skk-okuri-search は見出し語の長い順に候補を返す必要がある。
  ;; SKK 8.6 までは、skk-okuri-search が j-okuri-ari-min から j-okuri-ari-max
  ;; までを順に探し、見つけたもの順に候補を返すためにプライベート辞書が見出し
  ;; 語をキーとして降順にソートされている必要があった。
  ;; SKK 9.x では、skk-okuri-search が、見付けた候補を見出し語をキーとして昇順
  ;; にソートして返すため、プライベート辞書のソートは必要でない。よって、最後
  ;; に変換したものを (j-okuri-ari-min 改め) skk-okuri-ari-min の位置に挿入す
  ;; る。
  ;;
  (let ((jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
	(midasi (if skk-use-numeric-conversion
		    (skk-num-compute-henkan-key
		     skk-henkan-key)
		  skk-henkan-key))
	(henkan-buffer (and skk-update-end-function
			    (current-buffer))))
    ;; 入力履歴を更新する。
    ;; 送りあり入力は省略し、送りなし入力のみ履歴をとる。
    (unless skk-henkan-okurigana
      (skk-update-kakutei-history midasi word))
    (when jisyo-buffer
      (let ((inhibit-quit t)
	    buffer-read-only
	    old-words-list
	    okurigana)
	(when (> skk-okuri-index-min -1)
	  (setq word (skk-remove-common word)
		;; skk-henkan-key は skk-remove-common によって
		;; 変更されている可能性がある。
		midasi skk-henkan-key))
	(setq okurigana (or skk-henkan-okurigana
			    skk-okuri-char))
	(with-current-buffer jisyo-buffer
	  ;; 既存エントリを検索後消去する。挿入すべき候補が words1 に 1 つ
	  ;; しかなく、word と同じ文字であっても、いったん消してそのエント
	  ;; リを min ポイントに移動させなければならない。これは、読みの補
	  ;; 完を行うときに、 min ポイントから見出しを探すため、新しい見出
	  ;; しほど、min ポイントに近いところになければならないからである。
	  (setq skk-henkan-key midasi
		old-words-list (skk-search-jisyo okurigana 0 'delete))
	  (skk-update-jisyo-1 okurigana
			      word
			      old-words-list
			      purge)
	  ;; 複数の emacs で SKK が起動されているときに個人辞書を整合的に
	  ;; 更新するために確定の動作を記録する。
	  (when skk-share-private-jisyo
	    (aset skk-jisyo-update-vector skk-update-jisyo-count
		  (list midasi okurigana word purge)))
	  (when skk-update-end-function
	    (funcall skk-update-end-function
		     henkan-buffer midasi okurigana word purge))
	  (setq skk-update-jisyo-count (1+ skk-update-jisyo-count))
	  (when (and skk-jisyo-save-count
		     (= skk-jisyo-save-count skk-update-jisyo-count))
	    ;; auto save.
	    (skk-save-jisyo 'quiet)))))))

(defun skk-update-jisyo-1 (okurigana word old-words-list purge)
  ;; 既存エントリから計算した words[1-4] の値と、今回の変換の結果 word とを
  ;; 結合して、新たなエントリを計算し、挿入する。
  (let ((words1 (car old-words-list))
	(words2 (nth 1 old-words-list))
	(words3 (nth 2 old-words-list))
	(words4 (nth 3 old-words-list)))
    (cond
     ((not purge)
      ;; words1 の先頭の候補を word にする。
      (setq words1 (cons word (delete word words1))))
     ;; 送りなし、もしくは skk-henkan-okuri-strictly と
     ;; skk-henkan-strict-okuri-precedence が nil の場合。
     ((not (and okurigana
		(or skk-henkan-okuri-strictly
		    skk-henkan-strict-okuri-precedence)))
      ;; words1 を purge。共用辞書にある候補だったら、
      ;; skk-ignore-dic-word でクォートして次の変換から出力
      ;; しないようにする。共用辞書にない文字列は word を消す。
      (setq words1
	    (if (skk-public-jisyo-has-word-p okurigana word)
		(skk-compose-ignore-word words1 word)
	      (delete word words1))))
      ((and okurigana
	    (or skk-henkan-okuri-strictly
		skk-henkan-strict-okuri-precedence)
	    (null (member word words2))
	    (null (member word words4)))
       ;; 送りありで、かつ skk-henkan-okuri-strictly か
       ;; skk-henkan-strict-okuri-precedence が non-nil
       ;; の場合で、かつこの word とペアになる送り仮名が
       ;; okurigana しかないとき。
       (setq words1 (delete word words1)))
      (t
       ;; その他の場合は何もしない。
       nil))
    (when words1 ;; words1 が null であれば、もう何もすることはない。
      (goto-char (if okurigana
		     skk-okuri-ari-min
		   skk-okuri-nasi-min))
      (insert "\n" skk-henkan-key " /")
      ;; words1 -- 全候補群 (送りなしの場合) 、または
      ;;           全候補群の漢字部分 (送りありの場合)
      (insert (mapconcat 'skk-quote-char
			 words1
			 "/")
	      "/")
      (when okurigana
	;; words2 以降の候補群を処理するのは、送りありの場合のみ。
	;; 先に挿入すべき候補群を計算、調整する。
	(cond
	 (words3
	  (cond
	   ((not purge)
	    (setq words3 (cons word (delete word words3))))
	   (t
	    (setq words3 (delete word words3))
	    (when (null words3)
	      ;; words3 として挿入するものが全くなければ、"/[く/]/" のよ
	      ;; うな送り仮名のみの候補を作らないようにする (必要で
	      ;; あれば、words2 の最後方と) words4 の先頭の "]" を削除。
	      (let ((last2 (nthcdr (- (length words2) 2)
				   words2)))
		;; words2 の最後方は常に "[送り仮名" とは限らない。
		(when (string= (nth 1 last2)
			       (concat "[" okurigana))
		  (setcdr last2 nil))
		;; words4 の先頭は常に "]"。
		(setq words4 (cdr words4)))))))
	 (t
	  ;; words3 が null であれば
	  (unless (or skk-process-okuri-early
		      purge)
	    ;; skk-process-okuri-early が non-nil なら送り仮名が分らないので
	    ;; 何もしない。-- 今回使用した送り仮名がわからないまま変換してい
	    ;; るので、全ての候補が words2 に入っている -- words3, words4 は
	    ;; null。
	    ;; words3 として挿入するものが全くなければ、何もしない -- words3
	    ;; が purge 前から null なら、words2 の末尾は "[" でないし、
	    ;; words4 は null だから words[234] の操作は不要。
	    (setq words2 (nconc words2
				(list (concat "[" okurigana)))
		  words3 (list word)
		  ;; purge 前から words3 が null だったのだから
		  ;; words4 も null。
		  words4 (list "]"))))))
      (when words2
	;; words2 -- 今回使用しなかった送り仮名を使う漢字の候補群
	;;         + "["
	;;         + 今回使用した送り仮名 (送り仮名のみ。その送り
	;;           仮名を使用する漢字の候補群は、words3 に含まれる)
	(insert (mapconcat 'skk-quote-char
			   words2
			   "/")
		"/")
	;; words2 が null なら words3 も null。
	(when words3
	  ;; words3 -- 今回使用した送り仮名を使う全漢字候補
	  (insert (mapconcat 'skk-quote-char
			     words3
			     "/")
		  "/"))
	;; purge で words3 が null になった場合は words4 が残っている
	;; ときがある。
	(when words4
	  ;; words4 -- "]" + 他の送り仮名を使う全漢字候補
	  ;; (words2 の残り)。
	  (insert (mapconcat 'skk-quote-char
			     words4
			     "/")
		  "/"))))))

(defun skk-quote-char (word)
  ;; 辞書の制限から辞書エントリ内に含めてはならない文字が WORD の中にあれば、
  ;; 評価したときにその文字となるような Lisp コードを返す。
  (save-match-data
    (cond
     ((and word
	   (string-match "[/\n\r\"]" word)
	   ;; we should not quote WORD if it is a symbolic expression
	   (not (skk-lisp-prog-p word))
	   ;; has an annotation
	   (not (string-match ";" word)))
      (format "(concat \"%s\")"
	      (skk-quote-char-1 word (cdr skk-quote-char-alist))))
     (t
       word))))

(defun skk-quote-semicolon (word)
  ;; `save-match-data' は要らない。
  (cond
   ((string-match ";" word)
    (format "(concat \"%s\")"
	    (skk-quote-char-1 word skk-quote-char-alist)))
   (t
    word)))

(defun skk-public-jisyo-has-word-p (okurigana word)
  ;; 共有辞書が MIDASHI 及びそれに対応する 候補 WORD を持っていれば、
  ;; non-nil を返す。個人辞書のバッファで実行される。
  (let (fn
	skk-henkan-okuri-strictly
	skk-henkan-strict-okuri-precedence)
    (when okurigana
      (setq skk-henkan-okurigana okurigana))
    ;; skkserv を使う設定になっていたら、skk-server.el をロードする。
    (when (or skk-servers-list
	      skk-server-host
	      (getenv "SKKSERVER"))
      (require 'skk-server))
    (setq fn (funcall skk-public-jisyo-to-be-searched-function))
    ;;
    (and fn
	 (member word (eval fn)))))

(defun skk-public-jisyo-to-be-searched-original ()
  ;; skk-search-prog-list の中から、一番大きな共有辞書でサーチするプロ
  ;; グラムを返す。
  (let (fn)
    (when (and (featurep 'skk-server)
	       (or skk-servers-list
		   skk-server-host))
      (setq fn (assq 'skk-search-server skk-search-prog-list)))
    ;; skk-search-server から始まるリストがなければ、とにかく大きい辞書を引数
    ;; にしている skk-search-jisyo-file プログラムを探す。
    (when (and (not fn)
	       (or skk-aux-large-jisyo
		   skk-large-jisyo))
      (let ((spl skk-search-prog-list)
	    cell)
	(while (setq cell (car spl))
	  (if (and (eq (car cell) 'skk-search-jisyo-file)
		   (memq (nth 1 cell) '(skk-aux-large-jisyo skk-large-jisyo)))
	      (setq fn cell
		    spl nil)
	    (setq spl (cdr spl))))))
    fn))

(defun skk-compose-ignore-word (words &optional add)
  ;; WORDS の中に skk-ignore-dic-word 関数でクォートした候補が
  ;; あれば、一つの候補にまとめる。
  ;; オプショナル引数の ADD が指定されていたら、ADD を含めた
  ;; skk-ignore-dic-word 候補群を作る。
  ;; 新しい skk-ignore-dic-word 候補を car に、それ以外の候補を
  ;; cdr にしたリストを返す。
  (let (l arg e)
    (when add
      (setq words (delete add words)))
    (setq l words)
    (save-match-data
      (while l
	(setq e (car l)
	      l (cdr l))
	(when (string-match "(skk-ignore-dic-word +\\([^\)]+\\))"
			    e)
	     (setq arg (concat arg
			       (substring e
					  (1+ (match-beginning 1))
					  (1- (match-end 1)))
			       "\" \"")
		   words (delq e words))))
      (setq arg (cond
		 ((not add)
		  ;; 末尾の " \"" を切り落とす。
		  (substring arg 0 -2))
		 (arg
		  (concat arg
			  (skk-compose-ignore-word-sub-quote-char
			   add)))
		 (t
		  add)))
      (cons (format "(skk-ignore-dic-word \"%s\")"
		    (if (equal arg add)
			(skk-compose-ignore-word-sub-quote-char arg)
		      arg))
	    words))))

(defun skk-compose-ignore-word-sub-quote-char (str)
  (cond
   ((string-match "[/\n\r\";]" str)
    (let ((alist (if (string-match ";" str)
		     skk-quote-char-alist
		   (cdr skk-quote-char-alist))))
      (skk-quote-char-1 str alist)))
   (t
    str)))

(defun skk-search-katakana (&optional jisx0201-kana)
  ;; これは `skk-search-prog-list' に追加されるべき機能で、変換キーを単純にカ
  ;; タカナに変換したものを候補として返す。
  ;; 一般的な FEP は単純にカタカナに変換したものが候補に現れるものが多いが、
  ;; そのような挙動が好みの場合にはこの関数を用いるとよい。
  (unless skk-henkan-okurigana
    (let ((key skk-henkan-key)
	  char
	  words)
      (with-temp-buffer
	(insert key)
	(goto-char (point-min))
	(while (and
		(not (eobp))
		(or
		 ;; "ー" では文字種別が判別できないので、ポイントを進める。
		 (looking-at "ー")
		 (eq 'unknown (setq char (skk-what-char-type)))))
	  (forward-char 1))
	(when (eq char 'hiragana)
	  (skk-katakana-region (point-min) (point-max) t)
	  (setq words (list (buffer-string))))
	(when (and jisx0201-kana
		   (or (eq char 'hiragana)
		       (string-match "ー" key)))
	  (skk-katakana-to-jisx0201-region (point-min) (point-max))
	  (setq words (nconc words (list (buffer-string))))))
      words)))

(defun skk-search-sagyo-henkaku (&optional okuri-list anything)
  (unless okuri-list
    (setq okuri-list '("さ" "し" "す" "せ")))
  (when (and skk-henkan-okurigana
	     (or (member skk-henkan-okurigana okuri-list)
		 anything))
    (let ((skk-henkan-key (skk-substring
			   skk-henkan-key
			   0 (1- (skk-str-length skk-henkan-key))))
	  skk-henkan-okurigana
	  skk-okuri-char
	  skk-auto-okuri-process
	  words)
      (ignore-errors
	(dolist (form skk-search-prog-list)
	  (setq words (skk-nunion words (eval form)))))
      words)))

(defun skk-katakana-region (start end &optional vcontract)
  "領域のひらがなをカタカナに変換する。
オプショナル引数の VCONTRACT が non-nil であれば、\"う゛\" を \"ヴ\" に変換す
る。
引数の START と END は数字でもマーカーでも良い。"
  (interactive "*r\nP")
  (when vcontract
    (skk-search-and-replace
     start end "う゛" (lambda (matched) nil "ヴ")))
  (skk-search-and-replace
   start end "[ぁ-ん]+"
   (lambda (matched) (skk-hiragana-to-katakana matched))))

(defun skk-hiragana-region (start end &optional vexpand)
  "領域のカタカナをひらがなに変換する。
オプショナル引数の VEXPAND が non-nil であれば、\"ヴ\" を \"う゛\" に変換する。
引数の START と END は数字でもマーカーでも良い。
\"ヵ\" と \"ヶ\" は変更されない。この 2 つの文字は対応するひらがながないので、
カタカナとしては扱われない。"
  (interactive "*r\nP")
  (when vexpand
    (skk-search-and-replace
     start end "ヴ" (lambda (matched) nil "う゛")))
  (skk-search-and-replace
   start end "[ァ-ン]+"
   (lambda (matched)
     (skk-katakana-to-hiragana matched))))

(defun skk-jisx0208-latin-region (start end)
  "領域の ascii 文字を対応する全角英文字に変換する。"
  (interactive "*r")
  (skk-search-and-replace
   start end "[ -~]"
   (lambda (matched)
     (aref skk-default-jisx0208-latin-vector (string-to-char matched)))))

(defun skk-latin-region (start end)
  ;; 領域の全角英数字を対応する ascii 文字に変換する。
  (interactive "*r")
  (skk-search-and-replace
   start end "\\cj"
   (lambda (matched)
     (or (skk-jisx0208-to-ascii matched)
	 matched))))

(defun skk-search-and-replace (start end regexp func)
  (let (matched replace)
    (save-match-data
      (skk-save-point
       ;; END may be changed when length of MATCHED and one of REPLACE
       ;; are different.
       (setq end (set-marker (make-marker) end))
       (goto-char start)
       (while (re-search-forward regexp end 'noerror)
	 (setq matched (buffer-substring-no-properties
			(match-beginning 0) (match-end 0))
	       replace (funcall func matched))
	 (goto-char (match-beginning 0))
	 ;; firstly insert a new string, secondly delete an old string to save
	 ;; the cursor position.
	 (insert-and-inherit replace)
	 (delete-region (+ (match-beginning 0) (length replace))
			(+ (match-end 0) (length replace))))
       (set-marker end nil)))))

(defun skk-jisx0208-to-ascii (string)
  (let ((char
	 (static-cond
	  ((eq skk-emacs-type 'mule2)
	   (let* ((ch (string-to-char string))
		  (ch1 (char-component ch 1)))
	     (cond ((eq ch1 ?\241)
		    (cdr (assq (char-component ch 2)
			       skk-hankaku-alist)))
		   ((eq ch1 ?\243)
		    (- (char-component ch 2) ?\200)))))
	  (t
	   (require 'japan-util)
	   (get-char-code-property (string-to-char string)
				   'ascii)))))
    ;;
    (if char
	(char-to-string char)
      nil)))

(defun skk-katakana-henkan (arg)
  "▽モードであれば、領域のひらがなをカタカナに変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (skk-*-henkan-2 'skk-katakana-region 'vcontract))

(defun skk-hiragana-henkan (arg)
  "▽モードであれば、領域のカタカナをひらがなに変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (skk-*-henkan-2 'skk-hiragana-region 'vexpand))

(defun skk-jisx0208-latin-henkan (arg)
  "▽モードであれば、ascii 文字を対応する全角英文字に変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (skk-*-henkan-2 'skk-jisx0208-latin-region))

(defun skk-latin-henkan (arg)
  "▽モードであれば、ascii 文字を対応する全角文字に変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (skk-*-henkan-2 'skk-latin-region))

(defun skk-*-henkan-1 (func &rest args)
  ;; 変換可能かどうかのチェックをした後に ARGS を引数として FUNC を適用し、
  ;; skk-henkan-start-point と skk-henkan-end-point の間の文字列を変換する。

  (when (skk-get-prefix skk-current-rule-tree)
    (skk-error "入力途中の仮名ブレフィックスがあります"
	       "There remains a kana prefix"))

  (when (< (point) skk-henkan-start-point)
    (skk-error "カーソルが変換開始地点より前にあります"
	       "Henkan end point must be after henkan start point"))

  (when (and (not skk-allow-spaces-newlines-and-tabs)
	     (skk-save-point
	      (beginning-of-line)
	      (> (point) skk-henkan-start-point)))
    (skk-error "変換キーに改行が含まれています"
	       "Henkan key may not contain a line feed"))

  (apply func args)
  (skk-kakutei))

(defun skk-*-henkan-2 (func &optional arg)
  (skk-with-point-move
   (cond
    ((eq skk-henkan-mode 'active)
     nil)
    ((eq skk-henkan-mode 'on)
     (skk-set-marker skk-henkan-end-point (point))
     (apply 'skk-*-henkan-1
	    func
	    skk-henkan-start-point
	    skk-henkan-end-point
	    (when arg
	      (list arg))))
    (t
     (skk-emulate-original-map arg)))))

(defun skk-hiragana-to-katakana (hiragana)
  (let ((diff (- ?ア ?あ)))
    (mapconcat
     (function (lambda (e)
		 (if (and (<= ?ぁ e) (>= ?ん e))
		     (char-to-string (+ e diff))
		   (char-to-string e))))
     (string-to-int-list hiragana) "")))

(defun skk-katakana-to-hiragana (katakana)
  (let ((diff (- ?ア ?あ)))
    (mapconcat
     (function (lambda (e)
		 (if (and (<= ?ァ e) (>= ?ン e))
		     (char-to-string (- e diff))
		   (char-to-string e))))
     (string-to-int-list katakana) "")))

(defun skk-splice-in (org offset spliced)
  ;; ORG := '(A B C), SPLICED := '(X Y), OFFSET := 1
  ;; -> '(A B X Y C)
  (let (tmp tail)
    (unless (> offset 0)
      (error "%s" "Cannot splice in!"))
    (setq tmp (nthcdr (1- offset) org)
	  tail (cdr tmp))
    (setcdr tmp nil) ;cut off
    (setcdr tmp (if tail
		    (nconc spliced tail)
		  spliced))
    org))

;; (defun skk-chomp (nth list)
;;   ;; LIST := '(A B C D), NTH := 1
;;   ;; -> '(A B)
;;   (and (> nth -1) (setcdr (nthcdr nth list) nil))
;;   list)

(defun skk-henkan-face-on ()
  ;; skk-use-face が non-nil の場合、skk-henkan-start-point と
  ;; skk-henkan-end-point の間の face 属性を skk-henkan-face の値に変更する。
  ;;
  ;; SKK 9.4 より Text Properties を使用するのを止めて、Overlays を使用するよ
  ;; うにした (egg.el, canna.el, wnn-egg.el を参考にした)。
  ;; Overlays は、テキストの一部ではないので、バッファから文字を切り出してもコ
  ;; ピーの対象にならないし、アンドゥ時も無視されるので、変換された候補の表示
  ;; を一時的に変更するには Text Properties よりも好都合である。
  (when (and skk-henkan-face
	     (marker-position skk-henkan-start-point)
	     (marker-position skk-henkan-end-point))
    (skk-face-on skk-henkan-overlay
		 skk-henkan-start-point skk-henkan-end-point
		 skk-henkan-face skk-henkan-overlay-priority)))

(defun skk-henkan-face-off ()
  ;; skk-henkan-start-point と skk-henkan-end-point の間の表示を変更している
  ;; skk-henkan-overlay を消す。
  (when skk-henkan-face
    (skk-detach-extent skk-henkan-overlay)))

(defun skk-detach-extent (object)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (when (extentp object)
      (detach-extent object)))
   (t
    (when (overlayp object)
      (delete-overlay object)))))

(defun skk-make-face (face)
  ;; hilit-lookup-face-create のサブセット。tutorial で色付けを行なう場合でも
  ;; hilit19 に依存せずとりあえず face を自前で作ることができるように、という
  ;; 目的で作ったもので、簡単な色付けしかできない。あまり賢くはない。複雑な
  ;; face を作りたい人は hilit-lookup-face-create 等を使って下さい。
  (unless (car (memq face (face-list)))
    (let ((face-name (symbol-name face)))
      (setq face (make-face face))
      (save-match-data
	(if (not (string-match "/" face-name))
	    (set-face-foreground face face-name)
	  (set-face-foreground
	   face
	   (substring face-name 0 (match-beginning 0)))
	  (set-face-background
	   face
	   (substring face-name (1+ (match-beginning 0)))))
	face))))

;; skk-auto.el, skk-rdbms.el の両方で使うので、skk-auto.el より移動した。
(defun skk-remove-common (word)
  ;; skk-henkan-key と word の間に共通の送り仮名を取り除き、送り仮名以外の部分
  ;; の文字列を返す。skk-henkan-key と skk-henkan-okurigana の値をセットする。
  ;; 例えば、word == 持ってきた であれば、skk-henkan-key := "もt",
  ;; skk-henkan-okurigana := "って", word := "持" のように分解し、word を返す。
  ;; skk-auto-okuri-process の値が non-nil であるときにこの関数を使用する。
  ;; 変換が行なわれたバッファでコールされる (辞書バッファではない)。
  (when (and (not (skk-numeric-p))
	     (not skk-abbrev-mode)
	     (or skk-henkan-in-minibuff-flag
		 (and (<= skk-okuri-index-min
			  skk-henkan-count)
		      (<= skk-henkan-count
			  skk-okuri-index-max))))
    (let ((midasi skk-henkan-key)
	  (midasi-len (skk-str-length skk-henkan-key))
	  (word-len (skk-str-length word))
	  (cont t)
	  char pos pos2
	  midasi-tail word-tail new-word okuri-first
	  new-skk-okuri-char new-skk-henkan-key)
      (when (and (>= midasi-len 2) (>= word-len 2))
	;; check if both midasi and word end with the same ascii char.
	(when (and (skk-ascii-char-p (skk-str-ref midasi
						  (1- midasi-len)))
		   (eq (skk-str-ref midasi (1- midasi-len))
		       (skk-str-ref word (1- word-len))))
	  ;; if so chop off the char from midasi and word.
	  ;; assume size of an ASCII char is always 1.
	  (setq midasi (substring midasi 0 -1)
		midasi-len (1- midasi-len)
		word (substring word 0 -1)
		word-len (1- word-len)))
	(setq midasi-tail (skk-substring midasi (1- midasi-len)
					 midasi-len)
	      word-tail (skk-substring word (1- word-len)
				       word-len))
	(when (and (string= midasi-tail word-tail)
		   (or (and (skk-string<= "ぁ" midasi-tail)
			    (skk-string<= midasi-tail "ん"))
		       (member midasi-tail '("、" "。" "，" "．"))))
	  ;; 見出し語と単語との末尾が同一のかな文字の場合。
	  ;; 送りなしを送りありへ
	  (setq pos (1- word-len)
		new-word new-skk-henkan-key)
	  (while (and cont (> pos 0))
	    (setq char (skk-substring word (1- pos) pos))
	    (if (and (skk-string<= "亜" char)
		     (skk-string<= char "瑤"))
		;; char is the right-most Kanji
		(setq cont nil)
	      (setq pos (1- pos))))
	  (setq pos2 (- midasi-len (- word-len pos)))
	  ;; check if midasi and word has the same tail of length
	  (when (string= (skk-substring midasi pos2 midasi-len)
			 (skk-substring word pos word-len))
	    (setq okuri-first (skk-substring word pos (1+ pos)))
	    (setq skk-henkan-okurigana
		  (if (and (string= okuri-first "っ")
			   (<= (+ pos 2) word-len))
		      ;; in this case okuriga consits of two
		      ;; characters, e.g., 「残った」
		      (skk-substring word pos (+ pos 2))
		    okuri-first))
	    (setq new-word (skk-substring word 0 pos)
		  new-skk-okuri-char (skk-okurigana-prefix
				      skk-henkan-okurigana)
		  new-skk-henkan-key (concat
				      (skk-substring midasi 0 pos2)
				      new-skk-okuri-char))
	    (let (inhibit-quit)	; allow keyboard quit
	      (cond
	       ((not skk-henkan-in-minibuff-flag)
		(setq word new-word
		      skk-henkan-key new-skk-henkan-key))
	       ;; 辞書登録モードで登録された場合。
	       ;; ask if register as okuri-ari word.
	       ((y-or-n-p
		 (format
		  (if skk-japanese-message-and-error
		      "%s /%s/ を送りあり候補として登録しますか？"
		    "Shall I register this as okuri-ari word: %s /%s/ ? ")
		  new-skk-henkan-key new-word))
		(setq word new-word
		      skk-okuri-char new-skk-okuri-char
		      skk-henkan-key new-skk-henkan-key))
	       (t
		(setq skk-henkan-okurigana nil
		      skk-okuri-char nil)
		(message "")))))))))
  ;; 分解した word (送り仮名部分を除いたもの) を返す。
  word)

(defun skk-okurigana-prefix (okurigana)
  (let ((headchar (skk-substring okurigana 0 1)))
    (cond ((string= headchar "ん")
	   "n")
	  ((not (and (skk-string<= "ぁ" headchar)
		     (skk-string<= headchar "ん")))
	   nil)
	  ((and (string= headchar "っ")
		(not (string= okurigana "っ")))
	   (aref skk-kana-rom-vector
		 ;; assume the character is hiragana of JIS X 0208.
		 (- (skk-char-octet
		     (string-to-char (skk-substring okurigana
						    1 2))
		     1)
		    33)))
	  (t
	   (aref skk-kana-rom-vector
		 (- (skk-char-octet (string-to-char headchar)
				    1)
		    33))))))

(defun skk-time-difference (a b)
  ;; from type-break.el.  Welcome!
  ;; Compute the difference, in seconds, between a and b, two structures
  ;; similar to those returned by `current-time'.
  ;; Use addition rather than logand since that is more robust; the low 16
  ;; bits of the seconds might have been incremented, making it more than 16
  ;; bits wide.
  (+ (lsh (- (car b) (car a)) 16)
     (- (nth 1 b) (nth 1 a))))

(defun skk-update-kakutei-history (midasi &optional word)
  ;; 変数 `skk-kakutei-history' を更新する。
  ;; この履歴はskk-comp.el において利用される。
  (cond
   ((<= skk-kakutei-history-limit 0)
    (setq skk-kakutei-history nil))
   (t
    (setq skk-kakutei-history (cons (cons midasi word)
				    skk-kakutei-history))
    (when (> (length skk-kakutei-history)
	     skk-kakutei-history-limit)
      (setcdr (nthcdr (1- skk-kakutei-history-limit)
		      skk-kakutei-history)
	      nil)))))

;;; functions for hooks.
(defun skk-after-point-move ()
  (when (and (not (and skk-previous-point
		       (= skk-previous-point (point))))
	     (skk-get-prefix skk-current-rule-tree))
    (skk-with-point-move
     (skk-erase-prefix 'clean))))

(defun skk-pre-command ()
  (when (and (memq last-command
		   '(skk-insert skk-previous-candidate))
	     (null (memq this-command
			 skk-kana-cleanup-command-list)))
    (skk-kana-cleanup t)))

(defun skk-remove-minibuffer-setup-hook (&rest args)
  ;; Remove all args from minibuffer-setup-hook.
  (while args
    (remove-hook 'minibuffer-setup-hook (car args))
    (setq args (cdr args))))

(add-hook 'edit-picture-hook 'skk-misc-for-picture 'append)
(add-hook 'skk-before-kill-emacs-hook 'skk-record-jisyo-data)
;; add 'skk-save-jisyo only to remove easily.
(add-hook 'skk-before-kill-emacs-hook 'skk-save-jisyo)
(add-hook 'minibuffer-exit-hook
	  '(lambda ()
	    (remove-hook 'pre-command-hook 'skk-pre-command 'local)
	    (skk-remove-minibuffer-setup-hook
	     'skk-j-mode-on 'skk-setup-minibuffer
	     '(lambda ()
		(add-hook 'pre-command-hook 'skk-pre-command nil 'local)))))

;;; cover to original functions.
(skk-defadvice keyboard-quit (around skk-ad activate)
  "▼モードであれば、候補の表示をやめて▽モードに戻す (見出し語は残す)。
▽モードであれば、見出し語を削除する。
上記のどちらのモードでもなければ keyboard-quit と同じ動作をする。"
  (cond
   ;; SKK is not invoked in the current buffer.
   ((not skk-mode)
    ad-do-it)
   ;; ■ mode (Kakutei input mode).
   ((not skk-henkan-mode)
    (cond ((skk-get-prefix skk-current-rule-tree)
	   (skk-erase-prefix 'clean))
	  (t
	   ad-do-it)))
   ;; ▼ mode (Conversion mode).
   ((eq skk-henkan-mode 'active)
    (setq skk-henkan-count 0)
    (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
	(let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
	  (skk-previous-candidate)
	  ;; ここでは delete-backward-char に第二引数を渡さない方がベター？
	  (delete-backward-char count))
      (skk-previous-candidate)))
   ;; ▽ mode (Midashi input mode).
   (t
    (skk-erase-prefix 'clean)
    (when (> (point) skk-henkan-start-point)
      (delete-region (point) skk-henkan-start-point))
    (skk-kakutei))))

(skk-defadvice abort-recursive-edit (around skk-ad activate)
  "▼モードであれば、候補の表示をやめて▽モードに戻す (見出し語は残す)。
▽モードであれば、見出し語を削除する。
上記のどちらのモードでもなければ abort-recursive-edit と同じ動作をする。"
  ;; subr command but no arg.
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer
   '(lambda () (add-hook 'pre-command-hook 'skk-pre-command nil 'local)))
  (cond ((not skk-mode)
	 ad-do-it)
	((not skk-henkan-mode)
	 (cond ((skk-get-prefix skk-current-rule-tree)
		(skk-erase-prefix 'clean))
	       (t
		ad-do-it)))
	((eq skk-henkan-mode 'active)
	 (setq skk-henkan-count 0)
	 (if (and skk-delete-okuri-when-quit
		  skk-henkan-okurigana)
	     (let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
	       (skk-previous-candidate)
	       ;; ここでは delete-backward-char に
	       ;; 第二引数を渡さない方がベター？
	       (delete-backward-char count))
	   (skk-previous-candidate)))
	(t
	 (skk-erase-prefix 'clean)
	 (when (> (point) skk-henkan-start-point)
	   (delete-region (point) skk-henkan-start-point))
	 (skk-kakutei))))

(skk-defadvice newline (around skk-ad activate)
  "`skk-egg-like-newline' だったら、変換中は確定のみ行い、改行しない。"
  (interactive "*P")
  (if (not (or skk-j-mode
	       skk-jisx0201-mode
	       skk-abbrev-mode))
      ad-do-it
    (let (;;(arg (ad-get-arg 0))
	  ;; `skk-kakutei' を実行すると `skk-henkan-mode' の値が
	  ;; 無条件に nil になるので、保存しておく必要がある。
	  (no-newline (and skk-egg-like-newline
			   skk-henkan-mode))
	  (auto-fill-function (if (interactive-p)
				  auto-fill-function
				nil)))
      ;; fill されても nil が帰ってくる :-<
      ;;(if (skk-kakutei)
      ;;    (setq arg (1- arg)))
      ;;(if skk-mode
      ;;    (let ((opos (point)))
      ;;      ;; skk-kakutei (skk-do-auto-fill) によって行が折り返されたら
      ;;      ;; arg を 1 つ減らす。
      ;;      (skk-kakutei)
      ;;      (if (and (not (= opos (point))) (integerp arg))
      ;;          (ad-set-arg 0 (1- arg)))))
      (when skk-mode
	(skk-kakutei))
      (undo-boundary)
      (unless no-newline
	ad-do-it))))

(skk-defadvice newline-and-indent (around skk-ad activate)
  "`skk-egg-like-newline' だったら、変換中は確定のみ行い、改行しない。"
  (if (not (or skk-j-mode
	       skk-jisx0201-mode
	       skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline
			   skk-henkan-mode))
	  (auto-fill-function (if (interactive-p)
				  auto-fill-function
				nil)))
      (when skk-mode
	(skk-kakutei))
      (undo-boundary)
      (unless no-newline
	ad-do-it))))

(skk-defadvice exit-minibuffer (around skk-ad activate)
  ;; subr command but no arg.
  "`skk-egg-like-newline' だったら、変換中は確定のみ行う。"
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer
   '(lambda ()
      (add-hook 'pre-command-hook 'skk-pre-command nil 'local)))
  (if (not (or skk-j-mode
	       skk-jisx0201-mode
	       skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline
			   skk-henkan-mode)))
      (when skk-mode
	(skk-kakutei))
      (unless no-newline
	ad-do-it))))

(defadvice picture-mode-exit (before skk-ad activate)
  "SKK のバッファローカル変数を無効にし、`picture-mode-exit' をコールする。
`picture-mode' から出たときにそのバッファで SKK を正常に動かすための処理。"
  (when skk-mode
    (skk-kill-local-variables)))

(skk-defadvice undo (before skk-ad activate)
  "SKK モードが on なら `skk-self-insert-non-undo-count' を初期化する。"
  (when skk-mode
    (setq skk-self-insert-non-undo-count 0)))

(skk-defadvice kill-buffer (before skk-ad activate)
  "SKK の▼モードだったら、確定してからバッファをキルする。"
  (interactive "bKill buffer: ") ; subr command with arg.
  (when (and skk-mode
	     skk-henkan-mode
	     (interactive-p))
    (skk-kakutei)))

(skk-defadvice save-buffers-kill-emacs (before skk-ad activate)
  (run-hooks 'skk-before-kill-emacs-hook))

(defadvice comint-send-input (around skk-ad activate compile)
  (cond (skk-henkan-mode
	 (skk-kakutei)
	 (unless skk-egg-like-newline
	   ad-do-it))
	(t
	 ad-do-it)))

(run-hooks 'skk-load-hook)

(require 'product)
(product-provide
    (provide 'skk)
  (require 'skk-version))

;;; skk.el ends here
