;;; skk-foreword.el --- 前書き
;; Copyright (C) 1997, 1998, 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
;;             Murata Shuuichirou  <mrt@astec.co.jp>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-foreword.el,v 1.5 1999/08/31 03:15:40 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/08/31 03:15:40 $

;; This file is not part of SKK yet.

;; SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; このファイルは、ユーザー変数の宣言自体に使用するマクロ、skk-*.el で
;; 使用するマクロなど、変数の宣言以前、skk-*.el の最初に定義しておかな
;; ければならないものをまとめたものです。ユーザー変数の定義の前に、ご
;; ちゃごちゃとユーザーに興味がないものが並んでいたのでは、ユーザーフ
;; レンドリーではないと考えるからです。
;;
;; Following people contributed to skk-foreword.el (Alphabetical order):
;;       小野 孝男 <takao@hirata.nuee.nagoya-u.ac.jp>
;;       Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
;;       Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;;       Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;;       TSUMURA Tomoaki <tsumura@kuis.kyoto-u.ac.jp>

;;; Change log:

;;; Code:
(cond ((or (and (boundp 'epoch::version) epoch::version)
	   (string< (substring emacs-version 0 2) "18") )
       (error "THIS SKK requires Emacs 19 or later") )
      ((not (featurep 'mule))
       (error "THIS SKK requires MULE features") ))

(eval-when-compile
  (defvar skk-abbrev-cursor-color)
  (defvar skk-abbrev-mode)
  (defvar skk-abbrev-mode-string)
  (defvar skk-current-rule-tree)
  (defvar skk-default-cursor-color)
  (defvar skk-downcase-alist)
  (defvar skk-echo)
  (defvar skk-hankaku-alist)
  (defvar skk-henkan-count)
  (defvar skk-henkan-list)
  (defvar skk-hiragana-cursor-color)
  (defvar skk-hiragana-mode-string)
  (defvar skk-input-mode-string)
  (defvar skk-j-mode)
  (defvar skk-jisx0208-latin-cursor-color)
  (defvar skk-jisx0208-latin-mode)
  (defvar skk-jisx0208-latin-mode-string)
  (defvar skk-kana-cleanup-command-list)
  (defvar skk-kana-input-search-function)
  (defvar skk-kana-start-point)
  (defvar skk-katakana)
  (defvar skk-katakana-cursor-color)
  (defvar skk-katakana-mode-string)
  (defvar skk-last-henkan-data)
  (defvar skk-latin-cursor-color)
  (defvar skk-latin-mode)
  (defvar skk-latin-mode-string)
  (defvar skk-look-completion-words)
  (defvar skk-mode)
  (defvar skk-prefix)
  (defvar skk-previous-point)
  (defvar skk-use-numeric-conversion) )

(require 'advice)
(require 'easymenu)
;; APEL 9.21 or later required.
(require 'poe)
(require 'poem)
(require 'pcustom)
(require 'alist)
;; Elib 1.0 is required.
(require 'queue-m)

;;;###autoload
(eval-and-compile
  (defconst skk-emacs-type (cond ((string-match "XEmacs" emacs-version) 'xemacs)
				 ((and (boundp 'mule-version)
				       (string< "4.0" mule-version) 'mule4 ))
				 ((and (boundp 'mule-version)
				       (string< "3.0" mule-version) 'mule3 ))
				 ((and (boundp 'mule-version)
				       (string< "2.0" mule-version) 'mule2 )))))

;; necessary macro and functions to be declared before user variable declarations.

;;;; macros

;; Who uses SKK without compilaition?
;;(eval-when-compile

;; Why I use non-intern temporary variable in the macro --- see comment in
;; save-match-data of subr.el of GNU Emacs. And should we use the same manner
;; in the save-current-buffer, with-temp-buffer and with-temp-file macro
;; definition?
(defmacro skk-save-point (&rest body)
  (` (let ((skk-save-point (point-marker)))
       (unwind-protect
	   (progn (,@ body))
	 (goto-char skk-save-point)
         (skk-set-marker skk-save-point nil) ))))

(defmacro skk-message (japanese english &rest arg)
  ;; skk-japanese-message-and-error が non-nil だったら JAPANESE を nil であれ
  ;; ば ENGLISH をエコーエリアに表示する。
  ;; ARG は message 関数の第２引数以降の引数として渡される。
  (append (list 'message (list 'if 'skk-japanese-message-and-error
			       japanese english ))
	  arg ))

(defmacro skk-error (japanese english &rest arg)
  ;; skk-japanese-message-and-error が non-nil だったら JAPANESE を nil であれ
  ;; ば ENGLISH をエコーエリアに表示し、エラーを発生させる。
  ;; ARG は error 関数の第２引数以降の引数として渡される。
  (append (list 'error (list 'if 'skk-japanese-message-and-error
			     japanese english ))
	  arg ))

(defmacro skk-yes-or-no-p (japanese english)
  ;; skk-japanese-message-and-error が non-nil であれば、japanese を nil であ
  ;; れば english をプロンプトとして yes-or-no-p を実行する。
  ;; yes-or-no-p の引数のプロンプトが複雑に入れ込んでいる場合はこのマクロを使
  ;; うよりオリジナルの yes-or-no-p を使用した方がコードが複雑にならない場合が
  ;; ある。
  (list 'yes-or-no-p (list 'if 'skk-japanese-message-and-error
				   japanese english )))

(defmacro skk-y-or-n-p (japanese english)
  ;; skk-japanese-message-and-error が non-nil であれば、japanese を nil であ
  ;; れば english をプロンプトとして y-or-n-p を実行する。
  (list 'y-or-n-p (list 'if 'skk-japanese-message-and-error
				japanese english )))

(defmacro skk-set-marker (marker position &optional buffer)
  ;; バッファローカル値である skk-henkan-start-point, skk-henkan-end-point,
  ;; skk-kana-start-point, あるいは skk-okurigana-start-point が nil だったら、
  ;; 新規マーカーを作って代入する。
  (list 'progn
        (list 'if (list 'not marker)
              (list 'setq marker (list 'make-marker)) )
        (list 'set-marker marker position buffer) ))

;; From viper-util.el.  Welcome!
(defmacro skk-deflocalvar (var default-value &optional documentation)
  (` (progn
       (defvar (, var) (, default-value)
	       (, (format "%s\n\(buffer local\)" documentation)))
       (make-variable-buffer-local '(, var))
     )))

;; Message-Id: <19981218224936N.sakurada@kuis.kyoto-u.ac.jp>| Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; From: Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>      | の実装
;; Date: Fri, 18 Dec 1998 22:49:36 +0900 (JST)              | (Mule 4 では多バイト文字の長さが 1 になっちゃったので、カラムが
;; 山下さん:                                                 | そろわんぜよ...トホホ。Picture mode の意味半減...)
;; > やはり、カーソル移動をした場合は skk-prefix をクリアしてれ     |
;; > 方がうれしいような気がします。                              | o with-point-move って名前はカッコいいし、そのまま使いたかった
;; このあたりはなんとかしたいところの一つですね.                   |   んだけど、他のパッケージと名前の衝突が起こってはいけないので、
;; コーディングする暇もないので単なる思いつきですが...              |   `skk-' prefix を付けた。
;;                                                          |
;; (1) pre-command-hook よりも post-command-hook と          |
;; last-command の組みあわせのほうがいい気がする.                |
;; (2) 「カーソル(ポイント)の移動」を抽象化したほうが              |
;; いい気がする. たとえばこんなかんじ.                           |
;; # 10分で書いたのであっさり破綻するかも...                     |
;;                                                          |
;; -- 櫻田                                                  |
;;                                                         |
;; 仮定:                                                    |
;;   with-point-move ではバッファをまたがる移動はしない          | o この仮定をそのまま前提とした。
;; TODO:                                                   |
;;   hook/変数をバッファローカルにする必要がある                  | o skk-previous-point 変数、post-command-hook をそれぞれ
;;                                                         |   バッファローカル変数・フックにした (skk-mode を起動した
;;                                                          |   バッファだけで動作させるため)。
;;                                                          |
;;   with-point-move でのエラーがおきたら? (unwinding)         | o バッファをまたがない、という仮定をしたので、unwinding につ
;;                                                          |   いては skk-previous-point のセット以外は考慮していない。
;;                                                          |
;; SKK側の変更:                                              |
;;   skk-kana-input 等はほとんど (with-point-move ...)        | o post-command-hook というぐらいだから、interactive command
;;   で囲む                                                  |        ^^^^^^^
;;                                                          |   だけに的をしぼって skk-with-point-move を使った。
;;                                                          |
;; point が移動したときに走る hook                             |
;; (defvar point-move-hook)                                 | o 複数の関数をフックに入れたりしないので、このフックは実装しない。
;;                                                          |
;; ポイントを保存する変数                                      |
;; (defvar previous-point nil)                              | o skk-previous-point を skk-deflocalvar で宣言。
;;                                                          |
;; ポイントを移動するがフックを実行してほしくない場合に使う          |
;;(defmacro with-point-move (&rest form)                    | o `skk-' prefix を付けほとんどそのままパクり。
;;  `(progn                                                 |
;;     ,@form                                               |
;;     (setq previous-point (point))))                      |
;;                                                          |
;;;; ポイントが移動したら point-move-hook を実行                |
;;(defun point-move-hook-execute ()                         | o skk-after-point-move として inline function として実装。
;;  (if (and point-move-hook                                |   フックを run せずに、関数の中で必要処理をこなす。
;;         (or (null previous-point)                        |
;;             (not (= previous-point (point)))))           |
;;      (with-point-move                                    |
;;       (run-hooks 'point-move-hook))))                    |
;;                                                          |
;;                                                          |
;;(add-hook 'post-command-hook 'point-move-hook-execute)    | o skk-mode の中でフックをローカル化して skk-after-point-move
;;                                                          |   をフックした。
;;                                                          |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   |
;;
;; 例
;;
;; (defun foo ()
;;   (message "move !")
;;   (beep))
;;
;; (add-hook 'point-move-hook 'foo)

(defmacro skk-with-point-move (&rest form)
  ;; ポイントを移動するがフックを実行してほしくない場合に使う。
  (` (unwind-protect
	 (progn (,@ form))
       (setq skk-previous-point (point)) )))

(defmacro skk-defun-cond (name args &optional doc &rest everything-else)
  (or (stringp doc)
      (setq everything-else (cons doc everything-else)
	    doc nil))
  (` (prog1
	 (cond
	  (,@ (mapcar
	       (function
		(lambda (case)
		  (list (car case)
			(if doc
			    (` (defun (, name) (, args)
				 (, doc)
				 (,@ (cdr case))))
			  (` (defun (, name) (, args)
			       (,@ (cdr case))))))))
	       everything-else)))
       (setq current-load-list
	     (cons (quote (, name)) current-load-list))
       )))

(defmacro skk-defsubst-cond (name args &optional doc &rest everything-else)
  (or (stringp doc)
      (setq everything-else (cons doc everything-else)
	    doc nil))
  (` (prog1
	 (cond
	  (,@ (mapcar
	       (function
		(lambda (case)
		  (list (car case)
			(if doc
			    (` (defsubst (, name) (, args)
				 (, doc)
				 (,@ (cdr case))))
			  (` (defsubst (, name) (, args)
			       (,@ (cdr case))))))))
	       everything-else)))
       (setq current-load-list
	     (cons (quote (, name)) current-load-list))
       )))

;;(defmacro skk-defmacro-cond (name args &optional doc &rest everything-else)
;;  (or (stringp doc)
;;      (setq everything-else (cons doc everything-else)
;;	    doc nil))
;;  (` (prog1
;;	 (cond
;;	  (,@ (mapcar
;;	       (function
;;		(lambda (case)
;;		  (list (car case)
;;			(if doc
;;			    (` (defmacro (, name) (, args)
;;				 (, doc)
;;				 (,@ (cdr case))))
;;			  (` (defmacro (, name) (, args)
;;			       (,@ (cdr case))))))))
;;	       everything-else)))
;;       (setq current-load-list
;;	     (cons (quote (, name)) current-load-list)))))
;;
;;;;) ;eval-when-compile
;;
(put 'skk-deflocalvar 'lisp-indent-function 'defun)
;;(put 'skk-defmacro-cond 'lisp-indent-function 'defun)
(put 'skk-defsubst-cond 'lisp-indent-function 'defun)
(put 'skk-defun-cond  'lisp-indent-function 'defun)

;;(defun-maybe mapvector (function sequence)
;;  "Apply FUNCTION to each element of SEQUENCE, making a vector of the results.
;;The result is a vector of the same length as SEQUENCE.
;;SEQUENCE may be a list, a vector or a string."
;;  (vconcat (mapcar function sequence) nil) )

;;(defun-maybe mapc (function sequence)
;;  "Apply FUNCTION to each element of SEQUENCE.
;;SEQUENCE may be a list, a vector, a bit vector, or a string.
;;--- NOT emulated enough, just discard newly constructed list made by mapcar ---
;;This function is like `mapcar' but does not accumulate the results,
;;which is more efficient if you do not use the results."
;;  (mapcar function sequence)
;;  sequence )

;;;; inline functions
(skk-defsubst-cond skk-uninsertable-p (p)
  ((eq skk-emacs-type 'xemacs)
   (if (= (point-min) p)
       (and 
	;; get all extents not just text-property extents.
	(get-char-property p 'read-only)
	(get-char-property p 'start-closed) )
     (or (and (get-char-property p 'read-only)
	      (get-char-property (1- p) 'end-closed) )
	 (and (get-char-property p 'read-only)
	      (get-char-property (1+ p) 'start-closed) ))))
  ;; GNU Emacs
  (t (if (or (= (point-min) p)
	     (eq (get-text-property (1- p) 'rear-nonsticky) t)
	     (memq 'read-only (get-text-property (1- p) 'rear-nonsticky)) )
	 (and (get-text-property p 'read-only)
	      (or (eq (get-text-property p 'front-sticky) t)
		  (memq 'read-only (get-text-property p 'front-sticky)) ))
       (get-text-property (1- p) 'read-only) )))

(defsubst skk-lower-case-p (char)
  ;; CHAR が小文字のアルファベットであれば、t を返す。
  (and (<= ?a char) (>= ?z char) ))

(defsubst skk-downcase (char)
  (or (cdr (assq char skk-downcase-alist)) (downcase char)) )

(defsubst skk-mode-off ()
  (setq skk-mode nil
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        ;; j's sub mode.
        skk-katakana nil )
  ;; initialize
  (setq skk-input-mode-string skk-hiragana-mode-string)
  (skk-set-cursor-color skk-default-cursor-color)
  (force-mode-line-update)
  (remove-hook 'pre-command-hook 'skk-pre-command 'local) )

(defsubst skk-j-mode-on (&optional katakana)
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode t
        skk-jisx0208-latin-mode nil
        ;; j's sub mode.
        skk-katakana katakana )
  ;; mode line
  (if katakana
      (progn
        (setq skk-input-mode-string skk-katakana-mode-string)
        (skk-set-cursor-color skk-katakana-cursor-color) )
    (setq skk-input-mode-string skk-hiragana-mode-string)
    (skk-set-cursor-color skk-hiragana-cursor-color) )
  (force-mode-line-update) )

(defsubst skk-latin-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode t
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        ;; j's sub mode.
        skk-katakana nil
        skk-input-mode-string skk-latin-mode-string )
  (skk-set-cursor-color skk-latin-cursor-color)
  (force-mode-line-update) )

(defsubst skk-jisx0208-latin-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode t
        ;; j's sub mode.
        skk-katakana nil
        skk-input-mode-string skk-jisx0208-latin-mode-string )
  (skk-set-cursor-color skk-jisx0208-latin-cursor-color)
  (force-mode-line-update) )

(defsubst skk-abbrev-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode t
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        ;; j's sub mode.
        skk-katakana nil
        skk-input-mode-string skk-abbrev-mode-string )
  (skk-set-cursor-color skk-abbrev-cursor-color)
  (force-mode-line-update) )

(defsubst skk-in-minibuffer-p ()
  ;; カレントバッファがミニバッファかどうかをチェックする。
  (window-minibuffer-p (selected-window)) )

(defsubst skk-insert-prefix (&optional char)
  ;; skk-echo が non-nil であればカレントバッファに skk-prefix を挿入する。
  (and skk-echo
       ;; skk-prefix の挿入をアンドゥの対象としない。挿入したプレフィックスは、
       ;; かな文字を挿入する前に全て消去するので、その間、buffer-undo-list を
       ;; t にしてアンドゥ情報を蓄えなくとも問題がない。
       (let ((buffer-undo-list t))
         (insert-and-inherit (or char skk-prefix)) )))

(defsubst skk-erase-prefix (&optional clean)
  ;; skk-echo が non-nil であればカレントバッファに挿入された skk-prefix を消
  ;; す。オプショナル引数の CLEAN が指定されると、変数としての skk-prefix を
  ;; null 文字に、skk-current-rule-tree を nil 初期化する。
  ;;
  ;; かな文字の入力がまだ完成していない場合にこの関数が呼ばれたときなどは、バッ
  ;; ファに挿入されている skk-prefix は削除したいが、変数としての skk-prefix は
  ;; null 文字にしたくない。
  (and skk-echo skk-kana-start-point
       (not (string= skk-prefix ""))	; fail safe.
       ;; skk-prefix の消去をアンドゥの対象としない。
       (let ((buffer-undo-list t)
	     (start (marker-position skk-kana-start-point)) )
	 (and start
	      (condition-case nil
		  (delete-region start (+ start (length skk-prefix)))
		(error
		 (skk-set-marker skk-kana-start-point nil) 
		 (setq skk-prefix ""
		       skk-current-rule-tree nil ))))))
  (and clean (setq skk-prefix ""
		   skk-current-rule-tree nil ))) ; fail safe

(defsubst skk-string<= (str1 str2)
  ;; STR1 と STR2 とを比較して、string< か string= であれば、t を返す。
  (or (string< str1 str2) (string= str1 str2)) )

(defsubst skk-do-auto-fill ()
  ;; auto-fill-function に値が代入されておれば、do-auto-fill をコールする。
  (and auto-fill-function (funcall auto-fill-function)) )

;;;; from dabbrev.el.  Welcome!
;; 判定間違いを犯す場合あり。要改良。
(defsubst skk-minibuffer-origin ()
  (nth 1 (buffer-list)) )

(defsubst skk-current-insert-mode ()
  (cond (skk-abbrev-mode 'abbrev)
	(skk-latin-mode 'latin)
	(skk-jisx0208-latin-mode 'jisx0208-latin)
	(skk-katakana 'katakana)
	(skk-j-mode 'hiragana) ))

(defsubst skk-numeric-p ()
  (and skk-use-numeric-conversion (require 'skk-num) skk-num-list) )

(defsubst skk-substring-head-character (string)
  (char-to-string (string-to-char string)) )

(defsubst skk-get-current-candidate-simply (&optional noconv)
  (if (> skk-henkan-count -1)
      ;; (nth -1 '(A B C)) は、A を返すので、負でないかどうかチェックする。
      (let ((word (nth skk-henkan-count skk-henkan-list)))
        (and word
             (if (and (skk-numeric-p) (consp word))
                 (if noconv (car word) (cdr word))
               word )))))

;; convert skk-rom-kana-rule-list to skk-rule-tree.
;; The rule tree follows the following syntax:
;; <branch-list>    ::= nil | (<tree> . <branch-list>)
;; <tree>         ::= (<char> <prefix> <nextstate> <kana> <branch-list>)
;; <kana>         ::= (<ひらがな文字列> . <カタカナ文字列>) | nil
;; <char>         ::= <英小文字>
;; <nextstate>    ::= <英小文字文字列> | nil

;; ツリーにアクセスするためのインターフェース

(defsubst skk-make-rule-tree (char prefix nextstate kana branch-list)
  (list char
	prefix
	(if (string= nextstate "") nil nextstate)
	kana
	branch-list ))

(defsubst skk-get-char (tree)
  (car tree) )

(defsubst skk-set-char (tree char)
  (setcar tree char) )

(defsubst skk-set-prefix (tree prefix)
  (setcar (nthcdr 1 tree) prefix) )

(defsubst skk-get-prefix (tree)
  (nth 1 tree) )

(defsubst skk-get-nextstate (tree)
  (nth 2 tree) )

(defsubst skk-set-nextstate (tree nextstate)
  (if (string= nextstate "") (setq nextstate nil))
  (setcar (nthcdr 2 tree) nextstate) )

(defsubst skk-get-kana (tree)
  (nth 3 tree) )

(defsubst skk-set-kana (tree kana)
  (setcar (nthcdr 3 tree) kana) )

(defsubst skk-get-branch-list (tree)
  (nth 4 tree) )

(defsubst skk-set-branch-list (tree branch-list)
  (setcar (nthcdr 4 tree) branch-list) )

;; tree procedure for skk-kana-input.
(defsubst skk-add-branch (tree branch)
  (skk-set-branch-list tree (cons branch (skk-get-branch-list tree))) )

(defsubst skk-select-branch (tree char)
  (assq char (skk-get-branch-list tree)) )

(defsubst skk-kana-cleanup (&optional force)
  (let ((data (or
	       (and skk-current-rule-tree
		    (null (skk-get-nextstate skk-current-rule-tree))
		    (skk-get-kana skk-current-rule-tree) )
	       (and skk-kana-input-search-function
		    (car (funcall skk-kana-input-search-function)) )))
	kana )
	(if (or force data)
	    (progn
	      (skk-erase-prefix 'clean)
	      (setq kana (if (functionp data) (funcall data nil) data))
	      (if (consp kana)
		  (setq kana (if skk-katakana (car kana) (cdr kana))) )
	      (if (stringp kana) (skk-insert-str kana))
	      (skk-set-marker skk-kana-start-point nil)
	      t ))))

(defsubst skk-pre-command ()
  (and (memq last-command '(skk-insert skk-previous-candidate))
       (null (memq this-command skk-kana-cleanup-command-list))
       (skk-kana-cleanup t) ))

(defsubst skk-make-raw-arg (arg)
  (cond ((= arg 1) nil)
	((= arg -1) '-)
	((numberp arg) (list arg)) ))

(defsubst skk-unread-event (event)
  ;; Unread single EVENT.
  (setq unread-command-events (nconc unread-command-events (list event))) )

(defsubst skk-after-point-move ()
  (and (or (not skk-previous-point) (not (= skk-previous-point (point))))
       (skk-get-prefix skk-current-rule-tree)
       (skk-with-point-move (skk-erase-prefix 'clean)) ))

;;(defsubst skk-get-current-henkan-data (key)
;;  (cdr (assq key skk-current-henkan-data)) )

;;(defsubst skk-put-current-henkan-data (key val)
;;  (setq skk-current-henkan-data (put-alist key val skk-current-henkan-data)) )

(defsubst skk-get-last-henkan-data (key)
  (cdr (assq key skk-last-henkan-data)) )

(defsubst skk-put-last-henkan-data (key val)
  (setq skk-last-henkan-data (put-alist key val skk-last-henkan-data)) )

(defun skk-terminal-face-p ()
  (and (not window-system)
       (fboundp 'frame-face-alist) ;; 変数名みたいな関数だな...。
       (fboundp 'selected-frame) ))

;;;; aliases
;; for backward compatibility.
(define-obsolete-function-alias 'skk-zenkaku-mode 'skk-jisx0208-latin-mode)
(define-obsolete-function-alias 'skk-zenkaku-mode-on 'skk-jisx0208-latin-mode-on)
(define-obsolete-function-alias 'skk-zenkaku-insert 'skk-jisx0208-latin-insert)
(define-obsolete-function-alias 'skk-zenkaku-region 'skk-jisx0208-latin-region)
(define-obsolete-function-alias 'skk-zenkaku-henkan 'skk-jisx0208-latin-henkan)
(define-obsolete-function-alias 'skk-ascii-mode-on 'skk-latin-mode-on)
(define-obsolete-function-alias 'skk-ascii-mode 'skk-latin-mode)
(define-obsolete-function-alias 'skk-ascii-region 'skk-latin-region)
(define-obsolete-function-alias 'skk-ascii-henkan 'skk-latin-henkan)
(define-obsolete-function-alias 'skk-convert-ad-to-gengo 'skk-ad-to-gengo)
(define-obsolete-function-alias 'skk-convert-gengo-to-ad 'skk-gengo-to-ad)
(define-obsolete-function-alias 'skk-isearch-forward 'isearch-forward)
(define-obsolete-function-alias 'skk-isearch-forward-regexp 'isearch-forward-regexp)
(define-obsolete-function-alias 'skk-isearch-backward 'isearch-backward)
(define-obsolete-function-alias 'skk-isearch-backward-regexp 'isearch-backward-regexp)

(defconst skk-background-mode
  ;; from font-lock-make-faces of font-lock.el  Welcome!
  (cond
   ((eq skk-emacs-type 'xemacs)
    (if (< (apply '+ (color-rgb-components
                      (face-property 'default 'background) ))
           (/ (apply '+ (color-rgb-components
                         (make-color-specifier "white"))) 3))
        'dark
      'light ))
   ((and window-system (x-display-color-p))
    (let ((bg-resource (x-get-resource ".backgroundMode"
                                       "BackgroundMode"))
          params )
      (if bg-resource
          (intern (downcase bg-resource))
        (setq params (frame-parameters))
        (cond ((cdr (assq 'background-mode params)));; Emacs20.x (Meadow)
	      ((and (eq system-type 'windows-nt);; Mule for Win32
                    (fboundp 'win32-color-values) )
               (< (apply '+ (win32-color-values
                             (cdr (assq 'background-color params)) ))
                  (/ (apply '+ (win32-color-values "white")) 3) )
               'dark )
              ((and (memq system-type '(ms-dos windows-nt))
                    (fboundp 'x-color-values) )
               (if (string-match "light"
                                 (cdr (assq 'background-color params)) )
                   'light
                 'dark ))
              ((< (apply '+ (x-color-values
                             (cdr (assq 'background-color params)) ))
                  (/ (apply '+ (x-color-values "white")) 3) )
               'dark )
              (t 'light) ))))
   (t 'mono) ))

;;;; version specific matter.
(eval-and-compile
  ;; Emacs ver. による分類
  (cond ((eq skk-emacs-type 'xemacs)
	 (defalias 'skk-char-to-string 'char-to-string)
	 (defalias 'skk-read-event 'next-command-event)
	 )
	(t
	 ;; for Mule/GNU Emacs
         (defalias 'skk-read-event 'read-event)

	 (if (string< "20" emacs-version)
	     ;; for Emacs 20.x
	     (defun skk-char-to-string (char)
	       (condition-case nil (char-to-string char) (error nil)) )
	   ;; for Emacs 19.x
	   (defalias 'skk-char-to-string 'char-to-string) )))

  ;; Mule ver. による分類
  (cond ((eq skk-emacs-type 'xemacs)
	 (defsubst skk-ascii-char-p (char)
	   ;; CHAR が ascii 文字だったら t を返す。
	   (eq (char-charset char) 'ascii) )
	 (defalias 'skk-charsetp (cond ((fboundp 'charsetp) 'charsetp)
				       ;; Is there XEmacs that doesn't have
				       ;; `charsetp'?
				       (t 'find-charset) ))
	 (defun skk-make-char (charset n1 n2)
	   (make-char charset
		      (logand (lognot 128) n1)
		      (logand (lognot 128) n2) ))
	 (defsubst skk-jisx0208-p (char)
	   (eq (char-charset char) 'japanese-jisx0208) )
	 (defun skk-jisx0208-to-ascii (string)
	   (require 'japan-util)
	   (let ((char
		  (get-char-code-property (string-to-char string) 'ascii) ))
	     (and char (char-to-string char)) ))
	 (defalias 'skk-str-length 'length)
	 (defalias 'skk-str-ref 'aref)
	 (defalias 'skk-substring 'substring)
	 )

	((eq skk-emacs-type 'mule4)
	 (defsubst skk-ascii-char-p (char)
	   ;; CHAR が ascii 文字だったら t を返す。
	   (eq (char-charset char) 'ascii) )
	 (defalias 'skk-charsetp 'charsetp)
	 (defalias 'skk-make-char 'make-char)
	 (defsubst skk-jisx0208-p (char)
	   (eq (char-charset char) 'japanese-jisx0208) )
	 (defun skk-jisx0208-to-ascii (string)
	   (require 'japan-util)
	   (let ((char
		  (get-char-code-property (string-to-char string) 'ascii) ))
	     (and char (char-to-string char)) ))
	 (defalias 'skk-str-length 'length)
	 (defalias 'skk-str-ref 'aref)
	 (defalias 'skk-substring 'substring)
	 )

	((eq skk-emacs-type 'mule3)
 	 (defsubst skk-ascii-char-p (char)
 	   ;; CHAR が ascii 文字だったら t を返す。
 	   (eq (char-charset char) 'ascii) )
	 (defalias 'skk-charsetp 'charsetp)
	 (defalias 'skk-make-char 'make-char)
	 (defsubst skk-jisx0208-p (char)
	   (eq (char-charset char) 'japanese-jisx0208) )
	 (defun skk-jisx0208-to-ascii (string)
	   (require 'japan-util)
	   (let ((char
		  (get-char-code-property (string-to-char string) 'ascii) ))
	     (and char (char-to-string char)) ))
	 (defun skk-str-length (str)
	   (length (string-to-vector str)) )
	 (defun skk-str-ref (str pos)
	   (aref (string-to-vector str) pos ) )
	 (defun skk-substring (str pos1 pos2)
	   (if (< pos1 0)
	       (setq pos1 (+ (skk-str-length str) pos1)) )
	   (if (< pos2 0)
	       (setq pos2 (+ (skk-str-length str) pos2)) )
	   (if (>= pos1 pos2)
	       ""
	     (let ((sl (nthcdr pos1 (string-to-char-list str))))
	       (setcdr (nthcdr (- pos2 pos1 1) sl) nil)
	       (concat sl) )))
	 )

	((eq skk-emacs-type 'mule2)
	 (defsubst skk-ascii-char-p (char)
	   ;; CHAR が ascii 文字だったら t を返す。
	   (= (char-leading-char char) 0) )
	 (defalias 'skk-charsetp 'character-set)
	 (defalias 'skk-make-char 'make-chararacter)
	 (defsubst skk-jisx0208-p (char)
	   (= (char-leading-char char) lc-jp) )
	 (defun skk-jisx0208-to-ascii (string)
	   (let ((char
		  (let* ((ch (string-to-char string))
			 (ch1 (char-component ch 1)) )
		    (cond ((eq 161 ch1)	; ?\241
			   (cdr (assq (char-component ch 2) skk-hankaku-alist)) )
			  ((eq 163 ch1)	; ?\243
			   (- (char-component ch 2) 128) ; ?\200
			   )))))
	     (and char (char-to-string char)) ))
	 (defun skk-str-length (str)
	   (length (string-to-char-list str)) )
	 (defun skk-str-ref (str pos)
	   (nth pos (string-to-char-list str)) )
	 (defun skk-substring (str pos1 pos2)
	   (if (< pos1 0)
	       (setq pos1 (+ (skk-str-length str) pos1)) )
	   (if (< pos2 0)
	       (setq pos2 (+ (skk-str-length str) pos2)) )
	   (if (>= pos1 pos2)
	       ""
	     (let ((sl (nthcdr pos1 (string-to-char-list str))))
	       (setcdr (nthcdr (- pos2 pos1 1) sl) nil)
	       (mapconcat 'char-to-string sl "") )))
	 )))

(defun skk-define-menu-bar-map (map)
  ;; SKK メニューのトップに出現するコマンドのメニューへの定義を行なう。
  (easy-menu-define
   skk-menu map
   "Menu used in SKK mode."
   '("SKK"
     ("Convert Region and Echo"
      ("Gyakubiki"
       ["to Hiragana" skk-gyakubiki-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hiragana, All Candidates"
        ;; あれれ、lambda 関数は定義できないのか？？？  動かないぞ...。
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-message start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-gyakubiki-katakana-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-katakana-message
                     start end 'all-candidates ) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      ("Hurigana"
       ["to Hiragana" skk-hurigana-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hiragana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-hurigana-message start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-hurigana-katakana-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-hurigana-katakana-message
                     start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      )
     ("Convert Region and Replace"
      ["Ascii" skk-ascii-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ("Gyakubiki"
       ["to Hiragana" skk-gyakubiki-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hiragana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-region start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-gyakubiki-katakana-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-katakana-region
                     start end 'all-candidates ) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      ["Hiragana" skk-hiragana-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ("Hurigana"
       ["to Hiragana" skk-hurigana-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hiragana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-hurigana-region start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-hurigana-katakana-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates" (function
                                       (lambda (start end) (interactive "r")
                                         (skk-hurigana-katakana-region
                                          start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      ["Katakana" skk-katakana-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ["Romaji" skk-romaji-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ["Zenkaku" skk-jisx0208-latin-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      )
     ["Count Jisyo Candidates" skk-count-jisyo-candidates
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
     ["Save Jisyo" skk-save-jisyo
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
     ["Undo Kakutei" skk-undo-kakutei
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
     ["Version" skk-version
      (or (not (boundp 'skktut-problem-count))
          (eq skktut-problem-count 0)) ]
     )))

(provide 'skk-foreword)
;;; Local Variables:
;;; End:
;;; skk-forwords.el ends here
