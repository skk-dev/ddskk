;;; skk-macs.el --- macros and inline functions commonly used in SKK

;; Copyright (C) 1999, 2000, 2001, 2002 SKK Development Team <skk@ring.gr.jp>

;; Author: SKK Development Team <skk@ring.gr.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-macs.el,v 1.94 2004/11/28 09:06:22 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2004/11/28 09:06:22 $

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
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (defvar mule-version)
  (defvar skk-e21-modeline-property))

(eval-when-compile
  (require 'advice)
  (require 'static))

(eval-and-compile
  (require 'skk-vars))

;;;; macros

(put 'ignore-errors 'defmacro-maybe (not (eq skk-emacs-type 'xemacs)))
(defmacro-maybe ignore-errors (&rest body)
  "Execute FORMS; if an error occurs, return nil.
Otherwise, return result of last FORM."
  `(condition-case nil
       (progn
	 ,@body)
     (error nil)))

;;;###autoload
(put 'dolist 'lisp-indent-function 1)
(put 'dolist 'defmacro-maybe (eq skk-emacs-type 'mule4))
(defmacro-maybe dolist (spec &rest body)
  "(dolist (VAR LIST [RESULT]) BODY...): loop over a list.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil."
  (let ((temp (make-symbol "--dolist-temp--")))
    (list 'let (list (list temp (nth 1 spec)) (car spec))
	  (list 'while temp
		(list 'setq (car spec) (list 'car temp))
		(cons 'progn
		      (append body
			      (list (list 'setq temp (list 'cdr temp))))))
	  (if (cdr (cdr spec))
	      (cons 'progn
		    (cons (list 'setq (car spec) nil) (cdr (cdr spec))))))))

;;;###autoload
(put 'dotimes 'lisp-indent-function 1)
(put 'dotimes 'defmacro-maybe (eq skk-emacs-type 'mule4))
(defmacro-maybe dotimes (spec &rest body)
  "(dotimes (VAR COUNT [RESULT]) BODY...): loop a certain number of times.
Evaluate BODY with VAR bound to successive integers running from 0,
inclusive, to COUNT, exclusive.  Then evaluate RESULT to get
the return value (nil if RESULT is omitted)."
  (let ((temp (make-symbol "--dotimes-temp--")))
    (list 'let (list (list temp (nth 1 spec)) (list (car spec) 0))
	   (list 'while (list '< (car spec) temp)
		 (cons 'progn
		       (append body (list (list 'setq (car spec)
						(list '1+ (car spec)))))))
	   (if (cdr (cdr spec))
	       (car (cdr (cdr spec)))
	     nil))))

(defmacro skk-defadvice (function &rest everything-else)
  "Defines a piece of advice for FUNCTION (a symbol).
This is like `defadvice', but warns if FUNCTION is a subr command and advice
doesn't give arguments of `interactive'. See `interactive' for details."
  (let ((origfunc (and (fboundp function)
		       (if (ad-is-advised function)
			   (ad-get-orig-definition function)
			 (symbol-function function))))
	interactive)
    (unless
	(or (not origfunc)
	    (not (subrp origfunc))
	    (memq function ; XXX possibilly Emacs version dependent
		  ;; built-in commands which do not have interactive specs.
		  '(abort-recursive-edit
		    bury-buffer
		    delete-frame
		    delete-window
		    exit-minibuffer)))
      ;; check if advice definition has a interactive call or not.
      (setq interactive
	    (cond
	     ((and (stringp (nth 1 everything-else)) ; have document
		   (eq 'interactive (car-safe (nth 2 everything-else))))
	      (nth 2 everything-else))
	     ((eq 'interactive (car-safe (nth 1 everything-else)))
	      (nth 1 everything-else))))
      (cond
       ((and (commandp origfunc)
	     (not interactive))
	(message
	 "\
*** WARNING: Adding advice to subr %s\
 without mirroring its interactive spec ***"
		 function))
       ((and (not (commandp origfunc))
	     interactive)
	(setq everything-else (delq interactive everything-else))
	(message
	 "\
*** WARNING: Deleted interactive call from %s advice\
 as %s is not a subr command ***"
	 function function))))
    `(defadvice ,function ,@everything-else)))

;;;###autoload
(put 'skk-defadvice 'lisp-indent-function 'defun)
(def-edebug-spec skk-defadvice defadvice)

(defmacro skk-save-point (&rest body)
  `(let ((skk-save-point (point-marker)))
     (unwind-protect
	 (progn
	   ,@body)
       (goto-char skk-save-point)
       (skk-set-marker skk-save-point nil))))

(def-edebug-spec skk-save-point t)

(defmacro skk-message (japanese english &rest arg)
  "メッセージを表示する。
`skk-japanese-message-and-error' が non-nil だったら JAPANESE を nil であれば
ENGLISH をエコーエリアに表示する。ARG は `message' 関数の第２引数以降の引数と
して渡される。"
  (append
   (if arg
       (list 'message (list 'if
			    'skk-japanese-message-and-error
			    japanese
			    english))
     (list 'message "%s" (list 'if
			       'skk-japanese-message-and-error
			       japanese
			       english)))
   arg))

(defmacro skk-error (japanese english &rest arg)
  "メッセージを表示して、エラーを発生させる。
`skk-japanese-message-and-error' が non-nil だったら JAPANESE を nil であれば
ENGLISH をエコーエリアに表示し、エラーを発生させる。 ARG は `error' 関数の第
２引数以降の引数として渡される。"
  (append
   (if arg
       (list 'error (list 'if
			  'skk-japanese-message-and-error
			  japanese
			  english))
     (list 'error "%s" (list 'if
			     'skk-japanese-message-and-error
			     japanese
			     english)))
   arg))

(defmacro skk-yes-or-no-p (japanese english)
  "ユーザに yes-or-no の質問をし、答えが yes だったら t を返す。
`skk-japanese-message-and-error' が non-nil であれば JAPANESE を nil であれ
ば ENGLISH をプロンプトとして `yes-or-no-p' を実行する。
`yes-or-no-p' の引数のプロンプトが複雑に入れ込んでいる場合はこのマクロを使う
よりオリジナルの `yes-or-no-p' を使用した方がコードが複雑にならない場合があ
る。"
 (list 'yes-or-no-p (list 'if 'skk-japanese-message-and-error
				   japanese english)))

(defmacro skk-y-or-n-p (japanese english)
  "ユーザに \"y or n\" の質問をし、答えが \y\" だったら t を返す。
`skk-japanese-message-and-error' が non-nil であれば JAPANESE を nil であれ
ば ENGLISH をプロンプトとして `y-or-n-p' を実行する。"
  (list 'y-or-n-p (list 'if 'skk-japanese-message-and-error
				japanese english)))

(defmacro skk-set-marker (marker position &optional buffer)
  "マーカ MARKER を BUFFER の POSITION に移動する。
BUFFER のディフォルト値はカレントバッファである。
MARKER が nil だったら、新規マーカーを作って代入する。"
  (list 'progn
	(list 'if (list 'not marker)
	      (list 'setq marker (list 'make-marker)))
	(list 'set-marker marker position buffer)))

;; From viper-util.el.  Welcome!
;;;###autoload
(put 'skk-deflocalvar 'lisp-indent-function 'defun)
(defmacro skk-deflocalvar (symbol initvalue &optional docstring)
  "SYMBOL について INITVALUE を値に持つ変数として宣言し、バッファローカル値にする。"
  `(progn
     (defvar ,symbol ,initvalue
       ,(format "%s\n\(buffer local\)" docstring))
     (make-variable-buffer-local ',symbol)))

(defmacro skk-with-point-move (&rest form)
  "ポイントを移動するがフックを実行してほしくない場合に使う。"
  `(unwind-protect
       (progn
	 ,@form)
     (setq skk-previous-point (point))))

(def-edebug-spec skk-with-point-move t)

(defmacro skk-face-on (object start end face &optional priority)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    `(let ((inhibit-quit t))
       (if (not (extentp ,object))
	   (progn
	     (setq ,object (make-extent ,start ,end))
	     (if (not ,priority)
		 (set-extent-face ,object ,face)
	       (set-extent-properties
		,object (list 'face ,face 'priority ,priority))))
	 (set-extent-endpoints ,object ,start ,end))))
   (t
    `(let ((inhibit-quit t))
       (if (not (overlayp ,object))
	   (progn
	     (setq ,object (make-overlay ,start ,end))
	     (when ,priority
	       (overlay-put ,object 'priority ,priority))
	       (overlay-put ,object 'face ,face)
	       ;;(overlay-put (, object) 'evaporate t)
	       )
	 (move-overlay ,object ,start ,end))))))

(defmacro skk-cannot-be-undone (&rest body)
  `(let ((buffer-undo-list t)
	 ;;buffer-read-only
	 (modified (buffer-modified-p)))
     (unwind-protect
	 (progn
	   ,@body)
       (set-buffer-modified-p modified))))

;;;###autoload
(put 'skk-loop-for-buffers 'lisp-indent-function 1)
(defmacro skk-loop-for-buffers (buffers &rest body)
  "BUFFERS が指定する各バッファに移動して BODY を実行する。"
  `(save-current-buffer
     (dolist (buf ,buffers)
       (when (buffer-live-p buf)
	 (set-buffer buf)
	 ,@body))))

;;(defun-maybe mapvector (function sequence)
;; "Apply FUNCTION to each element of SEQUENCE, making a vector of the results.
;;The result is a vector of the same length as SEQUENCE.
;;SEQUENCE may be a list, a vector or a string."
;;  (vconcat (mapcar function sequence) nil))

;;(defun-maybe mapc (function sequence)
;;  "Apply FUNCTION to each element of SEQUENCE.
;;SEQUENCE may be a list, a vector, a bit vector, or a string.
;;-- NOT emulated enough, just discard newly constructed list made by mapcar --
;;This function is like `mapcar' but does not accumulate the results,
;;which is more efficient if you do not use the results."
;;  (mapcar function sequence)
;;  sequence)

;;;; INLINE FUNCTIONS.
;;; version dependent
(defsubst skk-sit-for (seconds &optional nodisplay)
  "`sit-for' の Emacsen による違いを吸収する。"
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (sit-for seconds  nodisplay))
   (t
    (sit-for seconds nil nodisplay))))

(defsubst skk-ding (&optional arg sound device)
  "`ding' の Emacsen による違いを吸収する。"
  (static-cond
   ((eq skk-emacs-type 'xemacs)
     (ding arg sound device))
   (t
    (ding arg))))

(defsubst skk-color-display-p ()
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (eq (device-class (selected-device)) 'color))
   ((fboundp 'x-display-color-p)
    ;; FSF Emacs on X Window System.
    (and window-system (x-display-color-p)))))

(defsubst skk-char-to-string (char)
  (ignore-errors
    (char-to-string char)))

(defsubst skk-ascii-char-p (char)
  (eq (char-charset char) 'ascii))

(defsubst skk-jisx0208-p (char)
  (eq (char-charset char) 'japanese-jisx0208))

(defsubst skk-jisx0213-p (char)
  (and (featurep 'jisx0213)
       (memq (char-charset char)
	     '(japanese-jisx0213-1 japanese-jisx0213-2))))

(defsubst skk-char-octet (ch &optional n)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (or (nth (if n (1+ n) 1) (split-char ch))
	0))
   (t
    ;; FSF Emacs.
    (char-octet ch n))))

;; this one is called once in skk-kcode.el, too.
(defsubst skk-charsetp (object)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (find-charset object))
   (t
    ;; FSF Emacs 20 or later.
    (charsetp object))))

(defun skk-indicator-to-string (indicator &optional no-properties)
  "SKK インジケータ型オブジェクト INDICATOR を文字列に変換する。"
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (if (stringp indicator)
	indicator
      (cdr indicator)))
   ((eq skk-emacs-type 'mule5)
    (if no-properties
	(with-temp-buffer
	  (insert indicator)
	  (buffer-substring-no-properties (point-min) (point-max)))
      indicator))
   (t
    indicator)))

(defun skk-mode-string-to-indicator (mode string)
  "文字列 STRING を SKK インジケータ型オブジェクトに変換する。"
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (cons (cdr (assq mode skk-xemacs-extent-alist))
	  string))
   ((memq skk-emacs-type '(mule5))
    (if (and window-system
	     (not (eq mode 'default)))
	(apply 'propertize string
	       (cdr (assq mode skk-e21-property-alist)))
      string))
   (t
    string)))

;;; This function is not complete, but enough for our purpose.
(defsubst skk-local-variable-p (variable &optional buffer afterset)
  "Non-nil if VARIABLE has a local binding in buffer BUFFER.
BUFFER defaults to the current buffer."
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (local-variable-p variable (or buffer (current-buffer)) afterset))
   (t
    (local-variable-p variable (or buffer (current-buffer))))))

(defsubst skk-face-proportional-p (face)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (face-proportional-p face))
   ((eq skk-emacs-type 'mule5)
    (or (face-equal face 'variable-pitch)
	(eq (face-attribute face :inherit) 'variable-pitch)))))

(defun skk-event-key (event)
  "イベント EVENT を発生した入力の情報を取得する。"
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (let ((tmp (event-key event)))
      (if (symbolp tmp)
	  (vector tmp)
	event)))
   (t
    (let ((char (event-to-character event))
	  keys)
      (if char
	  (vector char)
	(setq keys (recent-keys))
	(vector (aref keys (1- (length keys)))))))))

(defun skk-replace-regexp-in-string (regexp rep string
					    &optional
					    fixedcase literal subexp start)
  (static-cond
   ((and (eq skk-emacs-type 'xemacs)
	 (not (fboundp 'replace-regexp-in-string)))
    (replace-in-string string regexp rep literal))
   (t
    (replace-regexp-in-string regexp rep string
			      fixedcase literal subexp start))))

;;; version independent
(defsubst skk-cursor-set (&optional color force)
  (when (or skk-use-color-cursor
	    force)
    (skk-cursor-set-1 color)))

(defsubst skk-cursor-off ()
  (when skk-use-color-cursor
    (skk-cursor-off-1)))

(defsubst skk-modify-indicator-alist (mode string)
  (setcdr (assq mode skk-indicator-alist)
	  (cons string (skk-mode-string-to-indicator mode string))))

(defun skk-update-modeline (&optional mode string)
  (unless mode
    (setq mode 'default))
  ;;
  (when string
    (skk-modify-indicator-alist mode string))
  ;;
  (let ((indicator (cdr (assq mode skk-indicator-alist))))
    (setq skk-modeline-input-mode
	  (if (eq skk-status-indicator 'left)
	      (cdr indicator)
	    (car indicator)))
    (force-mode-line-update)))

;; ツリーにアクセスするためのインターフェース
(defsubst skk-make-rule-tree (char prefix nextstate kana branch-list)
  (list char
	prefix
	(if (string= nextstate "")
	    nil
	  nextstate)
	kana
	branch-list))

;;(defsubst skk-get-char (tree)
;;  (car tree))
;;
;; skk-current-rule-tree に対して破壊的な操作は行なえない。skk-rule-tree の
;; 内容まで変わってしまい、skk-current-rule-tree の initialize が手軽に行な
;; えなくなる。ここが解決できれば skk-prefix を全滅できるのに...。
;;(defsubst skk-set-char (tree char)
;;  (setcar tree char))
;;
;;(defsubst skk-set-prefix (tree prefix)
;;  (setcar (cdr tree) prefix))

(defsubst skk-get-prefix (tree)
  (nth 1 tree))

(defsubst skk-get-nextstate (tree)
  (nth 2 tree))

(defsubst skk-set-nextstate (tree nextstate)
  (when (string= nextstate "")
    (setq nextstate nil))
  (setcar (nthcdr 2 tree) nextstate))

(defsubst skk-get-kana (tree)
  (nth 3 tree))

(defsubst skk-set-kana (tree kana)
  (setcar (nthcdr 3 tree) kana))

(defsubst skk-get-branch-list (tree)
  (nth 4 tree))

(defsubst skk-set-branch-list (tree branch-list)
  (setcar (nthcdr 4 tree) branch-list))

;; tree procedure for skk-kana-input.
(defsubst skk-add-branch (tree branch)
  (skk-set-branch-list tree (cons branch (skk-get-branch-list tree))))

(defsubst skk-select-branch (tree char)
  (assq char (skk-get-branch-list tree)))

(defun skk-erase-prefix (&optional clean)
  "`skk-echo' が非 nil であれば現在のバッファに挿入された `skk-prefix' を消す。
オプション引数の CLEAN が指定されると、変数としての `skk-prefix' を空文字に、
`skk-current-rule-tree' を nil に初期化する。"

  ;; かな文字の入力がまだ完成していない場合にこの関数が呼ばれたときなどは、バッ
  ;; ファに挿入されている skk-prefix は削除したいが、変数としての skk-prefix は
  ;; null 文字にしたくない。
  (when (and skk-echo
	     skk-kana-start-point
	     (not (string= skk-prefix ""))) ; fail safe.
    (let ((start (marker-position skk-kana-start-point)))
      (when start
	(condition-case nil
	    ;; skk-prefix の消去をアンドゥの対象としない。
	    (skk-cannot-be-undone
	     (delete-region start (+ start (length skk-prefix))))
	  (error
	   (skk-set-marker skk-kana-start-point nil)
	   (setq skk-prefix ""
		 skk-current-rule-tree nil))))))
  (when clean
    (setq skk-prefix ""
	  skk-current-rule-tree nil))) ; fail safe

(defun skk-kana-cleanup (&optional force)
  (let ((data (cond
	       ((and skk-current-rule-tree
		     (null (skk-get-nextstate skk-current-rule-tree)))
		(skk-get-kana skk-current-rule-tree))
	       (skk-kana-input-search-function
		(car (funcall skk-kana-input-search-function)))))
	kana)
    (when (or force data)
      (skk-erase-prefix 'clean)
      (setq kana (if (functionp data)
		     (funcall data nil)
		   data))
      (when (consp kana)
	(setq kana (if skk-katakana
		       (car kana)
		     (cdr kana))))
      (when (stringp kana)
	(skk-insert-str kana))
      (skk-set-marker skk-kana-start-point nil)
      t)))

(defsubst skk-numeric-p ()
  (and skk-use-numeric-conversion
       (require 'skk-num)
       skk-num-list))

(defsubst skk-file-exists-and-writable-p (file)
  (and (setq file (expand-file-name file))
       (file-exists-p file)
       (file-writable-p file)))

(defsubst skk-lower-case-p (char)
  "CHAR が小文字のアルファベットであれば、t を返す。"
  (and (<= ?a char)
       (>= ?z char)))

(defsubst skk-downcase (char)
  (or (cdr (assq char skk-downcase-alist))
      (downcase char)))

(defun skk-mode-off ()
  (setq skk-mode nil
	skk-abbrev-mode nil
	skk-latin-mode nil
	skk-j-mode nil
	skk-jisx0208-latin-mode nil
	skk-jisx0201-mode nil
	;; sub mode of skk-j-mode.
	skk-katakana nil)
  ;; initialize
  (skk-update-modeline)
  (skk-cursor-off)
  (remove-hook 'pre-command-hook 'skk-pre-command 'local))

(defun skk-j-mode-on (&optional katakana)
  (setq skk-mode t
	skk-abbrev-mode nil
	skk-latin-mode nil
	skk-j-mode t
	skk-jisx0208-latin-mode nil
	skk-jisx0201-mode nil
	;; sub mode of skk-j-mode.
	skk-katakana katakana)
  (skk-setup-keymap)
  (skk-update-modeline (if skk-katakana
			   'katakana
			 'hiragana))
  (skk-cursor-set))

(defun skk-latin-mode-on ()
  (setq skk-mode t
	skk-abbrev-mode nil
	skk-latin-mode t
	skk-j-mode nil
	skk-jisx0208-latin-mode nil
	skk-jisx0201-mode nil
	;; sub mode of skk-j-mode.
	skk-katakana nil)
  (skk-setup-keymap)
  (skk-update-modeline 'latin)
  (skk-cursor-set))

(defun skk-jisx0208-latin-mode-on ()
  (setq skk-mode t
	skk-abbrev-mode nil
	skk-latin-mode nil
	skk-j-mode nil
	skk-jisx0208-latin-mode t
	skk-jisx0201-mode nil
	;; sub mode of skk-j-mode.
	skk-katakana nil)
  (skk-setup-keymap)
  (skk-update-modeline 'jisx0208-latin)
  (skk-cursor-set))

(defun skk-abbrev-mode-on ()
  (setq skk-mode t
	skk-abbrev-mode t
	skk-latin-mode nil
	skk-j-mode nil
	skk-jisx0208-latin-mode nil
	skk-jisx0201-mode nil
	;; skk-abbrev-mode は一時的な ascii 文字による変換なので、変換後は元の
	;; 入力モード (かなモードかカナモード) に戻ることが期待される。
	;; skk-katakana は minor-mode フラグではなく、skk-j-mode マイナーモード
	;; の中でこのフラグにより入力文字を決定するポインタを変更するだけなので
	;; skk-abbrev-mode マイナーモード化するのに skk-katakana フラグを初期化
	;; しなければならない必然性はない。
	;; sub mode of skk-j-mode.
	;;skk-katakana nil
	)
  (skk-setup-keymap)
  (skk-update-modeline 'abbrev)
  (skk-cursor-set))

(defsubst skk-in-minibuffer-p ()
  "カレントバッファがミニバッファかどうかをチェックする。"
  (eq (current-buffer) (window-buffer (minibuffer-window))))

(defsubst skk-insert-prefix (&optional char)
  "`skk-echo' が non-nil であればカレントバッファに `skk-prefix' を挿入する。"
  (when skk-echo
    ;; skk-prefix の挿入をアンドゥの対象としない。挿入したプレフィックスは、
    ;; かな文字を挿入する前に全て消去するので、その間、buffer-undo-list を
    ;; t にしてアンドゥ情報を蓄えなくとも問題がない。
    (skk-cannot-be-undone
     (insert-and-inherit (or char skk-prefix)))))

(defsubst skk-string<= (str1 str2)
  "STR1 と STR2 とを比較して、string< か string= であれば、t を返す。"
  (or (string< str1 str2)
      (string= str1 str2)))

(defsubst skk-do-auto-fill ()
  "`auto-fill-function' に値が代入されていれば、それをコールする。"
  (when auto-fill-function
    (funcall auto-fill-function)))

(defsubst skk-current-input-mode ()
  (cond (skk-abbrev-mode 'abbrev)
	(skk-latin-mode 'latin)
	(skk-jisx0208-latin-mode 'jisx0208-latin)
	(skk-katakana 'katakana)
	(skk-j-mode 'hiragana)))

;;(defsubst skk-substring-head-character (string)
;;  (char-to-string (string-to-char string)))

(defsubst skk-get-current-candidate-1 (&optional count)
  (setq count (or count (skk-henkan-count)))
  (when (> 0 count)
    (skk-error "候補を取り出すことができません"
	       "Cannot get current candidate"))
  ;; (nth -1 '(A B C)) は、A を返すので、負でないかどうかチェックする。
  (nth count skk-henkan-list))

;; convert skk-rom-kana-rule-list to skk-rule-tree.
;; The rule tree follows the following syntax:
;; <branch-list>    ::= nil | (<tree> . <branch-list>)
;; <tree>         ::= (<char> <prefix> <nextstate> <kana> <branch-list>)
;; <kana>         ::= (<ひらがな文字列> . <カタカナ文字列>) | nil
;; <char>         ::= <英小文字>
;; <nextstate>    ::= <英小文字文字列> | nil

(defsubst skk-make-raw-arg (arg)
  (cond ((= arg 1) nil)
	((= arg -1) '-)
	((numberp arg) (list arg))))

(defsubst skk-unread-event (event)
  "Unread single EVENT."
  (setq unread-command-events
	(nconc unread-command-events (list event))))

(defsubst skk-get-last-henkan-datum (key)
  (cdr (assq key skk-last-henkan-data)))

(defsubst skk-put-last-henkan-datum (key val)
  (let ((e (assq key skk-last-henkan-data)))
    (if e
	(setcdr e val)
      (push (cons key val) skk-last-henkan-data))))

(defun skk-put-last-henkan-data (alist)
  (let (e)
    (dolist (kv alist)
      (if (setq e (assq (car kv) skk-last-henkan-data))
	  (setcdr e (cdr kv))
	(push (cons (car kv) (cdr kv))
	      skk-last-henkan-data)))))

(defun skk-find-coding-system (code)
  (cond ((and code
	      (or (and (fboundp 'coding-system-p)
		       (coding-system-p code))
		  (and (fboundp 'find-coding-system)
		       (symbolp code)
		       (find-coding-system code))))
	 code)
	((and code (stringp code))
	 (cdr (assoc code skk-coding-system-alist)))
	(t
	 (cdr (assoc "euc" skk-coding-system-alist)))))

(defsubst skk-lisp-prog-p (string)
  "STRING が Lisp プログラムであれば、t を返す。"
  (let ((l (length string)))
    (and (> l 2)
	 (eq (aref string 0) ?\()
	 ;; second character is ascii or not.
	 (skk-ascii-char-p (aref string 1))
	 (eq (aref string (1- l)) ?\)))))

(defsubst skk-eval-string (string)
  "Eval STRING as a lisp program and return the result."
  (let (func)
    ;; (^_^;) のような文字列に対し、read-from-string を呼ぶと
    ;; エラーになるので、ignore-errors で囲む。
    (ignore-errors
      (setq func (car (read-from-string string)))
      (when (and (listp func)
		 (functionp (car func)))
	(setq string (eval func))))
    string))

;;;; from dabbrev.el.  Welcome!
;; 判定間違いを犯す場合あり。要改良。
(defsubst skk-minibuffer-origin ()
  (nth 1 (buffer-list)))

(defun skk-quote-char-1 (word alist)
  (mapconcat
   #'(lambda (char)
       (or (cdr (assq char alist))
	   (char-to-string char)))
   ;; 文字列を対応する char のリストに分解する。
   (append word nil) ""))

(defun skk-key-binding-member (key commands &optional map)
  "入力 KEY が発動するコマンドが、COMMANDS に含まれれば 非 nil を返す。
MAP は入力が書かれているキーマップを指定するが、指定されなければ
`skk-j-mode-map' を参照する。
この関数は、入力 KEY が `lookup-key' で探せない形式でありうる場合に用いる。"
  (let (keys)
    (unless map
      (setq map skk-j-mode-map))
    (dolist (command commands)
      (setq keys (nconc keys
			(where-is-internal command map))))
    (member (key-description key)
	    (mapcar #'(lambda (k)
			(key-description k))
		    keys))))

(require 'product)
(product-provide
    (provide 'skk-macs)
  (require 'skk-version))

;;; skk-macs.el ends here
