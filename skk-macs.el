;;; skk-macs.el --- Macros and inline functions commonly use in
;;                  Daredevil SKK package programs.
;; Copyright (C) 1999, 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-macs.el,v 1.18 2000/12/04 03:41:17 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/12/04 03:41:17 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; Daredevil SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (defvar mule-version)
  (defvar skk-e21-modeline-property))

(eval-when-compile
  (require 'advice)
  (require 'cl)
  (require 'static)
  (require 'skk-vars)
  (defconst skk-emacs-type
    (cond ((featurep 'xemacs) 'xemacs)
	  ((and (boundp 'NEMACS)) 'nemacs)
	  ((and (boundp 'mule-version)
		(string< "5.0" mule-version) 'mule5))
	  ((and (boundp 'mule-version)
		(string< "4.0" mule-version) 'mule4))
	  ((and (boundp 'mule-version)
		(string< "3.0" mule-version) 'mule3))
	  ((and (boundp 'mule-version)
		(string< "2.0" mule-version) 'mule2))
	  ((and (boundp 'mule-version)
		(string< "1.0" mule-version) 'mule1)))))

;;;; macros

(defmacro skk-defadvice (function &rest everything-else)
  (let ((origfunc (and (fboundp function)
		       (if (ad-is-advised function)
			   (ad-get-orig-definition function)
			 (symbol-function function))))
	interactive)
    (if (or (not origfunc)
	    (not (subrp origfunc))
	    (memq function		; XXX possibilly Emacs version dependent
		  ;; interactive commands which do not have interactive specs.
		  '(abort-recursive-edit bury-buffer delete-frame delete-window
					 exit-minibuffer)))
	nil
      ;; check if advice definition has a interactive call or not.
      (setq interactive
	    (cond ((and (stringp (nth 1 everything-else)) ; have document
			(eq 'interactive (car-safe (nth 2 everything-else))))
		   (nth 2 everything-else))
		  ((eq 'interactive (car-safe (nth 1 everything-else)))
		   (nth 1 everything-else))))
      (cond ((and (commandp origfunc) (not interactive))
	     (message
	      "*** WARNING: Adding advice to subr %s without mirroring its interactive spec ***"
	      function))
	    ((and (not (commandp origfunc)) interactive)
	     (setq everything-else (delq interactive everything-else))
	     (message
	      "*** WARNING: Deleted interactive call from %s advice as % is not a subr command ***"
	      function function))))
    (` (defadvice (, function) (,@ everything-else)))))

(put 'skk-defadvice 'lisp-indent-function 'defun)
(def-edebug-spec skk-defadvice defadvice)

(defmacro skk-save-point (&rest body)
  (` (let ((skk-save-point (point-marker)))
       (unwind-protect
	   (progn (,@ body))
	 (goto-char skk-save-point)
         (skk-set-marker skk-save-point nil)))))

(def-edebug-spec skk-save-point t)

(defmacro skk-message (japanese english &rest arg)
  ;; skk-japanese-message-and-error が non-nil だったら JAPANESE を nil であれ
  ;; ば ENGLISH をエコーエリアに表示する。
  ;; ARG は message 関数の第２引数以降の引数として渡される。
  (append (list 'message (list 'if 'skk-japanese-message-and-error
			       japanese english))
	  arg))

(defmacro skk-error (japanese english &rest arg)
  ;; skk-japanese-message-and-error が non-nil だったら JAPANESE を nil であれ
  ;; ば ENGLISH をエコーエリアに表示し、エラーを発生させる。
  ;; ARG は error 関数の第２引数以降の引数として渡される。
  (append (list 'error (list 'if 'skk-japanese-message-and-error
			     japanese english))
	  arg))

(defmacro skk-yes-or-no-p (japanese english)
  ;; skk-japanese-message-and-error が non-nil であれば、japanese を nil であ
  ;; れば english をプロンプトとして yes-or-no-p を実行する。
  ;; yes-or-no-p の引数のプロンプトが複雑に入れ込んでいる場合はこのマクロを使
  ;; うよりオリジナルの yes-or-no-p を使用した方がコードが複雑にならない場合が
  ;; ある。
  (list 'yes-or-no-p (list 'if 'skk-japanese-message-and-error
				   japanese english)))

(defmacro skk-y-or-n-p (japanese english)
  ;; skk-japanese-message-and-error が non-nil であれば、japanese を nil であ
  ;; れば english をプロンプトとして y-or-n-p を実行する。
  (list 'y-or-n-p (list 'if 'skk-japanese-message-and-error
				japanese english)))

(defmacro skk-set-marker (marker position &optional buffer)
  ;; バッファローカル値である skk-henkan-start-point, skk-henkan-end-point,
  ;; skk-kana-start-point, あるいは skk-okurigana-start-point が nil だったら、
  ;; 新規マーカーを作って代入する。
  (list 'progn
        (list 'if (list 'not marker)
              (list 'setq marker (list 'make-marker)))
        (list 'set-marker marker position buffer)))

;; From viper-util.el.  Welcome!
(defmacro skk-deflocalvar (var default-value &optional documentation)
  (` (progn
       (defvar (, var) (, default-value)
	       (, (format "%s\n\(buffer local\)" documentation)))
       (make-variable-buffer-local '(, var))
       )))
(put 'skk-deflocalvar 'lisp-indent-function 'defun)

(defmacro skk-with-point-move (&rest form)
  ;; ポイントを移動するがフックを実行してほしくない場合に使う。
  (` (unwind-protect
	 (progn (,@ form))
       (setq skk-previous-point (point)))))

(def-edebug-spec skk-with-point-move t)

(defmacro skk-face-on (object start end face &optional priority)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (` (let ((inhibit-quit t))
	 (if (not (extentp (, object)))
	     (progn
	       (setq (, object) (make-extent (, start) (, end)))
	       (if (not (, priority))
		   (set-extent-face (, object) (, face))
		 (set-extent-properties
		  (, object) (list 'face (, face) 'priority (, priority)))))
	   (set-extent-endpoints (, object) (, start) (, end))))))
   (t
    (` (let ((inhibit-quit t))
	 (if (not (overlayp (, object)))
	     (progn
	       (setq (, object) (make-overlay (, start) (, end)))
	       (and (, priority) (overlay-put (, object) 'priority (, priority)))
	       (overlay-put (, object) 'face (, face)))
	   (move-overlay (, object) (, start) (, end))))))))

(defmacro skk-sit-for (seconds &optional nodisplay)
  (case skk-emacs-type
   ((nemacs mule1 xemacs)
    (` (sit-for (, seconds) (, nodisplay))))
   (t
    ;; Emacs 19 or later.
    (` (sit-for (, seconds) nil (, nodisplay))))))

(defmacro skk-ding (&optional arg sound device)
  (case skk-emacs-type
    (xemacs
     (` (ding (, arg) (, sound) (, device))))
    (t
     (` (ding (, arg))))))

(defmacro skk-update-modeline (indicator)
  (` (setq skk-modeline-input-mode (, indicator))))

(defmacro skk-cannot-be-undone (&rest body)
  (` (let ((buffer-undo-list t)
	   ;;buffer-read-only
	   (modified (buffer-modified-p)))
       (unwind-protect
	   (progn (,@ body))
	 (set-buffer-modified-p modified)))))

;;(defun-maybe mapvector (function sequence)
;;  "Apply FUNCTION to each element of SEQUENCE, making a vector of the results.
;;The result is a vector of the same length as SEQUENCE.
;;SEQUENCE may be a list, a vector or a string."
;;  (vconcat (mapcar function sequence) nil))

;;(defun-maybe mapc (function sequence)
;;  "Apply FUNCTION to each element of SEQUENCE.
;;SEQUENCE may be a list, a vector, a bit vector, or a string.
;;--- NOT emulated enough, just discard newly constructed list made by mapcar ---
;;This function is like `mapcar' but does not accumulate the results,
;;which is more efficient if you do not use the results."
;;  (mapcar function sequence)
;;  sequence)

;;;; INLINE FUNCTIONS.
;;; version dependent
(defsubst skk-color-display-p ()
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (eq (device-class (selected-device)) 'color))
   ((fboundp 'x-display-color-p)
    ;; Emacs 19 or later.
    (and window-system (x-display-color-p)))))

(defsubst skk-str-length (str)
  ;; multibyte 文字を 1 と数えたときの文字列の長さ。
  (static-cond
   ((memq skk-emacs-type '(nemacs mule1 mule2))
    (length (string-to-char-list str)))
   ((eq skk-emacs-type 'mule3)
    (length (string-to-vector str)))
   (t
    ;; XEmacs, MULE 4.0 or later.
    (length str))))

(defsubst skk-substring (str pos1 &optional pos2)
  ;; multibyte 文字を 1 と数えて substring する。
  (or pos2 (setq pos2 (skk-str-length str)))
  (static-cond
   ((eq skk-emacs-type 'nemacs)
    ;; XXX not yet tested.
    (if (< pos1 0)
	(setq pos1 (+ (skk-str-length str) pos1)))
    (if (< pos2 0)
	(setq pos2 (+ (skk-str-length str) pos2)))
    (if (>= pos1 pos2)
	""
      (let (start end n)
	(with-temp-buffer
	  (insert str)
	  (goto-char (point-min))
	  (setq n pos1)
	  (while (and (not (eobp)) (> n 0))
	    (if (eobp)
		(signal 'error
			(cons "Args out of range" (list str pos1 pos2)))
	      (forward-char 1)
	      (setq n (1- n))))
	  (setq start (point))
	  (setq n (- pos2 pos1))
	  (while (and (not (eobp)) (> n 0))
	    (if (eobp)
		(signal 'error
			(cons "Args out of range" (list str pos1 pos2)))
	      (forward-char 1)
	      (setq n (1- n))))
	  (setq end (point))
	  ;;
	  (buffer-substring start end)))))
   ((memq skk-emacs-type '(nemacs mule1 mule2))
    (if (< pos1 0)
	(setq pos1 (+ (skk-str-length str) pos1)))
    (if (< pos2 0)
	(setq pos2 (+ (skk-str-length str) pos2)))
    (if (>= pos1 pos2)
	""
      (let ((sl (nthcdr pos1 (string-to-char-list str))))
	(setcdr (nthcdr (- pos2 pos1 1) sl) nil)
	(mapconcat 'char-to-string sl ""))))
   ((eq skk-emacs-type 'mule3)
    (if (< pos1 0)
	(setq pos1 (+ (skk-str-length str) pos1)))
    (if (< pos2 0)
	(setq pos2 (+ (skk-str-length str) pos2)))
    (if (>= pos1 pos2)
	""
      (let ((sl (nthcdr pos1 (string-to-char-list str))))
	(setcdr (nthcdr (- pos2 pos1 1) sl) nil)
	(concat sl))))
   (t
    ;; XEmacs, MULE 4.0 or later.
    (substring str pos1 pos2))))

(defsubst skk-char-to-string (char)
  (condition-case nil (char-to-string char) (error)))

(defsubst skk-ascii-char-p (char)
  ;; CHAR が ascii 文字だったら t を返す。
  (static-cond
   ((eq skk-emacs-type 'nemacs)
    (and (< ?\37 char) (< char ?\200)))
   ((memq skk-emacs-type '(mule1 mule2))
    ;; Can I use this for mule1?
    ;; (maybe < cz)
    (= (char-leading-char char) 0))
   (t
    ;; XEmacs, Emacs 20 or later.
    (eq (char-charset char) 'ascii))))

(defsubst skk-str-ref (str pos)
  (static-cond
   ((memq skk-emacs-type '(nemacs mule1 mule2))
    (nth pos (string-to-char-list str)))
   ((eq skk-emacs-type 'mule3)
    (aref (string-to-vector str) pos))
   (t
    ;; XEmacs, MULE 4.0 or later.
    (aref str pos))))

(defsubst skk-jisx0208-p (char)
  (static-cond
   ((eq skk-emacs-type 'nemacs)
    (and (<= char ?\200) (<= ?\377 char)))
   ((memq skk-emacs-type '(mule1 mule2))
    ;; Can I use this for mule1?
    ;; (maybe < cz)
    (= (char-leading-char char) lc-jp))
   (t
    ;; XEmacs, MULE 3.0 or later.
    (eq (char-charset char) 'japanese-jisx0208))))

(defsubst skk-jisx0213-p (char)
  (and (featurep 'jisx0213)
       (memq (char-charset char) '(japanese-jisx0213-1 japanese-jisx0213-2))))

(defsubst skk-char-octet (ch &optional n)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (or (nth (if n (1+ n) 1) (split-char ch)) 0))
   (t
    ;; FSF Emacs
    (char-octet ch n))))

;; this one is called once in skk-kcode.el, too.
(defsubst skk-charsetp (object)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (find-charset object))
   ((eq skk-emacs-type 'nemacs)
    ;; XXX not yet tested.
    (charset-description object))
   ((memq skk-emacs-type '(mule1 mule2))
    (character-set object))
   (t
    ;; MULE 3.0 or later.
    (charsetp object))))

;;; version independent
;; ツリーにアクセスするためのインターフェース
(defsubst skk-make-rule-tree (char prefix nextstate kana branch-list)
  (list char
	prefix
	(if (string= nextstate "") nil nextstate)
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
  (if (string= nextstate "") (setq nextstate nil))
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
       (let ((start (marker-position skk-kana-start-point)))
	 (and start
	      (condition-case nil
		  ;; skk-prefix の消去をアンドゥの対象としない。
		  (skk-cannot-be-undone
		   (delete-region start (+ start (length skk-prefix))))
		(error
		 (skk-set-marker skk-kana-start-point nil)
		 (setq skk-prefix ""
		       skk-current-rule-tree nil))))))
  (and clean (setq skk-prefix ""
		   skk-current-rule-tree nil))) ; fail safe

(defsubst skk-kana-cleanup (&optional force)
  (let ((data (or
	       (and skk-current-rule-tree
		    (null (skk-get-nextstate skk-current-rule-tree))
		    (skk-get-kana skk-current-rule-tree))
	       (and skk-kana-input-search-function
		    (car (funcall skk-kana-input-search-function)))))
	kana)
	(if (or force data)
	    (progn
	      (skk-erase-prefix 'clean)
	      (setq kana (if (functionp data) (funcall data nil) data))
	      (if (consp kana)
		  (setq kana (if skk-katakana (car kana) (cdr kana))))
	      (if (stringp kana) (skk-insert-str kana))
	      (skk-set-marker skk-kana-start-point nil)
	      t))))

(defsubst skk-numeric-p ()
  (and skk-use-numeric-conversion (require 'skk-num) skk-num-list))

(defsubst skk-file-exists-and-writable-p (file)
  (and (setq file (expand-file-name file))
       (file-exists-p file) (file-writable-p file)))

(defsubst skk-lower-case-p (char)
  ;; CHAR が小文字のアルファベットであれば、t を返す。
  (and (<= ?a char) (>= ?z char)))

(defsubst skk-downcase (char)
  (or (cdr (assq char skk-downcase-alist)) (downcase char)))

(defsubst skk-mode-off ()
  (setq skk-mode nil
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        ;; sub mode of skk-j-mode.
        skk-katakana nil)
  ;; initialize
  (skk-update-modeline skk-default-indicator)
  (static-when (memq skk-emacs-type '(nemacs mule1))
    (use-local-map skk-current-local-map)
    (setq skk-current-local-map nil))
  (force-mode-line-update)
  (remove-hook 'pre-command-hook 'skk-pre-command 'local))

(defsubst skk-j-mode-on (&optional katakana)
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode t
        skk-jisx0208-latin-mode nil
        ;; sub mode of skk-j-mode.
        skk-katakana katakana)
  (skk-update-modeline (if skk-katakana
			   skk-katakana-mode-indicator
			 skk-hiragana-mode-indicator))
  (static-when (memq skk-emacs-type '(nemacs mule1))
    (use-local-map
     (skk-e18-make-local-map skk-j-mode-map
			     (if (skk-in-minibuffer-p)
				 minibuffer-local-map
			       skk-current-local-map))))
  (force-mode-line-update))

(defsubst skk-latin-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode t
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        ;; sub mode of skk-j-mode.
        skk-katakana nil)
  (skk-update-modeline skk-latin-mode-indicator)
  (static-when (memq skk-emacs-type '(nemacs mule1))
    (use-local-map
     (skk-e18-make-local-map skk-latin-mode-map
			     (if (skk-in-minibuffer-p)
				 minibuffer-local-map
			       skk-current-local-map))))
  (force-mode-line-update))

(defsubst skk-jisx0208-latin-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode t
        ;; sub mode of skk-j-mode.
        skk-katakana nil)
  (skk-update-modeline skk-jisx0208-latin-mode-indicator)
  (static-when (memq skk-emacs-type '(nemacs mule1))
    (use-local-map
     (skk-e18-make-local-map skk-jisx0208-latin-mode-map
			     (if (skk-in-minibuffer-p)
				 minibuffer-local-map
			       skk-current-local-map))))
  (force-mode-line-update))

(defsubst skk-abbrev-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode t
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
	;; skk-abbrev-mode は一時的な ascii 文字による変換なので、変換後は元の
	;; 入力モード (かなモードかカナモード) に戻ることが期待される。
	;; skk-katakana は minor-mode フラグではなく、skk-j-mode マイナーモード
	;; の中でこのフラグにより入力文字を決定するポインタを変更するだけなので
	;; skk-abbrev-mode マイナーモード化するのに skk-katakana フラグを初期化
	;; しなければならない必然性はない。
        ;; sub mode of skk-j-mode.
        ;;skk-katakana nil
        )
  (skk-update-modeline skk-abbrev-mode-indicator)
  (static-when (memq skk-emacs-type '(nemacs mule1))
    (use-local-map
     (skk-e18-make-local-map skk-abbrev-mode-map
			     (if (skk-in-minibuffer-p)
				 minibuffer-local-map
			       skk-current-local-map))))
  (force-mode-line-update))

(defsubst skk-in-minibuffer-p ()
  ;; カレントバッファがミニバッファかどうかをチェックする。
  (window-minibuffer-p (selected-window)))

(defsubst skk-insert-prefix (&optional char)
  ;; skk-echo が non-nil であればカレントバッファに skk-prefix を挿入する。
  (and skk-echo
       ;; skk-prefix の挿入をアンドゥの対象としない。挿入したプレフィックスは、
       ;; かな文字を挿入する前に全て消去するので、その間、buffer-undo-list を
       ;; t にしてアンドゥ情報を蓄えなくとも問題がない。
       (skk-cannot-be-undone
	(insert-and-inherit (or char skk-prefix)))))

(defsubst skk-string<= (str1 str2)
  ;; STR1 と STR2 とを比較して、string< か string= であれば、t を返す。
  (or (string< str1 str2) (string= str1 str2)))

(defsubst skk-do-auto-fill ()
  ;; auto-fill-function/auto-fill-hook に値が代入されていれば、それをコールする。
  (static-cond
   ((memq skk-emacs-type '(nemacs mule1))
    (and auto-fill-hook (run-hooks 'auto-fill-hook)))
   (t (and auto-fill-function (funcall auto-fill-function)))))

(defsubst skk-current-input-mode ()
  (cond (skk-abbrev-mode 'abbrev)
	(skk-latin-mode 'latin)
	(skk-jisx0208-latin-mode 'jisx0208-latin)
	(skk-katakana 'katakana)
	(skk-j-mode 'hiragana)))

;;(defsubst skk-substring-head-character (string)
;;  (char-to-string (string-to-char string)))

(defsubst skk-get-current-candidate-1 ()
  (if (> 0 skk-henkan-count)
      (skk-error "候補を取り出すことができません"
		 "Cannot get current candidate")
    ;; (nth -1 '(A B C)) は、A を返すので、負でないかどうかチェックする。
    (nth skk-henkan-count skk-henkan-list)))

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
  ;; Unread single EVENT.
  (static-cond
   ((memq skk-emacs-type '(nemacs mule1))
    (setq unread-command-char event))
   (t (setq unread-command-events (nconc unread-command-events (list event))))))

(defsubst skk-get-last-henkan-datum (key)
  (cdr (assq key skk-last-henkan-data)))

(defsubst skk-put-last-henkan-datum (key val)
  (let ((e (assq key skk-last-henkan-data)))
    (if e
	(setcdr e val)
      (setq skk-last-henkan-data (cons (cons key val) skk-last-henkan-data)))))

(defsubst skk-put-last-henkan-data (alist)
  (let (kv e)
    (while (setq kv (car alist))
      (if (setq e (assq (car kv) skk-last-henkan-data))
	  (setcdr e (cdr kv))
	(setq skk-last-henkan-data
	      (cons (cons (car kv) (cdr kv)) skk-last-henkan-data)))
      (setq alist (cdr alist)))))

(defsubst skk-find-coding-system (code)
  (cond ((and code
	      (or (and (fboundp 'coding-system-p) 
		       (coding-system-p code))
		  (and (fboundp 'find-coding-system)
		       (symbolp code)
		       (find-coding-system code))))
	 code)
	((and code (stringp code))
	 (cdr (assoc code skk-coding-system-alist)))
	(t (cdr (assoc "euc" skk-coding-system-alist)))))

(defsubst skk-lisp-prog-p (string)
  ;; STRING が Lisp プログラムであれば、t を返す。
  (let ((l (skk-str-length string)))
    (and (> l 2) (eq (aref string 0) ?\()
	 ;; second character is ascii or not.
	 (skk-ascii-char-p (aref string 1))
         (eq (skk-str-ref string (1- l)) ?\)))))

(defsubst skk-eval-string (string)
  ;; eval STRING as a lisp program and return the result.
  (let (func)
    ;; (^_^;) のような文字列に対し、read-from-string を呼ぶとエラーになるの
    ;; で、condition-case でそのエラーを捕まえる。
    (condition-case nil
	(setq func (car (read-from-string string)))
      (error (setq func string)))
    (condition-case nil
	(and (listp func) (functionp (car func))
	     (setq string (eval func)))
      (error))
    string))

;;; This function is not complete, but enough for our purpose.
(defsubst skk-local-variable-p (variable &optional buffer afterset)
  "Non-nil if VARIABLE has a local binding in buffer BUFFER.
BUFFER defaults to the current buffer."
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (local-variable-p variable (or buffer (current-buffer)) afterset))
   ((fboundp 'local-variable-p)
    (local-variable-p variable (or buffer (current-buffer))))
   (t
    (and
     (or (assq variable (buffer-local-variables buffer)) ; local and bound.
	 (memq variable (buffer-local-variables buffer))); local but void.
     ;; docstring is ambiguous; 20.3 returns bool value.
     t))))

(defsubst skk-face-proportional-p (face)
  (static-cond
   ((fboundp 'face-proportional-p)
    (face-proportional-p face))
   (t
    nil)))

(defsubst skk-mode-string-to-indicator (mode string)
  (cond
   ((eq skk-status-indicator 'left)
    (static-cond
     ((eq skk-emacs-type 'xemacs)
      (cons (symbol-value (intern (format "skk-xemacs-%s-extent" mode)))
	    string))
     ((memq skk-emacs-type '(mule5))
      (apply 'propertize string skk-e21-modeline-property))
     (t
      string)))
   (t
    string)))

(defsubst skk-indicator-to-string (indicator &optional no-properties)
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

;;;; from dabbrev.el.  Welcome!
;; 判定間違いを犯す場合あり。要改良。
(defsubst skk-minibuffer-origin ()
  (nth 1 (buffer-list)))

(defsubst skk-quote-semicolon (word)
  (format "(concat \"%s\")"
 	  (mapconcat
 	   (function (lambda (c) (if (eq c ?\;) "\\073" (char-to-string c))))
 	   (append word nil) "")))

(require 'product)
(product-provide (provide 'skk-macs) (require 'skk-version))
;;; end of skk-macs.el.
