;;; skk-kcode.el --- 文字コードを使った変換のためのプログラム -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999, 2000
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Copyright (C) 1998-2010 SKK Development Team <skk@ring.gr.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-kcode.el,v 1.76 2010/12/21 03:23:21 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2010/12/21 03:23:21 $

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

;;; Code:

(eval-when-compile
  (require 'font-lock)
  (require 'skk-macs)
  (require 'skk-vars)
  (defvar enable-recursive-minibuffers)
  (defvar message-log-max))

(require 'skk-tankan)

;;;###autoload
(defun skk-input-by-code-or-menu (&optional arg)
  "7/8 bit JIS コード もしくは 区点番号に対応する文字を挿入する。"
  (interactive "*P")
  (when arg
    (setq skk-kcode-charset (intern (completing-read "Character set: "
						     '(("japanese-jisx0213-1")
						       ("japanese-jisx0213-2")
						       ("japanese-jisx0208"))
						     nil t
						     (symbol-name skk-kcode-charset)))))
  (let* ((str (read-string (format "\
7/8 bits JIS code (00nn) or KUTEN code (00-00) for `%s' (CR for Jump Menu): "
				   skk-kcode-charset)))
	 (list (split-string str "-"))
	 (len (length list))
	 (enable-recursive-minibuffers t)
	 n1 n2 flag)
    (cond ((eq len 2)			; ハイフン `-' で区切られた「区-点」
	   (setq n1 (+ (string-to-number (nth 0 list))
		       32 128)
		 n2 (+ (string-to-number (nth 1 list))
		       32 128)))
	  ((eq len 3)			; ハイフン `-' で区切られた「面-区-点」
	   (setq flag (if (equal "2" (nth 0 list))
			  'x0213-2
			'x0213-1)
		 n1 (+ (string-to-number (nth 1 list))
		       32 128)
		 n2 (+ (string-to-number (nth 2 list))
		       32 128)))
	  ((string-match "^[uU]\\+\\(.*\\)$" str) ; `U+' で始まればユニコード
	   (setq flag 'unicode
		 n1 161
		 n2 0
		 str (string-to-number (match-string-no-properties 1 str) 16)))
	  (t				; 上記以外は JIS コードとみなす
	   (setq n1 (if (string= str "")
			128
		      (+ (* 16 (skk-char-to-hex (aref str 0) 'jis))
			 (skk-char-to-hex (aref str 1))))
		 n2 (if (string= str "")
			128
		      (+ (* 16 (skk-char-to-hex (aref str 2) 'jis))
			 (skk-char-to-hex (aref str 3)))))))
    ;;
    (when (or (> n1 256)
	      (> n2 256))
      (skk-error "無効なコードです"
		 "Invalid code"))
    (insert (if (> n1 160)
		(cond ((eq flag 'x0213-1)
		       (char-to-string (skk-make-char 'japanese-jisx0213-1 n1 n2)))
		      ((eq flag 'x0213-2)
		       (char-to-string (skk-make-char 'japanese-jisx0213-2 n1 n2)))
		      ((eq flag 'unicode)
		       (char-to-string
			(if (eval-when-compile (fboundp
						'ucs-representation-decoding-backend))
			    (ucs-representation-decoding-backend 'ucs str nil)
			  str)))
		      (t
		       (skk-make-string n1 n2)))
	      (skk-input-by-code-or-menu-0 n1 n2)))
    (when (eq skk-henkan-mode 'active)
      (skk-kakutei))))

(defun skk-char-to-hex (char &optional jischar)
  "CHAR を 16 進数とみなして、対応する数値を 10 進数で返す。"
  (cond
   ;; a(97) -- f(102)
   ((and (<= 97 char) (<= char 102))
    (- char 87))			; a なら 10 が、f なら 15 が返る。
   ;; A(65) -- F(70)
   ((and (<= 65 char) (<= char 70))
    (- char 55))			; A なら 10 が、F なら 15 が返る。
   ;; 0(48) -- 9(57)
   ((and (<= 48 char) (<= char 57))
    (if jischar
	(- char 40)			; 0 なら 8 が、9 なら 17 が返る。
      (- char 48)))			; 0 なら 0 が、9 なら 9 が返る。
   (t
    (skk-error "`%c' を 16 進数に変換できません"
	       "Cannot convert `%c' to hexadecimal number"
	       char))))

(defun skk-make-string (n1 n2)
  "skk-kcode-charset が示す文字集合に従って n1 n2 に対応する STRING を返す"
  (char-to-string (skk-make-char skk-kcode-charset n1 n2)))

;; tiny function, but called once in skk-kcode.el.  So not make it inline.
(defun skk-make-char (charset n1 n2)
  (cond ((eval-when-compile (featurep 'xemacs))
	 (make-char charset
		    (logand (lognot 128) n1)
		    (logand (lognot 128) n2)))
	(t
	 (make-char charset n1 n2))))

(defun skk-next-n2-code (n)
  (if (<= (setq n (1+ n)) skk-code-n2-max)
      n
    skk-code-n2-min))

(defun skk-previous-n2-code (n)
  (if (<= skk-code-n2-min (setq n (1- n)))
      n
    skk-code-n2-max))

(defun skk-next-n1-code (n)
  (if (<= (setq n (1+ n)) skk-code-n1-max)
      n
    skk-code-n1-min))

(defun skk-previous-n1-code (n)
  (if (<= skk-code-n1-min (setq n (1- n)))
      n
    skk-code-n1-max))

(defun skk-input-by-code-or-menu-0 (n1 n2)
  (if (= n1 skk-code-null)
      (skk-input-by-code-or-menu-jump n2)
    (skk-input-by-code-or-menu-1 n1 n2)))

(defun skk-input-by-code-or-menu-jump (n)
  (let ((menu-keys1 (mapcar #'(lambda (char) ; 表示用のキーリスト
				(skk-char-to-unibyte-string (upcase char)))
			    skk-input-by-code-menu-keys1))
	kanji-char)
    (when (< n skk-code-n1-min)
      (setq n skk-input-by-code-or-menu-jump-default))
    (while (not kanji-char)
      (let ((n-org n)
	    (chars (list
		    (list (skk-make-string n skk-code-n1-min)
			  n skk-code-n1-min)
		    (list (skk-make-string n 177) n 177)
		    (list (skk-make-string n 193) n 193)
		    (list (skk-make-string n 209) n 209)
		    (list (skk-make-string n 225) n 225)
		    (list (skk-make-string n 241) n 241)
		    (progn
		      (setq n (skk-next-n1-code n))
		      (list (skk-make-string n skk-code-n1-min)
			    n skk-code-n1-min))
		    (list (skk-make-string n 177) n 177)
		    (list (skk-make-string n 193) n 193)
		    (list (skk-make-string n 209) n 209)
		    (list (skk-make-string n 225) n 225)
		    (list (skk-make-string n 241) n 241))))
	(skk-save-point
	 (let ((i 0)
	       message-log-max str)
	   (while (< i 12)
	     (setq str (concat
			str
			(propertize (nth i menu-keys1) 'face
				    'skk-henkan-show-candidates-keys-face)
			":"
			(car (nth i chars))
			(if (and skk-show-tooltip (= i 5))
			    "\n"
			  "  "))
		   i (1+ i)))
	   (if skk-show-tooltip
	       (funcall skk-tooltip-function str)
	     (message str)))
	 ;;
	 (let* ((event (next-command-event))
		(char (event-to-character event))
		(key (skk-event-key event))
		rest ch)
	   (cond
	    ((skk-key-binding-member key skk-quit-commands skk-j-mode-map)
	     (signal 'quit nil))
	    ;;
	    ((not (characterp char))
	     (skk-message "`%s' は無効なキーです！"
			  "`%s' is not valid here!"
			  (or (key-description key)
			      (key-description char)))
	     (sit-for 1)
	     (message "")
	     (setq n n-org))
	    ;;
	    ((setq rest (or (memq char skk-input-by-code-menu-keys1)
			    (if (skk-lower-case-p char)
				(memq (upcase char)
				      skk-input-by-code-menu-keys1)
			      (memq (downcase char)
				    skk-input-by-code-menu-keys1)))
		   ch (if rest
			  ;; 12 == (length skk-input-by-code-menu-keys1)
			  (nth (- 12 (length rest)) chars)
			nil))
	     (setq kanji-char ch))
	    ;;
	    ((or (skk-key-binding-member (skk-char-to-unibyte-string char)
					 '(skk-previous-candidate))
		 (and (not skk-delete-implies-kakutei)
		      (eq 'skk-delete-backward-char
			  (lookup-key skk-j-mode-map (vector char)))))
	     (when (< (setq n (- n-org 2)) skk-code-n1-min)
	       (setq n skk-code-n1-max)))
	    ;;
	    ((eq char skk-start-henkan-char) ; space
	     (setq n (skk-next-n1-code n)))
	    ;;
	    ((eq char ?\?)
	     (skk-message
	      "\
`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d) [何かキーを押してください]"
	      "\
`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d) [Hit any key to continue]"
	      (car (car chars))
	      n-org skk-code-n1-min
	      n-org skk-code-n1-min
	      (- n-org 128) (- skk-code-n1-min 128)
	      (- n-org 128) (- skk-code-n1-min 128))
	     (next-command-event)
	     (setq n n-org))
	    ;;
	    (t
	     (skk-message "`%c' は無効なキーです！"
			  "`%c' is not valid here!"
			  char)
	     (sit-for 1)
	     (message "")
	     (setq n n-org)
	     nil))))))
    (setq skk-input-by-code-or-menu-jump-default
	  (car (cdr kanji-char)))
    (skk-input-by-code-or-menu-1 (car (cdr kanji-char))
				 (car (cdr (cdr kanji-char))))))

(defun skk-input-by-code-or-menu-1 (n1 n2)
  (let ((menu-keys2 (mapcar #'(lambda (char) ; 表示用のキーリスト
				(skk-char-to-unibyte-string (upcase char)))
			    skk-input-by-code-menu-keys2))
	kanji-char)
    (while (not kanji-char)
      (let ((n1-org n1)
	    (n2-org n2)
	    (i 0)
	    (chars (list (skk-make-string n1 n2))))
	;; 16 == (length skk-input-by-code-menu-keys2)
	(while (< i (1- 16))
	  (nconc chars (list (progn
			       (setq n2 (skk-next-n2-code n2))
			       (if (= n2 skk-code-n2-min)
				   (setq n1 (skk-next-n1-code n1)))
			       (skk-make-string n1 n2))))
	  (setq i (1+ i)))
	(skk-save-point
	 (let ((i 0)
	       message-log-max str)
	   (while (< i 16)
	     (setq str (concat
			str
			(propertize (nth i menu-keys2) 'face
				    'skk-henkan-show-candidates-keys-face)
			":"
			(nth i chars)
			(if (and skk-show-tooltip (= i 8))
			    "\n"
			  " "))
		   i (1+ i)))
	   (if skk-show-tooltip
	       (funcall skk-tooltip-function str)
	     (message str)))
	 (let* ((event (next-command-event))
		(char (event-to-character event))
		(key (skk-event-key event))
		rest ch)
	   (cond
	    ((skk-key-binding-member key skk-quit-commands skk-j-mode-map)
	     (signal 'quit nil))
	    ((not (characterp char))
	     (skk-message "`%s' は無効なキーです！" "`%s' is not valid here!"
			  (or (key-description key) (key-description char)))
	     (sit-for 1)
	     (message "")
	     (setq n1 n1-org n2 n2-org))
	    ((setq rest (or (memq char skk-input-by-code-menu-keys2)
			    (if (skk-lower-case-p char)
				(memq (upcase char)
				      skk-input-by-code-menu-keys2)
			      (memq (downcase char)
				    skk-input-by-code-menu-keys2)))
		   ch (when rest
			;; 16 == (length skk-input-by-code-menu-keys2)
			(nth (- 16 (length rest)) chars)))
	     (setq kanji-char ch))
	    ((or (skk-key-binding-member (skk-char-to-unibyte-string char)
					 '(skk-previous-candidate))
		 (and (not skk-delete-implies-kakutei)
		      (eq 'skk-delete-backward-char
			  (lookup-key skk-j-mode-map(vector char)))))
	     (when (< (setq n2 (- n2 31)) skk-code-n2-min)
	       (setq n2 (+ n2 94)
		     n1 (skk-previous-n1-code n1))))
	    ;;
	    ((eq char skk-start-henkan-char) ; space
	     (if (= (setq n2 (skk-next-n2-code n2))
		    skk-code-n2-min)
		 (setq n1 (skk-next-n1-code n1))))
	    ;;
	    ((eq char ?\?)
	     (skk-message
	      "\
`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d) [何かキーを押してください]"
	      "\
`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d) [Hit any key to continue]"
	      (car chars)
	      n1-org n2-org
	      n1-org n2-org
	      (- n1-org 128) (- n2-org 128)
	      (- n1-org 128) (- n2-org 128))
	     (next-command-event)
	     (setq n1 n1-org n2 n2-org))
	    ;;
	    ((eq char ?>)
	     (if (= (setq n2 (skk-next-n2-code n2-org))
		    skk-code-n2-min)
		 (setq n1 (skk-next-n1-code n1-org))
	       (setq n1 n1-org)))
	    ;;
	    ((eq char ?<)
	     (if (= (setq n2 (skk-previous-n2-code n2-org))
		    skk-code-n2-max)
		 (setq n1 (skk-previous-n1-code n1-org))
	       (setq n1 n1-org)))
	    ;;
	    (t
	     (skk-message "`%c' は無効なキーです！"
			  "`%c' is not valid here!"
			  char)
	     (sit-for 1)
	     (message "")
	     (setq n1 n1-org n2 n2-org)))))))
    kanji-char))

;;;###autoload
(defun skk-display-code-for-char-at-point (&optional arg)
  "ポイントにある文字の区点番号、JIS コード、EUC コード及びシフト JIS コード\
 を表示する。"
  (if (eobp)
      (and (skk-message "カーソルがバッファの終端にあります"
			"Cursor is at the end of the buffer")
	   t) ; エコーした文字列をカレントバッファに挿入しないように。
    (skk-display-code (following-char))
    t))

(defun skk-display-code (char)
  (require 'font-lock)
  (let* ((charset (if (eval-when-compile (and skk-running-gnu-emacs
					      (>= emacs-major-version 23)))
		      ;; GNU Emacs 23.1 or later
		      (char-charset char skk-charset-list)
		    (char-charset char)))
	 mesg)
    (cond
     ((memq charset '(japanese-jisx0213-1
		      japanese-jisx0213-2
		      japanese-jisx0208
		      japanese-jisx0208-1978))
      (let* ((char1-j (skk-char-octet char 0))
	     (char1-k (- char1-j 32))
	     (char1-e (+ char1-j 128))
	     (char2-j (skk-char-octet char 1))
	     (char2-k (- char2-j 32))
	     (char2-e (+ char2-j 128))
	     (sjis (if (eq charset 'japanese-jisx0213-2)
		       (skk-jis2sjis2 char1-j char2-j)
		     (skk-jis2sjis char1-j char2-j)))
	     (char1-s (car sjis))
	     (char2-s (cadr sjis))
	     (char-data (skk-tankan-get-char-data char))
	     (anno (skk-tankan-get-char-annotation char))
	     (unicode (cond ((eval-when-compile
			       (and skk-running-gnu-emacs
				    (>= emacs-major-version 23)))
			     (concat ", "
				     (propertize "UNICODE:" 'face
						 'skk-display-code-prompt-face)
				     (format "U+%04x" char)))
			    ((and (eval-when-compile (fboundp 'char-to-ucs))
				  (char-to-ucs char))
			     (concat ", "
				     (propertize "UNICODE:" 'face
						 'skk-display-code-prompt-face)
				     (format "U+%04x" (char-to-ucs char))))
			    (t
			     ""))))
	;;
	(setq mesg
	      (concat (propertize (char-to-string char)
				  'face 'skk-display-code-char-face)
		      "\t"
		      (propertize "KUTEN:" 'face 'skk-display-code-prompt-face)
		      (if (eq charset 'japanese-jisx0213-2)
			  "(plane2)" "")
		      (format "%02d-%02d, " char1-k char2-k)

		      (propertize "JIS:" 'face 'skk-display-code-prompt-face)
		      (format "#x%2x%2x, " char1-j char2-j)

		      (propertize "EUC:" 'face 'skk-display-code-prompt-face)
		      (format "#x%2x%2x, " char1-e char2-e)

		      (propertize "SJIS:" 'face 'skk-display-code-prompt-face)
		      (format "#x%2x%2x" char1-s char2-s)

		      unicode
		      (if (zerop (nth 2 char-data))
			  ""
			(concat ", "
				(propertize
				 (format "総%d画（%s部 %d画）"
					 (nth 2 char-data)
					 (aref skk-tankan-radical-vector
					       (nth 0 char-data))
					 (nth 1 char-data))
				 'face 'font-lock-string-face)))
		      (if anno
			  (concat ", "
				  (propertize anno
					      'face 'font-lock-string-face)))
			   ))))
     ;;
     ((memq charset '(ascii latin-jisx0201))
      (setq mesg
	    (concat (format "`%c', " char)
		    (propertize "HEX:" 'face 'skk-display-code-prompt-face)
		    (format "#x%2x, " (skk-char-octet char 0))
		    (propertize "DECIMAL:" 'face 'skk-display-code-prompt-face)
		    (format "%3d" (skk-char-octet char 0)))))
     ;;
     (t
      (skk-error "判別できない文字です"
		 "Cannot understand this character")))
    ;;
    (if (and window-system
	     skk-show-tooltip
	     (not (eq (symbol-function 'skk-tooltip-show-at-point) 'ignore)))
	(funcall skk-tooltip-function (mapconcat #'(lambda (x) x)
						 (split-string mesg ", ")
						 "\n\t"))
      (message mesg))))

(defun skk-jis2sjis (char1 char2)
  (let* ((ch2 (if (eq (* (/ char1 2) 2) char1)
		  (+ char2 125) (+ char2 31)))
	 (c2 (if (>= ch2 127)
		 (+ ch2 1) ch2))
	 (ch1 (+ (/ (- char1 33) 2) 129))
	 (c1 (if (> ch1 159)
		  (+ ch1 64) ch1)))
    (list c1 c2)))

(defun skk-sjis2jis (char1 char2)
  (let* ((ch1 (if (<= char1 159) (+ (* (- char1 113) 2) 1)
		(+ (* (- char1 177) 2) 1)))
	 (ch2 (if (> char2 127) (- char2 1) char2))
	 (c2 (if (>= ch2 158) (- ch2 125) (- ch2 31)))
	 (c1 (if (> ch2 127) (+ ch1 1) ch1)))
    (list c1 c2)))

;;;; skk-list-chars
;; TODO
;;   o mode-line に現在 charset を表示したい
;;   o 挿入先のバッファ skk-list-chars-destination-buffer が存在しない場合の対処
;;   o キー操作 a, e で区の先頭/末尾へ
;;   o キー操作 <, > で先頭/末尾へ

(defun skk-list-chars-mode ()
  "Major mode for skk-list-chars.

\\{skk-list-chars-mode-map}"
  (kill-all-local-variables)
  (use-local-map skk-list-chars-mode-map)
  (setq mode-name "skk-list-chars"
	major-mode 'skk-list-chars-mode)
  (setq tab-width 4))

(defun skk-list-chars-sub (high charset)
  "Docstring."
  (let ((min 33)			;?\x21
	(max 126)			;?\x7e
	i ch)
    (insert "\n"
	    (propertize
	     (format "%02d-#x--- 0-- 1-- 2-- 3-- 4-- 5-- 6-- 7-- 8-- 9-- A-- B-- C-- D-- E-- F" (- high 32)) 'face 'font-lock-comment-face))
    (setq i (* (/ min 16) 16))		; i は 下位バイト
    (while (<= i max)			; 0x21 .. 0x7e
      (when (= (% i 16) 0)
	(insert (propertize (format "\n %5X0" (/ (+ (* high 256)
						   i)
						16))
			    'face 'font-lock-comment-face)))
      (setq ch (if (< i min)
		   32
		 (or (make-char charset (/ (* high 256) 256) i)
		     32)))
      (insert "\t" (propertize (char-to-string ch)
			       'mouse-face
			       'highlight))
      (setq i (1+ i)))))

;;;###autoload
(defun skk-list-chars (arg)
  "Docstring."
  (require 'font-lock)
  (let ((buf (progn (and (get-buffer skk-list-chars-buffer-name)
			 (kill-buffer skk-list-chars-buffer-name))
		    (get-buffer-create skk-list-chars-buffer-name)))
	(ch (char-to-string (if arg
				(following-char)
			      (make-char skk-kcode-charset 33 33)))))
    (setq skk-kcode-charset (if arg
				(car (split-char (string-to-char ch)))
			      skk-kcode-charset))
    (if (eq skk-kcode-charset 'ascii)
	(setq skk-kcode-charset 'japanese-jisx0208
	      ch (char-to-string (make-char skk-kcode-charset 33 33))))
    (skk-kakutei)			; ▽ or ▼ で \ した場合
    (setq skk-list-chars-destination-buffer (current-buffer))
    (set-buffer buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-buffer-multibyte t)
    (insert (propertize (format "variable skk-kcode-charset's value is `%s'.\n"
				skk-kcode-charset)
			'face font-lock-doc-face))

    (let ((high 33))			; ?\x21
      (while (<= high 126)		; ?\x7e
	(skk-list-chars-sub high skk-kcode-charset)
	(setq high (1+ high))))
    (pop-to-buffer buf)
    (search-backward ch)
    (setq skk-list-chars-point (point))
    (put-text-property skk-list-chars-point (progn (forward-char) (point))
    		       'face 'font-lock-warning-face)
    (goto-char skk-list-chars-point)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (skk-list-chars-mode)))

(defun skk-list-chars-quit ()
  (interactive)
  (if (one-window-p)
      (switch-to-buffer skk-list-chars-destination-buffer) ;killされている可能性あり
    (delete-window)))

(defun skk-list-chars-display-code ()
  (interactive)
  (let ((c (following-char)))
    (if (eq 'ascii (car (split-char c)))
	;; 区切り行などで $ された場合
	(next-completion 1)
      (skk-display-code c))))

(defun skk-list-chars-copy ()
  (interactive)
  (unless (eobp)
    (message "`%s' copied." 
	     (kill-new (char-to-string (following-char))))))

(defun skk-list-chars-next-line ()
  (interactive)
  (let ((col (current-column)))
    (forward-line)
    (if (eq 'ascii (car (split-char (following-char))))
	(next-completion 1))
    (move-to-column col)))

(defun skk-list-chars-previous-line ()
  (interactive)
  (let ((col (current-column)))
    (forward-line -1)
    (if (eq 'ascii (car (split-char (following-char))))
	(next-completion -1))
    (move-to-column col)))

(defun skk-list-chars-goto-point ()
  (interactive)
  (goto-char skk-list-chars-point))

(defun skk-list-chars-insert ()
  (interactive)
  (if (eobp)
      (forward-char -1)
    (if (eq 'ascii (car (split-char (following-char))))
	;; 区切り行などで RET された場合
	(next-completion 1)
      (let ((c (following-char)))
	(set-buffer skk-list-chars-destination-buffer) ; kill されている可能性あり
	(insert c))
      )))

(defun skk-list-chars-other-charset ()
  (interactive)
  (setq skk-kcode-charset
	(intern-soft (completing-read "Character set: "
				      '(("japanese-jisx0213-1")
					("japanese-jisx0213-2")
					("japanese-jisx0208"))
				      nil t
				      (symbol-name skk-kcode-charset))))
  (skk-list-chars-quit)
  (skk-list-chars nil))

(defun skk-list-chars-code-input ()
  (interactive)
  (let ((code (read-string
	       (format "7/8 bits JIS code (00nn) or KUTEN code (00-00) or UNICODE (U+00nn) for `%s': " skk-kcode-charset))))
    (unless (string= code "")
      (skk-list-chars-code-input-1 code))))

(defun skk-list-chars-code-input-1 (code)
  (let ((str (cond
	      ;; ハイフン `-' で区切られた「区-点」
	      ((string-match "^\\([0-9]*[0-9]\\)-\\([0-9]*[0-9]\\)$" code)
	       (skk-make-string (+ (string-to-number (match-string 1 code))
				   32 128)
				(+ (string-to-number (match-string 2 code))
				   32 128)))
	      ;; `U+' で始まればユニコード
	      ((string-match "^[uU]\\+\\(.*\\)$" code)
	       (let ((char (string-to-number (match-string-no-properties 1 code) 16)))
		 (char-to-string
		  (cond ((eval-when-compile
			   (fboundp 'ucs-representation-decoding-backend))
			 (ucs-representation-decoding-backend 'ucs char nil))
			((>= emacs-major-version 23)
			 char)
			(t
			 32)))))
	      ;; 上記以外は JIS コードとみなす
	      (t				
	       (skk-make-string (+ (* 16 (skk-char-to-hex (aref code 0) 'jis))
				   (skk-char-to-hex (aref code 1)))
				(+ (* 16 (skk-char-to-hex (aref code 2) 'jis))
				   (skk-char-to-hex (aref code 3))))))
	     ))
    (save-current-buffer
      (set-buffer skk-list-chars-destination-buffer)
      (insert str))
    (goto-char (point-min))
    (search-forward str nil t)
    (forward-char -1))
  (when (eq skk-henkan-mode 'active)
    (skk-kakutei)))

;; 2面
;; XEmacs でのエラー回避のためにこの関数を一時 skk-emacs.el に退避する。
;; (autoload 'skk-jis2sjis2 "skk-emacs")
(when (eval-when-compile (featurep 'xemacs))
  (defalias 'skk-jis2sjis2 'ignore))

(run-hooks 'skk-kcode-load-hook)

(provide 'skk-kcode)

;;; skk-kcode.el ends here
