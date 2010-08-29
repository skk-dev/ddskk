;;; skk-kcode.el --- 漢字コードを使った変換のためのプログラム -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999, 2000
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Copyright (C) 1998-2010 SKK Development Team <skk@ring.gr.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-kcode.el,v 1.47 2010/08/29 06:17:17 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2010/08/29 06:17:17 $

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
  (require 'static)
  (require 'skk-macs)
  (require 'skk-vars)
  (defvar enable-recursive-minibuffers)
  (defvar message-log-max))

(require 'skk-tankan)

;;;###autoload
(defun skk-input-by-code-or-menu (&optional arg)
  "7bit もしくは 8bit もしくは 区点コードに対応する 2byte 文字を挿入する。"
  (interactive "*P")
  (when arg
    (let ((charset
	   (intern (completing-read
		    (format "CHARSET(%s): "
			    skk-kcode-charset)
		    skk-kcode-charset-list
		    nil t))))
      (cond
       ((eq charset (intern ""))
	nil)
       ((not (skk-charsetp charset))
	(skk-error "無効なキャラクターセットです"
		   "Invalid charset"))
       (t
	(setq skk-kcode-charset charset)))))
  (let ((str
	 (read-string
	  (format
	   "7/8 bits or KUTEN code for %s (00nn or CR for Jump Menu): "
	   skk-kcode-charset)))
	(enable-recursive-minibuffers t)
	n1 n2)
    (if (string-match "\\(.+\\)-\\(.+\\)" str)
	(setq n1 (+ (string-to-number (match-string-no-properties 1 str))
		    32
		    128)
	      n2 (+ (string-to-number (match-string-no-properties 2 str))
		    32
		    128))
      (setq n1 (if (string= str "")
		   128
		 (+ (* 16 (skk-char-to-hex (aref str 0) 'jis))
		    (skk-char-to-hex (aref str 1))))
	    n2 (if (string= str "")
		   128
		 (+ (* 16 (skk-char-to-hex (aref str 2) 'jis))
		    (skk-char-to-hex (aref str 3))))))
    (when (or (> n1 256)
	      (> n2 256))
      (skk-error "無効なコードです"
		 "Invalid code"))
    (insert (if (> n1 160)
		(skk-make-string n1 n2)
	      (skk-input-by-code-or-menu-0 n1 n2)))
    (when (eq skk-henkan-mode 'active)
      (skk-kakutei))))

(defun skk-char-to-hex (char &optional jischar)
  (cond ((and (<= char 102)
	      (> char 96))
	 ;; a-f
	 (- char 87))
	((and (<= char 70)
	      (> char 64))
	 ;; A-F
	 (- char 55))
	((and (<= char 57)
	      (> char 47))
	 ;; 0-9
	 (cond (jischar
		(- char 40))
	       (t
		(- char 48))))
	(t
	 (skk-error "%c を 16 進数に変換できません"
		    "Cannot convert %c to hexadecimal number"
		    char))))

(defun skk-make-string (n1 n2)
  (char-to-string (skk-make-char skk-kcode-charset n1 n2)))

;; tiny function, but called once in skk-kcode.el.  So not make it inline.
(defun skk-make-char (charset n1 n2)
  (static-cond
   ((featurep 'xemacs)
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
  (let ((menu-keys1 ; 表示用のキーリストを組み立てる。
	 (mapcar (function
		  (lambda (char)
		    (skk-char-to-unibyte-string (upcase char))))
		 skk-input-by-code-menu-keys1))
	kanji-char)
    (when (< n skk-code-n1-min)
      (setq n skk-input-by-code-or-menu-jump-default))
    (while (not kanji-char)
      (let ((n-org n)
	    (chars
	     (list
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
	       message-log-max
	       str)
	   (while (< i 12)
	     (setq str (concat str
			       (propertize (nth i menu-keys1) 'face
					   'skk-henkan-show-candidates-keys-face)
			       ":"
			       (car (nth i chars))
			       "  "))
	     (setq i (1+ i)))
	   (message "%s" str))
	 (let ((char (event-to-character (next-command-event)))
	       rest ch)
	   (if (not (characterp char))
	       (progn
		 (skk-message "`%s' は有効なキーではありません！"
			      "`%s' is not valid here!"
			      (prin1 char))
		 (sit-for 1)
		 (message "")
		 (setq n n-org))
	     (setq rest (or (memq char skk-input-by-code-menu-keys1)
			    (if (skk-lower-case-p char)
				(memq (upcase char)
				      skk-input-by-code-menu-keys1)
			      (memq (downcase char)
				    skk-input-by-code-menu-keys1)))
		   ch (if rest
			  ;; 12 == (length skk-input-by-code-menu-keys1)
			  (nth (- 12 (length rest)) chars)
			nil)
		   kanji-char
		   (cond
		    (ch
		     ch)
		    ((or (eq char skk-previous-candidate-char)
			 (and (not skk-delete-implies-kakutei)
			      (eq 'skk-delete-backward-char
				  (lookup-key skk-j-mode-map (vector char)))))
		     (when (< (setq n (- n-org 2)) skk-code-n1-min)
		       (setq n skk-code-n1-max))
		     nil)
		    ((eq char skk-start-henkan-char) ; space
		     (setq n (skk-next-n1-code n))
		     nil)
		    ((eq char ?\?)
		     (skk-message
		      "`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)\
 [何かキーを押してください]"
		      "
`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)\
 [Hit any key to continue]"
		      (car (car chars))
		      n-org skk-code-n1-min
		      n-org skk-code-n1-min
		      (- n-org 128) (- skk-code-n1-min 128)
		      (- n-org 128) (- skk-code-n1-min 128))
		     (next-command-event)
		     (setq n n-org)
		     nil)
		    (t
		     (skk-message "`%c' は有効なキーではありません！"
				  "`%c' is not valid here!"
				  char)
		     (sit-for 1)
		     (message "")
		     (setq n n-org)
		     nil))))))))
    (setq skk-input-by-code-or-menu-jump-default
	  (car (cdr kanji-char)))
    (skk-input-by-code-or-menu-1
     (car (cdr kanji-char))
     (car (cdr (cdr kanji-char))))))

(defun skk-input-by-code-or-menu-1 (n1 n2)
  (let ((menu-keys2 ; 表示用のキーリストを組み立てる。
	 (mapcar (function (lambda (char) (skk-char-to-unibyte-string (upcase char))))
		 skk-input-by-code-menu-keys2))
	kanji-char)
    (while (not kanji-char)
      (let ((n1-org n1) (n2-org n2) (i 0)
	    (chars (list (skk-make-string n1 n2))))
	;; 16 == (length skk-input-by-code-menu-keys2)
	(while (< i (1- 16))
	  (nconc chars (list
			(progn
			  (setq n2 (skk-next-n2-code n2))
			  (if (= n2 skk-code-n2-min)
			      (setq n1 (skk-next-n1-code n1)))
			  (skk-make-string n1 n2))))
	  (setq i (1+ i)))
	(skk-save-point
	 (let ((i 0) message-log-max str)
	   (while (< i 16)
	     (setq str (concat str
			       (propertize (nth i menu-keys2) 'face
					   'skk-henkan-show-candidates-keys-face)
			       ":"
			       (nth i chars)
			       " "))
	     (setq i (1+ i)))
	   (message str))
	 (let ((char (event-to-character (next-command-event)))
	       rest ch)
	   (if (not (characterp char))
	       (progn
		 (skk-message "`%s' は有効なキーではありません！"
			      "`%s' is not valid here!"
			      (prin1 char))
		 (sit-for 1)
		 (message "")
		 (setq n1 n1-org n2 n2-org))
	     (setq rest
		   (or (memq char skk-input-by-code-menu-keys2)
		       (if (skk-lower-case-p char)
			   (memq (upcase char)
				 skk-input-by-code-menu-keys2)
			 (memq (downcase char)
			       skk-input-by-code-menu-keys2)))
		   ch (when rest
			;; 16 == (length skk-input-by-code-menu-keys2)
			(nth (- 16 (length rest)) chars))
		   kanji-char
		   (cond
		    (ch
		     ch)
		    ((or (eq char skk-previous-candidate-char)
			 (and (not skk-delete-implies-kakutei)
			      (eq 'skk-delete-backward-char
				  (lookup-key skk-j-mode-map (vector char)))))
		     (when (< (setq n2 (- n2 31)) skk-code-n2-min)
		       (setq n2 (+ n2 94)
			     n1 (skk-previous-n1-code n1)))
		     nil)
		    ((eq char skk-start-henkan-char) ; space
		     (if (= (setq n2 (skk-next-n2-code n2))
			    skk-code-n2-min)
			 (setq n1 (skk-next-n1-code n1)))
		     nil)
		    ((eq char ?\?)
		     (skk-message
		      "`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)\
 [何かキーを押してください]"
		      "`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)\
  [Hit any key to continue]"
		      (car chars)
		      n1-org n2-org
		      n1-org n2-org
		      (- n1-org 128) (- n2-org 128)
		      (- n1-org 128) (- n2-org 128))
		     (next-command-event)
		     (setq n1 n1-org n2 n2-org)
		     nil)
		    ((eq char ?>)
		     (if (= (setq n2 (skk-next-n2-code n2-org))
			    skk-code-n2-min)
			 (setq n1 (skk-next-n1-code n1-org))
		       (setq n1 n1-org))
		     nil)
		    ((eq char ?<)
		     (if (= (setq n2 (skk-previous-n2-code n2-org))
			    skk-code-n2-max)
			 (setq n1 (skk-previous-n1-code n1-org))
		       (setq n1 n1-org))
		     nil)
		    (t
		     (skk-message "`%c' は有効なキーではありません！"
				  "`%c' is not valid here!"
				  char)
		     (sit-for 1)
		     (message "")
		     (setq n1 n1-org n2 n2-org)
		     nil))))))))
    kanji-char))

;;;###autoload
(defun skk-display-code-for-char-at-point (&optional arg)
  "ポイントにある文字の区点コード、JIS コード、EUC コード及びシフト JIS コード を表示する。"
  (interactive "P")
  (when (eobp)
    (skk-error "カーソルがバッファの終端にあります"
	       "Cursor is at the end of the buffer"))
  (skk-display-code (buffer-substring-no-properties
		     (point) (skk-save-point
			      (forward-char 1)
			      (point))))
    ;; エコーした文字列をカレントバッファに挿入しないように。
    t)

(defun skk-display-code (str)
  (let* ((char (string-to-char str))
	 (charset (static-if (and (not (featurep 'xemacs))
				  (>= emacs-major-version 23))
		      ;; GNU Emacs 23.1 or later
		      (char-charset char skk-charset-list)
		    (char-charset char))))
    (cond
     ((memq charset '(japanese-jisx0213-1 japanese-jisx0213-2 japanese-jisx0208 japanese-jisx0208-1978))
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
	     (list (skk-tankan-get-char-data char))
	     (stroke (nth 2 list))
	     (anno (skk-tankan-get-char-annotation char))
	     )
	(if (eq charset 'japanese-jisx0213-2)
	    (message
	     "`%s' (plane 2) KUTEN: %02d-%02d, JIS: %2x%2x, EUC: %2x%2x, SJIS: %2x%2x"
	     str
	     char1-k char2-k
	     char1-j char2-j
	     char1-e char2-e
	     char1-s char2-s)
	  ;; 
	  (message
	   "`%s' KUTEN: %02d-%02d, JIS: %2x%2x, EUC: %2x%2x, SJIS: %2x%2x%s%s"
	   str
	   char1-k char2-k
	   char1-j char2-j 
	   char1-e char2-e 
	   char1-s char2-s
	   (if (= 0 stroke)
	       ""
	     (format ", 総%d画(%s部%d画)"
		     stroke
		     (aref skk-tankan-radical-vector (nth 0 list))
		     (nth 1 list)))
	   (if anno
	       (concat ", " anno)
	     "")
	   ))))
     ;;
     ((memq charset '(ascii latin-jisx0201))
      (message "`%s' HEX: %2x, DECIMAL: %3d"
	       str
	       (skk-char-octet char 0)
	       (skk-char-octet char 0)))
     ;;
     (t
      (skk-error "判別できない文字です"
		 "Cannot understand this character")))))

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

;; 2面
;; XEmacs でのエラー回避のためにこの関数を一時 skk-emacs.el に退避する。
;; (autoload 'skk-jis2sjis2 "skk-emacs")
(static-when (featurep 'xemacs)
  (defalias 'skk-jis2sjis2 'ignore))

(run-hooks 'skk-kcode-load-hook)

(provide 'skk-kcode)

;;; skk-kcode.el ends here
