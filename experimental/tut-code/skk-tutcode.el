;;; skk-tutcode.el --- SKK TUT-code inputting program.
;; Copyright (C) 1999 GUNJI Takao <gunji@sils.shoin.ac.jp>

;; Author: GUNJI Takao <gunji@sils.shoin.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-tutcode.el,v 1.2 1999/08/21 13:22:50 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/08/21 13:22:50 $

;; This file is not part of SKK yet.

;; SKK is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Following people contributed modifications to skk-tutcode.el
;; (Alphabetical order):
;;      Mikio Nakajima <minakaji@osaka.email.ne.jp>
;;
;; INSTALL
;; 
;; Put the following two advices in your .emacs.
;;
;; (defadvice skk-mode (before my-ad activate)
;;   (require 'skk-tutcdef)
;;   (require 'skk-tutcode) )
;; 
;; (defadvice skk-auto-fill-mode (before my-ad activate)
;;   (require 'skk-tutcdef)
;;   (require 'skk-tutcode) )
;; 
;; If you would like to customize some definitions in skk-tutcdef.el,
;; you could do, for example;
;; 
;; (defadvice skk-mode (before my-ad activate)
;;   (require 'skk-tutcdef)
;;   (require 'skk-tutcode)
;;   ;; your customizations...
;;   (setq skk-rom-kana-rule-list
;;	 '(...) ))

;;; Code:
(eval-when-compile (require 'skk))

;; all prefix of functions, variables and constants are
;; `skk-tutcode-'.
;;
;;(defgroup skk-tutcode nil "SKK/TUT-code related customization."
;;  :prefix "skk-tutcode-"
;;  :group 'skk )

;;;###autoload
(defun skk-tutcode-mode-off (foo) 
  (skk-latin-mode t)
  (skk-insert-str "\\") )

;;;###autoload
(defun skk-tutcode-display-code (&optional arg)
  ;; adapted from skk-kcode.el
  "ポイントにある文字の EUC コード、JIS コード、TUT コードを表示する。"
  (interactive "P")
  (if (eobp)
      (skk-error "カーソルがバッファの終端にあります"
                 "Cursor is at the end of the buffer" )
    (skk-tutcode-display-code-1 
     (buffer-substring-no-properties
      (point)
      (skk-save-point (forward-char 1) (point)) ))
    ;; エコーした文字列をカレントバッファに挿入しないように。
    t ))

(skk-defun-cond skk-tutcode-display-code-1 ()
  ((memq skk-emacs-type '(xemacs mule4 mule3))
   (let* ((char (string-to-char str))
	  (charset (char-charset char)))
     (cond
      ((memq charset '(japanese-jisx0208 japanese-jisx0208-1978))
       (let* ((char1-j (char-octet char 0))
	      (char1-k (- char1-j 32))
	      (char1-e (+ char1-j 128))
	      (char2-j (char-octet char 1))
	      (char2-k (- char2-j 32))
	      (char2-e (+ char2-j 128))
	      (char3 (skk-tutcode-get-code str)))
	 (message
	  "『%s』  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d), KUTEN: (%2d, %2d), TUT: `%s'"
	  str char1-e char2-e char1-e char2-e
	  char1-j char2-j char1-j char2-j char1-k char2-k char3)))
      ((memq charset '(ascii latin-jisx0201))
       (message "\"%s\"  %2x (%3d)"
		str (char-octet char 0)  (char-octet char 0)))
      (t
       (skk-error "判別できない文字です"
		  "Cannot understand this character")))
     ))
  ;; 'mule2
  (t
   (let (
	 ;; 文字列を char に分解。
	 ;; (mapcar '+ str) == (append str nil)
	 (char-list (mapcar (function +) str)))
     (cond
      ((and (= (length char-list) 3)
	    (memq (car char-list) (list lc-jp lc-jpold)))
       (let* ((char1-e (car (cdr char-list)))
	      (char1-j (- char1-e 128))
	      (char1-k (- char1-j 32))
	      (char2-e (car (cdr (cdr char-list))))
	      (char2-j (- char2-e 128))
	      (char2-k (- char2-j 32))
	      (char3 (skk-tutcode-get-code str)))
	 (message
	  "『%s』  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d), KUTEN: (%2d, %2d), TUT: `%s'"
	  str char1-e char2-e char1-e char2-e
	  char1-j char2-j char1-j char2-j char1-k char2-k char3)))
      ((or (= (length char-list) 1)	; ascii character
	   (memq (car char-list) (list lc-ascii lc-roman)))
       (let ((char (car char-list)))
	 (message "\"%c\"  %2x (%3d)" char char char)))
      (t
       (skk-error "判別できない文字です"
		  "Cannot understand this character" ))))))

;; some new stuff
(defun skk-tutcode-get-code (key)
  (let ((srkr-list skk-rom-kana-rule-list) (cont t) (val nil))
    (while cont
      (if (null srkr-list)
	  (setq cont nil)
	(if (listp (car (cdr (cdr (car srkr-list)))))
	    (cond
	     ((string= key (car (car (cdr (cdr (car srkr-list))))))
	      (setq cont nil
		    val (car (car srkr-list))))
	     ((string= key (cdr (car (cdr (cdr (car srkr-list))))))
	      (setq cont nil
		    val (car (car srkr-list))))
	     (t (setq srkr-list (cdr srkr-list))))
	  (cond
	   ((string= key (car (cdr (cdr (car srkr-list)))))
	    (setq cont nil
		  val (car (car srkr-list))))
	   (t  (setq srkr-list (cdr srkr-list)))))))

    val))

(provide 'skk-tutcode)
;;; skk-tutcode.el ends here
