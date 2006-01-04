;;; skk-tutcode.el --- assist TUT-code inputting in SKK environment

;; Copyright (C) 1999, 2000, 2001 GUNJI Takao <gunji@sils.shoin.ac.jp>

;; Author: GUNJI Takao <gunji@sils.shoin.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-tutcode.el,v 1.12 2006/01/04 10:10:46 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2006/01/04 10:10:46 $

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
;; the Free Software Foundation Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; <INFORMATION>
;;
;; TUT-code was developed by Hajime Ohiwa <ohiwa@sfc.keio.ac.jp> and
;; Takaaki Takashima of Toyohashi University of Technology in 1982.
;; TUT-code is a kind of direct input method of Japanese with two
;; strokes.  You can pronounce it `Tea You Tea' or `TUT' like
;; Tutankamen's nickname, king `TUT'.
;;
;; At Mon, 23 Aug 1999 01:13:34 +0900,
;; Hajime Ohiwa <ohiwa@sfc.keio.ac.jp> wrote:
;;
;; > 私は「てぃーゆーてぃーこーど」と読んでいますが、もう少し言い易い方が
;; > よいと思います。TUTはこれを開発した豊橋技術科学大学の略称です。
;; > 米国人に見せたら、「たっと」と読みました。つたんかーめん王のことを英語
;; > では king TUT と呼び、発音は「たっと」なのだそうです。「たっとこーど」
;; > の方がよいかもしれません。御意見をお聞かせ下さい。
;;
;; To get more information, access following URL;
;;    http://www.crew.sfc.keio.ac.jp/~chk/
;;
;; To join TUT-code mailing list, send mail like following;
;;    To: tut-code-control@crew.sfc.keio.ac.jp
;;    Subject: APPEND
;;    Body: an introduction of yourself (null body is available).
;;
;; <INSTALL>
;;
;; Put the following lines in your .emacs.
;;
;; (defadvice skk-mode (before my-ad activate)
;;   (require 'skk-tutcdef)
;;   (require 'skk-tutcode))
;;
;; (defadvice skk-auto-fill-mode (before my-ad activate)
;;   (require 'skk-tutcdef)
;;   (require 'skk-tutcode))
;;
;; If you would like to customize some definitions in skk-tutcdef.el,
;; you could do, for example;
;;
;; (defadvice skk-mode (before my-ad activate)
;;   (require 'skk-tutcdef)
;;   ;; your customizations...
;;   (setq skk-rom-kana-rule-list
;;         '(...)))
;;   (require 'skk-tutcode))
;;
;; <TODO>
;; - Efficient mazegaki (e.x. provided by T-code driver) support.
;; - To switch easyly okurigana prefix in jisyo buffer.

;;; Code:

(eval-when-compile (require 'skk))

;;;###autoload
(defgroup skk-tutcode nil "SKK/TUT-code related customization."
  :prefix "skk-tutcode-"
  :group 'skk)

;; all prefix of functions, variables and constants are
;; `skk-tutcode-'.
;;;###autoload
(defcustom skk-tutcode-use-touch16+ nil
 "*Non-nil であれば、Touch16+ 拡張コードを利用する。"
 :type 'boolean
 :group 'skk-tutcode)

;;;###autoload
(defun skk-tutcode-mode-off (foo)
  (skk-latin-mode t)
  (skk-insert-str "\\"))

;;;###autoload
(defun skk-tutcode-display-code (&optional arg)
  ;; adapted from skk-kcode.el
  "ポイントにある文字の EUC コード、JIS コード、TUT コードを表示する。"
  (interactive "P")
  (if (eobp)
      (skk-error "カーソルがバッファの終端にあります"
		 "Cursor is at the end of the buffer")
    (skk-tutcode-display-code-1
     (buffer-substring-no-properties
      (point)
      (skk-save-point (forward-char 1) (point))))
    ;; エコーした文字列をカレントバッファに挿入しないように。
    t))

(defun skk-tutcode-display-code-1 (str)
  (static-cond
   ((memq skk-emacs-type '(xemacs mule5 mule4 mule3))
    (let* ((char (string-to-char str))
	   (charset (char-charset char))
	   (charset-list
	    '(japanese-jisx0208 japanese-jisx0208-1978)))
      (if (charsetp 'japanese-jisx0213-1)
	  (setq charset-list
		(nconc
		 (list 'japanese-jisx0213-1 'japanese-jisx0213-2)
		 charset-list)))
      (cond
       ((memq charset charset-list)
	(let* ((char1-j (skk-char-octet char 0))
	       (char1-k (- char1-j 32))
	       (char1-e (+ char1-j 128))
	       (char2-j (skk-char-octet char 1))
	       (char2-k (- char2-j 32))
	       (char2-e (+ char2-j 128))
	       (char3 (skk-tutcode-get-code str)))
	  (message
	   "『%s』  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d), KUTEN: (%2d, %2d), TUT: `%s'"
	   str char1-e char2-e char1-e char2-e
	   char1-j char2-j char1-j char2-j char1-k char2-k char3)))
       ((memq charset '(ascii latin-jisx0201))
	(message "\"%s\"  %2x (%3d)"
		 str (skk-char-octet char 0)  (skk-char-octet char 0)))
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
		   "Cannot understand this character")))))))

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

(require 'product)
(product-provide
    (provide 'skk-tutcode)
  (require 'skk-version))

;;; skk-tutcode.el ends here
