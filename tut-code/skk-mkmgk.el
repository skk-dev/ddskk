;;; skk-mkmgk.el --- Make Mazegaki dictionary from SKK-JISYO.*
;; Copyright (C) 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-mkmgk.el,v 1.4 2001/07/28 10:51:00 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/07/28 10:51:00 $

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
;; along with Daredevil SKK, see the file COPYING.  If not, write to the
;; Free Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary
;;
;; M-x skk-make-mazegaki-dic して出力された辞書 (下記の例では TMP) を
;;
;;   $ skkdic-expr TMP | skkdic-sort > NEWDICT
;;
;; と加工すると混ぜ書き SKK 辞書 (NEWDICT) が出来上がります。

;;; Code:
(eval-when-compile (require 'skk-macs))
(require 'skk)

(defvar skk-mkmgk-region-limit 10000
  "*検索リージョンのサイズがこの数より小さい場合に、バイナリサーチを止めリニアサーチに移行する。
個人辞書を加工する場合は、0 に設定する。")

;;;###autoload
(defun skk-make-mazegaki-dic (dic &optional nomsg)
  "SKK 形式の辞書から TUT-Code などで使われれる混ぜ書き辞書を作る。
出力された辞書 (下記の例では TMP) を

  $ skkdic-expr TMP | skkdic-sort > NEWDICT

として加工する必要あり。"
  (interactive "f辞書ファイル:\nP ")
  (let ((cont t)
	(workbuf (get-buffer-create " *skkmkmgk working*"))
	max output)
    (while cont
      (setq output (read-file-name
		    "どこに出力しますか? "
		    nil
		    (convert-standard-filename "~/.skk-jisyo.tmp")))
      (if (or (not (file-exists-p output))
	      (yes-or-no-p
	       (format "%s に上書きしてよろしいですか? " output)))
	  (setq cont nil)))
    (with-current-buffer workbuf (erase-buffer))
    (with-temp-buffer
      (insert-file-contents-as-coding-system
       (cdr (assoc "euc" skk-coding-system-alist))
       (expand-file-name dic))
      (re-search-forward "^;; okuri-nasi entries\\.$")
      ;; abbrev 候補を skip
      (delete-region (point-min) (progn (re-search-forward "^あ")
					(beginning-of-line)
					;;(forward-char -1)
					(point)))
      (skk-make-mazegaki-dic-1 workbuf nomsg))
    (with-current-buffer workbuf
      (if (not (> (buffer-size) 0))
	  (or nomsg
	      (message "No entries of Mazegaki dictionary"))
	(write-region-as-coding-system
	 (cdr (assoc "euc" skk-coding-system-alist))
	 ;; only Emacs 21's write-region has 6th arg MUSTBENEW.
	 1 (point-max) output nil t nil)
	(or nomsg
	    (message "Making Mazegaki dictionary...100%% done"))))
    (kill-buffer workbuf)))

;;;###autoload
(defun skk-make-mazegaki-dic-region (min max &optional nomsg)
  "SKK 形式の辞書から TUT-Code などで使われれる混ぜ書き辞書を作る。
加工した辞書を作業バッファに書き出し `pop-to-buffer' する。
出力されたバッファをファイル (下記の例では TMP) に保存し

  $ skkdic-expr TMP | skkdic-sort > NEWDICT

として加工する必要あり。"
  (interactive "r\nP")
  (let ((outbuf (get-buffer-create " *skkmkmgk working*")))
    (save-excursion
      (save-restriction
	(narrow-to-region min max)
	(goto-char min)
	(skk-make-mazegaki-dic-1 outbuf nomsg)))
    (pop-to-buffer outbuf)
    (goto-char (point-min))))
 
(defun skk-make-mazegaki-dic-1 (outbuf nomsg)
  (let ((max (point-max))
	(cont t)
	header0 header-list candidates0 candidates1)
    (while (and cont (not (eobp)))
      (or nomsg
	  (message (format "Making Mazegaki dictionary...%d%%%% done"
			   (* (/ (* (point) 100.00) (* max 100.00))
			      100.0))))
      (beginning-of-line)
      ;; ひらがな見出し e.x. "あいかわ"
      (setq header0 (buffer-substring-no-properties
		     (point) (save-excursion (search-forward " ")
					     (backward-char 1) (point))))
      (if (= (skk-str-length header0) 1)
	  nil
	(search-forward " /")
	;; ひらがな見出しをキーにした候補リスト (2 文字以上) e.x. "相川"
	(setq candidates0 (skk-mkmgk-filter
			   (car (skk-compute-henkan-lists nil))))
	(if (null candidates0)
	    nil
	  ;; ひらがな見出しを分解
	  (setq header-list (string-to-char-list header0))
	  (while header-list
	    (let* ((header-list1 header-list)
		   (header1 (char-to-string (car header-list1)))
		   (n 1))
	      (while (and header1 (> (length header0) (length header1)))
		(save-excursion
		  (goto-char (point-min))
		  ;; 分解したひらがな見出しを 1 文字づつ見出しにして再検索 e.x. "あい"
		  (if (not (setq candidates1 (skk-mkmgk-binary-search
					      header1 (point-min) max skk-mkmgk-region-limit)))
		      nil
		    (let ((can0 candidates0))
		      (while (and candidates1 can0)
			(if (not (string-match (car candidates1) (car can0)))
			    (or (setq candidates1 (cdr candidates1))
				(setq can0 (cdr can0)))
			  ;; XXX
			  ;; "あいかわ" のような例では、"あい" と "かわ" で
			  ;; 2 度 match するので、同じものを出力してしまう。
			  ;; いずれにせよ skkdic-expr & skkdic-sort しなけ
			  ;; れば SKK 辞書として使えないので、まぁいっか...。
			  (with-current-buffer outbuf
			    ;; 分解キー (e.x. あい) による候補 e.x. "相" が
			    ;; オリジナルキー (e.x. あいかわ) の候補 e.x.
			    ;; "相川" に match したら
			    (goto-char (point-max))
			    ;; あい川 /相川/
			    (insert (substring (car can0) 0 (match-beginning 0))
				    header1
				    (substring (car can0) (match-end 0))
				    " /" (car can0) "/\n")
			    (if (string-match header1 header0)
				(insert
				 ;; 相かわ /相川/
				 (substring header0 0 (match-beginning 0))
				 (car candidates1)
				 (substring header0 (match-end 0))
				 " /" (car can0) "/\n")))
			  (setq candidates1 nil)))))
		  (if (> (skk-str-length
			  (mapconcat 'char-to-string header-list1 nil)) n)
		      ;; HEADER1 を伸ばして再検索
		      ;; e.x. "あ" → "あい" → "あいか" → "あいかわ"
		      ;;      "い" → "いか" → "いかわ"
		      ;;      "か" → "かわ"
		      ;;      "わ"
		      (setq header1 (concat
				     header1
				     (char-to-string (nth n header-list1)))
			    n (1+ n))
		    ;; 次の char を頭にした検索開始
		    ;; e.x. "あ" → "い" → "か" → "わ"
		    (setq header1 nil))))
	      (setq header-list (cdr header-list1))))))
      (setq cont (= (forward-line 1) 0)))
    (or nomsg
	(message "Making Mazegaki dictionary...100%% done"))))

(defun skk-mkmgk-filter (list)
  (delq nil (mapcar
	     (function
	      (lambda (word)
		(if (and
		     ;; 漢字 1 文字を混ぜ書きすることはないでしょう...
		     (> (skk-str-length word) 1)
		     ;; カタカナ語を skip
		     (not (string-match "^[ーァ-ン]+$" word))
		     ;; 英語を skip
		     (not (string-match "^[a-zA-Z]+$" word)))
		    (if (string-match ";" word)
			(substring word 0 (match-beginning 0))
		      word))))
	     list)))

(defun skk-mkmgk-binary-search (key min max limit)
  (let ((case-fold-search nil)
        size p)
    (if (> limit 0)
	(while (progn (setq size (- max min)) (> size limit))
	  (goto-char (+ min (/ size 2)))
	  (beginning-of-line)
	  (setq p (point))
	  (if (string< key (buffer-substring-no-properties
			    p (1- (search-forward " "))))
	      (setq max p)
	    (setq min p))))
    (goto-char min)
    (beginning-of-line)
    (if (re-search-forward (concat "^" key " /") max 'noerror)
	(skk-mkmgk-filter (car (skk-compute-henkan-lists nil))))))

(require 'product)
(product-provide (provide 'skk-mkmgk) (require 'skk-version))
;; end of skk-mkmgk.el
