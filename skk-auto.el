;;; skk-auto.el --- 送り仮名の自動処理のためのプログラム
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
;;               1999, 2001
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-auto.el,v 1.7 2001/08/31 19:30:14 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/08/31 19:30:14 $

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
(eval-when-compile (require 'skk-macs) (require 'skk-vars))

;;;###autoload
(defun skk-okuri-search ()
  ;; skk-auto-okuri-process が non-nil ならば "Uresii" のように送り仮名も含め
  ;; てタイプしても送りありの "嬉しい" を探し出す。
  (if (and skk-auto-okuri-process
	   (not (or skk-abbrev-mode skk-process-okuri-early
		    skk-henkan-okurigana))
	   ;; we don't do auto-okuri-process if henkan key contains numerals.
	   (not (skk-numeric-p))
	   (> (length skk-henkan-key) skk-kanji-len))
      (let (l)
	(setq skk-okuri-index-min (length skk-henkan-list)
	      l (funcall skk-okuri-search-function)
	      skk-okuri-index-max (+ skk-okuri-index-min (length l)))
	(if (not skk-katakana)
	    l
	  (mapcar (function (lambda (e) (skk-hiragana-to-katakana e))) l)))))

(defun skk-okuri-search-subr-original ()
  ;; skk-okuri-search のサブルーチン。見つけたエントリのリストを返す。
  (let* ((henkan-key skk-henkan-key)
	 (key (substring henkan-key 0 skk-kanji-len))
	 (len (length henkan-key))
	 (key1 (concat "\n" key))
	 key2 len2 key3 len3 okuri3
	 ;; case-fold-search は、辞書バッファでは常に nil。
	 ;;case-fold-search
	 (inhibit-quit t)
	 key-cand-alist p q r)
    (save-match-data
      (with-current-buffer (skk-get-jisyo-buffer skk-jisyo)
	(goto-char skk-okuri-ari-min)
	(while (search-forward key1 skk-okuri-ari-max t)
	  (setq p (point)
		key2 (concat key (buffer-substring-no-properties
				  p (- (search-forward " ") 2)))
		len2 (length key2))
	  (if (not (and (<= len2 len)
			(string= key2 (substring henkan-key 0 len2))))
	      nil
	    (let ((cont t))
	      (skk-save-point
	       (end-of-line)
	       (setq q (point)))
	      (while (and cont (search-forward "/[" q t))
		(setq r (point))
		(setq okuri3 (buffer-substring-no-properties
			      r
			      (1- (search-forward "/")))
		      key3 (concat key2 okuri3)
		      len3 (length key3))
		(if (not (and (<= len3 len)
			      (string= key3 (substring henkan-key 0 len3))))
		    nil
		  ;; finally found a candidate!
		  (let ((okuri
			 (concat okuri3 (substring henkan-key len3 len)))
			cand)
		    (while (not (eq (following-char) ?\]))
		      (setq cand
			    (concat
			     (buffer-substring-no-properties
			      (point)
			      (1- (search-forward "/" skk-okuri-ari-max t)))
			     okuri))
		      ;; 見出し語が違っても候補が同じことがあり得る。
		      ;;   かんz /感/[じ/感/]/
		      ;;   かんj /感/[じ/感/]/
		      ;; など。
		      (if (null (rassoc cand key-cand-alist))
			  (setq key-cand-alist (cons (cons key3 cand)
						     key-cand-alist))))
		    ;; it is not necessary to seach for "\[" on this line
		    ;; any more
		    (setq cont nil)))))))
	;; key3 の長いもの順にソートして返す。
	(mapcar (function
		 (lambda (x) (cdr x)))
		(sort (nreverse key-cand-alist)
		      (function (lambda (x y)
				  (string< (car y) (car x))))))))))

;;;###autoload
(defun skk-adjust-search-prog-list-for-auto-okuri ()
  ;; skk-auto-okuri-process が nil であれば、skk-search-prog-list から
  ;; '(skk-okuri-search) を消し、non-nil であれば加える。
  ;;
  ;; '(skk-okuri-search) を加える位置については、skk-jisyo の後が最良かどうか
  ;; は分らないので、オプションで変更できるようにすべきだが...。
  (if (not skk-auto-okuri-process)
      (setq skk-search-prog-list
	    (delete '(skk-okuri-search) skk-search-prog-list))
    (if (null (member '(skk-okuri-search) skk-search-prog-list))
	(let ((pl skk-search-prog-list)
	      (n 0) dic mark)
	  (while pl
	    (setq dic (car pl))
	    (if (memq (nth 1 dic) '(skk-jisyo skk-rdbms-private-jisyo-table))
		(setq mark n
		      pl nil)
	      (setq pl (cdr pl)
		    n (1+ n))))
	  (skk-splice-in skk-search-prog-list (1+ mark)
			 '((skk-okuri-search)))))))

;;(add-hook 'skk-mode-hook 'skk-adjust-search-prog-list-for-auto-okuri)

(run-hooks 'skk-auto-load-hook)
(require 'product)
(product-provide (provide 'skk-auto) (require 'skk-version))
;;; skk-auto.el ends here
