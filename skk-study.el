;; -*-byte-compile-dynamic: t;-*-
;; -*- byte-compile-dynamic-docstring: t;-*-
;;; skk-study.el --- SKK 学習効果提供プログラム
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-study.el,v 1.5 1999/10/05 11:02:04 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/10/05 11:02:04 $

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
;;
;; ある単語を変換した場合に、直前に変換した語を関連語として登録しておき、再度そ
;; の単語の変換を行なったときに登録した関連語が辞書にあればそれを優先して出力す
;; る単純な学習効果を提供するプログラムです。
;;
;; 昔 SKK ML で話題になった単語の属性の保存のために、skk-attr.el を作りました
;; が、機能を欲張りすぎてものになりませんでした。直前の変換との関連性を保存するた
;; めだけに機能を絞って再構成したのがこのプログラムです。

;; <How to work>
;;
;; XEmacs で SKK をパッケージインストールした場合は、.emacs に
;;
;;   (setq skk-search-end-function 'skk-study-search)
;;   (setq skk-update-end-function 'skk-study-update)
;;
;; と書くだけで十分です。それ以外の方は、
;;
;;   (add-hook 'skk-load-hook (function (lambda () (require 'skk-study))))
;;
;; などと書いて下さい。

;; <DATA STRUCTURE (SKK-STUDY-ALIST)>
;;
;; ((okuri-ari . (("現在の HENKAN-KEY" . ((("直前の HENKAN-KEY" . "直前の漢字") . ("HENKAN-KEY を見出し語とする関連語" ...))
;;                                        ... ))))
;;  (okuri-nasi . (("現在の HENKAN-KEY" . ((("直前の HENKAN-KEY" . "直前の漢字") . ("HENKAN-KEY を見出し語とする関連語" ...))
;;                                         ... )))))
;;
;;  o examples
;;
;; ((okuri-ari .
;;           (("きr" . ((("ふく" . "服") . ("着"))
;;                      (("き" . "木") . ("切"))
;;                      (("えん" . "縁") . ("切")) ))
;;            ("なk" . ((("こども" . "子供") . ("泣"))
;;                      (("ことり" . "小鳥") . ("鳴")) ))
;;            ("かk" . ((("かみ" . "紙") . ("書")) (("ひんかく" . "品格") . ("欠")))) )
;;           ... )
;;  (okuri-nasi .
;;            (("かみ" . ((("きr" . "切") . ("紙"))))
;;             ... )))
;;
;; <TODO>
;;

;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-foreword)

;;;###autoload
(defgroup skk-study nil "SKK study related customization."
  :prefix "skk-study-"
  :group 'skk )

;;; user variables.
(defcustom skk-study-file (convert-standard-filename "~/.skk-study")
  "*学習結果を保存するファイル。"
  :type 'file
  :group 'skk-study )

(defcustom skk-study-backup-file (convert-standard-filename "~/.skk-study.BAK" )
  "*学習結果を保存するバックアップファイル。"
  :type 'file
  :group 'skk-study )

(defcustom skk-study-associates-number 3
  "*保存する関連語の数。"
  :type 'integer
  :group 'skk-study )

(defcustom skk-study-sort-saving t
  "*Non-nil であれば学習結果をソートしてセーブする。"
  :type 'boolean
  :group 'skk-study )

(defcustom skk-study-check-alist-format t
  "*Non-nil であれば、学習結果の読み込み時に連想リストのフォーマットをチェックする。"
  :type 'boolean
  :group 'skk-study )
	 
;;; system internal variables and constants.
;; global variable
(defvar skk-study-alist nil)
(defvar skk-search-end-function 'skk-study-search)
(defvar skk-update-end-function 'skk-study-update)
(defvar skk-kakutei-end-function nil)
(defconst skk-study-file-format-version 0.2)

;;;###autoload
(defun skk-study-search (henkan-buffer midasi okurigana entry)
  (if (null entry)
      nil
    (with-current-buffer henkan-buffer
      (let ((index skk-study-associates-number)
	    grandpa papa associates r
	    ;; buffer local variables.
	    last-key last-word )
	(or skk-study-alist (skk-study-read))
	(if (and skk-study-alist
		 (setq last-key (skk-get-last-henkan-data 'henkan-key))
		 (setq last-word (car (skk-get-last-henkan-data 'henkan-list)))
		 ;; grandpa ::= (("きr" . ((("ふく" . "服") . ("着")) (("き" . "木") . ("切"))))
		 ;;              ("なk" . ((("こども" . "子供") . ("泣")))) )
		 (setq grandpa (cdr (assq (cond (skk-okuri-char 'okuri-ari)
						(t 'okuri-nasi) )
					  skk-study-alist )))
		 ;; papa ::= ((("ふく" . "服") . ("着")) (("き" . "木") . ("切")))
		 (setq papa (cdr (assoc midasi grandpa)))
		 ;; associates ::= ("着")
		 (setq associates (cdr (assoc (cons last-key last-word) papa))) )
	    (while (and (> index 0) (setq r (nth (1- index) associates)))
	      (setq entry (cons r (delete r entry))
		    index (1- index) )))
	entry ))))

;;;###autoload
(defun skk-study-update (henkan-buffer midasi okurigana word purge)
  (with-current-buffer henkan-buffer
    (let ((inhibit-quit t)
	  grandpa papa baby
	  ;; to get buffer local variables in henkan-buffer.
	  last-key last-word )
      (or skk-study-alist (skk-study-read))
      (if (and (setq last-key (skk-get-last-henkan-data 'henkan-key))
	       (setq last-word (car (skk-get-last-henkan-data 'henkan-list)))
	       ;; grandpa ::= (okuri-ari . (("きr" . ((("ふく" . "服") . ("着")) (("き" . "木") . ("切"))))))
	       (setq grandpa (assq (cond (skk-okuri-char 'okuri-ari )
					 (t 'okuri-nasi) )
				   skk-study-alist )))
	  ;; papa ::= ("きr" . ((("ふく" . "服") . ("着")) (("き" . "木") . ("切"))))
	  ;; skk-study-alist に該当の cell がないと nil を返すので、and 条件に入れない。
	  (progn
	    (setq papa (assoc midasi (cdr grandpa)))
	    (cond (
		   ;; car に見出し語を持つ cell がない
		   (not (or papa purge))
		   (setcdr grandpa
			   (nconc
			    (list (cons midasi (list (cons (cons last-key last-word)
							   (list word) ))))
			    (cdr grandpa) )))
		  ;; 見出し語から始まる cell はあるが、cdr に (last-key . last-word) をキーにした
		  ;; cell がない。
		  ((not (or
			 ;; baby ::= (("ふく" . "服") . ("着"))
			 (setq baby (assoc (cons last-key last-word) (cdr papa)))
			 purge ))
		   (setcdr papa (cons (cons (cons last-key last-word) (list word)) (cdr papa))) )
		  ;; 見出し語をキーとした既存の cell 構造ができあがっているので、関連語だけアップデートする。
		  ((not purge)
		   (setcdr baby (cons word (delete word (cdr baby))))
		   (if (> (1- (length (cdr baby))) skk-study-associates-number)
		       (skk-study-chomp (cdr baby) (1- skk-study-associates-number)) ))
		  (t (setcdr papa (delq baby (cdr papa)))) ))))))

;;;###autoload
(defun skk-study-save (&optional nomsg)
  "skk-study-file に学習結果を保存する."
  (interactive "P")
  (let ((inhibit-quit t)
	e )
    (if (and (null skk-study-alist) (not nomsg))
	(progn
	  (skk-message "SKK の学習結果をセーブする必要はありません"
		       "No SKK study need saving" )
	  (sit-for 1) )
      (if (not nomsg)
	  (skk-message "%s に SKK の学習結果をセーブしています..."
		       "Saving SKK study to %s..." skk-study-file ))
      (and skk-study-backup-file
	   (file-exists-p (expand-file-name skk-study-file))
	   (copy-file (expand-file-name skk-study-file)
		      (expand-file-name skk-study-backup-file)
		      'ok-if-already-exists 'keep-date ))
      (with-temp-buffer
	(insert
	 (format ";;; -*- emacs-lisp -*-\n;;; skk-study-file format version %s\n"
		 skk-study-file-format-version ))
	(if (not skk-study-sort-saving)
	    nil
	  ;; sort is not necessary, but make an alist rather readable.
	  (setq e (assq 'okuri-ari skk-study-alist))
	  (setcdr e (sort (cdr e) (function (lambda (a b) (string< (car a) (car b))))))
	  (setq e (assq 'okuri-nasi skk-study-alist))
	  (setcdr e (sort (cdr e) (function (lambda (a b) (string< (car a) (car b)))))) )
	(skk-study-prin1 skk-study-alist (current-buffer))
	(write-region-as-coding-system
	 (cond ((and skk-jisyo-code (coding-system-p skk-jisyo-code))
		skk-jisyo-code )
	       ((and skk-jisyo-code (stringp skk-jisyo-code))
		(cdr (assoc skk-jisyo-code skk-coding-system-alist)) )
	       (t (cdr (assoc "euc" skk-coding-system-alist))) )
	 (point-min) (point-max) skk-study-file ))
      (if (not nomsg)
	  (progn
	    (skk-message "%s に SKK 学習結果をセーブしています...完了！"
			 "Saving SKK study to %s...done" skk-study-file )
	    (sit-for 1)
	    (message "") )))))

;;;###autoload
(defun skk-study-read (&optional nomsg)
  "skk-study-file から学習結果を読み込む。"
  (interactive "P")
  (skk-create-file
   skk-study-file
   (if (not nomsg)
       (if skk-japanese-message-and-error
	   "SKK の学習結果ファイルを作りました"
	 "I have created an SKK study file for you" )))
  (if (or (null skk-study-alist)
	  (skk-yes-or-no-p (format "%s を再読み込みしますか？" skk-study-file)
			   (format "Reread %s?" skk-study-file) ))
      (progn
	(or nomsg
	    (skk-message "%s の SKK 学習結果を展開しています..."
			 "Expanding SKK study of %s ..."
			 (file-name-nondirectory skk-study-file) ))
	;; 安定したらディフォルトを nil にするね。
	(if skk-study-check-alist-format
	    (skk-study-check-alist-format skk-study-file) )
	(setq skk-study-alist (skk-study-read-1 skk-study-file))
	(if (null skk-study-alist)
	    nil
	  (or nomsg
	      (progn
		(skk-message
		 "%s の SKK 学習結果を展開しています...完了！"
		 "Expanding SKK study of %s ...done"
		 (file-name-nondirectory skk-study-file) )
		(sit-for 1)
		(message "") ))))))

(defun skk-study-read-1 (file)
  ;; read FILE and return alist.
  (with-temp-buffer
    (let ((version-string
	   (format ";;; -*- emacs-lisp -*-\n;;; skk-study-file format version %s\n"
		   skk-study-file-format-version )))
      (insert-file-contents-as-coding-system
       (cond ((and skk-jisyo-code
		   (or (coding-system-p skk-jisyo-code)
		       (and (fboundp 'find-coding-system)
			    (find-coding-system skk-jisyo-code) )))
	      skk-jisyo-code )
	     ((and skk-jisyo-code (stringp skk-jisyo-code))
	      (cdr (assoc skk-jisyo-code skk-coding-system-alist)) )
	     (t (cdr (assoc "euc" skk-coding-system-alist))) )
       file )
      (if (= (buffer-size) 0)
	  ;; bare alist
	  (insert version-string "((okuri-ari) (okuri-nasi))") )
      (goto-char (point-min))
      (if (looking-at (regexp-quote version-string))
	  (read (current-buffer))
	(let ((old-version-string
	       (format
		";;; -*- emacs-lisp -*-\n;;; skk-study-file format version %s\n"
		(- skk-study-file-format-version 0.1) ))
	      (skk-study-sort-saving t) )
	  (cond ((and (looking-at (regexp-quote old-version-string))
		      (skk-yes-or-no-p
		       "skk-study-file フォーマットのバージョンアップを行ないますか？ "
		       "Do you want to make skk-study-file format version up? " ))
		 (prog1
		     (setq skk-study-alist
			   (skk-study-convert-alist-format (read (current-buffer))) )
		   (skk-study-save 'nomsg) ))
		((skk-error
		  "skk-study-file フォーマットのバージョンが一致しません"
		  "skk-study-file format version is inconsistent" ))))))))

;;;###autoload
(defun skk-study-check-alist-format (alist-file)
  "ALIST-FILE の連想リストのフォーマットをチェックする。"
  (interactive
   (list (read-file-name
	  (format "Alist file to check: (default: %s) " skk-study-file)
	  default-directory skk-study-file )))
  (skk-message "%s ファイルの連想リストのフォーマットチェックを行なっています..."
	       "Checking %s file alist format..." alist-file )
  (or (skk-study-check-alist-format-1 (skk-study-read-1 alist-file))
      (skk-error "%s の連想リストのフォーマットは壊れています"
		 "%s alist format is corrupt" alist-file ))
  (skk-message
   "%s ファイルの連想リストのフォーマットチェックを行なっています...完了!"
   "Checking %s file alist format... done" alist-file )
  (sit-for 1)
  (message "") )

(defun skk-study-check-alist-format-1 (alist)
  (if (not (and (= (length alist) 2) (assq 'okuri-ari alist)
		(assq 'okuri-nasi alist) ))
      nil
    (catch 'exit
      (let ((index '(okuri-ari okuri-nasi))
	    (func (function
		   (lambda (str)
		     (let ((len (length str)))
		       (and
			(> len 1)
			(skk-ascii-char-p (skk-str-ref str (1- len))) )))))
	    alist2 e f )
	(while index
	  (and (eq (car index) 'okuri-nasi)
	       ;;(setcdr (nthcdr 1 (nth 2 func))
	       ;;        (list (cons 'not (cdr (nthcdr 1 (nth 2 func))))) )))
	       (setq func
		     (function
		      (lambda (str)
			(let ((len (length str)))
			  (or (= len 1)
			      (not (skk-ascii-char-p (skk-str-ref str (1- len)))) ))))))
	  (setq alist2 (cdr (assq (car index) alist)))
	  (while alist2
	    (setq e (car alist2))
	    (or (funcall func (car e))
		;; 見出し語のチェック
		(throw 'exit nil) )
	    (setq f (cdr e))
	    (while f
	      (if (not (and
			;; 直前の変換の情報
			(consp (car (car f)))
			;; 関連語リスト
			(listp (cdr (car f))) ))
		  (throw 'exit nil) )
	      (setq f (cdr f)) )
	    (setq alist2 (cdr alist2)) )
	  (setq index (cdr index)) )
	t ))))

(defun skk-study-convert-alist-format (alist)
  ;; convert format version 0.1 to 0.2.
  (let ((inhibit-quit t)
	(base '((okuri-ari) (okuri-nasi)))
	e len )
    (while alist
      (setq e (car alist)
	    len (length (car e)) )
      (if (and (> len 1) (skk-ascii-char-p (skk-str-ref (car e) (1- len))))
	  ;; okuri-ari
	  (setcdr (car base) (cons e (cdr (car base))))
	;; okuri-nasi
	(setcdr (car (cdr base)) (cons e (cdr (car (cdr base))))) )
      (setq alist (cdr alist)) )
    (and (skk-study-check-alist-format-1 base)
	 base )))

(defun skk-study-prin1 (form &optional stream)
  (let ((print-readably t)
	print-level print-length print-quoted )
    (prin1 form stream) ))

(defun skk-study-chomp (nth list)
  ;; LIST ::= '(A B C D), NTH ::= 1
  ;; -> '(A B)
  (and (> nth -1) (setcdr (nthcdr nth list) nil))
  list )

(add-hook 'skk-before-kill-emacs-hook 'skk-study-save)
(provide 'skk-study)
;;; Local Variables:
;;; End:
;;; skk-study.el ends here
