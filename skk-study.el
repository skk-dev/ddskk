;;; skk-study.el --- SKK 学習効果提供プログラム -*- coding: iso-2022-jp -*-
;; Copyright (C) 1999, 2000, 2002, 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-study.el,v 1.55 2010/08/24 11:37:41 skk-cvs Exp $
;; Keywords: japanese
;; Created: Apr. 11, 1999
;; Last Modified: $Date: 2010/08/24 11:37:41 $

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

;; ある語 A' を確定した場合に、A' 及びその見出し語 A に対して、直前に
;; 変換した語 B' とその見出し語 B を関連語として登録しておき、再度 A
;; の変換を行ったときに、B 及び B' のペアが直前の何回かに確定した語の
;; 中に見つかれば、を優先して出力する単純な学習効果を提供するプログラ
;; ムです。
;;
;; 昔 SKK ML で話題になった単語の属性の保存のために、skk-attr.el を作
;; りましたが、機能を欲張りすぎてものになりませんでした。直前の変換と
;; の関連性を保存するためだけに機能を絞って再構成したのがこのプログラ
;; ムです。

;; <How to install>
;;
;; ~/.skk に
;;
;;   (require 'skk-study)
;;
;; と書いて下さい。

;; <DATA STRUCTURE (SKK-STUDY-ALIST)>
;;
;; ((okuri-ari .  ((A . (((B . B') . (A' ...))
;;                                       ...))))
;;  (okuri-nasi . ((A . (((B . B') . (A' ...))
;;                                       ...)))))
;;
;;  o examples
;;
;; ((okuri-ari .
;;           (("きr" . ((("ふく" . "服") . ("着"))
;;                      (("き" . "木") . ("切"))
;;                      (("えん" . "縁") . ("切"))))
;;            ("なk" . ((("こども" . "子供") . ("泣"))
;;                      (("ことり" . "小鳥") . ("鳴"))))
;;            ("かk" . ((("かみ" . "紙") . ("書")) (("ひんかく" . "品格") . ("欠")))))
;;           ...)
;;  (okuri-nasi .
;;            (("かみ" . ((("きr" . "切") . ("紙"))))
;;             ...)))
;;
;; <TODO>
;; 科学、法律などとテーマを決めて、バッファ毎に学習データを切り替えで
;; きると便利かも。-> experimental/skk-study.el で実現されています。


;;; Code:

(eval-when-compile
  (require 'cl)
  (defvar print-quoted))

(require 'pym)
(require 'skk-macs)
(require 'skk-vars)
(require 'ring)

(defun-maybe ring-elements (ring)
  "Return a list of the elements of RING."
  ;; ring.el of Emacs 20 does not have ring-elements.
  (mapcar #'identity (cddr ring)))

;;;; inline functions.
(defsubst skk-study-get-last-henkan-data (index)
  (and (> (ring-length skk-study-data-ring) index)
       (ring-ref skk-study-data-ring index)))

(add-to-list 'skk-search-end-function 'skk-study-search)
(add-to-list 'skk-update-end-function 'skk-study-update)

;;;###autoload
(defun skk-study-search (henkan-buffer midasi okurigana entry)
  "学習データを参照して ENTRY を加工し、関連性のある語の優先順位を上げて返す。"
  (or skk-study-data-ring
      (setq skk-study-data-ring (make-ring skk-study-search-times)))
  (when (and entry (cdr entry))
    (or skk-study-alist (skk-study-read))
    (with-current-buffer henkan-buffer
      ;; (("きr" . ((("ふく" . "服") . ("着")) (("き" . "木") . ("切"))))
      ;;  ("なk" . ((("こども" . "子供") . ("泣")))))
      (let ((alist
	     (cdr
	      (assoc
	       midasi
	       (cdr (assq (cond ((or skk-okuri-char skk-henkan-okurigana)
				 'okuri-ari)
				(t 'okuri-nasi))
			  skk-study-alist))))))
	(when alist
	  (setq entry (skk-study-search-1 alist midasi okurigana entry))))))
  entry)

(defun skk-study-search-1 (target-alist midasi okurigana entry)
  (do ((index 0 (1+ index))
       (times skk-study-search-times (1- times))
       last-data associates e exit)
      ((or exit (= times 0)) entry)
    (and
     (setq last-data (skk-study-get-last-henkan-data index))
     ;; ((("ふく" . "服") . ("着")) (("き" . "木") . ("切")))
     ;; ("着")
     (setq associates (cdr (assoc last-data target-alist)))
     (setq associates (reverse associates))
     (setq exit t)
     (while (setq e (car associates))
       ;;uniq
       (setq entry (cons e (delete e entry))
	     associates (cdr associates))))))

;;;###autoload
(defun skk-study-update (henkan-buffer midasi okurigana word purge)
  "MIDASI と WORD について `skk-study-data-ring' の最初の関連語を関連付けて学習する。"
  (or skk-study-data-ring
      (setq skk-study-data-ring (make-ring skk-study-search-times)))
  (let ((inhibit-quit t)
	last-data diff grandpa papa baby)
    (with-current-buffer henkan-buffer
      (when (and
	     ;; 第一候補で確定したかどうか
	     (or skk-study-first-candidate
		 (not (string= word (car skk-henkan-list))))
	     ;; 変換バッファが変わっていないかどうか
	     (eq (skk-get-last-henkan-datum 'henkan-buffer) henkan-buffer)
	     (or (not skk-study-max-distance)
		 (and (setq diff
			    (- (point)
			       (skk-get-last-henkan-datum 'henkan-point)))
		      ;; 直前の変換よりポイントが前へ移動していないかどうか
		      (> diff 0)
		      ;; skk-study-max-distance を超えて直前の変換とポイン
		      ;; トが離れていないかどうか。
		      (> skk-study-max-distance diff)))
	     midasi word
	     (setq last-data (if (not (ring-empty-p skk-study-data-ring))
				 (ring-ref skk-study-data-ring 0)))
	     (not (or (string= midasi "") (string= word "")
		      (and (string= midasi (car last-data))
			   (string= word (cdr last-data))))))
	(or skk-study-alist (skk-study-read))
	(setq grandpa (assq (cond ((or skk-okuri-char skk-henkan-okurigana)
				   'okuri-ari)
				  (t 'okuri-nasi))
			    skk-study-alist)
	      ;; ((("ふく" . "服") . ("着")) (("き" . "木") . ("切")))
	      papa (assoc midasi (cdr grandpa)))
	(cond (
	       ;; car に見出し語を持つ cell がない
	       (not (or papa purge))
	       (setcdr grandpa
		       (nconc
			(list (cons midasi (list (cons last-data (list word)))))
			(cdr grandpa))))
	      ;; 見出し語から始まる cell はあるが、cdr に (last-key . last-word) を
	      ;; キーにした cell がない。
	      ((not (or
		     ;; (("ふく" . "服") . ("着"))
		     (setq baby (assoc last-data (cdr papa)))
		     purge))
	       (setcdr papa (cons (cons last-data (list word)) (cdr papa))))
	      ;; 見出し語をキーとした既存の cell 構造ができあがっているので、関連語だけ
	      ;; アップデートする。
	      ((not purge)
	       ;; ring データの方がもっと効率的か？  でもここの部分のデータのアップデート
	       ;; が効率良くできない。
	       (setcdr baby (cons word (delete word (cdr baby))))
	       (if (> (1- (length (cdr baby))) skk-study-associates-number)
		   (skk-study-chomp (cdr baby) (1- skk-study-associates-number))))
	      (t (setcdr grandpa (delq baby (cdr grandpa)))))))))

;;;###autoload
(defun skk-study-save (&optional nomsg)
  "`skk-study-file' に学習結果を保存する。
オプショナル引数の NOMSG が non-nil であれば、保存メッセージを出力しない。"
  (interactive "P")
  (let ((inhibit-quit t)
	e)
    (if (or (and (null skk-study-alist) (not nomsg))
	    (not skk-study-last-read)
	    (and skk-study-last-save
		 (skk-study-time-lessp
		  skk-study-last-save skk-study-last-read)))
	(progn
	  (skk-message "SKK の学習結果をセーブする必要はありません"
		       "No SKK study need saving")
	  (sit-for 1))
      (when (not nomsg)
	(skk-message "%s に SKK の学習結果をセーブしています..."
		     "Saving SKK study to %s..." skk-study-file))
      (and skk-study-backup-file
	   (file-exists-p (expand-file-name skk-study-file))
	   (cond ((eq system-type 'ms-dos)
		  (with-temp-file skk-study-backup-file
		    (erase-buffer)
		    (insert-file-contents skk-study-file)))
		 (t
		  (copy-file (expand-file-name skk-study-file)
			     (expand-file-name skk-study-backup-file)
			     'ok-if-already-exists 'keep-date))))
      (with-temp-buffer
	(insert
	 (format ";;; skk-study-file format version %s\n"
		 skk-study-file-format-version))
	(when skk-study-sort-saving
	  ;; sort is not necessary, but make an alist rather readable.
	  (setq e (assq 'okuri-ari skk-study-alist))
	  (setcdr e (sort (cdr e)
			  (function (lambda (a b)
				      (skk-string< (car a) (car b))))))
	  (setq e (assq 'okuri-nasi skk-study-alist))
	  (setcdr e (sort (cdr e)
			  (function (lambda (a b)
				      (skk-string< (car a) (car b)))))))
	(skk-study-prin1 skk-study-alist (current-buffer))
	(write-region-as-coding-system
	 (skk-find-coding-system skk-jisyo-code)
	 (point-min) (point-max) skk-study-file))
      (setq skk-study-last-save (current-time))
      (when (not nomsg)
	(skk-message "%s に SKK の学習結果をセーブしています...完了！"
		     "Saving SKK study to %s...done" skk-study-file)
	(sit-for 1)
	(message "")))))

;;;###autoload
(defun skk-study-read (&optional nomsg force)
  "`skk-study-file' から学習結果を読み込む。
オプショナル引数の FORCE が non-nil であれば、破棄の確認をしない。"
  (interactive "P")
  (skk-create-file
   skk-study-file
   (if (not nomsg)
       (if skk-japanese-message-and-error
	   "SKK の学習結果ファイルを作りました"
	 "I have created an SKK study file for you")))
  (when (or (null skk-study-alist)
	    force
	    (skk-yes-or-no-p
	     (format "%s を再読み込みしますか？" skk-study-file)
	     (format "Reread %s?" skk-study-file)))
    (unless nomsg
      (skk-message "%s の SKK 学習結果を展開しています..."
		   "Expanding SKK study of %s ..."
		   (file-name-nondirectory skk-study-file)))
    (when skk-study-check-alist-format
      (skk-study-check-alist-format skk-study-file))
    (setq skk-study-alist (skk-study-read-1 skk-study-file))
    (setq skk-study-last-read (current-time))
    (when (and skk-study-alist (not nomsg))
      (skk-message
       "%s の SKK 学習結果を展開しています...完了！"
       "Expanding SKK study of %s ...done"
       (file-name-nondirectory skk-study-file))
      (sit-for 1)
      (message ""))))

(defun skk-study-read-1 (file)
  ;; read FILE and return alist.
  (with-temp-buffer
    (let ((version-string
	   (format ";;; skk-study-file format version %s\n"
		   skk-study-file-format-version)))
      (insert-file-contents-as-coding-system
       (skk-find-coding-system skk-jisyo-code) file)
      (when (= (buffer-size) 0)
	;; bare alist
	(insert version-string "((okuri-ari) (okuri-nasi))"))
      (goto-char (point-min))
      (if (looking-at (regexp-quote version-string))
	  (read (current-buffer))
	(skk-error
	 "skk-study-file フォーマットのバージョンが一致しません"
	 "skk-study-file format version is inconsistent")))))

(defun skk-study-check-alist-format (alist-file)
  "ALIST-FILE の連想リストのフォーマットをチェックする。"
  (interactive
   (list (read-file-name
	  (format "Alist file to check: (default: %s) " skk-study-file)
	  default-directory skk-study-file)))
  (skk-message "%s ファイルの連想リストのフォーマットチェックを行なっています..."
	       "Checking %s file alist format..." alist-file)
  (or (skk-study-check-alist-format-1 (skk-study-read-1 alist-file))
      (skk-error "%s の連想リストのフォーマットは壊れています"
		 "%s alist format is corrupt" alist-file))
  (skk-message
   "%s ファイルの連想リストのフォーマットチェックを行なっています...完了!"
   "Checking %s file alist format... done" alist-file)
  (sit-for 1)
  (message ""))

(defun skk-study-check-alist-format-1 (alist)
  (when (and (= (length alist) 2)
	     (assq 'okuri-ari alist)
	     (assq 'okuri-nasi alist))
    (catch 'exit
      (let ((index '(okuri-ari okuri-nasi))
	    (func (function
		   (lambda (str)
		     (let ((len (length str)))
		       (and
			(> len 1)
			(skk-ascii-char-p (aref str (1- len))))))))
	    alist2 e f)
	(while index
	  (and (eq (car index) 'okuri-nasi)
	       (setq func
		     (function
		      (lambda (str)
			(let ((len (length str)))
			  (cond ((= len 1))
				((not (skk-ascii-char-p (aref str (1- len)))))
				((skk-ascii-char-p (aref str (- len 2))))))))))
	  (setq alist2 (cdr (assq (car index) alist)))
	  (while alist2
	    (setq e (car alist2))
	    (or (funcall func (car e))
		;; 見出し語のチェック
		(throw 'exit nil))
	    (setq f (cdr e))
	    (while f
	      (if (not (and
			;; 直前の変換の情報
			(consp (car (car f)))
			;; 関連語リスト
			(listp (cdr (car f)))))
		  (throw 'exit nil))
	      (setq f (cdr f)))
	    (setq alist2 (cdr alist2)))
	  (setq index (cdr index)))
	t))))

(defun skk-study-prin1 (form &optional stream)
  (let ((print-readably t)
	print-level print-length print-quoted)
    (prin1 form stream)))

(defun skk-study-chomp (nth list)
  ;; LIST := '(A B C D), NTH := 1
  ;; -> '(A B)
  (and (> nth -1) (setcdr (nthcdr nth list) nil))
  list)

(defadvice skk-kakutei-initialize (before skk-study-ad activate)
  (let ((kakutei-word (ad-get-arg 0)))
    (when kakutei-word
      (ring-insert
       skk-study-data-ring (cons skk-henkan-key kakutei-word)))))

(defadvice skk-undo-kakutei (after skk-study-ad activate)
  (let ((last (ring-ref skk-study-data-ring 0))
	(last2 (ring-ref skk-study-data-ring 1))
	target)
    (when (and last last2)
      (setq target (assoc (car last)
			  (assq (cond ((skk-get-last-henkan-datum 'okuri-char)
				       'okuri-ari)
				      (t 'okuri-nasi))
				skk-study-alist)))
      (setq target (delq (assoc last2 (cdr target)) target)))))

;; time utilities...
;;  from ls-lisp.el.  Welcome!
(defun skk-study-time-lessp (time0 time1)
  (let ((hi0 (car time0))
	(hi1 (car time1))
	(lo0 (nth 1 time0))
	(lo1 (nth 1 time1)))
    (or (< hi0 hi1) (and (= hi0 hi1) (< lo0 lo1)))))

(add-hook 'kill-emacs-hook 'skk-study-save)

(provide 'skk-study)

;;; skk-study.el ends here
