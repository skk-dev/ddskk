;;; skk-dbm.el --- SKK dbm interfaces. -*- coding: iso-2022-jp -*-
;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-dbm.el,v 1.9 2010/08/02 15:21:06 skk-cvs Exp $
;; Keywords: japanese, dbm, gdbm
;; Created: Jan. 1, 1999
;; Last Modified: $Date: 2010/08/02 15:21:06 $

;; This file is not part of Daredevil SKK yet.

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

;; このプログラムは、skkserv を介するサーチの代替の辞書アクセス方法を提供します。
;; サーバーを介さず、XEmacs の機能を使用し、直接 dbm データベースファイル (以下
;; 単に「データベース」と言います) をオープンして検索します。
;; --with-database オプションの指定された XEmacs で *のみ* 使用することができま
;; す。動作確認は、XEmacs 21.2b8 で行ないました。
;;
;; データベースライブラリとして gdbm を使う場合で、データベースを gdbm 形式とし
;; たいときは、XEmacs 21.2b8 が gdbm ファイルに直接アクセスできない (libgdbm を
;; リンクしていながら libndbm 互換関数しか使用しないので .dir, .pag を拡張子とす
;; るファイルしか開けない) ことから、別途作業が必要となります。
;; 別添の diff-to-xemacs-21.2.8 を xemacs-21.2.8 のソースに当て、
;; --with-database=gnudbm オプションを指定して XEmacs を再 configure し、再コン
;; パイルする必要があります。このパッチを当てた XEmacs では、gdbm ファイルの
;; synchronize や reorganize が可能となる他、cashsize や fastmode のオプションも
;; 指定可能となります。
;;
;; ** berkeley-db もしくは ndbm ファイルだけであればパッチを当てる必要はありません。***
;;
;; skk-search-prog-list を例えば、下記のように設定することで、autoload されます。
;;
;;    (setq skk-search-prog-list
;;         '((skk-search-jisyo-file skk-jisyo 0 t)
;;   	     (skk-dbm-search-jisyo-database skk-dbm-large-jisyo 'nomsg)))
;;
;; 下記のように設定すると、個人辞書もデータベース化して検索を行なうことができます。
;;
;;    (setq skk-search-prog-list
;;         '((skk-dbm-search-jisyo-database skk-dbm-jisyo)
;;   	     (skk-dbm-search-jisyo-database skk-dbm-large-jisyo 'nomsg)))
;;
;; pskkserv 添付の makedbmdic で作った辞書では何故か検索できません (XEmacs のデー
;; タベース機能がコーディングシステムを無視しているから？)。このファイルの
;; skk-dbm-make-jisyo 関数を使ってデータベースを作って下さい。

;; TODO
;; 補完・自動送り処理対応。

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

(unless (or (featurep 'gdbm)
	    (featurep 'dbm)
	    (featurep 'berkeley-db))
  (error "%s" "You need XEmacs built with --with-database option"))

(defgroup skk-dbm nil "SKK dbm related customization."
  :prefix "skk-dbm-"
  :group 'skk)

;; User variables.
;;;###autoload
(defcustom skk-dbm-jisyo "~/.skk-jisyo.db"
  "*dbm データベース化された個人辞書のファイル名。"
  :type 'file
  :group 'skk-dbm)

;;;###autoload
(defcustom skk-dbm-large-jisyo "/usr/local/share/skk/SKK-JISYO.L.db"
  "*dbm データベース化された SKK-JISYO.L のファイル名。"
  :type 'file
  :group 'skk-dbm)

;;;###autoload
(defcustom skk-dbm-subtype
  (save-match-data
    (and (or (string-match "\\.db$" skk-dbm-jisyo)
	     (string-match "\\.db$" skk-dbm-large-jisyo))
	 'hash))
  "*database 辞書の subtype。 hash, btree, recno のいずれか。
berkeley-db を使用する場合のみ指定すること。"
  :type '(choice (const hash) (const btree) (const recno) (const nil))
  :group 'skk-dbm)

(and (member '(skk-dbm-search-jisyo-database skk-dbm-jisyo) skk-search-prog-list)
     (setq skk-update-jisyo-function 'skk-dbm-update-jisyo)
     (setq skk-save-jisyo-function
	   (function
	    (lambda (quiet)
	      (if (not quiet)
		  (progn
		    (skk-message "SKK データベース辞書を閉じています..."
				 "Closing SKK database jisyo...")
		    (sit-for 1)))
	      (skk-dbm-close-all-database)
	      (if (not quiet)
		  (progn
		    (skk-message "SKK データベース辞書を閉じています...完了!"
				 "Closing SKK database jisyo...done")
		    (sit-for 1)))))))
	
;; System constants and variables.
(defvar skk-dbm-alist nil)
(defvar skk-dbm-type nil)
(defconst skk-dbm-working-buffer " *skk-dbm*")
(defconst skk-dbm-coding-system (cdr (assoc "euc" skk-coding-system-alist)))

;; Functions.
;;;###autoload
(defun skk-dbm-search-jisyo-database (dbfile &optional nomsg)
  (setq dbfile (expand-file-name dbfile))
  (let (
	;; I want `get-file-database'...
	(database (cdr (assoc dbfile skk-dbm-alist)))
	(okurigana (or skk-henkan-okurigana skk-okuri-char))
	(midasi
	 (if skk-use-numeric-conversion
	     (skk-num-compute-henkan-key skk-henkan-key)
	   skk-henkan-key))
	(henkan-buffer (current-buffer))
	string entry-list entry)
    (or (and database (databasep database) (database-live-p database))
	(setq database (skk-dbm-get-jisyo-database dbfile nomsg)
	      ;; this should do in Emacs internal like Vbuffer_alist of
	      ;; buffer.c.
	      skk-dbm-alist (cons (cons dbfile database) skk-dbm-alist)))
    (skk-dbm-init-working-buffer)
    (with-current-buffer skk-dbm-working-buffer
      ;;(and okurigana
      ;;     (setq okurigana (encode-coding-string
      ;;                    okurigana skk-dbm-coding-system)))
      ;;(setq midasi (encode-coding-string midasi skk-dbm-coding-system))
      (setq string (get-database midasi database))
      (if (not string)
	  nil
	(erase-buffer)
	(insert string)
	;; skip first character '/'.
	(goto-char (1+ (point-min)))
	(setq entry-list (skk-compute-henkan-lists okurigana))
	(setq entry
	      (cond ((and okurigana skk-henkan-okuri-strictly)
		     (nth 2 entry-list))
		    ((and okurigana skk-henkan-strict-okuri-precedence)
		     (skk-nunion (nth 2 entry-list) (car entry-list)))
		    (t (car entry-list))))
	(and skk-search-end-function
	     (setq entry (funcall skk-search-end-function henkan-buffer
				  midasi okurigana entry)))
	entry))))

(defun skk-dbm-get-jisyo-database (dbfile &optional nomsg)
  ;; Return database object.
  (save-match-data
    (setq dbfile (expand-file-name dbfile))
    (let (database access modes)
      (if (string= (expand-file-name skk-dbm-jisyo) dbfile)
	  ;; 個人辞書
	  (progn
	    (setq modes 0600)
	    (or (file-exists-p dbfile)
		;; なければ plain text の辞書からデータベース辞書を作成する。
		(skk-dbm-make-private-jisyo)))
	;; 共有辞書
	(setq modes 0444)
	(or (file-exists-p dbfile)
	    ;; なかったらエラーにする。作成するのに時間がかかるものね。
	    (skk-error "データベース辞書 %s が見つかりません"
		       "Cannot find out database jisyo %s" dbfile))
	(or (file-readable-p dbfile)
	    (skk-error "データベース辞書 %s が読めません"
		       "Cannot read database jisyo %s" dbfile)))
      (or nomsg
	  (skk-message "SKK 辞書データベース %s を開いています..."
		       "Opening SKK dictionary database %s ..."
		       (file-name-nondirectory dbfile)))
      (setq skk-dbm-type
	    (cond
	     ((and (featurep 'berkeley-db)
		   (string-match "\\.db$" dbfile))
	      'berkeley-db)
	     ((and (featurep 'gdbm)
		   (string-match "\\.gdbm$" dbfile))
	      'gdbm)
	     ((and (featurep 'dbm)
		   (not (string-match "\\.db$" dbfile))
		   (not (string-match "\\.gdbm$" dbfile)))
	      'dbm)
	     (t
	      (skk-error
	       "データベース辞書を開くための適当なタイプを決めることができません"
	       "Cannot find out proper type for opening database jisyo")))
	    access (if (string= (file-name-nondirectory dbfile)
				(file-name-nondirectory skk-dbm-jisyo))
		       "+rw" "r"))
      (and skk-dbm-subtype (not (eq skk-dbm-type 'berkeley-db))
	   (skk-error "データベースタイプとサブタイプが矛盾しています"
		      "Database type and subtype conflicts"))
      (and (string= access "+rw")
	   (not (file-writable-p (file-name-directory dbfile)))
	   (skk-error "%s に書き込み権限がありません"
		      "You don't have write permission to %s"
		      (file-name-directory dbfile)))
      (setq database (open-database dbfile skk-dbm-type skk-dbm-subtype access
				    modes))
      (or (databasep database)
	  (skk-error "SKK 辞書データベース %s を開くことができません"
		     "Cannot open SKK dictionary database %s"
		     (file-name-nondirectory dbfile)))
      (or nomsg
	  (skk-message "SKK 辞書データベース %s を開いています...完了！"
		       "Opening SKK dictionary database %s ...done"
		       (file-name-nondirectory dbfile)))
      database)))

(defun skk-dbm-init-working-buffer ()
  (or (get-buffer skk-dbm-working-buffer)
      (with-current-buffer (get-buffer-create skk-dbm-working-buffer)
	(set-buffer-file-coding-system skk-dbm-coding-system)
	(buffer-disable-undo)
	(auto-save-mode -1)
	(setq buffer-read-only nil
	      case-fold-search nil
	      major-mode 'skk-jisyo-mode
	      mode-name "SKK dbmdic"))))

(defun skk-dbm-update-jisyo (word &optional purge)
  (let* ((database (cdr (assoc (expand-file-name skk-dbm-jisyo) skk-dbm-alist)))
	 (midasi (if skk-use-numeric-conversion
		     (skk-num-compute-henkan-key skk-henkan-key)
		   skk-henkan-key))
	 (old-str (get-database midasi database))
	 (inhibit-quit t)
	 (henkan-buffer (current-buffer))
	 old-entry okurigana)
    (if (> skk-okuri-index-min -1)
	(setq word (skk-remove-common word)
	      midasi skk-henkan-key))
    (setq okurigana (or skk-henkan-okurigana skk-okuri-char))
    (with-current-buffer skk-dbm-working-buffer
      (let ((skk-okuri-ari-min (point-min)) ; dymmy
	    (skk-okuri-nasi-min (point-min)) ; dymmy
	    buffer-read-only)
	(setq skk-henkan-key midasi)
	(erase-buffer)
	(if (not old-str)
	    nil
	  (goto-char (point-min))
	  (insert old-str)
	  (goto-char (1+ (point-min)))
	  ;; skk-compute-henkan-lists と skk-update-jisyo-1 は、ポイントに依存せ
	  ;; ず、文字列 (あるいはリスト) を引数に取って文字列 (あるいはリスト)
	  ;; を返すような、もっと一般的なライブラリにするべきかな。
	  (setq old-entry (skk-compute-henkan-lists okurigana))
	  (erase-buffer))
	(skk-update-jisyo-1 okurigana word old-entry purge)
	(if (> 1 (buffer-size))
	    nil
	  (goto-char (point-min))
	  (search-forward " /" nil)
	  (put-database
	   skk-henkan-key
	   (buffer-substring-no-properties (1- (point)) (point-max))
	   database 'replace))
	(and skk-update-end-function
	     (funcall skk-update-end-function
		      henkan-buffer midasi okurigana word purge))))))

;;;###autoload
(defun skk-dbm-make-jisyo (file dbm &optional type subtype nomsg)
  (save-match-data
    (let ((start (current-time)))
      (or nomsg
	  (skk-message "SKK データベース辞書を作成しています..."
		       "Making SKK database jisyo..."))
      (or type (setq type (cond ((featurep 'berkeley-db) 'berkeley-db)
				((featurep 'gdbm) 'gdbm)
				(t 'dbm))))
      (and subtype (not (eq type 'berkeley-db))
	   (skk-error
	    "berkeley-db でないデータベースに subtype を指定することはできません"
	    "Cannot specify subtype for a non berkeley-db database"))
      (save-excursion
	(set-buffer (get-buffer-create " *skk-work*"))
	(let ((dbase (or (open-database (expand-file-name dbm) type subtype "+")
			 ;; モードを指定すると何故か database を open できない。
			 ;;0600)
			 (skk-error "データベース %s を開くことができません"
				    "Cannot open database %s" dbm)))
	      enable-character-translation enable-character-unification
	      midasi cand)
	  (and (eq type 'gdbm) (fboundp 'set-database-property)
	       (set-database-property dbase 'fastmode t))
	  (buffer-disable-undo)
	  (erase-buffer)
	  ;; coding-system-for-read が undecided や automatic-conversion じゃ文字コー
	  ;; ド誤判定になってしまう... (on XEmacs 21.2b7)。
	  (insert-file-contents-as-coding-system 
	   (cond ((and skk-jisyo-code (coding-system-p skk-jisyo-code))
		  skk-jisyo-code)
		 ((and skk-jisyo-code (stringp skk-jisyo-code))
		  (cdr (assoc skk-jisyo-code skk-coding-system-alist)))
		 (t skk-dbm-coding-system))
	   (expand-file-name file))
	  (goto-char (point-min))
	  (while (= (forward-line 1) 0)
	    (beginning-of-line)
	    (if (or (looking-at ";") (eobp))
		nil
	      (setq midasi (buffer-substring-no-properties
			    (point) (search-forward " ")))
	      (and (string-match " $" midasi)
		   (setq midasi (substring midasi 0 (match-beginning 0))))
	      (setq cand (buffer-substring-no-properties
			  (point) (progn (end-of-line) (point))))
	      (put-database midasi cand dbase 'replace)))
	  (close-database dbase)
	  (or nomsg
	      (skk-message "SKK データベース辞書を作成しています...完了！"
			   "Making SKK database jisyo...done"))
	  (sit-for 1)
	  (or nomsg
	      (skk-message "データベース辞書を作成するのに %s 秒かかりました"
			   "It took %s minutes to make database jisyo"
			   (skk-time-difference start (current-time))))
	  (sit-for 2))))))

(defun skk-dbm-make-private-jisyo ()
  (save-match-data
    (let* ((type
	    (cond
	     ((and (featurep 'berkeley-db)
		   (string-match "\\.db$" skk-dbm-jisyo))
	      'berkeley-db)
	     ((and (featurep 'gdbm)
		   (string-match "\\.gdbm$" skk-dbm-jisyo))
	      'gdbm)
	     ((and (featurep 'dbm)
		   (not (string-match "\\.db$" skk-dbm-jisyo))
		   (not (string-match "\\.gdbm$" skk-dbm-jisyo)))
	      'dbm)
	     (t
	      (skk-error
	       "データベース辞書を作成するための適当なタイプを決めることができません"
	       "Cannot find out proper type for making database jisyo"))))
	   (subtype (and (eq type 'berkeley-db) 'hash)))
      (skk-dbm-make-jisyo skk-jisyo skk-dbm-jisyo type subtype))))

(defun skk-dbm-close-all-database ()
  (let ((alist skk-dbm-alist)
	e)
    (condition-case nil
	(progn
	  (while alist
	    (and (setq e (car alist))
		 (database-live-p (cdr e))
		 (close-database  (cdr e)))
	    (setq alist (cdr alist)))
	  ;; set global alist to nil if successfully finished.
	  (setq skk-dbm-alist nil))
      (error
       ;; if error occurred, delete such element from skk-dbm-alist.
       (setq skk-dbm-alist (delq e skk-dbm-alist))))))

;; advices.
(defadvice close-database (around skk-ad activate)
  (let ((file (database-file-name (ad-get-arg 0))))
    (prog1
	ad-do-it
      ;; this should do in Emacs internal.
      (setq skk-dbm-alist (delq (assoc file skk-dbm-alist) skk-dbm-alist)))))
	
;;(add-hook 'kill-emacs-hook 'skk-dbm-close-all-database)

(run-hooks 'skk-dbm-load-hook)

(provide 'skk-dbm)
;; Local Variables:
;; mode: auto-fill
;; fill-column: 78
;; End:

;;; skk-dbm.el ends here
