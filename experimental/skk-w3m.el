;;; skk-w3m.el --- SKK search using w3m-search -*- coding: euc-jp -*-
;; Copyright (C) 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-w3m.el,v 1.35 2012/01/05 12:06:11 skk-cvs Exp $
;; Keywords: japanese
;; Created: Apr. 12, 2001 (oh, its my brother's birthday!)
;; Last Modified: $Date: 2012/01/05 12:06:11 $

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

;; w3m (http://w3m.sourceforge.net/) を利用し、Emacs の中から Web 検
;; 索エンジンによる検索をし、検索結果の中から SKK の候補として取り出
;; したいものを切り出して利用するプログラムです。
;;
;; skk-w3m-use-w3m-backend が non-nil であれば、w3m を backend オプ
;; ション付きで起動して w3m と直接交信します。
;; nil であれば emacs-w3m (http://emacs-w3m.namazu.org/) を経由して
;; w3m を利用します (emacs-w3m では w3m を backend で動かしていません)。
;; w3m backend を利用することで、検索の度毎に w3m を起動する必要がなく
;; なり、プロセスの起動、終了に伴なうオーバーヘッドを減らすことができま
;; す。
;; w3m backend は開発中で、今後大幅な仕様変更が行なわれる可能性もあり
;; 予断を許しませんが、一方で emacs-w3m も開発のスピードが早いプログラ
;; ムです。変動する要素は少ない方が、様々な環境に合う可能性が少しでも上
;; がるので、可能な限り skk-w3m-use-w3m-backend を non-nil で使用するこ
;; とをお勧めします。
;;
;; <HOW TO INSTALL>
;; このファイルを SKK-MK があるディレクトリにコピーし (リンク
;; が使えるファイルシステムでは SKK-MK のあるディレクトリで
;;   ln -s ./experimental/skk-w3m.el .
;; した方が良いかもしれません)、後は普通に make install するだけです。
;;
;; <HOW TO WORK>
;; skk-search-prog-list に (skk-w3m-search "goo-daijirin") のような
;; 要素を追加します。通常、他のどの skk search engine よりも最も遅い
;; ので、最も最後が良いでしょう。こんな感じになります。
;;
;; (setq skk-search-prog-list
;;       '((skk-search-jisyo-file skk-jisyo 0 t)
;;         (skk-search-server skk-aux-large-jisyo 10000)
;;         (skk-w3m-search "goo-daijirin")
;;         (skk-w3m-search "goo-exceed-eiwa")))
;;
;; skk-w3m-search の引数は検索エンジンの種類を文字列で指定します。
;; 但し、skk-w3m-search-engine-alist に対応するエントリが必要です。
;;
;; skk-w3m.el では search-engine 毎に検索結果を cache します。
;; (skk-w3m-search "goo-daijirin" t) のように `skk-w3m-search' の第
;; 二引数に non-nil argument を指定すると cache を行なわず、毎回 w3m
;; に検索をさせます。
;;
;; <TODO>
;; o とりあえず skk-w3m-get-candidates-from-goo-exceed-waei,
;;   skk-w3m-get-candidates-from-goo-exceed-eiwa,
;;   skk-w3m-get-candidates-from-goo-daily-shingo を完成させる。
;; o 検索エンジンの増加。
;; o lookup は w3m-search.el を使った Web search を統合しないのだろう
;;   か...。統合すれば skk-lookup.el で一元管理できる？
;; o w3m backend の改良に追従。

;;; Code

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars)
  (condition-case nil
      (progn (require 'w3m) (require 'w3m-search))
   (error)))

(defgroup skk-w3m nil "SKK w3m related customization."
  :prefix "skk-w3m-"
  :group 'skk)

;;;; user variables.
(defvar skk-w3m-search-engine-alist
  '(("goo-daijirin"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=2" euc-japan
     skk-w3m-get-candidates-from-goo-daijirin
     (or
      ;; cannot search a key which contains okuri prefix.
      skk-okuri-char
      ;; cannot search by Web engine a string which containing SKK special `#' character.
      skk-num-list skk-num-recompute-key
      ;; this engine does not contain English entries.
      skk-abbrev-mode))
    ("goo-exceed-waei"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=1" euc-japan
     ;;skk-w3m-get-candidates-from-goo-exceed-waei ; not yet finished.
     nil
     (or skk-okuri-char skk-num-list skk-num-recompute-key skk-abbrev-mode))
    ("goo-exceed-eiwa"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=0" euc-japan
     ;;skk-w3m-get-candidates-from-goo-exceed-eiwa ; not yet finished.
     nil
     (not skk-abbrev-mode))
    ("goo-daily-shingo"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=3" euc-japan
    ;;skk-w3m-get-candidates-from-goo-daily-shingo ; not yet finished.
     nil
     (or skk-okuri-char skk-num-list skk-num-recompute-key))
    ("quote-yahoo"
     "http://quote.yahoo.com/m5?a=%s&s=%s&t=%s&c=0" nil
     skk-w3m-get-candidates-from-quote-yahoo ; not yet finished.
     nil ;(not skk-abbrev-mode)
     nil
     skk-w3m-make-query-quote-yahoo))
  "*検索エンジン毎の検索オプションを指定するエーリスト。
car は検索エンジンを表わす文字列、
1th は URL \(検索文字列を %s で表わす\),
2th は Web page の coding-system,
3th は候補切り出しに使用する関数を表わすシンボル。
4th \(optional\) は S 式を指定し、評価して non-nil になる状態のときは w3m
    に検索処理をさせない。
5th \(optional\) は `skk-henkan-key' を加工する関数。
6th \(optional\) は 1th のテンプレートに合わせた文字列を出力する関数名。
    指定された関数は、見出し語\(string\) を引数として `funcall' される。
    指定がない場合は、`w3m-search-escape-query-string' が `funcall' される。")

(defvar skk-w3m-use-w3m-backend t
  "*Non-nil であれば、w3m を backend オプション付きで起動して検索を行なう。
`start-process' が使えない Emacs では利用不可。
nil であれば、emacs-w3m を経由して w3m を利用する (現在の emacs-w3m では
w3m を backend で動かしていない)。")

(defvar skk-w3m-command (or (and (boundp 'w3m-command) w3m-command) "w3m")
  "*w3m コマンド名。")

(defvar skk-w3m-command-args "-backend"
  "*w3m の backend オプション。")

(defvar skk-w3m-backend-command-prompt "w3m>"
  "*w3m backend のコマンドプロンプト。")

(defvar skk-w3m-default-process-coding-system 'euc-japan
  "*w3m backend プロセスのディフォルトの coding-system。")

(defvar skk-w3m-kill-command "quit"
  "*w3m backend の終了コマンド。")

(defvar skk-w3m-no-wait nil
  "*Non-nil であれば、w3m backend プロセスが何か出力するまで待たない。")

(defvar skk-w3m-quote-yahoo-currency-symbol-alist
  ;;http://quote.yahoo.com/m5?a=1&s=USD&t=JPY&c=0 ; U.S. Dollar, Japanese Yen
  ;;http://quote.yahoo.com/m5?a=1&s=DEM&t=JPY&c=0 ; German Mark
  ;;http://quote.yahoo.com/m5?a=1&s=FRF&t=JPY&c=0 ; French Franc
  ;;http://quote.yahoo.com/m5?a=1&s=EUR&t=JPY&c=0 ; Euro
  ;;http://quote.yahoo.com/m5?a=1&s=ITL&t=JPY&c=0 ; Italian Lira .L
  ;;http://quote.yahoo.com/m5?a=100&s=KRW&t=JPY&c=0 ; Korean Won
  ;;http://quote.yahoo.com/m5?a=100&s=MYR&t=JPY&c=0 ; Malaysian Ringgit
  ;;http://quote.yahoo.com/m5?a=100&s=THB&t=JPY&c=0 ; Thai Baht
  ;;http://quote.yahoo.com/m5?a=100&s=CHF&t=JPY&c=0 ; Swiss Franc
  '((ARS . "Argentine Peso") (CHF . "Swiss Franc") (DEM . "German Mark")
    (EUR . "Euro") (FRF . "French Franc") (ITL . "Italian Lira .L")
    (JP . "Japanese Yen") (KRW . "Korean Won") (MYR . "Malaysian Ringgit")
    (THB . "Thai Baht") (USD . "U.S. Dollar"))
  "*")

;;;; system internal variables and constants.
;;; constants.
(defconst skk-w3m-working-buffer " *skk-w3m-work*")
(defconst skk-w3m-w3m-w3m-retrieve-has-new-argument-spec
  (condition-case nil
      (with-temp-buffer
	(w3m-w3m-retrieve "http://openlab.ring.gr.jp")
	nil)
    (wrong-number-of-arguments t)
    (error)))

;;; global variables
(defvar skk-w3m-process nil)
(defvar skk-w3m-cache nil)
(defvar skk-w3m-currency-from nil)
(defvar skk-w3m-currency-to nil)

;;;; macros
(defmacro skk-w3m-with-work-buffer (&rest body)
  "Execute the forms in BODY with working buffer as the current buffer."
  `(with-current-buffer
       (w3m-get-buffer-create skk-w3m-working-buffer)
     ,@body))

(put 'skk-w3m-with-work-buffer 'lisp-indent-function 0)
(put 'skk-w3m-with-work-buffer 'edebug-form-spec '(body))

;;;; inline functions
(defsubst skk-w3m-process-alive ()
  (and skk-w3m-process
       (memq (process-status skk-w3m-process) '(run stop))))

;;;; functions
;;; common tools
;;;###autoload
(defun skk-w3m-search (search-engine &optional no-cache)
  (let* ((dbase (assoc search-engine skk-w3m-search-engine-alist))
	 (sex (nth 4 dbase))
	 cache v)
    (if (and dbase
	     (or (not sex)	       ; always search this engine, or
		 (not (eval sex))))	; search this time.
	(if (and (not no-cache)
		 (setq cache (cdr (assoc search-engine skk-w3m-cache))
		       cache (cdr (assoc skk-henkan-key cache))))
	    cache
	  (condition-case nil
	      (prog1
		  (setq v (if skk-w3m-use-w3m-backend
			      (skk-w3m-search-by-backend dbase skk-henkan-key)
			    (skk-w3m-search-by-emcas-w3m dbase skk-henkan-key)))
		(or no-cache
		    (skk-w3m-cache search-engine skk-henkan-key v)))
	    (error)))))) ; catch network unreachable error or something like that.

(defun skk-w3m-cache (search-engine key list)
  (let ((cache (assoc search-engine skk-w3m-cache))
	l)
    (cond
     ((and cache (setq l (assoc key cache)))
      (setcdr l list))
     (cache
      (setcdr cache (cons (cons key list) (cdr cache))))
     (t
      (setq skk-w3m-cache (cons (cons search-engine (list (cons key list)))
				skk-w3m-cache))))))

(defun skk-w3m-filter-string (string filters)
  (while filters
    (while (string-match (car filters) string)
      (setq string (concat (substring string 0 (match-beginning 0))
			   (substring string (match-end 0)))))
    (setq filters (cdr filters)))
  string)

;;; emacs-w3m dependent
(defun skk-w3m-search-by-emcas-w3m (dbase key)
  (require 'w3m)
  (require 'w3m-search)
  (let ((post-process (nth 3 dbase))
	(process-key (nth 5 dbase))
	(query-string-function (nth 6 dbase))
	(w3m-async-exec nil))
    (if process-key (setq key (funcall process-key key)))
    (if post-process
	(skk-w3m-with-work-buffer
	  (or (skk-w3m-w3m-retrieve
	       (if query-string-function
		   (apply 'format (nth 1 dbase)
			  (funcall query-string-function key))
		 (format (nth 1 dbase)
			 (w3m-search-escape-query-string key (nth 2 dbase)))))
	      (error ""))
	  (decode-coding-region (point-min) (point-max) (nth 2 dbase))
	  (prog1
	      (funcall post-process key)
	    ;; not to enlarge working buffer
	    (erase-buffer))))))

(defun skk-w3m-w3m-retrieve (url)
  (if skk-w3m-w3m-w3m-retrieve-has-new-argument-spec
      ;;(w3m-w3m-retrieve url no-decode no-cache post-data referer handler)
      (w3m-w3m-retrieve url nil t nil nil nil)
    (w3m-w3m-retrieve url)))

;;; w3m backend dependent
(defun skk-w3m-search-by-backend (dbase key)
  (let (pos)
    (skk-w3m-with-work-buffer
      (or (skk-w3m-process-alive) (skk-w3m-init-w3m-backend))
      (let ((process-key (nth 5 dbase))
	    (post-process (nth 3 dbase))
	    (query-string-function (nth 6 dbase)))
	(if (not post-process)
	    nil
	  (if process-key
	      (setq key (funcall process-key key)))
	  (if (nth 2 dbase)
	      (skk-w3m-set-process-coding-system (nth 2 dbase)))
	  (message "Reading...")
	  (setq pos (skk-w3m-run-command
		     (concat "get "
			     (if query-string-function
				 (apply 'format (nth 1 dbase)
					(funcall query-string-function key))
			       (format (nth 1 dbase)
				       (skk-w3m-search-escape-query-string
					key (nth 2 dbase)))))))
	  (message "Reading...done")
	  (if pos
	      (progn
		(goto-char pos)
		;; not to enlarge working buffer
		(delete-region (point-min) (progn (beginning-of-line) (point)))
		(setq key (funcall post-process key)))))))))

(defun skk-w3m-set-process-coding-system (coding-system)
  (static-cond
   ((featurep 'xemacs)
    (set-process-input-coding-system skk-w3m-process coding-system)
    (set-process-output-coding-system skk-w3m-process coding-system))
   (t
    (set-process-coding-system skk-w3m-process coding-system coding-system))))

(defun skk-w3m-init-w3m-backend ()
  (let ((process-connection-type t))
    (buffer-disable-undo)
    ;;(insert "\nStarting w3m backend...\n\n")
    (skk-message "skk のために w3m backend を起動しています..."
		 "Starting w3m backend for skk...")
    (condition-case nil
	(progn
	  (setq skk-w3m-process
		(start-process "skk w3m" (current-buffer) skk-w3m-command
			       skk-w3m-command-args))
	  (static-cond
	   ((and (string-match "^GNU" (emacs-version))
		 (string-lessp "22.0" emacs-version))
	    (set-process-query-on-exit-flag skk-w3m-process nil))
	   (t
	    (process-kill-without-query skk-w3m-process)))
	  (skk-w3m-set-process-coding-system
	   skk-w3m-default-process-coding-system))
      (file-error (skk-error "システム上に \"%s\" が見つかりません"
			     "Sorry, can't find \"%s\" on your system"
			     skk-w3m-command))
      (error (skk-w3m-kill 'nomsg)))
    (if (eq (process-status skk-w3m-process) 'exit)
	(progn
	  (skk-w3m-kill 'nomsg)
	  (skk-error "%s プロセスが異常終了しました。"
		     "Process %s exited abnormally with code 1"
		     skk-w3m-process)))
    (while (and (memq (process-status skk-w3m-process) '(run stop))
		(goto-char (point-min))
		(not (re-search-forward skk-w3m-backend-command-prompt nil t)))
      (accept-process-output skk-w3m-process))
    ;;(or (memq (process-status skk-w3m-process) '(run stop))
    ;;    (skk-error "w3m backend プロセスをスタートすることができません"
    ;;               "Unable to start w3m backend process"))
    (goto-char (process-mark skk-w3m-process))
    (skk-message "skk のために w3m backend を起動しています...完了!"
		 "Starting w3m backend for skk...done")))

(defun skk-w3m-kill (&optional nomsg)
  "w3m backend プロセスを殺す。"
  (interactive "P")
  (if (not (skk-w3m-process-alive))
      ;; 北斗神拳の世界ですな...。
      (or nomsg
	  (skk-message "w3m backend プロセスは既に死んでます"
		       "w3m backend process has already died"))
    (with-current-buffer (get-buffer skk-w3m-working-buffer)
      (unwind-protect
	  (let ((skk-w3m-no-wait t))
	    (skk-w3m-run-command skk-w3m-kill-command)
	    ;;(sit-for 1)
	    (and (process-status skk-w3m-process)
		 (delete-process skk-w3m-process))
	    ;;(setq skk-w3m-process nil)
	    (or nomsg
		(skk-message "w3m backend プロセスが死にました"
			     "w3m backend process died")))
	(kill-buffer (current-buffer))))))

(defun skk-w3m-run-command (command)
  ;; return last point where last command issued.
  (save-match-data
    (setq command (concat command " \n"))
    (let ((pmark (process-mark skk-w3m-process))
	  origpoint)
      (accept-process-output)
      ;; 動いたポイントを保存するため save-excursion は使わない。
      (goto-char pmark)
      (setq origpoint (point))
      (insert command)
      (set-marker pmark (point))
      (process-send-string skk-w3m-process command)
      (accept-process-output (and (not skk-w3m-no-wait) skk-w3m-process))
      (goto-char origpoint)
      (while (and (not (re-search-forward
			skk-w3m-backend-command-prompt pmark t))
		  ;; quit コマンドを送ったらプロンプトは帰ってこない。
		  (not (eq (process-status skk-w3m-process) 'exit)))
	(accept-process-output))
      ;;(skk-w3m-check-errors)
      origpoint)))

;; just a copy of w3m-url-encode-string of w3m.el
(defun skk-w3m-url-encode-string (str &optional coding)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     (t
	      (format "%%%02X" ch))))	; escape
	  ;; Coerce a string to a list of chars.
	  (append (encode-coding-string str (or coding 'iso-2022-jp))
		  nil))))

;; just a copy of w3m-search-escape-query-string of w3m-search.el
(defun skk-w3m-search-escape-query-string (str &optional coding)
  (mapconcat (lambda (s)
	       (skk-w3m-url-encode-string
		s (or coding skk-w3m-default-process-coding-system)))
	     (split-string str)
	     "+"))

;;; process functions for each databases.
(defun skk-w3m-get-candidates-from-goo-daijirin (key)
  (save-match-data
    (let ((startregexp
	   (if skk-w3m-use-w3m-backend
	       nil
	       ;;(format
		;;"■［%s］の大辞林第二版からの検索結果　 <b>[0-9]+件</b>" key)
	     "<!-- RESULT_BLOCK -->"))
	  (endregexp
	   (if skk-w3m-use-w3m-backend
	       nil
	       ;;(format
		;;"■［%s］の大辞林第二版からの検索結果　 <b>[0-9]+件</b>" key)
	     "<!-- RESULT_BLOCK -->"))
	  (start (if skk-w3m-use-w3m-backend (point-min)))
	  (end (if skk-w3m-use-w3m-backend (process-mark skk-w3m-process)))
	  temp v)
      (if startregexp
	  (progn
	    (re-search-forward startregexp nil t nil)
	    (setq start (point))))
      (if endregexp
	  (progn
	    (re-search-forward endregexp nil t nil)
	    (setq end (point))))
      (if (not (and start end))
	  nil
	(goto-char start)
	(setq key (mapconcat 'char-to-string key "-*"))
	(setq key (format "\\(%s\\|%s\\)"
			  ;; <b>8</b>  <a href="/cgi-bin/jp-more_print.cgi?MT=%A4%AB%A4%F3%A4%AD%A4%E7%A4%A6&amp;ID=a4ab/04290800.txt&amp;sw=2" target="_blank" hseq="35"><img_alt src="/Common/icon01.gif">新規で開く</img_alt></a>  <a href="/cgi-bin/jp-more_print.cgi?MT=%A4%AB%A4%F3%A4%AD%A4%E7%A4%A6&amp;ID=a4ab/04290800.txt&amp;sw=2" hseq="36">かんきょう【艦橋】</a>
			  (format "<a href=\".+\">%s *【\\([^<>【】]+\\)】</a>" key)
			  ;; <B>しこう-さくご―かう―【試行錯誤】</B>
			  ;; <B>がいはんぼしぐわいはん―【外反拇趾】</B>
			  ;; <B>なかみ  【中身・中味】  </B>
			  ;; <B><FONT COLOR="0000FF">しこう-さくご</FONT>  <FONT><SMALL> ―かう―</SMALL></FONT>  【試行錯誤】  </B>
			  ;; <B>えがおゑがほ【《笑顔》】</B>
			  (format "<B>\\(<FONT COLOR=\"[0-9A-Z]+\">\\)*%s[^【]*【\\([^<>【】]+\\)】 *</B>" key)))
	(while (re-search-forward key end t nil)
	  ;; KEY = "\\(<a href=\".+\">し-*こ-*う-*さ-*く-*ご *【\\([^<>【】]+\\)】</a>\\|<B>\\(<FONT COLOR=\"[0-9A-Z]+\">\\)*し-*こ-*う-*さ-*く-*ご[^<【]*【\\([^<>【】]+\\)】 *</B>\\)"
	  (setq temp (skk-w3m-filter-string
		      ;; 〈何時〉
		      ;; 《笑顔》
		      (or (match-string-no-properties 2)
			  (match-string-no-properties 4))
		      '("〈" "〉" "《" "》")))
	  (dolist (elm (split-string temp "・"))
	    ;; do not add a redundant candidate.
	    (unless (member elm v)
	      (setq v (cons elm v)))))
	(nreverse v)))))

(defun skk-w3m-get-candidates-from-goo-exceed-waei (key)
  ;; SORRY, NOT YET.
  ;;   ;; 15:■［ねっしん］のEXCEED和英辞典からの検索結果
  ;;   ;; 16:*
  ;;   ;; 17:
  ;;   ;; 18:ねっしん
  ;;   ;; 19:[clear] 熱心
  ;;   ;; 20:[clear] zeal；　ardor；　eagerness；　enthusiasm．　〜な
  ;;   ;; 21:        eager；　ardent；　keen．　〜に　eagerly；
  ;;   ;; 22:        earnestly；　intently．
  ;;   ;; 23:
  ;;   ;; 24:*
  ;;   ;; 25:■［ねっしん］のEXCEED和英辞典からの検索結果
  ;;   (let (temp v)
  ;;     (save-match-data
  ;;       (if (not (re-search-forward
  ;; 		(concat "■\\［" (regexp-quote key) "\\］のEXCEED和英辞典からの検索結果")
  ;; 		nil t nil))
  ;; 	  nil
  ;; 	(while (re-search-forward "\\[clear\\] [a-z]+\\.　\\([^ a-zA-Z][^．]+\\)．" nil t nil)
  ;; 	  (setq temp (match-string-no-properties 1))
  ;; 	  (setq temp (skk-w3m-filter-string
  ;; 		      ;; [[米話]]
  ;; 		      temp '("\n" "[0-9]+: +" "[　 ]+" "（[ぁ-ん]+）" "([, a-z]+)"
  ;; 			     "\\[\\[[^a-zA-Z]+\\]\\]")))
  ;; 	  (while (string-match "\\([^，；]+\\)［\\([^，；]+\\)］\\([^，；]+\\)*" temp)
  ;; 	    (setq temp (concat (substring temp 0 (match-beginning 0))
  ;; 			       (match-string-no-properties 1 temp)
  ;; 			       (match-string-no-properties 3 temp)
  ;; 			       "，"
  ;; 			       (match-string-no-properties 2 temp)
  ;; 			       (match-string-no-properties 3 temp)
  ;; 			       (substring temp (match-end 0)))))
  ;;
  ;; 	  (setq v (nconc v (split-string temp "[，；]"))))
  ;; 	v))))
  )

(defun skk-w3m-get-candidates-from-goo-exceed-eiwa (key)
  ;; SORRY, NOT YET.
  ;;
  ;; <!-- RESULT_BLOCK -->
  ;; <table width="100%" border="0" cellspacing="0" cellpadding="0"><tr><td>
  ;; <!-- ej_res1 -->
  ;; <table width="100%" border="0" cellspacing="0" cellpadding="0">
  ;;   <tr>
  ;;     <td>
  ;;       ■［<font color="#993333">collaborate</font>］のEXCEED英和辞典からの検索結果
  ;;     </td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td bgcolor="#993333"><img src="/Common/clear.gif" width="1" height="1" alt=""></td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td>
  ;;       <br>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR ALIGN="LEFT" VALIGN="MIDDLE">
  ;;     <TD>
  ;;       <SPAN CLASS="css4g">
  ;;         <B>col・lab・o・rate</B>　<A HREF="http://dictionary2.goo.ne.jp/ej/voice/C/01010419.wav"><IMG LOWSRC="/ej/image/voice.gif" WIDTH="23" HEIGHT="12" BORDER="0" ALIGN="absmiddle"></A>
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <IMG SRC="/ej/image/e1073.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1015.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1016.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1022.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1001.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e101b.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1054.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1013.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1003.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1074.gif" WIDTH="8" HEIGHT="16" ALT="">
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>vi.</i>　共に働く；　共同研究する　<i>(with, on, in)；</i>　敵側［占領軍］に協力する．
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="48" HEIGHT="2"></TD>
  ;;     <TD WIDTH="420" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <FONT COLOR="FF0000">collaboration</FONT>　<A HREF="http://dictionary2.goo.ne.jp/ej/voice/C/02020773.wav"><IMG LOWSRC="/ej/image/voice.gif" WIDTH="23" HEIGHT="12" BORDER="0" ALIGN="absmiddle"></A>
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>n.</i>　<FONT COLOR="FF0000">collaborationism</FONT>　<i>n.</i>　<FONT COLOR="FF0000">collaborationist</FONT>　<i>n.</i>　（敵側への）協力者．
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="48" HEIGHT="2"></TD>
  ;;     <TD WIDTH="420" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <FONT COLOR="FF0000">collaborative</FONT>
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <IMG SRC="/ej/image/e1073.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1015.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1016.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1022.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1001.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e101b.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1054.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1013.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1003.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1013.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e101d.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1074.gif" WIDTH="8" HEIGHT="16" ALT="">
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>a.</i>　共同制作の．
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="48" HEIGHT="2"></TD>
  ;;     <TD WIDTH="420" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <FONT COLOR="FF0000">collaborator</FONT>　<A HREF="http://dictionary2.goo.ne.jp/ej/voice/C/02020774.wav"><IMG LOWSRC="/ej/image/voice.gif" WIDTH="23" HEIGHT="12" BORDER="0" ALIGN="absmiddle"></A>
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>n.</i>
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;;       <br>
  ;;     </td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td bgcolor="#993333"><img src="/Common/clear.gif" width="1" height="1" alt=""></td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td>
  ;;       ■［<font color="#993333">collaborate</font>］のEXCEED英和辞典からの検索結果
  ;;     </td>
  ;;   </tr>
  ;; </table>
  ;; <!-- ej_res1 -->
  ;; </td></tr></table>
  ;; <!-- RESULT_BLOCK -->
  ;;
  ;; con・tem・po・ra・ry
  ;; [clear] ●●●●●●●●●●●●●●
  ;; [clear] a., n.　同時代の（人，雑誌）　(with)；　同年齢の（
  ;; 人）；　現代の（人）．
  ;;
  ;; *
  ;; ■［contemporary］のEXCEED英和辞典からの検索結果
  ;;
  ;; 14:■［collaborate］のEXCEED英和辞典からの検索結果
  ;; 15:*
  ;; 16:
  ;; 17:col・lab・o・rate
  ;; 18:[clear] ●●●●●●●●●●●●
  ;; 19:[clear] vi.　共に働く；　共同研究する　(with, on, in)；
  ;; 20:        敵側［占領軍］に協力する．
  ;; 21:[clear] collaboration
  ;; 22:[clear] n.　collaborationism　n.　collaborationist　n.　（
  ;; 23:        敵側への）協力者．
  ;; 24:[clear] collaborative
  ;; 25:[clear] ●●●●●●●●●●●●●●
  ;; 26:[clear] a.　共同制作の．
  ;; 27:[clear] collaborator
  ;; 28:[clear] n.
  ;; 29:
  ;; 30:*
  ;; 31:■［collaborate］のEXCEED英和辞典からの検索結果
  ;;
  ;; ■［very］のEXCEED英和辞典からの検索結果　 2件
  ;; *
  ;;
  ;; 1  新規で開く  very
  ;;
  ;; 2  新規で開く  Very light
  ;;
  ;; *
  ;; ■［very］のEXCEED英和辞典からの検索結果　 2件
  ;;
  ;; ■［contemporary］のEXCEED英和辞典からの検索結果
  ;; *
  ;;   (save-match-data
  ;;     (let (temp v start end)
  ;;       (if (not (search-forward "<!-- RESULT_BLOCK -->" nil t nil))
  ;; 	  nil
  ;; 	(setq start (point))
  ;; 	(if (search-forward "<!-- RESULT_BLOCK -->" nil t nil)
  ;; 	    (setq end (point)))
  ;; 	(goto-char start)
  ;; 	(setq key (concat "<a href=\".+\">" (regexp-quote key) " +【\\([^【】]+\\)】</a>"))
  ;; 	(while (re-search-forward key end t nil)
  ;; 	  (setq temp (skk-w3m-filter-string
  ;; 		      ;; 〈何時〉
  ;; 		      (match-string-no-properties 1) '("〈" "〉")))
  ;; 	  (setq v (nconc (split-string temp "・") v)))
  ;; 	(nreverse v)))))
  ;;   (save-match-data
  ;;     (let (v)
  ;;       (if (not (re-search-forward "[0-9]+  新規で開く" nil t nil))
  ;; 	  (if (re-search-forward
  ;; 	       (concat "■\\［" (regexp-quote key) "\\］のEXCEED英和辞典からの検索結果")
  ;; 	       nil t nil)
  ;; 	      (setq v (skk-w3m-get-candidates-from-goo-exceed-eiwa-1)))
  ;; 	(beginning-of-line)
  ;; 	(while (re-search-forward "[0-9]+  新規で開く" nil t nil)
  ;; 	  (backward-char)
  ;; 	  (w3m-view-this-url)
  ;; 	  (goto-char (point-min))
  ;; 	  (if (re-search-forward
  ;; 	       (concat "■\\［" (regexp-quote key) "\\］のEXCEED英和辞典からの検索結果")
  ;; 	       nil t nil)
  ;; 	      (setq v (nconc v (skk-w3m-get-candidates-from-goo-exceed-eiwa-1))))
  ;; 	  (w3m-view-previous-page)))
  ;;       v))
  )

;; (defun skk-w3m-get-candidates-from-goo-exceed-eiwa-1 ()
;;   (save-match-data
;;     (let (temp temp1 temp2 temp3 tail v)
;;       (while (re-search-forward
;; 	      "\\[clear\\] [a-z]+\\.\\(, [a-z]+\\.\\)*　\\([^ a-zA-Z][^．]+\\)．"
;; 	      nil t nil)
;; 	(setq temp (match-string-no-properties 2))
;; 	(setq temp (skk-w3m-filter-string
;; 		  ;; e.x. `捺染（なつせん）工', `(on, in)', `【経営】'
;; 		    temp '("\n" "[0-9]+: +" "[　 ]+" "（[ぁ-ん]+）" "([, a-z]+)"
;; 			   "…の" "【[^【】]+】" "(強意)")))
;; 	(while (string-match
;; 		;; ((...)) は意味を表わすようだ。
;; 		;; e.x. インジケータ　((機器の作動状態を表示する機能))
;; 		;; 括弧内をあえてフィルタリングしないで出力する。
;; 		"\\([^，；]+\\)\\(［\\|((\\)\\([^，；]+\\)\\(］\\|))\\)\\([^，；]+\\)*"
;; 		temp)
;; 	  (setq temp (concat (substring temp 0 (match-beginning 0))
;; 			     (match-string-no-properties 1 temp)
;; 			     (match-string-no-properties 5 temp)
;; 			     "，"
;; 			     (match-string-no-properties 3 temp)
;; 			     (match-string-no-properties 5 temp)
;; 			     (substring temp (match-end 0)))))
;; 	;; 当惑（の原因） → 当惑，当惑の原因
;; 	;; 同時代の（人，雑誌）→  同時代の，同時代の人，同時代の雑誌
;; 	(while (string-match "\\([^，；]+\\)（\\([^；]+\\)）\\([^，；]+\\)*" temp)
;; 	  (setq temp1 (match-string-no-properties 1 temp)
;; 		temp2 (match-string-no-properties 2 temp)
;; 		temp3 (match-string-no-properties 3 temp)
;; 		tail (substring temp (match-end 0)))
;; 	  (setq temp (concat (substring temp 0 (match-beginning 0))
;; 			     temp1 "，"
;; 			     (mapconcat 'identity
;; 					(mapcar
;; 					 (function (lambda (e) (concat temp1 e temp3)))
;; 					 (split-string temp2 "，"))
;; 					"，")
;; 			     tail)))
;; 	;; （問題を）紛糾させる → 紛糾させる，問題を紛糾させる
;; 	(while (string-match "（\\([^；]+\\)）\\([^，；]+\\)" temp)
;; 	  (setq temp1 (match-string-no-properties 1 temp)
;; 		temp2 (match-string-no-properties 2 temp)
;; 		tail (substring temp (match-end 0)))
;; 	  (setq temp (concat (substring temp 0 (match-beginning 0))
;; 			     temp2 "，"
;; 			     (mapconcat 'identity
;; 					(mapcar
;; 					 (function (lambda (e) (concat e temp2)))
;; 					 (split-string temp1 "，"))
;; 					"，")
;; 			     tail)))
;; 	(setq v (nconc v (split-string temp "[，；]")))
;; 	;; skip to next candidate.
;; 	(or (re-search-forward "\\[clear\\] ●+" nil t nil)
;; 	    (goto-char (point-max))))
;;       v)))

(defun skk-w3m-get-candidates-from-goo-daily-shingo (key)
  ;; not yet.
  ;; 15:■［SPA］のデイリー新語辞典からの検索結果
  ;; 16:*
  ;; 17:
  ;; 18:SPA
  ;; 19:
  ;; 20:  ［speciality store retailer of private label apparel］
  ;; 21:  自社ブランドの衣料品を売る直営店のこと。また，そのような事業形態。衣料品の企
  ;; 22:  画・開発から製造・流通・販売に至るまでを一括して取り扱い，顧客のニーズに効率
  ;; 23:  的に対応する。
  ;; 24:  →プライベート-ブランド
  ;; 25:  〔独自ブランド衣料の専門店販売業者の略。アメリカの衣料小売店による造語が起源
  ;; 26:  〕
  ;; 27:
  ;; 28:
  ;; 29:*
  ;; 30:■［SPA］のデイリー新語辞典からの検索結果
  )

(defun skk-w3m-get-candidates-from-quote-yahoo (key)
  ;;(if (search-forward "U.S. Markets Closed." nil t nil)
  ;;    'closed
  (re-search-forward
   ;; <a href="/q?s=USDJPY=X&amp;d=t" hseq="7">USDJPY</a>
   ;; <a href="/q?s=SFRJPY=X&amp;d=t" hseq="7">SFRJPY=X</a>
   (format "<a href=\"[^>]+%s\\(=X\\)?[^>]+\">%s\\(=X?\\)?</a>"
	   (concat skk-w3m-currency-from skk-w3m-currency-to)
	   (concat skk-w3m-currency-from skk-w3m-currency-to)))
  (re-search-forward "<b>\\([,.0-9]+\\)</b>")
  (match-string-no-properties 1))

(defun skk-w3m-make-query-quote-yahoo (key)
  ;; http://quote.yahoo.com/m5?a=%s&s=%s&t=%s&c=0"
  ;; http://quote.yahoo.com/m5?a=1&s=USD&t=JPY&c=0 ; U.S. Dollar, Japanese Yen
  (while (string-match "," key)
    (setq key (concat (substring key 0 (match-beginning 0))
		      (substring key (match-end 0)))))
  (if (string-match "[.0-9]+" key)
      (list (match-string-no-properties 0 key)
	    skk-w3m-currency-from skk-w3m-currency-to)))

;;;###autoload
(defun skk-w3m-query-quote-yahoo
  ;; $# /(skk-w3m-query-quote-yahoo "USD" "JPY" 'postfix "円")/(skk-w3m-query-quote-yahoo "USD" "DEM" 'prefix "DM")/
  ;; sfr# /(skk-w3m-query-quote-yahoo "CHF" "JPY" 'postfix "円")/
  ;; dm# /(skk-w3m-query-quote-yahoo "DEM" "JPY" 'postfix "円")/
  (currency-from currency-to &optional position convert-currency-to)
  (let (v)
    (setq skk-w3m-currency-from currency-from
	  skk-w3m-currency-to currency-to)
    (setq v (skk-w3m-search "quote-yahoo" 'no-cache))
    ;;(if (eq v 'closed)
    ;;    (message "U.S. markets closed, cannot get currency information!")
    (concat (if (not convert-currency-to) currency-to)
	    (eval (if (eq 'prefix position) convert-currency-to))
	    v
	    (eval (if (eq 'postfix position) convert-currency-to)))))

(provide 'skk-w3m)
;;; Local Variables:
;;; End:
;;; skk-w3m.el ends here
