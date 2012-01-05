;;; skk-rdbms.el --- SKK Relational Data Base Management System. -*- coding: iso-2022-jp -*-
;; Copyright (C) 1998, 2000, 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Maintainer: NAKAJIMA Mikio <minakaji@namazu.org>
;; Version: $Id: skk-rdbms.el,v 1.14 2012/01/05 12:06:11 skk-cvs Exp $
;; Keywords: japanese, rdbms
;; Last Modified: $Date: 2012/01/05 12:06:11 $

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

;; Currently this program only supports PostgreSQL, but may work with
;; other rdbms (such as mSQL or mySQL) by slightly changes.

;; SQL のシェルを Emacs のバッファの中で起動して、SQL シェルにコマンド
;; を送る、という部分は、Emacs Calc の Gnuplot インターフェイス部分で
;; ある calc-graph.el を参考にした。

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

;; User variables.
(defvar skk-rdbms-shell "psql"
  "*データベース本体と接続するためのシェルプログラム。")

(defvar skk-rdbms-shell-args "skk"
  "*skk-rdbms-shell の引数。
ディフォルトでは、skk データベースに接続するための引数を指定している。")

(defvar skk-rdbms-process-coding-system
  (cdr (assoc "euc" skk-coding-system-alist))
  "*skk-rdbms-shell とのプロセス通信に使うコーディングシステム。")

(defvar skk-rdbms-shell-prompt-regexp "^skk=> "
  "*skk-rdbms-shell のプロンプトを示す正規表現。")

(defvar skk-rdbms-shell-version-check-function nil
  "*skk-rdbms-shell のバージョンをチェックする関数。
skk-rdbms-shell のバージョンによって異なる動きをしたい場合に使用する。")

;; 何故か process-send-string したコマンドの直後に帰ってくる文字列の先
;; 頭に ^M が付く。
;; SQL 文の途中に改行を入れると psql の第 2 プロンプトが出る。これを正規表現で
;; 表現して検索するとオーバーヘッドが大きく、確定してからの辞書更新が遅くなる。
(defvar skk-rdbms-error-regexp "^*ERROR: +\\(.+\\)$"
  "*skk-rdbms-shell が出すエラーメッセージの正規表現。
\(match-string 1\)でエラーの本体を切り出せるように正規表現を書く。")

(defvar skk-rdbms-update-fail-regexp "^*UPDATE 0$"
  "*個人辞書テーブルの UPDATE に失敗したときに skk-rdbms-shell が出すメッセージの正規表現。")

(defvar skk-rdbms-shell-special-command-regexp "^\\\\[a-z?!]+$"
  "*skk-rdbms-shell のコマンドで SQL コマンド以外のコマンドを表す正規表現。")

(defvar skk-rdbms-kill-command "\\q"
  "*skk-rdbms-shell 終了のために使用するコマンド。")

(defvar skk-rdbms-SQL-wildcard "%"
  "*LIKE 演算子でパターンマッチングに使われるワイルドカード。")

(defvar skk-rdbms-private-jisyo-table (concat (user-login-name) "_private_jisyo")
  "*個人辞書のテーブル名。

CREATE TABLE YOUR-LOGIN-NAME_private_jisyo \(
	okuriari int2 NOT NULL,
	yomi varchar\(50\) NOT NULL, -- longest entry of yomi in SKK-JISYO.L is
				   -- 'ほくりくせんたんかがくぎじゅつだいがくいんだいがく'
	kanji text NOT NULL,
	okurigana varchar\(4\),
	date abstime NOT NULL
\);

テーブル名の YOUR-LOGIN-NAME の個所は、Emacs の user-login-name が返す値を書く。")

(defvar skk-rdbms-public-jisyo-table "large_jisyo"
  "*共有辞書のテーブル名。

CREATE TABLE large_jisyo \(
	okuriari int2 NOT NULL,
	yomi varchar\(50\) NOT NULL,
	kanji text NOT NULL
\);")

(defvar skk-rdbms-jis2-jisyo-table "jis2_jisyo"
  "*JIS 第 2 水準の文字を集めた辞書テーブル名。

CREATE TABLE jis2_jisyo \(
	okuriari int2 NOT NULL,
	yomi varchar\(50\) NOT NULL,
	kanji text NOT NULL
\);")

(defvar skk-rdbms-kakutei-jisyo-table (concat (user-login-name) "_kakutei_jisyo")
  "*確定辞書のテーブル名。

CREATE TABLE YOUR-LOGIN-NAME_kakutei_jisyo (
	okuriari int2 NOT NULL,
	yomi varchar(50) NOT NULL,
	kanji text NOT NULL
);

テーブル名の YOUR-LOGIN-NAME の個所は、Emacs の user-login-name が返す値を書く。")

(defvar skk-rdbms-initial-jisyo-table (concat (user-login-name) "_initial_jisyo")
  "*initial サーチ辞書のテーブル名。

CREATE TABLE YOUR-LOGIN-NAME_kakutei_jisyo (
	okuriari int2 NOT NULL,
	yomi varchar(50) NOT NULL,
	kanji text NOT NULL
);

テーブル名の YOUR-LOGIN-NAME の個所は、Emacs の user-login-name が返す値を書く。")

(defvar skk-rdbms-kcode-table "kcode"
  "*文字の JIS コード及び Unicode を集めたテーブル名。

CREATE TABLE kcode \(
	kanji varchar\(2\) NOT NULL PRIMARY KEY,
	JIScode char\(4\) NOT NULL UNIQUE,
        Unicode char\(4\) NOT NULL UNIQUE
\);")

(defvar skk-rdbms-stroke-table "stroke"
  "*画数データを集めたテーブル名。

CREATE TABLE stroke \(
	stroke int2 NOT NULL,
	kanji varchar\(2\) NOT NULL PRIMARY KEY
\);")

(defvar skk-rdbms-busyu-base-table "busyu_base"
  "*部首のデータを集めたテーブル名。

CREATE TABLE busyu_base \(
	busyuID int2 NOT NULL, -- radical \(Busyu\) number in the Nelson New
                               -- Japanese-English Character Dictionary.
                               -- why aren't they unique?
	busyuID2 int2, -- classic radical number
	kanji varchar\(2\), -- 部首のみの標記が不能な文字もあるので、
                            -- NOT NULL 条件を付けない。
	yomi1 varchar\(16\) NOT NULL,
	yomi2 varchar\(16\),
	yomi3 varchar\(16\),
	yomi4 varchar\(16\)
\);")

(defvar skk-rdbms-busyu-data-table "busyu_data"
  "*各漢字の部首に関するデータを集めたテーブル名。

CREATE TABLE busyu_data \(
	busyuID int2 NOT NULL, -- radical \(Busyu\) number.
	busyuID2 int2, -- classic radical number
	kanji varchar\(2\) NOT NULL UNIQUE
\);")

(defvar skk-rdbms-hinsi-base-table "hinsi_base"
  "*品詞に関するデータを集めたテーブル名。

CREATE TABLE hinsi_base \(
	hinsi varchar\(23\) NOT NULL, -- longest is `サ行(する)&名詞化接尾語'
	hinsiID int2 NOT NULL,
	hinsiID2 int2
\);")

(defvar skk-rdbms-hinsi-data-table "hinsi_data"
  "*各漢字の品詞に関するデータを集めたテーブル名。

CREATE TABLE hinsi_data \(
	yomi varchar\(26\) NOT NULL, -- longest is `ばっくぷろぱげーしょんほう'
	kanji varchar\(24\) NOT NULL, -- longest is `オペレーティングシステム'
	hinsiID int2 NOT NULL,
	hinsiID2 int2
\);")

(defvar skk-rdbms-private-jisyo-dump "~/.skk-jisyo.dump"
  "*skk-rdbms-private-jisyo-table をダンプして保存するファイル。")

(defvar skk-rdbms-dump-error "~/.skk-dump.error"
  "*skk-rdbms-private-jisyo-dump をダンプする際に発生したエラーメッセー
ジが保存されるファイル。")

;; ユーザー変数にするつもりが、マクロでないと動かないことに気が付い
;; た...。format の引数になっている変数達がコンパイルのときは皆 nil だ
;; からね。PostgreSQL 以外の RDBMS を使う人にはマクロを書き替えてもら
;; う...酷だろうか？
(defmacro skk-rdbms-SQL-insert-command (word)
  `(if (not skk-henkan-okurigana)
       (format
	"INSERT INTO %s (okuriari,yomi,kanji,date) VALUES (%s, '%s', '%s', '%s');"
	skk-rdbms-private-jisyo-table 0 skk-henkan-key ,word
	(current-time-string))
     (format
      "INSERT INTO %s (okuriari,yomi,kanji,okurigana,date) VALUES (%s, '%s', '%s', '%s', '%s');"
      skk-rdbms-private-jisyo-table 1 skk-henkan-key ,word skk-henkan-okurigana
      (current-time-string))))

(defmacro skk-rdbms-SQL-delete-command (word)
  `(concat
    (format
     "DELETE FROM %s WHERE kanji = '%s' AND yomi = '%s' AND okuriari = %s "
     skk-rdbms-private-jisyo-table ,word skk-henkan-key
     (if skk-okuri-char 1 0))
    (if (and skk-henkan-okuri-strictly skk-henkan-okurigana)
	(format "AND okurigana = '%s'" skk-henkan-okurigana))
    ";"))

(defmacro skk-rdbms-SQL-regexp-delete-command (word)
  `(concat
    ;; 当然だけど、正規表現が使えない RDBMS では使えないね...。どん
    ;; どん PostgreSQL 寄りになってゆく...f(^_^;;;。
    (format
     "DELETE FROM %s WHERE kanji ~ '%s' AND yomi = '%s' AND okuriari = %s "
     skk-rdbms-private-jisyo-table ,word skk-henkan-key
     (if skk-okuri-char 1 0))
    (if (and skk-henkan-okuri-strictly skk-henkan-okurigana)
	(format "AND okurigana = '%s'" skk-henkan-okurigana))
    ";"))

(defmacro skk-rdbms-SQL-update-command (word)
  `(concat
    ;; SET date = 'now'::abstime って PostgreSQL だけでしか通用しないコ
    ;; マンド？  SQL92 (というのかな？) ではどう書くんでしょう？
    (format
     "UPDATE %s SET date = 'now'::abstime WHERE kanji = '%s' AND yomi = '%s' AND okuriari = %s "
     skk-rdbms-private-jisyo-table ,word skk-henkan-key (if skk-okuri-char 1 0))
    (if (and skk-henkan-okuri-strictly skk-henkan-okurigana)
	(format "AND okurigana = '%s'" skk-henkan-okurigana))
    ";"))

(defmacro skk-rdbms-SQL-search-jisyo-command (table)
  `(format "SELECT kanji FROM %s WHERE yomi = '%s' AND okuriari = %s;"
	   ,table skk-henkan-key (if skk-okuri-char 1 0)))

(defmacro skk-rdbms-SQL-search-private-jisyo-command ()
  `(concat
    (format "SELECT kanji FROM %s WHERE yomi = '%s' AND okuriari = %s "
	    skk-rdbms-private-jisyo-table skk-henkan-key (if skk-okuri-char 1 0))
    (if (and skk-henkan-okurigana skk-henkan-okuri-strictly)
	(format "AND okurigana = '%s' " skk-henkan-okurigana))
    "ORDER BY date DESC;"))

(defmacro skk-rdbms-SQL-search-completion-word-command ()
  `(format
    "SELECT yomi FROM %s WHERE yomi LIKE '%s%s' AND okuriari = 0 GROUP BY yomi ORDER BY max(date) DESC;"
    skk-rdbms-private-jisyo-table skk-completion-word skk-rdbms-SQL-wildcard))

(defmacro skk-rdbms-SQL-search-busyu-command (key)
  `(format
    ;; SQL 文の途中に改行を入れると psql の第 2 プロンプトが出る。これを正規表現で
    ;; 表現して検索するとオーバーヘッドが大きく、確定してからの辞書更新が遅くなる。
    ;; 従い SQL 文は長くても無理矢理 1 行で表現するか、嫌なら細切れに切って
    ;; concat でつなぐ。
    "SELECT kanji FROM %s WHERE busyuID = (SELECT busyuID FROM %s WHERE yomi1 = '%s' OR yomi2 = '%s' OR yomi3 = '%s' OR yomi4 = '%s');"
    skk-rdbms-busyu-data-table skk-rdbms-busyu-base-table
    ,key ,key ,key ,key))

(defmacro skk-rdbms-SQL-search-stroke-command (stroke)
  `(format "SELECT kanji FROM %s WHERE stroke = %s;"
	   skk-rdbms-stroke-table ,stroke))

(defmacro skk-rdbms-search-sahen-command (dakuten)
  `(format
    "SELECT kanji FROM %s WHERE yomi = '%s' AND hinsiID = (SELECT hinsiID FROM %s WHERE hinsi = '%s');"
    skk-rdbms-hinsi-data-table skk-henkan-key skk-rdbms-hinsi-base-table
    (if dakuten "ザ行(ずる)" "サ行(する)")))

(defvar skk-rdbms-cutoff-output-function
  (function
   (lambda ()
     (save-match-data
       (save-excursion
	 (let (candidates pos)
	   (if (not (re-search-backward "^(\\([0-9]+\\) rows*)$"
					skk-rdbms-last-process-point t))
	       (error "")
	     (setq candidates (match-string 1) pos (point))
	     (if (string= candidates "0")
		 nil
	       (forward-line (- (string-to-number candidates)))
	       (beginning-of-line) ; fail safe
	       (split-string (buffer-substring-no-properties pos (point)) " *\n"))))))))
  "*skk-rdbms-shell によって検索したアウトプットを切り出して加工する関数。
skk-rdbms-public-jisyo-table や skk-rdbms-jis2-jisyo-table などに利用する。
候補の文字列をリストにして返す。
skk-rdbms-working-buffer の中でコールされる。")

(defvar skk-rdbms-cutoff-output-function-2
  (function
   (lambda ()
     (save-match-data
       (save-excursion
 	 (let (candidates var)
  	   (if (not (re-search-backward "^(\\([0-9]+\\) rows*)$"
					skk-rdbms-last-process-point t))
  	       (error "")
 	     (setq candidates (string-to-number (match-string 1)))
 	     (if (= candidates 0)
  		 nil
	       (beginning-of-line)	; fail safe
 	       ;; こうやって一つずつ切り出すんじゃなくて、正規表現かなんか
 	       ;; でパッと処理できませんかね。
 	       (while (> candidates 0)
		 (forward-line -1)
		 (setq var (cons (buffer-substring-no-properties
				  (point)
				  ;; yomi は途中に空白文字を許さないので、
				  ;; 前から空白文字以外をスキップしても OK。
				  (progn (skip-chars-forward "^ ") (point)))
				 var))
		 (beginning-of-line)
		 (setq candidates (1- candidates)))
	       var)))))))
  "*skk-rdbms-shell によって検索したアウトプットを切り出して加工する関数。
切り出す対象の出力は、複数カラムの最初のカラムで、切り出す文字列は途中に空白文字
を許さない、という条件下で使用する。
候補の文字列をリストにして返す。skk-rdbms-working-buffer の中でコールされる。")

(defvar skk-rdbms-cutoff-output-function-3
  (function
   (lambda ()
     (save-match-data
       (save-excursion
	 (let (candidates)
	   (if (not (re-search-backward "^(\\([0-9]+\\) rows*)$"
					skk-rdbms-last-process-point t))
	       (error "")
	     (setq candidates (match-string 1))
	     (if (not (string= candidates "1"))
		 (skk-error "辞書の候補数を数えるのに失敗しました"
			    "Failed counting jisyo candidates")
	       (forward-line -1)
	       (skip-chars-forward " ")
	       (string-to-number
		(buffer-substring-no-properties (point) (progn (end-of-line) (point)))))))))))
  "*skk-rdbms-shell によって検索したアウトプットを切り出して加工し、辞書候補数を返す関数。
skk-rdbms-working-buffer の中でコールされる。")

(defvar skk-rdbms-save-jisyo-function
  (function
   (lambda (quiet)
     (if (not (skk-rdbms-process-alive))
	 (if (not quiet)
	     (progn
	       (skk-message "SKK データベースを掃除/ダンプする必要はありません"
			    "No need to vacuum/dump SKK database")
	       (sit-for 1)))
       (let ((wbuf (get-buffer-create " *SKK private jisyo dump*"))
	     v)
	 (unwind-protect
	     (progn
	       (if (not quiet)
		   (skk-message "SKK データベースを掃除しています..."
				"Vacuuming SKK database..."))
	       (if (eq this-command 'save-buffers-kill-emacs)
		   (skk-record-jisyo-data))
	       ;; vacuum が終わらない内に skk-rdbms-kill がコールされてしまうとど
	       ;; うなるんだろうか？
	       (skk-rdbms-run-SQL-command
		(format "VACUUM %s;" skk-rdbms-private-jisyo-table))
	       (if (not quiet)
		   (progn
		     (skk-message "SKK データベースを掃除しています...完了！"
				  "Vacuuming SKK database...done")
		     (sit-for 1)))
	       (if (and (file-exists-p skk-rdbms-private-jisyo-dump)
			(> 86399
			   (skk-time-difference
			    (nth 5 (file-attributes skk-rdbms-private-jisyo-dump))
			    (current-time))))
		   nil
		 ;; 前回ダンプしたのが 1 日以上前だったら、ダンプする。
		 (if (not quiet)
		     (skk-message "SKK データベースをダンプしています..."
				  "Dumping out SKK database..."))
		 (setq v (= (call-process
			     shell-file-name
			     nil (list wbuf skk-rdbms-dump-error) t
			     "-c" (format "pg_dump skk -t %s" skk-rdbms-private-jisyo-table))
			    0))
		 (cond ((and (not quiet) v)
			(save-excursion
			  (set-buffer wbuf)
			  (write-region (point-min) (point-max)
					skk-rdbms-private-jisyo-dump nil 'nomsg))
			(skk-message "SKK データベースをダンプしています...完了！"
				     "Dumping out SKK database...done")
			(sit-for 1))
		       ((not v)
			(skk-error "SKK データベースをダンプすることに失敗しました"
				   "Failed to dump out SKK database")))))
	   (kill-buffer wbuf))))))
  "*データベースプロセスの終了時に個人辞書に関する処理を行なう関数。
ディフォルトは PostgreSQL 専用コマンドとなっている。")

(defvar skk-rdbms-load-hook nil
  "*skk-rdbms.el をロードしたときのフック。")

;; for test use.
(setq skk-search-prog-list
      '(;;(skk-rdbms-search-kakutei-jisyo-table)
	(skk-rdbms-search-jisyo-table skk-rdbms-private-jisyo-table)
	(skk-rdbms-sahen-search)
	(skk-rdbms-search-jisyo-table skk-rdbms-public-jisyo-table)
	;;(skk-okuri-search)
	(skk-rdbms-search-jisyo-table skk-rdbms-jis2-jisyo-table)))

(setq skk-server-host nil
      skk-servers-list nil)

;; System constants and variables.
(defvar skk-rdbms-last-process-point nil)
(defvar skk-rdbms-process nil)
(defvar skk-rdbms-working-buffer nil)
(defvar skk-rdbms-no-wait nil)
(defvar skk-rdbms-completion-index 1)
(defvar skk-rdbms-completion-list nil)
(defvar skk-rdbms-no-update-command nil)
;;(defvar skk-rdbms-kakutei-word nil)
(setq skk-update-jisyo-function 'skk-rdbms-update-jisyo)
(setq skk-save-jisyo-function 'skk-rdbms-save-jisyo)
(setq skk-count-jisyo-candidates-function 'skk-rdbms-count-jisyo-candidates)
(setq skk-completion-function 'skk-rdbms-completion)
(setq skk-previous-completion-function 'skk-rdbms-previous-completion)
(setq skk-okuri-search-function 'skk-rdbms-okuri-search)
(setq skk-public-jisyo-to-be-searched-function 'skk-rdbms-public-jisyo-to-be-searched)

(defun skk-rdbms-init ()
  (or (skk-rdbms-process-alive)
      (let ((process-connection-type t)
	    (cbuf (current-buffer))
	    origin)
	(if skk-rdbms-process
	    (progn
	      (delete-process skk-rdbms-process)
	      (setq skk-rdbms-process nil)))
	(or (and skk-rdbms-working-buffer
		 (buffer-name skk-rdbms-working-buffer))
	    (setq skk-rdbms-working-buffer (get-buffer-create " *SKK rdbms*")))
	;; 動いたポイントを保存するため save-excursion は使わない。
	(unwind-protect
	    (progn
	      (set-buffer skk-rdbms-working-buffer)
              (buffer-disable-undo)
	      (setenv "PAGER" nil)
	      (insert "\nStarting SKK rdbms...\n\n")
	      (skk-message "SKK データベースを起動しています..."
			   "Starting SKK rdbms...")
	      (setq origin (point))
	      (condition-case nil
		  (progn
		    (setq skk-rdbms-process
			  (start-process "SKK rdbms"
					 skk-rdbms-working-buffer
					 skk-rdbms-shell
					 skk-rdbms-shell-args))
		    (process-kill-without-query skk-rdbms-process)
		    (cond ((featurep 'xemacs)
			   (set-process-input-coding-system
			    skk-rdbms-process
			    skk-rdbms-process-coding-system)
			   (set-process-output-coding-system
			    skk-rdbms-process
			    skk-rdbms-process-coding-system))
			  (t
			   (set-process-coding-system
			    skk-rdbms-process
			    skk-rdbms-process-coding-system
			    skk-rdbms-process-coding-system))))
		(file-error
		 (skk-error "システム上に \"%s\" が見つかりません"
			    "Sorry, can't find \"%s\" on your system"
			    skk-rdbms-shell)))
	      (skk-save-point
	       (while
		   (and
		    (not
		     (progn
		       (goto-char origin)
		       (re-search-forward skk-rdbms-shell-prompt-regexp nil t)))
		    (memq (process-status skk-rdbms-process) '(run stop)))
		 (accept-process-output skk-rdbms-process)))
	      (or (memq (process-status skk-rdbms-process) '(run stop))
		  (skk-error "SKK データベースプロセスをスタートすることができません"
			     "Unable to start SKK database process"))
	      (and skk-rdbms-shell-version-check-function
		   (funcall skk-rdbms-shell-version-check-function))
	      (goto-char (process-mark skk-rdbms-process))
	      (setq skk-rdbms-last-process-point (point)))
	      (skk-message "SKK データベースを起動しています...完了!"
			   "Starting SKK rdbms...done")
	  (set-buffer cbuf)))))

(defun skk-rdbms-process-alive ()
  (and skk-rdbms-process
       skk-rdbms-working-buffer
       (buffer-name skk-rdbms-working-buffer)
       (memq (process-status skk-rdbms-process) '(run stop))))

(defun skk-rdbms-kill ()
  "SKK データベースのプロセスを殺す。"
  (interactive)
  (if (not (skk-rdbms-process-alive))
      ;; 北斗神拳の世界ですな...。
      (skk-message "SKK データベースプロセスは既に死んでます"
		   "SKK database process has already died")
    (or (eq this-command 'save-buffers-kill-emacs)
	(skk-rdbms-save-jisyo))
    (let ((skk-rdbms-no-wait t))
      (skk-rdbms-run-SQL-command skk-rdbms-kill-command)
      (sit-for 1)
      (and (process-status skk-rdbms-process)
	   (delete-process skk-rdbms-process))
      (setq skk-rdbms-process nil)
      (skk-message "SKK データベースプロセスが死にました"
		   "SKK database process died"))))

(defun skk-rdbms-search-jisyo-table (table)
  (and skk-use-numeric-conversion
       (setq skk-henkan-key (skk-compute-numeric-henkan-key skk-henkan-key)))
  (let* ((private-table-p (string= table skk-rdbms-private-jisyo-table))
	 (command
	  (cond (private-table-p (skk-rdbms-SQL-search-private-jisyo-command))
		(t (skk-rdbms-SQL-search-jisyo-command table)))))
    (skk-rdbms-run-SQL-command command skk-rdbms-cutoff-output-function)))

(defun skk-rdbms-search-kakutei-jisyo-table ()
  (prog1
      (setq skk-kakutei-henkan-flag
	    (skk-rdbms-run-SQL-command
	     (skk-rdbms-SQL-search-jisyo-command skk-rdbms-kakutei-jisyo-table)
	     skk-rdbms-cutoff-output-function))
    (and skk-kakutei-henkan-flag
	 (setq skk-rdbms-no-update-command t))))

(defun skk-rdbms-run-SQL-command (command &optional cutoff-func)
  (combine-after-change-calls
    (save-match-data
      (if (and (not (string-match ";$" command))
	       skk-rdbms-shell-special-command-regexp
	       (not (string-match skk-rdbms-shell-special-command-regexp command)))
	  (skk-error "%s のコマンドに文法ミスがあります"
		     "A grammatical mistake of %s command exists"
		     skk-rdbms-shell)
	(setq command (concat command " \n")))
      (let ((cbuf (current-buffer))
	    (okuri-char skk-okuri-char)
	    (henkan-okurigana skk-henkan-okurigana)
	    (henkan-key skk-henkan-key)
	    pmark var)
	(skk-rdbms-init)
	(accept-process-output)
	(setq pmark (process-mark skk-rdbms-process))
	;; 動いたポイントを保存するため save-excursion は使わない。
	(unwind-protect
	    (catch 'exit
	      (set-buffer skk-rdbms-working-buffer)
	      ;; バッファローカル変数を移しておく。
	      (setq skk-okuri-char okuri-char
		    skk-henkan-okurigana henkan-okurigana
		    skk-henkan-key henkan-key)
	      ;; ポイントは保存されているので本来要らないはずだが。
	      (goto-char pmark)
	      (setq skk-rdbms-last-process-point (point))
	      (insert command)
	      (set-marker pmark (point))
	      (process-send-string skk-rdbms-process command)
	      (accept-process-output (and (not skk-rdbms-no-wait)
					  skk-rdbms-process))
	      (goto-char skk-rdbms-last-process-point)
	      ;; こうしないと psql がもたもたしていると search に失敗する場
	      ;; 合がある。
	      (while (not (re-search-forward skk-rdbms-shell-prompt-regexp
					     pmark t))
		;; \q コマンドを送ったらプロンプトは帰ってこない。
		(if (eq (process-status skk-rdbms-process) 'exit)
		    (throw 'exit nil))
		(accept-process-output))
	      (skk-rdbms-check-for-errors)
	      (goto-char pmark)
	      (and cutoff-func (setq var (funcall cutoff-func))))
	  (set-buffer cbuf)
	  var)))))

(defun skk-rdbms-check-for-errors ()
  ;; エラー後のポイント位置を保護するために if 節を save-excursion でつつむ。
  (save-match-data
    (if (save-excursion
	  (set-buffer skk-rdbms-working-buffer)
	  (goto-char skk-rdbms-last-process-point)
	  (re-search-forward skk-rdbms-error-regexp nil t))
	(error "%s" (concat "SQL error: " (match-string 1))))))

(defun skk-rdbms-update-jisyo (word &optional purge)
  (if skk-rdbms-no-update-command
      (setq skk-rdbms-no-update-command nil)
    (and (> skk-okuri-index-min -1) (setq word (skk-remove-common word)))
    (and skk-use-numeric-conversion
	 (setq skk-henkan-key (skk-compute-numeric-henkan-key skk-henkan-key)))
    (let ((command
	   (cond (purge (skk-rdbms-SQL-delete-command word))
		 ;; ミニバッファで登録した場合
		 (skk-henkan-in-minibuff-flag
		  (skk-rdbms-SQL-insert-command word))
		 (t (skk-rdbms-SQL-update-command word))))
	  ignore)
      (skk-rdbms-run-SQL-command command)
      (cond (purge
	     (cond
	      ((skk-public-jisyo-has-entry-p skk-henkan-okurigana word)
	       (skk-rdbms-run-SQL-command
		(skk-rdbms-SQL-regexp-delete-command "^\\\\(skk-ignore-dic-word .*\\\\)"))
	       (setq ignore (car (skk-compose-ignore-entry skk-henkan-list word)))
	       (skk-rdbms-run-SQL-command (skk-rdbms-SQL-insert-command ignore)))))
	    ((and (not skk-henkan-in-minibuff-flag)
		  ;; L 辞書などから得たエントリだから個人辞書の UPDATE に失敗した。
		  (skk-rdbms-update-jisyo-failp))
	     ;; 最初に SELECT でレコードがあるかどうかチェックしてから
	     ;; UPDATE するのが本当だろうけど、そうすると結構遅い。
	     (skk-rdbms-run-SQL-command (skk-rdbms-SQL-insert-command word))))
      (setq skk-henkan-in-minibuff-flag nil))))

(defun skk-rdbms-update-jisyo-failp ()
  (save-match-data
    (save-excursion
      (set-buffer skk-rdbms-working-buffer)
      (goto-char (process-mark skk-rdbms-process))
      (re-search-backward skk-rdbms-update-fail-regexp
			  skk-rdbms-last-process-point t))))

(defun skk-rdbms-save-jisyo (&optional quiet)
  (let ((inhibit-quit t))
    (funcall skk-rdbms-save-jisyo-function quiet)
      (skk-set-cursor-properly)))

(defun skk-rdbms-restore-private-jisyo (force)
  "ダンプファイルから個人辞書データベースを復元する。
C-u M-x skk-rdbms-restore-private-jisyo すると確認なしに復元する。"
  (interactive "P")
  (if (and
       (not force)
       (not (skk-yes-or-no-p
	     "ダンプファイルから個人辞書データベースを復元します。よろしいですか？ "
	     "Restore private jisyo database from dump file? ")))
      (progn
	(ding)
	(skk-message "ダンプファイルからの個人辞書データベースの復元を中止しました"
		     "Stop restoring private jisyo database from dump file"))
    (skk-message "ダンプファイルから個人辞書データベースを復元しています..."
		 "Restoring private jisyo database from dump file...")
    (condition-case nil
	(skk-rdbms-run-SQL-command
	 (format "DROP TABLE %s;" skk-rdbms-private-jisyo-table))
      ;; error will occur if the table does not exist.
      (error nil))
    (if (= (call-process
	    shell-file-name
	    nil (list nil skk-rdbms-dump-error) t
	    "-c" (format "%s %s < %s" skk-rdbms-shell ; PostgreSQL only command.
			 skk-rdbms-shell-args
			 (expand-file-name skk-rdbms-private-jisyo-dump)))
	   0)
	(skk-message "ダンプファイルから個人辞書データベースを復元しています...完了!"
		     "Restoring private jisyo database from dump file...done!")
      (skk-error "ダンプファイルからの個人辞書データベースの復元に失敗しました"
		 "Failed to restore private jisyo database from dump file"))))

(defun skk-rdbms-count-jisyo-candidates (table)
  (if (interactive-p)
      (message "Counting jisyo candidates..."))
  (skk-rdbms-run-SQL-command (format "SELECT COUNT(*) FROM %s;" table)
			     skk-rdbms-cutoff-output-function-3))

(defun skk-rdbms-public-jisyo-to-be-searched ()
  (list 'skk-rdbms-search-jisyo-table 'skk-rdbms-public-jisyo-table))

(defun skk-rdbms-completion (first)
  (require 'skk-comp)
  (let (c-word)
    (skk-kana-cleanup 'force)
    (if (or first skk-dabbrev-like-completion)
	(setq skk-completion-word
	      (buffer-substring-no-properties skk-henkan-start-point (point))
	      ;; 0th には補完しようとする元の語を入れるので、スキップ。
	      skk-rdbms-completion-index 1))
    (if (string= skk-completion-word "")
        (skk-error "空文字から補完することはできません！"
                   "Cannot complete an empty string!"))
    (if (or first skk-dabbrev-like-completion)
	(setq skk-rdbms-completion-list
	      (skk-rdbms-run-SQL-command
	       (skk-rdbms-SQL-search-completion-word-command)
	       skk-rdbms-cutoff-output-function)))
    (setq skk-rdbms-completion-list
	  (cons skk-completion-word
		(delete skk-completion-word skk-rdbms-completion-list))
	  c-word (nth skk-rdbms-completion-index skk-rdbms-completion-list))
    (if (null c-word)
        (if skk-japanese-message-and-error
            (error "\"%s\" で補完すべき見出し語は%sありません"
                   skk-completion-word (if first "" "他に"))
          (error "No %scompletions for \"%s\""
                 (if first "" "more ") skk-completion-word))
      (setq skk-rdbms-completion-index (1+ skk-rdbms-completion-index))
      (delete-region skk-henkan-start-point (point))
      (insert c-word))))

(defun skk-rdbms-previous-completion ()
  (require 'skk-comp)
  (let ((inhibit-quit t)
        c-word)
    (catch 'exit
      (while (not c-word)
	(setq skk-rdbms-completion-index (1- skk-rdbms-completion-index))
	;; (nth -1 '(A B C D)) は A を返す...。
	(if (> 0 skk-rdbms-completion-index)
	    (throw 'exit nil)
	  (setq c-word (nth skk-rdbms-completion-index
			    skk-rdbms-completion-list)))
	(if (string= c-word (buffer-substring-no-properties skk-henkan-start-point (point)))
	    ;; ポップした語がバッファのポイント直前にある文字列と同じ
	    ;; だったら 1 つ捨てる。
	    (setq c-word nil))))
    ;; エラーを出す前にこのコマンド名を修整しておく。
    (setq this-command 'skk-completion)
    (if (not c-word)
	(skk-error "\"%s\"で補完すべき見出し語は他にありません"
		   "No more previous completions for \"%s\""
		   skk-completion-word)
      (delete-region skk-henkan-start-point (point))
      (insert c-word))))

(defun skk-rdbms-okuri-search ()
  (require 'skk-auto)
  (let* ((inhibit-quit t)
	 ;; "たちあげる" -> (53825 53794 53810 53867) == (?ち ?あ ?げ ?る)
	 (okurigana-list (cdr (append skk-henkan-key nil)))
	 (henkan-key-length (length skk-henkan-key))
	 (length skk-kanji-len)
	 (original-henkan-key skk-henkan-key)
	 var full-okurigana kanji)
    (catch 'exit
      (while (> henkan-key-length length)
	(let ((skk-henkan-okurigana (char-to-string (car okurigana-list))) ; "ち"
	      skk-okuri-char skk-henkan-key next-okurigana)
	  (if (string= skk-henkan-okurigana "っ")
	      (progn
		;; 促音「っ」だったら次のかなを含めて skk-henkan-okurigana に入
		;; れる必要あり。
		(setq next-okurigana (nth 1 okurigana-list))
		(if (not next-okurigana)
		    (throw 'exit nil)
		  (setq skk-henkan-okurigana
			(concat skk-henkan-okurigana
				(char-to-string next-okurigana))))))
	  (setq skk-okuri-char (skk-auto-okurigana-prefix
				(char-to-string (car okurigana-list))) ; "t"
		skk-henkan-key (concat (substring original-henkan-key 0 length) ; "たt"
				       skk-okuri-char))
	  (if (not skk-okuri-char)
	      nil
	    ;; ("経" "建" "断" "立")
	    (setq kanji (skk-rdbms-search-jisyo-table skk-rdbms-private-jisyo-table))
	    (if (null kanji)
		nil
	      (setq full-okurigana (concat okurigana-list) ; "ちあげる"
		    var (nconc
			 ;; ("経ちあげる" "建ちあげる" "断ちあげる" "立ちあげる")
			 (mapcar
			  (function (lambda (x) (concat x full-okurigana)))
			  kanji) var))))
	  (if next-okurigana
	      (setq okurigana-list (nthcdr 2 okurigana-list)
		    length (+ length (* 2 skk-kanji-len)))
	    ;; 送り仮名を短くして、
	    (setq okurigana-list (cdr okurigana-list)
		  ;; skk-henkan-key を長くする。
		  length (+ length skk-kanji-len))))))
    ;; ("立ち上げる" "経ちあげる" "建ちあげる" "断ちあげる" "立ちあげる")
    var))

(defun skk-rdbms-sahen-search ()
  (save-match-data
    (if (not
	 (or (and skk-okuri-char (memq (string-to-char skk-okuri-char) '(?s ?z)))
	     (and (not skk-okuri-char) (string-match "[すず]る$" skk-henkan-key))))
	nil
      (let (l)
	(setq skk-okuri-index-min (if (= skk-okuri-index-min -1)
				      (length skk-henkan-list)
				    (min (length skk-henkan-list) skk-okuri-index-min))
	      l (skk-rdbms-sahen-search-1)
	      skk-okuri-index-max (max skk-okuri-index-min
				       (+ skk-okuri-index-min (length l))))
	l))))

(defun skk-rdbms-sahen-search-1 ()
  (let* ((okurigana
	  (and (not skk-okuri-char)
	       (string-match "[すず]る$" skk-henkan-key)
	       (substring skk-henkan-key (match-beginning 0))))
	 (dakuten
	  (cond
	   ((and
	     skk-okuri-char
	     (eq ?z (car (memq (string-to-char skk-okuri-char) '(?s ?z))))))
	   ((and okurigana
		 (string= "ず" (char-to-string (aref okurigana 0)))))))
	 (skk-henkan-key
	  (cond
	   (skk-okuri-char
	    (substring skk-henkan-key 0 (1- (length skk-henkan-key))))
	   (t (substring skk-henkan-key 0 (match-beginning 0)))))
	 v)
    (setq v (skk-rdbms-run-SQL-command
	     (skk-rdbms-search-sahen-command dakuten)
	     skk-rdbms-cutoff-output-function))
    (if (not okurigana)
	v
      (mapcar (function (lambda (x) (concat x okurigana))) v))))

(defun skk-rdbms-busyu-henkan (&optional busyu)
  "部首から文字を検索する。"
  (interactive)
  ;; 直接関数引数として busyu を与えられたら文句を言わない。
  (and (not busyu) (not skk-henkan-on)
       (skk-error "▽モードに入っていません" "Not in ▽ mode"))
  (or busyu
      (setq busyu (buffer-substring-no-properties (point) skk-henkan-start-point)))
  (and (string= busyu "")
       (skk-error "検索すべき部首がありません" "No BUSYU to search"))
  (setq skk-henkan-key busyu)
  (let ((skk-search-prog-list
	 '((skk-rdbms-run-SQL-command (skk-rdbms-SQL-search-busyu-command busyu)
				      skk-rdbms-cutoff-output-function))))
    ;; 個人辞書テーブルに取り込まない。
    (setq skk-rdbms-no-update-command t)
    (skk-start-henkan nil)))

(defun skk-rdbms-stroke-henkan (&optional stroke)
  "画数から文字を検索する。"
  (interactive)
  ;; 直接関数引数として stroke を与えられたら文句を言わない。
  (if (and (not stroke) (not skk-henkan-on))
      (skk-error "▽モードに入っていません" "Not in ▽ mode"))
  (save-match-data
    (or stroke
	(setq stroke (buffer-substring-no-properties (point) skk-henkan-start-point)))
    (if (or (string= stroke "") (string-match "[^0-9]" stroke))
	(skk-error "検索すべき画数がありません" "No stroke to search"))
    (setq skk-henkan-key stroke)
    (let ((skk-search-prog-list
	   '((skk-rdbms-run-SQL-command
	      (skk-rdbms-SQL-search-stroke-command stroke)
	      skk-rdbms-cutoff-output-function))))
      ;; 個人辞書テーブルに取り込まない。
      (setq skk-rdbms-no-update-command t)
      (skk-start-henkan nil))))

(defun skk-rdbms-stroke (kanji)
  "漢字の画数を表示する。"
  (interactive (list (read-from-minibuffer
		      (if skk-japanese-message-and-error "漢字: " "Kanji: "))))
  (if (interactive-p)
      (message (skk-rdbms-kanji-to-stroke kanji))
   (let (v)
     (add-hook 'minibuffer-setup-hook 'skk-j-mode-on)
     (add-hook
      'minibuffer-setup-hook
      (function (lambda () (add-local-hook 'pre-command-hook 'skk-pre-command))))
     (setq v (car (skk-rdbms-run-SQL-command
		   (format "SELECT stroke FROM %s WHERE kanji = '%s';"
			   skk-rdbms-stroke-table kanji)
		   skk-rdbms-cutoff-output-function)))
     (if (not (string-match "[0-9]+" v))
	 nil
       (setq v (substring v (match-beginning 0) (match-end 0)))
       (if skk-japanese-message-and-error
	   (concat v " 画")
	 (concat v " stroke" (if (not (string= v "1")) "s")))))))

;; skk-rdbms-save-jisyo の中で VACUUM した後にプロセスを殺す。
(add-hook 'skk-before-kill-emacs-hook 'skk-rdbms-kill 'append)

(run-hooks 'skk-rdbms-load-hook)

(provide 'skk-rdbms)
;;; skk-rdbms.el ends here
