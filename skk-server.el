;;; skk-server.el --- SKK サーバーのためのプログラム
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996,
;;               1997, 1998, 1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-server.el,v 1.4 2000/03/21 21:55:28 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/03/21 21:55:28 $

;; This file is part of SKK.

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
;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-foreword)

;;;###autoload
(defgroup skk-server nil "SKK server related customization."
  :prefix "skk-server-"
  :group 'skk )

;; user variables.
(defcustom skk-server-host (getenv "SKKSERVER")
  "*SKK 辞書サーバーを走らせているホスト名。"
  :type 'string
  :group 'skk-server )

(defcustom skk-server-prog (getenv "SKKSERV")
  "*SKK 辞書サーバープログラム名。フルパスで書く。"
  :type 'file
  :group 'skk-server )

(defcustom skk-server-jisyo (getenv "SKK_JISYO")
  "*SKK 辞書サーバープログラムに渡す辞書名。フルパスで書く。"
  :type 'file
  :group 'skk-server )

(defcustom skk-server-portnum nil
  "*Non-nil であれば、その値を port number として skkserv と TCP 接続する。
/etc/services を直接書き換える権限がないユーザーのための変数。"
  :type '(choice integer (const nil))
  :group 'skk-server )

;;(defvar skk-server-debug nil
;;  "*Non-nil であれば、辞書サーバープログラムをディバッグモードで起動する。
;;ディバッグ・モードで skkserv を走らせると、そのまま foreground で走り、メッセー
;;ジを出力する。キーボードから割りこみをかけることもできる。" )

(defcustom skk-servers-list nil
  "*辞書サーバー毎の情報リスト。

複数のホストで動いているサーバにアクセスできる場合には、以下のようにリストの
各要素に順にホスト名、フルパスでの SKK サーバー名、SKK サーバーに渡す辞書名、
SKK サーバーが使用するポート番号を書き、設定をすることができる。

   \(setq skk-servers-list
         '\(\(\"host1\" \"/path/to/skkserv\" \"/path/to/SKK-JISYO.L\" 1178\)
           \(\"host2\" \"/path/to/skkserv\"\) \)\)

この場合、最初に指定したサーバにアクセスできなくなると、自動的に順次リストにあ
る残りのサーバにアクセスするようになる。
サーバーのディフォルトの辞書およびポート番号を使用する場合は nil を指定するか、
何も書かないで良い。

なお、ユーザー自身に実行権限のないサーバーを指定する場合は、

   \(setq skk-servers-list '\(\(\"host1\"\) \(\"host2\"\)\)\)

のように、ホスト名だけを書くことができる。上記の設定例では、host1, host2 にお
ける skkserv サービスの TCP 接続の開始のみ試み、サーバーの起動は試みない。"
  :type '(repeat
	  (list (string :tag "Hostname")
		(choice :tag "Server" file (const nil))
		(choice :tag "Dictionary" file (const nil))
		(choice :tag "Port number" integer (const nil)) ))
  :group 'skk-server )

(defcustom skk-server-report-response nil
  "*Non-nil であれば、変換時サーバーの送出する文字を受け取るまでに accept-process-output を何回実行したかを報告する。"
  :type 'boolean
  :group 'skk-server )

(defcustom skk-server-remote-shell-program
  (or (getenv "REMOTESHELL")
      (and (boundp 'remote-shell-program) remote-shell-program)
      (cond
       ((eq system-type 'berkeley-unix)
        (if (file-exists-p "/usr/ucb/rsh") "/usr/ucb/rsh" "/usr/bin/rsh") )
       ((eq system-type 'usg-unix-v)
        (if (file-exists-p "/usr/ucb/remsh") "/usr/ucb/remsh" "/bin/rsh"))
       ((eq system-type 'hpux) "/usr/bin/remsh")
       ((eq system-type 'EWS-UX/V) "/usr/ucb/remsh")
       ((eq system-type 'pcux) "/usr/bin/rcmd")
       (t "rsh") ))
  "*リモートシェルのプログラム名。"
  :type 'file
  :group 'skk-server )

(defcustom skk-server-load-hook nil
  "*skk-server.el をロードした後にコールされるフック。"
  :type 'hook
  :group 'skk-server )

;; internal constants and variables.
(defconst skk-network-open-status 'open)
(defconst skkserv-working-buffer " *skkserv*")
(defvar skkserv-process nil)

(defun skk-server-version ()
  (interactive)
  (if (interactive-p)
      (message (skk-server-version))
    (let (status)
      (if (not (or skk-server-host skk-servers-list))
          (skk-error "Lack of host information of SKK server"
                     "SKK サーバーのホスト情報がありません" ))
      (setq status (process-status "skkservd"))
      (or (eq status skk-network-open-status) (setq status (skk-open-server)))
      (if (eq status skk-network-open-status)
          (let (v)
            (save-match-data
              (with-current-buffer skkserv-working-buffer
                (erase-buffer)
                ;; サーバーバージョンを得る。
                (process-send-string "skkservd" "2")
                (while (eq (buffer-size) 0)
                  (accept-process-output) )
                (setq v (buffer-string))
                (erase-buffer)
                ;; ホスト名を得る。
                (process-send-string "skkservd" "3")
                (while (eq (buffer-size) 0)
                  (accept-process-output) )
                (goto-char (point-min))
                (format
                 (concat "SKK SERVER version %s"
                         (if skk-japanese-message-and-error
                             "(ホスト名 %s)"
                           "running on HOST %s" ))
                 v (prog1 (buffer-string) (erase-buffer)) ))))))))

(defun skk-search-server (file limit &optional nomsg)
  ;; SKK 辞書フォーマットの FILE で SKK サーバーを使用して skk-henkan-key をキー
  ;; にして検索を行う。
  ;; SKK サーバーが使用できないときは、FILE をバッファに読み込んでサーチを行
  ;; う。
  ;; LIMIT と NOMSG は SKK サーバーを使用しないときのみ使う。
  ;; 検索リージョンが LIMIT 以下になるまでバイナリサーチを行い、その後リニア
  ;; サーチを行う。
  ;; LIMIT が 0 であれば、リニアサーチのみを行う。
  ;; 辞書がソートされていないのであれば、LIMIT を 0 する必要がある。
  ;; オプショナル引数の NOMSG が non-nil であれば skk-get-jisyo-buffer のメッ
  ;; セージを出力しないようにする。
  (if (or skk-server-host skk-servers-list)
      (skk-search-server-subr file limit)
    (skk-search-jisyo-file file limit nomsg) ))

(defun skk-search-server-subr (file limit)
  ;; skk-search-server のサブルーチン。
  (let ((key
	 (if skk-use-numeric-conversion
	     (skk-num-compute-henkan-key skk-henkan-key)
	   skk-henkan-key))
        ;; バッファローカル値の受け渡しのため、別名の一時変数に取る。
        (okurigana (or skk-henkan-okurigana skk-okuri-char))
        (status (process-status "skkservd")) )
    (or (eq status skk-network-open-status) (setq status (skk-open-server)))
    (if (eq status skk-network-open-status)
        (with-current-buffer skkserv-working-buffer
          (let ((cont t) (count 0)
                l )
            (erase-buffer)
            (process-send-string "skkservd" (concat "1" key " "))
            (while (and cont (eq (process-status "skkservd")
                                 skk-network-open-status ))
              (accept-process-output)
              (setq count (1+ count))
              (if (> (buffer-size) 0)
                  (if (eq (char-after 1) ?1) ;?1
                      ;; found key successfully, so check if a whole line
                      ;; is received.
                      (if (eq (char-after (1- (point-max))) ?\n) ;?\n
                          (setq cont nil) )
                    ;; not found or error, so exit
                    (setq cont nil) )))
            (goto-char (point-min))
            (if skk-server-report-response
                (skk-message "%d 回 SKK サーバーの応答待ちをしました"
                             "Waited for server response %d times" count ))
            (if (eq (following-char) ?1) ;?1
                (progn
                  (forward-char 2)
                  (setq l (skk-compute-henkan-lists okurigana))
                  (if l
                      (cond ((and okurigana skk-henkan-okuri-strictly)
			     ;; 送り仮名が同一のエントリのみを返す。
			     (nth 2 l) )
			    ((and okurigana skk-henkan-strict-okuri-precedence)
			     (skk-nunion (nth 2 l) (car l)) )
			    (t (car l)) ))))))
      ;; server is not active, so search file instead
      (skk-search-jisyo-file file limit) )))

(defun skk-open-server ()
  ;; SKK サーバーと接続する。サーバープロセスの status を返す。
  (let (status code proc)
    (if (or (skk-open-network-stream) (skk-open-server-1))
        (progn
          (setq status (process-status "skkservd"))
          (if (eq status skk-network-open-status)
              (progn
                (setq code (cdr (assoc "euc" skk-coding-system-alist))
		      proc (get-process "skkservd") )
		(cond ((eq skk-emacs-type 'xemacs)
		       (set-process-input-coding-system proc code)
		       (set-process-output-coding-system proc code) )
		      (t
		       (set-process-coding-system proc code code) ))))))
    status ))

(defun skk-open-server-1 ()
  ;; skk-open-server のサブルーチン。
  ;; skkserv サービスをオープンできたら t を返す。
  ;; skkserv は引数に辞書が指定されていなければ、DEFAULT_JISYO を参照する。
  (if (null skk-servers-list)
      (progn
	;; Emacs 起動後に環境変数を設定した場合。
	(if (not skk-server-host)
	    (setq skk-server-host (getenv "SKKSERVER")) )
	(if (not skk-server-prog)
	    (setq skk-server-prog (getenv "SKKSERV")) )
	(if (not skk-server-jisyo)
	    (setq skk-server-jisyo (getenv "SKK_JISYO")) )
	(if skk-server-host
	    (setq skk-servers-list (list (list skk-server-host
					       skk-server-prog
					       skk-server-jisyo
					       skk-server-portnum )))
	  (setq skk-server-prog nil) )))
  (while (and (not (eq (process-status "skkservd") skk-network-open-status))
	      skk-servers-list )
    (let ((elt (car skk-servers-list))
	  arg )
      (setq skk-server-host (car elt)
	    skk-server-prog (nth 1 elt)
	    skk-server-jisyo (nth 2 elt)
	    skk-server-portnum (nth 3 elt)
	    skk-servers-list (cdr skk-servers-list) )
      ;; skkserv の起動オプションは下記の通り。
      ;;     skkserv [-d] [-p NNNN] [JISHO]
      ;;     `-d'     ディバッグ・モード
      ;;     `-p NNNN'     通信用のポート番号としてNNNNを使う.
      ;;     `~/JISYO'     ~/JISYOを辞書として利用.
      (if skk-server-jisyo
	  (setq arg (list skk-server-jisyo))
	;; skkserv は引数に辞書が指定されていなければ、DEFAULT_JISYO を
	;; 参照する。
	)
      ;;(if skk-server-debug
      ;;    (setq arg (cons "-d" arg)) )
      (if (and skk-server-portnum (not (= skk-server-portnum 1178)))
	  (setq arg
		(nconc (list "-p" (number-to-string skk-server-portnum)) arg) ))
      (if (and skk-server-host (not (skk-open-network-stream))
	       skk-server-prog )
	  ;; skk-startup-server でサーバーを起動するには、skk-server-host と
	  ;; skk-server-prog が設定されていることが必要。
	  (skk-startup-server arg) )))
  (if (not (eq (process-status "skkservd") skk-network-open-status))
      ;; reset SKK-SERVER-HOST so as not to use server in this session
      (setq skk-server-host nil
	    skk-server-prog nil
	    skk-servers-list nil )
    t ))

(defun skk-open-network-stream ()
  ;; skk-server-host における skkserv サービスの TCP 接続をオープンし、プロセ
  ;; スを返す。
  (condition-case nil
      (progn
	(setq skkserv-process
	      (open-network-stream "skkservd" skkserv-working-buffer
				   skk-server-host
				   (or skk-server-portnum "skkserv") ))
	(process-kill-without-query skkserv-process) )
    (error nil) ))

(defun skk-startup-server (arg)
  ;; skkserv を起動できたら t を返す。
  (let (
        ;;(msgbuff (get-buffer-create " *skkserv-msg*"))
        (count 7) )
    (while (> count 0)
      (skk-message
       "%s の SKK サーバーが起動していません。起動します%s"
       "SKK SERVER on %s is not active, I will activate it%s"
       skk-server-host (make-string count ?.) )
      (if (or (string= skk-server-host (system-name))
              (string= skk-server-host "localhost"))
          ;; server host is local machine
          (apply 'call-process skk-server-prog nil
                 ;;msgbuff
                 0 nil arg)
        (apply 'call-process
               skk-server-remote-shell-program nil
               ;; 0 にしてサブプロセスの終了を待ってはいけない理由がある？
               ;; なければ msgbuf にエラー出力を取った方が建設的では？  またそ
               ;; の場合はこの while ループ自身がいらない？
               ;; msgbuff
               0 nil skk-server-host skk-server-prog arg ))
      (sit-for 3)
      (if (and (skk-open-network-stream)
               (eq (process-status "skkservd") skk-network-open-status) )
          (setq count 0)
        (setq count (1- count)) ))
    (if (eq (process-status "skkservd") skk-network-open-status)
        (progn
          (skk-message "ホスト %s の SKK サーバーが起動しました"
                       "SKK SERVER on %s is active now"
                       skk-server-host )
          (sit-for 1) ; return t
          t ) ; でも念のため
      (skk-message "%s の SKK サーバーを起動することができませんでした"
                   "Could not activate SKK SERVER on %s"
                   skk-server-host )
      (sit-for 1)
      (ding) ;return nil
      nil ))) ; でも念のため

;;;###autoload
(defun skk-adjust-search-prog-list-for-server-search (&optional non-del)
  ;; skk-server-host もしくは skk-servers-list が nil であれば、
  ;; skk-search-prog-list から skk-search-server を car に持つリストを消す。
  ;; non-nil であれば、加える。
  (if (or skk-server-host skk-servers-list)
      (if (null (assq 'skk-search-server skk-search-prog-list))
          ;; skk-search-prog-list が nil ということはまずないだろうが、念のた
          ;; め、setq しておく。
          (setq skk-search-prog-list
                ;; 末尾に付ける。末尾には (skk-okuri-search) を持ってきたい人
                ;; もいるかも。オプションで付ける場所を変更するようにした方が
                ;; 良い？
                (nconc skk-search-prog-list
                       (list
                        '(skk-search-server skk-aux-large-jisyo 10000) ))))
    (if (not non-del)
	(remove-alist 'skk-search-prog-list 'skk-search-server) )))

(defun skk-disconnect-server ()
  ;; サーバーを切り離す。
  (if (and skk-server-host
           (eq (process-status "skkservd") skk-network-open-status) )
      (progn
        (process-send-string "skkservd" "0") ; disconnect server
        (accept-process-output (get-process "skkservd")) )))

;;(add-hook 'skk-mode-hook 'skk-adjust-search-prog-list-for-server-search)
(add-hook 'skk-before-kill-emacs-hook 'skk-disconnect-server)

(run-hooks 'skk-server-load-hook)

(provide 'skk-server)
;;; skk-server.el ends here
