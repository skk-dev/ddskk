;;; skk-server.el --- SKK サーバーのためのプログラム

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996,
;;               1997, 1998, 1999, 2000
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-server.el,v 1.15 2001/11/17 05:40:16 czkmt Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2001/11/17 05:40:16 $

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
;;
;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars) (require 'static))

;;;###autoload
(defun skk-server-version ()
  (interactive)
  (cond
   ((interactive-p)
    (message "%s" (skk-server-version)))
   (t
    (let (status)
      (unless (or skk-server-host
		  skk-servers-list)
	(skk-error "Lack of host information of SKK server"
		   "SKK サーバーのホスト情報がありません"))
      (setq status (process-status "skkservd"))
      (unless (eq status skk-network-open-status)
	(setq status (skk-open-server)))
      (when (eq status skk-network-open-status)
	(let (v)
	  (save-match-data
	    (with-current-buffer skkserv-working-buffer
	      (erase-buffer)
	      ;; サーバーバージョンを得る。
	      (process-send-string "skkservd" "2")
	      (while (eq (buffer-size) 0)
		(accept-process-output))
	      (setq v (buffer-string))
	      (erase-buffer)
	      ;; ホスト名を得る。
	      (process-send-string "skkservd" "3")
	      (while (eq (buffer-size) 0)
		(accept-process-output))
	      (goto-char (point-min))
	      (format (concat "SKK SERVER version %s"
			      (if skk-japanese-message-and-error
				  "(ホスト名 %s)"
				"running on HOST %s"))
		      v
		      (prog1
			  (buffer-string)
			(erase-buffer)))))))))))

;;;###autoload
(defun skk-search-server-1 (file limit)
  ;; skk-search-server のサブルーチン。
  (let ((key
	 (if skk-use-numeric-conversion
	     (skk-num-compute-henkan-key skk-henkan-key)
	   skk-henkan-key))
	;; バッファローカル値の受け渡しのため、別名の一時変数に取る。
	(okurigana (or skk-henkan-okurigana
		       skk-okuri-char))
	(status (process-status "skkservd")))
    (unless (eq status skk-network-open-status)
      (setq status (skk-open-server)))
    (cond
     ((eq status skk-network-open-status)
      (with-current-buffer skkserv-working-buffer
	(let ((cont t)
	      (count 0)
	      l)
	  (erase-buffer)
	  (process-send-string "skkservd" (concat "1" key " "))
	  (while (and cont
		      (eq (process-status "skkservd")
			  skk-network-open-status))
	    (accept-process-output)
	    (setq count (1+ count))
	    (when (> (buffer-size) 0)
	      (if (eq (char-after 1) ?1) ;?1
		  ;; found key successfully, so check if a whole line
		  ;; is received.
		  (when (eq (char-after (1- (point-max)))
			    ?\n) ;?\n
		    (setq cont nil))
		;; not found or error, so exit
		(setq cont nil))))
	  (goto-char (point-min))
	  (when skk-server-report-response
	    (skk-message "%d 回 SKK サーバーの応答待ちをしました"
			 "Waited for server response %d times"
			 count))
	  (when (eq (following-char) ?1) ;?1
	    (forward-char 2)
	    (setq l (skk-compute-henkan-lists okurigana))
	    (when l
	      (cond ((and okurigana
			  skk-henkan-okuri-strictly)
		     ;; 送り仮名が同一のエントリのみを返す。
		     (nth 2 l))
		    ((and okurigana
			  skk-henkan-strict-okuri-precedence)
		     (skk-nunion (nth 2 l) (car l)))
		    (t
		     (car l))))))))
     (t
      ;; server is not active, so search file instead
      (skk-search-jisyo-file file limit)))))

(defun skk-open-server ()
  ;; SKK サーバーと接続する。サーバープロセスの status を返す。
  (let (status code proc)
    (when (or (skk-open-network-stream)
	      (skk-open-server-1))
      (setq status (process-status "skkservd"))
      (when (eq status skk-network-open-status)
	(setq code (cdr (assoc "euc" skk-coding-system-alist))
	      proc (get-process "skkservd"))
	(set-process-coding-system proc code code)))
    status))

(defun skk-open-server-1 ()
  ;; skk-open-server のサブルーチン。
  ;; skkserv サービスをオープンできたら t を返す。
  ;; skkserv は引数に辞書が指定されていなければ、DEFAULT_JISYO を参照する。
  (unless skk-servers-list
    ;; Emacs 起動後に環境変数を設定した場合。
    (unless skk-server-host
      (setq skk-server-host (getenv "SKKSERVER")))
    (unless skk-server-prog
      (setq skk-server-prog (getenv "SKKSERV")))
    (unless skk-server-jisyo
      (setq skk-server-jisyo (getenv "SKK_JISYO")))
    (if skk-server-host
	(setq skk-servers-list (list (list skk-server-host
					   skk-server-prog
					   skk-server-jisyo
					   skk-server-portnum)))
      (setq skk-server-prog nil)))
  (while (and (not (eq (process-status "skkservd")
		       skk-network-open-status))
	      skk-servers-list)
    (let ((elt (car skk-servers-list))
	  arg)
      (setq skk-server-host (car elt)
	    skk-server-prog (nth 1 elt)
	    skk-server-jisyo (nth 2 elt)
	    skk-server-portnum (nth 3 elt)
	    skk-servers-list (cdr skk-servers-list))
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
      ;;    (setq arg (cons "-d" arg)))
      (when (and skk-server-portnum
		 (not (= skk-server-portnum 1178)))
	(setq arg
	      (nconc (list "-p" (number-to-string skk-server-portnum))
		     arg)))
      (when (and skk-server-host
		 (not (skk-open-network-stream))
		 skk-server-prog)
	;; skk-startup-server でサーバーを起動するには、skk-server-host と
	;; skk-server-prog が設定されていることが必要。
	(skk-startup-server arg))))
  (if (not (eq (process-status "skkservd")
	       skk-network-open-status))
      ;; reset SKK-SERVER-HOST so as not to use server in this session
      (setq skk-server-host nil
	    skk-server-prog nil
	    skk-servers-list nil)
    t))

(defun skk-open-network-stream ()
  ;; skk-server-host における skkserv サービスの TCP 接続をオープンし、プロセ
  ;; スを返す。
  (ignore-errors
    (setq skkserv-process
	  (open-network-stream "skkservd"
			       skkserv-working-buffer
			       skk-server-host
			       (or skk-server-portnum
				   "skkserv")))
    (process-kill-without-query skkserv-process)))

(defun skk-startup-server (arg)
  ;; skkserv を起動できたら t を返す。
  (let (
	;;(msgbuff (get-buffer-create " *skkserv-msg*"))
	(count 7))
    (while (> count 0)
      (skk-message
       "%s の SKK サーバーが起動していません。起動します%s"
       "SKK SERVER on %s is not active, I will activate it%s"
       skk-server-host (make-string count ?.))
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
	       0 nil skk-server-host skk-server-prog arg))
      (sit-for 3)
      (if (and (skk-open-network-stream)
	       (eq (process-status "skkservd") skk-network-open-status))
	  (setq count 0)
	(setq count (1- count))))
    (if (eq (process-status "skkservd") skk-network-open-status)
	(progn
	  (skk-message "ホスト %s の SKK サーバーが起動しました"
		       "SKK SERVER on %s is active now"
		       skk-server-host)
	  (sit-for 1) ; return t
	  t) ; でも念のため
      (skk-message "%s の SKK サーバーを起動することができませんでした"
		   "Could not activate SKK SERVER on %s"
		   skk-server-host)
      (sit-for 1)
      (ding) ;return nil
      nil))) ; でも念のため

;;;###autoload
(defun skk-adjust-search-prog-list-for-server-search (&optional non-del)
  ;; skk-server-host もしくは skk-servers-list が nil であれば、
  ;; skk-search-prog-list から skk-search-server を car に持つリストを消す。
  ;; non-nil であれば、加える。
   (when (and (or skk-server-host
		  skk-servers-list)
	      (not (assq 'skk-search-server
			 (default-value 'skk-search-prog-list))))
     ;; skk-search-prog-list が nil ということはまずないだろうが、念のた
     ;; め、setq しておく。
     (setq-default
      skk-search-prog-list
      ;; 末尾に付ける。末尾には (skk-okuri-search) を持ってきたい人
      ;; もいるかも。オプションで付ける場所を変更するようにした方が
      ;; 良い？
      (nconc (default-value 'skk-search-prog-list)
	     (list
	      '(skk-search-server skk-aux-large-jisyo 10000))))))

(defun skk-disconnect-server ()
  ;; サーバーを切り離す。
  (when (and skk-server-host
	     (eq (process-status "skkservd")
		 skk-network-open-status))
    (process-send-string "skkservd" "0") ; disconnect server
    (accept-process-output (get-process "skkservd"))))

;;(add-hook 'skk-mode-hook 'skk-adjust-search-prog-list-for-server-search)
(add-hook 'skk-before-kill-emacs-hook 'skk-disconnect-server)

(run-hooks 'skk-server-load-hook)

(require 'product)
(product-provide
    (provide 'skk-server)
  (require 'skk-version))

;;; skk-server.el ends here
