;;; skk-exserv.el --- SKK サーバーのためのプログラム -*- coding: iso-2022-jp -*-
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996,
;;               1997, 1998, 1999, 2000
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;;
;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-exserv.el,v 1.10 2012/01/05 12:06:09 skk-cvs Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2012/01/05 12:06:09 $

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

;; This file needs eieio package (which provides CLOS like OO
;; programming) that can be found at;
;;
;;    ftp://ftp.ultranet.com/pub/zappo

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars)
  (require 'static))
(require 'eieio)
(require 'skk-exsearch)

(defclass network-search-engine (search-engine)
  ((host :initarg :host
	 :initform "localhost"
	 :documentation "Host name.")
   (service :initarg :service
	    :initform nil
	    :documentation
	    "Name of the service desired, or an integer specifying a port number to connect to.")
   (process-name :initarg :process-name
		 :initform nil
		 :documentation
		 "Name of process.  It is modified if necessary to make it unique.")
   (process initarg :process
	    :initform nil
	    :documentation "Process object that belongs to program.")
   (buffer :initarg :buffer
	   :initform nil
	   :documentation
	   "The buffer (or buffer-name) to associate with the process.\
Process output goes at end of that buffer, unless you specify\
an output stream or filter function to handle the output.\
buffer may be also nil, meaning that this process is not associated\
with any buffer."))
  "A class of Network search engine via TCP connection.")

(defclass dbskkd-engine (network-search-engine)
  ((service :initarg :service
	    :initform "skkserv")
   (process-name :initarg :process-name :initform "dbskkd")
   (buffer :initarg :buffer
	   :initform (lambda () (get-buffer-create " *dbskkd*")))
   (found :initform 1
	  :documentation "A magic number that indicates the server found a candidate.")
   (not-found :initform 4
	      :documentation
	      "A magic number that indicates the server did not find a candidates.")
   (coding-system :initarg :coding-system
		  :initform (lambda () (cdr (assoc "euc" skk-coding-system-alist)))))
  "A class of dbskkd type server search engine via TCP connection.
Output of this type is a line that contains a magic number and
candidates that are delimited by slash.")

(defvar dbskkd (make-instance dbskkd-engine)
  "*dbskkd server object.")

(defvar skk-exserv-list (list dbskkd)
  "*Add other SKK server objects if you like.")

(defun skk-open-server ()
  ;; return active server object.
  (while (and (car skk-exserv-list)
	      (not (server-opened-p (car skk-exserv-list)))
	      (not (open-server (car skk-exserv-list)))
	      (setq skk-exserv-list (cdr skk-exserv-list))))
  (car skk-exserv-list))

(defmethod server-opened-p ((engine network-search-engine))
  (with-slots (process) engine
    (and process (eq (process-status process) 'open))))

(defmethod open-server ((engine network-search-engine))
  ;; Return t if process is opened.
  (with-slots (process coding-system) engine
    (condition-case nil
	(progn
	  (setq process
		(open-network-stream
		 (oref engine process-name) (oref engine buffer)
		 (oref engine host) (oref engine service)))
	  (if (not process)
	      nil
	    (process-kill-without-query process)
	    (static-cond
	     ((featurep 'xemacs)
	      (set-process-input-coding-system process coding-system)
	      (set-process-output-coding-system process coding-system))
	     (t
	      (set-process-coding-system process coding-system coding-system)))
	    (oset engine process process)
	    (eq (process-status process) 'open)))
      (error nil))))

(defmethod disconnect-server ((engine dbskkd-engine))
  (with-slots (process-name) engine
    (if (eq (process-status process-name) 'open)
	(progn
	  (process-send-string process-name "0") 
	  (accept-process-output (get-process process-name))))))

;;;###autoload
(defun skk-server-version ()
  (interactive)
  (if (interactive-p)
      (message (skk-server-version))
    (server-version (car skk-exserv-list))))

(defmethod server-version ((engine dbskkd-engine))
  (or skk-exserv-list
      (skk-error "Lack of host information of SKK server"
		 "SKK サーバーのホスト情報がありません" ))
  (or (skk-open-server)
      (skk-error "Cannot open connection to SKK server"
		 "SKK サーバーとコネクションを張ることができません"))
  (save-excursion
    (unwind-protect
	(progn
	  (set-buffer (oref engine buffer))
	  (let (v)
	    (erase-buffer)
	    ;; サーバーバージョンを得る。
	    (process-send-string (oref engine process-name) "2")
	    (while (and (server-opened-p engine) (eq (buffer-size) 0))
	      (accept-process-output))
	    (setq v (buffer-string))
	    (erase-buffer)
	    ;; ホスト名を得る。
	    (process-send-string (oref engine process-name) "3")
	    (while (and (server-opened-p engine) (eq (buffer-size) 0))
	      (accept-process-output))
	    (goto-char (point-min))
	    (format
	     (concat "SKK SERVER version %s"
		     (if skk-japanese-message-and-error
			 "(ホスト名 %s)"
		       "running on HOST %s"))
	     v (buffer-string) )))
      (erase-buffer))))

;;;###autoload
(defun skk-exserv-search ()
  (let ((server (skk-open-server)))
    (and server (search-server server))))

(defmethod search-server ((engine dbskkd-engine))
  (let ((key
	 (if skk-use-numeric-conversion
	     (skk-num-compute-henkan-key skk-henkan-key)
	   skk-henkan-key))
        (okurigana (or skk-henkan-okurigana skk-okuri-char)))
    (with-current-buffer (oref engine buffer)
      (let (l)
	(erase-buffer)
	(process-send-string (oref engine process-name) (concat "1" key " "))
	(while (and (server-opened-p engine) (eq (buffer-size) 0))
	  (accept-process-output))
	;; found key successfully, so check if a whole line is received.
	(if (and (eq (char-after 1) ?1) (eq (char-after (1- (point-max))) ?\n))
	    (progn
	      (goto-char (point-min))
	      (forward-char 2)
	      (and (setq l (skk-compute-henkan-lists okurigana))
		   (cond ((and okurigana skk-henkan-okuri-strictly)
			  ;; 送り仮名が同一のエントリのみを返す。
			  (nth 2 l))
			 ((and okurigana skk-henkan-strict-okuri-precedence)
			  (skk-nunion (nth 2 l) (car l)))
			 (t (car l))))))))))

;;;###autoload
(defun skk-adjust-search-prog-list-for-server-search (&optional non-del)
  ;; skk-exserv-list が nil であれば、
  ;; skk-search-prog-list から skk-exserv-search を car に持つリストを消す。
  ;; non-nil であれば、加える。
  (if skk-exserv-list
      (if (null (assq 'skk-exserv-search skk-search-prog-list))
          ;; skk-search-prog-list が nil ということはまずないだろうが、念のた
          ;; め、setq しておく。
          (setq skk-search-prog-list
                ;; 末尾に付ける。末尾には (skk-okuri-search) を持ってきたい人
                ;; もいるかも。オプションで付ける場所を変更するようにした方が
                ;; 良い？
                (nconc skk-search-prog-list (list '(skk-exserv-search)))))
    (if (not non-del)
	(remove-alist 'skk-search-prog-list 'skk-exserv-search))))

;;(add-hook 'skk-mode-hook 'skk-adjust-search-prog-list-for-server-search)
(add-hook 'kill-emacs-hook
	  (function (lambda () (disconnect-server (car skk-exserv-list)))))

(run-hooks 'skk-exserv-load-hook)

(provide 'skk-exserv)
;;; Local Variables:
;;; eval: (require 'eieio)
;;; End:
;;; skk-exserv.el ends here
