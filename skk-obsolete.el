;;; skk-obsolete.el --- obsolte check for SKK environment.
;; Copyright (C) 1998, 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-obsolete.el,v 1.2 1999/11/10 12:09:03 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/11/10 12:09:03 $

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
;;; Code:
(eval-when-compile (require 'skk))

(defvar skk-obsolete-variable-alist
  '((skk-ascii-mode . skk-latin-mode)
    (skk-ascii-mode-map . skk-latin-mode-map)
    (skk-ascii-mode-string . skk-latin-mode-string)
    (skk-default-zenkaku-vector . skk-default-jisx0208-vector)
    (skk-hirakana-cursor-color . skk-hiragana-cursor-color)
    (skk-hirakana-mode-string . skk-hiragana-mode-string)
    (skk-num-type-list . skk-num-type-alist)
    (skk-numeric-conversion-float-num . skk-num-convert-float)
    (skk-recompute-numerals-key . skk-num-recompute-key)
    (skk-report-server-response . skk-server-report-response)
    (skk-remote-shell-program . skk-server-remote-shell-program)
    (skk-uniq-numerals . skk-num-uniq)
    (skk-zenkaku-cursor-color . skk-jisx0208-latin-cursor-color)
    (skk-zenkaku-mode . skk-jisx0208-latin-mode)
    (skk-zenkaku-mode-map . skk-jisx0208-latin-mode-map)
    (skk-zenkaku-mode-string . skk-jisx0208-latin-mode-string)
    (skk-zenkaku-vector . skk-jisx0208-latin-vector) )
  "obsolete 変数のエーリスト。" )

(defvar skk-obsolete-function-alist
  '((skk-adjust-numeric-henkan-data . skk-num-process-user-minibuf-input)
    (skk-ascii-mode . skk-latin-mode)
    (skk-ascii-mode-on . skk-latin-mode-on)
    (skk-attr-time-difference . skk-time-difference)
    (skk-compute-numeric-henkan-key . skk-num-compute-henkan-key)
    (skk-date . skk-current-date)
    (skk-flatten-list . skk-num-flatten-list)
    (skk-init-numeric-conversion-variables . skk-num-initialize)
    ;; hirakana -> hiragana
    (skk-isearch-skk-hirakana-mode-p . skk-isearch-skk-hiragana-mode-p)
    ;; typo
    (skk-isearch-skk-jix0208-latin-mode-p . skk-isearch-skk-jisx0208-latin-mode-p)
    ;; hirakana -> hiragana
    (skk-isearch-skk-turn-on-hirakana-mode . skk-isearch-skk-turn-on-hiragana-mode)
    (skk-jisx0208-latin-num-str . skk-num-jisx0208-latin)
    (skk-kakutei-cleanup-henkan-buffer . skk-kakutei-cleanup-buffer) 
    (skk-kakutei-save-and-init-variables . skk-kakutei-initialize)
    (skk-kanji-num-str . skk-num-type2-kanji)
    (skk-kanji-num-str2 . skk-num-type3-kanji)
    (skk-kanji-num-str2-subr . skk-num-type3-kanji-1)
    (skk-kanji-num-str3 . skk-num-type5-kanji)
    (skk-kanji-num-str3-subr . skk-num-type5-kanji-1)
    (skk-middle-list . skk-splice-in)
    (skk-numeric-convert . skk-num-convert)
    (skk-numeric-convert*7 . skk-num-convert*7)
    (skk-numeric-midasi-word . skk-num-henkan-key)
    (skk-raw-number-to-skk-rep . skk-num-rawnum-exp)
    (skk-raw-number-to-skk-rep-1 . skk-num-rawnum-exp-1)
    (skk-recompute-numerals . skk-num-recompute)
    (skk-set-cursor-color-properly . skk-set-cursor-properly)
    (skk-set-cursor-properly . skk-cursor-set-properly)
    (skk-shogi-num-str . skk-num-shogi)
    (skk-update-jisyo-for-numerals . skk-num-update-jisyo)
    (skk-uniq-numerals . skk-num-uniq)
    (skk-public-jisyo-contains-p . skk-public-jisyo-has-entry-p)
    (skk-zenkaku-mode . skk-jisx0208-latin-mode)
    (skk-zenkaku-mode-on . skk-jisx0208-latin-mode-on)
    (skktut-quit-tutorial . skk-tutorial-quit)
    (skktut-tutorial-again . skk-tutorial-again) )
  "obsolete 関数のエーリスト。" )

;;;###autoload
(defun skk-obsolete-check (file)
  "FILE 内の obsolete 変数名と obsolete 関数名をチェックし、書換える。"
  (interactive (list (read-file-name
		      (format "File to check: (default: %s) " skk-init-file)
		      default-directory skk-init-file )))
  (save-window-excursion (skk-obsolete-check-1 file)) )

;;;###autoload
(defun skk-obsolete-check-all-files (&optional program-files-too)
  "関連ファイル全ての obsolete 変数名と obsolete 関数名をチェックし、書換える。
C-u M-x skk-obsolete-check-all-files のように起動したときは、ディフォルトディレ
クトリにある SKK プログラムファイルもチェックを行なう。"
  (interactive "p")
  (save-window-excursion
    (let ((lp load-path)
	  (user-files (list skk-init-file user-init-file))
	  (system-files '("default.el" "site-start.el"))
	  (program-files
	   (if program-files-too
	       '("skk-foreword.el" "skk-gadget.el" "skk-isearch.el" "skk-auto.el"
		 "skk-comp.el" "skk-kakasi.el" "skk-kcode.el" "skk-leim.el"
		 "skk-look.el" "skk-num.el" "skk-server.el" "skk-tut.el" "skk.el"
		 "skk-vip.el" "skk-viper.el" "skk-dbm.el" "skk-rdbms.el"
		 "skk-attr.el" "skk-assoc.el" )))
	  files modified )
      (while lp
	(setq files (nconc (skk-obsolete-check-all-files-1 system-files (car lp))
			   files )
	      lp (cdr lp) ))
      (setq files (nconc (skk-obsolete-check-all-files-1 user-files) files)
	    files (nconc (skk-obsolete-check-all-files-1 program-files) files) )
      (while files
	(setq modified (cons (skk-obsolete-check-1 (car files) 'no-restart-question)
			     modified )
	      files (cdr files) ))
      (message "Obsolete check is completely done")
      (sit-for 1)
      (and (memq t modified)
	   (y-or-n-p "You are strongly recommended to kill this session and restart Emacs.  Kill Emacs?")
	   (save-buffers-kill-emacs) ))))

(defun skk-obsolete-check-1 (file &optional no-restart-question)
  (let ((alist (sort
		(mapcar (function
			 (lambda (al)
			   (cons (prin1-to-string (car al))
				 (prin1-to-string (cdr al)) )))
			(append skk-obsolete-function-alist
				skk-obsolete-variable-alist ))
		;; sort by length.
		(function (lambda (x y) (string< (car y) (car x)))) ))
	modified )
    (find-file (expand-file-name file))
    (delete-other-windows)
    (message "Obsolete check for %s..." file)
    (while alist
      (goto-char (point-min))
      (query-replace-regexp
       ;; not to match a name which contains pattern.
       (concat (car (car alist)) "\\([^-]\\)")
       (concat (cdr (car alist)) "\\1") )
      (setq alist (cdr alist)) )
    (and (buffer-modified-p)
	 (y-or-n-p "Obsolete check for this buffer done.  Save this buffer? ")
	 (setq modified t)
	 (save-buffer) )
    (kill-buffer (current-buffer))
    (message "Obsolete check for %s...done" file)
    (sit-for 1)
    (and (not no-restart-question)
	 modified
	 (y-or-n-p "You are strongly recommended to kill and restart Emacs.  Kill Emacs?")
	 (save-buffers-kill-emacs) )
    modified ))

(defun skk-obsolete-check-all-files-1 (files &optional directory)
  (let ((tmpfiles files)
	objfile objfile-list)
    (while tmpfiles
      (setq objfile (expand-file-name (car tmpfiles) directory))
      (if (file-exists-p objfile)
	  (progn
	    (if (file-readable-p objfile)
		;; OBJFILE may not be writable, but check anyway.
		(setq objfile-list (cons objfile (delete objfile objfile-list)))
	      (message "%s is not readable.  Pass checking" objfile)
	      (sit-for 1) )))
      (setq tmpfiles (cdr tmpfiles)) )
    objfile-list ))

;;;###autoload
(defun skk-obsolete-put-obsolete-mark ()
  (let ((vl skk-obsolete-variable-alist)
	;;(fl skk-obsolete-function-alist) )
	)
    (while vl
      (make-obsolete-variable (car (car vl)) (cdr (car vl)))
      (setq vl (cdr vl)) )
    ;; Put mark by define-obsolete-function-alias in skk-foreword.el.
    ;;(while fl
    ;;  (make-obsolete (car (car fl)) (cdr (car fl)))
    ;;  (setq fl (cdr fl)) )))
    ))

(provide 'skk-obsolete)
;;; skk-obsolete.el ends here
