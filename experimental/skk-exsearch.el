;;; skk-exsearch.el --- 外部検索プログラム共用 interface -*- coding: iso-2022-jp -*-
;; Copyright (C) 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-exsearch.el,v 1.10 2010/08/02 15:21:05 skk-cvs Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2010/08/02 15:21:05 $

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
;;
;; This program may be (or may not be) a core engine of external
;; searching program of Daredevil (or some other new branch) SKK.

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))
(require 'path-util) ; for exec-installed-p.
(require 'eieio)

(defvar skk-grep-dic 
  "/usr/local/share/skk/SKK-JISYO.L"
  "*grep search dictionary.")

(defclass search-engine ()
  ((program :initarg :program
	    :initform nil
	    :documentation "Program file.")
   ;;(argument :initarg :argument :initform nil :documentation "")
   (dictionary :initarg :dictionary
	       :initform nil
	       :documentation "Dictionary file to be searched."))
  "External search engine superclass.")

(defclass synchronous-search-engine (search-engine)
  ((infile :initarg :infile
	   :initform nil
	   :documentation
	   "This is where the program's input comes from. (nil means `/dev/null').")
   (stderr :initarg :stderr
	   :initform t
	   :documentation "What to do with standard error in the child.\
nil (discard standard error output), t (mix it with ordinary output),\
or a file name string.")
   (success-exit-code :initarg :success-exit-code 
		      :initform 0
		      :documentation "Numeric exit status of success.")
   (error-exit-code :initarg :error-exit-code
		    :initform 2
		    :documentation "Numeric exit status of error."))
  "External synchronous search engine class.")

(defclass regular-engine (synchronous-search-engine)
  ((coding-system :initarg :coding-system
		  :initform (lambda () (skk-find-coding-system skk-jisyo-code))))
  "Regular search engine type.
Call program synchronously in separate process.
Output of this type is a line that contains candidates delimited by slash.")


(defclass grep-engine (synchronous-search-engine)
  ((coding-system :initarg :coding-system
		  :initform (lambda () (skk-find-coding-system skk-jisyo-code))))
  "Regular search engine type for grep search.
Call program synchronously in separate process.
Output of this type is a line that contains candidates delimited by slash.")


(defclass look-engine (synchronous-search-engine)
  (())
  "look type.
Call program synchronously in separate process.
This type inserts multiple lines to the buffer.  Each line contains a candidate.")

(defmethod setup-synchronous-engine ((engine synchronous-search-engine))
  (and (slot-exists-p engine 'coding-system)
       ;; return nil if cdbget executable binary does not installed.
       (slot-value engine 'program)
       (with-slots ((code coding-system)) engine
	 (if (and code (fboundp 'modify-coding-system-alist))
	     (modify-coding-system-alist
	      'process (oref engine program) (cons code code))))))

(defvar cdbget (make-instance regular-engine
			      :program (exec-installed-p "cdbget")
			      :infile "/usr/local/share/skk/SKK-JISYO.L.cdb")
  "*cdbget search engine object.")
(setup-synchronous-engine cdbget)

(defvar grep (make-instance grep-engine :program 
			    (exec-installed-p "grep")
			    :infile skk-grep-dic)
  "*grep search engine object.")

(setup-synchronous-engine grep)

(defvar look (make-instance look-engine :program 
			    (exec-installed-p "look"))
  "*look search engine object.")

			       
(defmethod core-engine ((engine synchronous-search-engine) argument)
  ;; core search engine
  (save-excursion
    (let ((exit-code (apply 'call-process (oref engine program)
			    (oref engine infile)
			    (list t		;output to current buffer.
				  (oref engine stderr))
			    nil
			    argument))
	  error)
      (cond ((and (= (oref engine success-exit-code) exit-code)
		  (> (buffer-size) 0)))
	    ((>= exit-code (oref engine error-exit-code))
	     (error (buffer-substring-no-properties (point-min) (point-max))))))))

(defmethod search-engine ((engine regular-engine) &rest argument)
  (let ((okurigana (or skk-henkan-okurigana skk-okuri-char))
	l)
    (with-temp-buffer 
      (if (core-engine engine argument)
	  (progn
	    (forward-char 1)
	    (and (setq l (skk-compute-henkan-lists okurigana))
		 (cond ((and okurigana skk-henkan-okuri-strictly)
			(nth 2 l))
		       ((and okurigana skk-henkan-strict-okuri-precedence)
			(skk-nunion (nth 2 l) (car l)))
		       (t (car l)))))))))

(defmethod search-engine ((engine grep-engine) &rest argument)
  (let ((okurigana (or skk-henkan-okurigana skk-okuri-char))
        (tmparg argument)
        l)
    (with-temp-buffer 
      (setq argument (cons (concat "^" (car tmparg) " ") nil))
      (if (core-engine engine argument)
	  (progn
	    (while (not (eq (char-after) ?/))
	      (forward-char 1))
	    (forward-char 1)
	    (and (setq l (skk-compute-henkan-lists okurigana))
		 (cond ((and okurigana skk-henkan-okuri-strictly)
			(nth 2 l))
		       ((and okurigana skk-henkan-strict-okuri-precedence)
			(skk-nunion (nth 2 l) (car l)))
		       (t (car l)))))))))



(defmethod search-engine ((engine look-engine) &rest argument)
  (with-temp-buffer 
    (let ((word argument)
	  opt)
      (and (oref engine dictionary) 
	   (nconc argument (list (oref engine dictionary))))
      (and skk-look-dictionary-order (setq opt "d"))
      (and skk-look-ignore-case (setq opt (concat "f" opt)))
      (and skk-look-use-alternate-dictionary
	   (setq opt (concat "a" opt)))
      (and opt (setq argument (cons (concat "-" opt) argument)))
      (and skk-look-termination-character
	   (setq argument
		 (cons (list "-t" skk-look-termination-character) argument)))
      (and (core-engine engine argument)
	   (delete word (split-string (buffer-substring-no-properties
				       (point-min) (1- (point-max)))
				      "\n"))))))

;;;###autoload
(defun skk-cdbget-search ()
  (search-engine cdbget (if skk-use-numeric-conversion
			    (skk-num-compute-henkan-key skk-henkan-key)
			  skk-henkan-key)))

;;;###autoload
(defun skk-grep-search ()
  (search-engine grep (if skk-use-numeric-conversion
			    (skk-num-compute-henkan-key skk-henkan-key)
			  skk-henkan-key)))

;;;###autoload
(defun skk-look-search ()
  (and skk-abbrev-mode
       (eq (aref skk-henkan-key (1- (length skk-henkan-key))) ?*)
       (let ((args (substring skk-henkan-key 0 (1- (length skk-henkan-key))))
	     v)
	 (if (not skk-look-use-ispell)
	     (setq v (search-engine look args))
	   (setq v (skk-look-ispell args)))
	 (if (not skk-look-recursive-search)
	     v
	   (let (skk-henkan-key v2 v3)
	     (while v
	       (let ((skk-current-search-prog-list (copy-sequence skk-search-prog-list)))
		 (setq skk-current-search-prog-list
		       (delete '(skk-look) skk-current-search-prog-list))
		 (setq skk-current-search-prog-list
		       (delete '(skk-look-search) skk-current-search-prog-list))
		 (setq skk-henkan-key (car v))
		 (while skk-current-search-prog-list
		   (setq v3 (skk-search)
			 v2 (if (not skk-look-expanded-word-only)
				(skk-nunion v2 (cons (car v) v3))
			      (if v3
				  (skk-nunion v2 (cons (car v) v3))
				v2)))))
	       (setq v (cdr v)))
	     v2)))))

(provide 'skk-exsearch)

;;; skk-exsearch.el ends here
