;;; install-info.el -- install-info in Emacs Lisp
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: docs, help

;; This file is not part of GNU Emacs.

;; This program  is free software;  you  can  redistribute it  and/or modify it
;; under the terms  of the GNU General Public License as published  by the Free
;; Software  Foundation;  either versions  2,  or  (at your option)  any  later
;; version.

;; This program is distributed  in the hope that it will be useful  but WITHOUT
;; ANY  WARRANTY;  without  even  the implied  warranty  of MERCHANTABILITY  or
;; FITNESS  FOR A PARTICULAR PURPOSE.  See  the GNU General Public License  for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs,  see  the  file  COPYING.  If not,  write  to  the  Free Software
;; Foundation Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

;;; Commentary:

;; This program updates info/dir entries, like install-info in GNU texinfo.

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (ignore-errors
    (require 'jka-compr)))

;; Macros.
(defmacro install-info-compressed-name-p (filename)
  (if (not (featurep 'jka-compr))
      nil
    (` (jka-compr-get-compression-info (, filename)))))

(defmacro install-info-forward-line (n)
  (` (when (< 0 (forward-line (, n)))
       (unless (bolp)
	 (insert "\n")))))

(defmacro install-info-point-at-eol (&optional n)
  (if (fboundp 'point-at-eol)
      (` (point-at-eol (, n)))
    (` (save-excursion
	 (if (or (null (, n)) (= 1 (, n)))
	     (end-of-line)
	   (when (eq (forward-line (- (, n) 1)) 0)
	     (end-of-line)))
	 (point)))))

;; Functions.
(defun install-info-skip-blank-lines ()
  (while (and (eolp) (not (eobp)))
    (install-info-forward-line 1)))

(defun install-info-groups (section entry)
  (let (groups)
    (dolist (sec section)
      (setq groups
	    (nconc groups (list (cons sec entry)))))
    groups))

(defun install-info-insert-file-contents (file &optional visit beg end replace)
  (let ((coding-system-for-read 'raw-text))
    (funcall (if (install-info-compressed-name-p file)
		 'jka-compr-insert-file-contents
	       'insert-file-contents)
	     file visit beg end replace)))

(defun install-info-write-region (start end file &optional append visit)
  (let ((coding-system-for-write 'raw-text))
    (funcall (if (install-info-compressed-name-p file)
		 'jka-compr-write-region
	       'write-region)
	     start end file append visit)))

(defun install-info (info-file dir-file &optional entry section delete)
  "Install or delete dir entries from INFO-FILE in the Info directory file
DIR-FILE.

Optioinal third arg ENTRY specifies the text inserted as an Info directory
entry. ENTRY should have the form of an Info menu item line plus zero or more
extra lines starting with whitespace. If you specify more than one entry (i.e.
list of entries), they are all added. If you don't specify any entries, they
are determined from information in the Info file itself.

If optional fourth arg SECTION is given, put this file's entries in section
SECTION of the directory. If you specify more than one section (i.e. list of
sections), all the entries are added in each of the sections. If you don't
specify any sections, they are determined from information in the Info file
itself.

If optional fifth arg DELETE is non-nil, delete existing entries for INFO-FILE
from DIR-FILE; don't insert any new entries."
  (interactive "fInfo File: \nFDir File: ")
  (let ((buf (get-buffer-create " *install-info-tmp*"))
	groups)
    ;;
    (if (stringp info-file)
	(setq info-file (expand-file-name info-file))
      (error "%s" "No input file specified"))
    (unless (file-exists-p info-file)
      (error "No such file or directory for %s" info-file))
    ;;
    (if (stringp dir-file)
	(setq dir-file (expand-file-name dir-file))
      (error "%s" "No dir file specified"))
    ;;
    (when (stringp entry)
      (setq entry (list entry)))
    (when (stringp section)
      (setq section (list section)))
    ;;
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (install-info-insert-file-contents info-file))
    ;;
    (cond
     ((and entry section)
      (setq groups (install-info-groups section entry)))
     (entry
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(while (re-search-forward "^INFO-DIR-SECTION " nil t)
	  (setq section
		(nconc section
		       (list (buffer-substring
			      (match-end 0)
			      (install-info-point-at-eol)))))))
      (unless section
	(setq section (list "Miscellaneous")))
      (setq groups (install-info-groups section entry)))
     (section
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(while (re-search-forward "^START-INFO-DIR-ENTRY" nil t)
	  (install-info-forward-line 1)
	  (while (not (looking-at "^END-INFO-DIR-ENTRY"))
	    (let ((start (point))
		  str)
	      (unless (eolp)
		(setq str (buffer-substring start (install-info-point-at-eol)))
		(if (string-match "^* " str)
		    (setq entry (cons str entry))
		  (when entry
		    (setq entry
			  (cons (format "%s\n%s" (car entry) str)
				(cdr entry))))))
	      (install-info-forward-line 1)))))
      (unless (setq entry (nreverse entry))
	(error "warning; no info dir entry in %s" info-file))
      (setq groups (install-info-groups section entry)))
     (t
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(while (re-search-forward "^INFO-DIR-SECTION " nil t)
	  (let (section entry)
	    (beginning-of-line)
	    (while (looking-at "^INFO-DIR-SECTION ")
	      (setq section
		    (nconc section
			   (list (buffer-substring
				  (match-end 0) (install-info-point-at-eol)))))
	      (install-info-forward-line 1))
	    (install-info-skip-blank-lines)
	    (when (looking-at "^START-INFO-DIR-ENTRY")
	      (install-info-forward-line 1)
	      (while (not (looking-at "^END-INFO-DIR-ENTRY"))
		(let ((start (point))
		      str)
		  (unless (eolp)
		    (setq str (buffer-substring start
						(install-info-point-at-eol)))
		    (if (string-match "^* " str)
			(setq entry (cons str entry))
		      (when entry
			(setq entry
			      (cons (format "%s\n%s" (car entry) str)
				    (cdr entry))))))
		  (install-info-forward-line 1))))
	    (when (and section (setq entry (nreverse entry)))
	      (setq groups
		    (nconc groups
			   (install-info-groups section entry))))))
	;;
	(unless groups
	  (goto-char (point-min))
	  (while (re-search-forward "^START-INFO-DIR-ENTRY" nil t)
	    (install-info-forward-line 1)
	    (while (not (looking-at "^END-INFO-DIR-ENTRY"))
	      (let ((start (point))
		    str)
		(unless (eolp)
		  (setq str (buffer-substring start
					      (install-info-point-at-eol)))
		  (if (string-match "^* " str)
		      (setq entry (cons str entry))
		    (when entry
		      (setq entry
			     (cons (format "%s\n%s" (car entry) str)
				   (cdr entry))))))
		(install-info-forward-line 1))))
	  (unless (setq entry (nreverse entry))
	    (error "warning; no info dir entry in %s" info-file))
	  (unless section
	    (setq section (list "Miscellaneous")))
	  (setq groups (install-info-groups section entry))))))
    ;;
    (if delete
	(install-info-delete-groups groups dir-file)
      (install-info-add-groups groups dir-file))
    ;;
    (kill-buffer buf)))

(defun install-info-delete-groups (groups dir)
  (setq dir (expand-file-name dir))
  (let ((buf (get-buffer-create " *install-info-dir*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (if (not (file-exists-p dir))
	  (error "No such file or directory for %s" dir)
	(install-info-insert-file-contents dir)
	(dolist (en (apply 'append (mapcar 'cdr groups)))
	  (let ((key (when (string-match ")" en)
		       (substring en 0 (match-beginning 0)))))
	    (goto-char (point-min))
	    (while (re-search-forward
		    (concat "^" (regexp-quote key) "\\(\\.info\\)?)") nil t)
	      (let ((start (match-beginning 0)))
		(install-info-forward-line 1)
		(while (not (or (eolp)
				(looking-at "^* ")))
		  (install-info-forward-line 1))
		(delete-region start (point)))))))
      (install-info-write-region (point-min) (point-max) dir))
    (kill-buffer buf)))

(defun install-info-add-groups (groups dir)
  (setq dir (expand-file-name dir))
  (let ((buf (get-buffer-create " *install-info-dir*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (when (file-exists-p dir)
	(install-info-insert-file-contents dir))
      (goto-char (point-min))
      (unless (let ((case-fold-search t))
		(and (re-search-forward "^\037" nil t)
		     (re-search-forward "Node:.*Top" nil t)
		     (re-search-forward "^\\* Menu:" nil t)))
	(unless (= 0 (buffer-size))
	  (install-info-write-region (point-min) (point-max) (concat dir "~")))
	(erase-buffer)
	(insert "This is the file .../info/dir, which contains the
topmost node of the Info hierarchy, called (dir)Top.
The first time you invoke Info you start off looking at this node.
\037
File: dir,	Node: Top	This is the top of the INFO tree

  This (the Directory node) gives a menu of major topics.
  Typing \"q\" exits, \"?\" lists all Info commands, \"d\" returns here,
  \"h\" gives a primer for first-timers,
  \"mEmacs<Return>\" visits the Emacs manual, etc.

  In Emacs, you can click mouse button 2 on a menu item or cross reference
  to select it.

* Menu:
"))
      (dolist (group (sort groups (function
				   (lambda (g1 g2)
				     (string-lessp (car g1) (car g2))))))
	(let ((sec (car group))
	      (entry (cdr group)))
	  (goto-char (point-min))
	  (cond
	   ((re-search-forward (concat "^" sec "$") nil t)
	    (install-info-forward-line 1)
	    (let ((sec-start (point)))
	      (install-info-skip-blank-lines)
	      (let ((en-start (point)))
		(dolist (en entry)
		  (if (or (eobp)
			  (not (looking-at "^\\* ")))
		      ;; No entries in the section.
		      (progn
			(goto-char sec-start)
			(unless (eolp)
			  (insert "\n")
			  (goto-char sec-start)))
		    ;; Delete the old entry.
		    (let ((key (when (string-match ")" en)
				 (substring en 0 (match-beginning 0)))))
		      (while (not
			      (catch 'end
				(cond
				 ((looking-at
				   (concat "^" (regexp-quote key)
					   "\\(\\.info\\)?)"))
				  (let ((start (point)))
				    (install-info-forward-line 1)
				    (while (looking-at "^[ \t]")
				      ;; Entry has two or more lines.
				      (install-info-forward-line 1))
				    (delete-region start (point))
				    (throw 'end t)))
				 ((eobp)
				  (throw 'end t))
				 ((eolp)
				  (install-info-forward-line 1))
				 ((not (looking-at "^\\(\\*\\|[ \t]\\)"))
				  ;; next section
				  (throw 'end t))
				 (t
				  (install-info-forward-line 1)))))))
		    (goto-char en-start)
		    ;; Insert the new entry.
		    (while (not
			    (catch 'here
			      (while (not (eolp))
				(let ((line (buffer-substring
					     (point)
					     (install-info-point-at-eol))))
				  (cond
				   ((not (looking-at "^\\(\\*\\|[ \t]\\)"))
				    ;; next section
				    (let ((pt (point)))
				      (insert "\n")
				      (foto-char (point)))
				    (throw 'here t))
				   ((or (looking-at "^[ \t]")
					(string-lessp line en))
				    (install-info-forward-line 1))
				   (t
				    (throw 'here t)))))
			      (let ((maybe-here (point)))
				(install-info-skip-blank-lines)
				(if (looking-at "^\\(\\*\\|[ \t]\\)")
				    (throw 'here nil)
				  ;; reaches next section.
				  (goto-char maybe-here)
				  (throw 'here t)))))))
		  (when (and (eobp) (not (bolp)))
		    (install-info-forward-line 1))
		  (insert (format "%s\n" en))
		  (goto-char en-start)))))
	   (t
	    (goto-char (point-max))
	    (install-info-forward-line 1)
	    (insert (format "\n%s\n" sec))
	    (dolist (en (sort entry 'string-lessp))
		(insert (format "%s\n" en)))))))
      (install-info-write-region (point-min) (point-max) dir))
    (kill-buffer buf)))

;;

(provide 'install-info)

;;; install-info.el ends here
