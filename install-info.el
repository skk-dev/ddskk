;;; install-info.el --- install-info in Emacs Lisp

;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: docs, help

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs, see the file COPYING.  If not, write to the
;; Free Software Foundation Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

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
  (when (featurep 'jka-compr)
    `(jka-compr-get-compression-info ,filename)))

(defmacro install-info-forward-line (n)
  `(when (< 0 (forward-line ,n))
     (unless (bolp)
       (insert "\n"))))

(put 'install-info-save-point 'lisp-indent-function 0)
(defmacro install-info-save-point (&rest forms)
  `(let ((original-point (point)))
     (prog1
	 (progn ,@forms)
       (goto-char original-point))))

(defmacro install-info-point-at-eol (&optional n)
  (if (fboundp 'point-at-eol)
      `(point-at-eol ,n)
    `(install-info-save-point
      (if (or (null ,n) (= 1 ,n))
	  (end-of-line)
	(when (eq (forward-line (- ,n 1)) 0)
	  (end-of-line)))
      (point))))

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
		 #'jka-compr-insert-file-contents
	       #'insert-file-contents)
	     file visit beg end replace)))

(defun install-info-write-region (start end file &optional append visit)
  (let ((coding-system-for-write 'raw-text))
    (funcall (if (install-info-compressed-name-p file)
		 #'jka-compr-write-region
	       #'write-region)
	     start end file append visit)))

(defun install-info (info-file dir-file &optional entry section delete)
  "Install or delete dir entries from INFO-FILE in the Info directory file
DIR-FILE.

Optional third arg ENTRY specifies the text inserted as an Info directory
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
  (save-match-data
    (install-info-1 info-file dir-file entry section delete)))

(defun install-info-1 (info-file dir-file entry section delete)
  (let ((buf (get-buffer-create " *install-info-tmp*"))
	groups)
    ;;
    (unless (stringp info-file)
      (error "%s" "No input file specified"))
    (setq info-file (expand-file-name info-file))
    ;;
    (unless (file-exists-p info-file)
      (error "No such file or directory for %s" info-file))
    ;;
    (unless (stringp dir-file)
      (error "%s" "No dir file specified"))
    (setq dir-file (expand-file-name dir-file))
    ;;
    (when (stringp entry)
      (setq entry (list entry)))
    (when (stringp section)
      (setq section (list section)))
    ;;
    (unless (and entry section)
      (save-excursion
	(set-buffer buf)
	(setq buffer-read-only nil)
	(erase-buffer)
	(install-info-insert-file-contents info-file)))
    ;;
    (cond
     ((and entry section)
      ;; Both entry and section are given.
      (unless delete
	(setq groups (install-info-groups section entry))))
     (entry
      ;; Only entry is given. Determine section from the info file.
      (unless delete
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
	(setq groups (install-info-groups section entry))))
     ((or section delete)
      ;; Only section is given. Determine entry from the info file.
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(while (re-search-forward "^START-INFO-DIR-ENTRY" nil t)
	  (install-info-forward-line 1)
	  (while (not (looking-at "^END-INFO-DIR-ENTRY"))
	    (let ((start (point))
		  str)
	      (when (eobp)
		(error
		 "%s"
		 "START-INFO-DIR-ENTRY without matching END-INFO-DIR-ENTRY"))
	      (unless (eolp)
		(setq str (buffer-substring start (install-info-point-at-eol)))
		(if (string-match "^\\* " str)
		    (push str entry)
		  (when entry
		    (setcar entry (format "%s\n%s" (car entry) str)))))
	      (install-info-forward-line 1)))))
      (unless (setq entry (nreverse entry))
	(error "warning; no info dir entry in %s" info-file))
      (unless delete
	(setq groups (install-info-groups section entry))))
     (t
      ;; Neither entry nor section is given.
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(while (re-search-forward "^INFO-DIR-SECTION " nil t)
	  (let (section entry)
	    (beginning-of-line)
	    (while (looking-at "^INFO-DIR-SECTION ")
	      ;; Sometimes multiple sections are given for one entry.
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
		    (if (string-match "^\\* " str)
			(push str entry)
		      ;; Sometimes mutiple lines are used for one entry.
		      (when entry
			(setcar entry (format "%s\n%s" (car entry) str)))))
		  (install-info-forward-line 1))))
	    (when (and section (setq entry (nreverse entry)))
	      (setq groups
		    (nconc groups
			   (install-info-groups section entry)))))))))
    ;;
    (if delete
	(install-info-delete-entry entry dir-file)
      (unless groups
	;; No section is specified on the info file. Use "Miscellaneous"
	;; as the section name.
	(save-excursion
	  (set-buffer buf)
	  (goto-char (point-min))
	  (while (re-search-forward "^START-INFO-DIR-ENTRY" nil t)
	    (install-info-forward-line 1)
	    (while (not (looking-at "^END-INFO-DIR-ENTRY"))
	      (let ((start (point))
		    str)
		(unless (eolp)
		  (setq str (buffer-substring start
					      (install-info-point-at-eol)))
		  (if (string-match "^\\* " str)
		      (push str entry)
		    (when entry
		      (setcar entry (format "%s\n%s" (car entry) str)))))
		(install-info-forward-line 1)))))
	(unless (setq entry (nreverse entry))
	  (error "warning; no info dir entry in %s" info-file))
	(unless section
	  (setq section (list "Miscellaneous")))
	(setq groups (install-info-groups section entry)))
      ;;
      (install-info-add-groups groups dir-file))
    ;;
    (kill-buffer buf)))

(defun install-info-delete-entry (entry dir)
  ;; Delete all entries given.
  (setq dir (expand-file-name dir))
  (let ((buf (get-buffer-create " *install-info-dir*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (unless (file-exists-p dir)
	(error "No such file or directory for %s" dir))
      (install-info-insert-file-contents dir)
      (dolist (en entry)
	(let (start file)
	  ;; XEmacs-style entry is not considered here.
	  (when (string-match "(" en)
	    (setq en (substring en (match-end 0)))
	    (setq file (when (string-match ")" en)
			 (substring en 0 (match-beginning 0)))))
	  (goto-char (point-min))
	  (while (re-search-forward
		  (concat "^\\*.*("
			  (regexp-quote file)
			  "\\(\\.info\\)?)") nil t)
	    (let ((start (match-beginning 0)))
	      (install-info-forward-line 1)
	      (while (not (or (eolp)
			      (looking-at "^\\* ")))
		(install-info-forward-line 1))
	      (delete-region start (point))))))
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
	;; If the existing dir file is not valid, replace it with a good one.
	(unless (= 0 (buffer-size))
	  ;; Create a backup file.
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
      (dolist (group (sort groups #'(lambda (g1 g2)
				      (string-lessp (car g1) (car g2)))))
	(let ((sec (car group))
	      (entry (cdr group)))
	  (goto-char (point-min))
	  (cond
	   ((re-search-forward (concat "^" sec "$") nil t)
	    ;; section exists
	    (install-info-forward-line 1)
	    (let ((sec-start (point)))
	      (install-info-skip-blank-lines)
	      (let ((en-start (point)))
		(dolist (en entry)
		  (cond
		   ((or (eobp)
			(not (looking-at "^\\* ")))
		      ;; No entries in the section.
		    (goto-char sec-start)
		    (unless (eolp)
		      (insert "\n")
		      (goto-char sec-start)))
		   (t
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
				  ;; reaches the next section
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
				    ;; reaches the next section
				    (let ((pt (point)))
				      (insert "\n")
				      (goto-char (point)))
				    (throw 'here t))
				   ((or
				     ;; not at the head of an entry
				     (looking-at "^[ \t]")
				     ;; on the way
				     (string-lessp line en))
				    (install-info-forward-line 1))
				   (t
				    (throw 'here t)))))
			      (let ((maybe-here (point)))
				(install-info-skip-blank-lines)
				(when (looking-at "^\\(\\*\\|[ \t]\\)")
				  (throw 'here nil))
				;; reaches the next section.
				(goto-char maybe-here)
				(throw 'here t)))))))
		  (when (and (eobp) (not (bolp)))
		    (install-info-forward-line 1))
		  (insert (format "%s\n" en))
		  (goto-char en-start)))))
	   (t
	    ;; section doesn't exist. Add it at the bottom.
	    (goto-char (point-max))
	    (install-info-forward-line 1)
	    (insert (format "\n%s\n" sec))
	    (dolist (en (sort entry #'string-lessp))
		(insert (format "%s\n" en)))))))
      ;;
      (install-info-write-region (point-min) (point-max) dir))
    (kill-buffer buf)))

(provide 'install-info)

;;; install-info.el ends here
