;; skk-isearch.el --- Incremental search for SKK (for Emacs 18)
;; Copyright (C) 1985, 1986, 1989, 1990, 1991, 1992, 1994, 1996, 1997
;;  Free Software Foundation, Inc.

;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-is-n.el,v 1.2 2000/10/30 22:18:15 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/10/30 22:18:15 $

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
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; SKK 9.6 に付属の Emacs 18 用 skk-isearch.el ほぼそのままです。
;; ~/.emacs に
;;
;; (define-key global-map "\C-s" 'skk-isearch-forward)
;; (define-key global-map "\C-r" 'skk-isearch-backward)
;; (define-key esc-map "\C-s" 'skk-isearch-forward-regexp)
;; (define-key esc-map "\C-r" 'skk-isearch-backward-regexp)
;;
;; と書いてインストールします。

;;; Notes in the original isearch.el:

;;  in loaddefs.el
;; (defvar search-last-string ""
;;   "Last string search for by a search command.
;; This does not include direct calls to the primitive search functions,
;; and does not include searches that are aborted.")
;; (defvar search-last-regexp ""
;;   "Last string searched for by a regexp search command.
;; This does not include direct calls to the primitive search functions,
;; and does not include searches that are aborted.")
;;
;; (defconst search-repeat-char ?\C-s
;;   "Character to repeat incremental search forwards.")
;; (defconst search-reverse-char ?\C-r
;;   "Character to repeat incremental search backwards.")
;; (defconst search-exit-char ?\e
;;   "Character to exit incremental search.")
;; (defconst search-delete-char ?\177
;;   "Character to delete from incremental search string.")
;; (defconst search-quote-char ?\C-q
;;   "Character to quote special characters for incremental search.")
;; (defconst search-yank-word-char ?\C-w
;;   "Character to pull next word from buffer into search string.")
;; (defconst search-yank-line-char ?\C-y
;;   "Character to pull rest of line from buffer into search string.")
;; (defconst search-exit-option t
;;   "Non-nil means random control characters terminate incremental search.")
;;
;; (defvar search-slow-window-lines 1
;;   "*Number of lines in slow search display windows.")
;; (defconst search-slow-speed 1200
;;   "*Highest terminal speed at which to use \"slow\" style incremental search.
;; This is the style where a one-line window is created to show the line
;; that the search has reached.")

;;; Code:

;; suggested by Tsugutomo Enami, 1991.5.27.
(require 'skk)

(skk-deflocalvar skk-isearch-use-previous-mode nil
  "*If non-nil, search mode will be same as the last search mode for the
previous search in the buffer.")

;; suggested by Yoshiyuki Kondo, 1991.6.19.
(skk-deflocalvar skk-isearch-start-mode nil
  "*Specifies the search mode when isearch is called.  nil -> if skk-mode
has been called in this buffer, same as the mode of the buffer, otherwise
ascii search. ascii -> ascii search.  kana -> hira kana search.")

(skk-deflocalvar skk-isearch-mode nil
  "Current search mode.  0 -> hira kana search.  1 -> kata kana search.
2 -> zenei search.  3 -> ascii search.")

(defvar j-ignore-exp "[\n\t <>|]*"
  "*If non-nil, it is inserted between consecutive JIS characters in the
search string.")

(defvar j-ascii-ignore-exp "[\n\t <>|]*"
  "*If non-nil, it is inserted between an ASCII character and a space
charcter.  The space character will be deleted.")

;; modified by Tsugutomo Enami, 1994.4.18.
;; modified by Tsugutomo Enami, 1994.6.17.

(defun j-eval-one-command (char)
  "Interpret CHAR under the current keymaps getting additional keys from
the keyboard if CHAR does not correspond to a command."
  (setq unread-command-char char)
  (condition-case conditions
      (let* ((keys (read-key-sequence nil))
	     (this-command (key-binding keys)))
	(setq last-command-char (aref keys (1- (length keys))))
	(command-execute this-command))
    (quit
     (j-message-conditions conditions))
    (error
     (j-message-conditions conditions))))


(defun j-message-conditions (conditions)
  (let ((case (car conditions)) (msg (cdr conditions)))
    (message "%s %s" case msg)
    (sit-for 3)
    (ding)))

;;;###autoload
(defun skk-isearch-forward (&optional prefix)
  "Forward incremental search under SKK"
  (interactive "P")
  (let ((j-ignore-exp (if prefix nil j-ignore-exp))
	(j-ascii-ignore-exp (if prefix nil j-ascii-ignore-exp)))
    (if (check-region-kanji-code (point-min) (point-max))
	(skk-isearch t)
      (isearch t))))

;;;###autoload
(defun skk-isearch-backward (&optional prefix)
  "Backward incremental search under SKK"
  (interactive "P")
  (let ((j-ignore-exp (if prefix nil j-ignore-exp))
	(j-ascii-ignore-exp (if prefix nil j-ascii-ignore-exp)))
    (if (check-region-kanji-code (point-min) (point-max))
	(skk-isearch nil)
      (isearch nil))))

;;;###autoload
(defun skk-isearch-forward-regexp (&optional prefix)
  "Forward incremental search for regular expression under SKK"
  (interactive "P")
  (let ((j-ignore-exp (if prefix nil j-ignore-exp))
	(j-ascii-ignore-exp (if prefix nil j-ascii-ignore-exp)))
    (if (check-region-kanji-code (point-min) (point-max))
	(skk-isearch t t)
      (isearch t t))))

;;;###autoload
(defun skk-isearch-backward-regexp (&optional prefix)
  "Backward incremental search for regular expression under SKK"
  (interactive "P")
  (let ((j-ignore-exp (if prefix nil j-ignore-exp))
	(j-ascii-ignore-exp (if prefix nil j-ascii-ignore-exp)))
    (if (check-region-kanji-code (point-min) (point-max))
	(skk-isearch nil t)
      (isearch nil t))))

;; This function does all the work of incremental search under SKK.

(defun skk-isearch (forward &optional regexp)
  (let ((search-string "")
	(search-buff " *search-buffer*")
	(cont nil)
	(j skk-j-mode)
	(k skk-katakana)
	(z skk-jisx0208-latin-mode)
	(s skk-mode)
	(iupm skk-isearch-use-previous-mode)
	(im skk-isearch-mode)
	(ism skk-isearch-start-mode)
	(skk-echo nil) ; inhibit echoing of roman prefix
	;(skk-isearch-message nil)
	quote
	(cmds nil)
	(success t)
	(wrapped nil)
	(barrier (point))
	adjusted
	(invalid-regexp nil)
	(slow-terminal-mode (and (<= (baud-rate) search-slow-speed)
				 (> (window-height)
				    (* 4 search-slow-window-lines))))
	(other-end nil)    ;Start of last match if fwd, end if backwd.
	(small-window nil)		;if t, using a small window
	(found-point nil)		;to restore point from a small window
	;; This is the window-start value found by the search.
	(found-start nil)
	(opoint (point))
	(inhibit-quit t))  ;Prevent ^G from quitting immediately.
    (save-excursion
      (set-buffer (get-buffer-create search-buff))
      (erase-buffer)
      (setq skk-j-mode j
	    skk-katakana k
	    skk-jisx0208-latin-mode z)
      ;; modified by Tsugutomo Enami, 1990.7.13
      (skk-mode 1)
      (skk-kakutei)
      (if (and iupm im)
	  (cond ((eq im 0) ;; hira kana search
		 nil)
		((eq im 1) ;; kata kana search
		 ;; modified at the suggestion of Koichi Eryou, 1992.10.13
		 (setq skk-katakana (not skk-katakana)))
		((eq im 2) ;; zenei serach
		 (skk-jisx0208-latin-mode))
		((eq im 3) ;; ascii serach
		 (skk-mode-off))
		(t (error "Invalid skk-isearch-mode!")))
	(cond ((null ism)
	       (if s
		   (if j
		       (if k
			   (progn
			     (setq im 1)
			     ;; modified at the suggestion of Koichi Eryou,
			     ;; 1992.10.13
			     (setq skk-katakana (not skk-katakana)))
			 (setq im 0))
		     (if z
			 (progn
			   (setq im 2)
			   (skk-jisx0208-latin-mode-on))
		       (setq im 3)
		       (skk-mode-off)))
		 (setq im 3)
		 (skk-mode-off)))
	      ((eq ism 'ascii)
	       (setq im 3)
	       (skk-mode-off))
	      ((eq ism 'kana)
	       (setq im 0))
	      (t (error "Invalid skk-isearch-start-mode!")))))
    (setq skk-isearch-mode im)
    (j-isearch-push-state)
    (save-window-excursion
     (catch 'search-done
       (while t
	 (setq quote nil)
	 (or (>= unread-command-char 0)
	     (progn
	       (or (input-pending-p)
		   (j-isearch-message))
	       (if (and slow-terminal-mode
			(not (or small-window (pos-visible-in-window-p))))
		   (progn
		     (setq small-window t)
		     (setq found-point (point))
		     (move-to-window-line 0)
		     (let ((window-min-height 1))
		       (split-window nil (if (< search-slow-window-lines 0)
					     (1+ (- search-slow-window-lines))
					   (- (window-height)
					      (1+ search-slow-window-lines)))))
		     (if (< search-slow-window-lines 0)
			 (progn (vertical-motion
				 (- 1 search-slow-window-lines))
				(set-window-start (next-window) (point))
				(set-window-hscroll (next-window)
						    (window-hscroll))
				(set-window-hscroll (selected-window) 0))
		       (other-window 1))
		     (goto-char found-point)))))
	 (let ((char (if quit-flag
			 ?\C-g
		       (read-char))))
	   (setq quit-flag nil adjusted nil)
	   ;; Meta character means exit search.
	   (cond ((and (>= char 128)
		       search-exit-option)
		  (setq unread-command-char char)
		  (throw 'search-done t))
		 ((eq char search-exit-char)
		  ;; Esc means exit search normally.
		  ;; Except, if first thing typed, it means do nonincremental
		  (if (= 0 (length search-string))
		      (j-nonincremental-search forward regexp))
		  (throw 'search-done t))
		 ((= char ?\C-g)
		  ;; ^G means the user tried to quit.
		  (ding)
		  (discard-input)
		  (if success
		      ;; If search is successful, move back to starting point
		      ;; and really do quit.
		      (progn (goto-char opoint)
			     (signal 'quit nil))
		    ;; If search is failing, rub out until it is once more
		    ;;  successful.
		    (while (not success) (j-isearch-pop))))
		 ((or (eq char search-repeat-char)
		      (eq char search-reverse-char))
		  (if (eq forward (eq char search-repeat-char))
		      ;; C-s in forward or C-r in reverse.
		      (if (equal search-string "")
			  ;; If search string is empty, use last one.
			  (progn
			    (setq search-string
				  (if regexp
				      search-last-regexp search-last-string))
			    (save-excursion
			      (set-buffer search-buff)
			      (erase-buffer)
			      (insert search-string)))
			;; If already have what to search for, repeat it.
			(or success
			    (progn (goto-char
				    (if forward (point-min) (point-max)))
				   (setq wrapped t))))
		    ;; C-s in reverse or C-r in forward, change direction.
		    (setq forward (not forward)))
		  (setq barrier (point)) ; For subsequent \| if regexp.
		  (setq success t)
		  (or (equal search-string "")
		      (j-isearch-search))
		  (j-isearch-push-state))
		 ((and (= char search-delete-char)
		       (not
			(save-excursion
			  (set-buffer search-buff)
			  skk-henkan-on)))
		  ;; Rubout means discard last input item and move point
		  ;; back.  If buffer is empty, just beep.
		  (if (null (cdr cmds))
		      (ding)
		    (j-isearch-pop)))
		 (t
		  (cond ((or (eq char search-yank-word-char)
			     (eq char search-yank-line-char))
			 ;; ^W means gobble next word from buffer.
			 ;; ^Y means gobble rest of line from buffer.
			 (let ((word (save-excursion
				       (and (not forward) other-end
					    (goto-char other-end))
				       (buffer-substring
					(point)
					(save-excursion
					  (if (eq char search-yank-line-char)
					      (end-of-line)
					    (forward-word 1))
					  (point))))))
			   (setq search-string (concat search-string word))
			   (save-excursion
			     (set-buffer search-buff)
			     (erase-buffer)
			     (insert search-string))))
			 ;; Any other control char =>
			 ;;  unread it and exit the search normally.
			 ((and search-exit-option
			       (/= char search-quote-char)
			       (/= char ?\n); masahiko
			       ;(or (= char ?\177);)
			       (and (< char ? ) (/= char ?\t) (/= char ?\r)))
			  (setq unread-command-char char)
			  (throw 'search-done t))
			 (t
			  ;; Any other character => interpret the character
			  ;; in the search-buff.
			  (if (= char search-quote-char)
			      (setq char (read-quoted-char
					  (j-isearch-message t))
				    quote t))
			  (let (string (j-isearch-message t))
			    (save-excursion
			      (set-buffer search-buff)
			      (if quote (insert char)
				(j-eval-one-command char))
			      (if (or skk-henkan-on
				      (and skk-echo
					   (not (string= skk-prefix ""))))
				  (progn
				    (setq search-string
					  (buffer-substring
					   (point-min) (point-max)))
				    (j-isearch-message)
				    (setq cont t))
				(progn
				  (setq string
					(buffer-substring
					 (point-min) (point-max)))
				  (setq cont (string= string search-string))
				  (or cont
				      (setq search-string string))))))))
		  (if (or cont
			  (and (not success)
			       ;; unsuccessful regexp search may become
			       ;;  successful by addition of characters which
			       ;;  make search-string valid
			       (not regexp)))
		      nil
		    ;; If a regexp search may have been made more
		    ;; liberal, retreat the search start.
		    ;; Go back to place last successful search started
		    ;; or to the last ^S/^R (barrier), whichever is nearer.
		    (and regexp success cmds
			 (cond ((memq char '(?* ??))
				(setq adjusted t)
				(let ((cs (nth (if forward
						   4 ; other-end
						 1) ; saved (point)
					       (car (cdr cmds)))))
				  ;; (car cmds) is after last search;
				  ;; (car (cdr cmds)) is from before it.
				  (setq cs (or cs barrier))
				  (goto-char
				   (if forward
				       (max cs barrier)
				     (min cs barrier)))))
			       ((eq char ?\|)
				(setq adjusted t)
				(goto-char barrier))))
		    ;; In reverse regexp search, adding a character at
		    ;; the end may cause zero or many more chars to be
		    ;; matched, in the string following point.
		    ;; Allow all those possibiities without moving point as
		    ;; long as the match does not extend past search origin.
		    (if (and regexp (not forward) (not adjusted)
			     (condition-case ()
				 (looking-at search-string)
			       (error nil))
			     (<= (match-end 0) (min opoint barrier)))
			(setq success t invalid-regexp nil
			      other-end (match-end 0))
		      ;; Not regexp, not reverse, or no match at point.
		      (if (and other-end (not adjusted))
			  (progn
			    (goto-char (if forward other-end
					 (min opoint barrier other-end)))
			    (if (and (not forward)
				     (= (point) other-end))
				(forward-char 1))))
		      (j-isearch-search)))
		  (or cont (j-isearch-push-state)))))))
     (setq found-start (window-start (selected-window)))
     (setq found-point (point)))
    (if (> (length search-string) 0)
	(if regexp
	    (setq search-last-regexp search-string)
	    (setq search-last-string search-string)))
    ;; If there was movement, mark the starting position.
    ;; Maybe should test difference between and set mark iff > threshold.
    (if (/= (point) opoint)
	(push-mark opoint)
      (message ""))
    (if small-window
	(goto-char found-point)
      ;; Exiting the save-window-excursion clobbers this; restore it.
      (set-window-start (selected-window) found-start t))))

(defun j-isearch-message (&optional c-q-hack ellipsis)
  ;; If about to search, and previous search regexp was invalid,
  ;; check that it still is.  If it is valid now,
  ;; let the message we display while searching say that it is valid.
  (and invalid-regexp ellipsis
       (condition-case ()
	   (progn (re-search-forward search-string (point) t)
		  (setq invalid-regexp nil))
	 (error nil)))
  ;; If currently failing, display no ellipsis.
  (or success (setq ellipsis nil))
  (let ((m (concat (if success "" "failing ")
		   (if wrapped "wrapped ")
		   (if regexp "regexp " "")
		   (if (save-excursion
			 (set-buffer search-buff)
			 skk-j-mode)
		       (if (save-excursion
			     (set-buffer search-buff)
			     skk-katakana)
			   "I-サーチ"
			 "I-さーち")
		     (if (save-excursion
			   (set-buffer search-buff)
			   skk-jisx0208-latin-mode)
			 "I-ｓ全英"
		       "I-search"))
		   (if forward ": " " backward: ")
		   search-string
		   (if c-q-hack "^Q" "")
		   (if invalid-regexp
		       (concat " [" invalid-regexp "]")
		     "")))
	sm)
    (aset m 0 (upcase (aref m 0)))
    (save-excursion
      (set-buffer search-buff)
      (setq sm
	    (if skk-j-mode (if skk-katakana 1 0)
	      (if skk-jisx0208-latin-mode 2 3))))
    (setq skk-isearch-mode sm)
    ;(setq M search-message); for debug, masahiko
    (let ((cursor-in-echo-area ellipsis))
      (if c-q-hack m (message "%s" m)))))

(defun j-isearch-pop ()
  (setq cmds (cdr cmds))
  (let ((cmd (car cmds)))
    (setq search-string (car cmd)
	  success (nth 2 cmd)
	  forward (nth 3 cmd)
	  other-end (nth 4 cmd)
	  invalid-regexp (nth 5 cmd)
	  wrapped (nth 6 cmd)
	  barrier (nth 7 cmd))
    (save-excursion
      (set-buffer search-buff)
      (erase-buffer)
      (insert search-string))
    (goto-char (car (cdr cmd)))))

(defun j-isearch-push-state ()
  (setq cmds (cons (list search-string (point)
			 success forward other-end invalid-regexp
			 wrapped barrier)
		   cmds)))

(defun j-isearch-search ()
  (j-isearch-message nil t)
  (condition-case lossage
      (let ((inhibit-quit nil))
	(if regexp (setq invalid-regexp nil))
	(setq success
	      (if j-ignore-exp
		  (funcall
		   (if forward 're-search-forward 're-search-backward)
		   (if regexp
		       search-string
		     (j-add-ignore-exp (regexp-quote search-string)))
		   nil t)
		(funcall
		 (if regexp
		     (if forward 're-search-forward 're-search-backward)
		   (if forward 'search-forward 'search-backward))
		 search-string nil t)))
	(if success
	    (setq other-end
		  (if forward (match-beginning 0) (match-end 0)))))
    (quit (setq unread-command-char ?\C-g)
	  (setq success nil))
    (invalid-regexp (setq invalid-regexp (car (cdr lossage)))
		    (if (string-match
			 "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
			 invalid-regexp)
			(setq invalid-regexp "incomplete input"))))
  (if success
      nil
    ;; Ding if failed this time after succeeding last time.
    (and (nth 2 (car cmds))
	 (ding))
    (goto-char (nth 1 (car cmds)))))

(defun j-add-ignore-exp (str)
  "Expand STR by inserting j-ignore-str between JIS characters."
  (save-excursion
    (set-buffer (get-buffer-create " *search-work-buff*"))
    (erase-buffer)
    (insert str)
    (goto-char (point-min))
    (or (eobp)
	(forward-char 1)
	(while (not (eobp))
	  (if (< (preceding-char) 128)
	      (if (and (= (following-char) ? ) j-ascii-ignore-exp)
		  ;; if j-ascii-ignore-exp is non-nil (and in such a case
		  ;; it must be a string for a regular expression) and
		  ;; if the preceding char is an ascii char and the following
		  ;; char is a space then replace it by j-ascii-ignore-exp.
		  (progn
		    (insert j-ascii-ignore-exp)
		    (delete-char 1))
		;; otherwise go forward
		(forward-char 1))
	    (if (>= (following-char) 128)
		;; if the point is between two JIS characters, insert
		;; j-ignore-exp.
		(progn
		  (insert j-ignore-exp)
		  (forward-char 1))
	      (forward-char 1)))))
    ;; it is necessary to enclose the string by a pair of parentheses
    ;; to cope with a bug in Nemacs' regexp search
    (concat "\\(" (buffer-substring (point-min) (point-max)) "\\)")))

;; This is called from incremental-search
;; if the first input character is the exit character.
;; The interactive-arg-reader uses free variables `forward' and `regexp'
;; which are bound by `incremental-search'.

;; We store the search string in `search-string'
;; which has been bound already by `incremental-search'
;; so that, when we exit, it is copied into `search-last-string'.

(defun j-nonincremental-search (forward regexp)
  (let (message char function string inhibit-quit
		(cursor-in-echo-area t))
    ;; Prompt assuming not word search,
    (setq message (if regexp
		      (if forward "Regexp search: "
			"Regexp search backward: ")
		    (if forward "Search: " "Search backward: ")))
    (message "%s" message)
    ;; Read 1 char and switch to word search if it is ^W.
    (setq char (read-char))
    (if (eq char search-yank-word-char)
	(setq message (if forward "Word search: " "Word search backward: "))
      ;; Otherwise let that 1 char be part of the search string.
      (setq unread-command-char char))
    (setq function
	  (if (eq char search-yank-word-char)
	      (if forward 'word-search-forward 'word-search-backward)
	    (if regexp
		(if forward 're-search-forward 're-search-backward)
	      (if forward 'search-forward 'search-backward))))
    ;; Read the search string with corrected prompt.
    (save-excursion
      (set-buffer (get-buffer-create " *Minibuf-0*"))
      (setq skk-j-mode
	    (save-excursion
	      (set-buffer search-buff)
	      skk-j-mode)))
    (setq string (read-string message))
    ;; Empty means use default.
    (if (= 0 (length string))
	(setq string search-last-string)
      ;; Set last search string now so it is set even if we fail.
      (setq search-last-string string))
    ;; Since we used the minibuffer, we should be available for redo.
    (setq command-history (cons (list function string) command-history))
    ;; Go ahead and search.
    (funcall function string)))

;; patch for old Nemacs (v2)
;; suggested by Masakazu Takahashi, 1992.4.12.
;; this modification is necessary since the function
;; check-region-kanji-code is unbound in Nemacs v2.

(or (fboundp 'check-region-kanji-code)
    ;; we define check-region-kanji-code as a function which simply
    ;; returns t.  it doesn't work like the real check-region-kanji-code,
    ;; but we don't care.
    (defun check-region-kanji-code (p q) t))

;; end of patch for old Nemacs (v2)

(require 'product)
(product-provide (provide 'skk-isearch) (require 'skk-version))
;;; skk-isearch.el ends here
