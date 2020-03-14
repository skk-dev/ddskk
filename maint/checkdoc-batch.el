;;; checkdoc-batch.el --- batch report of M-x checkdoc things

;; Copyright 2010, 2011, 2014, 2015, 2016, 2019 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 8
;; Keywords: lisp, maint, checkdoc
;; URL: http://user42.tuxfamily.org/checkdoc-batch/index.html

;; checkdoc-batch.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; checkdoc-batch.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.

;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `M-x checkdoc-batch' runs checkdoc.el over the current buffer.  It's not
;; interactive and doesn't change anything but instead presents a report of
;; problems found using a compilation-mode buffer.  `M-x next-error' can
;; step through the problems in the usual compilation-mode style.

;; An accompanying script "emacs-checkdoc-batch" can run checkdoc-batch on
;; .el files from the command line.  It can be run under M-x compile if
;; desired.

;; See the `checkdoc-batch' docstring below for more.

;;; Emacsen:

;; Designed for Emacs 21 and up.  Works in XEmacs 21 except
;; M-x checkdoc-batch-files needs a `file-expand-wildcards' copied from
;; Emacs.

;;; Install:

;; Put checkdoc-batch.el in one of your `load-path' directories and to make
;; `M-x checkdoc-batch' etc available add to your .emacs
;;
;;     (autoload 'checkdoc-batch       "checkdoc-batch" nil t)
;;     (autoload 'checkdoc-batch-files "checkdoc-batch" nil t)
;;
;; There's autoload cookies for this if you install via
;; `M-x package-install' or know `update-file-autoloads'.

;;; History:

;; Version 1 - the first version
;; Version 2 - new checkdoc-batch-files, fix for linenum at end of file
;; Version 3 - another linenum fix
;; Version 4 - support checkdoc-spellcheck-documentation-flag
;; Version 5 - revert-buffer and auto-revert-mode support
;;           - don't show ispell start/stop messages
;; Version 6 - don't use remove-if
;; Version 7 - macros not needed when running byte compiled
;; Version 8 - explicit let of global

;;; Code:

;; for `ad-find-advice' macro when running uncompiled
;; (don't unload 'advice before our -unload-function)
(require 'advice)

(require 'checkdoc)

(eval-when-compile
  (unless (and (fboundp 'declare)
               (fboundp 'dolist)
               (fboundp 'declare))
    (require 'cl))) ;; for `dolist' macro in emacs20, `declare' in emacs21


;;----------------------------------------------------------------------------
;; xemacs21 missing bits

;; [same in ffap-makefile-vars.el, man-completion.el]
;;
(eval-and-compile ;; quieten emacs byte compiler
  ;; no `eval-when-compile' on this fboundp because in xemacs 21.4.22
  ;; easy-mmode.el (which is define-minor-mode etc) rudely defines a
  ;; replace-regexp-in-string, so a compile-time test is unreliable
  (if (fboundp 'replace-regexp-in-string)
      ;; emacs (21 up)
      (defalias 'checkdoc-batch--replace-regexp-in-string
        'replace-regexp-in-string)

    ;; xemacs21
    (defun checkdoc-batch--replace-regexp-in-string
        (regexp rep string fixedcase literal)
      "`replace-regexp-in-string' made available in xemacs.
The FIXEDCASE argument is ignored, case is always fixed."
      (replace-in-string string regexp rep literal))))

;;----------------------------------------------------------------------------
;; emacs22 new bits

(defalias 'checkdoc-batch--buffer-chars-modified-tick
  (if (eval-when-compile (fboundp 'buffer-chars-modified-tick))
      'buffer-chars-modified-tick
    'buffer-modified-tick))


;;----------------------------------------------------------------------------
;; generic

(defun checkdoc-batch-column-at-pos (pos)
  "An internal part of checkdoc-batch.el.
Return the column number of buffer position POS.
The leftmost column is number 1, unlike `current-column' which
gives 0."
  (save-excursion
    (goto-char pos)
    (1+ (current-column))))

(defun checkdoc-batch-completion-first-candidate (collection)
  "An internal part of checkdoc-batch.el.
Return first candidate from completion COLLECTION.
COLLECTION can be anything `all-completions' and friends accept,
like an alist, handler function, etc."
  (let ((checkdoc-batch-completion-first-candidate--stop nil))
    (car (all-completions
          ""
          collection
          (lambda (candidate)
            (if checkdoc-batch-completion-first-candidate--stop
                nil
              (setq checkdoc-batch-completion-first-candidate--stop t)))))))

(eval-when-compile
  (defmacro checkdoc-batch--with-buffer-file-name (filename &rest body)
    ;; checkdoc-params: (filename body)
    "An internal part of checkdoc-batch.el.
This macro doesn't exist when running byte compiled.

Temporarily set `buffer-file-name' to FILENAME and evaluate BODY.
This is done by a `setq' and `unwind-protect' since not sure that
a `let'-binding of `buffer-file-name' is safe against all kinds
of buffer switching."

    (declare (debug t)
             (indent 1))
    `(let ((checkdoc-batch--with-buffer-file-name buffer-file-name)
           (checkdoc-batch--with-buffer-file-name--buffer (current-buffer)))
       (unwind-protect
           (progn
             (setq buffer-file-name ,filename)
             ,@body)
         (if (buffer-live-p checkdoc-batch--with-buffer-file-name--buffer)
             (with-current-buffer checkdoc-batch--with-buffer-file-name--buffer
               (setq buffer-file-name checkdoc-batch--with-buffer-file-name)))))))

(defun checkdoc-batch-display-buffer-other-window (buffer)
  "An internal part of checkdoc-batch.el.
Display BUFFER in another window.
For a new window `shrink-window-if-larger-than-buffer' is used to
set its size.  If BUFFER is already in another window then its
size is left alone."
  (unless (get-buffer-window buffer nil) ;; if not in a window already
    (save-current-buffer
      (save-selected-window
        (condition-case nil
            ;; emacs two args
            (switch-to-buffer-other-window buffer t) ;; no-record
          (error
           ;; xemacs one arg
           (switch-to-buffer-other-window buffer)))
        (shrink-window-if-larger-than-buffer
         (get-buffer-window buffer))))))

(defun checkdoc-batch-lock-file-p (filename)
  "An internal part of checkdoc-batch.el.
Return non-nil if FILENAME is an Emacs lock file.
A lock file is \".#foo.txt\" etc per `lock-buffer'."
  (string-match "\\`\\.#" (file-name-nondirectory filename)))

(defun checkdoc-batch-prin1-strings-list (lst)
  "An internal part of checkdoc-batch.el.
Return a string to display a list LST of strings.
This is like `prin1-to-string' but without the surrounding
parens."
  (mapconcat 'prin1-to-string lst " "))

(defun checkdoc-batch-file-truename (filename)
  "An internal part of checkdoc-batch.el.
Return the true name of FILENAME.
This is `file-truename', but nil on error in XEmacs 21."
  ;; xemacs 21.4.22 `file-truename' throws an error if
  ;; /foo/bar/quux has "foo" or "bar" as files instead of
  ;; directories, treat that as nil
  (condition-case nil
      (file-truename filename)
    (error nil)))

(defun checkdoc-batch-save-some-files (filename-list)
  "An internal part of checkdoc-batch.el.
Offer to save buffers visiting any files in FILENAME-LIST.
Saving is done with `save-some-buffers'.
Filenames are compared with `file-truename'."
  (let ((checkdoc-batch--truename-list
         (delq nil (mapcar 'checkdoc-batch-file-truename filename-list))))
    (save-some-buffers
     nil (lambda ()
           (and buffer-file-name
                (member (checkdoc-batch-file-truename buffer-file-name)
                        checkdoc-batch--truename-list))))))


;;----------------------------------------------------------------------------

(defvar checkdoc-batch-filename nil
  "An internal part of checkdoc-batch.el.
Current buffer filename during a `checkdoc-batch'.")

(defun checkdoc-batch-linenums-annotate ()
  "An internal part of checkdoc-batch.el.
Add some text properties to the buffer to record original line numbers."
  (let ((linenum 1))
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (add-text-properties (match-beginning 0) (match-end 0)
                           (list 'checkdoc-batch-linenum linenum))
      (setq linenum (1+ linenum)))))

(defun checkdoc-batch-linenum (pos)
  "An internal part of checkdoc-batch.el.
Return original line number of buffer position POS."
  (or (get-text-property pos 'checkdoc-batch-linenum)
      (let ((ppos (next-single-property-change pos 'checkdoc-batch-linenum)))
        (if ppos
            (get-text-property ppos 'checkdoc-batch-linenum)
          (setq ppos (previous-single-property-change
                      pos 'checkdoc-batch-linenum))
          (if (not ppos)
              1 ;; no properties at all, eg. empty buffer
            (1+ (or (get-text-property ppos      'checkdoc-batch-linenum)
                    (get-text-property (1- ppos) 'checkdoc-batch-linenum)
                    0)))))))

(defun checkdoc-batch-message (format-string &rest args)
  "An internal part of checkdoc-batch.el.
Output a `checkdoc-batch' message."
  ;; checkdoc-params: (format-string args)
  ;; strip trailing whitespace from lines, since the questions from checkdoc
  ;; often include that at the end of prompts
  (princ (checkdoc-batch--replace-regexp-in-string
          "[ \t]+$" ""
          (apply 'format format-string args)
          t    ;; fixedcase
          t)))
(eval-when-compile
  (put 'checkdoc-batch-message 'byte-compile-format-like t)) ;; literal

(defun checkdoc-batch-error (pos text)
  "An internal part of checkdoc-batch.el.
Output a `checkdoc-batch' error about buffer position POS."
  ;; checkdoc-params: (pos text)
  (checkdoc-batch-message "%s:%s:%s: %s\n"
                          checkdoc-batch-filename
                          (checkdoc-batch-linenum pos)
                          (checkdoc-batch-column-at-pos pos)
                          text))

;;-----------------------------------------------------------------------------

(defadvice checkdoc-autofix-ask-replace (around checkdoc-batch)
  "Temporary hack to capture message and say yes to change."
  (checkdoc-batch-error start (concat question " " replacewith))
  (delete-region start end)
  (save-excursion
    (goto-char start)
    (insert replacewith)
    (set-buffer-modified-p nil))
  (setq ad-return-value t))

(defadvice checkdoc-y-or-n-p (around checkdoc-batch)
  "Temporary hack to capture message and say yes to change."
  (checkdoc-batch-error (point)
                        (ad-get-arg 0)) ;; QUESTION
  (setq ad-return-value t))

(defadvice checkdoc-recursive-edit (around checkdoc-batch)
  "Temporary hack to capture message and suppress edit."
  (checkdoc-batch-error (point)
                        (ad-get-arg 0)) ;; MSG
  (setq ad-return-value t))

(defadvice checkdoc-create-error (around checkdoc-batch)
  "Temporary hack to capture checkdoc error messages."
  ;; START is nil for a whole-buffer thing like missing ";;; Commentary"
  ;; section
  (checkdoc-batch-error (or start (point-min))
                        text)
  (setq ad-return-value nil))

(defadvice message (around checkdoc-batch)
  "Temporary hack to capture checkdoc messages."
  (let ((format (ad-get-arg 0)))
    (if (null format)
        (setq ad-return-value nil)
      (let ((str (apply 'format (ad-get-args 0))))
        (setq ad-return-value str)
        (unless (string-match "\\(\\`Searching for \\|Done\\|Starting new Ispell\\|Ispell process killed\\)" format)
          (checkdoc-batch-message "%s\n" str))))))

(defadvice read-string (around checkdoc-batch)
  "Temporary hack to just return an empty string."
  (setq ad-return-value ""))

(defadvice completing-read (around checkdoc-batch)
  "Temporary hack to just return first completion candidate."
  (setq ad-return-value
        (or (checkdoc-batch-completion-first-candidate
             (ad-get-arg 1)) ;; COLLECTION or TABLE
            "")))

(defadvice ispell-command-loop (around checkdoc-batch)
  "Temporary hack to capture spelling error reports."
  (let ((maybe (delq nil (list miss guess))))
    (setq maybe
          (if maybe
              (concat "\n  maybe "
                      (mapconcat 'checkdoc-batch-prin1-strings-list maybe
                                 " or "))
            ""))
    (checkdoc-batch-error (point)
                          (format "spelling %S%s" word maybe)))
  nil) ;; keep word

(defconst checkdoc-batch-advised-functions
  '(checkdoc-autofix-ask-replace
    checkdoc-y-or-n-p
    checkdoc-recursive-edit
    checkdoc-create-error
    message
    read-string
    completing-read
    ispell-command-loop)
  "An internal part of checkdoc-batch.el.
List of functions (symbols) with `checkdoc-batch' advice.")

(defun checkdoc-batch-advice (action)
  "An internal part of checkdoc-batch.el.
Call ACTION on `checkdoc-batch-advised-functions'.
ACTION can be symbol `ad-enable-advice' or `ad-disable-advice'."
  (dolist (func checkdoc-batch-advised-functions)
    (funcall action func 'around 'checkdoc-batch)
    (ad-activate    func)))

;; this cleans up in emacs22 up, but since the advice is only enabled while
;; checkdoc-batch executes it doesn't matter if it's left behind in
;; emacs21/xemacs21
;;
(defun checkdoc-batch-unload-function ()
  "An internal part of checkdoc-batch.el.
Remove advice from `checkdoc-batch-advised-functions'.
This is called by `unload-feature'."
  (dolist (func checkdoc-batch-advised-functions)
    (when (ad-find-advice func 'around 'checkdoc-batch)
      (ad-remove-advice   func 'around 'checkdoc-batch)
      (ad-activate        func)))
  nil) ;; and do normal unload-feature actions too


;;-----------------------------------------------------------------------------
;; revert and stale

(defvar checkdoc-batch-originating-buffer nil)
(defvar checkdoc-batch-originating-files nil)
(defvar checkdoc-batch-stale-data nil)

(defun checkdoc-batch-stale-buffer (&optional noconfirm)
  "An internal part of checkdoc-batch.el.
Return non-nil if the checkdoc output is stale.
This is the `buffer-stale-function' for the output of a
`checkdoc-batch'.  It should be called with \"*checkdoc-batch
output*\" as the current buffer.

If the originating buffer has changed then the output is
considered stale.  (But the output buffer is flagged as modified,
which means `auto-revert-mode' and friends won't automatically
re-run the checking.)"

  (not (equal checkdoc-batch-stale-data
              (checkdoc-batch--buffer-chars-modified-tick checkdoc-batch-originating-buffer))))

(defun checkdoc-batch-stale-files (&optional noconfirm)
  "An internal part of checkdoc-batch.el.
Return non-nil if the checkdoc output is stale.
This is the `buffer-stale-function' of a `checkdoc-batch' run on
a set of files.  It should be called with \"*checkdoc-batch
output*\" as the current buffer.

If the file modtimes have changed then the checking output is
considered stale.  (But the output buffer is flagged as modified,
which means `auto-revert-mode' and friends won't automatically
re-run the checking.)"

  (not (equal checkdoc-batch-stale-data
              (mapcar (lambda (filename)
                        (nth 5 (file-attributes filename)))
                      checkdoc-batch-originating-files))))

(defun checkdoc-batch-revert-from-buffer (&optional ignore-auto noconfirm)
  "An internal part of checkdoc-batch.el.
Re-run `checkdoc-batch' to update an output buffer.
This is the `revert-buffer-function' in a \"*checkdoc-batch
output*\" buffer run from `checkdoc-batch' of a buffer.  It
re-runs that `checkdoc-batch', replacing the previous checking
output."

  (unless (buffer-live-p checkdoc-batch-originating-buffer)
    (error "Original buffer gone"))
  (save-window-excursion
    (with-current-buffer checkdoc-batch-originating-buffer
      (checkdoc-batch))))

(defun checkdoc-batch-revert-from-files (&optional ignore-auto noconfirm)
  "An internal part of checkdoc-batch.el.
Re-run `checkdoc-batch-files' to update an output buffer.
This is the `revert-buffer-function' in a \"*checkdoc-batch
output*\" buffer run from `checkdoc-batch-files' of a set of
files.  It re-runs that check on the same files, replacing the
previous checking output."

  (save-window-excursion
    (checkdoc-batch-files checkdoc-batch-originating-files)))


;;-----------------------------------------------------------------------------
;; checking proper

(defun checkdoc-batch-temp-buffer (filename)
  "An internal part of checkdoc-batch.el.
Generate a report for the current temp buffer contents."
  ;; checkdoc-params: (checkdoc-batch-filename)

  (let ((checkdoc-batch-filename filename))
    (checkdoc-batch-linenums-annotate)

    ;; must be in elisp mode for various moving things
    (emacs-lisp-mode)
    (setq buffer-read-only nil)

    (checkdoc-batch--with-buffer-file-name
     (and checkdoc-batch-filename
          (concat "/tmp/checkdoc-batch/mangled/"
                  (file-name-nondirectory checkdoc-batch-filename)))
     (let ((checkdoc-autofix-flag 'automatic))
       (unwind-protect
           (progn
             (checkdoc-batch-advice 'ad-enable-advice)
             (checkdoc))
         (checkdoc-batch-advice 'ad-disable-advice))))))

;;;###autoload
(defun checkdoc-batch ()
  "Run `checkdoc' on the current buffer, showing results in batch form.
The advantage of `checkdoc-batch' is that it's not interactive,
you just get a list of the problems to step through with
`next-error'.  Some things you might not want to fix, or not yet,
etc.  The full list of problems helps avoid confusion with the
way interactively \"n\" means next docstring, not next
problem (where refusing one change means you don't see other
things in that docstring).

The disadvantage of `checkdoc-batch' is that it's not
interactive, so you don't get the auto-fixes `checkdoc' can
apply.  Normally they're not too complicated though and can be
done manually, or do an interactive `checkdoc' when ready.

The summary output is a bit rough and the way it hooks into
checkdoc.el is very nasty and probably rather fragile.

\\[revert-buffer] in the summary buffer re-runs the checks.
`auto-revert-mode' can be used there too, though its re-running
might be a bit intrusive.

----------
See `checkdoc-current-buffer' (with prefix arg) for much the same
non-interactive checking, but without spell checking of
`checkdoc-spellcheck-documentation-flag'.

The checkdoc-batch.el home page is
URL `http://user42.tuxfamily.org/checkdoc-batch/index.html'"

  (interactive)
  (let ((orig-buffer    (current-buffer))
        (orig-filename  buffer-file-name)
        (orig-directory default-directory)
        (output-buffer  (get-buffer-create "*checkdoc-batch output*")))

    (with-current-buffer output-buffer
      (fundamental-mode)
      (setq buffer-read-only nil)
      (erase-buffer))

    (with-temp-buffer
      (let ((tempbuf (current-buffer)))
        (with-current-buffer orig-buffer
          (copy-to-buffer tempbuf (point-min) (point-max))))

      (let ((standard-output output-buffer))
        (checkdoc-batch-message
         "-*- mode: compilation; default-directory: %S -*-\n\n" orig-directory)
        (checkdoc-batch-temp-buffer (and orig-filename
                                         (file-name-nondirectory orig-filename)))
        (checkdoc-batch-message "done\n")))

    (with-current-buffer output-buffer
      (goto-char (point-min))
      (compilation-mode)
      (set-buffer-modified-p nil)
      (setq default-directory orig-directory)
      (set (make-local-variable 'checkdoc-batch-originating-buffer)
           orig-buffer)
      (set (make-local-variable 'checkdoc-batch-stale-data)
           (checkdoc-batch--buffer-chars-modified-tick orig-buffer))
      (set (make-local-variable 'revert-buffer-function)
           'checkdoc-batch-revert-from-buffer)
      (set (make-local-variable 'buffer-stale-function)
           'checkdoc-batch-stale-buffer))
    (checkdoc-batch-display-buffer-other-window output-buffer)))

;;;###autoload
(defun checkdoc-batch-files (filename-list)
  ;; checkdoc-params: (filename-list)
  "Run `checkdoc-batch' on multiple files.
Interatively a wildcard like \"*.el\" is read.

See the \"emacs-checkdoc-batch\" script (which comes with
checkdoc-batch.el) for running on multiple files in a separate
process.

\\[revert-buffer] in the summary buffer re-runs the checks.
`auto-revert-mode' can be used there too, though its re-running
might be a bit intrusive."

  (interactive
   (let* ((pattern       (read-file-name "Filename with wildcards: " nil))
          (filename-list (file-expand-wildcards pattern)))

     ;; not .#foo lockfiles
     (dolist (filename filename-list)
       (if (checkdoc-batch-lock-file-p filename)
           (setq filename-list (remove filename filename-list))))

     (unless filename-list
       (error "No files: %S" pattern))
     (checkdoc-batch-save-some-files filename-list)
     (list filename-list)))

  (let ((output-buffer (get-buffer-create "*checkdoc-batch output*")))

    (with-current-buffer output-buffer
      (fundamental-mode)
      (setq buffer-read-only nil)
      (erase-buffer))

    (let ((standard-output output-buffer))
      (checkdoc-batch-message "-*- mode: compilation -*-\n\n")
      (dolist (filename filename-list)
        (checkdoc-batch-message "%s:\n" filename)
        (with-temp-buffer
          (insert-file-contents filename)
          (checkdoc-batch-temp-buffer filename)))
      (checkdoc-batch-message "done\n"))

    (with-current-buffer output-buffer
      (goto-char (point-min))
      (compilation-mode)
      (set-buffer-modified-p nil)
      (set (make-local-variable 'checkdoc-batch-originating-files)
           filename-list)
      (set (make-local-variable 'revert-buffer-function)
           'checkdoc-batch-revert-from-files)
      (setq checkdoc-batch-stale-data
            (mapcar (lambda (filename)
                      (nth 5 (file-attributes filename)))
                    checkdoc-batch-originating-files))
      (set (make-local-variable 'buffer-stale-function)
           'checkdoc-batch-stale-files))
    (checkdoc-batch-display-buffer-other-window output-buffer)))

(defun checkdoc-batch-commandline ()
  "Run `checkdoc-batch' on files in `command-line-args-left'.
This is designed for use from Emacs startup as

    emacs -batch -f checkdoc-batch-commandline filename.el...

See the \"emacs-checkdoc-batch\" script for a handy way to run
this line."

  (dolist (filename command-line-args-left)
    (checkdoc-batch-message "%s:\n" filename)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents filename)
          (checkdoc-batch-temp-buffer filename))
      (error
       (checkdoc-batch-message "Oops, error: %S\n"
                               (error-message-string err))))))

;; LocalWords: parens docstring txt filename Filenames arg startup el
;; LocalWords: modtimes foo

(provide 'checkdoc-batch)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; checkdoc-batch.el ends here
