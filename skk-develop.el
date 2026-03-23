;;; skk-develop.el --- support SKK developper -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'skk)
(require 'tar-util)

(eval-when-compile
  (require 'url)
  (defvar skk-exserv-list))

(defvar skk-get-files '("SKK-JISYO.JIS2.gz"
                        "SKK-JISYO.JIS2004.gz"
                        "SKK-JISYO.JIS3_4.gz"
                        "SKK-JISYO.L.gz"
                        "SKK-JISYO.assoc.gz"
                        "SKK-JISYO.edict.tar.gz"
                        "SKK-JISYO.fullname.gz"
                        "SKK-JISYO.geo.gz"
                        "SKK-JISYO.itaiji.gz"
                        "SKK-JISYO.jinmei.gz"
                        "SKK-JISYO.law.gz"
                        "SKK-JISYO.lisp.gz"
                        "SKK-JISYO.mazegaki.gz"
                        "SKK-JISYO.okinawa.gz"
                        "SKK-JISYO.propernoun.gz"
                        "SKK-JISYO.pubdic+.gz"
                        "SKK-JISYO.station.gz"
                        "zipcode.tar.gz")
  "")

(defun skk-get-delete-files (dir)
  "DIR."
  (let ((files (cons "edict_doc.txt" skk-get-files))
        p)
    (dolist (filename files)
      (dolist (file (list filename
                          (replace-regexp-in-string ".gz" "" filename)
                          (replace-regexp-in-string ".tar.gz" "" filename)))
        (setq p (expand-file-name file dir))
        (when (file-exists-p p)
          (cond ((null (car (file-attributes p)))
                 (delete-file p))
                (t
                 (delete-directory p t))))))))

(defun skk-get-mkdir (dir)
  "DIR."
  (if (file-exists-p dir)
      (skk-get-delete-files dir)
    (make-directory dir t)))

(defun skk-get-download (dir)
  "DIR."
  (let ((url "https://skk-dev.github.io/dict/")
        fn)
    (dolist (f skk-get-files)
      (setq fn (expand-file-name f dir))
      (unless (file-exists-p fn)
        (url-copy-file (format "%s%s" url f) fn)))))

(defun skk-get-expand-tar (dir)
  "DIR."
  ;; (let (fn)
  ;;   (dolist (f (directory-files dir t "tar"))
  ;;     (setq fn (convert-standard-filename f))
  ;;     (shell-command (format "tar -xf %s -C %s && rm %s"
  ;;                 fn dir fn))))
  (let ((list '(("SKK-JISYO.edict.tar" . "SKK-JISYO.edict")
                ("zipcode.tar"         . "SKK-JISYO.zipcode")
                ("zipcode.tar"         . "SKK-JISYO.office.zipcode"))))
    (dolist (c list)
      (tar-salvage-file (expand-file-name (car c) dir)
                        (cdr c)
                        (expand-file-name (cdr c) dir)))))

;;;###autoload
(defun skk-get (dir)
  "DIR."
  (interactive (list (read-directory-name "skk-get directory: " (expand-file-name skk-get-jisyo-directory))))
  (let ((jisyo-dir (expand-file-name dir)))
    (skk-get-mkdir jisyo-dir)
    (skk-get-download jisyo-dir)
    (skk-get-expand-tar jisyo-dir))
  (message "skk-get...done")
  nil)

;;;###autoload
(add-hook
 'before-init-hook
 (lambda ()
   (eval-after-load "font-lock"
     '(set 'lisp-el-font-lock-keywords-2
           (nconc
            (list (list (concat "(\\(\\(skk-\\)?def\\("
                                ;; Function type declarations.
                                "\\(un-cond\\|subst-cond\\|advice\\|"
                                "macro-maybe\\|alias-maybe\\|un-maybe\\)\\|"
                                ;; Variable type declarations.
                                "\\(var\\|localvar\\)"
                                "\\)\\)\\>"
                                ;; Any whitespace and defined object.
                                "[ \t'\(]*"
                                "\\(\\sw+\\)?")
                        '(1 font-lock-keyword-face)
                        '(6 (cond ((match-beginning 4) font-lock-function-name-face)
                                  ((match-beginning 5) font-lock-variable-name-face))
                            nil t)))

            (list (list (concat "("
                                (regexp-opt '("skk-save-point"
                                              "skk-with-point-move"
                                              "skk-loop-for-buffers")
                                            t)
                                "\\>")
                        '(1 font-lock-keyword-face)))

            (list (list "(\\(skk-error\\)\\>"
                        '(1 font-lock-warning-face)))

            (symbol-value 'lisp-el-font-lock-keywords-2))))
   ;;
   (put 'skk-deflocalvar 'doc-string-elt 3)))

(provide 'skk-develop)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-develop.el ends here
