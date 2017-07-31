;;; ox-texinfo+.el --- add @deffn support to the Texinfo Back-End

;; Copyright (C) 2012-2015  Free Software Foundation, Inc.
;; Copyright (C) 2015-2017  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Package-Requires: ((dash "2.10.0") (org "8.3"))
;; Homepage: https://github.com/tarsius/ox-texinfo-plus
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package patches the `texinfo' exporter defined in `ox-texinfo'
;; to support `@deffn' and similar definition items.  To enable this
;; add:
;;
;;   #+TEXINFO_DEFFN: t
;;
;; to your Org file.  Then you can create definition items by writing
;; something that looks similar to how the corresponding items look in
;; Info, for example:
;;
;;   - Command: magit-section-show
;;   - Function: magit-git-exit-code &rest args
;;   - Macro: magit-insert-section &rest args
;;   - Variable: magit-display-buffer-noselect
;;   - User Option: magit-display-buffer-function
;;   - Key: q, magit-mode-bury-buffer
;;
;; As you might have guessed this package was written to be used by
;; Magit's manual.  You might want to check that out.

;; Additionally this package works around `ox-texinfo's misguided
;; handling of unnumbered vs. numbered sections.  You might have
;; something like this in your Org file:
;;
;;   #+OPTIONS: H:4 num:3 toc:2
;;
;; This works as long as you have no level-4 sections.  If you do,
;; then the use of `num:3' causes an error when exporting the `texi'
;; file to `info'.  `num' cannot actually be used for its intended
;; purpose with this exporter, because otherwise it produces invalid
;; output.  So it is useless, but to add insult to injury, it also
;; affects how links to sections look, i.e. it makes all links look
;; like: "Also see [1.2.3]" instead of the much more useful: "Also
;; see [Title of the section this links to].".

;; To fix this this package defines the `TEXINFO_CLASS' `info+', which
;; is like `info' but for levels one through three it always uses the
;; numbered variant, even when `num' calls for the unnumbered variant:
;;
;;   * level-1 => `@chapter'
;;   ** level-2 => `@section'
;;   *** level-3 => `@subsection'
;;   **** level-4 => `@unnumberedsubsubsec'
;;
;; To enable to use this you need:
;;
;;   #+TEXINFO_CLASS: info+

;; It's possible to force a level-4 section to get its own node
;; by setting its `:texinfo-node' property to `t', for example:
;;
;;   **** Risk of Reverting Automatically
;;   :PROPERTIES:
;;   :texinfo-node: t
;;   :END:

;; This package hard-codes which sections get their own node and which
;; have to share it with their parent.  Sections at levels one through
;; three get their own node, while those at level four don't.

;; This package does not disable the effect `num' has on how links are
;; formatted, you have to explicitly set `num' to `nil' if you want to
;; use descriptive links, for example:
;;
;;   #+OPTIONS: H:4 num:nil toc:2

;;; Code:

(eval-and-compile
  (require 'cl)
  (require 'dash (expand-file-name "dash"))
  (require 'ox-texinfo))

(setq org-texinfo-info-process '("makeinfo --no-split %f"))

;;; Nodes and Sections

(add-to-list 'org-texinfo-classes
             '("info+"
               "@documentencoding AUTO\n@documentlanguage AUTO"
               ("@chapter %s" . "@chapter %s")
               ("@section %s" . "@section %s")
               ("@subsection %s" . "@subsection %s")
               ("@subsubsection %s" . "@unnumberedsubsubsec %s")))

(let* ((exporter (org-export-get-backend 'texinfo))
       (options (org-export-backend-options exporter)))
  (unless (assoc :texinfo-deffn options)
    (setf (org-export-backend-options exporter)
          (cons (list :texinfo-deffn "TEXINFO_DEFFN" nil nil t)
                options))))

(defun org-texinfo-headline--nonode (fn headline contents info)
  (let ((string (funcall fn headline contents info)))
    (if (and (not (equal (org-element-property :TEXINFO-NODE headline) "t"))
             (> (org-element-property :level headline) 3))
        (let ((n (string-match-p "\n" string)))
          (substring string (1+ n)))
      string)))
(advice-add 'org-texinfo-headline :around
            'org-texinfo-headline--nonode)

(defun org-texinfo--menu-entries (scope info)
  "List direct children in SCOPE needing a menu entry.
SCOPE is a headline or a full parse tree.  INFO is a plist
holding contextual information."
  (let* ((cache (or (plist-get info :texinfo-entries-cache)
                    (plist-get (plist-put info :texinfo-entries-cache
                                          (make-hash-table :test #'eq))
                               :texinfo-entries-cache)))
         (cached-entries (gethash scope cache 'no-cache)))
    (if (not (eq cached-entries 'no-cache)) cached-entries
      (puthash
       scope
       (org-element-map (org-element-contents scope) 'headline
         (lambda (h)
           (and (or (equal (org-element-property :TEXINFO-NODE h) "t")
                    (and (not (> (org-element-property :level h) 3))
                         (not (org-not-nil (org-element-property :COPYING h)))
                         (not (org-element-property :footnote-section-p h))
                         (not (org-export-low-level-p h info))))
                h))
         info nil 'headline)
       cache))))

;;; Definition Items

(defun org-texinfo-plain-list--texinfo+ (fn plain-list contents info)
  (if (equal (plist-get info :texinfo-deffn) "t")
      (org-texinfo+plain-list plain-list contents info)
    (funcall fn plain-list contents info)))
(advice-add 'org-texinfo-plain-list :around
            'org-texinfo-plain-list--texinfo+)


(defun org-texinfo-item--texinfo+ (fn item contents info)
  (if (equal (plist-get info :texinfo-deffn) "t")
      (org-texinfo+item item contents info)
    (funcall fn item contents info)))
(advice-add 'org-texinfo-item :around
            'org-texinfo-item--texinfo+)

(defconst org-texinfo+item-regexp
  (format "\\`%s: \\(.*\\)\n"
          (regexp-opt '("deffn"        ; CATEGORY NAME ARGUMENTS
                        "Command" ; deffn Command NAME ARGUMENTS
                        "defun"   "Function"    ; NAME ARGUMENTS
                        "defmac"  "Macro"       ; NAME ARGUMENTS
                        "defspec"               ; NAME ARGUMENTS
                        "defvr"        ; CATEGORY NAME
                        "defvar"  "Variable"    ; NAME
                        "defopt"  "User Option" ; NAME
                        "Face"                  ; NAME
                        "Key"                   ; KEY COMMAND
                        ) t)))

(defun org-texinfo+get-list-type (item)
  (plist-get (cadr (plist-get (cadr item) :parent)) :previous-list-type))

(defun org-texinfo+set-list-type (item value)
  (let ((parent (plist-get (cadr item) :parent)))
    (setf (cadr parent)
          (plist-put (cadr parent) :previous-list-type value))))

(defun org-texinfo+maybe-begin-list (this type)
  (prog1 (pcase (list (org-texinfo+get-list-type this) type)
           (`(list               table) "@end itemize\n\n@table @asis\n")
           (`(,(or `nil `single) table) "@table @asis\n")
           (`(table               list) "@end table\n\n@itemize\n")
           (`(,(or `nil `single)  list) "@itemize\n"))
    (org-texinfo+set-list-type this type)))

(defun org-texinfo+maybe-end-list (this type)
  (prog1 (pcase (list (if (eq (car this) 'item)
                          (org-texinfo+get-list-type this)
                        (plist-get (cadr this) :previous-list-type))
                      type)
           (`(list  ,_) "@end itemize\n\n")
           (`(table ,_) "@end table\n\n"))
    (org-texinfo+set-list-type this type)))

(defun org-texinfo+plain-list (plain-list contents info)
  (concat contents (org-texinfo+maybe-end-list plain-list nil)))

(defun org-texinfo+item (item contents info)
  (if (let ((case-fold-search nil))
        (string-match org-texinfo+item-regexp contents))
      (pcase (match-string 1 contents)
        ("Face" (org-texinfo+face-item item contents info))
        ("Key"  (org-texinfo+key-item  item contents info))
        (_      (org-texinfo+def-item  item contents info)))
    (let* ((plain-list (plist-get (cadr item) :parent))
           (attr (org-export-read-attribute :attr_texinfo plain-list))
           (indic (or (plist-get attr :indic)
                      (plist-get info :texinfo-def-table-markup)))
           (table-type (plist-get attr :table-type))
           (type (org-element-property :type plain-list))
           (list-type (cond
                       ((eq type 'ordered) "enumerate")
                       ((eq type 'unordered) "itemize")
                       ((member table-type '("ftable" "vtable")) table-type)
                       (t "table"))))
      (concat (--when-let (org-texinfo+maybe-begin-list
                           item (if (equal type "table") 'table 'list))
                (concat (substring it 0 -1)
                        (and (eq type 'descriptive) (concat " " indic))))
              "\n@item\n"
              (--when-let (org-element-property :tag item)
                (concat " " (org-export-data it info)))
              contents))))

(defun org-texinfo+face-item (item contents info)
  (concat (org-texinfo+maybe-begin-list item 'table)
          (format "@item @w{ }--- Face: %s\n%s"
                  (match-string 2 contents)
                  (substring contents (match-end 0)))))

(defun org-texinfo+key-item (item contents info)
  (concat (org-texinfo+maybe-begin-list item 'table)
          (let ((head (match-string 2 contents))
                (body (substring contents (match-end 0))))
            (if (string-match ", " head)
                (let ((key (substring head 0 (match-beginning 0)))
                      (cmd (substring head (match-end 0))))
                  (format "\
@kindex %s
@cindex %s
@item @kbd{%s} @tie{}@tie{}@tie{}@tie{}(@code{%s})
%s" key cmd key cmd body))
              (error "Bad Key item %s" head)))))

(defun org-texinfo+def-item (item contents info)
  (let ((type (match-string 1 contents))
        (head (match-string 2 contents))
        (body (substring contents (match-end 0)))
        (prefix ""))
    (pcase type
      ("Command"
       (setq prefix (format "@cindex %s\n" head))
       (setq type "deffn")
       (setq head (concat "Command " head)))
      ("Function"    (setq type "defun"))
      ("Macro"       (setq type "defmac"))
      ("Variable"    (setq type "defvar"))
      ("User Option" (setq type "defopt")))
    (format "%s%s@%s %s\n%s@end %s\n\n"
            (or (org-texinfo+maybe-end-list item 'single) "")
            prefix type head body type)))

;;; Advices for `ox.el'.

(defun ox-texinfo+--disable-indent-tabs-mode
    (fn backend file-or-buffer
        &optional async subtreep visible-only body-only ext-plist post-process)
  (let ((saved-indent-tabs-mode (default-value 'indent-tabs-mode)))
    (when (equal backend 'texinfo)
      (setq-default indent-tabs-mode nil))
    (unwind-protect
        (funcall fn backend file-or-buffer
                 async subtreep visible-only body-only ext-plist post-process)
      (setq-default indent-tabs-mode saved-indent-tabs-mode))))

(advice-add 'org-export-to-file   :around 'ox-texinfo+--disable-indent-tabs-mode)
(advice-add 'org-export-to-buffer :around 'ox-texinfo+--disable-indent-tabs-mode)

;;; Redefine `org-texinfo-example-block'.

(defun org-texinfo-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "@example\n%s@end example"
	  (org-export-format-code-default example-block info)))

;;; ox-texinfo+.el ends soon
(provide 'ox-texinfo+)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ox-texinfo+.el ends here
