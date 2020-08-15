;;; ddskk-pkg.el --- define ddskk for package.el

;;; GNU Emacs Lisp Reference Manual
;;;   39 Preparing Lisp code for distribution
;;;     39.3 Multi-file Packages
;;;       One of the files in the content directory must be named name-pkg.el.

(define-package "ddskk" "17.1"
  "Simple Kana to Kanji conversion program."
  '((ccc "1.43") (cdb "20141201.754")))     ; REQUIREMENTS

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
