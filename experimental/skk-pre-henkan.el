;;; skk-pre-henkan.el --- SKK 見出し語の代わりに候補を表示 -*- coding: iso-2022-jp -*-

;; Copyright (C) 2017 Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>

;; Author: Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>
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

;;    skk-dcomp-multiple-get-candidates()
;;      => skk-comp-get-candidate(first)
;;        => when first setq skk-comp-first t
;;           eval skk-completion-prog-list
;;           setq skk-comp-first nil

;; (defcustom skk-completion-prog-list '((skk-comp-by-history)
;;                                       (skk-comp-from-jisyo skk-jisyo)
;;                                       (skk-look-completion))

;;; How to use:

;;    (require 'skk-pre-henkan)

;;; Code:

(setq skk-dcomp-activate nil)
(setq skk-completion-prog-list '((skk-pre-henkan)))

;; internal variable.
(defvar skk-pre-henkan-candidates nil)

(defun skk-pre-henkan ()
  "リスト `skk-completion-prog-list' の要素として使用."
  ;; `skk-pre-henkan-candidates' の car を返す。`skk-pre-henkan-candidates' は縮む。
  ;; `skk-comp-first' が t なら、新たな `skk-pre-henkan-candidates' を作る。
  (unless (string= skk-comp-key "")
    (when skk-comp-first
      (setq skk-pre-henkan-candidates (skk-pre-henkan-make-candidates)))
    (prog1
        (car (skk-treat-strip-note-from-word (car skk-pre-henkan-candidates)))
      (setq skk-pre-henkan-candidates (cdr skk-pre-henkan-candidates)))))

(defun skk-pre-henkan-make-candidates ()
  "`skk-comp-key' をキー（先頭一致）として、候補のリストを返す."
  (let ((list-jisyo '(skk-jisyo skk-large-jisyo))
        ;; skk-comp-key は buffer-local なので with-current-buffer() 内では nil になる。
        (key (format "^%s.* /" (car (split-string skk-comp-key "*" t))))
        (i 0)
        candidates)
    (dolist (jisyo list-jisyo)
      (with-current-buffer (skk-get-jisyo-buffer (symbol-value jisyo) 'nomsg)
        (goto-char skk-okuri-nasi-min)
        (while (and (re-search-forward key nil t)
                    (< i 100))
          (setq candidates (concat candidates
                                   (buffer-substring-no-properties (point)
                                                                   (progn (end-of-line)
                                                                          (point)))))
          (setq i (1+ i)))))
    (when candidates
      (split-string candidates "/" t))))

(provide 'skk-pre-henkan)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-pre-henkan.el ends here
