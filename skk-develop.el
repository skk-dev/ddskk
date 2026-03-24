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

(eval-when-compile
  (require 'url)
  (defvar skk-exserv-list))

(defvar skk-dict-collection
  '(("SKK-JISYO.L.gz"          . euc-jp-unix)
    ("SKK-JISYO.JIS2.gz"       . euc-jp-unix)
    ("SKK-JISYO.JIS2004.gz"    . euc-jisx0213-unix)
    ("SKK-JISYO.JIS3_4.gz"     . euc-jisx0213-unix)
    ("SKK-JISYO.assoc.gz"      . euc-jp-unix)
    ("SKK-JISYO.edict.tar.gz"  . archive)
    ("SKK-JISYO.fullname.gz"   . euc-jisx0213-unix)
    ("SKK-JISYO.geo.gz"        . euc-jp-unix)
    ("SKK-JISYO.itaiji.gz"     . euc-jp-unix)
    ("SKK-JISYO.jinmei.gz"     . euc-jp-unix)
    ("SKK-JISYO.law.gz"        . euc-jp-unix)
    ("SKK-JISYO.lisp.gz"       . euc-jp-unix)
    ("SKK-JISYO.mazegaki.gz"   . euc-jp-unix)
    ("SKK-JISYO.okinawa.gz"    . euc-jp-unix)
    ("SKK-JISYO.propernoun.gz" . euc-jp-unix)
    ("SKK-JISYO.pubdic+.gz"    . euc-jp-unix)
    ("SKK-JISYO.station.gz"    . euc-jp-unix)
    ("zipcode.tar.gz"          . archive))
  "SKK 辞書のリスト。
値が 'archive なら tar で展開。
それ以外のシンボルなら、その文字コードで単一ファイルを展開。")

;; TODO:
;;   SKK-JISYO.office.zipcode . euc-jisx0213-unix
;;   SKK-JISYO.zipcode        . euc-jisx0213-unix

(defun skk-get-delete-files (dir)
  "DIR 内の辞書ファイル（アーカイブ、単一ファイル、ディレクトリ）を一掃する。"
  (pcase-dolist (`(,f . ,_conf) skk-dict-collection)
    (let ((targets (list f                                       ; 元の .gz / .tar.gz
                         (replace-regexp-in-string "\\.tar\\.gz\\'" "" f) ; 展開後(tar)
                         (replace-regexp-in-string "\\.gz\\'" "" f))))    ; 展開後(gz)

      ;; SKK-JISYO.edict.tar.gz に付随する特殊なドキュメントも対象に加える
      (when (string= f "SKK-JISYO.edict.tar.gz")
        (push "edict_doc.txt" targets))

      (dolist (file targets)
        (let ((path (expand-file-name file dir)))
          (when (file-exists-p path)
            (if (file-directory-p path)
                (delete-directory path t)
              (delete-file path))))))))

(defun skk-get-mkdir (dir)
  "DIR."
  (if (file-exists-p dir)
      (skk-get-delete-files dir)
    (make-directory dir t)))

(defun skk-get-download (dir)
  "SKK辞書をダウンロードし、`skk-dict-collection' に基づき展開・保存する。"
  (let ((base-url "https://skk-dev.github.io/dict/"))
    ;; (ファイル名 . 設定) を直接分解してループ
    (pcase-dolist (`(,f . ,conf) skk-dict-collection)
      (let* ((dest-name (if (eq conf 'archive)
                            (replace-regexp-in-string "\\.tar\\.gz\\'" "" f)
                          (replace-regexp-in-string "\\.gz\\'" "" f)))
             (dest-path (expand-file-name dest-name dir))
             (url (concat base-url f))
             (tmp-gz (expand-file-name f dir))) ; 一時保存用パス

        (unless (file-exists-p dest-path)
          (message "Downloading %s..." f)
          ;; unwind-protect でエラー時も一時ファイルを確実に消去
          (unwind-protect
              (progn
                (url-copy-file url tmp-gz t)
                (skk-unpack-entry tmp-gz dir conf))
            (when (file-exists-p tmp-gz)
              (delete-file tmp-gz))))))))

(defun skk-unpack-entry (file-path dest-dir config)
  "CONFIG に基づき 'archive か文字コードかを判定して展開する。"
  (let ((default-directory dest-dir)
        (file-name (file-name-nondirectory file-path)))
    (pcase config
      ('archive
       (if (executable-find "tar")      ; Windows 10 以降では標準
           (call-process "tar" nil nil nil "-xf" file-path)
         (error "tar コマンドが見つかりません: %s" file-name)))

      (coding
       (let ((out-file (expand-file-name (replace-regexp-in-string "\\.gz\\'" "" file-name)
                                         dest-dir))
             (coding-system-for-write coding))
         (with-temp-file out-file
           (insert-file-contents file-path)))))))

;;;###autoload
(defun skk-get (dir)
  "DIR."
  (interactive (list (read-directory-name "skk-get directory: " (expand-file-name skk-get-jisyo-directory))))
  (let ((jisyo-dir (expand-file-name dir)))
    (skk-get-mkdir jisyo-dir)
    (skk-get-download jisyo-dir))
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
