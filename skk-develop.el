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
(require 'cl-lib)

(eval-when-compile
  (require 'url)
  (defvar skk-exserv-list))

(defvar skk-dict-collection
  '((:file "SKK-JISYO.assoc.gz"      :coding euc-jp-unix)
    (:file "SKK-JISYO.fullname.gz"   :coding euc-jisx0213-unix)
    (:file "SKK-JISYO.geo.gz"        :coding euc-jp-unix)
    (:file "SKK-JISYO.itaiji.gz"     :coding euc-jp-unix)
    (:file "SKK-JISYO.jinmei.gz"     :coding euc-jp-unix)
    (:file "SKK-JISYO.JIS2.gz"       :coding euc-jp-unix)
    (:file "SKK-JISYO.JIS2004.gz"    :coding euc-jisx0213-unix)
    (:file "SKK-JISYO.JIS3_4.gz"     :coding euc-jisx0213-unix)
    (:file "SKK-JISYO.L.gz"          :coding euc-jp-unix)
    (:file "SKK-JISYO.law.gz"        :coding euc-jp-unix)
    (:file "SKK-JISYO.lisp.gz"       :coding euc-jp-unix)
    (:file "SKK-JISYO.mazegaki.gz"   :coding euc-jp-unix)
    (:file "SKK-JISYO.okinawa.gz"    :coding euc-jp-unix)
    (:file "SKK-JISYO.propernoun.gz" :coding euc-jp-unix)
    (:file "SKK-JISYO.pubdic+.gz"    :coding euc-jp-unix)
    (:file "SKK-JISYO.station.gz"    :coding euc-jp-unix)
    (:file "SKK-JISYO.edict.tar.gz"  :type archive :targets ("SKK-JISYO.edict"))
    (:file "zipcode.tar.gz"          :type archive :targets ("SKK-JISYO.office.zipcode" "SKK-JISYO.zipcode")))
  "SKK 辞書のコレクション定義。
:file    - ダウンロードするファイル名
:coding  - 単一 .gz ファイルの場合のエンコーディング
:type    - 'archive の場合は tar 展開を指定
:targets - tar 展開するファイルのリスト")

(defalias 'skk-get--remove-suffix
  (if (fboundp 'string-remove-suffix)   ; Emacs 27.1 で導入
      #'string-remove-suffix
    (lambda (suffix string)
      (if (string-suffix-p suffix string)
          (substring string 0 (- (length string) (length suffix)))
        string))))

(defun skk-get--strip-extension (filename)
  (let ((name (file-name-nondirectory filename)))
    (skk-get--remove-suffix ".tar" (skk-get--remove-suffix ".gz" name))))

(defun skk-get--get-targets (entry)
  "辞書のコレクション定義のエントリー ENTRY から、ファイル名のリストを返す。
:targets プロパティがあればそれを使用し、なければ :file から推測する。"
  (or (plist-get entry :targets)
      (let ((file (plist-get entry :file)))
        (list (skk-get--strip-extension file)))))

(defun skk-unpack-archive (file-path dest-dir entry)
  "アーカイブ内のパス構造（./zipcode/ 等）を無視して、dest-dir に展開する。"
  (let ((targets          (plist-get entry :targets))
        (abs-file-path    (expand-file-name file-path))
        (archive-contents (process-lines "tar" "-tf" (expand-file-name file-path))))
    (dolist (target targets)
      (let ((real-path (cl-find target archive-contents
                                :test (lambda (tgt path) (string-suffix-p tgt path))))
            (out-path (expand-file-name (file-name-nondirectory target) dest-dir)))
        (if (not real-path)
            (message "Skipping: %s not found in archive." target)
          (message "Extracting %s as %s..." real-path out-path)
          (with-temp-file out-path
            (set-buffer-multibyte nil)
            (let ((exit-code (call-process "tar" nil t nil "-xOf" abs-file-path real-path)))
              (if (= exit-code 0)
                  (message "Successfully extracted: %s" out-path)
                (delete-file out-path)
                (message "Failed to extract %s" real-path)))))))))

(defun skk-unpack-gz (file-path dest-dir coding)
  "FILE-PATH (.gz) を解凍し、DEST-DIR 内に CODING で保存する。"
  (let* ((file-name (file-name-nondirectory file-path))
         (out-name  (skk-get--strip-extension file-name))
         (out-path  (expand-file-name out-name dest-dir))
         (auto-compression-mode t))
    (with-temp-buffer
      (insert-file-contents file-path)
      (let ((coding-system-for-write coding))
        (write-region (point-min) (point-max) out-path))
      (message "Unpacked %s -> %s (%s)" file-name out-name coding))))

(defun skk-get-download (dir)
  "SKK辞書をダウンロードし、展開・保存する。
既存のファイルがある場合は上書きされる。"
  (unless (file-directory-p dir)
    (make-directory dir t))
  (let ((base-url "https://skk-dev.github.io/dict/"))
    (dolist (entry skk-dict-collection)
      (let* ((file     (plist-get entry :file))
             (url      (concat base-url file))
             (tmp-path (expand-file-name file dir))
             (type     (plist-get entry :type))
             (coding   (plist-get entry :coding))
             (check-target (expand-file-name (car (skk-get--get-targets entry)) dir)))

        (unless (file-exists-p check-target)
          (message "Downloading %s..." file)
          (condition-case err
              (progn
                (url-copy-file url tmp-path t)
                (if (eq type 'archive)
                    (skk-unpack-archive tmp-path dir entry)
                  (skk-unpack-gz tmp-path dir coding))
                (when (file-exists-p tmp-path)
                  (delete-file tmp-path)))
            (error (message "Failed to process %s: %s" file (error-message-string err)))))))))

;;;###autoload
(defun skk-get (dir)
  "DIR."
  (interactive (list (read-directory-name "skk-get directory: " (expand-file-name skk-get-jisyo-directory))))
  (let ((jisyo-dir (expand-file-name dir)))
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
