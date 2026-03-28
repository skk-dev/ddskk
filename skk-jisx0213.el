;;; skk-jisx0213.el --- SKK 用 JISX0213 文字コード関連プログラム -*- lexical-binding: t; coding: iso-2022-jp -*-

;; Copyright (C) 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
;; Keywords: japanese, mule, input method
;; Created: Sep. 30, 2000.

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

(defconst skk-jisx0213-target-charsets
  ;; 実行時に存在する文字集合のリスト
  (cl-remove-if-not #'charsetp
                    '(japanese-jisx0213-a      ;2000年版 第1面追加分
                      japanese-jisx0213-2      ;2000年版 第2面（面全部が追加分）
                      japanese-jisx0213.2004-1 ;2004年版 改定・追加分
                      japanese-jisx0213-b      ;補助漢字との重複考慮分
                      )))

(defconst skk-jisx0213--filter-regexp
  (rx-to-string `(any ,@skk-jisx0213-target-charsets)))

(defsubst skk-jisx0213--match-p (candidate)
  "CANDIDATE が JIS X 0213 文字を含んでいれば非 nil を返す。"
  (string-match-p skk-jisx0213--filter-regexp
                  (if (consp candidate)
                      (cdr candidate)
                    candidate)))

;;;###autoload
(defun skk-jisx0213-henkan-list-filter ()
  "JIS X 0213 文字を含む候補を `skk-henkan-list' から削除する。
現在選択中のインデックス（`skk-henkan-count'）以降の候補が対象。"
  (let ((unprocessed (nthcdr skk-henkan-count skk-henkan-list)))
    (when unprocessed
      (let ((filtered (cl-delete-if #'skk-jisx0213--match-p unprocessed)))
        (if (zerop skk-henkan-count)
            (setq skk-henkan-list filtered)
          (setcdr (nthcdr (1- skk-henkan-count) skk-henkan-list) filtered))))))

(provide 'skk-jisx0213)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-jisx0213.el ends here
