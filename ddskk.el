;;; ddskk.el --- Daredevil SKK (Simple Kana to Kanji conversion program) -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988-1997 Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Copyright (C) 1999-2020 SKK Development Team

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
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

;; SKK-MODE is a mode for inputting Japanese to a current buffer which is
;; composed of four minor modes described below.

;;      +---------------+-------- skk-mode -----+--------------------+
;;      |               |                       |                    |
;;      |               |                       |                    |
;;  skk-j-mode   skk-latin-mode   skk-jisx0208-latin-mode   skk-abbrev-mode
;;                  ASCII               JISX0208 LATIN         ABBREVIATION
;; (C-j wakes up skk-j-mode)      (ZEN'KAKU EIMOJI)
;;
;; skk-j-mode-map               skk-jisx0208-latin-mode-map
;;              skk-latin-mode-map                        skk-abbrev-mode-map

;; skk-katakana: nil
;;   HIRAKANA

;; skk-katakana: t
;;   KATAKANA

;;; Code:

(require 'skk)
(provide 'ddskk)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ddskk.el ends here
