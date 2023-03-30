;;; skk-kanagaki-menu.el --- NICOLA-DDSKK のメニューサポート -*- coding: iso-2022-jp -*-

;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese, mule, input method

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

;; NICOLA-DDSKK のメニューを SKK 標準のメニューに追加します。

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'skk-vars))

(eval-and-compile
  (autoload 'browse-url-netscape "browse-url"))

(require 'easymenu)

(eval-and-compile
  (defvar skk-kanagaki-menu-items
    '("NICOLA DDSKK"
      ["Set Henkan point" skk-set-henkan-point-subr t]
      ["Input Prefix or Suffix" skk-kanagaki-midashi-henkan t]
      ["Start Conversion with Okuri" skk-kanagaki-set-okurigana t]
      "--"
      ["Input a Character by Code" skk-input-by-code-or-menu t]
      ["Enter SKK Abbrev Mode" skk-abbrev-mode t]
      ["\
Convert  Hiragana <=> Katakana  or  Toggle Hiragana <=> Katakana Mode"
       skk-toggle-kana t]
      ["Enter SKK JIS X 0208 Latin Mode" skk-jisx0208-latin-mode t]
      ["\
Convert to Hankaku Katakana  or  Toggle Katakana <=> Hankaku Katakana Mode"
       skk-toggle-katakana t]
      ["Enter SKK Latin Mode" skk-latin-mode t]
      ["Enter SKK Japanese Mode" skk-kakutei t]
      ["Toggle Roma <=> Kana" skk-kanagaki-toggle-rom-kana t]
      "--"
      ["Show Key Bindings" skk-kanagaki-help t]
      ["Show the Current Keymap based on NICOLA" skk-nicola-help
       (featurep 'skk-nicola)]
      ["Show NICOLA-Specific Key Bindings" skk-nicola-2nd-help
       (featurep 'skk-nicola)]
      "--"
      ["Visit NIHONGO-NYURYOKU CONSORTIUM Web Site"
       skk-nicola-visit-nicola-website (locate-library "browse-url")])))

(dolist (map (list skk-j-mode-map
                   skk-latin-mode-map
                   skk-abbrev-mode-map
                   skk-jisx0208-latin-mode-map))
  (easy-menu-add-item
   map
   '("menu-bar" "SKK")
   skk-kanagaki-menu-items))

(when (eval-when-compile (featurep 'emacs))
  (setq skk-emacs-menu-resource-ja
        (append
         skk-emacs-menu-resource-ja
         '(("Set Henkan point" . "変換開始点をセット")
           ("Input Prefix or Suffix" . "接頭辞・接尾辞を入力")
           ("Start Conversion with Okuri" . "送りあり変換を開始")
           ("Input a Character by Code" . "コード入力")
           ("Enter SKK Abbrev Mode" . "Abbrev モードに入る")
           ("\
Convert  Hiragana <=> Katakana  or  Toggle Hiragana <=> Katakana Mode"
            . "かな <=> カナ 変換  または  かなモード <=> カナモード 切換え")
           ("Enter SKK JIS X 0208 Latin Mode" . "全英モードに入る")
           ("\
Convert to Hankaku Katakana  or  Toggle Katakana <=> Hankaku Katakana Mode"
            . "\
半角カナに変換  または  全角カナモード <=> 半角カナモード 切換え")
           ("Enter SKK Latin Mode" . "アスキーモードに入る")
           ("Enter SKK Japanese Mode" . "かなモードに入る")
           ("Toggle Roma <=> Kana" . "かな入力方式  ローマ  <=> かな 切換え")
           ("Show Key Bindings" . "かな入力独自のキー定義を表示")
           ("Show the Current Keymap based on NICOLA"
            . "NICOLA キー配列を表示")
           ("Show NICOLA-Specific Key Bindings"
            . "NICOLA 特有のキー定義を表示")
           ("Visit NIHONGO-NYURYOKU CONSORTIUM Web Site"
            . "日本語入力コンソーシアムのサイトへ")))))

(provide 'skk-kanagaki-menu)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-kanagaki-menu.el ends here
