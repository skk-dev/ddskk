;;; skk-106-jis.el --- 日本語 106 キーボードによる仮名入力サポート
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; このファイルは、日本語 106 キーボード (旧 JIS 配列) による仮名入力のためのル
;; ールを提供します。

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-macs)
  (require 'skk-vars))

(eval-when-compile
  (require 'skk-kanagaki-util))

(require 'skk-kanagaki)


;; 日本語 106 キーボード (旧 JIS 配列) のルール

(defvar skk-kanagaki-106-jis-base-rule-list
  '(("1" nil ("ヌ" . "ぬ")) ("2" nil ("フ" . "ふ")) ("3" nil ("ア" . "あ"))
    ("4" nil ("ウ" . "う")) ("5" nil ("エ" . "え")) ("6" nil ("オ" . "お"))
    ("7" nil ("ヤ" . "や")) ("8" nil ("ユ" . "ゆ")) ("9" nil ("ヨ" . "よ"))
    ("0" nil ("ワ" . "わ")) ("-" nil ("ホ" . "ほ")) ("^" nil ("ヘ" . "へ"))
    ("q" nil ("タ" . "た")) ("w" nil ("テ" . "て")) ("e" nil ("イ" . "い"))
    ("r" nil ("ス" . "す")) ("t" nil ("カ" . "か")) ("y" nil ("ン" . "ん"))
    ("u" nil ("ナ" . "な")) ("i" nil ("ニ" . "に")) ("o" nil ("ラ" . "ら"))
    ("p" nil ("セ" . "せ"))
    ("@" nil skk-kanagaki-dakuten)
    ("[" nil skk-kanagaki-handakuten)
    ("a" nil ("チ" . "ち")) ("s" nil ("ト" . "と"))  ("d" nil ("シ" . "し"))
    ("f" nil ("ハ" . "は")) ("g" nil ("キ" . "き"))  ("h" nil ("ク" . "く"))
    ("j" nil ("マ" . "ま")) ("k" nil ("ノ" . "の"))  ("l" nil ("リ" . "り"))
    (";" nil ("レ" . "れ")) (":" nil ("ケ" . "け"))  ("]" nil ("ム" . "む"))
    ("z" nil ("ツ" . "つ")) ("x" nil ("サ" . "さ"))  ("c" nil ("ソ" . "そ"))
    ("v" nil ("ヒ" . "ひ")) ("b" nil ("コ" . "こ"))  ("n" nil ("ミ" . "み"))
    ("m" nil ("モ" . "も")) ("," nil ("ネ" . "ね"))  ("." nil ("ル" . "る"))
    ("/" nil ("メ" . "め")) ("\\" nil ("ロ" . "ろ"))
    ;;
    ("#" nil ("ァ" . "ぁ"))
    ("$" nil ("ゥ" . "ぅ")) ("%" nil ("ェ" . "ぇ"))  ("&" nil ("ォ" . "ぉ"))
    ("'" nil ("ャ" . "ゃ")) ("(" nil ("ュ" . "ゅ"))  (")" nil ("ョ" . "ょ"))
    ("~" nil ("ヲ" . "を")) ("=" nil "£")
    ("|" nil "ー") ;; これが一番の問題。
    ("Q" nil skk-set-henkan-point-subr)
    ("E" nil ("ィ" . "ぃ"))
    ("T" nil ("ヵ" . "ヵ"))  ("Y" nil ("ン" . "ん"))
    ("P" nil "『")
    ("`" nil "¢")
    ("{" nil "「")
    ("A" nil skk-latin-mode)
    ("S" nil skk-kanagaki-set-okurigana-no-sokuon)
    ("D" nil skk-today)
    ("F" nil skk-display-code-for-char-at-point)
    ("J" nil skk-abbrev-mdoe)
    ("K" nil skk-toggle-kana)
    ("L" nil skk-jisx0208-latin-mode)
    ("+" nil "』")          ("*" nil ("ヶ" . "ヶ"))  ("}" nil "」")
    ("Z" nil ("ッ" . "っ"))
    ("X" nil skk-purge-from-jisyo)
    ("C" nil skk-input-by-code-or-menu)
    ("M" nil skk-kanagaki-midashi-henkan)
    ("<" nil skk-current-touten)
    (">" nil skk-current-kuten)
    ("?" nil "・")
    ("_" nil ("ロ" . "ろ")) ;; 上記の「ー」の問題をひきずっている。
    ;;
    ) "\
日本語 106 キーボードで仮名入力するための基本ルール。
この設定では \"ー\" の入力が刻印どおりにできないが、 SHIFT キーを押すことででき
る。 刻印どおりに入力できるようにするためには、仮想キーコードのレベルで制御する
必要がある。")

;;

(case skk-kanagaki-jidou-keymap-kakikae-service
  ;;
  (106-jis
   (skk-kanagaki-call-xmodmap
       "keycode 123 = underscore underscore\n"
     (setq skk-kanagaki-rule-list
	   (nconc skk-kanagaki-rule-list
		  '(("\\" nil "ー"))))))
  ;;
  (106-jis-kodawari
   (skk-kanagaki-call-xmodmap
       "keycode 123 = quotedbl underscore
keycode 19 = 0 exclam
keycode 21 = asciicircum asciitilde
keycode 34 = at grave\n")
   (setq skk-kanagaki-rule-list
	 (nconc skk-kanagaki-rule-list
		'(("~" nil "々")
		  ("\\" nil "ー")
		  ("|" nil "¬")
		  ("!" nil ("ヲ" . "を"))
		  ("\"" nil ("ロ" . "ろ"))
		  ("_" nil "｜"))))))

;;

(require 'product)
(product-provide (provide 'skk-106-jis) (require 'skk-version))

;; skk-106-jis.el ends here
