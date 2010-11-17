;;; skk-nicola-dvorak.el --- SKK に NICOLA (Dvorak) 入力環境を提供 -*- coding: iso-2022-jp -*-

;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;;   Itsushi Minoura <minoura@eva.hi-ho.ne.jp>

;; Author: Itsushi Minoura <minoura@eva.hi-ho.ne.jp>
;;      Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese, mule, input method

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; このファイルは NICOLA Dvorak 配列とそれを実現するためのルールを提供します。

;;; Code:


;; NICOLA Dvorak 配列

(defvar skk-kanagaki-nicola-dvorak-base-rule-list
  '(("`" nil skk-nicola-insert)
    ;;
    ("1" nil skk-nicola-insert)  ("2" nil skk-nicola-insert)
    ("3" nil skk-nicola-insert)  ("4" nil skk-nicola-insert)
    ("5" nil skk-nicola-insert)
    ;;
    ("6" nil skk-nicola-insert) ("7" nil skk-nicola-insert)
    ("8" nil skk-nicola-insert) ("9" nil skk-nicola-insert)
    ("0" nil skk-nicola-insert) ("[" nil skk-nicola-insert)
    ("]" nil skk-nicola-insert) ("\\" nil skk-nicola-insert)
    ;;
    ("'" nil skk-nicola-insert) ("," nil skk-nicola-insert)
    ("." nil skk-nicola-insert) ("p" nil skk-nicola-insert)
    ("y" nil skk-nicola-insert)
    ;;
    ("f" nil skk-nicola-insert)  ("g" nil skk-nicola-insert)
    ("c" nil skk-nicola-insert)  ("r" nil skk-nicola-insert)
    ("l" nil skk-nicola-insert)  ("/" nil skk-nicola-insert)
    ("=" nil skk-nicola-insert)
    ;;
    ("a" nil skk-nicola-insert) ("o" nil skk-nicola-insert)
    ("e" nil skk-nicola-insert) ("u" nil skk-nicola-insert)
    ("i" nil skk-nicola-insert)
    ;;
    ("d" nil skk-nicola-insert)  ("h" nil skk-nicola-insert)
    ("t" nil skk-nicola-insert)  ("l" nil skk-nicola-insert)
    ("n" nil skk-nicola-insert)  ("s" nil skk-nicola-insert)
    ("-" nil skk-nicola-insert)
    ;;
    (";" nil skk-nicola-insert) ("q" nil skk-nicola-insert)
    ("j" nil skk-nicola-insert) ("k" nil skk-nicola-insert)
    ("x" nil skk-nicola-insert)
    ;;
    ("b" nil skk-nicola-insert)  ("m" nil skk-nicola-insert)
    ("w" nil skk-nicola-insert)  ("v" nil skk-nicola-insert)
    ("z" nil skk-nicola-insert)
    ;;
    ("@" nil skk-today)
    ("$" nil skk-display-code-for-char-at-point)
    ("\"" nil skk-set-henkan-point-subr)
    ("A" nil skk-latin-mode)
    (":" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-purge-from-jisyo)
    ("J" nil skk-input-by-code-or-menu))
  "ANSI Dvorak 配列キーボードで NICOLA 入力するための基本ルール。")

(defconst skk-nicola-dvorak-keymap-display 'dummy
  "以下は、ANSI Dvorak 配列キーボードで NICOLA 入力するためのキー配列図です。

┌──┐
│ヵ￣│
│` ｀│
├──┼──┬──┬──┬──┐ ┌──┬──┬──┬──┬──┬──┬──┬──┐
│？！│／＠│〜＃│「＄│」％│ │’＾│＋＆│“＊│”（│『）│』｛│＜｝│＞｜│
│1 　│2 　│3 　│4 　│5 　│ │6 　│7 　│8 　│9 　│0 　│[ ［│] ］│\\ ￥│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┼──┼──┘
│ゐぁ│がえ│だり│ごゃ│ざれ│ │ぱよ│ぢに│ぐる│づま│ぴぇ│：〃│＝゜│
│' 。│, か│. た│P こ│Y さ│ │F ら│G ち│C く│R つ│L ，│/ 、│= ゛│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┼──┘
│ヴを│じあ│でな│げゅ│ぜも│ │ばみ│どお│ぎの│ぽょ│；っ│＿…│
│A う│O し│E て│U け│I せ│ │D は│H と│T き│N い│S ん│- −│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┘
│ゑぅ│びー│ずろ│ぶや│べぃ│ │ぷぬ│ぞゆ│ぺむ│ぼわ│ゎぉ│
│; ．│Q ひ│J す│K ふ│X へ│ │B め│M そ│W ね│V ほ│Z ・│
└──┴──┴──┴──┴──┘ └──┴──┴──┴──┴──┘

各枠の文字は以下のように書かれています。

 左下 … ASCII 文字
 右下 … 親指シフトしないで入力されるべき文字 (単独打鍵)
 右上 … 同側親指シフトにより入力されるべき文字 (straight shift)
 左上 … 反対側親指シフトにより入力されるべき文字 (cross shift)

これに基いて以下の 3 つのルールが決定されます。

 `skk-nicola-dvorak-plain-rule-list'
 `skk-nicola-dvorak-lshift-rule-list'
 `skk-nicola-dvorak-rshift-rule-list'

")

(defvar skk-nicola-dvorak-plain-rule-list
  '((?` "｀")
    ;;
    (?1 "1") (?2 "2") (?3 "3") (?4 "4") (?5 "5")
    ;;
    (?6 "6") (?7 "7") (?8 "8") (?9 "9") (?0 "0") (?\[ "［") (?\] "］")
    (?\\ "￥")
    ;;
    (?' "。") (?, ("カ" . "か")) (?. ("タ" . "た")) (?p ("コ" . "こ"))
    (?y ("サ" . "さ"))
    ;;
    (?f ("ラ" . "ら")) (?g ("チ" . "ち")) (?c ("ク" . "く")) (?r ("ツ" . "つ"))
    (?l "，") (?/ "、") (?= skk-kanagaki-dakuten)
    ;;
    (?a ("ウ" . "う")) (?o ("シ" . "し")) (?e ("テ" . "て")) (?u ("ケ" . "け"))
    (?i ("セ" . "せ"))
    ;;
    (?d ("ハ" . "は")) (?h ("ト" . "と")) (?t ("キ" . "き")) (?n ("イ" . "い"))
    (?s ("ン" . "ん")) (?- "−")
    ;;
    (?\; "．") (?q ("ヒ" . "ひ")) (?j ("ス" . "す")) (?k ("フ" . "ふ"))
    (?x ("ヘ" . "へ"))
    ;;
    (?b ("メ" . "め")) (?m ("ソ" . "そ")) (?w ("ネ" . "ね")) (?v ("ホ" . "ほ"))
    (?z "・")
    ;;
    (?\  " "))
  "単独打鍵時の入力ルール。")

(defvar skk-nicola-dvorak-lshift-rule-list
  '((?` "￣")
    ;;
    (?1 "！") (?2 "＠") (?3 "＃") (?4 "＄") (?5 "％")
    ;;
    (?6 "’") (?7 "＋") (?8 "“") (?9 "”") (?0 "『") (?\[ "』") (?\] "＜")
    (?\\ "＞")
    ;;
    (?' ("ァ" . "ぁ")) (?, ("エ" . "え")) (?. ("リ" . "り")) (?p ("ャ" . "ゃ"))
    (?y ("レ" . "れ"))
    ;;
    (?f ("パ" . "ぱ")) (?g ("ヂ" . "ぢ")) (?c ("グ" . "ぐ")) (?r ("ヅ" . "づ"))
    (?l ("ピ" . "ぴ")) (?/ "：") (?= "＝")
    ;;
    (?a ("ヲ" . "を")) (?o ("ア" . "あ")) (?e ("ナ" . "な")) (?u ("ュ" . "ゅ"))
    (?i ("モ" . "も"))
    ;;
    (?d ("バ" . "ば")) (?h ("ド" . "ど")) (?t ("ギ" . "ぎ")) (?n ("ポ" . "ぽ"))
    (?s "；") (?- "＿")
    ;;
    (?\; ("ゥ" . "ぅ")) (?q "ー") (?j ("ロ" . "ろ")) (?k ("ヤ" . "や"))
    (?x ("ィ" . "ぃ"))
    ;;
    (?b ("プ" . "ぷ")) (?m ("ゾ" . "ぞ")) (?w ("ペ" . "ぺ")) (?v ("ボ" . "ぼ"))
    (?z ("ヮ" . "ゎ"))
    ;;
    (?\  " "))
  "左親指キーが押されたときの入力ルール。")

(defvar skk-nicola-dvorak-rshift-rule-list
  '((?` "ヵ")
    ;;
    (?1 "？") (?2 "／") (?3 "〜") (?4 "「") (?5 "」")
    ;;
    (?6 "＾") (?7 "＆") (?8 "＊") (?9 "（") (?0 "）") (?\[ "｛") (?\] "｝")
    (?\\ "｜")
    ;;
    (?' ("ヰ" . "ゐ")) (?, ("ガ" . "が")) (?. ("ダ" . "だ")) (?p ("ゴ" . "ご"))
    (?y ("ザ" . "ざ"))
    ;;
    (?f ("ヨ" . "よ")) (?g ("ニ" . "に")) (?c ("ル" . "る")) (?r ("マ" . "ま"))
    (?l ("ェ" . "ぇ")) (?/ "〃") (?= skk-kanagaki-handakuten)
    ;;
    (?a "ヴ") (?o ("ジ" . "じ")) (?e ("デ" . "で")) (?u ("ゲ" . "げ"))
    (?i ("ゼ" . "ぜ"))
    ;;
    (?d ("ミ" . "み")) (?h ("オ" . "お")) (?t ("ノ" . "の")) (?n ("ョ" . "ょ"))
    (?s ("ッ" . "っ")) (?- "…")
    ;;
    (?\; ("ヱ" . "ゑ")) (?q ("ビ" . "び")) (?j ("ズ" . "ず"))
    (?k ("ブ" . "ぶ")) (?x ("ベ" . "べ"))
    ;;
    (?b ("ヌ" . "ぬ")) (?m ("ユ" . "ゆ")) (?w ("ム" . "む")) (?v ("ワ" . "わ"))
    (?z ("ォ" . "ぉ"))
    ;;
    (?\  " "))
  "右親指キーが押されたときの入力ルール。")

(require 'skk-nicola)

(provide 'skk-nicola-dvorak)

;;; skk-nicola-dvorak.el ends here
