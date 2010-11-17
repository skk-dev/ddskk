;;; skk-oasys.el --- SKK に OASYS 風入力環境を提供 -*- coding: iso-2022-jp -*-

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

;; このファイルは 富士通のワードプロセッサ OASYS 風のキー配列とそれを実現する
;; ためのルールを提供します。

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-kanagaki-util))

(require 'skk-kanagaki)


;; OASYS 風配列

(defvar skk-kanagaki-oasys-base-rule-list
  '(("1" nil skk-nicola-insert)  ("2" nil skk-nicola-insert)
    ("3" nil skk-nicola-insert)  ("4" nil skk-nicola-insert)
    ("5" nil skk-nicola-insert)
    ;;
    ("6" nil skk-nicola-insert) ("7" nil skk-nicola-insert)
    ("8" nil skk-nicola-insert) ("9" nil skk-nicola-insert)
    ("0" nil skk-nicola-insert) ("-" nil skk-nicola-insert)
    ("^" nil skk-nicola-insert) ("\\" nil skk-nicola-insert)
    ;;
    ("q" nil skk-nicola-insert) ("w" nil skk-nicola-insert)
    ("e" nil skk-nicola-insert) ("r" nil skk-nicola-insert)
    ("t" nil skk-nicola-insert)
    ;;
    ("y" nil skk-nicola-insert)  ("u" nil skk-nicola-insert)
    ("i" nil skk-nicola-insert)  ("o" nil skk-nicola-insert)
    ("p" nil skk-nicola-insert)  ("@" nil skk-nicola-insert)
    ;;
    ("a" nil skk-nicola-insert) ("s" nil skk-nicola-insert)
    ("d" nil skk-nicola-insert) ("f" nil skk-nicola-insert)
    ("g" nil skk-nicola-insert)
    ;;
    ("h" nil skk-nicola-insert)  ("j" nil skk-nicola-insert)
    ("k" nil skk-nicola-insert)  ("l" nil skk-nicola-insert)
    (";" nil skk-nicola-insert)
    ;;
    ("z" nil skk-nicola-insert) ("x" nil skk-nicola-insert)
    ("c" nil skk-nicola-insert) ("v" nil skk-nicola-insert)
    ("b" nil skk-nicola-insert)
    ;;
    ("n" nil skk-nicola-insert)  ("m" nil skk-nicola-insert)
    ("," nil skk-nicola-insert)  ("." nil skk-nicola-insert)
    ("/" nil skk-nicola-insert)
    ;;
    ("!" nil "！") ("\"" nil "”") ("#" nil "＃") ("$" nil "＄") ("%" nil "％")
    ;;
    ("&" nil "＆") ("'" nil "’") ("(" nil "（") (")" nil "）") ("~" nil "￣")
    ("=" nil "＝")
    ;; ("~" nil "＊")
    ("|" nil "｜")
    ;;
    ("Q" nil "。") ("W" nil ("カ" . "ゑ")) ("E" nil ("タ" . "ヵ"))
    ("R" nil ("コ" . "こ")) ("T" nil ("サ" . "さ"))
    ;;
    ("Y" nil ("ラ" . "ら")) ("U" nil ("チ" . "ち")) ("I" nil ("ク" . "く"))
    ("O" nil ("ツ" . "つ")) ("P" nil "，") ("`" nil "、")
    ;;
    ("A" nil ("ウ" . "う")) ("S" nil ("シ" . "し")) ("D" nil ("テ" . "て"))
    ("F" nil ("ケ" . "ヶ")) ("G" nil ("セ" . "せ"))
    ;;
    ("H" nil ("パ" . "ぱ")) ("J" nil ("ト" . "と")) ("K" nil ("キ" . "き"))
    ("L" nil ("イ" . "ゐ")) ("+" nil ("ン" . "ん"))
    ;;
    ("Z" nil "．") ("X" nil ("ピ" . "ぴ")) ("C" nil ("ス" . "す"))
    ("V" nil ("プ" . "ぷ")) ("B" nil ("ペ" . "ぺ"))
    ;;
    ("N" nil ("メ" . "め")) ("M" nil ("ソ" . "そ")) ("<" nil ("ネ" . "ね"))
    (">" nil ("ポ" . "ぽ")) ("?" nil "・"))
  "日本語 106 キーボードで OASYS 風入力をするための基本ルール。")

(defconst skk-oasys-keymap-display 'dummy
  "以下は、日本語 106 キーボードで OASYS 風入力をするためのキー配列図です。

┌──┬──┬──┬──┬──┐ ┌──┬──┬──┬──┬──┬──┬──┐
│　？│　／│　〜│　「│　」│ │　［│　］│　（│　）│　『│　』│´　│
│1 １│2 ２│3 ３│4 ４│5 ５│ │6 ６│7 ７│8 ８│9 ９│0 ０│- −│^ ：│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┼──┘
│゜ぁ│がえ│だり│ごゃ│ざれ│ │ぱよ│ぢに│ぐる│づま│ぴぇ│　　│
│Q 。│W か│E た│R こ│T さ│ │Y ら│U ち│I く│O つ│P ，│@ 、│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┼──┐
│ヴを│じあ│でな│げゅ│ぜも│ │ばみ│どお│ぎの│ぽょ│っっ│後　│取　│
│A う│S し│D て│F け│G せ│ │H は│J と│K き│L い│; ん│: 退│] 消│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┼──┘
│．ぅ│びー│ずろ│ぶや│べぃ│ │ぷぬ│ぞゆ│ぺむ│ぼわ│゛ぉ│＼　│
│Z ．│X ひ│C す│V ふ│B へ│ │N め│M そ│, ね│. ほ│/ ・│\\ ￥│
└──┴──┴──┴──┴──┘ └──┴──┴──┴──┴──┴──┘

各枠の文字は以下のように書かれています。

 左下 … ASCII 文字
 右下 … 親指シフトしないで入力されるべき文字 (単独打鍵)
 右上 … 同側親指シフトにより入力されるべき文字 (straight shift)
 左上 … 反対側親指シフトにより入力されるべき文字 (cross shift)

\(但し下から 2 列めの右の 2 つのキーはそれぞれ後退キー、取消キーであることを
意味します。)

これに基いて以下の 3 つのルールが決定されます。

 `skk-oasys-plain-rule-list'
 `skk-oasys-lshift-rule-list'
 `skk-oasys-rshift-rule-list'

")

(defvar skk-oasys-plain-rule-list
  '((?1 "１") (?2 "２") (?3 "３") (?4 "４") (?5 "５")
    ;;
    (?6 "６") (?7 "７") (?8 "８") (?9 "９") (?0 "０") (?- "−")
    (?^ ("＊" ."：")) (?\\ "￥")
    ;;
    (?q "。") (?w ("ヱ" . "か")) (?e ("ヵ" . "た")) (?r ("コ" . "こ"))
    (?t ("サ" . "さ"))
    ;;
    (?y ("ラ" . "ら")) (?u ("チ" . "ち")) (?i ("ク" . "く")) (?o ("ツ" . "つ"))
    (?p "，") (?@ "、")
    ;;
    (?a ("ウ" . "う")) (?s ("シ" . "し")) (?d ("テ" . "て")) (?f ("ヶ" . "け"))
    (?g ("セ" . "せ"))
    ;;
    (?h ("ハ" . "は")) (?j ("ト" . "と")) (?k ("キ" . "き")) (?l ("ヰ" . "い"))
    (?\; ("ン" . "ん"))
    ;;
    (?z "．") (?x ("ヒ" . "ひ")) (?c ("ス" . "す")) (?v ("フ" . "ふ"))
    (?b ("ヘ" . "へ"))
    ;;
    (?n ("メ" . "め")) (?m ("ソ" . "そ")) (?, ("ネ" . "ね")) (?. ("ホ" . "ほ"))
    (?/ "・"))
  "単独打鍵時の入力ルール。")

(defvar skk-oasys-rshift-rule-list
  '((?1 "？") (?2 "／") (?3 "〜") (?4 "「") (?5 "」")
    ;;
    (?6 "［") (?7 "］") (?8 "（") (?9 "）") (?0 "『") (?- "』") (?^ "　")
    (?\\ "　")
    ;;
    (?q ("ァ" . skk-kanagaki-handakuten))
    (?w ("ガ" . "が")) (?e ("ダ" . "だ")) (?r ("ゴ" . "ご"))
    (?t ("ザ" . "ざ"))
    ;;
    (?y ("ヨ" . "よ")) (?u ("ニ" . "に")) (?i ("ル" . "る")) (?o ("マ" . "ま"))
    (?p ("ェ" . "ぇ")) (?@ ("、" . "　"))
    ;;
    (?a "ヴ") (?s ("ジ" . "じ")) (?d ("デ" . "で")) (?f ("ゲ" . "げ"))
    (?g ("ゼ" . "ぜ"))
    ;;
    (?h ("ミ" . "み")) (?j ("オ" . "お")) (?k ("ノ" . "の")) (?l ("ョ" . "ょ"))
    (?\; ("ッ" . "っ"))
    ;;
    (?z ("ゥ" . "．")) (?x ("ビ" . "び")) (?c ("ズ" . "ず")) (?v ("ブ" . "ぶ"))
    (?b ("ベ" . "べ"))
    ;;
    (?n ("ヌ" . "ぬ")) (?m ("ユ" . "ゆ")) (?, ("ム" . "む")) (?. ("ワ" . "わ"))
    (?/ ("・" . "ぉ"))
    ;;
    (?\ " "))
  "右親指キーが押されたときの入力ルール。")

(defvar skk-oasys-lshift-rule-list
  '((?1 "？") (?2 "／") (?3 "〜") (?4 "「") (?5 "」")
    ;;
    (?6 "［") (?7 "］") (?8 "（") (?9 "）") (?0 "『") (?- "』")
    (?^ ("　" . "´")) (?\\ ("　" . "＼"))
    ;;
    (?q ("ァ" . "ぁ")) (?w ("エ" . "え")) (?e ("リ" . "り")) (?r ("ャ" . "ゃ"))
    (?t ("レ" . "れ"))
    ;;
    (?y ("パ" . "ぱ")) (?u ("ヂ" . "ぢ")) (?i ("グ" . "ぐ")) (?o ("ツ" . "づ"))
    (?p ("ピ" . "ぴ")) (?@ "　")
    ;;
    (?a ("ヲ" . "を")) (?s ("ア" . "あ")) (?d ("ナ" . "な")) (?f ("ュ" . "ゅ"))
    (?g ("モ" . "も"))
    ;;
    (?h ("バ" . "ば")) (?j ("ド" . "ど")) (?k ("ギ" . "ぎ")) (?l ("ポ" . "ぽ"))
    (?\; ("ッ" . "っ"))
    ;;
    (?z ("ゥ" . "ぅ")) (?x "ー") (?c ("ロ" . "ろ")) (?v ("ヤ" . "や"))
    (?b ("ィ" . "ぃ"))
    ;;
    (?n ("プ" . "ぷ")) (?m ("ゾ" . "ぞ")) (?, ("ペ" . "ぺ")) (?. ("ボ" . "ぼ"))
    (?/ ("　" . skk-kanagaki-dakuten))
    ;;
    (?\ " "))
  "左親指キーが押されたときの入力ルール。")

(require 'skk-nicola)

(when skk-nicola-use-koyubi-functions
  (define-key skk-j-mode-map ":" 'skk-kanagaki-bs)
  (define-key skk-j-mode-map "]" 'skk-kanagaki-esc))

(provide 'skk-oasys)

;;; skk-oasys.el ends here
