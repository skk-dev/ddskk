;;; skk-omelet-colemak.el --- SKK に omelet (Colemak) 入力環境を提供 -*- coding: iso-2022-jp -*-

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

;; このファイルは NICOLA 規格に準拠した omelet 独自 Colemak 配列とそれを実現す
;; るためのルールを提供します。

;;; Code:


;; omelet 独自 Colemak 配列

(defvar skk-kanagaki-omelet-colemak-base-rule-list
  '(("`" nil skk-nicola-insert)
    ;;
    ("1" nil skk-nicola-insert) ("2" nil skk-nicola-insert)
    ("3" nil skk-nicola-insert) ("4" nil skk-nicola-insert)
    ("5" nil skk-nicola-insert)
    ;;
    ("6" nil skk-nicola-insert) ("7" nil skk-nicola-insert)
    ("8" nil skk-nicola-insert) ("9" nil skk-nicola-insert)
    ("0" nil skk-nicola-insert) ("-" nil skk-nicola-insert)
    ("=" nil skk-nicola-insert)
    ;;
    ("q" nil skk-nicola-insert) ("w" nil skk-nicola-insert)
    ("e" nil skk-nicola-insert) ("r" nil skk-nicola-insert)
    ("t" nil skk-nicola-insert)
    ;;
    ("y" nil skk-nicola-insert) ("u" nil skk-nicola-insert)
    ("i" nil skk-nicola-insert) ("o" nil skk-nicola-insert)
    ("p" nil skk-nicola-insert) ("[" nil skk-nicola-insert)
    ("]" nil skk-nicola-insert)
    ;;
    ("a" nil skk-nicola-insert) ("s" nil skk-nicola-insert)
    ("d" nil skk-nicola-insert) ("f" nil skk-nicola-insert)
    ("g" nil skk-nicola-insert)
    ;;
    ("h" nil skk-nicola-insert) ("j" nil skk-nicola-insert)
    ("k" nil skk-nicola-insert) ("l" nil skk-nicola-insert)
    (";" nil skk-nicola-insert) ("'" nil skk-nicola-insert)
    ;;
    ("z" nil skk-nicola-insert) ("x" nil skk-nicola-insert)
    ("c" nil skk-nicola-insert) ("v" nil skk-nicola-insert)
    ("b" nil skk-nicola-insert)
    ;;
    ("n" nil skk-nicola-insert) ("m" nil skk-nicola-insert)
    ("," nil skk-nicola-insert) ("." nil skk-nicola-insert)
    ("/" nil skk-nicola-insert)
    ;;
    ("@" nil skk-today)
    ("$" nil skk-display-code-for-char-at-point)
    ("Q" nil skk-set-henkan-point-subr)
    ("A" nil skk-latin-mode)
    ("L" nil skk-jisx0208-latin-mode)
    ("Z" nil skk-jisx0208-latin-mode)
    ("X" nil skk-purge-from-jisyo)
    ("C" nil skk-input-by-code-or-menu))
  "US Colemak 配列キーボードで omelet 入力するための基本ルール。")

(defconst skk-omelet-colemak-keymap-display 'dummy "\
以下は、US Colemak 配列キーボードで omelet 入力するためのキー配列図です。

┌──┐
│ヵ￣│
│` ｀│
├──┼──┬──┬──┬──┐ ┌──┬──┬──┬──┬──┬──┬──┐
│！？│＠／│＃〜│＄「│％」│ │　［│＆］│”“│【（│】）│…『│＋』│
│1 　│2 　│3 　│4 　│5 　│ │6 　│7 　│8 　│9 　│0 　│- −│= ＝│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┼──┼──┐
│ゐぁ│がえ│だり│ごゃ│ざれ│ │ぱよ│ぢに│ぐる│づま│ぴぇ│｛〃│｝゜│｜＼│
│Q 。│W か│F た│P こ│G さ│ │J ら│L ち│U く│Y つ│; ，│[ 、│] ゛│\\ ￥│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┼──┴──┘
│ヴを│じあ│でな│げゅ│ぜも│ │ばみ│どお│ぎの│ぽょ│ヶっ│：‘│
│A う│R し│S て│T け│D せ│ │H は│N と│E き│I い│O ん│' ’│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┘
│ゑぅ│びー│ずろ│ぶや│べぃ│ │ぷぬ│ぞゆ│ぺむ│ぼわ│ゎぉ│
│Z ．│X ひ│C す│V ふ│B へ│ │K め│M そ│, ね│. ほ│/ ・│
└──┴──┴──┴──┴──┘ └──┴──┴──┴──┴──┘

各枠の文字は以下のように書かれています。

 左下 … ASCII 文字
 右下 … 親指シフトしないで入力されるべき文字 (単独打鍵)
 右上 … 同側親指シフトにより入力されるべき文字 (straight shift)
 左上 … 反対側親指シフトにより入力されるべき文字 (cross shift)

これに基いて以下の 3 つのルールが決定されます。

 `skk-omelet-colemak-plain-rule-list'
 `skk-omelet-colemak-lshift-rule-list'
 `skk-omelet-colemak-rshift-rule-list'

")

(defvar skk-omelet-colemak-plain-rule-list
  '((?\` "｀")
    ;;
    (?1 "1") (?2 "2") (?3 "3") (?4 "4") (?5 "5")
    ;;
    (?6 "6") (?7 "7") (?8 "8") (?9 "9") (?0 "0") (?- "−") (?= "＝")
    ;;
    (?q "。") (?w ("カ" . "か")) (?f ("タ" . "た")) (?p ("コ" . "こ"))
    (?g ("サ" . "さ"))
    ;;
    (?j ("ラ" . "ら")) (?l ("チ" . "ち")) (?u ("ク" . "く")) (?y ("ツ" . "つ"))
    (?\; "、") (?\[ "、") (?\] skk-kanagaki-dakuten) (?\\ "￥")
    ;;
    (?a ("ウ" . "う")) (?r ("シ" . "し")) (?s ("テ" . "て")) (?t ("ケ" . "け"))
    (?d ("セ" . "せ"))
    ;;
    (?h ("ハ" . "は")) (?n ("ト" . "と")) (?e ("キ" . "き")) (?i ("イ" . "い"))
    (?o ("ン" . "ん")) (?\' "’")
    ;;
    (?z "．") (?x ("ヒ" . "ひ")) (?c ("ス" . "す")) (?v ("フ" . "ふ"))
    (?b ("ヘ" . "へ"))
    ;;
    (?k ("メ" . "め")) (?m ("ソ" . "そ")) (?, ("ネ" . "ね")) (?. ("ホ" . "ほ"))
    (?/ "・")
    ;;
    (?\  " "))
  "単独打鍵時の入力ルール。")

(defvar skk-omelet-colemak-rshift-rule-list
  '((?` "ヵ")
    ;;
    (?1 "！") (?2 "＠") (?3 "＃") (?4 "＄") (?5 "％")
    ;;
    (?6 "［") (?7 "］") (?8 "“") (?9 "（") (?0 "）") (?- "『") (?= "』")
    ;;
    (?q ("ヰ" . "ゐ")) (?w ("ガ" . "が")) (?f ("ダ" . "だ")) (?p ("ゴ" . "ご"))
    (?g ("ザ" . "ざ"))
    ;;
    (?j ("ヨ" . "よ")) (?l ("ニ" . "に")) (?u ("ル" . "る")) (?y ("マ" . "ま"))
    (?\; ("ェ" . "ぇ")) (?\[ "〃") (?\] skk-kanagaki-handakuten) (?\\ "｜")
    ;;
    (?a "ヴ") (?r ("ジ" . "じ")) (?s ("デ" . "で")) (?t ("ゲ" . "げ"))
    (?d ("ゼ" . "ぜ"))
    ;;
    (?h ("ミ" . "み")) (?n ("オ" . "お")) (?e ("ノ" . "の")) (?i ("ョ" . "ょ"))
    (?o ("ッ" . "っ")) (?' "‘")
    ;;
    (?z ("ヱ" . "ゑ")) (?x ("ビ" . "び")) (?c ("ズ" . "ず")) (?v ("ブ" . "ぶ"))
    (?b ("ベ" . "べ"))
    ;;
    (?k ("ヌ" . "ぬ")) (?m ("ユ" . "ゆ")) (?, ("ム" . "む")) (?. ("ワ" . "わ"))
    (?/ ("ォ" . "ぉ"))
    ;;
    (?\  " "))
  "右親指キーが押されたときの入力ルール。")

(defvar skk-omelet-colemak-lshift-rule-list
  '((?\` "￣")
    ;;
    (?1 "？") (?2 "／") (?3 "〜") (?4 "「") (?5 "」")
    ;;
    (?6 "［") (?7 "＆") (?8 "”") (?9 "【") (?0 "】") (?- "…") (?= "＋")
    ;;
    (?q ("ァ" . "ぁ")) (?w ("エ" . "え")) (?f ("リ" . "り")) (?p ("ャ" . "ゃ"))
    (?g ("レ" . "れ"))
    ;;
    (?j ("パ" . "ぱ")) (?l ("ヂ" . "ぢ")) (?u ("グ" . "ぐ")) (?y ("ヅ" . "づ"))
    (?\; ("ピ" . "ぴ")) (?\[ "｛") (?\] "｝") (?\\ "＞")
    ;;
    (?a ("ヲ" . "を")) (?r ("ア" . "あ")) (?s ("ナ" . "な")) (?t ("ュ" . "ゅ"))
    (?d ("モ" . "も"))
    ;;
    (?h ("バ" . "ば")) (?n ("ド" . "ど")) (?e ("ギ" . "ぎ")) (?i ("ポ" . "ぽ"))
    (?o "；") (?' "：")
    ;;
    (?z ("ゥ" . "ぅ")) (?x "ー") (?c ("ロ" . "ろ")) (?v ("ヤ" . "や"))
    (?b ("ィ" . "ぃ"))
    ;;
    (?k ("プ" . "ぷ")) (?m ("ゾ" . "ぞ")) (?, ("ペ" . "ぺ")) (?. ("ボ" . "ぼ"))
    (?/ ("ヮ" . "ゎ"))
    ;;
    (?\  " "))
  "左親指キーが押されたときの入力ルール。")

(require 'skk-nicola)

(when skk-nicola-use-koyubi-functions
  (add-hook 'skk-mode-hook
	    #'(lambda ()
		(define-key skk-j-mode-map "'" 'skk-kanagaki-bs))))

(provide 'skk-omelet-colemak)

;;; skk-omelet-colemak.el ends here
