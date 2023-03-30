;;; skk-omelet-jis.el --- SKK に omelet (JIS) 入力環境を提供 -*- coding: iso-2022-jp -*-

;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;;   Itsushi Minoura <minoura@eva.hi-ho.ne.jp>

;; Author: Itsushi Minoura <minoura@eva.hi-ho.ne.jp>
;;         Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
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

;; このファイルは NICOLA 規格に準拠した omelet 独自  JIS 配列とそれを実現する
;; ためのルールを提供します。

;;; Code:


;; omelet 独自 JIS 配列

(defvar skk-kanagaki-omelet-jis-base-rule-list
  '(("1" nil skk-nicola-insert) ("2" nil skk-nicola-insert)
    ("3" nil skk-nicola-insert) ("4" nil skk-nicola-insert)
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
    ("y" nil skk-nicola-insert) ("u" nil skk-nicola-insert)
    ("i" nil skk-nicola-insert) ("o" nil skk-nicola-insert)
    ("p" nil skk-nicola-insert) ("@" nil skk-nicola-insert)
    ("[" nil skk-nicola-insert)
    ;;
    ("a" nil skk-nicola-insert) ("s" nil skk-nicola-insert)
    ("d" nil skk-nicola-insert) ("f" nil skk-nicola-insert)
    ("g" nil skk-nicola-insert)
    ;;
    ("h" nil skk-nicola-insert) ("j" nil skk-nicola-insert)
    ("k" nil skk-nicola-insert) ("l" nil skk-nicola-insert)
    (";" nil skk-nicola-insert)
    (":" nil skk-nicola-insert) ("]" nil skk-nicola-insert)
    ;;
    ("z" nil skk-nicola-insert) ("x" nil skk-nicola-insert)
    ("c" nil skk-nicola-insert) ("v" nil skk-nicola-insert)
    ("b" nil skk-nicola-insert)
    ;;
    ("n" nil skk-nicola-insert) ("m" nil skk-nicola-insert)
    ("," nil skk-nicola-insert) ("." nil skk-nicola-insert)
    ("/" nil skk-nicola-insert)
    ;;
    ("!" nil "!") ("\"" nil "\"") ("#" nil "#") ("%" nil "%")
    ("&" nil "&") ("'" nil "'") ("(" nil "(") (")" nil ")") ("~" nil "￣")
    ("=" nil "=") ("|" nil "@")
    ;;
    ("$" nil skk-display-code-for-char-at-point)
    ("Q" nil skk-set-henkan-point-subr)
    ("A" nil skk-latin-mode)
    ("S" nil skk-kanagaki-set-okurigana-no-sokuon)
    ("D" nil skk-today)
    ("L" nil skk-jisx0208-latin-mode)
    ("Z" nil skk-jisx0208-latin-mode)
    ("X" nil skk-purge-from-jisyo)
    ("C" nil skk-input-by-code-or-menu))
  "日本語 106 キーボードで omelet 入力するための基本ルール。")

(defconst skk-omelet-jis-keymap-display 'dummy "\
以下は、日本語 106 キーボードで omelet 入力するためのキー配列図です。

┌──┬──┬──┬──┬──┐ ┌──┬──┬──┬──┬──┬──┬──┬──┐
│！？│“／│”～│＃「│％」│ │　［│　］│【（│】）│｛『│｝』│＝‘│＿’│
│1 　│2 　│3 　│4 　│5 　│ │6 　│7 　│8 　│9 　│0 　│- －│^ 々│\\ ￥│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┼──┼──┘
│ゐぁ│がえ│だり│ごゃ│ざれ│ │ぱよ│ぢに│ぐる│づま│ぴぇ│×＠│〃゜│
│Q 。│W か│E た│R こ│T さ│ │Y ら│U ち│I く│O つ│P ，│@ 、│[ ゛│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┼──┤
│ヴを│じあ│でな│げゅ│ぜも│ │ばみ│どお│ぎの│ぽょ│ヶっ│＊；│＋―│
│A う│S し│D て│F け│G せ│ │H は│J と│K き│L い│; ん│: ：│] …│
├──┼──┼──┼──┼──┤ ├──┼──┼──┼──┼──┼──┼──┘
│ゑぅ│びー│ずろ│ぶや│べぃ│ │ぷぬ│ぞゆ│ぺむ│ぼわ│ゎぉ│＿’│
│Z ．│X ひ│C す│V ふ│B へ│ │N め│M そ│, ね│. ほ│/ ・│\\ ￥│
└──┴──┴──┴──┴──┘ └──┴──┴──┴──┴──┴──┘

各枠の文字は以下のように書かれています。

 左下 … ASCII 文字
 右下 … 親指シフトしないで入力されるべき文字 (単独打鍵)
 右上 … 同側親指シフトにより入力されるべき文字 (straight shift)
 左上 … 反対側親指シフトにより入力されるべき文字 (cross shift)

これに基いて以下の 3 つのルールが決定されます。

 `skk-omelet-jis-plain-rule-list'
 `skk-omelet-jis-lshift-rule-list'
 `skk-omelet-jis-rshift-rule-list'

")

(defvar skk-omelet-jis-plain-rule-list
  '((?1 "1") (?2 "2") (?3 "3") (?4 "4") (?5 "5")
    ;;
    (?6 "6") (?7 "7") (?8 "8") (?9 "9") (?0 "0") (?- "－") (?^ "々") (?\\ "￥")
    ;;
    (?q "。") (?w ("カ" . "か")) (?e ("タ" . "た")) (?r ("コ" . "こ"))
    (?t ("サ" . "さ"))
    ;;
    (?y ("ラ" . "ら")) (?u ("チ" . "ち")) (?i ("ク" . "く")) (?o ("ツ" . "つ"))
    (?p "，") (?@ "、") (?\[ skk-kanagaki-dakuten)
    ;;
    (?a ("ウ" . "う")) (?s ("シ" . "し")) (?d ("テ" . "て")) (?f ("ケ" . "け"))
    (?g ("セ" . "せ"))
    ;;
    (?h ("ハ" . "は")) (?j ("ト" . "と")) (?k ("キ" . "き")) (?l ("イ" . "い"))
    (?\; ("ン" . "ん")) (?: "：") (?\] "…")
    ;;
    (?z "．") (?x ("ヒ" . "ひ")) (?c ("ス" . "す")) (?v ("フ" . "ふ"))
    (?b ("ヘ" . "へ"))
    ;;
    (?n ("メ" . "め")) (?m ("ソ" . "そ")) (?, ("ネ" . "ね")) (?. ("ホ" . "ほ"))
    (?/ "・")
    ;;
    (?\  " "))
  "単独打鍵時の入力ルール。")

(defvar skk-omelet-jis-rshift-rule-list
  '((?1 "！") (?2 "“") (?3 "”") (?4 "＃") (?5 "％")
    ;;
    (?6 "［") (?7 "］") (?8 "（") (?9 "）") (?0 "『") (?- "』")
    (?^ ("＾" . "‘")) (?\\ ("｜" . "’"))
    ;;
    (?q ("ヰ" . "ゐ")) (?w ("ガ" . "が")) (?e ("ダ" . "だ")) (?r ("ゴ" . "ご"))
    (?t ("ザ" . "ざ"))
    ;;
    (?y ("ヨ" . "よ")) (?u ("ニ" . "に")) (?i ("ル" . "る")) (?o ("マ" . "ま"))
    (?p ("ェ" . "ぇ")) (?@ "＠") (?\[ skk-kanagaki-handakuten)
    ;;
    (?a "ヴ") (?s ("ジ" . "じ")) (?d ("デ" . "で")) (?f ("ゲ" . "げ"))
    (?g ("ゼ" . "ぜ"))
    ;;
    (?h ("ミ" . "み")) (?j ("オ" . "お")) (?k ("ノ" . "の")) (?l ("ョ" . "ょ"))
    (?\; ("ッ" . "っ")) (?: "；") (?\] "―")
    ;;
    (?z ("ヱ" . "ゑ")) (?x ("ビ" . "び")) (?c ("ズ" . "ず")) (?v ("ブ" . "ぶ"))
    (?b ("ベ" . "べ"))
    ;;
    (?n ("ヌ" . "ぬ")) (?m ("ユ" . "ゆ")) (?, ("ム" . "む")) (?. ("ワ" . "わ"))
    (?/ ("ォ" . "ぉ"))
    ;;
    (?\  " "))
  "右親指キーが押されたときの入力ルール。")

(defvar skk-omelet-jis-lshift-rule-list
  '((?1 "？") (?2 "／") (?3 "～") (?4 "「") (?5 "」")
    ;;
    (?6 "［") (?7 "］") (?8 "【") (?9 "】") (?0 "｛") (?- "｝") (?^ "＝")
    (?\\ "＿")
    ;;
    (?q ("ァ" . "ぁ")) (?w ("エ" . "え")) (?e ("リ" . "り")) (?r ("ャ" . "ゃ"))
    (?t ("レ" . "れ"))
    ;;
    (?y ("パ" . "ぱ")) (?u ("ヂ" . "ぢ")) (?i ("グ" . "ぐ")) (?o ("ヅ" . "づ"))
    (?p ("ピ" . "ぴ")) (?@ "×") (?\[ "〃")
    ;;
    (?a ("ヲ" . "を")) (?s ("ア" . "あ")) (?d ("ナ" . "な")) (?f ("ュ" . "ゅ"))
    (?g ("モ" . "も"))
    ;;
    (?h ("バ" . "ば")) (?j ("ド" . "ど")) (?k ("ギ" . "ぎ")) (?l ("ポ" . "ぽ"))
    (?\; "ヶ") (?: ("ヵ" . "＊")) (?\] "＋")
    ;;
    (?z ("ゥ" . "ぅ")) (?x "ー") (?c ("ロ" . "ろ")) (?v ("ヤ" . "や"))
    (?b ("ィ" . "ぃ"))
    ;;
    (?n ("プ" . "ぷ")) (?m ("ゾ" . "ぞ")) (?, ("ペ" . "ぺ")) (?. ("ボ" . "ぼ"))
    (?/ ("ヮ" . "ゎ"))
    ;;
    (?\  " "))
  "左親指キーが押されたときの入力ルール。")

(require 'skk-nicola)

(when skk-nicola-use-koyubi-functions
  (add-hook 'skk-mode-hook
            #'(lambda ()
                (define-key skk-j-mode-map ":" 'skk-kanagaki-bs)
                (define-key skk-j-mode-map "]" 'skk-kanagaki-esc))))

(provide 'skk-omelet-jis)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-omelet-jis.el ends here
