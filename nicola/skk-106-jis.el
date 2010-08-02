;;; skk-106-jis.el --- 日本語 106 キーボードによる仮名入力サポート -*- coding: iso-2022-jp -*-

;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
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

;; このファイルは、日本語 106 キーボード (旧 JIS 配列) による仮名入力のためのル
;; ールを提供します。

;; X 上などの標準では \ (backslash) を割りあてられているキーが 2 つあるため、
;; "ろ" と "ー" の両方を仕様の通りに挿入することはできません。デフォルトでは
;; \ の入力により "ろ" を挿入し、 "ー" の方は当該のキーを SHIFT と共に押すこと
;; で挿入可能としています。 XFree86 上では、これを次のような手順で仕様の通りの
;; 挙動にすることができます。

;; (例)  1. xmodmap にて以下のような設定をする。
;;
;;        % cat ~/.Xmodmap
;;        keycode 211 = underscore underscore
;;        % xmodmap ~/.Xmodmap
;;
;;       (注) 上記の例では仮想キーコード 211 となっていますが、環境によって
;;            異なるので xev など用いて自分で調べてください。
;;
;;       2. ~/.skk にて以下の設定をする。
;;
;;        (eval-after-load "skk-106-jis"
;;          '(setcar (cdr (assq ?\\ skk-106-jis-plain-rule-list)) "ー"))

;; 更に "々" が標準では入力できなくなっている点が旧 JIS 配列の仕様と異なりま
;; す。これに関しても必要な場合は上記と同様な方法で対処することになります。

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
    ("p" nil skk-nicola-insert)
    ("@" nil skk-kanagaki-dakuten)
    ("[" nil skk-kanagaki-handakuten)
    ;;
    ("a" nil skk-nicola-insert) ("s" nil skk-nicola-insert)
    ("d" nil skk-nicola-insert) ("f" nil skk-nicola-insert)
    ("g" nil skk-nicola-insert)
    ;;
    ("h" nil skk-nicola-insert)  ("j" nil skk-nicola-insert)
    ("k" nil skk-nicola-insert)  ("l" nil skk-nicola-insert)
    (";" nil skk-nicola-insert)  (":" nil skk-nicola-insert)
    ("]" nil skk-nicola-insert)
    ;;
    ("z" nil skk-nicola-insert) ("x" nil skk-nicola-insert)
    ("c" nil skk-nicola-insert) ("v" nil skk-nicola-insert)
    ("b" nil skk-nicola-insert)
    ;;
    ("n" nil skk-nicola-insert)  ("m" nil skk-nicola-insert)
    ("," nil skk-nicola-insert)  ("." nil skk-nicola-insert)
    ("/" nil skk-nicola-insert)
    ;;
    ("#" nil ("ァ" . "ぁ"))
    ("$" nil ("ゥ" . "ぅ")) ("%" nil ("ェ" . "ぇ"))  ("&" nil ("ォ" . "ぉ"))
    ("'" nil ("ャ" . "ゃ")) ("(" nil ("ュ" . "ゅ"))  (")" nil ("ョ" . "ょ"))
    ("~" nil ("ヲ" . "を")) ("=" nil "£")
    ("|" nil skk-nicola-insert) ;; これが一番の問題。
    ("Q" nil skk-set-henkan-point-subr)
    ("E" nil ("ィ" . "ぃ"))
    ("T" nil ("ヵ" . "ヵ"))
    ("Y" nil skk-nicola-insert)
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
    ("+" nil "』") ("*" nil ("ヶ" . "ヶ"))  ("}" nil "」")
    ("Z" nil skk-nicola-insert)
    ("X" nil skk-purge-from-jisyo)
    ("C" nil skk-input-by-code-or-menu)
    ("M" nil skk-kanagaki-midashi-henkan)
    ("<" nil skk-current-touten)
    (">" nil skk-current-kuten)
    ("?" nil "・")
    ("_" nil skk-nicola-insert)) "\
日本語 106 キーボードで仮名入力するための基本ルール。
この設定では \"ー\" の入力が刻印どおりにできないが、 SHIFT キーを押すことででき
る。 刻印どおりに入力できるようにするためには、仮想キーコードのレベルで制御する
必要がある。")

(defvar skk-106-jis-plain-rule-list
  '((?1 ("ヌ" . "ぬ")) (?2 ("フ" . "ふ")) (?3 ("ア" . "あ"))
    (?4 ("ウ" . "う")) (?5 ("エ" . "え")) (?6 ("オ" . "お"))
    (?7 ("ヤ" . "や")) (?8 ("ユ" . "ゆ")) (?9 ("ヨ" . "よ"))
    (?0 ("ワ" . "わ")) (?- ("ホ" . "ほ")) (?^ ("ヘ" . "へ"))
    (?q ("タ" . "た")) (?w ("テ" . "て")) (?e ("イ" . "い"))
    (?r ("ス" . "す")) (?t ("カ" . "か")) (?y ("ン" . "ん"))
    (?u ("ナ" . "な")) (?i ("ニ" . "に")) (?o ("ラ" . "ら"))
    (?p ("セ" . "せ"))
    (?a ("チ" . "ち")) (?s ("ト" . "と"))  (?d ("シ" . "し"))
    (?f ("ハ" . "は")) (?g ("キ" . "き"))  (?h ("ク" . "く"))
    (?j ("マ" . "ま")) (?k ("ノ" . "の"))  (?l ("リ" . "り"))
    (?\; ("レ" . "れ")) (?: ("ケ" . "け"))  (?\] ("ム" . "む"))
    (?z ("ツ" . "つ")) (?x ("サ" . "さ"))  (?c ("ソ" . "そ"))
    (?v ("ヒ" . "ひ")) (?b ("コ" . "こ"))  (?n ("ミ" . "み"))
    (?m ("モ" . "も")) (?\, ("ネ" . "ね"))  (?\. ("ル" . "る"))
    (?/ ("メ" . "め"))
    ;; 次の 2 つが問題。
    (?\\ ("ロ" . "ろ"))
    (?| "ー")
    ;; 上記の「ー」の問題をひきずっている。
    (?_ ("ロ" . "ろ"))
    (?Y ("ン" . "ん"))
    (?Z ("ッ" . "っ"))))

(defvar skk-106-jis-lshift-rule-list skk-106-jis-plain-rule-list)
(defvar skk-106-jis-rshift-rule-list skk-106-jis-plain-rule-list)

(require 'skk-nicola)

(provide 'skk-106-jis)

;;; skk-106-jis.el ends here
