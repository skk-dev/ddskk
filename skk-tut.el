; SKK tutorial for SKK version 10.46 and later versions
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999, 2000
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-tut.el,v 1.11 2000/09/21 10:49:43 akiho Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/09/21 10:49:43 $

;; This file is part of SKK.

;; SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;; Code:
(require 'skk)

;;;###autoload
(defgroup skk-tut nil "SKK tutorial conversion related customization."
  :prefix "skk-tut-"
  :group 'skk )

;; User variables.  prefix should be `skk-tut-'.
(defcustom skk-tut-file 
  (static-cond ((eq skk-emacs-type 'xemacs) (locate-data-file "SKK.tut"))
	       (t "/usr/local/share/skk/SKK.tut"))
  "*SKK チュートリアルのファイル名。
The English version is SKK.tut.E."
  :type 'file
  :group 'skk-tut )

(defvar skk-tut-file-alist
  (` (("Japanese" . (, skk-tut-file))
      ("English" . (, (concat skk-tut-file ".E"))) ))
  "*Alist of `(LANGUAGE . TUTORIAL-FILE)' pairs." )

(defcustom skk-tut-use-face t
  "*Non-nil であれば、チュートリアルで face を利用した表示を行なう。" 
  :type 'boolean
  :group 'skk-tut )

(defface skk-tut-section-face
  '((((class color) (background light))
     (:foreground "yellow" :background "dodgerblue"))
    (((class color) (background dark))
     (:foreground "yellow" :background "slateblue"))
    (((class grayscale)) (:bold t) (:italic t)) )
  "*チュートリアル中のセクションの表示部分の face。" 
  :group 'skk-faces )

(defface skk-tut-do-it-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (((class grayscale)) (:bold t)) )
  "*チュートリアル中の指示項目の表示部分の face。"
  :group 'skk-faces )

(defface skk-tut-question-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (((class grayscale)) (:underline t)) )
  "*チュートリアル中の問題の表示部分の face。"
  :group 'skk-faces )

(defface skk-tut-key-bind-face
  '((((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "OrangeRed"))
    (((class grayscale)) (:bold t)) )
  "*チュートリアル中のキーバインドの表示部分の face。"
  :group 'skk-faces )

(defface skk-tut-hint-face
  '((((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (((class grayscale)) (:italic t)) )
  "*チュートリアル中のヒントの表示部分の face。
現在のところ、SKK.tut.E でしか使用されていない。"
  :group 'skk-faces )

;; internal variables and constants.
;; prefix should be `skktut-'.
(defvar skk-tut-section-face 'skk-tut-section-face)
(defvar skk-tut-do-it-face 'skk-tut-do-it-face)
(defvar skk-tut-question-face 'skk-tut-question-face)
(defvar skk-tut-key-bind-face 'skk-tut-key-bind-face)
(defvar skk-tut-hint-face 'skk-tut-hint-face)

(defconst skktut-adviced-alist
  '((skk-abbrev-mode . before) (skk-insert . before)
    (skk-kakutei . before) (skk-mode . before)
    (kill-buffer . around) (other-frame . before)
    (save-buffers-kill-emacs . around)
    ;;(select-frame . before)
    )
  "SKK チュートリアルで advice が付けられる関数と advice class のエーリスト。" )

(defconst skktut-question-numbers 37 "SKK チュートリアルの問題数。")

(defconst skktut-tut-jisyo "~/skk-tut-jisyo"
  "SKK チュートリアル用のダミー辞書。" )

(defconst skktut-init-variables-alist
  '((skk-abbrev-cursor-color . "royalblue")
    (skk-abbrev-mode-string . " aあ")
    (skk-allow-spaces-newlines-and-tabs . t)
    (skk-auto-fill-mode-hook . nil)
    (skk-auto-insert-paren . nil)
    (skk-auto-okuri-process . nil)
    (skk-auto-start-henkan . nil)
    (skk-byte-compile-init-file . nil)
    (skk-comp-load-hook . nil)
    (skk-compare-jisyo-size-when-saving . nil)
    (skk-completion-function . 'skk-completion-original)
    (skk-convert-okurigana-into-katakana . nil)
    (skk-count-jisyo-candidates-function . 'skk-count-jisyo-candidates-original)
    (skk-count-private-jisyo-candidates-exactly . nil)
    (skk-dabbrev-like-completion . nil)
    (skk-date-ad . 1)
    (skk-default-cursor-color . (if (eq skk-emacs-type 'xemacs)
				    (frame-property (selected-frame) 'cursor-color)
				  (cdr (assq 'cursor-color (frame-parameters (selected-frame)))) ))
    (skk-delete-implies-kakutei . t)
    (skk-delete-okuri-when-quit . nil)
    (skk-downcase-alist . nil)
    (skk-echo . t)
    (skk-egg-like-newline . nil)
    (skk-gadget-load-hook . nil)
    (skk-henkan-face . 'highlight)
    (skk-henkan-okuri-strictly . nil)
    (skk-henkan-overlay-priority . 600)
    (skk-henkan-show-candidates-keys . '(?a ?s ?d ?f ?j ?k ?l))
    (skk-henkan-strict-okuri-precedence . nil)
    (skk-hiragana-mode-string . " かな")
    (skk-init-file . "")
    (skk-input-by-code-menu-keys1 . '(?a ?s ?d ?f ?g ?h ?q ?w ?e ?r ?t ?y))
    (skk-input-by-code-menu-keys2 . '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u))
    (skk-japanese-message-and-error . nil)
    (skk-jisx0208-latin-cursor-color . "gold")
    (skk-jisx0208-latin-mode-string . " 全英")
    (skk-jisx0208-latin-vector . skk-default-jisx0208-latin-vector)
    (skk-jisyo . "~/skk-tut-jisyo")
    (skk-jisyo-save-count . nil)
    (skk-kakutei-early . t)
    (skk-kakutei-key . "\C-j")
    (skk-kana-input-search-function . (function
				       (lambda ()
					 (save-match-data
					   (and (string-match
						 "^h\\([bcdfghjklmnpqrstvwxz]\\)$"
						 skk-prefix )
						(member (char-to-string (preceding-char))
							'("お" "オ") )
						(cons '("オ" . "お") (match-string 1 skk-prefix)) )))))
    (skk-katakana-mode-string . " カナ")
    (skk-kcode-load-hook . nil)
    (skk-keep-record . nil)
    (skk-kuten-touten-alist . '((jp . ("。" . "、"))))
    (skk-kutouten-type . 'jp)
    (skk-latin-cursor-color . "ivory4")
    (skk-latin-mode-string . " SKK")
    (skk-load-hook . nil)
    (skk-mode-hook . nil)
    (skk-next-completion-key . ".")
    (skk-number-style . 1)
    (skk-okuri-char-alist . nil)
    (skk-previous-completion-function . 'skk-previous-completion-original)
    (skk-previous-completion-key . ",")
    (skk-process-okuri-early . nil)
    (skk-public-jisyo-has-entry-p-function . 'skk-public-jisyo-has-entry-p-original)
    (skk-report-set-cursor-error . t)
    (skk-rom-kana-base-rule-list .
				 '(("a" nil ("ア" . "あ")) ("bb" "b" ("ッ" . "っ"))
				   ("ba" nil ("バ" . "ば")) ("be" nil ("ベ" . "べ"))
				   ("bi" nil ("ビ" . "び")) ("bo" nil ("ボ" . "ぼ"))
				   ("bu" nil ("ブ" . "ぶ")) ("bya" nil ("ビャ" . "びゃ"))
				   ("bye" nil ("ビェ" . "びぇ")) ("byi" nil ("ビィ" . "びぃ"))
				   ("byo" nil ("ビョ" . "びょ")) ("byu" nil ("ビュ" . "びゅ"))
				   ("cc" "c" ("ッ" . "っ")) ("cha" nil ("チャ" . "ちゃ"))
				   ("che" nil ("チェ" . "ちぇ")) ("chi" nil ("チ" . "ち"))
				   ("cho" nil ("チョ" . "ちょ")) ("chu" nil ("チュ" . "ちゅ"))
				   ("cya" nil ("チャ" . "ちゃ")) ("cye" nil ("チェ" . "ちぇ"))
				   ("cyi" nil ("チィ" . "ちぃ")) ("cyo" nil ("チョ" . "ちょ"))
				   ("cyu" nil ("チュ" . "ちゅ")) ("dd" "d" ("ッ" . "っ"))
				   ("da" nil ("ダ" . "だ")) ("de" nil ("デ" . "で"))
				   ("dha" nil ("デャ" . "でゃ")) ("dhe" nil ("デェ" . "でぇ"))
				   ("dhi" nil ("ディ" . "でぃ")) ("dho" nil ("デョ" . "でょ"))
				   ("dhu" nil ("デュ" . "でゅ")) ("di" nil ("ヂ" . "ぢ"))
				   ("do" nil ("ド" . "ど")) ("du" nil ("ヅ" . "づ"))
				   ("dya" nil ("ヂャ" . "ぢゃ")) ("dye" nil ("ヂェ" . "ぢぇ"))
				   ("dyi" nil ("ヂィ" . "ぢぃ")) ("dyo" nil ("ヂョ" . "ぢょ"))
				   ("dyu" nil ("ヂュ" . "ぢゅ")) ("e" nil ("エ" . "え"))
				   ("ff" "f" ("ッ" . "っ")) ("fa" nil ("ファ" . "ふぁ"))
				   ("fe" nil ("フェ" . "ふぇ")) ("fi" nil ("フィ" . "ふぃ"))
				   ("fo" nil ("フォ" . "ふぉ")) ("fu" nil ("フ" . "ふ"))
				   ("fya" nil ("フャ" . "ふゃ")) ("fye" nil ("フェ" . "ふぇ"))
				   ("fyi" nil ("フィ" . "ふぃ")) ("fyo" nil ("フョ" . "ふょ"))
				   ("fyu" nil ("フュ" . "ふゅ")) ("gg" "g" ("ッ" . "っ"))
				   ("ga" nil ("ガ" . "が")) ("ge" nil ("ゲ" . "げ"))
				   ("gi" nil ("ギ" . "ぎ")) ("go" nil ("ゴ" . "ご"))
				   ("gu" nil ("グ" . "ぐ")) ("gya" nil ("ギャ" . "ぎゃ"))
				   ("gye" nil ("ギェ" . "ぎぇ")) ("gyi" nil ("ギィ" . "ぎぃ"))
				   ("gyo" nil ("ギョ" . "ぎょ")) ("gyu" nil ("ギュ" . "ぎゅ"))
				   ;;("h" "" ("オ" . "お"))
				   ("ha" nil ("ハ" . "は")) ("he" nil ("ヘ" . "へ"))
				   ("hi" nil ("ヒ" . "ひ")) ("ho" nil ("ホ" . "ほ"))
				   ("hu" nil ("フ" . "ふ")) ("hya" nil ("ヒャ" . "ひゃ"))
				   ("hye" nil ("ヒェ" . "ひぇ")) ("hyi" nil ("ヒィ" . "ひぃ"))
				   ("hyo" nil ("ヒョ" . "ひょ")) ("hyu" nil ("ヒュ" . "ひゅ"))
				   ("i" nil ("イ" . "い")) ("jj" "j" ("ッ" . "っ"))
				   ("ja" nil ("ジャ" . "じゃ")) ("je" nil ("ジェ" . "じぇ"))
				   ("ji" nil ("ジ" . "じ")) ("jo" nil ("ジョ" . "じょ"))
				   ("ju" nil ("ジュ" . "じゅ")) ("jya" nil ("ジャ" . "じゃ"))
				   ("jye" nil ("ジェ" . "じぇ")) ("jyi" nil ("ジィ" . "じぃ"))
				   ("jyo" nil ("ジョ" . "じょ")) ("jyu" nil ("ジュ" . "じゅ"))
				   ("kk" "k" ("ッ" . "っ")) ("ka" nil ("カ" . "か"))
				   ("ke" nil ("ケ" . "け")) ("ki" nil ("キ" . "き"))
				   ("ko" nil ("コ" . "こ")) ("ku" nil ("ク" . "く"))
				   ("kya" nil ("キャ" . "きゃ")) ("kye" nil ("キェ" . "きぇ"))
				   ("kyi" nil ("キィ" . "きぃ")) ("kyo" nil ("キョ" . "きょ"))
				   ("kyu" nil ("キュ" . "きゅ")) ("mm" "c" ("ッ" . "っ"))
				   ("ma" nil ("マ" . "ま")) ("me" nil ("メ" . "め"))
				   ("mi" nil ("ミ" . "み")) ("mo" nil ("モ" . "も"))
				   ("mu" nil ("ム" . "む")) ("mya" nil ("ミャ" . "みゃ"))
				   ("mye" nil ("ミェ" . "みぇ")) ("myi" nil ("ミィ" . "みぃ"))
				   ("myo" nil ("ミョ" . "みょ")) ("myu" nil ("ミュ" . "みゅ"))
				   ("n" nil ("ン" . "ん")) ("n'" nil ("ン" . "ん"))
				   ("na" nil ("ナ" . "な")) ("ne" nil ("ネ" . "ね"))
				   ("ni" nil ("ニ" . "に")) ("nn" nil ("ン" . "ん"))
				   ("no" nil ("ノ" . "の")) ("nu" nil ("ヌ" . "ぬ"))
				   ("nya" nil ("ニャ" . "にゃ")) ("nye" nil ("ニェ" . "にぇ"))
				   ("nyi" nil ("ニィ" . "にぃ")) ("nyo" nil ("ニョ" . "にょ"))
				   ("nyu" nil ("ニュ" . "にゅ")) ("o" nil ("オ" . "お"))
				   ("pp" "p" ("ッ" . "っ")) ("pa" nil ("パ" . "ぱ"))
				   ("pe" nil ("ペ" . "ぺ")) ("pi" nil ("ピ" . "ぴ"))
				   ("po" nil ("ポ" . "ぽ")) ("pu" nil ("プ" . "ぷ"))
				   ("pya" nil ("ピャ" . "ぴゃ")) ("pye" nil ("ピェ" . "ぴぇ"))
				   ("pyi" nil ("ピィ" . "ぴぃ")) ("pyo" nil ("ピョ" . "ぴょ"))
				   ("pyu" nil ("ピュ" . "ぴゅ")) ("rr" "r" ("ッ" . "っ"))
				   ("ra" nil ("ラ" . "ら")) ("re" nil ("レ" . "れ"))
				   ("ri" nil ("リ" . "り")) ("ro" nil ("ロ" . "ろ"))
				   ("ru" nil ("ル" . "る")) ("rya" nil ("リャ" . "りゃ"))
				   ("rye" nil ("リェ" . "りぇ")) ("ryi" nil ("リィ" . "りぃ"))
				   ("ryo" nil ("リョ" . "りょ")) ("ryu" nil ("リュ" . "りゅ"))
				   ("ss" "s" ("ッ" . "っ")) ("sa" nil ("サ" . "さ"))
				   ("se" nil ("セ" . "せ")) ("sha" nil ("シャ" . "しゃ"))
				   ("she" nil ("シェ" . "しぇ")) ("shi" nil ("シ" . "し"))
				   ("sho" nil ("ショ" . "しょ")) ("shu" nil ("シュ" . "しゅ"))
				   ("si" nil ("シ" . "し")) ("so" nil ("ソ" . "そ"))
				   ("su" nil ("ス" . "す")) ("sya" nil ("シャ" . "しゃ"))
				   ("sye" nil ("シェ" . "しぇ")) ("syi" nil ("シィ" . "しぃ"))
				   ("syo" nil ("ショ" . "しょ")) ("syu" nil ("シュ" . "しゅ"))
				   ("tt" "t" ("ッ" . "っ")) ("ta" nil ("タ" . "た"))
				   ("te" nil ("テ" . "て")) ("tha" nil ("テァ" . "てぁ"))
				   ("the" nil ("テェ" . "てぇ")) ("thi" nil ("ティ" . "てぃ"))
				   ("tho" nil ("テョ" . "てょ")) ("thu" nil ("テュ" . "てゅ"))
				   ("ti" nil ("チ" . "ち")) ("to" nil ("ト" . "と"))
				   ("tsu" nil ("ツ" . "つ")) ("tu" nil ("ツ" . "つ"))
				   ("tya" nil ("チャ" . "ちゃ")) ("tye" nil ("チェ" . "ちぇ"))
				   ("tyi" nil ("チィ" . "ちぃ")) ("tyo" nil ("チョ" . "ちょ"))
				   ("tyu" nil ("チュ" . "ちゅ")) ("u" nil ("ウ" . "う"))
				   ("vv" "v" ("ッ" . "っ")) ("va" nil ("ヴァ" . "う゛ぁ"))
				   ("ve" nil ("ヴェ" . "う゛ぇ")) ("vi" nil ("ヴィ" . "う゛ぃ"))
				   ("vo" nil ("ヴォ" . "う゛ぉ")) ("vu" nil ("ヴ" . "う゛"))
				   ("ww" "w" ("ッ" . "っ")) ("wa" nil ("ワ" . "わ"))
				   ("we" nil ("ウェ" . "うぇ")) ("wi" nil ("ウィ" . "うぃ"))
				   ("wo" nil ("ヲ" . "を")) ("wu" nil ("ウ" . "う"))
				   ("xx" "x" ("ッ" . "っ")) ("xa" nil ("ァ" . "ぁ"))
				   ("xe" nil ("ェ" . "ぇ")) ("xi" nil ("ィ" . "ぃ"))
				   ("xka" nil ("ヵ" . "か")) ("xke" nil ("ヶ" . "け"))
				   ("xo" nil ("ォ" . "ぉ")) ("xtsu" nil ("ッ" . "っ"))
				   ("xtu" nil ("ッ" . "っ")) ("xu" nil ("ゥ" . "ぅ"))
				   ("xwa" nil ("ヮ" . "ゎ")) ("xwe" nil ("ヱ" . "ゑ"))
				   ("xwi" nil ("ヰ" . "ゐ")) ("xya" nil ("ャ" . "ゃ"))
				   ("xyo" nil ("ョ" . "ょ")) ("xyu" nil ("ュ" . "ゅ"))
				   ("yy" "y" ("ッ" . "っ")) ("ya" nil ("ヤ" . "や"))
				   ("ye" nil ("イェ" . "いぇ")) ("yo" nil ("ヨ" . "よ"))
				   ("yu" nil ("ユ" . "ゆ")) ("zz" "z" ("ッ" . "っ"))
				   ("z," nil "‥") ("z-" nil "〜") ("z." nil "…")
				   ("z/" nil "・") ("z[" nil "『") ("z]" nil "』")
				   ("za" nil ("ザ" . "ざ")) ("ze" nil ("ゼ" . "ぜ"))
				   ("zh" nil "←") ("zi" nil ("ジ" . "じ"))
				   ("zj" nil "↓") ("zk" nil "↑") ("zl" nil "→")
				   ("zo" nil ("ゾ" . "ぞ")) ("zu" nil ("ズ" . "ず"))
				   ("zya" nil ("ジャ" . "じゃ")) ("zye" nil ("ジェ" . "じぇ"))
				   ("zyi" nil ("ジィ" . "じぃ")) ("zyo" nil ("ジョ" . "じょ"))
				   ("zyu" nil ("ジュ" . "じゅ")) ("." nil skk-current-kuten)
				   ("," nil skk-current-touten) ("-" nil "ー")
				   (":" nil "：") (";" nil "；") ("?" nil "？")
				   ("[" nil "「") ("]" nil "」") ("l" nil skk-latin-mode)
				   ("q" nil skk-toggle-kana) ("L" nil skk-jisx0208-latin-mode)
				   ("Q" nil skk-set-henkan-point-subr)
				   ("X" nil skk-purge-from-jisyo) ("/" nil skk-abbrev-mode)
				   ("$" nil skk-display-code-for-char-at-point)
				   ("@" nil skk-today) ("\\" nil skk-input-by-code-or-menu) ))
    (skk-rom-kana-rule-list . '(("hh" "h" ("ッ" . "っ"))))
    (skk-save-jisyo-function . 'skk-save-jisyo-original)
    (skk-search-excluding-word-pattern-function . nil)
    (skk-search-prog-list . '((skk-search-jisyo-file skktut-tut-jisyo 0 t)))
    (skk-set-henkan-point-key . '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?M ?N ?O ?P ?R ?S ?T ?U ?V ?W ?Y ?Z))
    (skk-special-midashi-char-list . '(?> ?< ??))
    (skk-start-henkan-key . " ")
    (skk-try-completion-key . "\t")
    (skk-update-jisyo-function . 'skk-update-jisyo-original)
    (skk-use-color-cursor . (and window-system (fboundp 'x-display-color-p) (x-display-color-p)))
    (skk-use-cursor-change . t)
    (skk-use-face . (or window-system (skk-terminal-face-p)))
    (skk-use-look . nil)
    (skk-use-numeric-conversion . t)
    (skk-use-rdbms . nil)
    (skk-use-relation . nil)
    (skk-use-viper . nil)

    ;; not user variables but to be localized.
    (skk-insert-new-word-function . nil)
    (skk-mode-invoked . t)
    (skk-rule-tree
     .
     (skk-compile-rule-list skk-rom-kana-base-rule-list skk-rom-kana-rule-list) ))
  "skk.el のユーザー変数のリスト。" )

(defvar skktut-japanese-tut nil
  "Non-nil であれば、チュートリアルが日本語であることを示す。" )
(defvar skktut-right-answer nil "正解の文字列。")
(defvar skktut-question-count 1 "チュートリアルの現在の問題番号。")
(defvar skktut-tutorial-end nil "チュートリアルの終了を示すフラグ。")
(defvar skktut-working-buffer " *skk-tutorial*")
(defvar skktut-question-buffer "*問*")
(defvar skktut-answer-buffer "*答*")
(defvar skktut-jisyo-buffer " *skk-tut-jisyo*")
(defvar skktut-original-window-configuration nil)
(defvar skktut-working-window-configuration nil)
(defvar skktut-skk-mode-on nil
  "Non-nil であれば、skk-tutorial を起動したときに SKK が既に起動されていたことを示す。" )

(defvar skktut-latin-mode-map nil
  "SKK チュートリアル ASCII モードキーマップ。" )

(or skktut-latin-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-j" 'skk-kakutei)
      (setq skktut-latin-mode-map map) ))

(defvar skktut-j-mode-map nil
  "SKK チュートリアルかな/カナモードキーマップ。" )

(or skktut-j-mode-map
    (let ((map (make-sparse-keymap)))
      (substitute-key-definition 'self-insert-command 'skk-insert map
				 global-map)
      (substitute-key-definition 'egg-self-insert-command 'skk-insert map
				 global-map)
      (substitute-key-definition 'canna-self-insert-command 'skk-insert map
				 global-map)
      (substitute-key-definition 'can-n-egg-self-insert-command 'skk-insert map
				 global-map)
      (define-key map "x" 'skk-previous-candidate)
      (define-key map "\C-j" 'skk-kakutei)
      (define-key map "\t" 'skk-insert)
      (setq skktut-j-mode-map map) ))

(defvar skktut-jisx0208-latin-mode-map nil
  "SKK チュートリアル全角英数字モードキーマップ。" )

(or skktut-jisx0208-latin-mode-map
    (let ((map (make-sparse-keymap))
	  (i 0) )
      (while (< i 128)
	(if (aref skk-jisx0208-latin-vector i)
	    (define-key map (char-to-string i) 'skk-jisx0208-latin-insert) )
	(setq i (1+ i)) )
      (define-key map "\C-j" 'skk-kakutei)
      (setq skktut-jisx0208-latin-mode-map map) ))

(defvar skktut-abbrev-mode-map nil
  "SKK チュートリアル Abbrev モードキーマップ。" )

(or skktut-abbrev-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "," 'skk-abbrev-comma)
      (define-key map "." 'skk-abbrev-period)
      (define-key map "\C-q" 'skk-jisx0208-latin-henkan)
      (define-key map "\C-j" 'skk-kakutei)
      (define-key map " " 'skk-start-henkan)
      (define-key map "\t" 'skk-try-completion)
      (setq skktut-abbrev-mode-map map) ))

;; -- macros
(defmacro skktut-message (japanese english &rest arg)
  ;; skktut-japanese-tut が non-nil だったら JAPANESE を nil であれば ENGLISH
  ;; をエコーエリアに表示する。
  ;; ARG は message 関数の第２引数以降の引数として渡される。
  (append (list 'message (list 'if 'skktut-japanese-tut japanese english))
          arg ))

(defmacro skktut-error (japanese english &rest arg)
  ;; skktut-japanese-tut が non-nil だったら JAPANESE を nil であれば ENGLISH
  ;; をエコーエリアに表示し、エラーを発生させる。
  ;; ARG は error 関数の第２引数以降の引数として渡される。
  (append (list 'error (list 'if 'skktut-japanese-tut japanese english))
          arg ))

(defmacro skktut-yes-or-no-p (japanese english)
  (list 'yes-or-no-p (list 'if 'skktut-japanese-tut japanese english)) )

;; advices.
(defadvice skk-abbrev-mode (before skktut-ad disable)
  "SKK チュートリアル用アドバイス付。"
  (and (> 12 skktut-question-count)
       (skktut-error "このキーはまだ使えません" "Cannot use this key yet" ) ))

(defadvice skk-insert (before skktut-ad disable)
  "SKK チュートリアル用アドバイス付。"
  (and (memq last-command-char skk-set-henkan-point-key)
       (> 12 skktut-question-count)
       (skktut-error "かな/カナモードでは、英大文字はまだ使えません"
		     "Cannot use upper case character in kana/katakana mode" ) ))

(defadvice skk-kakutei (before skktut-ad disable)
  "SKK チュートリアル用アドバイス付。"
  (and (interactive-p)
       (= skktut-question-count 1)
       (skktut-error "このキーはまだ使えません" "Cannot use this key yet" ) ))

(defadvice skk-mode (before skktut-ad disable)
  "SKK チュートリアル用アドバイス付。"
  (and (interactive-p)
       (= skktut-question-count 1)
       (skktut-error "このキーはまだ使えません" "Cannot use this key yet" ) ))

(defadvice kill-buffer (around skktut-ad disable)
  "SKK チュートリアル用アドバイス付。"
  (cond ((or (not (interactive-p))
	     (null (member (ad-get-arg 0) (list skktut-working-buffer
						skktut-question-buffer
						skktut-answer-buffer
						skktut-jisyo-buffer ))))
	 ad-do-it )
	((skktut-yes-or-no-p "チュートリアルをやめますか? "
			     "Quit tutorial? " )
	 (skk-tutorial-quit 'now)
	 ;; already killed.
	 ;;ad-do-it
	 )))

(defadvice other-frame (before skktut-ad disable)
  "SKK チュートリアル用アドバイス付。"
  (skktut-before-move-to-other-frame) )

;;(defadvice select-frame (before skktut-ad disable)
;;(defadvice select-frame (before skktut-ad activate)
;;  "SKK チュートリアル用アドバイス付。"
;;  (skktut-before-move-to-other-frame) )

(defadvice save-buffers-kill-emacs (around skktut-ad disable)
  "SKK チュートリアル用アドバイス付。"
  (if (skktut-yes-or-no-p "Tutorial も Emacs も終了します。よろしいですね？ "
			  "Quit tutorial and kill emacs? " )
      (progn (skk-tutorial-quit 'now)
	     ad-do-it )))

;; interactive commands. prefix should be `skk-tutorial'.
;;;###autoload
(defun skk-tutorial (&optional query-language)
  "SKK チュートリアルを起動する。
C-u M-x skk-tutorial すると、チュートリアルファイルの選択が可能。"
  (interactive "P")
  (if query-language
      (let* ((lang (completing-read "Language: " skk-tut-file-alist))
	     (file (cdr (assoc lang skk-tut-file-alist))) )
	(if (not (file-exists-p (expand-file-name file)))
	    (error "No file found as %s" file)
	  (setq skk-tut-file file)
	  (message "SKK tutorial language set to %s until you exit Emacs"
		   lang ))))
  (let ((inhibit-quit t))
    (if (not (and (boundp 'skk-major-version) (boundp 'skk-minor-version)
		  (>= skk-major-version 10) (>= skk-minor-version 46) ))
        (error "skk.el version 10.46 or later is required")
      (skktut-pre-setup-tutorial)
      (skktut-setup-jisyo-buffer)
      (skktut-setup-working-buffer)
      (skktut-setup-question-buffer)
      (skktut-setup-answer-buffer)
      (skktut-enable-advice)
      (skktut-enable-tutmap)
      (add-hook 'before-make-frame-hook 'skktut-before-move-to-other-frame)
      (add-hook 'minibuffer-setup-hook 'skktut-localize-and-init-variables)
      (skktut-make-windows) )))

(defun skk-tutorial-again (&optional now)
  "SKK チュートリアルを最初からやり直す。
C-u M-x skk-tutorial-again すると、yes-or-no-p で尋ねられることなく直ちにやり直す。"
 (interactive "P")
  (if (or now
	  (skktut-yes-or-no-p "最初から Tutorial をやり直します。よろしいですね？ "
			      "Quit tutorial and start from question 1 again? " ))
      (progn (skk-tutorial-quit 'now)
             (skk-tutorial) )))

(defun skk-tutorial-quit (&optional now)
  "SKK チュートリアルをやめる。
C-u M-x skk-tutorial-quit すると、yes-or-no-p で尋ねられることなく直ちにやめる。"
  (interactive "P")
  (if (or now (skktut-yes-or-no-p "本当にチュートリアルをやめますか? "
                                  "Really quit tutorial? " ))
      (let ((inhibit-quit t))
        (delete-other-windows)
        ;; 再度チュートリアルを使えるように、内部変数を初期化しておく。
        (setq skktut-japanese-tut nil
              skktut-question-count 1
              skktut-right-answer nil
              skktut-tutorial-end nil )
        (remove-hook 'minibuffer-setup-hook 'skktut-localize-and-init-variables)
        (remove-hook 'before-make-frame-hook 'skktut-before-move-to-other-frame)
	(skktut-disable-tutmap)
	(skktut-disable-advice)
	(save-excursion
	  (set-buffer skktut-jisyo-buffer)
	  (set-buffer-modified-p nil)
	  (kill-buffer skktut-jisyo-buffer) )
        (kill-buffer skktut-working-buffer)
        (kill-buffer skktut-answer-buffer)
        (kill-buffer skktut-question-buffer)
	(set-window-configuration skktut-original-window-configuration)
        ;; SKK を起動せずにいきなり
        ;; skk-tutorial を実行したときに skk-jisyo バッファが作られないので
        ;; skk-setup-jisyo-buffer でエラーとなり、Emacs の終了ができなく
        ;; なるので SKK モードを一度起こしておく。
        (skk-mode 1)
        ;; チュートリアル起動直前に開いていたバッファで、skk-mode を起動して
        ;; いたら、その状態にして、チュートリアルを終了する。
        (or skktut-skk-mode-on
            (skk-mode -1) ))))

;; the following commands are also interactive, but users may not call
;; them by name.  So prefix should be `skktut-'.
(defun skktut-next-question ()
  (interactive)
  (set-window-configuration skktut-working-window-configuration)
  ;; called in skktut-answer-buffer.
  (save-match-data
    (let (user-ans)
      (goto-char (point-min))
      (end-of-line)
      (skip-chars-backward " \t")
      (setq user-ans (buffer-substring-no-properties (point-min) (point)))
      (if (string-match "^>* *" user-ans)
	  (setq user-ans (substring user-ans (match-end 0))) )
      (if (not (string= skktut-right-answer user-ans))
	  (progn
	    (skktut-message "答が違います。もう一度やってみて下さい"
			    "Wrong.  Try again")
	    (ding) )
	(setq skktut-question-count (1+ skktut-question-count))
	;; buffer independent.
	(skktut-get-question-page skktut-question-count)
	(if (>= skktut-question-count (1+ skktut-question-numbers))
	    (skk-tutorial-quit 'now)
	  (skktut-next-answer-buffer) )))))

(defun skktut-skip-question (arg)
  (interactive "p")
  (set-window-configuration skktut-working-window-configuration)
  ;; called in skktut-answer-buffer.
  (skktut-erase-buffer)
  (setq skktut-question-count (+ skktut-question-count arg))
  (cond ((> 1 skktut-question-count)
	 (setq skktut-question-count 1) )
	;; overrun
	((> skktut-question-count skktut-question-numbers)
	 (setq skktut-question-count skktut-question-numbers) )
	((and (>= skktut-question-count 3) (not skk-j-mode))
	 (skk-mode 1) ))
  ;; buffer independent.
  (skktut-get-question-page skktut-question-count)
  (if skktut-tutorial-end
      (skk-tutorial-quit 'now)
    (skktut-next-answer-buffer) ))

;; internal functions.  prefix should be `skktut-'.
(defun skktut-make-windows ()
  ;; Make window fill its frame.
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  ;; make it selected window and current buffer.
  (switch-to-buffer skktut-answer-buffer)
  (enlarge-window (- (window-height (selected-window)) 20))
  ;; not make it current buffer but visible.
  (display-buffer skktut-question-buffer)
  (setq skktut-working-window-configuration (current-window-configuration)) )

(defun skktut-enable-advice ()
  (let ((alist skktut-adviced-alist)
	 e )
    (while alist
      (setq e (car alist) )
      (ad-enable-advice (car e) (cdr e) 'skktut-ad)
      (ad-activate (car e))
      (setq alist (cdr alist)) )))

(defun skktut-disable-advice ()
  (let ((alist skktut-adviced-alist)
	 e )
    (while alist
      (setq e (car alist) )
      (ad-disable-advice (car e) (cdr e) 'skktut-ad)
      (ad-activate (car e))
      (setq alist (cdr alist)) )))

(defun skktut-enable-tutmap ()
  (let ((inhibit-quit t))
    (set-modified-alist
     'minor-mode-map-alist
     ;; tut map
     (list (cons 'skk-latin-mode skktut-latin-mode-map)
	   (cons 'skk-abbrev-mode skktut-abbrev-mode-map)
	   (cons 'skk-j-mode skktut-j-mode-map)
	   (cons 'skk-jisx0208-latin-mode skktut-jisx0208-latin-mode-map) ))
    ;; for minor-mode-map-alist localized by Viper.
    (if (not (featurep 'viper))
	nil
      (if (if (eq skk-emacs-type 'xemacs)
	      (local-variable-p 'minor-mode-map-alist nil t)
	    (local-variable-p 'minor-mode-map-alist) )
	  (setq-default minor-mode-map-alist minor-mode-map-alist) ))))

(defun skktut-disable-tutmap ()
  (let ((inhibit-quit t)
	(minor-mode-list
	 '(skk-abbrev-mode skk-latin-mode skk-j-mode skk-jisx0208-latin-mode) )
	minor-mode e )
    (while minor-mode-list
      (setq minor-mode (car minor-mode-list)
	    minor-mode-list (cdr minor-mode-list) )
      ;; fail safe.
      (while (setq e (assq minor-mode minor-mode-map-alist))
	(setq minor-mode-map-alist (delq e minor-mode-map-alist)) ))
    (set-modified-alist
     'minor-mode-map-alist
     (list (cons 'skk-latin-mode skk-latin-mode-map)
	   (cons 'skk-abbrev-mode skk-abbrev-mode-map)
	   (cons 'skk-j-mode skk-j-mode-map)
	   (cons 'skk-jisx0208-latin-mode skk-jisx0208-latin-mode-map) )))
  ;; for minor-mode-map-alist localized by Viper.
  (and (default-value skk-use-viper) (skk-viper-normalize-map)) )

(defun skktut-pre-setup-tutorial ()
  (setq skktut-original-window-configuration (current-window-configuration)
	skktut-skk-mode-on skk-mode
	skktut-question-count 1 ))

(defun skktut-setup-jisyo-buffer ()
  ;; setup skktut-tut-jisyo buffer.
  (save-excursion
    (set-buffer (get-buffer-create skktut-jisyo-buffer))
    (buffer-disable-undo (current-buffer))
    (skktut-localize-and-init-variables)
    (setq case-fold-search nil
	  buffer-file-name (expand-file-name skktut-tut-jisyo) )
    (insert (concat ";; okuri-ari entries.\n"
		    "ほっs /欲/\n"
		    "つかt /使/\n"
		    "たっs /達/\n"
		    "しt /知/\n"
		    "うごk /動/\n"
		    ";; okuri-nasi entries.\n"
		    "Greek /Α/Β/Γ/Δ/Ε/Ζ/Η/Θ/Ι/Κ/Λ/Μ/Ν/Ξ/Ο/Π/"
		    "Ρ/Σ/Τ/Υ/Φ/Χ/Ψ/Ω/\n"
		    "Russia /А/Б/В/Г/Д/Е/Ё/Ж/З/И/Й/К/Л/М/Н/О/"
		    "П/Р/С/Т/У/Ф/Х/Ц/Ч/Ш/Щ/Ъ/Ы/Ь/Э/Ю/Я/\n"
		    "greek /α/β/γ/δ/ε/ζ/η/θ/ι/κ/λ/μ/ν/ξ/ο/π/"
		    "ρ/σ/τ/υ/φ/χ/ψ/ω/\n"
		    "russia /а/б/в/г/д/е/ё/ж/з/и/й/к/л/м/н/о/"
		    "п/р/с/т/у/ф/х/ц/ч/ш/щ/ъ/ы/ь/э/ю/я/\n"
		    "いちおく /一億/\n"
		    "おおさか /大阪/\n"
		    "かな /仮名/\n"
		    "かんじ /漢字/幹事/監事/\n"
		    "がくしゅう /学習/\n"
		    "き /基/記/気/木/帰/\n"
		    "きごう /記号/、/。/，/．/・/：/；/？/！/゛/゜/´/｀/¨/"
		    "＾/￣/＿/ヽ/ヾ/ゝ/ゞ/〃/仝/々/〆/〇/ー/―/‐/／/＼/〜/"
		    "‖/｜/…/‥/‘/’/“/”/（/）/〔/〕/［/］/｛/｝/〈/〉/"
		    "《/》/「/】/『/』/【/】/＋/−/±/×/÷/＝/≠/＜/＞/≦/≧/"
		    "∞/∴/♂/♀/°/′/″/℃/￥/＄/¢/£/％/＃/＆/＊/＠/§/☆/"
		    "★/○/●/◎/◇/◆/□/■/△/▲/▽/▼/※/〒/→/←/↑/↓/"
		    "〓/\n"
		    "きょうと /京都/\n"
		    "こうべ /神戸/\n"
		    "ご /五/互/伍/午/呉/吾/娯/後/御/悟/梧/檎/瑚/碁/語/誤/護/"
		    "醐/\n"
		    "さい /細/最/再/\n"
		    "さいしょ /最初/\n"
		    "さいとう /斎藤/\n"
		    "さとう /佐藤/\n"
		    "しゅうりょう /終了/\n"
		    "じしょ /辞書/地所/\n"
		    "じんこう /人口/\n"
		    "せんたく /選択/洗濯/\n"
		    "そう /走/\n"
		    "だい /大/第/代/\n"
		    "てき /的/敵/滴/適/摘/\n"
		    "とう /東/\n"
		    "とうほく /東北/\n"
		    "とうろく /登録/\n"
		    "とうろく /登録/\n"
		    "どう /動/\n"
		    "にゅうりょく /入力/\n"
		    "ひこうき /飛行機/\n"
		    "へんかん /変換/\n"
		    "ほく /北/\n"
		    "みょうじ /名字/\n"
		    "ようい /容易/用意/\n" ))
    (skk-setup-jisyo-buffer) ))

(defun skktut-setup-working-buffer ()
  (save-match-data
    (let (sexp)
      (set-buffer (get-buffer-create skktut-working-buffer))
      (buffer-disable-undo (current-buffer))
      (skktut-localize-and-init-variables)
      (skktut-erase-buffer) ; fail safe.
      (insert-file-contents skk-tut-file)
      (goto-char (point-min))
      (setq skktut-japanese-tut (looking-at ";; SKK Japanese"))
      (while (re-search-forward "^>> \\((.+)\\)$" nil t nil)
        (setq sexp (buffer-substring-no-properties (match-beginning 1)
						   (match-end 1) ))
        (delete-region (match-beginning 1) (match-end 1))
	;; insert evaluated string instead of lisp program.
        (insert (eval (car (read-from-string sexp)))) )
      (goto-char (point-min))
      (if skk-tut-use-face (skktut-colored)) )))

(defun skktut-setup-question-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create skktut-question-buffer))
    (buffer-disable-undo (current-buffer))
    (skktut-erase-buffer) ; fail safe.
    (setq buffer-read-only t)
    (skktut-get-question-page skktut-question-count)
    (local-set-key "\C-xq" 'skk-tutorial-quit)
    (local-set-key "\C-xt" 'skk-tutorial-again)
    (local-set-key "\C-xj" 'skktut-error-command)
    (local-set-key "\C-xn" 'skktut-next-question)
    (local-set-key "\C-xs" 'skktut-skip-question) ))

(defun skktut-setup-answer-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create skktut-answer-buffer))
    ;; users may use undo.
    ;; (buffer-disable-undo (current-buffer))
    ;; skktut-answer-buffer の skk.el の変数をバッファローカル化し、初期化する。
    (skktut-localize-and-init-variables)
    (local-set-key "\C-xq" 'skk-tutorial-quit)
    (local-set-key "\C-xt" 'skk-tutorial-again)
    (local-set-key "\C-xj" 'skktut-error-command)
    (local-set-key "\C-xn" 'skktut-next-question)
    (local-set-key "\C-xs" 'skktut-skip-question)
    (auto-fill-mode -1)
    (skktut-next-answer-buffer) ))

(defun skktut-localize-and-init-variables ()
  ;; ユーザーが skk.el の変数をカスタマイズしている可能性があるので、カレント
  ;; バッファの skk.el の変数をバッファローカル化し、初期化する。
  (let ((alist skktut-init-variables-alist)
	v )
    (while alist
      (setq v (car (car alist)))
      (make-local-variable v)
      (set v (eval (cdr (car alist))))
      (setq alist (cdr alist)) )))

(defun skktut-erase-buffer ()
  (let ((inhibit-read-only t)
	buffer-read-only )
    (set-text-properties (point-min) (point-max) nil)
    (erase-buffer) ))

(defun skktut-before-move-to-other-frame ()
  (if (skktut-yes-or-no-p "Tutorial を終了します。よろしいですね？ "
			  "Quit tutorial?" )
      (skk-tutorial-quit 'now)
    (skktut-error "Tutorial を終了せずに他のフレームに移ることはできません。"
                  "Quit tutorial or you cannot move to other frame" )))

(defun skktut-colored ()
  ;; face を Text Property にしておくとテキストをコピーしたときに一緒にコピーで
  ;; きるので好都合。
  (while (re-search-forward "▼\\([^】 ぁ-んァ-ン]+\\)" nil t nil)
    (put-text-property (match-beginning 1) (match-end 1) 'face
                       'highlight ))
  (goto-char (point-min))
  (while (re-search-forward "^==.+==$" nil t nil)
    (put-text-property (match-beginning 0) (match-end 0)
                       'face skk-tut-section-face ))
  (goto-char (point-min))
  (while (re-search-forward "^!!.+" nil t nil)
    (put-text-property (match-beginning 0) (match-end 0)
                       'face skk-tut-do-it-face ))
  (goto-char (point-min))
  (while (re-search-forward "^>> \\(.+\\)$" nil t nil)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face skk-tut-question-face ))
  (if skktut-japanese-tut
      nil
    (goto-char (point-min))
    (while (re-search-forward "Hint: .*$" nil t nil)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face skk-tut-hint-face ))))

(defun skktut-next-answer-buffer ()
  (save-match-data
    (save-excursion
      ;; first get right answer in `skktut-question-buffer'.
      (set-buffer skktut-question-buffer)
      (goto-char (point-max))
      (search-backward "\n>>")
      (forward-char 1)
      (setq skktut-right-answer
	    (buffer-substring-no-properties
	     (+ 3 (point))
	     (skk-save-point (end-of-line) (point)) )))
    ;; not to save point.
    (let ((cbuf (current-buffer))
	  p )
      (unwind-protect
	  (let ((plist (cons (if (eq skk-emacs-type 'xemacs)
				 'end-open
			       'rear-nonsticky )
			     '(t intangible t read-only t) )))
	    ;; secondary make a new answer buffer.
	    (set-buffer skktut-answer-buffer)
	    (skktut-erase-buffer)
	    (insert ">> \n\n")
	    (add-text-properties (point-min) (- (point) 2) plist)
	    (setq p (point))
	    (insert
	     (if skktut-japanese-tut
		 (concat "* 答ができたら `C-x n'; 途中でやめるには `C-x q'"
			 (if (= skktut-question-count 37) " *"
			   "; スキップするには`C-x s' *" ))
	       (concat "* For next question `C-x n'; to quit `C-x q'"
		       (if (= skktut-question-count 37) " *"
			 "; to skip this question `C-x s' *" ))))
	    (if skk-tut-use-face
		(put-text-property p (point) 'face skk-tut-key-bind-face) )
	    (add-text-properties p (point) plist)
	    (goto-char (+ (point-min) 3)))
	(set-buffer cbuf) ))))

(defun skktut-get-question-page (page)
  (save-excursion
    (save-match-data
      (set-buffer skktut-working-buffer)
      (let (pos str)
        (goto-char (point-min))
        (search-forward "--\n" nil t page)
        (if (looking-at ";") ; lisp program exists.
            (progn
	      (forward-char 3)
	      (setq pos (point))
	      (end-of-line)
	      (save-excursion
		(eval-region pos (point) nil)
		(forward-char 1) )))
        (if (not skktut-tutorial-end)
            (progn
              (setq pos (point))
              (search-forward "\n>>")
              (end-of-line)
              (setq str (buffer-substring pos (point)))
	      (set-buffer skktut-question-buffer)
	      (skktut-erase-buffer)
	      (let (buffer-read-only)
		(insert str)
		(setq mode-line-buffer-identification
		      (concat "ＳＫＫチュートリアル: ［問 "
			      (number-to-string page)
			      "］ （残り "
			      (number-to-string (- skktut-question-numbers page))
			      "問）"))
		(set-buffer-modified-p nil)
		(force-mode-line-update 'all) )))))))

;; The following two functions are tricky, since they are executed by
;; `eval-region' in skktut-working-buffer.
(defun skktut-today ()
  (save-restriction
    (save-match-data
      (let (p)
	(widen)
        (search-forward "\n>> ")
	(if (re-search-forward "「.*」" (skk-save-point (end-of-line) (point)) t)
	    (delete-region (match-beginning 0) (match-end 0)) )
	(setq p (point))
	(insert (concat "「きょうは、" (skk-current-date) "です。」"))
	(narrow-to-region (point-min) (point))
	(if skk-tut-use-face
	    (put-text-property p (point) 'face skk-tut-question-face) )))))

(defun skktut-end-tutorial ()
  (switch-to-buffer skktut-question-buffer)
  (delete-other-windows)
  (skktut-erase-buffer)
  (let (buffer-read-only)
    (goto-char (point-min))
    (insert
     (if skktut-japanese-tut
	 (concat "SKK チュートリアルはこれで終りです。\n\n"
		 "SKK 10.x に関する質問、コメント、bug report 等は\n\n"
		 "\tskk@ring.gr.jp\n\n"
		 "迄お送り下さい。なお、このアドレスは SKK Ring Server Openlab メイリング"
 		 "リストのアドレスです。\n"
		 "回答は通常このアドレスに対してなされるので、メンバーでない"
		 "方はその旨を明記して\n"
		 "メールをお送りください。SKK Ring Server Openlab メイリングリストへ参"
		 "加希望の場合は\n\n"
		 "\tskk-request@ring.gr.jp\n\n"
		 "へメールをお送りください\n\n"
		 "!! 最後に <return> キーを押してください。" )
       (concat "Now we end the SKK tutorial.\n\n"
	       "Please send comments, questions and bug reports on SKK "
	       "version 10.x to:\n\n"
	       "\tskk@ring.gr.jp\n\n"
	       "This is the address of the SKK Ring Server Openlab mailing list, and "
	       "normally the responces\n"
	       "will be sent only to the ML members.  So, if you are not a ML "
	       "member, please say so \n"
	       "in your mail. If you are interested in joining the SKK Ring Server "
	       "Openlab ML, send a mail to:\n\n"
	       "\tskk-request@ring.gr.jp\n\n"
	       "!! Hit <return> key when you are ready." )))
    (if skk-tut-use-face
	(save-match-data
	  (goto-char (point-min))
	  (re-search-forward "^!!.+" nil t nil)
	  (put-text-property (match-beginning 0) (match-end 0)
			     'face skk-tut-do-it-face )))
    (while (not skktut-tutorial-end)
      (condition-case nil
	  (let* ((event (skk-read-event))
		 (char (event-to-character event)) )
	    (skktut-message "<return> キーを押してください" "Hit <return> key")
	    (if (and char (eq ?\C-m char))
		(setq skktut-tutorial-end t)
	      ;;(skk-unread-event event)
	      ))
	(error nil) ))))

(provide 'skk-tut)
;;; skk-tut.el ends here
