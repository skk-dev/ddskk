;;; skk-hankaku-mode.el --- SKK 用 JISX 0201 カナ入力プログラム
;; Copyright (C) 1999 Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Version: $Id: skk-hankaku-mode.el,v 1.1 1999/10/31 08:33:09 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/10/31 08:33:09 $

;; This file is not part of SKK yet.

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
;;
;; ~/.skk に
;;
;; (require 'skk-hankaku-mode)
;;
;; と書くことでインストールされます。使い方は以下のようになります。
;;
;; ◎ひらがなモードにおいて、
;;   ・"qq" で全角カナモードになります。
;;   ・"qa" で半角カナモードになります。
;; ◎カタカナモードにおいて、
;;   ・"qq" でひらがなモードになります。
;;   ・"qs" で全角カナモードと半角カナモードを切りかえます。
;; ◎ひらがな/カタカナ両モード内での▽モードにおいて、
;;   ・ひらがな/カタカナのトグル変換は "q" ではなく "qq" で行われます。
;;
;; その他は従来通り使えます。

;;; Code:
(require 'skk)

(defvar skk-hankaku-rule-list
  '(("a" nil ("ｱ" . "あ"))
    ("bb" "b" ("ｯ" . "っ"))
    ("ba" nil ("ﾊﾞ" . "ば"))
    ("be" nil ("ﾍﾞ" . "べ"))
    ("bi" nil ("ﾋﾞ" . "び"))
    ("bo" nil ("ﾎﾞ" . "ぼ"))
    ("bu" nil ("ﾌﾞ" . "ぶ"))
    ("bya" nil ("ﾋﾞｬ" . "びゃ"))
    ("bye" nil ("ﾋﾞｪ" . "びぇ"))
    ("byi" nil ("ﾋﾞｨ" . "びぃ"))
    ("byo" nil ("ﾋﾞｮ" . "びょ"))
    ("byu" nil ("ﾋﾞｭ" . "びゅ"))
    ("cc" "c" ("ｯ" . "っ"))
    ("cha" nil ("ﾁｬ" . "ちゃ"))
    ("che" nil ("ﾁｪ" . "ちぇ"))
    ("chi" nil ("ﾁ" . "ち"))
    ("cho" nil ("ﾁｮ" . "ちょ"))
    ("chu" nil ("ﾁｭ" . "ちゅ"))
    ("cya" nil ("ﾁｬ" . "ちゃ"))
    ("cye" nil ("ﾁｪ" . "ちぇ"))
    ("cyi" nil ("ﾁｨ" . "ちぃ"))
    ("cyo" nil ("ﾁｮ" . "ちょ"))
    ("cyu" nil ("ﾁｭ" . "ちゅ"))
    ("dd" "d" ("ｯ" . "っ"))
    ("da" nil ("ﾀﾞ" . "だ"))
    ("de" nil ("ﾃﾞ" . "で"))
    ("dha" nil ("ﾃﾞｬ" . "でゃ"))
    ("dhe" nil ("ﾃﾞｪ" . "でぇ"))
    ("dhi" nil ("ﾃﾞｨ" . "でぃ"))
    ("dho" nil ("ﾃﾞｮ" . "でょ"))
    ("dhu" nil ("ﾃﾞｭ" . "でゅ"))
    ("di" nil ("ﾁﾞ" . "ぢ"))
    ("do" nil ("ﾄﾞ" . "ど"))
    ("du" nil ("ﾂﾞ" . "づ"))
    ("dya" nil ("ﾁﾞｬ" . "ぢゃ"))
    ("dye" nil ("ﾁﾞｪ" . "ぢぇ"))
    ("dyi" nil ("ﾁﾞｨ" . "ぢぃ"))
    ("dyo" nil ("ﾁﾞｮ" . "ぢょ"))
    ("dyu" nil ("ﾁﾞｭ" . "ぢゅ"))
    ("e" nil ("ｴ" . "え"))
    ("ff" "f" ("ｯ" . "っ"))
    ("fa" nil ("ﾌｧ" . "ふぁ"))
    ("fe" nil ("ﾌｪ" . "ふぇ"))
    ("fi" nil ("ﾌｨ" . "ふぃ"))
    ("fo" nil ("ﾌｫ" . "ふぉ"))
    ("fu" nil ("ﾌ" . "ふ"))
    ("fya" nil ("ﾌｬ" . "ふゃ"))
    ("fye" nil ("ﾌｪ" . "ふぇ"))
    ("fyi" nil ("ﾌｨ" . "ふぃ"))
    ("fyo" nil ("ﾌｮ" . "ふょ"))
    ("fyu" nil ("ﾌｭ" . "ふゅ"))
    ("gg" "g" ("ｯ" . "っ"))
    ("ga" nil ("ｶﾞ" . "が"))
    ("ge" nil ("ｹﾞ" . "げ"))
    ("gi" nil ("ｷﾞ" . "ぎ"))
    ("go" nil ("ｺﾞ" . "ご"))
    ("gu" nil ("ｸﾞ" . "ぐ"))
    ("gya" nil ("ｷﾞｬ" . "ぎゃ"))
    ("gye" nil ("ｷﾞｪ" . "ぎぇ"))
    ("gyi" nil ("ｷﾞｨ" . "ぎぃ"))
    ("gyo" nil ("ｷﾞｮ" . "ぎょ"))
    ("gyu" nil ("ｷﾞｭ" . "ぎゅ"))
    ;;("h" "" ("ｵ" . "お"))
    ("ha" nil ("ﾊ" . "は"))
    ("he" nil ("ﾍ" . "へ"))
    ("hi" nil ("ﾋ" . "ひ"))
    ("ho" nil ("ﾎ" . "ほ"))
    ("hu" nil ("ﾌ" . "ふ"))
    ("hya" nil ("ﾋｬ" . "ひゃ"))
    ("hye" nil ("ﾋｪ" . "ひぇ"))
    ("hyi" nil ("ﾋｨ" . "ひぃ"))
    ("hyo" nil ("ﾋｮ" . "ひょ"))
    ("hyu" nil ("ﾋｭ" . "ひゅ"))
    ("i" nil ("ｲ" . "い"))
    ("jj" "j" ("ｯ" . "っ"))
    ("ja" nil ("ｼﾞｬ" . "じゃ"))
    ("je" nil ("ｼﾞｪ" . "じぇ"))
    ("ji" nil ("ｼﾞ" . "じ"))
    ("jo" nil ("ｼﾞｮ" . "じょ"))
    ("ju" nil ("ｼﾞｭ" . "じゅ"))
    ("jya" nil ("ｼﾞｬ" . "じゃ"))
    ("jye" nil ("ｼﾞｪ" . "じぇ"))
    ("jyi" nil ("ｼﾞｨ" . "じぃ"))
    ("jyo" nil ("ｼﾞｮ" . "じょ"))
    ("jyu" nil ("ｼﾞｭ" . "じゅ"))
    ("kk" "k" ("ｯ" . "っ"))
    ("ka" nil ("ｶ" . "か"))
    ("ke" nil ("ｹ" . "け"))
    ("ki" nil ("ｷ" . "き"))
    ("ko" nil ("ｺ" . "こ"))
    ("ku" nil ("ｸ" . "く"))
    ("kya" nil ("ｷｬ" . "きゃ"))
    ("kye" nil ("ｷｪ" . "きぇ"))
    ("kyi" nil ("ｷｨ" . "きぃ"))
    ("kyo" nil ("ｷｮ" . "きょ"))
    ("kyu" nil ("ｷｭ" . "きゅ"))
    ("mm" "c" ("ｯ" . "っ"))
    ("ma" nil ("ﾏ" . "ま"))
    ("me" nil ("ﾒ" . "め"))
    ("mi" nil ("ﾐ" . "み"))
    ("mo" nil ("ﾓ" . "も"))
    ("mu" nil ("ﾑ" . "む"))
    ("mya" nil ("ﾐｬ" . "みゃ"))
    ("mye" nil ("ﾐｪ" . "みぇ"))
    ("myi" nil ("ﾐｨ" . "みぃ"))
    ("myo" nil ("ﾐｮ" . "みょ"))
    ("myu" nil ("ﾐｭ" . "みゅ"))
    ("n" nil ("ﾝ" . "ん"))
    ("n'" nil ("ﾝ" . "ん"))
    ("na" nil ("ﾅ" . "な"))
    ("ne" nil ("ﾈ" . "ね"))
    ("ni" nil ("ﾆ" . "に"))
    ("nn" nil ("ﾝ" . "ん"))
    ("no" nil ("ﾉ" . "の"))
    ("nu" nil ("ﾇ" . "ぬ"))
    ("nya" nil ("ﾆｬ" . "にゃ"))
    ("nye" nil ("ﾆｪ" . "にぇ"))
    ("nyi" nil ("ﾆｨ" . "にぃ"))
    ("nyo" nil ("ﾆｮ" . "にょ"))
    ("nyu" nil ("ﾆｭ" . "にゅ"))
    ("o" nil ("ｵ" . "お"))
    ("pp" "p" ("ｯ" . "っ"))
    ("pa" nil ("ﾊﾟ" . "ぱ"))
    ("pe" nil ("ﾍﾟ" . "ぺ"))
    ("pi" nil ("ﾋﾟ" . "ぴ"))
    ("po" nil ("ﾎﾟ" . "ぽ"))
    ("pu" nil ("ﾌﾟ" . "ぷ"))
    ("pya" nil ("ﾋﾟｬ" . "ぴゃ"))
    ("pye" nil ("ﾋﾟｪ" . "ぴぇ"))
    ("pyi" nil ("ﾋﾟｨ" . "ぴぃ"))
    ("pyo" nil ("ﾋﾟｮ" . "ぴょ"))
    ("pyu" nil ("ﾋﾟｭ" . "ぴゅ"))
    ("rr" "r" ("ｯ" . "っ"))
    ("ra" nil ("ﾗ" . "ら"))
    ("re" nil ("ﾚ" . "れ"))
    ("ri" nil ("ﾘ" . "り"))
    ("ro" nil ("ﾛ" . "ろ"))
    ("ru" nil ("ﾙ" . "る"))
    ("rya" nil ("ﾘｬ" . "りゃ"))
    ("rye" nil ("ﾘｪ" . "りぇ"))
    ("ryi" nil ("ﾘｨ" . "りぃ"))
    ("ryo" nil ("ﾘｮ" . "りょ"))
    ("ryu" nil ("ﾘｭ" . "りゅ"))
    ("ss" "s" ("ｯ" . "っ"))
    ("sa" nil ("ｻ" . "さ"))
    ("se" nil ("ｾ" . "せ"))
    ("sha" nil ("ｼｬ" . "しゃ"))
    ("she" nil ("ｼｪ" . "しぇ"))
    ("shi" nil ("ｼ" . "し"))
    ("sho" nil ("ｼｮ" . "しょ"))
    ("shu" nil ("ｼｭ" . "しゅ"))
    ("si" nil ("ｼ" . "し"))
    ("so" nil ("ｿ" . "そ"))
    ("su" nil ("ｽ" . "す"))
    ("sya" nil ("ｼｬ" . "しゃ"))
    ("sye" nil ("ｼｪ" . "しぇ"))
    ("syi" nil ("ｼｨ" . "しぃ"))
    ("syo" nil ("ｼｮ" . "しょ"))
    ("syu" nil ("ｼｭ" . "しゅ"))
    ("tt" "t" ("ｯ" . "っ"))
    ("ta" nil ("ﾀ" . "た"))
    ("te" nil ("ﾃ" . "て"))
    ("tha" nil ("ﾃｧ" . "てぁ"))
    ("the" nil ("ﾃｪ" . "てぇ"))
    ("thi" nil ("ﾃｨ" . "てぃ"))
    ("tho" nil ("ﾃｮ" . "てょ"))
    ("thu" nil ("ﾃｭ" . "てゅ"))
    ("ti" nil ("ﾁ" . "ち"))
    ("to" nil ("ﾄ" . "と"))
    ("tsu" nil ("ﾂ" . "つ"))
    ("tu" nil ("ﾂ" . "つ"))
    ("tya" nil ("ﾁｬ" . "ちゃ"))
    ("tye" nil ("ﾁｪ" . "ちぇ"))
    ("tyi" nil ("ﾁｨ" . "ちぃ"))
    ("tyo" nil ("ﾁｮ" . "ちょ"))
    ("tyu" nil ("ﾁｭ" . "ちゅ"))
    ("u" nil ("ｳ" . "う"))
    ("vv" "v" ("ｯ" . "っ"))
    ("va" nil ("ｳﾞｧ" . "うﾞぁ"))
    ("ve" nil ("ｳﾞｪ" . "うﾞぇ"))
    ("vi" nil ("ｳﾞｨ" . "うﾞぃ"))
    ("vo" nil ("ｳﾞｫ" . "うﾞぉ"))
    ("vu" nil ("ｳﾞ" . "うﾞ"))
    ("ww" "w" ("ｯ" . "っ"))
    ("wa" nil ("ﾜ" . "わ"))
    ("we" nil ("ｳｪ" . "うぇ"))
    ("wi" nil ("ｳｨ" . "うぃ"))
    ("wo" nil ("ｦ" . "を"))
    ("wu" nil ("ｳ" . "う"))
    ("xx" "x" ("ｯ" . "っ"))
    ("xa" nil ("ｧ" . "ぁ"))
    ("xe" nil ("ｪ" . "ぇ"))
    ("xi" nil ("ｨ" . "ぃ"))
    ("xka" nil ("ｶ" . "か"))
    ("xke" nil ("ｹ" . "け"))
    ("xo" nil ("ｫ" . "ぉ"))
    ("xtsu" nil ("ｯ" . "っ"))
    ("xtu" nil ("ｯ" . "っ"))
    ("xu" nil ("ｩ" . "ぅ"))
    ("xwa" nil ("ﾜ" . "ゎ"))
    ("xwe" nil ("ｪ" . "ゑ"))
    ("xwi" nil ("ｨ" . "ゐ"))
    ("xya" nil ("ｬ" . "ゃ"))
    ("xyo" nil ("ｮ" . "ょ"))
    ("xyu" nil ("ｭ" . "ゅ"))
    ("yy" "y" ("ｯ" . "っ"))
    ("ya" nil ("ﾔ" . "や"))
    ("ye" nil ("ｲｪ" . "いぇ"))
    ("yo" nil ("ﾖ" . "よ"))
    ("yu" nil ("ﾕ" . "ゆ"))
    ("zz" "z" ("ｯ" . "っ"))
    ("z," nil "‥")
    ("z-" nil "〜")
    ("z." nil "…")
    ("z/" nil "･")
    ("z[" nil "『")
    ("z]" nil "』")
    ("za" nil ("ｻﾞ" . "ざ"))
    ("ze" nil ("ｾﾞ" . "ぜ"))
    ("zh" nil "←")
    ("zi" nil ("ｼﾞ" . "じ"))
    ("zj" nil "↓")
    ("zk" nil "↑")
    ("zl" nil "→")
    ("zo" nil ("ｿﾞ" . "ぞ"))
    ("zu" nil ("ｽﾞ" . "ず"))
    ("zya" nil ("ｼﾞｬ" . "じゃ"))
    ("zye" nil ("ｼﾞｪ" . "じぇ"))
    ("zyi" nil ("ｼﾞｨ" . "じぃ"))
    ("zyo" nil ("ｼﾞｮ" . "じょ"))
    ("zyu" nil ("ｼﾞｭ" . "じゅ"))
    ("," nil "､")
    ("." nil "｡")
    ("-" nil "ｰ")
    (":" nil ":")
    (";" nil ";")
    ("?" nil "?")
    ("[" nil "｢")
    ("]" nil "｣")
    ("l" nil skk-latin-mode)
;;    ("q" nil skk-toggle-kana)
    ("L" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-set-henkan-point-subr)
    ("X" nil skk-purge-from-jisyo)
    ("/" nil skk-abbrev-mode)
    ("$" nil skk-display-code-for-char-at-point)
    ("@" nil skk-today)
    ("\\" nil skk-input-by-code-or-menu)
    )
  "*SKK 半角モードのルール。")

(defvar skk-hankaku-added-base-rule-list
  '(("qq" nil skk-toggle-kana-zenkaku)
    ("qa" nil skk-toggle-kana-hankaku)
    ("qs" nil skk-toggle-zenkaku-hankaku)
    )
  "SKK 半角モードのための skk-j-mode 全般のキー定義。")

(defvar skk-original-katakana-mode-string nil)
(defvar skk-hankaku-mode-string " ｶﾅ")

(add-hook 'skk-mode-hook
	  (function
	   (lambda ()
	     (or skk-original-katakana-mode-string
		 (setq skk-original-katakana-mode-string
		       skk-katakana-mode-string))
	     (if (and (string= skk-hankaku-mode-string " ｶﾅ")
		      (string= skk-original-katakana-mode-string "--カナ:"))
		 (setq skk-hankaku-mode-string "--ｶﾅ:")))))

(and (assoc "q" skk-rom-kana-base-rule-list)
     (delete (assoc "q" skk-rom-kana-base-rule-list) 
	     skk-rom-kana-base-rule-list))

(add-hook 'skk-mode-hook
	  (function
	   (lambda ()
	     (setq skk-rule-tree
		   (skk-compile-rule-list
		    skk-rom-kana-base-rule-list skk-rom-kana-rule-list
		    skk-hankaku-added-base-rule-list))))
	  t)

(defvar skk-hankaku-stat nil)

(defun skk-toggle-kana-zenkaku (arg)
  (interactive)
  (setq skk-rule-tree
	(skk-compile-rule-list
	 skk-rom-kana-base-rule-list skk-rom-kana-rule-list
	 skk-hankaku-added-base-rule-list) )
  (setq skk-katakana-mode-string skk-original-katakana-mode-string)
  (skk-toggle-kana arg)
  (setq skk-hankaku-stat nil))

(defun skk-toggle-kana-hankaku (arg)
  (interactive)
  (setq skk-rule-tree
	(skk-compile-rule-list
	 skk-hankaku-rule-list skk-rom-kana-rule-list
	 skk-hankaku-added-base-rule-list))
  (setq skk-katakana-mode-string skk-hankaku-mode-string)
  (skk-toggle-kana arg)
  (setq skk-hankaku-stat t))

(defun skk-toggle-zenkaku-hankaku (&optional arg)
  (interactive)
  (if skk-hankaku-stat
      (progn
	(setq skk-rule-tree
	      (skk-compile-rule-list
	       skk-rom-kana-base-rule-list skk-rom-kana-rule-list
	       skk-hankaku-added-base-rule-list))
	(setq skk-katakana-mode-string skk-original-katakana-mode-string)
	(setq skk-hankaku-stat nil))
    (setq skk-rule-tree
	  (skk-compile-rule-list
	   skk-hankaku-rule-list skk-rom-kana-rule-list
	   skk-hankaku-added-base-rule-list))
    (setq skk-katakana-mode-string skk-hankaku-mode-string)
    (setq skk-hankaku-stat t))
  (and skk-katakana (setq skk-input-mode-string skk-katakana-mode-string))
  (force-mode-line-update)
  nil)

(provide 'skk-hankaku-mode)
;;; Local Variables:
;;; End:
;;; skk-hankaku-mode.el ends here
