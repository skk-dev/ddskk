;;; skk-jisx0201.el --- SKK 用 JISX 0201 コード文字入力プログラム
;; Copyright (C) 1999, 2000 Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-jisx0201.el,v 1.10 2001/06/16 07:24:31 minakaji Exp $
;; Keywords: japanese
;; Created: Oct. 30, 1999.
;; Last Modified: $Date: 2001/06/16 07:24:31 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK  is free software;  you  can redistribute it  and/or modify it
;; under the terms  of the GNU General Public License  as published by the Free
;; Software  Foundation;  either versions  2,  or  (at your option)  any  later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful  but WITHOUT
;; ANY  WARRANTY;  without  even  the implied  warranty  of MERCHANTABILITY  or
;; FITNESS  FOR  A PARTICULAR PURPOSE.  See the GNU General Public License  for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; Daredevil SKK,  see the file COPYING.  If not,  write  to  the Free Software
;; Foundation Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

;;; Commentary:
;;
;; <簡単な説明>
;;
;; ~/.skk に
;;
;; (require 'skk-jisx0201)
;;
;; と書くことでインストールされます。使い方は以下のようになります。
;;
;; ◎カタカナモードにおいて、
;;   ・"C-q" で全角カナモードと半角カナモードを切りかえます。
;;
;; ◎ひらがな/カタカナ両モード内での▽モードにおいて、
;;   ・"C-q" を押すと､見出し語として入力されたひらがな/カタカナをﾊﾝｶｸｶﾀｶﾅに変換
;;     します。
;;
;; skk-jisx0201-roman-rule-list に JISX0201.1976 Japanese Roman (latin-jisx0201)
;; の文字列を定義しています。ただし JISX0201.1976 Japanese Roman 入力は今のとこ
;; ろ Emacs 20.3 以降とXEmacs 21 以降でしか出来ていません。ｶﾅ と roman を切り替
;; える key はとりあえず C-c C-q にしてあります。
;;
;;
;; <業務連絡>
;;
;; このファイルを編集するときは、できれば XEmacs を使ってください。 Emacs 20 は
;; デフォルトでは JISX0201.1976 Japanese Roman を自動的に US-ASCII に変換するよ
;; うになっているからです。 Emacs 20.3 以降でこのファイルを編集する場合は、この
;; ファイルを開く前に
;;
;; (setq standard-translation-table-for-decode (make-translation-table nil))
;;
;; を評価してください。 同様に Emacs 20.2 でこのファイルを編集する場合は、 この
;; ファイルを開く前に
;;
;; (setq standard-character-unification-table-for-decode (make-unification-table nil))
;;
;; を評価してください (Mule 2.3 については分かりません)。

;;; Code:
(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars)
  (require 'static))

(static-cond ((eq skk-emacs-type 'mule2)
	      (eval-and-compile
		(defvar fence-mode-map (make-keymap))
		(require 'jisx0201)))
	     (t
	      (require 'japan-util)))

(eval-and-compile
  (unless (eq skk-emacs-type 'xemacs)
    (autoload 'set-buffer-local-cursor-color "ccc"))
  (autoload 'skk-cursor-current-color "skk-cursor")
  (autoload 'skk-isearch-message "skk-isearch"))

;; 諸般の事情により skk-vars.el に入れるべきでない変数
(defvar skk-jisx0201-base-rule-list
  '(("a" nil "ｱ")
    ("bb" "b" "ｯ") ("ba" nil "ﾊﾞ") ("be" nil "ﾍﾞ")
    ("bi" nil "ﾋﾞ") ("bo" nil "ﾎﾞ") ("bu" nil "ﾌﾞ") ("bya" nil "ﾋﾞｬ")
    ("bye" nil "ﾋﾞｪ") ("byi" nil "ﾋﾞｨ") ("byo" nil "ﾋﾞｮ") ("byu" nil "ﾋﾞｭ")
    ("cc" "c" "ｯ") ("cha" nil "ﾁｬ") ("che" nil "ﾁｪ") ("chi" nil "ﾁ")
    ("cho" nil "ﾁｮ") ("chu" nil "ﾁｭ") ("cya" nil "ﾁｬ") ("cye" nil "ﾁｪ")
    ("cyi" nil "ﾁｨ") ("cyo" nil "ﾁｮ") ("cyu" nil "ﾁｭ")
    ("dd" "d" "ｯ") ("da" nil "ﾀﾞ") ("de" nil "ﾃﾞ") ("dha" nil "ﾃﾞｬ")
    ("dhe" nil "ﾃﾞｪ") ("dhi" nil "ﾃﾞｨ") ("dho" nil "ﾃﾞｮ") ("dhu" nil "ﾃﾞｭ")
    ("di" nil "ﾁﾞ") ("do" nil "ﾄﾞ") ("du" nil "ﾂﾞ") ("dya" nil "ﾁﾞｬ")
    ("dye" nil "ﾁﾞｪ") ("dyi" nil "ﾁﾞｨ") ("dyo" nil "ﾁﾞｮ") ("dyu" nil "ﾁﾞｭ")
    ("e" nil "ｴ")
    ("ff" "f" "ｯ") ("fa" nil "ﾌｧ") ("fe" nil "ﾌｪ") ("fi" nil "ﾌｨ")
    ("fo" nil "ﾌｫ") ("fu" nil "ﾌ") ("fya" nil "ﾌｬ") ("fye" nil "ﾌｪ")
    ("fyi" nil "ﾌｨ") ("fyo" nil "ﾌｮ") ("fyu" nil "ﾌｭ") ("gg" "g" "ｯ")
    ("ga" nil "ｶﾞ") ("ge" nil "ｹﾞ") ("gi" nil "ｷﾞ") ("go" nil "ｺﾞ")
    ("gu" nil "ｸﾞ") ("gya" nil "ｷﾞｬ") ("gye" nil "ｷﾞｪ") ("gyi" nil "ｷﾞｨ")
    ("gyo" nil "ｷﾞｮ") ("gyu" nil "ｷﾞｭ")
    ("ha" nil "ﾊ") ("he" nil "ﾍ") ("hi" nil "ﾋ") ("ho" nil "ﾎ")
    ("hu" nil "ﾌ") ("hya" nil "ﾋｬ") ("hye" nil "ﾋｪ") ("hyi" nil "ﾋｨ")
    ("hyo" nil "ﾋｮ") ("hyu" nil "ﾋｭ") ("i" nil "ｲ")
    ("jj" "j" "ｯ") ("ja" nil "ｼﾞｬ") ("je" nil "ｼﾞｪ") ("ji" nil "ｼﾞ")
    ("jo" nil "ｼﾞｮ") ("ju" nil "ｼﾞｭ") ("jya" nil "ｼﾞｬ") ("jye" nil "ｼﾞｪ")
    ("jyi" nil "ｼﾞｨ") ("jyo" nil "ｼﾞｮ") ("jyu" nil "ｼﾞｭ")
    ("kk" "k" "ｯ") ("ka" nil "ｶ") ("ke" nil "ｹ") ("ki" nil "ｷ")
    ("ko" nil "ｺ") ("ku" nil "ｸ") ("kya" nil "ｷｬ") ("kye" nil "ｷｪ")
    ("kyi" nil "ｷｨ") ("kyo" nil "ｷｮ") ("kyu" nil "ｷｭ")
    ("mm" "c" "ｯ") ("ma" nil "ﾏ") ("me" nil "ﾒ") ("mi" nil "ﾐ")
    ("mo" nil "ﾓ") ("mu" nil "ﾑ") ("mya" nil "ﾐｬ") ("mye" nil "ﾐｪ")
    ("myi" nil "ﾐｨ") ("myo" nil "ﾐｮ") ("myu" nil "ﾐｭ")
    ("n" nil "ﾝ") ("n'" nil "ﾝ") ("na" nil "ﾅ") ("ne" nil "ﾈ")
    ("ni" nil "ﾆ") ("nn" nil "ﾝ") ("no" nil "ﾉ") ("nu" nil "ﾇ")
    ("nya" nil "ﾆｬ") ("nye" nil "ﾆｪ") ("nyi" nil "ﾆｨ") ("nyo" nil "ﾆｮ")
    ("nyu" nil "ﾆｭ")
    ("o" nil "ｵ")
    ("pp" "p" "ｯ") ("pa" nil "ﾊﾟ") ("pe" nil "ﾍﾟ") ("pi" nil "ﾋﾟ")
    ("po" nil "ﾎﾟ") ("pu" nil "ﾌﾟ") ("pya" nil "ﾋﾟｬ") ("pye" nil "ﾋﾟｪ")
    ("pyi" nil "ﾋﾟｨ") ("pyo" nil "ﾋﾟｮ") ("pyu" nil "ﾋﾟｭ")
    ("rr" "r" "ｯ") ("ra" nil "ﾗ") ("re" nil "ﾚ") ("ri" nil "ﾘ")
    ("ro" nil "ﾛ") ("ru" nil "ﾙ") ("rya" nil "ﾘｬ") ("rye" nil "ﾘｪ")
    ("ryi" nil "ﾘｨ") ("ryo" nil "ﾘｮ") ("ryu" nil "ﾘｭ")
    ("ss" "s" "ｯ") ("sa" nil "ｻ") ("se" nil "ｾ") ("sha" nil "ｼｬ")
    ("she" nil "ｼｪ") ("shi" nil "ｼ") ("sho" nil "ｼｮ") ("shu" nil "ｼｭ")
    ("si" nil "ｼ") ("so" nil "ｿ") ("su" nil "ｽ") ("sya" nil "ｼｬ")
    ("sye" nil "ｼｪ") ("syi" nil "ｼｨ") ("syo" nil "ｼｮ") ("syu" nil "ｼｭ")
    ("tt" "t" "ｯ") ("ta" nil "ﾀ") ("te" nil "ﾃ") ("tha" nil "ﾃｧ")
    ("the" nil "ﾃｪ") ("thi" nil "ﾃｨ") ("tho" nil "ﾃｮ") ("thu" nil "ﾃｭ")
    ("ti" nil "ﾁ") ("to" nil "ﾄ") ("tsu" nil "ﾂ") ("tu" nil "ﾂ")
    ("tya" nil "ﾁｬ") ("tye" nil "ﾁｪ") ("tyi" nil "ﾁｨ") ("tyo" nil "ﾁｮ")
    ("tyu" nil "ﾁｭ")
    ("u" nil "ｳ")
    ("vv" "v" "ｯ") ("va" nil "ｳﾞｧ") ("ve" nil "ｳﾞｪ") ("vi" nil "ｳﾞｨ")
    ("vo" nil "ｳﾞｫ") ("vu" nil "ｳﾞ")
    ("ww" "w" "ｯ") ("wa" nil "ﾜ") ("we" nil "ｳｪ") ("wi" nil "ｳｨ")
    ("wo" nil "ｦ") ("wu" nil "ｳ")
    ("xx" "x" "ｯ") ("xa" nil "ｧ") ("xe" nil "ｪ") ("xi" nil "ｨ")
    ("xka" nil "ｶ") ("xke" nil "ｹ") ("xo" nil "ｫ") ("xtsu" nil "ｯ")
    ("xtu" nil "ｯ") ("xu" nil "ｩ") ("xwa" nil "ﾜ") ("xwe" nil "ｪ")
    ("xwi" nil "ｨ") ("xya" nil "ｬ") ("xyo" nil "ｮ") ("xyu" nil "ｭ")
    ("yy" "y" "ｯ") ("ya" nil "ﾔ") ("ye" nil "ｲｪ") ("yo" nil "ﾖ")
    ("yu" nil "ﾕ")
    ("zz" "z" "ｯ") ("z," nil "‥") ("z-" nil "〜") ("z." nil "…")
    ("z/" nil "･") ("z[" nil "『") ("z]" nil "』") ("za" nil "ｻﾞ")
    ("ze" nil "ｾﾞ") ("zh" nil "←") ("zi" nil "ｼﾞ") ("zj" nil "↓")
    ("zk" nil "↑") ("zl" nil "→") ("zo" nil "ｿﾞ") ("zu" nil "ｽﾞ")
    ("zya" nil "ｼﾞｬ") ("zye" nil "ｼﾞｪ") ("zyi" nil "ｼﾞｨ") ("zyo" nil "ｼﾞｮ")
    ("zyu" nil "ｼﾞｭ")
    ("," nil "､") ("." nil "｡") ("-" nil "ｰ") (":" nil ":") (";" nil ";")
    ("?" nil "?") ("[" nil "｢") ("]" nil "｣")
    ("l" nil skk-latin-mode)
    ("q" nil skk-toggle-katakana)
    ("L" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-set-henkan-point-subr)
    ("X" nil skk-purge-from-jisyo)
    ("/" nil skk-abbrev-mode)
    ("$" nil skk-display-code-for-char-at-point)
    ("@" nil skk-today)
    ("\\" nil skk-input-by-code-or-menu))
  "*SKK JISX0201 モードのベースのルール。")

(defvar skk-jisx0201-roman-rule-list
  '(("!" nil "!") ("\"" nil "\"") ("#" nil "#") ("$" nil "$") ("%" nil "%")
    ("&" nil "&") ("'" nil "'") ("\(" nil "(") ("\)" nil ")") ("*" nil "*")
    ("+" nil "+") ("," nil ",") ("-" nil "-") ("." nil ".") ("/" nil "/")
    ("0" nil "0") ("1" nil "1") ("2" nil "2") ("3" nil "3") ("4" nil "4")
    ("5" nil "5") ("6" nil "6") ("7" nil "7") ("8" nil "8") ("9" nil "9")
    (":" nil ":") (";" nil ";") ("<" nil "<") ("=" nil "=") (">" nil ">")
    ("?" nil "?") ("@" nil "@")
    ("A" nil "A") ("B" nil "B") ("C" nil "C") ("D" nil "D") ("E" nil "E")
    ("F" nil "F") ("G" nil "G") ("H" nil "H") ("I" nil "I") ("J" nil "J")
    ("K" nil "K") ("L" nil "L") ("M" nil "M") ("N" nil "N") ("O" nil "O")
    ("P" nil "P") ("Q" nil "Q") ("R" nil "R") ("S" nil "S") ("T" nil "T")
    ("U" nil "U") ("V" nil "V") ("W" nil "W") ("X" nil "X") ("Y" nil "Y")
    ("Z" nil "Z")
    ("[" nil "[") ("\\" nil "\\") ("]" nil "]") ("^" nil "^") ("_" nil "_")
    ("`" nil "`")
    ("a" nil "a") ("b" nil "b") ("c" nil "c") ("d" nil "d") ("e" nil "e")
    ("f" nil "f") ("g" nil "g") ("h" nil "h") ("i" nil "i") ("j" nil "j")
    ("k" nil "k") ("l" nil "l") ("m" nil "m") ("n" nil "n") ("o" nil "o")
    ("p" nil "p") ("q" nil "q") ("r" nil "r") ("s" nil "s") ("t" nil "t")
    ("u" nil "u") ("v" nil "v") ("w" nil "w") ("x" nil "x") ("y" nil "y")
    ("z" nil "z")
    ("{" nil "{") ("|" nil "|") ("}" nil "}") ("~" nil "~") (" " nil " "))
  "*SKK JISX0201 モードの Roman のルール。")

(defvar skk-jisx0201-rule-list
  '(("!" nil "!") ("\"" nil "\"") ("#" nil "#") ("%" nil "%") ("&" nil "&")
    ("'" nil "'") ("\(" nil "(") ("\)" nil ")") ("*" nil "*") ("+" nil "+")
    ("-" nil "-")
    ("1" nil "1") ("2" nil "2") ("3" nil "3") ("4" nil "4") ("5" nil "5")
    ("6" nil "6") ("7" nil "7") ("8" nil "8") ("9" nil "9")
    (":" nil ":") (";" nil ";") ("<" nil "<") ("=" nil "=") (">" nil ">")
    ("?" nil "?") ("@" nil "@")
    ("A" nil "A") ("B" nil "B") ("C" nil "C") ("D" nil "D") ("E" nil "E")
    ("F" nil "F") ("G" nil "G") ("H" nil "H") ("I" nil "I") ("J" nil "J")
    ("K" nil "K") ("L" nil "L") ("M" nil "M") ("N" nil "N") ("O" nil "O")
    ("P" nil "P") ("Q" nil "Q") ("R" nil "R") ("S" nil "S") ("T" nil "T")
    ("U" nil "U") ("V" nil "V") ("W" nil "W") ("X" nil "X") ("Y" nil "Y")
    ("Z" nil "Z")
    ("^" nil "^") ("_" nil "_") ("`" nil "`") ("{" nil "{") ("|" nil "|")
    ("}" nil "}") ("~" nil "~") (" " nil " "))
"*SKK JISX0201 モードの追加のルール。")

(or skk-jisx0201-mode-map
    (let ((map (make-sparse-keymap)))
      (substitute-key-definition 'self-insert-command 'skk-jisx0201-insert map
				 global-map)
      ;; for Mule-2.x
      (substitute-key-definition 'egg-self-insert-command 'skk-jisx0201-insert
				 map global-map)
      (substitute-key-definition 'canna-self-insert-command
				 'skk-jisx0201-insert map global-map)
      (substitute-key-definition 'canna-henkan-region-or-self-insert 'skk-insert
				 map global-map)
      (substitute-key-definition 'can-n-egg-self-insert-command
				 'skk-jisx0201-insert map global-map)
      ;;(define-key map "\C-q" 'skk-jisx0201-henkan)
      (skk-define-menu-bar-map map)
      (setq skk-jisx0201-mode-map map)))

(set-modified-alist
 'minor-mode-map-alist
 (list (cons 'skk-jisx0201-mode skk-jisx0201-mode-map)))

(setq skk-jisx0201-base-rule-tree
      (skk-compile-rule-list skk-jisx0201-base-rule-list skk-jisx0201-rule-list))
(setq skk-jisx0201-roman-rule-tree
      (skk-compile-rule-list skk-jisx0201-roman-rule-list))

;; inline functions.
(defsubst skk-jisx0201-mode-on (&optional arg)
  (setq skk-mode t
        skk-jisx0201-mode t
	skk-jisx0201-roman arg
	skk-jisx0201-rule-tree (if arg
				   skk-jisx0201-roman-rule-tree
				 skk-jisx0201-base-rule-tree)
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        skk-katakana nil)
  (skk-update-modeline 'jisx0201))

;; Pieces of advice.
(defadvice skk-mode (after skk-jisx0201-ad activate)
  (define-key skk-jisx0201-mode-map skk-kakutei-key 'skk-kakutei)
  (setq skk-jisx0201-mode nil))

(defadvice skk-kakutei (after skk-jisx0201-ad activate)
  (and skk-jisx0201-mode (skk-jisx0201-mode-on skk-jisx0201-roman)))

(defadvice skk-latin-mode (after skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil))

(defadvice skk-jisx0208-latin-mode (after skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil))

(defadvice skk-abbrev-mode (after skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil))

(skk-defadvice newline (around skk-jisx0201-ad activate)
  "skk-egg-like-newline が non-nil だったら、変換中の newline で確定のみ行い、改行しない。"
  (interactive "*P")
  (if (not (or skk-jisx0201-mode skk-abbrev-mode))
      ad-do-it
    (let (
	  ;;(arg (ad-get-arg 0))
          ;; skk-kakutei を実行すると skk-henkan-on の値が無条件に nil になる
          ;; ので、保存しておく必要がある。
          (no-newline (and skk-egg-like-newline skk-henkan-on))
	  (auto-fill-function (and (interactive-p) auto-fill-function)))
      ;; fill されても nil が帰ってくる :-<
      ;;(if (skk-kakutei)
      ;;    (setq arg (1- arg)))
      ;;(if skk-mode
      ;;    (let ((opos (point)))
      ;;      ;; skk-kakutei (skk-do-auto-fill) によって行が折り返されたら arg を 1 つ減らす。
      ;;      (skk-kakutei)
      ;;      (if (and (not (= opos (point))) (integerp arg))
      ;;          (ad-set-arg 0 (1- arg)))))
      (and skk-mode (skk-kakutei))
      (if (not no-newline)
	  ad-do-it))))

(skk-defadvice newline-and-indent (around skk-jisx0201-ad activate)
  "skk-egg-like-newline が non-nil だったら、変換中の newline-and-indent で確定のみ行い、改行しない。"
  (if (not (or skk-jisx0201-mode skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline skk-henkan-on))
	  (auto-fill-function (and (interactive-p) auto-fill-function)))
      (and skk-mode (skk-kakutei))
      (or no-newline ad-do-it))))

(skk-defadvice exit-minibuffer (around skk-jisx0201-ad activate)
  "skk-egg-like-newline が non-nil だったら、変換中の exit-minibuffer で確定のみ行う。"
  (skk-remove-minibuffer-setup-hook
   'skk-jisx0201-mode-on 'skk-setup-minibuffer
   (function (lambda ()
	       (add-hook 'pre-command-hook 'skk-pre-command nil 'local))))
  (if (not (or skk-jisx0201-mode skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline skk-henkan-on)))
      (and skk-mode (skk-kakutei))
      (or no-newline ad-do-it))))

;; functions.
;;;###autoload
(defun skk-jisx0201-mode (arg)
  "SKK のモードを JISX0201 モードに変更する。"
  (interactive "P")
  (skk-kakutei)
  (skk-jisx0201-mode-on))

(defun skk-toggle-jisx0201 (arg)
  "半角カナモードとローマ字モードを切り替える。"
  (interactive "P")
  (cond ((and skk-henkan-on (not skk-henkan-active))
	 (skk-jisx0201-henkan arg))
	(t
	 (cond (skk-jisx0201-roman
		(setq skk-jisx0201-rule-tree skk-jisx0201-base-rule-tree)
		(setq skk-jisx0201-roman nil))
	       (t
		(or skk-jisx0201-base-rule-tree
		    (setq skk-jisx0201-base-rule-tree skk-jisx0201-rule-tree))
		(setq skk-jisx0201-rule-tree skk-jisx0201-roman-rule-tree)
		(setq skk-jisx0201-roman t))))))

(defun skk-jisx0201-string-conversion (str func)
  (let ((buf (get-buffer-create " *SKK JIS X 0201 work*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert str)
      (funcall func 1 (point))
      (buffer-string))))

(defun skk-jisx0201-zenkaku (str)
  "STR の JIS X 0201 カナに属する文字列を対応する JIS X 0208 の文字列で置き換え
る。"
  (skk-jisx0201-string-conversion
   str
   (static-cond ((eq skk-emacs-type 'mule2)
		 'zenkaku-katakana-region)
		(t
		 'japanese-zenkaku-region))))

(defun skk-jisx0201-hankaku (str)
  "STR の JIS X 0208 に属する文字列を対応する JIS X 0201 カナの文字列で置き換え
る。"
  (skk-jisx0201-string-conversion
   str
   (static-cond ((eq skk-emacs-type 'mule2)
		 'hankaku-katakana-region)
		(t
		 'japanese-hankaku-region))))

(defun skk-jisx0201-insert (&optional arg)
  "SKK JISX0201 モードの文字入力を行なう。"
  (interactive "*p")
  (skk-with-point-move
   (let ((ch last-command-char))
     (cond (
	    ;; start writing a midasi key.
	    (or (and (not skk-jisx0201-roman)
		     (memq ch skk-set-henkan-point-key)
		     (or skk-okurigana
			 (not (skk-get-prefix skk-jisx0201-current-rule-tree))
			 (not (skk-select-branch skk-jisx0201-current-rule-tree ch))))
		(and skk-henkan-on (memq ch skk-special-midashi-char-list)))
	    ;; normal pattern
	    ;; skk-set-henkan-point -> skk-jisx0201-kana-input.
	    (skk-jisx0201-set-henkan-point arg))
	   ;; start conversion.
	   ((and skk-henkan-on (eq ch skk-start-henkan-char))
	    (let ((jisx0201 (buffer-substring-no-properties
			     skk-henkan-start-point (point)))
		  jisx0208 )
	      (if (and jisx0201 (setq jisx0208 (skk-jisx0201-zenkaku jisx0201)))
		  (progn
		    (insert-before-markers jisx0208)
		    (delete-region skk-henkan-start-point
				   (- (point) (length jisx0208)))))
	      (let ((skk-katakana t)) (skk-start-henkan arg)))
	    (when skk-use-color-cursor
		 (static-cond
		  ((eq skk-emacs-type 'xemacs)
		   (set-face-property
		    'text-cursor 'background (skk-cursor-current-color)
		    (current-buffer)))
		  (t
		   (set-buffer-local-cursor-color (skk-cursor-current-color))))))
	   ;; for completion.
	   ((and skk-henkan-on (not skk-henkan-active))
	    (cond ((eq ch skk-try-completion-char)
		   (setq this-command 'skk-comp-do)
		   (skk-comp (not (eq last-command 'skk-comp-do))))
		  ((and (eq last-command 'skk-comp-do)
			(memq ch (list skk-next-completion-char
				       skk-previous-completion-char)))
		   (skk-comp-previous/next ch))
		  (t (skk-jisx0201-kana-input arg))))
	   ;; just imput JISX0201 Kana.
	   (t (skk-jisx0201-kana-input arg))))))

(defun skk-jisx0201-kana-input (&optional arg)
  ;;"JISX0201 モードの文字の入力を行うルーチン。"
  (let ((echo-keystrokes 0)
	(queue (list last-command-char)))
    (while queue
      (if (not (skk-get-prefix skk-jisx0201-current-rule-tree))
	  (progn
	    (skk-set-marker skk-kana-start-point (point))
	    (setq skk-jisx0201-current-rule-tree skk-jisx0201-rule-tree))
	(skk-erase-prefix))
      (setq skk-prefix (concat (skk-get-prefix skk-jisx0201-current-rule-tree)
			       (char-to-string last-command-char)))
      (let ((next (skk-select-branch skk-jisx0201-current-rule-tree (car queue)))
	    data)
	(if next
	    ;; can go down SKK-JISX0201-CURRENT-RULE-TREE
	    (if (skk-get-branch-list next)
		;; NEXT have at least one branch
		(progn
		  (and skk-henkan-active
		       skk-kakutei-early
		       (not skk-process-okuri-early)
		       (skk-kakutei))
		  (setq queue (cdr queue)
			skk-jisx0201-current-rule-tree next))
	      ;; NEXT does not have any branch (i.e. NEXT is a leaf)
	      (setq data (skk-get-kana next)
		    queue (nconc (string-to-char-list (skk-get-nextstate next))
				 (cdr queue))
		    skk-jisx0201-current-rule-tree nil))
	  ;; can not go down SKK-JISX0201-CURRENT-RULE-TREE
	  (let ((d (skk-get-kana skk-jisx0201-current-rule-tree)))
	    (if d
		;; SKK-JISX0201-CURRENT-RULE-TREE have a roma->kana rule
		(setq data d
		      queue
		      (nconc (string-to-char-list
			      (skk-get-nextstate skk-jisx0201-current-rule-tree))
			     queue )
		      skk-jisx0201-current-rule-tree nil)
	      ;; SKK-JISX0201-CURRENT-RULE-TREE does not have any roma->kana rule
	      (let ((dd (and skk-kana-input-search-function
			     (funcall skk-kana-input-search-function))))
		(if dd
		    (setq data (car dd)
			  queue (nconc (string-to-char-list (cdr dd))
				       (cdr queue))
			  skk-jisx0201-current-rule-tree nil)
		  (if (eq skk-jisx0201-current-rule-tree skk-jisx0201-rule-tree)
		      ;; typo on the root of tree
		      (setq queue nil
			    skk-jisx0201-current-rule-tree nil)
		    ;; otherwise move to root of the tree, and redo
		    (setq skk-jisx0201-current-rule-tree nil)))))))
	(if (not data)
	    (if skk-jisx0201-current-rule-tree
		(progn
		  (or skk-isearch-message (setq prefix-arg arg))
		  (setq skk-prefix (skk-get-prefix skk-jisx0201-current-rule-tree))
		  (skk-insert-prefix skk-prefix))
	      (and skk-henkan-active (skk-kakutei))
	      (setq skk-prefix "")
	      (or queue
		  (skk-emulate-original-map (skk-make-raw-arg arg))))
	  (skk-cancel-undo-boundary)
	  (setq skk-prefix "")
	  (and (functionp data)
	       (setq data (funcall data (skk-make-raw-arg arg))))
	  (if (not (stringp (if (consp data) (car data) data)))
	      nil
	    (let ((pair (and skk-auto-insert-paren
			     (cdr (assoc data skk-auto-paren-string-alist))))
		  (count0 arg) (count1 arg) (inserted 0))
	      (and skk-henkan-active
		   skk-kakutei-early (not skk-process-okuri-early)
		   (skk-kakutei))
	      (while (> count0 0)
		(skk-insert-str data)
		(setq count0 (1- count0)))
	      (if (not pair)
		  nil
		(while (> count1 0)
		  (if (not (string= pair (char-to-string (following-char))))
		      (progn
			(setq inserted (1+ inserted))
			(skk-insert-str pair)))
		  (setq count1 (1- count1)))
		(or (= inserted 0) (backward-char inserted)))
	      (and skk-okurigana (null queue) (skk-set-okurigana))))))
      (and skk-isearch-message (skk-isearch-message)))))

(defun skk-jisx0201-set-henkan-point (&optional arg)
  ;;"変換を開始するポイントをマークし、対応する skk-prefix か、母音を入力する。"
  (let* ((last-char (skk-downcase last-command-char))
	 (normal (not (eq last-char last-command-char)))
	 (sokuon (and (string= skk-prefix (char-to-string last-char))
		      (/= last-char ?o)))
	 (henkan-active skk-henkan-active))
    (if (or (not skk-henkan-on) skk-henkan-active)
	(if normal
	    (skk-jisx0201-set-henkan-point-subr)
	  (and skk-henkan-on (skk-jisx0201-set-henkan-point-subr))
	  (if henkan-active
	      (skk-emulate-original-map arg)
	    ;; What's to be here?
	    ;;(skk-self-insert arg)
	    ))
      (if (not normal)
	  (progn			; special char
	    (insert-and-inherit last-char)
	    (skk-set-marker skk-henkan-end-point (point))
	    (setq skk-henkan-count 0
		  skk-henkan-key (buffer-substring-no-properties
				  skk-henkan-start-point (point))
		  skk-prefix "")
	    (skk-henkan))
	;; prepare for the processing of okurigana if not skk-okurigana
	;; and the preceding character is not a numeric character.
	;; if the previous char is a special midashi char or a
	;; numeric character, we assume that the user intended to type the
	;; last-command-char in lower case.
	(if (and (or (not (skk-get-prefix skk-jisx0201-current-rule-tree))
		     ;; for KAnji, KanJIru
		     (and
		      (not (= skk-henkan-start-point skk-kana-start-point))
		      (or sokuon	; for TaSSi or TasSi
			  (skk-kana-cleanup)))) ; for NEko
		 (not skk-okurigana)
		 (or (= skk-henkan-start-point (point))
		     (let ((p (char-before)))
		       (not
			(or
			 ;; previous char is a special midashi char
			 (memq p skk-special-midashi-char-list)
			 ;; previous char is an ascii numeric char
			 (and (<= ?0 p) (<= p ?9))
			 ;; previous char is a JIS X 0208 numeric char
			 (and (skk-jisx0208-p p)
			      (= (skk-char-octet p 0) 35) ;?#
			      (<= 48 (skk-char-octet p 1)) ; ?0
			      (<= (skk-char-octet p 1) 57)) ; ?9
			 )))))
	    (if skk-process-okuri-early
		(progn
		  (skk-set-marker skk-henkan-end-point (point))
		  (setq skk-okuri-char (char-to-string last-char))
		  (if sokuon
		      (progn
			(setq skk-henkan-key
			      (concat (buffer-substring-no-properties
				       skk-henkan-start-point
				       skk-kana-start-point)
				      "ﾂ"
				      skk-henkan-okurigana))
			(skk-erase-prefix)
			(insert-and-inherit "ﾂ")
			(setq skk-prefix ""
			      skk-henkan-count 0)
			(skk-henkan)
			(delete-backward-char 2))
		    (setq skk-henkan-key (concat
					  (buffer-substring-no-properties
					   skk-henkan-start-point
					   (point))
					  skk-okuri-char))
		    (insert-and-inherit " ")
		    (setq skk-prefix ""
			  skk-henkan-count 0)
		    (skk-henkan)
		    (delete-backward-char 1))
		  ;; we set skk-kana-start-point here, since the marker may no
		  ;; longer point at the correct position after skk-henkan.
		  (skk-set-marker skk-kana-start-point (point)))
	      (if (= skk-henkan-start-point (point))
		  nil
		(if sokuon
		    (progn
		      (skk-erase-prefix 'clean)
		      (insert-and-inherit "ﾂ")))
		(skk-set-marker skk-okurigana-start-point (point))
		(insert-and-inherit "*")
		(skk-set-marker skk-kana-start-point (point))
		(setq skk-okuri-char (char-to-string last-char)
		      skk-okurigana t))))))
    (if normal
	(progn
	  (setq last-command-char last-char)
	  (skk-jisx0201-kana-input arg)))))

(defun skk-jisx0201-set-henkan-point-subr (&optional arg)
  "かなを入力した後で、ポイントに変換開始のマーク \(▽\) を付ける。
元々はこの関数は skk-set-henkan-point の内部関数である。"
  (interactive "*P")
  (skk-with-point-move
   (cancel-undo-boundary)
   (if skk-henkan-on (skk-kakutei)
     (skk-kana-cleanup));; XXX
   (if (not (skk-get-prefix skk-jisx0201-current-rule-tree))
       (insert-and-inherit "▽")
     (skk-erase-prefix)
     (insert-and-inherit "▽")
     (skk-set-marker skk-kana-start-point (point))
     (skk-insert-prefix))
   (setq skk-henkan-on t)
   (skk-set-marker skk-henkan-start-point (point))))

;;;###autoload
(defun skk-toggle-katakana (arg)
  (interactive "P")
  (if (and skk-henkan-on (not skk-henkan-active))
      (skk-jisx0201-henkan arg)
    (if skk-jisx0201-mode
	(progn
	  (setq skk-jisx0201-mode nil)
	  (skk-j-mode-on 'katakana)
	  (skk-update-modeline 'katakana))
      (skk-jisx0201-mode-on)
      (skk-update-modeline 'jisx0201)))
  (when skk-use-color-cursor
    (static-cond
     ((eq skk-emacs-type 'xemacs)
      (set-face-property
       'text-cursor 'background (skk-cursor-current-color)
       (current-buffer)))
     (t
      (set-buffer-local-cursor-color (skk-cursor-current-color))))))

(defun skk-jisx0201-henkan (arg)
  "▽モードであれば、リージョンのひらがな/カタカナをﾊﾝｶｸｶﾀｶﾅに変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (skk-with-point-move
   (if skk-henkan-on
       (if skk-henkan-active
	   nil
	 (skk-set-marker skk-henkan-end-point (point))
	 (skk-*-henkan-1 'skk-jisx0201-region skk-henkan-start-point
			 skk-henkan-end-point 'vcontract))
     (skk-emulate-original-map arg))))

(defun skk-jisx0201-region (start end &optional vcontract)
  "リージョンのひらがな/カタカナをﾊﾝｶｸｶﾀｶﾅに変換する。
オプショナル引数の VCONTRACT が non-nil であれば、\"う゛\" を \"ｳﾞ\" に変換す
る。
引数の START と END は数字でもマーカーでも良い。"
  (interactive "*r\nP")
  (setq end (set-marker (make-marker) end))
  (skk-hiragana-to-jisx0201-region start end vcontract)
  (skk-katakana-to-jisx0201-region start end vcontract)
  (set-marker end nil))

(defun skk-hiragana-to-jisx0201-region
  (start end &optional vcontract latin-jisx0201)
  (skk-search-and-replace
   start end "[ぁ-ん]+"
   (lambda (matched) (save-match-data (skk-jisx0201-hankaku matched))))
  (if vcontract
      (skk-search-and-replace
       start end "う゛" (lambda (matched) "ｳﾞ")))
  (if latin-jisx0201
      nil
      ;; not yet
      ))

(defun skk-katakana-to-jisx0201-region
  (start end &optional vcontract latin-jisx0201)
  (skk-search-and-replace
   start end "[ァ-ン]+"
   (lambda (matched) (save-match-data (skk-jisx0201-hankaku matched))))
  (if vcontract
      (skk-search-and-replace
       start end "ヴ" (lambda (matched) "ｳﾞ")))
  (if latin-jisx0201
      nil
      ;; not yet
      ))

;;

(define-key skk-jisx0201-mode-map skk-kakutei-key 'skk-kakutei)
(define-key skk-jisx0201-mode-map "\C-q" 'skk-toggle-katakana)
(define-key skk-jisx0201-mode-map "\C-c\C-q" 'skk-toggle-jisx0201)
(define-key skk-j-mode-map "\C-q" 'skk-toggle-katakana)

(require 'product)
(product-provide (provide 'skk-jisx0201) (require 'skk-version))
;;; Local Variables:
;;; eval: (cond ((boundp 'standard-translation-table-for-decode) (setq standard-translation-table-for-decode (make-translation-table nil)))((boundp 'standard-character-unification-table-for-decode) (setq standard-character-unification-table-for-decode (make-unification-table nil))))
;;; End:
;;; skk-jisx0201.el ends here
