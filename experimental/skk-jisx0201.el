;;; skk-jisx0201.el --- SKK 用 JISX 0201 カナ入力プログラム
;; Copyright (C) 1999 Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-jisx0201.el,v 1.1 1999/10/31 10:52:16 minakaji Exp $
;; Keywords: japanese
;; Created: Oct. 30, 1999.
;; Last Modified: $Date: 1999/10/31 10:52:16 $

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
;; (require 'skk-jisx0201)
;;
;; と書くことでインストールされます。使い方は以下のようになります。
;;
;; ◎カタカナモードにおいて、
;;   ・"C-q" で全角カナモードと半角カナモードを切りかえます。
;; ◎ひらがな/カタカナ両モード内での▽モードにおいて、
;;   ・"C-q" を押すと､見出し語として入力されたひらがな/カタカナをﾊﾝｶｸｶﾀｶﾅに変換します｡
;;
;;; Code:
(require 'skk)

;; user variables.
(defgroup skk-jisx0201 nil "SKK jisx0201 related customization."
  :prefix "skk-jisx0201-"
  :group 'skk )

(defcustom skk-jisx0201-cursor-color (if (eq skk-background-mode 'light)
					 "green"
				       "forestgreen" )
  "*JISX0201 モードを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。"
  :type 'string
  :group 'skk )

(defcustom skk-jisx0201-mode-string " ｶﾅ"
  "*SKK が latin (ascii) モードであるときにモードラインに表示される文字列。"
  :type 'string
  :group 'skk )

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
    ;;("h" "" "ｵ")
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
    ("," nil "､")
    ("." nil "｡")
    ("-" nil "ｰ")
    (":" nil ":")
    (";" nil ";")
    ("?" nil "?")
    ("[" nil "｢")
    ("]" nil "｣")
    ("l" nil skk-latin-mode)
    ;; ("q" nil skk-toggle-kana)
    ("L" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-set-henkan-point-subr)
    ("X" nil skk-purge-from-jisyo)
    ("/" nil skk-abbrev-mode)
    ("$" nil skk-display-code-for-char-at-point)
    ("@" nil skk-today)
    ("\\" nil skk-input-by-code-or-menu)
    )
  "*SKK JISX0201 カナモードのベースのルール。")

(defvar skk-jisx0201-rule-list nil
  "*SKK JISX0201 カナモードの追加のルール。")

(defvar skk-jisx0201-mode-map nil
  "*SKK JISX0201 モードのキーマップ。")
(or skk-jisx0201-mode-map
    (let ((map (make-sparse-keymap)))
      (substitute-key-definition 'self-insert-command 'skk-jisx0201-insert map
				 global-map)
      ;; for Mule-2.x
      (substitute-key-definition 'egg-self-insert-command 'skk-jisx0201-insert
				 map global-map)
      (substitute-key-definition 'canna-self-insert-command
				 'skk-jisx0201-insert map global-map)
      (substitute-key-definition 'can-n-egg-self-insert-command
				 'skk-jisx0201-insert map global-map)
      ;;(define-key map "\C-q" 'skk-jisx0201-henkan)
      (skk-define-menu-bar-map map)
      (setq skk-jisx0201-mode-map map) ))

;; system variables.
(defvar skk-jisx0201-rule-tree nil
  "ローマ字 -> JISX0201 変換の状態遷移規則を表すツリーの初期状態。
skk-mode の起動時に毎回 skk-rom-kana-base-rule-list と
skk-rom-kana-rule-list から木の形にコンパイルされる。" )

(skk-deflocalvar skk-jisx0201-current-rule-tree nil
  "ローマ字 -> JISX0201 変換の状態遷移規則を表わすツリーの現時点の状態。
ローマ字入力の初期では skk-jisx0201-rule-tree と同一の状態で、文字入力が進むに
つれ、木をたどってゆく状態の遷移を表す。" )

(skk-deflocalvar skk-jisx0201-mode nil
  "Non-nil であれば、入力モードが JISX0201 モードであることを示す。" )

(set-modified-alist
 'minor-mode-map-alist
 (list (cons 'skk-jisx0201-mode skk-jisx0201-mode-map)) )

;; inline functions.
(defsubst skk-jisx0201-mode-on ()
  (setq skk-mode t
        skk-jisx0201-mode t
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        skk-katakana nil
        skk-input-mode-string skk-jisx0201-mode-string )
  (skk-set-cursor-color skk-jisx0201-cursor-color)
  (force-mode-line-update) )

;; advices.
(defadvice skk-regularize (before skk-jisx0201-ad activate)
  (setq skk-jisx0201-rule-tree
	(skk-compile-rule-list skk-jisx0201-base-rule-list skk-jisx0201-rule-list) ))

(defadvice skk-mode (after skk-jisx0201-ad activate)
  (define-key skk-jisx0201-mode-map skk-kakutei-key 'skk-kakutei) )

(defadvice skk-kakutei (after skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil) )

(defadvice skk-latin-mode (after skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil) )

(defadvice skk-jisx0208-latin-mode (after skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil) )

(defadvice skk-abbrev-mode (after skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil) )

;; functions.
;;;###autoload
(defun skk-jisx0201-mode (arg)
  "SKK のモードを JISX0201 モードに変更する。"
  (interactive "P")
  (skk-kakutei)
  (skk-jisx0201-mode-on) )

(defun skk-jisx0201-insert (&optional arg)
  "SKK JISX0201 モードの文字入力を行なう。"
  (interactive "p*")
  (skk-with-point-move
   (let ((ch last-command-char))
     (cond (
	    ;; start writing a midasi key.
	    (or (and (memq ch skk-set-henkan-point-key)
		     (or skk-okurigana
			 (not (skk-get-prefix skk-jisx0201-current-rule-tree))
			 (not (skk-select-branch skk-jisx0201-current-rule-tree ch)) ))
		(and skk-henkan-on (memq ch skk-special-midashi-char-list)) )
	    ;; normal pattern
	    ;; skk-set-henkan-point -> skk-kana-input.
	    (skk-set-henkan-point arg) )
	   ;; start conversion.
	   ((and skk-henkan-on (eq ch skk-start-henkan-char))
	    (skk-start-henkan arg) )
	   ;; for completion.
	   ((and skk-henkan-on (not skk-henkan-active))
	    (cond ((eq ch skk-try-completion-char)
		   (setq this-command 'skk-completion)
		   (skk-completion (not (eq last-command 'skk-completion))) )
		  ((eq last-command 'skk-completion)
		   (cond ((eq ch skk-next-completion-char)
			  (setq this-command 'skk-completion)
			  (skk-completion nil) )
			 ((eq ch skk-previous-completion-char)
			  (skk-previous-completion) )))
		  (t (skk-kana-input arg)) ))
	   ;; just imput JISX0201 Kana.
	   (t (skk-jisx0201-kana-input arg)) ))))

(defun skk-jisx0201-kana-input (&optional arg)
  ;;"JISX0201 モードの文字の入力を行うルーチン。"
  (let ((echo-keystrokes 0)
	(queue (list last-command-char)) )
    (while queue
      (if (not (skk-get-prefix skk-jisx0201-current-rule-tree))
	  (progn
	    (skk-set-marker skk-kana-start-point (point))
	    (setq skk-jisx0201-current-rule-tree skk-jisx0201-rule-tree) )
	(skk-erase-prefix) )
      (setq skk-prefix (concat (skk-get-prefix skk-jisx0201-current-rule-tree)
			       (char-to-string last-command-char)))
      (let ((next (skk-select-branch skk-jisx0201-current-rule-tree (car queue)))
	    data )
	(if next
	    ;; can go down SKK-JISX0201-CURRENT-RULE-TREE
	    (if (skk-get-branch-list next)
		;; NEXT have at least one branch
		(progn
		  (and skk-henkan-active
		       skk-kakutei-early
		       (not skk-process-okuri-early)
		       (skk-kakutei) )
		  (setq queue (cdr queue)
			skk-jisx0201-current-rule-tree next ))
	      ;; NEXT does not have any branch (i.e. NEXT is a leaf)
	      (setq data (skk-get-kana next)
		    queue (nconc (string-to-char-list (skk-get-nextstate next))
				 (cdr queue) )
		    skk-jisx0201-current-rule-tree nil ))
	  ;; can not go down SKK-JISX0201-CURRENT-RULE-TREE
	  (let ((d (skk-get-kana skk-jisx0201-current-rule-tree)))
	    (if d
		;; SKK-JISX0201-CURRENT-RULE-TREE have a roma->kana rule
		(setq data d
		      queue
		      (nconc (string-to-char-list
			      (skk-get-nextstate skk-jisx0201-current-rule-tree) )
			     queue )
		      skk-jisx0201-current-rule-tree nil )
	      ;; SKK-JISX0201-CURRENT-RULE-TREE does not have any roma->kana rule
	      (let ((dd (and skk-kana-input-search-function
			     (funcall skk-kana-input-search-function) )))
		(if dd
		    (setq data (car dd)
			  queue (nconc (string-to-char-list (cdr dd))
				       (cdr queue) )
			  skk-jisx0201-current-rule-tree nil )
		  (if (eq skk-jisx0201-current-rule-tree skk-jisx0201-rule-tree)
		      ;; typo on the root of tree
		      (setq queue nil
			    skk-jisx0201-current-rule-tree nil )
		    ;; otherwise move to root of the tree, and redo
		    (setq skk-jisx0201-current-rule-tree nil) ))))))
	(if (not data)
	    (if skk-jisx0201-current-rule-tree
		(progn
		  (or skk-isearch-message (setq prefix-arg arg))
		  (setq skk-prefix (skk-get-prefix skk-jisx0201-current-rule-tree))
		  (skk-insert-prefix skk-prefix) )
	      (and skk-henkan-active (skk-kakutei))
	      (setq skk-prefix "")
	      (or queue
		  (skk-emulate-original-map (skk-make-raw-arg arg)) ))
	  (skk-cancel-undo-boundary)
	  (setq skk-prefix "")
	  (and (functionp data)
	       (setq data (funcall data (skk-make-raw-arg arg))) )
	  (if (not (stringp (if (consp data) (car data) data)))
	      nil
	    (let* ((str (if (consp data) (if skk-katakana (car data) (cdr data))
			  data ))
		   (pair (and skk-auto-insert-paren
			      (cdr (assoc str skk-auto-paren-string-alist)) ))
		   (count0 arg) (count1 arg) (inserted 0) )
	      (and skk-henkan-active
		   skk-kakutei-early (not skk-process-okuri-early)
		   (skk-kakutei) )
	      (while (> count0 0)
		(skk-insert-str str)
		(setq count0 (1- count0)) )
	      (if (not pair)
		  nil
		(while (> count1 0)
		  (if (not (string= pair (char-to-string (following-char))))
		      (progn
			(setq inserted (1+ inserted))
			(skk-insert-str pair) ))
		  (setq count1 (1- count1)) )
		(or (= inserted 0) (backward-char inserted)) )
	      (and skk-okurigana (null queue) (skk-set-okurigana)) ))))
      (and skk-isearch-message (skk-isearch-message)) )))

(defun skk-toggle-katakana (arg)
  (interactive "P")
  (if (and skk-henkan-on (not skk-henkan-active))
      (skk-jisx0201-henkan arg)
    (if skk-jisx0201-mode
	(progn
	  (setq skk-jisx0201-mode nil)
	  (skk-j-mode-on 'katakana) )
      (skk-jisx0201-mode-on) )))

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
			 skk-henkan-end-point 'vcontract ))
     (skk-emulate-original-map arg) )))

(defun skk-jisx0201-region (start end &optional vcontract)
  "リージョンのひらがな/カタカナをﾊﾝｶｸｶﾀｶﾅに変換する。
オプショナル引数の VCONTRACT が non-nil であれば、\"う゛\" を \"ｳﾞ\" に変換す
る。
引数の START と END は数字でもマーカーでも良い。"
  (interactive "*r\nP")
  (setq end (set-marker (make-marker) end))
  (skk-hiragana-to-jisx0201-region start end vcontract)
  (skk-katakana-to-jisx0201-region start end vcontract)
  (set-marker end nil)
  (skk-set-cursor-properly) )

(defun skk-hiragana-to-jisx0201-region (start end &optional vcontract)
  (save-match-data
    (let (object jisx0201)
      (skk-save-point
       (goto-char start)
       (while (re-search-forward  "[ぁ-ん]+" end 'noerror)
	 (setq object (buffer-substring-no-properties
		       (match-beginning 0) (match-end 0) )
	       jisx0201 (save-match-data (japanese-hankaku object)) )
	 (backward-char (skk-str-length object))
	 ;; firstly insert a new string, secondly delete an old string to save
	 ;; the cursor position.
	 (insert-and-inherit jisx0201)
	 (delete-region (+ (match-beginning 0) (length jisx0201))
			(+ (match-end 0) (length jisx0201)) ))
       (if vcontract
	   (progn
	     (goto-char start)
	     (while (re-search-forward  "ウ゛" end 'noerror)
	       (backward-char (skk-str-length "ウ゛"))
	       (let ((vu-len (length "ｳﾞ")))
		 (insert-and-inherit "ｳﾞ")
		 (delete-region (+ (match-beginning 0) vu-len)
				(+ (match-end 0) vu-len) )))))))))

(defun skk-katakana-to-jisx0201-region (start end &optional vcontract)
  (save-match-data
    (let (object jisx0201)
      (skk-save-point
       (goto-char start)
       (while (re-search-forward  "[ァ-ン]+" end 'noerror)
	 (setq object (buffer-substring-no-properties
		       (match-beginning 0) (match-end 0) )
	       jisx0201 (save-match-data (japanese-hankaku object)) )
	 (backward-char (skk-str-length object))
	 ;; firstly insert a new string, secondly delete an old string to save
	 ;; the cursor position.
	 (insert-and-inherit jisx0201)
	 (delete-region (+ (match-beginning 0) (length jisx0201))
			(+ (match-end 0) (length jisx0201)) ))
       (if vcontract
	   (progn
	     (goto-char start)
	     (while (re-search-forward  "う゛" end 'noerror)
	       (backward-char (skk-str-length "う゛"))
	       (let ((vu-len (length "ｳﾞ")))
		 (insert-and-inherit "ｳﾞ")
		 (delete-region (+ (match-beginning 0) vu-len)
				(+ (match-end 0) vu-len) )))))))))

;; overwrite the function of same name in skk.el
(defun skk-setup-modeline ()
  "モード行へのステータス表示を準備する。"
  (cond ((eq skk-status-indicator 'left)
	 (mapcar (function
		  (lambda (el)
		    (let ((sym (car el))
			  (strs (cdr el)))
		      (if (string= (symbol-value sym) (cdr strs))
			  (set sym (car strs)) ))))
		 '((skk-latin-mode-string . ("--SKK:" . " SKK"))
		   (skk-hiragana-mode-string . ("--かな:" . " かな"))
		   (skk-katakana-mode-string . ("--カナ:" . " カナ"))
		   (skk-jisx0208-latin-mode-string . ("--全英:" . " 全英"))
		   (skk-abbrev-mode-string . ("--aあ:" . " aあ"))
		   (skk-jisx0201-mode-string . ("--ｶﾅ" . " ｶﾅ")) ))
	 (cond ((featurep 'xemacs)
		(or (memq 'skk-input-mode-string default-mode-line-format)
		    (setq-default default-modeline-format
				  (append '("" skk-input-mode-string)
					  default-modeline-format) ))
		(mapc
		 (function
		  (lambda (buf)
		    (if (buffer-live-p buf)
			(save-excursion
			  (set-buffer buf)
			  (or (memq 'skk-input-mode-string modeline-format)
			      (setq modeline-format
				    (append '("" skk-input-mode-string)
					    modeline-format) ))))))
		 (buffer-list) ))
	       (t
		(or (memq 'skk-input-mode-string mode-line-format)
		    (setq-default
		     mode-line-format
		     (append '("" skk-input-mode-string)
			     mode-line-format) ))))
	 (setq-default skk-input-mode-string "")
	 (force-mode-line-update t) )
	(t
	 (setq minor-mode-alist
	       (put-alist 'skk-mode
			  ;; each element of minor-mode-alist is not cons cell.
			  '(skk-input-mode-string) minor-mode-alist) ))))

(define-key skk-jisx0201-mode-map skk-kakutei-key 'skk-kakutei)
(define-key skk-jisx0201-mode-map "\C-q" 'skk-toggle-katakana)
(define-key skk-j-mode-map "\C-q" 'skk-toggle-katakana)

(provide 'skk-jisx0201)
;;; Local Variables:
;;; End:
;;; skk-jisx0201.el ends here
