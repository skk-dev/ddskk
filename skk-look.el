;;; skk-look.el --- UNIX look command interface for SKK

;; Copyright (C) 1998, 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-look.el,v 1.20 2001/11/18 16:28:11 czkmt Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2001/11/18 16:28:11 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; Daredevil SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; <How to work>
;; .skk か .emacs で `skk-use-look' を t にセットしてこれを評価して下さい。その
;; 後 skk-mode を立ち上げるか、M-x skk-restart すると、 下記のような芸当が可能
;; になります。
;;
;; (1)英単語を補完ができます。
;;
;;    ▽abstr(TAB) ---> ▽abstract
;;
;;    通常の補完機能同様、`.' で次の補完候補、`,' でひとつ前の補完候補に
;;    移動できます。
;;
;;    SKK 形式の英和辞書があれば、ここから SPC を押して英和変換ができます
;;    ね。
;;
;; (2)英単語をあいまいに変換して取り出すことができます。
;;
;;    ▽abstr* (SPC) ---> ▼abstract
;;
;;    見出し語にアスタリスク (`*') を入れるのをお忘れなく。
;;
;;    確定すると、`abstr*' を見出し語、`abstract' を候補とするエントリが個人辞
;;    書に追加されます。このようなエントリを追加したくない場合は、
;;    ユーザー変数、`skk-search-excluding-word-pattern-function' を適切に
;;    設定することで、これを実現することができます。詳しくは、
;;    `skk-search-excluding-word-pattern-function' のドキュメントをご覧下さい。
;;
;; (3)(2)で変換した後、更に再帰的な英和変換を行なうことができます。
;;
;;    まず、`skk-look-recursive-search' の値を non-nil にセットして下さ
;;    い。Emacs/SKK を再起動する必要はありません。
;;
;;    すると、例えば、
;;
;;    ▽abstr* (SPC)
;;
;;      ---> ▼abstract (SPC) -> ▼アブストラクト (SPC) -> ▼抽象 (SPC)
;;        -> ▼abstraction (SPC) -> ▼アブストラクション
;;
;;    このように英単語 + その英単語を見出し語にした候補の「セット」を変換
;;    結果として出力することができます。
;;
;;    この際、`skk-look-expanded-word-only' の値が non-nil であれば、再帰
;;    検索に成功した英単語の「セット」だけを出力することができます (再帰
;;    検索で検出されなかった英単語は無視して出力しません) 。
;;
;;    もちろん、SKK 辞書に
;;
;;       abstract /アブストラクト/抽象/
;;       abstraction /アブストラクション/
;;
;;    というエントリがあることを前提としています。edict を SKK 辞書形式に
;;    変換すると良いですね。
;;
;; 動作確認を行なった look は、Slackware 3.5 に入っていた、man page に
;; `BSD Experimental June 14, 1993' と記載のあるもの (バージョン情報がない)
;; にて行なっています。オプションの指定などが異なる look があれば、ご一報下さ
;; い。よろしくお願いいたします。

;; <Dictionary>
;; ftp://ftp.u-aizu.ac.jp:/pub/SciEng/nihongo/ftp.cc.monash.edu.au/
;; に置いてある edict を利用すると手軽に英和辞書ができます。
;;
;;   % jgawk -f skk-10/lisp/look/edict2skk.awk edict > temp
;;   % skkdic-expr temp | skkdic-sort > SKK-JISYO.E2J
;;   % rm temp
;;
;; できた SKK-JISYO.E2J の利用方法は色々ありますが、
;;
;;   % skkdic-expr SKK-JISYO.E2J + /usr/local/share/skk/SKK-JISYO.L\
;;     | skkdic-sort > SKK-JISYO.L
;;
;; などとして、SKK-JISYO.L とマージして使うのが手軽です。

;; <Motivation>
;; このプログラムは、eWnn for Linux/FreeBSD の広告に類似の機能紹介があったのを
;; 見て、「こんな機能なら SKK 上にすぐインプリメントできるさ」と思うとたまらく
;; なって書いてしまいました。eWnn に負けるな、SKK!
;;
;; 昔、Seiichi Namba <sn@asahi-net.email.ne.jp> さんと一緒に Emacs Lisp で
;; look interface を書いたことがあるのですが、今回はその際の経験を生かすことが
;; できました。難波さんに感謝いたします。

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars)
  ;; shut up compiler warnings.
  (defvar ispell-process)
  (defvar ispell-filter)
  (defvar ispell-filter))

(eval-and-compile
  (autoload 'ispell-accept-buffer-local-defs "ispell")
  (autoload 'ispell-parse-output "ispell"))

(when (and skk-look-command
	   (null (member '(skk-look)
			 (default-value 'skk-search-prog-list))))
  (let ((pl (default-value 'skk-search-prog-list))
	(n 0)
	dic mark)
    (while pl
      (setq dic (car pl))
      (if (memq (nth 1 dic) '(skk-jisyo skk-rdbms-private-jisyo-table))
	  (setq mark n
		pl nil)
	(setq pl (cdr pl)
	      n (1+ n))))
    (cond
     (mark
      (skk-splice-in (default-value 'skk-search-prog-list)
		     (1+ mark)
		     '((skk-look))))
     (t
      (setq-default skk-search-prog-list
		    (skk-nunion
		     (default-value 'skk-search-prog-list)
		     '((skk-look))))))))

;; program
;;;###autoload
(defun skk-look ()
  ;; UNIX look コマンドを利用した変換を行なう。
  ;; SKK abbrev モードにて、英文字 + アスタリスクで uncompleted spelling を指定
  ;; する。
  (when (and skk-use-look
	     skk-abbrev-mode
	     (eq (skk-str-ref skk-henkan-key (1- (length skk-henkan-key)))
		 ?*))
    (let* ((args (substring skk-henkan-key 0 (1- (length skk-henkan-key))))
	   (v (if skk-look-use-ispell
		  (skk-look-ispell args)
		(skk-look-1 args)))
	   skk-henkan-key
	   skk-use-look
	   v2 v3)
      (cond
       ((not skk-look-recursive-search)
	v)
       (t
	(dolist (key v)
	  (let ((skk-current-search-prog-list
		 (copy-sequence skk-search-prog-list)))
	    (setq skk-henkan-key key)
	    (while skk-current-search-prog-list
	      (setq v3 (let (skk-use-numeric-conversion)
			 (skk-search))
		    v2 (if (or (not skk-look-expanded-word-only)
			       v3)
			   (skk-nunion v2 (cons key v3))
			 v2)))))
	v2)))))

(defun skk-look-1 (args)
  ;; core search engine
  (with-temp-buffer
    (let ((word args)
	  opt)
      (setq args (list args))
      (when skk-look-dictionary
	(nconc args (list skk-look-dictionary)))
      (when skk-look-dictionary-order
	(setq opt "d"))
      (when skk-look-ignore-case
	(setq opt (concat "f" opt)))
      (when skk-look-use-alternate-dictionary
	(setq opt (concat "a" opt)))
      (when opt
	(setq args (cons (concat "-" opt) args)))
      (when skk-look-termination-character
	(setq args
	      (cons (list "-t" skk-look-termination-character) args)))
      (when (and (= 0 (apply 'call-process skk-look-command nil t nil args))
		 (> (buffer-size) 0))
	(delete word (split-string (buffer-substring-no-properties
				    (point-min) (1- (point-max)))
				   "\n"))))))

;;;###autoload
(defun skk-look-completion ()
  (unless skk-look-completion-words
    (let ((stacked skk-comp-stack)) ; 他の機能による補完候補。
      ;; look は複数の候補を吐くので、一旦貯めておいて、
      ;; 一つずつ complete する。
      (setq skk-look-completion-words
	    (if skk-look-use-ispell
		(skk-look-ispell skk-comp-key)
	      (skk-look-1 skk-comp-key)))
      (dolist (word stacked)
	(setq skk-look-completion-words
	      (delete word skk-look-completion-words)))
      ;;skk-look-completion-words の各要素は、実際に補完を行なった段階で
      ;; `skk-completion' により skk-comp-stack に入れられる。
      ))
  (pop skk-look-completion-words))

(defadvice skk-kakutei-initialize (after skk-look-ad activate)
  (setq skk-look-completion-words nil))


;;;###autoload
(defun skk-look-ispell (word)
  (require 'ispell)
  (ispell-accept-buffer-local-defs)
  (message "")
  (process-send-string ispell-process "%\n") ;put in verbose mode
  (process-send-string ispell-process (concat "^" word "\n"))
  (while (progn
	   (accept-process-output ispell-process)
	   (not (string= "" (car ispell-filter)))))
  (setq ispell-filter (cdr ispell-filter)) ; remove extra \n
  (let ((poss (when (and ispell-filter
			 (listp ispell-filter))
		;; 1: t for an exact match.
		;; 2: A string containing the root word matched via suffix
		;;    removal.
		;; 3: A list of possible correct spellings of the format:
		;;    (ORIGINAL-WORD OFFSET MISS-LIST GUESS-LIST)
		;;    ORIGINAL-WORD is a string of the possibly misspelled
		;;    word.
		;;    OFFSET is an integer giving the line offset of the word.
		;;    MISS-LIST and GUESS-LIST are possibly null lists of
		;;    guesses and misses.
		;; 4: Nil when an error has occurred."
		(or (ispell-parse-output (car ispell-filter))
		    'error)))
	ret var)
    (setq ispell-filter nil)
    (cond
     ((eq poss 'error)
      (skk-message "ispell process でエラーが発生しました"
		   "error in ispell process")
      (sit-for 1)
      (message "")
      nil)
     ((or (eq poss t)
	  ;; root word に対して skk-look-1 かけちゃおうか？
	  ;; でもちっとも補完ぢゃなくなっちまいますね... (^^;;。
	  (stringp poss)
	  (null (or (nth 2 poss) (nth 3 poss))))
      (skk-look-1 word))
     (t
      (setq var (nconc (nth 2 poss) (nth 3 poss)))
      (dolist (key var)
	;; call look command by each candidate put out by ispell.
	(setq ret (skk-nunion ret (cons key (skk-look-1 key)))))
      (delete word (skk-nunion (skk-look-1 word) ret))))))

(require 'product)
(product-provide
    (provide 'skk-look)
  (require 'skk-version))

;;; skk-look.el ends here
