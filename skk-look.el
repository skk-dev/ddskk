;; -*-byte-compile-dynamic: t;-*-
;;; skk-look.el --- UNIX look command interface for SKK
;; Copyright (C) 1998, 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-look.el,v 1.6 1999/11/28 04:46:02 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/11/28 04:46:02 $

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
;; <How to work>
;; .skk か .emacs で `skk-use-look' を t にセットしてこれを評価して下さい。その
;; 後 skk-mode を立ち上げるか、M-x skk-restart すると、SKK abbrev モードで、
;; UNIX look コマンドを利用した英文字 + アスタリスクの変換ができるようになりま
;; す。こんな感じです。
;;
;; ▽confere* (SPC)
;; ---> ▼conference
;;
;; 確定すると、`confere*' を見出し語、`conference' を候補とするエントリが個人辞
;; 書に追加されます。このようなエントリを追加したくない場合は、
;; skk.el のユーザー変数、`skk-search-excluding-word-pattern-function' を適切に
;; 設定することで、これを実現することができます。詳しくは、
;; `skk-search-excluding-word-pattern-function' のドキュメントをご覧下さい。
;;
;; `skk-look-recursive-search' の値を non-nil にすると、look が見つけた英単語を
;; 見出し語にして、再帰的に SKK 辞書内を検索することができます。例えば、いずれか
;; の SKK 辞書に
;;
;;  abstract /アブストラクト/抽象/
;;  abstraction /アブストラクション/
;;
;; というエントリがある場合、
;;
;;  ▽abs* (SPC)
;;
;;  ---> ▼abstract (SPC) -> ▼アブストラクト (SPC) -> ▼抽象 (SPC)
;;       -> ▼abstraction (SPC) -> ▼アブストラクション
;;
;; のように英単語 + その英単語を見出し語にした候補の「セット」を変換結果として出
;; 力することができます。この際、`skk-look-expanded-word-only' の値が non-nil で
;; あれば、再帰検索に成功した英単語の「セット」だけ (再帰検索で検出されなかった
;; 英単語は無視する) を出力することができます。
;;
;; abbrev モードで補完を行なうと、個人辞書を検索し尽した後で、look コマンドによる
;; 英単語補完を行ないます。例えば、こんな感じに動作します。
;;
;;  ▽confe (TAB)
;;  ---> ▽conference
;;
;; 動作確認を行なった look は、Slackware 3.5 に入っていた、man page に
;; `BSD Experimental June 14, 1993' と記載のあるもの (バージョン情報がない) にて
;; 行なっています。オプションの指定などが異なる look があれば、ご一報下さい。よろ
;; しくお願いいたします。

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
;;   % skkdic-expr SKK-JISYO.E2J + /usr/local/share/skk/SKK-JISYO.L | skkdic-sort > SKK-JISYO.L
;;
;; などとして、SKK-JISYO.L とマージして使うのが手軽です。

;; <Motivation>
;; このプログラムは、eWnn for Linux/FreeBSD の広告に類似の機能紹介があったのを見
;; て、「こんな機能なら SKK 上にすぐインプリメントできるさ」と思うとたまらくなっ
;; て書いてしまいました。eWnn に負けるな、SKK!
;;
;; 昔、Seiichi Namba <sn@asahi-net.email.ne.jp> さんと一緒に Emacs Lisp で
;; look interface を書いたことがあるのですが、今回はその際の経験を生かすことができ
;; ました。難波さんに感謝いたします。

;;; Code:
(eval-when-compile (require 'skk) (require 'skk-comp))
(require 'skk-foreword)
;; Elib
(require 'stack-m)
;; APEL
(require 'path-util)

;;;###autoload
(defgroup skk-look nil "SKK look conversion related customization."
  :prefix "skk-look-"
  :group 'skk )

;; user variable.
(defcustom skk-look-command (exec-installed-p "look")
  "*UNIX look コマンドの名前。"
  :type 'file
  :group 'skk-look )

(defcustom skk-look-ignore-case t
  "*Non-nil であれば、大文字・小文字を区別しないで検索を行なう。
look コマンドにオプション \"-f\" を渡す。"
  :type 'boolean
  :group 'skk-look )

(defcustom skk-look-dictionary-order t
  "*Non-nil であれば、辞書順にソートされた検索ファイルを使用する。
look コマンドにオプション \"-d\" を渡す。"
  :type 'boolean
  :group 'skk-look )

(defcustom skk-look-use-alternate-dictionary nil
  "*Non-nil であれば、/usr/dict/web2 を使い検索を行なう。
ディフォルトの辞書は、/usr/dict/words。
look コマンドにオプション \"-a\" を渡す。"
  :type '(choice file (const nil))
  :group 'skk-look )

(defcustom skk-look-termination-character nil
  "*Non-nil であれば、その文字列を UNIX look コマンドが使う終端文字列として明示的に指定する。
look コマンドにオプション \"-t\" とその文字列を渡す。"
  :type '(choice string (const nil))
  :group 'skk-look )

(defcustom skk-look-dictionary nil
  "*look コマンドが検索する辞書ファイル。
nil であれば、/usr/dict/words を使用する。"
  :type '(choice file (const nil))
  :group 'skk-look )

(defcustom skk-look-recursive-search nil
  "*Non-nil であれば、look コマンドが見つけた英単語を変換キーにし、再検索を行なう。
再検索の結果、候補が見つからなければ、元の英単語自身を候補として出力する。"
  :type 'boolean
  :group 'skk-look )

(defcustom skk-look-expanded-word-only t
  "*Non-nil であれば、look の出力に対する再検索が成功した場合のみを最終的な候補として表示する。
skk-look-recursive-search が non-nil であるときのみ有効。"
  :type 'boolean
  :group 'skk-look )

;; internal constant and variable.
(defconst skk-look-working-buffer " *skk look*")
(defvar skk-look-completion-words nil)


(and skk-look-command
     (null (member '(skk-look) skk-search-prog-list))
     (let ((pl skk-search-prog-list)
	   (n 0) dic mark )
       (while pl
	 (setq dic (car pl))
	 (if (memq (nth 1 dic) '(skk-jisyo skk-rdbms-private-jisyo-table))
	     (setq mark n
		   pl nil)
	   (setq pl (cdr pl)
		 n (1+ n) )))
       (skk-splice-in skk-search-prog-list (1+ mark)
		      '((skk-look)) )))

;; program
;;;###autoload
(defun skk-look ()
  ;; UNIX look コマンドを利用した変換を行なう。
  ;; SKK abbrev モードにて、英文字 + アスタリスクで uncompleted spelling を指定
  ;; する。
  (and skk-abbrev-mode
       (eq (skk-str-ref skk-henkan-key (1- (length skk-henkan-key))) ?*)
       (let ((args (substring skk-henkan-key 0 (1- (length skk-henkan-key))))
	     v )
	 (setq v (skk-look-1 args))
	 (if (not skk-look-recursive-search)
	     v
	   (let (skk-henkan-key v2 v3)
	     (while v
	       (let ((skk-current-search-prog-list
		      (delete '(skk-look) (copy-sequence skk-search-prog-list)) ))
		 (setq skk-henkan-key (car v))
		 (while skk-current-search-prog-list
		   (setq v3 (skk-search)
			 v2 (if (not skk-look-expanded-word-only)
				(skk-nunion v2 (cons (car v) v3))
			      (if v3
				  (skk-nunion v2 (cons (car v) v3))
				v2 )))))
	       (setq v (cdr v)) )
	     v2 )))))

(defun skk-look-1 (args)
  ;; core search engine
  (condition-case nil
      (save-excursion
	(let (opt buffer-read-only)
	  (set-buffer (get-buffer-create skk-look-working-buffer))
	  (erase-buffer)
	  (setq args (list args))
	  (and skk-look-dictionary (nconc args (list skk-look-dictionary)))
	  (and skk-look-dictionary-order (setq opt "d"))
	  (and skk-look-ignore-case (setq opt (concat "f" opt)))
	  (and skk-look-use-alternate-dictionary
	       (setq opt (concat "a" opt)) )
	  (and opt (setq args (cons (concat "-" opt) args)))
	  (and skk-look-termination-character
	       (setq args
		     (cons (list "-t" skk-look-termination-character) args) ))
 	  (and
	   (= 0 (apply 'call-process skk-look-command nil t nil args))
	   (> (buffer-size) 0)
	   (split-string (buffer-substring-no-properties (point-min) (1- (point-max)))
			 "\n" ))))
    (file-error
     (setq skk-search-prog-list (delete '(skk-look) skk-search-prog-list))
     (skk-error "システム上に look コマンドが見つかりません"
		"Sorry, can't find look command on your system" ))))

;;;###autoload
(defun skk-look-completion ()
  (or skk-look-completion-words
      (let ((stacked (stack-all skk-completion-stack)))
	(setq skk-look-completion-words
	      (delete skk-completion-word (skk-look-1 skk-completion-word)) )
	(while stacked
	  (setq skk-look-completion-words
		(delete (car stacked) skk-look-completion-words)
		stacked (cdr stacked) ))))
  (prog1
      (car skk-look-completion-words)
    (setq skk-look-completion-words (cdr skk-look-completion-words)) ))

(provide 'skk-look)
;;; skk-look.el ends here
