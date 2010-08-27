;;; skk-annotation.el --- SKK annotation 関連プログラム -*- coding: iso-2022-jp -*-

;; Copyright (C) 2000, 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Copyright (C) 2000-2010  SKK Development Team <skk@ring.gr.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-annotation.el,v 1.159 2010/08/27 10:42:17 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Created: Oct. 27, 2000.
;; Last Modified: $Date: 2010/08/27 10:42:17 $

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

;; これは、SKK 個人辞書に付けたアノテーション (註釈) を活用するプログ
;; ラムです。
;;
;; <INSTALL>
;;
;; SKK を普通に make して下さい。特に作業は不要です。
;;
;;
;; <HOW TO USE>
;;
;;   (setq skk-show-annotation t)
;;
;; と ~/.emacs に書きましょう。辞書の候補に `;' から始まる文字列があれ
;; ば、その該当の候補が変換されてバッファに出力された際、`;' 以降をそ
;; の候補のアノテーションとしてエコーエリアに表示します。
;;
;;   (setq skk-annotation-show-as-message nil)
;;
;; と ~/.emacs に書いた場合は、other-window を一時的に開いてアノテーショ
;; ンを表示します。other-window はその候補について確定するか、その候補
;; の選択を止める (次の候補を選択したり、quit したり) すると自動的に閉
;; じられます。
;;
;; SKK では 5 番目の候補以降をエコーエリアを使って表示します。従い、5
;; 番目以降の候補については、skk-annotation-show-as-message が t でも
;; ウィンドウを開いてアノテーションを表示します。
;;
;; ある単語について、アノテーションを付けたいときは、確定した直後に同
;; じバッファで
;;
;;   M-x skk-annotation-add
;;
;; しましょう。アノテーションを編集するバッファが開いて、カレントバッ
;; ファになりますので、そこへアノテーションを付けましょう。
;; 1 行である必要はありませんが、複数行のアノテーションを付けると echo
;; area へ表示されたときに全体が見えなくなります。
;; また、`;' の文字自体は入れる必要はありません。
;; 今までに既に付けていたアノテーションがあれば編集バッファが表示され
;; たときにそのアノテーションが prefix 的に出力されます。既存のアノテー
;; ションも含めて編集して下さい。バッファの先頭行を除いて全ての行が新
;; しいアノテーションとして上書きされます。
;; 編集が終わったら C-c C-c しましょう。
;;
;; 上記の動作でユーザが付けたアノテーションを「ユーザアノテーション」
;; と呼びます。ユーザアノテーションは、
;;
;;   「きかん /期間/機関;*機関投資家/基幹;*基幹業務/」
;;
;; のように `;' の直後に `*' の文字が自動的に振られます。これはユーザ
;; が独自に付けたアノテーションであることを示します (`*' の文字は変換
;; 時には表示されません)。
;;
;; 一方、共有辞書に元々付けられているアノテーションを「システムアノテー
;; ション」と呼び、これは `;' の直後に `*' の文字を伴ないません。
;; <例>
;;    「いぜん /以前;previous/依然;still/」
;;
;; ユーザアノテーションとシステムアノテーションを区別することで、ユー
;; ザアノテーションだけを表示したり、あるいはその逆を行なうことが可能
;; です。`skk-annotation-function' に表示したいアノテーションを
;; non-nil と判定する関数を書きましょう。こんな感じです。
;;
;;   (setq skk-annotation-function
;;         (lambda (annotation) (eq (aref annotation 0) ?*)))
;;
;; 上記の例では、アノテーションの先頭が `*' で始まる「ユーザアノテーショ
;; ン」の場合に t を返しますので、ユーザアノテーションだけを表示します。
;;
;; M-x skk-annotation-add したものの、結局アノテーションを付けずに置き
;; たいときは、
;;
;;   M-x skk-annotation-kill
;;
;; して下さい。
;;
;; また、最後に確定した候補についてのアノテーションを取り去りたいとき
;; は、
;;
;;   M-x skk-annotation-remove
;;
;; して下さい。
;;
;; Viper 対策はまだ行なっていません。~/.viper に次のように書いて下さい。
;; (viper-harness-minor-mode "skk-annotation")
;;
;; <Wikipedia アノテーション>
;;
;; ▼モードにて C-i をタイプすると、表示中の候補を Wikipedia/Wiktionary
;; の項目から探し，見つかった場合は、内容の抜粋をアノテーションとして表示
;; します。この機能は Emacs 22 でテストされています。XEmacs 21.5 では以下
;; の 1 と 2 を導入する必要があります。XEmacs 21.4 では更に 3 も必要です。
;; Emacs 21.4 でも 1, 2, 3 が必要となります。Emacs 20.7 での動作はサポート
;; しませんが、MULE 4.1 パッチを当てていれば Emacs 21 と同様に動作する可能
;; 性があります。
;;
;; 1. html2text.el
;;
;;    これは比較的最近の gnus に含まれています。しかし
;;
;;    http://www.ring.gr.jp/archives/elisp/gnus/gnus-5.10.8.tar.gz
;;
;;    に含まれるバージョンではエラーを発生する可能性があります。
;;
;;    もし Wikipedia/Wiktionary 検索の際にエラーが出るようでしたら、
;;    html2text.el だけ開発版 No Gnus (ngnus) v0.6 以上のものに差し替える
;;    必要があります。
;;
;;    http://www.ring.gr.jp/archives/elisp/gnus/snapshots/
;;
;;    または CVS より最新版をインストールしてください。また、Emacs 22.1 に
;;    付属する Gnus 5.11 ではこの問題は修正されています。
;;
;; 2. URL パッケージ
;;
;;    これは Emacs/W3 に含まれていたものの拡張です。例えば
;;
;;    http://ftp.debian.org/debian/pool/main/w/w3-url-e21/
;;
;;    などから新しめの *.orig.tar.gz を取得してインストールします。
;;
;;    XEmacs の場合、 xemacs-sumo 中の w3 に含まれる url.el が読み込まれてしま
;;    うと正しく機能しないので、注意してください。
;;
;; 3. Mule-UCS
;;
;;    UTF-8 の取り扱いに必要となります。
;;
;;    http://www.meadowy.org/~shirai/
;;
;;    から最新版が入手できます。
;;
;; <旧い SKK からの移行>
;;
;; この項はアノテーション機能がない旧い SKK (DDSKK 11.2 以前または SKK
;; 10.62 以前) から最新のものに移行する場合の注意事項です。
;;
;; アノテーションはセパレータとして `;' を使用しているため、`;' の文字
;; を含んだ候補は、eval すると `;' になる Lisp 式として quote し辞書候
;; 補に収める必要があります。
;;
;; まだアノテーション機能を一度も使用していない個人辞書については、以下
;; の S 式を評価した後、
;;
;;   (defun skk-annotation-update-jisyo-format ()
;;     (interactive)
;;     (skk-setup-jisyo-buffer)
;;     (let ((min skk-okuri-ari-min) (max skk-okuri-ari-max))
;;       (skk-annotation-update-jisyo-format-1 min max)
;;       (setq min skk-okuri-nasi-min
;;	     max (point-max))
;;       (skk-annotation-update-jisyo-format-1 min max)))
;;
;;   (defun skk-annotation-update-jisyo-format-1 (min max)
;;     (let (candidate)
;;       (goto-char min)
;;       (while (re-search-forward "\\/\\([^\n/]*;[^\n/]*\\)\\/" max t nil)
;;	 (setq candidate (buffer-substring-no-properties
;;			  (match-beginning 1) (match-end 1)))
;;	 (delete-region (match-beginning 1) (match-end 1))
;;	 (goto-char (match-beginning 1))
;;	 (insert
;;	  (concat "(concat \""
;;		  (mapconcat
;;		   (function
;;		    (lambda (c)
;;		      (if (eq c ?\;)
;;			  "\\073"
;;			(char-to-string c))))
;;		   (append candidate nil) "")
;;		  "\")")))))
;;
;; 個人辞書を読みこみ、辞書を読み込んだバッファで
;;
;;   M-x skk-annotation-update-jisyo-format
;;
;; することでこの作業を行なうことができます。
;;
;; 但し、既にアノテーションが付けられている場合は、このアノテーション
;; 自体も候補と区別できずに quote されてしまいますので、ご注意下さい
;; (今のところ手作業で quote されないように退避するなどしか方法はあり
;; ません)。

;;; Code:

(eval-and-compile
  (require 'skk-macs)
  (require 'skk-vars)

  (autoload 'html2text "html2text")
  (autoload 'html2text-delete-tags "html2text")
  (autoload 'url-hexify-string "url-util")
  (autoload 'url-retrieve "url"))

(eval-when-compile
  (require 'static)

  (defvar mule-version)
  (defvar html2text-remove-tag-list)
  (defvar html2text-format-tag-list)

  (when (and (string-match "^GNU" (emacs-version))
	     (= emacs-major-version 20))
    (defalias 'skk-tooltip-show-at-point 'ignore)))

(static-when (featurep 'xemacs)
  (require 'skk-xemacs))

(unless skk-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'skk-annotation-save-and-quit)
    (define-key map "\C-c\C-k" 'skk-annotation-kill)
    (setq skk-annotation-mode-map map)))

(unless (assq 'skk-annotation-mode minor-mode-alist)
  (setq minor-mode-alist (cons '(skk-annotation-mode " annotation")
			       minor-mode-alist)))

(when (and (boundp 'minor-mode-map-alist)
	   (not (assq 'skk-annotation-mode-map minor-mode-map-alist)))
  (setq minor-mode-map-alist
	(cons (cons 'skk-annotation-mode skk-annotation-mode-map)
	      minor-mode-map-alist)))

;; inline functions.
(defsubst skk-annotation-erase-buffer ()
  (let ((inhibit-read-only t)
	buffer-read-only)
    (static-when
	(fboundp 'set-text-properties)
      (set-text-properties (point-min) (point-max) nil))
    (erase-buffer)))

(defsubst skk-annotation-insert (annotation)
  (with-current-buffer (get-buffer-create skk-annotation-buffer)
    (skk-annotation-erase-buffer)
    (insert annotation)
    (goto-char (point-min))))

;; functions.
;;;###autoload
(defun skk-annotation-get (annotation)
  (if (string= annotation "")
      ""
    (if (eq (aref annotation 0) ?*)
	(substring annotation 1)
      annotation)))

;;;###autoload
(defun skk-annotation-find-and-show (pair)
  ;; ミニバッファにいるとき余計なメッセージをクリアする
  (when (or skk-isearch-switch
	    (skk-in-minibuffer-p))
    (message nil))
  ;;
  (when (and (car-safe pair)
	     (not (cdr-safe pair)))
    ;; Wikipedia の URL 利用の場合はここで注釈を設定する。
    (setcdr pair (or (car (skk-annotation-wikipedia-cache (car pair)))
		     (when skk-annotation-show-wikipedia-url
		       (skk-annotation-treat-wikipedia (car pair))))))
  (skk-annotation-show (or (cdr pair) "") (car pair)))

;;;###autoload
(defun skk-annotation-show (annotation &optional word sources)
  (when (and (not skk-kakutei-flag)
	     (or (not skk-annotation-function)
		 (funcall skk-annotation-function annotation)))
    (setq annotation (skk-annotation-get annotation))
    (let ((notes (mapcar #'skk-eval-string (split-string annotation ";")))
	  (inhibit-wait skk-isearch-switch))
      (setq annotation (skk-eval-string annotation))
      (unless (string= annotation "")
	(setq inhibit-wait (skk-annotation-show-2 annotation)))
      ;; 注釈の表示はここまでだが、ここでユーザが注釈の内容をコピーしたり
      ;; して利用できるようにする。
      (unless inhibit-wait
	(skk-annotation-wait-for-input annotation notes word sources)))))

(defun skk-annotation-show-2 (annotation)
  (let (inhibit-wait)
    (cond (skk-isearch-switch
	   ;; do nothing
	   (setq inhibit-wait t))
	  ((and (not (skk-annotation-display-p 'minibuf))
		(skk-in-minibuffer-p))
	   ;; do nothing
	   (setq inhibit-wait t))
	  ((and window-system skk-show-tooltip)
	   (skk-tooltip-show-at-point annotation 'annotation))
	  ((and skk-annotation-show-as-message
		(not (or skk-isearch-switch
			 (skk-in-minibuffer-p))))
	   (skk-annotation-show-as-message annotation))
	  (t
	   (skk-annotation-show-buffer annotation)))
    inhibit-wait))

(defun skk-annotation-wait-for-input (annotation notes &optional word sources)
  (let* ((copy-command (key-binding skk-annotation-copy-key))
	 (browse-command (key-binding skk-annotation-browse-key))
	 (list (list copy-command browse-command))
	 event key command urls note cache char digit)
    (while (and list
		(or (memq this-command
			  '(skk-annotation-wikipedia-region-or-at-point
			    skk-annotation-wikipedia-region))
		    (eq skk-henkan-mode 'active))
		(if digit
		    t
		  (skk-annotation-message (if (and annotation
						   (> (length annotation) 0))
					      'annotation
					    nil)))
		(condition-case nil
		    (progn
		      (setq event (next-command-event)
			    key (skk-event-key event)
			    command (key-binding
				     (static-if (featurep 'xemacs)
					 event
				       key)))
		      ;; Return value of the following expression is important.
		      (or (memq command list)
			  (eq command 'digit-argument)
			  (memq command
				'(skk-annotation-wikipedia-region-or-at-point
				  skk-annotation-wikipedia-region))
			  (equal (key-description key)
				 (key-description
				  skk-annotation-wikipedia-key))))
		  (quit
		   (static-when
		       (and (featurep 'xemacs)
			    (= emacs-major-version 21)
			    (<= emacs-minor-version 4))
		     ;; workaround
		     (keyboard-quit)))))
      (cond ((eq command copy-command)
	     (setq list (delq copy-command list))
	     (unless (equal annotation "")
	       (kill-new (substring-no-properties annotation))
	       (skk-message "現在の注釈をコピーしました"
			    "Copying the current note...done")
	       (setq event nil
		     digit nil
		     char  nil)
	       (skk-annotation-show-2 annotation)))
	    ((eq command browse-command)
	     (setq list (delq browse-command list))
;	     (setq urls (delq nil (mapcar #'skk-annotation-find-url notes)))
	     (when word
	       (cond ((setq cache
			    (skk-annotation-wikipedia-cache word sources))
		      (setq urls
			    (cons (apply
				   #'skk-annotation-generate-url
				   "http://%s.org/wiki/%s"
				   ;; split-string の非互換性に配慮
				   (static-if (and (string-match
						    "^GNU"
						    (emacs-version))
						   (<= emacs-major-version 21))
				       (cdr (split-string (cdr cache) " "))
				     (cdr (split-string (cdr cache) " " t))))
				  urls)))
		     (skk-annotation-show-wikipedia-url
		      (add-to-list 'urls
				   (skk-annotation-generate-url
				    "http://ja.wikipedia.org/wiki/%s"
				    word)))))
	     (unless (equal annotation "")
	       (cond
		(urls
		 (dolist (url urls)
		   (browse-url url))
		 (skk-message "注釈のためのサイトをブラウズしています..."
			      "Browsing web sites for the current notes..."))
		(t
		 (skk-message "注釈のためのサイトが見つかりません"
			      "No web sites found for the current notes")))
	       (setq event nil
		     digit nil
		     char  nil)
	       (skk-annotation-show-2 annotation)))
	    ((eq command 'digit-argument)
	     (setq char  (static-if (featurep 'xemacs)
			     key
			   (if (integerp event)
			       event
			     (get event 'ascii-character)))
		   digit (- (logand char ?\177) ?0)
		   event nil))
	    ((or (equal (key-description key)
			(key-description skk-annotation-wikipedia-key))
		 (memq command
		       '(skk-annotation-wikipedia-region-or-at-point
			 skk-annotation-wikipedia-region)))
	     (setq sources
		   (if (and digit
			    (> digit 0)
			    (<= digit
				(length skk-annotation-wikipedia-sources)))
		       (list (nth (1- digit)
				  skk-annotation-wikipedia-sources))
		     skk-annotation-wikipedia-sources))
	     (setq event nil
		   digit nil
		   char  nil)
	     (when word
	       (let ((skk-annotation-show-wikipedia-url nil))
		 (setq note (skk-annotation-treat-wikipedia word sources))))
	     (cond ((null note)
		    (setq note annotation))
		   (t
		    (setq annotation note)))
	     (unless (equal note "")
	       (add-to-list 'list browse-command)
	       (add-to-list 'list copy-command)
	       (skk-annotation-show-2 (or note annotation))))
	    (t
	     (setq list nil))))
    (when event
      (skk-unread-event event))))

;;;###autoload
(defun skk-annotation-message (&optional situation)
  (when (and skk-verbose
	     (not (or skk-isearch-switch
		      (skk-in-minibuffer-p))))
    (unless skk-annotation-wikipedia-message
      (let* ((key (key-description skk-annotation-wikipedia-key))
	     (string "{どのWiki?}")
	     (i 0)
	     source)
	(when (equal key "TAB")
	  (setq key "C-i"))
	(while (setq source (nth i skk-annotation-wikipedia-sources))
	  (setq string (format "%s[C-%d %s]%s " string (1+ i) key source))
	  (setq i (1+ i)))
	(setq skk-annotation-wikipedia-message string)))
    (unless skk-annotation-message
      (let ((key-copy (or (key-description skk-annotation-copy-key)
			  "未定義"))
	    (key-wiki (or (key-description skk-annotation-wikipedia-key)
			  "未定義"))
	    (key-browse (or (key-description skk-annotation-browse-key)
			    "未定義")))
	(when (equal key-wiki "TAB")
	  (setq key-wiki "C-i"))
	(setq skk-annotation-message
	      (format "{アノテーション操作}[%s]コピーする [%s]URLを見つける [%s]デフォルトWikiから抜粋"
		      key-copy key-browse key-wiki))))
    (condition-case nil
	(cond ((eq situation 'annotation)
	       (if (skk-sit-for skk-verbose-wait)
		   (let ((i 0))
		     (catch 'loop
		       (while (< i 20)
			 (message "%s" skk-annotation-message)
			 (unless (skk-sit-for skk-verbose-message-interval)
			   (throw 'loop nil))
			 (message "%s" skk-annotation-wikipedia-message)
			 (unless (skk-sit-for skk-verbose-message-interval)
			   (throw 'loop nil))
			 (setq i (1+ i))))
		     (message nil))
		 nil))
	      (t
	       (when (skk-sit-for skk-verbose-wait)
		 (message "%s" skk-annotation-wikipedia-message))))
      (quit
       (cond
	((eq skk-henkan-mode 'active)
	 (setq skk-henkan-count 0)
	 (skk-unread-event
	  (character-to-event
	   (aref
	    (car (where-is-internal 'skk-previous-candidate skk-j-mode-map))
	    0))))
	(t
	 (keyboard-quit))))))
  ;; 常に t を返す
  t)

(defun skk-annotation-find-url (string)
  (let (url)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (save-match-data
	(while (and (not url)
		    (re-search-forward "\\." nil t))
	  (backward-char 1)
	  (setq url (thing-at-point 'url))
	  ;; http://foo のような URL を生成してしまうので対策
	  (when (and url
		     (not (string-match "\\." url)))
	    (setq url nil))
	  (unless url
	    (forward-char 1)))))
    url))

(defun skk-annotation-show-buffer (annotation)
  (condition-case nil
      (save-window-excursion
	(let ((minibuf-p (skk-in-minibuffer-p))
	      event window)
	  (skk-annotation-insert annotation)
	  (cond
	   (minibuf-p
	    (if (setq window (get-buffer-window (skk-minibuffer-origin)))
		(select-window window)
	      (other-window 1))
	    (unless (eq (next-window) (selected-window))
	      (delete-other-windows)))
	   (t
	    (split-window-vertically)))
	  (display-buffer skk-annotation-buffer)
	  (when minibuf-p
	    (select-window (minibuffer-window)))
	  ;;
	  (skk-annotation-message 'annotation)
	  ;;
	  (setq event (next-command-event))
	  (when (skk-key-binding-member
		 (skk-event-key event)
		 '(key-board-quit
		   skk-kanagaki-bs
		   skk-kanagaki-esc)
		 skk-j-mode-map)
	    (signal 'quit nil))
	  (skk-unread-event event)))
    (quit
     ;; skk-previous-candidate へ
     (setq skk-henkan-count 0)
     (skk-unread-event
      (character-to-event
       (aref
	(car (where-is-internal 'skk-previous-candidate
				skk-j-mode-map))
	0))))))

(defun skk-annotation-show-as-message (annotation)
  (message "%s" annotation))

(defun skk-annotation-setup ()
  (let ((skk-henkan-key (skk-get-last-henkan-datum 'henkan-key))
	(skk-okuri-char (skk-get-last-henkan-datum 'okuri-char))
	(cand (car (skk-get-last-henkan-datum 'henkan-list)))
	word)
    (unless cand
      (setq skk-henkan-key
	    (read-from-minibuffer "Midasi: "))
      (when (string= skk-henkan-key "")
	(skk-error "アノテーションする単語がありません"
		   "No word to be annotated"))
      (when (string-match "\\cj\\([a-z]+\\)$"
			  skk-henkan-key)
	(setq skk-okuri-char (match-string 1 skk-henkan-key)
	      ;; 送りあり変換を指定すると
	      ;; skk-henkan-okurigana の指定に困る。
	      skk-henkan-okurigana ""))
      (setq cand
	    (prog1
		(skk-henkan-in-minibuff)
	      (setq skk-kakutei-flag nil))))
    ;; この時点では skk-num-list は既に nil
    ;; ミニバッファから対象を指定した場合には consp にならない
    (when (consp cand)
      (setq cand (car cand)))
    (setq word (car (skk-treat-strip-note-from-word cand)))
    (when (and (string-match "[0-9]" skk-henkan-key)
	       (or (string-match "#[0-9]" word)
		   (skk-lisp-prog-p word)))
      (setq skk-henkan-key
	    (skk-num-compute-henkan-key skk-henkan-key)))
    (setq skk-annotation-target-data
	  (list skk-henkan-key
		skk-okuri-char
		cand))
    ;; 意図を理解してないが、skk-kakutei-initialize のほうが適切な気も
    (skk-kakutei)))

;;;###autoload
(defun skk-annotation-add (&optional no-previous-annotation)
  "最後に確定した語に annotation を付ける。
既に付けられている annotation があればそれを編集バッファに出力する。
no-previous-annotation を指定すると \(C-u M-x skk-annotation-add で指定可\)
既に付けられている annotation を編集バッファに出力しない。"
  (interactive "P")
  (save-match-data
    (skk-kakutei)
    (skk-annotation-setup)
    (let* ((plist (append
		   '(intangible t read-only t)
		   (static-if (featurep 'xemacs)
		       '(start-closed t end-open t)
		     '(front-sticky t rear-nonsticky t))))
	   (wholestring (nth 2 skk-annotation-target-data))
	   (realword (if (and wholestring
			      (string-match ";\\*?" wholestring))
			 (substring wholestring 0 (match-beginning 0))
		       wholestring))
	   (annotation (if (and realword
				(string-match ";\\*?" wholestring))
			   (substring wholestring (match-end 0))
			 nil)))
      (setq skk-annotation-original-window-configuration
	    (current-window-configuration))
      (delete-other-windows)
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer (get-buffer-create skk-annotation-buffer))
      (setq buffer-read-only nil
	    skk-annotation-mode t)
      (skk-annotation-erase-buffer)
      (insert
       (format "\
;; Add a note to word `%s' (this line will not be added to the note.)
"
	       realword))
      (static-if (fboundp 'set-text-properties)
	  (add-text-properties (point-min) (1- (point)) plist))
      (when (and (not no-previous-annotation)
		 annotation)
	(insert annotation))
      (run-hooks 'skk-annotation-mode-hook)
      (message "%s to save edits, %s to just kill this buffer"
	       (mapconcat 'key-description
			  (where-is-internal 'skk-annotation-save-and-quit
					     skk-annotation-mode-map)
			  ", ")

	       (mapconcat 'key-description
			  (where-is-internal 'skk-annotation-kill
					     skk-annotation-mode-map)
			  ", ")))))

(defun skk-annotation-save-and-quit (&optional quiet)
  "最後に確定した語に annotation を付けて annotation バッファを閉じる。"
  ;; called in the annotation buffer.
  (interactive "P")
  (let (annotation)
    (save-match-data
      (with-current-buffer (get-buffer-create skk-annotation-buffer)
	(goto-char (point-min))
	(when (looking-at ";; Add a note to word") ; 中途半端
	  (forward-line 1)
	  (beginning-of-line))
	(setq annotation (buffer-substring-no-properties
			  (point) (point-max)))
	(when (string-match "^[\t\n 　]+" annotation)
	  (setq annotation (substring annotation (match-end 0))))
	(when (string-match "[\t\n 　]+$" annotation)
	  (setq annotation (substring annotation 0 (match-beginning 0))))
	(when (string= annotation "")
	  (setq annotation nil))
	(setq annotation (skk-quote-char annotation))))
    (if annotation
	(skk-annotation-last-word-1
	 (lambda (beg end)
	   (goto-char beg)
	   (when (re-search-forward ";[^/]*" end t)
	     (delete-region (match-beginning 0) (match-end 0)))
	   (goto-char end)
	   (insert ";*" annotation)))
      ;; 削除した時
      (let ((old-annotation
	     (cdr (skk-treat-strip-note-from-word
		   (nth 2 skk-annotation-target-data)))))
	(when (and old-annotation
		   (yes-or-no-p
		    (format (if skk-japanese-message-and-error
				"既存のアノテーション `%s' を削除しますか？ "
			      "Delete old annotation `%s' ? ")
			    (skk-annotation-get old-annotation))))
	  (skk-annotation-last-word-1
	   (lambda (beg end)
	     (goto-char beg)
	     (when (re-search-forward ";[^/]*" end t)
	       (delete-region (match-beginning 0) (match-end 0))))))))
    (skk-annotation-erase-buffer)
    (kill-buffer (current-buffer))
    (set-window-configuration
     skk-annotation-original-window-configuration)
    (when annotation
      (unless quiet
	(message "%s" "Added annotation")))))

(defun skk-annotation-kill ()
  "annotation を付けずに annotation バッファを kill する。"
  ;; called in the annotation buffer.
  (interactive)
  (skk-annotation-erase-buffer)
  (kill-buffer (current-buffer))
  (set-window-configuration
   skk-annotation-original-window-configuration))

;;;###autoload
(defun skk-annotation-remove ()
  "最後に確定した語から annotation を取り去る。"
  (interactive)
  (save-match-data
    (skk-kakutei)
    (skk-annotation-setup)
    (when (yes-or-no-p
	   (format (if skk-japanese-message-and-error
		       "%s についてのアノテーションを削除しますか？ "
		     "Really delete annotation for %s? ")
		   (nth 2 skk-annotation-target-data)))
      (skk-annotation-last-word-1
       (lambda (beg end)
	 (goto-char beg)
	 (when (re-search-forward ";[^/]*" end t)
	   (delete-region (match-beginning 0) (match-end 0))))))))

;;;###autoload
(defun skk-annotation-display-p (test)
  ;; ミニバッファにいるとき余計なメッセージをクリアする
  (when (or skk-isearch-switch
	    (skk-in-minibuffer-p))
    (message nil))
  ;;
  (cond ((eq skk-show-annotation nil)
	 nil)
	((and (listp skk-show-annotation)
	      (eq (car skk-show-annotation) 'not)
	      ;; (not ...)
	      (memq test skk-show-annotation))
	 ;; (not list), (not minibuf) or (not list minibuf)
	 nil)
	(t
	 ;; non-nil
	 t)))

;;;###autoload
(defun skk-annotation-toggle-display-p ()
  (interactive)
  (cond ((eq skk-show-annotation nil)
	 ;; do nothing
	 nil)
	((and (listp skk-show-annotation)
	      (eq (car skk-show-annotation) 'not))
	 ;; (not ...)
	 (cond ((memq 'list skk-show-annotation)
		(if (eq (length skk-show-annotation) 2)
		    ;; (not list) -> t  i.e. turn on
		    (setq skk-show-annotation t)
		  ;; (not list minibuf) -> (not minibuf)
		  (setq skk-show-annotation '(not minibuf))))
	       (t
		;; (not minibuf) -> (not list minibuf)  i.e. turn off
		(setq skk-show-annotation '(not list minibuf)))))
	(t
	 ;; non-nil -> (not list)  i.e. turn off
	 (setq skk-show-annotation '(not list)))))

(defun skk-annotation-last-word-1 (function)
  ;; funcall FUNCTION with BEG and END where BEG and END are markers.
  (let ((inhibit-quit t)
	(jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
	(word (nth 2 skk-annotation-target-data))
	(beg (make-marker))
	(end (make-marker))
	(eol (make-marker))
	pattern)
    (when (buffer-live-p jisyo-buffer)
      (save-match-data
	(with-current-buffer jisyo-buffer
	  (goto-char (if (nth 1 skk-annotation-target-data)
			 skk-okuri-ari-min
		       skk-okuri-nasi-min))
	  (when (re-search-forward
		    (concat "^\\("
			    (regexp-quote (car skk-annotation-target-data))
			    "\\) /")
		    (if (nth 1 skk-annotation-target-data)
			skk-okuri-ari-max nil)
		    t nil)
	    (goto-char (match-beginning 1))
	    (set-marker eol (skk-save-point (end-of-line) (point)))
	    (when (string-match ";" word)
	      (setq word (substring word 0 (match-beginning 0))))
	    (when (re-search-forward
		   (concat "/\\(" word "\\)\\(;[^/]*\\)*/")
		   eol t nil)
	      (set-marker beg (match-beginning 1))
	      (set-marker end (or (match-end 2) (match-end 1)))
	      (funcall function beg end)
	      (when (nth 1 skk-annotation-target-data)
		(goto-char end)
		;; skip other candidates that has not a okuirigana.
		(search-forward "/[" eol t nil)
		(setq pattern (concat "/\\(" word "\\)\\(;[^/]*\\)*/"))
		(while (re-search-forward pattern eol t nil)
		  (set-marker beg (match-beginning 1))
		  (set-marker end (or (match-end 2)
				      (match-end 1)))
		  (funcall function beg end)))
	      (set-marker beg nil)
	      (set-marker end nil)
	      (set-marker eol nil))))))))

;;;###autoload
(defun skk-annotation-quote (&optional quiet)
  "最後に確定した語に含まれる `;' を候補の一部として quote する。"
  (interactive "P")
  (skk-kakutei)
  (skk-annotation-setup)
  (let (candidate)
    (skk-annotation-last-word-1
     (lambda (beg end)
       (goto-char beg)
       (setq candidate (buffer-substring-no-properties beg end))
       (when (string-match ";" candidate)
	 (delete-region beg end)
	 (insert (skk-quote-semicolon candidate))
	 (unless quiet
	   (message "%s" "Quoted")))))))

;;;###autoload
(defun skk-annotation-wikipedia (word &optional sources)
  "Wiktionary/Wikipedia の WORD に相当する記事からアノテーションを取得する。"
  (let ((sources (or sources skk-annotation-wikipedia-sources))
	source
	(string "")
	(note nil))
    ;; sources に指定された順番に参照する
    (if (catch 'skk-annotation-wikipedia-suspended
	  (save-match-data
	    (while (and (or (not note) (equal note ""))
			sources)
	      (setq source (car sources))
	      ;; Wiktionary ではそのまま、Wikipedia では第 1 文字のみ upcase
	      (setq note (skk-annotation-wikipedia-1 word source
						     (= 1 (length sources))))
	      (when (equal note "")
		(setq note nil))
	      ;;
	      (when (and (null note)
			 (memq source '(en.wiktionary ja.wiktionary))
			 (skk-ascii-char-p (aref word 0))
			 (not (skk-lower-case-p (aref word 0))))
		;; Wiktionary ですべて downcase する場合
		;; e.g. FOO or Foo -> foo
		(setq note (skk-annotation-wikipedia-1
			    (downcase word)
			    source
			    (= 1 (length sources))))
		(when (equal note "")
		  (setq note nil)))
	      ;;
	      (sleep-for 0.01) ; これがないと止まることあり
	      ;;
	      (when (and (null note)
			 (skk-ascii-char-p (aref word 0))
			 (>= (length word) 2)
			 (skk-lower-case-p (aref word 1)))
		;; すべて upcase する場合
		;; e.g. skk or Skk -> SKK
		(setq note (skk-annotation-wikipedia-1
			    (upcase word)
			    source
			    (= 1 (length sources))))
		(when (equal note "")
		  (setq note nil)))
	      ;;
	      (setq string (format (if (string= "" string)
				       "%s%s"
				     "%s/%s")
				   string source))
	      (setq sources (cdr sources)))
	    (unless note
	      (message "%s に項目がありません" string)))
	  nil)
	;; ダウンロードが中断されたとき
	(progn
	  (message "%s の転送が中断されました" source)
	  nil)
      ;;
      note)))

(defun skk-annotation-wikipedia-clean-sup (p1 p2 p3 p4)
  (put-text-property p2 p3 'face 'underline)
  (save-excursion
    (goto-char p2)
    (insert "^"))
  (html2text-delete-tags p1 p2 (1+ p3) (1+ p4)))

(defun skk-annotation-wikipedia-clean-sub (p1 p2 p3 p4)
  (put-text-property p2 p3 'face 'underline)
  (save-excursion
    (goto-char p2)
    (insert "_"))
  (html2text-delete-tags p1 p2 (1+ p3) (1+ p4)))

(defun skk-annotation-wikipedia-1 (word source last)
  "Wiktionary/Wikipedia の WORD に相当する記事を実際にダウンロードして調べる。
該当ページ (html) をダウンロードする機能は Emacs に付属の URL パッケージに依
る。適切な URL を生成するためには、"
  (require 'html2text)
  (require 'url)
  ;;
  (setq word (skk-annotation-wikipedia-normalize-word word source))
  ;;
  (let ((cache-buffer (format " *skk %s %s" source word))
	;; html2text が正しく扱えない tag は以下のリストに指定する
	(html2text-remove-tag-list
	 (append '("a" "span" "table" "tr" "td" "h2" "h3" "h4" "h5" "small"
		   "code")
		 html2text-remove-tag-list))
	(html2text-format-tag-list
	 (append '(("sup" . skk-annotation-wikipedia-clean-sup)
		   ("sub" . skk-annotation-wikipedia-clean-sub))
		 html2text-format-tag-list))
	buf buffer)
    (if (get-buffer cache-buffer)
	(with-current-buffer cache-buffer
	  (buffer-string))
      ;; キャッシュがない場合
      (setq buffer (url-retrieve (skk-annotation-generate-url
				  "http://%s.org/wiki/%s"
				  source word)
				 #'skk-annotation-wikipedia-retrieved
				 (list (list source))))
      (while (not buf)
	(setq buf (catch 'skk-annotation-wikipedia-retrieved
		    (condition-case nil
			(sleep-for 0.01)
		      ((error quit)
		       (kill-buffer buffer)
		       (throw 'skk-annotation-wikipedia-suspended
			      source))))))
      (when (and (setq buffer buf)
		 (buffer-live-p buffer))
	(skk-annotation-wikipedia-format-buffer source buffer cache-buffer)))))

(defun skk-annotation-wikipedia-format-buffer (source buffer cache-buffer)
  (let ((html2text-remove-tag-list
	 (append '("a" "span" "table" "tr" "td" "h2" "h3" "h4" "h5" "small"
		   "code")
		 html2text-remove-tag-list))
	(html2text-format-tag-list
	 (append '(("sup" . skk-annotation-wikipedia-clean-sup)
		   ("sub" . skk-annotation-wikipedia-clean-sub))
		 html2text-format-tag-list))
	note aimai continue nop point top pt1 pt2 btag etag end)
    (with-current-buffer buffer
      (set-buffer-multibyte t)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (when (> (buffer-size) 0)
	(when (get-buffer cache-buffer)
	  (kill-buffer cache-buffer))
	(rename-buffer cache-buffer)
	;; 要らない部分を消す
	(cond
	 ;; ja.wiktionary
	 ((eq source 'ja.wiktionary)
	  (goto-char (point-min))
	  (if (save-excursion
		(re-search-forward "\
\\(^HTTP/1\\.0 301 Moved Permanently\\|<div class=\"noarticletext\">\
\\|:Badtitle\\)"
				   nil t))
	      ;; 項目がない場合
	      (erase-buffer)
	    (search-forward "<!-- start content -->" nil t)
	    (delete-region (point-min) (point))
	    ;;
	    (goto-char (point-min))
	    (when (re-search-forward
		   skk-annotation-ja-wiktionary-lang-regexp
		   nil t)
	      (save-excursion
		(goto-char (match-end 2))
		(insert ", "))
	      (delete-region (point-min) (match-beginning 0))
	      (setq top (point))
	      (when (re-search-forward
		     skk-annotation-ja-wiktionary-lang-regexp
		     nil t)
		(delete-region (setq pt1 (match-beginning 0))
			       (point-max))))
	    ;;
	    (setq point top)
	    (goto-char (point-min))
	    ;; ja.wiktionary の書式が en.wiktionary ほど整っていないので
	    ;; workaround
	    (unless
		(save-excursion
		  (re-search-forward
		   skk-annotation-ja-wiktionary-part-of-speech-regexp
		   nil t))
	      (setq point pt1))
	    ;;
	    (while (re-search-forward
		    skk-annotation-ja-wiktionary-part-of-speech-regexp
		    nil t)
	      (setq nop t)
	      (save-match-data
		(when (looking-at "</h3>")
		  (delete-region (match-beginning 0) (match-end 0))))
	      (goto-char (match-beginning 0))
	      (delete-region (or point (point-min)) (point))
	      (when (re-search-forward "<\\(ol\\|dl\\)>" nil t)
		(setq btag (match-string 0)
		      etag (if (string= btag "<ol>")
			       "</ol>"
			     "</dl>")
		      point nil
		      pt1 (point)
		      pt2 nil)
		(while (and (not point)
			    (search-forward etag nil t))
		  (setq pt2 (point))
		  (goto-char pt1)
		  (if (and (search-forward btag nil t)
			   (< (point) pt2))
		      (progn
			(goto-char pt2)
			(setq pt1 (point)))
		    (setq point pt2)
		    (goto-char point)))))
	    ;;
	    ;; ja.wiktionary の書式が en.wiktionary ほど整っていないので
	    ;; 消しすぎてしまう危険性あり。
	    (when point
	      (delete-region point (point-max)))
	    ;; (用例などを除く -- 除かないほうがいい？)
	    ;; ja.wiktionary は en.wiktionary と全く統一された書き方には
	    ;; なっていないので、ul を除くと情報がほとんど残らない場合が
	    ;; ある
	    ;; (skk-annotation-wikipedia-remove-nested "<ul>" "</ul>")
	    (skk-annotation-wikipedia-remove-nested "<dl>" "</dl>")
	    (skk-annotation-wikipedia-remove-nested "<table[^<]*>"
						    "</table>")
	    (skk-annotation-wikipedia-remove-nested "\
<div class=\"\\(infl-table\\|thumb.+\\)\"[^<]*>" "</div>" "<div[^<]*>")
	    ;;
	    (goto-char (point-min))
	    (while (re-search-forward
		    "<span.*>\\[<a.+>編集</a>\\]</span>"
		    nil t)
	      (replace-match ""))))
	 ;; en.wiktionary
	 ((eq source 'en.wiktionary)
	  (goto-char (point-min))
	  (if (save-excursion
		(re-search-forward "\
\\(^HTTP/1\\.0 301 Moved Permanently\\|<div class=\"noarticletext\">\
\\|:Badtitle\\)"
				   nil t))
	      ;; 項目がない場合
	      (erase-buffer)
	    (search-forward "<!-- start content -->" nil t)
	    (delete-region (point-min) (point))
	    ;;
	    (goto-char (point-min))
	    (when (re-search-forward
		   skk-annotation-en-wiktionary-lang-regexp
		   nil t)
	      (save-excursion
		(goto-char (match-end 2))
		(insert ", "))
	      (delete-region (point-min) (match-beginning 0))
	      (setq top (point))
	      (when (re-search-forward
		     skk-annotation-en-wiktionary-lang-regexp
		     nil t)
		(delete-region (match-beginning 0) (point-max))))
	    ;;
	    (setq point top)
	    (goto-char (point-min))
	    (while (re-search-forward
		    skk-annotation-en-wiktionary-part-of-speech-regexp
		    nil t)
	      (setq nop t)
	      (save-match-data
		(when (looking-at "</h3>")
		  (delete-region (match-beginning 0) (match-end 0))))
	      (goto-char (match-beginning 0))
	      (delete-region (or point (point-min)) (point))
	      (cond
	       ((re-search-forward "<\\(ol\\|dl\\)>" nil t)
		(setq btag (match-string 0)
		      etag (if (string= btag "<ol>")
			       "</ol>"
			     "</dl>")
		      point nil
		      pt1 (point)
		      pt2 nil)
		(while (and (not point)
			    (search-forward etag nil t))
		  (setq pt2 (point))
		  (goto-char pt1)
		  (if (and (search-forward btag nil t)
			   (< (point) pt2))
		      (progn
			(goto-char pt2)
			(setq pt1 (point)))
		    (setq point pt2)
		    (goto-char point))))
	       (t
		(goto-char (match-end 0))
		(when (search-forward "</p>" nil t)
		  (setq point (point))))))
	    ;;
	    (when point
	      (delete-region point (point-max)))
	    ;; (用例などを除く -- 除かないほうがいい？)
	    (skk-annotation-wikipedia-remove-nested "<ul>" "</ul>")
	    (skk-annotation-wikipedia-remove-nested "<dl>" "</dl>")
	    (skk-annotation-wikipedia-remove-nested "<table[^<]*>"
						    "</table>")
	    (skk-annotation-wikipedia-remove-nested "\
<div class=\"\\(infl-table\\|thumb.+\\)\"[^<]*>" "</div>" "<div[^<]*>")
	    (skk-annotation-wikipedia-remove-nested "\
<span class=\"interProject\">" "</span>")
	    ;; Wikipedia への案内を除く
	    (goto-char (point-min))
	    (while (re-search-forward "\
\\(<a href=\"/wiki/Wikipedia\" title=\"Wikipedia\">\\)?\
Wikipedia\\(</a>\\)? has an article on:$" nil t)
	      (save-excursion
		(goto-char (match-beginning 0))
		(beginning-of-line)
		(setq point (point)))
	      (forward-line 2)
	      (end-of-line)
	      (delete-region point (point)))
	    ;;
	    (goto-char (point-min))
	    (while (re-search-forward "\
<span.*>\\(\\[<a.+>edit</a>\\]\\|Inflection\\)</span>"
				      nil t)
	      (replace-match ""))))
	 ;; wikipedia
	 ((memq source '(ja.wikipedia simple.wikipedia en.wikipedia))
	  (goto-char (point-min))
	  (if (save-excursion
		(re-search-forward "\
\\(^HTTP/1\\.0 301 Moved Permanently\\|<div class=\"noarticletext\">\
\\|:Badtitle\\)"
				   nil t))
	      ;; 項目がない場合
	      (erase-buffer)
	    (setq aimai
		  (save-excursion
		    (re-search-forward "\
^wgCategories=.+\\(曖昧さ回避\\|[Dd]isambiguation\\).+$" nil t)))
	    ;; <span> を除去する
	    (setq point nil)
	    (goto-char (point-min))
	    (while (re-search-forward "\
<span class=\"\\(.+audiolink.+\\|editsection\\)\".*>" nil t)
	      (setq point (match-beginning 0))
	      (goto-char point)
	      (search-forward "</span>" nil t)
	      (delete-region point (point))
	      (goto-char point))
	    ;; <big> を除去する
	    (goto-char (point-min))
	    (while (re-search-forward "<p><big>.+</big></p>" nil t)
	      (replace-match ""))
	    ;; &#160; を処理
	    (goto-char (point-min))
	    (while (re-search-forward "&#160;" nil t)
	      (replace-match " "))
	    ;; <br /> を除去する
	    (goto-char (point-min))
	    (while (re-search-forward "<p>.+\\(<br />\\)$" nil t)
	      (replace-match "" nil nil nil 1))
	    ;; xxx > xxx > xxx ... を除去する
	    (goto-char (point-min))
	    (while (re-search-forward
		    "<p>.+</a> &gt; \\(<a.+>\\|<b>\\).+</p>" nil t)
	      (replace-match ""))
	    ;; <script> を除去
	    (skk-annotation-wikipedia-remove-nested "<script.*>" "</script>")
	    ;; <table> を除去
	    (skk-annotation-wikipedia-remove-nested "<table.*>" "</table>")
	    ;;
	    (goto-char (point-min))
	    (when (or (when (re-search-forward
			     "<p>\\(<br />\n\\|[^\n]*\\)?\
<b>[^\n]+</b>[^\n]+"
			     nil t)
			(goto-char (match-beginning 0))
			(if (and (save-excursion
				   (re-search-forward "</p>" nil t))
				 (string-match
				  (cond
				   ((eq source 'ja.wikipedia)
				    "。\\|．")
				   (t
				    "\\."))
				  (buffer-substring (point)
						    (match-beginning 0))))
			    t
			  (setq point (point)
				continue t)
			  nil))
		      (when (progn
			      (goto-char (point-min))
			      (re-search-forward "<\\(u\\|o\\)l>" nil t))
			(goto-char (if continue
				       point
				     (match-beginning 0)))
			(setq nop t)))
	      (delete-region (point-min) (point))
	      (goto-char (point-min))
	      ;;
	      (cond
	       ((or aimai nop)
		(setq pt1 (if (re-search-forward "<\\(u\\|o\\)l>" nil t)
			      (match-end 0)
			    nil)
		      pt2 nil)
		(while (and (not end)
			    (re-search-forward "</\\(u\\|o\\)l>"
					       nil t))
		  (setq pt2 (match-end 0))
		  (save-excursion
		    (goto-char (or pt1 (1+ (point-min))))
		    (when (re-search-forward "<\\(u\\|o\\)l>"
					     nil t)
		      (setq pt1 (match-end 0))))
		  (when (or (null pt1)
			    (> pt1 pt2))
		    (setq end t))))
	       (t
		(re-search-forward "</p>" nil t)))
	      (delete-region (point) (point-max))))))
	;;
	(setq point nil)
	(when (> (buffer-size) 0)
	  (html2text)
	  (goto-char (point-min))
	  (cond
	   ((memq source '(ja.wiktionary en.wiktionary))
	    ;; wiktionary の整形結果は空行だらけになる...
	    (goto-char (point-min))
	    (while (re-search-forward "\n[\n]+" nil t)
	      (replace-match "\n"))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (beginning-of-line)
	      (setq point (point))
	      (forward-line 1)
	      (fill-region point (point))))
	   (t
	    (while (looking-at "^[ \t]*$")
	      (kill-line 1))
	    (cond ((or aimai nop)
		   (while (not (eobp))
		     (beginning-of-line)
		     (setq point (point))
		     (forward-line 1)
		     (fill-region point (point))))
		  (t
		   (fill-paragraph nil)))))
	  ;;
	  (when aimai
	    (insert (if (eq source 'ja.wikipedia)
			"\n(曖昧さ回避のページ)"
		      "\n(Disambiguation page)")))
	  ;;
	  (goto-char (point-max))
	  (while (and (looking-at "^$")
		      (not (string= "" (buffer-string))))
	    (delete-char -1))
	  ;;
	  (when (and (not (equal (buffer-string) ""))
		     (not (get-text-property 1 'face)))
	    (put-text-property 1 2 'face 'default))
	  (setq note (buffer-string)))))
    ;;
    (cond ((stringp note)
	   (if (equal note "")
	       nil
	     note))
	  (t
	   nil))))

(defun skk-annotation-wikipedia-remove-nested (btag etag &optional ibtag)
  "<dl> <ul> <table> などの入れ子構造を除去する。"
  (unless ibtag
    (setq ibtag btag))
  (let (point pt1 pt2 orig-btag)
    (setq point nil)
    (goto-char (point-min))
    (while (re-search-forward btag nil t)
      (setq point (match-beginning 0))
      (goto-char point)
      (cond
       ((not (search-forward etag nil t))
	(delete-region point (match-end 0))
	(goto-char (point-min)))
       (t
	(setq pt2 (match-end 0))
	(goto-char (1+ point))
	(cond
	 ((not (re-search-forward ibtag nil t))
	  (delete-region point pt2)
	  (when orig-btag
	    (setq btag      orig-btag
		  orig-btag nil))
	  (goto-char (point-min)))
	 (t
	  (setq pt1 (match-beginning 0))
	  (cond
	   ((< pt2 pt1)
	    (delete-region point pt2)
	    (setq point nil)
	    (when orig-btag
	      (setq btag      orig-btag
		    orig-btag nil))
	    (goto-char (point-min)))
	   (t
	    (unless orig-btag
	      (setq orig-btag btag
		    btag      ibtag))
	    (goto-char pt1))))))))))

(defun skk-annotation-wikipedia-retrieved (&rest args)
  (cond ((or (member "deleted\n" (assq 'error (memq :error (car args))))
	     (< (buffer-size) 7)
	     (not (progn
		    (goto-char (point-max))
		    (search-backward "</html>" nil t))))
	 ;; 不完全な retrieval においても STATUS が nil となることがあるので
	 ;; ここで調整する。
	 (kill-buffer (current-buffer))
	 (ignore-errors
	   (throw 'skk-annotation-wikipedia-suspended (cadr args))))
	(t
	 (throw 'skk-annotation-wikipedia-retrieved (current-buffer)))))

;;;###autoload
(defun skk-annotation-treat-wikipedia (word &optional sources)
  "WORD が挿入されるときに表示されるべき注釈を生成する。
生成した注釈を返す。"
  (save-match-data
    (let* ((string
	    (if skk-annotation-show-wikipedia-url
		;; このときは URL を注釈とする。
		(concat "ダミー;"
			(skk-quote-char
			 (skk-annotation-generate-url
			  "http://%s.org/wiki/%s"
			  (or (car sources)
			      'ja.wikipedia)
			  word)))
	      nil))
	   (value (if string
		      ;; まだ「注釈の装飾」を受けていないので、ここで
		      ;; 適用する。
		      (if (functionp skk-treat-candidate-appearance-function)
			  (funcall skk-treat-candidate-appearance-function
				   string nil)
			string)
		    nil)))
      ;;
      (cond ((consp value)
	     ;; (候補 . 注釈) だが、候補は dummy なので破棄する。
	     (cond
	      ((consp (cdr value))
	       ;; (候補 . (セパレータ . 注釈))
	       ;; 注釈は既にセパレータ抜き
	       (cddr value))
	      ((string-match "^;" (cdr value))
	       ;; (候補 . 注釈)
	       ;; 注釈はまだセパレータを含んで
	       ;; いる
	       (substring (cdr value)
			  (match-end 0)))
	      (t
	       ;; (候補 . 注釈)
	       ;; 注釈は既にセパレータを除去して
	       ;; いるものと判断する
	       (cdr value))))
	    ;;
	    ((stringp value)
	     ;; 返り値が文字列だった場合
	     (if (string-match ";" value)
		 (substring value (match-end 0))
	       nil))
	    (t
	     ;; Wikipedia の内容の表示が要求された場合。
	     (skk-annotation-wikipedia word sources))))))

;;;###autoload
(defun skk-annotation-wikipedia-cache (word &optional sources)
  (let ((sources (or sources skk-annotation-wikipedia-sources))
	(word (skk-annotation-wikipedia-normalize-word word 'en.wiktionary))
	(cword (skk-annotation-wikipedia-normalize-word word))
	(ccword (skk-annotation-wikipedia-normalize-word word
							 'upcase-initials)))
    (catch 'found
      (while sources
	(let* ((source (pop sources))
	       (ccache-buffer (if (equal word cword)
				  nil
				(format " *skk %s %s" source cword)))
	       (cccache-buffer (if (or (equal word ccword)
				       (equal cword ccword))
				   nil
				 (format " *skk %s %s" source ccword)))
	       (cache-buffer (format " *skk %s %s" source word))
	       string)
	  (setq string
		(if (and ccache-buffer
			 (get-buffer ccache-buffer))
		    ;; Word word
		    (with-current-buffer (get-buffer ccache-buffer)
		      (buffer-string))
		  ""))
	  (if (> (length string) 0)
	      (throw 'found (cons string ccache-buffer))
	    (setq string
		  (if (and cccache-buffer
			 (get-buffer cccache-buffer))
		      ;; Word Word
		      (with-current-buffer (get-buffer cccache-buffer)
			(buffer-string))
		    ""))
	    (if (> (length string) 0)
		(throw 'found (cons string cccache-buffer))
	      (setq string
		    (if (get-buffer cache-buffer)
			;; word word
			(with-current-buffer (get-buffer cache-buffer)
			  (buffer-string))
		      ""))
	      (if (string= string "")
		  nil
		(throw 'found (cons string cache-buffer))))))))))

;;;###autoload
(defun skk-annotation-wikipedia-region-or-at-point (&optional prefix-arg start end)
  "選択領域またはポイント位置の単語を Wikipedia/Wikitionary で調べる。
領域が選択されていなければ単語の始めと終わりを推測して調べる。"
  (interactive (cons (prefix-numeric-value current-prefix-arg)
		     (cond
		      ((static-if (featurep 'xemacs)
			   (region-active-p)
			 (and transient-mark-mode mark-active))
		       (list (region-beginning) (region-end)))
		      ((eq skk-henkan-mode 'on)
		       (list (marker-position skk-henkan-start-point)
			     (point)))
		      (t
		       ;; dummy
		       (list 1 1)))))
  ;; ミニバッファにいるとき余計なメッセージをクリアする
  (when (or skk-isearch-switch
	    (skk-in-minibuffer-p))
    (message nil))
  ;;
  (let ((word (if (and (= start 1) (= end 1))
		  ;; region が active でないときは，ポイントにある
		  ;; 単語を推測する
		  (thing-at-point 'word)
		(buffer-substring-no-properties start end)))
	(sources
	 (if (and current-prefix-arg
		  (> prefix-arg 0)
		  (<= prefix-arg (length skk-annotation-wikipedia-sources)))
	     (list (nth (1- prefix-arg) skk-annotation-wikipedia-sources))
	   skk-annotation-wikipedia-sources))
	note)
    (when (and word
	       (> (length word) 0))
      (setq note (or (car (skk-annotation-wikipedia-cache word sources))
		     (skk-annotation-wikipedia word sources)))
      (skk-annotation-show (or note "") word sources))))

;;;###autoload
(defalias 'skk-annotation-wikipedia-region
  'skk-annotation-wikipedia-region-or-at-point)

(defun skk-annotation-generate-url (format-string &rest args)
  (condition-case nil
      (require 'url-util)
    (error
     (error "%s" "新しい URL パッケージが必要です")))
  (if (skk-annotation-url-package-available-p)
      (apply #'format format-string
	     (mapcar #'(lambda (element)
			 (if (stringp element)
			     (url-hexify-string element)
			   element))
		     args))
    (error "%s" "URL パッケージまたは Mule-UCS が利用できません")))

(defun skk-annotation-wikipedia-normalize-word (word &optional method)
  ;; スペースは %20 ではなく、アンダースコアに変換する
  (replace-regexp-in-string " "
			    "_"
			    (cond
			     ((memq method '(ja.wiktionary en.wiktionary))
			      (if (and (> (length word) 1)
				       (skk-ascii-char-p (aref word 0))
				       (skk-lower-case-p (aref word 1)))
				  ;; 二文字めが lower case なら downcase
				  (downcase word)
				;; 一文字だったら元の case
				;; 二文字めが upper case なら元の case
				;; 英語以外は未対応
				word))
			      ((eq method 'upcase-initials)
			       (upcase-initials word))
			      (t
			       (concat (vector (upcase (aref word 0)))
				       (substring word 1))))))

(defun skk-annotation-url-package-available-p ()
  (when (eq skk-annotation-url-package-available-p 'untested)
    ;; Emacs 22 以降以外で URL パッケージをテストする
    (cond
     ((or (and (eq skk-emacs-type 'mule4)
	       (string-lessp mule-version "4.1"))
	  (and (not (and (featurep 'xemacs)
			 (string< "21.5" emacs-version)))
	       (not (ignore-errors
		      (require 'un-define)))))
      ;; Emacs 20.7 (MULE 4.0) ではサポートしない
      ;; Emacs 20.7 (MULE 4.1) は排除しないでおく
      (setq skk-annotation-url-package-available-p nil))
     (t
      ;; Emacs 21 と XEmacs
      (defadvice url-hexify-string (around multibyte-char activate)
	(setq ad-return-value
	      (mapconcat (lambda (byte)
			   (if (memq byte url-unreserved-chars)
			       (char-to-string byte)
			     (format "%%%02x" byte)))
			 (if (multibyte-string-p (ad-get-arg 0))
			     (encode-coding-string (ad-get-arg 0) 'utf-8)
			   (ad-get-arg 0))
			 "")))
      ;;
      (setq skk-annotation-url-package-available-p t))))
  ;;
  skk-annotation-url-package-available-p)

(provide 'skk-annotation)

;;; skk-annotation.el ends here
