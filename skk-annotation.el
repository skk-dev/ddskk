;;; skk-annotation.el --- SKK annotation 関連プログラム

;; Copyright (C) 2000, 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-annotation.el,v 1.64 2007/03/20 05:42:27 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Created: Oct. 27, 2000.
;; Last Modified: $Date: 2007/03/20 05:42:27 $

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
;; the Free Software Foundation Inc., 51 Franklin St, Fifth Floor,
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
;; `;' の文字を含んだ候補は、eval すると `;' になる Lisp 式として
;; quote されて辞書候補として収められなければなりません。既存の辞書に
;; ついては、辞書を読み込んだバッファで
;;
;;   M-x skk-annotation-update-jisyo-format
;;
;; することでこの作業を行なうことができます。個人辞書、SKK-JISYO.L に
;; ついては是非行なっておいた方が良いでしょう。
;; SKK Openlab で今後配布する辞書は `;' は予め quote されている状態に
;; します。
;; 但し、既にアノテーションが付けられている場合は、このアノテーション
;; 自体も候補と区別できずに quote されてしまいますので、ご注意下さい
;; (今のところ手作業で quote されないように退避するなどしか方法はあり
;; ません)。
;;
;; Viper 対策はまだ行なっていません。~/.viper に次のように書いて下さい。
;; (viper-harness-minor-mode "skk-annotation")

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars)
  (require 'static))

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
    (insert annotation)))

;; functions.
;;;###autoload
(defun skk-annotation-get (annotation)
  (if (string= annotation "")
      ""
    (if (eq (aref annotation 0) ?*)
	(substring annotation 1)
      annotation)))

;;;###autoload
(defun skk-annotation-show (annotation &optional word)
  (unless skk-kakutei-flag
    (when (or (not skk-annotation-function)
	      (funcall skk-annotation-function annotation))
      (skk-annotation-show-1 (skk-annotation-get annotation) word))))

(defun skk-annotation-show-1 (annotation &optional word)
  (let ((notes (mapcar #'skk-eval-string (split-string annotation ";"))))
    (setq annotation (skk-eval-string annotation))
    (unless (string= annotation "")
      (skk-annotation-show-2 annotation))
    ;; 注釈の表示はここまでだが、ここでユーザが注釈の内容をコピーしたり
    ;; して利用できるようにする。
    (skk-annotation-wait-for-input annotation notes word)))

(defun skk-annotation-show-2 (annotation)
  (cond
   ((and (eval-when-compile (eq skk-emacs-type 'mule5))
	 window-system
	 skk-show-tooltip)
    (skk-tooltip-show-at-point annotation))
   ((and skk-annotation-show-as-message
	 (not (skk-in-minibuffer-p)))
    (skk-annotation-show-as-message annotation))
   ((and (not (skk-annotation-display-p 'minibuf))
	 (skk-in-minibuffer-p))
    ;; do nothing
    nil)
   (t
    (skk-annotation-show-buffer annotation))))

(defun skk-annotation-wait-for-input (annotation notes &optional word)
  (let* ((copy-command (key-binding skk-annotation-copy-key))
	 (browse-command (key-binding skk-annotation-browse-key))
	 (list (list copy-command browse-command))
	 event key command urls note)
    (while (and list
		(condition-case nil
		    (progn
		      (setq event (next-command-event)
			    key (skk-event-key event)
			    command (key-binding key))
		      ;; Return value of the following expression is important.
		      (or (memq command list)
			  (equal (key-description key)
				 (key-description
				  skk-annotation-wikipedia-key))))
		  (quit
		   nil)))
      (cond ((eq command copy-command)
	     (setq list (delq copy-command list))
	     (unless (equal annotation "")
	       (kill-new (substring-no-properties annotation))
	       (skk-message "現在の注釈をコピーしました"
			    "Copying the current note...done")
	       (setq event nil)
	       (skk-annotation-show-2 annotation)))
	    ((eq command browse-command)
	     (setq list (delq browse-command list))
	     (setq urls (delq nil (mapcar #'skk-annotation-find-url notes)))
	     (unless (equal annotation "")
	       (cond (urls
		      (dolist (url urls)
			(browse-url url))
		      (skk-message "注釈内のサイトをブラウズしています..."
				   "Browsing sites in the current notes..."))
		     (t
		      (skk-message "注釈内にサイトが見つかりません"
				   "No sites found in the current notes")))
	       (setq event nil)
	       (skk-annotation-show-2 annotation)))
	    ((equal (key-description key)
		    (key-description skk-annotation-wikipedia-key))
	     (setq event nil)
	     (when word
	       (let ((skk-annotation-show-wikipedia-url nil))
		 (setq note (skk-annotation-treat-wikipedia word))))
	     (when (null note)
	       (setq note annotation))
	     (unless (equal note "")
	       (skk-annotation-show-2 (or note annotation))))
	    (t
	     (setq list nil))))
    (when event
      (skk-unread-event event))))

(defun skk-annotation-find-url (string)
  (let (url)
    (with-temp-buffer
      (insert string)
      (goto-char (point-max))
      (setq url (thing-at-point 'url))
      (while (not (or url (bobp)))
	(backward-char 1)
	(setq url (thing-at-point 'url)))
      url)))

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
		   (static-if (eq skk-emacs-type 'xemacs)
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
(defun skk-annotation-update-jisyo-format ()
  (interactive)
  (skk-setup-jisyo-buffer)
  (let ((min skk-okuri-ari-min) (max skk-okuri-ari-max))
    (skk-annotation-update-jisyo-format-1 min max)
    (setq min skk-okuri-nasi-min
	  max (point-max))
    (skk-annotation-update-jisyo-format-1 min max)))

(defun skk-annotation-update-jisyo-format-1 (min max)
  (let (candidate)
    (goto-char min)
    (while (re-search-forward "\\/\\([^\n/]*;[^\n/]*\\)\\/" max t nil)
      (setq candidate (buffer-substring-no-properties
		       (match-beginning 1) (match-end 1)))
      (delete-region (match-beginning 1) (match-end 1))
      (goto-char (match-beginning 1))
      (insert
       (concat "(concat \""
	       (mapconcat
		(function
		 (lambda (c)
		   (if (eq c ?\;)
		       "\\073"
		     (char-to-string c))))
		(append candidate nil) "")
	       "\")")))))

;;;###autoload
(defun skk-annotation-wikipedia (word)
  (let ((sources skk-annotation-wikipedia-sources)
	(string "")
	(note nil))
    (while (and (not note)
		sources)
      (setq note (skk-annotation-wikipedia-1 word
					     (car sources)
					     (= 1 (length sources))))
      (setq string (format (if (string= "" string)
			       "%s%s"
			     "%s/%s")
			     string (car sources)))
      (setq sources (cdr sources)))
    (unless note
      (message "%s に項目がありません" string))
    note))

(defun skk-annotation-wikipedia-1 (word source last)
  "Wiktionary/Wikipedia の WORD に相当する記事からアノテーションを取得する。"
  (let ((cache-buffer (format " *skk wikipedia %s *" word))
	(html2text-remove-tag-list
	 '("a" "p" "img" "dir" "head" "div" "br" "font" "span" "sup"
	   "table" "tr" "td" "h2" "h3" "h4"))
	(sources skk-annotation-wikipedia-sources)
	buffer html note aimai continue nop point)
    (if (get-buffer cache-buffer)
	(with-current-buffer cache-buffer
	  (setq note (buffer-string)))
      ;; キャッシュがない場合
      (setq buffer (url-http (url-generic-parse-url
			      (format
			       "http://ja.%s.org/wiki/%s"
			       source
			       (url-hexify-string
				(upcase-initials word))))
			     #'skk-annotation-wikipedia-retrieved
			     ()))
      (when (catch 'retrieved
	      (progn
		(skk-sit-for 100 t)
		t))
	(when (buffer-live-p buffer)
	  (with-current-buffer buffer
	    (setq html (buffer-string)))
	  (kill-buffer buffer)
	  (when html
	    (with-current-buffer (get-buffer-create cache-buffer)
	      (insert (decode-coding-string html 'utf-8))
	      ;;
	      (cond
	       ((eq source 'wiktionary)
		(goto-char (point-min))
		(search-forward "<!-- start content -->" nil t)
		(save-excursion
		  (when (and
			 (re-search-forward
			  "<h2>.*<span class=\"mw-headline\">日本語</span></h2>"
			  nil t)
			 (re-search-forward
			  "<h2>.*<span class=\"mw-headline\">.+語</span></h2>"
			  nil t))
		    (delete-region (match-beginning 0) (point-max))))
		(if (save-excursion
		      (search-forward "<div class=\"noarticletext\">" nil t))
		    ;;
		    (progn
		      (erase-buffer)
		      (setq html ""))
		  (setq point nil)
		  (while (re-search-forward "<span class=\"mw-headline\">\
\\(\\(名\\|動\\|形容動?\\|副\\)詞.*\\|漢字混じり表記\\|意義\\)</span>" nil t)
		    (setq nop t)
		    (save-match-data
		      (when (looking-at "</h3>")
			(delete-region (match-beginning 0) (match-end 0))))
		    (goto-char (match-beginning 0))
		    (delete-region (or point (point-min)) (point))
		    (unless (null point)
		      (insert "\n"))
		    (save-match-data
		      (or (re-search-forward "</\\(u\\|o\\)l>" nil t)
			  (search-forward "</dl>" nil t))
		      ;;		    (insert "<p>")
		      (setq point (point))))
		  (when point
		    (delete-region point (point-max)))
		  (save-excursion
		    (goto-char (point-min))
		    (save-match-data
		      (while (re-search-forward
			      "<span.*>\\[<a.+>編集</a>\\]</span>"
			      nil t)
			(replace-match "")))))
		)
	       ((eq source 'wikipedia)
		(goto-char (point-min))
		(setq aimai
		      (save-excursion
			(search-forward "<a href=\"/wiki/Wikipedia:\
%E6%9B%96%E6%98%A7%E3%81%95%E5%9B%9E%E9%81%BF\"" nil t)))
		(search-forward "<!-- start content -->" nil t)
		(if (save-excursion
		      (search-forward "<div class=\"noarticletext\">" nil t))
		    ;;
		    (progn
		      (erase-buffer)
		      (setq html ""))
		  (setq point (point))
		  (when (or (when (re-search-forward
				   "<p>\\(<br />\n\\|[^\n]*\\)?\
<b>[^\n]+</b>[^\n]+</p>"
				   nil t)
			      (goto-char (match-beginning 0))
			      (if (and (save-excursion
					 (re-search-forward "</p>" nil t))
				       (string-match "。\\|．"
						     (buffer-substring
						      (point)
						      (match-beginning 0))))
				  t
				(setq point (point)
				      continue t)
				nil))
			    (when (progn
				    (goto-char point)
				    (re-search-forward "<\\(u\\|o\\)l>" nil t))
			      (goto-char (if continue
					     point
					   (match-beginning 0)))
			      (setq nop t)))
		    (delete-region (point-min) (point))
		    (goto-char (point-min))
		    (re-search-forward (if (or aimai nop)
					   "</\\(u\\|o\\)l>"
					 "</p>")
				       nil t)
		    (delete-region (point) (point-max))))))
	      ;;
	      (unless (equal html "")
		(html2text)
		(goto-char (point-min))
		(while (looking-at "^[ \t]*$")
		  (kill-line 1))
		  (cond ((or aimai nop)
			 (while (not (eobp))
			   (beginning-of-line)
			   (setq point (point))
			   (forward-line 1)
			   (fill-region point (point)))
			 (when aimai
			   (insert "\n(曖昧さ回避のページ)")))
			(t
			 (fill-paragraph nil)))
		  ;;
		  (goto-char (point-max))
		  (while (and (looking-at "^$")
			      (not (string= "" (buffer-string))))
		    (delete-char -1))
		  ;;
		  (when (and (not (equal (buffer-string) ""))
			     (not (get-text-property 1 'face)))
		    (put-text-property 1 2 'face 'default))
		  (setq note (buffer-string)))))))
      ;;
      (when (and (not last)
		 (get-buffer cache-buffer)
		 (string= "" (with-current-buffer cache-buffer
			       (buffer-string))))
	(kill-buffer cache-buffer)))
    ;;
    (cond ((stringp note)
	   (if (equal note "")
	       nil
	     note))
	  (t
	   nil))))

(defun skk-annotation-wikipedia-retrieved (&rest args)
  (ignore-errors
    (throw 'retrieved t)))

;;;###autoload
(defun skk-annotation-treat-wikipedia (word)
  "WORD が挿入されるときに表示されるべき注釈を生成する。
生成した注釈を返す。"
  (save-match-data
    (let* ((string
	    (if skk-annotation-show-wikipedia-url
		;; このときは URL を注釈とする。
		(concat "ダミー;"
			(skk-quote-char
			 (format "http://ja.wikipedia.org/wiki/%s"
				 (url-hexify-string word))))
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
	     (skk-annotation-wikipedia word))))))

;;;###autoload
(defun skk-annotation-wikipedia-cache (word)
  (let* ((cache-buffer (format " *skk wikipedia %s *" word))
	 (string (if (get-buffer cache-buffer)
		     (with-current-buffer (get-buffer cache-buffer)
		       (buffer-string))
		   "")))
    (if (string= string "")
	nil
      string)))

;;;###autoload
(defun skk-annotation-wikipedia-region (start end)
  (interactive "r")
  (let ((word (buffer-substring-no-properties start end))
	note)
    (when (> (length word) 0)
      (setq note (or (skk-annotation-wikipedia-cache word)
		     (skk-annotation-wikipedia word)))
      (when note
	(skk-annotation-show note)))))

(require 'product)
(product-provide
    (provide 'skk-annotation)
  (require 'skk-version))

;;; skk-annotation.el ends here
