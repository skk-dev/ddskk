;;; skk-annotation.el --- SKK annotation 関連プログラム
;; Copyright (C) 2000, 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-annotation.el,v 1.3 2001/08/26 08:19:47 czkmt Exp $
;; Keywords: japanese
;; Created: Oct. 27, 2000.
;; Last Modified: $Date: 2001/08/26 08:19:47 $

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
;; along with Daredevil SKK, see the file COPYING.  If not, write to the
;; Free Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.
;;
;;; Commentary:
;;
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
;; システムアノテーションが装備された辞書は今のところありません。
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
;;
;;; Code:
(eval-when-compile
  (require 'skk-macs) (require 'skk-vars) (require 'static))

(if skk-annotation-mode-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'skk-annotation-save-and-quit)
    (define-key map "\C-c\C-k" 'skk-annotation-kill)
    (setq skk-annotation-mode-map map)))

(or (assq 'skk-annotation-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(skk-annotation-mode " annotation")
				 minor-mode-alist)))
(when (boundp 'minor-mode-map-alist)
  (or (assq 'skk-annotation-mode-map minor-mode-map-alist)
      (setq minor-mode-map-alist
	    (cons (cons 'skk-annotation-mode skk-annotation-mode-map)
		  minor-mode-map-alist))))

;; inline functions.
(defsubst skk-annotation-erase-buffer ()
  (let ((inhibit-read-only t)
	buffer-read-only)
    (static-if (fboundp 'set-text-properties)
	(set-text-properties (point-min) (point-max) nil))
    (erase-buffer)))

(defsubst skk-annotation-insert (annotation)
  (with-current-buffer (get-buffer-create skk-annotation-buffer)
    (skk-annotation-erase-buffer)
    (insert (skk-eval-string annotation))))

(defsubst skk-annotation-get (annotation)
  (or (string= annotation "")
      (if (eq (aref annotation 0) ?*)
	  (substring annotation 1)
	annotation)))

;; advices.

;; functions.
;;;###autoload
(defun skk-annotation-show (annotation)
  (if (or (not skk-annotation-function)
	  (funcall skk-annotation-function annotation))
      (skk-annotation-show-1 (skk-annotation-get annotation))))

(defun skk-annotation-show-1 (annotation)
  (if (and skk-annotation-show-as-message
	   (not (skk-in-minibuffer-p)))
      (skk-annotation-show-as-message annotation)
    (skk-annotation-show-buffer annotation)))

(defun skk-annotation-show-buffer (annotation)
  (condition-case nil
      (save-window-excursion
	(let ((minibuf-p (skk-in-minibuffer-p))
	      event char key)
	  (skk-annotation-insert annotation)
	  (cond
	   (minibuf-p
	    (select-window (get-buffer-window (skk-minibuffer-origin)))
	    (unless (eq (next-window) (selected-window))
	      (delete-other-windows)))
	   (t
	    (split-window-vertically)))
	  (display-buffer skk-annotation-buffer)
	  (when minibuf-p
	    (select-window (minibuffer-window)))
	  (setq event (next-command-event)
		key (skk-event-key event))
	  (when (skk-key-binding-member
		 key
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
	(car (where-is-internal 'skk-previous-candidate skk-j-mode-map))
	0))))))

(defun skk-annotation-show-as-message (annotation)
  (if (> skk-henkan-count 3)
      ;; cannot use echo area, so we should use other window.
      (skk-annotation-show-buffer annotation)
    (message (skk-eval-string annotation))))

(defun skk-annotation-setup ()
  (if (skk-get-last-henkan-datum 'henkan-list)
      (setq skk-annotation-annotated-word
	    (list 
	     (skk-get-last-henkan-datum 'henkan-key)
	     (skk-get-last-henkan-datum 'okuri-char)
	     (skk-get-last-henkan-datum 'henkan-list)))
    (setq skk-henkan-key
	  (read-from-minibuffer
	   "Midasi: " nil
	   (static-when (memq skk-emacs-type '(nemacs mule1))
	     (with-current-buffer
		 (get-buffer-create
		  (format " *Minibuf-%d*" (minibuffer-depth)))
	       (skk-j-mode-on))
	     (append skk-j-mode-map (cdr minibuffer-local-map)))))
    (if (not skk-henkan-key)
	(skk-error "アノテーションを付ける単語がありません"
		   "No annotated word")
      (setq skk-annotation-annotated-word
	    (list skk-henkan-key
		  (if (string-match "^[^a-zA-Z]+\\([a-z]+\\)$" skk-henkan-key)
		      (setq skk-okuri-char
			    (substring skk-henkan-key (match-beginning 1))
			    ;; 送りあり変換を指定すると skk-henkan-okurigana の指定に困る。
			    skk-henkan-okurigana ""))
		  (list (skk-henkan-in-minibuff))))
      (skk-kakutei))))

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
	   (wholestring (car (nth 2 skk-annotation-annotated-word)))
	   (realword (and wholestring
			  (string-match ";\\**" wholestring)
			  (substring wholestring 0 (match-beginning 0))))
	   (annotation (and realword (string-match ";\\**" wholestring)
			    (substring wholestring (match-end 0)))))
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
       (format ";; Add annotation to word `%s' (this line will not be added as a part of annotation.)\n"
	       realword))
      (static-if (fboundp 'set-text-properties)
	  (add-text-properties (point-min) (1- (point)) plist))
      (if (and (not no-previous-annotation) annotation) (insert annotation))
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
	(if (looking-at "^[\t ]*;")
	    (progn
	      (forward-line 1)
	      (beginning-of-line)))
	(setq annotation (buffer-substring-no-properties
			  (point) (point-max)))
	(if (string-match "^[\t\n 　]+" annotation)
	    (setq annotation (substring annotation (match-end 0))))
	(if (string-match "[\t\n 　]+$" annotation)
	    (setq annotation (substring annotation 0 (match-beginning 0))))
	(if (string= annotation "")
	    (setq annotation nil))
	(setq annotation (skk-quote-char annotation))))
    (if annotation
	(skk-annotation-last-word-1 
	 (lambda (beg end)
	   (goto-char beg)
	   (if (re-search-forward ";[^/]*" end t)
	       (delete-region (match-beginning 0) (match-end 0)))
	   (goto-char end)
	   (insert ";*" annotation))))
    (set-window-configuration
     skk-annotation-original-window-configuration)
    (if annotation (or quiet (message "Added annotation")))))

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
    (if (yes-or-no-p
	 (format (if skk-japanese-message-and-error
		     "%s についてのアノテーションを削除しますか？ "
		   "Really delete annotation for %s? ")
		 (car (nth 2 skk-annotation-annotated-word))))
	(skk-annotation-last-word-1 
	 (lambda (beg end)
	   (goto-char beg)
	   (if (re-search-forward ";[^/]*" end t)
	       (delete-region (match-beginning 0) (match-end 0))))))))

(defun skk-annotation-last-word-1 (function)
  ;; funcall FUNCTION with BEG and END where BEG and END are markers.
  (let ((inhibit-quit t)
	(jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
	(word (car (nth 2 skk-annotation-annotated-word)))
	(beg (make-marker)) (end (make-marker))
	(eol (make-marker))
	pattern)
    (if (not jisyo-buffer)
	nil
      (save-match-data
	(with-current-buffer jisyo-buffer
	  (goto-char (if (nth 1 skk-annotation-annotated-word)
			 skk-okuri-ari-min skk-okuri-nasi-min))
	  (if (not (re-search-forward 
		    (concat "^\\("
			    (regexp-quote (car skk-annotation-annotated-word))
			    "\\) /")
		    (if (nth 1 skk-annotation-annotated-word)
			skk-okuri-ari-max nil)
		    t nil))
	      nil 
	    (goto-char (match-beginning 1))
	    (set-marker eol (skk-save-point (end-of-line) (point)))
	    (if (string-match ";" word)
		(setq word (substring word 0 (match-beginning 0))))
	    (if (not (re-search-forward
		      (concat "/\\(" word "\\)\\(;[^/]*\\)*/")
		      eol t nil))
		nil
	      (set-marker beg (match-beginning 1))
	      (set-marker end (or (match-end 2) (match-end 1)))
	      (funcall function beg end)
	      (if (not (nth 1 skk-annotation-annotated-word))
		  nil
		(goto-char end)
		;; skip other candidates that has not a okuirigana.
		(search-forward "/[" eol t nil)
		(setq pattern (concat "/\\(" word "\\)\\(;[^/]*\\)*/"))
		(while (re-search-forward pattern eol t nil)
		  (set-marker beg (match-beginning 1))
		  (set-marker end (or (match-end 2) (match-end 1)))
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
       (if (string-match ";" candidate)
	   (progn
	     (delete-region beg end)
	     (insert (skk-quote-semicolon candidate))
	     (or quiet
		 (message "Quoted"))))))))

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
		 (lambda (c) (if (eq c ?\;) "\\073" (char-to-string c))))
		(append candidate nil) "")
	       "\")")))))

(require 'product)
(product-provide (provide 'skk-annotation) (require 'skk-version))
;;; end of skk-annotation.el.
