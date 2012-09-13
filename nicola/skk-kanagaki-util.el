;;; skk-kanagaki-util.el --- SKK の仮名入力サポートのための道具箱 -*- coding: iso-2022-jp -*-

;; Copyright (C) 2000, 2001, 2002, 2003, 2004
;;   Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

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

;; macro、inline function はここに置きます。必要な場合は各モジュールの中から
;; このプログラムをロードします。

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-vars)
  (require 'skk-macs))

(eval-when-compile
  (defvar skk-dcomp-start-point)
  (defvar skk-dcomp-end-point)
  (defvar skk-isearch-current-buffer)
  (defvar skk-nicola-okuri-flag)
  (defvar skk-nicola-hiragana-mode-string)
  (defvar skk-nicola-katakana-mode-string)
  (defvar skk-nicola-hiragana-rom-string)
  (defvar skk-nicola-katakana-rom-string))

;; Variables.

(defconst skk-kanagaki-dakuten-alist
  '(("か" "が") ("き" "ぎ") ("く" "ぐ") ("け" "げ") ("こ" "ご")
    ("さ" "ざ") ("し" "じ") ("す" "ず") ("せ" "ぜ") ("そ" "ぞ")
    ("た" "だ") ("ち" "ぢ") ("つ" "づ") ("て" "で") ("と" "ど")
    ("は" "ば" "ぱ") ("ひ" "び" "ぴ") ("ふ" "ぶ" "ぷ") ("へ" "べ" "ぺ")
    ("ほ" "ぼ" "ぽ")
    ("ウ" "ヴ")
    ("カ" "ガ") ("キ" "ギ") ("ク" "グ") ("ケ" "ゲ") ("コ" "ゴ")
    ("サ" "ザ") ("シ" "ジ") ("ス" "ズ") ("セ" "ゼ") ("ソ" "ゾ")
    ("タ" "ダ") ("チ" "ヂ") ("ツ" "ヅ") ("テ" "デ") ("ト" "ド")
    ("ハ" "バ" "パ") ("ヒ" "ビ" "ピ") ("フ" "ブ" "プ") ("ヘ" "ベ" "ペ")
    ("ホ" "ボ" "ポ"))
  "濁点と半濁点を入力するためのルール。")

(defconst skk-kanagaki-print-help-function
  (cond ((and (featurep 'emacs)
	      (>= emacs-major-version 23))
	 #'help-print-return-message)
	(t
	 #'print-help-return-message)))

;;;###autoload
(defmacro skk-kanagaki-help-1 (bufname title list)
  `(let ((buf (get-buffer-create ,bufname)))
     (with-current-buffer buf
       (setq buffer-read-only nil)
       (erase-buffer)
       (insert
	(concat
	 (format "%s\n\n" ,title)
	 (mapconcat
	  #'(lambda (cons)
	      (cond
	       ((and (symbolp (car cons))
		     (symbol-value (car cons)))
		(format "%s … %s\n"
			(key-description (symbol-value (car cons)))
			(cdr cons)))
	       (t
		(format "%s … %s\n" (car cons) (cdr cons)))))
	  ;;
	  (delq nil ,list) "")))
       ;;
       (setq buffer-read-only t)
       (set-buffer-modified-p nil)
       (goto-char (point-min))
       (help-mode))
     (let ((standard-output buf))
       (funcall skk-kanagaki-print-help-function))
     (display-buffer buf)))

;;;###autoload
(defun skk-nicola-visit-nicola-website ()
  (interactive)
  (let ((func (cond
	       ((fboundp 'browse-url)
		'browse-url)
	       (t
		'browse-url-netscape))))
    (funcall func "http://nicola.sunicom.co.jp/")))

;;;###autoload
(defun skk-kanagaki-toggle-rom-kana (&optional arg)
  "ローマ字入力 ⇔ 仮名入力 を切り替える。"
  (interactive)
  ;;
  (when (featurep 'skk-nicola)
      (setq skk-nicola-okuri-flag nil))
  ;;
  (setq skk-kanagaki-state
	(if (memq arg '(kana rom))
	    arg
	  (case skk-kanagaki-state
	    (kana 'rom)
	    (rom 'kana)
	    ;; とりあえず。
	    (t 'kana))))
  (skk-kanagaki-adjust-rule-tree)
  ;;
  (when (featurep 'skk-nicola)
    ;; モード行の表示の調節。
    (case skk-kanagaki-state
      (kana
       (setq skk-hiragana-mode-string skk-nicola-hiragana-mode-string
	     skk-katakana-mode-string skk-nicola-katakana-mode-string))
      (rom
       (setq skk-hiragana-mode-string skk-nicola-hiragana-rom-string
	     skk-katakana-mode-string skk-nicola-katakana-rom-string)))
    ;;
    (skk-modify-indicator-alist 'katakana skk-katakana-mode-string)
    (skk-modify-indicator-alist 'hiragana skk-hiragana-mode-string)
    ;;
    (skk-loop-for-buffers (buffer-list)
      (when (and skk-j-mode
		 (listp mode-line-format))
	(skk-update-modeline (if skk-katakana
				 'katakana
			       'hiragana))))))

;;;###autoload
(defun skk-kanagaki-dakuten (&optional arg handakuten)
  "直前の文字を見て可能なら濁点を付加し、さもなければ \"゛\" を入力する。"
  (interactive "*p")
  (let ((list skk-kanagaki-dakuten-alist)
	(pt1 (point))
	(henkan-on (and skk-isearch-switch
			(with-current-buffer
			    (get-buffer-create skk-isearch-working-buffer)
			  (eq skk-henkan-mode 'on))))
	char1 char2 str)
    (ignore-errors
      (setq char1 (cond
		   ((and skk-isearch-switch
			 (not (skk-in-minibuffer-p)))
		    (if henkan-on
			(with-current-buffer skk-isearch-working-buffer
			  (skk-save-point
			   (backward-char 1)
			   (buffer-substring-no-properties
			    (point)
			    pt1)))
		      (substring isearch-string -1)))
		   (t
		    (skk-save-point
		     (backward-char 1)
		     (buffer-substring-no-properties
		      (point)
		      pt1))))))
    (cond
     ((setq char2 (nth (if handakuten 2 1) (assoc char1 list)))
      (cond
       ((and skk-isearch-switch
	     (not (skk-in-minibuffer-p)))
	(if henkan-on
	    (with-current-buffer skk-isearch-working-buffer
	      (delete-char -1)
	      (skk-insert-str char2))
	  (setq str isearch-string)
	  (while (string= str (if (vectorp (car isearch-cmds))
				  (aref (car isearch-cmds) 0)
				(caar isearch-cmds)))
	    (with-current-buffer skk-isearch-current-buffer
	      (skk-isearch-delete-char arg)))
	  (setq isearch-string (concat (if (vectorp (car isearch-cmds))
					   (aref (car isearch-cmds) 0)
					 (caar isearch-cmds))
				       char2)
		isearch-message (concat
				 (skk-isearch-mode-string)
				 (mapconcat
				  #'isearch-text-char-description
				  isearch-string "")))
	  (put 'isearch-barrier 'skk-kanagaki t)
	  (skk-unread-event (character-to-event
			     (aref (where-is-internal
				    (if isearch-forward 'isearch-repeat-forward
				      'isearch-repeat-backward)
				    isearch-mode-map t)
				   0)))))
       (t
	(delete-char -1)
	(skk-insert-str char2))))
     (t
      (skk-insert-str (if handakuten
			  "゜"
			"゛"))))))

(defadvice isearch-repeat (around skk-kanagaki-workaround activate)
  (cond ((get 'isearch-barrier 'skk-kanagaki)
	 (goto-char isearch-barrier)
	 ad-do-it
	 (put 'isearch-barrier 'skk-kanagaki nil))
	(t
	 ad-do-it)))

;;;###autoload
(defun skk-kanagaki-handakuten (&optional arg)
  "直前の文字を見て可能なら半濁点を付加し、さもなければ \"゜\" を入力する。"
  (interactive "*p")
  (skk-kanagaki-dakuten arg t))

;;;###autoload
(defun skk-kanagaki-bs (arg)
  ;; OASYS における BS キーの機能の代わり。どのような挙動をさせるべきかまだ決ま
  ;; っていない。現在のところ
  ;;
  ;; o ▼モードでは `skk-kanagaki-esc' と同じ挙動
  ;; o ▽モードでは `skk-delete-backward-char' と同じ挙動
  ;; o ■モードでは `delete-backward-char' と同じ挙動
  ;;
  ;; というふうに考えている。
  (interactive "*p")
  ;;
  (cond
   ((eq skk-henkan-mode 'active)
    (call-interactively 'keyboard-quit))
   ((and (eq skk-henkan-mode 'on)
	 (= (point) (marker-position
		     skk-henkan-start-point)))
    (skk-kakutei arg))
   ((eq skk-henkan-mode 'on)
    (forward-char -1)
    (delete-char 1))
   ((and skk-isearch-switch
	 (buffer-live-p skk-isearch-current-buffer))
    (with-current-buffer skk-isearch-current-buffer
      (skk-isearch-delete-char arg)))
   (t
    (delete-char (- 0 arg))))
  ;;
  (when skk-dcomp-activate
    (skk-dcomp-after-delete-backward-char)))

;;;###autoload
(let ((property (if (featurep 'xemacs)
		    'pending-del
		  'delete-selection)))
  (put 'skk-kanagaki-bs property 'supersede))

;;;###autoload
(defun skk-kanagaki-esc (&optional arg)
  ;; OASYS における取り消し機能の代わり。 とりあえず keyboard-quit の場合と同様
  ;; の動作をするようににしておく。OAK β版だと
  ;;
  ;; o 1 回目の取り消しで、変換前の状態に戻した上で変換開始点にポイントを移動
  ;; o 2 回目の取り消しで変換対象の文字列全体を消去
  ;;
  ;; するようになっているが、SKK における変換対象の文字列は ▽ とポイントの間の
  ;; 文字列であり、ポイントを移動すると変換対象が変わってしまう。そのため、ポイ
  ;; ントは移動しないこととする。
  (interactive "*P")
  (cond
   ((skk-in-minibuffer-p)
    (call-interactively
     (if (fboundp 'minibuffer-keyboard-quit)
	 'minibuffer-keyboard-quit
       'abort-recursive-edit)))
   (skk-henkan-mode
    (call-interactively 'keyboard-quit))
   (t
    nil)))

(provide 'skk-kanagaki-util)

;;; skk-kanagaki-util.el ends here
