;;; skk-kanagaki-util.el --- SKK の仮名入力サポートのための道具箱
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; macro、inline function はここに置きます。必要な場合は各モジュールの中から
;; このプログラムをロードします。

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-macs)
  (require 'static))

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
    ("ホ" "ボ" "ポ")) "\
濁点と半濁点を入力するためのルール。")

(defvar skk-kanagaki-temp-dir
  (static-cond
   ((fboundp 'temp-directory)
    (temp-directory))
   (t
    (cond
     ((and (boundp 'temporary-file-directory)
	   temporary-file-directory)
      temporary-file-directory)
     (t
      (or (getenv "TMP")
	  "/tmp"))))))

;;;###autoload
(defmacro skk-kanagaki-help-1 (bufname title list)
  (`
   (let ((buf (get-buffer-create (, bufname))))
     (save-excursion
       (set-buffer buf)
       (setq buffer-read-only nil)
       (erase-buffer)
       (insert
	(concat
	 (format "%s\n\n" (, title))
	 (mapconcat
	  (function
	   (lambda (cons)
	     (cond
	      ((and (symbolp (car cons))
		    (symbol-value (car cons)))
	       (format "%s … %s\n"
		       (key-description (symbol-value (car cons)))
		       (cdr cons)))
	      (t
	       (format "%s … %s\n" (car cons) (cdr cons))))))
	  ;;
	  (delq nil (, list)) "")))
       ;;
       (setq buffer-read-only t)
       (set-buffer-modified-p nil)
       (goto-char (point-min))
       (help-mode))
     (let ((standard-output buf))
       (print-help-return-message))
     (display-buffer buf))))

;;;###autoload
(put 'skk-kanagaki-call-xmodmap 'lisp-indent-function 1)

;;;###autoload
(defmacro skk-kanagaki-call-xmodmap (string &rest form)
  ;; STRING の内容を xmodmap に渡す。成功したら FORM を実行する。
  (list
   'let '((x (eq window-system 'x))
	  (prog (exec-installed-p "xmodmap"))
	  (tmp (make-temp-name
		(expand-file-name "kanagaki"
				  skk-kanagaki-temp-dir))))
   (list
    'cond
    (list
     (list 'and 'x 'prog
	   '(message "xmodmap を呼んでいます...")
	   (list
	    'save-excursion
	    '(set-buffer (get-buffer-create " *kanagaki*"))
	    '(erase-buffer)
	    (list 'insert string)
	    '(write-region (point-min) (point-max) tmp)
	    '(eq 0 (call-process prog nil nil nil tmp))))
     ;;
     (` (progn
	  (,@ form)))
     '(delete-file tmp)
     '(message "xmodmap を呼んでいます...完了"))
    '(t
      (message "xmodmap の呼び出しに失敗しました")))))

;;;###autoload
(defmacro skk-kanagaki-make-string (n str)
  (case skk-emacs-type
   ((xemacs mule4 mule5)
    (` (make-string (, n) (string-to-char (, str)))))
   (t
    (` (mapconcat 'identity
		  (make-vector (, n) (, str)) "")))))

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
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
	(with-current-buffer buf
	  (when (and skk-j-mode (listp mode-line-format))
	    ;;
	    (skk-update-modeline (if skk-katakana
				     'katakana
				   'hiragana))))))))

;;;###autoload
(defun skk-kanagaki-dakuten (&optional arg)
  "直前の文字を見て可能なら濁点を付加し、さもなければ \"゛\" を入力する。"
  (interactive "*p")
  (let ((list skk-kanagaki-dakuten-alist)
	(pt1 (point))
	char1 char2)
    (condition-case nil
	(setq char1
	      (skk-save-point
		(backward-char 1)
		(buffer-substring-no-properties
		 (point)
		 pt1)))
      (error))
    (cond ((setq char2 (cadr (assoc char1 list)))
	   (delete-char -1)
	   (skk-insert-str char2))
	  (t
	   (skk-insert-str "゛")))))

;;;###autoload
(defun skk-kanagaki-handakuten (&optional arg)
  "直前の文字を見て可能なら半濁点を付加し、さもなければ \"゜\" を入力する。"
  (interactive "*p")
  (let ((list skk-kanagaki-dakuten-alist)
	(pt1 (point))
	char1 char2)
    (condition-case nil
	(setq char1
	      (skk-save-point
		(backward-char 1)
		(buffer-substring-no-properties
		 (point)
		 pt1)))
      (error))
    (cond ((setq char2 (caddr (assoc char1 list)))
	   (delete-char -1)
	   (skk-insert-str char2))
	  (t
	   (skk-insert-str "゜")))))

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
  ;; skk-dcomp 利用時の際の使い勝手をよくする。
  (when (and (featurep 'skk-dcomp)
	     skk-henkan-on
	     (not skk-henkan-active)
	     (markerp skk-dcomp-end-point)
	     (marker-position skk-dcomp-end-point))
    (delete-region
     skk-dcomp-end-point
     (if (< (point) (marker-position skk-dcomp-start-point))
	 skk-dcomp-start-point
       (point))))
  ;;
  (cond
   (skk-henkan-active
    (call-interactively 'keyboard-quit))
   (skk-henkan-on
    (if (= (point) (marker-position skk-henkan-start-point))
	(skk-kakutei arg)
      (forward-char -1)
      (delete-char 1)))
   ((and skk-isearch-switch
	 (buffer-live-p skk-isearch-current-buffer))
    (with-current-buffer skk-isearch-current-buffer
      (skk-isearch-delete-char arg)))
   (t
    (delete-backward-char arg))))

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
   ((or skk-henkan-on skk-henkan-active)
    (call-interactively 'keyboard-quit))
   (t
    nil)))

;;

(require 'product)
(product-provide
    (provide 'skk-kanagaki-util)
  (require 'skk-version))

;;; skk-kanagaki-util.el ends here
