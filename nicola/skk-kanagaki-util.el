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

;; NICOLA-DDSKK にとって必ずしも重要度の高くないもの、macro、inline function は
;; ここに置きます。  必要な場合は各モジュールの中からこのプログラムをロードしま
;; す。

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'static))

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
    ("ホ" "ボ" "ポ")
    ;;
    ) "\
濁点と半濁点を入力するためのルール。")


;; Macros

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
		       (key-description (symbol-value (car cons))) (cdr cons)))
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
		(expand-file-name "kanagaki" skk-kanagaki-temp-dir))))
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
     (` (progn (,@ form)))
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
    (` (mapconcat 'identity (make-vector (, n) (, str)) "")))))

;; Functions.

;;;###autoload
(defun skk-kanagaki-dakuten (&optional arg)
  "直前の文字を見て可能なら濁点を付加し、さもなければ \"゛\" を入力する。"
  (interactive "*p")
  (let ((list skk-kanagaki-dakuten-alist)
	(pt1 (point))
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	char1 char2)
    (setq char1
	  (save-excursion
	    (backward-char (* len 1))
	    (buffer-substring-no-properties (point) pt1)))
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
	(len (if (eq skk-emacs-type 'nemacs) 2 1))
	char1 char2)
    (setq char1
	  (save-excursion
	    (backward-char (* len 1))
	    (buffer-substring-no-properties (point) pt1)))
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
  (cond (skk-henkan-active
	 (call-interactively 'keyboard-quit))
	(skk-henkan-on
	 (if (= (point) (marker-position skk-henkan-start-point))
	     (skk-kakutei arg)
	   (forward-char -1)
	   (delete-char 1)))
	((and skk-isearch-switch
	      (buffer-live-p skk-kanagaki-isearch-buffer))
	 (with-current-buffer skk-kanagaki-isearch-buffer
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
  (cond ((skk-in-minibuffer-p)
	 (call-interactively
	  (if (fboundp 'minibuffer-keyboard-quit)
	      'minibuffer-keyboard-quit
	    'abort-recursive-edit)))
	 ((or skk-henkan-on skk-henkan-active)
	  (call-interactively 'keyboard-quit))
	 (t
	  nil)))

;;;###autoload
(defun skk-nicola-visit-nicola-website ()
  (interactive)
  (let ((func (cond ((fboundp 'browse-url)
		     'browse-url)
		    (t
		     'browse-url-netscape))))
    (funcall func "http://nicola.sunicom.co.jp/")))


;;

(require 'product)
(product-provide (provide 'skk-kanagaki-util) (require 'skk-version))

;;; skk-kanagaki-util.el ends here
