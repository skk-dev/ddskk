;; skk-viper.el --- SKK related code for Viper
;; Copyright (C) 1996, 1997, 1998, 1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>, Murata Shuuichirou <mrt@astec.co.jp>
;;
;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>,
;;         Murata Shuuichirou <mrt@astec.co.jp>
;; Maintainer: Murata Shuuichirou <mrt@astec.co.jp>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-viper.el,v 1.3 1999/08/29 21:25:58 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/08/29 21:25:58 $

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

;; Following people contributed modifications to skk.el (Alphabetical order):
;;      Mikio Nakajima <minakaji@osaka.email.ne.jp>
;;      Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>

;;; Change log:

;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-foreword)
(require 'viper)

;;(defgroup skk-viper nil "SKK/Viper related customization."
;;  :prefix "skk-"
;;  :group 'skk )

;; internal constant.
;;;###autoload
(defconst skk-viper-use-vip-prefix
  (not (fboundp 'viper-normalize-minor-mode-map-alist)) )

;;;###autoload
(defconst skk-viper-normalize-map-function
  (if skk-viper-use-vip-prefix 
      'vip-normalize-minor-mode-map-alist 
    'viper-normalize-minor-mode-map-alist )
  "Viper が minor-mode-map-alist を調整するための関数。" )

;; macros and inline functions.
(defmacro skk-viper-advice-select (viper vip arg body)
  (` (if skk-viper-use-vip-prefix
	 (defadvice (, vip) (, arg) (,@ body))
       (defadvice (, viper) (, arg) (,@ body)) )))

(skk-defsubst-cond skk-jisx0208-p (char)
  ((boundp 'lc-jp)
   (eq lc-jp (car (find-charset-string (char-to-string char)))) )
  (t
   (eq 'japanese-jisx0208 (car (find-charset-string (char-to-string char)))) ))

(setq skk-kana-cleanup-command-list
      (cons 
       (if skk-viper-use-vip-prefix
	   'vip-del-backward-char-in-insert
	 'viper-del-backward-char-in-insert )
       skk-kana-cleanup-command-list ))

(setq skk-use-viper t)
(save-match-data
  (or (string-match sentence-end "。？！")
      (setq sentence-end (concat "[。？！]\\|" sentence-end)) ))

;; cursor color support.
(if (and (boundp 'viper-insert-state-cursor-color)
	 viper-insert-state-cursor-color
	 (fboundp 'viper-color-defined-p)
	 (viper-color-defined-p viper-insert-state-cursor-color)
	 ;;(member (x-color-values viper-insert-state-cursor-color)
         ;;        (list (x-color-values skk-hiragana-cursor-color)
         ;;              (x-color-values skk-katakana-cursor-color)
         ;;              (x-color-values skk-jisx0208-latin-cursor-color)
         ;;              (x-color-values skk-latin-cursor-color) )))
	 )
    (setq skk-use-color-cursor nil) )
;; とりあえず、外そうね、結構重いもんね。
;;(add-hook 'viper-post-command-hooks
;; 	    (function (lambda () (and skk-mode (skk-set-cursor-properly)))) ))

;; advices.
(or skk-viper-use-vip-prefix
    ;; vip-hide-replace-overlay はインライン関数
    (defadvice viper-hide-replace-overlay (after skk-ad activate)
      "SKK のモードに従いカーソルの色を変える。"
      (and skk-mode (skk-set-cursor-properly)) ))

(skk-viper-advice-select
 viper-insert vip-insert
 (after skk-ad activate)
 ("SKK のモードに従いカーソルの色を変える。"
  (and skk-mode (skk-set-cursor-properly)) ))

(skk-viper-advice-select
 viper-forward-word-kernel vip-forward-word-kernel
 (around skk-ad activate)
 ("SKK モードがオンで、ポイントの直後の文字が JISX0208 だったら forward-word する。"
  (if (and skk-mode (skk-jisx0208-p (following-char)))
      (forward-word val)
    ad-do-it )))

(skk-viper-advice-select
 viper-backward-word-kernel vip-backward-word-kernel
 (around skk-ad activate)
 ("SKK モードがオンで、ポイントの直前の文字が JISX0208 だったら backward-word する。"
  (if (and skk-mode (skk-jisx0208-p (preceding-char)))
      (backward-word val)
    ad-do-it )))

;; please sync with advice to delete-backward-char
(skk-viper-advice-select
 viper-del-backward-char-in-insert vip-del-backward-char-in-insert
 (around skk-ad activate)
 ("▼モードで skk-delete-implies-kakutei が non-nil だったら直前の文字を消して確定する。
▼モードで skk-delete-implies-kakutei が nil だったら前候補を表示する。
▽モードだったら確定する。
確定入力モードで、かなプレフィックスの入力中ならば、かなプレフィックスを消す。"
  (let ((count (or (prefix-numeric-value (ad-get-arg 0)) 1)))
    (cond (skk-henkan-active
	   (if (and (not skk-delete-implies-kakutei)
		    (= skk-henkan-end-point (point)) )
	       (skk-previous-candidate)
	     ;;(if skk-use-face (skk-henkan-face-off))
 	     ;; overwrite-mode で、ポイントが全角文字に囲まれていると
	     ;; きに delete-backward-char を使うと、全角文字は消すが半
	     ;; 角文字分しか backward 方向にポイントが戻らない (Emacs
	     ;; 19.31 にて確認)。変換中の候補に対しては
	     ;; delete-backward-char で必ず全角文字 1 文字分 backward
	     ;; 方向に戻った方が良い。
	     (if overwrite-mode
		 (progn
		   (backward-char count)
		   (delete-char count) )
	       ad-do-it )
	     ;; XXX assume skk-prefix has no multibyte chars.
	     (if (> (length skk-prefix) count)
		 (setq skk-prefix (substring skk-prefix 0 (- (length skk-prefix) count)))
	       (setq skk-prefix "") )
	     (if (>= skk-henkan-end-point (point)) (skk-kakutei)) ))
	  ((and skk-henkan-on (>= skk-henkan-start-point (point)))
	   (setq skk-henkan-count 0)
	   (skk-kakutei) )
	  ;; 入力中の見出し語に対しては delete-backward-char で必ず全角文字 1
	  ;; 文字分 backward 方向に戻った方が良い。
	  ((and skk-henkan-on overwrite-mode)
	   (backward-char count)
	   (delete-char count) )
	  (t
	   (if (string= skk-prefix "")
	       ad-do-it
	     (skk-erase-prefix 'clean) ))))))

(skk-viper-advice-select
 viper-intercept-ESC-key vip-intercept-ESC-key
 (around skk-add activate)
 ("▽モード、▼モードだったら確定する。確定後、SKK のモードに従いカーソルの色を変える。"
  (and skk-mode skk-henkan-on (skk-kakutei))
  ad-do-it
  (and skk-mode (skk-set-cursor-properly)) ))

(skk-viper-advice-select
 viper-join-lines vip-join-lines
 (after skk-ad activate)
 ("スペースの両側の文字セットが JISX0208 だったらスペースを取り除く。" ;
  (save-match-data
    (and (skk-jisx0208-p
	  (char-after (progn (skip-chars-forward " ") (point))) )
	 (skk-jisx0208-p
	  (char-before (progn (skip-chars-backward " ") (point))) )
	 (while (looking-at " ")
	   (delete-char 1) )))))

;;;###autoload
(defun skk-viper-normalize-map ()
  (let ((other-buffer
	 (if (eq skk-emacs-type 'xemacs)
	     (local-variable-p 'minor-mode-map-alist nil t)
	   (local-variable-p 'minor-mode-map-alist) )))
    ;; for current buffer and buffers to be created in the future.
    ;; substantially the same job as viper-harness-minor-mode does.
    (funcall skk-viper-normalize-map-function)
    (setq-default minor-mode-map-alist minor-mode-map-alist)
    (if (not other-buffer)
	nil
      ;; for buffers which are already created and have the minor-mode-map-alist
      ;; localized by Viper.
      (save-current-buffer
	(let ((buf (buffer-list)))
	  (while buf
	    (set-buffer (car buf))
	    (if (null (assq 'skk-j-mode minor-mode-map-alist))
		(progn
		  (set-modified-alist
		   'minor-mode-map-alist
		   (list (cons 'skk-latin-mode skk-latin-mode-map)
			 (cons 'skk-abbrev-mode skk-abbrev-mode-map)
			 (cons 'skk-j-mode skk-j-mode-map)
			 (cons 'skk-jisx0208-latin-mode skk-jisx0208-latin-mode-map) ))
		  (funcall skk-viper-normalize-map-function) ))
	    (setq buf (cdr buf)) ))))))

(defun skk-set-cursor-properly ()
  ;; カレントバッファの SKK のモードに従い、カーソルの色を変更する。
  (if (and skk-use-color-cursor
	   ;;(x-color-defined-p viper-insert-state-cursor-color)
	   (get-buffer-window (current-buffer)) )
      (cond
       ((or (not skk-mode)
	    ;; vi-state のときは、SKK モードになっていてもカーソルをディ
	    ;; フォルトにしておく。
	    (or (and (boundp 'viper-current-state)
		     (eq viper-current-state 'vi-state) )
		(and (boundp 'vip-current-state)
		     (eq vip-current-state 'vi-state) )))
	(skk-set-cursor-color skk-default-cursor-color) )
       (t
	(skk-set-cursor-color (cond (skk-jisx0208-latin-mode
				     skk-jisx0208-latin-cursor-color )
				    (skk-katakana skk-katakana-cursor-color)
				    (skk-j-mode skk-hiragana-cursor-color)
				    (t skk-latin-cursor-color) )))))
  (if skk-use-cursor-change
      (skk-change-cursor-when-ovwrt) ))

(skk-viper-normalize-map)

(provide 'skk-viper)
;;; skk-viper.el ends here
