;;; skk-cursor.el --- SKK cursor control.
;; Copyright (C) 1996, 1997, 1998, 1999
;; Masatake YAMATO <jet@airlab.cs.ritsumei.ac.jp>

;; Author: Masatake YAMATO <jet@airlab.cs.ritsumei.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-cursor.el,v 1.3 1999/12/13 23:45:14 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/12/13 23:45:14 $

;; This file is part of SKK.

;; SKK is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; [Todo] Use `skk-cursor-' prefix for all variables and functions.
;;   skk-default-cursor-color -> skk-cursor-default-color
;;   skk-jisx0208-latin-cursor-color -> skk-cursor-jisx0208-latin-color
;;   skk-katakana-cursor-color -> skk-cursor-katakana-color
;;   skk-hiragana-cursor-color -> skk-cursor-hiragana-color
;;   skk-latin-cursor-color -> skk-cursor-latin-color

;;; Code:
(eval-when-compile (require 'static) (require 'skk-foreword))

;; functions.
(defun skk-cursor-set-color (color)
  ;; カーソルの色を COLOR に変更する。
  (and skk-use-color-cursor
       (condition-case nil
	   (set-cursor-color color)
	 (error
	  (set-cursor-color skk-default-cursor-color)
	  (and skk-cursor-report-set-error
	       (skk-message
		"カラーマップ切れです。ディフォルトのカラーを使います。"
		"Color map is exhausting, use default cursor color" ))))))

(defun skk-cursor-change-when-ovwrt ()
  (static-cond
   ((eq skk-emacs-type 'xemacs) (setq bar-cursor overwrite-mode))
   (t (if overwrite-mode
	  (modify-frame-parameters (selected-frame) '((cursor-type bar . 3)))
	(modify-frame-parameters (selected-frame) '((cursor-type . box))) ))))

;; Overwite by skk-viper.el
(defun skk-cursor-set-properly ()
  ;; カレントバッファの SKK のモードに従い、カーソルの色を変更する。
  (if (and skk-use-color-cursor (get-buffer-window (current-buffer)))
      (if (not skk-mode)
	  (skk-cursor-set-color skk-default-cursor-color)
	(skk-cursor-set-color (cond (skk-jisx0208-latin-mode
				     skk-jisx0208-latin-cursor-color )
				    (skk-katakana skk-katakana-cursor-color)
				    (skk-j-mode skk-hiragana-cursor-color)
				    (t skk-latin-cursor-color) ))))
  (and skk-use-cursor-change (skk-cursor-change-when-ovwrt)) )

;;; advices.
;; cover to original Emacs functions.
(defadvice overwrite-mode (after skk-cursor-ad activate)
  "skk-use-cursor-change が non-nil だったら、カーソルの幅を縮める。"
  (and skk-use-cursor-change (skk-cursor-change-when-ovwrt)) )

(defadvice abort-recursive-edit (before skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly)) )

(defadvice exit-minibuffer (before skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly)) )

(defadvice kill-buffer (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  ;; 別のバッファへ飛ぶコマンドは skk-mode が nil でもカーソル色を調整する必要
  ;; がある。
  (skk-cursor-set-properly) )

(defadvice goto-line (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice yank (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice yank-pop (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice recenter (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice insert-file (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

;; 別のバッファへ飛ぶコマンドは skk-mode が nil でもカーソル色を調整する必要が
;; ある。
(defadvice bury-buffer (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice switch-to-buffer (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) ) 

(defadvice hilit-yank (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice hilit-yank-pop (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice hilit-recenter (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice execute-extended-command (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice pop-to-buffer (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice other-window (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(if (eq skk-emacs-type 'xemacs)
    (defadvice minibuffer-keyboard-quit (before skk-cursor-ad activate)
      "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
      (with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly)) ))

;; cover to VIP/Viper functions.
(defadvice viper-intercept-ESC-key (after skk-cursor-ad activate)
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice vip-intercept-ESC-key (after skk-cursor-ad activate)
  (and skk-mode (skk-cursor-set-properly)) )

;; cover to SKK functions.
(defadvice skk-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-latin-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-jisx0208-latin-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-abbrev-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-auto-fill-mode (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-color (if skk-mode
			    skk-hiragana-cursor-color
			  skk-default-cursor-color )))

(defadvice skk-toggle-kana (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (if skk-katakana
      (skk-cursor-set-color skk-katakana-cursor-color)
    (skk-cursor-set-color skk-hiragana-cursor-color) ))

(defadvice skk-kakutei (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (if (interactive-p)
      (skk-cursor-set-color (if skk-katakana skk-katakana-cursor-color
			      skk-hiragana-cursor-color ))))

(defadvice skk-save-jisyo (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-katakana-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-hiragana-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-jisx0208-latin-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-latin-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-jisx0201-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-gyakubiki-message (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-gyakubiki-katakana-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-gyakubiki-katakana-message (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-hurigana-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-hurigana-message (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-hurigana-katakana-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-romaji-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-romaji-message (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(add-hook 'after-make-frame-hook 'skk-cursor-set-properly)
(add-hook 'minibuffer-setup-hook 'skk-cursor-set-properly)
(add-hook 'minibuffer-exit-hook 'skk-cursor-set-properly 'append)

(defalias 'skk-set-cursor-color 'skk-cursor-set-color)
(defalias 'skk-change-cursor-when-ovwrt 'skk-cursor-change-when-ovwrt)
(defalias 'skk-set-cursor-properly 'skk-cursor-set-properly)

(provide 'skk-cursor)
;;; Local Variables:
;;; End:
;;; skk-cursor.el ends here
