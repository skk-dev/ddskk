;;; skk-cursor.el --- SKK cursor control.
;; Copyright (C) 1996, 1997, 1998, 1999
;; Masatake YAMATO <jet@airlab.cs.ritsumei.ac.jp>

;; Author: Masatake YAMATO <jet@airlab.cs.ritsumei.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-cursor.el,v 1.4 2000/03/11 02:14:53 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/03/11 02:14:53 $

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

;; User variables.
;; 変数名の prefix を skk-cursor- に統一すればこのグループが生きてくるんだが...
;; 10.x はこれ以上変数名を変更しない方が良いと思うので、このままにしておく。
;;(defgroup skk-cursor nil "SKK cursor related customization."
;;  :prefix "skk-"
;;  :group 'skk)

(defcustom skk-default-cursor-color
  (if (eq skk-emacs-type 'xemacs)
      (frame-property (selected-frame) 'cursor-color)
    (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))
  "*SKK のオフを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :group 'skk)

(defcustom skk-hiragana-cursor-color (if (eq skk-background-mode 'light)
					 "coral4"
				       "pink" )
  "*かなモードを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :type 'string
  :group 'skk)

(defcustom skk-katakana-cursor-color (if (eq skk-background-mode 'light)
					 "forestgreen"
				       "green" )
  "*カタカナモードを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :type 'string
  :group 'skk)

(defcustom skk-jisx0208-latin-cursor-color "gold"
  "*全角英字モードを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :type 'string
  :group 'skk)

(defcustom skk-latin-cursor-color (if (eq skk-background-mode 'light)
				      "ivory4"
				    "gray" )
  "*アスキーモードを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :type 'string
  :group 'skk)

(defcustom skk-abbrev-cursor-color "royalblue"
  "*abbrev モードを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :type 'string
  :group 'skk)

(defcustom skk-report-set-cursor-error t
  "*Non-nil であれば、カラーマップ切れが起きた場合、エラーメッセージを表示する。
nil であれば、表示しない。" 
  :type 'boolean
  :group 'skk)

;; functions.
(defun skk-cursor-set-color (color)
  ;; カーソルの色を COLOR に変更する。
  (condition-case nil
      (set-cursor-color color)
    (error
     (set-cursor-color skk-default-cursor-color)
     (and skk-cursor-report-set-error
	  (skk-message
	   "カラーマップ切れです。ディフォルトのカラーを使います。"
	   "Color map is exhausting, use default cursor color" )))))

(defun skk-cursor-change-when-ovwrt ()
  (static-cond
   ((eq skk-emacs-type 'xemacs) (setq bar-cursor overwrite-mode))
   (t (if overwrite-mode
	  (modify-frame-parameters (selected-frame) '((cursor-type bar . 3)))
	(modify-frame-parameters (selected-frame) '((cursor-type . box)))))))

(defun skk-cursor-set-properly (&optional color)
  ;; カレントバッファの SKK のモードに従い、カーソルの色を変更する。
  ;; オプショナル引数の COLOR が指定されたときは、そのカーソル色を使う。
  ;; OVWRT モードのときはカーソルの幅を小さくする。
   (if (not (get-buffer-window (current-buffer)))
      nil
    (if skk-use-color-cursor 
	(skk-cursor-set-color 
	 (cond (color)
	       ((not skk-mode) skk-default-cursor-color)
	       (skk-jisx0208-latin-mode
		skk-jisx0208-latin-cursor-color)
	       (skk-katakana skk-katakana-cursor-color)
	       (skk-j-mode skk-hiragana-cursor-color)
	       (t skk-latin-cursor-color))))
    (and skk-use-cursor-change (skk-cursor-change-when-ovwrt))))

;;; advices.
(let ((funcs '(
	       ;; cover to original Emacs functions.
	       bury-buffer
	       delete-frame
	       delete-window
	       execute-extended-command 
	       kill-buffer
	       other-window
	       overwrite-mode
	       pop-to-buffer
	       select-frame
	       select-window
	       switch-to-buffer
	       ;; cover to SKK functions.
	       skk-auto-fill-mode 
	       skk-gyakubiki-katakana-message 
	       skk-gyakubiki-katakana-region 
	       skk-gyakubiki-message 
	       skk-hiragana-region 
	       skk-hurigana-katakana-region 
	       skk-hurigana-message 
	       skk-hurigana-region 
	       skk-jisx0201-region 
	       skk-jisx0208-latin-region 
	       skk-katakana-region 
	       skk-latin-region 
	       skk-mode 
	       skk-romaji-message 
	       skk-romaji-region 
	       skk-save-jisyo 
	       skk-toggle-kana)))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(after skk-cursor-ad activate)
	"入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
	;; 別のバッファへ飛ぶコマンドは skk-mode が nil でもカーソル色を調整する必要が
	;; ある。
	;; CLASS は after.
	(skk-cursor-set-properly))))
    (setq funcs (cdr funcs))))

(let ((funcs '(
	       goto-line 
	       insert-file 
	       recenter 
	       yank
	       yank-pop 
	       ;; cover to hilit functions.
	       hilit-recenter 
	       hilit-yank 
	       hilit-yank-pop 
	       ;; cover to VIP/Viper functions.
	       vip-Append
	       vip-Insert
	       vip-insert
	       vip-intercept-ESC-key 
	       vip-open-line
	       viper-Append
	       viper-Insert
	       viper-hide-replace-overlay 
	       viper-insert
	       viper-intercept-ESC-key
	       viper-open-line
	       )))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(after skk-cursor-ad activate)
	"入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
	;; skk-mode が nil か non-nil かの判定付き。
	;; CLASS は after.
	(and skk-mode (skk-cursor-set-properly)))))
    (setq funcs (cdr funcs))))

(let ((funcs '(abort-recursive-edit exit-minibuffer)))
  (if (eq skk-emacs-type 'xemacs)
      (setq funcs (cons 'minibuffer-keyboard-quit funcs)))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(before skk-cursor-ad activate)
	"入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
	;; ミニバッファから元のカレントバッファを探し出し、カーソルをセット。
	;; CLASS は before.
	(with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly)))))
    (setq funcs (cdr funcs))))

(defadvice skk-latin-mode (after skk-cursor-ad activate)
  "カーソル色を skk-latin-cursor-color に変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly skk-latin-cursor-color))

(defadvice skk-jisx0208-latin-mode (after skk-cursor-ad activate)
  "カーソル色を skk-jisx0208-latin-cursor-color に変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly skk-jisx0208-latin-cursor-color))
 
(defadvice skk-abbrev-mode (after skk-cursor-ad activate)
  "応じカーソル色を skk-abbrev-cursor-color に変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly skk-abbrev-cursor-color))

(defadvice skk-kakutei (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (if (interactive-p) (skk-cursor-set-properly)))

;; VIP/Viper related
(defadvice skk-mode (after skk-viper-ad activate)
  (add-hook 'viper-post-command-hooks
	    (function (lambda () (and skk-mode (skk-set-cursor-properly))))
	    'append 'local))

;;; Hooks.
(add-hook 'after-make-frame-hook 'skk-cursor-set-properly)
(add-hook 'minibuffer-setup-hook 'skk-cursor-set-properly)
(add-hook 'minibuffer-exit-hook 'skk-cursor-set-properly 'append)

(defalias 'skk-set-cursor-color 'skk-cursor-set-color)
(defalias 'skk-change-cursor-when-ovwrt 'skk-cursor-change-when-ovwrt)
(defalias 'skk-set-cursor-properly 'skk-cursor-set-properly)

;; 最初に load されたときは、skk-cursor adviced function になる前の関数によって
;; 呼ばれており、advice が効いてないので、トップレベルでカーソルを合わせておく。
(and (get-buffer-window (current-buffer))
     ;; only first time when this file loaded.
     ;;(not skk-mode-invoked)
     (skk-cursor-set-properly skk-hiragana-cursor-color))

;;; Hooks
(add-hook 'isearch-mode-end-hook 'skk-cursor-set-properly 'append)

(provide 'skk-cursor)
;;; Local Variables:
;;; End:
;;; skk-cursor.el ends here
