;;; skk-cursor.el --- SKK cursor control.
;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;; Masatake YAMATO <masata-y@is.aist-nara.ac.jp>

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-cursor.el,v 1.14 2001/09/15 19:20:04 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/09/15 19:20:04 $

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

;;; Code:
(or (skk-color-display-p)
    (error "%s" "SKK-CURSOR requires color display"))

(eval-when-compile
  (require 'static)
  (require 'skk-macs)
  (require 'skk-vars))

(static-unless
    (eq skk-emacs-type 'xemacs)
  (require 'ccc))

(defun skk-cursor-current-color ()
  ;; カレントバッファの SKK のモードから、カーソルの色を取得する。
  (cond
   ((not (and skk-use-color-cursor
	      skk-mode))
    skk-cursor-default-color)
   ;; skk-start-henkan の中では、skk-j-mode フラグを立てながら、
   ;; skk-abbrev-mode フラグも立てている (変換後、直後に入力する文
   ;; 字が元の入力モードにて行なわれるように)。従い、skk-abbrev-mode
   ;; フラグのチェックの優先度を上げる。
   (skk-abbrev-mode
    skk-cursor-abbrev-color)
   (skk-jisx0208-latin-mode
    skk-cursor-jisx0208-latin-color)
   (skk-katakana
    skk-cursor-katakana-color)
   (skk-j-mode
    skk-cursor-hiragana-color)
   (skk-jisx0201-mode
    skk-cursor-jisx0201-color)
   (t
    skk-cursor-latin-color)))

(defun skk-cursor-set (&optional color force)
  (when (or skk-use-color-cursor
	    force)
    (static-cond
     ((eq skk-emacs-type 'xemacs)
      (set-face-property 'text-cursor
			 'background
			 (or color
			     (skk-cursor-current-color))
			 (current-buffer)))
     (t
      (set-buffer-local-cursor-color
       (or color
	   (skk-cursor-current-color)))))))

(static-cond
 ;; XEmacs
 ((eq skk-emacs-type 'xemacs)
  ;; advices.
  (defadvice skk-kakutei (after skk-cursor-ad activate)
    ;;At 10 Jul 2000 16:37:49 +0900,
    ;;Yoshiki Hayashi <t90553@mail.ecc.u-tokyo.ac.jp> wrote:
    ;;> foreground を background に変える必要があること以外は、今の
    ;;> ところそのままで動いているようです。しばらく test してみます。
    ;;> どうも、text-cursor も普通の face のようで、foreground が文
    ;;> 字の色を、background が文字の背景の色を表しているようです。
    (skk-cursor-set))

  (defadvice skk-mode (after skk-cursor-ad activate)
    (skk-cursor-set (cond ((not (and skk-use-color-cursor
				     skk-mode))
			   skk-cursor-default-color)
			  (skk-katakana
			   skk-cursor-katakana-color)
			  (skk-j-mode
			   skk-cursor-hiragana-color))))

  (defadvice skk-auto-fill-mode (after skk-cursor-ad activate)
    (skk-cursor-set (cond ((not (and skk-use-color-cursor
				     skk-mode))
			   skk-cursor-default-color)
			  (skk-katakana
			   skk-cursor-katakana-color)
			  (skk-j-mode
			   skk-cursor-hiragana-color))))

  (defadvice skk-abbrev-mode (after skk-cursor-ad activate)
    (skk-cursor-set skk-cursor-abbrev-color))

  (defadvice skk-jisx0201-mode (after skk-cursor-ad activate)
    (skk-cursor-set skk-cursor-jisx0201-color))

  (defadvice skk-jisx0208-latin-mode (after skk-cursor-ad activate)
    (skk-cursor-set skk-cursor-jisx0208-latin-color))

  (defadvice skk-latin-mode (after skk-cursor-ad activate)
    (skk-cursor-set skk-cursor-latin-color))

  (defadvice skk-toggle-kana (after skk-cursor-ad activate)
    (skk-cursor-set (cond (skk-katakana
			   skk-cursor-katakana-color)
			  (skk-j-mode
			   skk-cursor-hiragana-color))))

  (skk-defadvice minibuffer-keyboard-quit (before skk-cursor-ad activate)
    (unless (or skk-henkan-on
		skk-henkan-active)
      (skk-cursor-set skk-cursor-default-color)))

  ;; Hooks
  (add-hook 'isearch-mode-end-hook
	    (lambda ()
	      (skk-cursor-set))
	    'append)

  (add-hook 'minibuffer-setup-hook
	    (lambda ()
	      (skk-cursor-set))
	    'append)

  (add-hook 'minibuffer-exit-hook
	    (lambda ()
	      (with-current-buffer (nth 1 (buffer-list))
		(skk-cursor-set))
	      (skk-cursor-set skk-cursor-default-color 'force))
	    'append))
 ;; FSF Emacs
 (t
  ;; advices.
  (defvar skk-cursor-buffer-local-frame-params-ad-targets
    '(;; cover to SKK functions.
      skk-abbrev-mode
      skk-auto-fill-mode
      skk-jisx0201-mode
      skk-jisx0208-latin-mode
      skk-kakutei
      skk-latin-mode
      skk-mode
      skk-toggle-kana))

  (dolist (func skk-cursor-buffer-local-frame-params-ad-targets)
    (if (and (commandp func)
	     (subr-fboundp func))
	(message "%s"
		 "\
WARNING: Adding advice to a subr command, %s\
 without mirroring its interactive spec"
		 func))
    (eval
     (`
      (defadvice (, (intern (symbol-name func))) (after skk-cursor-ad
							activate)
	"Set cursor color which represents skk mode."
	(skk-cursor-set)))))))

(defun skk-cursor-init-function ()
  (skk-cursor-set)
  (remove-hook 'skk-mode-hook
	       'skk-cursor-init-function))

;;; Hooks
;;(add-hook 'isearch-mode-end-hook 'update-buffer-local-frame-params 'append)
(add-hook 'skk-mode-hook 'skk-cursor-init-function)

(require 'product)
(product-provide
    (provide 'skk-cursor)
  (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-cursor.el ends here
