;;; skk-dcomp.el --- SKK dynamic completion
;; Copyright (C) 1999, 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-dcomp.el,v 1.13 2000/12/03 23:35:15 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/12/03 23:35:15 $

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

;;; Commentary
;;
;; これは見出し語の入力を、自動的にダイナミックにコンプリーションする
;; プログラムです。増井俊之 さんが開発している POBox や MS Excel のセ
;; ルでの文字入力に影響を受けています (POBox とはインタフェイス自身は
;; 少し異なりますが、動作はこちらの方がかなり高速なはずです)。
;;
;; <INSTALL>
;; skk-11/experimental/skk-dcomp.el を skk-11/skk-dcomp.el にコピーし
;; て後は普通に make して下さい。skk-dcomp.el がインストールされ、
;; autoload の設定が自動的に生成されます。
;;
;; <HOW TO USE>
;; .emacs もしくは .skk に (require 'skk-dcomp) と書きましょう。それだ
;; けです。
;;
;; <HOW TO WORK>
;; SKK を普通に使い始めてみて下さい。きっと最初は驚くはずですが、イン
;; タフェイスは言葉で説明するよりも、体感した方が早いと思います ;-)。
;; 
;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-comp)

(defgroup skk-dcomp nil "SKK dynamic completion related customization."
  :prefix "skk-dcomp-"
  :group 'skk)

(defface skk-dcomp-face
  '((((class color)) (:foreground "DarkKhaki"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t)))
  "*Face used to highlight region dynamically completed."
  :group 'skk-dcomp
  :group 'skk-faces)

(defcustom skk-dcomp-face-priority 700
  "*Overlay/extent priority of `skk-dcomp-face'."
  :type 'integer
  :group 'skk-dcomp)

(defcustom skk-dcomp-keep-completion-keys nil
  ;;   (delq
  ;;    nil
  ;;    (list
  ;;     (car (rassoc (list nil 'skk-toggle-kana) skk-rom-kana-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-characters) skk-rom-kana-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-kana) skk-rom-kana-base-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-characters) skk-rom-kana-base-rule-list))))
  "*自動補完された見出し語を消さないキーのリスト。
通常は見出し語の補完後、次のキー入力をすると、自動補完されたキー入力が消えて
しまうが、このリストに指定されたキー入力があったときは自動補完された見出し語
を消さない。"
  :type '(choice (repeat string) (const nil))
  :group 'skk-dcomp
  :group 'skk-filenames)

(skk-deflocalvar skk-dcomp-start-point nil)
(skk-deflocalvar skk-dcomp-end-point nil)
(skk-deflocalvar skk-dcomp-extent nil)
(defvar skk-dcomp-face 'skk-dcomp-face)

;; functions.
(defsubst skk-extentp (object)
  (static-cond
   ((eq skk-emacs-type 'xemacs) (extentp object))
   (t (overlayp object))))

(defun skk-dcomp-face-on (start end)
  (skk-face-on skk-dcomp-extent start end skk-dcomp-face
	       skk-dcomp-face-priority))

(defun skk-dcomp-face-off ()
  (skk-detach-extent skk-dcomp-extent))

;; main dynamic completion engine.
(defadvice skk-kana-input (around skk-dcomp-ad activate)
  (if (not skk-henkan-on)
      ad-do-it
    (if (or skk-henkan-active (skk-get-prefix skk-current-rule-tree)
	    (not skk-comp-stack))
	(progn
	  (skk-set-marker skk-dcomp-start-point nil)
	  (skk-set-marker skk-dcomp-end-point nil))
      (when (and (marker-position skk-dcomp-start-point)
		 (marker-position skk-dcomp-end-point))
	(skk-dcomp-face-off)
	(or (member (this-command-keys) skk-dcomp-keep-completion-keys)
	    (condition-case nil
		(delete-region skk-dcomp-start-point skk-dcomp-end-point)
	      (error)))))
    ad-do-it
    (if (and (not (skk-get-prefix skk-current-rule-tree))
	     (not skk-okurigana))
	(let ((pos (point)))
	  (condition-case nil
	      (progn
		(skk-comp-do 'first 'silent)
		(skk-set-marker skk-dcomp-start-point pos)
		(skk-set-marker skk-dcomp-end-point (point))
		(skk-dcomp-face-on skk-dcomp-start-point skk-dcomp-end-point)
		(goto-char skk-dcomp-start-point))
	    (error
	     (setq skk-comp-stack nil)
	     (message nil)))))))

(defadvice skk-kakutei (around skk-dcomp-ad activate)
  (if (and skk-henkan-on (not skk-henkan-active)
	   (markerp skk-dcomp-start-point)
	   (markerp skk-dcomp-end-point)
	   (marker-position skk-dcomp-start-point)
	   (marker-position skk-dcomp-end-point))
      (progn
	(skk-dcomp-face-off)
	(condition-case nil
	    (delete-region skk-dcomp-start-point skk-dcomp-end-point)
	  (error))))
  ad-do-it
  (skk-set-marker skk-dcomp-start-point nil)
  (skk-set-marker skk-dcomp-end-point nil)
  (setq skk-comp-stack nil))

(defadvice skk-start-henkan (before skk-dcomp-ad activate)
  (if (and (markerp skk-dcomp-start-point)
	   (markerp skk-dcomp-end-point)
	   (marker-position skk-dcomp-start-point)
	   (marker-position skk-dcomp-end-point))
      (progn
	(skk-dcomp-face-off)
	(delete-region skk-dcomp-end-point (point))
	(skk-set-marker skk-dcomp-end-point (point)))))

(skk-defadvice keyboard-quit (around skk-dcomp-ad activate)
  (if (and skk-henkan-on (not skk-henkan-active)
	   (marker-position skk-dcomp-start-point)
	   (marker-position skk-dcomp-end-point))
      (progn
	(skk-dcomp-face-off)
	(condition-case nil
	    (delete-region skk-dcomp-start-point skk-dcomp-end-point)
	  (error))))
  ad-do-it
  (skk-set-marker skk-dcomp-start-point nil)
  (skk-set-marker skk-dcomp-end-point nil)
  (setq skk-comp-stack nil))

(defadvice skk-comp (around skk-dcomp-ad activate)
  (if (and (marker-position skk-dcomp-start-point)
	   (marker-position skk-dcomp-end-point))
      (progn
	(goto-char skk-dcomp-end-point)
	(setq this-command 'skk-comp-do)
	(skk-dcomp-face-off)
	(skk-set-marker skk-dcomp-start-point nil)
	(skk-set-marker skk-dcomp-end-point nil))
    ad-do-it))

(require 'product)
(product-provide (provide 'skk-dcomp) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-dcomp.el ends here
