;;; skk-dcomp.el --- SKK dynamic completion -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999, 2000, 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-dcomp.el,v 1.43 2007/04/29 01:38:17 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2007/04/29 01:38:17 $

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

;;; Commentary

;; これは▽モードにおける見出し語の入力を、自動的にダイナミックにコンプ
;; リーションするプログラムです。
;;
;; MS Excel のセル入力の自動補完 (同じ列に既に入力している文字列があっ
;; たときにそれを参照して補完しようとする機能) を見ていて、これ便利だなぁ
;; と思ったのが、開発のきっかけです。
;;
;; その後、増井俊之 さんが開発している POBox を見て、MS Excel を見た際に
;; 思ったことを思い出し、SKK の skk-comp.el で提供されているコンプリーシ
;; ョンの機能を自動的に提供する方向で実装してみたのが skk-dcomp.el のコー
;; ディング始まりです。
;;
;; POBox は沢山候補を出しますが、少し動作が遅いのが難点です。skk-dcomp.el
;; は一つしか候補を出しませんが、ユーザの見出し語の入力に追従しダイナミッ
;; クにコンプリーションする機能は POBox 同様持っていますし、また動作はかな
;; り高速で、skk-dcomp.el を使うことによるオーバーヘッドを体感することはな
;; いと思います。
;;
;;
;; <INSTALL>
;;
;; SKK を普通に make して下さい。特に作業は不要です。
;;
;; <HOW TO USE>
;;
;; .emacs もしくは .skk に (setq skk-dcomp-activate t) と書きましょう。
;; SKK 起動後にダイナミックコンプリーションの機能を止めたかったら、
;; (setq skk-dcomp-activate nil) を評価しましょう。
;;
;;
;; <HOW TO WORK>
;;
;; ▽モードに入り見出し語を入力すると、個人辞書を自動的に検索し、見出
;; し語を コンプリーションします。下記のように動作します (カッコ内はキー
;; 入力を、-!- はポイント位置を表します)。
;;
;;   (Ho) ▽ほ -> ▽ほ-!-んとう
;;
;;   * SKK のコンプリーションは、元来個人辞書のみを参照して行なわれる
;;     仕様になっていますので、個人辞書にない見出し語のコンプリーション
;;     は行なわれません。
;;   * コンプリーションは、送りなし変換の場合しか行なわれません。
;;   * Ho の入力に対し、「ほんとう」がコンプリーションされるかどうかは個
;;     人辞書のエントリの順番次第 (変換順に降順に並んでいる) ですので、人
;;     それぞれ違うはずです。
;;
;; 自動的にコンプリーションされた見出し語が、自分の意図したものであれば TAB
;; を押すことでポイント位置を動かし、コンプリーションされた見出し語を選択す
;; ることができます。そのまま SPC を押して変換するなり、q を押してカタカナ
;; にするなり SKK 本来の動作を何でも行なうことができます。
;;
;;   (Ho) ▽ほ -> ▽ほ-!-んとう (TAB) -> ▽ほんとう-!- (TAB)
;;
;; コンプリーションされた見出し語が自分の意図したものでない場合は、かま
;; わず次の入力をして下さい。コンプリーションされた部分を無視したかのように
;; 動作します。
;;
;;   (Ho) ▽ほ -> ▽ほ-!-んとう (ka) -> ▽ほか-!-ん
;;
;; コンプリーションされない状態が自分の意図したものである場合も、コンプリー
;; ションされた部分を単に無視するだけで OK です。
;;
;;   (Ho) ▽ほ -> ▽ほ-!-んとう (C-j) -> ほ
;;   (Ho) ▽ほ -> ▽ほ-!-んとう (SPC) -> ▼保 (「ほ」を見出し語とした変換が
;;                                       行なわれる)
;;   (Ho) ▽ほ -> ▽ほ-!-んとう (q) -> ホ
;;
;; コンプリーションされた状態から BS を押すと、消されたコンプリーション前の
;; 見出し語から再度コンプリーションを行ないます。
;;
;;   (Ho) ▽ほ -> ▽ほ-!-んとう (ka) -> ▽ほか-!-ん (BS) -> ▽ほ-!-んとう

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))
(require 'skk-comp)

;;; functions.
;; (defsubst skk-extentp (object)
;;   (static-cond
;;    ((eq skk-emacs-type 'xemacs) (extentp object))
;;    (t (overlayp object))))

(defsubst skk-dcomp-face-on (start end)
  (skk-face-on skk-dcomp-extent start end skk-dcomp-face
	       skk-dcomp-face-priority))

(defsubst skk-dcomp-face-off ()
  (skk-detach-extent skk-dcomp-extent))

(defsubst skk-dcomp-delete-completion ()
  (ignore-errors
    (delete-region skk-dcomp-start-point skk-dcomp-end-point)))

;;;###autoload
(defun skk-dcomp-marked-p ()
  (and (eq skk-henkan-mode 'on)
       (markerp skk-dcomp-start-point)
       (markerp skk-dcomp-end-point)
       (marker-position skk-dcomp-start-point)
       (marker-position skk-dcomp-end-point)
       (< skk-dcomp-start-point skk-dcomp-end-point)))

(defun skk-dcomp-cleanup-buffer ()
  (when (and skk-dcomp-activate
	     (skk-dcomp-marked-p))
    (skk-dcomp-face-off)
    (delete-region skk-dcomp-end-point (point))
    (skk-set-marker skk-dcomp-end-point (point))))

(defun skk-dcomp-activate-p ()
  (and skk-dcomp-activate
       (cond ((functionp skk-dcomp-activate)
	      (save-match-data
		(funcall skk-dcomp-activate)))
	     ((listp skk-dcomp-activate)
	      (save-match-data
		(eval skk-dcomp-activate)))
	     (skk-hint-inhibit-dcomp
	      nil)
	     (t
	      t))))

(defun skk-dcomp-do-completion (pos)
  (when (and (eq skk-henkan-mode 'on)
	     (not skk-okurigana)
	     (not (eq (marker-position skk-henkan-start-point) (point)))
	     (skk-dcomp-activate-p))
    (condition-case nil
	(progn
	  (skk-comp-do 'first 'silent)
	  (skk-set-marker skk-dcomp-start-point pos)
	  (skk-set-marker skk-dcomp-end-point (point))
	  (skk-dcomp-face-on skk-dcomp-start-point skk-dcomp-end-point)
	  (goto-char skk-dcomp-start-point))
      (error
       (setq skk-comp-stack nil)
       (message nil)))))

;;;###autoload
(defun skk-dcomp-before-kakutei ()
  (when (and skk-dcomp-activate
	     (eq skk-henkan-mode 'on)
	     (skk-dcomp-marked-p))
    (skk-dcomp-face-off)
    (skk-dcomp-delete-completion)))

(defun skk-dcomp-after-kakutei ()
  (when skk-dcomp-activate
    (skk-set-marker skk-dcomp-start-point nil)
    (skk-set-marker skk-dcomp-end-point nil)
    (setq skk-comp-stack nil)))

;;;###autoload
(defun skk-dcomp-after-delete-backward-char ()
  (when (and skk-dcomp-activate
	     skk-mode
	     (eq skk-henkan-mode 'on)
	     (not skk-hint-inhibit-dcomp))
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    (when (and skk-abbrev-mode
	       skk-use-look)
      (setq skk-look-completion-words nil))
    (skk-dcomp-do-completion (point)))
  ;; dcomp との順番制御のため、ここで呼ぶ
  (skk-henkan-on-message))

;;; advices.
;; main dynamic completion engine.
(defadvice skk-kana-input (around skk-dcomp-ad activate)
  (cond
   ((or skk-hint-inhibit-dcomp
	(not (and skk-dcomp-activate
		  skk-henkan-mode)))
    ad-do-it)
   (t
    (cond
     ((or (eq skk-henkan-mode 'active)
	  (skk-get-prefix skk-current-rule-tree)
	  (not skk-comp-stack))
      (skk-set-marker skk-dcomp-start-point nil)
      (skk-set-marker skk-dcomp-end-point nil))
     ((skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (unless (member (this-command-keys)
		      skk-dcomp-keep-completion-keys)
	(skk-dcomp-delete-completion))))
    ad-do-it
    (when (and skk-j-mode
	       (or skk-use-kana-keyboard
		   ;; 送りあり変換が始まったら補完しない
		   (not (memq last-command-char skk-set-henkan-point-key)))
	       (not (skk-get-prefix skk-current-rule-tree)))
      (skk-dcomp-do-completion (point))))))

(defadvice skk-set-henkan-point-subr (around skk-dcomp-ad activate)
  (cond
   (skk-dcomp-activate
    (let ((henkan-mode skk-henkan-mode))
      ad-do-it
      (unless (or henkan-mode
		  (char-after (point)))
	(skk-dcomp-do-completion (point)))))
   (t
    ad-do-it)))

(defadvice skk-abbrev-insert (around skk-dcomp-ad activate)
  (cond
   (skk-dcomp-activate
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    ad-do-it
    (when skk-use-look
      (setq skk-look-completion-words nil))
    (unless (memq last-command-char '(?*))
      (skk-dcomp-do-completion (point))))
   (t
    ad-do-it)))

(defadvice skk-abbrev-comma (around skk-dcomp-ad activate)
  (cond
   ((and skk-dcomp-activate
	 (not (eq last-command 'skk-comp-do)))
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    ad-do-it
    (when skk-use-look
      (setq skk-look-completion-words nil))
    (unless (memq last-command-char '(?*))
      (skk-dcomp-do-completion (point))))
   (t
    ad-do-it)))

(defadvice skk-abbrev-period (around skk-dcomp-ad activate)
  (cond
   ((and skk-dcomp-activate
	 (not (eq last-command 'skk-comp-do)))
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    ad-do-it
    (when skk-use-look
      (setq skk-look-completion-words nil))
    (unless (memq last-command-char '(?*))
      (skk-dcomp-do-completion (point))))
   (t
    ad-do-it)))

(defadvice skk-kakutei (around skk-dcomp-ad activate)
  (skk-dcomp-before-kakutei)
  ad-do-it
  (skk-dcomp-after-kakutei))

(defadvice keyboard-quit (around skk-dcomp-ad activate)
  (skk-dcomp-before-kakutei)
  ad-do-it
  (skk-dcomp-after-kakutei))

;;(defadvice skk-henkan (before skk-dcomp-ad activate)
(defadvice skk-start-henkan (before skk-dcomp-ad activate)
  (skk-dcomp-cleanup-buffer))

(defadvice skk-process-prefix-or-suffix (before skk-dcomp-ad activate)
  (when skk-henkan-mode
    (skk-dcomp-cleanup-buffer)))

(defadvice skk-comp (around skk-dcomp-ad activate)
  (cond ((and skk-dcomp-activate
	      (skk-dcomp-marked-p))
	 (cond ((integerp (ad-get-arg 0))
		(skk-dcomp-cleanup-buffer)
		ad-do-it)
	       (t
		(goto-char skk-dcomp-end-point)
		(setq this-command 'skk-comp-do)
		(skk-dcomp-face-off)
		(skk-set-marker skk-dcomp-start-point nil)
		(skk-set-marker skk-dcomp-end-point nil))))
	(t
	 ad-do-it)))

(defadvice skk-comp-start-henkan (around skk-dcomp-ad activate)
   (cond ((and (eq skk-henkan-mode 'on)
	       skk-dcomp-activate
	       (skk-dcomp-marked-p))
	  (goto-char skk-dcomp-end-point)
	  (setq this-command 'skk-comp-do)
	  (skk-dcomp-face-off)
	  (skk-set-marker skk-dcomp-start-point nil)
	  (skk-set-marker skk-dcomp-end-point nil)
	  (skk-start-henkan (ad-get-arg 0)))
	 (t
	  ad-do-it)))

(defadvice skk-delete-backward-char (after skk-dcomp-ad activate)
  (skk-dcomp-after-delete-backward-char))

(defadvice viper-del-backward-char-in-insert (after skk-dcomp-ad activate)
  (skk-dcomp-after-delete-backward-char))

(defadvice vip-del-backward-char-in-insert (after skk-dcomp-ad activate)
  (skk-dcomp-after-delete-backward-char))

(require 'product)
(product-provide
    (provide 'skk-dcomp)
  (require 'skk-version))

;;; skk-dcomp.el ends here
