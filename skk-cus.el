;;; skk-cus.el --- SKK の簡単かすたまいず試作品

;; Copyright (C) 2001 SKK Development Team

;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; Daredevil SKK, see the file COPYING.  If not, write to the Free Software
;; Foundation Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; SKK 起動後 M-x skk-customize で設定する。

;;; Code:

(eval-when-compile
  (require 'skk-macs))

(require 'skk-vars)
(require 'wid-edit)

(defvar skk-custom-file "~/.skk-cus")
(defvar skk-custom-params nil)
(defvar skk-custom-alist nil)
(defvar skk-custom-buffer-original nil)

(defconst skk-cus-params-visual
  '((skk-use-face
     (const :tag "変換中に色をつける" t) "")
    (skk-use-color-cursor
     (const :tag "カーソルに色をつける" t) "")
    (skk-japanese-message-and-error
     (const :tag "メッセージは日本語で通知する" t) "")
    (skk-show-annotation
     (const :tag "変換時に註釈を表示する" t) "")))

(defconst skk-cus-params-ui
  '((skk-egg-like-newline
     (const :tag "Return [Enter] キーで確定する" t) "")
    (skk-kakutei-early
     (const :tag "明示的な確定を省略可能にする" t) "")
    (skk-delete-implies-kakutei
     (const :tag "▼モードで BS を押したら確定する" t) "")
    (skk-auto-insert-paren
     (const :tag "閉括弧を自動的に挿入する" t) "")))

(defconst skk-cus-params-henkan
  '((skk-auto-start-henkan
     (const :tag "特定の文字の入力時に自動的に変換を開始する" t) "")
    (skk-henkan-okuri-strictly
     (const :tag "送り仮名が厳密に正しい候補のみ表示する" t) "")
    (skk-henkan-strict-okuri-precedence
     (const :tag "送り仮名が厳密に正しい候補を優先して表示する" t) "")
    (skk-check-okurigana-on-touroku
     (choice :tag "辞書登録時の余計な送り仮名の自動処理は？"
      (const :tag "Auto" auto)
      (const :tag "Query" ask)
      (const :tag "Do Nothing" nil))
     "")))

(defconst skk-cus-params-search
  '((skk-use-look
     (const :tag "補完の時に look コマンドを使う" t) "")
    (skk-auto-okuri-process
     (const :tag "送りなし変換で送りあり候補も検索する" t) "")))

(defconst skk-cus-params-input
  '((skk-use-jisx0201-input-method
     (const :tag "半角カナを入力可能にする" t) "")
    (skk-use-kana-keyboard
     (const :tag "かな入力を可能にする" t) "")))

(defconst skk-cus-params-misc
  '((skk-share-private-jisyo
     (const :tag "複数の SKK が個人辞書を共有する" t) "")))

(defun skk-cus-set ()
  (dolist (param skk-custom-alist)
    (set (car param) (cdr param))))

(defun skk-custom-mode ()
  (kill-all-local-variables)
  (setq major-mode 'skk-custom-mode
	mode-name "SKK の設定")
  (use-local-map widget-keymap)
  (run-hooks 'skk-custom-mode-hook))

(defun skk-cus-info (params)
  (delq nil
	(mapcar
	 #'(lambda (el)
	     (let ((val (symbol-value (car el))))
	       (and val
		    (cons (car el) val))))
	 params)))

(defun skk-customize ()
  (interactive)
  (dolist (param (append skk-cus-params-visual
			 skk-cus-params-ui
			 skk-cus-params-henkan
			 skk-cus-params-search
			 skk-cus-params-input
			 skk-cus-params-misc))
    (let ((var (car param)))
      (when (and (eq 'const (caadr param))
		 (symbol-value var))
	(set var t))))
  (setq skk-custom-buffer-original (current-buffer))
  (let (
	(visual (mapcar #'(lambda (entry)
			   `(cons :format "%v%h\n"
				  :doc ,(nth 2 entry)
				  (const :format "" ,(nth 0 entry))
				  ,(nth 1 entry)))
		       skk-cus-params-visual))
	(ui (mapcar #'(lambda (entry)
			   `(cons :format "%v%h\n"
				  :doc ,(nth 2 entry)
				  (const :format "" ,(nth 0 entry))
				  ,(nth 1 entry)))
		       skk-cus-params-ui))
	(henkan (mapcar #'(lambda (entry)
			   `(cons :format "%v%h\n"
				  :doc ,(nth 2 entry)
				  (const :format "" ,(nth 0 entry))
				  ,(nth 1 entry)))
		       skk-cus-params-henkan))
	(search (mapcar #'(lambda (entry)
			   `(cons :format "%v%h\n"
				  :doc ,(nth 2 entry)
				  (const :format "" ,(nth 0 entry))
				  ,(nth 1 entry)))
		       skk-cus-params-search))
	(input (mapcar #'(lambda (entry)
			   `(cons :format "%v%h\n"
				  :doc ,(nth 2 entry)
				  (const :format "" ,(nth 0 entry))
				  ,(nth 1 entry)))
		       skk-cus-params-input))
	(misc (mapcar #'(lambda (entry)
			   `(cons :format "%v%h\n"
				  :doc ,(nth 2 entry)
				  (const :format "" ,(nth 0 entry))
				  ,(nth 1 entry)))
		       skk-cus-params-misc))
	(info (append
	       (skk-cus-info skk-cus-params-visual)
	       (skk-cus-info skk-cus-params-ui)
	       (skk-cus-info skk-cus-params-henkan)
	       (skk-cus-info skk-cus-params-search)
	       (skk-cus-info skk-cus-params-input)
	       (skk-cus-info skk-cus-params-misc))))
    (kill-buffer (get-buffer-create "*SKK の基本設定*"))
    (switch-to-buffer (get-buffer-create "*SKK の基本設定*"))
    (skk-custom-mode)
    (widget-insert "SKK の基本設定。終わったら ")
    (widget-create 'push-button
		   :tag "done"
		   :help-echo "終わったらボクを押して。"
		   :action 'skk-customize-done)
    (widget-insert " を押してください。\n\n")
    (widget-insert "注意: いくつかの設定は再起動が必要です。\n\n")
    (setq skk-custom-params
	  (list
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "表示に関する設定"
				:format "%t:\n%h%v"
				:doc ""
				,@visual))
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "基本的なユーザ・インターフェース"
				:format "%t:\n%h%v"
				:doc ""
				,@ui))
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "変換に関する設定"
				:format "%t:\n%h%v"
				:doc ""
				,@henkan))
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "辞書検索に関する設定"
				:format "%t:\n%h%v"
				:doc ""
				,@search))
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "入力方式に関する設定"
				:format "%t:\n%h%v"
				:doc ""
				,@input))
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "その他の設定"
				:format "%t:\n%h%v"
				:doc ""
				,@misc))))
    (use-local-map widget-keymap)
    (local-set-key "q" 'bury-buffer)
    (widget-setup)
    (goto-char (point-min))))

(defun skk-customize-done (&rest args)
  (interactive)
  (setq skk-custom-alist nil)
  (dolist (params skk-custom-params)
    (setq skk-custom-alist (append skk-custom-alist
				   (widget-value params))))
  (dolist (param (append skk-cus-params-visual
			 skk-cus-params-ui
			 skk-cus-params-henkan
			 skk-cus-params-search
			 skk-cus-params-input
			 skk-cus-params-misc))
    (unless (assq (car param) skk-custom-alist)
      (push (cons (car param) nil) skk-custom-alist)))
  (skk-cus-set)
  (with-temp-buffer
    (insert "(setq skk-custom-alist '"
	    (prin1-to-string skk-custom-alist)
	    ")\n")
    (write-region (point-min) (point-max) skk-custom-file))
  (bury-buffer)
  (unless (eq skk-custom-buffer-original (current-buffer))
    (switch-to-buffer skk-custom-buffer-original))
  (skk-adjust-user-option))

;;;###autoload
(defun skk-cus-setup ()
  (let ((file (expand-file-name skk-custom-file)))
    (when (file-readable-p file)
      (load-file file)
      (skk-cus-set))))

;;

(require 'product)
(product-provide
    (provide 'skk-cus)
  (require 'skk-version))

;;; skk-cus.el ends here
