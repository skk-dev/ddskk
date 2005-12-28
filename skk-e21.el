;;; skk-e21.el --- GNU Emacs 21 support for SKK

;; Copyright (C) 1999-2005 SKK Development Team <skk@ring.gr.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method

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
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'static)
  (require 'tooltip))

(eval-and-compile
  (autoload 'Info-goto-node "info")
  (autoload 'browse-url "browse-url"))

;; Variables.
(defvar skk-e21-modeline-menu-items
  (when window-system
    '("Daredevil SKK Menu"
      ["Hiragana"
       (skk-j-mode-on)
       :selected (and skk-j-mode (not skk-katakana))
       :style radio
       :keys nil
       :key-sequence nil]
      ["Katakana"
       (skk-j-mode-on t)
       :selected (and skk-j-mode skk-katakana)
       :style radio
       :keys nil
       :key-sequence nil]
      ["Hankaku alphabet"
       skk-latin-mode
       :selected skk-latin-mode
       :style radio
       :keys nil
       :key-sequence nil]
      ["Zenkaku alphabet"
       skk-jisx0208-latin-mode
       :selected skk-jisx0208-latin-mode
       :style radio
       :keys nil
       :key-sequence nil]
      "--"
      ["Read Manual" skk-e21-info t]
      ["Start Tutorial" skk-tutorial t]
      ["Customize Daredevil SKK" skk-customize t]
      ["Send a Bug Report"
       (let (skk-japanese-message-and-error)
	 (skk-submit-bug-report)) t]
      "--"
      ["About Daredevil SKK..." skk-version t]
      ["Visit Daredevil Web Site" skk-e21-visit-openlab t])))

(defvar skk-e21-menu-resource-ja
  '(("Daredevil SKK Menu" . "Daredevil SKK メニュー")
    ("Convert Region and Echo" . "領域を変換してミニバッファに表示")
    ("Gyakubiki" . "逆引き")
    ("to Hiragana" . "ひらがなに変換")
    ("to Hiragana, All Candidates" . "ひらがなに変換、全ての候補を表示")
    ("to Katakana" . "カタカナに変換")
    ("to Katakana, All Candidates" . "カタカナに変換、全ての候補を表示")
    ("Hurigana" . "ふりがな")
    ("Convert Region and Replace" . "領域を変換して置き換える")
    ("Ascii" . "全角英数を ASCII に変換")
    ("Hiragana" . "ひらがな")
    ("Katakana" . "カタカナ")
    ("Romaji" . "ローマ字に変換")
    ("Zenkaku" . "ASCII を全角英数に変換")
    ("Count Jisyo Candidates" . "辞書中の候補数を数える")
    ("Save Jisyo" . "辞書を保存する")
    ("Undo Kakutei" . "確定を取り消す (アンドゥー)")
    ("Version" . "SKK のバージョン")
    ("Daredevil SKK Menu" . "Daredevil SKK メニュー")
    ("Hankaku alphabet" . "半角英数")
    ("Zenkaku alphabet" . "全角英数")
    ("Read Manual" . "マニュアルを読む")
    ("Start Tutorial" . "チュートリアル")
    ("Customize Daredevil SKK" . "Daredevil SKK をカスタマイズ")
    ("Send a Bug Report" . "バグを報告する")
    ("About Daredevil SKK..." . "Daredevil SKK について...")
    ("Visit Daredevil Web Site" . "Daredevil SKK のサイトへ")))

(defvar skk-e21-modeline-property
  (when window-system
    (list 'local-map (let ((map (make-sparse-keymap)))
		       (define-key map [mode-line mouse-3]
			 #'skk-e21-modeline-menu)
		       (define-key map [mode-line mouse-1]
			 #'skk-e21-circulate-modes)
		       map)
	  'help-echo
	  "mouse-1: モード切替(循環), mouse-3: SKK メニュー"
	  'mouse-face
	  'highlight)))

(defvar skk-e21-property-alist
  (when window-system
    (list
     (cons 'latin skk-e21-modeline-property))))

(defvar skk-e21-coding-system (if (memq window-system '(w32 nil))
				  nil
				locale-coding-system))

;; Functions.

(defun skk-e21-modeline-menu ()
  (interactive)
  ;; Find keys
  (aset (nth 1 skk-e21-modeline-menu-items)
	7
	(cond (skk-katakana
	       (skk-e21-find-func-keys 'skk-toggle-kana))
	      ((not skk-mode)
	       (skk-e21-find-func-keys 'skk-mode))
	      ((not skk-j-mode)
	       (skk-e21-find-func-keys 'skk-kakutei))
	      (t
	       nil)))
  (aset (nth 2 skk-e21-modeline-menu-items)
	7
	(if (and skk-j-mode
		 (not skk-katakana))
	    (skk-e21-find-func-keys 'skk-toggle-kana)
	  nil))
  (aset (nth 3 skk-e21-modeline-menu-items)
	7
	(if skk-j-mode
	    (skk-e21-find-func-keys 'skk-latin-mode)
	  nil))
  (aset (nth 4 skk-e21-modeline-menu-items)
	7
	(if skk-j-mode
	    (skk-e21-find-func-keys 'skk-jisx0208-latin-mode)
	  nil))
  ;;
  (let ((easy-menu-converted-items-table
	 (make-hash-table :test 'equal)))
    (popup-menu skk-e21-modeline-menu-items)))

(defun skk-e21-circulate-modes (&optional arg)
  (interactive "P")
  (cond
   (skk-henkan-mode
    nil)
   ((not skk-mode)
    (skk-mode arg))
   (skk-j-mode
    (if skk-katakana
	(skk-jisx0208-latin-mode arg)
      (skk-toggle-kana arg)))
   (skk-jisx0208-latin-mode
    (skk-latin-mode arg))
   (skk-latin-mode
    (skk-j-mode-on))))

(defun skk-e21-info ()
  (interactive)
  (Info-goto-node "(skk)"))

(defun skk-e21-customize ()
  (interactive)
  (customize-group "skk"))

(defun skk-e21-visit-openlab ()
  (interactive)
  (browse-url "http://openlab.ring.gr.jp/skk/index-j.html"))

;;;###autoload
(defun skk-e21-prepare-modeline-properties ()
  (setq skk-icon
	(let* ((dir (file-name-directory skk-tut-file))
	       (image (find-image
		       `((:type xpm
				:file ,(expand-file-name "skk.xpm" dir)
				:ascent center))))
	       (string "dummy"))
	  (if (and skk-show-icon window-system image)
	      (apply 'propertize string
		     (cons 'display (cons image skk-e21-modeline-property)))
	    nil)))
  ;;
  (unless skk-use-color-cursor
    (setq skk-indicator-use-cursor-color nil))
  ;;
  (when window-system
    (let (face)
      (dolist (mode '(hiragana
		      katakana
		      jisx0208-latin
		      jisx0201
		      abbrev))
	(setq face (intern (format "skk-e21-%s-face" mode)))
	(unless (facep face)
	  (make-face face)
	  (when skk-indicator-use-cursor-color
	    (set-face-foreground face
				 (symbol-value
				  (intern
				   (format "skk-cursor-%s-color"
					   mode))))))
	(push (cons mode (append skk-e21-modeline-property
				 (list 'face face)))
	       skk-e21-property-alist)))))

(defun skk-e21-find-func-keys (func)
  (let ((keys
	 (or (do ((spec (nth 4 skk-rule-tree) (cdr spec))
		  (list nil (car spec))
		  (str nil (when (eq (nth 3 list)
				     func)
			     (nth 1 list))))
		 ((or str (null spec))
		  (if (stringp str)
		      str
		    nil)))
	     (car (where-is-internal func skk-j-mode-map)))))
    (if keys
	(format "%s" (key-description keys))
      nil)))

(defun skk-e21-encode-string (str)
  (if (null skk-e21-coding-system)
      str
    (encode-coding-string str skk-e21-coding-system)))

(defun skk-e21-menu-replace (list)
  (let (cons)
    (while list
      (cond
       ((listp (car list))
	(skk-e21-menu-replace (car list)))
       ((and (stringp (car list))
	     (setq cons (assoc (car list) skk-e21-menu-resource-ja)))
	(setcar list (skk-e21-encode-string (cdr cons))))
       ((and (vectorp (car list))
	     (setq cons (assoc (aref (car list) 0) skk-e21-menu-resource-ja)))
	(aset (car list) 0 (skk-e21-encode-string (cdr cons)))))
      (setq list (cdr list)))))

(defun skk-e21-mouse-position ()
  "Return the position of point as (FRAME X . Y).
Analogous to mouse-position."
  (let* ((w (if skk-isearch-switch
		(minibuffer-window)
	      (selected-window)))
	 (edges (window-edges w))
	 (list
	  (compute-motion (max (window-start w) (point-min))   ; start pos
			  ;; window-start can be < point-min if the
			  ;; latter has changed since the last redisplay
			  '(0 . 0)       ; start XY
			  (or (ignore-errors
				(marker-position
				 skk-henkan-start-point))
			      (point))       ; stop pos
			  (cons (window-width w)
				(window-height w)); stop XY: none
			  (1- (window-width w))       ; width
			  (cons (window-hscroll w) 0)     ; 0 may not be right?
			  w)))
    ;; compute-motion returns (pos HPOS VPOS prevhpos contin)
    ;; we want:               (frame hpos . vpos)
    (cons (selected-frame)
	  (cons (+ (car edges)       (car (cdr list)))
		(+ (car (cdr edges)) (car (cdr (cdr list))))))))

(defun skk-tooltip-show-at-point (text &optional listing)
  (require 'tooltip)
  (let* ((P (skk-e21-mouse-position))
	 (frame (car P))
	 (x (cadr P))
	 (y (cddr P))
	 (oP (mouse-position))
	 (oframe (car oP))
	 (ox (cadr oP))
	 (oy (cddr oP))
	 (inhibit-quit t)
	 event)
    (set-mouse-position frame x y)
    (skk-tooltip-show-1 text skk-tooltip-parameters)
    (setq event (next-command-event))
    (cond
     ((skk-key-binding-member (skk-event-key event)
			      '(keyboard-quit
				skk-kanagaki-bs
				skk-kanagaki-esc)
			      skk-j-mode-map)
      (tooltip-hide)
      (when (and ox oy)
	(set-mouse-position oframe ox oy))
      (skk-set-henkan-count 0)
      (skk-unread-event
       (character-to-event
	(aref (car (where-is-internal
		    'skk-previous-candidate
		    skk-j-mode-map))
	      0)))
      (when listing
	 ;; skk-henkan まで一気に throw する。
	(throw 'unread nil)))
     (t
      (when (and ox oy)
	(set-mouse-position oframe ox oy))
      (tooltip-hide)
      (skk-unread-event event)))))

(defun skk-tooltip-show-1 (text skk-params)
  (condition-case error
      (let ((params (copy-sequence tooltip-frame-parameters))
	    fg bg)
	(if skk-params
	    ;; ユーザが独自に tooltip 表示設定する
	    (dolist (cell skk-params)
	      (setq params (tooltip-set-param params
					      (car cell)
					      (cdr cell))))
	  ;; tooltip のデフォルトの設定をする
	  (setq fg (face-attribute 'tooltip :foreground))
	  (setq bg (face-attribute 'tooltip :background))
	  (when (stringp fg)
	    (setq params (tooltip-set-param params 'foreground-color fg))
	    (setq params (tooltip-set-param params 'border-color fg)))
	  (when (stringp bg)
	    (setq params (tooltip-set-param params 'background-color bg))))
	(unless (ignore-errors
		  (or (get-text-property 0 'face text)
		      (get-text-property 2 'face text)))
	  (setq text (propertize text 'face 'tooltip)))
	(x-show-tip text
		    (selected-frame)
		    params
		    skk-tooltip-hide-delay
		    tooltip-x-offset
		    tooltip-y-offset))
    (error
     (message "Error while displaying tooltip: %s" error)
     (sit-for 1)
     (message "%s" text))))

;; advices.

(defadvice tooltip-hide (after ccc-ad activate)
  (update-buffer-local-frame-params))

(require 'product)
(product-provide
    (provide 'skk-e21)
  (require 'skk-version))

;;; skk-e21.el ends here
