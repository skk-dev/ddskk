;;; skk-e21.el --- GNU Emacs 21 support for SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999-2010 SKK Development Team <skk@ring.gr.jp>

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
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'static)
  (require 'tooltip)

  (defvar tool-bar-border)

  (when (= emacs-major-version 21)
    (defalias 'window-inside-pixel-edges 'ignore)
    (defalias 'posn-at-point 'ignore)))

(eval-and-compile
  (autoload 'mouse-avoidance-banish-destination "avoid")
  (autoload 'mouse-avoidance-point-position "avoid")
  (autoload 'mouse-avoidance-set-mouse-position "avoid")
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
      ["Customize SKK" skk-customize-group-skk t]
      ["Customize SKK (simple)" skk-customize t]
      ["Send a Bug Report"
       (let (skk-japanese-message-and-error)
	 (skk-submit-bug-report)) t]
      "--"
      ["About Daredevil SKK.." skk-version t]
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
    ("Hiragana" . "ひらがな")
    ("Katakana" . "カタカナ")
    ("Hiragana to Katakana" . "ひらがなをカタカナに変換")
    ("Katakana to Hiragana" . "カタカナをひらがなに変換")
    ("Kana and Zenkaku to Romaji" . "かな・カナ・全角をローマ字に変換")
    ("Ascii to Zenkaku" . "ASCII を全角英数に変換")
    ("Zenkaku to Ascii" . "全角英数を ASCII に変換")
    ("Count Jisyo Candidates" . "辞書中の候補数を数える")
    ("Save Jisyo" . "辞書を保存する")
    ("Undo Kakutei" . "確定を取り消す (アンドゥー)")
    ("Version" . "SKK のバージョン")
    ("Daredevil SKK Menu" . "Daredevil SKK メニュー")
    ("Hankaku alphabet" . "半角英数")
    ("Zenkaku alphabet" . "全角英数")
    ("Read Manual" . "マニュアルを読む")
    ("Start Tutorial" . "チュートリアル")
    ("Customize SKK" . "SKK をカスタマイズ")
    ("Customize SKK (simple)" . "SKK をカスタマイズ (簡易版)")
    ("Send a Bug Report" . "バグを報告する")
    ("About Daredevil SKK.." . "Daredevil SKK について..")
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

(defvar skk-e21-modeline-menu nil)

;; Functions.

;;;###autoload
(defun skk-e21-prepare-menu ()
  (unless skk-e21-modeline-menu
    (setq skk-e21-modeline-menu
	  (easy-menu-create-menu (car skk-e21-modeline-menu-items)
				 (cdr skk-e21-modeline-menu-items))))
  ;;
  (unless (or (null window-system)
	      (eq window-system 'w32)
	      (boundp 'mac-carbon-version-string) ; Carbon Emacs
	      (featurep 'ns) ; Cocoa Emacs
	      (and (eq window-system 'x)
		   (>= emacs-major-version 22)
		   (boundp 'gtk-version-string)
		   (stringp (symbol-value 'gtk-version-string))
		   (string< "2.0" (symbol-value 'gtk-version-string))))
    (setq skk-show-japanese-menu nil))
  ;;
  (when skk-show-japanese-menu
    (skk-e21-menu-replace skk-e21-modeline-menu)
    (dolist (map (list skk-j-mode-map skk-latin-mode-map
		       skk-jisx0208-latin-mode-map skk-abbrev-mode-map))
      (skk-e21-menu-replace (or (assq 'skk (assq 'menu-bar map))
				(assq 'SKK (assq 'menu-bar map)))))))

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
    (popup-menu skk-e21-modeline-menu)))

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
  (browse-url "http://openlab.jp/skk/index-j.html"))

;;;###autoload
(defun skk-e21-prepare-modeline-properties ()
  (setq skk-icon
	(let* ((dir (ignore-errors
		      (file-name-directory
		       (static-if (fboundp 'locate-file)
			   ;; Emacs 22.1 or later
			   (or (locate-file "skk/skk.xpm"
					    (list (expand-file-name
						   "../../.."
						   data-directory)))
			       (locate-file "skk/skk.xpm"
					    (list data-directory)))
			 ;; Emacs 21
			 skk-tut-file))))
	       (image (when dir
			(find-image
			 `((:type xpm
				  :file ,(expand-file-name "skk.xpm" dir)
				  :ascent center)))))
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
      (dolist (mode '(hiragana katakana jisx0208-latin jisx0201 abbrev))
	(setq face (intern (format "skk-e21-%s-face" mode)))
	(unless (facep face)
	  (make-face face)
	  (when skk-indicator-use-cursor-color
	    (set-face-foreground face
				 (symbol-value
				  (intern
				   (format "skk-cursor-%s-color" mode))))))
	(push (cons mode (append skk-e21-modeline-property
				 (list 'face face)))
	       skk-e21-property-alist)))))

(defun skk-e21-find-func-keys (func)
  (let ((keys
	 (or (do ((spec (nth 4 skk-rule-tree) (cdr spec))
		  (list nil (car spec))
		  (str nil (when (eq (nth 3 list) func)
			     (nth 1 list))))
		 ((or str (null spec))
		  (if (stringp str)
		      str
		    nil)))
	     (car (where-is-internal func skk-j-mode-map)))))
    (if keys
	(format "%s" (key-description keys))
      nil)))

(defun skk-e21-menu-replace (list)
  (let ((running-ntemacs (and (eq window-system 'w32)
			      (not (fboundp 'Meadow-version))))
	cons)
    (while (and list (listp list))
      (cond
       ((and (car-safe list)
	     (listp (car list)))
	(skk-e21-menu-replace (car list)))
       ((and (stringp (car-safe list))
	     (setq cons (assoc (car list) skk-e21-menu-resource-ja)))
	(setcar list (if (and running-ntemacs
			      (member (car list) '("Hiragana" "Katakana"
				      "Hankaku alphabet" "Zenkaku alphabet")))
			 ;; NTEmacs で Widget 付きメニューアイテムの
			 ;; 日本語がうまく表示できない問題への対策
			 ;; (NTEmacs 22.1, 23.1)
			 (encode-coding-string (cdr cons) 'shift_jis)
		       (cdr cons))))
       ((and (vectorp (car-safe list))
	     (setq cons (assoc (aref (car list) 0) skk-e21-menu-resource-ja)))
	(aset (car list) 0 (if (and running-ntemacs
				    (member (aref (car list) 0)
					    '("Hiragana" "Katakana"
					      "Hankaku alphabet" "Zenkaku alphabet")))
			       ;; NTEmacs で Widget 付きメニューアイテムの
			       ;; 日本語がうまく表示できない問題への対策
			       ;; (NTEmacs 22.1, 23.1)
			       (encode-coding-string (cdr cons) 'shift_jis)
			     (cdr cons)))))
      (setq list (cdr list)))))

(defun skk-e21-mouse-position ()
  "ポイントの位置を (FRAME X . Y) の形で返す。
これは `mouse-avoidance-point-position' とほぼ同じだが、SKK ▼モードのときは
▼のポイントを返す。"
  (let* ((w (if skk-isearch-switch
		(minibuffer-window)
	      (selected-window)))
	 (edges (window-edges w))
	 (list
	  (compute-motion (max (window-start w) (point-min))   ; start pos
			  ;; window-start can be < point-min if the
			  ;; latter has changed since the last redisplay
			  '(0 . 0)       ; start XY
			  (if (eq skk-henkan-mode 'active)
			      (ignore-errors
				(marker-position skk-henkan-start-point))
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

(defun skk-tooltip-resize-text (text)
  (let ((lines 0)
	(max-lines
	 ;; 画面の半分の高さを基準に最大高を決める
	 (- (/ (/ (display-pixel-height) 2) (frame-char-height))
	    2))
	(columns 0)
	(current-column nil))
    (with-temp-buffer
      (set-buffer-multibyte t)
      (insert text)
      (goto-char (point-min))
      (while (not (eobp))
	(setq lines (1+ lines))
	(cond ((= lines max-lines)
	       ;; 長すぎる
	       (beginning-of-line)
	       (insert "(長すぎるので省略されました)")
	       (delete-region (point) (point-max))
	       (goto-char (point-max))
	       (setq text (buffer-string)))
	      (t
	       (end-of-line)
	       (setq current-column (current-column))
	       (when (> current-column columns)
		 (setq columns current-column))
	       (forward-line 1)))))
    ;; (text . (x . y))
    (cons text (cons columns lines))))

(defun skk-tooltip-show-at-point (text &optional situation)
  (require 'tooltip)
  ;; Emacs 21 では、マウスポインタ非依存の位置決定ができない (と思われる)
  (when (eq emacs-major-version 21)
    (setq skk-tooltip-mouse-behavior 'follow))
  ;;
  (let* ((P (cdr (skk-e21-mouse-position)))
	 (oP (cdr (mouse-position)))
	 event
	 parameters
	 (avoid-destination (if (memq skk-tooltip-mouse-behavior
				      '(avoid avoid-maybe banish))
				(mouse-avoidance-banish-destination)
			       nil))
	 edges
	 tip-destination
	 fontsize
	 left top
	 tooltip-info tooltip-size
	 text-width text-height
	 screen-width screen-height
	 (inhibit-quit t)
	 (tooltip-use-echo-area nil))
    ;;
    (when (null (car P))
      (unless (memq skk-tooltip-mouse-behavior '(avoid-maybe banish nil))
	(setq oP (cdr (mouse-avoidance-point-position)))))
    ;; Elscreen 使用時は Y 座標がずれる。とりあえず workaround。
    (when (and (featurep 'elscreen)
	       (not (or skk-isearch-switch
			(skk-in-minibuffer-p)))
	       (symbol-value 'elscreen-display-tab))
      (setcdr P (1+ (cdr P))))
    ;;
    (when (eq skk-tooltip-mouse-behavior 'follow)
      (mouse-avoidance-set-mouse-position P))
    ;;
    (when (or (and (memq skk-tooltip-mouse-behavior '(avoid banish))
		   (not (equal (mouse-position) avoid-destination)))
	      (and (eq skk-tooltip-mouse-behavior 'avoid-maybe)
		   (cadr (mouse-position))
		   (not (equal (mouse-position) avoid-destination))))
      (set-mouse-position (selected-frame)
			  (car avoid-destination)
			  ;; XXX pending
			  ;; マウスポインタはどこへいくべきか
			  ;; (cdr avoid-destination)
			  0))
    ;;
    (cond
     ((eq skk-tooltip-mouse-behavior 'follow)
      (setq tooltip-info (skk-tooltip-resize-text text)
	    text (car tooltip-info)))
     (t
      ;; マウスポインタに依存せず tooptip の位置を決定する。
      (setq edges (window-inside-pixel-edges
		   (if skk-isearch-switch
		       (minibuffer-window)
		     (selected-window)))
	    tip-destination (posn-x-y
			     (if skk-isearch-switch
				 (posn-at-point
				  (with-current-buffer
				      (window-buffer (minibuffer-window))
				    (point-min))
				  (minibuffer-window))
			       (posn-at-point (point))))
	    fontsize (frame-char-height)
	    ;; x 座標 (左からの)
	    left (+ (car tip-destination)
		    (nth 0 edges)
		    (frame-parameter (selected-frame) 'left)
		    skk-tooltip-x-offset)
	    ;; y 座標 (上からの)
	    ;; 正しいか分からないが、ツールバーとメニューバーの分も計算する
	    ;; (ただしウィンドウデコレータなどの扱いは関知できない？)
	    top  (+ (if (boundp 'tool-bar-images-pixel-height)
			tool-bar-images-pixel-height
		      0)
		    (if (boundp 'tool-bar-button-margin)
			(* 2 tool-bar-button-margin)
		      0)
		    (if (boundp 'tool-bar-button-relief)
			(* 2 tool-bar-button-relief)
		      0)
		    (if (boundp 'tool-bar-border)
			(cond ((integerp tool-bar-border)
			       tool-bar-border)
			      ((symbolp tool-bar-border)
			       (frame-parameter (selected-frame)
						tool-bar-border))
			      (t
			       0))
		      0)
		    (if menu-bar-mode
			(* 1 fontsize)
		      0)
		    (if (and (featurep 'elscreen)
			     (not (or skk-isearch-switch
				      (skk-in-minibuffer-p)))
			     (symbol-value 'elscreen-display-tab))
			(* 1 fontsize)
		      0)
		    ;; magic
		    (* 1 fontsize)
		    ;;
		    (cdr tip-destination)
		    (nth 1 edges)
		    (frame-parameter (selected-frame) 'top)
		    skk-tooltip-y-offset)
	    tooltip-info (skk-tooltip-resize-text text)
	    text (car tooltip-info)
	    tooltip-size (cdr tooltip-info)
	    text-width (* (/ (1+ fontsize) 2) (+ 2 (car tooltip-size)))
	    text-height (* fontsize (+ 1 (cdr tooltip-size)))
	    screen-width (display-pixel-width)
	    screen-height (display-pixel-height))
      ;;
      (when (> (+ left text-width) screen-width)
	;; 右に寄りすぎて欠けてしまわないように
	(setq left (- left (- (+ left text-width
				 ;; 少し余計に左に寄せないと avoid
				 ;; したマウスポインタと干渉する
				 (* 2 fontsize))
			      screen-width))))
      (when (> (+ top text-height) screen-height)
	;; 下に寄りすぎて欠けてしまわないように
	(setq top (- top
		     (- (+ top text-height) screen-height)
		     ;; 十分上げないとテキストと重なるので、
		     ;; いっそテキストの上にしてみる
		     (- screen-height top)
		     fontsize))
	;; さらに X 座標を...
	(let ((right (+ left
			text-width
			skk-tooltip-x-offset))
	      (mouse-x (+ (frame-parameter (selected-frame) 'left)
			  (* (frame-pixel-width)))))
	  (when (and (<= left mouse-x) (<= mouse-x right))
	    ;; マウスポインタと被りそうなとき
	    (setq left (- left (- right mouse-x) fontsize)))))))
    ;;
    (setq parameters (if (eq skk-tooltip-mouse-behavior 'follow)
			 skk-tooltip-parameters
		       (append skk-tooltip-parameters
			       (list (cons 'top top)
				     (cons 'left left)))))
    ;;
    (skk-tooltip-show-1 text parameters)
    ;;
    (when (eq situation 'annotation)
      (skk-annotation-message situation))
    ;;
    (setq event (next-command-event))
    (cond
     ((skk-key-binding-member (skk-event-key event)
			      '(keyboard-quit
				skk-kanagaki-bs
				skk-kanagaki-esc)
			      skk-j-mode-map)
      (tooltip-hide)
      (when (and (not (memq skk-tooltip-mouse-behavior '(banish nil)))
		 (car oP))
	(mouse-avoidance-set-mouse-position oP))
      (skk-set-henkan-count 0)
      (cond ((eq skk-henkan-mode 'active)
	     (skk-unread-event
	      (character-to-event
	       (aref (car (where-is-internal 'skk-previous-candidate
					     skk-j-mode-map))
		     0)))
	     (when (eq situation 'listing)
	       ;; skk-henkan まで一気に throw する。
	       (throw 'unread nil)))
	    (t
	     (skk-unread-event event))))
     (t
      (when (and (not (memq skk-tooltip-mouse-behavior '(banish nil)))
		 (car oP))
	(mouse-avoidance-set-mouse-position oP))
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
	(when (facep skk-tooltip-face)
	  (setq text (propertize text 'face skk-tooltip-face)))
	;; ミニバッファにいるとき余計なメッセージをクリアする
	(when (or skk-isearch-switch
		  (skk-in-minibuffer-p))
	  (message nil))
	;;
	(let ((x-gtk-use-system-tooltips nil))
	  ;; GTK 付 Emacs で、GTK のツールティップを利用すると
	  ;; 現状フェイス属性が適用されないので、Emacs のツール
	  ;; ティップを用いる。
	  (x-show-tip text (selected-frame) params skk-tooltip-hide-delay
		      ;;
		      (if (eq skk-tooltip-mouse-behavior 'follow)
			  skk-tooltip-x-offset
			tooltip-x-offset)
		      ;;
		      (if (eq skk-tooltip-mouse-behavior 'follow)
			  skk-tooltip-y-offset
			tooltip-y-offset))))
    (error
     (message "Error while displaying tooltip: %s" error)
     (sit-for 1)
     (message "%s" text))))

(defalias 'skk-tooltip-hide 'tooltip-hide)

;; advices.

(defadvice tooltip-hide (after ccc-ad activate)
  (update-buffer-local-frame-params))

(provide 'skk-e21)

;;; skk-e21.el ends here
