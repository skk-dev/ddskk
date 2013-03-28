;;; skk-xemacs.el --- XEmacs support for SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 2000-2010 SKK Development Team <skk@ring.gr.jp>

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
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'avoid))

(require 'balloon-help)
(require 'overlay)
(require 'poe)
(require 'skk-macs)

(eval-and-compile
  (autoload 'display-pixel-height "frame")
  (autoload 'display-pixel-width "frame"))

;;;###autoload (unless noninteractive (require 'skk-setup))

;; Variables.
(defvar skk-xemacs-extent-alist
  (list
   (cons 'default (make-extent nil nil))
   (cons 'hiragana (make-extent nil nil))
   (cons 'katakana (make-extent nil nil))
   (cons 'jisx0208-latin (make-extent nil nil))
   (cons 'latin (make-extent nil nil))
   (cons 'jisx0201 (make-extent nil nil))
   (cons 'abbrev (make-extent nil nil))))

(defvar skk-xemacs-modeline-menu-items
  '("Daredevil SKK Menu"
    ["Hiragana"
     (cond (skk-mode
	    (skk-j-mode-on))
	   (t
	    (skk-mode t)))
     :selected (and skk-j-mode (not skk-katakana))
     :style radio
     :keys nil]
    ["Katakana"
     (cond (skk-mode
	    (skk-j-mode-on t))
	   (t
	    (skk-mode t)
	    (skk-j-mode-on t)))
     :selected (and skk-j-mode skk-katakana)
     :style radio
     :keys nil]
    ["Hankaku alphabet"
     skk-latin-mode
     :selected skk-latin-mode
     :style radio
     :keys nil]
    ["Zenkaku alphabet"
     skk-jisx0208-latin-mode
     :selected skk-jisx0208-latin-mode
     :style radio
     :keys nil]
    "--"
    ["Read Manual" skk-xemacs-info t]
    ["Start Tutorial" skk-tutorial t]
    ["Customize SKK" skk-customize-group-skk t]
    ["Customize SKK (simple)" skk-customize t]
    ["Send a Bug Report"
     (let (skk-japanese-message-and-error)
       (skk-submit-bug-report)) t]
    "--"
    ["About Daredevil SKK..." skk-version t]
    ["Visit Daredevil SKK Web Site" skk-xemacs-visit-openlab t]))

(defvar skk-tooltip-default-font-pixel-size 12)

(defvar skk-xemacs-need-redraw-tooltip nil
  "*Compiz 稼動時などツールティップの横幅がおかしいときに設定。
Non-nil ならばツールティップを再描画する。")

(defvar skk-xemacs-redraw-interval 0.01
  "*ツールティップ再描画の際に置く時間。")

;; Functions.

(defun skk-xemacs-modeline-menu ()
  (interactive)
  ;; Find keys
  (aset (nth 1 skk-xemacs-modeline-menu-items)
	7
	(cond (skk-katakana
	       (skk-xemacs-find-func-keys 'skk-toggle-kana))
	      ((not skk-mode)
	       (skk-xemacs-find-func-keys 'skk-mode))
	      ((not skk-j-mode)
	       (skk-xemacs-find-func-keys 'skk-kakutei))
	      (t
	       nil)))
  (aset (nth 2 skk-xemacs-modeline-menu-items)
	7
	(if (and skk-j-mode
		 (not skk-katakana))
	    (skk-xemacs-find-func-keys 'skk-toggle-kana)
	  nil))
  (aset (nth 3 skk-xemacs-modeline-menu-items)
	7
	(if skk-j-mode
	    (skk-xemacs-find-func-keys 'skk-latin-mode)
	  nil))
  (aset (nth 4 skk-xemacs-modeline-menu-items)
	7
	(if skk-j-mode
	    (skk-xemacs-find-func-keys 'skk-jisx0208-latin-mode)
	  nil))
  ;;
  (popup-menu skk-xemacs-modeline-menu-items))

(defun skk-xemacs-info ()
  (interactive)
  (Info-goto-node "(skk)"))

(defun skk-xemacs-customize ()
  (interactive)
  (customize-group "skk"))

(defun skk-xemacs-visit-openlab ()
  (interactive)
  (browse-url "http://openlab.jp/skk/index-j.html"))

;;;###autoload
(defun skk-xemacs-prepare-modeline-properties ()
  (setq skk-icon
	(if (and skk-show-icon
		 (locate-data-file "skk.xpm")
		 (featurep 'xpm))
	    (let ((glyph (make-glyph)))
	      (set-glyph-image glyph
			       (vector 'xpm
				       :file (locate-data-file "skk.xpm")))
	      (cons (cdr (assq 'hiragana skk-xemacs-extent-alist))
		    glyph))
	  nil))
  ;;
  (unless skk-use-color-cursor
    (setq skk-indicator-use-cursor-color nil))
  ;;
  (let (extent face)
    (when window-system
      (defvar skk-xemacs-modeline-map
	(let ((map (make-sparse-keymap)))
	  (define-key map
	    [button3]
	    (eval '(make-modeline-command-wrapper 'skk-xemacs-modeline-menu)))
	  (define-key map
	    [button1]
	    (eval '(make-modeline-command-wrapper 'skk-xemacs-modeline-menu)))
	  map)))
    (dolist (mode '(hiragana
		    katakana
		    jisx0208-latin
		    latin
		    jisx0201
		    abbrev))
      ;;
      (setq extent (cdr (assq mode skk-xemacs-extent-alist)))
      (when window-system
	(set-extent-keymap extent skk-xemacs-modeline-map)
	(set-extent-property extent
			     'help-echo
			     "button1 or button3 shows SKK menu"))
      ;;
      (setq face (intern (format "skk-xemacs-%s-face" mode)))
      (unless (find-face face)
	(make-face face)
	(set-face-parent face 'modeline nil '(default))
	(when (and window-system
		   skk-indicator-use-cursor-color)
	  (set-face-foreground face
			       (symbol-value
				(intern (format
					 "skk-cursor-%s-color"
					 mode)))
			       nil
			       '(default color win))))
      (set-extent-face extent face))))

(defun skk-xemacs-find-func-keys (func)
  (let ((keys
	 (or (do ((spec (nth 4 skk-rule-tree) (cdr spec))
		  (list nil (car spec))
		  (str nil (if (eq (nth 3 list)
				   func)
			       (nth 1 list)
			     nil)))
		 ((or str (null spec))
		  (cond
		   ((not (stringp str))
		    nil)
		   ((string= str "\C-j")
		    [(control j)])
		   (t
		    str))))
	     (let ((k (where-is-internal func skk-j-mode-map)))
	       (dolist (key k)
		 (when (and (= (length key) 2) (eq (aref key 1) 'linefeed))
		   (aset key 1 '(control j))))
	       k))))
    (if keys
	(sorted-key-descriptions (if (listp keys)
				     (skk-remove-duplicates keys)
				   keys))
      nil)))

(defun skk-xemacs-mouse-position (pos)
  "Returns (WINDOW X . Y) of current point - analogous to mouse-position"
  (let* ((beg (window-start))
	 (col (save-excursion
		(goto-char pos)
		(current-column)))
	 (row))
    (setq row (count-lines beg pos))
    (cons (selected-window) (cons col row))))


(unless (fboundp 'multibyte-string-p)
  (defalias 'multibyte-string-p 'stringp))

;; XEmacs 21.4
(unless (fboundp 'current-column)
  (defalias 'current-pixel-row 'ignore))

(defalias 'skk-tooltip-hide 'balloon-help-undisplay-help)

(defun skk-tooltip-resize-text (text)
  (let ((lines 0)
	(max-lines
	 ;; 画面の半分の高さを基準に最大高を決める
	 (- (/ (/ (display-pixel-height) 2)
	       (or (cdr (assq 'PIXEL_SIZE (font-properties (face-font 'default))))
		   skk-tooltip-default-font-pixel-size))
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
  (require 'avoid)
  (let* ((pos (if skk-isearch-switch
		  (with-current-buffer
		      (window-buffer (minibuffer-window))
		    (point-min))
		(or (and (eq skk-henkan-mode 'active)
			 (ignore-errors
			   (marker-position
			    skk-henkan-start-point)))
		    (point))))
	 (P (cdr (skk-xemacs-mouse-position pos)))
	 (oP (cdr (mouse-position)))
	 (avoid-destination (if (memq skk-tooltip-mouse-behavior
				      '(avoid avoid-maybe banish))
				(mouse-avoidance-banish-destination)
			       nil))
	 (window (if skk-isearch-switch
		     (minibuffer-window)
		   (selected-window)))
	 (fontsize (or (cdr (assq 'PIXEL_SIZE
				  (font-properties (face-font 'default))))
		       0))
	 (edges (window-pixel-edges window))
	 (left (+ (car edges)
		  (* 0 (/ (1+ fontsize) 2))
		  (or (current-pixel-column window pos)
		      (+ (car P) (/ (1+ fontsize) 2)))
		  (frame-parameter (selected-frame) 'left)))
	 (top (+ (cadr edges)
		 (* 7 (/ (1+ fontsize) 2))
		 (or (current-pixel-row window pos)
		     (+ (cdr P) fontsize))
		 (frame-parameter (selected-frame) 'top)))
	 (tooltip-info (skk-tooltip-resize-text text))
	 (text (car tooltip-info))
	 (tooltip-size (cdr tooltip-info))
	 (text-width (* (/ (1+ fontsize) 2) (+ 2 (car tooltip-size))))
	 (text-height (* fontsize (+ 1 (cdr tooltip-size))))
	 (screen-width (display-pixel-width))
	 (screen-height (display-pixel-height))
	 (inhibit-quit t)
	 event)
    ;;
    (when (null (car P))
      (unless (memq skk-tooltip-mouse-behavior '(avoid-maybe banish nil))
	(setq oP (cdr (mouse-avoidance-point-position)))))
    ;;
    (when (> (+ left text-width (* 4 fontsize)) screen-width)
      ;; 右に寄りすぎて欠けてしまわないように
      (setq left (- left (- (+ left text-width
			       ;; 少し余計に左に寄せないと avoid
			       ;; したマウスポインタと干渉する
			       (* 10 (/ (1+ fontsize) 2)))
			    screen-width))))
    (when (> (+ top text-height (* 3 fontsize)) screen-height)
      ;; 下に寄りすぎて欠けてしまわないように
      (setq top (- top
		   (- (+ top text-height) screen-height)
		   ;; 十分上げないとテキストと重なるので、
		   ;; いっそテキストの上にしてみる
		   (- screen-height top)
		   (* 4 fontsize)))
      ;; さらに X 座標を...
      (let ((right (+ left
		      text-width
		      skk-tooltip-x-offset))
	    (mouse-x (+ (frame-parameter (selected-frame) 'left)
			(* (frame-pixel-width)))))
	(when (and (<= left mouse-x) (<= mouse-x right))
	  ;; マウスポインタと被りそうなとき
	  (setq left (- left (- right mouse-x) (* 4 fontsize))))))
    ;;
    (setq balloon-help-help-object-x (+ left skk-tooltip-x-offset)
	  balloon-help-help-object-y (+ top skk-tooltip-y-offset))
    ;;
    (when (eq skk-tooltip-mouse-behavior 'follow)
      (mouse-avoidance-set-mouse-position P))
    ;;
    (when (or (and (memq skk-tooltip-mouse-behavior '(avoid banish))
		   (not (equal (mouse-position) avoid-destination)))
	      (and (eq skk-tooltip-mouse-behavior 'avoid-maybe)
		   (cadr (mouse-position))
		   (not (equal (mouse-position) avoid-destination))))
      (save-window-excursion
	(select-window (frame-rightmost-window (selected-frame) 0))
	(mouse-avoidance-banish-mouse)))
    ;;
    (skk-tooltip-show-1 text (eq situation 'listing))
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
      (skk-tooltip-hide)
      (when (and (not (memq skk-tooltip-mouse-behavior '(banish nil)))
		 (car oP))
	(mouse-avoidance-set-mouse-position oP))
      (skk-set-henkan-count 0)
      (cond ((eq skk-henkan-mode 'active)
	     (skk-unread-event
	      (character-to-event
	       (aref (car (where-is-internal
			   'skk-previous-candidate
			   skk-j-mode-map))
		     0)))
	     (when (eq situation 'listing)
	       ;; skk-henkan まで一気に throw する。
	       (throw 'unread nil)))
	    (t
	     (skk-unread-event event))))
     (t
      (skk-tooltip-hide)
      (when (and (not (memq skk-tooltip-mouse-behavior '(banish nil)))
		 (car oP))
	(mouse-avoidance-set-mouse-position oP))
      ;; I don't know what magic it is...
      (sit-for 0.01)
      ;;
      (skk-unread-event event)))))

(defun skk-tooltip-show-1 (help &optional listing)
  (let ((balloon-help-frame-name
	 (or (cdr (assq 'name skk-tooltip-parameters))
	     balloon-help-frame-name))
	(balloon-help-foreground
	 (or (cdr (assq 'foreground-color skk-tooltip-parameters))
	     balloon-help-foreground))
	(balloon-help-background
	 (or (cdr (assq 'background-color skk-tooltip-parameters))
	     balloon-help-background))
	(balloon-help-border-color
	 (or (cdr (assq 'border-color skk-tooltip-parameters))
	     balloon-help-border-color))
	(balloon-help-border-width
	 (or (cdr (assq 'border-with skk-tooltip-parameters))
	     balloon-help-border-width))
	(balloon-help-timeout skk-tooltip-hide-delay)
	(balloon-help-font (face-font 'default)))
    (setq balloon-help-timeout-id nil)
    (when (and (device-on-window-system-p)
	       (stringp help))
      (save-excursion
	(when (find-face skk-tooltip-face)
	  (setq help (propertize help 'face skk-tooltip-face)))
	(when (or (not (frame-live-p balloon-help-frame))
		  (not (eq (selected-device)
			   (frame-device balloon-help-frame))))
	  (setq balloon-help-frame (balloon-help-make-help-frame)))
	(set-buffer balloon-help-buffer)
	(erase-buffer)
	(insert help)
	(if (not (bolp))
	    (insert ?\n))
	(indent-rigidly (point-min) (point-max) 1)
	(balloon-help-set-frame-properties)
	(skk-xemacs-balloon-help-resize-help-frame listing)
	(balloon-help-move-help-frame)
	(balloon-help-expose-help-frame)
	(when skk-xemacs-need-redraw-tooltip
	  ;; Compiz 稼動時に再描画しないと横幅がおかしくなることあり
	  (sleep-for skk-xemacs-redraw-interval)
	  (skk-tooltip-hide)
	  (sleep-for skk-xemacs-redraw-interval)
	  (balloon-help-expose-help-frame))))
    ;; Is this right?
    (setq balloon-help-timeout-id
	  (add-timeout (/ balloon-help-timeout 1000.0)
		       #'balloon-help-display-help
		       nil))))

(defun skk-xemacs-balloon-help-resize-help-frame (&optional listing)
  ;; GNU Emacs と違い XEmacs の balloon help はサイズの自動調整が
  ;; うまく効かず、文字が欠けることもあるため、ここで出来るだけ
  ;; 調整する。
  (save-excursion
    (set-buffer balloon-help-buffer)
    (let* ((longest 0)
	   (lines 0)
	   (done nil)
	   (inst (vector 'string ':data nil))
	   (window (frame-selected-window balloon-help-frame))
	   (font-width (min (font-width (face-font 'default)
					balloon-help-frame)
			    (glyph-width (make-glyph "a") window)
			    (/ (glyph-width (make-glyph "あ") window)
			       2)))
	   start width
	   (window-min-height 1)
	   (window-min-width 1))
      (goto-char (point-min))
      (while (not done)
	(setq start (point))
	(end-of-line)
	(aset inst 2 (buffer-substring start (point)))
	(setq longest (max longest (glyph-width (make-glyph inst) window))
	      done (not (zerop (forward-line))))
	(and (not done) (setq lines (1+ lines))))
      (setq width (round (/ (float longest) (float font-width))))
      ;; Increase width and lines...
      (when (<= width 10)
	(setq width (+ (round (log (float width) 2)) width)))
      (setq lines (cond
		   ((= lines 1)
		    lines)
		   ((or listing (= lines 2))
		    (+ 1 lines))
		   (t
		    (+ (max 1
			    (round (/ (float lines) 10.0))) ; 1+ per 10 lines
			    lines))))
      ;;
      (set-frame-size balloon-help-frame width lines))))

;; Hooks.

;;; Not necessary, but...
;;;###autoload (add-hook 'before-init-hook
;;;###autoload	  #'(lambda ()
;;;###autoload	      (define-key ctl-x-map "\C-j" 'skk-mode)))

;; Advice.

(skk-defadvice minibuffer-keyboard-quit (around skk-xemacs-ad activate)
  ;; XEmacs has `minibuffer-keyboard-quit'
  ;; that has nothing to do with delsel.
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer 'skk-add-skk-pre-command)
  (skk-exit-henkan-in-minibuff)
  (cond ((not skk-mode)
	 ad-do-it)
	((not skk-henkan-mode)
	 (cond ((skk-get-prefix skk-current-rule-tree)
		(skk-erase-prefix 'clean))
	       (t
		ad-do-it)))
	((eq skk-henkan-mode 'active)
	 (setq skk-henkan-count 0)
	 (if (and skk-delete-okuri-when-quit
		  skk-henkan-okurigana)
	     (let ((count (length skk-henkan-okurigana)))
	       (skk-previous-candidate)
	       ;; ここでは `delete-backward-char' に
	       ;; 第二引数を渡さない方がベター？
	       (delete-backward-char count))
	   (skk-previous-candidate)))
	(t
	 (skk-erase-prefix 'clean)
	 (when (> (point) skk-henkan-start-point)
	   (delete-region (point) skk-henkan-start-point))
	 (skk-kakutei))))

;; XEmacs 21.4 workarounds.

(when (and (= emacs-major-version 21)
	   (<= emacs-minor-version 4))
  (defalias 'skk-tooltip-show-at-point 'ignore)
  (defalias 'skk-tooltip-hide 'ignore))

(provide 'skk-xemacs)

;;; skk-xemacs.el ends here
