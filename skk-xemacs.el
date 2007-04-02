;;; skk-xemacs.el --- XEmacs support for SKK

;; Copyright (C) 2000-2007 SKK Development Team <skk@ring.gr.jp>

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
;; the Free Software Foundation Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'avoid)
  (require 'static))

(eval-and-compile
  (require 'balloon-help)
  (require 'poe)
  (require 'skk-macs))

;;;###autoload (unless (noninteractive) (require 'skk-setup))

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
    ["Customize Daredevil SKK" skk-customize t]
    ["Send a Bug Report"
     (let (skk-japanese-message-and-error)
       (skk-submit-bug-report)) t]
    "--"
    ["About Daredevil SKK..." skk-version t]
    ["Visit Daredevil SKK Web Site" skk-xemacs-visit-openlab t]))

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
  (browse-url "http://openlab.ring.gr.jp/skk/index-j.html"))

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
	    (eval '(make-modeline-command-wrapper
		    'skk-xemacs-modeline-menu)))
	  (define-key map
	    [button1]
	    (eval '(make-modeline-command-wrapper
		    'skk-xemacs-modeline-menu)))
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
	(set-extent-property
	 extent
	 'help-echo
	 "button1 or button3 shows SKK menu"))
      ;;
      (setq face (intern (format "skk-xemacs-%s-face"
				 mode)))
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


(defalias-maybe 'multibyte-string-p 'stringp)

;; XEmacs 21.4
(defalias-maybe 'current-pixel-row 'ignore)

(defalias 'skk-tooltip-hide 'balloon-help-undisplay-help)

(defun skk-tooltip-show-at-point (text &optional listing)
  (require 'avoid)
  (let* ((pos (or (ignore-errors
		  (marker-position
		   skk-henkan-start-point))
		(point)))
	 (P (cdr (skk-xemacs-mouse-position pos)))
	 (window (selected-window))
	 (fontsize (cdr (assq 'PIXEL_SIZE
			      (font-properties (face-font 'default)))))
	 (x (or (current-pixel-column window pos)
		(+ (car P) (/ (or fontsize 0) 2))))
	 (y (or (current-pixel-row window pos)
		(+ (cdr P) (or fontsize 0))))
	 (oP (cdr (mouse-position)))
	 (inhibit-quit t)
	 event)
    (unless (car oP)
      (setq oP (cdr (mouse-avoidance-point-position))))
    (setq balloon-help-help-object-x
	  (+ x (cdr (assq 'left (frame-parameters (selected-frame)))))
	  balloon-help-help-object-y
	  (+ y (cdr (assq 'top (frame-parameters (selected-frame))))))
    ;;
    (mouse-avoidance-set-mouse-position P)
    (let ((balloon-help-font (face-font 'default)))
      (skk-tooltip-show-1 text listing))
    (setq event (next-command-event))
    (cond
     ((skk-key-binding-member (skk-event-key event)
			      '(keyboard-quit
				skk-kanagaki-bs
				skk-kanagaki-esc)
			      skk-j-mode-map)
      (balloon-help-go-away)
      (mouse-avoidance-set-mouse-position oP)
      (skk-set-henkan-count 0)
      (cond ((eq skk-henkan-mode 'active)
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
	     (skk-unread-event event))))
     (t
      (skk-tooltip-hide)
      (mouse-avoidance-set-mouse-position oP)
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
	(balloon-help-timeout skk-tooltip-hide-delay))
    (setq balloon-help-timeout-id nil)
    (when (and (device-on-window-system-p)
	       (stringp help))
      (save-excursion
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
	(balloon-help-expose-help-frame)))
    ;; Is this right?
    (setq balloon-help-timeout-id
	  (add-timeout (/ balloon-help-timeout 1000.0)
		       #'balloon-help-display-help
		       nil))))

(defun skk-xemacs-balloon-help-resize-help-frame (&optional listing)
  ;; 縦の長さが合わないので、合わせる。
  (save-excursion
    (set-buffer balloon-help-buffer)
    (let* ((longest 0)
	   (lines 0)
	   (done nil)
	   (inst (vector 'string ':data nil))
	   (window (frame-selected-window balloon-help-frame))
	   (font-width (font-width (face-font 'default) balloon-help-frame))
	   start width
	   (window-min-height 1)
	   (window-min-width 1))
      (goto-char (point-min))
      (while (not done)
	(setq start (point))
	(end-of-line)
	(aset inst 2 (buffer-substring start (point)))
	(setq longest (max longest (glyph-width (make-glyph inst) window))
	      done (not (= 0 (forward-line))))
	(and (not done) (setq lines (1+ lines))))
      (setq width (/ longest font-width)
	    width (if (> longest (* width font-width)) (1+ width) width))
      ;; Increase width and lines...
      (setq width (1+ width))
      (setq lines (if (or listing
			  (= lines 1))
		      lines
		    (1+ lines)))
      ;;
      (set-frame-size balloon-help-frame (+ 0 width) lines))))

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

(require 'product)
(product-provide
    (provide 'skk-xemacs)
  (require 'skk-version))

;;; skk-xemacs.el ends here
