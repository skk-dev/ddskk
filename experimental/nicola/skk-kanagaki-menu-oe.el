;;; skk-kanagaki-menu-oe.el --- NICOLA-DDSKK のメニューサポート
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: japanese, keyboard
;; Last Modified: $Date: 2000/10/30 22:21:08 $

;; This file is not yet part of Daredevil SKK.

;; Daredevil SKK  is free software;  you  can redistribute it  and/or modify it
;; under the terms  of the GNU General Public License  as published by the Free
;; Software  Foundation;  either versions  2,  or  (at your option)  any  later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful  but WITHOUT
;; ANY  WARRANTY;  without  even  the implied  warranty  of MERCHANTABILITY  or
;; FITNESS  FOR  A PARTICULAR PURPOSE.  See the GNU General Public License  for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; Daredevil SKK,  see the file COPYING.  If not,  write  to  the Free Software
;; Foundation Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

;;; Commentary:

;; easymenu が低機能な Emacsen (20.2 以前) のためのメニューサポートです。

;;; Code:

(defvar skk-kanagaki-menu)

(defconst skk-kanagaki-skk-menu-original
  '("SKK"
    ("Convert Region and Echo"
     ("Gyakubiki"
      ["to Hiragana" skk-gyakubiki-message
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
      ["to Hiragana, All Candidates"
       (call-interactively
	(function (lambda (start end) (interactive "r")
		    (skk-gyakubiki-message start end 'all-candidates))))
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
      ["to Katakana" skk-gyakubiki-katakana-message
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
      ["to Katakana, All Candidates"
       (call-interactively
	(function (lambda (start end) (interactive "r")
		    (skk-gyakubiki-katakana-message
		     start end 'all-candidates))))
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))])
     ("Hurigana"
      ["to Hiragana" skk-hurigana-message
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
      ["to Hiragana, All Candidates"
       (call-interactively
	(function (lambda (start end) (interactive "r")
		    (skk-hurigana-message start end 'all-candidates))))
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
      ["to Katakana" skk-hurigana-katakana-message
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
      ["to Katakana, All Candidates"
       (call-interactively
	(function (lambda (start end) (interactive "r")
		    (skk-hurigana-katakana-message
		     start end 'all-candidates))))
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]))
    ("Convert Region and Replace"
    ["Ascii" skk-ascii-region
     (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
    ("Gyakubiki"
     ["to Hiragana" skk-gyakubiki-region
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
     ["to Hiragana, All Candidates"
      (call-interactively
       (function (lambda (start end) (interactive "r")
		   (skk-gyakubiki-region start end 'all-candidates))))
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
     ["to Katakana" skk-gyakubiki-katakana-region
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
     ["to Katakana, All Candidates"
      (call-interactively
       (function (lambda (start end) (interactive "r")
		   (skk-gyakubiki-katakana-region
		    start end 'all-candidates))))
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))])
    ["Hiragana" skk-hiragana-region
     (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
    ("Hurigana"
     ["to Hiragana" skk-hurigana-region
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
     ["to Hiragana, All Candidates"
      (call-interactively
       (function (lambda (start end) (interactive "r")
		   (skk-hurigana-region start end 'all-candidates))))
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
     ["to Katakana" skk-hurigana-katakana-region
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
     ["to Katakana, All Candidates" (function
				     (lambda (start end) (interactive "r")
				       (skk-hurigana-katakana-region
					start end 'all-candidates)))
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))])
    ["Katakana" skk-katakana-region
     (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
    ["Romaji" skk-romaji-region
     (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
    ["Zenkaku" skk-jisx0208-latin-region
     (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))])
    ["Count Jisyo Candidates" skk-count-jisyo-candidates
    (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
    ["Save Jisyo" skk-save-jisyo
    (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
    ["Undo Kakutei" skk-undo-kakutei
    (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
    ["Version" skk-version
    (or (not (boundp 'skktut-problem-count))
	(eq skktut-problem-count 0))]))

(easy-menu-define
 skk-kanagaki-menu
 (append
  (list skk-j-mode-map skk-latin-mode-map skk-abbrev-mode-map
	skk-jisx0208-latin-mode-map)
  (and (featurep 'skk-jisx0201)
       (list skk-jisx0201-mode-map)))
 "SKK menu modified by NICOLA-DDSKK"
 (append skk-kanagaki-skk-menu-original (list skk-kanagaki-menu-items)))

;;

(require 'product)
(product-provide (provide 'skk-kanagaki-menu-oe) (require 'skk-version))

;; skk-kanagaki-menu-oe.el ends here
