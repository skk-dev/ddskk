;;; skk-kanagaki-menu.el --- NICOLA-DDSKK のメニューサポート

;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese, mule, input method

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

;; NICOLA-DDSKK のメニューを SKK 標準のメニューに追加します。

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-vars))

(eval-and-compile
  (autoload 'browse-url-netscape "browse-url"))

(require 'path-util)
(require 'easymenu)

(eval-and-compile
  (defvar skk-kanagaki-menu-items
    '("NICOLA DDSKK"
      ["Set Henkan point" skk-set-henkan-point-subr t]
      ["Input Prefix or Suffix" skk-kanagaki-midashi-henkan t]
      ["Start Conversion with Okuri" skk-kanagaki-set-okurigana t]
      "--"
      ["Input a Character by Code" skk-input-by-code-or-menu t]
      ["Enter SKK Abbrev Mode" skk-abbrev-mode t]
      ["Convert to Hiragana <=> Katakana or Toggle Hiragana <=> Katakana Mode"
       skk-toggle-kana t]
      ["Enter SKK JIS X 0208 Latin Mode" skk-jisx0208-latin-mode t]
      ["\
Convert to Hankaku Katakana or Toggle Katakana <=> Hankaku Katakana Mode"
       skk-toggle-katakana t]
      ["Enter SKK Latin Mode" skk-latin-mode t]
      ["Enter SKK Japanese Mode" skk-kakutei t]
      ["Toggle Roma <=> Kana" skk-kanagaki-toggle-rom-kana t]
      "--"
      ["Show Key Bindings" skk-kanagaki-help t]
      ["Show the Current Keymap based on NICOLA" skk-nicola-help
       (featurep 'skk-nicola)]
      ["Show NICOLA-Specific Key Bindings" skk-nicola-2nd-help
       (featurep 'skk-nicola)]
      "--"
      ["Visit NIHONGO-NYURYOKU CONSORTIUM Web Site"
       skk-nicola-visit-nicola-website (module-installed-p 'browse-url)])))

(static-cond
 ((eq skk-emacs-type 'xemacs)
  (add-hook 'skk-mode-hook
	    (function
	     (lambda ()
	       (add-submenu
		'("SKK")
		skk-kanagaki-menu-items)))))
 ((not (fboundp 'easy-menu-add-item))
  nil)
 (t
  (dolist (map (list skk-j-mode-map
		     skk-latin-mode-map
		     skk-abbrev-mode-map
		     skk-jisx0208-latin-mode-map))
    (easy-menu-add-item
     map
     '("menu-bar" "SKK")
     skk-kanagaki-menu-items))))

;;

(require 'product)
(product-provide
    (provide 'skk-kanagaki-menu)
  (require 'skk-version))

;;; skk-kanagaki-menu.el ends here
