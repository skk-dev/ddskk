;;; skk-vars.el --- dummy file for XEmacs 20.4.
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-vars.el,v 1.6 1999/10/23 14:00:05 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/10/23 14:00:05 $

;; This file is not part of SKK yet.

;; SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;;  XEmacs 20.4 dumps certain files of SKK 10.38.  This is because
;;  there was not SKK package when 10.38 was developing.  It requires
;;  skk-vars.el (obsolete name of skk-autoloads.el or
;;  auto-autoloads.el).
;;  This file is just dummy only for XEmacs 20.4.  If you make SKK for
;;  XEmacs 20.4, this file will be installed automatically.
;;
;;  Thanks to Naoki Wakamatsu <naoki-w@ht-net21.ne.jp> for giving me
;;  information in detail about XEmacs 20.4.

;;; Code:
(or (and (boundp 'preloaded-file-list) (member "skk-leim" preloaded-file-list))
    (error "You don't have to load skk-vars.el") )

;; reset autoloads.
(autoload 'skk-abbrev-search "skk-abbrev" nil nil nil)
(autoload 'skk-activate "skk-leim" nil nil nil)
(autoload 'skk-ad-to-gengo "skk-gadget" nil nil nil)
(autoload 'skk-adjust-search-prog-list-for-auto-okuri "skk-auto" nil nil nil)
(autoload 'skk-adjust-search-prog-list-for-server-search "skk-server" nil nil nil)
(autoload 'skk-auto-fill-activate "skk-leim" nil nil nil)
(autoload 'skk-auto-fill-inactivate "skk-leim" nil nil nil)
(autoload 'skk-auto-fill-mode "skk" nil t nil)
(autoload 'skk-calc "skk-gadget" nil nil nil)
(autoload 'skk-clock "skk-gadget" nil t nil)
(autoload 'skk-completion "skk-comp" nil nil nil)
(autoload 'skk-current-date "skk-gadget" nil nil nil)
(autoload 'skk-dbm-make-jisyo "skk-dbm" nil nil nil)
(autoload 'skk-dbm-search-jisyo-database "skk-dbm" nil nil nil)
(autoload 'skk-display-code-for-char-at-point "skk-kcode" nil t nil)
(autoload 'skk-gengo-to-ad "skk-gadget" nil nil nil)
(autoload 'skk-gyakubiki-katakana-message "skk-kakasi" nil t nil)
(autoload 'skk-gyakubiki-katakana-region "skk-kakasi" nil t nil)
(autoload 'skk-gyakubiki-message "skk-kakasi" nil t nil)
(autoload 'skk-gyakubiki-region "skk-kakasi" nil t nil)
(autoload 'skk-henkan-face-off-and-remove-itself "skk-gadget" nil nil nil)
(autoload 'skk-hurigana-katakana-message "skk-kakasi" nil t nil)
(autoload 'skk-hurigana-katakana-region "skk-kakasi" nil t nil)
(autoload 'skk-hurigana-message "skk-kakasi" nil t nil)
(autoload 'skk-hurigana-region "skk-kakasi" nil t nil)
(autoload 'skk-ignore-dic-word "skk-gadget" nil nil nil)
(autoload 'skk-inactivate "skk-leim" nil nil nil)
(autoload 'skk-input-by-code-or-menu "skk-kcode" nil t nil)
(autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t nil)
(autoload 'skk-isearch-mode-setup "skk-isearch" nil t nil)
(autoload 'skk-look "skk-look" nil nil nil)
(autoload 'skk-look-completion "skk-look" nil nil nil)
(autoload 'skk-lookup-search "skk-lookup" nil nil nil)
(autoload 'skk-minus "skk-gadget" nil nil nil)
(autoload 'skk-mode "skk" nil t nil)
(autoload 'skk-num "skk-num" nil nil nil)
(autoload 'skk-num-compute-henkan-key "skk-num" nil nil nil)
(autoload 'skk-num-convert "skk-num" nil nil nil)
(autoload 'skk-num-convert*7 "skk-num" nil nil nil)
(autoload 'skk-num-henkan-key "skk-num" nil nil nil)
(autoload 'skk-num-initialize "skk-num" nil nil nil)
(autoload 'skk-num-process-user-minibuf-input "skk-num" nil nil nil)
(autoload 'skk-num-uniq "skk-num" nil nil nil)
(autoload 'skk-num-update-jisyo "skk-num" nil nil nil)
(autoload 'skk-obsolete-check "skk-obsolete" nil t nil)
(autoload 'skk-obsolete-check-all-files "skk-obsolete" nil t nil)
(autoload 'skk-obsolete-put-obsolete-mark "skk-obsolete" nil nil nil)
(autoload 'skk-okuri-search "skk-auto" nil nil nil)
(autoload 'skk-plus "skk-gadget" nil nil nil)
(autoload 'skk-previous-completion "skk-comp" nil nil nil)
(autoload 'skk-romaji-message "skk-kakasi" nil t nil)
(autoload 'skk-romaji-region "skk-kakasi" nil t nil)
(autoload 'skk-server-version "skk-server" nil t nil)
(autoload 'skk-start-henkan-with-completion "skk-comp" nil t nil)
(autoload 'skk-study-read "skk-study" nil t nil)
(autoload 'skk-study-save "skk-study" nil t nil)
(autoload 'skk-study-search "skk-study" nil nil nil)
(autoload 'skk-study-update "skk-study" nil nil nil)
(autoload 'skk-submit-bug-report "skk-develop" nil t nil)
(autoload 'skk-times "skk-gadget" nil nil nil)
(autoload 'skk-today "skk-gadget" nil t nil)
(autoload 'skk-tutcode-display-code "skk-tutcode" nil t nil)
(autoload 'skk-tutcode-mode-off "skk-tutcode" nil nil nil)
(autoload 'skk-tutorial "skk-tut" nil t nil)
(autoload 'skk-version "skk" nil t nil)
(autoload 'skk-viper-normalize-map "skk-viper" nil nil nil)

;; reload new skk-leim.el.
(load "skk-leim")
(provide 'skk-vars)
;;; skk-vars.el ends here
