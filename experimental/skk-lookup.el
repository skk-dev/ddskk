;;; skk-lookup.el --- SKK lookup interface
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-lookup.el,v 1.1 1999/09/28 12:30:51 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/09/28 12:30:51 $

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

;;; Commentary
;;
;; 次のように skk-search-prog-list に加えて指定し使用する。skkserv を
;; 使用した検索の後に持ってくるのがセオリー。
;; 
;;  (setq skk-search-prog-list
;;        '((skk-search-jisyo-file skk-jisyo 0 t)
;;          (skk-search-server skk-aux-large-jisyo 10000)
;;          (skk-lookup-search) ))
;;
;; EPWING 岩波広辞苑第四版を使用して開発した。Entry バッファが次のよう
;; な出力になることを前提に各ユーザー変数のディフォルト値を決めている。
;;
;;    広辞苑　第四版             かめ【瓶・甕】
;;    広辞苑　第四版             かめ【亀】
;;    広辞苑　第四版             カメ
;; 
;;; Code:
(eval-when-compile (require 'skk))
(require 'lookup)

(defgroup skk-lookup nil "SKK lookup related customization."
  :prefix "skk-lookup-"
  :group 'skk )

(defcustom skk-lookup-pickup-pattern "【\\(.+\\)】$"
  "*候補抽出のための regexp。"
  :type 'regexp
  :group skk-lookup )

(defcustom skk-lookup-split-pattern "・"
  "*複数の候補がある場合の候補の区切りの regexp。"
  :type 'regexp
  :group skk-lookup  )

(defcustom skk-lookup-query [:query exact pattern]
  "*Lookup に対し発行する query の形式。
pattern 部分に見出し語が挿入される。"
  :type 'vector
  :group skk-lookup  )

;;;###autoload
(defun skk-lookup-search ()
  (let* ((d (lookup-module-dictionaries lookup-default-module))
	 (query-base (copy-sequence skk-lookup-query))
	 (query (progn (aset query-base 2 skk-henkan-key) query-base))
	v r s )
    (while d
      (setq v (lookup-vse-search-query (car d) query))
      (while v
	(setq r (lookup-entry-heading (car v))
	      v (cdr v) )
	(when (string-match skk-lookup-pickup-pattern r)
	  (if (not skk-lookup-split-pattern)
	      (setq s (cons (match-string 1 r) s))
	    (setq s (nconc
		     (split-string (match-string 1 r) skk-lookup-split-pattern)
		     s )))))
      (setq d (cdr d)) )
    s ))


(provide 'skk-lookup)
;;; Local Variables:
;;; End:
;;; skk-lookup.el ends here
