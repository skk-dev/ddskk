;;; skk-lookup.el --- SKK lookup interface
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-lookup.el,v 1.5 1999/09/29 12:29:21 minakaji Exp $
;; Keywords: japanese
;; Created: Sep. 23, 1999
;; Last Modified: $Date: 1999/09/29 12:29:21 $

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
;; skk.el にある kill-buffer の advice を次のものと入れ替えインストー
;; ルし直す必要がある。
;;
;; (defadvice kill-buffer (around skk-ad activate)
;;   "SKK の▼モードだったら、確定してからバッファをキルする。
;;   バッファのキル後、SKK のモードに従いカーソルの色を変える。"
;;   (and skk-mode skk-henkan-on (interactive-p) (skk-kakutei))
;;   ad-do-it
;;   ;; 別のバッファへ飛ぶコマンドは skk-mode が nil でもカーソル色を調整する必要
;;   ;; がある。
;;   (skk-set-cursor-properly) )
;;
;; 次のように skk-search-prog-list に加えて指定し使用する。
;; skk-seach-server の検索の後に持ってくるのがセオリー。
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
;; つまり前提としている条件は次の 2 点。
;;
;;   (1)見出し語に対する候補は`【'と`】'でくくられている。
;;   (2)複数の候補があったときは、`・' で連接されている。
;;
;;; Code:
(eval-when-compile (require 'skk))
(require 'lookup)

(defgroup skk-lookup nil "SKK lookup related customization."
  :prefix "skk-lookup-"
  :group 'skk )

(defcustom skk-lookup-pickup-pattern "【\\(.+\\)】$"
  "*候補抽出のための regexp。
\(match-string 1\) で候補が取り出せるように指定する。"
  :type 'regexp
  :group 'skk-lookup )

(defcustom skk-lookup-split-pattern "・"
  "*複数の候補がある場合の候補の区切りの regexp。"
  :type 'regexp
  :group 'skk-lookup )

;;;###autoload
(defun skk-lookup-search ()
  (save-excursion
    (save-match-data
      (setq lookup-search-pattern 
	    (if skk-use-numeric-conversion
		(skk-num-compute-henkan-key skk-henkan-key)
	      skk-henkan-key ))
      (let ((module (lookup-default-module))
	    (query (lookup-make-query 'exact skk-henkan-key))
	    entries heading candidates-string candidates-list )
	(lookup-module-setup module)
	(lookup-foreach
	 (lambda (dictionary)
	   (when (and (lookup-dictionary-selected-p dictionary)
		      (memq 'exact (lookup-dictionary-methods dictionary))
		      (setq entries (lookup-vse-search-query dictionary query)) )
	     (lookup-foreach
	      (lambda (entry)
		;; heading しか取り出さないのはもったいない？  他にも
		;; 情報を取り出しておいて、必要に応じて参照するか？
		(setq heading (lookup-entry-heading entry))
		(when (string-match skk-lookup-pickup-pattern heading)
		  (setq candidates-string (match-string 1 heading))
		  (if (not skk-lookup-split-pattern)
		      (setq candidates-list
			    (cons candidates-string
				  (delete candidates-string candidates-list) ))
		    (lookup-foreach
		     (lambda (k)
		       (setq candidates-list
			     (cons k (delete k candidates-list)) ))
		     (split-string candidates-string skk-lookup-split-pattern) ))))
	      entries )))
	 (lookup-module-dictionaries module) )
	candidates-list ))))

(provide 'skk-lookup)
;;; Local Variables:
;;; End:
;;; skk-lookup.el ends here
