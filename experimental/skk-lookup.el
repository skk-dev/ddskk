;;; skk-lookup.el --- SKK lookup interface
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-lookup.el,v 1.8 1999/10/02 09:31:18 minakaji Exp $
;; Keywords: japanese
;; Created: Sep. 23, 1999
;; Last Modified: $Date: 1999/10/02 09:31:18 $

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
;;
;;; Code:
(eval-when-compile (require 'skk) (require 'cl))
(require 'lookup)

;;;###autoload
(defgroup skk-lookup nil "SKK lookup related customization."
  :prefix "skk-lookup-"
  :group 'skk )

;;;; user variables.
;; not used yet.
;;;###autoload
(defcustom skk-search-agents lookup-search-agents 
  "*検索エージェントの設定のリスト。
リストの各要素は次の形式を取る:

  \(CLASS LOCATION [KEY1 VALUE1 \[KEY2 VALUE2 \[...\]\]\]\)

CLASS には、エージェントの種類をシンボルで指定する。
LOCATION には、エージェントの所在を文字列で指定する。
KEY 及び VALUE は省略可能で、エージェントに対するオプションを指定する。

例: (setq skk-lookup-search-agents
          '((ndtp \"dserver\" :port 2010)
            (ndeb \"/cdrom\" :enable (\"EIWA\")))))"
  :type '(repeat (sexp :tag "agent"))	; type はちょっとややこしすぎ・・
  :group 'skk-lookup )

;;;###autoload
(defcustom skk-lookup-option-alist
  '(
    ;; "あか３ 淦", "ethanol"
    ("CHUJITEN" exact "[０-９]* *\\([^ ]+\\)$" nil nil nil)
    ;; "(皮膚などの)あか <grime>", "《英》 (パイプなどの)あか <fur>"
    ("COLLOC" exact "\\([^ 《》]+\\) <[a-z]+>$" nil nil nil)
    ;; like default but should process gaiji.
    ("IWAKOKU" exact "【\\([^【】]+\\)】" "・" t "_")
    ;; "垢", "赤" 
    ("KANWA" exact "^\\(.+\\)$" nil nil nil)
    ;; "垢"
    ("MYPAEDIA" exact "^\\(.+\\)$" nil nil nil)
    ;; "　あか <scud２>", "　「あか」 <rust>"
    ("PLUS" exact "^　\\(.+\\) <[a-z０-９]+>$" nil nil nil)
    )
  "*辞書毎の検索、文字切り出しオプション。
リストの各要素は下記の通り。

  0th: lookup-dictionary-name が返す文字列、 
  1th: search methods を示すシンボル、
  2th: 候補を切り出すための regexp、
  3th: 切り出された文字列の中に更に複数の候補を含む場合の区切りを表わす regexp、
  4th: 外字データを切り捨てるかどうか、
  5th: 外字データを示す regexp。

現在対応している辞書名は、\"CHUJITEN\", \"COLLOC\", \"IWAKOKU\", \"KANWA\",
\"MYPAEDIA\", \"PLUS\"."
  :type '(repeat (sexp :tag "Dictionary options alist"))
  :group 'skk-lookup )
    
;;;###autoload
(defcustom skk-lookup-default-option-list
  '(exact "【\\([^【】]+\\)】" "・" nil nil)
  "*辞書の検索、文字切り出しオプションのディフォルト。
リストの各要素は下記の通り。

  0th: search methods を示すシンボル、
  1th: 候補を切り出すための regexp、
  2th: 切り出された文字列の中に更に複数の候補を含む場合の区切りを表わす regexp、
  3th: 外字データを切り捨てるかどうか、
  4th: 外字データを示す regexp。

このオプションで対応している辞書名は、\"CHIEZO\", \"KOJIEN\", \"KOUJIEN\",
\"KOKUGO, \"RIKAGAKU\", \"WAEI\".
`lookup-entry-heading' で取り出した文字列が下記のようになることを前提にしている。

  \"あ‐か【亜科】‥クワ\"
  \"あか【閼伽】\"
  \"こ‐しょう【小姓・小性】‥シヤウ\""
  :type '(repeat (sexp :tag "Default dictionary options"))
  :group 'skk-lookup )

;;;; inline functions. 
(defsubst skk-lookup-get-method (name)
  (let ((list (assoc name skk-lookup-option-alist)))
    (car (if list (cdr list) skk-lookup-default-option-list)) ))

(defsubst skk-lookup-get-pickup-regexp (name)
  (let ((list (assoc name skk-lookup-option-alist)))
    (nth 1 (if list (cdr list) skk-lookup-default-option-list)) ))

(defsubst skk-lookup-get-split-regexp (name)
  (let ((list (assoc name skk-lookup-option-alist)))
    (nth 2 (if list (cdr list) skk-lookup-default-option-list)) ))

(defsubst skk-lookup-get-process-gaiji-flag (name)
  (let ((list (assoc name skk-lookup-option-alist)))
    (nth 3 (if list (cdr list) skk-lookup-default-option-list)) ))

;;;; funcitions.
;;;###autoload
(defun skk-lookup-search ()
  (save-excursion
    ;; search pattern.
    (setq lookup-search-pattern 
	  (if skk-use-numeric-conversion
	      (skk-num-compute-henkan-key skk-henkan-key)
	    skk-henkan-key ))
    (let ((module (lookup-default-module))
	  name method query entries candidates-string candidates-list )
      ;; setup modules.
      (lookup-module-setup module)
      (lookup-foreach
       (lambda (dictionary)
	 (when (and (lookup-dictionary-selected-p dictionary)
		    (setq name (lookup-dictionary-name dictionary))
		    (setq method (skk-lookup-get-method name))
		    ;; valid method or not?
		    (memq method (lookup-dictionary-methods dictionary))
		    ;; actual search.
		    (setq entries (lookup-vse-search-query
				   dictionary
				   (lookup-make-query method skk-henkan-key) )))
	   (lookup-foreach
	    (lambda (entry)
	      (setq candidates-string (lookup-entry-heading entry))
	      (if (not (string= lookup-search-pattern candidates-string))
		  (setq candidates-list
			(nconc candidates-list 
			       ;; pickup necessary string for SKK.
			       (skk-lookup-process-heading name candidates-string) ))))
	    entries )))
       ;; dictionaries to be searched.
       (lookup-module-dictionaries module) )
      candidates-list )))

(defun skk-lookup-process-heading (name heading)
  ;; heading しか取り出さないのはもったいない？  他にも情報を取り出し
  ;; ておいて、必要に応じて参照するか？
  (save-match-data
    (do ((pickup-pattern (skk-lookup-get-pickup-regexp name))
	 (split-pattern (skk-lookup-get-split-regexp name))
	 (process-gaiji (skk-lookup-get-process-gaiji-flag name))
	 gaji-regexp candidates-string candidates-list )
	((or (string= heading "")
	     (not (string-match pickup-pattern heading)) )
	 candidates-list )
      (setq candidates-string (match-string 1 heading)
	    heading (substring heading (min (+ (match-end 1) skk-kanji-len)
					    (length heading) )))
      (if (not split-pattern)
	  (progn
	    (when process-gaiji 
	      (setq gaji-regexp (skk-lookup-get-gaiji-pattern name))
	      (setq candidates-list (skk-lookup-process-gaiji
				     gaiji-regexp candidates-string )))
	    (if (not (string= lookup-search-pattern candidates-string))
		(setq candidates-string
		      (cons candidates-string
			    (delete candidates-string candidates-list) ))))
	(when process-gaiji 
	  (setq gaji-regexp (skk-lookup-get-gaiji-pattern name)) )
	(setq candidates-string
	      (lookup-foreach
	       (lambda (k)
		 (if process-gaiji
		     (setq k (skk-lookup-process-gaiji gaiji-regexp k)) )
		 (if (not (string= lookup-search-pattern candidates-string))
		     (setq candidates-list (cons k (delete k candidates-list))) ))
	       (split-string candidates-string split-pattern) )))
      candidates-string )))

(defun skk-lookup-process-gaiji (gaiji-regexp tring)
  (save-match-data
    (while (string-match gaiji-regexp  string)
      (setq string (concat (substring string 0 (match-beginning 0))
			   (substring string (min (1+ (match-beginning 0))
						  (length string) )))))
    string ))

;; for creating new regexp.
(defun skk-lookup-pickup-headings (pattern method)
  (let ((module (lookup-default-module))
	var )
    (lookup-module-setup module)
    (lookup-foreach 
     (lambda (dictionary)
       (lookup-foreach 
	(lambda (entry)
	  (setq var (nconc (list (list (lookup-dictionary-name dictionary)
				       (lookup-dictionary-id dictionary)
				       (lookup-entry-heading entry) ))
			   var )))
	(lookup-vse-search-query
	 dictionary (lookup-make-query method pattern) )))
     (lookup-module-dictionaries module) )
    var ))

(provide 'skk-lookup)
;;; Local Variables:
;;; End:
;;; skk-lookup.el ends here
