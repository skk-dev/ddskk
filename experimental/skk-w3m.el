;;; skk-w3m.el --- SKK search using w3m-search
;; Copyright (C) 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-w3m.el,v 1.3 2001/04/12 23:56:48 minakaji Exp $
;; Keywords: japanese
;; Created: Apr. 12, 2001 (oh, its my brother's birthday!)
;; Last Modified: $Date: 2001/04/12 23:56:48 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; Daredevil SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;; emacs-w3m (http://www.namazu.org/~tsuchiya/emacs-w3m) を利用し、
;; Emacs の中から Web 検索エンジンによる検索をし、検索結果の中から
;; SKK の候補として取り出したいものを切り出して利用するプログラムで
;; す。
;;
;; <HOW TO INSTALL>
;; .emacs を読み込まずに emacs-w3m が load できる環境が必須です。そ
;; の上でこのファイルを SKK-MK があるディレクトリにコピーし、後は普
;; 通に make install するだけです。
;;
;; <HOW TO WORK>
;; skk-search-prog-list に (skk-w3m-search "goo-daijirin") のような
;; 要素を追加します。通常、他のどの skk search engine よりも最も遅い
;; ので、最も最後が良いでしょう。こんな感じになります。
;;
;; (setq skk-search-prog-list
;;       '((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
;;         (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
;;         (skk-search-jisyo-file skk-jisyo 0 t)
;;         (skk-search-server skk-aux-large-jisyo 10000)
;;         (skk-w3m-search "goo-daijirin")))
;;
;; skk-w3m-search の引数は検索エンジンの種類を文字列で指定します。
;; 但し、skk-w3m-search-engine-alist に対応するエントリが必要です。
;; w3m-search.el の標準の w3m-search-engine-alist は見ませんので注意
;; が必要です。
;;
;; <TODO>
;; o とりあえず skk-w3m-get-candidates-from-goo-exceed-waei,
;;   skk-w3m-get-candidates-from-goo-exceed-eiwa,
;;   skk-w3m-get-candidates-from-goo-daily-shingo を完成させる。
;; o 検索エンジンの増加。
;; o lookup は w3m-search.el を使った Web search を統合しないのだろう
;;   か...。統合すれば skk-lookup.el で一元管理できる？
;; o w3m の代わりに wget が使えないか (その方が速いのでは？) と試した
;;   が、検索開始からファイルの書き込みまでの速度があまり速くない割には
;;   書き込まれたファイルには HTML タグという邪魔者が付いているという状
;;   態なのでとりあえず見送り...。
;;
;;; Code
(eval-when-compile (require 'skk-macs) (require 'skk-vars))
(require 'w3m-search)

(defgroup skk-w3m nil "SKK w3m related customization."
  :prefix "skk-w3m-"
  :group 'skk)

;;; user variables.
(defvar skk-w3m-search-engine-alist
  '(("goo-daijirin"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=2" euc-japan
     skk-w3m-get-candidates-from-goo-daijirin
     (or 
      ;; cannot search a key which contains okuri prefix.
      skk-okuri-char
      ;; cannot search by Web engine a string which containing SKK special `#' character.
      skk-num-list skk-num-recompute-key
      ;; this engine does not contain English entries.
      skk-abbrev-mode))
    ("goo-exceed-waei"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=1" euc-japan
     skk-w3m-get-candidates-from-goo-exceed-waei
     (or skk-okuri-char skk-num-list skk-num-recompute-key skk-abbrev-mode))
    ("goo-exceed-eiwa"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=0" euc-japan
     skk-w3m-get-candidates-from-goo-exceed-eiwa
     (or (not skk-abbrev-mode) skk-okuri-char skk-num-list skk-num-recompute-key))
    ("goo-daily-shingo"
     "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=3" euc-japan
     skk-w3m-get-candidates-from-goo-daily-shingo
     (or skk-okuri-char skk-num-list skk-num-recompute-key)))
  "*検索エンジン毎の検索オプションを指定するエーリスト。
car は検索エンジンを表わす文字列、
cdr は URL (検索文字列を %s で表わす),
2th は Web page の coding-system,
3th は候補切り出しに使用する関数を表わすシンボル。
4th (optional) は S 式を指定し、評価して non-nil になる状態のときは w3m
    に検索処理をさせない。
5th は `skk-henkan-key' を加工する関数。")

;;; system internal variables and constants.
;; constants.
(defconst skk-w3m-working-buffer " *skk-w3m*")

;; global variables

;;;###autoload
(defun skk-w3m-search (search-engine)
  nil
  (let* ((w3m-display-inline-image nil)
	 (w3m-search-engine-alist skk-w3m-search-engine-alist)
	 (info (assoc search-engine w3m-search-engine-alist))
	 (post-process (nth 3 info))
	 (sex (nth 4 info))
	 (process-key (nth 5 info))
	 (henkan-key skk-henkan-key))	; buffer local variable...
    (condition-case nil
	(save-excursion
	  (if (and info
		   (or (not sex)       ; always search this engine, or
		       (not (eval sex)))) ; search this time.
	      (save-window-excursion
		(if process-key
		   ; must proceed before entering into another buffer.
		    (setq henkan-key (funcall process-key henkan-key)))
		(set-buffer (get-buffer-create skk-w3m-working-buffer))
		(w3m-search search-engine henkan-key)
		(if post-process (funcall post-process henkan-key)))))
      (error)))) ; catch network unreachable error or something like that.

(defun skk-w3m-get-candidates (header0 header1)
  (if (re-search-forward header0 nil t nil)
      (let (v)
	(while (re-search-forward header1 nil t nil)
	  (setq v (cons (match-string-no-properties 1) v)))
	(nreverse v))))

(defun skk-w3m-get-candidates-from-goo-daijirin (key)
  (skk-w3m-get-candidates
   (concat "■\\［" (regexp-quote key) "\\］の大辞林第二版からの検索結果　 [0-9]+件")
   (concat "[0-9]+ +新規で開く +" (regexp-quote key) "【\\([^【】]+\\)】 +$")))

(defun skk-w3m-get-candidates-from-goo-exceed-waei (key)
  ;; not yet.
  ;;(skk-w3m-get-candidates
   ;;(concat "■\\［" (regexp-quote key) "\\］のEXCEED和英辞典からの検索結果")
   ;;(concat "[0-9]+ +新規で開く +" (regexp-quote key) "【\\([^【】]+\\)】 +$")))
  )

(defun skk-w3m-get-candidates-from-goo-exceed-eiwa (key)
  ;; not yet.
  )

(defun skk-w3m-get-candidates-from-goo-daily-shingo (key)
  ;; not yet.
  )

;; 15:■［こうこう］の大辞林第二版からの検索結果　 39件                              
;; 16:*                                                                              
;; 17:                                                                               
;; 18:  1   新規で開く  こうこう【口腔】                                             
;; 19:                                                                               
;; 20:  2   新規で開く  こうこう【工高】                                             
;; 21:                                                                               
;; 22:  3   新規で開く  こうこう【公行】                                             
;; 23:                                                                               
;; 24:  4   新規で開く  こうこう【公侯】                                             
;; 25:                                                                               
;; 26:  5   新規で開く  こうこう【甲香】                                             
;; 27:                                                                               
;; 28:  6   新規で開く  こうこう【交媾】                                             
;; 29:                                                                               
;; 30:  7   新規で開く  こうこう【坑口】                                             
;; 31:                                                                               
;; 32:  8   新規で開く  こうこう【孝行】                                             
;; 33:                                                                               
;; 34:  9   新規で開く  こうこう【後行】                                             
;; 35:                                                                               
;; 36:  10  新規で開く  こうこう【後考】                                             
;; 37:                                                                               
;; 38:  11  新規で開く  こうこう【後攻】                                             
;; 39:                                                                               
;; 40:  12  新規で開く  こうこう【後項】                                             
;; 41:                                                                               
;; 42:  13  新規で開く  こうこう【皇考】                                             
;; 43:                                                                               
;; 44:  14  新規で開く  こうこう【香香】                                             
;; 45:                                                                               
;; 46:  15  新規で開く  こうこう【航行】                                             
;; 47:                                                                               
;; 48:  16  新規で開く  こうこう【降紅】                                             
;; 49:                                                                               
;; 50:  17  新規で開く  こうこう【高校】                                             
;; 51:                                                                               
;; 52:  18  新規で開く  こうこう【黄口】                                             
;; 53:                                                                               
;; 54:  19  新規で開く  こうこう【港口】                                             
;; 55:                                                                               
;; 56:  20  新規で開く  こうこう【硬膏】                                             
;; 57:                                                                               
;; 58:  21  新規で開く  こうこう【硬鋼】                                             
;; 59:                                                                               
;; 60:  22  新規で開く  こうこう【鉱坑】                                             
;; 61:                                                                               
;; 62:  23  新規で開く  こうこう【構桁】                                             
;; 63:                                                                               
;; 64:  24  新規で開く  こうこう【膏肓】                                             
;; 65:                                                                               
;; 66:  25  新規で開く  こうこう【●●】                                             
;; 67:                                                                               
;; 68:  26  新規で開く  こうこう【鴻溝】                                             
;; 69:                                                                               
;; 70:  27  新規で開く  こうこう【高崗】                                             
;; 71:                                                                               
;; 72:  28  新規で開く  こうこう【黄興】                                             
;; 73:                                                                               
;; 74:  29  新規で開く  こうこう【浩浩】                                             
;; 75:                                                                               
;; 76:  30  新規で開く  こうこう【耿耿】                                             
;; 77:                                                                               
;; 78:  31  新規で開く  こうこう【皓皓・皎皎】                                       
;; 79:                                                                               
;; 80:  32  新規で開く  こうこう【煌煌・晃晃】                                       
;; 81:                                                                               
;; 82:  33  新規で開く  こうこう【遑遑】                                             
;; 83:                                                                               
;; 84:  34  新規で開く  こうこう【曠曠・広広】                                       
;; 85:                                                                               
;; 86:  35  新規で開く  こうこう【行行】                                             
;; 87:                                                                               
;; 88:  36  新規で開く  こうこう【杲杲】                                             
;; 89:                                                                               
;; 90:  37  新規で開く  こうこう                                                     
;; 91:                                                                               
;; 92:  38  新規で開く  こうこう【斯う斯う】                                         
;; 93:                                                                               
;; 94:  39  新規で開く  こうこう【鏗鏗】                                             
;; 95:                                                                               
;; 96:                                                                               
;; 97:*                                                                              
;; 98:■［こうこう］の大辞林第二版からの検索結果　 39件                              

;; 15:■［ねっしん］のEXCEED和英辞典からの検索結果　                                 
;; 16:*                                                                              
;; 17:                                                                               
;; 18:ねっしん                                                                       
;; 19:[clear] 熱心                                                                   
;; 20:[clear] zeal；　ardor；　eagerness；　enthusiasm．　〜な　                     
;; 21:        eager；　ardent；　keen．　〜に　eagerly；　                           
;; 22:        earnestly；　intently．　                                              
;; 23:                                                                               
;; 24:*                                                                              
;; 25:■［ねっしん］のEXCEED和英辞典からの検索結果　                                 

;; 15:■［SPA］のデイリー新語辞典からの検索結果　                                    
;; 16:*                                                                              
;; 17:                                                                               
;; 18:SPA                                                                            
;; 19:                                                                               
;; 20:  ［speciality store retailer of private label apparel］                       
;; 21:  自社ブランドの衣料品を売る直営店のこと。また，そのような事業形態。衣料品の企 
;; 22:  画・開発から製造・流通・販売に至るまでを一括して取り扱い，顧客のニーズに効率 
;; 23:  的に対応する。                                                               
;; 24:  →プライベート-ブランド                                                      
;; 25:  〔独自ブランド衣料の専門店販売業者の略。アメリカの衣料小売店による造語が起源 
;; 26:  〕                                                                           
;; 27:                                                                               
;; 28:                                                                               
;; 29:*                                                                              
;; 30:■［SPA］のデイリー新語辞典からの検索結果　                                    

;; 15:■［elaborate］のEXCEED英和辞典からの検索結果　                                
;; 16:*                                                                              
;; 17:                                                                               
;; 18:e・lab・o・rate　 　                                                           
;; 19:[clear] ●●●●●●●●●●　                                                 
;; 20:[clear] a.　念入りな，　綿密［精巧］な，　凝った．　                           
;; 21:        −　                                                                   
;; 22:[clear] ●●●●●●●　                                                       
;; 23:[clear] vt.　苦心して作る［作り出す］，　推敲（すいこう）                      
;; 24:        ［敷延（ふえん）］する．　                                             
;; 25:[clear] elaborately　 　                                                       
;; 26:[clear] ad.　念入りに，　綿密に．　                                            
;; 27:[clear] elaborateness　                                                        
;; 28:[clear] n.　                                                                   
;; 29:[clear] elaboration　 　                                                       
;; 30:[clear] n.　綿密な仕上げ；　推敲；　力作；　追加した詳細．                     
;; 31:        　                                                                     
;; 32:[clear] elaborative　                                                          
;; 33:[clear] a.　入念な．　                                                         
;; 34:                                                                               
;; 35:*                                                                              
;; 36:■［elaborate］のEXCEED英和辞典からの検索結果　                                

(require 'product)
(product-provide (provide 'skk-w3m) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-w3m.el ends here
