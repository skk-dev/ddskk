;;; skk-w3m.el --- SKK search using w3m-search
;; Copyright (C) 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-w3m.el,v 1.11 2001/05/13 12:16:57 minakaji Exp $
;; Keywords: japanese
;; Created: Apr. 12, 2001 (oh, its my brother's birthday!)
;; Last Modified: $Date: 2001/05/13 12:16:57 $

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
;; の上でこのファイルを SKK-MK があるディレクトリにコピーし (リンク
;; が使えるファイルシステムでは SKK-MK のあるディレクトリで
;;   ln -s ./experimental/skk-w3m.el .
;; した方が良いかもしれません)、後は普通に make install するだけです。
;;
;; <HOW TO WORK>
;; skk-search-prog-list に (skk-w3m-search "goo-daijirin") のような
;; 要素を追加します。通常、他のどの skk search engine よりも最も遅い
;; ので、最も最後が良いでしょう。こんな感じになります。
;;
;; (setq skk-search-prog-list
;;       '((skk-search-jisyo-file skk-jisyo 0 t)
;;         (skk-search-server skk-aux-large-jisyo 10000)
;;         (skk-w3m-search "goo-daijirin")
;;         (skk-w3m-search "goo-exceed-eiwa")))
;;
;; skk-w3m-search の引数は検索エンジンの種類を文字列で指定します。
;; 但し、skk-w3m-search-engine-alist に対応するエントリが必要です。
;;
;; <TODO>
;; o とりあえず skk-w3m-get-candidates-from-goo-exceed-waei,
;;   skk-w3m-get-candidates-from-goo-exceed-eiwa,
;;   skk-w3m-get-candidates-from-goo-daily-shingo を完成させる。
;; o 検索エンジンの増加。
;; o lookup は w3m-search.el を使った Web search を統合しないのだろう
;;   か...。統合すれば skk-lookup.el で一元管理できる？
;;
;;; Code
(eval-when-compile (require 'skk-macs) (require 'skk-vars))
(require 'w3m)
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
     (not skk-abbrev-mode))
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

;; global variables

;;;###autoload
(defun skk-w3m-search (search-engine)
  (let* ((w3m-async-exec nil)
	 (w3m-work-buffer-name " *skk-w3m-work*")
	 (info (assoc search-engine skk-w3m-search-engine-alist))
	 (post-process (nth 3 info))
	 (sex (nth 4 info))
	 (process-key (nth 5 info))
	 (henkan-key skk-henkan-key))
    (condition-case nil
	(save-excursion
	  (if (and info
		   (or (not sex)       ; always search this engine, or
		       (not (eval sex)))) ; search this time.
	      (progn
		(if process-key
		    (setq henkan-key (funcall process-key henkan-key)))
		(or
		 (w3m-w3m-retrieve
		  (format (nth 1 info)
			  (w3m-search-escape-query-string henkan-key (nth 2 info))))
		 (error ""))
		(w3m-with-work-buffer
		  (if post-process (funcall post-process henkan-key))))))
      (error)))) ; catch network unreachable error or something like that.

(defun skk-w3m-filter-string (string filters)
  (while filters
    (while (string-match (car filters) string)
      (setq string (concat (substring string 0 (match-beginning 0))
			   (substring string (match-end 0)))))
    (setq filters (cdr filters)))
  string)

;; (defun skk-w3m-get-candidates (header0 header1 &optional split)
;;   (save-match-data
;;     (if (re-search-forward header0 nil t nil)
;; 	(let (temp v)
;; 	  (while (re-search-forward header1 nil t nil)
;; 	    (setq temp (match-string-no-properties 1))
;; 	    (if split
;; 		(setq v (nconc (split-string temp split) v))
;; 	      (setq v (cons temp v))))
;; 	  (nreverse v)))))

(defun skk-w3m-get-candidates-from-goo-daijirin (key)
  ;; <!-- RESULT_BLOCK -->
  ;; <table width="100%" border="0" cellspacing="0" cellpadding="0"><tr><td>
  ;; <!-- ej_res1 -->
  ;; <table width="100%" border="0" cellspacing="0" cellpadding="0">
  ;;   <tr>
  ;;     <td>
  ;;       ■［<font color="#993333">こうこう</font>］の大辞林第二版からの検索結果　
  ;;      <font size="+1" color="#993333"><b>39件</b></font>
  ;;     </td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td bgcolor="#993333"><img src="/Common/clear.gif" width="1" height="1" alt=""></td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td>
  ;;       <br>
  ;;       <table border="0" cellspacing="4" cellpadding="4">
  ;;         <tr>
  ;;           <td><br></td>
  ;;           <td><b>1</b></td>
  ;;           <td>
  ;;             <a href="/cgi-bin/jp-more_print.cgi?MT=%A4%B3%A4%A6%A4%B3%A4%A6&ID=a4b3/06660300.txt&sw=2" target="_blank">
  ;;             <img src="/Common/icon01.gif" width="12" height="12" border="0" alt="新規で開く"></a>
  ;;             </td>
  ;;           <td nowrap>
  ;;             <a href="/cgi-bin/jp-more_print.cgi?MT=%A4%B3%A4%A6%A4%B3%A4%A6&ID=a4b3/06660300.txt&sw=2">こうこう 【口腔】</a>
  ;;           </td>
  ;;         </tr>
  ;;         ...
  ;;         <tr>
  ;;           <td><br></td>
  ;;           <td><b>25</b></td>
  ;;           <td>
  ;;             <a href="/cgi-bin/jp-more_print.cgi?MT=%A4%B3%A4%A6%A4%B3%A4%A6&ID=a4b3/06663300.txt&sw=2" target="_blank">
  ;;             <img src="/Common/icon01.gif" width="12" height="12" border="0" alt="新規で開く"></a>
  ;;             </td>
  ;;           <td nowrap>
  ;;             <a href="/cgi-bin/jp-more_print.cgi?MT=%A4%B3%A4%A6%A4%B3%A4%A6&ID=a4b3/06663300.txt&sw=2">こうこう 【<img src="/jp/image/G149A.gif" width="14" HEIGHT="19" align="absmiddle" hspace="2"><img src="/jp/image/G149A.gif" width="14" HEIGHT="19" align="absmiddle" hspace="2">】</a>
  ;;           </td>
  ;;         </tr>
  ;;         ...
  ;;       </table>
  ;;       <br>
  ;;     </td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td bgcolor="#993333"><img src="/Common/clear.gif" width="1" height="1" alt=""></td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td>
  ;;       ■［<font color="#993333">こうこう</font>］の大辞林第二版からの検索結果　
  ;;      <font size="+1" color="#993333"><b>39件</b></font>
  ;;     </td>
  ;;   </tr>
  ;; </table>
  ;; <!-- ej_res1 -->
  ;; </td></tr></table>
  ;; <!-- RESULT_BLOCK -->
  (save-match-data
    (let (temp v start end)
      (if (not (search-forward "<!-- RESULT_BLOCK -->" nil t nil))
	  nil
	(setq start (point))
	(if (search-forward "<!-- RESULT_BLOCK -->" nil t nil)
	    (setq end (point)))
	(goto-char start)
	(setq key (concat "<a href=\".+\">" (regexp-quote key) " +【\\([^【】]+\\)】</a>"))
	(while (re-search-forward key end t nil)
	  (setq temp (skk-w3m-filter-string
		      ;; 〈何時〉
		      (match-string-no-properties 1) '("〈" "〉")))
	  (setq v (nconc (split-string temp "・") v)))
	(nreverse v)))))

;; (defun skk-w3m-get-candidates-from-goo-exceed-waei (key)
;;   ;; 15:■［ねっしん］のEXCEED和英辞典からの検索結果　
;;   ;; 16:*
;;   ;; 17:
;;   ;; 18:ねっしん
;;   ;; 19:[clear] 熱心
;;   ;; 20:[clear] zeal；　ardor；　eagerness；　enthusiasm．　〜な　
;;   ;; 21:        eager；　ardent；　keen．　〜に　eagerly；　
;;   ;; 22:        earnestly；　intently．　
;;   ;; 23:
;;   ;; 24:*
;;   ;; 25:■［ねっしん］のEXCEED和英辞典からの検索結果　
;;   (let (temp v)
;;     (save-match-data
;;       (if (not (re-search-forward
;; 		(concat "■\\［" (regexp-quote key) "\\］のEXCEED和英辞典からの検索結果")
;; 		nil t nil))
;; 	  nil
;; 	(while (re-search-forward "\\[clear\\] [a-z]+\\.　\\([^ a-zA-Z][^．]+\\)．" nil t nil)
;; 	  (setq temp (match-string-no-properties 1))
;; 	  (setq temp (skk-w3m-filter-string
;; 		      ;; [[米話]]
;; 		      temp '("\n" "[0-9]+: +" "[　 ]+" "（[ぁ-ん]+）" "([, a-z]+)"
;; 			     "\\[\\[[^a-zA-Z]+\\]\\]")))
;; 	  (while (string-match "\\([^，；]+\\)［\\([^，；]+\\)］\\([^，；]+\\)*" temp)
;; 	    (setq temp (concat (substring temp 0 (match-beginning 0))
;; 			       (match-string-no-properties 1 temp)
;; 			       (match-string-no-properties 3 temp)
;; 			       "，"
;; 			       (match-string-no-properties 2 temp)
;; 			       (match-string-no-properties 3 temp)
;; 			       (substring temp (match-end 0)))))
;;
;; 	  (setq v (nconc v (split-string temp "[，；]"))))
;; 	v))))

(defun skk-w3m-get-candidates-from-goo-exceed-eiwa (key)
  ;; SORRY, NOT YET.
  ;;
  ;; <!-- RESULT_BLOCK -->
  ;; <table width="100%" border="0" cellspacing="0" cellpadding="0"><tr><td>
  ;; <!-- ej_res1 -->
  ;; <table width="100%" border="0" cellspacing="0" cellpadding="0">
  ;;   <tr>
  ;;     <td>
  ;;       ■［<font color="#993333">collaborate</font>］のEXCEED英和辞典からの検索結果　
  ;;     </td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td bgcolor="#993333"><img src="/Common/clear.gif" width="1" height="1" alt=""></td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td>
  ;;       <br>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR ALIGN="LEFT" VALIGN="MIDDLE">
  ;;     <TD>
  ;;       <SPAN CLASS="css4g">
  ;;         <B>col・lab・o・rate</B>　<A HREF="http://dictionary2.goo.ne.jp/ej/voice/C/01010419.wav"><IMG LOWSRC="/ej/image/voice.gif" WIDTH="23" HEIGHT="12" BORDER="0" ALIGN="absmiddle"></A>　
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <IMG SRC="/ej/image/e1073.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1015.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1016.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1022.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1001.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e101b.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1054.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1013.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1003.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1074.gif" WIDTH="8" HEIGHT="16" ALT="">　
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>vi.</i>　共に働く；　共同研究する　<i>(with, on, in)；</i>　敵側［占領軍］に協力する．　
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="48" HEIGHT="2"></TD>
  ;;     <TD WIDTH="420" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <FONT COLOR="FF0000">collaboration</FONT>　<A HREF="http://dictionary2.goo.ne.jp/ej/voice/C/02020773.wav"><IMG LOWSRC="/ej/image/voice.gif" WIDTH="23" HEIGHT="12" BORDER="0" ALIGN="absmiddle"></A>　
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>n.</i>　<FONT COLOR="FF0000">collaborationism</FONT>　<i>n.</i>　<FONT COLOR="FF0000">collaborationist</FONT>　<i>n.</i>　（敵側への）協力者．　
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="48" HEIGHT="2"></TD>
  ;;     <TD WIDTH="420" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <FONT COLOR="FF0000">collaborative</FONT>　
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <IMG SRC="/ej/image/e1073.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1015.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1016.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1022.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1001.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1009.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e101b.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1054.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1013.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1003.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1013.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e101d.gif" WIDTH="8" HEIGHT="16" ALT=""><IMG SRC="/ej/image/e1074.gif" WIDTH="8" HEIGHT="16" ALT="">　
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>a.</i>　共同制作の．　
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="48" HEIGHT="2"></TD>
  ;;     <TD WIDTH="420" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <FONT COLOR="FF0000">collaborator</FONT>　<A HREF="http://dictionary2.goo.ne.jp/ej/voice/C/02020774.wav"><IMG LOWSRC="/ej/image/voice.gif" WIDTH="23" HEIGHT="12" BORDER="0" ALIGN="absmiddle"></A>　
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;; <TABLE WIDTH="468" BORDER="0" CELLSPACING="0" CELLPADDING="0">
  ;;   <TR>
  ;;     <TD ALIGN="LEFT" VALIGN="TOP"><IMG SRC="/Common/clear.gif" WIDTH="68" HEIGHT="2"></TD>
  ;;     <TD WIDTH="400" ALIGN="LEFT" VALIGN="TOP">
  ;;       <SPAN CLASS="css3g">
  ;;         <i>n.</i>　
  ;;       </SPAN>
  ;;     </TD>
  ;;   </TR>
  ;; </TABLE>
  ;;       <br>
  ;;     </td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td bgcolor="#993333"><img src="/Common/clear.gif" width="1" height="1" alt=""></td>
  ;;   </tr>
  ;;   <tr>
  ;;     <td>
  ;;       ■［<font color="#993333">collaborate</font>］のEXCEED英和辞典からの検索結果　
  ;;     </td>
  ;;   </tr>
  ;; </table>
  ;; <!-- ej_res1 -->
  ;; </td></tr></table>
  ;; <!-- RESULT_BLOCK -->
  ;;
  ;; con・tem・po・ra・ry
  ;; [clear] ●●●●●●●●●●●●●●
  ;; [clear] a., n.　同時代の（人，雑誌）　(with)；　同年齢の（
  ;; 人）；　現代の（人）．　
  ;;
  ;; *
  ;; ■［contemporary］のEXCEED英和辞典からの検索結果
  ;;
  ;; 14:■［collaborate］のEXCEED英和辞典からの検索結果　
  ;; 15:*
  ;; 16:
  ;; 17:col・lab・o・rate　 　
  ;; 18:[clear] ●●●●●●●●●●●●　
  ;; 19:[clear] vi.　共に働く；　共同研究する　(with, on, in)；　
  ;; 20:        敵側［占領軍］に協力する．　
  ;; 21:[clear] collaboration　 　
  ;; 22:[clear] n.　collaborationism　n.　collaborationist　n.　（
  ;; 23:        敵側への）協力者．　
  ;; 24:[clear] collaborative　
  ;; 25:[clear] ●●●●●●●●●●●●●●　
  ;; 26:[clear] a.　共同制作の．　
  ;; 27:[clear] collaborator　 　
  ;; 28:[clear] n.　
  ;; 29:
  ;; 30:*
  ;; 31:■［collaborate］のEXCEED英和辞典からの検索結果　
  ;;
  ;; ■［very］のEXCEED英和辞典からの検索結果　 2件
  ;; *
  ;;
  ;; 1  新規で開く  very
  ;;
  ;; 2  新規で開く  Very light
  ;; 
  ;; *
  ;; ■［very］のEXCEED英和辞典からの検索結果　 2件
  ;;
  ;; ■［contemporary］のEXCEED英和辞典からの検索結果
  ;; *
  ;;   (save-match-data
  ;;     (let (temp v start end)
  ;;       (if (not (search-forward "<!-- RESULT_BLOCK -->" nil t nil))
  ;; 	  nil
  ;; 	(setq start (point))
  ;; 	(if (search-forward "<!-- RESULT_BLOCK -->" nil t nil)
  ;; 	    (setq end (point)))
  ;; 	(goto-char start)
  ;; 	(setq key (concat "<a href=\".+\">" (regexp-quote key) " +【\\([^【】]+\\)】</a>"))
  ;; 	(while (re-search-forward key end t nil)
  ;; 	  (setq temp (skk-w3m-filter-string
  ;; 		      ;; 〈何時〉
  ;; 		      (match-string-no-properties 1) '("〈" "〉")))
  ;; 	  (setq v (nconc (split-string temp "・") v)))
  ;; 	(nreverse v)))))
  ;;   (save-match-data
  ;;     (let (v)
  ;;       (if (not (re-search-forward "[0-9]+  新規で開く" nil t nil))
  ;; 	  (if (re-search-forward
  ;; 	       (concat "■\\［" (regexp-quote key) "\\］のEXCEED英和辞典からの検索結果")
  ;; 	       nil t nil)
  ;; 	      (setq v (skk-w3m-get-candidates-from-goo-exceed-eiwa-1)))
  ;; 	(beginning-of-line)
  ;; 	(while (re-search-forward "[0-9]+  新規で開く" nil t nil)
  ;; 	  (backward-char)
  ;; 	  (w3m-view-this-url)
  ;; 	  (goto-char (point-min))
  ;; 	  (if (re-search-forward
  ;; 	       (concat "■\\［" (regexp-quote key) "\\］のEXCEED英和辞典からの検索結果")
  ;; 	       nil t nil)
  ;; 	      (setq v (nconc v (skk-w3m-get-candidates-from-goo-exceed-eiwa-1))))
  ;; 	  (w3m-view-previous-page)))
  ;;       v))
  )

;; (defun skk-w3m-get-candidates-from-goo-exceed-eiwa-1 ()
;;   (save-match-data
;;     (let (temp temp1 temp2 temp3 tail v)
;;       (while (re-search-forward
;; 	      "\\[clear\\] [a-z]+\\.\\(, [a-z]+\\.\\)*　\\([^ a-zA-Z][^．]+\\)．"
;; 	      nil t nil)
;; 	(setq temp (match-string-no-properties 2))
;; 	(setq temp (skk-w3m-filter-string
;; 		  ;; e.x. `捺染（なつせん）工', `(on, in)', `【経営】'
;; 		    temp '("\n" "[0-9]+: +" "[　 ]+" "（[ぁ-ん]+）" "([, a-z]+)"
;; 			   "…の" "【[^【】]+】" "(強意)")))
;; 	(while (string-match
;; 		;; ((...)) は意味を表わすようだ。
;; 		;; e.x. インジケータ　((機器の作動状態を表示する機能))
;; 		;; 括弧内をあえてフィルタリングしないで出力する。
;; 		"\\([^，；]+\\)\\(［\\|((\\)\\([^，；]+\\)\\(］\\|))\\)\\([^，；]+\\)*"
;; 		temp)
;; 	  (setq temp (concat (substring temp 0 (match-beginning 0))
;; 			     (match-string-no-properties 1 temp)
;; 			     (match-string-no-properties 5 temp)
;; 			     "，"
;; 			     (match-string-no-properties 3 temp)
;; 			     (match-string-no-properties 5 temp)
;; 			     (substring temp (match-end 0)))))
;; 	;; 当惑（の原因） → 当惑，当惑の原因
;; 	;; 同時代の（人，雑誌）→  同時代の，同時代の人，同時代の雑誌
;; 	(while (string-match "\\([^，；]+\\)（\\([^；]+\\)）\\([^，；]+\\)*" temp)
;; 	  (setq temp1 (match-string-no-properties 1 temp)
;; 		temp2 (match-string-no-properties 2 temp)
;; 		temp3 (match-string-no-properties 3 temp)
;; 		tail (substring temp (match-end 0)))
;; 	  (setq temp (concat (substring temp 0 (match-beginning 0))
;; 			     temp1 "，"
;; 			     (mapconcat 'identity
;; 					(mapcar
;; 					 (function (lambda (e) (concat temp1 e temp3)))
;; 					 (split-string temp2 "，"))
;; 					"，")
;; 			     tail)))
;; 	;; （問題を）紛糾させる → 紛糾させる，問題を紛糾させる
;; 	(while (string-match "（\\([^；]+\\)）\\([^，；]+\\)" temp)
;; 	  (setq temp1 (match-string-no-properties 1 temp)
;; 		temp2 (match-string-no-properties 2 temp)
;; 		tail (substring temp (match-end 0)))
;; 	  (setq temp (concat (substring temp 0 (match-beginning 0))
;; 			     temp2 "，"
;; 			     (mapconcat 'identity
;; 					(mapcar
;; 					 (function (lambda (e) (concat e temp2)))
;; 					 (split-string temp1 "，"))
;; 					"，")
;; 			     tail)))
;; 	(setq v (nconc v (split-string temp "[，；]")))
;; 	;; skip to next candidate.
;; 	(or (re-search-forward "\\[clear\\] ●+" nil t nil)
;; 	    (goto-char (point-max))))
;;       v)))

(defun skk-w3m-get-candidates-from-goo-daily-shingo (key)
  ;; not yet.
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
  )

(require 'product)
(product-provide (provide 'skk-w3m) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-w3m.el ends here
