;;; skk-num.el --- 数値変換のためのプログラム
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-num.el,v 1.2 1999/08/29 13:28:01 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/08/29 13:28:01 $

;; This file is part of SKK.

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

;;; Change log:

;; Following people contributed modifications to skk.el (Alphabetical order):
;;      Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
;;      Manabu Kawashima <kaw@lp.nm.fujitsu.co.jp>

;;; Code:
(eval-when-compile (require 'skk))
(require 'skk-foreword)
(require 'cl)

(defgroup skk-num nil "SKK number conversion related customization."
  :prefix "skk-num-"
  :group 'skk )

;; user variables.
;;;###autoload
(defcustom skk-num-type-alist
  '((0 . identity)
    (1 . skk-num-jisx0208-latin)
    (2 . skk-num-type2-kanji)
    (3 . skk-num-type3-kanji)
    (4 . skk-num-recompute)
    (5 . skk-num-type5-kanji)
    (9 . skk-num-shogi) )
  "*数値の変換のための、インデクスと変換に使用する関数とのエーリスト。
各要素は、`(インデクス . 関数名)' という構成になっている。
car 部分は、例えば、見出し語が \"平成#1年\" のとき、`#' 記号の直後に表示される数
字 `1' を代入する。

インデクスと関数の関係は下記の通り。
    0 -> 無変換
    1 -> 全角数字へ変換
    2 -> 漢数字へ変換 \(位取りなし\)
    3 -> 漢数字へ変換 \(位取りをする\)
    4 -> その数字そのものをキーにして辞書を再検索
    5 -> 漢数字 (手形などで使用する文字を使用) へ変換 (位取りをする)
    9 -> 将棋で使用する数字 \(\"３四\" など\) に変換" 
  :type '(repeat (cons
		  (choice (integer 0 :tag "Muhenkan")
			  (integer 1 :tag "Zenkaku Henkan")
			  (integer 2 :tag "Kansuuji Henkan (Kuraidori ari)")
			  (integer 3 :tag "Kansuuji Henkan (Kuraidori nasi)")
			  (integer 4 :tag "Saikensaku")
			  (integer 5 :tag "Kansuuji Henkan (old Kanji)")
			  (integer 9 :tag "Shogi Moji") )
		  function ))
  :group 'skk-num )

(defcustom skk-num-convert-float nil
  "*Non-nil であれば、浮動小数点数を使った見出し語に対応して変換を行なう。
この値を non-nil にすることで、\"#.# /#1．#1/#0月#0日/\" などの辞書見出しが使用
できなくなるので、注意。"
  :type 'boolean
  :group 'skk-num )

;;;###autoload
(defcustom skk-num-uniq (or (assq 4 skk-num-type-alist)
			    (and (assq 2 skk-num-type-alist)
				 (assq 3 skk-num-type-alist) ))
  "*Non-nil であれば、異なる数値表現でも変換結果が同じ数値を重複して出力しない。"
  :type 'boolean
  :group 'skk-num )

(defcustom skk-num-load-hook nil
  "*skk-num.el をロードした後にコールされるフック。"
  :type 'hook
  :group 'skk-num )

;; internal constants and variables
;;;###autoload
(defconst skk-num-alist-type1
  '((?0 . "０") (?1 . "１") (?2 . "２") (?3 . "３")
    (?4 . "４") (?5 . "５") (?6 . "６") (?7 . "７")
    (?8 . "８") (?9 . "９")
    (?. . "．")				; 小数点。(?. . ".") の方が良い人もいるかも...。
    (?  . "") )
  "ascii 数字の char type と全角数字の string type の連想リスト。
\"1995\" -> \"１９９５\" のような文字列の変換を行う際に利用する。" )

(defconst skk-num-alist-type2
  '((?0 . "〇") (?1 . "一") (?2 . "二") (?3 . "三")
    (?4 . "四") (?5 . "五") (?6 . "六") (?7 . "七")
    (?8 . "八") (?9 . "九") (?  . "") )
  "ascii 数字の char type と漢数字の string type の連想リスト。
\"1995\" -> \"一九九五\" のような文字列の変換を行う際に利用する。" )

(defconst skk-num-alist-type5
  '((?1 . "壱") (?2 . "弐") (?3 . "参")
    (?4 . "四") (?5 . "伍") (?6 . "六") (?7 . "七")
    (?8 . "八") (?9 . "九") (?  . "") )
  "ascii 数字の char type と漢数字の string type の連想リスト。
\"1995\" -> \"壱阡九百九拾伍\" のような文字列の変換を行う際に利用する。" )

;;;###autoload
(skk-deflocalvar skk-num-list nil
  "skk-henkan-key の中に含まれる数字を表す文字列のリスト。
例えば、\"▽へいせい7ねん10がつ\" の変換を行うとき、skk-henkan-key は
\"へいせい7ねん10がつ\" であり、skk-num-list は \(\"7\" \"10\"\) となる。" )

;;;###autoload
(skk-deflocalvar skk-num-recompute-key nil
  "#4 タイプのキーにより数値の再計算を行なったときの検索キー。" )

;;;###autoload
(defun skk-num-compute-henkan-key (key)
  ;; KEY の中の連続する数字を現わす文字列を "#" に置き換えた文字列を返す。"12"
  ;; や "０９" など連続する数字を 1 つの "#" に置き換えることに注意。
  ;; 置き換えた数字を skk-num-list の中にリストの形で保存する。
  ;; 例えば、KEY が "へいせい7年12がつ" であれば、"へいせい#ねん#がつ"
  ;; と変換し、skk-num-list に ("7" "12") というリストを代入する。
  ;; 辞書の見出し語の検索に使用する。
  (let ((numexp (if skk-num-convert-float
		    "[.0-9]+" "[0-9]+" )))
    ;;(setq skk-noconv-henkan-key key)
    (save-match-data
      ;; 位取りの "," を除去する。
      (while (string-match "," key)
	(setq key (concat (substring key 0 (match-beginning 0))
			  (substring key (match-end 0)) )))
      ;; 全角数字を ascii 数字に変換する。
      (while (string-match "[０-９]" key)
        (let ((zen-num (match-string 0 key)))
          (setq key (concat (substring key 0 (match-beginning 0))
                            (skk-jisx0208-to-ascii zen-num)
                            (substring key (match-end 0)) ))))
      ;; ascii 数字を "#" に置き換え、その数字を skk-num-list の中に保存。
      (while (string-match numexp key)
        (setq skk-num-list (nconc skk-num-list (list (match-string 0 key)))
              key (concat (substring key 0 (match-beginning 0))
                          "#"
                          (substring key (match-end 0)) )))))
  key )

;;;###autoload
(defun skk-num-convert (key)
  ;; KEY と skk-num-list から数値変換後の文字列を返す。
  ;; skk-henkan-count が指している数値変換キーの候補を変換し、
  ;; skk-henkan-list を
  ;;   ("#2" ... ) -> (("#2" ."一") ...)
  ;; のように変形する。
  (if (not key)
      nil
    (let ((numexp (if skk-num-convert-float
                      "#[.0-9]+" "#[0-9]+" ))
          (n 0)
          (workkey key)
          num convnum string convlist current )
      (save-match-data
        (while (and (setq num (nth n skk-num-list))
                    (string-match numexp workkey) )
          (setq convnum (skk-num-exp num (string-to-number
                                          (substring workkey
                                                     (1+ (match-beginning 0))
                                                     (match-end 0) )))
                string (substring workkey 0 (match-beginning 0))
                workkey (substring workkey (match-end 0))
                n (1+ n) )
          (if (not (and (stringp convnum) (string= convnum "")
                        (string= string "") ))
              (setq convlist (nconc convlist (list string convnum))) ))
        (setq convlist (nconc convlist (list workkey)))
        (cond ((null convlist) nil)
              ((and (null (cdr convlist)) (stringp (car convlist)))
               (setq current (car convlist)) )
              ;; RAW-LIST の全要素が文字列。
              ((null (memq t (mapcar 'listp convlist)))
               (setq current (mapconcat 'identity convlist ""))
               (if (and (> skk-henkan-count -1)
                        (nth skk-henkan-count skk-henkan-list) )
                   ;; ("A" "#2" "C") -> ("A" ("#2" ."一") "C")
                   (setf (nth skk-henkan-count skk-henkan-list)
                         (cons key current) )
                 (setq skk-henkan-list
                       (nconc skk-henkan-list (list (cons key current))) )))
              ;; #4
              (t (let ((l (mapcar (function (lambda (e) (cons key e)))
                                  (skk-num-flatten-list (delete "" convlist)) )))
                   (setq current (cdr (car l)))
                   (if (and (> skk-henkan-count -1)
                            (nth skk-henkan-count skk-henkan-list) )
                       (progn
                         (setf (nth skk-henkan-count skk-henkan-list) (car l))
                         (setq skk-henkan-list (skk-splice-in
                                                skk-henkan-list
                                                (1+ skk-henkan-count)
                                                (cdr l) )))
                     (setq skk-henkan-list (nconc skk-henkan-list l)) ))))
        current ))))

;;;###autoload
(defun skk-num-convert*7 ()
  (let ((skk-henkan-count skk-henkan-count)
        (n 7) )
    (while (and (> n 0) (nth skk-henkan-count skk-henkan-list))
      (skk-num-convert (skk-get-current-candidate))
      (setq skk-henkan-count (1+ skk-henkan-count)
            n (1- n) ))
    (and skk-num-recompute-key (skk-num-uniq)) ))

(defun skk-num-rawnum-exp (string)
  (setq string (skk-num-rawnum-exp-1
                string "[０-９][〇一九五三四七二八六]" "#9" 0 ))
  (setq string (skk-num-rawnum-exp-1
                string "\\(^\\|[^#0-9]\\)\\([0-9]+\\)" "#0" 2 ))
  (setq string (skk-num-rawnum-exp-1
                string "[０-９]+" "#1" 0 ))
  (setq string (skk-num-rawnum-exp-1
                string "\\([〇一九五三四七二八六十][十百千万億兆京]\\)+" "#3" 0 ))
  ;; (mapcar 'char-to-string
  ;;         (sort
  ;;          '(?一 ?二 ?三 ?四 ?五 ?六 ?七 ?八 ?九 ?〇) '<))
  ;;   --> ("〇" "一" "九" "五" "三" "四" "七" "二" "八" "六")
  ;;
  ;; [〇-九] という正規表現が使えないので、生のままつっこんでおく。
  (skk-num-rawnum-exp-1 string "[〇一九五三四七二八六]+" "#2" 0))

(defun skk-num-rawnum-exp-1 (string key type place)
  (save-match-data
    (while (string-match key string)
      (setq string (concat (substring string 0 (match-beginning place))
			   type
			   (substring string (match-end place)) )))
    string ))

(defun skk-num-flatten-list (list)
  ;; 与えられたリストの各要素から組み合せ可能な文字列の連接を作り、リストで返
  ;; す。
  ;; (("A" "B") "1" ("X" "Y")) -> ("A1X" "A1Y" "B1X" "B1Y")
  (do ((result
        (if (atom (car list)) (list (car list)) (car list))
        (mapcan (function
                 (lambda (a)
                   (mapcar (function (lambda (b) (concat a b)))
                           (if (atom (car tail)) (list (car tail))
                             (car tail) ))))
                result ))
       (tail (cdr list) (cdr tail)) )
      ((null tail) result) ))

(defun skk-num-exp (num type)
  ;; ascii 数字の NUM を TYPE に従い変換し、変換後の文字列を返す。
  ;; TYPE は下記の通り。
  ;; 0 -> 無変換
  ;; 1 -> 全角数字へ変換
  ;; 2 -> 漢数字へ変換 (位取りなし)
  ;; 3 -> 漢数字へ変換 (位取りをする)
  ;; 4 -> その数字そのものをキーにして辞書を再検索
  ;; 5 -> 漢数字 (手形などで使用する文字を使用) へ変換 (位取りをする)
  ;; 9 -> 将棋で使用する数字 ("３四" など) に変換
  (let ((fun (cdr (assq type skk-num-type-alist))))
    (if fun (funcall fun num)) ))

(defun skk-num-jisx0208-latin (num)
  ;; ascii 数字の NUM を全角数字の文字列に変換し、変換後の文字列を返す。
  ;; 例えば "45" を "４５" に変換する。
  (let ((candidate
         (mapconcat (function (lambda (c) (cdr (assq c skk-num-alist-type1))))
                    num "" )))
    (if (not (string= candidate ""))
        candidate )))

(defun skk-num-type2-kanji (num)
  ;; ascii 数字 NUM を漢数字の文字列に変換し、変換後の文字列を返す。
  ;; 例えば、"45" を "四五" に変換する。
  (save-match-data
    (if (not (string-match "\\.[0-9]" num))
        (let ((candidate
               (mapconcat (function (lambda (c)
                                      (cdr (assq c skk-num-alist-type2)) ))
                          num "" )))
          (if (not (string= candidate ""))
              candidate )))))

(defun skk-num-type3-kanji (num)
  ;; ascii 数字 NUM を漢数字の文字列に変換し (位取りをする)、変換後の文字列を
  ;; 返す。例えば "1021" を "千二十一" に変換する。
  (save-match-data
    (if (not (string-match "\\.[0-9]" num))
	;; 小数点を含まない数
        (let ((str (skk-num-type3-kanji-1 num)))
          (if (string= "" str) "〇" str) ))))

(defun skk-num-type3-kanji-1 (num)
  ;; skk-num-type3-kanji のサブルーチン。
  (let ((len (length num))
        modulo char prevchar v )
    ;; 「千京」までは出力する。
    (when (> len 20) (skk-error "位が大きすぎます！" "Too big number!"))
    (setq num (append num nil))
    (while (setq char (car num))
      ;; 位:     一    十     百    千    万   十万   百万    千万     億
      ;; modulo: 1 --> 2 --> 3 --> 0 -> 1 --> 2 ---> 3 ---> 0 ---> 1
      ;; len:    1     2     3     4    5     6      7      8      9
      (setq modulo (mod len 4))
      (if (= len 1)
	  ;; 一の位で 0 でない数。
	  (unless (eq char ?0)
	    ;; 位を表わす漢数字以外の漢数字。
	    (setq v (concat v (cdr (assq char skk-num-alist-type2)))) )
	;; 位を表わす漢数字以外の漢数字。
	(when (or
	       ;; 十の位以上で、かつ 0, 1 以外の数字。
	       (null (memq char '(?0 ?1)))
	       ;; 十の位以上の 1 で、この位が、位を表わす漢数字に "一" を
	       ;; 併記すべき (例えば、"一億" など。"億" ではおかしい) とき。
	       (and (eq char ?1) (= modulo 1)) )
	  (setq v (concat v (cdr (assq char skk-num-alist-type2)))) )
	;; 位を表わす漢数字。
	(if (and (eq char ?0) (not (= modulo 1)))
	    nil
	  (when (memq modulo '(2 3 0))
	    (setq v (concat v (cdr (assq modulo '((2 . "十") (3 . "百") (0 . "千")))))) )
	  ;; 「十万」以上の位でその後も 0 が続くとき。
	  (when (> len 5)
	    (cond ((and (= modulo 2) (eq (nth 1 num) ?0))
		   (setq num (cdr num) len (1- len) char (nth 1 num)) )
		  ((and (= modulo 3) (eq (nth 1 num) ?0) (eq (nth 2 num) ?0))
		   (setq num (nthcdr 2 num) len (- len 2) char (nth 2 num)) )
		  ((and (= modulo 0) (eq (nth 1 num) ?0) (eq (nth 2 num) ?0)
			(eq (nth 3 num) ?0) )
		   (setq num (nthcdr 3 num) len (- len 3) char (nth 3 num)) )))
	  (when (and (memq len '(5 9 13 17)) (not (eq prevchar ?0)))
	    (setq v (concat
		     v
		     (cdr (assq len '((5 . "万") (9 . "億") (13 . "兆") (17 . "京")))) )))))
      (setq len (1- len) prevchar char num (cdr num)) )
    v ))

(defun skk-num-type5-kanji (num)
  ;; ascii 数字 NUM を漢数字の文字列に変換し (位取りをする)、変換後の文字列を
  ;; 返す。例えば "1021" を "壱阡弐拾壱" に変換する。
  (save-match-data
    (if (not (string-match "\\.[0-9]" num))
	;; 小数点を含まない数
        (let ((str (skk-num-type5-kanji-subr num)))
          (if (string= "" str) "零" str) ))))

(defun skk-num-type5-kanji-1 (num)
  ;; skk-num-type5-kanji のサブルーチン。
  (let ((len (length num))
        modulo char prevchar v )
    ;; 「千京」までは出力する。
    (when (> len 20) (skk-error "位が大きすぎます！" "Too big number!"))
    (setq num (append num nil))
    (while (setq char (car num))
      (setq modulo (mod len 4))
      (if (= len 1)
	  (unless (eq char ?0)
	    (setq v (concat v (cdr (assq char skk-num-alist-type5)))) )
	;; 位を表わす漢数字以外の漢数字。
	(setq v (concat v (cdr (assq char skk-num-alist-type5))))
	;; 位を表わす漢数字。
	(if (and (eq char ?0) (not (= modulo 1)))
	    nil
	  (when (memq modulo '(2 3 0))
	    (setq v (concat v (cdr (assq modulo '((2 . "拾") (3 . "百") (0 . "阡")))))) )
	  ;; 「十万」以上の位でその後も 0 が続くとき。
	  (when (> len 5)
	    (cond ((and (= modulo 2) (eq (nth 1 num) ?0))
		   (setq num (cdr num) len (1- len) char (nth 1 num)) )
		  ((and (= modulo 3) (eq (nth 1 num) ?0) (eq (nth 2 num) ?0))
		   (setq num (nthcdr 2 num) len (- len 2) char (nth 2 num)) )
		  ((and (= modulo 0) (eq (nth 1 num) ?0) (eq (nth 2 num) ?0)
			(eq (nth 3 num) ?0) )
		   (setq num (nthcdr 3 num) len (- len 3) char (nth 3 num)) )))
	  (when (and (memq len '(5 9 13 17)) (not (eq prevchar ?0)))
	    (setq v (concat
		     v
		     (cdr (assq len '((5 . "萬") (9 . "億") (13 . "兆") (17 . "京")))) )))))
      (setq len (1- len) prevchar char num (cdr num)) )
    v ))

(defun skk-num-shogi (num)
  ;; ascii 数字の NUM を将棋で使用される数字表記に変換する。
  ;; 例えば "34" を "３四" に変換する。
  (save-match-data
    (if (and (= (length num) 2)
             (not (string-match "\\.[0-9]" num)) )
        (let ((candidate
               (concat (cdr (assq (aref num 0) skk-num-alist-type1))
                       (cdr (assq (aref num 1) skk-num-alist-type2)) )))
          (if (not (string= candidate ""))
              candidate )))))

(defun skk-num-recompute (num)
  ;; #4 の見出しに対し、skk-henkan-key に代入された数字そのものを再度検索する。
  (let (result)
    ;; with-temp-buffer だと何故上手くゆかない...？ 確定されてしまう。
    ;;(with-temp-buffer
    (save-excursion
      (set-buffer (get-buffer-create " *skk-work*"))
      ;; カレントバッファのバッファローカル変数に影響を及ぼさないよう、ワーキ
      ;; ングバッファへ一旦逃げる
      (let ((skk-current-search-prog-list skk-search-prog-list)
            (skk-henkan-key num)
	    ;; カレントの変換は送りなし (skk-henkan-okurigana と skk-okuri-char は
	    ;; いずれも nil) だが、別バッファ (work バッファ) に入っているので、念
	    ;; のため、nil を入れておく。
            skk-henkan-okurigana skk-okuri-char skk-use-numeric-conversion )
        (while skk-current-search-prog-list
          (setq result (skk-nunion result (skk-search))) )))
    ;; ここで *skk-work* を出て変換を行なっているカレントバッファに戻る
    ;; (バッファローカル値である skk-henkan-list を操作したいため)。
    (setq skk-num-recompute-key num)
    (if result
        (if (null (cdr result));;(= (length result) 1)
            (car result)
          result )
      ;; 変換できなかったら元の数字をそのまま返しておく。
      num )))

;;;###autoload
(defun skk-num-uniq ()
  (if (or (not skk-num-uniq) (null skk-henkan-list))
      nil
    (save-match-data
      (let ((n1 -1) n2 e1 e2 e3
            ;; 1 つでも 2 桁以上の数字があれば、#2 と #3 では uniq しない。
            (type2and3 (> 2 (apply 'max (mapcar 'length skk-num-list))))
            type2 type3 index2 index3 head2 head3 tail2 tail3
            case-fold-search )
        (while (setq n1 (1+ n1) e1 (nth n1 skk-henkan-list))
          ;; cons cell でなければ skk-nunion で処理済みなので、重複はない。
          (if (consp e1)
              ;; (car e1) と equal のものが消えるのだから e1 自身が消えるこ
              ;; とはない。
              (setq skk-henkan-list (delete (car e1) skk-henkan-list)
                    skk-henkan-list (delete (cdr e1) skk-henkan-list) ))
          (if (not (and skk-num-recompute-key (consp e1)))
              nil
            ;; ("#4" . "xxx") を含む候補が skk-henkan-list の中にある。
            (setq n2 -1)
            (while (setq n2 (1+ n2) e2 (nth n2 skk-henkan-list))
              (if (and (not (= n1 n2)) (consp e2)
                       ;; 例えば ("#4" . "一") と ("#2" . "一") が並存してい
                       ;; る場合。
                       (string= (cdr e1) (cdr e2)) )
                  (setq skk-henkan-list (delq e2 skk-henkan-list)) )))
          (if (not type2and3)
              nil
            ;; 1 桁の数字を変換する際に、skk-henkan-list に #2 エントリと #3
            ;; エントリがあれば、#2 もしくは #3 エントリのうち、より後方にある
            ;; ものを消す。
            (setq e3 (if (consp e1) (car e1) e1))
            ;; e3 は "#2" のように数値変換を示す文字列のみとは限らないので、
            ;; member は使えない。
            (cond ((string-match "#2" e3)
                   (setq type2 e1
                         index2 n1
                         head2 (substring e3 0 (match-beginning 0))
                         tail2 (substring e3 (match-end 0)) ))
                  ((string-match "#3" e3)
                   (setq type3 e1
                         index3 n1
                         head3 (substring e3 0 (match-beginning 0))
                         tail3 (substring e3 (match-end 0)) )))))
        (if (and type2and3 type2 type3
                 ;; 数値変換を示す文字列 "#[23]" の前後の文字列も同一のと
                 ;; きのみ uniq を行なう。
                 (string= head2 head3) (string= tail2 tail3))
            (if (> index2 index3)
                ;; "#3" の方が前にある。
                (setq skk-henkan-list (delq type2 skk-henkan-list))
              ;; 変数 type[23] の値は、skk-henkan-list から直接抽出したも
              ;; のだから delete でなく、delq で十分。
              (setq skk-henkan-list (delq type3 skk-henkan-list)) ))))))

;;;###autoload
(defun skk-num-process-user-minibuf-input (key)
  (let (numexp orglen val)
    (if (or (and (string-match "#[012349]" key)
                 (setq numexp key) )
            (and (setq numexp (skk-num-rawnum-exp key))
                 (not (string= key numexp)) ))
        (progn
          (setq orglen (length skk-henkan-list)
                ;; skk-henkan-list の調整は、skk-num-convert の中で行なっ
                ;; てくれる。
                val (skk-num-convert numexp) )
          (if (= (length skk-henkan-list) (1+ orglen))
              ;; #4 で複数の候補に変換できた場合は確定しない。
              (setq skk-kakutei-flag t) ))
      (setq skk-henkan-list (nconc skk-henkan-list (list key))
            skk-kakutei-flag t
            val key ))
    val ))

;;;###autoload
(defun skk-num-initialize ()
  ;; skk-use-numeric-convert 関連の変数を初期化する。
  (setq skk-last-henkan-data
	(put-alist 'num-list skk-num-list skk-last-henkan-data)
	skk-num-list nil
        skk-num-recompute-key nil ))

;;;###autoload
(defun skk-num-henkan-key ()
  ;; type4 の数値再変換が行なわれたときは、数値自身を返し、それ以外の数値変換
  ;; では、skk-henkan-key を返す。
  (or skk-num-recompute-key skk-henkan-key) )

;;;###autoload
(defun skk-num-update-jisyo (noconvword word &optional purge)
  ;; 数字自身を見出し語として辞書のアップデートを行なう。
  (if (and skk-num-recompute-key
           (save-match-data (string-match "#4" noconvword)) )
      (let ((skk-henkan-key skk-num-recompute-key))
	(message "%S" skk-num-recompute-key)
        (skk-update-jisyo word purge) )))

;;;###autoload
(defun skk-num (str)
  ;; 数字を skk-number-style の値に従い変換する。
  ;; skk-current-date のサブルーチン。
  (mapconcat (function
	      (lambda (c)
		(cond ((or (not skk-number-style)
			   (and (numberp skk-number-style)
				(= skk-number-style 0) ))
		       (char-to-string c) )
		      ((or (eq skk-number-style t)
			   (and (numberp skk-number-style)
				(= skk-number-style 1) ))
		       (cdr (assq c skk-num-alist-type1)) )
		      (t (cdr (assq c skk-num-alist-type2))) )))
	     str "" ))

(run-hooks 'skk-num-load-hook)

(provide 'skk-num)
;;; Local Variables:
;;; End:
;;; skk-num.el ends here
