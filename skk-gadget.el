;; skk-gadget.el -- 実行変換のためのプログラム
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-gadget.el,v 1.6 2000/10/30 22:10:15 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/10/30 22:10:15 $

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
;;
;; プログラム実行変換とは
;; ======================
;; 送り仮名のない辞書の変換の候補に Emacs Lisp のコードが書いてあれば、SKK
;; はそのコードを Lisp のプログラムとして実行し、その結果の文字列を画面に挿
;; 入する。例えば、辞書に
;;
;;         now /(current-time-string)/
;;
;; という行があるとき、`/now ' とタイプすれば画面には現在の時刻が表示され、
;; `▼Fri Apr 10 11:41:43 1992' のようになる。
;;
;; ここで使える Lisp のコードは改行を含んでいないものに限られる。またこのコー
;; ドは結果として文字列を返すものでなければならない。
;;
;; このファイルは実行変換プログラムを集めたものである。
;;
;; skk-gadget.el の `gadget' は「上手く工夫した道具」の意味。「色々飛び出す
;; 気のきいたおもちゃ箱」というような意味で名付けられた。
;; 余談だが、X Window で使用される `Widget' という言葉は、`window'+`gadget'
;; から作られた造語らしい。

;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars) (require 'static))

;; -- programs
;;;###autoload
(defun skk-current-date (&optional and-time)
  ;; 現在の日時を日本語で返す。skk-today と skk-clock のサブルーチン。
  ;; オプショナル引数の AND-TIME を指定すると、時間も返す。
  (let* ((str (current-time-string))
         (year (if skk-date-ad
                   (skk-num (substring str 20 24))
                 (let ((y (- (string-to-number (substring str 20 24)) 1988)))
                   (if (= y 1) "元" (skk-num (number-to-string y))))))
         (month (skk-num (cdr (assoc (substring str 4 7) skk-month-alist))))
         (day (substring str 8 10))
         (day-of-week (cdr (assoc (substring str 0 3) skk-week-alist)))
         hour minute second)
    (if (eq (aref day 0) ?\040) ; SPC
	(setq day (substring day 1)))
    (setq day (skk-num day))
    (concat (if skk-date-ad "" "平成") year "年"
            month "月" day "日" "\(" day-of-week "\)"
            (if and-time
                (progn
                  (setq hour (skk-num (substring str 11 13))
                        minute (skk-num (substring str 14 16))
                        second (skk-num (substring str 17 19)))
                  (concat " " hour "時" minute "分" second "秒"))))))

;;;###autoload
(defun skk-today (&optional and-time)
  "現在の日時を日本語表記で返す。
オプショナル引数の AND-TIME を指定すると、日付に時間を加える。
skk-date-ad と skk-number-style によって表示方法のカスタマイズが可能。"
  (interactive "*P")
  (if (interactive-p)
      (insert (skk-today and-time))
    (skk-current-date and-time)))

;;;###autoload
(defun skk-clock (&optional kakutei-when-quit time-signal)
  "デジタル時計をミニバッファに表示する。
quit するとその時点の日時を候補として挿入する。
quit したときに起動してからの経過時間をミニバッファに表示する。
interactive に起動する他、\"clock /(skk-clock)/\" などのエントリを SKK の辞書
に加え、\"/clock\"+ SPC で変換することによっても起動可。C-g で止まる。
実行変換で起動した場合は、C-g した時点の時点の日時を挿入する。
オプショナル引数の KAKUTEI-WHEN-QUIT が non-nil であれば C-g したときに確
定する。
オプショナル引数の TIME-SIGNAL が non-nil であれば、NTT の時報風に ding する。
それぞれ、\"clock /(skk-clock nil t)/\" のようなエントリを辞書に挿入すれば良い。
skk-date-ad と skk-number-style によって表示方法のカスタマイズが可能。"
  (interactive "*")
  (let ((start (current-time))
        end mes expr1 expr2 sec snd)
    (cond ((or (not skk-number-style)
               (eq skk-number-style 0))
           (setq expr1 "[789]秒"
                 expr2 "0秒"))
          ((or (eq skk-number-style t)
               ;; skk-number-style に 数字と t 以外の non-nil 値を入れている場
               ;; 合、= を使うと Wrong type argument: number-or-marker-p, xxxx
               ;; になってしまう。
               (eq skk-number-style 1))
           (setq expr1 "[７８９]秒"
                 expr2 "０秒"))
          (t
           (setq expr1 "[七八九]秒"
                 expr2 "〇秒")))
    ;;
    (static-when (eq skk-emacs-type 'xemacs)
      ;; XEmacs で sound がロードされているかどうか。
      (when (setq snd (and (boundp 'sound-alist)
			   (eq t (catch 'tag
				   (mapc
				    (function
				     (lambda (list)
				       (and
					(eq 'drum
					    (cadr (memq :sound list)))
					(throw 'tag t))))
				    sound-alist)))))
	;;
	(unless (assq 'clink sound-alist)
	  (load-sound-file "clink" 'clink))))
    ;;
    (save-match-data
      (condition-case nil
          (let (case-fold-search
                inhibit-quit visible-bell
                skk-mode skk-latin-mode skk-j-mode skk-abbrev-mode
		skk-jisx0208-latin-mode)
            (while (not quit-flag)
              (setq mes (skk-current-date t)
		    sec 0)
	      (message "%s    Hit any key to quit" mes)
              (if time-signal
                  (if (string-match expr1 mes)
                      ;; [7890] のように正規表現を使わず、7 だけで全てのマシンが
                      ;; 着いてゆけば良いのだが...。丁度この関数実行時に Garbage
                      ;; collection が呼ばれても表示される数字が飛ぶ場合がある。
		      (static-if (eq skk-emacs-type 'xemacs)
			  ;; いい音がないなぁ...
			  (ding nil 'drum)
			(ding))
                    (if (string-match expr2 mes)
                        ;; 0 だけ「ポ〜ン」といきたいところですが、マシンによっ
                        ;; て差がある。
                        ;; 386SX 25Mhz + Mule-2.x だと「ピッ、ピッ」という感じ。
                        ;; 付いてゆくのが非常に辛い。68LC040 33Mhz + NEmacs だと
                        ;; 「ピピッ」となり、音のタイミングは良いのだが、とき
                        ;; どき 1 秒分ついていけなくなる。Pentium 90Mhz +
                        ;; Mule-2.xだと「ピッ」という単音になってしまう... (;_;)。
			(static-cond
			 ((eq skk-emacs-type 'xemacs)
			  (if snd
			      ;; ちょっともたつく ?
			      (ding nil 'clink)
			    (ding)
			    (unless (sit-for (setq sec
						   (+ sec
						      (/ (float 1) (float 6))))
					     'nodisplay)
			      (next-command-event)
			      (signal 'quit nil))
			    (ding)))
			 ((featurep 'lisp-float-type)
			  (ding)
			  (unless (sit-for (setq sec
						 (+ sec
						    (/ (float 1) (float 6))))
					   nil
					   'nodisplay)
			    (next-command-event)
			    (signal 'quit nil))
			  (ding))
			 (t
			  ;; Emacs 18
			  (ding)
			  (ding))))))
	      (unless (static-cond
		       ((memq skk-emacs-type '(nemacs mule1 xemacs))
			(sit-for (- 1 sec) 'nodisplay))
		       (t
			(sit-for (- 1 sec) nil 'nodisplay)))
		(next-command-event)
		(signal 'quit nil))))
        (quit
         (prog2
             (setq end (current-time))
             (skk-current-date t)
           (if kakutei-when-quit
               (setq skk-kakutei-flag t))
           (message "経過時間: %s 秒" (skk-time-difference start end))))))))

;;;###autoload
(defun skk-ad-to-gengo (&optional fstr lstr)
  ;; 西暦を元号に変換する。オプション引数の fstr が指定されていれば、年号と
  ;; 数字の間に、lstr が指定されていれば、数字の末尾に、それぞれの文字列を連結
  ;; する。
  ;; 辞書見出し例;
  ;; せいれき#ねん /(skk-ad-to-gengo nil "年")/(skk-ad-to-gengo " " " 年")/
  (let ((ad (string-to-number (car skk-num-list))))
    (concat (cond ((>= 1866 ad)
                   (skk-error "分りません" "Unkown year"))
                  ((>= 1911 ad)
                   (concat "明治" fstr (number-to-string (- ad 1867))))
                  ((>= 1925 ad)
                   (concat "大正" fstr (number-to-string (- ad 1911))))
                  ((>= 1988 ad)
                   (concat "昭和" fstr (number-to-string (- ad 1925))))
                  (t (concat "平成" fstr (number-to-string (- ad 1988)))))
            lstr)))

;;;###autoload
(defun skk-gengo-to-ad (&optional string)
  ;; 元号を西暦に変換する。オプション引数の string が指定されていれば、
  ;; その文字列を末尾に連結する。
  ;; 辞書見出し例;
  ;; しょうわ#ねん /(skk-gengo-to-ad "年")/(skk-gengo-to-ad " 年")/
  (save-match-data
    (let ((num (car skk-num-list))
          gengo)
      (string-match num skk-henkan-key)
      (setq gengo (substring skk-henkan-key 0 (match-beginning 0))
            num (string-to-number num))
      (concat (number-to-string
               (+ num
                  (cond ((eq num 0)
                         (skk-error "0 年はあり得ない"
                                    "Cannot convert 0 year"))
                        ((string= gengo "へいせい") 1988)
                        ((string= gengo "しょうわ")
                         (if (> 64 num)
                             1925
                           (skk-error "昭和は 63 年までです"
                                      "The last year of Showa is 63")))
                        ((string= gengo "たいしょう")
                         (if (> 15 num)
                             1911
                           (skk-error "大正は、14 年までです"
                                      "The last year of Taisyo is 14")))
                        ((string= gengo "めいじ")
                         (if (> 45 num)
                             1867
                           (skk-error "明治は、44 年までです"
                                      "The last year of Meiji is 44")))
                        (t (skk-error "判別不能な元号です！"
                                      "Unknown Gengo!")))))
              string))))

;(defun skk-calc (operator)
;  ;; 2 つの引数を取って operator の計算をする。
;  ;; 注意: '/ は引数として渡せないので (defalias 'div '/) などとし、別の形で
;  ;; skk-calc に渡す。
;  ;; 辞書見出し例; #*# /(skk-calc '*)/
;  (number-to-string
;   (funcall operator (string-to-number (car skk-num-list))
;            (string-to-number (nth 1 skk-num-list)))))

;;;###autoload
(defun skk-calc (operator)
  ;; 2 つの引数を取って operator の計算をする。
  ;; 注意: '/ は引数として渡せないので (defalias 'div '/) などとし、別の形で
  ;; skk-calc に渡す。
  ;; 辞書見出し例; #*# /(skk-calc '*)/
  (number-to-string (apply operator (mapcar 'string-to-number skk-num-list))))

;;;###autoload
(defun skk-plus ()
  ;; 辞書見出し例; #+#+# /(skk-plus)/
  (number-to-string
   (apply '+ (mapcar 'string-to-number skk-num-list))))

;;;###autoload
(defun skk-minus ()
  (number-to-string
   (apply '- (mapcar 'string-to-number skk-num-list))))

;;;###autoload
(defun skk-times ()
  (number-to-string
   (apply '* (mapcar 'string-to-number skk-num-list))))

;;;###autoload
(defun skk-ignore-dic-word (&rest no-show-list)
  ;; 共用辞書に登録されている、違っている/気に入らない変換を出さないようにす
  ;; る。
  ;; 辞書見出し例;
  ;;   るすばん /留守番/(skk-ignore-dic-word "留守電")/
  ;;   かくてい /(skk-ignore-dic-word "確定")/
  (let (new-word save-okurigana)
    ;; skk-ignore-dic-word 自身のエントリを消す。消すべき候補は
    ;; skk-henkan-list から直接抽出しているので delete ではなく delq で十分。
    (setq skk-henkan-list (delq (nth skk-henkan-count skk-henkan-list)
                                skk-henkan-list))
    ;; 全候補を skk-henkan-list に入れる。
    (while skk-current-search-prog-list
      (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search))))
    ;; 不要な候補を捨てる。
    (while no-show-list
      (setq skk-henkan-list (delete (car no-show-list) skk-henkan-list)
            no-show-list (cdr no-show-list)))
    ;; カレントの候補 (skk-ignore-dic-word 自身のエントリ) を消したので、
    ;; skk-henkan-count は次の候補を指している。
    (setq new-word (or (nth skk-henkan-count skk-henkan-list)
                       (progn (setq save-okurigana skk-okuri-char)
                              (skk-henkan-in-minibuff))))
    ;; 候補がないとき。
    (if (not new-word)
        ;; 空文字列が登録されたら辞書登録の前の状態に戻す。
        ;; (nth -1 '(A B C)) は、A を返すので、n が負の数でないことをチェック
        ;; しておく必要がある。
        (if (> skk-henkan-count 0)
            (setq skk-henkan-count (- skk-henkan-count 1)
                  new-word (nth skk-henkan-count skk-henkan-list))
          ;; (1- skk-henkan-count) == -1 になる。▽モードに戻す。
          (setq new-word (if save-okurigana
                             (substring skk-henkan-key 0
                                        (1- (length skk-henkan-key)))
                             skk-henkan-key)
                skk-henkan-count -1
                ;; 下記の変数は、skk-henkan-in-minibuff の中で調整される。
                ;; skk-henkan-active nil
                ;; skk-okuri-char nil
                ;; skk-henkan-okurigana nil
                 )
          (if skk-use-face
              (setq skk-insert-new-word-function
                    'skk-henkan-face-off-and-remove-itself))))
    new-word))

;;;###autoload
(defun skk-henkan-face-off-and-remove-itself ()
  ;; skk-insert-new-word-function にセットするための関数。カレントバッファの
  ;; 変換部分が Overlay の face 属性によって表示が変更されているのを戻し、その
  ;; 後自分自身を skk-insert-new-word-function から取り除く自爆関数。
  (skk-henkan-face-off)
  (setq skk-insert-new-word-function nil))

(run-hooks 'skk-gadget-load-hook)

(require 'product)
(product-provide (provide 'skk-gadget) (require 'skk-version))
;;; skk-gadget.el ends here
