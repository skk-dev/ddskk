;;; skk-correct.el --- correct key word for conversion. -*- coding: iso-2022-jp -*-
;; Copyright (C) 1999 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-correct.el,v 1.8 2010/08/02 15:21:05 skk-cvs Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2010/08/02 15:21:05 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary

;; 辞書中にない読みのブレた見出し語で変換した際に、内部で skk-correct-table
;; を参照して正しい見出し語に置き換え候補を検索するプログラムです。
;; skk-correct-search の引数に、ブレ補正後に検索したい辞書を、
;; skk-search-prog-list と同じ要領でリストで表し、検索プログラムとともに記載
;; して下さい。例えばこんな感じです。
;;
;;   (skk-correct-search
;;    '((skk-search-jisyo-file skk-jisyo 0 t)
;;      (skk-search-server skk-aux-large-jisyo 10000)
;;      (skk-okuri-search)))
;;
;;     * 上記の例では、補正した見出し語に対し、個人辞書、サーバー、自動送り
;;       処理して個人辞書という検索を行ないますが、どこかで補正後の見出し語
;;       に対する候補が見つかればそれ以上は検索を行ないません。
;;
;; これを更に skk-search-prog-list の中の適当な個所に入れましょう。下記のよ
;; うに個人辞書検索直後に、一度見出し語を補正して個人辞書を検索し直し、サー
;; バー検索後に見つからなかったら再度補正してサーバーを検索する、というのも
;; 一案です。あるいは、個人の癖をもろに反映している個人辞書のブレ補正は止め
;; て、サーバーのみ補正後検索をする、というのも良いでしょう。
;;
;;   (setq skk-search-prog-list
;;         '((skk-search-jisyo-file skk-jisyo 0 t)
;;	     (skk-correct-search '((skk-search-jisyo-file skk-jisyo 0 t)))
;;	     (skk-search-server skk-aux-large-jisyo 10000)
;;           (skk-okuri-search)
;;	     (skk-correct-search
;;	      '((skk-search-server skk-aux-large-jisyo 10000)
;;	        (skk-okuri-search)))
;;	     ))
;;
;; 現在のところ、skk-correct-table の各要素を最初から順に取り出し、ブレた見
;; 出し語がないかどうかを調べて、ブレが見つかったらそれ以上 skk-correct-table
;; の後半部分は見ない仕様になっています。
;; とりあえず Naoki Wakamatsu <m5032106@u-aizu.ac.jp> さんが Message-Id:
;;  <200001260732.QAA00868@ring.etl.go.jp> で列挙してくださった読みのブレを
;; 単純に sort して、2 文字のもの, 1 文字のものという順で skk-correct-table
;; に収めていますが、上記のように何か一つブレが見つかったらそれ以上はテーブル
;; を見ていないので、このテーブルのブレの優先順位は検討した方が良いでしょう。
;;
;; 今後の改良のアイディアの一つとしては、変換条件に応じて見るブレと見ないブレ
;; を作るのも良いかもしれません。例えば、このブレは送りあり変換のときだけしか
;; 検索しない、などというものです。この辺りは実際に使ってみてご意見をお聞かせ
;; 下さい。
;;
;; なお、変換時に当初手で入力したブレた見出し語は、そのまま個人辞書に取り込ま
;; れますが、これは仕様です。何故なら、確定時には、確定された候補が、見出し語
;; のブレを補正して見つけた候補かどうかを確認する術が現在のところ提供されてい
;; ないからです。

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

(defgroup skk-correct nil "SKK correct related customization."
  :prefix "skk-correct-"
  :group 'skk)

(defvar skk-correct-table
  '(
    ;; 2 chars
    ("おー" . "おう")			; 大きい
    ("おー" . "おお")
    ("おう" . "おお")
    ("とう" . "とお")			; 通る
    ("どう" . "どお")
    ("やう" . "よう")			; やうやk /漸/ -> ようやk /漸/
    ("らう" . "ろう")			; らうs /労/ -> ろうs /労/
    ;; 1 char
    ("い" . "ゆ")			; いきどm /行き止/ -> ゆきどm /行き止/
    ("い" . "よ")			; いi /良/ -> よi /良/
    ("お" . "う")			; ほおt /放/ -> ほうt /放/
    ("お" . "ほ")			; よそおu /裝/ -> よそほu /裝/
    ("お" . "を")			; くちおs /口惜/ -> くちをs /口惜/
    ("か" . "が")			; まぬかr /免/ -> まぬがr /免/
    ("ざ" . "さ")			; はだざむk /肌寒/ -> はださむi /肌寒/
    ("じ" . "ぢ")			; みじかn /身近/ -> みぢかn /身近/
    ("そ" . "ぞ")			; みぎそろe /右揃/ -> みぎぞろe /右揃/
    ("た" . "だ")			; やくたt /役立/ -> やくだt /役立/
    ("づ" . "ず")			; わづらw /煩/ -> わずらw /煩/ ; むづかs /難/ -> むずかs /難/
    ("と" . "ど")			; もとr /戻/ -> もどr /戻/
    ("は" . "わ")			; めざはr /目障/ -> めざわr /目障/
    ("ば" . "は")			; はらi /払/  -> ばらi /払/
    ("ぱ" . "は")			; ぱt /張/ -> はt /張/
    ("ひ" . "い")			; ふがひなi /腑甲斐無/ -> ふがいなi /腑甲斐無/
    ("ひ" . "び")			; りょうひらk /両開/ -> りょうびらk /両開/
    ("ふ" . "ぶ")			; ふかk /深/ -> ぶかk /深/
    ("ぶ" . "ふ")			; けぶかi /毛深/ -> けふかi /毛深/
    ("む" . "ん")			; やむごとなs /止事無/ -> やんごとなs /止事無/
    ("ゆ" . "い")			; ゆきどm /行き止/ -> いきどm /行き止/
    ("よ" . "い")			; よi /良/ -> いi /良/
    ("ゐ" . "い")			; ゐr /居/ -> いr /居/
    )
  "*見出し語変換のためのテーブル。
各要素のデータ構造は、\(\"読みがぶれた見出し語\" . \"正しい見出し語\"\)。")

;; internal variable
(defvar skk-correct-current-table nil)

;; functions.
;;;###autoload
(defun skk-correct-search (search-method-list)
  (let ((henkan-key skk-henkan-key)
	search-list skk-henkan-key v)
    (setq skk-correct-current-table skk-correct-table)
    (while (and (not v) (setq search-list search-method-list
			      skk-henkan-key (skk-correct henkan-key)))
      (while (and search-list (not (setq v (eval (car search-list)))))
	(setq search-list (cdr search-list))))
    v))

(defun skk-correct (string)
  ;; STRING 中にブレがあったらその部分を正しい文字列に置き換えて返す。
  (let (v)
    (save-match-data
      (while (and (not v) skk-correct-current-table)
	(if (string-match (car (car skk-correct-current-table)) string)
	    (setq v (concat (substring string 0 (match-beginning 0))
			    (cdr (car skk-correct-current-table))
			    (substring string (match-end 0)))))
	(setq skk-correct-current-table (cdr skk-correct-current-table)))
      v)))

(provide 'skk-correct)
;;; Local Variables:
;;; End:
;;; skk-correct.el ends here
