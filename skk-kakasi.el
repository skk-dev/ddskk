;;; skk-kakasi.el --- KAKASI 関連プログラム
;; Copyright (C) 1996, 1998, 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-kakasi.el,v 1.7 2000/09/14 08:40:51 akiho Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/09/14 08:40:51 $

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

;; skk-kakasi.el は KAKASI を SKK の中から使うインターフェイスです。KAKASI は、
;; 高橋裕信さん <takahasi@tiny.or.jp> による、「漢字かなまじり文をひらがな文や
;; ローマ字文に変換することを目的として作成したプログラムと辞書の総称」です。
;; 私自身がニュースやメールを読んでいて、日常読みが分らなくて恥ずかしい思いを
;; することが多いので、逆引きをしたくて作りました。
;;
;; KAKASI は、1996 年 4 月 25 日現在、
;; sunsite.sut.ac.jp:/pub/asia-info/japanese-src/packages/kakasi-2.2.5.tar.gz
;; sunsite.sut.ac.jp:/pub/asia-info/japanese-src/packages/kakasidict.940620.gz
;; にあり anonymous ftp で入手できます。
;;
;; 素晴しいプログラム KAKASI をお作りになった高橋さんと、KAKASI を anonymous
;; ftp で入手可能としている sunsite.sut.ac.jp に感謝いたします。

;;; Code:
(require 'skk)
(require 'skk-foreword)
;; APEL
(require 'path-util)

;;;###autoload
(defgroup skk-kakasi nil "SKK kakasi related customization."
  :prefix "skk-"
  :group 'skk )

;;;;  VARIABLES

;; --- user variable

(defcustom skk-use-kakasi (exec-installed-p "kakasi")
  "*Non-nil であれば KAKASI を使った変換を行なう。" 
  :type 'boolean
  :group 'skk-kakasi )

(defcustom skk-kakasi-command (exec-installed-p "kakasi")
  "*KAKASI コマンド本体。"
  :type 'file
  :group 'skk-kakasi )

(defcustom skk-romaji-*-by-hepburn t
  "*Non-nil であれば KAKASI を使ったローマ字への変換様式にヘボン式を用いる。
例えば、
  \"し\" -> \"shi\"

nil であれば、訓令式 \"(「日本式」とも言うようだ)\" を用いる。
例えば、
   \"し\" -> \"si\"

昭和 29 年 12 月 9 日付内閣告示第一号によれば、原則的に訓令式 \"(日本式)\" を
用いるかのように記載されているが、今日一般的な記載方法は、むしろ、ヘボン式であ
るように思う。"
  :type 'boolean
  :group 'skk-kakasi )

(defcustom skk-kakasi-load-hook nil
  "*skk-kakasi.el がロードされたときのフック。"
  :type 'hook
  :group 'skk-kakasi )

(if (fboundp 'modify-coding-system-alist)
    (let ((euc (cdr (assoc "euc" skk-coding-system-alist))))
      (modify-coding-system-alist 'process "kakasi" (cons euc euc)) ))

;;;; FUNCTIONS
;;;###autoload
(defun skk-gyakubiki-region (start end &optional all)
  "リージョンの漢字、送り仮名を全てひらがなに変換する。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}"
  (interactive "*r\nP")
  (let ((str (skk-gyakubiki-1 start end all)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str) )
  (and (featurep 'skk-cursor)
   (skk-set-cursor-properly)) )

;;;###autoload
(defun skk-gyakubiki-message (start end &optional all)
  "リージョンの漢字、送り仮名を全てひらがなに変換後、エコーする。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}"
  (interactive "r\nP")
  (let ((str (skk-gyakubiki-1 start end all)))
    (save-match-data
      (if (string-match "^[ 　\t]+" str)
          ;; 先頭の空白を取り除く。
          (setq str (substring str (match-end 0))) ))
    (message "%s" str)
    (and (featurep 'skk-cursor)
     (skk-set-cursor-properly)) ))


;;;###autoload
(defun skk-gyakubiki-katakana-region (start end &optional all)
  "リージョンの漢字、送り仮名を全てカタカナに変換する。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}"
  (interactive "*r\P")
  (let ((str (skk-gyakubiki-1 start end all 'katakana)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str) )
  (and (featurep 'skk-cursor)
   (skk-set-cursor-properly)) )

;;;###autoload
(defun skk-gyakubiki-katakana-message (start end &optional all)
  "リージョンの漢字、送り仮名を全てカタカナに変換後、エコーする。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}"
  (interactive "r\nP")
  (let ((str (skk-gyakubiki-1 start end all 'katakana)))
    (save-match-data
      (if (string-match "^[ 　\t]+" str)
          ;; 先頭の空白を取り除く。
          (setq str (substring str (match-end 0))) ))
    (message "%s" str)
    (and (featurep 'skk-cursor)
     (skk-set-cursor-properly)) ))

(defun skk-gyakubiki-1 (start end all &optional katakana)
  ;; skk-gyakubiki-* のサブルーチン。
  ;; オプショナル引数の KATAKANA が non-nil であれば、カタカナへ変換する。
  (let ((arg (if katakana '("-JK") '("-JH"))))
    (if skk-allow-spaces-newlines-and-tabs
        (setq arg (cons "-c" arg)) )
    (if all
        (setq arg (cons "-p" arg)) )
    (skk-kakasi-region start end arg)) )

;;;###autoload
(defun skk-hurigana-region (start end &optional all)
  "リージョンの漢字に全てふりがなを付ける。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[へんかんまえ]の漢字[かんじ]の脇[わき]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}"
  (interactive "*r\nP")
  (let ((str (skk-hurigana-1 start end all)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str) )
  (and (featurep 'skk-cursor)
   (skk-set-cursor-properly)) )

;;;###autoload
(defun skk-hurigana-message (start end &optional all)
  "リージョンの漢字に全てふりがなを付け、エコーする。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[へんかんまえ]の漢字[かんじ]の脇[わき]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}"
  (interactive "r\nP")
  (message "%s" (skk-hurigana-1 start end all))
  (and (featurep 'skk-cursor)
   (skk-set-cursor-properly)) )

;;;###autoload
(defun skk-hurigana-katakana-region (start end &optional all)
  "リージョンの漢字に全てフリガナを付ける。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[ヘンカンマエ]の漢字[カンジ]の脇[ワキ]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}"
  (interactive "*r\nP")
  (let ((str (skk-hurigana-1 start end all 'katakana)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str) )
  (and (featurep 'skk-cursor)
   (skk-set-cursor-properly)) )

;;;###autoload
(defun skk-hurigana-katakana-message (start end &optional all)
  "リージョンの漢字に全てフリガナを付け、エコーする。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[ヘンカンマエ]の漢字[カンジ]の脇[ワキ]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}"
  (interactive "r\nP")
  (message "%s" (skk-hurigana-1 start end all 'katakana))
  (and (featurep 'skk-cursor)
   (skk-set-cursor-properly)) )

(defun skk-hurigana-1 (start end all &optional katakana)
  ;; skk-hurigana-* のサブルーチン。
  ;; オプショナル引数の KATAKANA が non-nil であれば、カタカナへ変換する。
  (let ((arg (if katakana '("-JK" "-f") '("-JH" "-f"))))
    (if skk-allow-spaces-newlines-and-tabs
        (setq arg (cons "-c" arg)) )
    (if all
        (setq arg (cons "-p" arg)) )
    (skk-kakasi-region start end arg)) )

;;;###autoload
(defun skk-romaji-region (start end)
  "リージョンの漢字、ひらがな、カタカナ、全英文字を全てローマ字に変換する。
変換には、ヘボン式を用いる。
例えば、
   \"漢字かな混じり文をローマ字に変換\"
    -> \"  kan'zi  kana  ma  ziri  bun'  woro-ma  zi ni hen'kan' \"

skk-romaji-*-by-hepburn が nil であれば、ローマ字への変換様式を訓令式に変更す
る。例えば、\"し\" はヘボン式では \"shi\" だが、訓令式では \"si\" となる。"
  (interactive "*r")
  (let ((arg '("-Ha" "-Ka" "-Ja" "-Ea" "-ka" "-s"))
        str )
    (if skk-allow-spaces-newlines-and-tabs
        (setq arg (cons "-c" arg)) )
    (if (not skk-romaji-*-by-hepburn)
        (setq arg (cons "-rk" arg)) )
    (setq str (skk-kakasi-region start end arg))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str) )
  (and (featurep 'skk-cursor)
   (skk-set-cursor-properly)) )

;;;###autoload
(defun skk-romaji-message (start end)
  "リージョンの漢字、ひらがな、カタカナ、全英文字を全てローマ字に変換し、エコーする。
変換には、ヘボン式を用いる。
例えば、
   \"漢字かな混じり文をローマ字に変換\"
    -> \"  kan'zi  kana  ma  ziri  bun'  woro-ma  zi ni hen'kan' \"

skk-romaji-*-by-hepburn が nil であれば、ローマ字への変換様式を訓令式に変更す
る。例えば、\"し\" はヘボン式では \"shi\" だが、訓令式では \"si\" となる。"
  (interactive "r")
  (let ((arg '("-Ha" "-Ka" "-Ja" "-Ea" "-ka" "-s")))
    (if skk-allow-spaces-newlines-and-tabs
        (setq arg (cons "-c" arg)) )
    (if (not skk-romaji-*-by-hepburn)
        (setq arg (cons "-rk" arg)) )
    (message "%s" (skk-kakasi-region start end arg))
    (and (featurep 'skk-cursor)
     (skk-set-cursor-properly)) ))

(defun skk-kakasi-region (start end arglist)
  ;; START と END 間のリージョンに対し kakasi コマンドを適用する。ARGLIST を
  ;; kakasi の引数として渡す。kakasi の出力を返す。
  (or skk-use-kakasi skk-kakasi-command
      (skk-error "KAKASI がインストールされていないか、使用しない設定になっています。"
                 "KAKASI was not installed, or skk-use-kakasi is nil" ) )
  (let ((str (buffer-substring-no-properties start end)))
        ;; 頻度情報を使って何かおもしろい使い方ができるかな？  現状では使って
        ;; いない。
        ;;(hindo-file (skk-make-temp-file "skkKKS"))
    (with-temp-buffer
      ;; current buffer が read-only のときに current buffer で call-process
      ;; を呼ぶと destination buffer を別に指定していてもエラーになるので、リー
      ;; ジョンの文字列をワークバッファに退避する。
      (insert str)
      (if (and (eq (apply 'call-process-region (point-min) (point)
			  skk-kakasi-command
                          ;; kakasi-2.2.5.hindo.diff が当っていると標準エラー
                          ;; 出力に頻度情報が出力される。
                          'delete-original-text
                          ;;(list t hindo-file)
                          '(t nil)
                          nil arglist )
                   0 )
               (> (buffer-size) 0) )
          (buffer-string)
        (skk-error "変換できません" "Cannot convert!") ))))

(run-hooks 'skk-kakasi-load-hook)
(provide 'skk-kakasi)
;;; skk-kakasi.el ends here
