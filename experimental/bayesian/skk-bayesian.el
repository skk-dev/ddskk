;; skk-bayesian.el -- Bayesian estimation for SKK
;; Copyright (C) 2004 Kenichi Kurihara <kenichi_kurihara@nifty.com>

;; Author: Kenichi Kurihara <kenichi_kurihara@nifty.com>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-bayesian.el,v 1.10 2004/12/20 09:33:56 skk-cvs Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2004/12/20 09:33:56 $

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
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; skk-study が直前の履歴のみを使用するので、これを拡張したいと思ったの
;; が全ての動機です。SKK とそのコミュニティに感謝します。
;;
;;
;; <動作>
;; 例: (skk-bayesian-context-len = 5 の時)
;; 「その服を、」の後に、きr を変換する状況において、
;; entry が、("切" "着" "斬") である状況を考える。
;; この enrty を以下の確率を計算することで、ソートする。
;;
;; Prob( word="切" | p_1="、", p_2="を", p_3="服", p_4="の", p_5="そ" )
;; Prob( word="着" | p_1="、", p_2="を", p_3="服", p_4="の", p_5="そ" )
;; Prob( word="斬" | p_1="、", p_2="を", p_3="服", p_4="の", p_5="そ" )
;;
;; 学習すべきパラメータの数を減らすため、この確率モデルを以下のような
;; 混合分布であると仮定する。
;;
;; Prob( word="切" | p_1="、", p_2="を", p_3="服", p_4="の", p_5="そ" )
;;   ~= \sum_{i=1}^5 w_i * Prob( word="切" | p_i )
;;
;; ただし、w_i は混合分布の重みである。
;;
;;
;; <課題>
;; 1. bskk が単純に作られているので、変換の履歴が大きくなった時に、動作
;;    速度と必要なメモリの量が心配。
;; 2. 混合分布の重み w_i は現在、w_1, w_2, ..., w_n に対して、
;;    w_i : w_j = (n-i) : (n-j)
;;    となるように値を決めている。本来、いずれも隠れ変数として、EMアル
;;    ゴリズム, VBA 等により学習すべきかもしれない。
;; 3. skk-bayesian-context-len は変数にしているので、ユーザが決定できる
;;    が、理想的にはモデルの推定問題ととらえて、学習データから決定すべ
;;    きだろう。また、ある程度、学習した後に skk-bayesian-context-lenを
;;    大きい値に変更するのは、推定に悪影響を与えそう。
;; 4. 2と3に重なるが、著作権の心配をしなくてもよいコーパスから、学習を行い
;;    skk-bayesian-context-len と 混合分布の重みを決定したい。
;; 5. bskk とのプロトコルが素人臭い。
;;
;;
;; <使い方>
;; ~/.skk に、(require 'skk-bayesian) と書いて下さい。
;; skk-study との併用は機能が重なるので、お勧めできません。
;;
;; また、bskk は、サーバかサブプロセスとして使用します。
;; *サブプロセス
;; サブプロセスとして使用するには、bskk をパスの通った場所に置くだけです。
;; 問題は、いくつも emacs を起動すると ~/.skk-bayesian は最後に更新した
;; emacs に依るので、他の emacs での学習データは保存されません。
;; *サーバ
;; bskk をサーバとして使用するには、skk-bayesian.el が emacs から読み込
;; まれる前に、
;; % bskk -f ~/.skk-bayesian -s
;; として、立ち上げておく必要があります。
;; サーバを終了させる方法は、kill -TERM です。-TERM で終了させる際には、
;; 終了の前に bskk は履歴を保存します。
;;
;; skk-bayesian.el は、emacs が終了する前に bskk に対して履歴を保存する
;; ように指示をします。

;;; Code:

(require 'skk-vars)
(require 'skk-macs)

(defvar skk-bayesian-prefer-server nil
  "non-nil ならば、`skk-bayesian-host'の`skk-bayesian-port'に接続する。
そうでなければ、bskk をサブプロセスとして立ち上げる。")
(defvar skk-bayesian-port 51178
  "*`skk-bayesian-prefer-server'が non-nil の時に`skk-bayesian-host'に接続するポート番号")
(defvar skk-bayesian-host "localhost"
  "*`skk-bayesian-prefer-server'が non-nil の時に接続するホスト")
(defvar skk-bayesian-coding-system 'euc-jp)
(defvar skk-bayesian-context-len 20 "*学習や予測に使用する、変換語の直前の文字数")
(defvar skk-bayesian-last-context nil "*確定語の直前の文字列")
(defvar skk-bayesian-last-context-pending nil 
  "*pendingされた確定語の直前の文字列")
(defvar skk-bayesian-history-file "~/.skk-bayesian" "*history file")
(defvar skk-bayesian-debug nil "*デバッグ用のメッセージを表示")
(defvar skk-bayesian-number-of-command-after-kakutei 0 "*確定後のコマンドの回数")
(defvar skk-bayesian-last-kakutei-word nil "直前の確定語")
(defvar skk-bayesian-last-buffer nil "直前の確定が行われた buffer")
(defvar skk-bayesian-last-henkan-point nil "直前の確定が行われた marker")
(defvar skk-bayesian-last-okurigana nil "直前の確定の送り仮名")
(defvar skk-bayesian-add-to-history-pending nil "確定語の履歴登録待ちかどうか")
(defvar skk-bayesian-max-commands-to-wait-for 5
  "*確定の後に`skk-bayesian-max-commands-to-wait-for'回のコマンド
のうちに確定語(送り仮名を含む)が変更されなければ、その確定語を保存
する。`skk-bayesian-max-commands-to-wait-for'が0以下ならば、確定後、
直ちに履歴に保存する。")

(defconst skk-bayesian-command-sort "#sort\n")
(defconst skk-bayesian-command-add "#add\n")
(defconst skk-bayesian-command-save "#save\n")
(defvar skk-bayesian-process nil)

(defmacro skk-bayesian-debug-message (STRING &rest ARGS)
  `(if skk-bayesian-debug
       (message ,STRING ,@ARGS)))

(defsubst skk-bayesian-process-live-p ()
  "`skk-bayesian-process' が non-nil かつそのプロセスが実行中なら t を返す。 "
  (and skk-bayesian-process
       ;; ネットワークプロセスなら、open, 通常のサブプロセスなら、run。
       ;; これらは、排他的。
       (memq (process-status skk-bayesian-process) '(open run))))

(defsubst skk-bayesian-read-process-output (input)
  "input が nil の時、EOF を`skk-bayesian-process'に送り、そうでな
  ければ、input を`skk-bayesian-process'に送る。その後、\\nが
  `skk-bayesian-process'のバッファに出力されるまで待ち、\\nが出力
  された時点で、バッファを評価する。 "
  (with-current-buffer (process-buffer skk-bayesian-process)
    (delete-region (point-min) (point-max))
    (if input
        (process-send-string skk-bayesian-process input)
      (process-send-eof skk-bayesian-process))
    (while (not (and (> (point-max) 1)
                     (eq (char-after (1- (point-max))) ?\n)))
      (accept-process-output skk-bayesian-process 0 5))
    (goto-char (point-min))
    (condition-case err
        (read (current-buffer))
      (error (skk-message "Error while reading the out put of bskk; %s"
                          "bskk の出力の読み込み中にエラー; %s"
                          (error-message-string err))
             nil))))

(defun skk-bayesian-make-context (henkan-buffer)
  ;; "▼" があれば、`skk-bayesian-context-len'の長さの文字列を返す。
  ;; なければ、nil。
  (interactive)
  (let ((raw-text
         (with-current-buffer henkan-buffer
           (let ((kakutei-mark-point
                  (save-excursion
                    (search-backward "▼" (max (point-min) (- (point) 100)) t))))
             (if kakutei-mark-point
                 (buffer-substring-no-properties
                  (max (- kakutei-mark-point skk-bayesian-context-len)
                       (point-min))
                  kakutei-mark-point))))))
    (when raw-text
      (with-temp-buffer
        (let ((min (point-min)))
          (insert raw-text)
          ;; 文字列から改行を join-line で除く。
          ;; 但し、日本語の中の改行は空白が入るので、それを除く。
          (while (not (eq min (point)))
            (goto-char (point-max))
            (join-line)
            ;; from skk-viper.el
            (let ((char-after (char-after (progn (skip-chars-forward " ")
                                                 (point))))
                  (char-before (char-before (progn (skip-chars-backward " ")
                                                   (point)))))
              (when (and char-after char-before
                         (or (skk-jisx0208-p char-after)
                             (skk-jisx0213-p char-after))
                         (or (skk-jisx0208-p char-before)
                             (skk-jisx0213-p char-before)))
                (while (looking-at " ")
                  (delete-char 1))))))
        (buffer-string)))))

(defun skk-bayesian-search (henkan-buffer midasi okurigana entry)
  ;; 引数の例
  ;; entry : ("斬" "切" "着")
  ;; midasi: きr
  ;; okurigana: る
  (setq skk-bayesian-last-context nil)
  (if (= 1 (length entry))
      entry
    (skk-bayesian-init)
    (let ((context (skk-bayesian-make-context henkan-buffer))
          entry-str
          new-entry)
      ;; make entry-str
      (let ((e entry))
        (while e
          (setq entry-str (concat entry-str (car e) "/"))
          (setq e (cdr e))))
      ;; send context to skk-bayesian-process
      (setq new-entry
            (skk-bayesian-read-process-output
             (concat skk-bayesian-command-sort entry-str
                     "\n" context "\n")))
      ;; send debugging messages
      (skk-bayesian-debug-message (concat "search: entry-str=" entry-str))
      (skk-bayesian-debug-message (concat "search: context=" context))
      (skk-bayesian-debug-message (concat "search: new-entry="
                                          (prin1-to-string new-entry)))
      ;; return new-entry or entry
      (if (and new-entry
               (listp new-entry))
          (progn
            (setq skk-bayesian-last-context context)
            new-entry)
        entry))))

(defun skk-bayesian-update (henkan-buffer midasi okurigana word purge)
  (when skk-bayesian-last-context ;; entry の要素が 1 の時は、nil
    (add-hook 'post-command-hook 'skk-bayesian-check-modification-after-kakutei)
    (if skk-bayesian-add-to-history-pending
        ;; pending していたのを保存
        (skk-bayesian-add-to-history))
    ;; pending 開始
    (skk-bayesian-debug-message (concat "update: pending..." word))
    (setq skk-bayesian-number-of-command-after-kakutei -1;; 確定に1回かかるので-1
          skk-bayesian-add-to-history-pending t
          skk-bayesian-last-buffer henkan-buffer
          skk-bayesian-last-kakutei-word word
          skk-bayesian-last-okurigana okurigana
          skk-bayesian-last-context-pending skk-bayesian-last-context)
    (with-current-buffer henkan-buffer
      ;; skk-get-last-henkan-datum は、buffer-local な変数を用いている。
      (setq skk-bayesian-last-henkan-point
            (skk-get-last-henkan-datum 'henkan-point)))))

(defun skk-bayesian-check-modification-after-kakutei ()
  ;; skk-bayesian-max-commands-to-wait-for 回数コマンドが実行されれば、
  ;; skk-bayesian-add-to-history を実行する。
  (when skk-bayesian-add-to-history-pending
    (setq skk-bayesian-number-of-command-after-kakutei
          (1+ skk-bayesian-number-of-command-after-kakutei))
    (when (<= skk-bayesian-max-commands-to-wait-for
              skk-bayesian-number-of-command-after-kakutei)
      (skk-bayesian-add-to-history))))

(defun skk-bayesian-add-to-history ()
  "`skk-bayesian-last-kakutei-word' を、bskk の履歴に追加する。も
し、`skk-bayesian-last-kakutei-word' が変換後に修正されていた場合
は追加しない。参考:`skk-bayesian-max-commands-to-wait-for'。"
  ;; 依存している変数
  ;; skk-bayesian-add-to-history-pending
  ;; skk-bayesian-last-kakutei-word
  ;; skk-bayesian-last-buffer
  ;; skk-bayesian-last-okurigana
  ;; skk-bayesian-last-henkan-point
  ;; skk-bayesian-last-context-pending
  ;; 注意
  ;; skk-get-last-henkan-datum は、新しい確定が pending 中に起こるので、使えない。
  (setq skk-bayesian-add-to-history-pending nil)
  (when (and skk-bayesian-last-kakutei-word
             skk-bayesian-last-henkan-point
             skk-bayesian-last-buffer)
    (with-current-buffer skk-bayesian-last-buffer
      (let* ((kakutei-with-okuri (concat skk-bayesian-last-kakutei-word
                                         skk-bayesian-last-okurigana))
             (word-len (length kakutei-with-okuri))
             ;; henkan-point は、送り仮名がある場合は、送り仮名の point
             (end (marker-position skk-bayesian-last-henkan-point))
             (start (- end word-len))
             (current-word (if (and (<= (point-min) start) (<= end (point-max)))
                               (buffer-substring-no-properties start end))))
        ;; kakutei-word が変更されているか
        (if (not (string= current-word kakutei-with-okuri))
            (skk-bayesian-debug-message "add: kakutei-word has been modified")
          (skk-bayesian-debug-message (concat "add: context="
                                              skk-bayesian-last-context-pending))
          (skk-bayesian-debug-message (concat "add: kakutei-word="
                                              skk-bayesian-last-kakutei-word))
          (skk-bayesian-init)
          (if (skk-bayesian-read-process-output
               (concat skk-bayesian-command-add
                       skk-bayesian-last-kakutei-word "\n"
                       skk-bayesian-last-context-pending "\n"))
              (skk-bayesian-debug-message "add: done")
            (skk-bayesian-debug-message "add: failed")))))))

(defun skk-bayesian-save-history ()
  "Save skk-bayesian history to `skk-bayesian-history-file'."
  (interactive)
  (when (skk-bayesian-process-live-p)
    (skk-message "skk-bayesian の履歴を保存しています..." "saving history...")
    (if (skk-bayesian-read-process-output skk-bayesian-command-save)
        (skk-message "skk-bayesian の履歴を保存しています...完了"
                     "saving history...done")
      (skk-message "skk-bayesian の履歴を保存しています...失敗"
                   "saving history...failed"))))

(defun skk-bayesian-restart-process ()
  (if (skk-bayesian-process-live-p) (skk-bayesian-kill-process))
  (let  ((proc-buf (get-buffer-create (if skk-bayesian-debug
                                          "*skk-bayesian*"
                                        " *skk-bayesian*")))
         (proc-name "skk-bayesian"))
    (setq skk-bayesian-process
          (or (and skk-bayesian-prefer-server
                   (condition-case err
                       (open-network-stream proc-name
                                            proc-buf
                                            skk-bayesian-host skk-bayesian-port)
                     (error (skk-bayesian-debug-message "Error: %s\n%s"
                                                        (error-message-string err)
                                                        "run bskk as a sub process")
                            nil)))
              (start-process proc-name
                             proc-buf
                             "ruby" "-S" "bskk" "-f" skk-bayesian-history-file
                             (if skk-bayesian-debug "-v" "")
                             (if skk-bayesian-debug "-d" "")))))
  (set-process-coding-system skk-bayesian-process
                             skk-bayesian-coding-system
                             skk-bayesian-coding-system)
  (static-if (fboundp 'set-process-query-on-exit-flag)
      (set-process-query-on-exit-flag skk-bayesian-process nil)
    (process-kill-without-query skk-bayesian-process)))

(defun skk-bayesian-kill-process ()
  "Kill skk-bayesian process."
  (interactive)
  (when (skk-bayesian-process-live-p)
    (unless (skk-bayesian-read-process-output nil) ;; send EOF
      ;; skk-bayesian-processがEOFを受理しなかった時
      (when (skk-bayesian-process-live-p)
	(skk-bayesian-debug-message "sent EOF, but the process still lives")
	;; send SIGKILL or close the connection
	(delete-process skk-bayesian-process)))
    (setq skk-bayesian-process nil)))

(defun skk-bayesian-init ()
  "Set up skk-bayesian process."
  (interactive)
  (when (not (skk-bayesian-process-live-p))
    (skk-bayesian-restart-process)))

(provide 'skk-bayesian)

(add-to-list 'skk-search-end-function 'skk-bayesian-search)
(add-to-list 'skk-update-end-function 'skk-bayesian-update)
(add-hook 'skk-before-kill-emacs-hook 
          (function (lambda ()
                      (skk-bayesian-save-history)
                      (skk-bayesian-kill-process))))
(skk-bayesian-init)

;;; skk-bayesian.el ends here
