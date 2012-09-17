;; skk-bayesian.el -- Bayesian estimation for SKK -*- coding: iso-2022-jp -*-
;; Copyright (C) 2004 Kenichi Kurihara <kenichi_kurihara@nifty.com>

;; Author: Kenichi Kurihara <kenichi_kurihara@nifty.com>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-bayesian.el,v 1.6 2012/09/17 09:18:06 skk-cvs Exp $
;; Keywords: japanese, mule, input method, bayesian estimation
;; Last Modified: $Date: 2012/09/17 09:18:06 $

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
;; また、bskk は、サブプロセスかサーバとして使用します。
;; *サブプロセス
;;   サブプロセスとして使用するには、bskk を環境変数 PATH の通った場所に
;;   置くだけです。
;;   問題は、いくつも emacs を起動すると ~/.skk-bayesian は最後に更新した
;;   emacs に依るので、他の emacs での学習データは保存されません。
;; *サーバ
;;   bskk をサーバとして使用するには、emacs が skk-bayesian.el を読み込
;;   む前に、
;;     % bskk -f ~/.skk-bayesian -s
;;   と実行して、起動しておく必要があります。
;;   サーバを終了させる方法は、
;;     % kill -TERM {bskk の PID}
;;   です。
;;   ~/.skk には、(setq skk-bayesian-prefer-server t) を書いて下さい。
;;
;; <仕様の覚え書き>
;; 各関数と、bskk のコメント内の Specifications に書かれている。
;;

;;; Code:

(require 'skk-vars)
(require 'skk-macs)

(defgroup skk-bayesian nil "SKK Bayesian estimation group"
  :prefix "skk-bayesian-"
  :group 'skk)

;;;
;;; variables for skk-bayesian
;;;
(defcustom skk-bayesian-prefer-server nil
  "*non-nil ならば、`skk-bayesian-host'の`skk-bayesian-port'に接続する。
そうでなければ、bskk をサブプロセスとして立ち上げる。"
  :type 'boolean
  :group 'skk-bayesian)

(defcustom skk-bayesian-port 51178
  "*`skk-bayesian-host'に接続するポート番号。
サーバに接続するには`skk-bayesian-prefer-server'が non-nil である必要がある。"
  :type 'integer
  :group 'skk-bayesian)

(defcustom skk-bayesian-host "localhost"
  "*`skk-bayesian-prefer-server'が non-nil の時に接続するホスト名。"
  :type 'string
  :group 'skk-bayesian)

(defcustom skk-bayesian-context-len 20
  "*学習や予測に使用する、変換語の直前の文字数。"
  :type 'integer
  :group 'skk-bayesian)

(defcustom skk-bayesian-history-file
  (if skk-user-directory
      (expand-file-name "bayesian" skk-user-directory)
    (convert-standard-filename "~/.skk-bayesian"))
  "*履歴を記録するファイル名。
`skk-bayesian-prefer-server'が non-nil の時にのみ使用される。"
  :type 'file
  :group 'skk-bayesian)

(defcustom skk-bayesian-debug nil
  "*non-nil ならばデバッグ用のメッセージを表示する。"
  :type 'boolean
  :group 'skk-bayesian)

(defcustom skk-bayesian-max-commands-to-wait-for 15
  "*確定語を学習するまでに待つコマンドの数。
確定の後に`skk-bayesian-max-commands-to-wait-for'回のコマンド
のうちに確定語(送り仮名を含む)が変更されなければ、その確定語を保存
する。`skk-bayesian-max-commands-to-wait-for'が0以下ならば、確定後、
直ちに履歴に保存する。"
  :type 'integer
  :group 'skk-bayesian)

(defcustom skk-bayesian-corpus-make nil
  "*nin-nil ならば、corpus を `skk-bayesian-corpus-file' に作成する。"
  :type 'boolean
  :group 'skk-bayesian)

(defcustom skk-bayesian-corpus-file
  (if skk-user-directory
      (expand-file-name "corpus" skk-user-directory)
    (convert-standard-filename "~/.skk-corpus"))
  "*corpus を保存するファイル。"
  :type 'file
  :group 'skk-bayesian)

;; internal variables
(defvar skk-bayesian-last-context nil "*確定語の直前の文字列。")
(defvar skk-bayesian-number-of-command-after-kakutei 0
  "*前回の確定から現在までのコマンドの回数。")
(defvar skk-bayesian-pending-data-alist nil "*non-nil ならば pending 中。")
(defvar skk-bayesian-process nil)
(defvar skk-bayesian-corpus-buffer nil)
(defvar skk-bayesian-corpus-last-sorted-entry nil
  "*前回 skk-bayesian-search で返した entry")

;; constants
(defconst skk-bayesian-command-sort "#sort\n")
(defconst skk-bayesian-command-add "#add\n")
(defconst skk-bayesian-command-save "#save\n")
(defconst skk-bayesian-coding-system 'euc-jp)
(defconst skk-bayesian-corpus-buffer-name " *skk-corpus*")


;;;
;;; functions
;;;
(defmacro skk-bayesian-debug-message (STRING &rest ARGS)
  `(if skk-bayesian-debug
       (message ,STRING ,@ARGS)))

(defsubst skk-bayesian-process-live-p ()
  "`skk-bayesian-process' が non-nil かつそのプロセスが実行中なら t を返す。 "
  (and skk-bayesian-process
       ;; ネットワークプロセスなら、open, 通常のサブプロセスなら、run。
       ;; これらは、排他的。
       (memq (process-status skk-bayesian-process) '(open run))))

(defsubst skk-bayesian-make-pending-data-alist
  ;; henkan-point は確定語の最初の文字の位置の marker
  (word okurigana midasi buffer henkan-point context)
  (setq skk-bayesian-pending-data-alist
        (if (and word midasi buffer henkan-point context)
            ;; 特に henkan-point が nil になり易いようだ。
            ;; okurigana は、nil でもよい。
            (list (cons 'word word)
                  (cons 'okurigana okurigana)
                  (cons 'midasi midasi)
                  (cons 'buffer buffer)
                  (cons 'henkan-point henkan-point)
                  (cons 'context context)))))

(defsubst skk-bayesian-get-pending-data-alist (key)
  (if (memq key (list 'word 'okurigana 'midasi 'buffer 'henkan-point 'context))
      (cdr (assq key skk-bayesian-pending-data-alist))
    (error (concat "Error; invalid key=" (prin1-to-string 'key)))))
  
(defsubst skk-bayesian-read-process-output (input)
  "input を`skk-bayesian-process'に送る。その後、\\nが `skk-bayesian-process'のバッファに出力されるまで待ち、\\nが出力された時点で、バッファを評価し返す。"
  (when input
    (skk-bayesian-init)
    (with-current-buffer (process-buffer skk-bayesian-process)
      (delete-region (point-min) (point-max))
      (process-send-string skk-bayesian-process input)
      (while (not (and (> (point-max) 1)
                       (eq (char-after (1- (point-max))) ?\n)))
        (accept-process-output skk-bayesian-process 0 5))
      (goto-char (point-min))
      (condition-case err
          (read (current-buffer))
        (error (skk-message "Error while reading the out put of bskk; %s"
                            "bskk の出力の読み込み中にエラー; %s"
                            (error-message-string err))
               nil)))))

(defun skk-bayesian-make-context (henkan-buffer)
  ;; もし"▼"があれば、`skk-bayesian-context-len'の長さの文字列を返す。
  ;; なければ、nil。
  (let ((raw-text
         (with-current-buffer henkan-buffer
           (let ((kakutei-symbol-point
                  (save-excursion
                    ;; 100 文字前までしか▼を検索しない
                    (search-backward "▼" (max (point-min) (- (point) 100)) t))))
             (if kakutei-symbol-point
                 (buffer-substring-no-properties
                  (max (- kakutei-symbol-point skk-bayesian-context-len)
                       (point-min))
                  kakutei-symbol-point))))))
    (if raw-text
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
        (buffer-string))
      nil)))

(defun skk-bayesian-search (henkan-buffer midasi okurigana entry)
  ;; ソートした、entry を返す
  ;; 引数の例
  ;; entry : ("斬" "切" "着")
  ;; midasi: きr
  ;; okurigana: る
  (setq skk-bayesian-last-context nil)
  (if (= 1 (length entry))
      entry
    (let ((context (skk-bayesian-make-context henkan-buffer))
          ;; 末尾の "/" は多分不要だが
          (entry-str (concat (mapconcat #'identity entry "/") "/"))
          sorted-entry)
      ;; send context to skk-bayesian-process
      (setq sorted-entry
            (skk-bayesian-read-process-output
             (concat skk-bayesian-command-sort entry-str
                     "\n" context "\n")))
      ;; send debugging messages
      (skk-bayesian-debug-message "Search: entry-str=%s" entry-str)
      (skk-bayesian-debug-message "Search: context=%s" context)
      (skk-bayesian-debug-message "Search: sorted-entry=%s" sorted-entry)
      ;; return sorted-entry or entry
      (if (and sorted-entry
               (listp sorted-entry))
          (progn
            (setq skk-bayesian-last-context context
                  skk-bayesian-corpus-last-sorted-entry sorted-entry)
            sorted-entry)
        entry))))

(defun skk-bayesian-update (henkan-buffer midasi okurigana word purge)
  (when skk-bayesian-last-context ;; entry の要素が 1 の時は、nil
    (if (and skk-bayesian-corpus-make
             skk-bayesian-corpus-last-sorted-entry
             (not (string= word (car skk-bayesian-corpus-last-sorted-entry))))
        ;; 第一候補が間違いだった時
        (skk-bayesian-corpus-append 'bad-inference 
                                    skk-bayesian-last-context 
                                    midasi
                                    okurigana
                                    (car skk-bayesian-corpus-last-sorted-entry)))
    (add-hook 'post-command-hook 'skk-bayesian-check-modification-after-kakutei)
    (if skk-bayesian-pending-data-alist
        ;; pending していたのを保存
        (skk-bayesian-add-to-history))
    ;; pending 開始
    (skk-bayesian-debug-message "Update: pending... word=%s" word)
    (setq skk-bayesian-number-of-command-after-kakutei -1);; 確定に1回かかるので-1
    (skk-bayesian-make-pending-data-alist
     word 
     okurigana
     midasi
     henkan-buffer
     (with-current-buffer henkan-buffer
       ;; skk-get-last-henkan-datum は、buffer-local な変数を用いている。
       ;; skk-get-last-henkan-datum は、skk-update-end-function を読ん
       ;; だ後に更新される。ここでは使えない。
       (if skk-undo-kakutei-word-only
           (point-marker)
         (save-excursion
           (skk-bayesian-debug-message (prin1-to-string (point-marker)))
           (forward-char
	    (- 0
	       (length okurigana)
	       ;; word が注釈を含んでいる際、バッファに挿入される文字列の
	       ;; 長さよりも長くなってしまうので、point の位置によっては
	       ;; beginning-of-buffer のエラーとなる。ここで注釈を切り捨てた
	       ;; word の長さを取得しておけばその問題はない。
	       (length (car (skk-treat-strip-note-from-word word)))))
           (point-marker))))
     skk-bayesian-last-context)))

(defun skk-bayesian-check-modification-after-kakutei ()
  ;; skk-bayesian-max-commands-to-wait-for 回数コマンドが実行されれば、
  ;; skk-bayesian-add-to-history を実行する。
  (when skk-bayesian-pending-data-alist
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
  ;; skk-bayesian-pending-data-alist
  ;; 注意
  ;; skk-get-last-henkan-datum は、新しい確定が pending 中に起こるので、使えない。
  (if (not (skk-bayesian-process-live-p))
      (setq skk-bayesian-pending-data-alist nil))
  (when (and skk-bayesian-pending-data-alist
             (buffer-live-p (skk-bayesian-get-pending-data-alist 'buffer)))
    (with-current-buffer (skk-bayesian-get-pending-data-alist 'buffer)
      (let* ((kakutei-word (skk-bayesian-get-pending-data-alist 'word))
             (okurigana (skk-bayesian-get-pending-data-alist 'okurigana))
             (kakutei-with-okuri (concat kakutei-word okurigana))
             (word-len (length kakutei-with-okuri))
             (midasi (skk-bayesian-get-pending-data-alist 'midasi))
             ;; henkan-point は、送り仮名がある場合は、送り仮名の point
             (start (marker-position (skk-bayesian-get-pending-data-alist
                                    'henkan-point)))
             (end (+ start word-len))
             (current-word (if (and (<= (point-min) start) (<= end (point-max)))
                               (buffer-substring-no-properties start end)))
             (context (skk-bayesian-get-pending-data-alist 'context)))
        ;; kakutei-word が変更されているか
        (if (not (string= current-word kakutei-with-okuri))
            (progn
              (skk-bayesian-debug-message "Add: kakutei-word has been modified")
              (if skk-bayesian-corpus-make
                  (skk-bayesian-corpus-append 'modified context midasi okurigana
                                              kakutei-word)))
          (skk-bayesian-debug-message "Add: context=%s" context)
          (skk-bayesian-debug-message "Add: kakutei-word=%s" kakutei-word)
          (if (skk-bayesian-read-process-output
               (concat skk-bayesian-command-add
                       kakutei-word "\n"
                       context "\n"))
              (skk-bayesian-debug-message "Add: done")
            (skk-bayesian-debug-message "Add: failed"))
          (if skk-bayesian-corpus-make
              (skk-bayesian-corpus-append 'positive context midasi okurigana
                                          kakutei-word)))))
    (setq skk-bayesian-pending-data-alist nil)))

(defun skk-bayesian-save-history ()
  "Save skk-bayesian history to `skk-bayesian-history-file'."
  (interactive)
  (if skk-bayesian-pending-data-alist
      ;; pending していたのを保存
      (skk-bayesian-add-to-history))
  (when (skk-bayesian-process-live-p)
    (skk-message "skk-bayesian の履歴を保存しています..." 
		 "saving skk-bayesian history...")
    (if (skk-bayesian-read-process-output skk-bayesian-command-save)
        (skk-message "skk-bayesian の履歴を保存しています...完了"
                     "saving skk-bayesian history...done")
      (skk-message "skk-bayesian の履歴を保存しています...失敗"
                   "saving skk-bayesian history...failed"))))

(defun skk-bayesian-restart-process ()
  (if (skk-bayesian-process-live-p) (skk-bayesian-kill-process))
  (let  ((proc-buf (get-buffer-create (if skk-bayesian-debug
                                          "*skk-bayesian*"
                                        " *skk-bayesian*")))
         (proc-name "skk-bayesian"))
    (skk-message "プロセス bskk を起動しています..."
                 "Launching a process, bskk...")
    (setq skk-bayesian-process
          (or (and skk-bayesian-prefer-server
                   (condition-case err
                       (open-network-stream proc-name
                                            proc-buf
                                            skk-bayesian-host skk-bayesian-port)
                     (error (skk-bayesian-debug-message
                             "Error: %s\n%s"
                             (error-message-string err)
                             "Running bskk as a sub process")
                            nil)))
              (if skk-bayesian-debug
                  (start-process proc-name
                                 proc-buf
                                 "ruby" "-S" "bskk" "-f" 
                                 skk-bayesian-history-file
                                 "-v" "-d")
                (start-process proc-name
                               proc-buf
                               "ruby" "-S" "bskk" "-f"
                               skk-bayesian-history-file))))
    (if skk-bayesian-process
        (skk-message "プロセス bskk を起動しています...完了"
                     "Launching a process, bskk...done")
      (skk-message "プロセス bskk を起動しています...失敗"
                   "Launching a process, bskk...failed")))
  (set-process-coding-system skk-bayesian-process
                             skk-bayesian-coding-system
                             skk-bayesian-coding-system)
  (if (eval-when-compile (and (featurep 'emacs)
				(>= emacs-major-version 22)))
      (set-process-query-on-exit-flag skk-bayesian-process nil)
    (process-kill-without-query skk-bayesian-process)))

(defun skk-bayesian-kill-process ()
  "Kill skk-bayesian process."
  (interactive)
  (when skk-bayesian-process
    (let ((status (process-status skk-bayesian-process)))
      (cond 
       ((memq status '(open connect))
        ;; close connection
        (delete-process skk-bayesian-process))
       ((eq status 'run)
        ;; send SIGTERM=15
        (signal-process (process-id skk-bayesian-process) 15)))
      (setq skk-bayesian-process nil))))

(defun skk-bayesian-init ()
  "Set up skk-bayesian process."
  (interactive)
  (when (not (skk-bayesian-process-live-p))
    (skk-bayesian-restart-process)))

(provide 'skk-bayesian)

(add-to-list 'skk-search-end-function 'skk-bayesian-search)
(add-to-list 'skk-update-end-function 'skk-bayesian-update)
(add-hook 'kill-emacs-hook 
          (function (lambda ()
                      (skk-bayesian-save-history)
                      (skk-bayesian-corpus-save)
                      (skk-bayesian-kill-process))))

(skk-bayesian-init)


;;;
;;; functions for skk-bayesian-corpus
;;;
(defun skk-bayesian-corpus-init ()
  (unless (buffer-live-p skk-bayesian-corpus-buffer)
    (setq skk-bayesian-corpus-buffer
          (get-buffer-create skk-bayesian-corpus-buffer-name)) ))

(defun skk-bayesian-corpus-append (flag context midasi okurigana word)
  ;; called by `skk-bayesian-add-to-history'
  ;; if flag is non-nil then append data as positive data
  ;; otherwise append data as negative data
  (when (and context midasi word) ;; okurigana can be nil
    (skk-bayesian-corpus-init)
    (with-current-buffer skk-bayesian-corpus-buffer
      (goto-char (point-max))
      (insert (concat (cond
                       ((eq flag 'positive) "+")
                       ((eq flag 'modified) "m")
                       ((eq flag 'bad-inference) "-")
                       (t (error (concat "Error; invalid flag=" 
                                         (prin1-to-string flag)))))
                      midasi " ["
                      okurigana "/"
                      word "/]"
                      context "\n")))))

(defun skk-bayesian-corpus-save ()
  "Save corpus to `skk-bayesian-corpus-file'."
  (interactive)
  (if (let ((attrs (file-attributes skk-bayesian-corpus-file)))
        (or (not attrs) ;; ファイルが存在しなければ、attrs は、nil
            (eq (nth 8 attrs) 0))) ;; ファイルサイズが 0
      (with-temp-buffer
        (insert ";; + means positive
;; m means modified after henkan
;; - menas wrong inference
;; m and - are usually negative.
;; However, in some cases, m data would be correct henkan
;; because we might delete correct henkan.")
        (write-file skk-bayesian-corpus-file)))
  (when skk-bayesian-corpus-buffer
    (with-current-buffer skk-bayesian-corpus-buffer
      (write-region (point-min) (point-max) skk-bayesian-corpus-file 'append)
      (delete-region (point-min) (point-max)))))


;;; skk-bayesian.el ends here
