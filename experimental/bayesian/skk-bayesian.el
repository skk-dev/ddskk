;; skk-bayesian.el -- Bayesian estimation for SKK
;; Copyright (C) 2004 Kenichi Kurihara <kenichi_kurihara@nifty.com>

;; Author: Kenichi Kurihara <kenichi_kurihara@nifty.com>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-bayesian.el,v 1.2 2004/02/29 01:05:10 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2004/02/29 01:05:10 $

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
;; 例: (skk-bayesian-prefix-len = 5 の時)
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
;; 3. skk-bayesian-prefix-len は変数にしているので、ユーザが決定できる
;;    が、理想的にはモデルの推定問題ととらえて、学習データから決定すべ
;;    きだろう。また、ある程度、学習した後に skk-bayesian-prefix-lenを
;;    大きい値に変更するのは、推定に悪影響を与えそう。
;; 4. 2と3に重なるが、著作権の心配をしなくてもよいコーパスから、学習を行い
;;    skk-bayesian-prefix-len と 混合分布の重みを決定したい。
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

;;; Code:

(require 'skk-vars)
(require 'skk-macs)

(defvar skk-bayesian-prefer-server nil
  "*non-nil ならば、`skk-bayesian-server-port'の`skk-bayesian-port'に接続する。
そうでなければ、bskk をサブプロセスとして立ち上げる。")
(defvar skk-bayesian-port 51178
  "*`skk-bayesian-prefer-server'が non-nil の時に`skk-bayesian-host'に接続するポート番号")
(defvar skk-bayesian-host "localhost"
  "*`skk-bayesian-prefer-server'が non-nil の時に接続するホスト")
(defvar skk-bayesian-coding-system 'euc-jp)
(defvar skk-bayesian-prefix-len 20 "*学習や予測に使用する、変換語の直前の文字数")
(defvar skk-bayesian-last-prefix-str nil "*確定語の直前の文字列")
(defvar skk-bayesian-history-file "~/.skk-bayesian" "*history file")
(defvar skk-bayesian-debug nil "*デバッグ用のメッセージを表示")

(defconst skk-bayesian-command-sort "#sort\n")
(defconst skk-bayesian-command-add "#add\n")
(defconst skk-bayesian-command-save "#save\n")
(defvar skk-bayesian-process nil)

(defsubst skk-bayesian-debug-message (STRING &rest ARGS)
  (if skk-bayesian-debug
      (apply 'message STRING ARGS)))

(defsubst skk-bayesian-process-live-p ()
  "`skk-bayesian-process' が non-nil かつそのプロセスが実行中なら t を返す。 "
  (if skk-bayesian-process
      (let ((status (process-status skk-bayesian-process)))
        ;; ネットワークプロセスなら、open, 通常のサブプロセスなら、run。
        ;; これらは、排他的。
        (or (eq status 'open)
            (eq status 'run)))))

(defun skk-bayesian-search (henkan-buffer midasi okurigana entry)
  ;; 引数の例
  ;; entry : ("斬" "切" "着")
  ;; midasi: きr
  ;; okurigana: る
  (setq skk-bayesian-last-prefix-str nil)
  (when (< 1 (length entry))
    (skk-bayesian-init)
    (let ((prefix-str "")
          (entry-str "")
          new-entry)
      ;; make entry-str
      (let ((e entry))
        (while e
          (setq entry-str (concat entry-str " " (car e)))
          (setq e (cdr e))))
      (skk-bayesian-debug-message (concat "entry-str=" entry-str))
      ;; make prefix-str
      (with-current-buffer henkan-buffer
	(let* ((just-before-point (- (point) (length midasi) 2))
	       (prefix-str-len 0)
	       char)
	  (while (and (<= (point-min) just-before-point)
		      (<= prefix-str-len skk-bayesian-prefix-len))
	    (setq char (buffer-substring-no-properties
			just-before-point (1+ just-before-point)))
	    (when (not (string-match "[[:cntrl:][:blank:]]" char))
	      (setq prefix-str (concat char " " prefix-str))
	      (setq prefix-str-len (1+ prefix-str-len)))
	    (setq just-before-point (1- just-before-point)))
	  ))
      (skk-bayesian-debug-message (concat "prefix-str=" prefix-str))
      ;; send prefix-str to skk-bayesian-process
      (with-current-buffer (process-buffer skk-bayesian-process)
	(delete-region (point-min) (point-max))
	(process-send-string skk-bayesian-process skk-bayesian-command-sort)
	(process-send-string skk-bayesian-process (concat entry-str "\n"))
	(process-send-string skk-bayesian-process (concat prefix-str "\n"))
	(while (not (and (> (point-max) 1)
			 (eq (char-after (1- (point-max))) ?\n)))
	  (accept-process-output skk-bayesian-process 0 5))
	(goto-char (point-min))
	(setq new-entry
	      (condition-case err
		  (read (current-buffer))
		(error (skk-message "Error while reading the out put of bskk; %s"
				    "bskk の出力の読み込み中にエラー; %s"
				    (error-message-string err))
		       nil))))
      (skk-bayesian-debug-message (concat "new-entry=" (prin1-to-string new-entry)))
      (if (and new-entry
               (listp new-entry))
          (progn
            (setq skk-bayesian-last-prefix-str prefix-str)
            new-entry)
        entry))))

(defun skk-bayesian-update (henkan-buffer midasi okurigana word purge)
  (when skk-bayesian-last-prefix-str
    (skk-bayesian-init)
    (skk-bayesian-debug-message (concat "kakutei-word=" word))
    (skk-bayesian-debug-message (concat "prefix=" skk-bayesian-last-prefix-str))
    (with-current-buffer (process-buffer skk-bayesian-process)
      (delete-region (point-min) (point-max))
      (process-send-string skk-bayesian-process skk-bayesian-command-add)
      (process-send-string skk-bayesian-process
			   (concat word "\n"))
      (process-send-string skk-bayesian-process
			   (concat skk-bayesian-last-prefix-str "\n"))
      ;; wait for a message to synchronize skk-bayesian-process
      (skk-bayesian-debug-message "add history...")
      (while (not (and (> (point-max) 1)
		       (eq (char-after (1- (point-max))) ?\n)))
	(accept-process-output skk-bayesian-process 0 5))
      (skk-bayesian-debug-message "add history...done.")
      )))

(defun skk-bayesian-save-history ()
  "Save skk-bayesian history to `skk-bayesian-history-file'."
  (interactive)
  (when (skk-bayesian-process-live-p)
    (with-current-buffer (process-buffer skk-bayesian-process)
      (delete-region (point-min) (point-max))
      (process-send-string skk-bayesian-process skk-bayesian-command-save)
      ;; wait for a message to synchronize skk-bayesian-process
      (skk-message "skk-bayesian の履歴を保存しています..." "saving history...")
      (while (not (and (> (point-max) 1)
		       (eq (char-after (1- (point-max))) ?\n)))
	(accept-process-output skk-bayesian-process 0 5))
      (skk-message "skk-bayesian の履歴を保存しています...完了" "saving history...done")
      )))

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
                                                        "run bskk as a sub process.")
                            nil)))
              (start-process proc-name
                             proc-buf
                             "ruby" "-S" "bskk" "-f" skk-bayesian-history-file
                             (if skk-bayesian-debug "-v" "")))))
  (set-process-coding-system skk-bayesian-process
                             skk-bayesian-coding-system
                             skk-bayesian-coding-system)
  (process-kill-without-query skk-bayesian-process))

(defun skk-bayesian-kill-process ()
  "Kill skk-bayesian process."
  (interactive)
  (when (skk-bayesian-process-live-p)
    (with-current-buffer (process-buffer skk-bayesian-process)
      (delete-region (point-min) (point-max))
      ;; send EOF
      (process-send-eof skk-bayesian-process)
      ;; wait for a skk-bayesian-debug-message to synchronize skk-bayesian-process
      (while (not (and (> (point-max) 1)
		       (eq (char-after (1- (point-max))) ?\n)))
	(accept-process-output skk-bayesian-process 0 5))
      (when (skk-bayesian-process-live-p)
	(skk-bayesian-debug-message "sent EOF, but the process still live.")
	;; send SIGKILL or close the connection
	(delete-process skk-bayesian-process))
      (setq skk-bayesian-process nil))))

(defun skk-bayesian-init ()
  "Set up skk-bayesian process."
  (interactive)
  (when (not (skk-bayesian-process-live-p))
    (skk-bayesian-restart-process)))

(provide 'skk-bayesian)

(add-to-list 'skk-search-end-function 'skk-bayesian-search)
(add-to-list 'skk-update-end-function 'skk-bayesian-update)
(add-hook 'skk-before-kill-emacs-hook 'skk-bayesian-save-history)
(skk-bayesian-init)

;;; skk-bayesian.el ends here
