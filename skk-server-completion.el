;;; skk-server-completion.el --- server completion のクライアント
;;
;; Copyright (C) 2005 Fumihiko MACHIDA <machida@users.sourceforge.jp>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA

;;; Commentary:

;; Server completion に対応した辞書サーバを用い見出し語から始まる全ての
;; 語句の検索を行ないます。

;; このプログラムは以下の 2 つの機能を提供します。
;;
;; * skk-look の日本語版。読みの最後に `~' を付けて変換すると、その読みか
;;   ら始まる全ての候補を表示します。
;;
;; 例：
;;
;; ▽まちだ~
;; ==> "まちだ" "町田" "まちだえき" "町田駅" "まちだおだきゅう" "町田小田急" ..
;;
;; * skk-comp で、server completion を使用
;;
;; 例：
;;
;; ▽まちだ-!- で Tab を押すと、▽まちだえき → ▽まちだおだきゅう ……
;; となります。

;; [設定方法]
;;
;; .skk に、以下を追加します。
;;
;; (require 'skk-server-completion)
;; (add-to-list 'skk-search-prog-list
;;	     '(skk-server-completion-search) t)
;;
;; また、`~' を付けた変換結果を個人辞書に学習してしまうのをやめるためには
;; 以下を追加してください。
;;
;; (add-hook 'skk-search-excluding-word-pattern-function
;;	  #'(lambda (kakutei-word)
;;	      (string-match (format "%s$"
;;				    (regexp-quote
;;				     (char-to-string
;;				      skk-server-completion-search-char)))
;;			    skk-henkan-key)))

;;; Code:

(require 'skk)
(require 'skk-comp)


;;;###autoload
(defun skk-server-completion-search ()
  "サーバーコンプリーションを行い、得られた各見出しでさらに検索する。
送り有り変換には非対応。"
  (when (and (eq (aref skk-henkan-key (1- (length skk-henkan-key)))
		 skk-server-completion-search-char)
	     (not (or skk-henkan-okurigana
		      skk-okuri-char)))
    (let ((key (substring skk-henkan-key
			  0 (1- (length skk-henkan-key))))
	  midasi-list)
      (when skk-use-numeric-conversion
	(setq key (skk-num-compute-henkan-key key)))
      (setq midasi-list (skk-server-completion-search-midasi key))
      (skk-server-completion-search-recursive midasi-list))))

(defun skk-server-completion-search-midasi (key)
  "server completion を利用して、key から始まるすべての見出し語のリストを返却する。"
  (when (skk-server-live-p (skk-open-server))
    (with-current-buffer skkserv-working-buffer
      (let ((cont t)
	    (count 0)
	    l)
	(erase-buffer)
	(process-send-string skkserv-process (concat "4" key " "))
	(while (and cont (skk-server-live-p))
	  (accept-process-output)
	  (setq count (1+ count))
	  (when (> (buffer-size) 0)
	    (if (eq (char-after 1) ?1)	;?1
		;; found key successfully, so check if a whole line
		;; is received.
		(when (eq (char-after (1- (point-max)))
			  ?\n)		;?\n
		  (setq cont nil))
	      ;; not found or error, so exit
	      (setq cont nil))))
	(goto-char (point-min))
	(when skk-server-report-response
	  (skk-message "%d 回 SKK サーバーの応答待ちをしました"
		       "Waited for server response %d times"
		       count))
	(when (eq (following-char) ?1)	;?1
	  (forward-char 2)
	  (car (skk-compute-henkan-lists nil)))))))

(defun skk-server-completion-search-recursive (midasi-list)
  "`midasi-list' の見出しを再変換する"
  (let (result-list kouho-list)
    (dolist (skk-henkan-key midasi-list)
      (setq kouho-list (cons skk-henkan-key (skk-search-server-1 nil nil))
	    result-list (nconc result-list kouho-list)))
    result-list))

;;;###autoload
(defun skk-comp-by-server-completion ()
  ;; skk-comp-prefix は使えない
  ;; 今のところ非数値変換のみ
  (when skk-comp-first
    (setq skk-server-completion-words
	  (skk-server-completion-search-midasi skk-comp-key))
    (when (string= skk-comp-key
		   (car skk-server-completion-words))
      (pop skk-server-completion-words)))
  (pop skk-server-completion-words))

(provide 'skk-server-completion)


;;; skk-server-completion.el ends here
