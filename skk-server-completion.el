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
;; (add-to-list 'skk-search-prog-list
;;	     '(skk-server-completion-search) t)
;;
;; (add-to-list 'skk-completion-prog-list
;;	     '(skk-comp-by-server-completion) t)
;;
;; また、`~' を付けた変換結果を個人辞書に学習してしまうのをやめるためには
;; 以下を追加してください。
;;
;; (add-hook 'skk-search-excluding-word-pattern-function
;; 	  #'(lambda (kakutei-word)
;; 	      (eq (aref skk-henkan-key (1- (length skk-henkan-key)))
;; 		  skk-server-completion-search-char)))

;;; Code:

(require 'skk)
(require 'skk-comp)
(require 'skk-server)

(eval-when-compile
  (require 'cl))

;;;###autoload
(defun skk-server-completion-search ()
  "サーバーコンプリーションを行い、得られた各見出しでさらに検索する。
送り有り変換には非対応。"
  (when (and (eq (aref skk-henkan-key (1- (length skk-henkan-key)))
		 skk-server-completion-search-char)
	     (not (or skk-henkan-okurigana
		      skk-okuri-char)))
    ;; skk-search では見出しが数字を含む時のみ
    ;; skk-use-numeric-conversion が t な呼出しをするが、
    ;; 一応それに依存しないようにしている。
    (let* ((henkan-key (substring skk-henkan-key
				  0 (1- (length skk-henkan-key))))
	   (numericp (and skk-use-numeric-conversion
			  (save-match-data
			    (string-match "[0-9０-９]" henkan-key))))
	   (conv-key (and numericp
			  (skk-num-compute-henkan-key henkan-key)))
	   (key (or conv-key henkan-key))
	   midasi-list result-list kouho-list)
      (setq midasi-list (skk-server-completion-search-midasi key))
      (dolist (skk-henkan-key midasi-list)
	;; 見出しに対応したエントリがサーバに存在する事を前提としている。
	;; 不整合があってもエラーにはならないが、見出しだけが表示される事になるので
	;; 検索対象辞書から直接補完候補を生成していないサーバでは運用に気をつける事。
	(setq kouho-list (cons (if numericp
				   (concat henkan-key
					   (substring skk-henkan-key
						      (length key)))
				 skk-henkan-key)
			       (skk-search-server-1 nil nil))
	      result-list (nconc result-list kouho-list)))
      result-list)))

(defun skk-server-completion-search-midasi (key)
  "server completion を利用して、key から始まるすべての見出し語のリストを返却する。"
  (when (skk-server-live-p (skk-open-server))
    (with-current-buffer skkserv-working-buffer
      (let ((cont t)
	    (count 0))
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

;;;###autoload
(defun skk-comp-by-server-completion ()
  ;; skk-comp-prefix は使わないので、
  ;; 必要なら skk-comp-restrict-by-prefix() を併用する。
  "Server completion に対応した SKK サーバを利用する補完プログラム。
`skk-completion-prog-list' の要素に指定して使う。"
  (let* ((numericp (and skk-use-numeric-conversion
			(save-match-data
			  (string-match "[0-9０-９]" skk-comp-key))))
	 (conv-key (and numericp
			(skk-num-compute-henkan-key skk-comp-key)))
	 (comp-key (or conv-key skk-comp-key))
	 word)
    (when skk-comp-first
      (setq skk-server-completion-words
	    (skk-server-completion-search-midasi comp-key))
      (when (string= comp-key
		     (car skk-server-completion-words))
	(pop skk-server-completion-words)))
    (setq word (pop skk-server-completion-words))
    (when word
      (if numericp
	  (concat skk-comp-key
		  (substring word (length comp-key)))
	word))))

(provide 'skk-server-completion)


;;; skk-server-completion.el ends here
