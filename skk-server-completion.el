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
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA

(require 'skk)
(require 'skk-comp)

(defvar skk-server-completion-search-key ?~)

(defun skk-server-completion-search ()
  "SKK サーバーを使用して `skk-henkan-key' をキーにしたサーバコンプリーション検索を行う。
さらに得られた候補を `skk-search-server-1' を利用し変換する。
送り仮名がある場合は nil を返す。"
  (if (and (or skk-server-host
	       skk-servers-list)
	   (not (or skk-henkan-okurigana
		    skk-okuri-char)))
      (skk-server-completion-search-1)))

(defun skk-server-completion-search-1 ()
  "`skk-server-completion-search' のサブルーチン。
サーバーコンプリージョン検索を行なった後その見出しで再度検索を行なう。"
  (when (string-match (format "%s$" (regexp-quote
				     (char-to-string skk-server-completion-search-key)))
		      skk-henkan-key)
    (let* ((skk-henkan-key (substring skk-henkan-key 0 (match-beginning 0)))
	   (key
	    (if skk-use-numeric-conversion
		(skk-num-compute-henkan-key skk-henkan-key)
	      skk-henkan-key))
	   (midasi-list (skk-server-completion-search-midasi key))
	   (result-list))
      (skk-server-completion-search-recursive midasi-list))))

(defun skk-server-completion-search-midasi (key)
  "server completion を利用して、key から始まるすべての見出し語のリストを返却する。"
  (let (alst kana-list)
    (cond
     ((skk-server-live-p (skk-open-server))
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
	      (if (eq (char-after 1) ?1) ;?1
		  ;; found key successfully, so check if a whole line
		  ;; is received.
		  (when (eq (char-after (1- (point-max)))
			    ?\n)	;?\n
		    (setq cont nil))
		;; not found or error, so exit
		(setq cont nil))))
	  (goto-char (point-min))
	  (when skk-server-report-response
	    (skk-message "%d 回 SKK サーバーの応答待ちをしました"
			 "Waited for server response %d times"
			 count))
	  (when (eq (following-char) ?1) ;?1
	    (forward-char 2)
	    (setq kana-list (car (skk-compute-henkan-lists nil))))))
      kana-list))
    ))

(defun skk-server-completion-search-recursive  (midasi-list)
  "`midasi-list' の見出しを再変換する"
  (let (result-list)
    (dolist (midasi midasi-list)
      (let ((skk-henkan-key midasi))
	(setq result-list
	      (let ((kouho-list (cons skk-henkan-key (skk-search-server-1 nil nil))))
		(if result-list
		    (append result-list kouho-list)
		  kouho-list)))))
    result-list))


;; elisp 的には変数を使わずに、バッファを用意した方がよい？
(defvar skk-server-completion-advice-skk-comp-do-1-first t)
(defvar skk-server-completion-advice-skk-comp-do-1-miasi-list nil)

(defadvice skk-comp-do-1 (after skk-server-completion-advice-skk-comp-do-1 activate compile)
  (when first
    ;; 最初の問合せのとき初期化する
    ;; ここでサーバから取得しないのは、遅くなるのが嫌だから
    (setq skk-server-completion-advice-skk-comp-do-1-first t))
  ;; ユーザ辞書が全て無くなった時に初めて問い合わせる
  (unless ad-return-value
    (when skk-server-completion-advice-skk-comp-do-1-first
      (setq skk-server-completion-advice-skk-comp-do-1-first nil)
      (setq skk-server-completion-advice-skk-comp-do-1-miasi-list
	    (let ((midasi-list (skk-server-completion-search-midasi key)))
	      (if (string= (car midasi-list) key)
		  (cdr midasi-list)
		midasi-list))))
    ;; 見出しリストの頭を返す
    (setq ad-return-value (car skk-server-completion-advice-skk-comp-do-1-miasi-list))
    (setq skk-server-completion-advice-skk-comp-do-1-miasi-list
	  (cdr skk-server-completion-advice-skk-comp-do-1-miasi-list))))

(provide 'skk-server-completion)


;;; skk-server-completion.el ends here
