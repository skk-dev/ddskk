;;; dot.emacs --- SKK related customization in ~/.emacs  -*- mode: emacs-lisp; coding: iso-2022-jp -*-

;;; Commentary:

;; ~/.emacs に追加するための設定例です。

;;; Code:

;; @@ 基本の設定

;; カタカナ/ひらがな キーで SKK を起動する
(global-set-key [hiragana-katakana] 'skk-mode)

;; ~/.skk にいっぱい設定を書いているのでバイトコンパイルしたい
(setq skk-byte-compile-init-file t)
;; 注) 異なる種類の Emacsen を使っている場合は nil にします

;; SKK を Emacs の input method として使用する
(setq default-input-method "japanese-skk")

;; SKK を起動していなくても、いつでも skk-isearch を使う
(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
(add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)

;; @@ 応用的な設定

;; ~/.skk* なファイルがたくさんあるので整理したい
(if (not (file-directory-p "~/.ddskk"))
    (make-directory "~/.ddskk"))
(setq skk-init-file "~/.ddskk/init.el"
      skk-custom-file "~/.ddskk/custom.el"
      skk-emacs-id-file "~/.ddskk/emacs-id"
      skk-record-file "~/.ddskk/record"
      skk-jisyo "~/.ddskk/jisyo"
      skk-backup-jisyo "~/.ddskk/jisyo.bak")
;; 注) SKK の個人辞書は skkinput などのプログラムでも参照しますから、
;;     上記の設定をした場合はそれらのプログラムの設定ファイルも書き
;;     かえる必要があります。

;; migemo を使うから skk-isearch にはおとなしくしていて欲しい
(setq skk-isearch-start-mode 'latin)

;; super-smart-find のための設定 (意味あるかな？)
(setq super-smart-find-self-insert-command-list
      '(canna-self-insert-command
	egg-self-insert-command
	self-insert-command
	tcode-self-insert-command-maybe
	skk-insert))

;; YaTeX のときだけ句読点を変更したい
(add-hook 'yatex-mode-hook
	  (lambda ()
	    (require 'skk)
	    (setq skk-kutouten-type 'en)))

;; 文章系のバッファを開いた時には自動的に英数モード(「SKK」モード)に入る
(let ((function #'(lambda ()
		    (require 'skk)
		    (skk-latin-mode-on))))
  (dolist (hook '(find-file-hooks
		  ;; ...
		  mail-setup-hook
		  message-setup-hook))
    (add-hook hook function)))

;; Emacs 起動時に SKK を前もってロードする
(setq skk-preload t)
;; 注) skk.el をロードするだけなら (require 'skk) でもよい。上記設定の
;; 場合は、skk-search-prog-list に指定された辞書もこの時点で読み込んで
;; 準備する。Emacs の起動は遅くなるが，SKK を使い始めるときのレスポンス
;; が軽快になる。

;;; dot.emacs ends here
