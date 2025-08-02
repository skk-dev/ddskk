;;; context-skk-test.el --- context-skkのテスト -*- lexical-binding:t -*-

(require 'ert)
(require 'skk)
(require 'test-utils)

(skk-define-e2e-test context-skk/test1
  "`context-skk'を有効にした場合、コード中のコメントの中では日本語が入力され、
コメントの外では英字が入力される。"
  (actions #'emacs-lisp-mode
           #'skk-mode
           (lambda ()
             (require 'context-skk)
             (context-skk-mode 1))
           ;; コメント内。
           "; ; SPC C-j a a a RET"
           ;; コメント外。
           "a a a"
           (lambda () (context-skk-mode 0)))
  (expected-buffer-string ";; あああ\naaa"))

(provide 'context-skk-test)

;;; context-skk-test.el ends here
