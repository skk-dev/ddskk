;;; skk-isearch-test.el --- skk-isearchのテスト -*- lexical-binding:t -*-

(require 'ert)
(require 'skk)
(require 'test-utils)

(skk-define-e2e-test skk-isearch/test1
  "`skk-isearch-mode-enable' が t の場合、isearch 時にSKKモードになる。"
  ;; `skk-setup' が必要なのでスキップする。
  (skip-when t)
  (actions (lambda ()
             (setq-local skk-isearch-mode-enable t)
             (insert "あああいいい"))
           #'skk-mode
           "C-s i i i C-m u u u")
  (expected-buffer-string "あああいいいううう"))

(provide 'skk-isearch-test)

;;; skk-isearch-test.el ends here
