;;; skk-jisx0201-test.el --- skk-jisx0201のテスト -*- lexical-binding:t -*-

(require 'ert)
(require 'skk)
(require 'test-utils)

(skk-define-e2e-test skk-jisx0201/test1
  "`skk-use-jisx0201-input-method' が有効な場合、C-qで半角カナモードになる。"
  ;; `skk-use-jisx0201-input-method' はロード後に設定しても効果がないので
  ;; スキップする。
  (skip-when t)
  (actions (lambda () (setq-local skk-use-jisx0201-input-method t))
           #'skk-mode
           "q C-q k a n a")
  (expected-buffer-string "ｶﾅ"))

(provide 'skk-jisx0201-test)

;;; skk-jisx0201-test.el ends here
