;;; skk-sticky-test.el --- skk-stickyのテスト -*- lexical-binding:t -*-

(require 'ert)
(require 'skk)
(require 'test-utils)

(skk-define-e2e-test skk-sticky/test1
  "`skk-sticky-key' により指定したキーで変換開始および送り開始位置を
指定できる。"
  ;; ロード時に設定する必要があるためスキップする。
  (skip-when t)
  (okuri-ari-entries "かんj /感/")
  (actions (lambda () (setq-local skk-sticky-key ";"))
           #'skk-mode
           "; k a n ; j i C-j")
  (expected-buffer-string "感じ"))

(provide 'skk-sticky-test)

;;; skk-sticky-test.el ends here
