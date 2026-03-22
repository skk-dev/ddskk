;;; skk-num-test.el --- skk-numのテスト -*- lexical-binding:t -*-

(require 'ert)
(require 'skk)
(require 'test-utils)

(skk-define-e2e-test skk-num/test1
  "数字を変換できる。"
  (okuri-nasi-entries "#えん /#3円/")
  (actions #'skk-mode
           "Q 1 2 3 e n SPC C-j")
  (expected-buffer-string "百二十三円"))

(provide 'skk-num-test)

;;; skk-num-test.el ends here
