;;; skk-hint-test.el --- skk-hintのテスト -*- lexical-binding:t -*-

(require 'ert)
(require 'skk)
(require 'test-utils)

(skk-define-e2e-test skk-hint/test1
  "`skk-hint' を `require' した場合、;キーを使って候補を絞り込める。"
  (okuri-nasi-entries "かんじ /漢字/幹事/換字/"
                      "こと /事/")
  (actions (lambda () (require 'skk-hint))
           #'skk-mode
           ;; 「かんじ」の候補を「こと」で絞り込む。
           "K a n j i ; k o t o SPC C-j")
  (expected-buffer-string "幹事"))

(provide 'skk-hint-test)

;;; skk-hint-test.el ends here
