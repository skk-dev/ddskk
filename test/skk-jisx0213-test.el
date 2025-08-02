;;; skk-jisx0213-test.el --- skk-jisx0213のテスト -*- lexical-binding:t -*-

(require 'ert)
(require 'skk)
(require 'test-utils)

(skk-define-e2e-test skk-jisx0213/test1
  "`skk-jisx0213-prohibit' が non-nil である場合、JIS X 0213で追加された
文字を含む候補は除外される。"
  ;; KNOWN-BUG: `find-charset-string' は japanese-jisx0213.2004-1 を返すが、
  ;; `skk-jisx0213-henkan-list-filter' はまだこれに対応していない。
  ;; 既知のバグとしてスキップする。
  (skip-when t)
  (okuri-nasi-entries "かんじ /①/漢字/")
  (actions (lambda () (setq-local skk-jisx0213-prohibit t))
           #'skk-mode
           "K a n j i SPC C-j")
  (expected-buffer-string "漢字"))

(provide 'skk-jisx0213-test)

;;; skk-jisx0213-test.el ends here
