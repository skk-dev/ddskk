;;; skk-auto-test.el --- skk-autoのテスト -*- lexical-binding:t -*-

(require 'ert)
(require 'skk)
(require 'test-utils)

(skk-define-e2e-test skk-auto/test1
  "`skk-auto-okuri-process' が non-nil の場合、送り仮名を自動で決定する。
ただし、ユーザ辞書に入っている項目に限る。"
  (okuri-ari-entries "うれs /嬉/")
  (actions #'skk-mode
           (lambda ()
             (setq-local skk-auto-okuri-process t))
           ;; 送り仮名を明示的に指定して変換し、ユーザ辞書に「嬉」を追加する。
           "U r e S i i RET"
           ;; 送り仮名を自動で決定させて変換する。
           "U r e s i i SPC C-j")
  (expected-buffer-string "嬉しい\n嬉しい"))

(provide 'skk-auto-test)

;;; skk-auto-test.el ends here
