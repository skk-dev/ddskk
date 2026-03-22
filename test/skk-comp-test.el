;;; skk-comp-test.el --- skk-compのテスト -*- lexical-binding:t -*-

(require 'ert)
(require 'skk)
(require 'test-utils)

(skk-define-e2e-test skk-comp/test1
  "TABを押すと補完する。
ただし、ユーザ辞書に入っている項目に限る。
M-SPCはTAB SPCの意味となる。"
  (okuri-nasi-entries "かんじ /漢字/")
  (actions #'skk-mode
           ;; 個人辞書にエントリを追加する。
           "K a n j i SPC C-j"
           ;; 補完して変換する。
           "K a TAB SPC C-j"
           ;; 補完して変換を M-SPC でまとめてやる。
           "K a M-SPC C-j")
  (expected-buffer-string "漢字漢字漢字"))

(skk-define-e2e-test skk-comp/test2
  "読みの補完時に.と,で候補を切り替えられる。"
  (okuri-nasi-entries "かんじ /漢字/"
                      "かんたん /簡単/")
  (actions #'skk-mode
           ;; 個人辞書に「漢字」を追加する。
           "K a n j i SPC C-j"
           ;; 個人辞書に「簡単」を追加する。
           "K a n t a n SPC C-j"
           ;; 2番目の候補を補完する。
           ;; 辞書を変更しないように変換はせずにそのまま確定する。
           "K a TAB . C-j"
           ;; 2番目の候補を表示したあと、1番目の候補に戻る。
           ;; 辞書を変更しないように変換はせずにそのまま確定する。
           "K a TAB . , C-j")
  (expected-buffer-string "漢字簡単かんじかんたん"))

(skk-define-e2e-test skk-comp/test3
  "abbrevモードでも補完できる。"
  (okuri-nasi-entries "alpha /α/")
  (actions #'skk-mode
           ;; 個人辞書にエントリを追加する。
           "/ a l p h a SPC C-j"
           ;; 補完して変換する。
           "/ a TAB SPC C-j")
  (expected-buffer-string "αα"))

(provide 'skk-comp-test)

;;; skk-comp-test.el ends here
