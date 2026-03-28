;;; skk-jisx0213-test.el --- Edge-case Tests for JIS X 0213 -*- lexical-binding: t; coding: utf-8-unix; -*-

(require 'ert)
(require 'skk)
(require 'test-utils)

(defun skk-test-setup-jisx0213 ()
  (setq-local skk-jisyo nil)
  (setq-local skk-save-jisyo nil)
  (setq-local skk-user-jisyo-file-name (make-temp-name "skk-test-jisyo"))
  (setq-local select-safe-coding-system-accept-default-p t)
  (set-buffer-file-coding-system 'utf-8-unix))

(skk-define-e2e-test skk-jisx0213/test-prohibit-complex
  "多様な 0213 文字（第3・第4水準、結合文字）が混在する中でのフィルタリング。"
  :entries ("てすと /①/𠮷/󠄀/A/テスト/") ; ①(記号), 𠮷(第4水準), 󠄀(結合文字) を排除対象に
  :actions ((lambda ()
              (skk-test-setup-jisx0213)
              (setq-local skk-jisx0213-prohibit t))
            skk-mode
            "t e s u t o SPC")
  ;; 0213 禁止なら、ASCII の "A" か、JIS X 0208 の「テスト」まで飛ぶはず
  :expect "A")

(skk-define-e2e-test skk-jisx0213/test-allow-complex
  "0213 許可時、サロゲートペア文字が正しく入力できること。"
  :entries ("よし /𠮷/吉/")
  :actions ((lambda ()
              (skk-test-setup-jisx0213)
              (setq-local skk-jisx0213-prohibit nil))
            skk-mode
            "y o s i SPC")
  :expect "𠮷")

(skk-define-e2e-test skk-jisx0213/test-mixed-annotation
  "注釈付きの 0213 候補も正しくフィルタリングされること。"
  :entries ("あ /①;まるいち/亜/")
  :actions ((lambda ()
              (skk-test-setup-jisx0213)
              (setq-local skk-jisx0213-prohibit t))
            skk-mode
            "a SPC")
  :expect "亜")

(provide 'skk-jisx0213-test)

;;; skk-jisx0213-test.el ends here
