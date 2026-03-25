(require 'ert)

(require 'skk)
(require 'skk-auto)
(require 'skk-cdb)
(require 'skk-comp)
(require 'skk-cus)
(require 'skk-tankan)
(require 'skk-version)
(require 'test-utils)

(ert-deftest skk-compute-henkan-lists/test1 ()
  (let ((fixtures
         '(("てんさい /転載/天災/天才/" nil
            (("転載" "天災" "天才") nil nil nil))
           ("なk /亡/無/鳴/泣/[く/無/鳴/泣/]/[き/亡/]/" "く"
            (("亡" "無" "鳴" "泣") ("[く") ("無" "鳴" "泣") ("]" "[き" "亡" "]"))))))
    (dolist (f fixtures)
      (let ((entry (car f))
            (okurigana (nth 1 f))
            (expected  (nth 2 f)))
        (with-temp-buffer
          (insert ";;\n")
          (insert entry)
          (insert "\n")
          (forward-line -1)
          (search-forward "/" nil 'noerror)
          (should (equal (skk-compute-henkan-lists okurigana)
                         expected)))))))

(skk-define-e2e-test basic-henkan/test1
  "送り有りと送り無しで変換できる。"
  (okuri-ari-entries "かんj /感/")
  (okuri-nasi-entries "かんじ /幹事/漢字/")
  (actions #'skk-mode
           ;; 送り仮名有り。
           "K a n J i C-j"
           ;; 送り仮名無し。
           "K a n j i SPC SPC C-j")
  (expected-buffer-string "感じ漢字"))

(skk-define-e2e-test user-jisyo/test1
  "変換後はユーザ辞書にエントリが追加される。"
  (okuri-ari-entries "かんj /感/")
  (okuri-nasi-entries "かんじ /幹事/漢字/")
  (actions #'skk-mode
           ;; 送り仮名有り。
           "K a n J i C-j"
           ;; 送り仮名無し。
           "K a n j i SPC SPC C-j")
  (expected-okuri-ari-entries "かんj /感/[じ/感/]/")
  (expected-okuri-nasi-entries "かんじ /漢字/"))

(skk-define-e2e-test skk-previous-candidate/test1
  "xで前の候補に戻れる。"
  (okuri-nasi-entries "かんじ /幹事/漢字/")
  (actions #'skk-mode
           "K a n j i SPC SPC x C-j")
  (expected-buffer-string "幹事"))

(skk-define-e2e-test skk-mode-off/test1
  "もう一度呼ぶとSKKモードを切れる。"
  (actions #'skk-mode
           #'skk-mode
           "a")
  (expected-buffer-string "a"))

(skk-define-e2e-test katakana/test1
  "qでカナモードになり、もう一度押すと戻る。

カナモードでは送り仮名もカタカナになる。"
  (okuri-ari-entries "かんj /感/")
  (actions #'skk-mode
           ;; カナモードへ。
           "q k a n a K a n J i C-j"
           ;; かなモードへ。
           "q k a n a K a n J i C-j")
  (expected-buffer-string "カナ感ジかな感じ"))

(skk-define-e2e-test skk-latin-mode/test1
  "lでアスキーモードになり、C-jで戻る。"
  (actions #'skk-mode
           ;; アスキーモードへ。
           "l a"
           ;; かなモードへ。
           "C-j a")
  (expected-buffer-string "aあ"))

(skk-define-e2e-test skk-jisx0208-latin-mode/test1
  "Lで全英モードになり、C-jで戻る。"
  (actions #'skk-mode
           ;; 全英モードへ。
           "L a"
           ;; かなモードへ。
           "C-j a")
  (expected-buffer-string "ａあ"))

(skk-define-e2e-test skk-set-henkan-point-subr/test1
  "Qを押すとそこから▽モードになる"
  (okuri-nasi-entries "かんじ /漢字/")
  (actions #'skk-mode
           "k a C-a Q C-e n j i SPC C-j")
  (expected-buffer-string "漢字"))

(skk-define-e2e-test quit-conversion/test1
  "▽モードでC-jで見出し語そのままで■モードに戻る。"
  (actions #'skk-mode
           "k a n a K a n j i C-j")
  (expected-buffer-string "かなかんじ"))

(skk-define-e2e-test quit-conversion/test2
  "▽モードでC-gで見出し語を消して■モードに戻る。"
  (actions #'skk-mode
           "k a n a K a n j i C-g")
  (expected-buffer-string "かな"))

(skk-define-e2e-test skk-henkan-in-minibuff/test1
  "送り仮名が無い単語を登録できる。"
  (okuri-nasi-entries "かん /漢/"
                      "じ /字/")
  (actions #'skk-mode
           "K a n j i SPC K a n SPC C-j J i SPC RET")
  (expected-okuri-nasi-entries "かんじ /漢字/"
                               "じ /字/"
                               "かん /漢/")
  (expected-buffer-string "漢字"))

(skk-define-e2e-test skk-henkan-in-minibuff/test2
  "送り仮名が有る単語を登録できる。"
  (okuri-nasi-entries "かん /感/")
  (actions #'skk-mode
           "K a n J i K a n SPC RET")
  (expected-okuri-ari-entries "かんj /感/[じ/感/]/")
  (expected-okuri-nasi-entries "かん /感/")
  (expected-buffer-string "感じ"))

(skk-define-e2e-test skk-henkan-in-minibuff/test3
  "辞書登録モードはC-gや、なにも入力しない状態のRETで抜けられる。"
  (actions #'skk-mode
           ;; C-gで抜ける。
           "K a n j i SPC a C-g C-j"
           ;; RETで抜ける。
           "K a n j i SPC RET C-j")
  (expected-buffer-string "かんじかんじ"))

(skk-define-e2e-test skk-henkan-in-minibuff/test4
  "辞書登録は再帰的にできる。"
  ;; なぜか通らない。要調査。
  (skip-when (version< emacs-version "29"))
  (okuri-nasi-entries "さい /再/"
                      "き /帰/"
                      "てき /的/")
  (actions #'skk-mode
           ;; ここはミニバッファに入るので、1つのキーボードマクロとして
           ;; 実行する必要があるため、concatで1つの文字列にする。
           (concat "S a i k i t e k i SPC "
                   "S a i k i SPC S a i SPC C-j K i SPC RET "
                   "T e k i SPC RET"))
  (expected-okuri-nasi-entries "さいきてき /再帰的/"
                               "てき /的/"
                               "さいき /再帰/"
                               "き /帰/"
                               "さい /再/")
  (expected-buffer-string "再帰的"))

(skk-define-e2e-test skk-henkan-in-minibuff/test5
  "C-q C-jで改行を含む文字列を辞書に登録できる。"
  (actions #'skk-mode
           ;; 登録。
           "A SPC a C-q C-j i RET"
           ;; みやすさのために改行を入れる。
           "RET"
           ;; もう一度変換。
           "A SPC C-j")
  (expected-okuri-nasi-entries "あ /(concat \"あ\\nい\")/")
  (expected-buffer-string "あ\nい\nあ\nい"))

(skk-define-e2e-test skk-henkan-in-minibuff/test6
  ".で強制的に辞書登録モードに入れる"
  ;; KNOWN-BUG ミニバッファに出ていた最初の候補が登録されるため、スキップする。
  (skip-when t)
  (okuri-nasi-entries "かんじ /幹事/換字/あ/い/う/え/お/か/き/く/け/こ/"
                      "かん /漢/"
                      "じ /字/")
  (actions #'skk-mode
           "K a n j i SPC SPC SPC SPC SPC . K a n SPC C-j J i SPC RET")
  (expected-okuri-nasi-entries "かんじ /漢字/"
                               "じ /字/"
                               "かん /漢/")
  (expected-buffer-string "漢字"))

(skk-define-e2e-test skk-purge-from-jisyo/test1
  "Xで個人辞書から項目を削除できる。"
  (okuri-nasi-entries "かんじ /漢字/")
  (actions #'skk-mode
           ;; 「あああ」を登録。
           "K a n j i SPC SPC a a a RET"
           ;; 削除。
           "K a n j i SPC X y e s RET"
           ;; もう一度変換。
           "K a n j i SPC C-j")
  (expected-okuri-nasi-entries "かんじ /漢字/")
  (expected-buffer-string "あああ漢字"))

(skk-define-e2e-test skk-toggle-characters/test1
  "▽モードでqで文字種を変更できる。"
  (actions #'skk-mode
           ;; ひらがなからカタカナへ。
           "Q k a n a q RET"
           ;; カタカナからひらがなへ。
           "q Q k a n a q RET"
           ;; アスキー文字から全英文字へ
           "l a b c C-j C-a Q C-e q RET"
           ;; 全英文字からアスキー文字へ
           "L a b c C-j C-a Q C-e q RET")
  (expected-buffer-string "カナ\nかな\nａｂｃ\nabc\n"))

(skk-define-e2e-test skk-process-prefix-or-suffix/test1
  ">で接頭辞・接尾辞を入力できる。"
  ;; なぜか通らない。要調査。
  (skip-when (version< emacs-version "29"))
  (okuri-nasi-entries "ぜん /全/"
                      "ぜん> /前/"
                      "じだい /時代/"
                      "てき /適/"
                      ">てき /的/")
  (actions #'skk-mode
           "Z e n > J i d a i SPC > t e k i SPC C-j")
  (expected-buffer-string "前時代的"))

(skk-define-e2e-test skk-abbrev-mode/test1
  "/で SKK abbrev モードに入る。"
  (okuri-nasi-entries "alpha /α/")
  (actions #'skk-mode
           "/ a l p h a SPC C-j")
  (expected-buffer-string "α"))

(skk-define-e2e-test skk-search-katakana/test1
  "`skk-search-katakana' が non-nil の場合、カタカナ語が変換候補となる。

個人辞書にも登録される。"
  (actions (lambda () (setq-local skk-search-katakana t))
           #'skk-mode
           "K a t a k a n a SPC C-j")
  (expected-okuri-nasi-entries "かたかな /カタカナ/")
  (expected-buffer-string "カタカナ"))

(skk-define-e2e-test skk-search-sagyo-henkaku/test1
  "`skk-search-sagyo-henkaku' が non-nil の場合、サ行変格活用の動詞を
送りあり変換できる。"
  (okuri-nasi-entries "へんかん /変換/")
  (actions (lambda () (setq-local skk-search-sagyo-henkaku t))
           #'skk-mode
           "H e n k a n S u C-j r u")
  (expected-buffer-string "変換する"))

(skk-define-e2e-test skk-kutouten-type/test1
  "`skk-kutouten-type' を設定すると句読点の文字を変えられる。"
  (actions #'skk-mode
           (lambda () (setq-local skk-kutouten-type 'jp))
           ". , RET"
           (lambda () (setq-local skk-kutouten-type 'en))
           ". , RET"
           (lambda () (setq-local skk-kutouten-type 'jp-en))
           ". , RET"
           (lambda () (setq-local skk-kutouten-type 'en-jp))
           ". , RET"
           (lambda () (setq-local skk-kutouten-type '("abc" . "def")))
           ". , RET")
  (expected-buffer-string "。、\n．，\n。，\n．、\nabcdef\n"))

(skk-define-e2e-test skk-backward-and-set-henkan-point/test1
  "M-Qでbackward側にある同種の文字を対象に▽モードとなる。"
  (okuri-nasi-entries "かんじ /漢字/")
  (actions #'skk-mode
           "q k a n a q k a n j i M-Q SPC C-j")
  (expected-buffer-string "カナ漢字"))

(skk-define-e2e-test skk-undo-kakutei/test1
  "`skk-undo-kakutei' で確定を取り消せる。"
  (okuri-nasi-entries "かんじ /幹事/漢字/")
  (actions #'skk-mode
           "k a n a K a n j i SPC C-j k a n a"
           #'skk-undo-kakutei
           "C-j")
  (expected-buffer-string "かな漢字かな"))

(skk-define-e2e-test skk-auto-start-henkan/test1
  "▽モードにおいて、「。」などを入力したときに自動的に変換が開始される。"
  (okuri-nasi-entries "かんじ /漢字/")
  (actions #'skk-mode
           "K a n j i . C-j")
  (expected-buffer-string "漢字。"))

(skk-define-e2e-test skk-kakutei-early/test1
  "▼モードにおいて、印字可能な文字を入力すると確定する。"
  (okuri-nasi-entries "かんじ /漢字/")
  (actions #'skk-mode
           "K a n j i SPC h a")
  (expected-buffer-string "漢字は"))

(skk-define-e2e-test skk-kakutei-when-unique-candidate/test1
  "`skk-kakutei-when-unique-candidate' が non-nil の場合、候補が1つの場合に
即座に確定する。"
  (okuri-nasi-entries "かんじ /漢字/")
  (actions #'skk-mode
           (lambda () (setq-local skk-kakutei-when-unique-candidate t))
           "K a n j i SPC")
  (expected-buffer-string "漢字"))

(skk-define-e2e-test skk-kakutei-jisyo/test1
  "`skk-kakutei-jisyo' にある単語は即座に確定する。"
  (okuri-nasi-entries "かんじ /漢字/幹事/")
  (actions #'skk-mode
           ;; 個人辞書に「漢字」を登録。
           "K a n j i SPC C-j"
           ;; 確定辞書として個人辞書を設定。
           (lambda () (setq-local skk-kakutei-jisyo skk-jisyo))
           ;; 確定変換。
           "K a n j i SPC")
  (expected-buffer-string "漢字漢字"))

(skk-define-e2e-test skk-kakutei-jisyo/test2
  "確定変換後にxで未確定状態に戻せる。"
  (okuri-nasi-entries "かんじ /幹事/漢字/")
  (actions #'skk-mode
           ;; 個人辞書に「幹事」を登録。
           "K a n j i SPC C-j"
           ;; 確定辞書として個人辞書を設定。
           (lambda () (setq-local skk-kakutei-jisyo skk-jisyo))
           ;; 確定変換後取り消して次の候補で確定。
           "K a n j i SPC x C-j")
  (expected-buffer-string "幹事漢字"))

(skk-define-e2e-test skk-henkan-okuri-strictly/test1
  "`skk-henkan-okuri-strictly' が non-nil の場合、送り仮名の厳密なマッチを
行う。"
  (okuri-ari-entries "おおk /大/多/")
  (actions #'skk-mode
           ;; 「大き」を登録。
           "O o K i C-j i RET"
           ;; 「多く」を登録。
           "O o K u SPC n o RET"
           ;; `skk-henkan-okuri-strictly' は辞書バッファで評価されるので、
           ;; ローカル変数として定義できない。グローバル変数を変更する。
           (lambda ()
             (setq-local skk-henkan-okuri-strictly-orig
                         skk-henkan-okuri-strictly)
             (setq skk-henkan-okuri-strictly t))
           ;; 再度「おお*き」を変換。「大き」が出るはず。
           "O o K i C-j i RET"
           ;; 再度「おお*き」を変換。
           ;; 「多き」は出ず、辞書登録モードになるはずなので抜ける。
           "O o K i SPC C-g C-g C-g"
           ;; `skk-henkan-okuri-strictly' を戻す。
           (lambda ()
             (setq skk-henkan-okuri-strictly skk-henkan-okuri-strictly-orig)))
  (expected-buffer-string "大きい\n多くの\n大きい\n"))

(skk-define-e2e-test skk-henkan-strict-okuri-precedence/test1
  "`skk-henkan-strict-okuri-precedence' が non-nil の場合、送り仮名が厳密に
マッチする候補を優先する。"
  (okuri-ari-entries "おおk /大/多/")
  (actions #'skk-mode
           ;; 「大き」を登録。
           "O o K i C-j i RET"
           ;; 「多く」を登録。
           "O o K u SPC n o RET"
           ;; `skk-henkan-strict-okuri-precedence' は辞書バッファで
           ;; 評価されるので、ローカル変数として定義できない。
           ;; グローバル変数を変更する。
           (lambda ()
             (setq-local skk-henkan-strict-okuri-precedence-orig
                         skk-henkan-strict-okuri-precedence)
             (setq skk-henkan-strict-okuri-precedence t))
           ;; 再度「おお*き」を変換。「大き」が出るはず。
           "O o K i C-j i RET"
           ;; 再度「おお*き」を変換。2番目の候補は「多き」となる。
           "O o K i SPC C-j RET"
           ;; `skk-henkan-strict-okuri-precedence' を戻す。
           (lambda ()
             (setq skk-henkan-strict-okuri-precedence
                   skk-henkan-strict-okuri-precedence-orig)))
  (expected-buffer-string "大きい\n多くの\n大きい\n多き\n"))

(skk-define-e2e-test skk-process-okuri-early/test1
  "`skk-process-okuri-early' が non-nil の場合、送り仮名のローマ字
プレフィックス入力時点で変換を開始する。"
  (okuri-ari-entries "おおk /大/多/")
  (actions (lambda () (setq-local skk-process-okuri-early t))
           #'skk-mode
           "O o K SPC u C-j")
  (expected-buffer-string "多く"))

(skk-define-e2e-test skk-jisyo-fix-order/test1
  "`skk-jisyo-fix-order' が non-nil の場合、個人辞書の同音語の順序を固定する。"
  (okuri-nasi-entries "かんじ /漢字/幹事/")
  (actions #'skk-mode
           ;; `skk-jisyo-fix-order' は辞書バッファで評価されるので、
           ;; ローカル変数として定義できない。
           ;; グローバル変数を変更する。
           (lambda ()
             (setq-local skk-jisyo-fix-order-orig skk-jisyo-fix-order)
             (setq skk-jisyo-fix-order t))
           ;; 「漢字」を個人辞書に追加する。
           "K a n j i SPC C-j"
           ;; 「幹事」を個人辞書に追加する。
           "K a n j i SPC SPC C-j"
           ;; もう一度変換するが、「漢字」が先に出るはず。
           "K a n j i SPC C-j"
           ;; `skk-jisyo-fix-order' を戻す。
           (lambda ()
             (setq skk-jisyo-fix-order skk-jisyo-fix-order-orig)))
  (expected-buffer-string "漢字幹事漢字"))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-test.el ends here
