;;; test-utils.el --- テスト用便利関数 -*- lexical-binding:t -*-

(defmacro skk-define-e2e-test (name &rest args)
  "SKK用のE2Eテストを定義する。

テスト用の一時辞書を設定した上で一時バッファでキーボードマクロ等を実行し、
バッファの内容やユーザ辞書の内容を確認する。

実体としては `skk-e2e-test' の呼び出しとなる。

NAME はテストの名前となる。

ARGS はテストの詳細を定義する。第一要素はテストの docstring であり
省略可能である。
それ以降は alist であり、次のようなキーを持つ:

・skip-when: テストをスキップする条件。
・okuri-ari-entries: テスト用辞書の送り仮名有りエントリの行のリスト。
・okuri-nasi-entries: テスト用辞書の送り仮名無しエントリの行のリスト。
・actions: 一時バッファで実行するアクションのリスト。
           詳細は `skk-e2e-test' を参照。
・expected-okuri-ari-entries: 期待するユーザ辞書の送り仮名有りエントリの行のリスト。
・expected-okuri-nasi-entries: 期待するユーザ辞書の送り仮名無しエントリの行のリスト。
・expected-buffer-string: 期待するバッファの内容。

ユーザ辞書のチェックは expected-okuri-ari-entries か
expected-okuri-nasi-entries が alist に含まれるときのみ実施される。

バッファの内容のチェックは expected-buffer-string が non-nil であるときのみ
実施される。"
  (declare (indent defun))
  (let* ((docstring (car args))
         (have-skip-when (assoc 'skip-when args))
         (skip-when (cdr (assoc 'skip-when args)))
         (okuri-ari-entries (cdr (assoc 'okuri-ari-entries args)))
         (okuri-nasi-entries (cdr (assoc 'okuri-nasi-entries args)))
         (actions (cdr (assoc 'actions args)))
         (have-expected-jisyo (or (assoc 'expected-okuri-ari-entries args)
                                  (assoc 'expected-okuri-nasi-entries args)))
         (expected-okuri-ari-entries
          (cdr (assoc 'expected-okuri-ari-entries args)))
         (expected-okuri-nasi-entries
          (cdr (assoc 'expected-okuri-nasi-entries args)))
         (expected-buffer-string (cdr (assoc 'expected-buffer-string args)))
         body)
    ;; alist の . を忘れていた場合でも受け付ける。
    (when (consp expected-buffer-string)
      (setq expected-buffer-string (car expected-buffer-string)))
    (when (and (consp skip-when)
               (not (functionp (car skip-when)))
               (not (special-form-p (car skip-when)))
               (not (macrop (car skip-when))))
      (setq skip-when (car skip-when)))
    (setq body `(skk-e2e-test
                 (list ,@okuri-ari-entries)
                 (list ,@okuri-nasi-entries)
                 ,@actions
                 ,(when have-expected-jisyo
                    `(lambda ()
                       (skk-should-jisyo-equals-to
                        (list ,@expected-okuri-ari-entries)
                        (list ,@expected-okuri-nasi-entries))))
                 ,(when expected-buffer-string
                    `(lambda ()
                       (should (equal
                                (buffer-substring-no-properties (point-min)
                                                                (point-max))
                                ,expected-buffer-string))))))
    `(ert-deftest ,name ()
       ,@(when (stringp docstring)
           (list docstring))
       ;; Emacs 25 では `skip-unless' がサポートされないので、
       ;; 自前でスキップする。テスト成功として扱う。
       ,@(when (and have-skip-when (version<= "26" emacs-version))
           (list `(skip-unless (not ,skip-when))))
       ,(if (and have-skip-when (version< emacs-version "26"))
            `(unless ,skip-when
               ,body)
          body))))

(defun skk-e2e-test (okuri-ari-entries okuri-nasi-entries &rest actions)
  "テスト用の一時辞書を設定した上で一時バッファで ACTIONS を実行する。

ACTIONS は文字列・関数・その他のリストであり、順に次のように実行する:

・文字列の場合: キーボードマクロとして実行する。
  先に `kbd' を通してから処理する。
  例: \"K a n j i SPC C-j\"
・関数の場合: 引数無しで呼び出す。
・その他の場合: `eval' する。

OKURI-ARI-ENTRIES OKURI-NASI-ENTRIES は一時辞書の内容であり、
一時ファイルを作った上で `skk-large-jisyo' に一時的に設定する。
詳細は `skk-compose-test-dictionary' 参照。

また、 `skk-jisyo' も空の一時ファイルに設定する。
`skk-jisyo-code' は \"utf-8\" に設定する。

`skk-initial-search-jisyo', `skk-cdb-large-jisyo', `skk-aux-large-jisyo',
`skk-extra-jisyo-file-list' は nil とする。

一時バッファの内容を文字列として返すので簡単なテストであればそれが期待した値か
どうかチェックする。
複雑な状態を見るテストであれば ACTIONS に関数を渡してその中でチェックする。"
  (with-temp-buffer
    ;; キーボードマクロを実行する都合上、単にset-bufferするだけでなく、
    ;; ウィンドウのバッファを変える必要がある。
    (switch-to-buffer (current-buffer))
    (skk-with-temporary-files
     `(("skk-large-jisyo" . ,(skk-compose-test-dictionary
                              okuri-ari-entries
                              okuri-nasi-entries))
       ("skk-jisyo" . ""))
     (lambda (large-jisyo jisyo)
       (let* ((skk-initial-search-jisyo nil)
              (skk-large-jisyo (cons large-jisyo "UTF-8"))
              (skk-cdb-large-jisyo nil)
              (skk-aux-large-jisyo nil)
              (skk-extra-jisyo-file-list nil)
              (skk-jisyo jisyo)
              (skk-jisyo-code "utf-8")
              (skk-inhibit-ja-dic-search t)
              (skk-kakutei-count 0)
              (jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
         (dolist (action actions)
           (cond
            ((stringp action) (execute-kbd-macro (kbd action)))
            ((functionp action) (funcall action))
            (t (eval action))))
         (when jisyo-buffer
           (with-current-buffer jisyo-buffer
             (set-buffer-modified-p nil))
           (kill-buffer jisyo-buffer))
         (buffer-substring-no-properties (point-min) (point-max)))))))

(defun skk-should-jisyo-equals-to (okuri-ari-entries okuri-nasi-entries)
  "ユーザ辞書が期待された内容であることをアサートする。

OKURI-ARI-ENTRIES と OKURI-NASI-ENTRIES はそれぞれ送り仮名有りエントリと
送り仮名無しエントリの行のリストである。順序は保存される。"
  (should (equal
           (with-current-buffer (skk-get-jisyo-buffer skk-jisyo)
             (buffer-substring-no-properties (point-min) (point-max)))
           (skk-compose-test-dictionary okuri-ari-entries
                                        okuri-nasi-entries
                                        t))))

(defun skk-compose-test-dictionary
    (okuri-ari-entries okuri-nasi-entries &optional keep-order)
  "テスト用の辞書の内容を文字列として返す。

OKURI-ARI-ENTRIES と OKURI-NASI-ENTRIES は文字列のリストであり、各文字列は
辞書の行となる。
各文字列は改行を含んではいけない。

KEEP-ORDER が nil の場合、この関数は OKURI-ARI-ENTRIES と OKURI-NASI-ENTRIES
を破壊してソートする。"
  (unless keep-order
    (sort okuri-ari-entries (lambda (s1 s2) (string-lessp s2 s1)))
    (sort okuri-nasi-entries #'string-lessp))
  (concat
   (mapconcat
    #'identity
    (append
     '(";; okuri-ari entries.")
     okuri-ari-entries
     '(";; okuri-nasi entries.")
     okuri-nasi-entries)
    "\n")
   "\n"))

(defun skk-with-temporary-file (prefix content body)
  "一時ファイルを作成して BODY を呼び出す。

一時ファイルはリターンする前に削除する。

PREFIX は一時ファイル名のプレフィックスで、
CONTENT は一時ファイルの内容となる。"
  (let (file-name)
    (unwind-protect
        (progn
          (setq file-name
                (make-temp-file
                 (expand-file-name
                  prefix
                  (or small-temporary-file-directory
                      temporary-file-directory))))
          ;; Emacs 25 では `make-temp-file' でファイルの内容を指定できないので
          ;; 自前で書く。
          (with-temp-buffer
            (insert content)
            (write-region (point-min) (point-max) file-name))
          (funcall body file-name))
      (when file-name
        (delete-file file-name)))))

(defun skk-with-temporary-files (prefixes-and-contents body)
  "一時ファイルを複数作成して BODY を呼び出す。

PREFIXES-AND-CONTENTS は (PREFIX . CONTENT) のリストである。
PREFIX は一時ファイル名のプレフィックスで、
CONTENT は一時ファイルの内容となる。"
  (if prefixes-and-contents
      (skk-with-temporary-file
       (caar prefixes-and-contents)
       (cdar prefixes-and-contents)
       (lambda (file-name)
         (skk-with-temporary-files
          (cdr prefixes-and-contents)
          (lambda (&rest file-names)
            (apply body (cons file-name file-names))))))
    (funcall body)))

(provide 'test-utils)

;;; test-utils.el ends here
