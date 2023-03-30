;;; skk-vars.el --- common vars and consts in SKK -*- coding: iso-2022-7bit-ss2 -*-

;; Copyright (C) 1999-2010 SKK Development Team

;; Author: SKK Development Team
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'wid-edit)

(eval-when-compile
  ;; shut down compiler warnings.
  (defvar charset-list)
  (defvar word-across-newline)
  (defvar emacs-beta-version)
  (defvar mule-version))

(require 'skk)
(declare-function skk-lookup-get-content "skk-lookup.el")

;; Functions needed prior to loading skk-macs.el.
(defun skk-find-window-system ()
  (let ((frames (frame-list))
        val)
    (while (and (not val) frames)
      ;; 変数 window-system は frame local 値を持つ。
      ;; 例えば window system と "emacsclient -nw" の併用時など
      ;; いずれかの frame が window system 下で動いていることを
      ;; 確認する。
      (setq val (window-system (car frames))
            frames (cdr frames)))
    val))

(defun skk-jisyo (&optional coding)
  "CODING が nil であれば、個人辞書の PATH を返す (変数 `skk-jisyo' が文字列で
あればその値を、コンスセルであれば car を返す).
CODING が non-nil であれば、個人辞書に適用される CODING を返す (変数 `skk-jisyo' が
文字列であれば変数 `skk-jisyo-code' の値を、コンスセルであれば cdr を返す)."
  (let ((p (if (consp skk-jisyo) (car skk-jisyo) skk-jisyo))
        (c (if (consp skk-jisyo) (cdr skk-jisyo) skk-jisyo-code)))
    (if coding c p)))

;;;###autoload
(put 'skk-deflocalvar 'lisp-indent-function 'defun)
(defmacro skk-deflocalvar (symbol initvalue &optional docstring)
  (if docstring
      `(progn
         (defvar ,symbol ,initvalue ,docstring)
         (make-variable-buffer-local ',symbol))
    `(progn
       (defvar ,symbol ,initvalue)
       (make-variable-buffer-local ',symbol))))

;;;; Custom group definitions

(defgroup skk nil "日本語入力システム SKK のカスタマイズ"
  :group 'mule
  :group 'applications)

;;; by function
(defgroup skk-basic nil "SKK 基本の設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-dictionary nil "SKK 辞書の設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-private nil "SKK 個人辞書の取り扱い"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-input-basic nil "SKK 入力動作の基本設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-input-enhanced nil "SKK 入力動作の拡張設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-henkan nil "SKK 変換動作の設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-kakutei nil "SKK 変換候補確定動作の設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-okurigana nil "SKK 送りがなの取り扱い"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-misc nil "SKK その他いろいろ"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-visual nil "SKK の見た目をカスタマイズ"
  :prefix "skk-"
  :group 'skk)

;;; by filename
(defgroup skk-annotation nil "SKK アノテーション表示/編集の設定"
  :prefix "skk-annotation-"
  :group 'skk)

(defgroup skk-auto nil "SKK 自動送りがな機能の設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-cdb nil "SKK CDB 辞書検索機能の設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-comp nil "SKK 補完機能の設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-server-completion nil "辞書サーバ補完機能に関する設定"
  :group 'skk)

(defgroup skk-cursor nil "SKK カーソル制御の設定"
  :prefix "skk-cursor-"
  :group 'skk)

(defgroup skk-dcomp nil "SKK 動的補完の設定"
  :prefix "skk-dcomp-"
  :group 'skk)

(defgroup skk-gadget nil "SKK 実行変換機能 (gadget) の設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-hint nil "SKK ヒント付き変換機能の設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-isearch nil "SKK インクリメンタル・サーチの設定"
  :prefix "skk-isearch-"
  :group 'skk)

(defgroup skk-jisx0201 nil "SKK JIS X 0201 (おもに半角カナ) 関連の設定"
  :prefix "skk-jisx0201-"
  :group 'skk)

(defgroup skk-jisx0213 nil "SKK JIS X 0213 関連の設定"
  :group 'skk)

(defgroup skk-jisyo-edit-mode nil "SKK の辞書編集機能の設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-kakasi nil "SKK から kakasi を使う設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-kcode nil "SKK 文字コードを扱う設定"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-look nil "SKK から look コマンドを利用する設定"
  :prefix "skk-look-"
  :group 'skk)

(defgroup skk-lookup nil "SKK から Lookup パッケージを利用する設定"
  :prefix "skk-lookup-"
  :group 'skk)

(defgroup skk-num nil "SKK で数字を扱うための設定"
  :prefix "skk-num-"
  :group 'skk)

(defgroup skk-server nil "辞書サーバとの通信に関する設定"
  :prefix "skk-server-"
  :group 'skk)

(defgroup skk-sticky nil "SKK 変換位置指定方式の設定"
  :prefix "skk-sticky-"
  :group 'skk)

(defgroup skk-study nil "SKK 学習機能の設定"
  :prefix "skk-study-"
  :group 'skk)

(defgroup skk-tankan nil "SKK 単漢字変換機能の設定"
  :prefix "skk-tankan-"
  :group 'skk)

(defgroup skk-tooltip nil "SKK ツールティップ表示の設定"
  :prefix "skk-tooltip-"
  :group 'skk)

(defgroup skk-tut nil "SKK チュートリアルの設定"
  :prefix "skk-tut-"
  :group 'skk)

(defgroup skk-viper nil "SKK/Viper 関連の設定"
  :prefix "skk-viper-"
  :group 'skk)

(defgroup skk-act nil "SKK で拡張ローマ字入力 ACT を使う設定"
  :prefix "skk-act-"
  :group 'skk-input-enhanced)

(defgroup skk-azik nil "SKK で拡張ローマ字入力 AZIK を使う設定"
  :prefix "skk-azik-"
  :group 'skk-input-enhanced)

(defgroup skk-kanagaki nil "SKK かな入力の設定"
  :prefix "skk-kanagaki-"
  :group 'skk-input-enhanced)

(defgroup skk-nicola nil "SKK 親指シフト入力の設定"
  :prefix "skk-nicola-"
  :group 'skk-kanagaki)

;;; skk-vars.el related.
(defcustom skk-background-mode
  ;; from font-lock-make-faces of font-lock.el  Welcome!
  (or frame-background-mode
      (cond ((and window-system (x-display-color-p))
             (let ((bg-resource (x-get-resource ".backgroundMode"
                                                "BackgroundMode"))
                   (params (frame-parameters)))
               (cond (bg-resource
                      (intern (downcase bg-resource)))

                     ((and (eq system-type 'windows-nt)
                           (not (fboundp 'x-color-values)))
                      (if (string-match "light"
                                        (cdr (assq 'background-color params)))
                          'light
                        'dark))

                     ((not (null (cdr (assq 'background-mode params))))
                      ;; Emacs20.x (Meadow)
                      (cdr (assq 'background-mode params)))

                     ((< (apply '+ (x-color-values
                                    (cdr (assq 'background-color params))))
                         (/ (apply '+ (x-color-values "white")) 3))
                      'dark)

                     (t
                      'light))))
            (t
             nil)))
  "*SKK の標準のフェイス色を決めるための背景色に関する情報。
標準では `frame-background-mode' を設定している場合はそれに従い、
設定していない場合は独自の方法で `light' か `dark' かを決める。
ただし、ターミナルで Emacs を利用している場合は判定できず、
ユーザの意図と合わないかもしれないので、このオプションか
`frame-background-mode' をあらかじめ設定しておくことが望ましい。
このオプションは ~/.skk に設定しても反映されない。~/.emacs.d/init.el か
\\[customize] にて、SKK が読み込まれる前に設定することが必要。"
  :type '(choice (const dark)
                 (const light)
                 (const :tag "自動で決める" nil))
  :group 'skk-basic
  :group 'skk-visual)

;;; skk.el related.
(defcustom skk-user-directory nil
  "*SKK の設定ファイルなどを置くディレクトリ名。
各種設定ファイルをひとつのディレクトリにまとめたい場合に設定する。

  (例) (setq skk-user-directory \"~/.ddskk\")
"
  :type '(radio (directory :tag "ディレクトリ名" "~/.ddskk")
                (const :tag "設定しない" nil))
  :group 'skk-basic)

(defcustom skk-init-file (if skk-user-directory
                             (expand-file-name "init" skk-user-directory)
                           (convert-standard-filename "~/.skk"))
  "*SKK の初期設定を記述するファイル名。SKK を起動した最初の一度だけ読み
込まれる。このファイルに記述する代わりに ~/.emacs.d/init.el に SKK の各種初期設定を
記述することも可能だが、後者の場合は \\[skk-restart] では反映されない。

~/.emacs.d/init.el で 変数 `skk-byte-compile-init-file' を設定することで `skk-init-file' を
自動的にバイトコンパイルすることも可能。"
  ;;"*Name of the SKK initialization file.
  ;;From skk.el 9.x on all customization may be done in ~/.emacs."
  :type '(file :tag "ファイル名")
  :group 'skk-basic)

(defcustom skk-japanese-message-and-error nil
  "*Non-nil であれば、SKK のメッセージとエラーを日本語で表示する。
nil であれば、英語で表示する。"
  :type 'boolean
  :group 'skk-basic)

(defcustom skk-version-codename-ja nil
  "*Non-nil であれば、関数 `skk-version' でのコードネームを日本語で表示する。"
  :type 'boolean
  :group 'skk-basic)

(defcustom skk-jisyo-fix-order nil
  "*Non-nil であれば、確定の際に個人辞書の同音語の順序を変更せず、
個人辞書に新規追加する際は既出語の後に追加する。"
  :type 'boolean
  :group 'skk-dictionary)

(defcustom skk-kakutei-jisyo nil
  ;; ソートされている必要があるかどうかは設定次第だが、そこまで説明するのは面倒
  ;; (FILE . CODE) の形式もいけるはず (そのような設定のしかたは良くない?)
  "*「確定変換」で検索する辞書。
見出し語は、ソートされていなければならない。
Non-nil であれば、指定された辞書をバッファに読み込み、検索を行う。
各見出し語の最初のエントリで確定される。
確定アンドゥ時には 2 番目以降のエントリも利用できるが、
この仕様は変更される可能性もあり、また確定辞書の本質と無関係である。

関数 `skk-search-kakutei-jisyo-file' の引数として使用される。
確定変換機能を利用する場合には、
  (skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
のような要素を `skk-search-prog-list' の先頭に配置すること。"
  ;;  "*The first dictionary to be searched.
  ;;If non-nil, and this variable is used as a component of
  ;;`skk-search-prog-list', the indicated dictionary is read into a
  ;;buffer and searched.
  ;;The keys must be sorted.
  ;;Only the first entry in each key is checked; if several entries are
  ;;present the second and following entries are ignored.
  ;;By setting the value of `skk-search-prog-list' the dictionaries
  ;;searched and the order of search can be changed."
  :type '(radio (file :tag "辞書ファイル名")
                (const :tag "指定しない" nil))
  :group 'skk-dictionary)

(defcustom skk-initial-search-jisyo nil
  ;; ソートされている必要があるかどうかは設定次第だが、そこまで説明するのは面倒
  ;; (FILE . CODE) の形式もいけるはず
  "*個人辞書の検索の前に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nil であれば、指定された辞書を検索のためバッファに読み込み、検索を行う。

`skk-search-prog-list' において、
  (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
のような要素が
  (skk-search-jisyo-file skk-jisyo 0 t)
より先に配置されている事によりその意味を成している。"
  ;;  "*This dictionary is searched before the user's personal dictionary.
  ;;The keys must be sorted.
  ;;If non-nil, and this variable is used as a component of
  ;;`skk-search-prog-list', the indicated dictionary is read into a
  ;;buffer and searched.
  ;;By setting the value of `skk-search-prog-list' the dictionaries
  ;;searched and the order of search can be changed."
  :type '(radio (file :tag "辞書ファイル名")
                (const :tag "指定しない" nil))
  :group 'skk-dictionary)

(defcustom skk-large-jisyo nil
  ;; (FILE . CODE) の形式もいけるはず
  "*個人辞書の検索の後に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nil であれば、指定された辞書を検索のためバッファに読み込み、検索を行う。"
  :type `(radio (file :tag "辞書ファイル名"
                      ,(or (locate-file "skk/SKK-JISYO.L"
                                        (list (expand-file-name "../../.."
                                                                data-directory)))
                           (locate-file "skk/SKK-JISYO.L"
                                        (list data-directory))
                           ""))
                (const :tag "指定しない" nil))
  :group 'skk-dictionary)

(defcustom skk-aux-large-jisyo nil
  ;; (FILE . CODE) の形式もいけるはず
  "*辞書サーバが使えない時に、代わりに検索する辞書。
見出し語は、ソートされていなければならない。
Non-nil であれば、辞書サーバが active でない時に、
指定された辞書をバッファに読み込み、検索を行う。"
  :type `(radio (file :tag "辞書ファイル名"
                      ,(or (locate-file "skk/SKK-JISYO.L"
                                        (list (expand-file-name "../../.."
                                                                data-directory)))
                           (locate-file "skk/SKK-JISYO.L"
                                        (list data-directory))
                           ""))
                (const :tag "指定しない" nil))
  :group 'skk-dictionary
  :group 'skk-server)

(defcustom skk-inhibit-ja-dic-search nil
  "*「GNU Emacs 付属の辞書を用いた検索」の禁止を指示するオプション。
GNU Emacs には SKK-JISYO.L を元に変換された ja-dic.el という辞書が付属する。
これを用いて通常のかな漢字変換 (送りあり、送りなし、接頭辞、接尾辞) が可能
である (ただし SKK-JISYO.L による英数変換、数値変換などはできない)。
DDSKK 14.2 より「ja-dic.el 検索機能 `skk-search-ja-dic'」が追加された。
この `skk-search-ja-dic' は、 `skk-large-jisyo'、`skk-aux-large-jisyo'、
`skk-cdb-large-jisyo' 及び `skk-server-host' の全てが無効な場合に有効となる
が、あらゆる場面で禁止したい場合は、この変数を Non-nil に設定する。"
  :type 'boolean
  :group 'skk-dictionary)

(defcustom skk-extra-jisyo-file-list nil
  "*メイン辞書の他に検索する辞書のリストを指定する。
いずれの辞書も、見出し語はソートされていなければならない。

  (setq skk-extra-jisyo-file-list
        (list \\='(\"/usr/share/skk/SKK-JISYO.JIS3_4\" . euc-jisx0213)
             \"/usr/share/skk/SKK-JISYO.zipcode\"))

SKK 辞書には SKK OpenLab で配布しているもの、第三者によるものなど多数あるが、
メイン辞書 (SKK-JISYO.L や辞書サーバなど) の他に検索したい辞書のファイル名の
リストを指定する。ファイル名の代わりに、ファイル名とコード系のペアを指定する
こともできる。辞書は指定された順に検索される。"
  :type '(repeat (file :tag "辞書ファイル名"))
  :group 'skk-dictionary)

(defcustom skk-itaiji-jisyo nil
  "異体字辞書 `SKK-JISYO.itaiji', `SKK-JISYO.itaiji.JIS3_4' へのパスを指定する。"
  :type '(radio (file :tag "辞書ファイル名")
                (const :tag "指定しない" nil))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list
  '((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
    (skk-tankan-search 'skk-search-jisyo-file skk-large-jisyo 10000)
    (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
    (skk-search-jisyo-file skk-jisyo 0 t)
    (skk-okuri-search)
    (skk-search-cdb-jisyo skk-cdb-large-jisyo)
    (skk-search-jisyo-file skk-large-jisyo 10000)
    (skk-search-server skk-aux-large-jisyo 10000)
    (skk-search-ja-dic-maybe)
    (skk-search-extra-jisyo-files)
    (skk-search-katakana-maybe)
    (skk-search-sagyo-henkaku-maybe)
    (skk-search-itaiji))
  "*検索関数、検索対象の辞書を決定するためのリスト。

この変数の値を手動で変更すると、SKK の動作に影響する可能性があるので注意を要する。

変換した候補を返す S 式をリストの形に表記したもの。
関数 `skk-search' が `skk-search-prog-list' の car から後方向へ順番に S 式を
評価することによってかな漢字変換を実行する。

必要に応じて
  (skk-okuri-search)
  (skk-look)
  (skk-search-server skk-aux-large-jisyo 10000)
これらのプログラム（要素）が自動的に追加される。"
  :type '(repeat (sexp :tag "S式"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-1 nil
  "*検索関数、検索対象の辞書を決定するためのリスト。
C-1 SPC で使用される"
  :type '(repeat (sexp :tag "S式"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-2 nil
  "*検索関数、検索対象の辞書を決定するためのリスト。
C-2 SPC で使用される"
  :type '(repeat (sexp :tag "S式"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-3 nil
  "*検索関数、検索対象の辞書を決定するためのリスト。
C-3 SPC で使用される"
  :type '(repeat (sexp :tag "S式"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-4 nil
  "*検索関数、検索対象の辞書を決定するためのリスト。
C-4 SPC で使用される"
  :type '(repeat (sexp :tag "S式"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-5 '((skk-search-tankanji))
  "*検索関数、検索対象の辞書を決定するためのリスト。
C-5 SPC で使用される"
  :type '(repeat (sexp :tag "S式"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-6 '((skk-search-identity))
  "*検索関数、検索対象の辞書を決定するためのリスト。
C-6 SPC で使用される"
  :type '(repeat (sexp :tag "S式"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-7 '((skk-search-katakana))
  "*検索関数、検索対象の辞書を決定するためのリスト。
C-7 SPC で使用される"
  :type '(repeat (sexp :tag "S式"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-8 '((skk-search-hankaku-katakana))
  "*検索関数、検索対象の辞書を決定するためのリスト。
C-8 SPC で使用される"
  :type '(repeat (sexp :tag "S式"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-9 '((skk-search-jisx0208-romaji))
  "*検索関数、検索対象の辞書を決定するためのリスト。
C-9 SPC で使用される"
  :type '(repeat (sexp :tag "S式"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-0 '((skk-search-romaji))
  "*検索関数、検索対象の辞書を決定するためのリスト。
C-0 SPC で使用される"
  :type '(repeat (sexp :tag "S式"))
  :group 'skk-dictionary)

(defcustom skk-count-jisyo-candidates-function
  'skk-count-jisyo-candidates-original
  "*`skk-count-jisyo-candidates' で使用する関数。"
  :type 'function
  :group 'skk-dictionary)

(defcustom skk-public-jisyo-to-be-searched-function
  'skk-public-jisyo-to-be-searched-original
  "*`skk-public-jisyo-has-word-p' で使用する関数。"
  :type 'function
  :group 'skk-dictionary)

(defcustom skk-jisyo (if skk-user-directory
                         (expand-file-name "jisyo" skk-user-directory)
                       (convert-standard-filename "~/.skk-jisyo"))
  "*SKK の個人辞書。"
  :type '(radio (file :tag "辞書ファイル名")
                (cons :tag "`skk-jisyo-code' と異なる文字コードを使用する場合"
                      (file :tag "PATH/TO/FILE")
                      (coding-system :tag "CODING-SYSTEM-NAME")))
  :group 'skk-private)

(defcustom skk-backup-jisyo (if skk-user-directory
                                (expand-file-name "jisyo.bak"
                                                  skk-user-directory)
                              (convert-standard-filename "~/.skk-jisyo.BAK"))
  "*SKK の個人辞書のバックアップファイル。"
  :type '(file :tag "辞書ファイル名")
  :group 'skk-private)

(defcustom skk-jisyo-code nil
  ;; 現在の実装にべったりな説明は良くないかも
  "*辞書バッファのコーディングシステム。
基本的には coding system 名を指定する。
文字列 \"euc\", \"ujis\", \"sjis\", \"jis\" の指定も受け付ける (`skk-coding-system-alist')。
デフォルトは nil であり、辞書バッファのコーディングシステムは euc-jis-2004 となる (`skk-find-coding-system')。
個人辞書もこのコーディングシステムで保存される。"
  :type '(radio coding-system
                (radio :tag "コードの通称"
                       (const "euc")
                       (const "ujis")
                       (const "sjis")
                       (const "jis"))
                (const :tag "指定しない" nil))
  :group 'skk-private)

(defcustom skk-share-private-jisyo nil "\
*Non-nil であれば、個人辞書を更新する際に「複数の SKK プロセスが特定の個
人辞書を共有している」を考慮した上で処理を行う。
SKK 起動後にこの変数の値を変更した場合は \\[skk-restart] で反映させる事。"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-private)

(defcustom skk-jisyo-save-count 50
  "*数値であれば、その回数だけ個人辞書が更新されたときに自動的にセーブする。
nil であれば、個人辞書のオートセーブを行わない。
SKK 起動後で、変数 `skk-share-private-jisyo' が non-nil な場合
に `skk-jisyo-save-count' の値を変更した場合は
\\[skk-restart] で反映させる事。"
  :type '(radio (integer :tag "整数" 50)
                (const :tag "指定しない" nil))
  :group 'skk-private)

(defcustom skk-count-private-jisyo-candidates-exactly nil
  "*Non-nil であれば、Emacs を終了するときに `skk-record-file' に保存され
る統計情報の「語数」を正確に数える。
nil であれば、1 行に複数の候補があっても 1 候補として数える。"
  :type 'boolean
  :group 'skk-private)

(defcustom skk-compare-jisyo-size-when-saving t
  "*Non-nil であれば、`skk-jisyo' のセーブ時にファイルサイズをチェックする。
前回セーブした `skk-jisyo' と今回セーブしようとする辞書とのサイズを比較し、
後者の方が大きいときにユーザーにセーブを続けるかどうかの確認を求める。"
  :type 'boolean
  :group 'skk-private)

(defcustom skk-search-excluding-word-pattern-function nil
  "*「“個人辞書に取り込まない文字列のパターン”を検索する条件」を指定する。
この変数には、引数１個の関数（又は関数のリスト）を代入する。
代入した関数は、確定した文字列を引数に `skk-update-jisyo-p' 内で `funcall' される。
この変数のデフォルトは nil であるため、関数 `skk-update-jisyo-p' は t を返す。

基本的に、この変数はフック変数であり、その値を設定したい場合には `add-hook'
で追加するか `remove-hook' で削除する。

SKK では、かな漢字変換・確定を行った文字列は全て個人辞書に取り込まれるが、
この変数で指定された関数が non-nil を返すと、その文字列は個人辞書に取り込
まれない。

例えば、この変数に下記のような lambda 関数を指定すると、かな漢字変換によ
って (SKK abbrev mode での変換を除く) カタカナのみから成る文字列を得て確
定しても、それを個人辞書に取り込まない。

 (add-hook \\='skk-search-excluding-word-pattern-function
       (lambda (kakutei-word)
           ;; この関数が non-nil を返したときは、その文字列は個人
           ;; 辞書に取り込まれない。
           (and
            ;; 送りなし変換で、
            (not skk-okuri-char)
            ;; 確定語がカタカナのみから構成されていて、
            (string-match \"^[ーァ-ン]+$\" kakutei-word)
            ;; SKK abbrev mode 以外での変換か、
            (or (not skk-abbrev-mode)
            ;; 見出し語がカタカナ、ひらがな以外のとき。
            ;; (後で▽マークを付けたときは、見出し語が英文字でも、
            ;; skk-abbrev-modeが t になっていない)。
            (not (string-match \"^[^ーァ-ンぁ-ん]+$\"
                                       skk-henkan-key))))))

「かな漢字変換によってカタカナを求めたいが、個人辞書にはカタカナのみの候
補を取り込みたくない」など、個人辞書が必要以上に膨れるのを抑える目的に使
用できる。

なお、個人辞書に取り込まない見出し語については、補完が効かないので注意す
ること。"
  :type 'hook
  :group 'skk-private)

(defcustom skk-update-jisyo-function 'skk-update-jisyo-original
  "*この変数が指す関数は、関数 `skk-update-jisyo' にて funcall で実行される。"
  :type 'function
  :group 'skk-private)

(defcustom skk-save-jisyo-function 'skk-save-jisyo-original
  "*この変数が指す関数は、 関数 `skk-save-jisyo' にて funcall で実行される。"
  :type 'function
  :group 'skk-private)

(defcustom skk-update-end-function nil
  "*個人辞書の更新終了時にコールされる関数。
HENKAN-BUFFER, MIDASI, OKURIGANA, WORD, PURGE の 5 引数を伴なってコールされる。
この関数は、辞書バッファでコールされるので、変換を行ったバッファローカルな
情報を取り出したいときは、HENKAN-BUFFER を利用する。
`skk-kakutei-initialize' がコールされる前にこの関数がコールされるので、最後の
確定に関するフラグ類は、この関数の中から参照することができる。"
  :type '(list symbol)
  :group 'skk-private)

(defcustom skk-learn-combined-word nil
  "*接頭辞、接尾辞の入力の結果を自動的に学習するかどうかを設定する。
Non-nil ならば、接頭辞または接尾辞入力の際、接頭辞または接尾辞と結合した
語を自動的に学習する。"
  :type 'boolean
  :group 'skk-private)

(defcustom skk-save-jisyo-instantly nil
  "*non-nil であれば、単語登録（単語削除）の都度、個人辞書を保存する。"
  :type 'boolean
  :group 'skk-private)

(defvar skk-jisyo-updated nil
  "`skk-henkan-in-minibuff' (単語登録) されれば t となる。
`skk-update-jisyo' で参照している。")

(defcustom skk-rom-kana-base-rule-list
  '(("a" nil ("ア" . "あ"))
    ("bb" "b" ("ッ" . "っ"))
    ("ba" nil ("バ" . "ば"))
    ("be" nil ("ベ" . "べ"))
    ("bi" nil ("ビ" . "び"))
    ("bo" nil ("ボ" . "ぼ"))
    ("bu" nil ("ブ" . "ぶ"))
    ("bya" nil ("ビャ" . "びゃ"))
    ("bye" nil ("ビェ" . "びぇ"))
    ("byi" nil ("ビィ" . "びぃ"))
    ("byo" nil ("ビョ" . "びょ"))
    ("byu" nil ("ビュ" . "びゅ"))
    ("cc" "c" ("ッ" . "っ"))
    ("cha" nil ("チャ" . "ちゃ"))
    ("che" nil ("チェ" . "ちぇ"))
    ("chi" nil ("チ" . "ち"))
    ("cho" nil ("チョ" . "ちょ"))
    ("chu" nil ("チュ" . "ちゅ"))
    ("cya" nil ("チャ" . "ちゃ"))
    ("cye" nil ("チェ" . "ちぇ"))
    ("cyi" nil ("チィ" . "ちぃ"))
    ("cyo" nil ("チョ" . "ちょ"))
    ("cyu" nil ("チュ" . "ちゅ"))
    ("dd" "d" ("ッ" . "っ"))
    ("da" nil ("ダ" . "だ"))
    ("de" nil ("デ" . "で"))
    ("dha" nil ("デャ" . "でゃ"))
    ("dhe" nil ("デェ" . "でぇ"))
    ("dhi" nil ("ディ" . "でぃ"))
    ("dho" nil ("デョ" . "でょ"))
    ("dhu" nil ("デュ" . "でゅ"))
    ("di" nil ("ヂ" . "ぢ"))
    ("do" nil ("ド" . "ど"))
    ("du" nil ("ヅ" . "づ"))
    ("dya" nil ("ヂャ" . "ぢゃ"))
    ("dye" nil ("ヂェ" . "ぢぇ"))
    ("dyi" nil ("ヂィ" . "ぢぃ"))
    ("dyo" nil ("ヂョ" . "ぢょ"))
    ("dyu" nil ("ヂュ" . "ぢゅ"))
    ("e" nil ("エ" . "え"))
    ("ff" "f" ("ッ" . "っ"))
    ("fa" nil ("ファ" . "ふぁ"))
    ("fe" nil ("フェ" . "ふぇ"))
    ("fi" nil ("フィ" . "ふぃ"))
    ("fo" nil ("フォ" . "ふぉ"))
    ("fu" nil ("フ" . "ふ"))
    ("fya" nil ("フャ" . "ふゃ"))
    ("fye" nil ("フェ" . "ふぇ"))
    ("fyi" nil ("フィ" . "ふぃ"))
    ("fyo" nil ("フョ" . "ふょ"))
    ("fyu" nil ("フュ" . "ふゅ"))
    ("gg" "g" ("ッ" . "っ"))
    ("ga" nil ("ガ" . "が"))
    ("ge" nil ("ゲ" . "げ"))
    ("gi" nil ("ギ" . "ぎ"))
    ("go" nil ("ゴ" . "ご"))
    ("gu" nil ("グ" . "ぐ"))
    ("gya" nil ("ギャ" . "ぎゃ"))
    ("gye" nil ("ギェ" . "ぎぇ"))
    ("gyi" nil ("ギィ" . "ぎぃ"))
    ("gyo" nil ("ギョ" . "ぎょ"))
    ("gyu" nil ("ギュ" . "ぎゅ"))
    ;;("h" "" ("オ" . "お"))
    ("ha" nil ("ハ" . "は"))
    ("he" nil ("ヘ" . "へ"))
    ("hi" nil ("ヒ" . "ひ"))
    ("ho" nil ("ホ" . "ほ"))
    ("hu" nil ("フ" . "ふ"))
    ("hya" nil ("ヒャ" . "ひゃ"))
    ("hye" nil ("ヒェ" . "ひぇ"))
    ("hyi" nil ("ヒィ" . "ひぃ"))
    ("hyo" nil ("ヒョ" . "ひょ"))
    ("hyu" nil ("ヒュ" . "ひゅ"))
    ("i" nil ("イ" . "い"))
    ("jj" "j" ("ッ" . "っ"))
    ("ja" nil ("ジャ" . "じゃ"))
    ("je" nil ("ジェ" . "じぇ"))
    ("ji" nil ("ジ" . "じ"))
    ("jo" nil ("ジョ" . "じょ"))
    ("ju" nil ("ジュ" . "じゅ"))
    ("jya" nil ("ジャ" . "じゃ"))
    ("jye" nil ("ジェ" . "じぇ"))
    ("jyi" nil ("ジィ" . "じぃ"))
    ("jyo" nil ("ジョ" . "じょ"))
    ("jyu" nil ("ジュ" . "じゅ"))
    ("kk" "k" ("ッ" . "っ"))
    ("ka" nil ("カ" . "か"))
    ("ke" nil ("ケ" . "け"))
    ("ki" nil ("キ" . "き"))
    ("ko" nil ("コ" . "こ"))
    ("ku" nil ("ク" . "く"))
    ("kya" nil ("キャ" . "きゃ"))
    ("kye" nil ("キェ" . "きぇ"))
    ("kyi" nil ("キィ" . "きぃ"))
    ("kyo" nil ("キョ" . "きょ"))
    ("kyu" nil ("キュ" . "きゅ"))
    ("ma" nil ("マ" . "ま"))
    ("me" nil ("メ" . "め"))
    ("mi" nil ("ミ" . "み"))
    ("mo" nil ("モ" . "も"))
    ("mu" nil ("ム" . "む"))
    ("mya" nil ("ミャ" . "みゃ"))
    ("mye" nil ("ミェ" . "みぇ"))
    ("myi" nil ("ミィ" . "みぃ"))
    ("myo" nil ("ミョ" . "みょ"))
    ("myu" nil ("ミュ" . "みゅ"))
    ("n" nil ("ン" . "ん"))
    ("n'" nil ("ン" . "ん"))
    ("na" nil ("ナ" . "な"))
    ("ne" nil ("ネ" . "ね"))
    ("ni" nil ("ニ" . "に"))
    ("nn" nil ("ン" . "ん"))
    ("no" nil ("ノ" . "の"))
    ("nu" nil ("ヌ" . "ぬ"))
    ("nya" nil ("ニャ" . "にゃ"))
    ("nye" nil ("ニェ" . "にぇ"))
    ("nyi" nil ("ニィ" . "にぃ"))
    ("nyo" nil ("ニョ" . "にょ"))
    ("nyu" nil ("ニュ" . "にゅ"))
    ("o" nil ("オ" . "お"))
    ("pp" "p" ("ッ" . "っ"))
    ("pa" nil ("パ" . "ぱ"))
    ("pe" nil ("ペ" . "ぺ"))
    ("pi" nil ("ピ" . "ぴ"))
    ("po" nil ("ポ" . "ぽ"))
    ("pu" nil ("プ" . "ぷ"))
    ("pya" nil ("ピャ" . "ぴゃ"))
    ("pye" nil ("ピェ" . "ぴぇ"))
    ("pyi" nil ("ピィ" . "ぴぃ"))
    ("pyo" nil ("ピョ" . "ぴょ"))
    ("pyu" nil ("ピュ" . "ぴゅ"))
    ("rr" "r" ("ッ" . "っ"))
    ("ra" nil ("ラ" . "ら"))
    ("re" nil ("レ" . "れ"))
    ("ri" nil ("リ" . "り"))
    ("ro" nil ("ロ" . "ろ"))
    ("ru" nil ("ル" . "る"))
    ("rya" nil ("リャ" . "りゃ"))
    ("rye" nil ("リェ" . "りぇ"))
    ("ryi" nil ("リィ" . "りぃ"))
    ("ryo" nil ("リョ" . "りょ"))
    ("ryu" nil ("リュ" . "りゅ"))
    ("ss" "s" ("ッ" . "っ"))
    ("sa" nil ("サ" . "さ"))
    ("se" nil ("セ" . "せ"))
    ("sha" nil ("シャ" . "しゃ"))
    ("she" nil ("シェ" . "しぇ"))
    ("shi" nil ("シ" . "し"))
    ("sho" nil ("ショ" . "しょ"))
    ("shu" nil ("シュ" . "しゅ"))
    ("si" nil ("シ" . "し"))
    ("so" nil ("ソ" . "そ"))
    ("su" nil ("ス" . "す"))
    ("sya" nil ("シャ" . "しゃ"))
    ("sye" nil ("シェ" . "しぇ"))
    ("syi" nil ("シィ" . "しぃ"))
    ("syo" nil ("ショ" . "しょ"))
    ("syu" nil ("シュ" . "しゅ"))
    ("tt" "t" ("ッ" . "っ"))
    ("ta" nil ("タ" . "た"))
    ("te" nil ("テ" . "て"))
    ("tha" nil ("テァ" . "てぁ"))
    ("the" nil ("テェ" . "てぇ"))
    ("thi" nil ("ティ" . "てぃ"))
    ("tho" nil ("テョ" . "てょ"))
    ("thu" nil ("テュ" . "てゅ"))
    ("ti" nil ("チ" . "ち"))
    ("to" nil ("ト" . "と"))
    ("tsu" nil ("ツ" . "つ"))
    ("tu" nil ("ツ" . "つ"))
    ("tya" nil ("チャ" . "ちゃ"))
    ("tye" nil ("チェ" . "ちぇ"))
    ("tyi" nil ("チィ" . "ちぃ"))
    ("tyo" nil ("チョ" . "ちょ"))
    ("tyu" nil ("チュ" . "ちゅ"))
    ("u" nil ("ウ" . "う"))
    ("vv" "v" ("ッ" . "っ"))
    ("va" nil ("ヴァ" . "う゛ぁ"))
    ("ve" nil ("ヴェ" . "う゛ぇ"))
    ("vi" nil ("ヴィ" . "う゛ぃ"))
    ("vo" nil ("ヴォ" . "う゛ぉ"))
    ("vu" nil ("ヴ" . "う゛"))
    ("ww" "w" ("ッ" . "っ"))
    ("wa" nil ("ワ" . "わ"))
    ("we" nil ("ウェ" . "うぇ"))
    ("wi" nil ("ウィ" . "うぃ"))
    ("wo" nil ("ヲ" . "を"))
    ("wu" nil ("ウ" . "う"))
    ("xx" "x" ("ッ" . "っ"))
    ("xa" nil ("ァ" . "ぁ"))
    ("xe" nil ("ェ" . "ぇ"))
    ("xi" nil ("ィ" . "ぃ"))
    ("xka" nil ("ヵ" . "か"))
    ("xke" nil ("ヶ" . "け"))
    ("xo" nil ("ォ" . "ぉ"))
    ("xtsu" nil ("ッ" . "っ"))
    ("xtu" nil ("ッ" . "っ"))
    ("xu" nil ("ゥ" . "ぅ"))
    ("xwa" nil ("ヮ" . "ゎ"))
    ("xwe" nil ("ヱ" . "ゑ"))
    ("xwi" nil ("ヰ" . "ゐ"))
    ("xya" nil ("ャ" . "ゃ"))
    ("xyo" nil ("ョ" . "ょ"))
    ("xyu" nil ("ュ" . "ゅ"))
    ("yy" "y" ("ッ" . "っ"))
    ("ya" nil ("ヤ" . "や"))
    ("ye" nil ("イェ" . "いぇ"))
    ("yo" nil ("ヨ" . "よ"))
    ("yu" nil ("ユ" . "ゆ"))
    ("zz" "z" ("ッ" . "っ"))
    ("z " nil "　")
    ("z*" nil "※")
    ("z," nil "‥")
    ("z-" nil "～")
    ("z." nil "…")
    ("z/" nil "・")
    ("z0" nil "○")
    ("z:" nil "O!,")
    ("z;" nil "O!+")
    ("z@" nil "◎")
    ("z[" nil "『")
    ("z]" nil "』")
    ("z{" nil "【")
    ("z}" nil "】")
    ("z(" nil "（")
    ("z)" nil "）")
    ("za" nil ("ザ" . "ざ"))
    ("ze" nil ("ゼ" . "ぜ"))
    ("zh" nil "←")
    ("zi" nil ("ジ" . "じ"))
    ("zj" nil "↓")
    ("zk" nil "↑")
    ("zl" nil "→")
    ("zL" nil "⇒")
    ("zn" nil "ー")
    ("zo" nil ("ゾ" . "ぞ"))
    ("zu" nil ("ズ" . "ず"))
    ("zya" nil ("ジャ" . "じゃ"))
    ("zye" nil ("ジェ" . "じぇ"))
    ("zyi" nil ("ジィ" . "じぃ"))
    ("zyo" nil ("ジョ" . "じょ"))
    ("zyu" nil ("ジュ" . "じゅ"))
    ("." nil skk-auto-kutouten)
    ("," nil skk-auto-kutouten)
    ("-" nil skk-auto-kutouten)
    (":" nil "：")
    (";" nil "；")
    ("?" nil "？")
    ("[" nil "「")
    ("]" nil "」")
    ("l" nil skk-latin-mode)
    ("q" nil skk-toggle-characters)
    ("L" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-set-henkan-point-subr)
    ("X" nil skk-purge-from-jisyo)
    ("/" nil skk-abbrev-mode)
    ("$" nil skk-display-code-for-char-at-point)
    ("@" nil skk-today)
    ("\\" nil skk-input-by-code-or-menu)
    (skk-kakutei-key nil skk-kakutei)
    ;; XXX
    ;;("\t" nil skk-insert)
    ;;("," nil skk-previous-candidate)
    ;;("\M-\040" nil skk-comp-start-henkan); M-SPC
    ;;("\M-\121" nil skk-backward-and-set-henkan-point); M-Q
    )
  ;; コンスタントにしてしまわないのは、ローマ字入力とは全く別の設定を
  ;; する人もいるからです。
  "*キー入力をいかに処理するかを表す、状態遷移規則のリスト。

リストの各要素は、それぞれが一つの規則であり、下記の形式を満たしていなければ
ならない。

 (INPUT-STATE NEXT-STATE OUTPUT)

SKK は INPUT-STATE を検出すると、OUTPUT をバッファに挿入し、続いて
NEXT-STATE に状態を移したうえで、入力待ち状態となる。

例えば、

     (\"a\" nil (\"ア\" . \"あ\"))
     (\"ki\" nil (\"キ\" . \"き\"))
     (\"tt\" \"t\" (\"ッ\" . \"っ\"))
     (\"nn\" nil (\"ン\" . \"ん\"))
     (\"n'\" nil (\"ン\" . \"ん\"))

上記の規則は、それぞれ、

     a  => あ
     ki => き
     tt => っt
     nn => ん
     n' => ん

このように状態が移り変わることを意味する。

INPUT-STATE 及び NEXT-STATE は、通常 US-ASCII 文字から成る文字列を用いる。
ただし、特別な場合には INPUT-STATE にそれ以外の文字列を指定することがある。

OUTPUT には、以下の 3つの形式を指定できる。

文字列 -- かなモード、カナモードとも、これが挿入される。
文字列と文字列のセル (ドットペア)
       -- かなモードにおいては CDR の、カナモードにおいては CAR の文字列が、
          それぞれ挿入される。
関数名シンボル
       -- 関数を実行する。もしその関数の返り値が文字列ならば、その文字列を
          挿入する。

同様の規則を表す変数に `skk-rom-kana-rule-list' がある。SKK は両方の規則を利
用するが、 `skk-rom-kana-rule-list' の方が優先される。従ってユーザが独自の規
則を設定したい場合には、`skk-rom-kana-rule-list' の方を使うのがよい。"
  :type '(repeat
          (list :tag "ルール"
                (radio :tag "1 入力"
                       (string :tag "文字列")
                       (symbol :tag "変数名"))
                (radio :tag "2 次の状態"
                       (string :tag "文字列")
                       (const :tag "nil (空の状態)" nil))
                (radio :tag "3 出力"
                       (function :tag "関数できめる")
                       (string :tag "文字列")
                       (cons :tag "文字列の組"
                             (string :tag "3-1 カタカナ")
                             (string :tag "3-2 ひらがな")))))
  :group 'skk-input-basic)

(defcustom skk-rom-kana-rule-list
  '(;; ユーザーの好みで設定が分れそうな要素は、
    ;; skk-rom-kana-base-rule-list からこちらへ移しましょう...。
    ("hh" "h" ("ッ" . "っ"))
    ;; when you may want to insert 「がんま」by "gamma"...
    ("mm" "m" ("ン" . "ん")))
  "*状態遷移規則のリストで、ユーザの追加設定用の変数。

この変数は、`skk-rom-kana-base-rule-list' と同様の書式を満たす必要がある。

SKK は起動時にこの 2 変数を編集して `skk-rule-tree' を作成するが、
`skk-rom-kana-rule-list' の規則は `skk-rom-kana-base-rule-list' の規則よりも
優先される。

リストの各要素は、それぞれが一つの規則であり、下記の形式を満たしていなければ
ならない。

 (INPUT-STATE NEXT-STATE OUTPUT)

SKK は INPUT-STATE を検出すると、OUTPUT をバッファに挿入し、続いて
NEXT-STATE に状態を移したうえで、入力待ち状態となる。

詳しくは、`skk-rom-kana-base-rule-list' の説明を参照のこと。

ユーザは、追加したい規則を、例えば

    (setq skk-rom-kana-rule-list
      \\='(
    (\"hh\" \"h\" (\"ッ\" . \"っ\"))
    (\"@\" nil \"＠\")
    ...))

上記のように `~/.emacs.d/init.el' または `skk-init-file' にて設定することができる。

この変数は、標準では

    (\"hh\" \"h\" (\"ッ\" . \"っ\"))

の設定がされている。この規則に従うと、

    ohhonn => おっほん
    ohhira => おっひら

のように挿入される。もしこれを

    ohhonn  => おおほん
    ohhira  => おおひら

のように変更したければ、この設定

    (\"hh\" \"h\" (\"ッ\" . \"っ\"))

を削除する。

また、`@' で `skk-today' (当日の日付の入力) を起動する代りに `＠' を入
力したい場合は、`skk-rom-kana-rule-list' に

    (\"@\" nil \"＠\")

という要素を加える。

もし、SKK を起動した後で `skk-rom-kana-rule-list' を変更した場合、その設
定を反映させるには \\[skk-restart] を実行する必要がある。"
  :type '(repeat
          (list :tag "ルール"
                (radio :tag "1 入力"
                       (string :tag "文字列")
                       (symbol :tag "変数名"))
                (radio :tag "2 次の状態"
                       (string :tag "文字列")
                       (const :tag "nil (空の状態)" nil))
                (radio :tag "3 出力"
                       (function :tag "関数できめる")
                       (string :tag "文字列")
                       (cons :tag "文字列の組"
                             (string :tag "3-1 カタカナ")
                             (string :tag "3-2 ひらがな")))))
  :group 'skk-input-basic)

(defcustom skk-kana-input-search-function
  (lambda ()
    (save-match-data
      (and (string-match "^h\\([bcdfghjklmnpqrstvwxz]\\)$" skk-prefix)
           (member (char-to-string (preceding-char)) '("お" "オ"))
           (cons '("オ" . "お") (match-string 1 skk-prefix)))))
  "*ルールリストの中に記せない変換ルールを処理する関数。
`skk-rom-kana-base-rule-list' と `skk-rom-kana-rule-list' の要素を全て検索
した後にコールされる。引数はない。

 (現在の入力に対する出力 . \"続く unfixed prefix\")

というセルを返す。出力の種類については `skk-rom-kana-base-rule-list' を
参照のこと。

デフォルトでは、\"お\" の後の \"h\" + 子音の入力を \"おお\" + 続く子音
処理用の unfixed prefix に変換している。"
  :type 'function
  :group 'skk-input-basic)

(defcustom skk-downcase-alist nil
  "*変換キー (大文字ローマ字) の小文字への変換規則を表わす連想リスト。
変換キーの入力を開始する際、SKK では大文字で入力を行うので、
`skk-set-henkan-point' の中でこれを小文字に変換する。この連想
リストに大文字 -> 小文字の変換ルールを書いておくことで、キー入力をカス
タマイズすることができる。この連想リストが空リストの場合は、単に
downcase される。"
  :type '(repeat (cons character character))
  :group 'skk-input-basic)

(defcustom skk-jisx0208-latin-vector
  [nil  nil  nil  nil  nil  nil  nil  nil
        nil  nil  nil  nil  nil  nil  nil  nil
        nil  nil  nil  nil  nil  nil  nil  nil
        nil  nil  nil  nil  nil  nil  nil  nil
        "　"  "！" "”" "＃" "＄" "％" "＆" "’"
        "（" "）" "＊" "＋" "，" "－" "．" "／"
        "０" "１" "２" "３" "４" "５" "６" "７"
        "８" "９" "：" "；" "＜" "＝" "＞" "？"
        "＠" "Ａ" "Ｂ" "Ｃ" "Ｄ" "Ｅ" "Ｆ" "Ｇ"
        "Ｈ" "Ｉ" "Ｊ" "Ｋ" "Ｌ" "Ｍ" "Ｎ" "Ｏ"
        "Ｐ" "Ｑ" "Ｒ" "Ｓ" "Ｔ" "Ｕ" "Ｖ" "Ｗ"
        "Ｘ" "Ｙ" "Ｚ" "［" "＼" "］" "＾" "＿"
        "‘" "ａ" "ｂ" "ｃ" "ｄ" "ｅ" "ｆ" "ｇ"
        "ｈ" "ｉ" "ｊ" "ｋ" "ｌ" "ｍ" "ｎ" "ｏ"
        "ｐ" "ｑ" "ｒ" "ｓ" "ｔ" "ｕ" "ｖ" "ｗ"
        "ｘ" "ｙ" "ｚ" "｛" "｜" "｝" "～" nil]
  "*`skk-jisx0208-latin-insert' で参照される文字テーブル。
キーに対応する位置に文字列があれば、全英モードで該当のキーを押すことで、対応す
る文字が挿入される。
例えば、スペースキーに対応して、半角スペースを挿入させるように変更したければ、
skk.el のロード後 (もしくは `skk-load-hook' を利用して)、

     (aset skk-jisx0208-latin-vector 32 \" \")

とするか、もしくは、`skk-jisx0208-latin-vector' の 32 番目 (0 番から数えて)
 の値を \" \"とするような `skk-jisx0208-latin-vector' を直接書き、setq で
代入する。32 は、?  (半角スペースの char type) を評価したときの値。"
  :type 'sexp
  :group 'skk-input-basic)

(defcustom skk-special-midashi-char-list '(?> ?< ??)
  "*接頭辞、接尾辞の入力を指定する文字のリスト。"
  ;;  "*List of characters for entering prefixes and suffixes."
  :type '(repeat character)
  :group 'skk-input-basic)

(defcustom skk-kuten-touten-alist
  '((jp . ("。" . "、"))
    (en . ("．" . "，"))
    (jp-en . ("。" . "，"))
    (en-jp . ("．" . "、")))
  "*句点と読点の連想リスト。
各要素の形式は、

   (シンボル . (句点を表わす文字列 . 読点を表わす文字列))

という cons cell。シンボルの部分は、`jp' もしくは `en' 。
\\[skk-toggle-kutouten] は、これをトグルで切り換える。
デフォルトの句読点のタイプは、変数 `skk-kutouten-type' で指定する。"
  :type '(repeat (cons (radio :tag "組のなまえ"
                              (const jp)
                              (const en)
                              (const jp-en)
                              (const en-jp))
                       (cons :tag "句読点の組"
                             (string :tag "句点" "。")
                             (string :tag "読点" "、"))))
  :group 'skk-input-basic)

(defcustom skk-kutouten-type 'jp
  "*標準の句読点のタイプ。
この変数の値に指定できるシンボルと句読点の組との対応は以下の通り。

      `jp': 「。」「、」
      `en': 「．」「，」
   `jp-en': 「。」「，」
   `en-jp': 「．」「、」

この変数にはコンス・セルを指定することも可能。その場合は

 (句点を示す文字列 . 読点を示す文字列)

のように指定する。

この変数は `skk-use-kana-keyboard' が non-nil ならば無効である。

この変数は `setq' するとバッファローカル化されるため、グローバルに
値を設定したい場合は `setq-default' を用いることが推奨される。"
  :type '(radio (const jp)
                (const en)
                (const jp-en)
                (const en-jp)
                (cons :tag "任意の組"
                      (string :tag "句点" "。")
                      (string :tag "読点" "、")))
  :group 'skk-input-basic)
(make-variable-buffer-local 'skk-kutouten-type)
;;;###autoload
(put 'skk-kutouten-type 'safe-local-variable 'symbolp)

(defcustom skk-use-auto-kutouten nil
  "*Non-nil であれば、かなモードにおける長音(ー)、句点(。)又は読点(、)の動作を
変更する。ASCII 数字の直後であれば、長音(ー)は `-' へ、句点(。)は `.' へ、
読点(、)は `,' へと変更し、JISX0208(全角)数字の直後であれば、長音(ー)は `－' へ、
句点(。)は `．' へ、読点(、)は `，' へと変更する。"
  :type 'boolean
  :group 'skk-input-basic)

(defcustom skk-auto-insert-paren nil
  "*Non-nil であれば、括弧と閉括弧をまとめて挿入する。
例えば、\"「\" を入力したときに \"」\" を自動的に挿入し、両かぎ括弧の間に
カーソルを移動する。
挿入する文字列は、`skk-auto-paren-string-alist' で指定する。"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-input-basic)

(defcustom skk-auto-paren-string-alist
  '(("「" . "」") ("『" . "』") ("(" . ")") ("（" . "）")
    ("{" . "}")("｛" . "｝") ("〈" . "〉") ("《" . "》")
    ("[" . "]") ("［" . "］") ("〔" . "〕") ("【" . "】")
    ("\"" . "\"")("“" . "”") ("`" . "'")
    ;;("<" . ">") ;; skk-special-midashi-char-list の中にある文字。
    )
  "*自動的に対になる文字列を入力するための連想リスト。
`skk-auto-insert-paren' が non-nil の場合、car の文字列が挿入されたとき
に cdr の文字列を自動的に挿入し、カーソルはその 2 つの文字の間に移動する。
`skk-special-midashi-char-list' の要素になっている文字は、
`skk-auto-paren-string-alist' に含めても削除される。"
  :type '(repeat (cons string string))
  :group 'skk-input-basic)

(defcustom skk-use-auto-enclose-pair-of-region nil
  "*Non-nil であれば、リージョンが有効な状態で `skk-auto-insert-paren' を実行した際には、リージョンを括弧と閉括弧で囲む。"
  :type 'boolean
  :group 'skk-input-basic)

(defcustom skk-start-henkan-char ?\040  ; SPC
  "*漢字変換を開始するキーキャラクタ。"
  :type 'character
  :group 'skk-henkan)

(defcustom skk-show-candidates-nth-henkan-char 5
  "*`skk-henkan-show-candidates' を呼び出すまでの `skk-start-henkan-char' を打鍵する回数。
2 以上の整数である必要。"
  :type 'integer
  :group 'skk-henkan)

(defcustom skk-previous-candidate-keys (list "x" "\C-p")
  "*`skk-previous-candidate' を割当てるキー。
この変数にはキーを表すオブジェクトのリストを指定する。
オブジェクトとしては、キーを表す文字列または event vector が指定できる。"
  :type (if (get 'key-sequence 'widget-type)
            '(repeat (key-sequence :tag "キー (C-q key で取得可)"))
          '(repeat sexp))
  :group 'skk-henkan)

(make-obsolete-variable 'skk-previous-candidate-char
                        'skk-previous-candidate-keys
                        "DDSKK 14.2")

(defcustom skk-set-henkan-point-key
  '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?M ?N ?O ?P ?R ?S ?T ?U ?V ?W ?Y ?Z)
  "*変換の開始地点を決めるキーのリスト。"
  :type '(repeat character)
  :group 'skk-henkan)

(defcustom skk-henkan-show-candidates-keys
  '(?a ?s ?d ?f ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?z ?c ?v ?b ?n ?m ?,)
  "*メニュー形式で候補を選択するときの選択キーのリスト。
\"x\", \" \" 及び \"C-g\" 以外の 7 の倍数個のキー (char type) を含む必要があ
る。\"x\", \" \" 及び \"C-g\" は候補選択時にそれぞれ特別な機能に割り当
てられているので、このリストの中には含めないこと。"
  :type '(repeat character)
  :group 'skk-henkan)

(defface skk-henkan-show-candidates-keys-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*選択キーの face 属性。"
  :group 'skk-henkan
  :group 'skk-visual)

(defcustom skk-henkan-rest-indicator nil
  "*Non-nil であれば \[残り 99++\] の表示を右寄せ配置する。"
  :type 'boolean
  :group 'skk-henkan
  :group 'skk-visual)

(defface skk-henkan-rest-indicator-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*\[残り 99++\] の face 属性。"
  :group 'skk-henkan
  :group 'skk-visual)

(defcustom skk-auto-start-henkan t
  "*単語や文節の区切りを示す文字の打鍵により自動的に変換を開始する。
`skk-auto-start-henkan-keyword-list' により単語や文節の区切りを示す文字を
指定する。"
  :type 'boolean
  :group 'skk-henkan)

(defcustom skk-auto-start-henkan-keyword-list
  '("を" "、" "。" "．" "，" "？" "」" "！" "；" "：" ")" ";" ":"
    "）" "”" "】" "』" "》" "〉" "｝" "］" "〕" "}" "]" "?" "."
    "," "!")
  ;; あまりキーワードが多くなると、通常の変換を困難にする？
  "*自動変換を開始するキーワード。
`skk-auto-start-henkan' が non-nil のとき、このリストの要素の文字を打鍵
すると、SPC (`skk-start-henkan-char') を押したかのように変換を開始して
▼モードに入る。"
  :type '(repeat string)
  :group 'skk-henkan)

(defcustom skk-force-registration-mode-char ?.
  "*強制的に辞書登録モードに入るキーキャラクタ。
エコーエリアで候補を表示しているときにこの変数で定義したキーキャラクタを
タイプすると、強制的に辞書登録モードに入ります。"
  :type 'character
  :group 'skk-henkan)

(defcustom skk-show-candidates-toggle-display-place-char ?\C-f
  "*候補表示一覧の位置をエコーエリアとバッファとで切り替えるキーキャラクタ。"
  :type 'character
  :group 'skk-henkan)

(defcustom skk-backward-and-set-henkan-point-char ?\321 ; M-Q
  "*ポイントを戻して▽モードに入るキーキャラクタ。"
  :type 'character
  :group 'skk-henkan)

(defcustom skk-show-inline nil
  "*Non-nil であれば、変換候補をインライン表示する。
`vertical' であれば、縦方向にインライン表示する（XEmacs では動作しない）。"
  :type '(radio (const :tag "有効" t)
                (const :tag "有効 (縦表示)" vertical)
                (const :tag "無効" nil))
  :group 'skk-basic
  :group 'skk-henkan)

(defcustom skk-inline-show-face 'underline
  "*インライン表示する変換候補を装飾するフェイスを指定する変数。
候補文字列のフェイス属性をそのまま使いたい場合は nil に設定する。"
  :type '(radio (face :tag "フェイスを指定")
                (const :tag "候補文字列のフェイス属性をそのまま使用" nil))
  :group 'skk-visual)

(defcustom skk-inline-show-background-color
  (if (eq skk-background-mode 'light)
      "beige"
    "gray15")
  "*インライン表示する変換候補の背景色を指定する変数。
`skk-inline-show-face' または `skk-treat-candidate-appearance-function' で
背景色が指定されていない文字に対してのみ作用する。"
  :type 'string
  :group 'skk-visual)

(defcustom skk-inline-show-background-color-odd
  (if (eq skk-background-mode 'light)
      "wheat"
    "gray20")
  "*インライン表示する変換候補の背景色(奇数ライン)を指定する変数。
`skk-inline-show-face' または `skk-treat-candidate-appearance-function' で
背景色が指定されていない文字に対してのみ作用する。"
  :type 'string
  :group 'skk-visual)

(defcustom skk-search-end-function nil
  "*単語検索終了時にコールされる関数。
この関数を利用して検索した単語の優先順位を変更するなどの作業が可能。
HENKAN-BUFFER, MIDASI, OKURIGANA, ENTRY の 4 引数を伴なってコールされる。
加工した ENTRY を返すこと。
この関数は、辞書バッファでコールされるので、変換を行ったバッファローカルな
情報を取り出したいときは、HENKAN-BUFFER を利用する。"
  :type '(list symbol)
  :group 'skk-henkan)

(defcustom skk-allow-spaces-newlines-and-tabs t
  "*Non-nil であれば、見出し語の中のスペース、タブ、改行を取り除いて変換
できる。例えば、下記のように途中に改行が入っている見出し語でも変換が可能
である。

     \"▽か
  な\"
   -> \"仮名\"

この値が nil であれば、最初のスペースで見出し語を切り詰めてしまい、以降のスペー
ス、タブ、改行は無視される。
この値は、`skk-toggle-characters' 及び `skk-backward-and-set-henkan-point' の動
作に影響する。"
  :type 'boolean
  :group 'skk-henkan)

(defcustom skk-delete-okuri-when-quit nil
  "*Non-nil であれば変換中の \\[keyboard-quit] で送り仮名を消して▽モード
に入る。
  例） \"▽な*く -> ▼泣く -> \\[keyboard-quit] ->▽な\"

nil であれば、送り仮名を含めた見出し語をそのまま残して▽モードに入る。
  例） \"▽な*く -> ▼泣く -> \\[keyboard-quit] -> ▽なく\""
  :type 'boolean
  :group 'skk-henkan)

(make-obsolete-variable 'skk-henkan-show-candidates-rows
                        'skk-henkan-number-to-display-candidates
                        "DDSKK 16.2")

(defcustom skk-henkan-number-to-display-candidates 7
  "*変換候補を表示する個数。"
  :type 'integer
  :group 'skk-henkan)

(defcustom skk-show-candidates-always-pop-to-buffer nil
  "*この変数が non-nil であれば、常に\"*候補*\"バッファを作製して、変換候補一覧
を専用ウィンドウに表示する。
nil であれば、候補一覧をエコーエリアに表示する。ただし、候補一覧の文字列の長さが
フレームの横幅に収まらない場合は、\"*候補*\"バッファを作製(pop-to-buffer)して専
用のウィンドウで表示する。"
  :type 'boolean
  :group 'skk-henkan)

(defcustom skk-candidate-buffer-background-color nil
  "*\"*候補*バッファ\"の背景色。"
  :type '(radio (string :tag "色の名前")
                (const :tag "指定しない" nil))
  :group 'skk-henkan
  :group 'skk-visual)

(defcustom skk-candidate-buffer-background-color-odd nil
  "*\"*候補*バッファ\"の背景色（奇数ライン）。"
  :type '(radio (string :tag "色の名前")
                (const :tag "指定しない" nil))
  :group 'skk-henkan
  :group 'skk-visual)

(defcustom skk-search-katakana nil
  "*かなを単純にカタカナ変換した候補を表示するかどうかを決めるオプション。
nil ならば含めない。t ならば全角カナ候補を含める。
`jisx0201-kana' ならば全角に加えて半角カナ候補も含める。
この機能は一般的な FEP の使い勝手に近付けたいユーザー、個人辞書を育てたい
ユーザー向けに提供される。"
  :type '(radio (const :tag "この機能を無効にする" nil)
                (const :tag "全角カナのみ" t)
                (const :tag "半角カナも含める" jisx0201-kana))
  :group 'skk-henkan)

(defcustom skk-search-sagyo-henkaku nil
  "*簡易なサ変動詞変換機能を有効にするかどうか決めるオプション。
nil ならば、送り仮名が \"さ\" \"し\" \"す\" \"せ\" のいずれかの時に
送りなし候補が変換候補に現れる。
anything に設定すると、送り仮名が何であっても送りなし候補を送りあり変換に
用いる。この場合、送り仮名というよりも、任意の漢字とかなの切り替え位置を
指定するような入力になる。
この機能は不正確な出力をする可能性に注意する必要があるが、個人辞書を育てたい
ユーザー向けに提供される。"
  :type '(radio (const :tag "この機能を無効にする" nil)
                (const :tag "簡易サ変動詞変換をする" t)
                (const :tag "この機能を任意の送りあり変換に拡張する" anything))
  :group 'skk-henkan)

(defcustom skk-kakutei-key "\C-j"
  "*漢字変換の確定動作を行うキー。"
  :type (if (get 'key-sequence 'widget-type)
            'key-sequence
          'sexp)
  :group 'skk-basic
  :group 'skk-kakutei)

(defcustom skk-kakutei-early t
  "*Non-nil であれば `skk-insert' が呼ばれたときに現在の候補を確定する。
例えば、

    \"▽かくてい -> ▼確定 -> 確定s -> 確定す\"

のように変換後、「す」の prefix である \"s\" を入力した時点で確定する。
nil であれば、例えば

    \"▽かくてい -> ▼確定 -> ▼確定s -> ▼確定する -> 確定する。\"

のように `skk-kakutei' を直接、間接にコールするまで (句読点を入力したり、
新たな▽モードに入ったりすると間接的に `skk-kakutei' をコールする) は、確定
しないので、その間は、変換候補を選び直すことなどが可能。

このオプション利用時は、`skk-process-okuri-early' の値は nil でなければ
ならない。"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-kakutei)

(defcustom skk-egg-like-newline nil
  "*Non-nil であれば、▼モードで RET をタイプしても確定のみ行い、改行しない。"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-kakutei)

(defcustom skk-delete-implies-kakutei t
  "*Non-nil であれば、▼モードで BS を押すと、前の一文字を削除し確定する。
nil であれば、一つ前の候補を表示する。
シンボル `dont-update' であれば、個人辞書を更新しない。

なお、この変数の値にかかわらず、候補一覧を表示しているときの BS 打鍵は
前候補(群)の表示になる。"
  :type '(radio (const t)
                (const dont-update)
                (const nil))
  :group 'skk-basic
  :group 'skk-kakutei)

(defcustom skk-kakutei-when-unique-candidate nil
  "*Non-nil であれば、変換候補が一つしかないとき確定変換する。

この値が t であればどの変換モードでも確定変換する。
`okuri-ari', `okuri-nasi', `abbrev' のいずれかを要素とするリストで
あれば、変換モードがその条件に合致した場合のみ確定変換する。

候補が他に無い事を確認するため、`skk-search-prog-list' の内容次第
でレスポンスが悪くなる可能性がある。その場合
`skk-kakutei-search-prog-limit' を設定することで検索対象を制限する
ことも可能。"
  :type '(radio (const :tag "常に有効" t)
                (set :tag "有効にする変換モード"
                     (const :tag "送り有り変換" okuri-ari)
                     (const :tag "送り無し変換" okuri-nasi)
                     (const :tag "abbrev 変換" abbrev))
                (const :tag "無効" nil))
  :group 'skk-kakutei)

(defcustom skk-kakutei-search-prog-limit nil
  "*複数辞書による確定変換において、検索対象とする辞書を制限する。

これが数値であれば、検索対象を `skk-search-prog-list' の先頭からこ
の個数までの辞書に制限する。
それ以外であれば無制限に全ての辞書を対象とする。

`skk-kakutei-when-unique-candidate' が non-nil のときのみ有効。"
  :type '(radio (integer :tag "対象とする辞書の数")
                (const :tag "制限しない" nil))
  :group 'skk-kakutei)

(defcustom skk-kakutei-end-function nil
  "*確定時にコールされる関数。
`skk-kakutei-initialize' がコールされる前にこの関数がコールされるので、
最後の確定に関するフラグ類は、この関数の中から参照することができる。"
  :type '(radio (function :tag "関数")
                (const :tag "指定しない" nil))
  :group 'skk-kakutei)

(defcustom skk-henkan-okuri-strictly nil
  "*Non-nil であれば、見出し語と送り仮名が一致したときだけ候補として出力する。
例えば、下記のような辞書エントリが `skk-jisyo' (個人辞書) にあった
場合に

  \"おおk /大/多/[く/多/]/[き/大/]/\"

\"▽おお*く\" を変換したとき、\"多く\" のみを出力し、\"大く\" を出力しない。

SKK-JISYO.[SML] の送り仮名エントリは上記の形式になっていないので、`skk-jisyo'
 の送りありの辞書エントリがこの形式のものをあまり含んでいない場合は、この
オプションを on にすることで、すぐに単語登録に入ってしまうので注意すること。

`skk-process-okuri-early' の値が nil ならば、上記の形式で `skk-jisyo' が
作られる。

下記の式を評価することで、単語登録に入ったときだけ
一時的にこのオプションを無効にすることができる。

    (add-hook \\='minibuffer-setup-hook
              (function
               (lambda ()
                 (if (and (boundp \\='skk-henkan-okuri-strictly)
                          skk-henkan-okuri-strictly
                          (not (eq last-command \\='skk-purge-from-jisyo)))
                     (progn
                       (setq skk-henkan-okuri-strictly nil)
                       (put \\='skk-henkan-okuri-strictly \\='temporary-nil t))))))

    (add-hook \\='minibuffer-exit-hook
              (function
               (lambda ()
                 (if (and (get \\='skk-henkan-okuri-strictly \\='temporary-nil)
                          (<= (minibuffer-depth) 1))
                     (progn
                       (put \\='skk-henkan-okuri-strictly \\='temporary-nil nil)
                       (setq skk-henkan-okuri-strictly t))))))

このオプション利用時は、`skk-process-okuri-early' の値は nil でなければ
ならない。"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-okurigana)

(defcustom skk-henkan-strict-okuri-precedence nil
  "*Non-nil であれば、見出し語と送り仮名が一致した候補を優先して表示する。
例えば、下記のような辞書エントリが `skk-jisyo' (個人辞書) にあった
場合に

  \"おおk /大/多/[く/多/]/[き/大/]/\"

\"▽おお*く\" を変換したとき、まず\"多く\" を出力し、
次に \"大く\" を出力する。

\"大く\" などの候補はうっとうしいが、すぐに単語登録に入ってしまうのも
嫌な人にお勧め。

このオプション利用時は、`skk-process-okuri-early' の値は nil でなければ
ならない。
また `skk-henkan-okuri-strictly' が non-nil のときは、この変数は無視される。"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-okurigana)

(defcustom skk-process-okuri-early nil
  "*Non-nil であれば送り仮名のローマ字プレフィックス入力時点で変換を開始する。
例えば、

    \"UgoK -> ▼動k\"。

送り仮名が分らないまま変換していることになるので、`skk-jisyo' が送り仮名に
対応した形に成長しない。つまり

    \"うごk /動/\"

のような形態のままとなる。ただし、既に

    \"うごk /動/[く/動/]/[か/動/]/[け/動/]/[き/動/]/[こ/動/]/\"

のようなエントリが `skk-jisyo' にあれば、それを破壊しない。

nil であれば、送り仮名の入力が完了した時点で変換が開始する。例えば、

    \"UgoK -> ▽うご*k\", \"UgoKu -> ▼動く\"

このオプションを on にして `skk-mode' を起動すると、両立できないオプション
である `skk-kakutei-early', `skk-auto-okuri-process' 及び
`skk-henkan-okuri-strictly' は nil にセットされる。"
  :type 'boolean
  :group 'skk-okurigana)

(defcustom skk-check-okurigana-on-touroku nil
  "*Non-nil であれば、送りありの登録時に、余計な仮名をチェックする。

例えば、

     \"とびだ*す 飛び出\"

と登録するのが正しいにもかかわらず、ユーザが

     \"とびだ*す 飛び出す\"

でうっかり [RET] を押してしまったときに、最後の「す」が送り仮名であるかどうか
調べる。

この変数は以下の値をとり得る。既定値は nil。

ask  -- ユーザに確認を求め、送り仮名と認められればこれを取り除いてから登録す
        る。
auto -- ユーザに確認を求めず、勝手に送り仮名を判断して削除してから登録する。
nil  -- 一切送り仮名のチェックをせず、全体を単語として登録する。これは SKK 本
        来の動作である。"
  :type '(radio (const :tag "ユーザに確認する" ask)
                (const :tag "自動的に処理する" auto)
                (const :tag "チェックしない"  nil))
  :group 'skk-basic
  :group 'skk-okurigana
  :group 'skk-private)

(defcustom skk-okuri-char-alist nil
  "*送り仮名 prefix を変換するルールを記述する連想リスト。
car に「実際のキー入力によるかな prefix 文字列」、cdr に「SKK の辞書が予
想しているかな prefix 文字列」を持つ cons cell のリスト。

この規則が使われるのは、`skk-process-okuri-early' が non-nil の場合のみである。

例えば、か行の送り仮名入力に \"c\" の prefix を使うのであれば、

  (setq skk-okuri-char-alist \\='((\"c\" . \"k\")))

のように書く。"
  :type '(repeat (cons string string))
  :group 'skk-okurigana)

(defcustom skk-emacs-id-file (if skk-user-directory
                                 (expand-file-name "emacs-id"
                                                   skk-user-directory)
                               (convert-standard-filename "~/.skk-emacs-id"))
  "\
*`skk-jisyo-file' に最近アクセスした SKK の `skk-emacs-id' を保存するファイル。"
  :type 'file
  :group 'skk-misc)

(defcustom skk-keep-record t
  "*Non-nil であれば、変換及び個人辞書に関する統計を `skk-record-file' に取る。
数値であれば、`skk-record-file' をその行数より大きくしない。
nil であれば、変換及び個人辞書に関する統計を取らない。"
  :type '(radio (integer :tag "行数を指定")
                (const :tag "レコードサイズ制限なし" t)
                (const :tag "記録しない" nil))
  :group 'skk-misc)

(defcustom skk-record-file (if skk-user-directory
                               (expand-file-name "record" skk-user-directory)
                             (convert-standard-filename "~/.skk-record"))
  "*変換及び個人辞書に関する統計を取るファイル。
個人辞書を保存した日時、単語の登録数、確定した回数、確定率、全体の語数の
情報を収める。"
  :type 'file
  :group 'skk-misc)

(defcustom skk-byte-compile-init-file nil
  "*Non-nil であれば、`skk-mode' 起動時に `skk-init-file' をバイトコンパイルする。
正確に言うと、

  (1)`skk-init-file' をバイトコンパイルしたファイルがないか、
  (2)`skk-init-file' とそのバイトコンパイル済ファイルを比較して、前者の方が新し
     いとき

に `skk-init-file' をバイトコンパイルする。
nil であれば、`skk-init-file' とそのバイトコンパイル済みファイルを比較して
`skk-init-file' の方が新しいときは、そのバイトコンパイル済ファイルを消す。

この変数は ~/.emacs.d/init.el で設定すること。"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-read-from-minibuffer-function nil "\
*辞書登録モードに入ったときのプロンプトに表示する初期値を提供する関数。
この関数は文字列を返さなければならない。
関数 `read-from-minibuffer' の引数 INITIAL-CONTENTS に該当する。

`skk-henkan-key' をそのまま初期値として利用したいときは、

  (setq skk-read-from-minibuffer-function
        (lambda () skk-henkan-key))

と指定する。"
  :type '(radio (function :tag "関数")
                (const :tag "指定しない" nil))
  :group 'skk-misc)

(defface skk-jisyo-registration-badge-face
  '((((class color) (type tty))
     (:inherit default :inverse-video t))
    (((class color) (background light))
     (:inherit default :inverse-video t))
    (((class color) (background dark))
     (:inherit default :inverse-video t))
    (((class grayscale))
     (:inherit default :inverse-video t)))
  "*↓辞書登録中↓に適用するフェイス。"
  :group 'skk-visual)

;;;###autoload
(defcustom skk-preload nil
  "*Non-nil ならば、Emacs 起動時に SKK プログラムと辞書の読み込みを済ませる。
Emacs の起動そのものは遅くなるが、DDSKK の初回起動を早くすることができる。"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-undo-kakutei-word-only nil
  "*Non-nil であれば ▽モードと▼モード時のアンドゥ情報を記録しない。"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-undo-kakutei-return-previous-point nil
  "*Non-nil であれば、確定アンドゥ処理が完了した後に、確定アンドゥ処理の
直前の位置にカーソルを戻す。"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-show-japanese-menu
  (and window-system
       (or (eq window-system 'w32)
           (boundp 'mac-carbon-version-string) ; Carbon Emacs
           (featurep 'ns) ; Cocoa Emacs
           (and (eq window-system 'x)
                (boundp 'gtk-version-string)
                (stringp (symbol-value 'gtk-version-string))
                (string< "2.0" (symbol-value 'gtk-version-string))))
       (equal current-language-environment "Japanese")) "\
*Non-nil であればメニューバーを日本語で表示する。"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-verbose nil
  "*Non-nil であれば、入力中／変換中にエコーエリアに冗長なメッセージを表示する。"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-verbose-wait 1.5
  "*冗長なメッセージを表示するまでの待ち時間 (秒)。"
  :type 'number
  :group 'skk-misc)

(defcustom skk-verbose-message-interval 5.0
  "*冗長なメッセージが複数ある場合、１つあたり表示時間 (秒)。
この時間が経過したら次のメッセージに切り替える。"
  :type 'number
  :group 'skk-misc)

(defface skk-verbose-intention-face
  '((((class color) (type tty))
     (:inherit default :bold t))
    (((class color) (background light))
     (:inherit default :bold t))
    (((class color) (background dark))
     (:inherit default :bold t))
    (((class grayscale))
     (:inherit default :bold t)))
  "*▼モードの冗長なメッセージの {アノテーション} と {どれを参照?} に適用する
フェイス。"
  :group 'skk-visual)

(defface skk-verbose-kbd-face
  '((((class color) (type tty))
     (:inherit default :foreground "cyan"))
    (((class color) (background light))
     (:inherit default :foreground "Purple"))
    (((class color) (background dark))
     (:inherit default :foreground "Cyan"))
    (((class grayscale))
     (:inherit default :foreground "LightGray")))
  "*冗長なメッセージの操作キー部分に適用するフェイス。"
  :group 'skk-visual)

(defcustom skk-henkan-on-message nil
  "*▽モードで表示する冗長なメッセージの内容。
標準では自動設定する。"
  :type '(radio (string :tag "内容を指定")
                (const :tag "自動設定" nil))
  :group 'skk-misc)

(defcustom skk-j-mode-function-key-usage nil
  "*キーボード上の F1 ～ F10 キーの使い方を指定する。
`conversion' ならば、`skk-search-prog-list-1' ～ `skk-search-prog-list-0' を
実行できる。
`kanagaki' ならば、かなキーボード入力用の設定になる。
nil ならば自動設定はしない (自分で好きな設定ができる)。"
  :type '(radio (const :tag "切り替え変換機能用設定" conversion)
                (const :tag "かな入力用設定" kanagaki)
                (const :tag "設定しない" nil))
  :group 'skk-misc)

(defcustom skk-mode-hook nil
  "*skk-mode に入るたびに呼ばれるフック。
他に、`skk-auto-fill-mode-hook', `skk-load-hook', `skk-init-file' でも
カスタマイズが可能。"
  ;; "*Hook run at SKK startup.  This hook is also run
  ;;in skk-auto-fill-mode after skk-auto-fill-mode-hook.
  ;;skk-auto-fill-mode-hook, skk-load-hook, skk-init-file may also be used
  ;;for customization."
  :type 'hook
  :group 'skk-misc)

(defcustom skk-auto-fill-mode-hook nil
  "*`skk-auto-fill-mode' を起動したときのフック。
他に、`skk-mode-hook', `skk-load-hook', `skk-init-file' でもカスタマイズが
可能。"
  ;;  "*Hook run at startup of skk-auto-fill-mode.
  ;;skk-mode-hook、skk-load-hook, skk-init-file may also be used for
  ;;customization."
  :type 'hook
  :group 'skk-misc)

(defcustom skk-load-hook nil
  "*skk.el をロードした後にコールされるフック。
他に、`skk-mode-hook', `skk-auto-fill-mode-hook', `skk-init-file' でもカスタ
マイズが可能。"
  ;;  "*Hook run when SKK is loaded.
  ;;skk-auto-fill-mode-hook、skk-mode-hook, skk-init-file may also be used
  ;;for customization."
  :type 'hook
  :group 'skk-misc)

(defcustom skk-status-indicator 'left
  "*SKK の状態をモードラインのどの位置に表示するかを決める。
left であれば左端に表示する。
さもなければマイナーモードとしての表示法を取る。"
  :type '(radio (const :tag "モードラインの左端に表示" left)
                (const :tag "マイナーモードの一種として表示" minor-mode))
  :group 'skk-visual)

(defcustom skk-latin-mode-string "SKK"
  "*アスキーモードであるときにモードラインに表示する文字列。"
  :type 'string
  :group 'skk-visual)

(defcustom skk-hiragana-mode-string "かな"
  "*かなモードであるときにモードラインに表示する文字列。"
  :type 'string
  :group 'skk-visual)

(defcustom skk-katakana-mode-string "カナ"
  "*カナモードであるときにモードラインに表示する文字列。"
  :type 'string
  :group 'skk-visual)

(defcustom skk-jisx0208-latin-mode-string "全英"
  "*全英モードであるときにモードラインに表示する文字列。"
  :type 'string
  :group 'skk-visual)

(defcustom skk-abbrev-mode-string "aあ"
  "*SKK abbrev モードであるときにモードラインに表示する文字列。"
  :type 'string
  :group 'skk-visual)

(defcustom skk-indicator-use-cursor-color (and window-system
                                               (fboundp 'x-display-color-p)
                                               (x-display-color-p))
  "*Non-nil ならば、カーソルと同じ色でインジケータを表示する"
  :type 'boolean
  :group 'skk-visual)

(defcustom skk-indicator-prefix "--"
  "*インジケータの接頭辞とする文字列"
  :type 'string
  :group 'skk-visual)

(defcustom skk-indicator-suffix-func #'(lambda (mode)
                                         (cond ((memq mode '(latin abbrev))
                                                "::")
                                               (t
                                                ":")))
  "*インジケータの接尾語とする文字列を返す関数"
  :type 'function
  :group 'skk-visual)

(defvar skk-icon nil
  "SKK アイコンの画像ファイル skk.xpm のパス。")

(put 'skk-icon 'risky-local-variable t)

(defcustom skk-show-icon nil
  "*Non-nil であれば、モードラインに SKK のアイコンを常時表示する。
表示する SKK アイコンの画像は `skk-icon' で指定する。"
  :type 'boolean
  :set (lambda (symbol value)
         (prog1
             (if (fboundp 'custom-set-default)
                 (custom-set-default symbol value)
               (set-default symbol value))
           (when (and (boundp 'skk-mode-invoked)
                      skk-mode-invoked)
             (cond (value
                    (skk-emacs-prepare-modeline-properties)
                    (skk-setup-modeline))
                   (t
                    (setq skk-icon nil))))))
  :group 'skk-visual)

(defcustom skk-echo t
  "*Non-nil であれば、仮名文字のプレフィックスを表示する。"
  :type 'boolean
  :group 'skk-visual)

(defcustom skk-use-face (or window-system
                            (fboundp 'selected-frame)
                                        ; XEmacs does not have this.
                            (fboundp 'frame-face-alist))
  "*Non-nil であれば、Emacs の face の機能を使用して変換候補をハイライト表示する。"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-visual)

;; should use defface?  however, can I use defface for highlight?
(defcustom skk-henkan-face 'skk-henkan-face-default
  "*変換候補の face 属性。`skk-use-face' が non-nil のときのみ有効。
Emacs 標準のフェイスのほか、新たに face を作って指定することも可能。
新たな face を作って指定するには、

      (setq skk-henkan-face (skk-make-face \\='DimGray/PeachPuff1))

のように skk-make-face() を利用するのが手軽。
foreground と background の色指定だけでない凝った face を作る場合は、`skk-make-face' で
は対応できないので、Emacs の hilit19.el の `hilit-lookup-face-create' などを利用する。
色を付ける場合の配色は、canna.el の `canna:attribute-alist' が良い例かもしれない。

この変数よりも `skk-treat-candidate-appearance-function' の設定が優先される。"
  :type 'face
  :group 'skk-visual)

(defface skk-henkan-face-default
  '((((class color) (type tty))
     (:foreground "black" :background "green"))
    (((class color) (background light))
     (:foreground "black" :background "darkseagreen2"))
    (((class color) (background dark))
     (:foreground "white" :background "darkolivegreen"))
    (((class grayscale)) (:underline t)))
  "*標準の変換候補の face 属性。"
  :group 'skk-visual)

(when (and skk-use-face
           (boundp 'frame-background-mode)
           (not frame-background-mode)
           (fboundp 'face-background)
           (not (face-background 'skk-henkan-face-default)))
  (set-face-foreground 'skk-henkan-face-default "black")
  (set-face-background 'skk-henkan-face-default "darkseagreen2"))

(defcustom skk-henkan-overlay-priority 600
  "*変換した候補に重ねる overlay の priority。
例えば、Viper で R コマンドにより replace を行うときに、
`viper-replace-overlay' という priority 400 の overlay を重ねられるが、
`skk-henkan-overlay-priority' のデフォルト値はこの overlay より
priority が高いので、優先して表示される。"
  :type 'integer
  :group 'skk-visual)

(defcustom skk-treat-candidate-appearance-function nil
  "*候補の表示を装飾するための関数を指定する変数。
ユーザは候補となるべき文字列に対して、その注釈（アノテーション）も含めて
ほぼ任意の加工を施すことができる。この関数は以下の条件を満たす必要がある。

1. 引数を２つ取ること。
2. 第１引数は文字列として扱うこと。これは加工前の文字列に相当する。
3. 第２引数が nil の時は通常の変換時、non-nil の時は候補一覧表示時を表す
   ものとして扱うこと。
4. 返り値は以下のいずれかとすること。
 a. 文字列
    この場合、この文字列は候補とアノテーションを両方含みうるものとして処
    理される。

 b. cons cell (候補 . アノテーション)
    この場合、候補はもうアノテーションを含まないものとして処理される。
    アノテーションについては先頭が \";\" かどうかを調べた上で処理される。

 c. cons cell (候補 . (セパレータ . アノテーション))
    この場合、候補はもうアノテーションを含まないものとして処理される。
    セパレータは通常の \";\" の代わりに利用される。アノテーションはもう
    セパレータを含まないものとして処理される。

この関数は以下の場合に呼ばれる。

o 通常の変換動作の都度
  この場合は、候補はバッファに、アノテーションはエコーエリアなど（ユーザ
  の設定によって異なる場所）に表示される。セパレータは表示されない。

o 候補一覧を表示するとき (候補の文字列の後ろにアノテーションが付加される)
  この場合は、候補、セパレータ、アノテーションの各文字列が表示される。

 (設定例)

 (setq skk-treat-candidate-appearance-function
       (lambda (candidate listing-p)
     (cond
      ((string-match \";\" candidate)
       (put-text-property 0 (match-beginning 0)
                  \\='face (if listing-p \\='tooltip \\='underline)
                  candidate)
       (put-text-property (match-beginning 0)
                  (length candidate) \\='face \\='shadow candidate))
      (t
       (put-text-property 0 (length candidate)
                  \\='face (if listing-p \\='tooltip \\='underline)
                  candidate)))
      candidate))

"
  :type '(radio (const :tag "設定サンプル1" skk-treat-candidate-sample1)
                (const :tag "設定サンプル2" skk-treat-candidate-sample2)
                (const :tag "指定しない" nil)
                (function :tag "任意の関数"))
  :group 'skk-annotation
  :group 'skk-visual)

(defface skk-treat-default
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark)) (:foreground "white")))
  "背景なしの単純な face。`default' の代わりに使う。"
  :group 'skk-visual)

;;; -- Internal constants and variables of skk.el
(defconst skk-coding-system-alist
  '(("euc" . euc-jis-2004)
    ("ujis" . euc-jis-2004)
    ("sjis". japanese-shift-jis-2004)
    ("jis" . iso-2022-jp-3))
  "coding-system の文字列表現と、シンボル表現の連想リスト。")

(defconst skk-kana-rom-vector
  ["x" "a" "x" "i" "x" "u" "x" "e" "x" "o" "k" "g" "k" "g" "k" "g"
   "k" "g" "k" "g" "s" "z" "s" "j" "s" "z" "s" "z" "s" "z" "t" "d"
   "t" "d" "t" "t" "d" "t" "d" "t" "d" "n" "n" "n" "n" "n" "h" "b"
   "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "m" "m" "m"
   "m" "m" "x" "y" "x" "y" "x" "y" "r" "r" "r" "r" "r" "x" "w" "x"
   "x" "w" "n"]
  "かな文字からローマ字への変換ルール。
下記の該当するかな文字をその文字のローマ字プレフィックスで現わしたもの。
    ぁ  あ  ぃ  い  ぅ  う  ぇ  え  ぉ  お  か  が  き  ぎ  く  ぐ
    け  げ  こ  ご  さ  ざ  し  じ  す  ず  せ  ぜ  そ  ぞ  た  だ
    ち  ぢ  っ  つ  づ  て  で  と  ど  な  に  ぬ  ね  の  は  ば
    ぱ  ひ  び  ぴ  ふ  ぶ  ぷ  へ  べ  ぺ  ほ  ぼ  ぽ  ま  み  む
    め  も  ゃ  や  ゅ  ゆ  ょ  よ  ら  り  る  れ  ろ  ゎ  わ  ゐ
    ゑ  を  ん"
  ;; (length skk-kana-rom-vector)
  ;; --> 83
  ;; (setq kana '("ぁ" "あ" "ぃ" "い" "ぅ" "う" "ぇ" "え" "ぉ" "お"
  ;;          "か" "が" "き" "ぎ" "く" "ぐ" "け" "げ" "こ" "ご"
  ;;              "さ" "ざ" "し" "じ" "す" "ず" "せ" "ぜ" "そ" "ぞ"
  ;;              "た" "だ" "ち" "ぢ" "っ" "つ" "づ" "て" "で" "と" "ど"
  ;;          "な" "に" "ぬ" "ね" "の" "は" "ば" "ぱ" "ひ" "び" "ぴ"
  ;;          "ふ" "ぶ" "ぷ" "へ" "べ" "ぺ" "ほ" "ぼ" "ぽ"
  ;;          "ま" "み" "む" "め" "も" "ゃ" "や" "ゅ" "ゆ" "ょ" "よ"
  ;;              "ら" "り" "る" "れ" "ろ" "ゎ" "わ" "ゐ" "ゑ" "を" "ん"))
  ;; (length kana)
  ;; --> 83
  ;; (mapcar (lambda (s) (- (char-octet (string-to-char s) 1) 33))
  ;;    kana)
  ;; --> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25\
  ;;      26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48\
  ;;      49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71\
  ;;      72 73 74 75 76 77 78 79 80 81 82)
  )

(defconst skk-default-jisx0208-latin-vector
  ;; note that skk-jisx0208-latin-vector is a user variable.
  ;; skk.el ロード前に ~/.emacs.d/init.el などで、skk-jisx0208-latin-vector の別の値をユー
  ;; ザーが直接書いたり、skk.el ロード後にこの値を aset で直接いじったりしな
  ;; ければ default-value で skk-jisx0208-latin-vector にアクセスすることで
  ;; skk-default-jisx0208-latin-vector の値を保持することもできようが、それは
  ;; 望めない...。
  [nil  nil  nil  nil  nil  nil  nil  nil
        nil  nil  nil  nil  nil  nil  nil  nil
        nil  nil  nil  nil  nil  nil  nil  nil
        nil  nil  nil  nil  nil  nil  nil  nil
        "　"  "！" "”" "＃" "＄" "％" "＆" "’"
        "（" "）" "＊" "＋" "，" "－" "．" "／"
        "０" "１" "２" "３" "４" "５" "６" "７"
        "８" "９" "：" "；" "＜" "＝" "＞" "？"
        "＠" "Ａ" "Ｂ" "Ｃ" "Ｄ" "Ｅ" "Ｆ" "Ｇ"
        "Ｈ" "Ｉ" "Ｊ" "Ｋ" "Ｌ" "Ｍ" "Ｎ" "Ｏ"
        "Ｐ" "Ｑ" "Ｒ" "Ｓ" "Ｔ" "Ｕ" "Ｖ" "Ｗ"
        "Ｘ" "Ｙ" "Ｚ" "［" "＼" "］" "＾" "＿"
        "‘" "ａ" "ｂ" "ｃ" "ｄ" "ｅ" "ｆ" "ｇ"
        "ｈ" "ｉ" "ｊ" "ｋ" "ｌ" "ｍ" "ｎ" "ｏ"
        "ｐ" "ｑ" "ｒ" "ｓ" "ｔ" "ｕ" "ｖ" "ｗ"
        "ｘ" "ｙ" "ｚ" "｛" "｜" "｝" "～" nil]
  "`skk-jisx0208-latin-region' で参照する文字テーブル。
\"ascii\" -> \"ａｓｃｉｉ\" のような全角文字へ変換する際に利用する。")

(defconst skk-kana-cleanup-command-list
  '(skk-undo
    skk-kakutei
    skk-delete-backward-char
    skk-insert
    skk-try-completion
    skk-completion-wrapper
    skk-previous-candidate))

(defconst skk-delete-backward-char-commands
  ;; following two are SKK adviced.
  ;;viper-del-backward-char-in-insert
  ;;vip-del-backward-char-in-insert
  '(backward-delete-char-untabify
    backward-delete-char
    backward-or-forward-delete-char
    delete-backward-char
    picture-backward-clear-column))

(defconst skk-undo-commands
  '(undo advertised-undo))

(defconst skk-quote-char-alist
  '((?\; . "\\073")
    (?/ . "\\057")
    (?\n . "\\n")
    (?\r . "\\r")
    (?\" . "\\\"")
    (?\\  . "\\\\"))
  "辞書エントリ内に含めてはならない文字を置き変えるための連想リスト。
`;' は、註釈と関係ない場合だけ置換する。")

(defvar skk-charset-list nil
  "SKK が扱う文字集合のリスト。SKK 初回起動時に GNU Emacs 23 以上であれば設定される。")

(defvar skk-emacs-id nil
  "複数の emacs プロセスを識別する文字列。
ひとつの個人辞書ファイルを複数の emacs 上で起動している SKK で共有すると
きに参照する。")

(defvar skk-jisyo-update-vector nil
  "`skk-share-private-jisyo' 有効時に辞書バッファ更新情報を保持する vector.
長さは `skk-jisyo-save-count' より長くなるように設定している。
辞書バッファ更新の記録を保存し、辞書バッファを辞書ファイルにセーブするときに、
他の SKK が辞書ファイルに最近アクセスしているときには、辞書ファイルをバッファ
に読み込んでから、`skk-jisyo-update-vector' を用いてバッファを更新直し、その
結果をファイルにセーブする。")

(defvar skk-rule-tree nil
  "ローマ字 -> かな変換の状態遷移規則を表すツリーの初期状態。
最初に `skk-mode' を起動したときに `skk-rom-kana-base-rule-list' と
`skk-rom-kana-rule-list' から木の形にコンパイルされる。
\\[skk-restart] によっても再コンパイルされる。")

(defvar skk-insert-new-word-function nil
  "候補を挿入したときに `funcall' される関数を保存する変数。")

(defvar skk-mode-invoked nil
  "Non-nil であれば、Emacs を起動後既に `skk-mode' を起動したことを示す。")

(defvar skk-kakutei-count 0
  "変換候補を確定したカウントを保持する変数。
`skk-record-file' の \"確定:\" 項目のカウンター。")

(defvar skk-touroku-count 0
  "辞書登録したカウントを保持する変数。
`skk-record-file' の \"登録:\" 項目のカウンター。")

(defvar skk-update-jisyo-count 0
  "個人辞書を更新した回数。
この変数の数値が `skk-jisyo-save-count' 以上となったときに個人辞書が
オートセーブされる。
個人辞書がセーブされるとイニシャライズされる。")

(defvar skk-kakutei-history nil
  "送りなしで確定された見出し語・候補の履歴。

   (\"みだしご\" \"見出し語\" buffer)

   という形式のリスト。")

(defvar skk-minibuffer-origin-mode nil
  "入力モードを表わすシンボル。
有効な値は、`hiragana', `katakana', `abbrev', `latin', `jisx0208-latin'
もしくは nil のいずれか。")

(defvar skk-menu nil)

(skk-deflocalvar skk-modeline-input-mode nil)
(put 'skk-modeline-input-mode 'risky-local-variable t)

(defvar skk-indicator-alist nil)

(defvar skk-buffer-undo-list nil)

(defvar skk-inline-overlays nil)

(defvar skk-latin-mode-map nil
  "*アスキーモードのキーマップ。")
(defvar skk-j-mode-map nil
  "*かなモードのキーマップ。")
(defvar skk-jisx0208-latin-mode-map nil
  "*全英モードのキーマップ。")
(defvar skk-abbrev-mode-map nil
  "*SKK abbrev モードのキーマップ。")

(defvar skk-henkan-in-minibuff-nest-level nil)

(defvar skk-menu-items
  ;; SKK メニューの定義。
  '("SKK"
    ("Convert Region and Echo"
     ("Gyakubiki"
      ["to Hiragana" skk-gyakubiki-message skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
        (lambda (start end)
          (interactive "r")
          (skk-gyakubiki-message start end 'all-candidates)))
       skk-use-kakasi]
      ["to Katakana" skk-gyakubiki-katakana-message skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
        (lambda (start end)
          (interactive "r")
          (skk-gyakubiki-katakana-message
           start end 'all-candidates)))
       skk-use-kakasi])
     ("Hurigana"
      ["to Hiragana" skk-hurigana-message skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
        (lambda (start end)
          (interactive "r")
          (skk-hurigana-message start end 'all-candidates)))
       skk-use-kakasi]
      ["to Katakana" skk-hurigana-katakana-message skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
        (lambda (start end)
          (interactive "r")
          (skk-hurigana-katakana-message
           start end 'all-candidates)))
       skk-use-kakasi]))
    ("Convert Region and Replace"
     ("Gyakubiki"
      ["to Hiragana" skk-gyakubiki-region skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
        (lambda (start end)
          (interactive "r")
          (skk-gyakubiki-region start end 'all-candidates)))
       skk-use-kakasi]
      ["to Katakana" skk-gyakubiki-katakana-region skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
        (lambda (start end)
          (interactive "r")
          (skk-gyakubiki-katakana-region
           start end 'all-candidates)))
       skk-use-kakasi])
     ("Hurigana"
      ["to Hiragana" skk-hurigana-region skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
        (lambda (start end)
          (interactive "r")
          (skk-hurigana-region start end 'all-candidates)))
       skk-use-kakasi]
      ["to Katakana" skk-hurigana-katakana-region skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
        (lambda (start end) (interactive "r")
          (skk-hurigana-katakana-region
           start end 'all-candidates)))
       skk-use-kakasi])
     ["Hiragana to Katakana" skk-katakana-region t]
     ["Katakana to Hiragana" skk-hiragana-region t]
     ["Ascii to Zenkaku" skk-jisx0208-latin-region t]
     ["Zenkaku to Ascii" skk-latin-region t]
     ["Kana and Zenkaku to Romaji" skk-romaji-region skk-use-kakasi])
    ["Count Jisyo Candidates" skk-count-jisyo-candidates t]
    ["Save Jisyo" skk-save-jisyo t]
    ["Undo Kakutei" skk-undo-kakutei t]
    ["Restart SKK" skk-restart t]
    ["Version" (message "%s" (skk-version)) t])
  "Menu used in SKK mode.")

(defvar skk-quit-commands '(keyboard-quit abort-recursive-edit
                                          skk-kanagaki-bs
                                          skk-kanagaki-esc))

;; ---- buffer local variables

;; <フラグ類>

;;(skk-deflocalvar skk-current-henkan-data
;;  '(;; global variables

;;    ;; バッファローカル変数のデフォルト値を設定すると、これを直接書換えした
;;    ;; ときに他のバッファから見える値も変わってしまう。global なフラグはこれ
;;    ;; を利用してデフォルト値を与えておく。

;;    ;; Emacs を起動後既に skk-mode を起動したことを示す
;;    (invoked . nil)

;;    ;; skk-isearch 関数をコールするためのフラグ
;;    (isearch-message . nil)

;;    ;; 変換候補を確定したカウントを保持する変数
;;    (kakutei-count . 0)

;;    ;;入力モードを表わすシンボル
;;    (minibuffer-origin-mode . nil)

;;    ;; 辞書登録したカウントを保持する変数
;;    (touroku-count . 0)

;;    ;; 辞書を更新した回数
;;    (update-jisyo-count . 0)

;;    ;; buffer-local variables.

;;    ;; `skk-search-prog-list' の現在の値を保存するリスト
;;    ;; (current-search-prog-list . nil)

;;    ;; ミニバッファで候補を次々に表示して、候補が尽きたことを示す
;;    ;; (exit-show-candidates . nil)

;;    ;; ▼モード (変換中) であることを示す
;;    ;; (henkan-active . nil)

;;    ;; `skk-henkan-list' のリストのインデクスで現在の候補を差すもの
;;    ;; (henkan-count . -1)

;;    ;; 変換終了ポイントを示すマーカー
;;    ;; (henkan-end-point . nil)

;;    ;; ミニバッファで辞書登録を行ったときにこのフラグが立つ
;;    ;; (henkan-in-minibuff-flag . nil)

;;    ;; 変換すべき見出し語
;;    ;; (henkan-key . nil)

;;    ;; 変換結果の候補のリスト
;;    ;; (henkan-list . nil)

;;    ;; 現在の変換の送り仮名部分
;;    ;; (henkan-okurigana . nil)

;;    ;; ▽モード (変換対象の文字列決定のためのモード) であることを示す
;;    ;; (henkan-on . nil)

;;    ;; 変換開始ポイントを示すマーカー
;;    ;; (henkan-start-point . nil)

;;    ;; 確定して良い候補を見つけた状態であることを指す
;;    ;; (kakutei-flag . nil)

;;    ;; かな文字の開始ポイントを示すマーカー
;;    ;; (kana-start-point . nil)

;;    ;; 入力モードがカナモードであることを示す
;;    ;; (katakana . nil)

;;    ;; 辞書の送り有りエントリの終了点を示すバッファポイント
;;    ;; (okuri-ari-max . nil)

;;    ;; 辞書の送り有りエントリの開始点を示すバッファポイント
;;    ;; (okuri-ari-min . nil)

;;    ;; 変換すべき語の送り仮名の部分のプレフィックス
;;    ;; (okuri-char . nil)

;;    ;; `skk-henkan-list' のインデクスで自動送り処理、もしくはサ変検索で
;;    ;; 検索した最後の候補を指すもの
;;    ;; (okuri-index-max . -1)

;;    ;; `skk-henkan-list' のインデクスで自動送り処理、もしくはサ変検索で
;;    ;; 検索した最初の候補を指すもの
;;    ;; (okuri-index-min . -1)

;;    ;; 辞書の送りなしエントリの開始点を示すバッファポイント
;;    ;; (okuri-nasi-min . nil)

;;    ;; 送り仮名部分が入力中であることを示す
;;    ;;(okurigana . nil)

;;    ;; 送り仮名の開始ポイントを示すマーカー
;;    ;; (okurigana-start-point . nil)

;;    ;; 入力するかなを決定するためのプレフィックス
;;    ;; (prefix . "")

;;    ;; この変数に保持されるポイントが現在のポイントと異なる場合、
;;    ;; `skk-with-point-move' が使われていないコマンドを動作させると
;;    ;; `skk-after-point-move' が作動する
;;    ;; (previous-point . nil)

;;    ;; `skk-insert' もしくは `skk-jisx0208-latin-insert' で連続入力した
;;    ;; 文字数を表わすカウンター
;;    ;; (self-insert-non-undo-count . 1)))

(skk-deflocalvar skk-mode nil "\
Non-nil であれば、カレントバッファで現在 `skk-mode' を起動していることを示す。")

(skk-deflocalvar skk-latin-mode nil
  "Non-nil であれば、入力モードがアスキーモードであることを示す。")

(skk-deflocalvar skk-j-mode nil
  "Non-nil であれば、入力モードがかな・カナモードであることを示す。")

(skk-deflocalvar skk-katakana nil
  "Non-nil であれば、入力モードがカナモードであることを示す。
\"(and (not skk-katakana) skk-j-mode)\" が t であれば、かなモードであることを
示す。")

(skk-deflocalvar skk-jisx0208-latin-mode nil
  "Non-nil であれば、入力モードが全英モードであることを示す。")

(skk-deflocalvar skk-abbrev-mode nil
  "Non-nil であれば、入力モードが SKK abbrev モードであることを示す。")

(skk-deflocalvar skk-okurigana nil
  "Non-nil であれば、送り仮名部分が入力中であることを示す。")

(skk-deflocalvar skk-henkan-mode nil
  "変換モードを示す。
`on' であれば、▽モード。
`active' であれば、▼モード。
`nil' であれば、確定入力モード。")

(skk-deflocalvar skk-kakutei-flag nil
  "Non-nil なら確定して良い候補を見つけた状態であることを指す。")

(skk-deflocalvar skk-kakutei-henkan-flag nil
  "Non-nil なら確定変換する事を指す。
`skk-search-kakutei-jisyo-file' や、ユーザ自作の確定変換用プログラムは
この変数をセットする。

この変数が Non-nil にセットされても、変換して最初に得られた候補でなければ
確定変換されないことに注意。")

(skk-deflocalvar skk-exit-show-candidates nil
  "ミニバッファで候補を次々に表示して、候補が尽きたときに non-nil となる。
その値はリストで、car に `skk-henkan-show-candidates' 関数で while ループを
回った回数を示す一時変数 loop の値を、cdr 部に最後にミニバッファに表示した
1 つ前の候補群の最後の要素を指すインデクスが代入される。")

(skk-deflocalvar skk-insert-keysequence nil
  "関数 `skk-insert' 内でキーシーケンスを蓄積する。")

;; <キーマップ関連>
(skk-deflocalvar skk-current-rule-tree nil
  "ローマ字 -> かな変換の状態遷移規則を表わすツリーの現時点の状態。
ローマ字入力の初期では `skk-rule-tree' と同一の状態で、文字入力が進むに
つれ、木をたどってゆく状態の遷移を表す。")

;; <辞書関連の変数>
(skk-deflocalvar skk-okuri-ari-min nil
  "SKK 辞書の送り有りエントリの開始点を示すバッファポイント。")

(skk-deflocalvar skk-okuri-ari-max nil
  "SKK 辞書の送り有りエントリの終了点を示すバッファポイント。
`skk-jisyo' のバッファでは辞書の更新の必要があるためにマーカーが代入される。")

(skk-deflocalvar skk-okuri-nasi-min nil
  "SKK 辞書の送りなしエントリの開始点を示すバッファポイント。
`skk-jisyo' のバッファでは辞書の更新の必要があるためにマーカーが代入される。")

;; <その他>
(skk-deflocalvar skk-mode-line nil
  "SKK のモードを示すモードラインの文字列。
`skk-mode-string', `skk-hiragana-mode-string', `skk-katakana-mode-string',
 `skk-jisx0208-latin-mode-string' のいずれかが代入される。")

(skk-deflocalvar skk-previous-point nil
  "`skk-with-point-move' 関連変数。
この変数に保持されるポイントが現在のポイントと異なる場合、`skk-with-point-move'
が使われていないコマンドを動作させると、`skk-after-point-move' が作動する。")

(skk-deflocalvar skk-prefix ""
  "入力するかなを決定するためのプレフィックス。")

(defface skk-prefix-hiragana-face
  '((((class color) (type tty))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "coral4"))
    (((class color) (background dark))
     (:foreground "pink"))
    (((class grayscale)) (:underline t)))
  "*かなモードのローマ字プレフィックスの face 属性。"
  :group 'skk-visual)

(defface skk-prefix-katakana-face
  '((((class color) (type tty))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "forestgreen"))
    (((class color) (background dark))
     (:foreground "green"))
    (((class grayscale)) (:underline t)))
  "*カナモードのローマ字プレフィックスの face 属性。"
  :group 'skk-visual)

(defface skk-prefix-jisx0201-face
  '((((class color) (type tty))
     (:foreground "blue"))
    (((class color) (background light))
     (:foreground "blueviolet"))
    (((class color) (background dark))
     (:foreground "thistle"))
    (((class grayscale)) (:underline t)))
  "*JISX0201 モードのローマ字プレフィックスの face 属性。"
  :group 'skk-visual)

(skk-deflocalvar skk-prefix-overlay nil
  "`skk-prefix' を表示するために使用される overlay。
`skk-echo' の値が non-nil のときに使用される。")

(skk-deflocalvar skk-henkan-start-point nil
  "変換開始ポイントを示すマーカー。")

(skk-deflocalvar skk-henkan-end-point nil
  "変換終了ポイントを示すマーカー。")

(skk-deflocalvar skk-kana-start-point nil
  "かな文字の開始ポイントを示すマーカー。")

(skk-deflocalvar skk-okurigana-start-point nil
  "送り仮名の開始ポイントを示すマーカー。")

(skk-deflocalvar skk-henkan-key nil
  "変換すべき見出し語。
例えば、\"▽かな\" を変換すれば、`skk-henkan-key' には \"かな\" が代入される。
\"▽わら*う\" のような送りありの変換の場合には、\"わらu\" のように、漢字部分の
読みがな + 送り仮名の最初の文字のローマ字のプレフィックスが代入される。")

(skk-deflocalvar skk-okuri-char nil
  "変換すべき語の送り仮名の部分のプレフィックス。
例えば、\"おく*り\" を変換するときは、`skk-okuri-char' は \"r\"。
`skk-okuri-char' が non-nil であれば、送りありの変換であることを示す。")

(skk-deflocalvar skk-henkan-okurigana nil
  "現在の変換の送り仮名部分。
例えば、\"▽うまれ*る\" を変換すれば、`skk-henkan-okurigana' には \"る\" が代入
される。")

(skk-deflocalvar skk-last-kakutei-henkan-key nil
  "確定辞書により最後に確定したときの見出し語。
確定辞書による確定の直後に x キーを押すと確定がアンドゥされて、確定前の状態で
この見出し語がカレントバッファに挿入される。")

(skk-deflocalvar skk-henkan-list nil
  "変換結果の候補のリスト。
例えば、\"▽な*く\" という変換すれば、`skk-henkan-list' は
\(\"鳴\" \"泣\" \"無\" \"亡\") のようになる。")

(skk-deflocalvar skk-henkan-count -1
  "`skk-henkan-list' のリストのインデクスで現在の候補を差すもの。")

(skk-deflocalvar skk-self-insert-non-undo-count 1
  "連続入力した文字数を表わすカウンター。
`skk-insert' もしくは `skk-jisx0208-latin-insert' でカウントされる。
Emacs のオリジナルの動作では、`self-insert-command' にバインドされたキー入力は
連続 20 回までが 1 つのアンドゥの対象となる。この動作をエミュレートするための
カウンター。このカウンターが、20 未満であるときは、入力のたびに
`cancel-undo-boundary' がコールされる。")

(skk-deflocalvar skk-current-search-prog-list nil
  "`skk-search-prog-list' の現在の値を保存するリスト。
最初の変換時は `skk-search-prog-list' の全ての値を保持し、変換を繰り返すたびに
1 つずつ短くなってゆく。")

(defvar skk-search-state nil)
(defvar skk-search-ex-state nil)

;; for skk-undo-kakutei
(skk-deflocalvar skk-last-henkan-data nil
  "最後に行った変換に関するデータの連想リスト。デフォルトのキー
は、`henkan-key', `henkan-okurigana', `okuri-char',
`henkan-list', `henkan-point', `henkan-buffer', `abbrev-mode' の各
シンボル。
 (skk-num を require しているときは、num-list が追加される)。")

(skk-deflocalvar skk-undo-kakutei-flag nil
  "Non-nil ならば、確定アンドゥ中であることを指す。")

(skk-deflocalvar skk-undo-kakutei-prev-state nil
  "`skk-undo-kakutei' が呼ばれた時の入力モードの状態。")

(skk-deflocalvar skk-undo-kakutei-previous-point nil
  "確定アンドゥ直前のポイントを示すマーカー。")

(skk-deflocalvar skk-undo-kakutei-previous-length nil
  "確定アンドゥする対象の変換結果の長さ。")

(skk-deflocalvar skk-henkan-overlay nil
  "候補を表示するときに使用する Overlay。")

(skk-deflocalvar skk-henkan-in-minibuff-flag nil
  "ミニバッファで辞書登録を行ったときにこのフラグが立つ。
`skk-remove-common' で参照される。")

(skk-deflocalvar skk-okuri-index-min -1
  "`skk-henkan-list' のインデクスを挿すポインタのひとつ。
自動送り処理で検索した最初の候補を指す。")

(skk-deflocalvar skk-okuri-index-max -1
  "`skk-henkan-list' のインデクスを挿すポインタのひとつ。
自動送り処理で検索した最後の候補を指す。")

(skk-deflocalvar skk-last-buffer-undo-list nil
  "▽モードに入る直前の `buffer-undo-list' を退避しておく変数。")

(skk-deflocalvar skk-after-prefix nil
  "t であれば、接頭辞入力後の状態にあることを表す。
接頭辞入力開始時に t にセットされ、続く語の確定後に nil にセットされる。")

;; skk-act.el related.
(defcustom skk-use-act nil
  "*Non-nil であれば拡張ローマ字入力 ACT を利用する。"
  :type 'boolean
  :group 'skk-act)

(defcustom skk-act-use-normal-y nil
  "*Non-nil であれば \"y\" を使った拗音の入力を有効にする."
  :type 'boolean
  :group 'skk-act)

(defcustom skk-act-load-hook nil
  "*skk-act を load した後に実行される hook."
  :type 'hook
  :group 'skk-act)

;; skk-azik.el related.
(defcustom skk-use-azik nil
  "*Non-nil であれば拡張ローマ字入力 AZIK を利用する。"
  :type 'boolean
  :group 'skk-azik)

(defcustom skk-azik-keyboard-type 'jp106
  "*AZIK で使うときのキーボードのタイプをシンボルで指定する。
o \\='jp106    日本語 106 キーボード (デフォルト)
o \\='jp-pc98  NEC PC-98 キーボード
o \\='us101    英語キーボード  ※ jp106 及び jp-pc98 以外のシンボル

nil が指定された場合は、キーボードのタイプの違いを吸収する割当てを行いません。"
  :type '(radio (const :tag "日本語 106 キーボード" jp106)
                (const :tag "NEC PC-98 キーボード" jp-pc98)
                (const :tag "英語キーボード" us101)
                (const :tag "キーボード依存処理を無効にする" nil))
  :group 'skk-azik)

(defcustom skk-azik-load-hook nil
  "*skk-azik を load した後に実行される hook"
  :type 'hook
  :group 'skk-azik)

;; skk-annotation.el related.
(defcustom skk-show-annotation nil
  "*Non-nil であれば、変換時にアノテーションを表示する。
かな漢字変換の際、辞書の候補に含まれる `;' 以降の文字列をアノテーションとして\
エコーエリア、別 Window またはツールティップに表示する。"
  :type '(radio (const :tag "常に表示" t)
                (const :tag "候補一覧では非表示" (not list))
                (const :tag "ミニバッファでは非表示" (not minibuf))
                (const :tag "候補一覧とミニバッファでは非表示"
                       (not list minibuf))
                (const :tag "非表示" nil))
  :group 'skk-basic
  :group 'skk-annotation)

(defcustom skk-annotation-delay 1.0
  "*アノテーションを表示するまでの遅延。単位は秒。"
  :type 'number
  :group 'skk-annotation)

(defcustom skk-annotation-loop-interval 0.1
  "*アノテーションを表示中のプロセス待ち時間 (秒)。
高速な環境では小さめに設定するとレスポンスが快適になる。
低速な環境では大きめに設定すると動作が改善されうる。"
  :type 'number
  :group 'skk-annotation)

(defcustom skk-annotation-toggle-display-char ?^
  "*候補一覧を表示中にアノテーション表示を切り替えるキーキャラクタ。"
  :type 'character
  :group 'skk-annotation)

(defcustom skk-annotation-copy-key "\C-w"
  "*アノテーションをコピーするキー。
このキーをタイプすると、現在表示中のアノテーションを kill ring に保存する。
保存した内容を Emacs 以外のアプリケーションで利用したい場合は
変数 `interprogram-cut-function' を設定する。"
  :type (if (get 'key-sequence 'widget-type)
            'key-sequence
          'sexp)
  :group 'skk-annotation)

(defcustom skk-annotation-browse-key "\C-o"
  "*アノテーションを URL と見做してブラウズするキー。
このキーをタイプすると、現在表示中のアノテーションを関数 `browse-url' に渡す。"
  :type (if (get 'key-sequence 'widget-type)
            'key-sequence
          'sexp)
  :group 'skk-annotation)

(defcustom skk-annotation-function nil
  "*アノテーションを表示するか否かを判定するためにコールする関数を指定する。
アノテーションの対象とする文字列を引数にして `funcall' され、戻り値
が non-nil であればアノテーションを表示する。
候補一覧時には呼ばれない。

アノテーション表示の判断は `skk-treat-candidate-appearance-function' でも
実現できる。"
  :type 'function
  :group 'skk-annotation)

(defcustom skk-annotation-show-as-message t
  "*Non-nil であれば、アノテーションをエコーエリアに表示する。
nil であれば、別なウィンドゥに表示する。
この変数よりも `skk-show-tooltip' の設定が優先される。"
  :type 'boolean
  :group 'skk-annotation)

(defcustom skk-annotation-mode-hook nil
  "*SKK annotation mode に入ったときのフック。"
  :type 'hook
  :group 'skk-annotation)

(defcustom skk-annotation-lookup-DictionaryServices nil
  "*Non-nil であれば、Apple OS X で DictionaryServices より意味を取得する。
この場合、python を inferior process として起動する。
この設定は `skk-annotation-lookup-dict' より優先される。
Max OS X 以外の環境では機能しない。

候補一覧でもこの機能を使いたい場合は `always' に設定することで実現できる。
ただし、`always' は `skk-treat-candidate-appearance-function' を上書きしてし
まうため、上級者向けではない。"
  :type '(radio (const :tag "通常の変換時に辞書を参照する" t)
                (const :tag "上記に加え候補一覧でも参照する" always)
                (const :tag "利用しない" nil))
  :group 'skk-annotation)

(defcustom skk-annotation-python-program (executable-find "python")
  "*DictionaryServices のために起動する python のファイル名。"
  :type '(radio (file)
                (const nil))
  :group 'skk-annotation)

(defcustom skk-annotation-lookup-lookup nil
  "*Non-nil であれば elisp `lookup' から変換候補の意味を取得する。

候補一覧でもこの機能を使いたい場合は `always' に設定することで実現できる。
ただし、`always' は `skk-treat-candidate-appearance-function' を上書きし
てしまうため、上級者向けではない。"
  :type '(radio (const :tag "通常の変換時に lookup を参照する" t)
                (const :tag "上記に加え候補一覧でも参照する" always)
                (const :tag "利用しない" nil))
  :group 'skk-annotation
  :group 'skk-lookup)

(defcustom skk-annotation-lookup-dict nil
  "*Non-nil であれば、外部プログラムを読んで変換候補の意味を表示する。
外部プログラムは `skk-annotation-dict-program' で指定する。"
  :type 'boolean
  :group 'skk-annotation)

(defcustom skk-annotation-dict-program
  (cond ((eq system-type 'darwin)
         skk-annotation-python-program)
        (t
         nil))
  "*変換候補の意味を表示するための外部プログラムのファイル名。"
  :type '(radio (file)
                (const nil))
  :group 'skk-annotation)

(defcustom skk-annotation-dict-program-arguments
  (cond ((eq system-type 'darwin)
         '("-c" "import sys, DictionaryServices; word = sys.argv[1].decode(\"utf-8\"); print DictionaryServices.DCSCopyTextDefinition(None, word, (0, len(word))).encode(\"utf-8\")"))
        (t
         nil))
  "*変換候補の意味を表示するための外部プログラムの引数のリスト。"
  :type '(radio (repeat string)
                (const nil))
  :group 'skk-annotation)

(defcustom skk-annotation-dict-coding-system 'utf-8
  "*外部プログラムからアノテーション取得する際に用いるコード系。"
  :type 'coding-system
  :group 'skk-annotation)

(defcustom skk-annotation-other-sources
  (if (eq system-type 'darwin)
      '(lookup.el 辞書 ja.wiktionary ja.wikipedia
                  en.wiktionary simple.wikipedia en.wikipedia)
    '(lookup.el ja.wiktionary ja.wikipedia
                en.wiktionary simple.wikipedia en.wikipedia))
  "*アノテーションに使う情報のソースを指定するオプション。
標準では Wiktionary, Wikipedia (日本語版、英語版) を参照する。
Apple OS X では標準の「辞書」を利用できる。"
  :type '(radio (repeat :tag "\
次のソースを利用する (以下に項目と順番を指定してください)" symbol)
                (const :tag "Wikimedia などの情報を利用しない" nil))
  :group 'skk-annotation)

(make-obsolete-variable 'skk-annotation-wikipedia-sources
                        'skk-annotation-other-sources
                        "DDSKK 14.4")

(defcustom skk-annotation-wikipedia-key "\C-i"
  "*アノテーションとして Wikipedia の内容を表示するキー。
オプション `skk-show-annotation' が non-nil のときだけ有効。"
  :type (if (get 'key-sequence 'widget-type)
            'key-sequence
          'sexp)
  :group 'skk-annotation)

(defcustom skk-annotation-wiktionary-preferred-lang-alist
  '(("en" "Translingual" "English" "Japanese")
    ("ja" "日本語" "漢字" "英語" "古典日本語"))
  "*Wiktionary の記述言語と、単語所属言語の優先順との連想リスト。"
  :type '(repeat (repeat string))
  :group 'skk-annotation)

(defconst skk-annotation-buffer "*SKK annotation*")

(defvar skk-annotation-first-candidate nil)

(defvar skk-annotation-mode-map nil
  "*SKK annotation モードのキーマップ。")

(defvar skk-annotation-original-window-configuration nil
  "SKK annotation mode に入る前の window configuration。
`skk-annotation-save-and-quit' を呼ぶとこの window configuration
を使って SKK annotation mode に入る前の window 状態に戻す。")

(defvar skk-annotation-target-data nil
  "annotation を付けられる候補に関するデータ。")

(defvar skk-annotation-wikipedia-message nil
  "SKK Wikipedia 利用方法を示すメッセージ (自動設定)。")

(defvar skkannot-cached-srcs nil)

(defvar skk-annotation-message nil
  "SKK Annotation 利用方法を示すメッセージ (自動設定)。")

(defvar skkannot-remaining-delay 0)

(defvar skkannot-buffer-origin nil)

(defvar skkannot-py-buffer nil)

(defconst skkannot-py-none-regexp "^\\(Traceback\\|AttributeError\\|None\\)")

(defconst skkannot-DictServ-cmd-format-str "word = u\"%s\"; \
print \" %s(word)s in DictionaryServices\" %s {'word': word}; \
print DictionaryServices.DCSCopyTextDefinition(None, word, (0, len(word)))")

;; XXX まだ不完全
(defconst skkannot-en-wiktionary-lang-regexp "\
<h2>.*<span class=\"mw-headline\".+>\
\\(<a href=.+>\\)?\
\\(Aari\\|Abanyom\\|Abaza\\|Abenaki\\|Abkhaz\\|Acehnese\\|Acholi\\|Acholi\
\\|Achumawa\\|Adangme\\|Adele\\|Adnyamathanha\\|Adyghe\\|Adzera\\|Afar\
\\|Afrikaans\\|Aghul\\|Ainu\\|Akan\\|Akawaio\\|Akkadian\\|Aklanon\\|Alabama\
\\|Albanian\\|Aleut\\|Algonquin\\|Alsatian\\|Amaimon\\|Amanab\\|Ambai\
\\|Amharic\\|Amoy\\|Amuzgo\\|Ankave\\|Ansus\\|Apala\\|\\(Egyptian \\)?Arabic\
\\|Aragonese\\|Aramaic\\|Arapaho\\|Arawak\\|Armenian\\|Aromanian\\|Assamese\
\\|Asturian\\|'Auhelawa\\|Avar\\|Avestan\\|Awabakal\\|Aymara\\|Azeri\
\\|Balinese\\|Balti\\|Bambara\\|Bandjalang\\|Baruga\\|Bashkir\\|Basque\
\\|Baure\
\\|Belarusian\\|Bengali\\|Berbice Creole Dutch\\|Betawi\\|Bhojpuri\\|Biak\
\\|Bikol\\|Bislama\\|Blackfoot\\|Bokml\\|Bosnian\\|Breton\
\\|Broome Pearling Lugger Pidgin\\|Bube\\|Bulgarian\\|Burmese\
\\|Cantonese\\|Capeverdean Crioulo\\|Catalan\\|Catawba\\|Cebuano\
\\|Central Tarahumara\\|Ch'orti'\\|Chamorro\\|Chechen\\|Cherokee\\|Cheyenne\
\\|Chichewa\\|Chickasaw\\|Chinese Pidgin English\\|Chinese\\|Chinook Jargon\
\\|Chiricahua\\|Choctaw\\|Tumbal Chol\\|Chukchee\\|Chuvash\
\\|Classical Nahuatl\\|Coatln Mixe\\|Comorian\\|Coptic\\|Cornish\\|Corsican\
\\|Cree\\|Creek\\|Crimean Tatar\\|Croatian\\|Czech\
\\|Dacian\\|Dadibi\\|Northern Dagara\\\Dalmatian\\|Danish\\|Dargwa\
\\|Darkinjung\\|Darling\\|Dharuk\\|Dhivehi\\|Dhuwal\\|Dieri\\|Dusner\\|Dutch\
\\|Dyirbal\\|Dzongkha\
\\|Egyptian\\|English\\|Erzya\\|Esan\\|Esperanto\\|Estonian\\|Etruscan\\|Ewe\
\\|Fang\\|Faroese\\|Fijian\\|Filipino\\|Finnish\\|Fon\\|French\\|Frisian\
\\|Friulian\\|Fula\
\\|Ga\\|Gabi-Gabi\\|Gagauz\\|Galician\\|Gallo\\|Gamilaraay\\|Ge'ez\\|Georgian\
\\|\\(Middle High\\)?German\\|Gilbertese\\|Golin\\|Gooniyandi\\|Gothic\
\\|\\(Ancient \\|Mycenaean \\)?Greek\\|Greenlandic\\|Guaran\\|Mby Guaran\
\\|Gujarati\\|Guugu Yimidhirr\
\\|Hausa\\|Hawaiian\\|Hebrew\\|Hindi\\|Hittite\\|Hmong\\|Hopi\\|Hungarian\
\\|Icelandic\\|Ido\\|Igbo\\|Ilocano\\|Indoneian\\|Interlingua\\|Inuktitut\
\\|Irish\\|Italian\
\\|Japanese\\|Javanese\\|Jingpho\\|Jrriais\
\\|Kabardian\\|Kabyle\\|Kadiwu\\|Kannada\\|Kanuri\\|Kapingamarangi\\|Karelian\
\\|Karitina\\|Kashmiri\\|Kashubian\\|Kaurna\\|Kazakh\\|Khmer\\|Kickapoo\
\\|Kinyarwanda\\|Kiput\\|Kirundi\\|Kokborok\\|Komi\\|Kongo\\|Korean\\|Kriol\
\\|Krisa\\|!Kung\\|Kurdish\\|Kurnai\\|Kwanyama\\|Kyrgyz\
\\|Ladino\\|Lak\\|Lakota\\|Laotian\\|Latin\\|Latvian\\|Lavukaleve\\|Lenape\
\\|Lezgi\\|Limburgish\\|Lingala\\|Lithuanian\\|Livonian\\|Lojban\
\\|Low Saxon\\|Lower Sorbian\\|Luganda\\|Luxembourgish\
\\|Maay\\|Macedonian\\|Makhuwa\\(-Meetto\\|-Shirima\\)?\\|Malagasy\\|Malay\
\\|Malayalam\\|Maliseet\\|Maltese\\|Manchu\\|Mandarin\\|Mandinka\\|Mangarevan\
\\|Mansi\\|Manx\\|Maori\\|Mapudungun\\|Marathi\\|Marau\\|Maroon\\|Marshallese\
\\|Martuthunira\\|Mati Ke\\|Mbabaram\\|Mende\\|Menominee\\|Meriam\\|Mesquakie\
\\|Mi'kmaq\\|Miami\
\\|Middle \\(Dutch\\|English\\|French\\|Korean\\|Norwegian\\|Scots\\)\
\\|Min Nan\\|Mirandese\\|Miskito\\|\\(Alcozauca \\|Yosonda \\)?Mixtec\
\\|Miyako\\|Mohegan\\|Mohican\\|Moldavian\\|Mongolian\\|Montauk\\|Munduapa\
\\|Munggui\\|Munsee\\|Murrinh-Patha\\|Mutsun\
\\|\\(Isthmus-Mecayapan \\)?Nahuatl\\|Nanticoke\\|Narragansett\\|Nauruan\
\\|Navajo\\|Ndonga\\|Neapolitan\\|Nepali\\|Nhanta\\|Niuean\\|Nootka\\|Norfuk\
\\|Norman\\|Norn\\|Northern Sami\\|Norwegian\\|Novial\\|Nynorsk\\|Nyunga\
\\|O'odham\\|Occitan\\|Ohlone\\|Ojibwe\
\\|Old \\(Church Slavonic\\|English\\|French\\|Frisian\\|High German\\|Irish\
\\|Norse\\|Prussian\\|Saxon\\|Slavonic\\)\\|Oriya\\|Oromo\
\\|Pali\\|Pangasinan\\|Panyjima\\|Papiamentu\\|Papuma\\|Pashto\
\\|Passamaquoddy\\|Paumar\\|Pennsylvania German\\|Penobscot\
\\|\\(Old \\)?Perian\\|Phoenician\
\\|Pirah\\|Pitcairnese\\|Pitjantjatjara\\|Pitta-Pitta\\|Pochutec\\|Polish\
\\|Sayula Popoluca\\|Portuguese\\|Potawatomi\\|Powhatan\
\\|Proto-\\(Germanic\\|Indo-European\\|Uralic\\)\\|Provena\\|Punjabi\
\\|Quechua\\|Quenya\
\\|Rarotongan\\|Reconstructed\\|Rohingya\\|Roman\\(i\\|ian\\|sch\\)\\|Rotokas\
\\|Rotuman\\|Russian\\|Rutul\
\\|Saanich\
\\|\\(Inari \\|Kildin \\|Lule \\|Northern \\|Pite \\|Skolt \\|Sourthern \
\\|Ter \\|Ume \\)?Sami\\|Samoan\\( Plantation Pidgin\\)?\\|Sanskrit\
\\|Sardinian\\|Scots\\|Scottish Gaelic\\|Serbian\\|Serbo-Croatian\\|Seri\
\\|Shabo\\|Shawnee\\|Shelta\\|Shona\\|Shoshoni\\|Shuar\\|Sicilian\\|Sindarin\
\\|Sindhi\\|Sinhalese\\|Slovak\\|Slovene\\|Somali\\|Upper Sorbian\\|Spanish\
\\|Sranan\\|Sumerian\\|Swahili\\|Swazi\\|Swedish\\|Syriac\
\\|Tabassaran\\|TAchelhit\\|Tagalog\\|Tahitian\\|Taimyr Pidgin Russian\\|Tajik\
\\|Tamasheq\\|Tamazight\\|Tamil\\|Tatar\\|Tausug\\|Tano\\|Telugu\\|Tetum\
\\|Thai\\|Tibetan\\|Tigrinya\\|Tiwi\\|Tocharian \\(A\\|B\\)\\|Tok Pisin\
\\|Tokelauan\\|Tongan\\|Torres Strait Creole\\|Translingual\\|Tsakhur\
\\|Tshiluba\\|Tswana\\|Tuamotuan\\|Tumbuka\\|Tupi\\|Tupinamb\\|Turkish\
\\|Turkmen\\|Tuvaluan\\|Tuvan\\|Twi\\|Tz'utujil\
\\|Ugaritic\\|Ukrainian\\|Umbundu\\|Unami\\|Unserdeutsch\\|Urdu\\|Uyghur\
\\|Uzbek\
\\|Vandalic\\|Venda\\|Veps\\|Vietnamese\\|Volapk\\|Votic\\|Vro\
\\|Wageman\\|Walloon\\|Wampanoag\\|Wangaaybuwan-Ngiyambaa\\|Warlpiri\\|Welsh\
\\|Wembawemba\\|Western Apache\\|West Frisian\\|Wik-Mungkan\\|Wiradhuri\
\\|Woi\\|Woiwurrung\\|Wolof\\|Worimi\
\\|Xavnte\\|Xhosa\\|!X\
\\|Yapese\\|Yiddish\\|Yidiny\\|Yindjibarndi\\|Yoruba\\|Yucatec\\|Yup'ik\
\\|\\(Yatzachi \\|Zoogocho \\|Isthmus \\)Zapotec\\|Zenga\\|Zhuang\
\\|Zulgo-Gemzek\\|Zulu\\|Zuni\\)\
\\(</a>\\)?\
</span></h2>"
  "en.wiktionary において言語を表すヘッダの正規表現")

(defconst skkannot-en-wiktionary-part-of-speech-regexp "\
<span class=\"mw-headline\".+>\
\\(<a href=.+>\\)?\
\\(Article\\|Noun\\|Proper Noun\\|Adjective\\|Proper Adjective\
\\|Verb\\( form\\)?\\|Intransitive\\( verb\\)?\\|Transitive\\( verb\\)?\
\\|Adverb\
\\|Conjunction\\|Interjection\\|Numeral\\|Prefix\\|Suffix\\|Particle\
\\|Preposition\\|Contraction\\|Determiner\\|Demonstrative determiner\
\\|Interrogative determiner\\|Pronoun\\|Pronominal possessive adjective\
\\|Demonstrative pronoun\\|Demonstrative adjective\
\\|Quasi-Adjective\\|Proverb\\|Counter\\|Personal pronoun\
\\|Kanji\\|Hanja\\|Hanzi\
\\|Interrogative pronoun\\|Relative pronoun\\|Auxiliary verb\\( form\\)?\
\\|Indefinite article\\|Abbreviation\\|Initialism\\|Acronym\\|Symbol\
\\|\\(Han \\|Hiragana \\|Katakana \\)character\\|Phrase\\|Letter\\)\
\\(</a>\\)?\
</span>"
  "en.wiktionary において品詞を表すヘッダの正規表現")

(defconst skkannot-ja-wiktionary-lang-regexp "\
<h2>.*<span class=\"mw-headline\".+>\
\\(<a href=.+>\\)?\
\\(.+語\\|インターリングア\\|エスペラント\\|サンスクリット\\|トキポナ\
\\|トク・ピジン\\|記号\\|漢字\\)\
\\(</a>\\)?\
</span>"
  "ja.wiktionary において言語を表すヘッダの正規表現")

(defconst skkannot-ja-wiktionary-part-of-speech-regexp "\
<span class=\"mw-headline\".+>\
\\(<a href=.+>\\)?\
\\(\
\\(\\(固有\\|\\(人称\\|疑問\\)?代\\)?名\\|\\(助\\)?動\\|形容動?\\|\
接続\\|前置\\|副\\|冠\\|関係\\|間投\\|助\\|数\\|分\\|類別\\|感動\\)\
詞.*\
\\|漢字混じり表記\\|意義\\|借用語\\|略語\\|コピュラ\\|接頭辞\\|接尾辞\
\\|人称接辞\\|平仮名\\|片仮名\\|意義\\|漢字\\|和語の漢字表記\\)\
\\(</a>\\)?\
</span>"
  "ja.wiktionary において品詞を表すヘッダの正規表現")

(skk-deflocalvar skk-annotation-mode nil
  "Non-nil であれば、annotation モードであることを示す。")

;;; skk-auto.el related.
(defcustom skk-auto-okuri-process nil
  "*Non-nil であれば、送り仮名部分を自動認識して変換を行う。
例えば、

    \"Uresii (\"UreSii\" ではなく) -> 嬉しい\"

のように変換される。但し、`skk-jisyo' (個人辞書) が、

    \"うれs /嬉/[し/嬉/]/\"

のような形式になっていることが必要である (SKK-JISYO.[SML] はこの形式に対応し
ていないので、`skk-jisyo' にこのエントリがなければならない)。

このオプション利用時は、`skk-process-okuri-early' の値は nil でなければ
ならない。"
  :type 'boolean
  :group 'skk-okurigana
  :group 'skk-auto)

(defcustom skk-okuri-search-function 'skk-okuri-search-subr-original
  "*`skk-okuri-search' で使用する関数。"
  :type 'function
  :group 'skk-auto)

(defcustom skk-auto-load-hook nil
  "*skk-auto.el をロードした後にコールされるフック。"
  :type 'hook
  :group 'skk-auto)

;; skk-cdb.el related.
(defcustom skk-cdb-large-jisyo nil
  "*個人辞書の検索の後に検索する CDB 形式辞書ファイル名。
Non-nil であれば、指定された CDB 形式辞書を Emacs から直接利用し、
高速な検索を行う。"
  :type `(radio (file :tag "辞書ファイル名"
                      ,(or (locate-file "skk/SKK-JISYO.L.cdb"
                                        (list (expand-file-name "../../.."
                                                                data-directory)))
                           (locate-file "skk/SKK-JISYO.L.cdb"
                                        (list data-directory))
                           ""))
                (const :tag "指定しない" nil))
  :group 'skk-cdb
  :group 'skk-dictionary)

(defcustom skk-cdb-coding-system 'euc-jp
  "*個人辞書の検索の後に検索する CDB 形式辞書のコーディング・システム。"
  :type 'coding-system
  :group 'skk-cdb
  :group 'skk-dictionary)

;;; skk-comp.el related.
(defcustom skk-try-completion-char ?\011 ; TAB
  "*見出し語の補完動作を行うキーキャラクタ。"
  :type 'character
  :group 'skk-comp)

(defcustom skk-next-completion-char ?.
  "*見出し語の補完動作で、次の候補を出力するキーキャラクタ。"
  :type 'character
  :group 'skk-comp)

(defcustom skk-previous-completion-char ?,
  "*見出し語の補完動作で、前の候補を出力するキーキャラクタ。"
  :type 'character
  :group 'skk-comp)

(defcustom skk-previous-completion-use-backtab t
  "*見出し語の補完動作（前候補の出力）を Shift + TAB でも行う。"
  :type 'boolean
  :group 'skk-comp)

(defcustom skk-previous-completion-backtab-key
  (cond ((not (skk-find-window-system))          [backtab])
        ((memq system-type '(darwin windows-nt)) [S-tab])
        (t                                       [S-iso-lefttab])) ;X Window System
  "*Shift + TAB に相当するキー (key event)。
`skk-previous-completion-use-backtab' が有効な際に用いられる。"
  :type (if (get 'key-sequence 'widget-type)
            'key-sequence
          'sexp)
  :group 'skk-comp)

(defcustom skk-start-henkan-with-completion-char ?\240 ; M-SPC
  "*見出し語を補完しながら▼モードに入るキーキャラクタ。"
  :type 'character
  :group 'skk-comp)

(defcustom skk-comp-load-hook nil
  "*skk-comp.el をロードした後にコールされるフック。"
  :type 'hook
  :group 'skk-comp)

(defcustom skk-kakutei-history-limit 100
  "変数 `skk-kakutei-history' の値 (連想リスト) の長さの上限。"
  :type 'integer
  :group 'skk-comp)

(defcustom skk-comp-circulate nil
  "*見出し語を補完する際の、候補の表示順を制御する。non-nil であれば、
最後の見出し語が表示された状態で更に補完を行うと入力文字列に戻る。
nil であれば、最後の見出し語が表示された状態で停止する。"
  :type 'boolean
  :group 'skk-comp)

(defcustom skk-comp-use-prefix nil
  "*見出し語を補完する際にプレフィックス(`skk-prefix')も使うか。
例えば、\"▽あk\" とある状態で補完をした時に、non-nil であれば
\"あさ\" は対象とならず、\"あか\", \"あき\" などに絞られる。
しかし実際には補完プログラムも対応している必要がある。

kakutei-first を選んだ時は、\"しんりn\" を補完すると、
\"n\" は前もって \"ん\" に確定されてから補完候補を検索するので、
\"しんりん\" 自体は候補として提示されない事に注意。"
  :type '(radio (const nil)
                (const t)
                (const kakutei-first))
  :group 'skk-comp)

(defcustom skk-comp-prefix-regexp-alist nil
  "*プレフィックスを利用した補完時に使う、プレフィックスと正規表現の連想リスト。
この変数は `skk-rule-tree' を利用して自動で要素が追加されるが、
それが期待するものでない場合には予め必要なものだけ設定しておくこと。"
  :type '(repeat (cons string regexp))
  :group 'skk-comp)

(defcustom skk-comp-kana-list-filter-function
  (lambda (kana-list prefix)
    ;; "t" 以外で "っ" を補完しない
    (unless (string= prefix "t")
      (setq kana-list (delete "っ" kana-list)))
    ;; "m" で "ん" を補完しない
    (when (string= prefix "m")
      (setq kana-list (delete "ん" kana-list)))
    ;; "w" で "う" を補完しない
    (when (string= prefix "w")
      (setq kana-list (delete "う" kana-list)))
    ;; "x" で "か", "け" を補完しない
    ;; in skk-rom-kana-base-rule-list, "xka"→"か", "xke"→"け"
    (when (string= prefix "x")
      (setq kana-list (delete "か" kana-list))
      (setq kana-list (delete "け" kana-list)))
    ;; いちおうカナモードを考えて
    (when (string= prefix "v")
      (add-to-list 'kana-list "ヴ"))
    ;; 平仮名・片仮名のみ (記号類は不要)
    (save-match-data
      (delq nil
            (mapcar (lambda (kana)
                      (when (string-match "\\(\\cH\\|\\cK\\)" kana)
                        kana))
                    kana-list))))
  "*`skk-comp-prefix-regexp-alist' に自動で要素を追加する際に利用される関数。
`skk-rule-tree' からプレフィックスに対応する \"かな\" を集めた後、
この関数によって調整を行う。"
  :type '(radio (function :tag "関数")
                (const :tag "指定しない" nil))
  :group 'skk-comp)

(defcustom skk-completion-prog-list
  '((skk-comp-by-history)
    (skk-comp-from-jisyo skk-jisyo)
    (skk-look-completion))
  "*補完関数、補完対象の辞書を決定するためのリスト。
リストの要素は、`skk-comp-first' が t である時に
新規補完候補群の生成を開始し、１回の評価につき１つの候補を返す S 式。"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-1 nil
  "*補完プログラムのリスト。
リストの要素は、`skk-completion-prog-list' と全く同様。
C-1 TAB で使われる。"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-2 nil
  "*補完プログラムのリスト。
リストの要素は、`skk-completion-prog-list' と全く同様。
C-2 TAB で使われる。"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-3 nil
  "*補完プログラムのリスト。
リストの要素は、`skk-completion-prog-list' と全く同様。
C-3 TAB で使われる。"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-4 nil
  "*補完プログラムのリスト。
リストの要素は、`skk-completion-prog-list' と全く同様。
C-4 TAB で使われる。"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-5 nil
  "*補完プログラムのリスト。
リストの要素は、`skk-completion-prog-list' と全く同様。
C-5 TAB で使われる。"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-6 nil
  "*補完プログラムのリスト。
リストの要素は、`skk-completion-prog-list' と全く同様。
C-6 TAB で使われる。"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-7 nil
  "*補完プログラムのリスト。
リストの要素は、`skk-completion-prog-list' と全く同様。
C-7 TAB で使われる。"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-8 nil
  "*補完プログラムのリスト。
リストの要素は、`skk-completion-prog-list' と全く同様。
C-8 TAB で使われる。"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-9 nil
  "*補完プログラムのリスト。
リストの要素は、`skk-completion-prog-list' と全く同様。
C-9 TAB で使われる。"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-0 nil
  "*補完プログラムのリスト。
リストの要素は、`skk-completion-prog-list' と全く同様。
C-0 TAB で使われる。"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-search-char ?~
  "*`skk-completion-search' を利用する変換を指定するキーキャラクタ。"
  :type 'character
  :group 'skk-comp)

(defcustom skk-smart-find-file-path load-path
  "*`smart-find' にファイル名を探索させるパス。
この値が指定されなければ、`smart-find-file-path' の値が代わりに使用される。"
  :type '(repeat (directory))
  :group 'skk-comp)

(defcustom skk-smart-find-ignored-file-regexp "\
\\(\\.\\(elc\\|o\\(rig\\|ld\\)?\\|diff\\)\\|,v\\|~\\|/\\)$"
  "*`smart-find' に無視されるファイル名を指定する正規表現。"
  :type 'regexp
  :group 'skk-comp)

;; ---- buffer local variables
;; 空文字列に対して skk-comp-do を呼ぶこともありうるので、"" を nil では代
;; 用できない。
(skk-deflocalvar skk-comp-key ""
  "補完すべき見出し語。")
;; 辞書登録時ミニバッファで補完した場合、元のバッファに戻ったときに
;; skk-comp-key の値が破壊されていない方がベター。

(skk-deflocalvar skk-comp-prefix ""
  "補完時の `skk-prefix'")

;; buffer local な必要は無いかも?
(skk-deflocalvar skk-current-completion-prog-list nil
  "`skk-completion-prog-list' の現在の値を保存するリスト。
最初の補完時は `skk-completion-prog-list' の全ての値を保持し、
car にある補完プログラムが nil を返すごとに 1つずつ短くなってゆく。")

(skk-deflocalvar skk-comp-first nil
  "補完プログラムに新しい候補群を生成するよう通知する。")

(skk-deflocalvar skk-comp-stack nil
  "補完した語を保存しておくスタック。")

(skk-deflocalvar skk-comp-depth 0
  "補完した語を `skk-comp-stack' から取り出す位置。")

(skk-deflocalvar skk-comp-kakutei-midasi-list nil
  "確定履歴から得られた見出し語のリスト。")

(skk-deflocalvar skk-comp-search-done nil
  "見出し語の補完用の候補検索が終了したことを示す。")

(defvar skk-comp-smart-find-files nil
  "`smart-find' が返したファイル名リストを格納する。")

(defvar skk-comp-lisp-symbols nil
  "補完された lisp symbol のリストを格納する。")

;;; skk-server-completion.el related.
(defcustom skk-server-completion-search-char ?~
  "*server completion を利用した変換を行うキーキャラクタ。"
  :type 'character
  :group 'skk-server-completion)

(defvar skk-server-completion-words nil
  "server completion により得られた見出し語のリスト。")

(defvar skk-server-disable-completion nil
  "Non-nil なら server completion の機能を無効にする。
server completion が実装されておらず、かつ無反応な辞書サーバ対策。")

;;; skk-cursor.el related.
(defcustom skk-use-color-cursor (and (skk-find-window-system)
                                     (fboundp 'x-display-color-p)
                                     (x-display-color-p))
  "*Non-nil であれば、カーソルに入力モードに応じた色を付ける。"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-cursor)

(defcustom skk-cursor-default-color
  (cdr (assq 'cursor-color (frame-parameters (selected-frame))))
  "*SKK モードのオフを示すカーソル色。
`skk-use-color-cursor' が non-nil のときに使用される。"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-hiragana-color (if (eq skk-background-mode 'light)
                                         "coral4"
                                       "pink")
  "*かなモードを示すカーソル色。
`skk-use-color-cursor' が non-nil のときに使用される。"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-katakana-color (if (eq skk-background-mode 'light)
                                         "forestgreen"
                                       "green")
  "*カナモードを示すカーソル色。
`skk-use-color-cursor' が non-nil のときに使用される。"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-jisx0201-color (if (eq skk-background-mode 'light)
                                         "blueviolet"
                                       "thistle")
  "*JISX0201 モードを示すカーソル色。
`skk-use-color-cursor' が non-nil のときに使用される。"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-jisx0208-latin-color "gold"
  "*全英モードを示すカーソル色。
`skk-use-color-cursor' が non-nil のときに使用される。"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-latin-color (if (eq skk-background-mode 'light)
                                      "ivory4"
                                    "gray")
  "*アスキーモードを示すカーソル色。
`skk-use-color-cursor' が non-nil のときに使用される。"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-abbrev-color "royalblue"
  "*skk abbrev モードを示すカーソル色。
`skk-use-color-cursor' が non-nil のときに使用される。"
  :type 'string
  :group 'skk-cursor)

;;; skk-dcomp.el related.
(defface skk-dcomp-face
  '((((class color) (type tty))
     (:foreground "DarkKhaki"))
    (((class color) (background light))
     (:foreground "DimGray" :italic t))
    (((class color) (background dark))
     (:foreground "LightGray" :italic t))
    (((class grayscale))
     (:inherit default)))
  "*Face used to highlight region dynamically completed."
  :group 'skk-dcomp)

(defface skk-dcomp-multiple-face
  '((((class color) (type tty))
     (:foreground "blue" :background "yellow"))
    (((class color) (background light))
     (:foreground "dim gray" :background "beige"))
    (((class color) (background  dark))
     (:foreground "gainsboro" :background "gray15"))
    (((class grayscale))
     (:inherit default)))
  "*動的補完の複数表示群のフェイス。"
  :group 'skk-dcomp)

(defface skk-dcomp-multiple-trailing-face
  '((((class color) (type tty))
     (:inherit skk-dcomp-multiple-face :foreground "black" :bold t))
    (((class color) (background light))
     (:inherit skk-dcomp-multiple-face :foreground "black" :bold t))
    (((class color) (background  dark))
     (:inherit skk-dcomp-multiple-face :foreground "white" :bold t))
    (((class grayscale))
     (:inherit default)))
  "*動的補完の複数表示群の補完部分のフェイス。"
  :group 'skk-dcomp)

(defface skk-dcomp-multiple-selected-face
  '((((class color) (type tty))
     (:foreground "white" :background "magenta" :bold t))
    (((class color) (background light))
     (:foreground "yellow" :background "navy" :bold t))
    (((class color) (background  dark))
     (:foreground "dark slate blue" :background "peach puff" :bold t))
    (((class grayscale))
     (:inherit default)))
  "*動的補完の複数表示群の選択対象のフェイス。"
  :group 'skk-dcomp)

(defcustom skk-dcomp-activate nil
  "*Non-nil であれば見出し語の動的補完の機能を有効にする。
この変数の値が `eolp' だった場合、ポイントが行末にある時だけ補完する。"
  :type '(radio (const :tag "always on" t)
                (const :tag "only at the end of a line" eolp)
                (const :tag "off" nil)
                (sexp :tag "任意のルール"))
  :group 'skk-dcomp)

(defcustom skk-dcomp-face-priority 700
  "*Overlay/extent priority of `skk-dcomp-face'."
  :type 'integer
  :group 'skk-dcomp)

(defcustom skk-dcomp-keep-completion-keys nil
  ;;   (delq
  ;;    nil
  ;;    (list
  ;;     (car (rassoc (list nil 'skk-toggle-characters)
  ;;                  skk-rom-kana-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-characters)
  ;;                  skk-rom-kana-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-characters)
  ;;                  skk-rom-kana-base-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-characters)
  ;;                  skk-rom-kana-base-rule-list))))
  "*動的補完された見出し語を消さないキーのリスト。
通常は見出し語の補完後、次のキー入力をすると、動的
補完されたキー入力が消えてしまうが、このリストに指定されたキー
入力があったときは動的補完された見出し語を消さない。"
  :type '(radio (repeat :tag "リスト"
                        (string :tag "キー(文字)"))
                (const :tag "指定しない" nil))
  :group 'skk-dcomp)

(defcustom skk-dcomp-multiple-activate nil
  "*Non-nil であれば、動的補完の候補を複数表示する。
関数であれば、その評価結果が non-nil の時だけ動的補完の候補を複数表示する。"
  :type '(radio (const :tag "always on" t)
                (const :tag "off" nil)
                (sexp :tag "任意のルール"))
  :group 'skk-dcomp)

(defcustom skk-dcomp-multiple-rows 7
  "*動的補完の候補を複数表示する場合の表示数。"
  :type 'integer
  :group 'skk-dcomp)

(defcustom skk-dcomp-multiple-keep-point-buffer-list
  (list (concat " *" (file-name-nondirectory (skk-jisyo)) "*"))
  "*複数表示の為に補完候補を検索する際に `point' を保持するバッファのリスト。

動的補完で候補を複数表示する際に検索対象バッファ
内の `point' を動かしてしまうと通常の補完が正常に機能しなくなる。
そのため、複数表示用の検索が終わった後で `point' を戻すべきバッファ
をこのリストに設定する。

具体的には `skk-comp-from-jisyo' を使用して候補を検索する場合、そ
の対象バッファはこのリストに設定する必要がある。"
  :type '(repeat string)
  :group 'skk-dcomp)

(skk-deflocalvar skk-dcomp-start-point nil)
(skk-deflocalvar skk-dcomp-end-point nil)
(skk-deflocalvar skk-dcomp-extent nil)
(skk-deflocalvar skk-dcomp-multiple-candidates nil)
(skk-deflocalvar skk-dcomp-multiple-key "")
(skk-deflocalvar skk-dcomp-multiple-prefix "")
(skk-deflocalvar skk-dcomp-multiple-search-done nil)
(skk-deflocalvar skk-dcomp-multiple-select-index -1)
(skk-deflocalvar skk-dcomp-multiple-overlays nil)
(defvar skk-dcomp-face 'skk-dcomp-face)

;;; skk-gadget.el related.
(defcustom skk-gengo-alist
  '((reiwa "令和" "R") (heisei "平成" "H") (showa "昭和" "S")
    (taisho "大正" "T") (meiji "明治" "M"))
  "*元号を表記した文字列の alist。
car は元号をローマ字表記した symbol。
cdr は元号表記の string から成るリスト。"
  :type '(repeat (list (symbol :tag "roman")
                       (string :tag "日本語")
                       (string :tag "Initial")))
  :group 'skk-gadget)

(defcustom skk-month-alist
  '(("Jan" "1" "Januar") ("Feb" "2" "Februar") ("Mar" "3" "Mrz")
    ("Apr" "4" "April") ("May" "5" "Mai")
    ("Jun" "6" "Juni") ("Jul" "7" "Juli") ("Aug" "8" "August")
    ("Sep" "9" "September") ("Oct" "10" "Oktober")
    ("Nov" "11" "November") ("Dec" "12" "Dezember"))
  "*月名の英語表記とその他の表記法の連想リスト。
各 cons cell の car は Emacs 標準関数 `current-time-string' が返す形式。
cdr は対応する任意の形式。"
  :type '(repeat (list (string :tag "English")
                       (string :tag "日本式")
                       (string :tag "Deutsch")))
  :group 'skk-gadget)

(defcustom skk-day-of-week-alist
  '(("Sun" "日" "So") ("Mon" "月" "Mo") ("Tue" "火" "Di") ("Wed" "水" "Mi")
    ("Thu" "木" "Do") ("Fri" "金" "Fr") ("Sat" "土" "Sa"))
  "*曜日の英語表記とその他の表記法の連想リスト。
各 cons cell の car は Emacs 標準関数 `current-time-string' が返す形式。
cdr は対応する任意の形式。"
  :type '(repeat (list (string :tag "English")
                       (string :tag "日本語")
                       (string :tag "Deutsch")))
  :group 'skk-gadget)

(defcustom skk-default-current-date-function
  (lambda (date-information format gengo and-time)
    (skk-default-current-date date-information nil skk-number-style
                              gengo 0 0 0 and-time))
  "*`skk-current-date' でコールされるデフォルトの関数。
時間情報を引数に取り加工した文字列を出力する。

引数は DATE-INFORMATION, FORMAT, GENGO, AND-TIME の 4 つ。
DATE-INFORMATION は `current-time-string' が返した文字列を

  (year month day day-of-week hour minute second)

の形式で変換したリスト (各要素は文字列)。
FORMAT は `format' の第一引数の様式による出力形態を指定する文字列。
GENGO は元号表示するかどうか (boolean)。
AND-TIME は時刻も表示するかどうか (boolean)。"
  :type '(radio (function :tag "関数")
                (const :tag "指定しない" nil))
  :group 'skk-gadget)

(defcustom skk-date-ad nil
  "*Non-nil であれば、`skk-today', `skk-clock' で西暦表示する。
nil であれば、元号表示する。"
  :type 'boolean
  :group 'skk-gadget)

(defcustom skk-number-style 1
  "*`skk-today', `skk-clock' で表示する数字の形式を変化させる。
  0 , nil : ASCII 数字
  1 , t   : 全角数字
  2       : 漢数字(位取)
  3       : 漢数字"
  :type '(radio (const :tag "ASCII 数字" 0)
                (const :tag "全角数字" 1)
                (const :tag "漢数字(位取)" 2)
                (const :tag "漢数字" 3))
  :group 'skk-gadget)

(defcustom skk-units-alist
  '(("mile" ("km" . 1.6093) ("yard" . 1760))
    ("yard" ("feet" . 3) ("cm" . 91.44))
    ("feet" ("inch" . 12) ("cm" . 30.48))
    ("inch" ("feet" . 0.5) ("cm" . 2.54)))
  "*単位換算情報の連想リスト。
各要素は (基準となる単位 (変換する単位 . 変換時の倍率)) の形式による。
`skk-gadget-units-conversion' で参照する。"
  :type 'sexp
  :group 'skk-gadget)

(defcustom skk-gadget-load-hook nil
  "*skk-gadget.el をロードした後にコールされるフック。"
  :type 'hook
  :group 'skk-gadget)

;;; skk-isearch.el related.
(defcustom skk-isearch-mode-enable t
  "*Non-nil であれば、インクリメンタル・サーチで skk-isearch を利用する。

通常は SKK モードが ON のバッファでのみ skk-isearch が有効になるが、こ
の値が `always' であれば SKK モードが OFF のバッファでも有効になる。

この値が nil ならば skk-isearch は無効になる。migemo を利用したい場合
などには nil に設定するか、もしくは併用するのであれば
`skk-isearch-start-mode' を `latin' にするのが良い。"
  :type '(radio (const :tag "SKK モードが ON の時だけ利用する" t)
                (const :tag "常に利用する" always)
                (const :tag "利用しない" nil))
  :group 'skk-isearch)

(defcustom skk-isearch-mode-string-alist
  '((hiragana . "[か] ") (katakana . "[カ] ") (jisx0208-latin . "[英] ")
    (latin . "[aa] ") (abbrev . "[aあ] ") (nil . "[--] "))
  ;;  "*Alist of (MODE-SYMBOL . PROMPT-STRING).
  ;;MODE-SYMBOL is a symbol indicates canonical mode of skk for skk-isearch.
  ;;Valid MODE-SYMBOL is one of `hiragana', `katakana', `jisx0208-latin',
  ;;`latin' or nil.
  ;;PROMPT-STRING is a string used in prompt to indicates current mode of
  ;;skk for skk-isearch. "
  "*インクリメンタル・サーチ時のプロンプト表示のための連想リスト。
各要素は、
  (MODE-SYMBOL . PROMPT-STRING)
という cons cell。

MODE-SYMBOL は入力モードを表わすシンボルで、下記のいずれかを指定する。
   かなモード： `hiragana'
   カナモード： `katakana'
   全英モード： `jisx0208-latin'
   アスキーモード： `latin'
   Abbrev モード： `abbrev'
   nil : SKK モードオフ

PROMPT-STRING は、入力モードに応じてプロンプト表示する文字列。"
  :type '(list
          (cons (const :tag "かなモード" hiragana)
                (string :tag "プロンプト"))
          (cons (const :tag "カナモード" katakana)
                (string :tag "プロンプト"))
          (cons (const :tag "全英モード" jisx0208-latin)
                (string :tag "プロンプト"))
          (cons (const :tag "アスキーモード" latin)
                (string :tag "プロンプト"))
          (cons (const :tag "Abbrev モード" abbrev)
                (string :tag "プロンプト"))
          (cons (const :tag "SKKモードオフ" nil)
                (string :tag "プロンプト")))
  :group 'skk-isearch)

(defcustom skk-isearch-start-mode nil
  ;;  "*Specifies the search mode when isearch is called.
  ;;This variable is valid only when `skk-isearch-use-previous-mode' is nil.
  ;;If nil, it means that if skk-mode has been called in this buffer, same as
  ;;the mode of the buffer, otherwise perform ascii search.
  ;;If `latin' or `ascii' perfrom ascii search.
  ;;If `hiragana', `hirakana' or `kana' -> hira kana search.
  ;;If `jisx0208-latin' or `eiji', perform zenkaku eiji (i.e. JIS X0208
  ;;alphabet) search."
  "*カレントバッファでインクリメンタル・サーチを行う際の入力モード。
`skk-isearch-use-previous-mode' が nil の場合のみ有効。
インクリメンタル・サーチを行う場合、常にこの変数で指定した入力モードが使用される
 (ユーザーが明示的に変更することは可)。
下記のいずれかのシンボルで指定する。

   nil:  カレントバッファで SKK モードが起動されていればそのモード、
         起動されていなければ アスキーモード。
   `hiragana' (`hiragana' or `kana'): かなモード
   `jisx0208-latin' (`eiji') : 全英モード
   `latin' (`ascii'): アスキーモード"
  :type '(radio (const :tag "検索中バッファのモードを継承" nil)
                (const :tag "アスキーモード" latin)
                (const :tag "かなモード" hiragana)
                (const :tag "全英モード" jisx0208-latin))
  :group 'skk-isearch)

(defcustom skk-isearch-use-previous-mode nil
  ;; "*Non-nil means use the same search mode as that of the last search."
  "*Non-nil であれば、同じバッファでの最後の検索時のモードを使用する。"
  :type 'boolean
  :group 'skk-isearch)

(defcustom skk-isearch-initial-mode-when-skk-mode-disabled 'latin
  ;;  "*Symbol indicates the mode to use as initial mode for skk-isearch when
  ;;skk is turned off in the current buffer."
  "*SKK モードがオフのバッファで、最初にインクリメンタル・サーチを行う際の入力モード。"
  :type '(radio (const :tag "アスキーモード" latin)
                (const :tag "かなモード" hiragana)
                (const :tag "全英モード" jisx0208-latin))
  :group 'skk-isearch)

(defcustom skk-isearch-whitespace-regexp "\\(\\s \\|[ \t\n\r\f]\\)*"
  ;;  "*Regular expression to match a sequence of whitespace chars.
  ;;This applies to regular expression incremental search."
  "空白文字の連続としてマッチさせるべき正規表現。
regexp isearch の際、この正規表現にマッチする文字が検索文字列の間に含まれていて
もマッチする。"
  :type 'regexp
  :group 'skk-isearch)

(defconst skk-isearch-mode-canonical-alist
  '((hiragana . 0) (katakana . 1) (jisx0208-latin . 2) (latin . 3))
  "Alist of (SYMBOL . NUMBER).
The SYMBOL is canonical skk mode, and NUMBER is its numerical representation.")

(defconst skk-isearch-mode-alias-alist
  '((hirakana . hiragana) (kana . hiragana) (eiji . jisx0208-latin)
    (ascii . latin))
  "Alist of (ALIAS . CANONICAL).
The both ALIAS and CANONICAL should be symbol.
ALIAS can be used as an alias of CANONICAL.
CANONICAL should be found in `skk-isearch-mode-canonical-alist'. ")

(defconst skk-isearch-breakable-character-p-function
  (lambda (char)
    ;; see emacs/lisp/fill.el how the category `|' is
    ;; treated.
    (aref (char-category-set char) ?|))
  "Function to test if we can insert a newline around CHAR when filling.")

(defconst skk-isearch-working-buffer " *skk-isearch*"
  "Work buffer for skk isearch.")

(defvar skk-isearch-message nil
  "skk-isearch 関数をコールするためのフラグ。
Non-nil であれば、`skk-isearch-message' 関数をコールする。")

(defvar skk-isearch-mode nil
  "Current search mode.
0 means hira kana search.
1 means kana search.
2 means zenkaku eiji (i.e. JIS X0208 alphabet) search.
3 means ascii search.")

(defvar skk-isearch-incomplete-message ""
  "Incomplete isearch message")

(defvar skk-isearch-mode-map nil
  "Keymap for skk isearch mode.
This map should be derived from `isearch-mode-map'.")

(defvar skk-isearch-overriding-local-map
  'overriding-terminal-local-map
  "Variable holding overriding local map used in `isearch-mode'.")

(defvar skk-isearch-last-mode-string "")
(defvar skk-isearch-last-mode-regexp "")

;;;###autoload
(defvar skk-isearch-switch nil)
(defvar skk-isearch-state nil)
(defvar skk-isearch-in-editing nil)
(defvar skk-isearch-current-buffer nil)

;;; skk-hint.el related.
(defcustom skk-hint-start-char ?\73 ; ;
  "*ヒント変換を開始するキーキャラクタ"
  :type 'character
  :group 'skk-hint)

(skk-deflocalvar skk-hint-henkan-hint nil
  "ヒント付き変換時のヒント部分。
`skk-henkan-key', `skk-henkan-okurigana', `skk-okuri-char' のリスト。")

(skk-deflocalvar skk-hint-start-point nil)
(skk-deflocalvar skk-hint-end-point nil)
(skk-deflocalvar skk-hint-okuri-char nil)
(skk-deflocalvar skk-hint-state nil)
(skk-deflocalvar skk-hint-inhibit-kakutei nil)
(skk-deflocalvar skk-hint-inhibit-dcomp nil)

;;; skk-jisx0201.el related.
(defcustom skk-use-jisx0201-input-method nil "\
*Non-nil なら 半角カナと Japanese Roman の入力機能が利用可能になる。"
  :type 'boolean
  :group 'skk-jisx0201)

(defcustom skk-jisx0201-mode-string "ｶﾀｶﾅ"
  "*SKK が JISX0201 モードであるときにモードラインに表示される文字列。"
  :type 'string
  :group 'skk-jisx0201)

(defvar skk-jisx0201-base-rule-tree nil)
(defvar skk-jisx0201-roman-rule-tree nil)
(defvar skk-jisx0201-orig-rule-tree nil)
(skk-deflocalvar skk-jisx0201-roman nil)

(skk-deflocalvar skk-jisx0201-mode nil
  "Non-nil であれば、入力モードが JISX0201 モードであることを示す。")

;;; skk-jisx0213.el related.
(defcustom skk-jisx0213-prohibit nil
  "*Non-nil であれば JISX0213 の文字列を含む候補の出力をしない。
JISX0213 を扱えないときはこの値は動作に影響しない。"
  :type 'boolean
  :group 'skk-jisx0213)

;;; skk-jisyo-edit-mode.el related

(defcustom skk-jisyo-edit-user-accepts-editing nil
  "*Non-nil であれば、ユーザが個人辞書の編集を自己責任にて行う旨確認済である。
nil であれば、`skk-edit-private-jisyo' の実行時に確認する。"
  :type 'boolean
  :group 'skk-jisyo-edit-mode)

;;; skk-kakasi.el related.
(defcustom skk-use-kakasi (if (executable-find "kakasi") t nil)
  "*Non-nil であれば KAKASI を使った変換を行う。"
  :type 'boolean
  :group 'skk-kakasi)

(defcustom skk-kakasi-command (executable-find "kakasi")
  "*KAKASI コマンド本体。"
  :type 'file
  :group 'skk-kakasi)

(defcustom skk-romaji-*-by-hepburn t
  "*Non-nil であれば KAKASI を使ったローマ字への変換様式にヘボン式を用いる。
例えば、
  \"し\" -> \"shi\"

nil であれば、訓令式 \"(「日本式」とも言うようだ)\" を用いる。
例えば、
   \"し\" -> \"si\"

昭和 29 年 12 月 9 日付内閣告示第一号によれば、原則的に訓令式 \"(日本式)\" を
用いるかのように記載されているが、今日一般的な記載方法は、むしろ、ヘボン式であ
るように思う。"
  :type 'boolean
  :group 'skk-kakasi)

(defcustom skk-kakasi-load-hook nil
  "*skk-kakasi.el をロードした後にコールされるフック。"
  :type 'hook
  :group 'skk-kakasi)

(defcustom skk-gyakubiki-jisyo-list nil
  "KAKASI を使った変換の際に追加参照する逆引きユーザ辞書のリスト。"
  :type '(repeat file)
  :group 'skk-kakasi)

;;; skk-kanagaki.el related.
(defcustom skk-use-kana-keyboard nil "\
*Non-nil なら仮名入力用の設定をロードする。
SKK 使用中にこの変数の値を切り替えることで  ローマ字入力 ←→ 仮名入力 の
切り替えができる。"
  :type 'boolean
  :set (lambda (symbol value)
         (prog1
             (if (fboundp 'custom-set-default)
                 (custom-set-default symbol value)
               (set-default symbol value))
           (when (and value
                      (boundp 'skk-mode-invoked)
                      skk-mode-invoked)
             (require 'skk-kanagaki)
             (skk-kanagaki-initialize))))
  :group 'skk-kanagaki)

;;; skk-kcode.el related.
(defcustom skk-kcode-method 'code-or-char-list
  "*`skk-input-by-code-or-menu' で使われる文字挿入のためのインターフェース。
`char-list' であれば、文字一覧表 (`skk-list-chars') から選択する。
`code-or-char-list' であれば、まず JIS コード/区点コード入力プロンプトを表示
し、有効な入力が得られなかった場合に `skk-list-chars' を呼び出す。
`code-or-menu' であれば従来のように、まず JIS コード/区点コード入力プロンプト
を表示し、有効な入力が確定しなかった場合には候補文字一覧を表示する。"
  :type '(radio (const :tag "常に文字コード表から選ぶ" char-list)
                (const :tag "コード入力 → 文字コード表" code-or-char-list)
                (const :tag "コード入力 → 文字候補 (旧来のメニュー)"
                       code-or-menu)
                (const :tag "文字コード表／コード入力は利用しない" this-key))
  :group 'skk-kcode)

(defcustom skk-input-by-code-menu-keys1 '(?a ?s ?d ?f ?g ?h ?q ?w ?e ?r ?t ?y)
  "*メニュー形式で JIS 文字を入力するときに使用する選択キーのリスト。
第 1 段階のメニューで使用する。
12 個のキー (char type) を含む必要がある。"
  :type '(repeat character)
  :group 'skk-kcode)

(defcustom skk-input-by-code-menu-keys2
  '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u)
  "*メニュー形式で JIS 文字を入力するときに使用する選択キーのリスト。
第 2 段階のメニューで使用する。
16 個のキー (char type) を含む必要がある。"
  :type '(repeat character)
  :group 'skk-kcode)

(defcustom skk-kcode-charset 'japanese-jisx0213-1
  "*`skk-input-by-code-or-menu' で使われる文字セット。"
  :type (let ((list '((const japanese-jisx0213-1)
                      (const japanese-jisx0208)))
              (prompt (if (get 'charset 'widget-type)
                          '(charset)
                        '(symbol))))
          (append '(radio) list prompt))
  :group 'skk-jisx0213
  :group 'skk-kcode)

(defcustom skk-kcode-load-hook nil
  "*skk-kcode.el をロードした後にコールされるフック。"
  :type 'hook
  :group 'skk-kcode)

(defconst skk-code-n1-min 161)

(defconst skk-code-n1-max 254)

(defconst skk-code-n2-min 161)

(defconst skk-code-n2-max 254)

(defconst skk-code-null 128)

(defconst skk-kcode-charset-list
  (mapcar (lambda (x)
            (list (symbol-name x)))
          charset-list))

(defvar skk-display-code-method 'code
  "*Non-nil であればポイントにある文字のコードを表示する。
nil であれば `this-command-keys' を挿入する。")

(defvar skk-input-by-code-or-menu-jump-default skk-code-n1-min)

(defface skk-display-code-prompt-face
  '((((class color) (type tty))
     (:inherit default :foreground "cyan"))
    (((class color) (background light))
     (:inherit default :foreground "cyan"))
    (((class color) (background dark))
     (:inherit default :foreground "cyan"))
    (((class grayscale))
     (:inherit default)))
  "*skk-display-code `$' でエコーエリアに表示するメッセージ中の KUTEN:、JIS:、EUC:、
SJIS: 及び UNICODE: に適用する face 属性。"
  :group 'skk-kcode
  :group 'skk-visual)

(defface skk-display-code-char-face
  '((((class color) (type tty))
     (:inherit default :foreground "black" :background "yellow"))
    (((class color) (background light))
     (:inherit default :foreground "black" :background "yellow"))
    (((class color) (background dark))
     (:inherit default :foreground "black" :background "yellow"))
    (((class grayscale))
     (:inherit default)))
  "*skk-display-code `$' でエコーエリアに表示するメッセージ中の当該文字に適用する face 属性。"
  :group 'skk-kcode
  :group 'skk-visual)

(defface skk-display-code-tankan-radical-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*skk-display-code `$' でエコーエリアに表示するメッセージ中の総画数に適用する face 属性。"
  :group 'skk-kcode
  :group 'skk-visual)

(defface skk-display-code-tankan-annotation-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*skk-display-code `$' でエコーエリアに表示するメッセージ中の文字名に適用する face 属性。"
  :group 'skk-kcode
  :group 'skk-visual)

;;; skk-list-chars (in skk-kcode.el) related.
(defvar skk-list-chars-buffer-name "*skk-list-chars*"
  "Docstring.")

(defvar skk-list-chars-original-window-configuration nil
  "skk-list-chars-mode に入る前の window configuration。
`skk-list-chars-quit' の実行時、この変数を使って skk-list-chars-mode に
入る前の window 状態に復帰する。")

(defvar skk-list-chars-destination-buffer nil
  "skk-list-chars-insert の挿入先バッファ")

(defvar skk-list-chars-point nil
  "C-x C-x (skk-list-chars-goto-point) のジャンプ先")

(defvar skk-list-chars-default-charstr nil)

(defvar skk-list-chars-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "$" 'skk-list-chars-display-code)
    (define-key map "w" 'skk-list-chars-copy)
    (define-key map "q" 'skk-list-chars-quit)
    (define-key map (kbd "C-x C-x") 'skk-list-chars-goto-point)
    (define-key map "c" 'skk-list-chars-code-input)
    ;;     (define-key map (kbd "C-a") '区の先頭へ)
    ;;     (define-key map (kbd "C-e") '区の末尾へ)
    ;;     (define-key map "<" 'バッファ先頭へ)
    ;;     (define-key map ">" 'バッファ末尾へ)

    (define-key map (kbd "C-f") 'next-completion)
    (define-key map "f"         'next-completion)
    (define-key map "l"         'next-completion)
    (define-key map [right]     'next-completion)

    (define-key map (kbd "C-b") 'previous-completion)
    (define-key map "b"         'previous-completion)
    (define-key map "h"         'previous-completion)
    (define-key map [left]      'previous-completion)

    (define-key map (kbd "C-n") 'skk-list-chars-next-line)
    (define-key map "n"         'skk-list-chars-next-line)
    (define-key map "j"         'skk-list-chars-next-line)
    (define-key map [down]      'skk-list-chars-next-line)

    (define-key map (kbd "C-p") 'skk-list-chars-previous-line)
    (define-key map "p"         'skk-list-chars-previous-line)
    (define-key map "k"         'skk-list-chars-previous-line)
    (define-key map [up]        'skk-list-chars-previous-line)

    (define-key map (kbd "RET") 'skk-list-chars-insert)
    (define-key map "i"         'skk-list-chars-insert)

    (define-key map "g" 'skk-list-chars-jump)

    (define-key map "o"  'skk-list-chars-other-charset)
    (define-key map "\\" 'skk-list-chars-other-charset)
    map)
  "Keymap used in skk-list-chars mode.")

(defface skk-list-chars-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*skk-list-chars バッファにおける、目的文字を指し示す用途に適用する face 属性。"
  :group 'skk-kcode
  :group 'skk-visual)

(defface skk-list-chars-table-header-face
  '((((class color) (type tty))
     (:inherit default :foreground "red"))
    (((class color) (background light))
     (:inherit default :foreground "Firebrick"))
    (((class color) (background dark))
     (:inherit default :foreground "chocolate1"))
    (((class grayscale))
     (:inherit default :foreground "LightGray")))
  "*skk-list-chars バッファにおける、一覧見出しや枠線に適用する face 属性。"
  :group 'skk-visual)

;;; skk-look.el related.
(defcustom skk-use-look nil
  "*UNIX look コマンドを利用した補完・変換を行うかどうかを指定する。
t ならば、補完時と英数字変換時に look を使用する。
`completion' ならば、補完時だけ look を使用する。
`conversion' ならば、英数字変換時だけ look を使用する。
nil ならば、look を使用しない。

SKK abbrev モードで補完すると、個人辞書を検索し尽した後で、UNIX look コマン
ドによる英単語補完を行う。例えば、

  ▽abstr (TAB)
  ---> ▽abstract

SKK abbrev モードで、「英文字 + アスタリスク」にて変換すると、look コマンド
による曖昧検索を行うことができる。例えば、

 ▽abstra* (SPC)
  ---> ▼abstract

この状態で確定すると、`abstra*' を見出し語、`abstract' を候補とするエントリ
が個人辞書に追加される。`skk-search-excluding-word-pattern-function' によ
り、確定してもこのようなエントリを追加しないように設定することができる。"
  :type '(radio (const :tag "補完時と英数字変換時に有効" t)
                (const :tag "補完時だけ有効" completion)
                (const :tag "英数字変換時だけ有効" conversion)
                (const :tag "無効" nil))
  :group 'skk-basic
  :group 'skk-look)

(defcustom skk-look-command (executable-find "look")
  "*UNIX look コマンドの名前。"
  :type `(file :tag "ファイル名" ,(or (executable-find "look") ""))
  :group 'skk-look)

(defcustom skk-look-conversion-arguments
  (concat "-df %s "
          (cond ((file-exists-p "/usr/share/dict/words")
                 "/usr/share/dict/words")
                ((file-exists-p "/usr/share/lib/dict/words")
                 "/usr/share/lib/dict/words")
                ((file-exists-p "/usr/dict/words")
                 "/usr/dict/words")
                (t
                 "")))
  "*look コマンドが英数「変換」時に呼び出される際に渡す引数を指定する変数。
一般に look コマンドは以下の形式で呼び出される。

     look [-df] [-t termchar] string [file]

それぞれの意味については \\[man] look を参照されたい。
この変数には、上記のような全引数のうち string を %s に置換したものを指定する。

注意事項として、look コマンドに渡す引数 -d と -f に関しては、 file が 同じ
引数で sort されている必要がある。例えば look -df で検索するときは sort -df
で、 look -d で検索するときは sort -d で sort されている必要がある。このこと
に関しては \\[man] sort も参照されたい。

もうひとつの注意点として、 look の最後の引数として file を渡さないと (省略する
と) 強制的に引数 -d と -f の機能が有効になる。もし look を思い通り制御したけれ
ば適切な file を指定するべきである。

 (設定例)

 (setq skk-look-conversion-arguments \"-df %s /usr/share/dict/words\")
"
  :type 'string
  :group 'skk-look)

(defcustom skk-look-completion-arguments
  (concat "%s "
          (cond ((file-exists-p "/usr/share/dict/words")
                 "/usr/share/dict/words")
                ((file-exists-p "/usr/share/lib/dict/words")
                 "/usr/share/lib/dict/words")
                ((file-exists-p "/usr/dict/words")
                 "/usr/dict/words")
                (t
                 "")))
  "*look コマンドが英数「補完」時に呼び出される際に渡す引数を指定する変数。
look コマンドに関しては変数 `skk-look-conversion-arguments' のドキュメント、
及び \\[man] look を参照されたい。

 (設定例)

 (setq skk-look-completion-arguments \"-d %s /usr/share/dict/words.case\")
"
  :type 'string
  :group 'skk-look)

(defcustom skk-look-recursive-search nil
  "*Non-nil ならば、 look コマンドが見つけた英単語を変換キーにして再検索を行う。
再検索の結果、候補が見つからなければ、元の英単語自身を候補として出力する。"
  :type 'boolean
  :group 'skk-look)

(defcustom skk-look-expanded-word-only t
  "*Non-nil ならば、 look の出力に対する再検索が成功した候補のみを表示する。
`skk-look-recursive-search' が non-nil であるときのみ有効。"
  :type 'boolean
  :group 'skk-look)

(defcustom skk-look-use-ispell nil
  "*look による検索の際、ispell を併用するかどうかを指定する。
t ならば、補完時と英数字変換時に ispell を併用する。
`completion' ならば、補完時だけ ispell を併用する。
`conversion' ならば、英数字変換時だけ ispell を併用する。
nil ならば、ispell を使用しない。"
  :type '(radio (const :tag "補完時と英数字変換時に有効" t)
                (const :tag "補完時だけ有効" completion)
                (const :tag "英数字変換時だけ有効" conversion)
                (const :tag "無効" nil))
  :group 'skk-look)

(defvar skk-look-completion-words nil)

;;;; skk-lookup.el related.
(defcustom skk-lookup-search-agents nil
  "*検索エージェントの設定のリスト。
リストの各要素は次の形式を取る:

  (CLASS LOCATION [KEY1 VALUE1 [KEY2 VALUE2 [...]]])

CLASS には、エージェントの種類をシンボルで指定する。
LOCATION には、エージェントの所在を文字列で指定する。
KEY 及び VALUE は省略可能で、エージェントに対するオプションを指定する。

例: (setq skk-lookup-search-agents
          \\='((ndtp \"dserver\" :port 2010)
            (ndeb \"/cdrom\" :enable (\"EIWA\"))))"
  :type '(repeat (sexp :tag "Agent"))   ; type はちょっとややこしすぎ・・
  :group 'skk-lookup)

(defcustom skk-lookup-option-alist
  '(;; "[spla -> splat]"
    ("ispell" exact nil nil (not skk-okuri-char) ("-> \\([^ ]+\\)]$" . 1)
     nil nil)
    ;; what's this?
    ("jedict" exact nil nil (not skk-okuri-char) nil nil nil)
    ;; 知恵蔵
    ;; `▼ＩＭＦ［International Monetary Fund／International
    ;;            Metalworkers Federation］'
    ;; `ＩＭＦ（国際通貨基金）【International Monetary Fund】'
    ("CHIEZO" exact exact prefix t
     ("（\\(.+\\)）\\|【\\(.+\\)】$\\|［\\(.+\\)］$\\|^\\([^（【［］】）]+\\)$"
      .
      (cond ((match-beginning 1) 1)
            ((match-beginning 2) 2)
            ((match-beginning 3) 3)
            ((match-beginning 4) 4)))
     "／\\|、\\|, " nil)
    ;; 「辞・典・盤」
    ;; `あか３ 淦", "ethanol'
    ("CHUJITEN" exact exact prefix t ("[０-９]* *\\([^ ]+\\)$" . 1) nil nil)
    ;; `(皮膚などの)あか <grime>", "《英》 (パイプなどの)あか <fur>'
    ("COLLOC" exact exact prefix t ("\\([^ 《》]+\\) <[a-z]+>$" . 1) nil nil)
    ;; ジーニアス英和, ジーニアス英和・和英辞典
    ;; `あか[淦]'
    ;; `いれかえ[入れ替え,入れ換え]'
    ("GENIUS" exact exact prefix t
     ;;("\\[\\(.+\\)\\]$" . 1) ;;can I use `$' for GENIUS?
     ("\\[\\(.+\\)\\]" . 1)
     "," nil)
    ;; Super統合辞書99 Disk1, 2/現代用語の基礎知識
    ;; `◆朱・株・殊・珠〔似たもの漢字〕' ; `・' が区切り文字であるときと
    ;;  そうでないときがあるなぁ...。
    ;; `◆赤ワイン・ブーム〔健康問題〕'
    ("GN99EP01" exact exact prefix t ("^◆\\([^〔〕]+\\)〔.+〕$" . 1) nil nil)
    ("GN99EP02" exact exact prefix t ("^◆\\([^〔〕]+\\)〔.+〕$" . 1) nil nil)
    ;; 岩波国語辞典
    ;; `したい【死体・屍体】'
    ;; `したい【支隊】【枝隊】'
    ;; `あい【愛】'
    ;; `あい(あゐ)【藍】'
    ;; `あい<gaiji=za52a>哀<gaiji=za52b>'
    ;; `だし【出し】【出し・〈出汁〉】【｛山車｝】'
    ;; `ふうきり【封切(り)】'
    ("IWAKOKU" exact exact prefix t
     ;; cannot use `$' for this.
     ("【\\(.+\\)】" . 1)
     "】【\\|・" "[〈〉｛｝()]")
    ;; "垢", "赤"
    ("KANWA" exact exact prefix t nil nil nil)
    ;; KOUJIEN: 広辞苑 第4版(岩波,EPWING) マルチメディア版
    ;; `あい【合い・会い】アヒ' ; これには `】$' を使えない。
    ;; `あい【間】アヒ'
    ;; `ウィ【oui フランス】'
    ;; `ソ【sol イタリア】'
    ;; `アリストテレス‐しゅぎ【―主義】'
    ;; `アートマン【_tman 梵】'; 未対応。外字を含む候補。_ は外字
    ;; "○虎の威を借る狐"
    ("KOUJIEN" exact exact prefix t
     ("^\\([^【】]+\\)‐[ーぁ-ん]+【―\\([^【】]+\\)】$\\|\
\【\\([a-zA-Z]+\\) [ーァ-ン]+】$\\|【\\([^【】]+\\)】\\|\
^○\\(.+\\)$" .
(cond ((match-beginning 2) '(1 2))
      ((match-beginning 3) 3)
      ((match-beginning 4) 4)
      ((match-beginning 5) 5)))
     "・"
     ;;"‐[ーぁ-ん]+【―\\|】$"
     nil)
    ;; KOJIEN: 広辞苑第5版(岩波,EPWING)
    ;; `でんし‐ブック【電子―】'
    ("KOJIEN" exact exact prefix t
     ("^\\([^【】]+\\)‐[ーぁ-ん]+【―\\([^【】]+\\)】$\\|\
\【\\([a-zA-Z]+\\) [ーァ-ン]+】$\\|\
\【\\([^【】]+\\)】\\|\
^[ーぁ-ん]+‐\\([ーァ-ン]+\\)【\\([^【】]+\\)―】$\\|\
^○\\(.+\\)$" .
(cond ((match-beginning 2) '(1 2))
      ((match-beginning 3) 3)
      ((match-beginning 4) 4)
      ((match-beginning 5) '(6 5))
      ((match-beginning 7) 7)))
     "・"
     ;;"‐[ーぁ-ん]+【―\\|】$"
     nil)
    ;; KOKUGO: 三省堂 日本語辞典（現代国語、外来語）
    ;; `〈' は、当用漢字表にない漢字で、`《' は、当用漢字表にはあるが、その音、
    ;; 訓が当用漢字表の音訓表にない漢字。
    ("KOKUGO" exact exact prefix t ("【\\([^【】]+\\)】" . 1) "・" "[《〈]")
    ;; 「辞・典・盤」附属のマイペディア
    ;;`大和郡山(市)'
    ;;`ワシントン(George Washington)'
    ;;`ワシントン(州)'
    ;;`ワシントン Washington'
    ;;`アインシュタイン(Albert Einstein)'
    ;;`香良洲(町)'
    ;;`カラス (烏)'
    ;;`カラス(Maria Callas)'
    ("MYPAEDIA" exact exact prefix t
     ("\\([^ ]+\\)(.+)$\\|.+ (\\([^ ]+\\))$\\|^\\([^ ()]+\\)$" .
      (cond ((match-beginning 1) 1)
            ((match-beginning 2) 2)
            ((match-beginning 3) 3)))
     nil nil)
    ;;  mypaedia-fpw から生成した PC Success 版マイペディア (FreePWING 辞書)
    ;; `大和郡山 [やまとこおりやま] (市)'
    ;; `アインシュタイン (Albert Einstein)'
    ;; `ワシントン (Washington) (州)'
    ;; `ワシントン (Washington)'
    ;; `ワシントン (George Washington)'
    ;; `香良洲 [からす] (町)'
    ;; `カラス (烏) [カラス]'
    ;; `カラス (Maria Callas)'
    ;;("MYPAEDIA" exact exact prefix t
    ;; ("^\\([^ ]+\\) \\[.+\\] (.+)$\\|^[^ ]+ (\\(.+\\)) \\[.+\\]$\\|\
    ;;   ^\\([^][() ]+\\)\\( .+\\)?$" .
    ;;  (cond ((match-beginning 1) 1)
    ;;        ((match-beginning 2) 2)
    ;;        ((match-beginning 3) 3)))
    ;; nil nil)
    ;;
    ;; ニューアンカー英和
    ;; "あか２ 垢"
    ("NEWANC" exact exact prefix t ("[０-９]* *\\([^ ]+\\)$" . 1) nil nil)
    ;; what's this?
    ;; `　あか <scud２>',
    ;; `　「あか」 <rust>'
    ("PLUS" exact exact prefix t ("^　\\(.+\\) <[a-z０-９]+>$" . 1) nil nil)
    ("lsd" exact exact prefix t ("^\\([^〔〕]+\\)〔.+〕$" . 1) nil nil))
  "*辞書毎の検索、文字切り出しオプション。
リストの各要素は下記の通り。

  0th: `lookup-dictionary-name' が返す文字列 (辞書種別を表わす)。
  1th: 送りなし変換の際の search method を示すシンボル。regexp は指定不可。
  2th: 送りあり変換で、かつ `skk-process-okuri-early' オプションを指定して
       いないとき (送り仮名決定の後に検索を開始するので、送り仮名が特定できる)
       の search method を示すシンボル。regexp は指定不可。nil を指定すると、
       送りあり変換の際はその辞書を検索しない。
  3th: 送りあり変換で、かつ `skk-process-okuri-early' であるとき (送り仮名
       決定の前に検索を開始しており、送り仮名が特定できないので、送り仮名のかな
       prefix を除いた部分を検索キーとして lookup に渡している) の search
       method を示す シンボル。regexp は指定不可。nil を指定すると送りあり変換
       の際はその辞書を検索しない。
  4th: S 式。この S 式を評価して nil になるときは検索しない。ある一定の条件を満
       した場合に検索しないように指定できる。
  5th: `lookup-entry-heading' が返す heading から候補として出力する文字列を切り
       出すための regexp 指定及び切り出しオプション。
       car に regexp を示す文字列、cdr に `match-string' に渡す count を指定
       する (5th に文字列だけを指定した場合は `match-string' には 1 が
       渡される)。
       cdr 部に S 式を指定することも可能。下記のように cond 式で条件判定すれば
       複数の regexp を or 指定することが可能。

          (cond ((match-beginning 1) 1)
                ((match-beginning 2) 2)
            ((match-beginning 3) 3)
                ((match-beginning 4) 4))

       cdr 部の評価結果が数字のリストになるときは、その数字を順に `match-string'
       に渡して文字列を切り出し、それら連結した文字列を候補として返す。例えば、

          (cond ((match-beginning 5) \\='(6 5)))

       と指定すると、(match-beginning 5) が non-nil になった場合、
       (match-string 6) と (match-string 5) をその順に連結した文字列を候補とし
       て出力する。
       切り出さずに文字列全体を対象にするときは、5th に nil を指定する。
  6th: 切り出された文字列の中に更に複数の候補を含む場合の区切りを表わす
       regexp。
       複数の候補が同一 heading の中に出力されないときは、nil を指定する。
  7th: 切り出された文字列から特定の文字列を取り除く場合に指定する regexp。
       辞書の出力が辞書特有の記号文字を含む場合に指定する。

現在対応している辞書名は \"ispell\", \"jedict\", \"CHIEZO\", \"CHUJITEN\",
\"COLLOC\", \"GENIUS\", \"GN99EP01\", \"GN99EP02\", \"IWAKOKU\", \"KANWA\",
\"KOUJIEN\", \"KOJIEN\", \"KOKUGO\", \"MYPAEDIA\", \"NEWANC\", \"PLUS\" 及び
\"lsd\"。
`lookup-entry-heading' が自分の使用する辞書からどのような文字列を取り出すのか
確かめたいときは、`skk-lookup-pickup-headings' を使用する。例えば、

 (skk-lookup-pickup-headings \"こしょう\" \\='exact)"
  ;; for checking.
  ;; (pp (mapcar (lambda (e)(cons (car e) (length e)))
  ;;    skk-lookup-option-alist))
  :type '(repeat
          (list (string :tag "Dictionary name")
                (choice :tag "Search method for okuri nasi"
                        (const exact) (const prefix)
                        (const suffix) (const substring)
                        (const keyword) (const text)
                        (const nil))
                (choice
                 :tag "Search method for okuri ari (not process okuri early)"
                 (const exact) (const prefix)
                 (const suffix) (const substring)
                 (const keyword) (const text)
                 (const nil))
                (choice
                 :tag "Search method for okuri ari (process okuri early)"
                 (const exact) (const prefix)
                 (const suffix) (const substring)
                 (const keyword) (const text)
                 (const nil))
                (sexp :tag "S expression to search")
                (choice :tag "Regexp to substring candidate from heading"
                        (cons regexp sexp) (const nil))
                (choice :tag "Regexp to split candidates"
                        regexp (const nil))
                (choice :tag "Regexp to remove a string from candidates"
                        regexp (const nil))))
  :group 'skk-lookup)

(defcustom skk-lookup-default-option-list
  '(exact exact prefix t ("【\\([^【】]+\\)】" . 1) "・" nil)
  ;; CRCEN: 三省堂 ニューセンチュリー英和・新クラウン和英辞典
  ;; KANJIGEN: Super統合辞書99 Disk2/漢字源 : EPWING
  ;; RIKAGAKU: 理化学辞典
  ;; WAEI: what's this?
  "*デフォルトの辞書検索、文字切り出しオプション。
まず辞書名をキーにして `skk-lookup-option-alist' を引き、そこに辞書検索、文字切
り出しのオプションが見つかればそれを使用し、見つからなかった場合にこの変数で
指定される辞書検索、文字切り出しのオプションを使用する。

リストの各要素は下記の通り。

  0th: 送りなし変換の際の search method を示すシンボル。regexp は指定不可。
  1th: 送りあり変換で、かつ `skk-process-okuri-early' オプションを指定していな
       いとき (送り仮名決定の後に検索を開始するので、送り仮名が特定できる) の
       search method を示すシンボル。regexp は指定不可。nil を指定すると、送り
       あり変換の際はその辞書を検索しない。
  2th: 送りあり変換で、かつ `skk-process-okuri-early' である (送り仮名決定の前
       に検索を開始しており、送り仮名が特定できないので、送り仮名のかな prefix
       を除いた部分を検索キーとして lookup に渡している) ときの search method
       を示すシンボル。regexp は指定不可。nil を指定すると送りあり変換の際はそ
       の辞書を検索しない。
  3th: S 式。この S 式を評価して nil になるときは検索しない。ある一定の条件を満
       した場合に検索しないように指定できる。
  4th: `lookup-entry-heading' が返す heading から候補として出力する文字列を切り
       出すための regexp 指定及び切り出しオプション。
       car に regexp を示す文字列、cdr に `match-string' に渡す count を指定す
       る (4th に文字列だけを指定した場合は `match-string' には 1 が渡される)。
       cdr 部に S 式を指定することも可能。下記のように cond 式で条件判定すれば
       複数の regexp を or 指定することが可能。

          (cond ((match-beginning 1) 1)
                ((match-beginning 2) 2)
            ((match-beginning 3) 3)
                ((match-beginning 4) 4))

       cdr 部の評価結果が数字のリストになるときは、その数字を順に `match-string'
       に渡して文字列を切り出し、それら連結した文字列を候補として返す。例えば、

          (cond ((match-beginning 5) \\='(6 5)))

       と指定すると、(match-beginning 5) が non-nil になった場合、
       (match-string 6) と (match-string 5) をその順に連結した文字列を候補とし
       て出力する。
       切り出さずに文字列全体を対象にするときは、4th に nil を指定する。
  5th: 切り出された文字列の中に更に複数の候補を含む場合の区切りを表わす
        regexp。
       複数の候補が同一 heading の中に出力されないときは、nil を指定する。
  6th: 切り出された文字列から特定の文字列を取り除く場合に指定する regexp。
       辞書の出力が辞書特有の記号文字を含む場合に指定する。

このオプションで対応している辞書名は、\"CRCEN\", \"KANJIGEN\", \"RIKAGAKU\"
及び \"WAEI\".
`lookup-entry-heading' で取り出した文字列が下記のようになることを前提に
している。

  \"あ‐か【亜科】‥クワ\"
  \"あか【閼伽】\"
  \"こ‐しょう【小姓・小性】‥シヤウ\"

`lookup-entry-heading' が自分の使用する辞書からどのような文字列を取り出すのか
確かめたいときは、`skk-lookup-pickup-headings' を使用する。例えば、

 (skk-lookup-pickup-headings \"こしょう\" \\='exact)"
  :type '(list (choice :tag "Search method for okuri nasi"
                       (const exact) (const prefix)
                       (const suffix) (const substring)
                       (const keyword) (const text)
                       (const nil))
               (choice
                :tag "Search method for okuri ari (not process okuri early)"
                (const exact) (const prefix)
                (const suffix) (const substring)
                (const keyword) (const text)
                (const nil))
               (choice :tag "Search method for okuri ari (process okuri early)"
                       (const exact) (const prefix)
                       (const suffix) (const substring)
                       (const keyword) (const text)
                       (const nil))
               (sexp :tag "S expression to search")
               (choice :tag "Regexp to substring candidate from heading"
                       (cons regexp sexp) (const nil))
               (choice :tag "Regexp to split candidates"
                       regexp (const nil))
               (choice :tag "Regexp to remove a string from candidates"
                       regexp (const nil)))
  :group 'skk-lookup)

(defcustom skk-lookup-search-modules nil
  "*検索モジュールの設定のリスト。"
  :type '(repeat (cons :tag "Module" (string :tag "Name")
                       (repeat :tag "Dictionary" (string :tag "ID"))))
  :group 'skk-lookup)

(defcustom skk-lookup-process-henkan-key-function nil
  "*Lookup に渡す際に検索キーを加工するファンクション。
送りあり変換の際のみコールされる。引数は加工すべき文字列 HENKAN-KEY。
返り値は car に加工した文字列、cdr に送り仮名の加工方法を示すマジックナンバー
を入れた cons cell。
マジックナンバーは、0 が送りなしを表わす (本 function では使用することはない)。
1 は送りあり変換で `skk-process-okuri-early' が nil の場合。
2 は送りあり変換で `skk-process-okuri-early' が non-nil の場合を表わす。
近い将来、skk-lookup.el 全体を通じてこのようなマジックナンバーを使わないように
改良される可能性がある。"
  :type '(radio (function :tag "関数")
                (const :tag "指定しない" nil))
  :group 'skk-lookup)

(defcustom skk-lookup-kana-vector
  ["ぁ" "あ" "ぃ" "い" "ぅ" "う" "ぇ" "え" "ぉ" "お"
   "か" "が" "き" "ぎ" "く" "ぐ" "け" "げ" "こ" "ご"
   "さ" "ざ" "し" "じ" "す" "ず" "せ" "ぜ" "そ" "ぞ"
   "た" "だ" "ち" "ぢ" "っ" "つ" "づ" "て" "で" "と" "ど"
   "な" "に" "ぬ" "ね" "の"
   "は" "ば" "ぱ" "ひ" "び" "ぴ" "ふ" "ぶ" "ぷ" "へ" "べ" "ぺ" "ほ" "ぼ" "ぽ"
   "ま" "み" "む" "め" "も"
   "ゃ" "や" "ゅ" "ゆ" "ょ" "よ"
   "ら" "り" "る" "れ" "ろ"
   "ゎ" "わ" "ゐ" "ゑ" "を" "ん"]
  "*`skk-kana-rom-vector' の prefix に対応するかな文字のベクトル。
ある prefix がどのかな文字に対応するかのマップを作るために参照する。"
  :type 'sexp
  :group 'skk-lookup)

(defvar skk-lookup-agent-list nil)
(defvar skk-lookup-default-module nil)
(defvar skk-lookup-module-list nil)
(defvar skk-lookup-prefix-and-kana-map nil)

(defvar skk-lookup-get-content-nth-dic 0
  "*関数 `skk-lookup-get-content' の処理対象を数値で指定する.
数値は「関数 `skk-lookup-default-module' の評価結果のうち何番目の agent を
使用するか」を、ゼロを起点に数える.

*scratch* バッファで次の S 式を評価してみるとよい.
\(let ((n 0))
  (dolist (i (lookup-module-dictionaries (skk-lookup-default-module)))
    (insert (format \"%d %s\" n (lookup-dictionary-name i)) 10) ;10は改行
    (setq n (1+ n))))

なお、DDSKK の起動後に変数の値を変更した場合は、*scratch* バッファで
関数 `skk-lookup-get-content-setup-dic' を評価すること.")

(defvar skk-lookup-get-content-default-dic nil)
(defvar skk-lookup-get-content-default-dic-name nil)

;;; skk-num.el related.
(defcustom skk-use-numeric-conversion t
  "*Non-nil であれば、数値変換を行う。"
  :type 'boolean
  :group 'skk-num)

(defcustom skk-non-numeric-prog-list nil
  "*数値変換に使わない辞書検索プログラムのリスト。
`skk-use-numeric-conversion' が non-nil の場合のみ有効。リストの要素としては、

1. プログラムの関数名を表すシンボル
2. プログラムを引数の値まで指定した形のリスト

のいずれでも指定できる。

前者では、関数名の一致した全プログラムが一致と判断される。後者は
`skk-search-prog-list' の要素と同じ書式で表され、同リストの要素と関数名及び
すべての引数が一致した場合のみ一致と判断される。

一致の評価は、 1 は関数 `eq' によって、 2 はリストに対して `equal' によって
行われる。

 (設定例)

 (setq skk-non-numeric-prog-list
       \\='(skk-look
     skk-tankan-search
     (skk-search-jisyo-file \"/usr/share/skk/SKK-JISYO.jinmei\" 10000)))
"
  :type '(repeat (radio (symbol :tag "関数名のみで指定")
                        (list :tag "関数名と引数のリスト")))
  :group 'skk-num)

(defcustom skk-show-num-type-info t
  "*Non-nil ならば、数値変換エントリの辞書登録時に変換タイプの案内を表示する。"
  :type 'boolean
  :group 'skk-num)

(defcustom skk-num-type-alist
  '((0 . identity)
    (1 . skk-num-jisx0208-latin)
    (2 . skk-num-type2-kanji)
    (3 . skk-num-type3-kanji)
    (4 . skk-num-recompute)
    (5 . skk-num-type5-kanji)
    (8 . skk-num-grouping)
    (9 . skk-num-shogi))
  "*数値の変換のための、インデクスと変換に使用する関数との連想リスト。
関数 `skk-num-exp' が参照している。
各要素は、`(インデクス . 関数名)' という構成になっている。
インデクスには、例えば見出し語が \"平成#1年\" のとき、`#' 記号の直後に表示
される integer `1' を代入する。

インデクスと関数の関係 (デフォルト値) は下記の通り。
    0 -> 無変換
    1 -> 全角数字へ変換
    2 -> 漢数字 (位取りあり) へ変換
    3 -> 漢数字 (位取りなし) へ変換
    4 -> その数字そのものをキーにして辞書を再検索
    5 -> 漢数字 (手形などで使用する文字を使用) へ変換
    8 -> 桁区切りへ変換 (1,234,567)
    9 -> 将棋で使用する数字 (\"３四\" など) に変換"
  :type '(repeat (cons (radio :tag "インデクス"
                              (const 0)
                              (const 1)
                              (const 2)
                              (const 3)
                              (const 4)
                              (const 5)
                              (const 8)
                              (const 9))
                       (function :tag "関数")))
  :group 'skk-num)

(defcustom skk-num-convert-float nil
  "*Non-nil であれば、浮動小数点数を使った見出し語に対応して変換を行う。
この値を non-nil にすることで、\"#.# /#1．#1/#0月#0日/\" などの辞書見出しが使用
できなくなるので、注意。"
  :type 'boolean
  :group 'skk-num)

(defcustom skk-num-uniq (or (assq 4 skk-num-type-alist)
                            (and (assq 2 skk-num-type-alist)
                                 (or (assq 3 skk-num-type-alist)
                                     (assq 5 skk-num-type-alist)))) "\
*Non-nil であれば、異なる数値表現でも変換結果が同じ数値を重複して出力しない。"
                                     :type 'boolean
                                     :group 'skk-num)

(defcustom skk-num-load-hook nil
  "*skk-num.el をロードした後にコールされるフック。"
  :type 'hook
  :group 'skk-num)

(defconst skk-num-alist-type1
  '((?0 . "０") (?1 . "１") (?2 . "２") (?3 . "３")
    (?4 . "４") (?5 . "５") (?6 . "６") (?7 . "７")
    (?8 . "８") (?9 . "９")
    (?. . "．") ; 小数点。(?. . ".") の方が良い人もいるかも...。
    (?  . ""))
  "ascii 数字の char type と全角数字の string type の連想リスト。
\"1995\" -> \"１９９５\" のような文字列へ変換する際に利用する。")

(defconst skk-num-alist-type2
  '((?0 . "〇") (?1 . "一") (?2 . "二") (?3 . "三")
    (?4 . "四") (?5 . "五") (?6 . "六") (?7 . "七")
    (?8 . "八") (?9 . "九") (?\  . ""))
  "ascii 数字の char type と漢数字の string type の連想リスト。
\"1995\" -> \"一九九五\" のような文字列へ変換する際に利用する。")

(defconst skk-num-alist-type3
  (append
   '((ju . "十") (hyaku . "百") (sen . "千")
     (man . "万") (oku . "億") (cho . "兆") (kei . "京"))
   skk-num-alist-type2)
  "数字の漢字表記を表す連想リスト。
\"1995\" -> \"千九百九十五\" のような文字列へ変換する際に利用する。")

(defconst skk-num-alist-type5
  '((ju . "拾") (hyaku . "百") (sen . "阡")
    (man . "萬") (oku . "億") (cho . "兆") (kei . "京")
    (?0 . "零") (?1 . "壱") (?2 . "弐") (?3 . "参")
    (?4 . "四") (?5 . "伍") (?6 . "六") (?7 . "七")
    (?8 . "八") (?9 . "九") (?\  . ""))
  "数字の漢字表記を表す連想リスト。
\"1995\" -> \"壱阡九百九拾伍\" のような文字列へ変換する際に利用する。")

(skk-deflocalvar skk-num-list nil
  "`skk-henkan-key' の中に含まれる数字を表す文字列のリスト。
例えば、\"▽へいせい7ねん10がつ\" を変換するとき、`skk-henkan-key' は
\"へいせい7ねん10がつ\" であり、`skk-num-list' は (\"7\" \"10\") となる。
\(buffer local)")

(defvar skk-num-recompute-key nil
  "#4 タイプのキーにより数値の再計算を行ったときの検索キー。")

(defcustom skk-num-grouping-separator ","
  "#8 タイプ (桁区切り) で使用する記号"
  :type 'string
  :group 'skk-num)

(defcustom skk-num-grouping-places 3
  "#8 タイプ (桁区切り) を何桁で区切るか"
  :type 'integer
  :group 'skk-num)

;;; skk-server.el related.
(defcustom skk-server-host (or (getenv "SKKSERVER") "localhost")
  "*辞書サーバが起動しているホスト名又は IP アドレス。"
  :type `(radio (string :tag "ホスト名"
                        ,(or (getenv "SKKSERVER") "localhost"))
                (const nil))
  :group 'skk-server)

(defcustom skk-server-prog (getenv "SKKSERV")
  "*辞書サーバプログラム名。
フルパスで書く。
`skk-server-inhibit-startup-server' が nil の時に参照され、
このプログラムが SKK より起動される。"
  :type '(radio (file :tag "辞書サーバ名")
                (const :tag "指定しない" nil))
  :group 'skk-file
  :group 'skk-server)

(defcustom skk-server-jisyo (getenv "SKK_JISYO")
  "*辞書サーバプログラムに渡す辞書ファイル名。
フルパスで書く。
`skk-server-inhibit-startup-server' が nil の時に参照される。
辞書ファイルの指定法は辞書サーバにより異なるので注意。
  % skkserv jisyo
の形式の時のみ利用できるオプションである。"
  :type `(radio (file :tag "辞書ファイル名" ,(or skk-aux-large-jisyo ""))
                (const :tag "指定しない" nil))
  :group 'skk-file
  :group 'skk-server)

(defcustom skk-server-portnum (if (eq system-type 'windows-nt)
                                  1178
                                nil)
  "*Non-nil であれば、その値を port number として skkserv と TCP 接続する。
/etc/services を直接書き換える権限がないユーザーのための変数。
Microsoft Windows ではデフォルト値として 1178 が設定される。"
  :type '(radio (integer :tag "ポート番号" 1178)
                (const :tag "指定しない" nil))
  :group 'skk-server)

(defcustom skk-servers-list nil
  "*辞書サーバ毎の情報リスト。

複数のホストで動いている辞書サーバにアクセスできる場合には、以下のようにリストの
各要素に順にホスト名、フルパスでの辞書サーバ名、辞書サーバに渡す辞書ファイル名、
辞書サーバが使用するポート番号を書き、設定をすることができる。

   (setq skk-servers-list
         \\='((\"host1\" \"/path/to/skkserv\" \"/path/to/SKK-JISYO.L\" 1178)
           (\"host2\" \"/path/to/skkserv\")))

この場合、最初に指定した辞書サーバにアクセスできなくなると、自動的に順次リストにあ
る残りの辞書サーバにアクセスするようになる。
辞書サーバのデフォルトの辞書及びポート番号を使用する場合は nil を指定するか、
何も書かないで良い。

なお、ユーザー自身に実行権限のない辞書サーバを指定する場合は、

   (setq skk-servers-list \\='((\"host1\") (\"host2\")))

のように、ホスト名だけを書くことができる。上記の設定例では、host1, host2 にお
ける skkserv サービスの TCP 接続の開始のみ試み、辞書サーバの起動は試みない。"
  :type '(repeat
          (list (string :tag "ホスト名")
                (radio :tag "辞書サーバ名"
                       file
                       (const :tag "指定しない" nil))
                (radio :tag "辞書ファイル"
                       file
                       (const :tag "指定しない" nil))
                (radio :tag "ポート番号"
                       integer
                       (const :tag "指定しない" nil))))
  :group 'skk-server)

(defcustom skk-server-report-response nil
  "*Non-nil であれば、辞書サーバの応答状況を報告する。
具体的には、変換時に辞書サーバの送出する文字を受け取るまでに
`accept-process-output' を何回実行したかをエコーエリアに報告する。"
  :type 'boolean
  :group 'skk-server)

(defcustom skk-server-remote-shell-program
  (or (getenv "REMOTESHELL")
      (and (boundp 'remote-shell-program)
           (symbol-value 'remote-shell-program))
      (cond
       ((eq system-type 'berkeley-unix)
        (if (file-exists-p "/usr/ucb/rsh")
            "/usr/ucb/rsh"
          "/usr/bin/rsh"))
       ((eq system-type 'usg-unix-v)
        (if (file-exists-p "/usr/ucb/remsh")
            "/usr/ucb/remsh"
          "/bin/rsh"))
       ((eq system-type 'hpux)
        "/usr/bin/remsh")
       ((eq system-type 'EWS-UX/V)
        "/usr/ucb/remsh")
       ((eq system-type 'pcux)
        "/usr/bin/rcmd")
       (t
        "rsh")))
  "*リモートシェルのプログラム名。"
  :type 'file
  :group 'skk-server)

(defcustom skk-server-inhibit-startup-server t
  "*Non-nil であれば `call-process' での辞書サーバ起動を禁止する。"
  :type 'boolean
  :group 'skk-server)

(defcustom skk-server-load-hook nil
  "*skk-server.el をロードした後にコールされるフック。"
  :type 'hook
  :group 'skk-server)

;;(defvar skk-server-debug nil
;;  "*Non-nil であれば、辞書サーバプログラムをディバッグモードで起動する。
;;ディバッグ・モードで skkserv を走らせると、そのまま foreground で走り、
;;メッセージを出力する。キーボードから割りこみをかけることもできる。")

(defconst skkserv-working-buffer " *skkserv*")
(defvar skkserv-process nil)

;;; skk-sticky related.
(defcustom skk-sticky-key nil
  "*変換開始位置もしくは送り開始位置の指定をするキー。

キーの設定方法は割当てるキーの種類によって異なります。

1. 表示可能なキー

  \";\" などの表示が可能なキーの場合は

    (setq skk-sticky-key \";\")

  のように string を設定して下さい。`skk-sticky-key' に設定した文
  字そのものを入力したい場合は2回続けて打つと入力できます。

2. 表示されないキー

  \"無変換\" のような表示を伴わないキーの場合は

    (setq skk-sticky-key [muhenkan])    ; Windows 環境だと [noconvert]

  のようにそのキーを表わす vector を設定して下さい。

3. 同時打鍵

  2つのキーを同時に打鍵することでも変換位置を指定できます。例えば
  \"f\" と \"j\" の同時打鍵で指定する場合は

    (setq skk-sticky-key \\='(?f ?j))

  のように character のリストを設定して下さい。"
  :type '(radio (string :tag "表示可能なキー")
                (vector :tag "表示されないキー" symbol)
                (list :tag "同時打鍵" character character)
                (const :tag "指定しない" nil))
  :group 'skk-sticky)

(defcustom skk-sticky-double-interval 0.1
  "*この時間以内に打鍵されたものを同時打鍵と判定する。
単位は秒。デフォルトは 0.1 秒。"
  :type 'number
  :group 'skk-sticky)

;;; skk-study.el related.
(defcustom skk-study-file (if skk-user-directory
                              (expand-file-name "study" skk-user-directory)
                            (convert-standard-filename "~/.skk-study"))
  "*学習結果を保存するファイル。"
  :type 'file
  :group 'skk-study)

(defcustom skk-study-backup-file (if skk-user-directory
                                     (expand-file-name "study.bak"
                                                       skk-user-directory)
                                   (convert-standard-filename
                                    "~/.skk-study.BAK"))
  "*学習結果を保存するファイルのバックアップ。"
  :type 'file
  :group 'skk-study)

(defcustom skk-study-associates-number 5
  "*保存する関連語の数。"
  :type 'integer
  :group 'skk-study)

(defcustom skk-study-sort-saving nil
  "*Non-nil であれば学習結果をソートしてセーブする。"
  :type 'boolean
  :group 'skk-study)

(defcustom skk-study-check-alist-format nil
  "*Non-nil であれば、学習結果の読み込み時に連想リストのフォーマットをチェックする。"
  :type 'boolean
  :group 'skk-study)

(defcustom skk-study-search-times 5
  "*現在の変換キーに対する関連変換キーをいくつまで遡って検索するか。"
  :type 'integer
  :group 'skk-study)

(defcustom skk-study-first-candidate t
  "*Non-nil であれば、第一候補で確定した際も学習する。"
  :type 'boolean
  :group 'skk-study)

(defcustom skk-study-max-distance 30
  "*直前に確定したポイントと今回の変換ポイントがこの距離以上離れていると学習しない。
nil の場合は直前に確定したポイントとの距離を考慮せずに学習する。"
  :type '(radio integer (const nil))
  :group 'skk-study)

;;; system internal variables and constants.
;; global variable
(defconst skk-study-file-format-version "0.2")
(defvar skk-kakutei-end-function nil)
(defvar skk-study-alist nil)
(defvar skk-study-data-ring nil
  "直前の `skk-study-search-times' 個分の変換キーと確定語データ。
ring.el を利用しており、具体的には、下記のような構造になっている。

\(2 3 . [\(\"こうぞう\" . \"構造\"\)\
 \(\"ぐたいてき\" . \"具体的\"\) \(\"かき\" . \"下記\"\)]\)")

(defvar skk-study-last-save nil)
(defvar skk-study-last-read nil)

;;; skk-tankan.el related.
(defcustom skk-tankan-search-key ?@
  "*単漢字変換を行うキーキャラクタ。"
  :type 'character
  :group 'skk-jisx0213
  :group 'skk-tankan)

;;; 文字集合の文字に対して (部首 部首内画数 総画数) を返す関数の alist
;; 補助漢字等にも一応対応可能なように変数にしてある
(defvar skk-tankan-get-char-data-functions
  '((japanese-jisx0208 . skk-tankan-get-char-data-0213-1)
    (japanese-jisx0213-1 . skk-tankan-get-char-data-0213-1)
    (japanese-jisx0213-2 . skk-tankan-get-char-data-0213-2)))

(put 'annotation 'char-table-extra-slots 0)
(defvar skk-tankan-annotation-table
  (make-char-table 'annotation))

(defvar skk-tankan-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'skk-tankan-mode-prev)
    (define-key map "k" 'skk-tankan-mode-prev)
    (define-key map "n" 'skk-tankan-mode-next)
    (define-key map "j" 'skk-tankan-mode-next)
    (define-key map "w" 'skk-tankan-mode-copy)
    (define-key map "q" 'skk-tankan-mode-quit)
    (define-key map "?" 'skk-tankan-mode-usage)
    (define-key map "$" 'skk-tankan-mode-display-code)
    map)
  "Keymap used in skk-tankan mode.")

(defvar skk-tankan-mode-original-window-configuration nil
  "")

(defface skk-tankan-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*skk-tankan-mode の face 属性。"
  :group 'skk-tankan
  :group 'skk-visual)

(defface skk-tankan-radical-name-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*skk-tankan-bushu-compread() で使用する「部首の読み」の face 属性。"
  :group 'skk-tankan
  :group 'skk-visual)

(defvar skk-tankan-overlay nil)

;;; skk-tooltip related.
(defcustom skk-show-tooltip nil
  "*Non-nil であれば、エコーエリアの代わりに tooltip で候補などを表示する。"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-tooltip)

(defcustom skk-tooltip-hide-delay 1000
  "*tooltip を使って候補など表示する場合に、表示する時間 (秒)。
この時間が経過すると自動的に tooltip は消える。"
  :type 'integer
  :group 'skk-tooltip)

(defcustom skk-tooltip-face nil
  "*ツールティップに表示する文字列に適用するフェイスを指定する変数。
候補文字列のフェイス属性（`skk-treat-candidate-appearance-function' による
加工など）をそのまま使いたい場合は nil に設定する。

 (設定例)

 (setq skk-tooltip-face \\='font-lock-doc-face)"
  :type '(radio (face :tag "フェイスを指定" tooltip)
                (const :tag "候補文字列のフェイス属性をそのまま使用" nil))
  :group 'skk-henkan
  :group 'skk-tooltip)

(defcustom skk-tooltip-parameters nil
  "*tooltip を使う場合の SKK 独自の tooltip フレームパラメータ設定。

 (設定例)

 (setq skk-tooltip-parameters
       \\='((foreground-color . \"navy blue\")
     (background-color . \"alice blue\")
     (border-color . \"royal blue\")
     (border-width . 1)))
"
  :type '(radio (const :tag "設定例を試す"
                       ((foreground-color . "navy blue")
                        (background-color . "alice blue")
                        (border-color . "royal blue")
                        (border-width . 1)))
                (repeat :tag "任意の設定"
                        (cons (symbol :tag "パラメータ名")
                              (sexp :tag "値 (S式)"))))
  :group 'skk-tooltip)

(defcustom skk-tooltip-mouse-behavior
  'banish
  "*Tooltip を表示する場合の、マウスポインタの挙動。
`follow' ならば、 tip の位置に移動する。
`avoid' ならば、ウィンドウの端に退避する。
`avoid-maybe' ならば、ウィンドウ上にあるマウスポインタのみ退避する。
`banish' ならば、ウィンドウの端に退避したまま帰ってこない。
`nil' ならば、退避しない。この場合、tip のテキストとマウスポインタが
重なったり、うまく tip が表示できなかったりするので注意。"
  :type '(radio (const :tag "Tip に従う" follow)
                (const :tag "ウィンドウの端に逃げる" avoid)
                (const :tag "逃げたほうがよさそうなときだけ逃げる" avoid-maybe)
                (const :tag "逃げたまま帰らない" banish)
                (const :tag "居座る" nil))
  :group 'skk-tooltip)

(defcustom skk-tooltip-x-offset
  (/ (1+ (frame-char-height)) 2)
  "*Tooltip の表示位置を右にずらすピクセル数。
負の整数を指定すると左にずれる。"
  :type 'integer
  :group 'skk-tooltip)

(defcustom skk-tooltip-y-offset 0
  "*Tooltip の表示位置を下にずらすピクセル数。
負の整数を指定すると上にずれる。"
  :type 'integer
  :group 'skk-tooltip)

(defcustom skk-tooltip-function
  (lambda (tooltip-str)
    (skk-tooltip-show-at-point tooltip-str 'listing))
  "*Tip 描画機構の関数を指定する。
デフォルトでは Emacs 標準の Tooltip を使用する。
他の Tip 描画機構 である pos-tip や popup-tip も指定できる。"
  :type 'function
  :group 'skk-tooltip)

;;; skk-tut.el related.
(defcustom skk-tut-file
  (or (locate-file "skk/SKK.tut"
                   (list (expand-file-name "../../.."
                                           data-directory)))
      (locate-file "skk/SKK.tut" (list data-directory))
      "/usr/local/share/skk/SKK.tut")
  "*SKK 日本語チュートリアルのファイル名 (パスを含む)。"
  :type 'file
  :group 'skk-tut)

(defvar skk-tut-current-lang nil)

(defcustom skk-tut-lang "Japanese"
  "*SKK チュートリアルで用いる言語。
\\[universal-argument] \\[skk-tutorial] による言語指定は、この変数よりも優先
する。"
  :type '(radio (string "Japanese")
                (string "English"))
  :set (lambda (symbol value)
         (prog1
             (if (fboundp 'custom-set-default)
                 (custom-set-default symbol value)
               (set-default symbol value))
           (setq skk-tut-current-lang nil)))
  :group 'skk-tut)

(defvar skk-tut-file-suffix-alist
  `(("Japanese" . "")
    ("English" . ".E"))
  "Alist of (LANGUAGE . suffix) pairs.
For example, if filename of the Japanese version is \"SKK.tut\",
then filename of the English version will be \"SKK.tut.E\".")

(defcustom skk-tut-use-face skk-use-face
  "*Non-nil であれば、チュートリアルで face を利用して表示する。"
  :type 'boolean
  :group 'skk-tut)

(defface skk-tut-section-face
  '((((class color) (background light))
     (:foreground "yellow" :background "dodgerblue"))
    (((class color) (background dark))
     (:foreground "yellow" :background "slateblue"))
    (((class grayscale))
     (:bold t) (:italic t)))
  "*チュートリアル中のセクションの表示部分の face。"
  :group 'skk-tut)

(defface skk-tut-do-it-face
  '((((class color) (background light))
     (:foreground "DarkGoldenrod"))
    (((class color) (background dark))
     (:foreground "LightGoldenrod"))
    (((class grayscale))
     (:bold t)))
  "*チュートリアル中の指示項目の表示部分の face。"
  :group 'skk-tut)

(defface skk-tut-question-face
  '((((class color) (background light))
     (:foreground "Blue"))
    (((class color) (background dark))
     (:foreground "LightSkyBlue"))
    (((class grayscale))
     (:underline t)))
  "*チュートリアル中の問題の表示部分の face。"
  :group 'skk-tut)

(defface skk-tut-key-bind-face
  '((((class color) (background light))
     (:foreground "Firebrick"))
    (((class color) (background dark))
     (:foreground "OrangeRed"))
    (((class grayscale))
     (:bold t)))
  "*チュートリアル中のキーバインドの表示部分の face。"
  :group 'skk-tut)

(defface skk-tut-hint-face
  '((((class color) (background light))
     (:foreground "CadetBlue"))
    (((class color) (background dark))
     (:foreground "Aquamarine"))
    (((class grayscale))
     (:italic t)))
  "*チュートリアル中のヒントの表示部分の face。
現在のところ、SKK.tut.E でしか使用されていない。"
  :group 'skk-tut)

;;; skk-show-mode.el related.
(defvar skk-show-mode-invoked nil)
(defvar skk-show-mode-functions '((inline . skk-show-mode-inline)
                                  (tooltip . skk-show-mode-tooltip)))
(defcustom skk-show-mode-show nil
  "*Non-nil であれば、かなモードやアスキーモードへ切り替わったときにカーソル付近に skk-*-mode-string を表示する。

表示スタイルは `skk-show-mode-style' で指定する。"
  :type 'boolean
  :group 'skk-visual)

(defvar skk-show-mode-enable t
  "内部用。チュートリアル実行中のみ nil となる。")

(defcustom skk-show-mode-style 'inline
  "*skk-show-mode の表示スタイル。"
  :type '(radio (const :tag "tooltip" tooltip)
                (const :tag "inline" inline))
  :group 'skk-visual)

(defvar skk-show-mode-inline-overlays nil
  "内部スタック用")

(defface skk-show-mode-inline-face
  '((((class color) (type tty))
     (:inherit default :background "gray"))
    (((class color) (background light))
     (:inherit default :background "gray"))
    (((class color) (background dark))
     (:inherit default :background "dark slate gray" :box t))
    (((class grayscale))
     (:inherit default)))
  "*inline 向けの背景色"
  :group 'skk-visual)

;;; skk-get related.
(defvar skk-get-jisyo-directory "~/.emacs.d/skk-get-jisyo"
  ;; (expand-file-name "../../../skk" data-directory)
  "`skk-get'の保存先")

;;; skk-search-web related.
(defvar skk-use-search-web nil
  "*Non-nil であれば、skk-search-web を有効にする.")

;; XXX workaround
;; face の property が一部の状況で反映されないことに対処
(when (and (not noninteractive)
           window-system)
  (dolist (f '(skk-tut-section-face
               skk-tut-section-face
               skk-tut-do-it-face
               skk-tut-question-face
               skk-tut-key-bind-face
               skk-tut-hint-face))
    (set-face-foreground f (face-foreground f))))

;;; skk-viper.el related.
(defcustom skk-use-viper nil
  "*Non-nil であれば、VIPER に対応する。"
  :type 'boolean
  :group 'skk-viper)

(defvar skk-viper-saved-cursor-color
  (when (and (featurep 'viper)
             (boundp 'viper-insert-state-cursor-color))
    (symbol-value 'viper-insert-state-cursor-color)))
(make-variable-buffer-local 'viper-insert-state-cursor-color)

(defconst skk-viper-use-vip-prefix
  (not (fboundp 'viper-normalize-minor-mode-map-alist)))

(defconst skk-viper-normalize-map-function
  (if skk-viper-use-vip-prefix
      'vip-normalize-minor-mode-map-alist
    'viper-normalize-minor-mode-map-alist)
  "Viper が `minor-mode-map-alist' を調整するための関数。")

;;; skk-decor.el related.

;; skk-show-inline 'vertical に限ってフェイスを作用させる
(defvar skk-inline-show-vertically-decor nil)

(defface skk-inline-show-vertically-cand-face
  '((((type tty))
     (:foreground "light sea green" :background "dark sea green"))
    (t
     (:foreground "white" :background "gray40" :height 180
                  :box (:line-width 3 :color "gray40" :style released-button) )))
  "候補に適用する FACE"
  :group 'skk-visual)

(defface skk-inline-show-vertically-anno-face
  '((((type tty))
     (:foreground "DarkSeaGreen"))
    (t
     (:foreground "yellow" :height 120)))
  "アノテーションに適用する FACE"
  :group 'skk-visual)

;; tooltip に限ってフェイスを作用させる
(defvar skk-tooltip-show-at-point-decor nil)

(defface skk-tooltip-show-at-point-cand-face
  '((((type tty))
     (:foreground "light sea green" :background "dark sea green"))
    (t
     (:foreground "white" :background "gray40" :height 200
                  :box (:line-width 3 :color "gray40" :style released-button) )))
  "候補に適用する FACE"
  :group 'skk-visual)

(defface skk-tooltip-show-at-point-anno-face
  '((((type tty))
     (:foreground "DarkSeaGreen"))
    (t
     (:foreground "yellow" :height 120)))
  "アノテーションに適用する FACE"
  :group 'skk-visual)

;; 候補バッファに限ってフェイスを作用させる
(defvar skk-henkan-show-candidates-buffer-decor nil)

(defface skk-henkan-show-candidates-buffer-cand-face
  '((((type tty))
     (:foreground "light sea green" :background "dark sea green"))
    (t
     (:foreground "white" :background "gray40" :height 250
                  :box (:line-width 3 :color "gray40" :style released-button) )))
  "候補に適用する FACE"
  :group 'skk-visual)

(defface skk-henkan-show-candidates-buffer-anno-face
  '((((type tty))
     (:foreground "DarkSeaGreen"))
    (t
     (:foreground "yellow" :height 120)))
  "アノテーションに適用する FACE"
  :group 'skk-visual)

;; skk-treat-candidate-appearance-function のために用意する関数
(defun skk-treat-candidate-sample1 (candidate listing-p)
  (cond
   ((string-match ";" candidate)
    (put-text-property 0 (match-beginning 0)
                       'face 'skk-tut-question-face
                       candidate)
    (put-text-property (match-beginning 0) (length candidate)
                       'face 'skk-tut-hint-face
                       candidate))
   (t
    (put-text-property 0 (length candidate)
                       'face 'skk-tut-question-face
                       candidate)))
  candidate)

(defun skk-treat-candidate-sample2 (candidate listing-p)
  (let* ((value (skk-treat-strip-note-from-word candidate))
         (cand (car value))
         (note (if listing-p
                   (or (and (eq skk-annotation-lookup-lookup 'always)
                            (skk-lookup-get-content cand t))
                       (and (eq skk-annotation-lookup-DictionaryServices 'always)
                            (skk-annotation-lookup-DictionaryServices cand t))
                       (cdr value))
                 (cdr value)))
         (sep (if note
                  (propertize (if (skk-annotation-display-p 'list)
                                  " ≒ "
                                " !")
                              'face 'skk-tut-do-it-face)
                nil)))
    (cond (note
           (put-text-property 0 (length cand)
                              'face 'skk-tut-question-face cand)
           (put-text-property 0 (length note)
                              'face 'skk-tut-hint-face note)
           (cons cand (cons sep note)))
          (t
           (put-text-property 0 (length cand)
                              'face 'skk-treat-default cand)
           cand))))

(defvar skk-emacs-modeline-property
  (when window-system
    (list 'local-map (let ((map (make-sparse-keymap)))
                       (define-key map [mode-line mouse-3]
                         #'skk-emacs-modeline-menu)
                       (define-key map [mode-line mouse-1]
                         #'skk-emacs-circulate-modes)
                       map)
          'help-echo
          "mouse-1: モード切替(循環)\nmouse-3: SKK メニュー"
          'mouse-face
          'highlight)))

(defvar skk-emacs-property-alist
  (when window-system
    (list
     (cons 'latin skk-emacs-modeline-property))))

(provide 'skk-vars)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-vars.el ends here
