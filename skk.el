;;; skk.el --- SKK (Simple Kana to Kanji conversion program)
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
;;             Murata Shuuichirou <mrt@astec.co.jp>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk.el,v 1.25 2000/01/17 04:05:23 furue Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/01/17 04:05:23 $

;; SKK is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;; SKK-MODE is a mode for inputting Japanese to a current buffer which is 
;; composed of four minor modes described below.
;;
;;      +----------------------+-------- skk-mode -----+----------------------+
;;      |                      |                       |                      |
;;      |                      |                       |                      |
;;  skk-j-mode           skk-latin-mode      skk-jisx0208-latin-mode   skk-abbrev-mode
;;                           ASCII               JISX0208 LATIN         ABBREVIATION
;;                  (C-j wakes up skk-j-mode)   (ZEN'KAKU EIMOJI)
;;
;; skk-j-mode-map     skk-latin-mode-map  skk-jisx0208-latin-mode-map skk-abbrev-mode-map
;; skk-katakana: nil 
;;   HIRAKANA
;;
;;  skk-j-mode-map
;; skk-katakana: t
;;   KATAKANA


;;; Code:
(require 'skk-foreword)

(defconst skk-version "10.58")
(defconst skk-major-version (string-to-int (substring skk-version 0 2)))
(defconst skk-minor-version (string-to-int (substring skk-version 3)))

;;;###autoload
(defun skk-version ()
  (interactive)
  (if (not (interactive-p))
      skk-version
    (save-match-data
      (let* ((raw-date "$Date: 2000/01/17 04:05:23 $")
             (year (substring raw-date 7 11))
             (month (substring raw-date 12 14))
             (date (substring raw-date 15 17)) )
        (if (string-match "^0" month)
            (setq month (substring month (match-end 0))) )
        (if (string-match "^0" date)
            (setq date (substring date (match-end 0))) )
        (message "SKK version %s of %s, APEL inside"
                 skk-version
                 (concat (car (rassoc month skk-month-alist))
                         " " date ", " year ))))))

;;;; variables declaration
;;; user variables

(defvar skk-init-file (convert-standard-filename "~/.skk")
  "*SKK の初期設定ファイル名。
skk.el 9.x より ~/.emacs でのカスタマイズも可能となった。"
;  "*Name of the SKK initialization file.
;From skk.el 9.x on all customization may be done in ~/.emacs."
)

;;;###autoload
(defgroup skk nil "SKK basic customization."
  :prefix "skk-"
  :group 'japanese
  :group 'input-method )

(defgroup skk-faces nil
  "Faces used by SKK."
  :group 'skk
  :group 'faces)

(defcustom skk-special-midashi-char-list '(?> ?< ??)
  "*接頭辞、接尾辞の入力のためのプレフィックスキー、サフィックスキーのリスト。"
  ;;  "*List of prefix and suffix keys for entering `settoji' and `setsubiji'."
  :type '(repeat character)
  :group 'skk )

(defcustom skk-mode-hook nil
  "*SKK を起動したときのフック。
他に、skk-auto-fill-mode-hook、skk-load-hook, skk-init-file でもカスタ
マイズが可能。"
  ;; "*Hook run at SKK startup.  This hook is also run
  ;;in skk-auto-fill-mode after skk-auto-fill-mode-hook.
  ;;skk-auto-fill-mode-hook, skk-load-hook, skk-init-file may also be used
  ;;for customization."
  :type 'hook
  :group 'skk )

(defcustom skk-auto-fill-mode-hook nil
  "*skk-auto-fill-mode を起動したときのフック。
他に、skk-mode-hook, skk-load-hook, skk-init-file でもカスタマイズが可
能。"
  ;;  "*Hook run at startup of skk-auto-fill-mode.
  ;;skk-mode-hook、skk-load-hook, skk-init-file may also be used for
  ;;customization."
  :type 'hook
  :group 'skk )

(defcustom skk-load-hook nil
  "*skk.el をロードしたときのフック。
他に、skk-mode-hook, skk-auto-fill-mode-hook, skk-init-file でもカスタ
マイズが可能。"
  ;;  "*Hook run when SKK is loaded.
  ;;skk-auto-fill-mode-hook、skk-mode-hook, skk-init-file may also be used
  ;;for customization."
  :type 'hook
  :group 'skk )

(defcustom skk-search-end-function nil
  "*単語検索終了時にコールされる関数。
この関数を利用して検索した単語の優先順位を変更するなどの作業が可能。
HENKAN-BUFFER, MIDASI, OKURIGANA, ENTRY の 4 引数を伴なってコールされる。
加工した ENTRY を返すこと。
この関数は、辞書バッファでコールされるので、変換を行なったバッファローカルな情報を
取り出したいときは、HENKAN-BUFFER を利用する。"
  :type '(choice function (const nil))
  :group 'skk )
 
(defcustom skk-update-end-function nil
  "*個人辞書の更新終了時にコールされる関数。
HENKAN-BUFFER, MIDASI, OKURIGANA, WORD, PURGE の 5 引数を伴なってコールされる。
この関数は、辞書バッファでコールされるので、変換を行なったバッファローカルな情報を取り
出したいときは、HENKAN-BUFFER を利用する。
skk-kakutei-initialize がコールされる前にこの関数がコールされるので、最後の確定
に関するフラグ類は、この関数の中から参照することができる。"
  :type '(choice function (const nil))
  :group 'skk )
  
(defcustom skk-kakutei-end-function nil
  "*確定時にコールされる関数。
KAKUTEI-WORD 引数を伴なって、変換を行なったバッファでコールされる。
skk-kakutei-initialize がコールされる前にこの関数がコールされるので、最後の確定
に関するフラグ類は、この関数の中から参照することができる。" 
  :type '(choice function (const nil))
  :group 'skk )

(defcustom skk-kakutei-jisyo nil
  "*最初に検索する辞書。
Non-nil で、かつ skk-search-prog-list の要素の中にこの変数が使用されていれば、
指定された辞書を検索のためバッファに読み込み、検索を行なう。
見出し語は、ソートされていなければならない。
各見出し語の最初のエントリしか検索しない (複数のエントリがあっても 2 番目以降の
エントリは無視される)。
skk-search-prog-list の値を設定することにより、検索対象の辞書の変更、検索の順
序の変更が可能。"
  ;;  "*The first dictionary to be searched.
  ;;If non-nil, and this variable is used as a component of
  ;;`skk-search-prog-list', the indicated dictionary is read into a
  ;;buffer and searched.
  ;;The keys must be sorted.
  ;;Only the first entry in each key is checked; if several entries are
  ;;present the second and following entries are ignored.
  ;;By setting the value of `skk-search-prog-list' the dictionaries
  ;;searched and the order of search can be changed."
  :type '(choice file (const nil))
  :group 'skk )

(defcustom skk-initial-search-jisyo nil
  "*ユーザー辞書の検索の前に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nil で、かつ skk-search-prog-list の要素の中にこの変数が使用されていれば、
指定された辞書を検索のためバッファに読み込み、検索を行なう。
skk-search-prog-list の値を設定することにより、検索対象の辞書の変更、検索の順
序の変更が可能。"
  ;;  "*This dictionary is searched before the user's personal dictionary.
  ;;The keys must be sorted.
  ;;If non-nil, and this variable is used as a component of
  ;;`skk-search-prog-list', the indicated dictionary is read into a
  ;;buffer and searched.
  ;;By setting the value of `skk-search-prog-list' the dictionaries
  ;;searched and the order of search can be changed."
  :type '(choice file (const nil))
  :group 'skk )

(defcustom skk-large-jisyo nil
  "*ユーザー辞書の検索の後に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nil で、かつ skk-search-prog-list の要素の中にこの変数が使用されていれば、
指定された辞書を検索のためバッファに読み込み、検索を行なう。
skk-search-prog-list の値を設定することにより、検索対象の辞書の変更、検索の順
序の変更が可能。" 
  :type '(choice file (const nil))
  :group 'skk )

(defcustom skk-aux-large-jisyo nil
  "*SKK サーバーで最後に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nil で、かつ skk-search-prog-list の要素の中にこの変数が使用されていれば、
SKK サーバーを使い検索を行う。
SKK サーバーが active でなければ、指定された辞書をバッファに読み込む。
skk-search-prog-list の値を設定することにより、検索対象の辞書の変更、検索の順
序の変更が可能。
この値を設定することにより、skk-server.el が autoload される。" 
  :type '(choice file (const nil))
  :group 'skk )

(defcustom skk-search-prog-list
  '((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
    (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
    (skk-search-jisyo-file skk-jisyo 0 t)
    ;; skk-auto.el をロードすると下記の要素がプラスされる。
    ;;(skk-okuri-search)
    (skk-search-jisyo-file skk-large-jisyo 10000)
    ;; skk-server.el をロードすると下記の要素がプラスされる。
    ;;(skk-search-server skk-aux-large-jisyo 10000)
    ;; skk-server-host もしくは skk-servers-list を指定すると、skk-server.el 
    ;; が autoload される。
    )
  "*検索関数、検索対象の辞書を決定するためのリスト。
変換した候補を返す S 式をリストの形に表記したもの。
skk-search 関数が skk-search-prog-list の car から後方向へ順番に S 式の評価を
行い変換を行なう。" 
  :type '(repeat
	  (list (function :tag "Search funcition")
		(choice :tag "Dictionary" file (const nil))
		(choice :tag "Minimum region size to be binary-searched"
			integer (const nil) )
		(choice :tag "Quietly reading dictionary to Emacs buffer"
			(const t) (const nil) )))
  :group 'skk )

(defcustom skk-jisyo (convert-standard-filename "~/.skk-jisyo")
  "*SKK のユーザー辞書。" 
  :type 'file
  :group 'skk )

(defcustom skk-backup-jisyo (convert-standard-filename "~/.skk-jisyo.BAK")
  "*SKK のユーザー辞書のバックアップファイル。" 
  :type 'file
  :group 'skk )

(defcustom skk-jisyo-code nil
  "*Non-nil であれば、その値で辞書バッファの漢字コードを設定する。
Mule では、*euc-japan*, *sjis*, *junet*。
また、\"euc\", \"ujis\", \"sjis\", \"jis\" などの文字列によっても指定が可能。" 
  :type '(choice symbol string)
  :group 'skk )

(defcustom skk-keep-record t
  "*Non-nil であれば、変換に関する記録を skk-record-file に取る。"
  :type 'boolean
  :group 'skk )

(defcustom skk-record-file (convert-standard-filename "~/.skk-record")
  "*ユーザー辞書の統計を取るファイル。
辞書セーブの時刻、単語の登録数、確定を行った回数、確定率、全体の語数の
情報を収める。" 
  :type 'file
  :group 'skk )

(defcustom skk-kakutei-key "\C-j"
  "*漢字変換の確定動作を行うキー。"
  :type 'string
  :group 'skk )

(defcustom skk-previous-candidate-char ?x
  "*skk-previous-candidate を割当てたキーキャラクタ。" 
  :type 'character
  :group 'skk )

(defcustom skk-try-completion-char ?\011 ; TAB 
  "*見出し語の補完動作を行なうキーキャラクタ。" 
  :type 'character
  :group 'skk )

(defcustom skk-next-completion-char ?.
  "*見出し語の補完動作で、次の候補を出力するキーキャラクタ。" 
  :type 'character
  :group 'skk )

(defcustom skk-previous-completion-char ?,
  "*見出し語の補完動作で、前の候補を出力するキーキャラクタ。" 
  :type 'character
  :group 'skk )

(defcustom skk-start-henkan-char ?\040	; SPC
  "*漢字変換を開始するキーキャラクタ。" 
  :type 'character
  :group 'skk )

(defcustom skk-start-henkan-with-completion-char ?\240 ; M-SPC
  "*見出し語を補完しながら▼モードに入るキーキャラクタ。" 
  :type 'character
  :group 'skk )

(defcustom skk-backward-and-set-henkan-point-char ?\321 ; M-Q
  "*ポイントを戻して▽モードに入るキーキャラクタ。" 
  :type 'character
  :group 'skk )

(defcustom skk-use-viper nil
  "*Non-nil であれば、VIPER に対応する。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-henkan-okuri-strictly nil
  "*Non-nil であれば、見出し語と送り仮名が一致したときだけ候補として出力する。
例えば、下記のような辞書エントリが、skk-jisyo \(プライベート辞書\) にあった場合に

  \"おおk /大/多/[く/多/]/[き/大/]/\"

\"▽おお*く\" を変換したとき、\"多く\" のみを出力し、\"大く\" を出力しない。

SKK-JISYO.[SML] の送り仮名エントリは上記の形式になっていないので、skk-jisyo の
送りありの辞書エントリがこの形式のものをあまり含んでいない場合は、このオプショ
ンを on にすることで、すぐに単語登録に入ってしまうので注意すること。

skk-process-okuri-early の値が nil ならば上記の形式で skk-jisyo が作られる。

Emacs 19 ベースの Mule ならば、下記のフォームを評価することで、単語登録に入っ
たときだけ一時的にこのオプションを nil にすることができる。

    \(add-hook 'minibuffer-setup-hook
              \(function
               \(lambda \(\)
                 \(if \(and \(boundp 'skk-henkan-okuri-strictly\)
                          skk-henkan-okuri-strictly
                          \(not \(eq last-command 'skk-purge-from-jisyo\)\) \)
                     \(progn
                       \(setq skk-henkan-okuri-strictly nil\)
                       \(put 'skk-henkan-okuri-strictly 'temporary-nil t\) \)\)\)\)\)

    \(add-hook 'minibuffer-exit-hook
              \(function
               \(lambda \(\)
                 \(if \(get 'skk-henkan-okuri-strictly 'temporary-nil\)
                     \(progn
                       \(put 'skk-henkan-okuri-strictly 'temporary-nil nil\)
                       \(setq skk-henkan-okuri-strictly t\) \)\)\)\)\)

このオプション利用時は、skk-process-okuri-early の値は nil でなければならない
\(メニューバーを利用してカスタマイズした場合は自動的に調整される\)。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-henkan-strict-okuri-precedence nil
  "*Non-nil であれば、見出し語と送り仮名が一致した候補を優先して表示する。
例えば、下記のような辞書エントリが、skk-jisyo \(プライベート辞書\) にあった場合に

  \"おおk /大/多/[く/多/]/[き/大/]/\"

\"▽おお*く\" を変換したとき、まず\"多く\" を出力し、
次に \"大く\" を出力する。

\"大く\"などの候補はうっとうしいが、すぐに単語登録にはいってしまうのも
嫌なひとにおすすめ。

このオプション利用時は、skk-process-okuri-early の値は nil でなければならない。
また skk-henkan-okuri-strictly が non-nil のときは、この変数は無視される。
\(メニューバーを利用してカスタマイズした場合は自動的に調整される\)。"
  :type 'boolean
  :group 'skk )
 
(defcustom skk-auto-okuri-process nil
  "*Non-nil であれば、送り仮名部分を自動認識して変換を行う。
例えば、

    \"Uresii (\"UreSii\" ではなく) -> 嬉しい\"

のように変換される。但し、skk-jisyo 辞書 \(プライベート辞書\) が、

    \"うれs /嬉/[し/嬉/]/\"

のような形式になっていることが必要である \(SKK-JISYO.[SML] はこの形式に対応し
ていないので、skk-jisyo にこのエントリがなければならない\)。

このオプション利用時は、skk-process-okuri-early の値は nil でなければならない
\(メニューバーを利用してカスタマイズした場合は自動的に調整される\)。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-process-okuri-early nil
  "*Non-nil であれば、送り仮名のローマ字プレフィックスの入力時点で変換を開始する。
例えば、

    \"UgoK -> ▼動k\"。

送り仮名が分らないまま変換していることになるので、skk-jisyo が送り仮名に対応し
た形に成長しない。つまり

    \"うごk /動/\"

のような形態のままとなる。ただし、既に

    \"うごk /動/[く/動/]/[か/動/]/[け/動/]/[き/動/]/[こ/動/]/\"

のようなエントリが skk-jisyo にあれば、それを破壊しない。

nil であれば、送り仮名の入力が完了した時点で変換が開始する。例えば、

    \"UgoK -> ▽うご*k\", \"UgoKu -> ▼動く\"

このオプションを on にして skk-mode を起動すると、両立できないオプションである
skk-kakutei-early, skk-auto-okuri-process, skk-henkan-okuri-strictly は nil に
セットされる。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-egg-like-newline nil
  "*Non-nil であれば、▼モードで改行をタイプしても確定するのみで改行しない。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-kakutei-early t
  "*Non-nil であれば skk-insert が呼ばれたときに現在の候補を確定する。
例えば、

    \"▽かくてい -> ▼確定 -> 確定s -> 確定す\"

のように変換後、「す」の prefix である \"s\" を入力した時点で確定する。
nil であれば、例えば

    \"▽かくてい -> ▼確定 -> ▼確定s -> ▼確定する -> 確定する。\"

のように skk-kakutei を直接、間接にコールするまで \(句読点を入力したり、新たな
▽モードに入ったりすると間接的に skk-kakutei をコールする\) は、確定しないので、
その間は、変換候補を選びなおすことなどが可能。

このオプション利用時は、skk-process-okuri-early の値は nil でなければならない
\(メニューバーを利用してカスタマイズした場合は自動的に調整される\)。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-delete-implies-kakutei t
  "*Non-nil であれば、▼モードで BS を押すと、前の一文字を削除し確定する。
nil であれば、一つ前の候補を表示する。"
  :type 'boolean
  :group 'skk )

(defcustom skk-allow-spaces-newlines-and-tabs t
  "*Non-nil であれば、見出し語の中にスペース、タブ、改行があってもそれを取り除いて変換することが可能。
例えば、下記のように 中に改行が入っていても変換が可能である。

     \"▽か
  な\"
   -> \"仮名\"

この値が nil であれば、最初のスペースで見出し語を切り詰めてしまい、以降のスペー
ス、タブ、改行は無視される。
この値は、skk-start-henkan, skk-latin-henkan, skk-katakana-henkan,
skk-hiragana-henkan, skk-jisx0208-latin-henkan 及び
skk-backward-and-set-henkan-point の動作に影響する。"
  :type 'boolean
  :group 'skk )

(defcustom skk-convert-okurigana-into-katakana nil
  "*Non-nil であれば、カタカナモードで変換したときに送り仮名もカタカナに変換する。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-delete-okuri-when-quit nil
  "*Non-nil であれば、送りありの変換中に \"C-g\" を押すと送り仮名を消し▽モードに入る。
例えば、

    \"▽な*く -> ▼泣く -> \"C-g\" ->▽な\"

nil であれば、送り仮名を含めた見出し語をそのまま残し、■モードに入る。例えば、

    \"▽な*く -> ▼泣く -> \"C-g\" -> なく\"" 
  :type 'boolean
  :group 'skk )

(defcustom skk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)
  "*メニュー形式で候補を選択するときの選択キーのリスト。
\"x\", \" \" 及び \"C-g\" 以外の 7 つのキー (char type) を含む必要があ
る。\"x\", \" \" 及び \"C-g\" は候補選択時にそれぞれ特別な仕事に割り当
てられているので、このリストの中には含めないこと。"
  :type '(repeat character)
  :group 'skk )

(defcustom skk-status-indicator 'minor-mode
  "*SKK の状態をモード行のどこに表示するかを決める。
left であれば左端に表示する。
さもなければマイナーモードとしての表示法を取る。"
  :type '(choice (const minor-mode)
		 (const left))
  :group 'skk )

(defcustom skk-latin-mode-string " SKK"
  "*SKK が latin (ascii) モードであるときにモードラインに表示される文字列。" 
  :type 'string
  :group 'skk )

(defcustom skk-hiragana-mode-string " かな"
  "*ひらがなモードであるときにモードラインに表示される文字列。"
  :type 'string
  :group 'skk )

(defcustom skk-katakana-mode-string " カナ"
  "*カタカナモードであるときにモードラインに表示される文字列。"
  :type 'string
  :group 'skk )

(defcustom skk-jisx0208-latin-mode-string " 全英"
  "*全英モードであるときにモードラインに表示される文字列。"
  :type 'string
  :group 'skk )

(defcustom skk-abbrev-mode-string " aあ"
  "*SKK abbrev モードであるときにモードラインに表示される文字列。"
  :type 'string
  :group 'skk )

(defcustom skk-echo t
  "*Non-nil であれば、仮名文字のプレフィックスを表示する。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-use-numeric-conversion t
  "*Non-nil であれば、数値変換を行う。" 
  :type 'boolean
  :group 'skk )

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
    ("z," nil "‥")
    ("z-" nil "〜")
    ("z." nil "…")
    ("z/" nil "・")
    ("z[" nil "『")
    ("z]" nil "』")
    ("za" nil ("ザ" . "ざ"))
    ("ze" nil ("ゼ" . "ぜ"))
    ("zh" nil "←")
    ("zi" nil ("ジ" . "じ"))
    ("zj" nil "↓")
    ("zk" nil "↑")
    ("zl" nil "→")
    ("zo" nil ("ゾ" . "ぞ"))
    ("zu" nil ("ズ" . "ず"))
    ("zya" nil ("ジャ" . "じゃ"))
    ("zye" nil ("ジェ" . "じぇ"))
    ("zyi" nil ("ジィ" . "じぃ"))
    ("zyo" nil ("ジョ" . "じょ"))
    ("zyu" nil ("ジュ" . "じゅ"))
    ("." nil skk-current-kuten)
    ("," nil skk-current-touten)
    ("-" nil "ー")
    (":" nil "：")
    (";" nil "；")
    ("?" nil "？")
    ("[" nil "「")
    ("]" nil "」")
    ("l" nil skk-latin-mode)
    ("q" nil skk-toggle-kana)
    ("L" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-set-henkan-point-subr)
    ("X" nil skk-purge-from-jisyo)
    ("/" nil skk-abbrev-mode)
    ("$" nil skk-display-code-for-char-at-point)
    ("@" nil skk-today)
    ("\\" nil skk-input-by-code-or-menu)
    )
  ;; コンスタントにしてしまわないのは、ローマ字入力とは全く別の設定を
  ;; する人もいるからです。
  "*キー入力に対する変換文字を現わすオートマトン状態遷移規則。
リストの各要素は、下記のリスト形式を満たしていなければならない。

\(現在のキー入力状態[@次キー入力0][@次キー入力1]...[@次キー入力n] 最終のキー入力状態 出力\)

\(但し、\"@\" は連接\) を意味する。

出力に指定できるものは、文字列、文字列を car, cdr に持つ dot pair、
関数名シンボルのいずれか。dot pair は、カナモードのときは car の文
字列、かなモードのときは cdr の文字列が挿入される。文字列のみ指定され
ている場合は、入力モードにかかわらずその文字が挿入される。
文字列を挿入する関数については、insert を明示的に呼ぶ必要はなく、文字
列を返せば良い。文字列を挿入しない関数についても指定は可。

この変数の定義をベースに skk-rom-kana-rule-list が追加され、skk-mode
起動時に skk-rule-tree という木の形にコンパイルされる。
2 つのルールリストに重複するキーの設定がある場合は、
skk-rom-kana-rule-list の定義が優先される。" 
  :type '(repeat
	  (list string string
		(choice function string (cons string string)) ))
  :group 'skk )

(defcustom skk-rom-kana-rule-list
  '(
    ;; ユーザーの好みで設定が分れそうな要素は、
    ;; skk-rom-kana-base-rule-list からこちらへ移しましょう...。
    ("hh" "h" ("ッ" . "っ"))
    ;; when you may want to insert 「がんま」by "gamma"...
    ("mm" "m" ("ン" . "ん"))
    )
  "*キー入力に対する変換文字を現わすオートマトン状態遷移規則で、ユーザーの追加の設定を行なうもの。
ベースとなる skk-rom-kana-base-rule-list にこの変数の定義が追加され、
skk-mode 起動時に skk-rule-tree という木の形にコンパイルされる。
2 つのルールリストに重複するキーの設定がある場合は、この変数の定義が優
先される。

リストの各要素は、下記のリスト形式を満たしていなければならない。

\(現在のキー入力状態[@次キー入力0][@次キー入力1]...[@次キー入力n] 最終のキー入力状態 出力\)

\(但し、\"@\" は連接\) を意味する。

出力の種類については、skk-rom-kana-base-rule-list を参照のこと。
ユーザーが追加したいルールを

    \(setq skk-rom-kana-rule-list
      '\(
        \(\"hh\" \"h\" \(\"ッ\" . \"っ\"\)\)
        \(\"@\" nil \"＠\"\)
        ...
        \)

のように .emacs や skk-init-file に直接書くのが手軽。

ディフォルトでは、\(\"hh\" \"h\" \(\"ッ\" . \"っ\"\)\) という要素が設
定されているが、\"ohhira\" -> \"おおひら\" のように \"hh\" を促音処理
したくなければ、skk-rom-kana-rule-list から

    \(\"hh\" \"h\" \(\"ッ\" . \"っ\"\)\) 

という要素を消す。
また、`@' で skk-today (当日の日付の入力) を起動する代りに `＠' を入
力したい場合は、skk-rom-kana-rule-list に

    \(\"@\" nil \"＠\"\)

という要素を加える。skk-mode の起動後 skk-rom-kana-rule-list の変更を
行なった場合、その設定を反映させるには M-x skk-restart を実行する必要
がある。" 
  :type '(repeat
	  (list string string
		(choice function string (cons string string)) ))
  :group 'skk )

(defcustom skk-kana-input-search-function
  (function
   (lambda ()
     (save-match-data
       (and (string-match "^h\\([bcdfghjklmnpqrstvwxz]\\)$" skk-prefix)
	    (member (char-to-string (preceding-char)) '("お" "オ"))
	    (cons '("オ" . "お") (match-string 1 skk-prefix)) ))))
  "*ルールリストの中に記せない変換ルールを処理する関数。
skk-rom-kana-base-rule-list と skk-rom-kana-rule-list の要素を全て検索
した後にコールされる。引数はない。

\(現在の入力に対する出力 . \"続く unfixed prefix\"\)

というセルを返す。出力の種類については、skk-rom-kana-base-rule-list を
参照のこと。

ディフォルトでは、\"お\" の後の \"h\" + 子音の入力を \"おお\" + 続く子
音処理用の unfixed prefix に変換している。" 
  :type 'function
  :group 'skk )

(defcustom skk-okuri-char-alist nil
  "*ある送り仮名を別の送り仮名に変換するルールを記述するエーリスト。" 
  :type '(repeat (cons string string))
  :group 'skk )

(defcustom skk-downcase-alist nil
  "*変換キー (大文字ローマ字) の小文字への変換規則を表わすエーリスト。
変換キーの入力を開始する際、SKK では大文字で入力を行なうので、
skk-set-henkan-point の中でこれを小文字に変換する作業を行なう。このエー
リストに大文字 -> 小文字の変換ルールを書いておくことで、キー入力のカス
タマイズを行なうことができる。このエーリストが null の場合は、単に
downcase される。" 
  :type '(repeat (cons character character))
  :group 'skk )

(defcustom skk-jisx0208-latin-vector
  [nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "　"  "！" "”" "＃" "＄" "％" "＆" "’"
   "（" "）" "＊" "＋" "，" "−" "．" "／"
   "０" "１" "２" "３" "４" "５" "６" "７"
   "８" "９" "：" "；" "＜" "＝" "＞" "？"
   "＠" "Ａ" "Ｂ" "Ｃ" "Ｄ" "Ｅ" "Ｆ" "Ｇ"
   "Ｈ" "Ｉ" "Ｊ" "Ｋ" "Ｌ" "Ｍ" "Ｎ" "Ｏ"
   "Ｐ" "Ｑ" "Ｒ" "Ｓ" "Ｔ" "Ｕ" "Ｖ" "Ｗ"
   "Ｘ" "Ｙ" "Ｚ" "［" "＼" "］" "＾" "＿"
   "‘" "ａ" "ｂ" "ｃ" "ｄ" "ｅ" "ｆ" "ｇ"
   "ｈ" "ｉ" "ｊ" "ｋ" "ｌ" "ｍ" "ｎ" "ｏ"
   "ｐ" "ｑ" "ｒ" "ｓ" "ｔ" "ｕ" "ｖ" "ｗ"
   "ｘ" "ｙ" "ｚ" "｛" "｜" "｝" "〜" nil]
  "*skk-jisx0208-latin-insert で参照される文字テーブル。
キーに対応する位置に文字列があれば、全英モードで該当のキーを押すことで、対応す
る文字が挿入される。
例えば、スペースキーに対応して、半角スペースを挿入させるように変更したければ、
skk.el のロード後 (もしくは skk-load-hook を利用して)、

     \(aset skk-jisx0208-latin-vector 32 \" \"\)

とするか、もしくは、skk-jisx0208-latin-vector の 32 番目 (0 番から数えて) の値を \" \"
とするような skk-jisx0208-latin-vector を直接書き、setq で代入する。32 は、? (半角ス
ペースの char type) を評価したときの値。" 
  :type 'vector
  :group 'skk )

(defcustom skk-use-face (or window-system (skk-terminal-face-p))
  "*Non-nil であれば、Emacs の face の機能を使用して変換表示を行なう。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-henkan-face 'highlight
  "*変換候補の face 属性。skk-use-face が non-nil のときのみ有効。
Emacs 標準フェイスの default, modeline, region, secondary-selection,
highlight, underline, bold, italic, bold-italic の他、新たに face を作
り指定することも可能。
新たな face を作り指定するには skk-make-face を利用して、

      \(skk-make-face 'DimGray/PeachPuff1\)
      \(setq skk-henkan-face 'DimGray/PeachPuff1\)

のようにするのが手軽。foreground と background の色指定だけでない凝った face
を作る場合は、skk-make-face では対応できないので、Emacs の hilit19.el の
hilit-lookup-face-create などを利用する。色を付ける場合の配色は、canna.el の
canna:attribute-alist が良い例かもしれない。" 
  :type 'face
  :group 'skk )

(defcustom skk-use-color-cursor (and window-system (fboundp 'x-display-color-p)
				     (x-display-color-p) )
  "*Non-nil であれば、SKK モードの入力モードに応じてカーソルに色を付ける。"
  :type 'boolean
  :group 'skk )

(defcustom skk-default-cursor-color
  (if (eq skk-emacs-type 'xemacs)
      (frame-property (selected-frame) 'cursor-color)
    (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))
  "*SKK のオフを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :group 'skk )

(defcustom skk-hiragana-cursor-color (if (eq skk-background-mode 'light)
					 "coral4"
				       "pink" )
  "*かなモードを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :type 'string
  :group 'skk )

(defcustom skk-katakana-cursor-color (if (eq skk-background-mode 'light)
					 "forestgreen"
				       "green" )
  "*カタカナモードを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :type 'string
  :group 'skk )

(defcustom skk-jisx0208-latin-cursor-color "gold"
  "*全角英字モードを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :type 'string
  :group 'skk )

(defcustom skk-latin-cursor-color (if (eq skk-background-mode 'light)
				      "ivory4"
				    "gray" )
  "*アスキーモードを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :type 'string
  :group 'skk )

(defcustom skk-abbrev-cursor-color "royalblue"
  "*abbrev モードを示すカーソル色。
skk-use-color-cursor が non-nil のときに使用される。" 
  :type 'string
  :group 'skk )

(defcustom skk-report-set-cursor-error t
  "*Non-nil であれば、カラーマップ切れが起きた場合、エラーメッセージを表示する。
nil であれば、表示しない。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-use-cursor-change t
  "*Non-nil であれば、Ovwrt マイナーモード時にカーソルの幅を縮める。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-auto-insert-paren nil
  "*Non-nil であれば、2 つの文字列をまとめて挿入し、その文字列の間にカーソルを移動する。
例えば、\"「\" を入力したときに \"」\" を自動的に挿入し、両かぎかっこの間に
カーソルを移動する。
挿入する文字列は、skk-auto-paren-string-alist で指定する。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-auto-paren-string-alist
  '(("「" . "」") ("『" . "』") ("(" . ")") ("（" . "）")
    ("{" . "}")("｛" . "｝") ("〈" . "〉") ("《" . "》")
    ("[" . "]") ("［" . "］") ("〔" . "〕") ("【" . "】")
    ("\"" . "\"")("“" . "”") ("`" . "'")
    ;;("<" . ">") ;; skk-special-midashi-char-list の中にある文字。
    )
  "*自動的に対になる文字列を入力するための連想リスト。
 skk-auto-insert-paren が non-nil の場合、car の文字列が挿入されたとき
に cdr の文字列を自動的に挿入され、カーソルはその 2 つの文字列の間に移
動する。
skk-special-midashi-char-list の要素になっている文字は、
skk-auto-paren-string-alist に含めても削除される。 " 
  :type '(repeat (cons string string))
  :group 'skk ) 

(defcustom skk-japanese-message-and-error nil
  "*Non-nil であれば、SKK のメッセージとエラーを日本語で表示する。
nil であれば、英語で表示する。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-set-henkan-point-key
  '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?M ?N ?O ?P ?R ?S ?T ?U ?V ?W ?Y ?Z)
  "*変換の開始地点を決めるキーのリスト。"
  :type '(repeat character)
  :group 'skk )

(defcustom skk-jisyo-save-count 50
  "*数値であれば、その回数辞書が更新されたときに辞書を自動的にセーブする。
nil であれば、辞書のオートセーブを行なわない。" 
  :type '(choice integer (const nil))
  :group 'skk )

(defcustom skk-byte-compile-init-file nil
  "*Non-nil であれば、skk-mode 起動時に skk-init-file をバイトコンパイルする。
正確に言うと、

  (1)skk-init-file をバイトコンパイルしたファイルがないか、
  (2)skk-init-file とそのバイトコンパイル済ファイルを比較して、前者の方が新し
     いとき

に skk-init-file をバイトコンパイルする。
nil であれば、skk-init-file とそのバイトコンパイル済みファイルを比較して 
skk-init-file の方が新しいときは、そのバイトコンパイル済ファイルを消す。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-count-private-jisyo-candidates-exactly nil
  "*Non-nil であれば、Emacs を終了するときに正確に個人辞書の候補数を数える。
nil であれば、1 行に複数の候補があっても 1 候補として数える。
計算結果は、skk-record-file に保存される。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-compare-jisyo-size-when-saving t
  "*Non-nil であれば、skk-jisyo のセーブ時にファイルサイズのチェックを行なう。
前回セーブした skk-jisyo と今回セーブしようとする辞書とのサイズ比較を行ない、
後者の方が大きいときにユーザーにセーブを続けるかどうかの確認を求める。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-auto-start-henkan t
  "*単語や文節の区切りを示す文字の打鍵により自動的に変換を開始する。
skk-auto-start-henkan-keyword-list により単語や文節の区切りを示す文字を指定する。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-auto-start-henkan-keyword-list
  '("を" "、" "。" "．" "，" "？" "」" "！" "；" "：" ")" ";" ":"
    "）" "”" "】" "』" "》" "〉" "｝" "］" "〕" "}" "]" "?" "."
    "," "!" )
  ;; あまりキーワードが多くなると、通常の変換を困難にする？
  "*自動変換を開始するキーワード。
skk-auto-start-henkan が non-nil のとき、このリストの要素の文字を挿入
すると、SPC を押すことなく自動的に変換を開始する。" 
  :type '(repeat string)
  :group 'skk )

(defcustom skk-search-excluding-word-pattern-function nil
  "*個人辞書に取り込まない文字列のパターンを検索する関数を指定する。
確定した文字列を引数に渡して funcall される。

SKK では変換、確定を行なった文字列は全て個人辞書に取り込まれるが、この
変数で指定された関数が non-nil を返すとその文字列は個人辞書に取り込ま
れない。

例えば、この変数に下記のような指定すると、変換により (SKK abbrev mode
での変換を除く) カタカナのみからなる文字列を得て確定しても、それを個人
辞書に取り込まない。

  \(setq skk-search-excluding-word-pattern-function
        \(function
         \(lambda \(kakutei-word\)
         ;; この関数が t を返したときは、その文字列は個人辞書に取り込まれない。
           \(save-match-data
             \(and
            ;; 送りなし変換で、
              \(not skk-okuri-char\)
            ;; 確定語がカタカナのみから構成されていて、
              \(string-match \"^[ーァ-ン]+$\" kakutei-word\)
            ;; SKK abbrev mode 以外での変換か、
              \(or \(not skk-abbrev-mode\)
                ;; 見出し語がカタカナ、ひらがな以外のとき。
                ;; \(後で▽マークを付けたときは、見出し語が英文字でも、
                ;; skk-abbrev-modeが t になっていない\)。
                  \(not \(string-match \"^[^ーァ-ンぁ-ん]+$\" skk-henkan-key\)\) \)\)\)\)\)\)

カタカナを変換により求めたいが、個人辞書にはカタカナのみの候補を取り込みた
くない、など、個人辞書が必要以上に膨れるのを抑える目的に使用できる。

なお、個人辞書に取り込まない見出し語については補完が効かないので、注意すること。"
  :type 'function
  :group 'skk )

(defcustom skk-update-jisyo-function 'skk-update-jisyo-original
  "*skk-update-jisyo で使用する関数。" 
  :type 'function
  :group 'skk )

(defcustom skk-save-jisyo-function 'skk-save-jisyo-original
  "*skk-save-jisyo で使用する関数。" 
  :type 'function
  :group 'skk )

(defcustom skk-count-jisyo-candidates-function
  'skk-count-jisyo-candidates-original
  "*skk-count-jisyo-candidates で使用する関数。" 
  :type 'function
  :group 'skk )

(defcustom skk-public-jisyo-to-be-searched-function
  'skk-public-jisyo-to-be-searched-original
  "*skk-public-jisyo-has-entry-p で使用する関数。" 
  :type 'function
  :group 'skk )

(defcustom skk-use-look nil
  "*Non-nil であれば、UNIX look コマンドを利用した補完・変換を行なう。
SKK abbrev モードで補完を行なうと、個人辞書を検索し尽した後で、UNIX look コマン
ドによる英単語補完を行なう。例えば、 

  ▽confe \(TAB\)
  ---> ▽conference

SKK abbrev モードで、「英文字 + アスタリスク」にて変換を行なうと、look コマンド
によるあいまい検索を行なうことができる。例えば、

 ▽confere* \(SPC\)
  ---> ▼conference

この状態で確定すると、`confere*' を見出し語、`conference' を候補とするエントリ
が個人辞書に追加される。`skk-search-excluding-word-pattern-function' によ
り、確定してもこのようなエントリを追加しないように設定することができる。" 
  :type 'boolean
  :group 'skk )

(defcustom skk-kana-rom-vector
  ["x" "a" "x" "i" "x" "u" "x" "e" "x" "o" "k" "g" "k" "g" "k" "g"
   "k" "g" "k" "g" "s" "z" "s" "j" "s" "z" "s" "z" "s" "z" "t" "d"
   "t" "d" "x" "t" "d" "t" "d" "t" "d" "n" "n" "n" "n" "n" "h" "b"
   "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "m" "m" "m"
   "m" "m" "x" "y" "x" "y" "x" "y" "r" "r" "r" "r" "r" "x" "w" "x"
   "x" "w" "n"]
  "*skk-remove-common で使用するかな文字からローマ字への変換ルール。
下記の該当するかな文字をその文字のローマ字プレフィックスで現わしたもの。
    ぁ  あ  ぃ  い  ぅ  う  ぇ  え  ぉ  お  か  が  き  ぎ  く  ぐ
    け  げ  こ  ご  さ  ざ  し  じ  す  ず  せ  ぜ  そ  ぞ  た  だ
    ち  ぢ  っ  つ  づ  て  で  と  ど  な  に  ぬ  ね  の  は  ば
    ぱ  ひ  び  ぴ  ふ  ぶ  ぷ  へ  べ  ぺ  ほ  ぼ  ぽ  ま  み  む
    め  も  ゃ  や  ゅ  ゆ  ょ  よ  ら  り  る  れ  ろ  ゎ  わ  ゐ
    ゑ  を  ん
それぞれのかな文字が送り仮名である場合にどのローマ字プレフィックスを対応させる
のかを指定することができる。「じ」、「ち」、「ふ」の文字について、対応するロー
マ字プレフィックスを \"z\", \"c\",\"f\" に変更を希望する場合もあるであろう。
skk-auto-okuri-process の値が non-nil のとき、あるいはサ変補助変換が行なわれる
とき参照される。" 
  :type 'vector
  :group 'skk )

(defcustom skk-henkan-overlay-priority 600
  "*変換した候補に重ねる overlay の priority。
例えば、Viper で R コマンドにより replace を行なうときに、
viper-replace-overlay という priority 400 の overlay を重ねられるが、
skk-henkan-overlay-priority のディフォルト値はこの overlay より
priority が高いので、優先して表示される。" 
  :type 'integer
  :group 'skk )

(defcustom skk-kuten-touten-alist '((jp . ("。" . "、" )) (en . ("．" . "，")))
  "*句点と読点のエーリスト。
各要素の形式は、

   \(シンボル . \(句点を表わす文字列 . 読点を表わす文字列\)\)

という cons cell。シンボルの部分は、`jp' もしくは `en' とし、
skk-toggle-kutouten はこれをトグルで切り換える。
ディフォルトの句読点のタイプは、`skk-kutouten-type' で指定する。" 
  :type '(repeat (cons (choice (const jp) (const en))
		       (cons string string)  ))
  :group 'skk )

(skk-deflocalvar skk-kutouten-type 'jp
  "*ディフォルトの句読点のタイプ。`jp' もしくは `en' というシンボル。" )

(defcustom skk-read-from-minibuffer-function nil
  "*単語登録モードで read-from-minibuffer の INITIAL-CONTENTS を提供する funcition。
この function は文字列を返さなければならない。
例えば、skk-henkan-key をそのまま initial-contents として利用したいとき
は、
  \(setq skk-read-from-minibuffer-function 
        \(function \(lambda \(\) skk-henkan-key\)\) \)
と指定する。"
  :type 'function
  :group 'skk )

(defvar skk-latin-mode-map nil "*ASCII モードのキーマップ。")
(or skk-latin-mode-map 
    (let ((map (make-sparse-keymap)))
      ;; .skk で skk-kakutei-key の変更が可能になるように。
      ;;(define-key map skk-kakutei-key 'skk-kakutei)
      (skk-define-menu-bar-map map)
      (setq skk-latin-mode-map map) ))

(defvar skk-j-mode-map nil "*かなモードのキーマップ。")
(or skk-j-mode-map
    (let ((map (make-sparse-keymap)))
      (substitute-key-definition 'self-insert-command 'skk-insert map
				 global-map)
      ;; for Mule-2.x
      (substitute-key-definition 'egg-self-insert-command 'skk-insert map
				 global-map)
      (substitute-key-definition 'canna-self-insert-command 'skk-insert map
				 global-map)
      (substitute-key-definition 'can-n-egg-self-insert-command 'skk-insert map
				 global-map)
      ;; .skk で skk-kakutei-key の変更が可能になるように。
      ;;(define-key map skk-kakutei-key 'skk-kakutei)
      (skk-define-menu-bar-map map)
      (setq skk-j-mode-map map) ))

(defvar skk-jisx0208-latin-mode-map nil "*全角モードのキーマップ。")
(or skk-jisx0208-latin-mode-map 
    (let ((map (make-sparse-keymap))
	  (i 0) )
      (while (< i 128)
	(and (aref skk-jisx0208-latin-vector i)
	     (define-key map (char-to-string i) 'skk-jisx0208-latin-insert) )
	(setq i (1+ i)) )
      (define-key map "\C-q" 'skk-latin-henkan)
      (skk-define-menu-bar-map map)
      (setq skk-jisx0208-latin-mode-map map) ))

(defvar skk-abbrev-mode-map nil "*SKK abbrev モードのキーマップ。")
(or skk-abbrev-mode-map 
    (let ((map (make-sparse-keymap)))
      (define-key map "," 'skk-abbrev-comma)
      (define-key map "." 'skk-abbrev-period)
      (define-key map "\C-q" 'skk-jisx0208-latin-henkan)
      ;; .skk で skk-kakutei-key の変更が可能になるように。
      ;;(define-key map skk-kakutei-key 'skk-kakutei)
      (skk-define-menu-bar-map map)
      (setq skk-abbrev-mode-map map) ))

;;; -- internal constants and variables
;; ---- global ones.
;;(defvar skk-henkan-face 'skk-henkan-face)
(defconst skk-month-alist
  '(("Jan" . "1") ("Feb" . "2") ("Mar" . "3") ("Apr" . "4") ("May" . "5")
    ("Jun" . "6") ("Jul" . "7") ("Aug" . "8") ("Sep" . "9") ("Oct" . "10")
    ("Nov" . "11") ("Dec" . "12") )
  "英語の月名と算用数字の連想リスト。

算用数字から英語の月名のみを出力するのであれば、ベクターを使った方が高速だが、
英語の月名から算用数字を出力するのであれば連想リストでなければ無理なので、多
目的に使用できるよう連想リストの形態を取る。"
  ;;  "Alist of English month abbreviations and numerical values.
  ;;
  ;;Although it is faster to use a vector if we only want to output
  ;;month abbreviations given the ordinal, without the alist it's
  ;;unreasonable [sic] to output the ordinal given the abbreviation,
  ;;so for multi-purpose utility we use the alist form."
  )

(defconst skk-coding-system-alist
  (if (memq skk-emacs-type '(xemacs mule4 mule3))
      '(("euc" . euc-japan)
        ("ujis" . euc-japan)
        ("sjis". sjis)
        ("jis" . junet) )
    '(("euc" . *euc-japan*)
      ("ujis" . *euc-japan*)
      ("sjis". *sjis*)
      ("jis" . *junet*) ))
  "coding-system の文字列表現と、シンボル表現の連想リスト。" )

(defconst skk-default-jisx0208-latin-vector
  ;; note that skk-jisx0208-latin-vector is a user variable.
  ;; skk.el ロード前に .emacs などで、skk-jisx0208-latin-vector の別の値をユー
  ;; ザーが直接書いたり、skk.el ロード後にこの値を aset で直接いじったりしな
  ;; ければ default-value で skk-jisx0208-latin-vector にアクセスすることで
  ;; skk-default-jisx0208-latin-vector の値を保持することもできようが、それは
  ;; 望めない...。
  [nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "　"  "！" "”" "＃" "＄" "％" "＆" "’"
   "（" "）" "＊" "＋" "，" "−" "．" "／"
   "０" "１" "２" "３" "４" "５" "６" "７"
   "８" "９" "：" "；" "＜" "＝" "＞" "？"
   "＠" "Ａ" "Ｂ" "Ｃ" "Ｄ" "Ｅ" "Ｆ" "Ｇ"
   "Ｈ" "Ｉ" "Ｊ" "Ｋ" "Ｌ" "Ｍ" "Ｎ" "Ｏ"
   "Ｐ" "Ｑ" "Ｒ" "Ｓ" "Ｔ" "Ｕ" "Ｖ" "Ｗ"
   "Ｘ" "Ｙ" "Ｚ" "［" "＼" "］" "＾" "＿"
   "‘" "ａ" "ｂ" "ｃ" "ｄ" "ｅ" "ｆ" "ｇ"
   "ｈ" "ｉ" "ｊ" "ｋ" "ｌ" "ｍ" "ｎ" "ｏ"
   "ｐ" "ｑ" "ｒ" "ｓ" "ｔ" "ｕ" "ｖ" "ｗ"
   "ｘ" "ｙ" "ｚ" "｛" "｜" "｝" "〜" nil]
  "skk-jisx0208-latin-region で参照する文字テーブル。
\"ascii\" -> \"ａｓｃｉｉ\" のような全角文字への変換を行う際に利用する。" )

(defconst skk-kanji-len (length "あ")
  "漢字一文字の長さ。Mule[1-3] では 3 になる。Mule4, XEmacs では 1。" )

(defconst skk-hankaku-alist
  (if (eq skk-emacs-type 'mule2)
      '((161 . 32)	; ?\ 
	(170 . 33)	;?\!
	(201 . 34)	;?\"
	(244 . 35)	;?\#
	(240 . 36)	;?\$
	(243 . 37)	;?\%
	(245 . 38)	;?\&
	(199 . 39)	;?\'
	(202 . 40)	;?\(
	(203 . 41)	;?\)
	(246 . 42)	;?\*
	(220 . 43)	;?\+
	(164 . 44)	;?\,
	(221 . 45)	;?\-
	(165 . 46)	;?\.
	(191 . 47)	;?\/
	(167 . 58)	;?\:
	(168 . 59)	;?\;
	(227 . 60)	;?\<
	(225 . 61)	;?\=
	(228 . 62)	;?\>
	(169 . 63)	;?\?
	(247 . 64)	;?\@
	(206 . 91)	;?\[
	(239 . 92)	;?\\
	(207 . 93)	;?\]
	(176 . 94)	;?^ 
	(178 . 95)	;?\_
	(208 . 123)	;?\{
	(195 . 124)	;?\|
	(209 . 125)	;?\}
	(177 . 126)	;?\~
	(198 . 96) ))	;?` 
  "文字コードの 2 番目のバイトとその文字に対応する ascii 文字 \(char\) との連想リスト。
Mule l もしくは  Mule 2 を使用する場合に skk-latin-region で参照する。
Mule-2.3 添付の egg.el よりコピーした。" )

(defconst skk-kana-cleanup-command-list
  '(skk-delete-backward-char skk-insert skk-previous-candidate) )

(defvar skk-rule-tree nil
  "ローマ字 -> かな変換の状態遷移規則を表すツリーの初期状態。
skk-mode の起動時に毎回 skk-rom-kana-base-rule-list と
skk-rom-kana-rule-list から木の形にコンパイルされる。" )

(defvar skk-insert-new-word-function nil
  "候補を挿入したときに funcall される関数を保存する変数。" )

(skk-deflocalvar skk-input-mode-string skk-hiragana-mode-string
  "SKK の入力モードを示す文字列。skk-mode 起動時は、skk-hiragana-mode-string。" )

(defvar skk-isearch-message nil
  "skk-isearch 関数をコールするためのフラグ。
Non-nil であれば、skk-isearch-message 関数をコールする。" )

(defvar skk-mode-invoked nil
  "Non-nil であれば、Emacs を起動後既に skk-mode を起動したことを示す。" )

(defvar skk-kakutei-count 0
  "変換候補を確定したカウントを保持する変数。
skk-record-file の \"確定:\" 項目のカウンター。" )

(defvar skk-touroku-count 0
  "辞書登録したカウントを保持する変数。
skk-record-file の \"登録:\" 項目のカウンター。" )

(defvar skk-update-jisyo-count 0
  "辞書を更新した回数。
このカウンターの数字が skk-jisyo-save-count 以上となったときにユーザー辞書のオー
トセーブが行なわれる。
辞書のセーブが行なわれるとイニシャライズされる。" )

(defvar skk-minibuffer-origin-mode nil
  "入力モードを表わすシンボル。
有効な値は、`hiragana', `katakana', `abbrev', `latin', `jisx0208-latin' もしくは
nil のいずれか。" )

;; ---- buffer local variables
;; <フラグ類>
;;(skk-deflocalvar skk-current-henkan-data
;;  '(
;;    ;; global variables
;;    ;; バッファローカル変数のディフォルト値を設定すると、これを直接書換えしたと
;;    ;; きに他のバッファから見える値も変わってしまう。global なフラグはこれを利
;;    ;; 用してディフォルト値与えておく。
;;    (invoked . nil) ; Emacs を起動後既に skk-mode を起動したことを示す
;;    (isearch-message . nil) ; skk-isearch 関数をコールするためのフラグ
;;    (kakutei-count . 0) ; 変換候補を確定したカウントを保持する変数
;;    (minibuffer-origin-mode . nil) ;入力モードを表わすシンボル
;;    (touroku-count . 0) ; 辞書登録したカウントを保持する変数
;;    (update-jisyo-count . 0) ; 辞書を更新した回数
;;    ;; buffer-local variables.
;;    ;;(current-search-prog-list . nil) ;skk-search-prog-list の現在の値を保存するリスト
;;    ;;(exit-show-candidates . nil) ;ミニバッファで候補を次々に表示して、候補が尽きたことを示す
;;    ;;(henkan-active . nil) ; ▼モード (変換中) であることを示す
;;    ;;(henkan-count . -1) ;skk-henkan-list のリストのインデクスで現在の候補を差すもの
;;    ;;(henkan-end-point . nil ) ; 変換終了ポイントを示すマーカー
;;    ;;(henkan-in-minibuff-flag . nil) ;ミニバッファで辞書登録を行ったときにこのフラグが立つ
;;    ;;(henkan-key . nil) ;変換すべき見出し語
;;    ;;(henkan-list . nil) ; 変換結果の候補のリスト
;;    ;;(henkan-okurigana . nil) ;現在の変換の送り仮名部分
;;    ;;(henkan-on . nil) ; ▽モード (変換対象の文字列決定のためのモード) であることを示す
;;    ;;(henkan-start-point . nil) ; 変換開始ポイントを示すマーカー
;;    ;;(kakutei-flag . nil) ; 確定して良い候補を見つけた状態であることを指す
;;    ;;(kana-start-point . nil) ;かな文字の開始ポイントを示すマーカー
;;    ;;(katakana . nil) ; 入力モードがカナモードであることを示す
;;    ;;(okuri-ari-max . nil) ; 辞書の送り有りエントリの終了点を示すバッファポイント
;;    ;;(okuri-ari-min . nil) ; 辞書の送り有りエントリの開始点を示すバッファポイント
;;    ;;(okuri-char . nil) ;変換すべき語の送り仮名の部分のプレフィックス
;;    ;;(okuri-index-max . -1) ;skk-henkan-list のインデクスで自動送り処理、もしくはサ変検索で検索した最後の候補を指すもの
;;    ;;(okuri-index-min . -1) ;skk-henkan-list のインデクスで自動送り処理、もしくはサ変検索で検索した最初の候補を指すもの
;;    ;;(okuri-nasi-min . nil) ; 辞書の送りなしエントリの開始点を示すバッファポイント
;;    ;;(okurigana . nil) ; 送り仮名部分が入力中であることを示す
;;    ;;(okurigana-start-point . nil) ; 送り仮名の開始ポイントを示すマーカー
;;    ;;(prefix . "") ; 入力するかなを決定するためのプレフィックス
;;    ;;(previous-point . nil) ;この変数に保持されるポイントが現在のポイントと異なる場合、skk-with-point-move が使われていないコマンドを動作させると、skk-after-point-move が作動する
;;    ;;(self-insert-non-undo-count . 1) ;skk-insert もしくは skk-jisx0208-latin-insert で連続入力した文字数を表わすカウンター
;;    ))

(skk-deflocalvar skk-mode nil
  "Non-nil であれば、カレントバッファで現在 skk-mode を起動していることを示す。" )

(skk-deflocalvar skk-latin-mode nil
  "Non-nil であれば、入力モードが ASCII モードであることを示す。" )

(skk-deflocalvar skk-j-mode nil
  "Non-nil であれば、入力モードがかな・カナモードであることを示す。" )

(skk-deflocalvar skk-katakana nil
  "Non-nil であれば、入力モードがカナモードであることを示す。
\"(and (not skk-katakana) skk-j-mode))\" が t であれば、かなモードであることを
示す。" )

(skk-deflocalvar skk-jisx0208-latin-mode nil
  "Non-nil であれば、入力モードが全英モードであることを示す。" )

(skk-deflocalvar skk-abbrev-mode nil
  "Non-nil であれば、入力モードが SKK abbrev モードであることを示す。" )

(skk-deflocalvar skk-okurigana nil
  "Non-nil であれば、送り仮名部分が入力中であることを示す。" )

(skk-deflocalvar skk-henkan-on nil
  "Non-nil であれば、▽モード \(変換対象の文字列決定のためのモード\) であることを示す。" )

(skk-deflocalvar skk-henkan-active nil
  "Non-nil であれば、▼モード \(変換中\) であることを示す。" )

(skk-deflocalvar skk-kakutei-flag nil
  "Non-nil なら確定して良い候補を見つけた状態であることを指す。
skk-henkan, skk-search-kakutei-jisyo-file, skk-henkan-show-candidates,
skk-henkan-in-minibuff と skk-kakutei-save-and-init-variables で変更、参照され
る。" )

(skk-deflocalvar skk-exit-show-candidates nil
  "ミニバッファで候補を次々に表示して、候補が尽きたときに non-nil となる。
その値はリストで、car に skk-henkan-show-candidate 関数で while ループを回っ
た回数を示す一時変数 loop の値を、cdr 部に最後にミニバッファに表示した 1 つ前
の候補群の最後の要素を指すインデクスが代入される。
skk-henkan-show-candidates, skk-henkan-in-minibuff と
skk-kakutei-save-and-init-variables で変更、参照される。" )

;; <キーマップ関連>
(skk-deflocalvar skk-current-rule-tree nil
  "ローマ字 -> かな変換の状態遷移規則を表わすツリーの現時点の状態。
ローマ字入力の初期では skk-rule-tree と同一の状態で、文字入力が進むに
つれ、木をたどってゆく状態の遷移を表す。" )

;; <辞書関連の変数>
(skk-deflocalvar skk-okuri-ari-min nil
  "SKK 辞書の送り有りエントリの開始点を示すバッファポイント。")

(skk-deflocalvar skk-okuri-ari-max nil
  "SKK 辞書の送り有りエントリの終了点を示すバッファポイント。
skk-jisyo のバッファでは辞書の更新の必要があるためにマーカーが代入される。" )

(skk-deflocalvar skk-okuri-nasi-min nil
  "SKK 辞書の送りなしエントリの開始点を示すバッファポイント。
skk-jisyo のバッファでは辞書の更新の必要があるためにマーカーが代入される。" )

;; <その他>
(skk-deflocalvar skk-mode-line nil
  "SKK のモードを示すモードラインの文字列。
skk-mode-string, skk-hiragana-mode-string, skk-katakana-mode-string
and skk-jisx0208-latin-mode-string のいずれかが代入される。" )

(skk-deflocalvar skk-previous-point nil
  "skk-with-point-move 関連変数。
この変数に保持されるポイントが現在のポイントと異なる場合、skk-with-point-move が
使われていないコマンドを動作させると、skk-after-point-move が作動する。" )

;; "" に対応したエントリが skk-roma-kana-[aiue] にあるため、"" を nil で代用
;; できない。
(skk-deflocalvar skk-prefix ""
  "入力するかなを決定するためのプレフィックス。" )

(skk-deflocalvar skk-henkan-start-point nil
  "変換開始ポイントを示すマーカー。" )

(skk-deflocalvar skk-henkan-end-point nil
  "変換終了ポイントを示すマーカー。" )

(skk-deflocalvar skk-kana-start-point nil
  "かな文字の開始ポイントを示すマーカー。" )

(skk-deflocalvar skk-okurigana-start-point nil
  "送り仮名の開始ポイントを示すマーカー。" )

(skk-deflocalvar skk-henkan-key nil
  "変換すべき見出し語。
例えば、\"▽かな\" を変換すれば、skk-henkan-key には \"かな\" が代入される。
\"▽わら*う\" のような送りありの変換の場合には、\"わらu\" のように、漢字部分の
読みがな + 送り仮名の最初の文字のローマ字のプレフィックスが代入される。" )

(skk-deflocalvar skk-okuri-char nil
  "変換すべき語の送り仮名の部分のプレフィックス。
例えば、\"おく*り\" を変換するときは、skk-okuri-char は \"r\"。
skk-okuri-char が non-nil であれば、送りありの変換であることを示す。" )

(skk-deflocalvar skk-henkan-okurigana nil
  "現在の変換の送り仮名部分。
例えば、\"▽うまれ*る\" を変換すれば、skk-henkan-okurigana には \"る\" が代入
される。" )

(skk-deflocalvar skk-last-kakutei-henkan-key nil
  "確定辞書により最後に確定したときの見出し語。
確定辞書による確定の直後に x キーを押すと確定がアンドゥされて、確定前の状態で
この見出し語がカレントバッファに挿入される。" )

(skk-deflocalvar skk-henkan-list nil
  "変換結果の候補のリスト。
例えば、\"▽な*く\" という変換すれば、skk-henkan-list は
(\"鳴\" \"泣\" \"無\" \"亡\") のようになる。" )

(skk-deflocalvar skk-henkan-count -1
  "skk-henkan-list のリストのインデクスで現在の候補を差すもの。" )

(skk-deflocalvar skk-self-insert-non-undo-count 1
  "skk-insert もしくは skk-jisx0208-latin-insert で連続入力した文字数を表わすカウンター。
Emacs のオリジナルの動作では、self-insert-command にバインドされたキー入力は、
連続 20 回までが 1 つのアンドゥの対象となる。この動作をエミュレートするための
カウンター。このカウンターが、20 以下であるときは、入力のたびに 
cancel-undo-boundary がコールされる。" )

(skk-deflocalvar skk-current-search-prog-list nil
  "skk-search-prog-list の現在の値を保存するリスト。
最初の変換時は skk-search-prog-list の全ての値を保持し、変換を繰り返すたびに 1
つづつ短くなってゆく。" )
  
;; for skk-undo-kakutei
(skk-deflocalvar skk-last-henkan-data nil
  "最後に行なった変換に関するデータのエーリスト。
ディフォルトのキーは、henkan-key, henkan-okurigana,
okuri-char, henkan-list の各シンボル。
\(skk-num を require しているときは、num-list が追加される\)。" )

(skk-deflocalvar skk-henkan-overlay nil
  "候補を表示するときに使用する Overlay。" )

(skk-deflocalvar skk-henkan-in-minibuff-flag nil
  "ミニバッファで辞書登録を行ったときにこのフラグが立つ。
skk-remove-common で参照される。" )

(skk-deflocalvar skk-okuri-index-min -1
  "skk-henkan-list のインデクスで自動送り処理、もしくはサ変検索で検索した最初の候補を指すもの。" )

(skk-deflocalvar skk-okuri-index-max -1
  "skk-henkan-list のインデクスで自動送り処理、もしくはサ変検索で検索した最後の候補を指すもの。" )

(set-modified-alist
 'minor-mode-map-alist
 (list (cons 'skk-latin-mode skk-latin-mode-map)
       (cons 'skk-abbrev-mode skk-abbrev-mode-map)
       (cons 'skk-j-mode skk-j-mode-map)
       (cons 'skk-jisx0208-latin-mode skk-jisx0208-latin-mode-map) ))

;;;; defadvices.
;; defadvice で定義すると、後でユーザーが新規の機能を付けて更に defadvice して
;; もちゃんと動く。

;; cover to original functions.

(defadvice keyboard-quit (around skk-ad activate)
  "▼モードであれば、候補の表示をやめて▽モードに戻す (見出し語は残す)。
▽モードであれば、見出し語を削除する。
上記のどちらのモードでもなければ keyboard-quit と同じ動作をする。"
  (cond 
   ;; SKK is not invoked in the current buffer.
   ((not skk-mode) ad-do-it)
   ;; ■ mode (Kakutei input mode).
   ((not skk-henkan-on)
    (cond ((skk-get-prefix skk-current-rule-tree)
	   (skk-erase-prefix 'clean) )
	  (t ad-do-it) ))
   ;; ▼ mode (Conversion mode).
   (skk-henkan-active
    (setq skk-henkan-count 0)
    (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
	(let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
	  (skk-previous-candidate)
	  ;; ここでは delete-backward-char に第二引数を渡さない方がベター？
	  (delete-backward-char count) )
      (skk-previous-candidate) ))
   ;; ▽ mode (Midashi imput mode).
   (t (skk-erase-prefix 'clean)
      (and (> (point) skk-henkan-start-point)
	   (delete-region (point) skk-henkan-start-point) )
      (skk-kakutei) )))

(defadvice abort-recursive-edit (around skk-ad activate)
  "▼モードであれば、候補の表示をやめて▽モードに戻す (見出し語は残す)。
▽モードであれば、見出し語を削除する。
上記のどちらのモードでもなければ abort-recursive-edit と同じ動作をする。"
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer
   (function (lambda () (add-hook 'pre-command-hook 'skk-pre-command nil 'local))) )
  (cond ((not skk-mode) ad-do-it)
	((not skk-henkan-on)
	 (cond ((skk-get-prefix skk-current-rule-tree)
		(skk-erase-prefix 'clean) )
	       (t ad-do-it) ))
        (skk-henkan-active
         (setq skk-henkan-count 0)
         (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
             (let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
               (skk-previous-candidate)
               ;; ここでは delete-backward-char に第二引数を渡さない方がベター？
               (delete-backward-char count) )
           (skk-previous-candidate) ))
	(t (skk-erase-prefix 'clean)
	   (and (> (point) skk-henkan-start-point)
		(delete-region (point) skk-henkan-start-point) )
	   (skk-kakutei) )))
	
(defadvice newline (around skk-ad activate)
  "skk-egg-like-newline が non-nil だったら、変換中の newline で確定のみ行い、改行しない。"
  (if (not (or skk-j-mode skk-abbrev-mode))
      ad-do-it
    (let (
	  ;;(arg (ad-get-arg 0))
          ;; skk-kakutei を実行すると skk-henkan-on の値が無条件に nil になる
          ;; ので、保存しておく必要がある。
          (no-newline (and skk-egg-like-newline skk-henkan-on))
	  (auto-fill-function (and (interactive-p) auto-fill-function)) )
      ;; fill されても nil が帰ってくる :-<
      ;;(if (skk-kakutei)
      ;;    (setq arg (1- arg)) )
      ;;(if skk-mode
      ;;    (let ((opos (point)))
      ;;      ;; skk-kakutei (skk-do-auto-fill) によって行が折り返されたら arg を 1 つ減らす。
      ;;      (skk-kakutei)
      ;;      (if (and (not (= opos (point))) (integerp arg))
      ;;          (ad-set-arg 0 (1- arg)) )))
      (and skk-mode (skk-kakutei))
      (if (not no-newline)
	  ad-do-it ))))

(defadvice newline-and-indent (around skk-ad activate)
  "skk-egg-like-newline が non-nil だったら、変換中の newline-and-indent で確定のみ行い、改行しない。"
  (if (not (or skk-j-mode skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline skk-henkan-on))
	  (auto-fill-function (and (interactive-p) auto-fill-function)) )
      (and skk-mode (skk-kakutei))
      (or no-newline ad-do-it) )))

(defadvice exit-minibuffer (around skk-ad activate)
  "skk-egg-like-newline が non-nil だったら、変換中の exit-minibuffer で確定のみ行う。"
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer
   (function (lambda ()
	       (add-hook 'pre-command-hook 'skk-pre-command nil 'local) )))
  (if (not (or skk-j-mode skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline skk-henkan-on)))
      (and skk-mode (skk-kakutei))
      (or no-newline ad-do-it) )))

(defadvice picture-mode-exit (before skk-ad activate)
  "SKK のバッファローカル変数を無効にし、picture-mode-exit をコールする。
picture-mode から出たときにそのバッファで SKK を正常に動かすための処理。"
  (and skk-mode (skk-kill-local-variables)) )

(defadvice undo (before skk-ad activate)
  "SKK モードが on なら skk-self-insert-non-undo-count を初期化する。"
  (and skk-mode (setq skk-self-insert-non-undo-count 0)) )

(defadvice kill-buffer (before skk-ad activate)
  "SKK の▼モードだったら、確定してからバッファをキルする。
  バッファのキル後、SKK のモードに従いカーソルの色を変える。"
  (and skk-mode skk-henkan-on (interactive-p) (skk-kakutei)) )

(defadvice save-buffers-kill-emacs (before skk-ad activate)
  (run-hooks 'skk-before-kill-emacs-hook) )

(if (eq skk-emacs-type 'xemacs)
    ;; XEmacs has minibuffer-keyboard-quit that has nothing to do with delsel.
    (defadvice minibuffer-keyboard-quit (around skk-ad activate)
      (skk-remove-minibuffer-setup-hook
       'skk-j-mode-on 'skk-setup-minibuffer
       (function (lambda ()
		   (add-hook 'pre-command-hook 'skk-pre-command nil 'local) )))
      (cond ((not skk-mode) ad-do-it)
	    ((not skk-henkan-on)
	     (cond ((skk-get-prefix skk-current-rule-tree)
		    (skk-erase-prefix 'clean) )
		    (t ad-do-it) ))
            (skk-henkan-active
             (setq skk-henkan-count 0)
             (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
                 (let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
                   (skk-previous-candidate)
                   ;; ここでは delete-backward-char に第二引数を渡さない方がベター？
                   (delete-backward-char count) )
               (skk-previous-candidate) ))
            (t (skk-erase-prefix 'clean)
	       (and (> (point) skk-henkan-start-point)
		    (delete-region (point) skk-henkan-start-point) )
               (skk-kakutei) )))
  (defadvice minibuffer-keyboard-quit (around skk-ad activate)
    ;; for delsel.el
    (if (and skk-mode
	     (not (and delete-selection-mode transient-mark-mode mark-active)) )
	(keyboard-quit)
      ad-do-it )))

;;;; mode setup

;;;###autoload
(defun skk-mode (&optional arg)
  "日本語入力モード。
マイナーモードの一種で、オリジナルのモードには影響を与えない。
負の引数を与えると SKK モードから抜ける。

An input mode for Japanese, converting romanized phonetic strings to kanji.

A minor mode, it should not affect the use of any major mode or
orthogonal minor modes.

In the initial SKK mode, hiragana submode, the mode line indicator is 
\"かな\".  Lowercase romaji entry is automatically converted to
hiragana where possible.  The lowercase characters `q' and `l' change
submodes of SKK, and `x' is used as a prefix indicating a small kana.

`q' is used to toggle between hiragana and katakana \(mode line
indicator \"カナ\"\) entry submodes.

`l' is used to enter ASCII submode \(mode line indicator \"SKK\"\).
Uppercase `L' enters JISX0208 latin \(wide ASCII\) submode \(mode line
indicator \"全英\"\).  `\C-j' returns to hiragana submode from either
ASCII submode.

Kanji conversion is complex, but the basic principle is that the user
signals the appropriate stem to be matched against dictionary keys by
the use of uppercase letters.  Because SKK does not use grammatical
information, both the beginning and the end of the stem must be marked.

For non-inflected words \(eg, nouns\) consisting entirely of kanji, the
simplest way to invoke conversion is to enter the reading of the kanji,
the first character only in uppercase.  A leading \"▽\" indicates that
kanji conversion is in progress.  After entering the reading, press 
space.  This invokes dictionary lookup, and the hiragana reading will be
redisplayed in kanji as the first candidate.  Pressing space again gives
the next candidate.  Further presses of space produce further candidates,
as well as a list of the next few candidates in the minibuffer.  Eg,
\"Benri\" => \"▽べんり\", and pressing space produces \"▼便利\" \(the solid 
triangle indicates that conversion is in progress\).  Backspace steps 
through the candidate list in reverse.

A candidate can be accepted by pressing `\C-j', or by entering a
self-inserting character.  \(Unlike other common Japanese input methods,
RET not only accepts the current candidate, but also inserts a line
break.\)

Inflected words \(verbs and adjectives\), like non-inflected words, begin
entry with a capital letter.  However, for these words the end of the
kanji string is signaled by capitalizing the next mora.  Eg, \"TuyoI\"
=> \"▼強い\".  If no candidate is available at that point, the inflection
point will be indicated with an asterisk \"*\", and trailing characters
will be displayed until a candidate is recognized.  It will be
immediately displayed \(pressing space is not necessary\).  Space and
backspace are used to step forward and backward through the list of 
candidates.

For more information, see the `skk' topic in Info.  \(Japanese only.\)

A tutorial is available in Japanese or English via \"M-x skk-tutorial\".
Use a prefix argument to choose the language.  The default is system-
dependent."
  (interactive "P")
  (setq skk-mode (cond ((null arg) (not skk-mode))
                       ;; - は -1 に変換される。
                       ((> (prefix-numeric-value arg) 0) t) ))
  (if (not skk-mode)
      ;; exit skk-mode
      (progn
        (let ((skk-mode t)) (skk-kakutei))
        (skk-mode-off) 
	(and (eq skk-status-indicator 'left)
	     (setq skk-input-mode-string "") )
	(and (eq skk-emacs-type 'xemacs) (easy-menu-remove skk-menu)) )
    ;; enter skk-mode
    (if (not skk-mode-invoked)
        ;; enter skk-mode for the first time in this session
        (progn
	  (and (eq skk-emacs-type 'xemacs)
	       (boundp 'preloaded-file-list)
	       (member "skk-leim" preloaded-file-list)
	       ;; require dummy file.
	       (require 'skk-vars) )
          (skk-setup-init-file)
          (load skk-init-file t)
	  (skk-setup-modeline)
	  (require 'skk-autoloads)
	  (if (or (memq skk-emacs-type '(mule3 mule4))
		  (and (eq skk-emacs-type 'xemacs)
		       (or 
			;; XEmacs 21 or later.
			(> emacs-major-version 20)
			;; XEmacs 20.4 or later.
			(> emacs-minor-version 2) )))
	      (require 'skk-leim) )
	  (if skk-use-numeric-conversion (require 'skk-num))
          (if skk-keep-record
	      (skk-create-file skk-record-file
			       "SKK の記録用ファイルを作りました"
			       "I have created an SKK record file for you" ))
	  (skk-create-file skk-jisyo
			   "SKK の空辞書を作りました"
			   "I have created an empty SKK Jisyo file for you" )
	  (skk-regularize)
          (setq skk-mode-invoked t) ))
    ;; 以下は skk-mode に入るたびに毎度コールされるコード。
    (and skk-use-viper (require 'skk-viper))
    (and (or skk-use-color-cursor skk-use-cursor-change)
	 (require 'skk-cursor) )
    ;; .skk で skk-kakutei-key の変更が可能になるように。
    (define-key skk-abbrev-mode-map skk-kakutei-key 'skk-kakutei)
    (define-key skk-abbrev-mode-map (char-to-string skk-start-henkan-char)
      'skk-start-henkan )
    (define-key skk-abbrev-mode-map (char-to-string skk-try-completion-char)
      'skk-try-completion )
    (define-key skk-latin-mode-map skk-kakutei-key 'skk-kakutei)
    (define-key skk-j-mode-map skk-kakutei-key 'skk-kakutei)
    (define-key skk-j-mode-map (char-to-string skk-try-completion-char)
      'skk-insert )
    (define-key skk-j-mode-map (char-to-string skk-previous-candidate-char)
      'skk-previous-candidate )
    (define-key skk-jisx0208-latin-mode-map skk-kakutei-key 'skk-kakutei)
    (define-key minibuffer-local-map skk-kakutei-key 'skk-kakutei)
    (define-key minibuffer-local-completion-map skk-kakutei-key 'skk-kakutei)
    (if skk-use-viper
	()
      (define-key skk-j-mode-map
	(char-to-string skk-start-henkan-with-completion-char)
	'skk-start-henkan-with-completion)
      (define-key skk-abbrev-mode-map
	(char-to-string skk-start-henkan-with-completion-char)
 	'skk-start-henkan-with-completion)
      (define-key skk-j-mode-map
 	(char-to-string skk-backward-and-set-henkan-point-char)
 	'skk-backward-and-set-henkan-point) 
      (define-key skk-jisx0208-latin-mode-map
 	(char-to-string skk-backward-and-set-henkan-point-char)
 	'skk-backward-and-set-henkan-point) 
      )
    (skk-setup-delete-backward-char)
    ;; XEmacs doesn't have minibuffer-local-ns-map
    (and (boundp 'minibuffer-local-ns-map)
	 (define-key minibuffer-local-ns-map skk-kakutei-key 'skk-kakutei) )
    ;; To terminate kana input.
    (make-local-hook 'pre-command-hook)
    (add-hook 'pre-command-hook 'skk-pre-command nil 'local)
    (make-local-hook 'post-command-hook)
    (add-hook 'post-command-hook 'skk-after-point-move nil 'local)
    (and (eq skk-status-indicator 'left)
	 (setq skk-input-mode-string skk-hiragana-mode-string) )
    (skk-j-mode-on)
    (and (eq skk-emacs-type 'xemacs) (easy-menu-add skk-menu))
    (run-hooks 'skk-mode-hook) ))

;;;###autoload
(defun skk-auto-fill-mode (&optional arg)
  "日本語入力モード。自動折り返し機能付き。
マイナーモードの一種で、オリジナルのモードには影響を与えない。
正の引数を与えると、強制的に auto-fill-mode 及び SKK モードに入る。
負の引数を与えると auto-fill-mode 及び SKK モードから抜ける。"
  (interactive "P")
  (let ((auto-fill
         (cond ((null arg) (not auto-fill-function))
               ((> (prefix-numeric-value arg) 0) t) )))
    (auto-fill-mode (if auto-fill 1 -1))
    (skk-mode arg)
    (run-hooks 'skk-auto-fill-mode-hook) ))

(defun skk-kill-emacs-without-saving-jisyo (&optional query)
  "SKK 辞書をセーブしないで、Emacs を終了する。"
  (interactive "P")
  ;; format を引数に持たせた場合は、skk-yes-or-no-p を使うとかえって冗長になる。
  (if (yes-or-no-p
       (format (if skk-japanese-message-and-error
                   "辞書の保存をせずに %s を終了します。良いですか？"
                 "Do you really wish to kill %s without saving Jisyo? " )
               (cond ((eq skk-emacs-type 'xemacs) "XEmacs")
		     (t "Mule") )))
      (let ((buff (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
	(ad-disable-advice 'save-buffers-kill-emacs 'before 'skk-ad)
	(ad-activate 'save-buffers-kill-emacs)
	(remove-hook 'skk-before-kill-emacs-hook 'skk-save-jisyo) ; fail safe.
	(if buff
	    (progn (set-buffer buff)
		   (set-buffer-modified-p nil)
		   (kill-buffer buff) ))
	(save-buffers-kill-emacs query) )))

(defun skk-restart ()
  "skk-init-file の再ロード及び各種再セットアップの後、SKK モードを起動する。"
  (interactive)
  (let (skk-mode-invoked) (skk-mode 1)) )

(defun skk-regularize ()
  ;; SKK の動作の正規化を図るため、内部変数やユーザー変数の調整を行なう。
  (skk-setup-auto-paren)
  (setq skk-rule-tree
	(skk-compile-rule-list skk-rom-kana-base-rule-list skk-rom-kana-rule-list) )
  (and (not (featurep 'skk-server))
       (or (and (boundp 'skk-servers-list) skk-servers-list)
	   (or (and (boundp 'skk-server-host) skk-server-host)
	       (getenv "SKKSERVER") ))
       (require 'skk-server) )
  (and (featurep 'skk-server)
       ;; skk-search-server はサーバーが落ちても使えるので、外さない。
       (skk-adjust-search-prog-list-for-server-search 'non-del) )
  (and skk-auto-okuri-process (skk-adjust-search-prog-list-for-auto-okuri))
  (and skk-use-look (require 'skk-look))
  (skk-setup-delete-selection-mode)
  (skk-adjust-user-option) )

(defun skk-setup-delete-backward-char ()
  (let ((commands '(backward-delete-char-untabify
		    backward-delete-char
		    backward-or-forward-delete-char 
		    delete-backward-char
		    picture-backward-clear-column
		    ;; following two are SKK adviced.
		    ;;viper-del-backward-char-in-insert
		    ;;vip-del-backward-char-in-insert
		    ))
	keys )
    (while commands
      (setq keys (where-is-internal (car commands) overriding-local-map)
	    commands (cdr commands) )
      (while keys
	(define-key skk-abbrev-mode-map (car keys) 'skk-delete-backward-char)
	(define-key skk-j-mode-map (car keys) 'skk-delete-backward-char)
	(setq keys (cdr keys)) ))))

(defun skk-setup-init-file ()
  ;; skk-byte-compile-init-file が non-nil の場合で、skk-init-file をバイトコ
  ;; ンパイルしたファイルが存在しないか、そのバイトコンパイル済ファイルより 
  ;; skk-init-file の方が新しいときは、skk-init-file をバイトコンパイルする。
  ;;
  ;; skk-byte-compile-init-file が nil の場合で、skk-init-file をバイトコンパ
  ;; イルしたファイルより skk-init-file の方が新しいときは、そのバイトコンパイ
  ;; ル済ファイルを消す。
  (save-match-data
    (let* ((init-file (expand-file-name skk-init-file))
           (elc (concat init-file 
                        (if (string-match "\\.el$" init-file)
                            "c"
                          ".elc" ))))
      (if skk-byte-compile-init-file
          (and (file-exists-p init-file)
	       (or (not (file-exists-p elc))
		   (file-newer-than-file-p init-file elc) )
	       (save-window-excursion ; for keep window configuration.
		 (skk-message "%s をバイトコンパイルします。" "Byte-compile %s"
			      skk-init-file )
		 (sit-for 2)
		 (byte-compile-file init-file) ))
        (and (file-exists-p init-file)
	     (file-exists-p elc)
	     (file-newer-than-file-p init-file elc)
	     (delete-file elc) )))))

(defun skk-emulate-original-map (arg)
  ;; キー入力に対して、SKK のモードではなく、Emacs のオリジナルのキー割り付けで
  ;; コマンドを実行する。
  (let ((prefix-arg arg)
        (keys (skk-command-key-sequence (this-command-keys) this-command)) )
    (if (not keys)
        ;; no alternative commands.  may be invoked by M-x.
        nil
      (let (skk-mode skk-latin-mode skk-j-mode skk-abbrev-mode skk-jisx0208-latin-mode
                     command )
        (setq command (key-binding keys))
        (if (eq command this-command)
            ;; avoid recursive calling of skk-emulate-original-map.
            nil
          ;; if no bindings are found, call `undefined'.  it's
          ;; original behaviour.
          (skk-cancel-undo-boundary)
          (command-execute (or command (function undefined))))))))

(defun skk-command-key-sequence (key command)
  ;; KEY から universal arguments を取り除き、COMMAND を実行するキーを返す。
  ;; `execute-extended-command' によってコマンドが実行された場合は、nil を返す。
  (while (not (or (zerop (length key))
                  (eq command (key-binding key))))
    (setq key (vconcat (cdr (append key nil)))))
  (and (not (zerop (length key))) key))

(defun skk-setup-delete-selection-mode ()
  ;; Delete Selection モードが SKK を使った日本語入力に対しても機能するように
  ;; セットアップする。
  (and (featurep 'delsel)
       (not (get 'skk-insert 'delete-selection))
       (mapcar (function (lambda (func) (put func 'delete-selection t)))
	       '(skk-current-kuten
		 skk-current-touten
		 skk-input-by-code-or-menu
		 skk-insert
		 skk-today ))))

(defun skk-setup-auto-paren ()
  (if (and skk-auto-insert-paren skk-auto-paren-string-alist)
      (let ((strlst (mapcar 'char-to-string skk-special-midashi-char-list))
	    rulealst str alist )
	(while strlst
	  ;; skk-auto-paren-string-alist の中から、skk-special-midashi-char-list
	  ;; の要素に関連するものを取り除く。
	  (remove-alist 'skk-auto-paren-string-alist (car strlst))
	  (setq strlst (cdr strlst)) )
	(if (null (memq t (mapcar (function
				   (lambda (e)
				     (skk-ascii-char-p (string-to-char (car e))) ))
				  skk-auto-paren-string-alist )))
	    nil
	  (setq alist skk-auto-paren-string-alist
		rulealst (nconc (mapcar (function (lambda (e) (nth 2 e)))
					skk-rom-kana-rule-list )
				(mapcar (function (lambda (e) (nth 2 e)))
					skk-rom-kana-base-rule-list )))
	  (while alist
	    (setq str (car (car alist)))
	    (and (skk-ascii-char-p (string-to-char str))
		 ;; 出力文字が入っているセルを調べて、いずれかのキーにバインド
		 ;; されていないかどうかを確認する。
		 (null (assoc str rulealst))
		 (null (rassoc str rulealst))
		 ;; 割り付けようとしているキーが、何か他の出力文字にバインドさ
		 ;; れていないかどうかを確認する。
		 (null (assoc str skk-rom-kana-base-rule-list))
		 (null (assoc str skk-rom-kana-rule-list))
		 ;; skk-auto-paren-string-alist の各要素の car の文字が
		 ;; ascii char である場合は、skk-rom-kana-rule-list,
		 ;; skk-rom-kana-base-rule-list にその文字を書き込む (本
		 ;; 来は ascii char は skk-rom-kana-rule-list,
		 ;; skk-rom-kana-base-rule-list に書く必要がない ---
		 ;; skk-emulate-original-mapによる入力が行なわれる ---
		 ;; が、skk-auto-paren-string-alist に指定された対になる
		 ;; 文字の挿入のためには、キーとなる文字を書いておく必要が
		 ;; ある)。
		 (setq skk-rom-kana-rule-list (cons (list str nil str)
						    skk-rom-kana-rule-list )))
	    (setq alist (cdr alist)) )))))

(defun skk-adjust-user-option ()
  ;; 両立できないオプションの調整を行なう。
  (and skk-process-okuri-early
       ;; skk-process-okuri-early の値が non-nil であるときに下記の値が non-nil
       ;; であれば正常に動かないのでこの変数の優先順位を高くした。
       (setq skk-kakutei-early nil
	     skk-auto-okuri-process nil
	     skk-henkan-okuri-strictly nil
	     skk-henkan-strict-okuri-precedence nil )))

(defun skk-try-completion (arg)
  "▽モードで見出し語の補完を行う。
それ以外のモードでは、オリジナルのキー割り付けのコマンドをエミュレートする。"
  (interactive "P")
  (skk-with-point-move
   (if (and skk-henkan-on (not skk-henkan-active))
       (progn
	 (setq this-command 'skk-completion)
	 (skk-completion (not (eq last-command 'skk-completion))) )
     (skk-emulate-original-map arg) )))

(defun skk-latin-mode (arg)
  "SKK のモードを latin (ascii) モードに変更する。"
  (interactive "P")
  (skk-kakutei)
  (skk-latin-mode-on) )

(defun skk-jisx0208-latin-mode (arg)
  "SKK のモードを全角英字入力モードに変更する。"
  (interactive "P")
  (skk-kakutei)
  (skk-jisx0208-latin-mode-on) )

(defun skk-abbrev-mode (arg)
  "ascii 文字をキーにした変換を行うための入力モード。"
  (interactive "*P")
  (and skk-henkan-on (not skk-henkan-active)
       (skk-error "既に▽モードに入っています" "Already in ▽ mode") )
  (skk-kakutei)
  (skk-set-henkan-point-subr)
  (skk-abbrev-mode-on) )

(defun skk-toggle-kana (arg)
  "ひらがなモードとカタカナモードをトグルで切り替える。
カタカナモードで変換を行なうときに、送り仮名をカタカナに変換したくないときは、
skk-convert-okurigana-into-katakana の値を non-nil にする。

▽モードでは、skk-henkan-start-point (▽の直後) とカーソルの間の文字列を

    ひらがな <=> カタカナ
    全角英数字 <=> ascii

のように変換する。"
  (interactive "P")
  (cond ((and skk-henkan-on (not skk-henkan-active))
         (let (char)
           (skk-save-point
             (goto-char skk-henkan-start-point)
             ;; "ー" では文字種別が判別できないので、ポイントを進める。
             (while (looking-at "ー")
               (forward-char 1) )
             (setq char (skk-what-char-type)) )
           (skk-set-marker skk-henkan-end-point (point))
           (cond ((eq char 'hiragana)
                  (skk-katakana-henkan arg) )
                 ((eq char 'katakana)
                  (skk-hiragana-henkan arg) )
                 ((eq char 'jisx0208-latin)
                  (skk-jisx0208-latin-henkan arg) )
                 ((eq char 'ascii)
                  (skk-latin-henkan arg) ))))
        ((and (skk-in-minibuffer-p) (not skk-j-mode))
         ;; ミニバッファへの初突入時。
         (skk-j-mode-on) )
        (t (setq skk-katakana (not skk-katakana))) )
  (skk-kakutei)
  (setq skk-input-mode-string (if skk-katakana skk-katakana-mode-string
				skk-hiragana-mode-string ))
  (force-mode-line-update) )

(defun skk-misc-for-picture ()
  ;; picture-mode へ入ったときに SKK 起動前の状態に戻す。
  ;; edit-picture-hook に add-hook して使用する。
  ;;
  ;; 既存のバッファを picture mode にしたとき、picture-mode 関数は
  ;; kill-all-local-variables 関数を呼ばないので、SKK 関連のバッファローカル
  ;; 変数が元のバッファの値のままになってしまう。そこで、picture mode に入った
  ;; ときにフックを利用してこれらのバッファローカル変数を kill する。
  ;; RMS は picture-mode で kill-all-local-variables 関数を呼ばないのは、バグ
  ;; ではない、と言っていた。
  ;;
  ;; picture-mode で SKK を使用し漢字入力をした場合に、BS で全角文字が消せない
  ;; のは、SKK の不具合ではなく、picture.el の問題 (move-to-column-force 関数
  ;; の中で使用している move-to-column で全角文字を無視したカラム数が与えられ
  ;; たときにカーソル移動ができないから) である。消したい文字にポイントを合わ
  ;; せ、C-c C-d で一文字づつ消すしか方法はない。
  (and skk-mode (skk-kill-local-variables)) )

(defun skk-kill-local-variables ()
  ;; SKK 関連のバッファローカル変数を無効にする。
  (skk-mode -1)
  (let ((lv (buffer-local-variables))
        v vstr )
    (while lv
      (setq v (car (car lv))
            lv (cdr lv)
            vstr (prin1-to-string v) )
      (and (> (length vstr) 3) (string= "skk-" (substring vstr 0 4))
	   (kill-local-variable v) ))))

;;;; kana inputting functions

(defun skk-insert (&optional arg)
  "SKK の文字入力を行なう。"
  ;; skk-rom-kana-\\(base-\\)*rule-list の caddr に関数を書き、その関数内で、一
  ;; 定条件を満した場合に (文字挿入以外の) ある特定の動作をさせ、そうでない場合はある特
  ;; 定文字の挿入を行なうことのメリット、デメリットについて。
  ;;
  ;; メリット; 必ず skk-kana-input を通るので、unfixed prefix + トリガーキーの
  ;; 文字処理を行なってから指定の関数呼び出しに入ることができる。
  ;; 
  ;; デメリット; コールされた関数内で、独自に挿入文字を決定することはできるが、
  ;; skk-rom-kana-\\(base-\\)*rule-list 内で定義が行なえない (既に文字の代わり
  ;; に関数名が指定されているから。該当関数内で、skk-kana-input をコールすると、
  ;; 無限ループに陥いってしまう)。関数内でオリジナルのカレントマップの動作をエ
  ;; ミュレートすると、ユーザーが挿入文字の定義を変更できない。
  ;;
  ;; また、skk-input-vector を廃し、skk-rom-kana-\\(base-\\)*rule-list に挿入
  ;; すべき文字定義を集中させたことから、可能な限りこれを崩したくない。
  ;; 
  ;; 上記の考察から、下記のように方針を決めた。
  ;;
  ;; (1)挿入文字の定義は、skk-rom-kana-\\(base-\\)*rule-list 以外では行なわな
  ;;    い。
  ;; (2)トリガーキーをユーザー変数とし、このキーが押された場合かどうかの判定は、
  ;;    skk-insert 内で行ない、適当な関数をコールする。
  ;; (3)(2)のユーザー変数は、skk-abbrev-mode-map のキー定義などでも参照するこ
  ;;    ととし、可能な限り動作の統一を図る。
  ;; (4)unfixed prefix + トリガーキーの処理は必要に応じて該当関数の中に埋め込
  ;;    む。
  (interactive "p*")
  (skk-with-point-move
   (let ((ch last-command-char))
     (cond (
	    ;; start writing a midasi key.
	    (or (and (memq ch skk-set-henkan-point-key)
		     (or skk-okurigana
			 (not (skk-get-prefix skk-current-rule-tree))
			 (not (skk-select-branch skk-current-rule-tree ch)) ))
		(and skk-henkan-on (memq ch skk-special-midashi-char-list)) )
	    ;; normal pattern
	    ;; skk-set-henkan-point -> skk-kana-input.
	    (skk-set-henkan-point arg) )
	   ;; start conversion.
	   ((and skk-henkan-on (eq ch skk-start-henkan-char))
	    (skk-start-henkan arg) ) 
	   ;; for completion.
	   ((and skk-henkan-on (not skk-henkan-active))
	    (cond ((eq ch skk-try-completion-char)
		   (setq this-command 'skk-completion)
		   (skk-completion (not (eq last-command 'skk-completion))) )
		  ((eq last-command 'skk-completion)
		   (cond ((eq ch skk-next-completion-char)
			  (setq this-command 'skk-completion)
			  (skk-completion nil) )
			 ((eq ch skk-previous-completion-char)
			  (setq this-command 'skk-completion)
			  (skk-previous-completion) )
			 (t (skk-kana-input arg)) ))
		  (t (skk-kana-input arg)) ))
	   ;; just imput Kana.
	   (t (skk-kana-input arg)) ))))

(defun skk-kana-input (&optional arg)
  ;;"かな文字の入力を行うルーチン。"
  ;; Message-Id: <19980610190611B.sakurada@kuis.kyoto-u.ac.jp>
  ;; From: Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
  ;; Date: Wed, 10 Jun 1998 19:06:11 +0900 (JST)
  ;;
  ;; 新しい skk-kana-input は, 簡単に説明すると,
  ;; あらかじめルールを木の形に表現しておいて, 入力を見
  ;; て木を辿り, それ以上辿れなくなったらその節に登録さ
  ;; れている仮名を入力する. というしくみです.
  ;;
  ;; 例えば, n a t のみからなる以下のルール
  ;;
  ;;     a  → あ
  ;;     n  → ん
  ;;     nn → ん
  ;;     na → な
  ;;     ta → た
  ;;     tt → っ (次状態 t)
  ;;
  ;; をルール木に変換すると,
  ;;
  ;;                 ／/＼
  ;;               ／ /   ＼
  ;;           a ／  / t    ＼ n
  ;;           ／   /         ＼
  ;;          あ   ・           ん
  ;;             ／|           / ＼
  ;;         a ／  | t      a /    ＼ n
  ;;         ／    |         /       ＼
  ;;       た     っ        な        ん
  ;;          (次状態 "t")
  ;;
  ;; という形になります.
  ;;
  ;; 初期状態(木の根)で `a' を入力すると, 木の根から
  ;; 「あ」に移動します. 次にどのような入力が来ても,
  ;; それより下にたどれないので, 「あ」を出力して根に戻ります.
  ;; ルールに次状態が設定されている場合は, 設定されてい
  ;; る文字列をキューに戻してから根に戻ります.
  ;;
  ;; 初期状態で `n' を入力すると, 「ん」に移動します.
  ;; 次に `a' または `n' が入力されればそれより下にたどれる
  ;; ので次の入力を見るまでまだ出力しません.
  ;; 次に `t' が入力された場合は, `t' では下にたどれないので,
  ;; 「ん」を出力して `t' をキューに戻します.
  ;;
  ;; ここで, 初期状態, 現状態をそれぞれ skk-rule-tree,
  ;; skk-current-rule-tree で表し.
  ;; 木を下にたどる, という操作は, skk-select-branch を
  ;; 用いて,
  ;;
  ;;   (skk-select-branch rule-tree ?a)
  ;;
  ;; のようにします. 現状態に設定されているかな(("ア". "あ")など),
  ;; 次状態("t" など)は, それぞれ skk-get-kana,
  ;; skk-get-nextstate で取ります.
  ;; don't echo key strokes in the minibuffer.
  (let ((echo-keystrokes 0)
	(queue (list last-command-char)) )
    (while queue
      (if (not (skk-get-prefix skk-current-rule-tree))
	  (progn
	    (skk-set-marker skk-kana-start-point (point))
	    (setq skk-current-rule-tree skk-rule-tree) )
	(skk-erase-prefix) )
      (setq skk-prefix (concat (skk-get-prefix skk-current-rule-tree)
			       (char-to-string last-command-char)))
      (let ((next (skk-select-branch skk-current-rule-tree (car queue)))
	    data )
	(if next
	    ;; can go down SKK-CURRENT-RULE-TREE
	    (if (skk-get-branch-list next)
		;; NEXT have at least one branch
		(progn
		  (and skk-henkan-active
		       skk-kakutei-early
		       (not skk-process-okuri-early) 
		       (skk-kakutei) )
		  (setq queue (cdr queue)
			skk-current-rule-tree next ))
	      ;; NEXT does not have any branch (i.e. NEXT is a leaf)
	      (setq data (skk-get-kana next)
		    queue (nconc (string-to-char-list (skk-get-nextstate next))
				 (cdr queue) )
		    skk-current-rule-tree nil ))
	  ;; can not go down SKK-CURRENT-RULE-TREE
	  (let ((d (skk-get-kana skk-current-rule-tree)))
	    (if d
		;; SKK-CURRENT-RULE-TREE have a roma->kana rule
		(setq data d
		      queue
		      (nconc (string-to-char-list
			      (skk-get-nextstate skk-current-rule-tree) )
			     queue )
		      skk-current-rule-tree nil )
	      ;; SKK-CURRENT-RULE-TREE does not have any roma->kana rule
	      (let ((dd (and skk-kana-input-search-function
			     (funcall skk-kana-input-search-function) )))
		(if dd
		    (setq data (car dd)
			  queue (nconc (string-to-char-list (cdr dd))
				       (cdr queue) )
			  skk-current-rule-tree nil )
		  (if (eq skk-current-rule-tree skk-rule-tree)
		      ;; typo on the root of tree
		      (setq queue nil
			    skk-current-rule-tree nil )
		    ;; otherwise move to root of the tree, and redo
		    (setq skk-current-rule-tree nil) ))))))
	(if (not data)
	    (if skk-current-rule-tree
		(progn
		  ;;(digit-argument arg)
		  ;; う〜ん、よう分からん。とりあえず。
		  (or skk-isearch-message (setq prefix-arg arg))
		  (setq skk-prefix (skk-get-prefix skk-current-rule-tree))
		  (skk-insert-prefix skk-prefix) )
	      ;;(skk-kana-cleanup 'force)
	      (and skk-henkan-active (skk-kakutei))
	      (setq skk-prefix "")
	      (or queue
		  (skk-emulate-original-map (skk-make-raw-arg arg)) ))
	  (skk-cancel-undo-boundary)
	  (setq skk-prefix "")
	  (and (functionp data)
	       (setq data (funcall data (skk-make-raw-arg arg))) )
	  (if (not (stringp (if (consp data) (car data) data)))
	      nil
	    (let* ((str (if (consp data) (if skk-katakana (car data) (cdr data))
			  data ))
		   (pair (and skk-auto-insert-paren
			      (cdr (assoc str skk-auto-paren-string-alist)) ))
		   (count0 arg) (count1 arg) (inserted 0) )
	      (and skk-henkan-active
		   skk-kakutei-early (not skk-process-okuri-early)
		   (skk-kakutei) )
	      ;; arg は保存しておかないと、0 になってしまい、queue
	      ;; がたまっていて再度ここへやって来たときに文字入力が
	      ;; できなくなる。
	      (while (> count0 0)
		(skk-insert-str str)
		(setq count0 (1- count0)) )
	      (if (not pair)
		  nil
		(while (> count1 0)
		  (if (not (string= pair (char-to-string (following-char))))
		      (progn
			(setq inserted (1+ inserted))
			(skk-insert-str pair) ))
		  (setq count1 (1- count1)) )
		(or (= inserted 0) (backward-char inserted)) )
	      (and skk-okurigana (null queue) (skk-set-okurigana)) ))))
      ;; XXX I don't know how skk-isearch-message works....
      (and skk-isearch-message (skk-isearch-message)) )))

;; tree procedure (ツリーにアクセスするためのインターフェース)
(defun skk-search-tree (tree char-list)
  ;; TREE の根から先端へ CHAR-LIST に従ってたどる。
  ;; 成功した場合は nil と 結果の木の組を返し,
  ;; 失敗した場合はたどれなかった CHAR-LIST の残りとたどれなくなった
  ;; 節点の木の組を返す。
  (catch 'return
    (let (next char rest)
      (while char-list
	(setq char (car char-list)
	      rest (cdr char-list)
	      next (skk-select-branch tree char))
	(if next
	    (setq tree next
		  char-list rest)
	  (throw 'return (cons char-list tree))))
      (cons nil tree))))

(defun skk-add-rule (tree rule)
  (let* ((prefix (nth 0 rule))
	 (l (length prefix))
	 (result (skk-search-tree tree (string-to-char-list prefix)))
	 (rest (car result))
	 (addpoint (cdr result)) )
    (while rest
      (let ((addtree
	     (skk-make-rule-tree (car rest)
				 (substring prefix 0 (1+ (- l (length rest))))
				 nil nil nil)))
	(skk-add-branch addpoint addtree)
	(setq addpoint addtree
	      rest (cdr rest))))
    (skk-set-nextstate addpoint (nth 1 rule))
    (skk-set-kana addpoint (nth 2 rule))))

(defun skk-delete-rule (tree string)
  ;; 入力 STRING に対するルールをルール木 TREE から削除
  (catch 'return
    (let ((char-list (string-to-char-list string))
	  (cutpoint tree)
	  (cuttree (car (skk-get-branch-list tree)))
					; TREE の根から出る枝が1本しかない場合
					; のために一応初期化しておく
	  next)
      (while char-list
	(setq next (skk-select-branch tree (car char-list))
	      char-list (cdr char-list))
	(if next
	    (if (> (length (skk-get-branch-list tree)) 1)
		(setq cutpoint tree	; 枝が2本以上の時 cutpoint cuttree
		      cuttree next	; を update
		      tree next)
	      (setq tree next))
	  (throw 'return nil)))
      (skk-set-branch-list cutpoint
			   (delq cuttree (skk-get-branch-list cutpoint))))))

;; convert skk-rom-kana-rule-list to skk-rule-tree.
;; The rule tree follows the following syntax:
;; <branch-list>  := nil | (<tree> . <branch-list>)
;; <tree>         := (<char> <prefix> <nextstate> <kana> <branch-list>)
;; <kana>         := (<ひらがな文字列> . <カタカナ文字列>) | nil
;; <char>         := <英小文字>
;; <nextstate>    := <英小文字文字列> | nil
(defun skk-compile-rule-list (&rest l)
  ;; rule-list を木の形にコンパイルする。
  (let ((tree (skk-make-rule-tree nil "" nil nil nil))
	rule ll )
    (while l
      (setq ll (car l)
	    l (cdr l) )
      (while ll
	(setq rule (car ll)
	      ll (cdr ll) )
	(skk-add-rule tree rule) ))
    tree ))

(defun skk-insert-str (str)
  ;; STR を挿入する。必要であれば self-insert-after-hook をコ
  ;; ールする。overwrite-mode であれば、適切に上書きを行う。
  (insert-and-inherit str)
  (if (and skk-henkan-on (not skk-henkan-active))
      (and skk-auto-start-henkan (not skk-okurigana) (skk-auto-start-henkan str))
    (and (boundp 'self-insert-after-hook) self-insert-after-hook
	 (funcall self-insert-after-hook (- (point) (length str)) (point)) )
    (and overwrite-mode
	 (skk-del-char-with-pad (skk-ovwrt-len (string-width str))) )))

(defun skk-ovwrt-len (len)
  ;; 上書きして良い長さを返す。
  (min (string-width
	(buffer-substring-no-properties
	 (point) (skk-save-point (end-of-line) (point)) ))
       len ))

(defun skk-del-char-with-pad (length)
  ;; 長さ LENGTH の文字を消去する。調整のため、必要であれば、末尾にスペースを
  ;; 挿入する。
  (let ((p (point)) (len 0))
    (while (< len length)
      (forward-char 1)
      (setq len (string-width (buffer-substring-no-properties (point) p))) )
    (delete-region p (point))
    (or (= length len)
        (progn
	  (insert-and-inherit " ")
          (backward-char 1) ))))

(defun skk-cancel-undo-boundary ()
  ;; skk-insert, skk-jisx0208-latin-insert で連続して入力さ
  ;; れた 20 文字を 1 回のアンドゥの対象とする。`20' は
  ;; keyboard.c に定められたマジックナンバー。Mule-2.3 添付
  ;; の egg.el を参考にした。
  (if (and (< skk-self-insert-non-undo-count 20)
           (memq last-command
                 '(skk-insert
                   skk-jisx0208-latin-insert
                   ;; SKK abbrev モードでは、アスキー文字入力が Emacs オリジナ
                   ;; ルの self-insert-command により行なわれているので、
                   ;; skk-self-insert-non-undo-count をインクリメントすること
                   ;; ができないので、アンドゥをエミュレートできない。
                   ;; しかも、カンマやピリオドを挿入した時点で、
                   ;; skk-abbrev-comma や skk-abbrev-period を使うことになるの
                   ;; で (self-insert-command 以外のコマンドを使ってしまうので)、
		   ;; オリジナルのアンドゥの機能も損なってしまう。
		   ;; しかし現実問題としては、SKK abbrev モードは省略形としての
		   ;; 見出し語を挿入するためのモードであるので、長い見出し語を
		   ;; 挿入することはあまりなく、問題も小さいと考えられる。
                   ;;skk-abbrev-comma
                   ;;skk-abbrev-period
		   )))
      (progn
        (cancel-undo-boundary)
	(if (null skk-current-rule-tree)
	    ;; まだかな文字が完成していないときは、undo count をインクリメント
	    ;; しない。
	    (setq skk-self-insert-non-undo-count
		  (1+ skk-self-insert-non-undo-count) )))
    (setq skk-self-insert-non-undo-count 1) ))

(defun skk-translate-okuri-char (okurigana)
  (and skk-okuri-char-alist
       (cdr (assoc (skk-substring-head-character okurigana) skk-okuri-char-alist)) ))

(defun skk-set-okurigana ()
  ;; 見出し語から skk-henkan-okurigana, skk-henkan-key の各値をセットする。
  (cancel-undo-boundary)
  (and skk-katakana (skk-hiragana-region skk-henkan-start-point (point)))
  (skk-set-marker skk-henkan-end-point skk-okurigana-start-point)
  ;; just in case
  (skk-save-point
    (goto-char skk-okurigana-start-point)
    (or (eq (following-char) ?*) (insert-and-inherit "*")) )
  (setq skk-henkan-okurigana (buffer-substring-no-properties
                              (1+ skk-okurigana-start-point)
                              (point) ))
  (setq skk-henkan-key (concat (buffer-substring-no-properties
				skk-henkan-start-point
				skk-henkan-end-point )
			       (or (skk-translate-okuri-char
				    skk-henkan-okurigana)
				   skk-okuri-char ))
        skk-prefix "" )
  (delete-region skk-okurigana-start-point (1+ skk-okurigana-start-point))
  (setq skk-henkan-count 0)
  (skk-henkan)
  (setq skk-okurigana nil) )

;;;; other inputting functions

(defun skk-toggle-kutouten ()
  "句読点の種類をトグルで変更する。"
  (interactive)
  (setq skk-kutouten-type (if (eq skk-kutouten-type 'jp) 'en 'jp))
  (and (interactive-p)
       (skk-message "句点: `%s'  読点: `%s'"
		    "Kuten: `%s'  Touten: `%s'"
		    (skk-current-kuten nil) (skk-current-touten nil) )))

(defun skk-current-kuten (arg)
  ;; just ignore arg.
  (car (cdr (assq skk-kutouten-type skk-kuten-touten-alist))) )

(defun skk-current-touten (arg)
  ;; just ignore arg.
  (cdr (cdr (assq skk-kutouten-type skk-kuten-touten-alist))) )

(defun skk-abbrev-period (arg)
  "SKK abbrev モードで見出しの補完を行っている最中であれば、次の候補を表示する。
補完の直後でなければ、オリジナルのキー割り付けのコマンドをエミュレートする。
SKK abbrev モード以外では、skk-insert-period 関数を使用すること。"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-completion)
       (progn
	 (setq this-command 'skk-completion)
	 (skk-completion nil) )
     (skk-emulate-original-map arg) )))

(defun skk-abbrev-comma (arg)
  "SKK abbrev モードで見出しの補完を行っている最中であれば、直前の候補を表示する。
補完の直後でなければ、オリジナルのキー割り付けのコマンドをエミュレートする。
SKK abbrev モード以外では、skk-insert-comma 関数を使用すること。"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-completion)
       (skk-previous-completion)
     (skk-emulate-original-map arg) )))

(defun skk-jisx0208-latin-insert (arg)
  "全英文字をカレントバッファに挿入する。
skk-jisx0208-latin-vector をテーブルとして、最後に入力されたキーに対応する文
字を挿入する。
skk-auto-insert-paren の値が non-nil の場合で、skk-auto-paren-string-alist に
対応する文字列があるときは、その対応する文字列 (かっこ類) を自動的に挿入する。"
  (interactive "*p")
  (skk-with-point-move
   (let* ((str (aref skk-jisx0208-latin-vector last-command-char))
	  (arg2 arg)
	  (pair-str
	   (and skk-auto-insert-paren
		(cdr (assoc str skk-auto-paren-string-alist)) ))
	  (pair-str-inserted 0) )
     (if (not str)
	 (skk-emulate-original-map arg)
       (skk-cancel-undo-boundary)
       (while (> arg 0)
	 (skk-insert-str str)
	 (setq arg (1- arg)) )
       (if (not pair-str)
	   nil
	 (while (> arg2 0)
	   (if (not (string= pair-str (char-to-string (following-char))))
	       (progn
		 (setq pair-str-inserted (1+ pair-str-inserted))
		 (skk-insert-str pair-str) ))
	   (setq arg2 (1- arg2)) )
	 (or (= pair-str-inserted 0) (backward-char pair-str-inserted)) )))))

(defun skk-delete-backward-char (arg)
  "▼モードで skk-delete-implies-kakutei が non-nil だったら直前の文字を消して確定する。
▼モードで skk-delete-implies-kakutei が nil だったら前候補を表示する。
▽モードで`▽'よりも前のポイントで実行すると確定する。
確定入力モードで、かなプレフィックスの入力中ならば、かなプレフィックスを消す。"  
  (interactive "*P")
  (skk-with-point-move
   (let ((count (prefix-numeric-value arg)))
     (cond (skk-henkan-active
	    (if (and (not skk-delete-implies-kakutei)
		     (= skk-henkan-end-point (point)) )
		(skk-previous-candidate)
	      ;; overwrite-mode で、ポイントが全角文字に囲まれていると
	      ;; きに delete-backward-char を使うと、全角文字は消すが半
	      ;; 角文字分しか backward 方向にポイントが戻らない (Emacs
	      ;; 19.31 にて確認)。変換中の候補に対しては
	      ;; delete-backward-char で必ず全角文字 1 文字分 backward
	      ;; 方向に戻った方が良い。
	      (if overwrite-mode
		  (progn
		    (backward-char count)
		    (delete-char count arg) )
		(skk-emulate-original-map arg) )
	      ;; XXX assume skk-prefix has no multibyte chars.
	      (if (> (length skk-prefix) count)
		  (setq skk-prefix (substring skk-prefix 0 (- (length skk-prefix) count)))
		(setq skk-prefix "") )
	      (and (>= skk-henkan-end-point (point)) (skk-kakutei)) ))
	   ((and skk-henkan-on (>= skk-henkan-start-point (point)))
	    (setq skk-henkan-count 0)
	    (skk-kakutei) )
	   ;; 入力中の見出し語に対しては delete-backward-char で必ず全角文字 1
	   ;; 文字分 backward 方向に戻った方が良い。
	   ((and skk-henkan-on overwrite-mode)
	    (backward-char count)
	    (delete-char count arg) )
	   (t 
	    (skk-delete-okuri-mark)
	    (if (skk-get-prefix skk-current-rule-tree)
		(skk-erase-prefix 'clean)
	      (skk-emulate-original-map arg) ))))))

;;;; henkan routines
(defun skk-henkan ()
  ;; カナを漢字変換するメインルーチン。
  (let (mark new-word kakutei-henkan)
    (if (string= skk-henkan-key "")
        (skk-kakutei)
      ;; we use mark to go back to the correct position after henkan
      (or (eobp) (setq mark (skk-save-point (forward-char 1) (point-marker))))
      (if (not skk-henkan-active)
          (progn
            (skk-change-marker)
            (setq skk-current-search-prog-list skk-search-prog-list) ))
      ;; skk-henkan-1 の中からコールされる skk-henkan-show-candidate から throw
      ;; される。ここでキャッチした場合は、?x がストリームに戻されているので、
      ;; この関数を出て、skk-previous-candidates へゆく。
      (catch 'unread
        (setq new-word (or (skk-henkan-1) (skk-henkan-in-minibuff))
              kakutei-henkan skk-kakutei-flag )
        (and new-word (skk-insert-new-word new-word)) )
      (if mark
          (progn
            (goto-char mark)
            ;; 参照されていないマーカーは、Garbage Collection がコールされたと
            ;; きに回収されるが、それまでの間、テキストのどこかを指していると、
            ;; テキストのアップデートの際にそのマーカー値を更新する必要がある
            ;; ので、どこも指さないようにする。
            (skk-set-marker mark nil)
	    (backward-char 1) )
        (goto-char (point-max)) )
      (and kakutei-henkan
	   (skk-kakutei (if (skk-numeric-p)
			    (skk-get-current-candidate-simply 'noconv)
			  new-word ))))))

(defun skk-henkan-1 ()
  ;; skk-henkan のサブルーチン。
  (let (new-word)
    (if (= skk-henkan-count 0)
        (progn
          (and (eq last-command 'skk-undo-kakutei-henkan)
	       (eq (car (car skk-current-search-prog-list))
		   'skk-search-kakutei-jisyo-file )
	       ;; in this case, we should not search kakutei jisyo.
	       (setq skk-current-search-prog-list
		     (cdr skk-current-search-prog-list) ))
          (setq skk-henkan-list (skk-search))
          (if (null skk-henkan-list)
              nil
            (setq new-word (skk-get-current-candidate))
            (and skk-kakutei-flag
		 ;; found the unique candidate in kakutei jisyo
		 (setq this-command 'skk-kakutei-henkan) )))
      ;; 変換回数が 1 以上のとき。
      (setq new-word (skk-get-current-candidate))
      (or new-word
          ;; 新しい候補を見つけるか、skk-current-search-prog-list が空にな
          ;; るまで skk-search を連続してコールする。
          (while (and skk-current-search-prog-list (not new-word))
            (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search))
                  new-word (skk-get-current-candidate) )))
      (and new-word (> skk-henkan-count 3)
	   ;; show candidates in minibuffer
	   (setq new-word (skk-henkan-show-candidates) )))
    new-word ))

(defun skk-get-current-candidate ()
  (if (skk-numeric-p)
      (let (val)
        (skk-num-uniq)
        (setq val (skk-num-convert (skk-get-current-candidate-simply)))
        (if (not skk-num-recompute-key)
            val
          (skk-num-uniq)
          (skk-num-convert (skk-get-current-candidate-simply)) ))
    (skk-get-current-candidate-simply) ))

(defun skk-henkan-show-candidates ()
  ;; ミニバッファで変換した候補群を表示する。
  (skk-save-point
   (let* ((candidate-keys ; 表示用のキーリスト
           (mapcar
	    (function (lambda (c)
			(and (memq c '(?\C-g ?\040 ?x)) ; ?\040 is SPC.
			     (skk-error "`%s' に無効なキーが指定されています。"
					"Illegal key in `%s'"
					"skk-henkan-show-candidates-keys" ))
			(char-to-string (upcase c)) ))
	    skk-henkan-show-candidates-keys ))
          key-num-alist ; 候補選択用の連想リスト
          (key-num-alist1 ; key-num-alist を組み立てるための作業用連想リスト。
           (let ((count 6))
             (mapcar (function (lambda (key) (prog1 (cons key count)
                                               (setq count (1- count)) )))
                     ;; 逆さまにしておいて、表示する候補の数が少なかったら先
                     ;; 頭から幾つか削る。
                     (reverse skk-henkan-show-candidates-keys) )))
          (loop 0)
          inhibit-quit
          henkan-list new-one str reverse n )
     ;; Emacs 19.28 だと Overlay を消しておかないと、次に insert される
     ;; skk-henkan-key に何故か Overlay がかかってしまう。
     (and skk-use-face (skk-henkan-face-off))
     (delete-region skk-henkan-start-point skk-henkan-end-point)
     (while loop
       (if str
           (let (message-log-max)
             (message str) )
         (cond (reverse
                (setq loop (1- loop)
                      henkan-list (nthcdr (+ 4 (* loop 7)) skk-henkan-list)
                      reverse nil ))
               (skk-exit-show-candidates
                ;; 候補が尽きてしまって、skk-henkan-show-candidates ->
                ;; skk-henkan-in-minibuff -> skk-henkan
                ;; -> skk-henkan-show-candidates の順で、再びこの関数が呼ばれ
                ;; たときは、ここで henkan-list と loop を計算する。
                (setq henkan-list (nthcdr skk-henkan-count skk-henkan-list)
                      loop (car skk-exit-show-candidates)
                      skk-exit-show-candidates nil ))
               (t
                ;; skk-henkan-show-candidates-keys の最終のキーに対応する候補
                ;; が出てくるまでサーチを続ける。
                (and (skk-numeric-p) (skk-num-uniq))
                (while (and skk-current-search-prog-list
                            (null (nthcdr (+ 11 (* loop 7)) skk-henkan-list)) )
                  (setq skk-henkan-list
                        (skk-nunion skk-henkan-list (skk-search)) )
                  (and (skk-numeric-p) (skk-num-uniq)) )
                (and (skk-numeric-p) (skk-num-convert*7))
                (setq henkan-list (nthcdr (+ 4 (* loop 7)) skk-henkan-list)) ))
         (setq n (skk-henkan-show-candidate-subr candidate-keys henkan-list)) )
       (if (> n 0)
           (condition-case nil
               (let* ((event (skk-read-event))
                      (char (event-to-character event))
                      num )
		 (if (eq skk-emacs-type 'xemacs)
		     (message "")) ; clear out candidates in echo area
                 (if (null char)
                     (skk-unread-event event)
                   (setq key-num-alist (nthcdr (- 7 n) key-num-alist1))
                   (and key-num-alist
			(setq num (cdr (or (assq char key-num-alist)
					   (if (skk-lower-case-p char)
					       (assq (upcase char) key-num-alist)
					     (assq (downcase char) key-num-alist) )))))
                   (cond (num
                          (setq new-one (nth num henkan-list)
                                skk-henkan-count (+ 4 (* loop 7) num)
                                skk-kakutei-flag t
                                loop nil
                                str nil ))
                         ((eq char ?\040) ; SPC
                          (if (or skk-current-search-prog-list
                                  (nthcdr 7 henkan-list) )
                              (setq loop (1+ loop)
                                    str nil )
                            ;; 候補が尽きた。この関数から抜ける。
                            (let ((last-showed-index (+ 4 (* loop 7))))
                              (setq skk-exit-show-candidates
                                    ;; cdr 部は、辞書登録に入る前に最後に表示し
                                    ;; た候補群の中で最初の候補を指すインデクス
                                    (cons loop last-showed-index) )
                              ;; 辞書登録に入る。skk-henkan-count は
                              ;; skk-henkan-list の最後の候補の次 (存在しない
                              ;; --- nil )を指す。
                              (setq skk-henkan-count (+ last-showed-index n)
                                    loop nil
                                    str nil ))))
                         ((eq char skk-previous-candidate-char)	; ?x
                          (if (= loop 0)
                              ;; skk-henkan-show-candidates を呼ぶ前の状態に戻
                              ;; す。
                              (progn
                                (setq skk-henkan-count 4)
                                (skk-unread-event (character-to-event
						   skk-previous-candidate-char))
                                ;; skk-henkan まで一気に throw する。
                                (throw 'unread nil) )
                            ;; 一つ前の候補群をエコーエリアに表示する。
                            (setq reverse t
                                  str nil )))
			 ;; これがないと quit できない。何故？
			 ((and (eq skk-emacs-type 'xemacs)
			       (eq char (quit-char)))
			  (signal 'quit nil))
                         (t (skk-message "\"%c\" は有効なキーではありません！"
                                         "\"%c\" is not valid here!"
                                         char )
                            (sit-for 1) ))))
             (quit
              ;; skk-previous-candidate へ
              (setq skk-henkan-count 0)
              (skk-unread-event (character-to-event skk-previous-candidate-char))
              ;; skk-henkan まで一気に throw する。
              (throw 'unread nil) ))))  ; end of while loop
     (if (consp new-one)
         (cdr new-one)
       new-one ))))

(defun skk-henkan-show-candidate-subr (keys candidates)
  ;; key と candidates を組み合わせて 7 つの候補群 (候補数が 7 に満たなかっ
  ;; たらそこで打ち切る) の文字列を作り、ミニバッファに表示する。
  (let ((workinglst
	 ;; CANDIDATES の先頭の 7 つのみのリスト。
	 (let ((count 0) e v)
	   (while (> 7 count)
	     (setq e (nth count candidates))
	     (if e
		 (setq v (cons e v)
		       count (1+ count) )
	       (setq count 7) ))
	   (nreverse v) ))
	(n 0) str cand message-log-max )
    (if (not (car workinglst))
        nil
      (setq workinglst (skk-truncate-message workinglst))
      (setq n 1
            ;; 最初の候補の前に空白をくっつけないように最初の候補だけ先に取り
            ;; 出す。
            str (concat (car keys) ":" (if (consp (car workinglst))
                                            (cdr (car workinglst))
                                          (car workinglst) )))
      ;; 残りの 6 つを取り出す。候補と候補の間を空白でつなぐ。
      (while (and (< n 7) (setq cand (nth n workinglst)))
        (setq cand (if (consp cand) (cdr cand) cand)
              str (concat str "  " (nth n keys) ":" cand)
              n (1+ n) ))
      (message "%s  [残り %d%s]"
               str (length (nthcdr n candidates))
               (make-string (length skk-current-search-prog-list) ?+) ))
    ;; 表示する候補数を返す。
    n ))

(defun skk-truncate-message (l)
  (let* (
	 ;; L に入っているそれぞれの要素 (候補) の文字列の幅のリスト。
	 (width-list
	  (mapcar
	   (function (lambda (e) (string-width (if (consp e) (cdr e) e))))
	   l ))
	 ;; 候補数。
	 (candidates-num (length l))
	 ;; 候補以外にエコーエリアに表示される部品の文字列の幅。
	 ;; (string-width "  [残り 100+]") -> 13
	 ;; ` F:'などの候補の選択のために表示される width 3 の文字列が候補数分ある。
	 ;; エコーエリアの最初の候補は空白が前に付いていないので 1-。
	 (parts-len (+ 13 (1- (* 3 candidates-num))))
	 ;; で、トータルでどれだけの幅になるか。
	 (message-width (apply '+ parts-len width-list))
	 (diff (- (window-width) message-width))
	 (count 0) (plus 0) max )
    (if (> diff 0)
	;; window-width に収まっていれば何もしない。
	l
      ;; それぞれの候補の最大幅を仮決めする。
      (setq max (/ (float (- (window-width) parts-len)) candidates-num))
      (while width-list
	(if (> (car width-list) max)
	    (setq count (1+ count))
	  (setq plus (+ (- max (car width-list)) plus)) )
	(setq width-list (cdr width-list)) )
      ;; 最大幅に満たない長さを集めて最大幅を修正。
      (setq max (truncate (/ (+ plus (- (window-width) parts-len))
			     candidates-num )))
      (mapcar
       (function
	(lambda (e)
	  ;; 最大幅以上の文字列を
	  (cond ((and (stringp e) (> (string-width e) max))
		 ;; 最大幅に収まるように短かくする。
		 (concat (truncate-string-to-width e (- max 3)) "...") )
		((and (consp e) (> (string-width (cdr e)) max))
		 (cons (car e) 
		       (concat (truncate-string-to-width (cdr e) (- max 3))
			       "..." )))
		(t e) )))
       l ))))

(defun skk-henkan-in-minibuff ()
  ;; ミニバッファで辞書登録をし、登録したエントリの文字列を返す。
  (save-match-data
    (let ((enable-recursive-minibuffers t)
          ;; 変換中に isearch message が出ないようにする。
          skk-isearch-message new-one )
      (add-hook 'minibuffer-setup-hook 'skk-j-mode-on)
      (add-hook
       'minibuffer-setup-hook
       (function (lambda ()
		   (add-hook 'pre-command-hook 'skk-pre-command nil 'local) )))
      (condition-case nil
          (setq new-one
                (read-from-minibuffer
                 (concat (or (and (skk-numeric-p) (skk-num-henkan-key))
                             (if skk-okuri-char
                                 (skk-compute-henkan-key2)
                               skk-henkan-key ))
                         " " )
		 (if (and (not skk-okuri-char)
			  skk-read-from-minibuffer-function )
		     (funcall skk-read-from-minibuffer-function) )))
        (quit
         (setq new-one "") ))
      (if (string= new-one "")
          (if skk-exit-show-candidates
              ;; ミニバッファに表示した候補が尽きて辞書登録に入ったが、空文字
              ;; 列が登録された場合。最後にミニバッファに表示した候補群を再表
              ;; 示する。
              (progn
                (setq skk-henkan-count (cdr skk-exit-show-candidates))
                (skk-henkan) )
            ;; skk-henkan-show-candidates に入る前に候補が尽きた場合
            (setq skk-henkan-count (1- skk-henkan-count))
            (if (= skk-henkan-count -1)
                (progn
                  ;; 送りありの変換で辞書登録に入り、空文字を登録した後、その
                  ;; まま再度送りなしとして変換した場合は 
                  ;; skk-henkan-okurigana, skk-okuri-char の値を nil にしなけ
                  ;; れば、それぞれの値に古い送り仮名が入ったままで検索に失敗
                  ;; する。
                  (setq skk-henkan-okurigana nil
                        skk-okurigana nil
                        skk-okuri-char nil )
                  (skk-change-marker-to-white) )
              ;; skk-henkan-count が -1 でなければ、カレントバッファでは最後の
              ;; 候補を表示したままなので (表示関連では何もしなくても、もう既
              ;; に望みの状態になっている) 何もしない。
              ))
        ;; ミニバッファで変換した文字列がある (空文字列でない) とき。
        ;; 末尾の空白を取り除く。
        (and (string-match "[ 　]+$" new-one)
	     (setq new-one (substring new-one 0 (match-beginning 0))) )
        (if (skk-numeric-p)
            (setq new-one (skk-num-process-user-minibuf-input new-one))
          ;; すごくたくさんの候補がある場合に、その最後に新しい候補を加えるのは
          ;; けっこう骨だが。
          (setq skk-henkan-list (nconc skk-henkan-list (list new-one))
                ;; フラグをオンにする。
                skk-kakutei-flag t ))
        (setq skk-henkan-in-minibuff-flag t
              skk-touroku-count (1+ skk-touroku-count) ))
      ;; (nth skk-henkan-count skk-henkan-list) が nil だから辞書登録に
      ;; 入っている。skk-henkan-count をインクリメントする必要はない。
      ;; (setq skk-henkan-count (1+ skk-henkan-count))
      ;; new-one が空文字列だったら nil を返す。
      (if (not (string= new-one "")) new-one) )))

(defun skk-compute-henkan-key2 ()
  ;; skk-henkan-okurigana が non-nil なら skk-henkan-key から、かつて 
  ;; skk-henkan-key2 と呼ばれていたものを作る。
  ;; skk-henkan-key2 とは、「漢字部分の読み + "*" + 送り仮名」の形式の文字列を
  ;; 言う。
  (if skk-henkan-okurigana
      (save-match-data
 	(string-match "[a-z]+$" skk-henkan-key)
 	(concat (substring skk-henkan-key 0 (match-beginning 0))
 		"*" skk-henkan-okurigana ))))

(defun skk-setup-minibuffer ()
  ;; カレントバッファの入力モードに従いミニバッファの入力モードを設定する。
  (cond ((eq skk-minibuffer-origin-mode 'hiragana)
	 (skk-j-mode-on) )
	((eq skk-minibuffer-origin-mode 'katakana)
	 (skk-j-mode-on t) )
	((eq skk-minibuffer-origin-mode 'abbrev)
	 (skk-abbrev-mode-on) )
	((eq skk-minibuffer-origin-mode 'latin)
	 (skk-latin-mode-on) )
	((eq skk-minibuffer-origin-mode 'jisx0208-latin)
	 (skk-jisx0208-latin-mode-on) )))

(defun skk-previous-candidate (&optional arg)
  "▼モードであれば、一つ前の候補を表示する。
▼モード以外ではカレントバッファに \"x\" を挿入する。
確定辞書による確定の直後に呼ぶと確定がアンドゥされて、確定前の状態で
直前の見出し語がカレントバッファに挿入される。"
  (interactive "p*")
  (skk-with-point-move
   (if (not skk-henkan-active)
       (if (not (eq last-command 'skk-kakutei-henkan))
	   (skk-kana-input arg)
	 ;; restore the state just before the last kakutei henkan.
	 (delete-region skk-henkan-start-point (point))
	 (skk-set-henkan-point-subr)
	 (insert-and-inherit (skk-get-last-henkan-data 'henkan-key))
	 (setq this-command 'skk-undo-kakutei-henkan) )
     (if (string= skk-henkan-key "")
	 nil
       (let ((mark
	      (if (not (eobp))
		  (skk-save-point (forward-char 1) (point-marker)) )))
	 (skk-save-point
	  (if (= skk-henkan-count 0)
	      (progn
		(and skk-okuri-char
		     ;; roman prefix for okurigana should be removed.
		     (setq skk-henkan-key (substring skk-henkan-key 0 -1)) )
		(setq skk-henkan-count -1
		      skk-henkan-in-minibuff-flag nil
		      skk-henkan-list nil
		      skk-henkan-okurigana nil
		      skk-okuri-char nil
		      skk-okuri-index-min -1
		      skk-okuri-index-max -1
		      skk-okurigana nil
		      skk-prefix "" )
		(and (skk-numeric-p) (skk-num-initialize))
		;; Emacs 19.28 だと Overlay を消しておかないと、次に insert され
		;; る skk-henkan-key に何故か Overlay がかかってしまう。
		(and skk-use-face (skk-henkan-face-off))
		(delete-region skk-henkan-start-point skk-henkan-end-point)
		(goto-char skk-henkan-end-point)
		(insert-and-inherit skk-henkan-key)
		(skk-change-marker-to-white) )
	    (setq skk-henkan-count (1- skk-henkan-count))
	    (skk-insert-new-word (skk-get-current-candidate-simply)) ))
	 (if mark
	     (progn
	       (goto-char mark)
	       (skk-set-marker mark nil)
	       (backward-char 1) )
	   (goto-char (point-max)) )
	 (and skk-abbrev-mode (= skk-henkan-count -1) (skk-abbrev-mode-on) ))))))

(defun skk-insert-new-word (word)
  ;; 見出し語を消し、その場所へ変換結果の文字列を挿入する。
  (let (func)
    ;; Emacs 19.28 だと Overlay を消しておかないと、次に insert される
    ;; skk-henkan-key に何故か Overlay がかかってしまう。
    (and skk-use-face (skk-henkan-face-off))
    (delete-region skk-henkan-start-point skk-henkan-end-point)
    (goto-char skk-henkan-start-point)
    ;; (^_^;) のような見出し語に対し、read-from-string を呼ぶとエラーになるの
    ;; で、condition-case でそのエラーを捕まえる。
    (condition-case nil
	(setq func (car (read-from-string word)))
      (error (setq func word)))
    (condition-case nil
	(insert-and-inherit (if (and (listp func)
				     (functionp (car func)) )
				(eval func) word ))
      ;; 文字列を返さない Lisp プログラムを評価してもエラーにならない方が便利？
      (error nil) )
    (skk-set-marker skk-henkan-end-point (point))
    (and skk-use-face (skk-henkan-face-on))
    (and skk-insert-new-word-function
	 (funcall skk-insert-new-word-function) )))

(defun skk-kakutei (&optional word)
  "現在表示されている語で確定し、辞書の更新を行う。
オプショナル引数の WORD を渡すと、現在表示されている候補とは無関係に WORD で確
定する。"
  ;; read only でエラーになるようにすると read only バッファで SKK が起動でき
  ;; なくなる。
  (interactive)
  (let ((inhibit-quit t)
	converted kakutei-word )
    (if skk-mode
	(skk-j-mode-on skk-katakana)
      ;; カレントバッファでまだ skk-mode がコールされていなかったら、コールす
      ;; る。
      (skk-mode 1) )
    (if (not skk-henkan-on)
	nil
      (if (not skk-henkan-active)
	  nil
	(setq kakutei-word
	      ;; 確定辞書の語で確定したときは、辞書にその語を書き込む必要もな
	      ;; いし、更新する必要もないと思っていたが、補完を行なうときは、
	      ;; 個人辞書を参照する (確定辞書は参照しない) ので、多少資源と時
	      ;; 間を無駄にしても、個人辞書に確定辞書のエントリを書き込んで更
	      ;; 新もしておく。
	      (or word (skk-get-current-candidate-simply (skk-numeric-p))) )
	(if (or
	     (and (not skk-search-excluding-word-pattern-function) kakutei-word)
	     (and
	      kakutei-word skk-search-excluding-word-pattern-function
	      (not
	       (funcall skk-search-excluding-word-pattern-function kakutei-word) )))
	    (progn
	      (skk-update-jisyo kakutei-word)
	      (if (skk-numeric-p)
		  (progn
		    (setq converted (skk-get-current-candidate-simply))
		    (skk-num-update-jisyo kakutei-word converted) )))))
      (skk-kakutei-cleanup-buffer) )
    ;; KAKUTEI-WORD などの情報が必要であれば、skk-last-henkan-data から得られ
    ;; る。必要なデータがそれらの変数に限定されないので、引数にしない。
    (and skk-kakutei-end-function (funcall skk-kakutei-end-function))
    (skk-kakutei-initialize (if (skk-numeric-p) (cons kakutei-word converted)
			      kakutei-word ))
    (skk-do-auto-fill) ))

(defun skk-kakutei-cleanup-buffer ()
  ;; 確定直後のバッファの整形を行なう。
  (if skk-okurigana
      (progn
        (skk-delete-okuri-mark)
        (and skk-katakana skk-convert-okurigana-into-katakana
	     (skk-katakana-region skk-henkan-end-point (point)) )))
  (skk-delete-henkan-markers)
  (and (boundp 'self-insert-after-hook) self-insert-after-hook
       (funcall self-insert-after-hook skk-henkan-start-point (point)) )
  (and overwrite-mode
       (skk-del-char-with-pad
	(skk-ovwrt-len
	 (string-width
	  (buffer-substring-no-properties skk-henkan-start-point (point)) )))))

(defun skk-kakutei-initialize (&optional kakutei-word)
  ;; 確定時に変数の初期化とアンドゥのための変数の保存を行なう。
  (if (and kakutei-word (or (consp kakutei-word)
                            (not (string= kakutei-word "")) ))
      (progn
	(setq skk-kakutei-count (1+ skk-kakutei-count))
        ;; skk-undo-kakutei のために最後の変換のデータを保存する。
	(skk-put-last-henkan-data 'henkan-key skk-henkan-key)
	(skk-put-last-henkan-data 'okuri-char skk-okuri-char)
	(skk-put-last-henkan-data 'henkan-okurigana skk-henkan-okurigana)
	(skk-put-last-henkan-data
	 'henkan-list
	 ;; 確定した語を先頭にする。
	 (cons kakutei-word (delete kakutei-word skk-henkan-list)) )
	;; (eq last-command 'skk-kakutei-henkan) でポータブルに確認できるので
	;; あえていらないか。
	;;(skk-put-last-henkan-data
	;; 'kakutei-henkan
	;; (eq this-command 'skk-kakutei-henkan) )
	;;
	;; 上記以外の henkan data を skk-last-henkan-data に残したかったら、
	;; skk-kakutei-end-function を利用する。
	))
  (setq skk-abbrev-mode nil
        skk-exit-show-candidates nil
        skk-henkan-active nil
        skk-henkan-count -1
	skk-henkan-in-minibuff-flag nil
        skk-henkan-key nil
        skk-henkan-list nil
        skk-henkan-okurigana nil
        skk-henkan-on nil
        skk-kakutei-flag nil
        skk-okuri-char nil
	skk-okuri-index-min -1
	skk-okuri-index-max -1
	;; skk-prefix ""
	)
  (and (skk-numeric-p) (skk-num-initialize))
  (and skk-use-look (setq skk-look-completion-words nil)) )

(defun skk-undo-kakutei ()
  "一番最後の確定をアンドゥし、見出しに対する候補を表示する。
最後に確定したときの候補はスキップされる。
候補が他にないときは、ミニバッファでの辞書登録に入る。"
  (interactive) 
  (skk-with-point-move
   (cond ((eq last-command 'skk-undo-kakutei)
	  (skk-error "確定アンドゥは連続使用できません"
		     "Cannot undo kakutei repeatedly" ))
	 (skk-henkan-active
	  (skk-error "▼モードでは確定アンドゥできません"
		     "Cannot undo kakutei in ▼ mode" ))
	 ( ; skk-henkan-key may be nil or "".
	  (or (not (skk-get-last-henkan-data 'henkan-key))
	      (string= (skk-get-last-henkan-data 'henkan-key) "") )
	  (skk-error "アンドゥデータがありません" "Lost undo data") ))
   (condition-case nil
       (let ((end
	      (if (skk-get-last-henkan-data 'henkan-okurigana)
		  (+ (length (skk-get-last-henkan-data 'henkan-okurigana))
		     skk-henkan-end-point )
		skk-henkan-end-point )))
	 (setq skk-henkan-active t
	       skk-henkan-on t
	       skk-current-search-prog-list
	       (if (eq (car (car skk-search-prog-list))
		       'skk-search-kakutei-jisyo-file )
		   ;; 確定辞書は探しても無意味。
		   (cdr skk-search-prog-list)
		 skk-search-prog-list ))
	 ;; get henkan data back from skk-last-henkan-data.
	 (setq skk-henkan-key (skk-get-last-henkan-data 'henkan-key)
	       skk-henkan-list (skk-get-last-henkan-data 'henkan-list)
	       skk-henkan-okurigana (skk-get-last-henkan-data 'henkan-okurigana)
	       skk-okuri-char (skk-get-last-henkan-data 'okuri-char) )
	 (and skk-use-numeric-conversion
	      (setq skk-num-list (skk-get-last-henkan-data 'skk-num-list)) )
	 (and (>= (point-max) end)
	      ;; 最後の変換部分のテキストを消す。送り仮名を把握しているのなら
	      ;; (skk-process-okuri-early が non-nil なら送り仮名を把握できない)、
	      ;; 送り仮名を含めた部分までを消す。
	      (delete-region skk-henkan-start-point end) )
	 (goto-char skk-henkan-start-point)
	 (insert-and-inherit "▼")
	 (skk-set-marker skk-henkan-start-point (point))
	 (if skk-okuri-char
	     (progn			; 送りあり
	       (insert-and-inherit (substring skk-henkan-key 0
					      (1- (length skk-henkan-key)) ))
	       (skk-set-marker skk-henkan-end-point (point))
	       (and skk-henkan-okurigana (insert-and-inherit skk-henkan-okurigana)) )
	   (insert-and-inherit skk-henkan-key)
	   (skk-set-marker skk-henkan-end-point (point)) )
	 (skk-message "確定アンドゥ！" "Undo kakutei!")
	 (setq skk-henkan-count 1)
	 (skk-henkan) )
     ;; skk-kakutei-undo から途中で抜けた場合は、各種フラグを初期化しておかない
     ;; と次の動作をしようとしたときにエラーになる。
     (error (skk-kakutei))
     (quit (skk-kakutei)) )))
     
(defun skk-set-henkan-point (&optional arg)
  ;;"変換を開始するポイントをマークし、対応する skk-prefix か、母音を入力する。"
  (let* ((last-char (skk-downcase last-command-char))
	 (normal (not (eq last-char last-command-char)))
	 (sokuon (and (string= skk-prefix (char-to-string last-char))
		      (/= last-char ?o)))
	 (henkan-active skk-henkan-active))
    (if (or (not skk-henkan-on) skk-henkan-active)
	(if normal
	    (skk-set-henkan-point-subr)
	  (and skk-henkan-on (skk-set-henkan-point-subr))
	  (if henkan-active
	      (skk-emulate-original-map arg)
	    ;; What's to be here?
	    ;;(skk-self-insert arg)
	    ))
      (if (not normal)
	  (progn			; special char
	    (insert-and-inherit last-char)
	    (skk-set-marker skk-henkan-end-point (point))
	    (setq skk-henkan-count 0
		  skk-henkan-key (buffer-substring-no-properties
				  skk-henkan-start-point (point) )
		  skk-prefix "" )
	    (skk-henkan) )
	;; prepare for the processing of okurigana if not skk-okurigana
	;; and the preceding character is not a numeric character.
	;; if the previous char is a special midashi char or a
	;; numeric character, we assume that the user intended to type the
	;; last-command-char in lower case.
	(if (and (or (not (skk-get-prefix skk-current-rule-tree)) ; for KAnji, KanJIru
		     (and
		      (not (= skk-henkan-start-point skk-kana-start-point))
		      (or sokuon	; for TaSSi or TasSi
			  (skk-kana-cleanup)) )) ; for NEko
		 (not skk-okurigana)
		 (or (= skk-henkan-start-point (point))
		     (let ((p (char-before)))
		       (not
			(or
			 ;; previous char is a special midashi char
			 (memq p skk-special-midashi-char-list)
			 ;; previous char is an ascii numeric char
			 (and (<= ?0 p) (<= p ?9))
			 ;; previous char is a JIS X 0208 numeric char
			  (and (skk-jisx0208-p p)
			       (= (skk-char-octet p 0) 35) ;?#
			       (<= 48 (skk-char-octet p 1)) ; ?0
			       (<= (skk-char-octet p 1) 57) )  ; ?9
			  )))))
	    (if skk-process-okuri-early
		(progn
		  (skk-set-marker skk-henkan-end-point (point))
		  (setq skk-okuri-char (char-to-string last-char))
		  (if sokuon
		      (progn
			(setq skk-henkan-key
			      (concat (buffer-substring-no-properties
				       skk-henkan-start-point
				       skk-kana-start-point )
				      (if skk-katakana "ッ" "っ")
				      skk-henkan-okurigana ))
			(skk-erase-prefix)
			(insert-and-inherit (if skk-katakana "ッ " "っ "))
			(setq skk-prefix ""
			      skk-henkan-count 0 )
			(skk-henkan)
			(delete-backward-char 2) )
		    (setq skk-henkan-key (concat
					  (buffer-substring-no-properties
					   skk-henkan-start-point
					   (point) )
					  skk-okuri-char ))
		    (insert-and-inherit " ")
		    (setq skk-prefix ""
			  skk-henkan-count 0 )
		    (skk-henkan)
		    (delete-backward-char 1) )
		  ;; we set skk-kana-start-point here, since the marker may no
		  ;; longer point at the correct position after skk-henkan.
		  (skk-set-marker skk-kana-start-point (point)) )
	      (if (= skk-henkan-start-point (point))
		  nil
		(if sokuon
		    (progn
		      (skk-erase-prefix 'clean)
		      (insert-and-inherit (if skk-katakana "ッ" "っ")) ))
		(skk-set-marker skk-okurigana-start-point (point))
		(insert-and-inherit "*")
		(skk-set-marker skk-kana-start-point (point))
		(setq skk-okuri-char (char-to-string last-char)
		      skk-okurigana t ))))))
    (if normal
	(progn
	  (setq last-command-char last-char)
	  (skk-kana-input arg) ))))

(defun skk-start-henkan (arg)
  "▽モードでは漢字変換を開始する。▼モードでは次の候補を表示する。
▽モードで、カタカナモードのまま漢字変換を開始すると、見出し語を平仮名に
変換後、漢字変換を開始する。
見出し語の変換せずにそのまま漢字変換を行ないたければ、C-u SPC \(arg が 4
になる\) とタイプする。"
  (interactive "*p")
  (skk-with-point-move
   (cancel-undo-boundary)
   (if skk-henkan-active
       (progn
	 (setq skk-henkan-count (1+ skk-henkan-count))
	 (skk-henkan) )
     (save-match-data
       (let (pos)
	 (skk-kana-cleanup 'force)
	 (and (skk-get-prefix skk-current-rule-tree)
	      ;; Never.  `skk-erase-prefix' called by `skk-kana-cleanup'
	      ;; initializes `skk-prefix'.
	      (skk-error "フィックスされていない skk-prefix があります"
			 "Have unfixed skk-prefix" ))
	 (setq pos (point))
	 (and (< pos skk-henkan-start-point)
	      (skk-error
	       "カーソルが変換開始地点より前にあります"
	       "Henkan end point must be after henkan start point" ))
	 (and skk-katakana (= arg 1)
	      (skk-hiragana-region skk-henkan-start-point pos) )
	 (setq skk-henkan-key (buffer-substring-no-properties
			       skk-henkan-start-point pos ))
	 (and skk-okurigana (string-match "\\* *$" skk-henkan-key)
	      (skk-error
	       "空の送り仮名で漢字を登録しようとしています"
	       "No okurigana!" ))
	 (if skk-allow-spaces-newlines-and-tabs
	     ;; skk-henkan-key の中の "[ \n\t]+" を完全に取り除く。
	     (while (string-match "[ \n\t]+" skk-henkan-key)
	       (setq skk-henkan-key
		     (concat (substring skk-henkan-key 0 (match-beginning 0))
			     (substring skk-henkan-key (match-end 0)) )))
	   (skk-save-point
	    (beginning-of-line)
	    (and (> (point) skk-henkan-start-point)
		 (skk-error
		  "変換キーに改行が含まれています"
		  "Henkan key may not contain a new line character" )))
	   ;; 最初のスペースで skk-henkan-key をちょん切るだけ。
	   (setq skk-henkan-key (substring skk-henkan-key 0
					   (string-match " "
							 skk-henkan-key ))))
	 (skk-set-marker skk-henkan-end-point pos)
	 (setq skk-henkan-count 0)
	 (skk-henkan)
	 (if (and skk-abbrev-mode skk-henkan-active)
	     (progn
	       (skk-j-mode-on)
	       (setq skk-abbrev-mode t) )))))))

(defun skk-auto-start-henkan (str)
  ;; skk-auto-start-henkan-keyword-list の要素の文字列を挿入したときに自動的に 
  ;; (スペースを打鍵しなくとも) 変換を開始する。エー×イソフト社の MSDOS 用 の 
  ;; FEP、WX2+ 風。
  (and (member str skk-auto-start-henkan-keyword-list)
       (skk-save-point
        (backward-char 1)
        (and (> (point) skk-henkan-start-point)
	     (let ((skk-prefix ""))
	       (skk-start-henkan (prefix-numeric-value current-prefix-arg)) )))))

(defun skk-backward-and-set-henkan-point (arg)
  "ポイントの直前にある文字列の先頭に変換開始ポイントを示す \"▽\" を付ける。
カーソルの直前にある文字 \(スペース文字、タブ文字、長音を表わす「ー」 は無条件
にスキップされる\) を skk-what-char-type にて判別し、同種の文字列をひとかたま
りとして後方へスキップする。
但し、ひらかなの場合は「を」の直前で、カタカナの場合は「ヲ」の直前で止まる。
C-u ARG で ARG を与えると、その文字分だけ戻って同じ動作を行なう。"
  (interactive "*P")
  (if (not skk-mode)
      (skk-emulate-original-map arg)
    (catch 'exit1
      (skk-save-point
       ;; とりあえず最初の SPC, TAB, 全角 SPC だけジャンプする。
       (skip-chars-backward " \t　")
       ;; 引数あり。
       (if arg
	   (if (not skk-allow-spaces-newlines-and-tabs)
	       (backward-char (prefix-numeric-value arg))
	     (setq arg (prefix-numeric-value arg))
	     (while (> arg 0)
	       (skip-chars-backward " \t　")
	       (if (bolp)
		   ;; 行頭だったら一行前の行末まで戻るが、arg は減らさない。
		   (backward-char 1)
		 (backward-char 1)
		 (setq arg (1- arg)) )))
	 ;; 引数なし。
	 (let ((limit
		(if (not skk-allow-spaces-newlines-and-tabs)
		    (skk-save-point (beginning-of-line) (point))
		  (point-min) ))
	       ;; ＿￣＾¨｀´゜゛！？；：・．，。
	       (unknown-chars-regexp
		(if skk-allow-spaces-newlines-and-tabs
		    "[ 　\n\tー〃ゞゝヾヽ]"
		  "[　ー〃ゞゝヾヽ]" ))
	       type p )
	   (save-match-data
	     (skk-save-point
	      (backward-char 1)
	      (while (and (> (point) limit)
			  ;; unknown-chars-regexp では文字種別が判別できないの
			  ;; で、その文字列が続く限りポイントをバッファの先頭
			  ;; 方向へ戻す。
			  (looking-at unknown-chars-regexp) )
		(backward-char 1) )
	      (setq type (skk-what-char-type))
	      (if (eq type 'unknown)
		  (throw 'exit1 nil)
		(skk-backward-and-set-henkan-point-1 type)
		(setq p (point))
		(if skk-allow-spaces-newlines-and-tabs
		    (while (and (> (point) limit) (bolp))
		      ;; 1 行上の行末へ。
		      (backward-char 1)
		      ;; ポイントが判別できない文字種別の上にある間は 
		      ;; backward 方向へポイントを戻す。
		      ;;(while (and (> (point) limit)
		      ;;            (looking-at unknown-chars-regexp) )
		      ;;  (backward-char 1) )
		      (if;;(or
			  (> 0 (skk-backward-and-set-henkan-point-1 type))
			  ;;(eq (skk-what-char-type) type))
			  (setq p (point)) ))))))
	   (goto-char p)
	   (skip-chars-forward unknown-chars-regexp) ))
       (skk-set-henkan-point-subr) ))))

(defun skk-backward-and-set-henkan-point-1 (type)
  ;; skk-backward-and-set-henkan-point のサブルーチン。CHAR の種類に応じた文字
  ;; をスキップしてバッファの先頭方向へ戻る。
  (cond ((eq type 'hiragana)
         ;; "を" の前で止まった方が便利？
         (skip-chars-backward "ヽヾゝゞ〃ーんぁ-ゑ") )
        ((eq type 'katakana)
         ;; "ヲ" の前で止まった方が便利？
         (skip-chars-backward "ヽヾゝゞ〃ーンァ-ヱ") )
        ((eq type 'jisx0208-latin)
         (skip-chars-backward "　-ｚ") )
        ((eq type 'ascii)
         (skip-chars-backward " -~") )))

(defun skk-what-char-type ()
  ;; 現在のポイントにある文字がどんな種類かを判別する。
  (save-match-data
    (cond ((looking-at "[ぁ-ん]") 'hiragana)
          ((looking-at "[ァ-ン]") 'katakana)
          ;; "ー" を除外している ("ー" は "〇" と "―" の間に入っている)。
          ((looking-at "[　-〇―-ｚ]") 'jisx0208-latin)
          ((looking-at "[ -~]") 'ascii)
          (t 'unknown) )))

(defun skk-set-henkan-point-subr (&optional arg)
  "かなを入力した後で、ポイントに変換開始のマーク \(▽\) を付ける。
元々はこの関数は skk-set-henkan-point の内部関数である。"
  (interactive "*P")
  (skk-with-point-move
   (cancel-undo-boundary)
   (if skk-henkan-on (skk-kakutei)
     (skk-kana-cleanup) );; XXX
   (if (not (skk-get-prefix skk-current-rule-tree))
       (insert-and-inherit "▽")
     (skk-erase-prefix)
     (insert-and-inherit "▽")
     (skk-set-marker skk-kana-start-point (point))
     (skk-insert-prefix) )
   (setq skk-henkan-on t)
   (skk-set-marker skk-henkan-start-point (point)) ))

(defun skk-change-marker ()
  ;; "▽"を"▼"に変える。skk-henkan-active フラグを t にする。
  (skk-save-point
   (goto-char (- skk-henkan-start-point skk-kanji-len))
   (if (looking-at "▽")
       (progn
	 (cancel-undo-boundary)
	 (let ((buffer-undo-list t))
	     (insert-and-inherit "▼")
	     (delete-char 1) )
	 (setq skk-henkan-active t) )
     (skk-kakutei)
     (skk-error "▽がありません" "It seems that you have deleted ▽") )))

(defun skk-change-marker-to-white ()
  ;; "▼"を"▽"に変える。skk-henkan-active フラグを nil にする。
  (skk-save-point
   (goto-char (- skk-henkan-start-point skk-kanji-len))
   (cancel-undo-boundary)
   (if (looking-at "▼")
       (let ((buffer-undo-list t))
	 (insert-and-inherit "▽")
	 (delete-char 1) )
     (goto-char skk-henkan-start-point)
     (insert-and-inherit "▽")
     (skk-set-marker skk-henkan-start-point (point))
     (skk-message "▼がありません" "It seems that you have deleted ▼") )
   (setq skk-henkan-active nil) ))

(defun skk-delete-henkan-markers (&optional nomesg)
  ;; 変換時にカレントバッファに表われる `▽', `▼' マークを消す。
  (if (not (marker-position skk-henkan-start-point))
      nil
    (save-match-data
      (skk-save-point
       (goto-char (- skk-henkan-start-point skk-kanji-len))
       (if skk-henkan-active
	   (progn
	     (and skk-use-face (skk-henkan-face-off))
	     (if (looking-at "▼")
		 (delete-char 1)
	       (or nomesg
		   (skk-message "▼がありません"
				"It seems that you have deleted ▼" ))))
	 (if (looking-at "▽")
	     (delete-char 1)
	   (or nomesg
	       (skk-message "▽がありません"
			    "It seems that you have deleted ▽" ))))))))

(defun skk-delete-okuri-mark ()
  ;; 送り仮名入力中にカレントバッファに表われる `*' マークを消し、送り仮名関連
  ;; フラグを nil にセットする。
  (if (or (not skk-okurigana)
	  (not skk-okurigana-start-point)
	  (not (markerp skk-okurigana-start-point))
	  (not (marker-position skk-okurigana-start-point)) )
      nil
    (skk-save-point
      (and (eq (char-after skk-okurigana-start-point) ?*) ; ?*
	   (delete-region skk-okurigana-start-point
			  (1+ skk-okurigana-start-point) ))
      (setq skk-okurigana nil
            skk-okuri-char nil
            skk-henkan-okurigana nil ))))
            
;;;; jisyo related functions
(defun skk-purge-from-jisyo (&optional arg)
  "▼モードで現在の候補を辞書バッファから消去する。"
  (interactive "*P")
  (skk-with-point-move
   (if (and skk-henkan-active (not (string= skk-henkan-key "")))
       (if (not
	    (yes-or-no-p (format
			  (if skk-japanese-message-and-error
			      "%s /%s/%sを辞書から削除します。良いですか？"
			    "Really purge \"%s /%s/%s\"?" )
			  skk-henkan-key (skk-get-current-candidate-simply)
			  (if (and skk-henkan-okurigana
				   (or skk-henkan-okuri-strictly
				       skk-henkan-strict-okuri-precedence ))
			      (concat
			       (if skk-japanese-message-and-error
				   " (送り仮名: "
				 "(okurigana: " )
			       skk-henkan-okurigana
			       ") " )
			    " " ))))
	   nil
	 ;; skk-henkan-start-point から point まで削除してしまっても、変換直後
	 ;; に (カーソルを動かすことなく) skk-purge-from-jisyo を呼べば問題ない
	 ;; が、カーソルが違う場所へ移動していた場合は、削除すべきでないものま
	 ;; で削除してしまう可能性がある。そこで、送り仮名があればその長さを含
	 ;; めた end を求め、今回の変換に関連した個所だけを正確に切り取るように
	 ;; する。
	 (let ((end (if skk-henkan-okurigana (+ (length skk-henkan-okurigana)
						skk-henkan-end-point )
		      skk-henkan-end-point ))
	       (word (skk-get-current-candidate-simply (skk-numeric-p))) )
	   (skk-update-jisyo word 'purge)
	   ;; Emacs 19.28 だと Overlay を消しておかないと、次に insert される
	   ;; skk-henkan-key に何故か Overlay がかかってしまう。
	   (and skk-use-face (skk-henkan-face-off))
	   (delete-region skk-henkan-start-point end)
	   (skk-change-marker-to-white)
	   (skk-kakutei) )))))

(defun skk-save-jisyo (&optional quiet)
  "SKK の辞書バッファをセーブする。
  オプショナル引数の QUIET が non-nil であれば、辞書セーブ時のメッセージを出さな
  い。"
  (interactive "P")
  (funcall skk-save-jisyo-function quiet) )

(defun skk-save-jisyo-original (&optional quiet)
  ;;"SKK の辞書バッファをセーブする。
  ;;オプショナル引数の QUIET が non-nil であれば、辞書セーブ時のメッセージを出さな
  ;;い。"
  (let* ((skk-jisyo (expand-file-name skk-jisyo))
         (jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (if (or (not jisyo-buffer) (not (buffer-modified-p jisyo-buffer)))
        (if (not quiet) 
            (progn
	      (skk-message "SKK 辞書を保存する必要はありません"
                           "No need to save SKK jisyo" )
              (sit-for 1) ))
      (with-current-buffer jisyo-buffer
        (let ((inhibit-quit t)
              (tempo-file (skk-make-temp-jisyo)) )
          (if (not quiet)
              (skk-message "SKK 辞書を保存しています..."
                           "Saving SKK jisyo..." ))
          (skk-save-jisyo-1 tempo-file)
          (skk-check-size-and-do-save-jisyo tempo-file)
          ;; 辞書のセーブに成功して初めて modified フラッグを nil にする。
          (set-buffer-modified-p nil)
	  (setq skk-update-jisyo-count 0)
          (if (not quiet)
              (progn
                (skk-message "SKK 辞書を保存しています...完了！"
                             "Saving SKK jisyo...done" )
                (sit-for 1) ))
          (and (eq this-command 'save-buffers-kill-emacs)
	       (skk-record-jisyo-data) ))))))

(defun skk-save-jisyo-1 (file)
  (save-match-data
    (let (buffer-read-only)
      (goto-char (point-min))
      (if (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
          nil
        (skk-error
         "送りありエントリのヘッダーがありません！ SKK 辞書のセーブを中止します"
         "Header line for okuri-ari entries is missing!  Stop saving SKK jisyo" ))
      ;; おっ、コメントフェイスが $ で終わらないぞ > hilit19.el
      (if (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
          nil
        (skk-error
         "送りなしエントリのヘッダーがありません ！ SKK 辞書のセーブを中止します"
         "Header line for okuri-nasi entries is missing!  Stop saving SKK jisyo" )))
    (write-region-as-coding-system
     (cond ((and skk-jisyo-code
		 (or (coding-system-p skk-jisyo-code)
		     (and (fboundp 'find-coding-system)
			  (find-coding-system skk-jisyo-code) )))
	    skk-jisyo-code )
	   ((and skk-jisyo-code (stringp skk-jisyo-code))
	    (cdr (assoc skk-jisyo-code skk-coding-system-alist)) )
	   (t (cdr (assoc "euc" skk-coding-system-alist))) )
     1 (point-max) file nil 'nomsg )))

(defun skk-check-size-and-do-save-jisyo (new-file)
  (let ((new-size (nth 7 (file-attributes new-file)))
        old-size )
    (if (= new-size 0)
        (progn
          (delete-file new-file)
          (skk-error "SKK 辞書が空になっています！ 辞書のセーブを中止します"
                     "Null SKK jisyo!  Stop saving jisyo" )))
    (if (or (not skk-compare-jisyo-size-when-saving)
            ;; 旧辞書とのサイズ比較を行なわない。
            (progn
              ;; (1)skk-jisyo がないか、
              ;; (2)new-file と skk-jisyo が同一のサイズか
              ;;    (skk-(aux-)large-jisyo から新規の単語を読み込まなかったり、
              ;;    新規単語の登録を行なわなかった場合はサイズが同じ)、
              ;; (3)new-file の方が大きい
              ;; 場合 (上記の 3 通りであればいずれも正常)。
              (setq old-size (nth 7 (file-attributes skk-jisyo)))
              (or (not old-size)
                  (>= new-size old-size) )))
        (skk-make-new-jisyo new-file)
      ;; yes-or-no-p に回答し、newline すると、this-command が変ってしまう。
      (let (this-command this-command-char last-command last-command-char)
        (if (skk-yes-or-no-p
             (format
              "skk-jisyo が %dbytes 小さくなりますが、セーブして良いですか？"
              (- old-size new-size) )
             (format
              "New %s will be %dbytes smaller.  Save anyway?"
              skk-jisyo (- old-size new-size) ))
            ;; とにかくセーブ。
            (skk-make-new-jisyo new-file)
          ;; セーブとり止め。
          (delete-file new-file)
          (with-output-to-temp-buffer "*SKK warning*"
            (if skk-japanese-message-and-error
                (progn
                  (princ "セーブしようとする辞書のサイズが元のものよりも小さなってしまうので、")
                  (terpri)
                  (princ "セーブを途中で中止しました。辞書のサイズが小さくなった原因には例え")
                  (terpri)
                  (princ "ば、")
                  (terpri)
                  (terpri)
                  (princ "    ・M-x skk-purge-from-jisyo を実行した。")
                  (terpri)
                  (terpri)
                  (princ "    ・.skk-jisyo の漢字コードと、\" *.skk-jisyo*\" バッファの漢字コード")
                  (terpri)
                  (princ "      が異なっている。")
                  (terpri)
                  (terpri)
                  (princ "    ・\" *.skk-jisyo*\" バッファを自分で編集した。")
                  (terpri)
                  (terpri)
                  (princ "などが考えられます (最初の 2 つが原因であれば、異常ではありません。")
                  (terpri)
                  (princ "最後の場合は、あなたがどのような編集をしたかによります)。原因を確認")
                  (terpri)
                  (princ "後、慎重に辞書のセーブを行なうことをお勧めします。")
                  (terpri)
                  (terpri)
                  (princ "元の辞書を再度読み込むには、")
                  (terpri)
                  (terpri)
                  (princ "    M-x skk-reread-private-jisyo")
                  (terpri)
                  (terpri)
                  (princ "を実行して下さい。") )
              (princ "As size of your private JISYO to be saved is smaller than the")
              (terpri)
              (princ "original, we have stopped saving JISYO.  For example, the following")
              (terpri)
              (princ "condition makes a smaller private JISYO;")
              (terpri)
              (terpri)
              (princ "    (a)You executed M-x skk-purge-from-jisyo,")
              (terpri)
              (terpri)
              (princ "    (b)Kanji code of .skk-jisyo is different from the one of")
              (terpri)
              (princ "       \" *.skk-jisyo*\" buffer, or")
              (terpri)
              (terpri)
              (princ "    (c)You edited \" *.skk-jisyo*\" buffer manually.")
              (terpri)
              (terpri)
              (princ "The first two conditions are not strange, but the last one depends on")
              (terpri)
              (princ "how you edited JISYO.  We strongly recommend to save JISYO")
              (terpri)
              (princ "carefully after checking what causes this.")
              (terpri)
              (princ "If you want to reread your original private JISYO, type")
              (terpri)
              (terpri)
              (princ "    M-x skk-reread-private-jisyo")
              (terpri) ))
          (skk-error "SKK 辞書のセーブを中止しました！"
                     "Stop saving SKK jisyo!" ))))))

(defun skk-make-temp-jisyo ()
  ;; SKK 個人辞書保存のための作業用のファイルを作り、ファイルのモードを
  ;; skk-jisyo のものと同じに設定する。作った作業用ファイルの名前を返す。
  (let ((tempo-name (skk-make-temp-file "skkdic")))
    (skk-create-file tempo-name)
    ;; temporary file に remote file を指定することなど有り得ない？
    ;;(if (or 
    ;;     ;; XEmacs has efs.el
    ;;     (eq skk-emacs-type 'xemacs)
    ;;     ;; ange-ftp.el does not have a wrapper to set-file-modes.
    ;;     (not (and (featurep 'ange-ftp) (boundp 'ange-ftp-name-format)
    ;;               (string-match (car ange-ftp-name-format) tempo-name) )))
    (set-file-modes tempo-name  (file-modes skk-jisyo))
    ;;)
    tempo-name ))

(defun skk-make-temp-file (prefix)
  (let ((dir
	 (cond ((skk-file-exists-and-writable-p temporary-file-directory)
		(expand-file-name temporary-file-directory) )
	       ((and (memq system-type '(ms-dos windows-nt))
		     (skk-file-exists-and-writable-p "a:/temp") )
		;; NEC PC-9800 series.
		"a:/temp" )
	       (t (or (file-exists-p "~/tmp") (make-directory "~/tmp"))
		  (or (file-writable-p "~/tmp") (set-file-modes "~/tmp" 1023))
		  "~/tmp" ))))
    (make-temp-name
     (concat dir
	     (if (memq (skk-str-ref dir (1- (length dir)) ) '(?/ ?\\))
		 "" "/" )
	     prefix ))))

(defun skk-make-new-jisyo (tempo-file)
  ;; TEMPO-FILE を新規の skk-jisyo にする。skk-backup-jisyo が non-nil だった
  ;; らバックアップ辞書を作る。
  (if skk-backup-jisyo
      (progn
        (if (file-exists-p skk-backup-jisyo)
            (delete-file skk-backup-jisyo) )
        (rename-file skk-jisyo skk-backup-jisyo) )
    (delete-file skk-jisyo) )
  (rename-file tempo-file skk-jisyo 'ok-if-already-exists) )

(defun skk-reread-private-jisyo (&optional force)
  "バッファに読み込んだ個人辞書を破棄し、ファイルからバッファへ再読み込みする。
オプショナル引数の FORCE が non-nil であれば、破棄の確認をしない。"
  (interactive "P")
  (let ((buf (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (if (and buf
             (or force
                 (skk-yes-or-no-p "編集中の個人辞書を破棄しますか？"
                                  "Discard your editing private JISYO?" )))
        (progn
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (kill-buffer buf) )
          (or
           (skk-get-jisyo-buffer skk-jisyo 'nomsg)
           (skk-error "個人辞書を再読み込みすることができません！"
                      "Cannot reread private JISYO!" ))))))

(defun skk-record-jisyo-data ()
  ;; 辞書データを取り、Emacs の終了の際であれば、そのデータを 
  ;; skk-record-file に保存し、それ以外であれば、それをエコーする。
  (if (or (not skk-keep-record) (> 1 skk-kakutei-count))
      nil
    (with-temp-file skk-record-file
      (insert-file-contents skk-record-file)
      (goto-char (point-min))
      (insert
       (format
        "%s  登録: %3d  確定: %4d  確定率: %3d%%  語数:%6d\n"
        (current-time-string)
        skk-touroku-count skk-kakutei-count
        (/ (* 100 (- skk-kakutei-count skk-touroku-count))
           skk-kakutei-count )
        (cond ((featurep 'skk-rdbms)
	       ;; RDBMS を使えばもっと興味深い統計が取れるかもしれない
	       ;; が、とりあえず語数だけ数えて入れておく。
	       (skk-rdbms-count-jisyo-candidates skk-rdbms-private-jisyo-table) )
	      (skk-count-private-jisyo-candidates-exactly
	       (skk-count-jisyo-candidates (expand-file-name skk-jisyo)) )
	       ;; 1 行 1 候補とみなす。
	      (t (with-current-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)
		   (- (count-lines (point-min) (point-max)) 2) ))))))
    (setq skk-touroku-count 0 skk-kakutei-count 0) ))

(defun skk-count-jisyo-candidates (file-or-table)
  "SKK 辞書の候補数を数える。"
  (interactive
   (list (cond ((eq skk-count-jisyo-candidates-function
		    'skk-count-jisyo-candidates-original )
		(read-file-name
		 (format "Jisyo file: (default: %s) " skk-jisyo)
		 "~/" skk-jisyo 'confirm ))
	       ((eq skk-count-jisyo-candidates-function
		    'skk-rdbms-count-jisyo-candidates )
		;; データベースファイルを直接ファイル名で指定できる
		;; permission がない場合が多いよね...。
		;;(read-file-name
		;; (format "Jisyo table: (default: %s) "
		;;	 skk-rdbms-private-jisyo-table ))
		skk-rdbms-private-jisyo-table ))))
  ;; mule@emacs19.31 だと下記のようにすると (`ァ' が原因のよう) 何故か 
  ;; default-directory の末尾に改行が付く。
  ;; 通常は気が付かないが、rsz-mini.el を使って resize-minibuffer-mode を 
  ;; non-nil にしていると不要な 2 行目が出現する。
  ;; (interactive "f辞書ファイル: ")
  (let ((count (funcall skk-count-jisyo-candidates-function file-or-table)))
    (if (interactive-p)
	(message "%d entries" count)
      count )))

(defun skk-count-jisyo-candidates-original (file)
  ;;"SKK 辞書の候補数を数える。
  ;;`[' と `]' に囲まれた送り仮名毎のブロック内は数えない。"
  (with-current-buffer (find-file-noselect file)
    (save-match-data
      (let ((count 0)
            (min (point-min))
            (max (and (interactive-p) (point-max)))
            (interactive-p (interactive-p)) )
        (goto-char min)
        (if (or
             ;; こちらは skk-save-point を使わず、ポイントを移動させる。
             (not (re-search-forward "^;; okuri-ari entries.$" nil t nil))
             (not
              (skk-save-point
                (re-search-forward "^;; okuri-nasi entries.$" nil t nil) )))
            (skk-error "このファイルは SKK 辞書ではありません"
                       "This file is not a SKK dictionary") )
        (while (search-forward "/" nil t)
          (cond ((looking-at "\\[")
                 (forward-line 1)
                 (beginning-of-line) )
                ((not (eolp))
                 (setq count (1+ count)) ))
          (if interactive-p
              (message "Counting jisyo candidates...%3d%% done"
                       (/ (* 100 (- (point) min)) max) )))
	count ))))

(defun skk-create-file (file &optional japanese english)
  ;; FILE がなければ、FILE という名前の空ファイルを作る。
  ;; オプショナル引数の JAPANESE/ENGLISH を指定すると、ファイル作成後そのメッセ
  ;; ージをミニバッファに表示する。
  (let ((file (expand-file-name file)))
    (or (file-exists-p file)
	(progn
	  (write-region 1 1 file nil 0)
	  (if (or japanese english)
	      (progn
 		(message (if skk-japanese-message-and-error
 			     japanese english ))
		(sit-for 3) ))))))

(defun skk-get-jisyo-buffer (file &optional nomsg)
  ;; FILE を開いて SKK 辞書バッファを作り、バッファを返す。
  ;; オプショナル引数の NOMSG を指定するとファイル読み込みの際のメッセージを
  ;; 表示しない。
  (if file
      (let ((inhibit-quit t)
            (jisyo-buf (concat " *" (file-name-nondirectory file)
                               "*" )))
        ;; 辞書バッファとしてオープンされているなら、何もしない。
        (or (get-buffer jisyo-buf)
            (with-current-buffer (setq jisyo-buf (get-buffer-create jisyo-buf))
	      (setq file (expand-file-name file))
              (buffer-disable-undo jisyo-buf)
              (auto-save-mode -1)
              ;; ワーキングバッファのモードラインはアップデートされない？
              ;;(make-local-variable 'line-number-mode)
              ;;(make-local-variable 'column-number-mode)
              ;;(setq column-number-mode nil
              ;;      line-number-mode nil )
              (setq buffer-read-only nil
                    case-fold-search nil
                    ;; buffer-file-name を nil にしておくと M-x compile など
		    ;; 内部で save-some-buffers をコールしているコマンドを
		    ;; 使ったときでもセーブするかどうかを尋ねてこなくなる。
                    ;; buffer-file-name file
                    ;; cache-long-line-scans nil
                    ;; dabbrev のサーチとなるバッファにならないように存在しな
                    ;; いモード名にしておく。実害のある副作用はないはず。
                    major-mode 'skk-jisyo-mode
                    mode-name "SKK dic" )
              (or nomsg
                  (skk-message "SKK 辞書 %s をバッファに読み込んでいます..."
                               "Inserting contents of %s ..."
                               (file-name-nondirectory file) ))
	      (let (enable-character-translation enable-character-unification)
		(insert-file-contents-as-coding-system
		 (cond ((and skk-jisyo-code
			     (or (coding-system-p skk-jisyo-code)
				 (and (fboundp 'find-coding-system)
				      (find-coding-system skk-jisyo-code) )))
			skk-jisyo-code )
		       ((and skk-jisyo-code (stringp skk-jisyo-code))
			(cdr (assoc skk-jisyo-code skk-coding-system-alist)) )
		       (t (cdr (assoc "euc" skk-coding-system-alist))) )
		 file ))
              (or nomsg
                  (skk-message
                   "SKK 辞書 %s をバッファに読み込んでいます...完了！"
                   "Inserting contents of %s ...done"
                   (file-name-nondirectory file) ))
              (skk-setup-jisyo-buffer)
              (set-buffer-modified-p nil)
              jisyo-buf )))))

(defun skk-setup-jisyo-buffer ()
  ;; skk-jisyo の辞書バッファで、
  ;; (1)空バッファであれば、新しくヘッダーを作り、
  ;; (2)辞書エントリがある既存の辞書バッファならば、ヘッダーが正しいかどうかを
  ;;    チェックする。
  ;;
  ;; skk-okuri-ari-min と skk-okuri-nasi-min の位置を変更した。
  ;;                       ↓ 新しい skk-okuri-ari-min
  ;;   ;; okuri-ari entries.
  ;;   ← 以前の skk-okuri-ari-min
  ;;
  ;;   ↓ skk-okuri-ari-max ↓ 新しい skk-okuri-nasi-min
  ;;   ;; okuri-nasi entries.
  ;;   ← 以前の skk-okuri-nasi-min
  ;;
  ;;
  ;; 変更前の位置であれば、下記のような空辞書の場合、
  ;;
  ;;   ;; okuri-ari entries.
  ;;   ;; okuri-nasi entries.
  ;;
  ;; skk-okuri-ari-min と skk-okuri-ari-max のマーカーが重なってしまい、
  ;; skk-okuri-ari-min の位置に挿入したエントリが skk-okuri-ari-max のマーカー
  ;; を後方に押しやらない。
  ;;
  ;; この関数のオリジナルの名称は、j-check-jisyo だったが、skk-check-jisyo と
  ;; いう名前にすると skk-tools.el 内の関数名と重複する。
  ;; case-fold-search は、辞書バッファでは常に nil。
  (save-match-data
    (if (= (buffer-size) 0)
	;; 空バッファだったら、ヘッダーのみ挿入。
	(insert ";; okuri-ari entries.\n" ";; okuri-nasi entries.\n") )
    (goto-char (point-min))
    (if (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
	;; 固定ポイントなので、(point) で十分。
	(setq skk-okuri-ari-min (point))
      (skk-error "送りありエントリのヘッダーがありません！"
		 "Header line for okuri-ari entries is missing!" ))
    (if (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
	(progn
	  (beginning-of-line)
	  ;; 共有辞書なら固定ポイントでも良いのだが、辞書バッファで編集を行
	  ;; なったときのことを配慮してマーカーにしておく。
	  (setq skk-okuri-ari-max (point-marker))
	  (forward-line 1)
	  (backward-char 1)
	  (setq skk-okuri-nasi-min (point-marker)) )
      (skk-error "送りなしエントリのヘッダーがありません！"
		 "Header line for okuri-nasi entries is missing!" ))))

(defun skk-search ()
  ;; skk-current-search-prog-list の要素になっているプログラムを評価して、
  ;; skk-henkan-keyをキーにして検索を行う。
  (let (l)
    (while (and (null l) skk-current-search-prog-list)
      (setq l (eval (car skk-current-search-prog-list))
	    skk-current-search-prog-list (cdr skk-current-search-prog-list) ))
    l ))

(defun skk-search-jisyo-file (file limit &optional nomsg)
  ;; SKK 辞書フォーマットの FILE で skk-henkan-key をキーにして検索を行う。
  ;; 検索リージョンが LIMIT 以下になるまでバイナリサーチを行い、その後リニア
  ;; サーチを行う。
  ;; LIMIT が 0 であれば、リニアサーチのみを行う。
  ;; 辞書がソートされていないのであれば、LIMIT を 0 する必要がある。
  ;; オプショナル引数の NOMSG が non-nil であれば skk-get-jisyo-buffer のメッ
  ;; セージを出力しないようにする。
  (let ((jisyo-buffer (skk-get-jisyo-buffer file nomsg)))
    (if jisyo-buffer
        ;; skk-henkan-key と skk-henkan-okurigana はカレントバッファのローカル
        ;; 値。
        (let ((okurigana (or skk-henkan-okurigana skk-okuri-char))
              (midasi 
               (if skk-use-numeric-conversion
		   ;; skk-henkan-key が nil のことがある。何故?
                   (skk-num-compute-henkan-key skk-henkan-key)
                 skk-henkan-key ))
	      (henkan-buffer (current-buffer))
              entry-list entry )
          (with-current-buffer jisyo-buffer
            (setq skk-henkan-key midasi
                  entry-list (skk-search-jisyo-file-1 okurigana limit) )
            (if entry-list
                (progn
                  (setq entry
                        (cond ((and okurigana skk-henkan-okuri-strictly)
                               ;; 送り仮名が同一のエントリのみを返す。
                               (nth 2 entry-list) )
                              ((and okurigana skk-henkan-strict-okuri-precedence)
                               ;; 送り仮名が同一のエントリのうしろに、
                               ;; その他のエントリをつけてかえす。
                               (skk-nunion (nth 2 entry-list) (car entry-list)))
                              (t (car entry-list)) ))
		  (and skk-search-end-function
		       (setq entry (funcall skk-search-end-function
					    henkan-buffer midasi okurigana entry )) )
		  entry )))))))

(defun skk-search-jisyo-file-1 (okurigana limit &optional delete)
  ;; skk-search-jisyo-file のサブルーチン。skk-compute-henkan-lists を使用し、
  ;; 見出し語についてのエントリの情報を返す。
  ;; DELETE が non-nil であれば、MIDASI にマッチするエントリを削除する。
  (let ((key (concat "\n" skk-henkan-key " /"))
        min max size p )
    (save-match-data
      ;; skk-okuri-ari-min と skk-okuri-ari-max は辞書バッファのローカル値。
      (if okurigana
          (setq min skk-okuri-ari-min
                max skk-okuri-ari-max )
        (setq min skk-okuri-nasi-min
              max (point-max) ))
      (if (> limit 0)
          (while (progn (setq size (- max min)) (> size limit))
            (goto-char (+ min (/ size 2)))
            (beginning-of-line)
            (setq p (point))
            ;; 送りありなら逆順に比較を行なう。
            (if
                (if okurigana
                    (string< (buffer-substring-no-properties
			      p (1- (search-forward  " ")) )
                             skk-henkan-key )
                  (string< skk-henkan-key
                           (buffer-substring-no-properties
			    p (1- (search-forward " "))) ))
                (setq max p)
              (setq min p) )))
      (goto-char min)
      ;; key が検索開始地点にあった場合でも検索可能なように一文字戻る。key が
      ;; その先頭部分に "\n" を含んでいることに注意。
      (or (bobp) (backward-char 1))
      ;; case-fold-search は、辞書バッファでは常に nil。
      (if (search-forward key max 'noerror)
	  (prog1
	      (skk-compute-henkan-lists okurigana)
	    (if delete
		(progn
		  (beginning-of-line)
		  (delete-region (point)
				 (progn (forward-line 1) (point)) ))))))))


(defun skk-compute-henkan-lists (okurigana)
  ;; 辞書エントリを 4 つのリストに分解する。
  ;;
  ;; 送りなし (例えば、辞書エントリ "てんさい /転載/天災/天才/" の処理)
  ;; entry1 := ("転載" "天災" "天才") == 全エントリ
  ;; entry2 := nil
  ;; entry3 := nil
  ;; entry4 := nil
  ;;
  ;; 送りあり (例えば、「泣く」の変換を行った場合の、辞書エントリ
  ;;           "なk /亡/無/鳴/泣/[く/無/鳴/泣/]/[き/亡/]/" の処理)
  ;; entry1 := ("亡" "無" "鳴" "泣")  == 漢字部分の全エントリ
  ;; entry2 := ("[く")                == 他の送り仮名を使う漢字エントリ (あれ
  ;;                                     ば) + 今回の変換の送り仮名部分
  ;; entry3 := ("無" "鳴" "泣")       == 今回の変換の送り仮名を使う可能性の
  ;;                                     ある全漢字エントリ
  ;; entry4 := ("]" "[き" "亡" "]")   == 他の送り仮名を使う漢字エントリ (残
  ;;                                     り。あれば)
  ;;
  ;;   * "[" は直後に続くひらがなを送り仮名に持つ漢字のエントリの初まりを表し、
  ;;     "]" は、該当の送り仮名グループの終りを示す。
  ;;
  ;; この関数は、変換時と、確定直後の辞書のアップデート時の 2 度呼ばれる
  ;; (変換時に検索を行った辞書が、skk-jisyo とは限らないので、2 度計算せざる
  ;; を得ない)。
  ;;
  ;; 変換時は、skk-henkan-okuri-strictly が non-nil であれば、
  ;; 計算結果の entry3を、skk-henkan-okuri-strictly が nil であって
  ;; かつ skk-henkan-strict-okuri-precedence が non-nil あれば
  ;; (skk-nunion entry3 entry1) を取り出す。
  ;; ふたつの変数がともに nil の場合は entry1 を取り出す。
  (if (not okurigana)
      (list (split-string (buffer-substring-no-properties
			   (point) (progn (end-of-line) (1- (point))) )
			  "/" ) nil nil nil )
    (save-match-data
      (let ((stage 1) (q1 (queue-create)) (q2 (queue-create))
            (q3 (queue-create)) (q4 (queue-create))
            (okuri-key (concat "\[" okurigana)) item headchar )
        (catch 'exit
          (while (not (eolp))
            (setq item (buffer-substring-no-properties
			(point)
			(1- (search-forward "/")) )
                  headchar (if (string= item "") (int-char 0) (skk-str-ref item 0)) )
            (cond ((and (eq headchar ?\[) (<= stage 2))
                   (if (string= item okuri-key)
                       (progn (queue-enqueue q2 item)
                              (setq stage 3) )
                     (setq stage 2)
                     (queue-enqueue q2 item) ))
                  ((= stage 1)
                   (queue-enqueue q1 item) )
                  ((= stage 2)
                   (queue-enqueue q2 item) )
                  ((= stage 3)
                   (if (eq headchar ?\]) ; ?\]
                       (progn (setq stage 4)
                              (queue-enqueue q4 item) )
                     (queue-enqueue q3 item) ))
                  ((= stage 4)
                   (queue-enqueue q4 item) ))))
        ;;        entry1          entry2        entry3          entry4
        (list (queue-all q1) (queue-all q2) (queue-all q3) (queue-all q4)) ))))

(defun skk-nunion (x y)
  ;; X と Y の和集合を作る。等しいかどうかの比較は、equal で行われる。X に Y
  ;; を破壊的に連接する。
  (cond ((null x) y)
        ((null y) x)
        (t (let ((list2 y))
	     (while list2
	       (let* ((list1 (cons nil x))
		      (oldlist1 list1) )
		 (catch 'found
		   (while (cdr list1)
		     (if (equal (car (cdr list1)) (car list2))
			 (throw 'found nil)
		       (setq list1 (cdr list1)) ))
		   (setcdr list1 (list (car list2)))
		   (setq x (cdr oldlist1) ))
	       (setq list2 (cdr list2)) )
	       ))
	   x )))

(defun skk-search-kakutei-jisyo-file (file limit &optional nomsg)
  ;; 辞書ファイルを探し、候補をリストで返す。
  ;; 候補を見つけた場合は、大域変数 skk-kakutei-flag に non-nil を代入する。
  ;; 候補が見つからなかった場合は、nil を返す。
  (setq skk-kakutei-flag (skk-search-jisyo-file file limit nomsg)) )

(defun skk-update-jisyo (word &optional purge)
  (funcall skk-update-jisyo-function word purge) )

(defun skk-update-jisyo-original (word &optional purge)
  ;; WORD が次の変換時に最初の候補になるように、プライベート辞書を更新する。
  ;; PURGE が non-nil で WORD が共有辞書にあるエントリなら skk-ignore-dic-word
  ;; 関数でクォートしたエントリをプライベート辞書に作り、次の変換から出力しな
  ;; いようにする。
  ;; WORD が共有辞書になければ、プライベート辞書の辞書エントリから削除する。
  ;;
  ;; SKK 9.x より、プライベート辞書のエントリの挿入の方法を変更した (9.3 のみ
  ;; は例外)。
  ;;
  ;; 【変更前】
  ;;         ;; okuri-ari entries.
  ;;  見キ   わるk /悪/[か/悪/]/[く/悪/]/
  ;;  出ー   わるi /悪/[い/悪/]/
  ;;  しに   わたs /渡/[さ/渡/]/[せ/渡/]/
  ;;  語降   わすr /忘/[れ/忘/]/
  ;;  を順   わかt /分/判/[った/分/判/]/[って/分/]/
  ;;   ↓     .....
  ;;         あi /合/[い/合/]/
  ;;         ;; okuri-nasi entries.
  ;;  変で   じょうたい /状態/
  ;;  換昇   そうにゅう /挿入/
  ;;  順順   かな /仮名/
  ;;   ↓    ...
  ;;         ...
  ;;
  ;; 【変更後】
  ;;         ;; okuri-ari entries.
  ;;  変で   でt /出/[て/出/]/[た/出/]/
  ;;  換昇   つi /付/[い/付/]/
  ;;  順順   けs /消/[す/消/]/[し/消/]/[せ/消/]/[さ/消/]/
  ;;   ↓    かえs /返/[し/返/]/[す/返/]/[さ/返/]/[せ/返/]/
  ;;         ...
  ;;         ...
  ;;         ながs /長/流/[し/流/]/[さ/長/]/[そ/流/]/
  ;;         ;; okuri-nasi entries.
  ;;  変で   じょうたい /状態/
  ;;  換昇   そうにゅう /挿入/
  ;;  順順   かな /仮名/
  ;;   ↓    ...
  ;;         ...
  ;;
  ;; skk-auto-okuri-process が non-nil のときに、(j-okuri-search 改め)
  ;; skk-okuri-search は見出し語の長い順に候補を返す必要がある。
  ;; SKK 8.6 までは、skk-okuri-search が j-okuri-ari-min から j-okuri-ari-max
  ;; までを順に探し、見つけたもの順に候補を返すためにプライベート辞書が見出し
  ;; 語をキーとして降順にソートされている必要があった。
  ;; SKK 9.x では、skk-okuri-search が、見付けた候補を見出し語をキーとして昇順
  ;; にソートして返すため、プライベート辞書のソートは必要でない。よって、最後
  ;; に変換したものを (j-okuri-ari-min 改め) skk-okuri-ari-min の位置に挿入す
  ;; る。
  ;;
  (let ((jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
	(midasi 
	 (if skk-use-numeric-conversion
	     (skk-num-compute-henkan-key skk-henkan-key)
	   skk-henkan-key ))
	(henkan-buffer (current-buffer)) )
    (if jisyo-buffer
	(let ((inhibit-quit t) buffer-read-only old-entry okurigana)
	  (if (> skk-okuri-index-min -1)
	      (setq word (skk-remove-common word)
		    midasi skk-henkan-key ))
	  (setq okurigana (or skk-henkan-okurigana skk-okuri-char))
	  (with-current-buffer jisyo-buffer
	    ;; 既存エントリを検索後消去する。挿入すべきエントリが entry1 に 1
	    ;; つしかなく、word と同じ文字であっても、いったん消してそのエント
	    ;; リを min ポイントに移動させなければならない (読みの補完を行うと
	    ;; きは、min ポイントから見出しを探すため、新しい見出しほど、min
	    ;; ポイントに近いところになければならない)。
	    (setq skk-henkan-key midasi
		  old-entry (skk-search-jisyo-file-1 okurigana 0 'delete) )
	    (skk-update-jisyo-1 okurigana word old-entry purge)
	    (and skk-update-end-function
		 (funcall skk-update-end-function
			  henkan-buffer midasi okurigana word purge ))
	    (setq skk-update-jisyo-count (1+ skk-update-jisyo-count))
	    (if (and skk-jisyo-save-count
		     (= skk-jisyo-save-count skk-update-jisyo-count) )
		;; auto save.
		(skk-save-jisyo 'quiet) ))))))

(defun skk-update-jisyo-1 (okurigana word old-entry-list purge)
  ;; 既存エントリから計算した entry[1-4] の値と、今回の変換の結果 word とをマー
  ;; ジして、新たなエントリを計算し、挿入する。
  (let ((entry1 (car old-entry-list)) (entry2 (nth 1 old-entry-list))
        (entry3 (nth 2 old-entry-list)) (entry4 (nth 3 old-entry-list)) )
    (if (not purge)
        ;; entry1 の先頭のエントリを word にする。
        (setq entry1 (cons word (delete word entry1)))
      ;; 送りなし、もしくは skk-henkan-okuri-strictly と
      ;; skk-henkan-strict-okuri-precedence が nil の場合。
      (if (or (not okurigana) (not (or skk-henkan-okuri-strictly
				       skk-henkan-strict-okuri-precedence )))
          ;; entry1 を purge。共用辞書にあるエントリだったら、
          ;; skk-ignore-dic-word でクォートして次の変換から出力しないようにす
          ;; る。共用辞書にない文字列は word を消す。
          (if (skk-public-jisyo-has-entry-p okurigana word)
              (setq entry1 (skk-compose-ignore-entry entry1 word))
            (setq entry1 (delete word entry1)) )
        ;; 送りありで、かつ skk-henkan-okuri-strictly か
	;; skk-henkan-strict-okuri-precedence が non-nil の場合で、かつ
        ;; この word とペアになる送り仮名が okurigana しかないとき。
        (if (and okurigana (or skk-henkan-okuri-strictly
			       skk-henkan-strict-okuri-precedence )
                 (null (member word entry2)) (null (member word entry4)) )
            (setq entry1 (delete word entry1))
          ;; その他の場合は何もしない。
          )))
    (if (null entry1)
        ;; entry1 が null であれば、もう何もすることはない。
        nil
      (goto-char (if okurigana skk-okuri-ari-min skk-okuri-nasi-min))
      (insert "\n" skk-henkan-key " /")
      ;; entry1 -- 全エントリ (送りなしの場合) or 漢字部分の全エントリ (送りあ
      ;; りの場合)
      (insert (mapconcat 'skk-quote-char entry1 "/") "/")
      (if (not okurigana)
          nil
        ;; entry2 以降のエントリを処理するのは、送りありの場合のみ。
        ;; 先に挿入すべきエントリを計算、調整する。
        (if entry3
            (if (not purge)
                (setq entry3 (cons word (delete word entry3)))
              (setq entry3 (delete word entry3))
              (if (null entry3)
                  ;; entry3 として挿入するものが全くなければ、"/[く/]/" のよ
                  ;; うな送り仮名のみのエントリを作らないようにする (必要で
                  ;; あれば、entry2 の最後方と) entry4 の先頭のエントリ "]"
                  ;; を削除。
                  (let ((last2 (nthcdr (- (length entry2) 2) entry2)))
                    ;; entry2 の最後方は常に "[送り仮名" とは限らない。
                    (and (string= (nth 1 last2) (concat "[" okurigana))
			 (setcdr last2 nil) )
                    ;; entry4 の先頭は常に "]"。
                    (setq entry4 (cdr entry4)) )))
          ;; entry3 が null であれば
          (if (or skk-process-okuri-early purge)
              ;; skk-process-okuri-early が non-nil なら送り仮名が分らないので
              ;; 何もしない。-- 今回使用した送り仮名がわからないまま変換してい
              ;; るので、全てのエントリが entry2 に入っている -- entry3,
              ;; entry4 は null。
              ;; entry3 として挿入するものが全くなければ、何もしない -- entry3
              ;; が purge 前から null なら、entry2 の末尾は "[" でないし、
              ;; entry4 は null だから entry[234] の操作は不要。
              nil
            (setq entry2 (nconc entry2 (list (concat "[" okurigana)))
                  entry3 (list word)
                  ;; purge 前から entry3 が null だったのだから entry4 も null。
                  entry4 (list "]") ))))
      (if entry2
          ;; entry2 -- 今回使用しなかった送り仮名を使う漢字の候補群 + "[" + 今
          ;; 回使用した送り仮名 (送り仮名のみ。その送り仮名を使用する漢字の候
          ;; 補群は、entry3 に含まれる)。
          (progn
            (insert (mapconcat 'skk-quote-char entry2 "/" ) "/")
            ;; entry2 が null なら entry3 も null。
            (and entry3
		 ;; entry3 -- 今回使用した送り仮名を使う全漢字エントリ
		 (insert (mapconcat 'skk-quote-char entry3 "/") "/") )
            ;; purge で entry3 が null になった場合は entry4 が残っているとき
            ;; がある。
            (and entry4
		 ;; entry4 -- "]" + 他の送り仮名を使う全漢字エントリ (entry2 の
		 ;; 残り)。
		 (insert (mapconcat 'skk-quote-char entry4 "/") "/") ))))))

(defun skk-quote-char (word)
  ;; 辞書の制限から辞書エントリ内に含めてはならない文字が WORD の中にあれば、
  ;; 評価したときにその文字となるような Lisp コードを返す。
  (save-match-data
    (if (and word
             (string-match "[/\n\r\"]" word)
             ;; we should not quote WORD if it is a symbolic expression
             (not (skk-lisp-prog-p word)) )
        (concat "(concat \""
                (mapconcat (function (lambda (c)
                                       (cond ((eq c ?/) "\\057" )
                                             ((eq c ?\n) "\\n" )
                                             ((eq c ?\r) "\\r" )
                                             ((eq c ?\") "\\\"" )
                                             ((eq c ?\\) "\\\\" )
                                             (t (char-to-string c)))))
                           ;; 文字列を対応する char のリストに分解する。
                           (append word nil) "")
                "\")")
      word )))

(defun skk-lisp-prog-p (word)
  ;; word が Lisp プログラムであれば、t を返す。
  (let ((l (skk-str-length word)))
    (and (> l 2) (eq (skk-str-ref word 0) ?\() (< (aref word 1) 128)
         (eq (skk-str-ref word (1- l)) ?\)) )))

(defun skk-public-jisyo-has-entry-p (okurigana word)
  ;; 共有辞書が MIDASHI 及びそれに対応する WORDS エントリを持っていれば、
  ;; non-nil を返す。プライベート辞書のバッファでコールされる。
  (let (fn skk-henkan-okuri-strictly skk-henkan-strict-okuri-precedence)
    (if okurigana
        (setq skk-henkan-okurigana okurigana) )
    ;; skkserv を使う設定になっていたら、skk-server.el をロードする。
    (and (not (featurep 'skk-server))
	 (or (and (boundp 'skk-servers-list) skk-servers-list)
	     (or (and (boundp 'skk-server-host) skk-server-host)
		 (getenv "SKKSERVER") ))
	 (require 'skk-server) )
    (setq fn (funcall skk-public-jisyo-to-be-searched-function))
    (and fn (member word (eval fn))) ))

(defun skk-public-jisyo-to-be-searched-original ()
  ;; skk-search-prog-list の中から、一番大きな共有辞書でサーチするプロ
  ;; グラムを返す。
  (let (fn)
    (and (featurep 'skk-server) (or skk-servers-list skk-server-host)
	 (setq fn (assq 'skk-search-server skk-search-prog-list)) )
    ;; skk-search-server から始まるリストがなければ、とにかく大きい辞書を引数
    ;; にしている skk-search-jisyo-file プログラムを探す。
    (if (and (not fn) (or skk-aux-large-jisyo skk-large-jisyo))
	(let ((spl skk-search-prog-list)
	      cell )
	  (while (setq cell (car spl))
	    (if (and (eq (car cell) 'skk-search-jisyo-file)
		     (memq (nth 1 cell) '(skk-aux-large-jisyo skk-large-jisyo)) )
		(setq fn cell
		      spl nil )
	      (setq spl (cdr spl)) ))))
    fn ))

(defun skk-compose-ignore-entry (entry &optional add)
  ;; ENTRY の中に skk-ignore-dic-word 関数でクォートしたエントリがあれ
  ;; ば、一つのエントリにまとめる。
  ;; オプショナル引数の ADD が指定されていたら、ADD を含めた
  ;; skk-ignore-dic-word エントリを作る。
  ;; 新しい skk-ignore-dic-word エントリを car に、それ以外のエントリ cdr にし
  ;; たリストを返す。
  (let (l arg e)
    (and add (setq entry (delete add entry)))
    (setq l entry)
    (save-match-data
      (while l
        (setq e (car l)
              l (cdr l) )
        (and (string-match "(skk-ignore-dic-word +\\([^\)]+\\))" e)
	     (setq arg (concat arg
			       (substring e (1+ (match-beginning 1))
					  (1- (match-end 1)) )
			       "\" \"" )
		   entry (delq e entry) )))
      (if add
          (setq arg (if arg (concat arg add) add))
        ;; 末尾の " \"" を切り落とす。
        (setq arg (substring arg 0 -2)) )
      (cons (concat "(skk-ignore-dic-word \"" arg "\")") entry) )))


(defun skk-katakana-region (start end &optional vcontract)
  "リージョンのひらがなをカタカナに変換する。
オプショナル引数の VCONTRACT が non-nil であれば、\"う゛\" を \"ヴ\" に変換す
る。
引数の START と END は数字でもマーカーでも良い。"
  (interactive "*r\nP")
  (skk-save-point
   (let (katakana)
     (save-match-data
       (goto-char start)
       (while (re-search-forward  "[ぁ-ん]+" end 'noerror)
	 (setq katakana
	       (skk-hiragana-to-katakana
		(buffer-substring-no-properties (match-beginning 0)
						(match-end 0) )))
	 (backward-char (skk-str-length katakana))
	 ;; firstly insert a new string, secondly delete an old string to save
	 ;; the cursor position.
	 (insert-and-inherit katakana)
	 (delete-region (+ (match-beginning 0) (length katakana))
			(+ (match-end 0) (length katakana)) ))
       (if vcontract
	   (progn
	     (goto-char start)
	     (while (re-search-forward  "ウ゛" end 'noerror)
	       (backward-char (skk-str-length "ウ゛"))
	       (let ((vu-len (length "ヴ")))
		 (insert-and-inherit "ヴ")
		 (delete-region (+ (match-beginning 0) vu-len)
				(+ (match-end 0) vu-len) )))))))))

(defun skk-hiragana-region (start end &optional vexpand)
  "リージョンのカタカナをひらがなに変換する。
オプショナル引数の VEXPAND が non-nil であれば、\"ヴ\" を \"う゛\" に変換する。
引数の START と END は数字でもマーカーでも良い。
\"ヵ\" と \"ヶ\" は変更されない。この 2 つの文字は対応するひらがながないので、カ
タカナとしては扱われない。"
  (interactive "*r\nP")
  (skk-save-point
   (let (hiragana)
     (save-match-data
       (goto-char start)
       (while (re-search-forward  "[ァ-ン]+" end 'noerror)
	 (setq hiragana
	       (skk-katakana-to-hiragana
		(buffer-substring-no-properties (match-beginning 0)
						(match-end 0) )))
	 (backward-char (skk-str-length hiragana))
	 ;; firstly insert a new string, secondly delete an old string to save
	 ;; the cursor position.
	 (insert-and-inherit hiragana)
	 (delete-region (+ (match-beginning 0) (length hiragana))
			(+ (match-end 0) (length hiragana)) ))
       (if vexpand
	   (progn
	     (goto-char start)
	     (while (re-search-forward  "ヴ" end 'noerror)
	       (backward-char (skk-str-length "ヴ"))
	       (insert-and-inherit "う゛")
	       (let ((vu-len (length "う゛")))
		 (delete-region (+ (match-beginning 0) vu-len)
				(+ (match-end 0) vu-len) )))))))))

(defun skk-jisx0208-latin-region (start end)
  "リージョンの ascii 文字を対応する全角英文字に変換する。"
  (interactive "*r")
  (skk-save-point
   (save-match-data
     (goto-char end)
     (while (re-search-backward "[ -~]" start 'noerror)
       ;; firstly insert a new char, secondly delete an old char to save
       ;; the cursor position.
       (let* ((c (aref skk-default-jisx0208-latin-vector (following-char)))
	      (c-len (length c)) )
	 (insert-and-inherit c)
	 (delete-region (+ (match-beginning 0) c-len)
			(+ (match-end 0) c-len) ))))))

(defun skk-latin-region (start end)
  ;; リージョンの全角英数字を対応する ascii 文字に変換する。
  ;; egg.el 3.09 の hankaku-region を参考にした。
  (interactive "*r")
  (skk-save-point
   (save-match-data
     (let (val)
       (goto-char end)
       (while (re-search-backward "\\cS\\|\\cA" start 'noerror)
	 (setq val (skk-jisx0208-to-ascii (char-to-string (following-char))))
	 (if val
	     (progn
	       (insert-and-inherit val)
	       (delete-region (+ (match-beginning 0) 1)
			      (+ (match-end 0) 1) ))))))))

(defun skk-katakana-henkan (arg)
  "▽モードであれば、リージョンのひらがなをカタカナに変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (skk-with-point-move
   (if skk-henkan-on
       (if skk-henkan-active
	   nil
	 (skk-set-marker skk-henkan-end-point (point))
	 (skk-*-henkan-1 'skk-katakana-region skk-henkan-start-point
			 skk-henkan-end-point 'vcontract ))
     (skk-emulate-original-map arg) )))

(defun skk-hiragana-henkan (arg)
  "▽モードであれば、リージョンのカタカナをひらがなに変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (skk-with-point-move
   (if skk-henkan-on
       (if skk-henkan-active
	   nil
	 (skk-set-marker skk-henkan-end-point (point))
	 (skk-*-henkan-1 'skk-hiragana-region skk-henkan-start-point
			 skk-henkan-end-point 'vexpand ))
     (skk-emulate-original-map arg) )))

(defun skk-jisx0208-latin-henkan (arg)
  "▽モードであれば、ascii 文字を対応する全角英文字に変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (skk-with-point-move
   (if skk-henkan-on
       (if skk-henkan-active
	   nil
	 (skk-set-marker skk-henkan-end-point (point))
	 (skk-*-henkan-1 'skk-jisx0208-latin-region skk-henkan-start-point
			 skk-henkan-end-point ))
     (skk-emulate-original-map arg) )))

(defun skk-latin-henkan (arg)
  "▽モードであれば、ascii 文字を対応する全角文字に変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (skk-with-point-move
   (if skk-henkan-on
       (if skk-henkan-active
	   nil
	 (skk-set-marker skk-henkan-end-point (point))
	 (skk-*-henkan-1 'skk-latin-region skk-henkan-start-point
			 skk-henkan-end-point ))
     (skk-emulate-original-map arg) )))

(defun skk-*-henkan-1 (func &rest args)
  ;; 変換可能かどうかのチェックをした後に ARGS を引数として FUNC を適用し、
  ;; skk-henkan-start-point と skk-henkan-end-point の間の文字列を変換する。
  (cond ((skk-get-prefix skk-current-rule-tree)
	 (skk-error "フィックスされていない skk-prefix があります"
		    "Have unfixed skk-prefix" ))
	((< (point) skk-henkan-start-point)
	 (skk-error "カーソルが変換開始地点より前にあります"
		    "Henkan end point must be after henkan start point" ))
	((and (not skk-allow-spaces-newlines-and-tabs)
	      (skk-save-point (beginning-of-line)
			      (> (point) skk-henkan-start-point) ))
	 (skk-error "変換キーに改行が含まれています"
		    "Henkan key may not contain a new line character" )))
  (apply func args)
  (skk-kakutei) )

(defun skk-hiragana-to-katakana (hiragana)
  (let ((diff (- ?ア ?あ)))
    (mapconcat (function (lambda (e) (char-to-string (+ e diff))))
	       (string-to-int-list hiragana) "" )))

(defun skk-katakana-to-hiragana (katakana)
  (let ((diff (- ?ア ?あ)))
    (mapconcat (function (lambda (e) (char-to-string (- e diff))))
	       (string-to-int-list katakana) "" )))

(defun skk-splice-in (org offset spliced)
  ;; ORG := '(A B C), SPLICED := '(X Y), OFFSET := 1
  ;; -> '(A B X Y C)
  (let (tmp tail)
    (or (> offset 0) (error "Cannot splice in!"))
    (setq tmp (nthcdr (1- offset) org)
          tail (cdr tmp) )
    (setcdr tmp nil) ;cut off
    (setcdr tmp (if tail (nconc spliced tail) spliced))
    org ))

;; (defun skk-chomp (nth list)
;;   ;; LIST := '(A B C D), NTH := 1
;;   ;; -> '(A B)
;;   (and (> nth -1) (setcdr (nthcdr nth list) nil))
;;   list )

(defun skk-henkan-face-on ()
  ;; skk-use-face が non-nil の場合、skk-henkan-start-point と
  ;; skk-henkan-end-point の間の face 属性を skk-henkan-face の値に変更する。
  ;;
  ;; SKK 9.4 より Text Properties を使用するのを止めて、Overlays を使用するよ
  ;; うにした (egg.el, canna.el, wnn-egg.el を参考にした)。
  ;; Overlays は、テキストの一部ではないので、バッファから文字を切り出してもコ
  ;; ピーの対象にならないし、アンドゥ時も無視されるので、変換された候補の表示
  ;; を一時的に変更するには Text Properties よりも好都合である。
  (if (and skk-henkan-face
	   (marker-position skk-henkan-start-point)
	   (marker-position skk-henkan-end-point) )
      (skk-face-on skk-henkan-overlay
		   skk-henkan-start-point skk-henkan-end-point
		   skk-henkan-face skk-henkan-overlay-priority )))

(defun skk-henkan-face-off ()
  ;; skk-henkan-start-point と skk-henkan-end-point の間の表示を変更している
  ;; skk-henkan-overlay を消す。
  (and skk-henkan-face (skk-detach-extent skk-henkan-overlay)) )

(defun skk-detach-extent (object)
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (and (extentp object) (detach-extent object)) )
   (t
    (and (overlayp object) (delete-overlay object)) )))

(defun skk-make-face (face)
  ;; hilit-lookup-face-create のサブセット。tutorial で色付けを行なう場合でも
  ;; hilit19 に依存せずとりあえず face を自前で作ることができるように、という
  ;; 目的で作ったもので、簡単な色付けしかできない。あまり賢くはない。複雑な
  ;; face を作りたい人は hilit-lookup-face-create 等を使って下さい。
  (or (car (memq face (face-list)))
      (let ((face-name (symbol-name face)))
        (setq face (make-face face))
        (save-match-data
          (if (not (string-match "/" face-name))
              (set-face-foreground face face-name)
            (set-face-foreground
             face
             (substring face-name 0 (match-beginning 0)) )
            (set-face-background
             face
             (substring face-name (1+ (match-beginning 0))) ))
          face ))))
                        
;; skk-auto.el, skk-rdbms.el の両方で使うので、skk-auto.el より移動した。
(defun skk-remove-common (word)
  ;; skk-henkan-key と word の間に共通の送り仮名を取り除き、送り仮名以外の部分
  ;; の文字列を返す。skk-henkan-key と skk-henkan-okurigana の値をセットする。
  ;; 例えば、word == 持ってきた であれば、skk-henkan-key := "もt",
  ;; skk-henkan-okurigana := "って", word := "持" のように分解し、word を返す。
  ;; skk-auto-okuri-process の値が non-nil であるときにこの関数を使用する。
  (if (and (not (skk-numeric-p)) (not skk-abbrev-mode)
           (or skk-henkan-in-minibuff-flag
               (and (<= skk-okuri-index-min skk-henkan-count)
                    (<= skk-henkan-count skk-okuri-index-max) )))
      (let ((midasi skk-henkan-key)
            (midasi-len (skk-str-length skk-henkan-key))
            (word-len (skk-str-length word))
            (cont t)
            char pos pos2 midasi-tail word-tail new-word okuri-first
            new-skk-okuri-char new-skk-henkan-key )
        (if (not (and (>= midasi-len 2) (>= word-len 2)))
            nil
          ;; check if both midasi and word end with the same ascii char.
          (if (and (eq (skk-str-ref midasi (1- midasi-len))
		       (skk-str-ref word (1- word-len)))
                   (skk-ascii-char-p (skk-str-ref midasi (1- midasi-len))) )
              ;; if so chop off the char from midasi and word.
	      ;; assume size of an ASCII char is always 1.
              (setq midasi (substring midasi 0 -1)
                    midasi-len (1- midasi-len)
                    word (substring word 0 -1)
                    word-len (1- word-len) ))
          (setq midasi-tail (skk-substring midasi (1- midasi-len)
					   midasi-len )
		word-tail (skk-substring word (1- word-len)
					 word-len ))
          ;; もう少し展開できそうだが、バイトコンパイラーがオプティマイズしや
          ;; すいように not を付けるだけにしておく。
          (if (not (and (string= midasi-tail word-tail)
                        (or (and (skk-string<= "ぁ" midasi-tail)
                                 (skk-string<= midasi-tail "ん") )
                            (member midasi-tail '("、" "。" "，" "．")) )))
              nil
            (setq pos (1- word-len)
                  new-word new-skk-henkan-key )
            (while (and cont (> pos 0))
              (setq char (skk-substring word (1- pos) pos))
              (if (and (skk-string<= "亜" char) (skk-string<= char "瑤"))
                  ;; char is the right-most Kanji
                  (setq cont nil)
                (setq pos (1- pos)) ))
            (setq pos2 (- midasi-len (- word-len pos)))
            ;; check if midasi and word has the same tail of length
            (if (not (string= (skk-substring midasi pos2 midasi-len)
                              (skk-substring word pos word-len) ))
                nil
              (setq okuri-first (skk-substring word pos (1+ pos)))
              (setq skk-henkan-okurigana
                    (if (and (string= okuri-first "っ")
                             (<= (+ pos 2) word-len) )
                        ;; in this case okuriga consits of two
                        ;; characters, e.g., 「残った」
                        (skk-substring word pos (+ pos 2))
                      okuri-first ))
              (setq new-word (skk-substring word 0 pos)
		    new-skk-okuri-char (skk-okurigana-prefix okuri-first)
		    new-skk-henkan-key (concat
					(skk-substring midasi 0 pos2)
					new-skk-okuri-char ))
              (if (not skk-henkan-in-minibuff-flag)
                  (setq word new-word
                        skk-henkan-key new-skk-henkan-key )
                ;; ask if register as okuri-ari word.
                (let (inhibit-quit)	; allow keyboard quit
                  (if (y-or-n-p
                       (format
                        (if skk-japanese-message-and-error
                            "%s /%s/ を送りありエントリとして登録しますか？"
                          "Shall I register this as okuri-ari entry: %s /%s/ ? " )
                        new-skk-henkan-key new-word ))
                      (setq word new-word
			    skk-okuri-char new-skk-okuri-char
                            skk-henkan-key new-skk-henkan-key )
                    (setq skk-henkan-okurigana nil
                          skk-okuri-char nil )
                    (message "") ))))))))
  ;; 分解した word (送り仮名部分を除いたもの) を返す。
  word )

(defun skk-okurigana-prefix (okurigana)
  (cond ((string= okurigana "ん")
	 "n" )
	((string= okurigana "っ")
	 (aref skk-kana-rom-vector
	       ;; assume the character is hiragana of JIS X 0208.
	       (- (skk-char-octet
		   (string-to-char (skk-substring skk-henkan-okurigana 1 2))
		   1 )
		  33 )))
	(t (aref skk-kana-rom-vector
		 (- (skk-char-octet
		     (string-to-char (skk-substring skk-henkan-okurigana 0 1))
		     1 )
		    33 )))))

;; from type-break.el.  Welcome!
(defun skk-time-difference (a b)
  ;; Compute the difference, in seconds, between a and b, two structures
  ;; similar to those returned by `current-time'.
  ;; Use addition rather than logand since that is more robust; the low 16
  ;; bits of the seconds might have been incremented, making it more than 16
  ;; bits wide.
  (+ (lsh (- (car b) (car a)) 16)
     (- (nth 1 b) (nth 1 a)) ))

(defun skk-remove-minibuffer-setup-hook (&rest args)
  ;; Remove all args from minibuffer-setup-hook.
  (while args
    (remove-hook 'minibuffer-setup-hook (car args))
    (setq args (cdr args)) ))

(add-hook 'edit-picture-hook 'skk-misc-for-picture 'append)
;; add 'skk-save-jisyo only to remove easily.
(add-hook 'skk-before-kill-emacs-hook 'skk-save-jisyo)
(add-hook 'minibuffer-exit-hook
          (function
           (lambda ()
	     (remove-hook 'pre-command-hook 'skk-pre-command 'local)
	     (skk-remove-minibuffer-setup-hook
	      'skk-j-mode-on 'skk-setup-minibuffer
	      (function (lambda ()
			  (add-hook 'pre-command-hook 'skk-pre-command nil 'local) ))))))

(defun skk-setup-modeline ()
  "モード行へのステータス表示を準備する。"
  (cond ((eq skk-status-indicator 'left)
	 (mapcar (function
		  (lambda (el)
		    (let ((sym (car el))
			  (strs (cdr el)))
		      (if (string= (symbol-value sym) (cdr strs))
			  (set sym (car strs)) ))))
		 (cond
		  ((and (fboundp 'face-proportional-p)
			(face-proportional-p 'modeline))
		   '((skk-latin-mode-string . ("--SKK:" . " SKK"))
		     (skk-hiragana-mode-string . ("--かな:" . " かな"))
		     (skk-katakana-mode-string . ("--カナ:" . " カナ"))
		     (skk-jisx0208-latin-mode-string . ("--全英:" . " 全英"))
		     (skk-abbrev-mode-string . ("--aあ:" . " aあ"))))
		  (t
		   '((skk-latin-mode-string . ("--SKK::" . " SKK"))
		     (skk-hiragana-mode-string . ("--かな:" . " かな"))
		     (skk-katakana-mode-string . ("--カナ:" . " カナ"))
		     (skk-jisx0208-latin-mode-string . ("--全英:" . " 全英"))
		     (skk-abbrev-mode-string . ("--aあ::" . " aあ"))))))
	 (cond ((eq skk-emacs-type 'xemacs)
		(or (memq 'skk-input-mode-string default-mode-line-format)
		    (setq-default default-modeline-format
				  (append '("" skk-input-mode-string)
					  default-modeline-format) ))
		(mapc
		 (function
		  (lambda (buf)
		    (if (buffer-live-p buf)
			(save-excursion
			  (set-buffer buf)
			  (or (memq 'skk-input-mode-string modeline-format)
			      (setq modeline-format
				    (append '("" skk-input-mode-string)
					    modeline-format) ))))))
		 (buffer-list) ))
	       (t
		(or (memq 'skk-input-mode-string mode-line-format)
		    (setq-default
		     mode-line-format
		     (append '("" skk-input-mode-string)
			     mode-line-format) ))))
	 (setq-default skk-input-mode-string "")
	 (force-mode-line-update t) )
	(t
	 (setq minor-mode-alist
	       (put-alist 'skk-mode
			  ;; each element of minor-mode-alist is not cons cell.
			  '(skk-input-mode-string) minor-mode-alist) ))))

(run-hooks 'skk-load-hook)

(provide 'skk)
;;; Local Variables:
;;; End:
;;; skk.el ends here
