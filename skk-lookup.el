;;; skk-lookup.el --- SKK lookup gateway
;; Copyright (C) 1999, 2000 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-lookup.el,v 1.2 2000/09/10 01:07:41 minakaji Exp $
;; Keywords: japanese
;; Created: Sep. 23, 1999
;; Last Modified: $Date: 2000/09/10 01:07:41 $
;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; Daredevil SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary
;;
;; Keisuke Nishida <kxn30@po.cwru.edu> さんの作られた辞書検索ツール
;; Lookup と SKK との gateway を行ない、Lookup で検索できる辞書を使っ
;; て候補を出力するプログラムです。
;;
;; <HOW TO INSTALL>
;; make を実行する際に、lookup.el にパスが通っていて require できる
;; ときは、本プログラムも自動的にインストールされます。lookup.el が
;; インストールされているのに Emacs が検出してくれないときは、
;; SKK-CFG を編集して VERSION_SPECIFIC_LISPDIR にそのパスを書くと良
;; いでしょう。
;;
;; <HOW TO USE>
;; 当然ですが、Lookup がインストールされていて、かつ、対応する辞書が
;; マウントされていないと使えません。
;;
;; 次のように skk-search-prog-list に加えて指定し使用します。
;; SKK が用意している検索プログラムの中で最も重いので、
;; skk-seach-server の検索の後に持ってくるのがセオリーです。
;;
;;  (setq skk-search-prog-list
;;        '((skk-search-jisyo-file skk-jisyo 0 t)
;;          (skk-search-server skk-aux-large-jisyo 10000)
;;          (skk-lookup-search)))
;;
;; ディフォルトの設定では、lookup の変数である `lookup-search-agents'
;; をコピーして ndkks, ndcookie, ndnmz を取り去り、
;; `skk-lookup-search-agents' にセットしてこれを検索するようにしています。
;; もちろん lookup の検索とは異なる設定を `skk-lookup-search-agents' に明
;; 示することも可能です。
;;
;; 現在対応している辞書は
;;
;;   ispell, jedict, CHIEZO, CHUJITEN, COLLOC, GENIUS, GN99EP01,
;;   GN99EP02, IWAKOKU, KANJIGEN, KANWA, KOJIEN, KOKUGO, KOUJIEN,
;;   MYPAEDIA, NEWANC, PLUS, RIKAGAKU, WAEI
;;
;; です (lookup-dictionary-name が返す値で標記しています)。
;; kakasi (KAKASI を利用するなら skk-kakasi.el を使いましょう),
;; ndcookie, ndnmz には対応していませんし、対応の必要はないと考えてい
;; ます (メリットがあれば教えて下さい)。
;;
;; ご自分で使用している辞書の出力が上手く取り込めないときは、
;; `skk-lookup-pickup-headings' を使用して例えば、
;;
;;   (skk-lookup-pickup-headings "こしょう" 'exact)
;;
;; などと評価して ("こしょう" の文字列部分は問題となっている検索対象と
;; 入れ替えましょう) `lookup-dictionary-name' と
;; `lookup-entry-heading' が返す値を参考に、`'skk-lookup-option-alist'
;; に必要なリストを加えましょう。新たなリストを加えられたら是非作者に
;; も知せて下さい。default value に取り込みたいと思います。よろしくお
;; 願いいたします。
;;
;; 末尾ながら、Lookup を作られた Lookup Development Team の皆様、
;; Lookup の 原作者であり、本プログラムの開発にもいくつか貴重なご意見をいただ
;; きました Keisuke Nishida <kxn30@po.cwru.edu> さん、開発の初期からデ
;; バッグを手伝っていただいた、NEMOTO Takashi <tnemoto@mvi.biglobe.ne.jp>
;; さん、sphere <sphere@pop12.odn.ne.jp> さんに深く感謝いたします。

;;; Code:
(eval-when-compile (require 'skk) (require 'skk-num) (require 'cl))

(require 'poe)
(require 'lookup)

;;;###autoload
(defgroup skk-lookup nil "SKK lookup related customization."
  :prefix "skk-lookup-"
  :group 'skk)

(defcustom skk-lookup-search-agents
  ;; copy-list is a C primitive of XEmacs, but FSFmacs has it
  ;; in cl.el.
  (let ((agents (copy-sequence lookup-search-agents))
	e )
    ;; use `skk-kakasi.el' instead of ndkks.
    (setq agents (delete '(ndkks) agents))
    (while (setq e (assq 'ndcookie agents))
      (setq agents (delq e agents)) )
    (while (setq e (assq 'ndnmz agents))
      (setq agents (delq e agents)) )
    agents )
  "*検索エージェントの設定のリスト。
リストの各要素は次の形式を取る:

  \(CLASS LOCATION [KEY1 VALUE1 \[KEY2 VALUE2 \[...\]\]\]\)

CLASS には、エージェントの種類をシンボルで指定する。
LOCATION には、エージェントの所在を文字列で指定する。
KEY 及び VALUE は省略可能で、エージェントに対するオプションを指定する。

例: (setq skk-lookup-search-agents
          '((ndtp \"dserver\" :port 2010)
            (ndeb \"/cdrom\" :enable (\"EIWA\")))))"
  :type '(repeat (sexp :tag "Agent"))	; type はちょっとややこしすぎ・・
  :group 'skk-lookup
  :require 'lookup-vars)

(defcustom skk-lookup-option-alist
  '(
    ;; "[spla -> splat]"
    ("ispell" exact nil nil (not skk-okuri-char) "-> \\([^ ]+\\)]$" nil)
    ;; what's this?
    ("jedict" exact nil nil (not skk-okuri-char) nil nil)
    ;; 「辞・典・盤」 "あか３ 淦", "ethanol"
    ("CHUJITEN" exact exact prefix t "[０-９]* *\\([^ ]+\\)$" nil)
    ;; "(皮膚などの)あか <grime>", "《英》 (パイプなどの)あか <fur>"
    ("COLLOC" exact exact prefix t "\\([^ 《》]+\\) <[a-z]+>$" nil)
    ;; ジーニアス英和, "あか[淦]"
    ;; ジーニアス英和・和英辞典 いれかえ[入れ替え,入れ換え]
    ("GENIUS" exact exact prefix t "\\[\\(.+\\)\\]" ",")
    ;; Super統合辞書99 Disk1, 2/現代用語の基礎知識
    ;; "・" が区切り文字であるときとそうでないときがあるなぁ...。
    ;; "◆朱・株・殊・珠〔似たもの漢字〕" "◆赤ワイン・ブーム〔健康問題〕"
    ("GN99EP01" exact exact prefix t "^◆\\([^〔〕]+\\)〔.+〕$" nil)
    ("GN99EP02" exact exact prefix t "^◆\\([^〔〕]+\\)〔.+〕$" nil)
    ;; IWAKOKU: 「辞・典・盤」
    ;; "したい【死体・屍体】", "したい【支隊】【枝隊】",
    ;; "あい【愛】", "あい(あゐ)【藍】"
    ;; "あい<gaiji=za52a>哀<gaiji=za52b>"
    ("IWAKOKU" exact exact prefix t "【\\(.+\\)】" "】【\\|・")
    ;; "垢", "赤"
    ("KANWA" exact exact prefix t nil nil)
    ;; 「辞・典・盤」 "垢"
    ("MYPAEDIA" exact exact prefix t nil nil)
    ;; ニューアンカー英和 "あか２ 垢"
    ("NEWANC" exact exact prefix t "[０-９]* *\\([^ ]+\\)$" nil)
    ;; "　あか <scud２>", "　「あか」 <rust>"
    ("PLUS" exact exact prefix t "^　\\(.+\\) <[a-z０-９]+>$" nil)
   )
  "*辞書毎の検索、文字切り出しオプション。
リストの各要素は下記の通り。

  0th: lookup-dictionary-name が返す文字列 \(辞書種別を表わす\)。。
  1th: 送りなし変換の際の search method を示すシンボル。regexp 現在のところ指定
       不可。
  2th: 送りあり変換で、かつ skk-process-okuri-early オプションを指定していないと
       き \(送り仮名決定の後に検索を開始するので、送り仮名が特定できる\) の
       search method を示すシンボル。regexp 現在のところ指定不可。nil を指定する
       と、送りあり変換の際はその辞書を検索しない。
  3th: 送りあり変換で、かつ skk-process-okuri-early であるとき \(送り仮名決定の
       前に検索を開始するので、送り仮名が特定できないので、送り仮名のかな prefix
       を除いた部分を検索キーとして lookup に渡している\) の search method を示す
       シンボル。regexp 現在のところ指定不可。nil を指定すると送りあり変換の際は
       その辞書を検索しない。
  4th: S 式。この S 式を評価して nil になるときは検索しない。ある一定の条件を満
       した場合に検索しないように指定できる。
  5th: 候補を切り出すための regexp \(\(match-string 1\) で候補を取り出すことが
       できるよう指定する\)。切り出さずに文字列全体を対象にするときは、nil を指定
       する。
  6th: 切り出された文字列の中に更に複数の候補を含む場合の区切りを表わす regexp。
       複数の候補が同一 heading の中に出力されないときは、nil を指定する。

現在対応している辞書名は、\"CHUJITEN\", \"COLLOC\", \"GENIUS\", \"GN99EP01\",
\"GN99EP02\", \"IWAKOKU\", \"KANWA\", \"MYPAEDIA\", \"NEWANC\", \"PLUS\".

`lookup-entry-heading' が自分の使用する辞書からどのような文字列を取り出すのか
確かめたいときは、`skk-lookup-pickup-headings' を使用する。例えば、

 \(skk-lookup-pickup-headings \"こしょう\" 'exact\)"
  :type '(repeat
	  (list (string :tag "Dictionary name")
		(choice :tag "Search method for okuri nasi"
			(const exact) (const prefix)
			(const suffix) (const substring)
			(const keyword) (const text)
			(const nil))
		(choice :tag "Search method for okuri ari (not process okuri early)"
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
			regexp (const nil))
		(choice :tag "Regexp to split candidates"
		       regexp (const nil))))
  :group 'skk-lookup)

(defcustom skk-lookup-default-option-list
  '(exact exact prefix t "【\\([^【】]+\\)】" "・")
  ;; CHIEZO: 「辞・典・盤」
  ;; KANJIGEN: Super統合辞書99 Disk2/漢字源 : EPWING
  ;; KOUJIEN: 広辞苑 第4版(岩波,EPWING) マルチメディア版
  ;; KOJIEN: 広辞苑第5版(岩波,EPWING)
  ;; KOKUGO: what's this?
  ;; RIKAGAKU: 理化学辞典
  ;; WAEI: what's this?
  "*ディフォルトの辞書検索、文字切り出しオプション。
まず辞書名をキーにして `skk-lookup-option-alist' を引き、そこに辞書検索、文字切
り出しのオプションが見つかればそれを使用し、見つからなかった場合にこの変数で指定
される辞書検索、文字切り出しのオプションを使用する。

リストの各要素は下記の通り。

  0th: 送りなし変換の際の search method を示すシンボル。regexp 現在のところ指定
       不可。
  1th: 送りあり変換で、かつ skk-process-okuri-early オプションを指定していないと
       き \(送り仮名決定の後に検索を開始するので、送り仮名が特定できる\) の
       search method を示すシンボル。regexp 現在のところ指定不可。nil を指定する
       と、送りあり変換の際はその辞書を検索しない。
  2th: 送りあり変換で、かつ skk-process-okuri-early である \(送り仮名決定の前に
       検索を開始するので、送り仮名が特定できないので、送り仮名のかな prefix を除
       いた部分を検索キーとして lookup に渡している\) ときの search method を示す
       シンボル。regexp 現在のところ指定不可。nil を指定すると送りあり変換の際は
       その辞書を検索しない。
  3th: S 式。この S 式を評価して nil になるときは検索しない。ある一定の条件を満
       した場合に検索しないように指定できる。
  4th: 候補を切り出すための regexp \(\(match-string 1\) で候補を取り出すこと
       ができるよう指定する\)。切り出さずに文字列全体を対象にするときは、nil を指定
       する。
  5th: 切り出された文字列の中に更に複数の候補を含む場合の区切りを表わす regexp。
       複数の候補が同一 heading の中に出力されないときは、nil を指定する。

このオプションで対応している辞書名は、\"CHIEZO\", \"KANJIGEN\", \"KOJIEN\",
\"KOUJIEN\", \"KOKUGO, \"RIKAGAKU\", \"WAEI\".
`lookup-entry-heading' で取り出した文字列が下記のようになることを前提にしている。

  \"あ‐か【亜科】‥クワ\"
  \"あか【閼伽】\"
  \"こ‐しょう【小姓・小性】‥シヤウ\"

`lookup-entry-heading' が自分の使用する辞書からどのような文字列を取り出すのか
確かめたいときは、`skk-lookup-pickup-headings' を使用する。例えば、

 \(skk-lookup-pickup-headings \"こしょう\" 'exact\)"
  :type '(list (choice :tag "Search method for okuri nasi"
		       (const exact) (const prefix)
		       (const suffix) (const substring)
		       (const keyword) (const text)
		       (const nil))
	       (choice :tag "Search method for okuri ari (not process okuri early)"
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
		       regexp (const nil))
	       (choice :tag "Regexp to split candidates"
		       regexp (const nil)))
  :group 'skk-lookup)

(defcustom skk-lookup-search-modules nil
  "*検索モジュールの設定のリスト。"
  :type '(repeat (cons :tag "Module" (string :tag "Name")
		       (repeat :tag "Dictionary" (string :tag "ID"))))
  :group 'skk-lookup)

;; internal variables.
(defvar skk-lookup-agent-list nil)
(defvar skk-lookup-default-module nil)
(defvar skk-lookup-module-list nil)

;; aliases.
(defalias-maybe 'skk-okurigana-prefix 'skk-auto-okurigana-prefix)

;;;; inline functions.
(defsubst skk-lookup-get-method (name okuri-process)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist))
	  sex)
      ;; If you search via ndtpd, book's name and slash are attached to NAME
      ;; as prefix, like `IWANAMI/KOJIEN'.  The following forms will truncate
      ;; it to `KOJIEN'.
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (setq sex (nth okuri-process (if list (cdr list) skk-lookup-default-option-list)))
      (cond ((symbolp sex) sex)
	    (t (eval sex))))))

(defsubst skk-lookup-get-nonsearch-sex (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (nth 3 (if list (cdr list) skk-lookup-default-option-list)))))

(defsubst skk-lookup-get-pickup-regexp (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (nth 4 (if list (cdr list) skk-lookup-default-option-list)))))

(defsubst skk-lookup-get-split-regexp (name)
  (save-match-data
    (let ((list (assoc name skk-lookup-option-alist)))
      (if (and (null list) (string-match "/\\(.+\\)$" name))
	  (setq list (assoc (match-string 1 name) skk-lookup-option-alist)))
      (nth 5 (if list (cdr list) skk-lookup-default-option-list)))))

;;;; funcitions.
;;;###autoload
(defun skk-lookup-search ()
  (if (and (boundp 'skk-num-list) (or skk-num-list skk-num-recompute-key))
      ;; 数値変換のときは変換キーが `#' を含むものなので、lookup で検索しない。
      nil
    (save-excursion
      (let ((module (skk-lookup-default-module))
	    ;; if `lookup-enable-gaiji' is nil, gaiji tag like
	    ;; `<gaiji=za52a>' is put out.
	    ;;(lookup-enable-gaiji nil)
	    (lookup-gaiji-alternate "")
	    (henkan-key skk-henkan-key)
	    okuri-process)
	(cond ((not (or skk-henkan-okurigana skk-okuri-char))
	       ;; okuri-nasi
	       (setq okuri-process 0))
	      ;; okuri-ari and (not skk-process-okuri-early)
	      (skk-henkan-okurigana
	       ;; search method に regexp を許すならばここで henkan-key を決め打ちせず
	       ;; に一工夫いるね...。
	       (setq henkan-key (concat (substring henkan-key 0 (1- (length henkan-key)))
					skk-henkan-okurigana)
		     okuri-process 1))
	      ;; okuri-ari and skk-process-okuri-early
	      (skk-okuri-char
	       ;; 送り仮名のかな prefix を捨てて lookup に渡す。
	       (setq henkan-key (substring henkan-key 0 (1- (length henkan-key)))
		     okuri-process 2)))
	(skk-lookup-search-1 module henkan-key okuri-process)))))

(defun skk-lookup-search-1 (module key okuri-process)
  ;; search pattern.
  (let (name method entries pickup-regexp split-regexp
	     candidates-string candidates-list)
    (setq lookup-search-pattern key)
    ;; setup modules.
    (lookup-module-setup module)
    (lookup-foreach
     (lambda (dictionary)
       (when (and (lookup-dictionary-selected-p dictionary)
		  (setq name (lookup-dictionary-name dictionary))
		  (eval (skk-lookup-get-nonsearch-sex name))
		  (setq method (skk-lookup-get-method name okuri-process))
		  ;; valid method or not?
		  (memq method (lookup-dictionary-methods dictionary))
		  ;; actual search.
		  (setq entries (lookup-vse-search-query
				 dictionary
				 (lookup-make-query method lookup-search-pattern))))
	 (setq pickup-regexp (skk-lookup-get-pickup-regexp name)
	       split-regexp (skk-lookup-get-split-regexp name))
	 (lookup-foreach
	  (lambda (entry)
	    ;; pickup necessary string for SKK.
	    (setq candidates-string (lookup-entry-heading entry))
	    (if (not (or pickup-regexp split-regexp))
		(progn
		  (setq candidates-string (skk-lookup-process-okurigana
					   candidates-string
					   okuri-process))
		  (if (and candidates-string
			   (not (string= lookup-search-pattern candidates-string)))
		      (setq candidates-list (cons candidates-string
						  candidates-list))))
	      (setq candidates-list
		    (nconc (skk-lookup-process-heading
			    candidates-string pickup-regexp split-regexp
			    okuri-process)
			   candidates-list))))
	  entries)))
     ;; dictionaries to be searched.
     (lookup-module-dictionaries module))
    (nreverse candidates-list)))

(defun skk-lookup-process-okurigana (string process-type)
  (cond ((string= string "")
	 ;; KOUJIEN has a heading like `ま‐き【真木・(GAIJI)・槙】'
	 ;; As GAIJI cannot be processed by skk-lookup.el, the heading
	 ;; is equal to `ま‐き【真木・・槙】' for skk-lookup.el.
	 ;; It causes to produce a null string candidate. 
	 ;;   (split-string "真木・・槙" "・") -> ("真木" "" "槙")
	 ;; So return nil if STRING is a null string.
	 nil)
	((= process-type 0) string)
	(t
	 (let ((okuri-length
		(cond ((= process-type 1) (length skk-henkan-okurigana))
		      ((= process-type 2)
		       ;; don't know exactly how long okurigana is.
		       ;; truncate length of one character anyway.
		       skk-kanji-len))))
	   (cond ((= process-type 2)
		  (cond ((> okuri-length (length string))
			 string)
			((string= (skk-okurigana-prefix (substring string -1))
				  skk-okuri-char)
			 (substring string 0 (- okuri-length)))))
		 ((not (string= skk-henkan-okurigana
				(substring string (- okuri-length))))
		  nil)
		 ((> okuri-length (length string)) string)
		 (t (substring string 0 (- okuri-length))))))))

(defun skk-lookup-process-heading
  (heading pickup-regexp split-regexp okuri-process-type)
  ;; heading しか取り出さないのはもったいない？  他にも情報を取り出し
  ;; ておいて、必要に応じて参照するか？
  (save-match-data
    (do (candidates-string candidates-list)
	((or (string= heading "")
	     (and pickup-regexp (not (string-match pickup-regexp heading))))
	 candidates-list)
      (if pickup-regexp
	  (setq candidates-string (match-string 1 heading)
		heading (substring heading (min (+ (match-end 1) skk-kanji-len)
						(length heading))))
	(setq candidates-string heading
	      heading ""))
      (if split-regexp
	  (lookup-foreach
	   (lambda (c)
	     (if (string= lookup-search-pattern c)
		 nil
	       (setq c (skk-lookup-process-okurigana c okuri-process-type))
	       (if c
		   (setq candidates-list (cons c (delete c candidates-list))))))
	   (split-string candidates-string split-regexp))
	(if (string= lookup-search-pattern candidates-string)
	    nil
	  (setq candidates-string (skk-lookup-process-okurigana
				   candidates-string okuri-process-type))
	  (if candidates-string
	      (setq candidates-list
		    (cons candidates-string
			  (delete candidates-string candidates-list)))))))))

;; The following four functions were imported from lookup.el and
;; lookup-types.el.
(defun skk-lookup-default-module ()
  (or skk-lookup-default-module
      (setq skk-lookup-default-module (car (skk-lookup-module-list)))))

(defun skk-lookup-module-list ()
  (or skk-lookup-module-list
      (setq skk-lookup-module-list
	    (mapcar 'skk-lookup-new-module (or skk-lookup-search-modules
					       '(("%SKK-EVERY" "")))))))
(defun skk-lookup-new-module (spec)
  (let ((name (car spec))
	(id-list (cdr spec))
	module agents match start)
    ;; get agent list
    (lookup-foreach (lambda (id)
		      ;; get the list of agents matched with ID
		      (setq match (concat "^" (regexp-quote id))
			    start agents)
		      (lookup-foreach
		       (lambda (e)
			 (when (string-match match (lookup-agent-id e))
			   (setq agents (cons e agents))))
		       (skk-lookup-agent-list))
		      (when (eq start agents)
			(error "No match agent: %s" id)))
		    ;; get a list of agent-IDs
		    (lookup-nunique
		     (mapcar (lambda (id)
			       (string-match "^[^:]*" id)
			       (substring id 0 (match-end 0)))
			     id-list)))
    (setq agents (nreverse (lookup-nunique agents 'eq)))
    ;; construct module
    (setq module (lookup-make-module name nil))
    (lookup-module-put-property module 'agents agents)
    (lookup-module-put-property module 'id-list id-list)
    (lookup-module-init module)))

(defun skk-lookup-agent-list ()
  (or skk-lookup-agent-list
      (setq skk-lookup-agent-list
	    (mapcar 'lookup-new-agent skk-lookup-search-agents))))

;; the following two are to check dictionary output of heading for
;; creating new regexp.
(defun skk-lookup-test-regexp (regexp place string)
  "Search STRING by REGEXP and pick up a part of STRING in PLACE."
  (string-match regexp string)
  (match-string place string))

(defun skk-lookup-pickup-headings (pattern method)
  "Search PATTERN by METHOD."
  (let ((module (skk-lookup-default-module))
	(lookup-gaiji-alternate "")
	;;lookup-enable-gaiji ;  not to put out gaiji.
	var)
    (lookup-module-setup module)
    (lookup-foreach
     (lambda (dictionary)
       (lookup-foreach
	(lambda (entry)
	  (setq var (nconc (list
			    (list (lookup-dictionary-name dictionary)
				  (lookup-dictionary-id dictionary)
				  (lookup-entry-heading entry)
				  ;;(lookup-dictionary-command dictionary 'content entry)
				 ))
			   var)))
	(lookup-vse-search-query
	 dictionary (lookup-make-query method pattern))))
     (lookup-module-dictionaries module))
    var))

(provide 'skk-lookup)
;;; Local Variables:
;;; End:
;;; skk-lookup.el ends here
