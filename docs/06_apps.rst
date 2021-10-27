##############
便利な応用機能
##############

************
ファイル構成
************

SKK の基本的な機能はファイル :file:`skk.el` に収められています。一方、DDSKK で応
用機能を提供するプログラムのほとんどはファイル :file:`skk.el` とは別のファイルに
収めています。これらは、必要に応じてオートロードするように設計されています。各応
用機能の概略と該当のファイル名について説明します。

また、DDSKK の変数はファイル :file:`skk-vars.el` に集約されていますので、カスタマ
イズしたい場合などには、このファイルを見ると参考になるかもしれません。

.. index::
   pair: File; auto-autoloads.el

.. list-table::
   
   * - FILE
     - 説明
   * - ccc.el
     - buffer local cursor color control library
   * - cdb.el
     - constant database (cdb) reader for Emacs Lisp
   * - context-skk.el
     - | 編集の文脈に応じて自動的に skk のモードを切り替えたり、
       | SKK の各種設定を変更する機能を提供します 。
   * - ddskk-pkg.el
     - :infonode:`Multi-file Packages in GNU Emacs Lisp Reference Manual <(elisp)Multi-file Packages>`
   * - skk-abbrev.el
     - SKK abbrev モードの機能を提供するプログラムを集めたファイル
   * - skk-act.el
     - dvorak 配列での拡張ローマ字入力 ACT を SKK で使うための設定
   * - skk-annotation.el
     - | 個人辞書に付けた :ref:`アノテーション（注釈） <annotation>` を活用する
       | プログラムを集めたファイル
   * - skk-auto.el
     - :ref:`送り仮名の自動処理 <okurigana>` を行うプログラムを集めたファイル
   * - skk-autoloads.el
     - | コマンド :command:`make` 時に自動生成されるファイル。
       | オートロードの設定のほか関数 :el:defun:`register-input-method` も行う。
   * - skk-azik.el
     - 拡張ローマ字入力 AZIK の設定を提供します
   * - skk-bayesian.el
     - | 学習効果を提供するプログラム
       | ユーザの過去の入力から変換候補を予測します
   * - skk-cdb.el
     - CDB 形式辞書ファイルを辞書サーバなしに直接利用できるプログラム
   * - skk-comp.el
     - 見出し語の補完を行うプログラムを集めたファイル
   * - skk-cursor.el
     - カーソルの色を制御するプログラムを集めたファイル
   * - skk-cus.el
     - :kbd:`M-x customize-group` による対話的な設定変更機能の簡易版を提供します
   * - skk-dcomp.el
     - skk-comp による補完を自動的に実行して見出し語入力を支援します
   * - skk-develop.el
     - font-lock 関係のほか、おもに開発者向けのプログラムを集めたファイル
   * - skk-emacs.el
     - | GNU Emacs の拡張機能を利用するプログラムを集めたファイル
       | インジケータのカラー化や画像表示、ツールティップ利用など
   * - skk-gadget.el
     - :ref:`プログラム実行変換 <program-conversion>` を行うプログラムを集めたファイル
   * - skk-hint.el
     - SKK の変換候補が多いときにヒントを与えて絞りこむ機能を提供します
   * - skk-inline.el
     - 変換候補のインライン表示機能を集めたファイル
   * - skk-isearch.el
     - DDSKK を併用したインクリメンタル・サーチ機能を提供します
   * - skk-jisx0201.el
     - JIS X 0201 カナ [#]_ を利用する機能を提供します
   * - skk-jisx0213.el
     - JIS X 0213 文字集合を扱うプログラム
   * - skk-jisyo-edit-mode.el
     - SKK 辞書を編集するためのメジャーモードを提供します
   * - skk-kakasi.el
     - KAKASI インターフェイスプログラムを集めたファイル
   * - skk-kanagaki.el
     - | キーボードのかな配列などに対応する枠組みを提供します。
       | 旧 JIS 配列のかなキーボード及び NICOLA 規格の親指シフト配列に対応
   * - skk-kcode.el
     - :ref:`文字コードまたはメニューによる文字入力 <char-code-input>` を行うプロ
       グラムを集めたファイル
   * - skk-leim.el
     - | LEIM 関連プログラムファイル
       | DDSKK を Emacs の input method として利用できるようにします
   * - skk-look.el
     - コマンド :command:`look` とのインターフェイスプログラムを集めたファイル
   * - skk-lookup.el
     - Lookup で検索できる辞書を使って単語の候補を出力するプログラム
   * - skk-macs.el
     - 他のファイルで共通して使用するマクロなどを中心にまとめたファイル
   * - skk-num.el
     - 数値変換を行うプログラムを集めたファイル
   * - skk-search-web.el
     - | Google CGI API for Japanese Input を利用したかな漢字変換
       | :ref:`辞書登録モード <jisyo-register-mode>` に Google サジェストを初期表示する
   * - skk-server-completion.el
     - 拡張された辞書サーバによる見出し語補完機能を利用できます
   * - skk-server.el
     - 辞書サーバと通信して変換する機能を提供します
   * - skk-setup.el
     - 自動的に個人設定を行うためのファイル
   * - skk-show-mode.el
     - カーソル付近に入力モードを表示する機能を提供します
   * - skk-sticky.el
     - 変換開始位置及び送り開始位置の指定方法を変更可能にする
   * - skk-study.el
     - | 学習効果を提供するプログラム
       | 直前に確定したいくつかの語との関連性を確認し、候補順を操作する
   * - skk-tankan.el
     - 単漢字変換を行うプログラム
   * - skk-tut.el
     - SKK チュートリアルプログラム
   * - skk-tutcode.el
     - TUT-code 入力を実現します
   * - skk-vars.el
     - DDSKK で使われる変数を集約したファイル
   * - skk-version.el
     - DDSKK のバージョン情報を提供するプログラムファイル
   * - skk-viper.el
     - VIPER インターフェイスプログラムを集めたファイル
   * - tar-util.el
     - utility for tar archive

**************************
ユーザオプションの設定方法
**************************

DDSKK のカスタマイズは、ファイル :file:`~/.emacs.d/init.el` あるいはファイル :file:`~/.skk` に
記述します。また、各ファイルの提供するフックも利用します。上記のファイルやフック
を利用した設定がいつ有効になるのか、という点についてここで説明します。

.. _configure-file:

設定ファイル
============

.. index::
   pair: File; ~/.emacs.d/init.el

.. describe:: ~/.emacs.d/init.el

   Emacs を起動したときに一度だけ読み込まれます。

   :infonode:`The Emacs Initialization File in GNU Emacs Manual <(emacs)Init File>`

   このマニュアルではファイル :file:`~/.emacs.d/init.el` という記述で統一しています。

.. index::
   pair: File; ~/.skk
   pair: Function; convert-standard-filename

.. describe:: ~/.skk

   DDSKK を起動した最初の一度だけ読み込まれます。ファイル名の標準設定は OS の種類
   により異なりますが、実際は Emacs の関数 :el:defun:`convert-standard-filename` に
   より加工されます。

   ファイル :file:`~/.skk` の名称は、変数 :el:defvar:`skk-init-file` で変更するこ
   とができます。また、DDSKK にはこのファイルを自動的にバイトコンパイルする機能が
   あります。

.. el:defvar:: skk-user-directory

   DDSKK はファイル :file:`~/.skk` やファイル :file:`~/.skk-jisyo` といった複数の
   ファイルを使用します。これらのファイルをひとつのディレクトリにまとめて置きたい
   場合は、この変数にそのディレクトリ名を設定します。標準設定は nil です。

   この変数はファイル :file:`~/.emacs.d/init.el` で設定してください。
   DDSKK 起動時にこの変数が指すディレクトリが存在しない場合は、自動的に作られます。

   .. code:: emacs-lisp

      (setq skk-user-directory "~/.ddskk")  

   この変数を設定した場合（例えば上記ファイル :file:`~/.ddskk` ）、以下に挙げる各
   変数の標準設定値が変更されます。

   .. list-table::

      * - 影響を受ける変数
        - 標準の値
        - 変数 :el:defvar:`skk-user-directory` を設定した場合の値  
      * - :el:defvar:`skk-init-file`
        - :file:`~/.skk`
        - :file:`~/.ddskk/init`
      * - :el:defvar:`skk-jisyo`
        - :file:`~/.skk-jisyo`
        - :file:`~/.ddskk/jisyo`
      * - :el:defvar:`skk-backup-jisyo`
        - :file:`~/.skk-jisyo.BAK`
        - :file:`~/.ddskk/jisyo.bak`
      * - :el:defvar:`skk-emacs-id-file`
        - :file:`~/.skk-emacs-id`
        - :file:`~/.ddskk/emacs-id`
      * - :el:defvar:`skk-record-file`
        - :file:`~/.skk-record`
        - :file:`~/.ddskk/record`
      * - :el:defvar:`skk-study-file`
        - :file:`~/.skk-study`
        - :file:`~/.ddskk/study`
      * - :el:defvar:`skk-study-backup-file`
        - :file:`~/.skk-study.BAK`
        - :file:`~/.ddskk/study.bak`
      * - :el:defvar:`skk-bayesian-history-file`
        - :file:`~/.skk-bayesian`
        - :file:`~/.ddskk/bayesian`
      * - :el:defvar:`skk-bayesian-corpus-file`
        - :file:`~/.skk-corpus`
        - :file:`~/.ddskk/corpus`

   なお、 変数 :el:defvar:`skk-user-directory` を設定した場合でも、上記「影響を受
   ける変数」を個別に設定している場合は、その個別の設定が優先されます。

skk-init-file の自動コンパイル
------------------------------

ここでは、「DDSKK の設定ファイル」を ``el`` と、「DDSKK の設定ファイルをバイトコ
ンパイルしたファイル」を ``elc`` とそれぞれ呼びます。

変数 :el:defvar:`skk-byte-compile-init-file` を適切に設定することによって、DDSKK
の起動時に自動的に ``el`` をバイトコンパイルすることができます。

.. list-table::

   * - skk-byte-compile-init-file の値
     - DDSKK の起動時
   * - non-nil
     - | 「 ``elc`` が存在しない」又は「 ``elc`` よりも ``el`` が新しい」ときは、
       | ``el`` をバイトコンパイルした ``elc`` を生成します。
   * - nil
     - ``elc`` よりも ``el`` が新しいときは、 ``elc`` を消去します。

.. el:defvar:: skk-byte-compile-init-file

   設定ファイルの自動バイトコンパイル機能を有効にしたい場合は、
   ファイル :file:`~/.emacs.d/init.el` に

   .. code:: emacs-lisp

      (setq skk-byte-compile-init-file t)

   と記述します。この変数はファイル :file:`~/.skk` が読み込まれる前に調べられるた
   め、ファイル :file:`~/.skk` に上記の設定を記述しても無効です。

フック
======

.. el:defvar:: skk-mode-hook

   :kbd:`C-x C-j` と入力して SKK モードに入る度に呼ばれます。主にバッファローカルの
   設定などを行います。

.. el:defvar:: skk-auto-fill-mode-hook

   :kbd:`C-x j` と入力してオートフィルモード付きで SKK モードに入る度に呼ばれます。
   主にバッファローカルの設定などを行います。

.. el:defvar:: skk-load-hook

   ファイル :file:`skk.el` の読み込みを完了した時点で呼ばれます。ファイル :file:`~/.skk` は
   SKK モードを起動しなければ読み込まれないのに対し、このフックはファイル :file:`skk.el` を
   読み込んだら SKK モードを起動しなくとも呼ばれます。

各ファイルの読み込みが完了した直後に呼ばれるフックは以下のとおり。

.. list-table::

   * - ファイル
     - フック
   * - :file:`skk-act.el`
     - skk-act-load-hook
   * - :file:`skk-auto.el`
     - skk-auto-load-hook
   * - :file:`skk-azik.el`
     - skk-azik-load-hook
   * - :file:`skk-comp.el`
     - skk-comp-load-hook
   * - :file:`skk-gadget.el`
     - skk-gadget-load-hook
   * - :file:`skk-kakasi.el`
     - skk-kakasi-load-hook
   * - :file:`skk-kcode.el`
     - skk-kcode-load-hook
   * - :file:`skk-num.el`
     - skk-num-load-hook
   * - :file:`skk-server.el`
     - skk-server-load-hook

.. index::
   pair: Function; eval-after-load

``load-hook`` が提供されていないプログラムであっても、ロード完了後に何らかの設定
を行いたい場合は、関数 :el:defun:`eval-after-load` を使用します。

.. code:: emacs-lisp

   (eval-after-load "skk-look"
     '(...)
   )

Customize による設定変更
========================

Emacs 標準の Customize 機能を使って SKK を設定することもできます。
ただし、Customize での設定はファイル :file:`~/.emacs.d/init.el` での設定と同様に、
ファイル :file:`/.skk` による設定で上書きされてしまいますので注意してください。

.. index::
   pair: Key; M-x customize-group
   pair: Key; M-x skk-emacs-customize

:kbd:`M-x customize-group` を実行すると skk の設定を対話的に変更することができます。
ミニバッファに :samp:`Customize group:` とプロンプトが表示されます。

.. code:: text

   ------ Minibuffer -------
   Customize group: (default emacs) *
   ------ Minibuffer -------

ここで :kbd:`skk` と答えると、SKK グループの画面へ展開します。
:kbd:`M-x skk-emacs-customize` と実行するのも同様です。

あるいは、モードラインの SKK インジケータをマウスの右ボタン（第３ボタン）でクリッ
クすると表示されるメニューから「SKK をカスタマイズ」を選んでも同じ画面となります。

カスタマイズの使い方は Info を参照してください。

:infonode:`Easy Customization in GNU Emacs Manual <(emacs)Easy Customization>`

skk で設定できる変数の中には、まだこのマニュアルで解説されていないものもあります。
Customize を使うと、それらについても知ることができます。

skk-customize による設定変更
============================

.. el:define-key:: M-x skk-customize

   前述の「Emacs 標準の Customize 機能 M-x customize-group 」による設定が複雑
   すぎると感じるユーザのために、簡易版として :kbd:`M-x skk-customize` を用意して
   います。これは SKK グループのユーザオプションのうち、よく使うものだけ抜粋して
   設定できるようにしたものです。

   これは、モードラインの SKK インジケータをマウスの右ボタン（第３ボタン）でクリ
   ックして表示されるメニューから「SKK をカスタマイズ（簡易版）」を選んで呼び出す
   こともできます。

************************
カタカナ、英字入力の便法
************************

この節では、カタカナや全英文字を入力するための、便利な方法を説明します。

.. _input-katakana:

かなモードからカタカナを入力
============================

.. index::
   pair: Key; q
   keyword: トグル変換

まず、かなモードに入ります。 :kbd:`Q` キーでいったん▽モードにして何かひらがなを
入力し、最後に :kbd:`q` を打鍵すると、カタカナに変換され確定されます。

実際には、ひらがな以外からも変換できます。以下のようになります。

- カタカナ は ひらがな へ
- ひらがな は カタカナ へ
- 全英文字 は アスキー文字 へ
- アスキー文字 は 全英文字 へ

細かく言えば、▽マークとポイント間の文字列の種類 [#]_ をキーとして変換が行われま
す。かなモード、カナモード、どちらでも同じです。

このような変換を *トグル変換* と呼びます。以下はトグル変換の例です。

.. code:: text

   K a t a k a n a

     ------ Buffer: foo ------
     ▽かたかな*
     ------ Buffer: foo ------

   q

     ------ Buffer: foo ------
     カタカナ*
     ------ Buffer: foo ------

このトグル変換を上手く利用することにより、かなモードのまま一時的にカタカナを入力
したり、またその逆を行うことができます。こうすると、例えばひらがな／カタカナが混
在した文章を書くときに、その都度 :kbd:`q` キーを押して入力モードを切り換える必要
がありません [#]_ 。

:ref:`領域を対象としたコマンド <region-operation>` でも「かな←→カナ」のトグル変
換を行うことができます。

.. _input-zenei:

全英文字の入力
==============

まず、かなモードに入ります。次に :kbd:`/` を打鍵すると SKK abbrev モード [#]_ に
入りますのでアルファベット（アスキー文字）を入力します。アルファベットの入力後
に :kbd:`C-q` を打鍵する [#]_ ことで、▽マークから :kbd:`C-q` を打鍵した位置まで
の間にあるアルファベットが全角アルファベットに変換されて確定されます。

.. code:: text

   / f i l e

     ------ Buffer: foo ------
     ▽file*
     ------ Buffer: foo ------

   C-q

     ------ Buffer: foo ------
     ｆｉｌｅ*
     ------ Buffer: foo ------

なお、この変換を行うために、

.. code:: text

   file /ｆｉｌｅ/

のような :ref:`辞書エントリ <jisyo-entry>` を持つ必要はありません。
なぜなら、辞書を参照せずにアスキー文字を１文字ずつ全英文字に変換しているからです。

.. _region-operation:

領域の操作
==========

以下のコマンドを :kbd:`M-x` により呼ぶことで [#]_ 、領域内の文字列を一括変換する
ことができます。

.. el:define-key:: M-x skk-hiragana-region

  カタカナ を ひらがな へ変換

.. el:define-key:: M-x skk-katakana-region

  ひらがな を カタカナ へ変換

.. el:define-key:: M-x skk-latin-region

  全英文字 を アスキー文字 へ変換

.. el:define-key:: M-x skk-jisx0208-latin-region

  アスキー文字 を 全英文字 へ変換

.. index::
   keyword: 逆引き

以下に紹介する「漢字から読みを求めるコマンド」は、外部のコマンド :command:`KAKASI` [#]_ が
必要です。コマンド :command:`KAKASI` がインストールされていなければ使用することができません。

.. el:define-key:: M-x skk-gyakubiki-region

   漢字をひらがなへ変換。具体的な変換例をあげると、

   .. code:: text

     漢字をひらがなへ変換。 → かんじをひらがなへへんかん。

   のようになります。引数を渡して :kbd:`C-u M-x skk-gyakubiki-region` のようにする
   と、複数の候補がある場合に { } で囲って表示します。例えば

   .. code:: text

     中島 → {なかしま|なかじま}

   のようになります。

   送り仮名がある語は、送り仮名まで含めて領域に指定します（さもないと誤変換の原因
   となります）。 例えば「五月蝿い」について、送り仮名「い」を含めずにこのコマン
   ドを実行すると「ごがつはえ」に変換されてしまいます。

.. el:define-key:: M-x skk-gyakubiki-and-henkan

  領域の漢字をひらがなへ変換し、これで得たひらがなを見出し語として漢字変換を実行
  します。

.. el:define-key:: M-x skk-gyakubiki-katakana-region

   漢字をカタカナへ変換。

   引数を渡して :kbd:`C-u M-x skk-gyakubiki-katakana-region` のようにすると、複数
   の候補がある場合に { } で囲って表示します。

.. el:define-key:: M-x skk-hurigana-region

   漢字にふりがなを付ける。例えば、

   .. code:: text

      漢字の脇に → 漢字[かんじ]の脇[わき]に

   のようになります。引数を渡して :kbd:`C-u M-x skk-hurigana-region` のようにする
   と、複数の候補がある場合に { } で囲って表示します。

.. el:define-key:: M-x skk-hurigana-katakana-region

   漢字にカタカナのふりがなを付ける。

   引数を渡して :kbd:`C-u M-x skk-hurigana-katakana-region` のようにすると、複数
   の候補がある場合に { } で囲って表示します。

.. el:define-key:: M-x skk-romaji-region

   漢字、ひらがな、カタカナをローマ字へ、全英文字をアスキー文字へ変換。標準では、
   ローマ字への変換様式はヘボン式です。例えば、

   .. code:: text

      し → shi

   となります。

.. index::
   pair: Key; M-x skk-gyakubiki-message
   pair: Key; M-x skk-gyakubiki-katakana-message
   pair: Key; M-x skk-hurigana-message
   pair: Key; M-x skk-hurigana-katakana-message
   pair: Key; M-x skk-romaji-message

以下のコマンドは、領域内の文字列を置き換える代わりに、変換結果をエコーエリアに表
示します。

- M-x skk-gyakubiki-message
- M-x skk-gyakubiki-katakana-message
- M-x skk-hurigana-message
- M-x skk-hurigana-katakana-message
- M-x skk-romaji-message

.. index::
   pair: 環境変数; KANWADICTPATH

.. el:defvar:: skk-gyakubiki-jisyo-list

   関数 :el:defun:`skk-gyakubiki-region` は、 コマンド :command:`kakasi` を呼び出
   しています。 コマンド :command:`kakasi` には漢字をひらがなへ変換する機能があり、
   この変換には環境変数 ``KANWADICTPATH`` で指定されている辞書を利用しています。

   変数 :el:defvar:`skk-gyakubiki-jisyo-list` を設定することによって コマンド :command:`kakasi` へ
   与える辞書を任意に追加することができます。以下のように設定して コマンド :command:`kakasi` へ
   個人辞書 :el:defvar:`skk-jisyo` を与えることによって :ref:`辞書登録モード <jisyo-register-mode>` で
   登録したばかりの単語も コマンド :command:`kakasi` による逆引き変換の対象とする
   ことができます。

   .. code:: emacs-lisp

      (setq skk-gyakubiki-jisyo-list (list skk-jisyo))

.. el:defvar:: skk-romaji-*-by-hepburn

   この変数の値を nil に設定すると、関数 :el:defun:`skk-romaji-{region|message}` に
   よるローマ字への変換様式に訓令式 [#]_ を用います。標準設定は t です。

  .. code:: text

     し → si

カタカナの見出し語
==================

:kbd:`q` の打鍵でかなモード、カナモードを度々切り替えて入力を続けていると、カナモー
ドで誤って▼モードに入ってしまうことがあります。そのため、カナモードで▼モードに
入った場合は、まず見出し語をひらがなに変換してから辞書の検索に入るよう設計されて
います。なお、この場合の「送りあり変換」での送り仮名は、カタカナになります。

.. _context-skk:

文脈に応じた自動モード切り替え
==============================

.. index::
   pair: File; context-skk.el
   pair: Key; M-x context-skk-mode

ファイル :file:`context-skk.el` は、編集中の文脈に応じて SKK の入力モードを自動的
にアスキーモードに切り替える等の機能を提供します。

ファイル :file:`context-skk.el` をロードするにはファイル :file:`~/.emacs.d/init.el` に

.. code:: emacs-lisp

   (add-hook 'skk-load-hook
             (lambda ()
               (require 'context-skk)))

と書いてください。

あるプログラミング言語のプログラムを書いているとき、日本語入力の必要があるのは一
般に、そのプログラミング言語の文字列中かコメント中に限られます。
たとえば Emacs Lisp で日本語入力の必要があるのは

.. code:: emacs-lisp

   "文字列"
   ;; コメント

といった個所だけでしょう。文字列・コメントの *外* を編集するときは、多くの場合は
日本語入力は必要ありません。

現在の文字列・コメントの *外* で編集開始と同時に（skk がオンであれば） skk の入力
モードをアスキーモードに切り替えます。エコーエリアに

.. code:: text

   -------------------- Echo Area --------------------
   [context-skk] 日本語入力 off
   -------------------- Echo Area --------------------

と表示され、アスキーモードに切り替わったことが分かります。これにより、文字列・コ
メントの *外* での編集を開始するにあたって、日本語入力が on になっていたために発
生する入力誤りとその修正操作を回避することができます。

上記の機能は context-skk-mode というマイナーモードとして実装されており
:kbd:`M-x context-skk-mode` でオン／オフを制御できます。オンの場合、モードライン
のメジャーモード名の隣に「;▽」と表示されます。

.. el:defvar:: context-skk-programming-mode

   context-skk が「プログラミングモード」と見做すメジャーモード。

.. el:defvar:: context-skk-mode-off-message

   アスキーモードに切り替わった瞬間にエコーエリアに表示するメッセージ。

.. _completion:

****
補完
****

読みの前半だけを入力して :kbd:`TAB` を押せば残りを自動的に補ってくれる、これが補
完です。 Emacs ユーザにはおなじみの機能が DDSKK でも使えます。

よく使う長い語を効率良く入力するには、アルファベットの略語を登録する方法もありま
す。

:ref:`アスキー文字を見出し語とした変換 <conv-ascii-midasi>`

読みの補完
==========

.. index::
   pair: Key; TAB

▽モードで :kbd:`TAB` を押すと、見出し語（▽マークからポイントまでの文字列）に対
する補完 [#]_ が行われます。見出し語補完は、個人辞書のうち「送りなしエントリ」に
対して行われます。個人辞書に限っているのは、共有辞書では先頭の文字を共通にする見
出し語が多すぎて、望みの補完が行える確率が低いためです。

.. index::
   pair: Key; .
   pair: Key; ,

次の読みの候補を表示するには :kbd:`.` （ピリオド）を、戻る時には :kbd:`,` （コンマ）
を押します。その読みで別の語を出すには、いつものように :kbd:`SPC` を押します。

例を見てみましょう。実際の動作は、個人辞書の内容によって異なります。

.. code:: text

   S a

     ------ Buffer: foo ------
     ▽さ*
     ------ Buffer: foo ------

   TAB

     ------ Buffer: foo ------
     ▽さとう*
     ------ Buffer: foo ------

   .

     ------ Buffer: foo ------
     ▽さいとう*
     ------ Buffer: foo ------

   ,

     ------ Buffer: foo ------
     ▽さとう*
     ------ Buffer: foo ------

   SPC

     ------ Buffer: foo ------
     ▼佐藤*
     ------ Buffer: foo ------

   C-j

     ------ Buffer: foo ------
     佐藤*
     ------ Buffer: foo ------

補完される見出し語がどのような順で表示されるかと言うと「最近使われた語から」とな
ります。例えば「斉藤」、「佐藤」の順で変換した後、「さ」をキーにして見出し語の補
完を行うと、最初に「さとう」が、その次に「さいとう」が補完されます。これは、個人
辞書では、最近使われたエントリほど上位に来るようになっているためです。

:ref:`辞書の書式 <jisyo-format>`

いったん :kbd:`SPC` を入力して▼モードに入ると、以後は見出し語補完は行われません。

.. index::
   pair: Key; C-u TAB

また、 :kbd:`.` の代わりに :kbd:`C-u TAB` を入力すると、現在の候補に対して補完を
します。上の例では「さ」に対し「さとう」が補完された時に :kbd:`C-u TAB` を押すと、
以後の補完は「さとう」を含む語（例えば「さとうせんせい」など）について行われます。

.. el:defvar:: skk-completion-prog-list

  補完関数、補完対象の辞書を決定するためのリスト。標準設定は以下のとおり。

  .. code:: emacs-lisp

     '((skk-comp-by-history)
       (skk-comp-from-jisyo skk-jisyo)
       (skk-look-completion))

.. el:defvar:: skk-comp-circulate

   :kbd:`.` （ピリオド）で次の見出し語候補を、 :kbd:`,` （コンマ）で前の見出し語
   候補を表示するところ、候補が尽きていれば標準設定 nil では「○○で補完すべき見
   出し語は他にありません」とエコーエリアに表示して動作が止まります。
   この変数が non-nil であれば当初の見出し語を再び表示して見出し語補完を再開しま
   す。

.. el:defvar:: skk-try-completion-char

   見出し語補完を開始するキーキャラクタです。標準設定は :kbd:`TAB` です。

.. el:defvar:: skk-next-completion-char

   次の見出し語候補へ移るキーキャラクタです。標準設定はピリオド :kbd:`.` です。

.. el:defvar:: skk-previous-completion-char

   前の見出し語候補へ戻るキーキャラクタです。標準設定はコンマ :kbd:`,` です。

.. index::
   pair: Key; backtab
   pair: Key; SHIFT TAB

.. el:defvar:: skk-previous-completion-use-backtab

   Non-nil であれば、前の見出し語候補へ戻る動作を :kbd:`SHIFT` ＋ :kbd:`TAB` で
   も可能とします。標準設定は t です。この機能の有効化／無効化の切り替えは、
   ファイル :file:`~/.skk` を書き換えて Emacs を再起動してください。

.. el:defvar:: skk-previous-completion-backtab-key

   :kbd:`SHIFT` + :kbd:`TAB` が発行する key event です。Emacs の種類／実行環境に
   よって異なります。

.. el:defun:: skk-comp-lisp-symbol &optional PREDICATE

   この関数をリスト :el:defvar:`skk-completion-prog-list` へ追加すると、Lisp symbol 名の
   補完を行います。

   .. code:: emacs-lisp

      (add-to-list 'skk-completion-prog-list
                   '(skk-comp-lisp-symbol) t)

補完しながら変換
================

.. index::
   pair: Key; M-SPC

前節で見出し語の補完について述べました。本節では、見出し語の補完動作を行った後、
:kbd:`SPC` を打鍵し、▼モードに入るまでの動作を一回の操作で行う方法について説明し
ます。

やり方は簡単。 :kbd:`TAB` ・ :kbd:`SPC` と打鍵していたところを :kbd:`M-SPC` に換
えると、見出し語を補完した上で変換を開始します。

この方法によると、補完される見出し語があらかじめ分かっている状況では、キー入力を
一回分省略できるので、読みが長い見出し語の単語を連続して入力する場合などに威力を
発揮します。

.. code:: text

   K a s i t a n n p o s e k i n i n n

     ------ Buffer: foo ------
     ▽かしたんぽせきにん*
     ------ Buffer: foo ------

   SPC RET

     ------ Buffer: foo ------
     瑕疵担保責任*
     ------ Buffer: foo ------

   K a

     ------ Buffer: foo ------
     ▽か*
     ------ Buffer: foo ------

   M-SPC

     ------ Buffer: foo ------
     ▼瑕疵担保責任*
     ------ Buffer: foo ------

.. el:defvar:: skk-start-henkan-with-completion-char

  標準設定は :kbd:`M-SPC` です。

.. _dcomp:

動的補完
========

▽モードでは :kbd:`TAB` を押さなくとも、文字を入力する都度、自動的に見出し語補完
の読みを表示させる事ができます。この機能を以下「動的補完」と呼びます。
類似の機能としては、ウェブブラウザの URL の入力や、Microsoft Excel のセル入力の自
動補完 [#]_ をイメージすると分かりやすいかも知れません。動的補完も、個人辞書の送
りなしエントリに対してのみ行なわれます。

動的補完を利用するにはファイル :file:`~/.skk` に次の式を書きましょう。

.. code:: emacs-lisp

   (setq skk-dcomp-activate t)

例を見てみましょう。実際の動作は、個人辞書の内容によって左右されます。
``*`` はポイント位置を表します。

.. code:: text

   H o

     ---------------- Buffer: foo ------------------
     ▽ほ*んとう
     ---------------- Buffer: foo ------------------

face が使える環境では「んとう」の部分が異なる face で表示され、動的補完機能によっ
て補完された部分であることを示します。

自動的に補完された見出し語が自分の意図したものであれば :kbd:`TAB` を押すことでポ
イント位置を動かし、補完された見出し語を選択することができます。

.. code:: text

   TAB

     ---------------- Buffer: foo ------------------
     ▽ほんとう*
     ---------------- Buffer: foo ------------------

この状態から :kbd:`SPC` を押して変換するなり、 :kbd:`q` を押してカタカナにするな
り、DDSKK 本来の動作を何でも行うことができます。

補完された見出し語が自分の意図したものでない場合は、かまわず次の入力を続けて下さ
い。補完された部分を無視したかのように動作します。

.. code:: text

   H o

     ---------------- Buffer: foo ------------------
     ▽ほ*んとう
     ---------------- Buffer: foo ------------------

   k a

     ---------------- Buffer: foo ------------------
     ▽ほか*ん
     ---------------- Buffer: foo ------------------

補完されない状態が自分の意図したものである場合も、補完された部分を単に無視するだ
けで OK です。下記の例では「ほ」を見出し語とした変換を行っています。

.. code:: text

   H o

     ---------------- Buffer: foo ------------------
     ▽ほ*んとう
     ---------------- Buffer: foo ------------------

   SPC

     ---------------- Buffer: foo ------------------
     ▼保
     ---------------- Buffer: foo ------------------

補完された状態から :kbd:`BS` を押すと、消された補完前の見出し語から再度補完動作を
行います。

.. code:: text

   H o

     ---------------- Buffer: foo ------------------
     ▽ほ*んとう
     ---------------- Buffer: foo ------------------

   k a

     ---------------- Buffer: foo ------------------
     ▽ほか*ん
     ---------------- Buffer: foo ------------------

   BS

     ---------------- Buffer: foo ------------------
     ▽ほ*んとう
     ---------------- Buffer: foo ------------------

.. el:defvar:: skk-dcomp-activate

   この変数の値が Non-nil であれば、カーソル位置に関わらず常に動的補完が有効とな
   ります。値がシンボル 'eolp であれば、カーソルが行末にあるときに限って動的補完
   が有効となります。値が nil であれば、動的補完機能は無効となります。

.. el:defface:: skk-dcomp-face

   この変数の値はフェイスであり、このフェイスによって動的に補完された部分が装飾さ
   れます。標準は DarkKhaki です。

.. el:defvar:: skk-dcomp-multiple-activate

   Non-nil であれば、動的補完の候補をインラインに複数表示 [#]_ します。

   .. code:: text

      ---------------- Buffer: foo ------------------
      ▽ほ*んとう
        ほんとう
        ほかん
        ほっかいどう
        ほうほう
        :
      ---------------- Buffer: foo ------------------

   候補の選択には :kbd:`TAB` 又は :kbd:`SHIFT` + :kbd:`TAB` を押します。
   また、 :ref:`普通の補完<completion>` と同様に :kbd:`.` （ピリオド）
   と :kbd:`,` （コンマ）も利用できます。

.. el:defvar:: skk-dcomp-multiple-rows

   動的補完の候補を複数表示する場合の表示行数。標準は 7。

.. el:defface:: skk-dcomp-multiple-face

   動的補完の複数表示群のフェイス。上記例では「ほ」のフェイス。

.. el:defface:: skk-dcomp-multiple-trailing-face

   動的補完の複数表示群の補完部分のフェイス。上記例では「んとう」、「かん」
   「っかいどう」、「うほう」のフェイス。

.. el:defface:: skk-dcomp-multiple-selected-face

   動的補完の複数表示群の選択対象のフェイス。上記例では :kbd:`TAB` を押すたびに
   「ほんとう」、「ほかん」、「ほっかいどう」と選択位置が移ります。その現在選択位
   置に適用するフェイスです。

************************
便利な変換、その他の変換
************************

.. _tankan:

単漢字変換
==========

.. index::
   pair: File; skk-tankan.el
   pair: Key; @

ファイル :file:`skk-tankan.el` を読み込むことによって単漢字変換が可能となります。
候補は総画数の昇順でソートして表示します。

単漢字変換を使うには設定が必要ですが、先に例を見てみましょう。▽モードの最後の文
字に :kbd:`@` を付して変換を開始してください。

.. code:: text

   T a n @

     ----- Buffer: foo -----
     ▽たん@*
     ----- Buffer: foo -----

   SPC

     ----- Buffer: foo -----
     ▼丹*
     ----- Buffer: foo -----

     ----- Echo Area -----
     4画(丶部3画)
     ----- Echo Area -----

   SPC

     ----- Buffer: foo -----
     ▼反*
     ----- Buffer: foo -----

     ----- Echo Area -----
     4画(又部2画)
     ----- Echo Area -----

   SPC

     ----- Buffer: foo -----
     ▼旦*
     ----- Buffer: foo -----

     ----- Echo Area -----
     5画(日部1画)
     ----- Echo Area -----

   SPC

     ----- Buffer: foo -----
     ▼但*
     ----- Buffer: foo -----

     ----- Echo Area -----
     7画(人部5画)
     ----- Echo Area -----

   SPC

     ----- Buffer: foo -----
     ▼*
     ----- Buffer: foo -----

     ----- Buffer: *候補* -----
     A:坦;8画(土部5画)
     S:担;8画(手部5画)
     D:単;9画(十部7画)
     F:彖;9画(彑部6画)
     J:炭;9画(火部5画)
     K:眈;9画(目部4画)
     L:胆;9画(肉部5画)
     [残り 50+++++]
   ----- Buffer: *候補* -----

以上のとおり、総画数の昇順でソートされた候補が次々に表示されます。

検索キーの設定
--------------

標準設定の検索キーは :kbd:`@` です。DDSKK の標準設定ではキー :kbd:`@` は
関数 :el:defun:`skk-today` の実行に割り当てられていますが、DDSKK 14.2 からは特段の
設定なしに▽モードで :kbd:`@` の打鍵が可能となりました。

.. el:defvar:: skk-tankan-search-key

   単漢字変換の検索キー。以下は、検索キーを :kbd:`!` へと変更する例です。

   .. code:: emacs-lisp

      (setq skk-tankan-search-key ?!)

辞書の設定
----------

DDSKK 14.2 からは標準で変数 :el:defvar:`skk-search-prog-list` に関数 :el:defun:`skk-tankan-search` が
含まれています。DDSKK 14.1 を利用の方、ご自身で変数 :el:defvar:`skk-search-prog-list` を
設定する方は以下の解説を参考にしてください。

ファイル :file:`skk-tankan.el` には、漢字の部首とその中での画数のデータのみが入っ
ています。読みのデータは、普通の辞書ファイルを使います。

単漢字変換の辞書の設定は、変数 :el:defvar:`skk-search-prog-list` に以下の形式で要
素を追加します。

.. code:: emacs-lisp

   (skk-tankan-search 'function . args)

*確定変換* を併用する場合は、変数 :el:defvar:`skk-search-prog-list` の先頭の要素
は関数 :el:defun:`skk-search-kakutei-jisyo-file` でなければいけませんので、
変数 :el:defvar:`skk-search-prog-list` の２番目の要素に関数 :el:defun:`skk-tankan-search` を
追加します。

.. code:: emacs-lisp

   ;; skk-search-prog-list の２番目の要素に skk-tankan-search を追加する
   (setq skk-search-prog-list
         (cons (car skk-search-prog-list)
               (cons '(skk-tankan-search 'skk-search-jisyo-file
                                         skk-large-jisyo 10000)
                     (cdr skk-search-prog-list))))

なお、確定変換を使用しない場合は、変数 :el:defvar:`skk-search-prog-list` の要素の
先頭が関数 :el:defun:`skk-tankan-search` でも大丈夫です。

.. code:: emacs-lisp

   (add-to-list 'skk-search-prog-list
                '(skk-tankan-search 'skk-search-jisyo-file
                                    skk-large-jisyo 10000))

:ref:`辞書の検索方法の設定 <search-jisyo>`

総画数による単漢字変換
----------------------

▽モードで総画数を入力して最後に :kbd:`@` を付してから変換を開始します。
:kbd:`C-u 総画数 M-x skk-tankan` でも可能です。

.. code:: emacs-lisp

    Q 1 0 @

      ----- Buffer: foo -----
      ▽10@*
      ----- Buffer: foo -----

    SPC

      ----- Buffer: *候補* -----
      A:倹;10画(人部8画)
      S:倦;10画(人部8画)
      D:個;10画(人部8画)
      F:候;10画(人部8画)
      J:倖;10画(人部8画)
      K:借;10画(人部8画)
      L:修;10画(人部8画)
      [残り 532+++++++]
      ----- Buffer: *候補* -----

部首による単漢字変換
--------------------

▽モードで :kbd:`@` を２つ重ねて変換を開始すると、部首による単漢字変換ができます。
:kbd:`M-x skk-tankan` でも可能です。

.. code:: emacs-lisp

    Q @ @

      ----- Buffer: foo -----
      ▽@@*
      ----- Buffer: foo -----

    SPC

      ------ Minibuffer -------
      部首を番号で選択（TABで一覧表示）: *
      ------ Minibuffer -------

    TAB

      ------ *Completions* -------
      Click <mouse-2> on a completion to select it.
      In this buffer, type RET to select the completion near point.

      Possible completions are:
      001 一 (いち)                      002 ｜ (ぼう、たてぼう)
      003 丶 (てん)                      004 丿 (の)
      005 乙 (おつ)                      006 亅 (はねぼう)
       ：                                 ：
      ------ *Completions* -------

    0 1 8 RET
    注) M-v の打鍵で、カーソルを *Completions* バッファへ移すこともできます。

      ----- Buffer: *候補* -----
      A:切;4画(刀部2画)
      S:刈;4画(刀部2画)
      D:刊;5画(刀部3画)
      F:刋;5画(刀部3画)
      J:刎;6画(刀部4画)
      K:刑;6画(刀部4画)
      L:刔;6画(刀部4画)
      [残り 51+++++++]
      ----- Buffer: *候補* -----

.. el:defface:: skk-tankan-face

   :kbd:`M-x skk-tankan` を実行したときに表示される「単漢字バッファ」で使用するフ
   ェイスです。

.. el:defface:: skk-tankan-radical-name-face

   部首の読みに適用するフェイスです。

部首の読みによる単漢字変換
--------------------------

直前の小々節「部首による単漢字変換」にて、部首番号を入力するプロンプトで
単に :kbd:`RET` を打鍵すると、部首の読みを入力するプロンプトに替わります。

.. code:: text

      ------ Minibuffer -------
      部首を読みで選択（TABで一覧表示）: *
      ------ Minibuffer -------

    TAB

      ------ Completion List -------
      In this buffer, type RET to select the completion near point.

      Possible completions are:
      あいくち         (021) 匕          あお             (174) 青
      あか             (155) 赤          あくび           (076) 欠
      あさ             (200) 麻          あさかんむり     (200) 麻
       ：                                 ：
      ------ Completion List -------

.. _skk-hint:

候補の絞り込み
==============

ファイル :file:`skk-hint.el` は、２つの読みの積集合みたいなものを取ることによって
候補の絞り込みを行うプログラムです。インストールはファイル :file:`~/.skk` に以下
を記入します。

.. code:: emacs-lisp

    (require 'skk-hint)

例えば、読み「かんどう」に対する変換は L 辞書によると

.. code:: text

    感動、勘当、完動、間道、官道、貫道

と複数の候補があります。一方、これに「あいだ」という「他の読み」（ヒント）を与え
ると候補は「間道」に一意に決まります。

ヒントは :kbd:`;` に続けて入力します。

.. code:: text

    K a n d o u ; a i d a   ※ ; 自体は表示されません。

      ----- Buffer: foo -----
      ▽かんどうあいだ
      ----- Buffer: foo -----

    SPC

      ----- Buffer: foo -----
      ▼間道
      ----- Buffer: foo -----

ファイル :file:`skk-hint.el` は、２つの読みの厳密な積集合を取っているわけではなく、
通常の変換候補のなかでヒントとして与えられた読みを含んだ漢字を持つものに候補を絞
ります。この実例として「感動」と「感圧」を挙げます。

.. code:: text

    K a n d o u ; k a n n a t u

      ----- Buffer: foo -----
      ▽かんどうかんあつ
      ----- Buffer: foo -----

    SPC

      ----- Buffer: foo -----
      ▼感動
      ----- Buffer: foo -----

ファイル :file:`skk-hint.el` は単漢字の候補がたくさんある場合に、そこから候補を絞
りこむ手段としても非常に有効です。例えば

.. code:: text

    ▽わ*

を変換すると、輪、環、話、和、羽、… と大量に候補が出てきます。この中から「和」を
選びたいとします。普通に変換していてもそのうち「和」が表示されますが、
これを :kbd:`W a ; h e i w a` と入力し変換すると、「▼へいわ」の候補である「平和」
に含まれる

.. code:: text

    ▼和*

が唯一の候補となります。

.. code:: text

    W a ; h e i w a

      ----- Buffer: foo -----
      ▽わへいわ*
      ----- Buffer: foo -----

    SPC

      ----- Buffer: foo -----
      ▼和*
      ----- Buffer: foo -----

.. el:defvar:: skk-hint-start-char

   ヒント変換を開始するキーを character で指定します。

接頭辞・接尾辞
==============

接頭辞 (prefix)、接尾辞 (suffix) の入力のために特別な方法が用意されています。
たとえば、「し」の候補は沢山あり、「し」から「氏」を変換するのは、そのままでは効
率が悪いです。接尾辞の「し」ならば、「氏」や「市」が優先されるでしょう。

接頭辞・接尾辞は、辞書の中では ``>`` などで示されます。

.. code:: text

    >し /氏/

という :ref:`辞書エントリ <jisyo-entry>` があるとき、「小林氏」を接尾辞入力を用い
て、以下のように入力することができます。

.. code:: text

    K o b a y a s h i

      ------ Buffer: foo ------
      ▽こばやし*
      ------ Buffer: foo ------

    SPC

      ------ Buffer: foo ------
      ▼小林*
      ------ Buffer: foo ------

    >

      ------ Buffer: foo ------
      小林▽>*
      ------ Buffer: foo ------

    s i

      ------ Buffer: foo ------
      小林▽>し*
      ------ Buffer: foo ------

    SPC

      ------ Buffer: foo ------
      小林▼氏*
      ------ Buffer: foo ------


    C-j

      ------ Buffer: foo ------
      小林氏*
      ------ Buffer: foo ------

接頭辞も同様です。辞書に

.. code:: text

    ちょう> /超/

という :ref:`辞書エントリ <jisyo-entry>` があるとき、「超大型」を接頭辞入力を用い
て、以下のように入力することができます。

.. code:: text

    T y o u

      ------ Buffer: foo ------
      ▽ちょう*
      ------ Buffer: foo ------

    >

      ------ Buffer: foo ------
      ▼超*
      ------ Buffer: foo ------

    O o g a t a

      ------ Buffer: foo ------
      超▽おおがた*
      ------ Buffer: foo ------

    SPC

      ------ Buffer: foo ------
      超▼大型*
      ------ Buffer: foo ------

    C-j

      ------ Buffer: foo ------
      超大型*
      ------ Buffer: foo ------

キー :kbd:`>` を押しただけで :kbd:`SPC` が押されたかのように変換されます。他の接
頭辞を選びたいときは :kbd:`SPC` を押して下さい。

.. el:defvar:: skk-special-midashi-char-list

   ▽モードまたは▼モードにおいて、この変数の値に含まれる文字の入力があっ
   た場合、接頭辞・接尾辞の入力を開始します。この変数の標準設定は、

   .. code:: emacs-lisp

       (?> ?< ??)

   です。つまり、 :kbd:`>` と :kbd:`<` と :kbd:`?` を入力した時に接頭辞・接尾辞入
   力を行います。
   :kbd:`?` を入力したときに接頭辞・接尾辞入力を行わない場合は :kbd:`?` を外して

   .. code:: emacs-lisp

       (setq skk-special-midashi-char-list '(?> ?<))

   とします。L 辞書の接頭・接尾辞は、昔は :kbd:`<` と :kbd:`?` も使われていました
   が、 現在は :kbd:`>` に統一されています。

.. _number-conv:

数値変換
========

DDSKK は **数字を含む見出し語** を様々な候補に変換することができます。例えば、見
出し語「だい12かい」を変換すると「第１２回」、「第一二回」、「第十二回」といった
候補を挙げます。

この節では、このような候補を辞書に登録する方法を説明します。基本は、数字の部分を
# で置き替えることです。辞書ファイル :file:`SKK-JISYO.L` の :ref:`辞書エントリ <jisyo-entry>`
から具体例を見てみましょう。

.. code:: text

    だい#かい /第#1回/第#0回/第#2回/第#3回/第 #0 回/

「だい12かい」のような数字を含む見出し語を変換した場合、見出し語の中の数字の部分
は自動的に # に置き換えられますので、 :ref:`辞書エントリ <jisyo-entry>` の左辺
（つまり見出し語）である「だい#かい」にマッチします。

:ref:`辞書エントリ <jisyo-entry>` の右辺の #1 、 #2 などは「どのように数字を加工
するか」のタイプを表します。以下、各タイプについて説明します。

.. list-table::

  * - 各タイプ
    - 説明
  * - #0
    - | 無変換。入力されたアスキー文字をそのまま出力します。
      | 例えば、「第12回」のような変換を得るために使います。
  * - #1
    - 全角文字の数字。 12 を「１２」に変換します。
  * - #2
    - 漢数字で位取りあり。1024 を「一〇二四」に変換します。
  * - #3
    - 漢数字で位取りなし。1024 を「千二十四」に変換します。
  * - #4
    - | 数値再変換。
      | 見出し語中の数字そのもの [#]_ をキーとして辞書を再検索し、
      | #4 の部分を再検索の結果の文字列で入れ替えます。
      | これについては後で例を挙げて説明します。
  * - #5
    - | 小切手や手形の金額記入の際用いられる表記で変換します。
      | 例えば、1995 を「壱阡九百九拾伍」に変換します。これを大字と言います。
  * - #8
    - 桁区切り。1234567 を 1,234,567 に変換します。
  * - #9
    - | 将棋の棋譜の入力用。
      | 「全角数字＋漢数字」に変換します。これについては後で例を挙げて説明します。

以下にいくつか例を示します。辞書に

.. code:: text

   # /#3/

という :ref:`辞書エントリ <jisyo-entry>` があるときに、
:kbd:`Q 1 0 0 2 0 0 3 0 0 4 0 0 5 0 0 SPC` または
:kbd:`/ 1 0 0 2 0 0 3 0 0 4 0 0 5 0 0 SPC` とキー入力 [#]_ すれば、
「百兆二千三億四十万五百」と変換されます。

辞書に

.. code:: text

    #m#d /#0月#0日/

という :ref:`辞書エントリ <jisyo-entry>` があるときに
:kbd:`/ 2 m 2 5 d SPC` と入力 [#]_ すれば、「2月25日」と変換されます。

辞書に

.. code:: text

    #kin /#9金/

という :ref:`辞書エントリ <jisyo-entry>` があるときに
:kbd:`/ 3 4 k i n SPC` と入力すれば、「３四金」と変換されます。

辞書に

.. code:: text

    p# /#4/
    125 /東京都葛飾区/

という :ref:`辞書エントリ <jisyo-entry>` があるときに
:kbd:`/ p 1 2 5 SPC` と入力すれば、見出し語 p125 の候補が #4 なので、
見出し語の数字部分の 125 に対して辞書が再検索され、「東京都葛飾区」と変換されます。

最後に、実際に登録する例をひとつ挙げます。「２月２５日」を得るために、
:kbd:`Q 2 g a t u 2 5 n i t i SPC` とキー入力したときに、辞書に見出し語

.. code:: text

    #がつ#にち /#1月#1日/

がないときは、 :ref:`辞書登録モード <jisyo-register-mode>` のプロンプトは :samp:`「#がつ#にち」`
となります。 全角数字のタイプは #1 なので「#1月#1日」をミニバッファで作り登録します。

タイプを覚えている必要はありません。ちゃんと、ウィンドウが開かれて説明が表示され
ます。

.. el:defvar:: skk-num-convert-float

   この変数の値を non-nil に設定すると、浮動小数点数を使った見出し語に対応し
   て数値変換を行います。ただし、辞書において

   .. code:: text

       #.# /#1．#1/#0月#0日/

   などの見出し語が使用できなくなります。

.. el:defvar:: skk-show-num-type-info

   Non-nil であれば、 :ref:`辞書登録モード <jisyo-register-mode>` に入るのと
   同時に変換タイプの案内を表示します。標準設定は t です。

.. el:defvar:: skk-num-grouping-separator

   タイプ #8 で使用する記号。標準設定は ``,`` 。

.. el:defvar:: skk-num-grouping-places

   タイプ #8 について、何桁毎に区切るのかを数値で指定する。標準設定は 3。

.. el:defvar:: skk-use-numeric-conversion

   この変数を nil に設定すると、本節で説明した数値変換の機能を全て無効にしま
   す。

.. _conv-ascii-midasi:

アスキー文字を見出し語とした変換
================================

かなモードで :kbd:`/` を打鍵すると **SKK abbrev モード** に入り、以後の入力はアス
キー文字になります。普通に :kbd:`SPC` を押すと、その見出し語に係る変換が得られま
す。

仮に、辞書に

.. code:: text

    is /インクリメンタル・サーチ/

という :ref:`辞書エントリ <jisyo-entry>` があるとして、以下に例を示します。

.. code:: text

    /

      ------ Buffer: foo ------
      ▽*
      ------ Buffer: foo ------

    i s

      ------ Buffer: foo ------
      ▽is*
      ------ Buffer: foo ------

    SPC

      ------ Buffer: foo ------
      ▼インクリメンタル・サーチ*
      ------ Buffer: foo ------

    C-j

      ------ Buffer: foo ------
      インクリメンタル・サーチ*
      ------ Buffer: foo ------

候補を確定すると SKK abbrev モードを抜けてかなモードに戻ります。

SKK abbrve モードで使われる辞書は、普通のかな漢字変換と同じです。見出し語がアスキ
ー文字で書かれているだけで、特殊な点はありません。

上記の例において :kbd:`SPC` の代わりに :kbd:`C-q` を打鍵することで、入力したアス
キー文字をそのまま全角アルファベットに変換することもできます。

:ref:`全英文字の入力 <input-zenei>`

なお、SKK abbrev モードにおいても :kbd:`TAB` による :ref:`見出し語の補完 <completion>`
を行うことができます。

.. _input-today:

今日の日付の入力
================

かなモード／カナモードで :kbd:`@` を入力すれば、今日の日付が入力されます。

日付の形式は以下の変数により決定されます。

.. el:defvar:: skk-date-ad

   この変数の値が non-nil であれば西暦で、 nil であれば元号で表示します。
   標準設定は nil です。

.. el:defvar:: skk-number-style

   この変数の値は以下のように解釈されます。標準設定は 1 です。

   .. list-table::

      * - 設定値
        - 出力結果
      * - 0 or nil
        - | ASCII 数字
          | 「1996年7月21日(日)」のようになります。
      * - 1 or t
        - | 全角数字
          | 「１９９６年７月２１日(日)」のようになります。
      * - 2
        - | 漢数字（位取）
          | 「一九九六年七月二一日(日)」のようになります。
      * - 3
        - | 漢数字
          | 「千九百九十六年七月二十一日(日)」のようになります。

上記の「1996年」、「１９９６年」、「一九九六年」の部分は、変数 :el:defvar:`skk-date-ad` の
値が nil であれば「平成8年」のように元号で表示されます。

.. index::
   pair: File; SKK-JISYO.lisp

辞書ファイル :file:`SKK-JISYO.lisp` には、見出し語 ``today`` の候補として関数 :el:defun:`skk-date-ad` と
変数 :el:defvar:`skk-number-style` の全ての組み合わせが :ref:`プログラム実行変換 <program-conversion>`
機能を用いて登録されています。従って、 :kbd:`/ t o d a y SPC` と入力すると、今日
の日付が上記の形式で順次候補として表示されます。

関数 :el:defun:`skk-relative-date` を利用すると、昨日、一昨日、明後日など任意の日
付を求めることができます。詳細はファイル :file:`skk-gadget.el` のコメントを参照し
てください。

なお、 :kbd:`@` の打鍵で日付を挿入するのではなく、文字どおり @ を挿入したい場合の
設定は次のとおり。

.. code:: emacs-lisp

    (setq skk-rom-kana-rule-list
          (append skk-rom-kana-rule-list
                  '(("@" nil "@"))))

.. _program-conversion:

プログラム実行変換
==================

辞書の候補に Emacs Lisp のプログラムが書いてあれば、そのプログラムを Emacs に実行
させ、返り値をカレントバッファに挿入します。これを **プログラム実行変換** と呼ん
でいます。例えば、辞書に

.. code:: text

    now /(current-time-string)/

という :ref:`辞書エントリ <jisyo-entry>` があるとします。
このとき :kbd:`/ n o w SPC` とキー入力すれば、
現在のバッファに関数 :el:defun:`current-time-string` の返り値である

.. code:: text

    Sun Jul 21 06:40:34 1996

のような文字列が挿入されます。

ここで、プログラムの返り値は文字列である必要があります。
また、 :ref:`プログラム実行変換 <program-conversion>` の辞書登録は通常の単語と同
様に行うことができますが、その中に改行を含まないように書く必要 [#]_ があります。

:ref:`今日の日付の入力 <input-today>` で説明した ``today`` の :ref:`辞書エントリ <jisyo-entry>`
は、実際は下記のようなプログラムを候補に持っています。

.. code:: emacs-lisp

    today /(let ((skk-date-ad) (skk-number-style t)) (skk-today))/.../

ファイル :file:`skk-gadget.el` には、西暦／元号変換や簡単な計算など
:ref:`プログラム実行変換 <program-conversion>` 用の関数が集められています。

.. el:defun:: skk-calc operator

   関数 :el:defun:`skk-calc` は、引数をひとつ取り、見出し語の数字に対しその演算を
   行う簡単な計算プログラムです。

   .. code:: emacs-lisp

       (defun skk-calc (operator)
         ;; 2つの引数を取って operator の計算をする。
         ;; 注意: '/ は引数として渡せないので (defalias 'div '/) などとし、別の形で
         ;; skk-calc に渡す。
         ;; 辞書エントリの例 -> #*# /(skk-calc '*)/
         (number-to-string (apply operator
                                  (mapcar 'string-to-number
                                          skk-num-list))))

   この関数を実際に :ref:`プログラム実行変換 <program-conversion>` で利用するには、
   辞書に以下のような :ref:`辞書エントリ <jisyo-entry>` を追加します。 :ref:`数値変換 <number-conv>`

   .. code:: text

       #*# /(skk-calc '*)/

   :kbd:`Q 1 1 1 * 4 5 SPC` とキー入力します。ここで 111 と 45 の２つの数
   字は、変換時に :code:`("111" "45")` のような文字列のリストにまとめられ、
   変数 :el:defvar:`skk-num-list` の値として保存されます。
   次に関数 :el:defun:`skk-calc` が呼ばれます。この中で変数 :el:defvar:`skk-num-list` の各要素に対
   し演算を行うため、各要素は数に変換されます。
   その上で関数 :el:defun:`skk-calc` に与えられた引数（この場合は ``*`` ）を演算
   子として演算を行います。

.. el:defun:: skk-gadget-units-conversion 基準単位 数値 変換単位

   数値について、基準単位から変換単位への変換を行います。

   .. code:: text

       / 1 3 m i l e

         ------ Buffer: foo ------
         ▽13mile*
         ------ Buffer: foo ------

       SPC

         ------ Buffer: foo ------
         ▼20.9209km*
         ------ Buffer: foo ------

       RET

         ------ Buffer: foo ------
         20.9209km*
         ------ Buffer: foo ------

   単位変換の情報は、変数 :el:defvar:`skk-units-alist` で定義されています。

.. el:defvar:: skk-units-alist

   この変数は以下の形式の連想リストです。

   .. code:: emacs-lisp

       (基準となる単位 (変換する単位 . 変換時の倍率)
                       (… . …))

   関数 :el:defun:`skk-gadget-units-conversion` で利用されています。標準設定では、
   以下の単位変換の情報を定義しています。

   .. code:: emacs-lisp

       ("mile" ("km" . 1.6093)
               ("yard" . 1760))

       ("yard" ("feet" . 3)
               ("cm" . 91.44))

       ("feet" ("inch" . 12)
               ("cm" . 30.48))

       ("inch" ("feet" . 0.5)
               ("cm" . 2.54))

.. el:defun:: skk-relative-date pp-function format and-time &key (yy 0) (mm 0) (dd 0)

   関数 :el:defun:`skk-current-date` の拡張版。
   引数 PP-FUNCTION, FORMAT, AND-TIME の意味は関数 :el:defun:`skk-current-date` の
   docstring を参照のこと。
   キーワード変数 :yy, :mm, :dd に正または負の数値を指定することで明日、明後日、
   一昨日などの日付を求めることができる。詳細はファイル :file:`skk-gadget.el` の
   コメントを参照のこと。

空白・改行・タブを含んだ見出し語の変換
======================================

変換の際、見出し語の中の空白、改行、タブは無視されます。

.. code:: text

      ---------------- Buffer: foo ------------------
      ▽じんじょうしょ
      うがっこう*
      ---------------- Buffer: foo ------------------

    SPC

      ---------------- Buffer: foo ------------------
      ▼尋常小学校*
      ---------------- Buffer: foo ------------------

オートフィルモードで折り返された文字列に対し、折り返された状態のまま変換すること
もできます。

.. code:: text

      ---------------- Buffer: foo ------------------
      仮名漢字変換プログラムをさ
      くせいしました。*
      ---------------- Buffer: foo ------------------

    C-u 10 C-b Q

      ---------------- Buffer: foo ------------------
      仮名漢字変換プログラムを*さ
      くせいしました。
      ---------------- Buffer: foo ------------------

    C-u 5 C-f

      ---------------- Buffer: foo ------------------
      仮名漢字変換プログラムを▽さ
      くせい*しました。
    ---------------- Buffer: foo ------------------

    SPC

      ---------------- Buffer: foo ------------------
      仮名漢字変換プログラムを▼作成*しました。
      ---------------- Buffer: foo ------------------

ここでは改行を越えて見出し語を探し、変換する例を示しました。同様に、空白、タブ文
字を中間に含む文字列に対しても変換を行うことができます。

.. el:defvar:: skk-allow-spaces-newlines-and-tabs

   この変数を nil に設定すると、本節で説明したような２行以上にまたがる文字列に対
   する変換を禁止します。

.. _katakana-conv:

カタカナ変換
============

通常、SKK でカタカナ語を入力するには、

-  :kbd:`q` でカナモードに移ってからカタカナを入力する
-  ▽モードで :kbd:`q` によりカタカナへ変換する :ref:`かなモードからカタカナを入力 <input-katakana>`

のどちらかです。これらの方法は手軽ですが、個人辞書に登録されないため見出し語の補
完候補にも現れず、何度でも入力しなければなりません。

.. index::
   pair: Variable; skk-search-katakana

変数 :el:defvar:`skk-search-katakana` を設定することで、カタカナ語が普通の変換候
補として現れ、個人辞書にも登録されます。設定するには以下をファイル :file:`~/.skk` に
記述します [#]_ 。

.. code:: emacs-lisp

    (setq skk-search-katakana t)

また、値をシンボル 'jisx0201-kana とすると、カタカナ候補に加え半角カタカナ候補
も変換候補に現れます。

.. code:: emacs-lisp

    (setq skk-search-katakana 'jisx0201-kana)

.. _sahen-dousi:

サ変動詞変換
============

通常、SKK では諸般の事情によりサ行変格活用の動詞は送りなし変換をする前提になって
います。このことは共有辞書のメンテナンスにおける便宜上やむをえないのですが、個人
辞書が育たない（サ変動詞と名詞の区別ができない）という弱点もあります。

:ref:`サ変動詞の辞書登録に関する注意 <register-sahen>`

.. index::
   pair: Variable; skk-search-sagyo-henkaku

変数 :el:defvar:`skk-search-sagyo-henkaku` を設定することで、任意の送りなし候補を
利用してサ行の送りプレフィックスに限定して送りあり変換が可能になり、個人辞書を育
てることが可能になります。設定するには以下をファイル :file:`~/.skk` に記述します [#]_ 。

.. code:: emacs-lisp

    (setq skk-search-sagyo-henkaku t)

例えば「お茶する」の変換は以下のように変化します。

.. list-table::

   * - 従来
     - :kbd:`O c h a SPC s u r u`
   * - サ変
     - :kbd:`O c h a S u r u`

変数の値をシンボル 'anything に設定すると、サ行に限らず任意の送り仮名を許可し、
送りあり変換をします。これにより、送りあり変換の利用範囲を形容詞・動詞の変換のみ
ならず、あらゆるひらがな開始点の指定に拡張することができます。

このサ変動詞送りあり変換機能は、 :ref:`カタカナ変換機能 <katakana-conv>` と組み合
わせるとさらに有効です。

異体字へ変換する
================

「辺」（42区53点）の異体字である「邊」（78区20点）や「邉」（78区21点）を入力した
いときがあります [#]_ 。

.. code:: text

      ---- Buffer: foo ----
      *辺
      ---- Buffer: foo ----

    Q

      ---- Buffer: foo ----
      ▽*辺
      ---- Buffer: foo ----

    C-f

      ---- Buffer: foo ----
      ▽辺*
      ---- Buffer: foo ----

    SPC

      ---- Buffer: foo ----
      ▼邊*
      ---- Buffer: foo ----

    SPC

      ---- Buffer: foo ----
      ▼邉*
      ---- Buffer: foo ----

.. index::
   pair: File; SKK-JISYO.itaiji
   pair: File; SKK-JISYO.itaiji.JIS3_4

.. el:defvar:: skk-itaiji-jisyo

   辞書ファイル :file:`SKK-JISYO.itaiji` 又はファイル :file:`SKK-JISYO.itaiji.JIS3_4` へ
   のパスを指定する。他の辞書ファイルと異なり、この２つの辞書ファイルは見出し語が
   漢字です。

.. el:defun:: skk-search-itaiji

   not documented. http://mail.ring.gr.jp/skk/200303/msg00071.html

ファンクションキーの使い方
==========================

.. el:defvar:: skk-j-mode-function-key-usage

   シンボル 'conversion ならば、変数 :el:defvar:`skk-search-prog-list-1` 〜変数 :el:defvar:`skk-search-prog-list-9`
   および変数 :el:defvar:`skk-search-prog-list-0` を実行するよう自動設定します。
   これらのプログラムは▽モード限定でファンクションキー :kbd:`[F1]` 〜 :kbd:`[F10]`
   に割り当てられます。

   :kbd:`[F5]` 〜 :kbd:`[F10]` については本オプションの設定により自動的に割り当て
   られます。これらの割り当ては変数 :el:defvar:`skk-verbose` を設定するとエコー
   エリアに表示されるようになります。

   :ref:`冗長な案内メッセージの表示 <display-verbose-message>`

   .. list-table::

      * - :kbd:`[F5]`
        - 単漢字
      * - :kbd:`[F6]`
        - 無変換
      * - :kbd:`[F7]`
        - カタカナ
      * - :kbd:`[F8]`
        - 半角カナ
      * -  :kbd:`[F9]`
        - 全角ローマ
      * - :kbd:`[F10]`
        - ローマ

   シンボル 'kanagaki ならば、かなキーボード入力用に自動設定します。

   nil ならば、自動設定しません。

********
キー設定
********

かなモード／カナモードのキー設定
================================

ローマ字のルールの設定
----------------------

DDSKK の■モードにおける文字変換は、２つの変数

- :el:defvar:`skk-rom-kana-base-rule-list`
- :el:defvar:`skk-rom-kana-rule-list`

を用いて行われます。

変数 :el:defvar:`skk-rom-kana-base-rule-list` には、基本的なローマ字かな変換のル
ールが定められています。

変数 :el:defvar:`skk-rom-kana-rule-list` は、ユーザが独自のルールを定めるために用
意されており、変数 :el:defvar:`skk-rom-kana-base-rule-list` よりも優先して評価さ
れます。

.. _rom-kana-rule-list:

これらは「入出力の状態がいかに移り変わるべきか」を決定します。その内容は、
:code:`(入力される文字列 出力後に自動的に入力に追加される文字列 出力)`
という形のリストを列挙したものです。

.. list-table::

  * - 入力される文字列
    - 変換される前のアスキー文字の文字列
  * - 出力
    - | 次の入力状態に移るときにバッファに挿入される文字列の組み合わせ
      | :code:`("ア" . "あ")` のようなコンスセル

変数 :el:defvar:`skk-rom-kana-base-rule-list` の一部を見てみましょう。

.. code:: emacs-lisp

    ("a"  nil ("ア" . "あ"))
    ("ki" nil ("キ" . "き"))
    ("tt" "t" ("ッ" . "っ"))
    ("nn" nil ("ン" . "ん"))
    ("n'" nil ("ン" . "ん"))

のような規則があります。これによると

.. list-table::

    * - :kbd:`a`
      - → あ
    * - :kbd:`ki`
      - → き
    * - :kbd:`tt`
      - → っt
    * - :kbd:`nn`
      - → ん
    * - :kbd:`n'`
      - → ん

のようになります。

変数 :el:defvar:`skk-rom-kana-base-rule-list` には、次のような便利な変換ルールも
定められています。

.. list-table::

    * - :kbd:`z SPC`
      - → 全角スペース
    * - :kbd:`z*`
      - → ※
    * - :kbd:`z,`
      - → ‥
    * - :kbd:`z-`
      - → 〜
    * - :kbd:`z.`
      - → …
    * - :kbd:`z/`
      - → ・
    * - :kbd:`z0`
      - → ○
    * - :kbd:`z@`
      - → ◎
    * - :kbd:`z[`
      - → 『
    * - :kbd:`z]`
      - → 』
    * - :kbd:`z{`
      - → 【
    * - :kbd:`z}`
      - → 】
    * - :kbd:`z(`
      - → （
    * - :kbd:`z)`
      - → ）
    * - :kbd:`zh`
      - → ←
    * - :kbd:`zj`
      - → ↓
    * - :kbd:`zk`
      - → ↑
    * - :kbd:`zl`
      - → →
    * - :kbd:`zL`
      - → ⇒

ローマ字ルールの変更例
----------------------

変数 :el:defvar:`skk-rom-kana-base-rule-list` の規則に従うと

  - :kbd:`hannou` → はんおう
  - :kbd:`han'ou` → はんおう
  - :kbd:`hannnou` → はんのう

のようになります。ここで

.. code:: emacs-lisp

    (setq skk-rom-kana-rule-list
          (append skk-rom-kana-rule-list
                  '(("nn" "n" ("ン" . "ん")))))

のような設定にすることで

  - :kbd:`hannou` → はんのう

のようにローマ字かな変換が行われるようになります。

他の例として、略号を設定することもできます。

  - :kbd:`tp` → 東北大学
  - :kbd:`skk` → skk
  - :kbd:`skK` → SKK

といった変換は、

.. code:: emacs-lisp

    ("tp" nil ("東北大学" . "東北大学"))
    ("sk" nil ("" . ""))
    ("skk" nil ("skk" . "skk"))
    ("skK" nil ("SKK" . "SKK"))

のような規則を追加することで実現されます。自分の名前を入力することはよくあるので、
適当な省略形を用いて、このリストに追加しておく、といった利用をお勧めします。

更に変数 :el:defvar:`skk-rom-kana-rule-list` を用いれば TUT-code による日本語入力
を実現することもできます。TUT-code による入力についてはソースアーカイブの tut-code
ディレクトリに収録されている各ファイルを参照してください。

:doc:`ローマ字入力以外の入力方式 <07_other-IM>`

■モードに関連するその他の変数
------------------------------

.. el:defvar:: skk-kana-input-search-function

   ルールリストの中に記せない変換ルールを処理する関数。
   これは変数 :el:defvar:`skk-rom-kana-base-rule-list` と変数 :el:defvar:`skk-rom-kana-rule-list` の
   要素を全て検索した後にコールされます。引数はありません。バッファの文字を、
   直接 ``preceding-char`` などで調べて下さい。

   初期設定では ``h`` で、長音を表すために使われています。次の例を見て下さい。

     - :kbd:`ohsaka` → おおさか
     - :kbd:`ohta` → おおた

   一方で、 ``hh`` は「っ」になります。

     - :kbd:`ohhonn` → おっほん
     - :kbd:`ohhira` → おっひら

   これは変数 :el:defvar:`skk-rom-kana-rule-list` の標準設定に

   .. code:: emacs-lisp

       ("hh" "h" ("ッ" . "っ"))

   が入っているためです。これを削除すれば

     - :kbd:`ohhonn` → おおほん
     - :kbd:`ohhira` → おおひら

   となります。

.. _var-skk-kutouten-type:

.. el:defvar:: skk-kutouten-type

   ■モードの標準では、キーボードの :kbd:`.` を打鍵すると「。」が、 :kbd:`,` を打
   鍵すると「、」がバッファに挿入されます。変数 :el:defvar:`skk-kutouten-type` に
   適切なシンボルを設定することにより、この組み合せを変更 [#]_ することができます。
   そのシンボルとは、次の４つです。

   .. list-table::

      * - 設定するシンボル
        - バッファに挿入される文字
      * - 'jp （標準設定）
        - 「。」「、」
      * - 'en
        - 「．」「，」
      * - 'jp-en
        - 「。」「，」
      * - 'en-jp
        - 「．」「、」

   または、変数 :el:defvar:`skk-kutouten-type` にはコンスセルを指定することも可能
   です。その場合は、 :code:`(句点を示す文字列 . 読点を示す文字列)` のように指定
   します。
   
   例として、次のように設定すると、キーボードの :kbd:`.` で abc が、
   :kbd:`,` で def がバッファに入力されます。

   .. code:: emacs-lisp

       (setq skk-kutouten-type '("abc" . "def"))

   なお、変数 :el:defvar:`skk-kutouten-type` はバッファローカル変数です。すべての
   バッファで統一した設定としたい場合は、

   .. code:: emacs-lisp

       (setq-default skk-kutouten-type 'en)

   のように関数 :el:defun:`setq-default` を用いてください。

.. el:defvar:: skk-use-auto-kutouten

   標準設定は nil 。 Non-nil であれば、カーソル直前の文字種に応じて句読点を動的に
   変更します。

.. _insert-num:

数字や記号文字の入力
--------------------

かなモード／カナモードにおける次のキーは、関数 :el:defun:`skk-insert` にバインド
されています。

.. code:: text

    !  #  %  &  '  *  +

    -  0  1  2  3  4  5

    6  7  8  9  :  ;  <

    =  >  ?  "  (  )  [

    ]  {  }  ^  _  `  |

    ~

これらの数字や記号文字のキーに対応し挿入される文字をカスタマイズするためには、変
数 :el:defvar:`skk-rom-kana-rule-list` を利用します。

.. code:: emacs-lisp

    (setq skk-rom-kana-rule-list
          (append skk-rom-kana-rule-list
                  '(("!" nil "!")
                    ("," nil ",")
                    ("." nil ".")
                    (":" nil ":")
                    (";" nil ";")
                    ("?" nil "?"))))

関数 :el:defun:`skk-insert` は、Emacs のオリジナル関数 :el:defun:`self-insert-command` を
エミュレートしています。具体的には、引数を渡すことによって同じ文字を複数、一度に
挿入することが可能です。

.. code:: text

    C-u 2 !

      ------ Buffer: foo ------
      ！！
      ------ Buffer: foo ------

全英モードのキー設定
====================

全英モードにおける印字可能な全てのキーは関数 :el:defun:`skk-jisx0208-latin-insert` に
割り付けられています。また、変数 :el:defvar:`skk-jisx0208-latin-vector` の値によ
り挿入される文字が決定され、その標準設定は以下のようになっています。

.. code:: emacs-lisp

    [nil  nil  nil  nil  nil  nil  nil  nil
     nil  nil  nil  nil  nil  nil  nil  nil
     nil  nil  nil  nil  nil  nil  nil  nil
     nil  nil  nil  nil  nil  nil  nil  nil
     "　" "！" "”" "＃" "＄" "％" "＆" "’"
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

挿入される文字を変更したい場合: :ref:`数字や記号文字の入力 <insert-num>`

関数 :el:defun:`skk-jisx0208-latin-insert` も Emacs オリジナルの関数 :el:defun:`self-insert-command` を
エミュレートしています。つまり、関数 :el:defun:`skk-insert` における動作と同じく、
引数を渡すことにより同じ文字を複数、一度に挿入することができます。

:ref:`数字や記号文字の入力 <insert-num>`

閉じ括弧の自動入力
==================

通常、"「" を入力したら "」" を後で入力する必要があります。
"「" の入力時点で、対になる文字を自動挿入してくれると、打鍵数を減らすことができま
すし、なにより入力忘れの防止にもなるでしょう。

そのために変数 :el:defvar:`skk-auto-insert-paren` が用意されています。
この値を non-nil にすると、上記の自動挿入を行います。

.. code:: text

      ------ Buffer: foo ------
      彼はこう言った*
      ------ Buffer: foo ------

    [

      ------ Buffer: foo ------
      彼はこう言った「*」
      ------ Buffer: foo ------

上記のように "「" の入力時点で対となる "」" を自動挿入し、 "「" と ""」" の間にポ
イントを再配置するので、その位置からかぎかっこに囲まれた文字列の入力を即始めるこ
とができます。

.. el:defvar:: skk-auto-paren-string-alist

   自動挿入すべきペアの文字列を指定します。標準設定は下記のとおり。

   .. code:: emacs-lisp

       (("「" . "」") ("『" . "』") ("("  . ")")  ("（" . "）") ("{"  . "}")
        ("｛" . "｝") ("〈" . "〉") ("《" . "》") ("["  . "]")  ("［" . "］")
        ("〔" . "〕") ("【" . "】") ("\"" . "\"") ("“"  . "”")  ("`"  . "'"))

   これは、ひと言でまとめると、「開き括弧と閉じ括弧とのコンスセルを集めたリスト」
   です。各コンスセルの関数 :el:defun:`car` にある文字列を挿入したときに関数 :el:defun:`cdr` にあ
   る文字列が自動挿入されます。

   このリストの各要素の関数 :el:defun:`car` の文字列は、必ず変数 :el:defvar:`skk-rom-kana-rule-list` の
   :ref:`規則 <rom-kana-rule-list>` によって入力されなければなりません。
   例えば "(" に対する ")" を自動挿入するには

   .. code:: emacs-lisp

       (setq skk-rom-kana-rule-list
             (append skk-rom-kana-rule-list
                   '(("(" nil "("))))

   のように設定する必要があります。

   既に SKK モードになっているバッファで変数 :el:defvar:`skk-auto-paren-string-alist` を
   変更した場合は、 :kbd:`C-x C-j` もしくは :kbd:`C-x j` を２度キー入力して
   関数 :el:defun:`skk-mode` もしくは関数 :el:defun:`skk-auto-fill-mode` を起動し
   直す必要があります。

キーとなる文字が挿入されても、その挿入後のポイントに自動挿入すべき文字が
既に存在している場合には、自動挿入されないように設計されています。

.. code:: text

      ------ Buffer: foo ------
      *」
      ------ Buffer: foo ------

    [

      ------ Buffer: foo ------
      「*」
      ------ Buffer: foo ------

対になる文字を複数挿入したい場合は、引数を渡して文字を指定します。

.. code:: text

    C-u 2 [

      ------ Buffer: foo ------
      「「*」」
      ------ Buffer: foo ------

yatex-mode など、既に同様の機能が付いているモードがあります。そのようなモードにお
いてもこの自動挿入の機能が邪魔になることはないでしょうが、特定のモードに限って自
動入力機能をオフにしたい場合は、当該モードに入ったときにコールされるフック変数を
利用して設定することができます。

.. code:: emacs-lisp

    (add-hook 'yatex-mode-hook
              (lambda ()
                  (when skk-auto-insert-paren
                    (make-local-variable 'skk-auto-insert-paren)
                    (setq skk-auto-insert-paren nil))))

特定のモードにおいて、自動挿入すべき文字を変更したい場合にも同様にフック変数を用
いて操作できます。

.. code:: emacs-lisp

    (add-hook 'tex-mode-hook
              (lambda ()
                  (when skk-auto-insert-paren
                    (make-local-variable 'skk-auto-paren-string-alist)
                    (setq skk-auto-paren-string-alist
                          (cons '("$" . "$") skk-auto-paren-string-alist)))))

同様に、特定のペアを削除したい場合は、例えば下記のように設定します。

.. code:: emacs-lisp

    (add-hook 'tex-mode-hook
              (lambda ()
                  (when skk-auto-insert-paren
                    (make-local-variable 'skk-auto-paren-string-alist)
                    (setq skk-auto-paren-string-alist
                          (delete
                           '("$" . "$")
                           (copy-sequence skk-auto-paren-string-alist))))))

リージョンを括弧で囲む
======================

「閉じ括弧の自動入力」の応用として、リージョンを括弧で囲むことができます。

.. code:: text

      ------ Buffer: foo ------
      このマニュアルにおいて*DDSKK*と呼びます
      ------ Buffer: foo ------

    `

      ------ Buffer: foo ------
      このマニュアルにおいて`DDSKK'*と呼びます
      ------ Buffer: foo ------

.. el:defvar:: skk-use-auto-enclose-pair-of-region

   non-nil であれば、上記の機能が有効になります。
   当然に変数 :el:defvar:`skk-auto-insert-paren` も non-nil である必要があります。
   なお、 ``delete-selection-mode`` の方が優先されます。

確定するキー
============

.. el:defvar:: skk-kakutei-key

   この変数の値は、明示的な確定動作を行うキーを指定します。
   標準設定では :kbd:`C-j` となっています。

   :ref:`暗黙の確定のタイミング <ammoku-kakutei>`

.. _cand-select-key:

候補の選択に用いるキー
======================

変換において、候補が５つ以上あるときは、５番目以降の候補は７つずつまとめ
てエコーエリアに下記のように表示されます。

.. code:: text

    -------------------- Echo Area --------------------
    A:嘘  S:拒  D:拠  F:虚  J:挙  K:許  L:渠  [残り 2]
    -------------------- Echo Area --------------------

この際、候補の選択に用いるキーは、次の変数によって決定されます。

.. el:defvar:: skk-henkan-show-candidates-keys  

   ７つの異なる文字のリスト。文字は必ず小文字とする。
   ``x`` , ``SPC`` 及び ``C-g`` は、それぞれ候補選択中における前候補群の表示、次
   候補群の表示、取り止めのために割り付けられているので、含めてはならない。
   標準設定は、以下のとおり。

   .. code:: emacs-lisp

       (?a ?s ?d ?f ?j ?k ?l)

.. el:defface:: skk-henkan-show-candidates-keys-face

   選択キーを表示する際のフェイスを指定します。

.. el:defvar:: skk-henkan-rest-indicator

   標準設定は nil 。
   Non-nil であれば ``[残り 99++]`` の表示を右寄せ配置する。

.. el:defface:: skk-henkan-rest-indicator-face

   ``[残り 99++]`` の face 属性。標準設定は ``default`` 。

▼モードでの RET
================

標準設定では、

.. code:: text

    K a k u t e i SPC

      ------ Buffer: foo ------
      ▼確定*
      ------ Buffer: foo ------

    RET

      ------ Buffer: foo ------
      確定
      *
      ------ Buffer: foo ------


のように、▼モードで :kbd:`RET` を入力すると、確定し、かつ改行を行います。この挙
動を変えるためのユーザオプションが用意されています。

.. el:defvar:: skk-egg-like-newline

   この変数の値を non-nil にすると、▼モードで :kbd:`RET` を入力したときに確定の
   み行い、改行はしません。従って、 :ref:`辞書登録モード <jisyo-register-mode>`
   において▼モードであるときの :kbd:`RET` 打鍵時の挙動も変化 [#]_ します。

   .. code:: text

       K a k u t e i SPC

         ------ Buffer: foo ------
         ▼確定*
         ------ Buffer: foo ------

       RET

         ------ Buffer: foo ------
         確定*
         ------ Buffer: foo ------

▼モードでの BS
===============

標準設定では、▼モードで :kbd:`BS` を押すと、前の一文字を削除した上で確定します。

.. code:: text

    D e n k i y a SPC

      ------ Buffer: foo ------
      ▼電気屋*
      ------ Buffer: foo ------

    BS

      ------ Buffer: foo ------
      電気*
      ------ Buffer: foo ------

.. el:defvar:: skk-delete-implies-kakutei

   この変数の値を nil に設定すると、▼モードで :kbd:`BS` を押した時にひとつ前の候
   補を表示します。例えば、

   .. code:: text

       でんき /電気/伝記/

   という :ref:`辞書エントリ <jisyo-entry>` があるとき、以下のようになります。

   .. code:: text

       D e n k i

         ------ Buffer: foo ------
         ▽でんき*
         ------ Buffer: foo ------

       SPC

         ------ Buffer: foo ------
         ▼電気*
         ------ Buffer: foo ------

       SPC

         ------ Buffer: foo ------
         ▼伝記*
         ------ Buffer: foo ------

       BS

         ------ Buffer: foo ------
         ▼電気*
         ------ Buffer: foo ------

       BS

         ------ Buffer: foo ------
         ▽でんき*
         ------ Buffer: foo ------

変数 :el:defvar:`skk-delete-implies-kakutei` がシンボル 'dont-update であれば、
non-nil 時と同じ動作のうえで個人辞書を更新しません。

なお、変数 :el:defvar:`skk-delete-implies-kakutei` の値にかかわらず、候補バッファ
を表示している場合はひとつ前の候補表示に戻る動作となります。
      
送りあり変換中の C-g
====================

送りありの変換中に :kbd:`C-g` を入力すると、▼モードを抜け、その見出し語と送り仮
名を現在のバッファに挿入し、▽モードに入ります。

.. code:: text

    N a K u

      ------ Buffer: foo ------
      ▼泣く*
      ------ Buffer: foo ------

    C-g

      ------ Buffer: foo ------
      ▽なく*
      ------ Buffer: foo ------

.. el:defvar:: skk-delete-okuri-when-quit

   この変数の値を non-nil に設定すると、送りありの変換中に :kbd:`C-g` を入力した
   ときの挙動が変化します。▽モードに入るのは同じですが、同時に送り仮名を消します。
   送り仮名の入力間違いを修正するのには便利です。例えば、以下のようになります。

   .. code:: text

       N a K u

         ------ Buffer: foo ------
         ▼泣く*
         ------ Buffer: foo ------

       C-g

         ------ Buffer: foo ------
         ▽な*
         ------ Buffer: foo ------

.. _sticky:

変換位置の指定方法
==================

SKK では通常、「漢字変換の開始位置」と「送り仮名の開始位置」を大文字で指定します
が、これらを任意のキーで指定することで sticky-shift ライクな操作 [#]_ も可能です。

.. code:: emacs-lisp

    (setq skk-sticky-key ";")

と設定すると :kbd:`;` キーで [#]_ 漢字変換位置が指定できるようになります。

例えば「有る」という単語を入力するには :kbd:`;` :kbd:`a` :kbd:`;` :kbd:`r` :kbd:`u`
というキー入力で可能となり、シフトキーを押す必要がなくなります。

操作上は通常の sticky-shift [#]_ と変わりませんが、画面表示は

.. list-table::

   * - 打鍵
     - 通常の sticky
     - skk-sticky
   * - :kbd:`;`
     - 変化なし
     - ▽
   * - :kbd:`a`
     - ▽あ
     - ▽あ
   * - :kbd:`;`
     - ▽あ
     - ▽あ*
   * - :kbd:`r`
     - ▽あ*r
     - ▽あ*r

と遷移します。通常の sticky と比べて skk-sticky は :kbd:`;` を押した時点で画面表
示が変化するので若干分かり易いと思います。

キーの設定方法は、割り当てるキーの種類によって異なります。

- 表示を伴うキー

  :kbd:`;` などの表示を伴うキーの場合は

  .. code:: emacs-lisp

     (setq skk-sticky-key ";")

  のように string を設定して下さい。変数 :el:defvar:`skk-sticky-key` に設定した文
  字そのものを入力したい場合は２回続けて打鍵すると入力できます。

- 表示を伴わないキー

  :kbd:`無変換` のような表示を伴わないキーの場合は

  .. code:: emacs-lisp

     (setq skk-sticky-key [muhenkan]) ;Microsoft Windows では [noconvert]

  のようにそのキーを表わす vector を設定して下さい。

- 同時打鍵

  ２つのキーを同時に打鍵することでも漢字変換位置を指定できます。例えば
  :kbd:`f` と :kbd:`j` の同時打鍵で指定する場合は

  .. code:: emacs-lisp

     (setq skk-sticky-key '(?f ?j))

  のように character のリストを設定して下さい。

  Dvorak 配列のような、押しやすい場所に適当なキーがない環境でもこの機能を使いたい
  場合に便利かもしれません。

.. el:defvar:: skk-sticky-double-interval

   この変数が指定する秒数以内に打鍵されたものを同時打鍵と判定する。
   標準設定は 0.1 秒。

１回の取り消し操作 (undo) の対象
================================

Emacs では本来、連続する 20 文字の挿入が一回の取り消し操作（アンドゥ）の対象とな
っています。そこで DDSKK のかな・カナ・全英モードにおける入力も、これと同様の動作
をするように設計されています [#]_ 。

正確に言えば、関数 :el:defun:`skk-insert` ,関数 :el:defun:`skk-set-henkan-point` ,
関数 :el:defun:`skk-jisx0208-latin-insert` [#]_ の各関数にバインドされたキー入力
については、連続して入力された 20 文字 [#]_ をいちどのアンドゥの対象としています。

ただし、これらの DDSKK のコマンドと Emacs 本来の関数 :el:defun:`self-insert-command` を
織り混ぜてキー入力した場合 [#]_ は、このエミュレーションは正常に動作しませんが、
これは現在の仕様です。

.. code:: text

    a i u e o k a k i k u k e k o s a s i s u s e s o t a t i t u t e t o

      ------------------------- Buffer: foo -------------------------
      あいうえおかきくけこさしすせそたちつてと*   ※ 連続する20文字
      ------------------------- Buffer: foo -------------------------

    C-_

      ------------------------- Buffer: foo -------------------------
      *                                           ※ 20文字全てがアンドゥの対象
      ------------------------- Buffer: foo -------------------------

    a i u e o k a k i k u k e k o s a s i s u s e s o t a t i t u t e t o n a

      -------------------------- Buffer: foo --------------------------
      あいうえおかきくけこさしすせそたちつてとな* ※ 連続する21文字
      -------------------------- Buffer: foo --------------------------

    C-_

      -------------------------- Buffer: foo --------------------------
      あいうえおかきくけこさしすせそたちつてと*   ※ 最後の1文字のみがアンドゥの対象
      -------------------------- Buffer: foo --------------------------

****************
変換、確定の前後
****************

関連事項:

- :ref:`送りあり変換の変換開始のタイミング <okuri-conv-start>`

- :ref:`変換位置の指定方法 <sticky>`

ポイントを戻して▽モードへ
==========================

▽モードに入り忘れた場合に、手動で▽マークを付ける方法は
:ref:`後から▽モードに入る方法 <after>` で説明しました。

ここで述べる方法では、遡って▽マークを付ける位置を自動的に選び、しかもポイントは
動きません。

.. el:define-key:: M-Q
                   M-x skk-backward-and-set-henkan-point

   :kbd:`M-Q` （大文字の :kbd:`Q` です。）と打鍵すると、現在位置の直前の文字列に
   ついて走査し、同種の文字（ひらがな、カタカナ、全角アルファベット、アルファベッ
   トの４種類のいずれか）が続く限り後方に戻って▽マークを付けます。ポイントは動き
   ません。

   .. code:: text

       k a n j i

         ------ Buffer: foo ------
         かんじ*
         ------ Buffer: foo ------

       M-Q

         ------ Buffer: foo ------
         ▽かんじ*
         ------ Buffer: foo ------

   変換開始位置を決定するとき、スペース文字、タブ文字、長音を表わす「ー」は無条件
   に無視されます。ただし、ひらがなの場合は「を」が、カタカナの場合は「ヲ」が見つ
   かった時点で変換開始位置の走査を止めて▽モードに入ります。変換開始ポイントを
   「を」又は「ヲ」の直前で止めるのは、たいていその直後から単語が始まるからです。

以上は、引数を与えないで :kbd:`M-Q` を実行した場合です。一方で、 :kbd:`C-u 5 M-Q` の
ように引数を渡して実行すると、変換開始位置から現在位置までの文字数を指定すること
ができます。この場合は文字種別を問わず、与えられた文字数だけ無条件にポイントを戻
します。

.. el:defvar:: skk-allow-spaces-newlines-and-tabs

   後方にポイントを戻す途中で行頭に到達した場合は、更に上の行について、行末の文字
   列から同様の走査を行い、必要があれば更にポイントを戻します。こうした「行を超え
   ての走査」をやめるためには、この変数の値を nil に設定します。

直前の確定を再変換
==================

一番最後（直近）の確定を取り消して、再変換することができます。
これを **確定アンドゥ** と呼びます。

例えば、 :ref:`辞書エントリ <jisyo-entry>` が

.. code:: text

    こうこう /高校/孝行/航行/

のようになっているとします。

.. code:: text

    K o u k o u SPC

      ------ Buffer: foo ------
      ▼高校*
      ------ Buffer: foo ------

    s u r u

      ------ Buffer: foo ------
      高校する*
      ------ Buffer: foo ------

    M-x skk-undo-kakutei

      ------ Buffer: foo ------
      ▼孝行*
      ------ Buffer: foo ------

この例では、「高校」の確定を取り消しています。すると、辞書の第一候補である「高校」
をとばして、次候補である「孝行」が現れます。ここで更に :kbd:`SPC` を押せば次候補
である「航行」が現れ、更にもう一度 :kbd:`SPC` を押せば候補が尽きて
:ref:`辞書登録モード <jisyo-register-mode>` に入ります。

この例のとおり、確定アンドゥは、確定した直後でなくとも有効です。より正確には、次
の新たな確定を行うまで [#]_ は確定に関する情報が保持されているので、確定アンドゥ
することができます。

また、変換、確定に関連しない文字列は、確定アンドゥを行っても削除されないように設
計されています。上記の例では「する」がそのままカレントバッファに残っています。

.. el:defvar:: skk-undo-kakutei-return-previous-point

   この変数の値が non-nil であれば、確定アンドゥ処理が完了した後に、確定アン
   ドゥ処理の直前の位置にカーソルが戻ります。上の例の場合、確定アンドゥ処理が完了
   した後のカーソル位置は、標準設定 nil では「孝行」の直後 のままですが、
   non-nil であれば「する」の直後に復帰します。

自動変換開始
============

▽モードで見出し語を入力しているときに「を」や「。」などの文字を打鍵すると、
:kbd:`SPC` を押したかのように変換を開始 [#]_ し、▼モードに入るようになっていま
す。

.. code:: text

    K a n j i

      ------ Buffer: foo ------
      ▽かんじ*
      ------ Buffer: foo ------

    w o

      ------ Buffer: foo ------
      ▼漢字を*
      ------ Buffer: foo ------

.. el:defvar:: skk-auto-okuri-process

   この変数を non-nil に設定して :ref:`送り仮名の自動処理 <okurigana>` を行っ
   ている場合は、以下のような変換も可能です。ただし、個人辞書に

   .. code:: text

       できr /出来/[る/出来/]/

   というような :ref:`辞書エントリ <jisyo-entry>` があると仮定します。

   .. code:: text

       D e k i r u n n d e s u

         ------ Buffer: foo ------
         ▽できるんです
         ------ Buffer: foo ------

       .

         ------ Buffer: foo ------
         ▼出来るんです。
         ------ Buffer: foo ------

.. el:defvar:: skk-auto-start-henkan-keyword-list

   この変数の値は、単語や文節の区切りとなるような文字列のリストです。標準設定は以
   下のようになっています。

   .. code:: emacs-lisp

       ("を" "、" "。" "．" "，" "？" "」" "！" "；" "：" ")" ";"
        ":" "）" "”" "】" "』" "》" "〉" "｝" "］" "〕" "}"
        "]" "?" "." "," "!" )

.. el:defvar:: skk-auto-start-henkan

   この変数の値を nil に設定すると、本節で説明した自動変換開始機能を無効にし
   ます。標準設定は t です。

.. _ammoku-kakutei:

暗黙の確定のタイミング
======================

標準の設定では、確定が済む前に次の文字 [#]_ を入力すると、直ちに確定されます。
これを「暗黙の確定」と呼んでいます。具体的には以下のようになります。

.. code:: text

    K a k u t e i

      ------ Buffer: foo ------
      ▽かくてい*
      ------ Buffer: foo ------

    SPC

      ------ Buffer: foo ------
      ▼確定*
      ------ Buffer: foo ------

    s

      ------ Buffer: foo ------
      確定s*      ; 暗黙の確定
      ------ Buffer: foo ------

    u

      ------ Buffer: foo ------
      確定す*
      ------ Buffer: foo ------

.. el:defvar:: skk-kakutei-early

   この変数の値を nil にすると、「暗黙の確定」を遅らせます。具体的には、

   -  括弧 :kbd:`(`, :kbd:`)`, :kbd:`[`, :kbd:`]` の入力時
   -  句読点 :kbd:`,`, :kbd:`.` の入力時
   -  次の変換開始時（ :kbd:`A` から :kbd:`Z` までの大文字の入力時）
   -  :kbd:`RET` 入力時

   まで暗黙の確定が遅延されます [#]_ 。

   .. code:: text

       K a k u t e i

         ------ Buffer: foo ------
         ▽かくてい*
         ------ Buffer: foo ------

       SPC

         ------ Buffer: foo ------
         ▼確定*
         ------ Buffer: foo ------

       s

         ------ Buffer: foo ------
         ▼確定s*
         ------ Buffer: foo ------

       u r u

         ------ Buffer: foo ------
         ▼確定する*
         ------ Buffer: foo ------

       .

         ------ Buffer: foo ------
         確定する。*      ; 暗黙の確定
         ------ Buffer: foo ------

積極的な確定
============

変換候補がひとつしか見つからない場合は自動的に確定する、という設定ができます。

.. el:defvar:: skk-kakutei-when-unique-candidate

   この値が non-nil であれば、この機能が有効になります。

   t であれば、送りあり変換、送りなし変換、SKK abbrev モードでの変換の全てでこの
   機能が有効になります。

   または、 ``okuri-ari`` , ``okuri-nasi`` , ``abbrev`` を要素とするリストである
   こともできます。この場合は、変換対象がその条件に合致した場合のみ確定変換が機能
   します。

   .. code:: text

       '(okuri-nasi abbrev)

   この機能は、全ての辞書を検索した上で変換候補が唯一か否かを調べます。
   そのため、変数 :el:defvar:`skk-search-prog-list` の内容によってはレスポンスが
   悪くなる可能性があります。

   :ref:`辞書の検索方法の設定 <search-jisyo>`

.. el:defvar:: skk-kakutei-search-prog-limit

   この変数の値が数値であれば、積極的な確定 ``skk-kakutei-when-unique-candidate`` に
   おける「変換候補が唯一か否か」の判定を変数 :el:defvar:`skk-search-prog-list`
   の先頭から数えて当該数値の個数までの辞書に制限します。

   数値以外であれば、無制限に全ての辞書を検索対象とします。

.. _kakutei-jisyo:

確定辞書
========

特定の語は、変換したら即座に確定させる事ができます。これを **確定変換** と呼び、
利用するには **確定辞書** を用意します。例えば、

.. code:: text

    じしょ /辞書/

という :ref:`辞書エントリ <jisyo-entry>` が確定辞書にあったとします。このとき、

.. code:: text

    Z i s h o

      ------ Buffer: foo ------
      ▽じしょ*
      ------ Buffer: foo ------

    SPC

      ------ Buffer: foo ------
      辞書*
      ------ Buffer: foo ------

のように :kbd:`SPC` を押しただけでいきなり確定します。
:ref:`辞書エントリ <jisyo-entry>` の候補がひとつだけだからです。

確定辞書以外の辞書に登録されているであろう同音異義語を得るには、確定変換の直後に
:kbd:`x` を打鍵します。すると、▼モードに戻って次の候補を検索することができます。

次の例では、確定辞書に「辞書」が、個人辞書（や共有辞書）に「自署」が登録されてい
るとします。

.. code:: text

    Z i s y o SPC

      ------ Buffer: foo ------
      辞書*
      ------ Buffer: foo ------

    x

      ------ Buffer: foo ------
      ▼自署*
      ------ Buffer: foo ------

確定辞書の単語は、優先的に変換されます。

.. el:defvar:: skk-kakutei-jisyo

   確定変換用の辞書ファイル [#]_ を指定します。 nil であれば、確定変換は行わ
   れません。この辞書は、標準の配布パッケージには含まれていないので、使用するので
   あればユーザ側で用意する必要があります。

   :ref:`辞書の書式 <jisyo-format>`

************
送り仮名関連
************

SKK の送り仮名の処理は、好みが分かれるところです。色々な対策が用意されていますの
で、試してみて下さい。

.. _okuri-strictly:

送り仮名の厳密なマッチ
======================

今、個人辞書に

.. code:: text

    おおk /大/多/[く/多/]/[き/大/]/

という「送りありエントリ」があると仮定します。

ここで :kbd:`O o K i i SPC` と入力した場合、普通は「大きい」と「多きい」という
２通りの候補が出力されますが、このうち「多きい」は現代の日本語として正しくありま
せん。このような場合に、出力される候補を正しい表現のみに絞りこむ方法について説明
します。

.. el:defvar:: skk-henkan-okuri-strictly

   この変数の値を non-nil に設定すると、見出し語がマッチするかどうかのチェッ
   クの上に、送り仮名がマッチするかどうかのチェックが行われます。結果として送り仮
   名がマッチしない候補は出力されません。上記の例では、送り仮名「き」がマッチする
   「大きい」は出力されますが、「多きい」は出力されません [#]_ 。

   個人辞書の「送りありエントリ」が充実していれば、標準の設定よりも候補が絞り込ま
   れるので変換効率がアップしますが、さもなければ、すぐに
   :ref:`辞書登録モード <jisyo-register-mode>` に入ってしまうため逆に不便になりま
   す。

変数 :el:defvar:`skk-henkan-okuri-strictly` の値を non-nil にすると、
:ref:`辞書登録モード <jisyo-register-mode>` に入っても送り仮名のマッチが厳密に行
われます。これは辞書登録の際希望する候補を得るためには障害となります。そのような
障害を避けるためには、下記のようにフック変数を設定します。

これにより、辞書登録時だけは、一時的に
:ref:`送り仮名の厳密なマッチ <okuri-strictly>` をしないようになります [#]_ 。

.. code:: emacs-lisp

    (add-hook 'minibuffer-setup-hook
              (lambda ()
                  (when (and (boundp 'skk-henkan-okuri-strictly)
                             skk-henkan-okuri-strictly
                             (not (eq last-command 'skk-purge-jisyo)))
                    (setq skk-henkan-okuri-strictly nil)
                    (put 'skk-henkan-okuri-strictly 'temporary-nil t))))

.. code:: emacs-lisp

    (add-hook 'minibuffer-exit-hook
              (lambda ()
                  (when (and (get 'skk-henkan-okuri-strictly 'temporary-nil)
                             (<= (minibuffer-depth) 1))
                    (put 'skk-henkan-okuri-strictly 'temporary-nil nil)
                    (setq skk-henkan-okuri-strictly t))))

.. _okuri-precedence:

送り仮名の優先的なマッチ
========================

:ref:`送り仮名の厳密なマッチ <okuri-strictly>` では、見出し語と送り仮名が一致した
場合のみ候補を表示します。

ここでは、その条件を緩めて優先的に表示する方法を紹介します [#]_ 。

今、個人辞書に

.. code:: text

    おおk /大/多/[く/多/]/[き/大/]/

という「送りありエントリ」があると仮定します。

ここで :kbd:`O o K i i SPC` と入力した場合、普通は「大きい」と「多きい」という
２通りの候補が出力されますが、このうち「多きい」は現代の日本語として正しくありま
せん。このような場合に、出力される候補を正しい表現が優先的にする設定を紹介します。

.. el:defvar:: skk-henkan-strict-okuri-precedence

   この変数の値を non-nil に設定すると、見出し語と送り仮名がマッチした候補を
   優先して表示します。上記の例では「▽おお*く」を変換したとき、まず「多く」を出
   力し、次に「大く」を出力します。

   この変数の値が non-nil の時は、変数 :el:defvar:`skk-process-okuri-early` の値は nil で
   なければなりません [#]_ 。 また、変数 :el:defvar:`skk-henkan-okuri-strictly` が non-nil の
   ときは、この変数は無視されます。

.. _okurigana:

送り仮名の自動処理
==================

この節では、「あげる」と入力してから :kbd:`SPC` を押しても「上げる」と変換する機
能を紹介します。

どのように変換されるか
----------------------

.. el:defvar:: skk-auto-okuri-process

   この変数の値を non-nil に設定すると、 :ref:`送り仮名の自動処理 <okurigana>` が
   行われます。

例えば :kbd:`T a t i a g e r u SPC` とキー入力した場合を考えます。このとき、検索
される見出し語の変化を追うと、

-  たちあげる
-  たちあげr
-  たちあg
-  たちa
-  たt

のようになります。仮に個人辞書エントリが、

.. code:: text

    たちあg /立ち上/[げ/立ち上/]/[が/立ち上/]/
    たt /建/断/経/立/[つ/建/断/経/立/]/[ち/建/断/経/立/]/[て/経/立/建/]/

の２つのエントリを含むとすると、見出し語を後方から順に切り詰める過程で「たちあg」
と「たt」の２つの見出し語の検索時にこれらの :ref:`辞書エントリ <jisyo-entry>` が
マッチします。

つまり、「たちあげる」という見出し語に対し、見出し語を最後尾から１文字ずつ切り詰
め、「切り詰めの結果残った文字列」と、「切り捨てられた先頭の文字のローマ字プレフ
ィックスを連結した文字列」を送りあり変換の見出し語として検索します [#]_ 。

次に、マッチしたエントリの各候補に対し、切り捨てられた先頭の文字を送り仮名として
取るかどうかをチェックします。この判断には、個人辞書の送り仮名ブロック部分 [#]_
を利用します。

「たちあg」の場合の送り仮名チェックの対象は、切り捨てられた最初の文字の「げ」です。
個人辞書に

.. code:: text

    [げ/立ち上/]

の部分があることから、送り仮名として取るべきと判断します。また、「たt」の場合の送
り仮名チェックの対象は「ち」です。個人辞書に

.. code:: text

    [ち/建/断/経/立/]

の部分があることから、送り仮名として取るべきと判断します。

こうして、送り仮名がマッチする候補が「立ち上」、「建」、「断」、「経」、「立」の
５つに絞られます。これらは文字列の長さ順に昇順にソートされ [#]_ 、
それぞれの候補と該当の見出し語から切り捨てられた文字列と連結したもの [#]_ を、
:ref:`送り仮名の自動処理 <okurigana>` の最終候補として返します。

上記の例は、「立ち上げる」、「建ちあげる」、「断ちあげる」、「経ちあげる」、
「立ちあげる」 の５つが最終候補になります。

自動送り機能は、個人辞書のみを検索します。

ここで、自動送り機能の特徴を考えてみると、

.. list-table::

   * - 長所
     - - 送り仮名の最初のローマ字表現を大文字で始める必要がない。
       - 送り仮名を正確に思い出せない場合に送り仮名を指定しなくとも変換できる。
   * - 短所
     - - 意図しない変換をされる割合が増える。
       - 個人辞書の「送りありエントリ」が貧弱な場合は、自動処理ができない可能性が高い。

となります。

変数 :el:defvar:`skk-auto-okuri-process` の値を non-nil に設定したとしても、従来どおり
の送りあり変換も同時にできますので、一度この機能を試してみることをお勧めします [#]_ 。

辞書登録の際に注意すべきこと
----------------------------

:ref:`送り仮名の自動処理 <okurigana>` を行っている場合 [#]_ には、辞書登録の際に
注意すべきことがあります。

個人辞書に見出し語「わたs」についてのエントリが全くない場合、あるいは個人辞書のエ
ントリが

.. code:: text

    わたs /渡/[し/渡/]/

のような送り仮名のブロックを持たない場合を考えてみます。 ここで
:kbd:`W a t a s i t a SPC` と入力すると、 :ref:`送り仮名の自動処理 <okurigana>`
においては送り仮名がマッチしないので、候補が見つからずに :ref:`辞書登録モード <jisyo-register-mode>` に入ります。

.. code:: text

    W a t a s i t a SPC

      ------ Buffer: foo ------
      ▼わたした
      ------ Buffer: foo ------

      ------ Minibuffer -------
      [辞書登録] わたした*
      ------ Minibuffer -------

:ref:`辞書登録モード <jisyo-register-mode>` で :kbd:`W a t a S i t a RET` と送り
仮名を明示的に入力して「渡した」と変換して登録したとします。この場合、登録する語
の最後が平仮名で終わるので、その最後の平仮名の文字列（上記の例では「した」）が見
出し語の最後と一致するかを調べます。一致する場合には、辞書の登録を送りありエント
リとして行うのかどうかの確認を求めます。

.. code:: text

   W a t a S i t a

      ------ Minibuffer -------
      [辞書登録] わたした 渡した*
      ------ Minibuffer -------

   RET

      -------------------------- Echo Area --------------------------
      Shall I register this as okuri-ari word: わたs /渡/ ? (y or n)
      -------------------------- Echo Area --------------------------

この確認に対して :kbd:`y` と回答した場合は、

.. code:: text

   わたs /渡/[し/渡/]/

という :ref:`辞書エントリ <jisyo-entry>` が個人辞書の「送りありエントリ」に書き込
まれます。一方で、 :kbd:`n` と回答した場合は、個人辞書の「送りなしエントリ」に

.. code:: text

   わたした /渡した/

というエントリが書き込まれます。本例の場合は :kbd:`y` と回答するのが正解です。

.. el:defvar:: skk-kana-rom-vector

   この変数は、送り仮名部分を :ref:`ローマ字プレフィックス<roma-prefix>` に分解す
   る際に、参照されます。

変数 :el:defvar:`skk-kana-rom-vector` の標準設定は以下のようになっています。

.. code:: text

   ["x" "a" "x" "i" "x" "u" "x" "e" "x" "o" "k" "g" "k" "g" "k" "g"
    "k" "g" "k" "g" "s" "z" "s" "j" "s" "z" "s" "z" "s" "z" "t" "d"
    "t" "d" "x" "t" "d" "t" "d" "t" "d" "n" "n" "n" "n" "n" "h" "b"
    "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "m" "m" "m"
    "m" "m" "x" "y" "x" "y" "x" "y" "r" "r" "r" "r" "r" "x" "w" "x"
    "x" "w" "n"]

このベクトルは、それぞれ下記のかな文字をその :ref:`ローマ字プレフィックス<roma-prefix>`
で現したものです。

.. code:: text

   ぁ  あ  ぃ  い  ぅ  う  ぇ  え  ぉ  お  か  が  き  ぎ  く  ぐ
   け  げ  こ  ご  さ  ざ  し  じ  す  ず  せ  ぜ  そ  ぞ  た  だ
   ち  ぢ  っ  つ  づ  て  で  と  ど  な  に  ぬ  ね  の  は  ば
   ぱ  ひ  び  ぴ  ふ  ぶ  ぷ  へ  べ  ぺ  ほ  ぼ  ぽ  ま  み  む
   め  も  ゃ  や  ゅ  ゆ  ょ  よ  ら  り  る  れ  ろ  ゎ  わ  ゐ
   ゑ  を  ん

これに従うと、見出し語中の送り仮名が :ref:`ローマ字プレフィックス<roma-prefix>`
に分解される際、例えば「じ」は ``j`` に、「ち」は ``t`` に、「ふ」は ``h`` に、
それぞれ分解されます。これらをそれぞれ ``z`` 、 ``c`` 、 ``f`` に変更することもで
きます。それには変数 :el:defvar:`skk-kana-rom-vector` の該当部分を ``z`` 、 ``c`` 、 ``f`` に
変更します。

.. code:: emacs-lisp

   (setq skk-rom-kana-vector
         ["x" "a" "x" "i" "x" "u" "x" "e" "x" "o" "k" "g" "k" "g" "k" "g"
          "k" "g" "k" "g" "s" "z" "s" "z" "s" "z" "s" "z" "s" "z" "t" "d"
          "c" "d" "x" "t" "d" "t" "d" "t" "d" "n" "n" "n" "n" "n" "h" "b"
          "p" "h" "b" "p" "f" "b" "p" "h" "b" "p" "h" "b" "p" "m" "m" "m"
          "m" "m" "x" "y" "x" "y" "x" "y" "r" "r" "r" "r" "r" "x" "w" "x"
          "x" "w" "n"])

次にもうひとつ例を挙げます。「ありがさつき」に対して「有賀さつき」を登録したい場
合は、上記と同様に辞書登録をし、

.. code:: text

   -------------------------- Echo Area --------------------------
   Shall I register this as okuri-ari entry: ありがs /有賀/ ? (y or n)
   -------------------------- Echo Area --------------------------

の確認に対して :kbd:`n` と回答します。この結果、個人辞書の「送りなしエントリ」には、

.. code:: text

   ありがさつき /有賀さつき/

というエントリが書き込まれます。

.. _okuri-conv-start:

送りあり変換の変換開始のタイミング
==================================

.. el:defvar:: skk-process-okuri-early

   この変数の値を non-nil に設定すると、送りあり変換の変換開始のタイミングが
   早められます。つまり、送り仮名の :ref:`ローマ字プレフィックス<roma-prefix>` の
   入力時点で変換を開始します。

   .. code:: text

      U g o K

        ------ Buffer: foo ------
        ▼動k
        ------ Buffer: foo ------

   送り仮名が分からないまま変換しているため、個人辞書が送り仮名に対応した形に成長
   しません。つまり ``うごk /動/`` のような形態のままとなります。た だし、

   .. code:: text

      うごk /動/[く/動/]/[か/動/]/[け/動/]/[き/動/]/[こ/動/]/

   のようなエントリが既に個人辞書にある場合、それを破壊することはありません [#]_ 。

   このユーザオプションを non-nil に設定して SKK モードを起動すると、両立でき
   ないオプションである下記オプションは自動的に nil に設定されます。

   - 変数 :el:defvar:`skk-kakutei-early`
   - 変数 :el:defvar:`skk-auto-okuri-process`
   - 変数 :el:defvar:`skk-henkan-okuri-strictly`

   既に SKK モードに入った後でこの変数の設定を変更した場合は、カレントバッファで
   :kbd:`C-x C-j` もしくは :kbd:`C-x j` を２回打鍵して SKK モードを起動し直す
   ことで、これらの変数間の衝突を調整します。

   - :ref:`暗黙の確定のタイミング <ammoku-kakutei>`

   - :ref:`送り仮名の自動処理 <okurigana>`

   - :ref:`送り仮名の厳密なマッチ <okuri-strictly>`

**********
候補の順序
**********

skk の初期設定では、変換で確定された単語は、次の変換時では最初に表示されます。
この動作を変更して、効率良く変換する方法があります。

ここで解説するほか、 :ref:`確定辞書を用いた変換 <kakutei-jisyo>` も、候補の順序に
影響を与えます。

.. _skk-study:

変換の学習
==========

ファイル :file:`skk-study.el` は、ある語 A を確定した場合に、A 及びその見出し語 A' に対し
て、直前に変換した語 B とその見出し語 B' を関連語として登録しておき、
再度見出し語 A' の変換を行ったときに、B 及び B' のペアが直前の何回かに確定した語
の中に見つかれば、A を優先して出力する単純な学習効果を提供するプログラムです。

ファイル :file:`~/.skk` に :code:`(require 'skk-study)` と書いて DDSKK を起動して下さい。
以降、 かな漢字変換の学習を始めます。

例えば「梅雨には雨が降る」と変換した場合、

  -  雨（あめ）の関連語 → 梅雨（つゆ）

  -  降る（ふr）の関連語 → 雨（あめ）

という風に「直前に確定した語」を関連語として、語と語の関連性を学習します。

ここで続けて「傘を振る」と変換すると、個人辞書が更新されてしまい、見出し語「ふr」
の第一候補は「振る」になってしまいます。

しかし、更に続けて :kbd:`A m e SPC g a H u R u` とキー入力すると、
``H u R u`` （ふr）に対して「雨」（あめ）が関連語になっているため、「ふr」と対で
記憶されている「降る」に変換されるというわけです。

では、またここで「傘を振る」と変換し、個人辞書の第一候補が「振る」に更新された状
態で、

.. code:: text

    A m e SPC g a T a i r y o u SPC n i H u R u

と変換すれば、「ふr」はどう変換されるでしょうか？　今度は「雨」（あめ）と「ふr」
の間に「大量」（たいりょう）が入っています [#]_ 。

実はちゃんと「雨が大量に降る」と変換されます。何故なら「ふr」の関連語を探す際、
変数 :el:defvar:`skk-study-search-times` に指定された回数分だけ遡って、以前に確定
した語の中に関連語がないか探すのです。従って、この場合だと、２つ前の確定情報を探
した際に「雨」（あめ）を見つけ、これを関連語として「ふr」の値を決めようとするのです。

ファイル :file:`skk-study.el` に関するその他のオプションを説明します。

.. el:defvar:: skk-study-sesearch-times

   現在の変換キーに対する関連変換キーをいくつまで遡って検索するか。
   標準設定は 5 です。

.. el:defvar:: skk-study-max-distance

   この変数には integer を指定します。標準設定値は 30 です。直前に確定したポイン
   トと今回の変換ポイントがこの距離以上離れていると学習データを蓄積しないようにし
   ます。この変数は、必ずしも文章がバッファの point-min から point-max へ流れるよ
   うに書かれるものではなく、ポイントを前に戻したり後へ移動したりして書かれること
   を想定しています。この変数に integer を 設定すると、直前の変換よりも前のポイン
   トで変換した場合に学習データを蓄積しないようにします。

   この変数に nil を指定すると、直前に確定したポイントとの距離を考慮せずに学習し
   ます。

   なお、この変数の値にかかわらず、直前の変換バッファと現在変換を行っているバッフ
   ァが異なる場合は学習データを蓄積しません。

.. el:defvar:: skk-study-first-candidate

   この変数が non-nil であれば、第一候補で確定した際も学習します。
   nil であれば、第一候補で確定したときのみ学習データを蓄積しません。
   学習データをできるだけ小さくしたい場合、この変数を nil にすると効果がある
   かもしれません。この変数の標準設定値は t です。

.. el:defvar:: skk-study-file

   学習結果を保存するファイル名です。この変数の標準設定値は `~/.skk-study` です。
   変数 :el:defvar:`skk-user-directory` からも設定ができます。

   :ref:`設定ファイル <configure-file>`

.. el:defvar:: skk-study-backup-file

  ファイル :file:`~/.skk-study` のバックアップファイルです。
   この変数の標準設定値は `~/.skk-study.BAK` です。

.. el:defvar:: skk-study-sort-saving

   学習データのデータ構造に関するものです。この変数の値が non-nil であれば、
   学習結果をソートしてセーブします。この変数が影響を及ぼすのは学習データの単なる
   見映えの問題だけです。この変数の標準設定値は nil です。

.. el:defvar:: skk-study-check-alist-format

   学習データのデータ構造に関するものです。この変数の値が non-nil であれば、
   学習結果の読み込み時に連想リストのフォーマットをチェックします。
   これは主に debug の目的で使います。この変数の標準設定値は nil です。

.. el:define-key:: M-x skk-study-switch-current-theme

   そのバッファで利用する学習テーマを切り替えます。プロンプト

   .. code:: text

       ------ Minibuffer -------
       Theme of current buffer: *
       ------ Minibuffer -------

   に対して学習テーマ名を入力してください。例えば、科学の話題を書くバッファでは
   ``science`` と、法律の話題を書くバッファでは ``law`` などと入力してください。

.. el:define-key:: M-x skk-study-remove-theme

   不要な学習テーマを消去します。

.. el:define-key:: M-x skk-study-copy-theme

   学習テーマを複製します。

候補の順序の固定
================

skk の初期設定では、変換、選択された候補は、次回の変換では最初に表示されます。
これに対し、毎回同じ順序で候補を表示させることができます。

.. el:defvar:: skk-jisyo-fix-order

   non-nil であれば、確定の際に個人辞書の同音語の順序を変更せず、個人辞書に新
   規追加する際は既出語の後に追加する。標準は nil 。

これは、個人辞書のエントリの中の各候補の順序を変更しないことで実現されていますの
で、ファイル :file:`skk-study.el` による変換の学習と併用できます。

:ref:`変換の学習 <skk-study>`

変数 :el:defvar:`skk-jisyo-fix-order` が non-nil の時、個人辞書の候補を手軽に並べ
替える方法は、現時点ではありません。
コマンド :kbd:`M-x skk-edit-private-jisyo` を実行して
:ref:`個人辞書ファイルを直接編集 <edit-jisyo>` して下さい。
直前に変換したばかりの単語は、個人辞書の送りあり／なしエントリの一番上にあります
ので、すぐに見つけることができます。

ベイズ統計を用いた学習
======================

ファイル :file:`skk-bayesian.el` は、直前の履歴のみ使用するファイル :file:`skk-study.el` に
比べて、更に拡張された学習機能です。
ベイズ統計を用いて文脈から変換候補が選択される確率を計算して候補順をソートします。
なお、機能が重なることからファイル :file:`skk-study.el` と の併用はお勧めできません。

動作の枠組みは

- emacs lisp のファイル :file:`skk-bayesian.el` と
- ruby スクリプト [#]_ の コマンド :command:`bskk` [#]_

が連携することで実現しています。

ファイル :file:`skk-bayesian.el` のインストールについてはファイル :file:`bayesian/README.ja.md`
を参照してください。

.. el:defvar:: skk-bayesian-debug

   non-nil ならば、以下のとおりデバッグ用のメッセージを表示します。

   - ファイル :file:`skk-bayesian.el` が吐き出すメッセージを ``*Messages*`` バッファに表示します。

   -  コマンド :command:`bskk` サブプロセスを ``-d`` オプションで起動させます。
      コマンド :command:`bskk` はファイル :file:`$HOME/tmp/bskk.log` にメッセージを吐き出します。

   -  普段は非表示である ``*skk-bayesian*`` バッファを表示するようにします。
      このバッファには コマンド :command:`bskk` の出力が表示されます。

.. el:defvar:: skk-bayesian-prefer-server

   non-nil ならば変数 :el:defvar:`skk-bayesian-host` の変数 :el:defvar:`skk-bayesian-port` に
   接続します。
   nil であれば コマンド :command:`bskk` を emacs のサブプロセスとして起動します。

.. el:defvar:: skk-bayesian-host

   コマンド :command:`bskk` サーバが稼動しているホスト名

.. el:defvar:: skk-bayesian-port

   コマンド :command:`bskk` サーバのポート番号

.. el:defvar:: skk-bayesian-history-file

   not documented

.. el:defvar:: skk-bayesian-corpus-make

   not documented

.. el:defvar:: skk-bayesian-corpus-file

   not documented

.. el:define-key:: M-x skk-bayesian-kill-process

   not documented

********
辞書関連
********

本節では、辞書の種別と形式、設定方法、その他辞書にまつわる動作や設定を説明します。

.. _jisyo-variant:

辞書の種類
==========

- 共有辞書

  ユーザの変換操作によって内容が書き替えられることはありません。

  ファイル :file:`SKK-JISYO.S` (S 辞書)、ファイル :file:`SKK-JISYO.M` (M 辞書)、
  ファイル :file:`SKK-JISYO.ML` (ML 辞書)、ファイル :file:`SKK-JISYO.L` (L 辞書)
  などがあります。通常、個人辞書よりもサイズが大きく、省資源の面からユーザ間で共
  有して参照されます。

  これら以外にも、共有辞書として使えるファイルが配布されています。それぞれの辞書
  の詳細については https://skk-dev.github.io/dict/ をご参照下さい。

- 個人辞書

  変数 :el:defvar:`skk-jisyo` で指定されるファイル。
  DDSKKを一番最初に使い始めたときにホームディレクトリに自動的に作られます。その後
  の使用により日々刻々とエントリが追加され、更新されていきます。

  なお、最初の個人辞書として S 辞書をリネームして使用するのも良いかもしれません。

- skk-initial-search-jisyo

  これは共有辞書、個人辞書という区分のいずれにも属しません。これらは個人毎に持つ
  ものを使用するか、ユーザ間で共有しているものを使用します。

  その性格から、辞書内容の更新は行われず、参照のみ行われます。また使用目的から、
  通常は小さい辞書を使用します。

- skk-kakutei-jisyo

  同上

個人辞書、 skk-initial-search-jisyo, skk-kakutei-jisyo は Emacs のバッファに読み
込んで検索を行います。

共有辞書は設定により Emacs のバッファに読み込んで使用するか、または辞書サーバ経由
で使用します。

辞書ファイルの指定
==================

この節では、辞書ファイルを指定する変数を説明します。個人辞書とバックアップのディ
レクトリは、変数 :el:defvar:`skk-user-directory` でも変更できます。

:ref:`設定ファイル <configure-file>`

.. el:defvar:: skk-kakutei-jisyo

   :ref:`確定変換 <kakutei-jisyo>` のための辞書です。一番最初に参照されます。
   確定変換をしない時は、初期設定の nil のままで良いです。

.. el:defvar:: skk-initial-search-jisyo

   確定辞書の後、かつ、個人辞書の前に検索を行う辞書です。この辞書を適当に指定する
   ことにより、最初に出てくる候補を操作することができます。
   例えば、複数の専門用語毎の辞書を用意しておいて変数 :el:defvar:`skk-initial-search-jisyo`
   の値を切り替えることにより、専門分野毎の専門用語を切り替えて入力することができ
   ます。

   この辞書は、標準の配布パッケージには含まれていないので、使用するのであればユー
   ザ側で用意する必要があります。不要ならば、初期設定の nil のままで良いです。

.. el:defvar:: skk-jisyo

   個人辞書。DDSKK を一番最初に起動したとき、変数 :el:defvar:`skk-jisyo` が指すファイルが
   存在しなければ自動的に作られます。

   .. code:: emacs-lisp

       (setq skk-jisyo "/your/path/to/jisyofile")

   個人辞書は、変数 @code{skk-jisyo-code} で指定された文字コードで取り扱われます。ところで、
   変数 @code{skk-jisyo-code} の指定は、個人辞書だけに限らず共有辞書などにも影響しますので、
   もし、個人辞書だけを変数 :el:defvar:`skk-jisyo-code` の指定とは異なる文字コードで取り扱
   いたいときは、次のとおりコンス・セルを設定します。

   .. code:: emacs-lisp

      (setq skk-jisyo (cons "/your/path/to/jisyofile" 'utf-8))

   この設定は :file:`~/.emacs.d/init.el` で行ってください。同時に、Emacs を起動する前に個人
   辞書ファイルの文字コードを変換しておく必要があります。配布物の :file:`etc/dot.emacs` に
   設定例が記載されていますので、参考にしてください。

.. el:defvar:: skk-backup-jisyo

   個人辞書の予備（バックアップ）です。検索の対象ではなく、あくまで個人辞書のバッ
   クアップとして指定してください。

.. el:defvar:: skk-cdb-large-jisyo

   共有辞書のうち :ref:`CDB 形式に変換した辞書 <cdb-format>` です。
   指定した場合は変数 :el:defvar:`skk-large-jisyo` よりも先に検索されます。
   DDSKK 14.1 からは辞書サーバを経由せずとも CDB 形式辞書ファイルを直接検索できる
   ようになりました。

.. el:defvar:: skk-large-jisyo

   共有辞書のひとつ。バッファに読み込んで検索を行います。
   例えば変数 :el:defvar:`skk-large-jisyo` に S 辞書 か M 辞書を指定し、
   :el:defvar:`skk-aux-large-jisyo` に L 辞書を指定する、という選択肢もあります。

   また、辞書サーバ経由のアクセスも決して遅くはないので「共有辞書はバッファには読
   み込まない」という設定も自然であり、これには変数 :el:defvar:`skk-large-jisyo` を nil に
   設定します。

.. el:defvar:: skk-aux-large-jisyo

   共有辞書のひとつ。辞書サーバに接続できない時にバッファに読み込んで検索を行う辞
   書です。

.. el:defvar:: skk-extra-jisyo-file-list

   SKK では個人辞書の他に、共有辞書または辞書サーバを設定して利用するのが一般的で
   すが、郵便番号辞書ファイル :file:`SKK-JISYO.zipcode` をはじめとした多彩な辞書
   もメンテナンスされています。

   これらの辞書を利用するために変数 :el:defvar:`skk-search-prog-list` を手動で編
   集することもできますが、この変数は厳密にはユーザ変数に分類されていないため、予
   期しない問題が起こることもあります。

   DDSKK 14.2 以降では追加の辞書を簡単に設定する方法を提供します。以下の例を参考
   に変数 :el:defvar:`skk-extra-jisyo-file-list` の設定をファイル :file:`~/.skk` に
   記述します。

   .. code:: emacs-lisp

       (setq skk-extra-jisyo-file-list
             (list '("/usr/share/skk/SKK-JISYO.JIS3_4" . euc-jisx0213)
                   "/usr/share/skk/SKK-JISYO.zipcode"))

   このように、辞書のファイル名のリストを指定します [#]_ 。

   ただし、変数 :el:defvar:`skk-jisyo-code` [#]_ とは異なる文字コードのファイルに
   ついては、上記の例中のファイル :file:`SKK-JISYO.JIS3_4` のように「ファイル名と
   文字コードのペア」を記述します。

これらの変数の意味するところは初期設定でのものですが、変数 :el:defvar:`skk-search-prog-list` の
設定で変更することもできます。

:ref:`辞書検索のための関数 <jisyo-search-functions>`

.. _search-jisyo:

辞書の検索方法の設定
====================

辞書の検索方法の指定は、変数 :el:defvar:`skk-search-prog-list` で行われます。
特に必要が無ければ、読み飛ばして下さい。

.. _setting-search-jisyo:

辞書検索の設定の具体例
----------------------

この節では変数 :el:defvar:`skk-search-prog-list` の初期設定を示し、大体の流れを説
明します。

DDSKK では、複数の辞書を扱うことが可能です。複数の辞書が同時並列に検索されるので
はなく、指定した順番に検索します。変数 :el:defvar:`skk-search-prog-list` はリスト
であり、大雑把に言えば、確定されるまで先頭の要素から順に lisp として評価されます。

.. code:: emacs-lisp

   ((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
    (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
    (skk-search-jisyo-file skk-jisyo 0 t)
    (skk-okuri-search)
    (skk-search-cdb-jisyo skk-cdb-large-jisyo)
    (skk-search-jisyo-file skk-large-jisyo 10000)
    (skk-search-server skk-aux-large-jisyo 10000)
    (skk-search-ja-dic-maybe)
    (skk-search-extra-jisyo-files)
    (skk-search-katakana-maybe)
    (skk-search-sagyo-henkaku-maybe))

この例では、

  - skk-kakutei-jisyo （確定辞書）
  - skk-initial-search-jisyo
  - skk-jisyo （個人辞書）

の順に検索を行い、次に

  - :ref:`送り仮名の自動処理 <okurigana>`

を行い、その後

  - skk-cdb-large-jisyo と

  - skk-large-jisyo の

検索を順に行い、最後に skk-aux-large-jisyo に辞書サーバ経由でアクセスしています。

もし確定辞書で候補が見つかったらそのまま自動的に確定されます。１回 :kbd:`SPC` を
押す動作に対し、プログラム側では新たな候補を見つけるまで上記の動作を進めます。

例えば、

  - 確定辞書では候補は見つけられなかったが skk-initial-search-jisyo
    に候補がある場合、そこでいったん止まりユーザにその候補を表示します。

  - 更に :kbd:`SPC` が押されると、次は個人辞書を検索します。そこで候補が見つかり、
    しかもその候補が skk-initial-search-jisyo で見つけた候補とは異なるもので
    あったときは、そこでまた止まりその候補をユーザに表示します。

以降、共有辞書についても同様の繰り返しを行います。最後まで候補が見つからなかった
時は、 :ref:`辞書登録モード <jisyo-register-mode>` に入ります。

.. _jisyo-search-functions:

辞書検索のための関数
--------------------

前節で見たとおり、変数 :el:defvar:`skk-search-prog-list` を適切に定義することによ
って辞書の検索方法を指定します。そこで使われる辞書検索のための関数を使いこなすこ
とで、より細かい辞書検索の方法を指定することができます。

.. el:defun:: skk-search-jisyo-file FILE LIMIT &optional NOMSG

   通常の検索を行うプログラム。変数 :el:defvar:`skk-henkan-key` を見出し語（検索
   文字列）として、 ``FILE`` を被検索対象として変換検索を実施します。個人辞書、共
   有辞書又は辞書サーバを使わずに検索を行いたい場合はこの関数を使用します。

   第１引数 ``FILE`` は、被検索対象となる辞書ファイルを指定します。 nil を指定し
   たときは、検索を行いません。 ``FILE`` で指定した辞書ファイルは Emacs のバッフ
   ァに読み込まれます。

   第２引数 ``LIMIT`` は二分検索（バイナリ・サーチ）が行なわれる領域の大きさを指
   定します。ひとつの見出し語に対する変換動作に対し、検索対象の領域の大きさ [#]_ が
   第２引数に指定された数値より小さくなるまでは二分検索が行われ、最後に直線的検索
   （リニア・サーチ、関数 :el:defun:`search-forward` ）が １回行われます。

   第２引数に 0 を指定すると、常に直線的検索のみが行われます。個人辞書 :el:defvar:`skk-jisyo` は
   ソートされておらず二分検索が不可能であるため ``LIMIT`` を 0 にして下さい。

   第３引数 ``NOMSG`` が nil ならば、辞書ファイルをバッファに読み込む
   関数 :el:defun:`skk-get-jisyo-buffer` のメッセージをエコーエリアに出力します。
   non-nil を与えると出力しません。

.. el:defun:: skk-search-cdb-jisyo CDB-PATH

   not documented

.. el:defun:: skk-search-kakutei-jisyo-file FILE LIMIT &optional NOMSG

   確定変換を行う検索プログラム。検索対象の辞書ファイルは Emacs のバッファに読み
   込まれます。検索対象のファイルから候補を見つけると、内部変数 :el:defvar:`skk-kakutei-henkan-flag` を
   立てて、いきなり確定します。このためユーザが確定操作を行う必要はありません。
   引数の意味はいずれも関数 :el:defun:`skk-search-jisyo-file` の場合と同様です。

.. el:defun:: skk-okuri-search

   自動送り処理を行うプログラム。変数 :el:defvar:`skk-auto-okuri-process` の値が non-nil の
   ときだけ機能します。個人辞書の「送りありエントリ」を検索対象としているので、個
   人辞書のバッファを流用します。そのため、専用の辞書バッファは作りません。

   :ref:`送り仮名の自動処理 <okurigana>`

.. el:defun:: skk-search-server FILE LIMIT &optional NOMSG

   辞書サーバ経由で検索するプログラム。辞書サーバが使用不能になると辞書ファイルを
   Emacs のバッファに読み込んで検索を行います。引数の意味はいずれも
   関数 :el:defun:`skk-search-jisyo-file` と同じですが、これらは辞書を Emacs のバ
   ッファに読み込んだときのみ利用されます。

   辞書サーバが使う辞書ファイルの設定については、

     - :ref:`辞書サーバを使いたいときの設定 <setting-jisyo-server>`

     - :ref:`サーバ関連 <server-relative>`

   をご覧下さい。

Emacs 付属の辞書
================

GNU Emacs には、ファイル :file:`SKK-JISYO.L` を元に変換されたファイル :file:`leim/ja-dic/ja-dic.el` と
いう辞書が付属しています。

DDSKK 14.2 からは、このファイル :file:`ja-dic.el` を利用したかな漢字変換（送りあ
り、送りなし、接頭辞、接尾辞）が可能となりました。つまり、ファイル :file:`SKK-JISYO.L`
などの辞書ファイルを別途準備しなくても一応は DDSKK の使用が可能、ということです。

DDSKK 14.2 から追加された「ja-dic.el 検索機能」 (:el:defun:`skk-search-ja-dic`) は、
次の設定の **全てが無効** な場合に有効となります。

  -  skk-large-jisyo
  -  skk-aux-large-jisyo
  -  skk-cdb-large-jisyo
  -  skk-server-host

ただし、「ja-dic.el 検索機能」はファイル :file:`SKK-JISYO.L` を利用する場合と比べ
て、英数変換や数値変換などができません。可能な限りファイル :file:`SKK-JISYO.L` な
どの辞書を利用することを推奨します。

:ref:`辞書の入手 <getting-jisyo-files>`

.. el:defvar:: skk-inhibit-ja-dic-search

   この変数を non-nil に設定すると、変数 :el:defvar:`skk-large-jisyo` 等の値にか
   かわらず、あらゆる場面で関数 :el:defun:`skk-search-ja-dic` を無効とします。

.. el:defun:: skk-search-ja-dic

   GNU Emacs に付属するかな漢字変換辞書ファイル :file:`ja-dic.el` を用いて検索す
   る。現在 の GNU Emacs にはファイル :file:`SKK-JISYO.L` を基に変換されたファイ
   ル :file:`ja-dic.el` が付属している。この辞書データを用いて送りあり、送りなし、
   接頭辞、接尾辞の変換を行う。ただし、ファイル :file:`SKK-JISYO.L` のような英数
   変換、数値変換などはできず、また「大丈夫」のように複合語とみなしうる語彙が大幅
   に削除されている。

.. _server-relative:

サーバ関連
==========

辞書サーバの基本的な設定は、 :ref:`辞書サーバを使いたいときの設定 <setting-jisyo-server>`
を参照してください。

.. el:defvar:: skk-servers-list

   この変数を使うと、複数のホスト上の辞書サーバを使い分けることができます。
   この変数の値は、辞書サーバ毎の情報リストです。各リストは次の４つの要素から成り
   ます。

     - ホスト名
     - 辞書サーバ名（フルパス）
     - 辞書サーバが読み込む辞書ファイル名
     - 辞書サーバが使用するポート番号

   ただし、辞書ファイル名及びポート番号は、辞書サーバ自身が決定することもあるため、
   そのような場合は nil として構いません。

   例えば、以下のように設定します。

   .. code:: emacs-lisp

       (setq skk-servers-list
             '(("host1" "/your/path/to/skkserv" nil nil)
               ("host2" "/your/path/to/skkserv" nil nil)))

   上記の設定の場合、まず host1 上の辞書サーバと接続します。
   接続できなくなると、次に host2 上の辞書サーバと接続します。

.. el:defvar:: skk-server-report-response

   この変数の値が non-nil であれば、変換時に、辞書サーバの送出する文字を受け
   取るまでに関数 :el:defun:`accept-process-output` が実行された回数をエコーエリ
   アに報告します。

   .. code:: text

       -------------------- Echo Area --------------------
       辞書サーバの応答を 99 回待ちました
       -------------------- Echo Area --------------------

.. el:defvar:: skk-server-inhibit-startup-server

   nil であれば、辞書サーバと接続できない場合に、Emacs から関数 :el:defun:`call-process` で
   辞書サーバプログラムの起動を試みます。標準設定値は t です。

   ``inetd`` 経由で起動する多くの辞書サーバは関数 :el:defun:`call-process` で起動
   することができませんが、 ``skkserv`` のように

   .. code:: console

      skkserv [-p port] [jisyo]

   といったオプションを受け付けて関数 :el:defun:`call-process` で起動することがで
   きる辞書サーバを利用している場合には、この変数を nil に設定するのが良いかもし
   れません。

   辞書サーバプログラムと辞書ファイルは、次のように設定します。

  .. code:: elisp

     (setq skk-server-prog "/your/path/to/skkserv")
     (setq skk-server-jisyo "/your/path/to/SKK-JISYO.L")

.. el:defvar:: skk-server-remote-shell-program

   この変数には、リモートシェルのプログラム名を指定します。標準設定は、システム依
   存性を考慮する必要があるため、以下の Emacs Lisp コードを評価することにより決定
   されています。

   .. code:: emacs-lisp

       (or (getenv "REMOTESHELL")
           (and (boundp 'remote-shell-program) remote-shell-program)
           (cond
            ((eq system-type 'berkeley-unix)
             (if (file-exists-p "/usr/ucb/rsh") "/usr/ucb/rsh" "/usr/bin/rsh"))
            ((eq system-type 'usg-unix-v)
             (if (file-exists-p "/usr/ucb/remsh") "/usr/ucb/remsh" "/bin/rsh"))
            ((eq system-type 'hpux) "/usr/bin/remsh")
            ((eq system-type 'EWS-UX/V) "/usr/ucb/remsh")
            ((eq system-type 'pcux) "/usr/bin/rcmd")
            (t "rsh")))

.. el:defvar:: skk-server-version

   辞書サーバから得たバージョン文字列とホスト名文字列を表示する。

   .. code:: text

       (skk-server-version)
       -| SKK SERVER version (wceSKKSERV) 0.2.0.0 (ホスト名 foo:192.168.0.999: )

サーバコンプリージョン
======================

Server completion に対応した辞書サーバであれば、見出し語から始まる全ての語句の検
索が可能です。

.. el:defun:: skk-comp-by-server-completion

   この関数を変数 :el:defvar:`skk-completion-prog-list` の要素に追加すると、▽モ
   ードにおいて見出し語補完を実行します。

   .. code:: emacs-lisp

       (add-to-list 'skk-completion-prog-list
                    '(skk-comp-by-server-completion) t)

.. el:defun:: skk-server-completion-search

   この関数を変数 :el:defvar:`skk-search-prog-list` の要素に追加すると、変換を実
   行する際に変数 :el:defvar:`skk-server-completion-search-char` を付すことによっ
   て見出し語で始まるすべての候補を掲げます。

   .. code:: emacs-lisp

      (add-to-list 'skk-search-prog-list
                   '(skk-server-completion-search) t)

   .. code:: text

         ------ Buffer: foo ------
         ▽おおさか~*
         ------ Buffer: foo ------

      SPC

         ------ Buffer: *候補* ------
         A:おおさかいかだいがく
         S:大阪医科大学
         D:おおさかいがい
         F:大阪以外
         J:おおさかいだい
         K:大阪医大
         L:おおさかいちりつだいがく
         ------ Buffer: *候補* ------

.. el:defvar:: skk-server-completion-search-char

   標準設定は ``~`` （チルダ、 ``#x7e`` ）です。

.. _jisyo-format:

辞書の書式
==========

送りありエントリと送りなしエントリ
----------------------------------

以下は個人辞書の一例です。

.. code:: text

    ;; okuri-ari entries.
    たとe /例/[え/例/]/
    もt /持/[つ/持/]/[って/持/]/[た/持/]/[て/持/]/[ち/持/]/[と/持/]/
    たすk /助/[け/助/]/
    うごk /動/[く/動/]/[か/動/]/[け/動/]/[き/動/]/[こ/動/]/
    ふくm /含/[め/含/]/[む/含/]/[ま/含/]/[み/含/]/[も/含/]/
    :
    ;; okuri-nasi entries.
    てん /点/・/天/
    ひつよう /必要/
    さくじょ /削除/
    へんこう /変更/
    じゅんじょ /順序/
    ぐん /群/郡/
    こうほ /候補/
    いち /位置/一/壱/

.. index::
   keyword: エントリ

.. _jisyo-entry:

``てん /点/・/天/`` を例にして説明します。これは「てん」が見出し語であり、その候
補が「点」、「・」、「天」です。候補はそれぞれ ``/`` によって区切られています。
SKK では、見出し語と候補群を合わせた ``てん /点/・/天/`` の一行を **エントリ** と
呼びます。

.. index::
   keyword: ;; okuri-ari entries.
   keyword: ;; okuri-nasi entries.

辞書は単純なテキストファイルで、必ず下記の２つの行を持っています。

  - ``;; okuri-ari entries.``
  - ``;; okuri-nasi entries.``

この２つの行は、それぞれ「送り仮名あり」、「送り仮名なし」のエントリの開始地点を
示すマークです。

ファイルの先頭から ``;; okuri-ari entries.`` までの間の行で、行頭に ``;`` を持つ
行はコメント行として無視されます。
``;; okuri-ari entries.`` 以降にコメント行を含むことはできません。

.. index::
   keyword: 送りありエントリ
   keyword: 送りなしエントリ
   keyword: 送りあり変換
   keyword: 送りなし変換

``;; okuri-ari entries.`` と ``;; okuri-nasi entries.`` の間に囲まれた部分が
*送りありエントリ* です

``;; okuri-nasi entries.`` 以降のファイル末尾までの部分が *送りなしエントリ* です。

「送りありエントリ」を検索する変換を *送りあり変換* と、
「送りなしエントリ」を検索する変換を *送りなし変換* と呼びます。
SKK では、送り仮名の有無が変換方法のひとつの種別となっています。送り仮名がある変
換では「送りありエントリ」のみが検索され、送り仮名がない変換では送りなしエントリ
のみが検索されます。

ひとつの見出し語についてのエントリは１行内に書かれます。２行以上にまたがることは
できません。改行を含む候補については :code:`(concat "改\n行")` のように、 評価す
ると改行を該当個所に挿入するような Lisp プログラムに変換して辞書に収めています。

:ref:`プログラム実行変換 <program-conversion>`

.. _roma-prefix:

.. index::
   keyword: ローマ字プレフィックス

「送りありエントリ」は、基本的には ``もt /持/`` のようになっています。送り仮名部
分は、送り仮名をローマ字表現したときの１文字目 [#]_ で表現されています。この１エ
ントリで「持た」「持ち」「持つ」「持て」「持と」の５つの候補に対応します。その５
つの候補の送り仮名をローマ字プレフィックスで表現すれば、いずれも t になるからです。

.. _okuri-block-format:

送りありエントリのブロック形式
------------------------------

個人辞書の「送りありエントリ」には ``[`` と ``]`` に囲まれたブロックがあります。
これは、そのブロックの先頭にある平仮名を送り仮名に取る候補群です。

.. code:: text

    たとe /例/[え/例/]/
    :
    ふくm /含/[め/含/]/[む/含/]/[ま/含/]/[み/含/]/[も/含/]/

この例で見ると、見出し語「たとe」の場合は「え」を送り仮名とするひとつブロックから
構成されています。見出し語「ふくm」の場合は「ま」「み」「む」「め」「も」を送り仮
名とする５ブロックに分けられています。

この送り仮名毎のブロック部分は、変数 :el:defvar:`skk-henkan-okuri-strictly` ある
いは変数 :el:defvar:`skk-auto-okuri-process` のいずれかが non-nil である場合に使
用されます。この場合、検索において、見出し語の一致に加えて、更に送り仮名もマッチ
するかどうかをテストします。例えば、

.. code:: text

    おおk /大/多/[く/多/]/[き/大/]/

というエントリがあるとします。同じ見出し語「おおk」であっても、送り仮名が「き」で
あれば、候補は「大」のみで「多」は無視されます [#]_ 。

https://skk-dev.github.io/dict/ で配布している共有辞書では、 ``[`` と ``]`` を
使用した送り仮名毎のブロックの形式に対応していません。個人辞書のみがこの
形式で書き込まれていきます。変数 :el:defvar:`skk-henkan-okuri-strictly` が nil で
あっても送り仮名のブロック形式で書き込まれます [#]_ 。

.. _entries:

エントリの配列
--------------

共有辞書は「送りありエントリ」は ``;; okuri-ari entries.`` から順に下方向に、見出
し語をキーとして **降順** に配置され、
「送りなしエントリ」は ``;; okuri-nasi entries.`` から順に下方向に、見出し語をキー
として **昇順** に配置されます。

降順／昇順に配置されるのは、辞書サイズが大きいことに配慮して二分検索（バイナリサ
ーチ）を行うためです [#]_ 。

一方、個人辞書は、一番最後に変換された語が最も手前に置かれます。
つまり、「送りなしエントリ」は ``;; okuri-ari entries.`` を、
「送りありエントリ」は ``;; okuri-nasi entries.`` を基点として最小ポイントに挿入
されて辞書が更新されます [#]_ 。

個人辞書は、通常は共有辞書ほどはサイズが大きくないので、検索時にはそれぞれの基点
から直線的検索（リニアサーチ）が行われます。最後に確定された語は、ひとつのエント
リの中の最初の位置に置かれます。

強制的に辞書登録モードへ入る
============================

▼モードにて、エコーエリアに変換候補が表示されているときに :kbd:`.` を打鍵すると、
強制的に :ref:`辞書登録モード <jisyo-register-mode>` へ入ります。

.. el:defvar:: skk-force-registration-mode-char

   強制的に辞書登録モードへ入るためのキーキャラクタをこの変数で定義します。
   標準設定は ``.`` （ピリオド、0x2E）です。

.. _delete-wrong-register:

誤った登録の削除
================

誤って個人辞書に登録してしまった単語は、あとからでも削除できます。

いったん、削除したい単語を変換により求めて、その単語が表示された時点（確定する前
の▼モードの状態）で :kbd:`X` （大文字のエックス）を打鍵します。

ミニバッファに確認プロンプトが出るので :kbd:`y e s` と答えると、個人辞書の対応す
るエントリが削除されます。先程、現在のバッファにいったん入力した「誤りの変換結果」
も削除されます。

例えば、

.. code:: text

    さいきてき /再起的/

という :ref:`辞書エントリ <jisyo-entry>` を誤って登録してしまったという仮定で、こ
の誤登録を削除する場合を説明します。

.. code:: text

    S a i k i t e k i SPC

      ------ Buffer: foo ------
      ▼再起的*
      ------ Buffer: foo ------

    X

      ------------------ MiniBuffer ------------------
      Really purge "さいきてき /再起的/" ? (yes or no) *
      ------------------ MiniBuffer ------------------

    y e s RET

      ------ Buffer: foo ------
      *
      ------ Buffer: foo ------

.. _edit-jisyo:

個人辞書ファイルの編集
======================

.. warning::

   構文チェックが十分ではありませんので、個人辞書ファイルの編集は、自己責任のもと行ってください。

.. el:define-key:: M-x skk-edit-private-jisyo

   このコマンドを使うと、個人辞書ファイルが開かれます [#]_ 。個人辞書ファイルを開
   いて編集している最中も skk を使えますが、skk からの単語の登録、削除はできません。
   他にも少し制限がありますが、気にならないでしょう。

   編集が終わったら :kbd:`C-c C-c` とキー入力しましょう。個人辞書ファイルを保存し
   てバッファを閉じます。

.. _saving-jisyo:

個人辞書の保存動作
==================

個人辞書の保存動作について説明します。個人辞書の保存が行われる場合として、次の４
通りがあります。

  - :kbd:`C-x C-c` または :kbd:`M-x save-buffers-kill-emacs` によって Emacs を終
    了する場合

  - :kbd:`M-x skk-save-jisyo` と入力したか、メニューバーの ``Save Jisyo`` を選択
    した場合

  - 個人辞書の更新回数が、変数 :el:defvar:`skk-jisyo-save-count` で指定された値に
    達した結果として、自動保存（オートセーブ）機能が働くとき。

  - 変数 :el:defvar:`skk-save-jisyo-instantly` が non-nil であれば、単語登録（単
    語削除）のたびに個人辞書を保存する。

保存動作を分析して考えます。まず、 Emacs に読み込んだ個人辞書が更新されているかど
うかを調べます。更新されていたら保存動作に入ります。Emacs の個人辞書バッファを一
時ファイルに保存して、そのファイルサイズが現存の（セーブ前の）個人辞書より小さく
ないかどうかをチェックします。個人辞書より小さいときは、保存動作を継続するかどう
か、確認のための質問がされます [#]_ 。

.. code:: text

    --------------------------- Minibuffer -----------------------------
    New ~/.skk-jisyo will be 11bytes smaller.  Save anyway?(yes or no)
    --------------------------- Minibuffer -----------------------------


ここで :kbd:`n o RET` と答えた場合は、そこで保存動作が中止され、個人辞書は以前の
状態のままになります。 :kbd:`y e s RET` と答えた場合は、元の個人辞書を退避用の辞
書ファイル :file:`~/.skk-jisyo.BAK` に退避し、一時ファイルに保存した新しい個人辞
書を :el:defvar:`skk-jisyo` に保存します。

もし、一時ファイルのサイズが 0 である場合は、なんらかの異常と考えられるため保存動
作は直ちに中止されます。その場合は :kbd:`M-x skk-kill-emacs-without-saving-jisyo`
で Emacs を終了させ、 個人辞書 (:el:defvar:`skk-jisyo`) 及び個人辞書の退避用辞書 (:el:defvar:`skk-backup-jisyo`)
をチェックするよう強くお勧めします [#]_ 。

.. el:defvar:: skk-compare-jisyo-size-when-saving

   この変数の値を nil に設定すると、保存前の個人辞書とのサイズを比較しません。

.. el:defvar:: skk-jisyo-save-count

   この変数で指定された回数、個人辞書が更新された場合に個人辞書が自動保存されます。
   標準設定は 50 です。この値を nil にすると、個人辞書の自動保存機能が無効に
   なります。

   ここで、個人辞書の更新回数は確定回数と一致します。また、同じ候補について確定し
   た場合でもそれぞれ１回と数えられます  [#]_ 。

.. el:defvar:: skk-save-jisyo-instantly

   この変数が non-nil であれば、単語を登録するたび（削除するたび）に個人辞書
   を保存します。

.. el:defvar:: skk-share-private-jisyo

   Non-nil であれば、複数の SKK による個人辞書の共有を考慮して辞書を更新する。
   SKK 起動後にこの変数を変更した場合は :kbd:`M-x skk-restart` で反映させること。

変換及び個人辞書に関する統計
============================

DDSKK は、かな漢字変換及び個人辞書に関する統計を取っており、Emacs の終了時に
ファイル :file:`~/.skk-record` に保存します。保存する内容は、以下の形式です。

.. code:: text

    Sun Jul 28 09:38:59 1996  登録:   4  確定:  285  確定率:  98%  語数:  3042

上記の「語数:」の数は個人辞書 :el:defvar:`skk-jisyo` に登録されている候補数ですが、
ここでは１行を１語として数えています。そのため、ひとつの見出し語に対して複数の候
補を持っている場合は、２つ目以降の候補を無視しています。

.. el:defvar:: skk-record-file

   統計情報を保存するファイル名を指定します。

   :ref:`設定ファイル <configure-file>`

.. el:defvar:: skk-keep-record

   この変数の値を nil に設定すると、本節で説明した統計機能を無効にします。
   数値を設定すると、変数 :el:defvar:`skk-record-file` を指定数値の行数より大きく
   しません。

.. el:defvar:: skk-count-private-jisyo-candidates-exactly

   この変数の値を non-nil に設定すると、「語数」の数え方を変更します。具体的
   には、１行を１語として数えるのではなく、正確に語数を数えます。なお、その分時間
   がかかります。

   また、この場合でも ``[`` と ``]`` に囲まれた送り仮名毎のブロック形式内は数えま
   せん。

.. el:define-key:: M-x skk-count-jisyo-candidates

   このコマンドを使うと、辞書の候補数を数えることができます。

   .. code:: text

       M-x skk-count-jisyo-candidates

         --------------- MiniBuffer --------------
         Jisyo file: (default: /your/home/.skk-jisyo) ~/*
         --------------- MiniBuffer --------------

       . s k k - j i s y o RET

         -------------- Echo Area --------------
         Counting jisyo candidates... 100% done
         -------------- Echo Area --------------

         ------ Echo Area ------
         3530 candidates
         ------ Echo Area ------

   ただし、 ``[`` と ``]`` に囲まれた送り仮名毎のブロック形式内は数えません。

   また、メニューバーが使用できる環境では、メニューバーを使ってこのコマンドを呼び
   出すことができます。

   :infonode:`Menu Bars in GNU Emacs Manual <(emacs)Menu Bars>`

辞書バッファ
============

辞書検索プログラムを実行すると、必要ならば辞書が Emacs のバッファに読み込まれます。
このバッファを **辞書バッファ** と呼びます。辞書バッファの命名規則は、

.. code:: text

    空白 ＋ * ＋ 辞書ファイル名（ディレクトリ抜き） ＋ *

です。

例えば、変数 :el:defvar:`skk-large-jisyo` の値がファイル :file:`/usr/local/share/skk/SKK-JISYO.L` で
あるとき、これに対する辞書バッファ名は ``_*SKK-JISYO.L*`` （アンダーバーは
SPACE の 意）となります。

このバッファのメジャーモードは ``fundamental-mode`` です。しかし、諸般の事情によ
り、変数 :el:defvar:`major-mode` の値をシンボル 'skk-jisyo-mode と、
変数 :el:defvar:`mode-name` の値を文字列 ``SKK dic`` としています [#]_ 。

辞書バッファの文字コードの設定
==============================

.. el:defvar:: skk-jisyo-code

   この変数は、辞書ファイルの文字コードを決定し、以下のような値を取ります。

   -  nil （標準設定）
      この場合、シンボル 'euc-jis-2004 が使われます。
      詳細は、関数 :el:defun:`skk-find-coding-system` を参照のこと。

   -  Emacs の coding system （コード系） [#]_

   -  ``euc``, ``ujis``, ``sjis``, ``jis`` の文字列。
      :el:defvar:`skk-coding-system-alist` に従って、順に 'euc-jisx0213, 'euc-jisx0213,
      'shift_jisx0213, 'iso-2022-jp-3-strict の各シンボルへ変換されます。

辞書バッファの buffer-file-name
===============================

Emacs には関数 :el:defun:`save-some-buffers` という関数があります。この関数は、フ
ァイルに関連付けられている各バッファについて、変更があればファイルに保存しますが、
実際に保存するかどうかをユーザに質問します。

Emacs のコマンドには :kbd:`M-x compile` のように関数 :el:defun:`save-some-buffers` を呼び出す
ものがあります。もし、個人辞書の辞書バッファがファイル名と関連付けられていたとし
たら、こうしたコマンドを実行するたびに個人辞書を保存するかどうか質問されるので、
面倒です。

DDSKK では、このような事態を避けるため、辞書バッファにおける変数 :el:defvar:`buffer-file-name`
の値を nil に設定しています。

.. _annotation:

**********************
注釈（アノテーション）
**********************

かな漢字変換の際に、候補に注釈（アノテーション）が登録されていれば、それを表示す
ることができます。

アノテーションの基礎
====================

この節では、辞書の中でのアノテーションの取り扱いを説明します。

アノテーションは、

  1. ユーザが登録したもの、
  2. 共有辞書に元々登録されているもの、
  3. それ以外の情報源から取得されるもの

の３つに大別されます。

.. index::
   keyword: ユーザアノテーション

ユーザが付けたアノテーションを「ユーザアノテーション」と呼びます。ユーザアノテー
ションは、次の形式で個人辞書に登録されます。

.. code:: text

    きかん /期間/機関;*機関投資家/基幹;*基幹業務/

上記のとおり、 ``;`` の直後に ``*`` が自動的に振られる [#]_ ことによってユーザが
独自に登録したアノテーションであることが分かります。

.. index::
   keyword: システムアノテーション

一方、共有辞書に元々登録されているアノテーションを「システムアノテーション」と呼
び、これは ``;`` の直後に ``*`` の文字を伴いません。システムアノテーションは、次
の形式で辞書に登録されています。

.. code:: text

    いぜん /以前;previous/依然;still/

システムアノテーションは L 辞書等に採用されています。

.. index::
   keyword: 外部アノテーション

上記のいずれでもなく、外部の辞典その他の情報源から得られるものを「外部アノテーシ
ョン」といいます。外部アノテーションは Emacs Lisp パッケージである lookup.el、
Apple macOS 付属の辞書、Wiktionary/Wikipedia などから取得可能です。

アノテーションの使用
====================

.. el:define-key:: C-w

   :kbd:`C-w` をタイプすると、現在表示されているアノテーションを kill ring
   に保存します。

   :infonode:`The Kill Ring in GNU Emacs Manual <(emacs)Kill Ring>`

.. el:define-key:: ^

   候補バッファで変換候補を一覧表示しているときにアノテーションの表示／非表示を動
   的に切り替えるキーを設定します。標準設定は ``^`` です。

   .. code:: text

         ----- Buffer: *候補* -----
         A:射
         S:亥;[十二支](12)いのしし
         D:夷;夷狄
         F:姨;おば
         J:洟;はな
         K:痍;満身創痍
         L:維;維持
         ----- Buffer: *候補* -----

       ^

         ----- Buffer: *候補* -----
         A:射
         S:亥;
         D:夷;
         F:姨;
         J:洟;
         K:痍;
         L:維;
         ----- Buffer: *候補* -----

.. el:defvar:: skk-show-annotation

   .. list-table::

      * - 設定例
        - 動作
      * - t
        - アノテーションを常に表示します。
      * - '(not list)
        - 候補バッファではアノテーションを表示しません。
      * - '(not minibuf)
        - ミニバッファにおけるかな漢字変換（単語登録時）ではアノテーションを表示しません。
      * - '(not list minibuf)
        - 候補バッファ及びミニバッファではアノテーションを表示しません。
      * - nil
        - いかなる場合もアノテーションを表示しません。

.. el:defvar:: skk-annotation-delay

   アノテーションを表示するまでの遅延を秒で指定する。標準設定は 1.0 秒。

.. el:defvar:: skk-annotation-show-as-message

   .. list-table::

      * - Non-nil （標準設定）
        - アノテーションをエコーエリアに表示します。
      * - nil
        - other-window を一時的に開いてアノテーションを表示します。
      * - 'other-window
        - その候補を確定するか、その候補の選択を止める（次の候補の表示又は quit）と自動的に閉じます。

   この変数の値にかかわらず、変数 :el:defvar:`skk-show-tooltip` が non-nil の場合はア
   ノテーションをツールティップで表示します。

.. el:defvar:: skk-annotation-function

   ユーザアノテーションとシステムアノテーションを区別することで、ユーザアノテーシ
   ョンだけを表示したり、あるいはその逆を行うことが可能です。

   変数 :el:defvar:`skk-annotation-function` に「表示したいアノテーションを non-nil と
   判定する関数」を定義します。
   アノテーション文字列を引数にして変数 :el:defvar:`skk-annotation-function` が指
   し示す関数が関数 :el:defun:`funcall` されて、戻り値が non-nil である場合に限っ
   てアノテーションが表示されます。

   .. code:: emacs-lisp

       (setq skk-annotation-function
             (lambda (annotation)
                 (eq (aref annotation 0) ?*)))

   上記の例では、アノテーションがユーザアノテーション（先頭が ``*`` で始まる）の
   場合に t を返すラムダ式を変数 :el:defvar:`skk-annotation-function` に定義しま
   した。これによってユーザアノテーションだけを表示することができます。

.. el:defvar:: interprogram-cut-function

   保存した内容を Emacs 以外のアプリケーションで利用したい場合に設定してください。

アノテーションの登録
====================

.. el:define-key:: M-x skk-annotation-add

   アノテーションを登録／修正するには、アノテーションを付けたい単語を確定した直後
   に同じバッファで :kbd:`M-x skk-annotation-add` と実行します。アノテーションを
   編集するバッファ ``*SKK annotation*`` が開いてカレントバッファになりますので、
   アノテーションとして表示する文章を編集してください。
   編集が終わったら :kbd:`C-c C-c` とタイプします。

   その単語に既にアノテーションが付いている場合は、あらかじめ当該アノテーションを
   挿入して ``*SKK annotation*`` バッファを開きます。

.. el:define-key:: M-x skk-annotation-kill

   上記 :kbd:`M-x skk-annotation-add` を実行したもののアノテーションを付けずに
   ``*SKK annotation*`` バッファを閉じたいときは :kbd:`C-c C-k` とタイプするか
   :kbd:`M-x skk-annotation-kill` を実行してください。

.. el:define-key:: M-x skk-annotation-remove

   特定の語からアノテーションを取り去りたいときは、まず、かな漢字変換で当該語を確
   定し、続けて :kbd:`M-x skk-annotation-remove` と実行します。

アノテーションとして EPWING 辞書を表示する
==========================================

.. index::
   keyword: EPWING 辞書

ファイル :file:`skk-lookup.el` に含まれる関数 :el:defun:`skk-lookup-get-content`
を活用することにより、EPWING 辞書から得た内容をアノテーション表示することが可能で
す。辞書検索ツールの Lookup [#]_ が正常にインストールされていることが前提です。
Lookup を新規にインストールした場合は、SKK をインストールし直す必要があります。

EPWING 辞書の内容をアノテーション表示するには、２つの方法があります。

skk-treat-candidate-appearance-function を設定する方法
------------------------------------------------------

候補の表示を装飾する関数を指定する変数 :el:defvar:`skk-treat-candidate-appearance-function` を
設定する場合は、ファイル :file:`etc/dot.skk` に示されている設定例を以下のように変更してく
ださい。

.. code:: emacs-lisp

    + (require 'skk-lookup)
      (setq skk-treat-candidate-appearance-function
            #'(lambda (candidate listing-p)
                (let* ((value (skk-treat-strip-note-from-word candidate))
                       (cand (car value))     ;候補
    -                  (note (cdr value))     ;注釈
    +                  (note (skk-lookup-get-content cand listing-p))
                       (sep (if note          ;セパレータ
                       :

.. el:defun:: skk-lookup-get-content 単語 listing-p

   「単語」の意味を EPWING 辞書から取得します。
   オプション引数 ``listing-p`` が non-nil ならば、候補一覧用に一行の短い文字
   列を返しますが、 nil ならば全体を返します。

.. el:defvar:: skk-lookup-get-content-nth-dic

   関数 :el:defun:`skk-lookup-get-content` が「どの EPWING 辞書から単語の意味を取得す
   るのか」を、ゼロを起点とした数値で指定します。docstring に例示した S 式 を評価
   してみてください。

.. el:define-key:: M-x skk-lookup-get-content-setup-dic

   DDSKK の起動後に変数 :el:defvar:`skk-lookup-get-content-nth-dic` の数値を変更した場合は
   、このコマンドを必ず実行してください。

skk-annotation-lookup-lookup を設定する方法
-------------------------------------------

次に変数 :el:defvar:`skk-annotation-lookup-lookup` について説明します。
この変数は EPWING 経由アノテーションの設定を簡単にします。

.. el:defvar:: skk-annotation-lookup-lookup

   Non-nil ならばファイル :file:`lookup.el` を利用してアノテーションを取得する。

   .. code:: emacs-lisp

       (setq skk-annotation-lookup-lookup t)

   この値をシンボル 'always に設定 [#]_ すると、候補一覧でも辞書サー ビスを引く。

   .. code:: emacs-lisp

       (setq skk-annotation-lookup-lookup 'always)

Apple macOS 「辞書」サービスからアノテーションを取得する
========================================================

Apple Mac OS X 10.5 (Leopard) 以降に標準で入っている国語辞典などからアノテーショ
ンが取得できます。この機能を利用するには、python の拡張機能として ``readline`` と
``pyobject-framework-DictionaryServices`` が必要です。

``pyobject-framework-DictionaryServices`` については Apple Mac OS X 10.5 (Leopard)
以降の OS 標準の python にあらかじめインストールされています。

``readline`` については Apple Mac OS X 10.7 (Lion) 標準の python では
インストールする必要がありません。 Apple Mac OS X 10.6 (Snow Leopard) 以前の場合は

.. code:: console

   % easy_install readline

などの方法でインストールします。

今のところ、アノテーションを取得する辞典を選択することはできません。
Apple macOS の「辞書」アプリ (Dictionary.app) を起動し、環境設定から辞書の検索順
を指定してください。国語辞典を上位に指定すれば使いやすくなります。

.. el:defvar:: skk-annotation-lookup-DictionaryServices

   Non-nil ならば Apple macOS の辞書サービスを利用してアノテーションを取得する。

   .. code:: emacs-lisp

       (setq skk-annotation-lookup-DictionaryServices t)

   この値をシンボル 'always に設定すると、候補一覧でも辞書サービスを引く [#]_ 。

   .. code:: emacs-lisp

       (setq skk-annotation-lookup-DictionaryServices 'always)

.. el:defvar:: skk-annotation-python-program

   アノテーション取得のために呼びだす python のプログラム名。

   .. code:: emacs-lisp

       (setq skk-annotation-python-program "/usr/bin/python")

Wikipedia/Wiktionary からアノテーションを取得する
=================================================

候補にアノテーションの登録がない場合、アノテーションに代えて

  - `Wiktionary <http://ja.wiktionay.org/>`_

  - `Wikipedia <http://ja.wikipedia.org/>`_

による解説を表示することができます。他のアノテーションが変換時に自動的に表示され
るのに対し、 Wikipedia/Wiktionary アノテーションは基本的にユーザの指示によって取
得される点が異なります。

▼モードで候補を表示しているときに :kbd:`C-i` を押すと、
変数 :el:defvar:`skk-annotation-other-sources` で指定された順で解説を取得してエコ
ーエリアに表示 [#]_ します。

.. code:: text

   B o k u j o u

      ----- Buffer: foo -----
      ▽ぼくじょう*
      ----- Buffer: foo -----

   SPC

      ----- Buffer: foo -----
      ▼牧場*
      ----- Buffer: foo -----

   C-i

      ----------------------------- Echo Area ------------------------------
      牧場（ぼくじょう）とは、ウシ、ウマなどの家畜を飼養する施設。訓読みされ
      てまきばと呼ばれることもある。
      ----------------------------- Echo Area ------------------------------

エコーエリアに解説が表示されている最中に :kbd:`C-o` を押すと、
関数 :el:defun:`browse-url` を用いて、その解説の元となった URL をブラウズします。

.. el:defvar:: skk-annotation-wikipedia-key

   標準設定は :kbd:`C-i` です。

.. el:defvar:: skk-annotation-browse-key

   標準設定は :kbd:`C-o` です。
   EWW (Emacs Web Wowser) で閲覧したい場合は、次のとおり設定してください。

   .. code:: emacs-lisp

       (setq browse-url-browser-function 'eww-browse-url)

   :infonode:`Emacs Web Wowser Manual <(eww)Top>`

.. el:defvar:: skk-annotation-other-sources

   アノテーションを取得する SKK 辞書以外のソースを指定します。

外部コマンドからアノテーションを取得する
========================================

外部コマンドからアノテーションを取得できます。

.. el:defvar:: skk-annotation-lookup-dict

   Non-nil ならば、変数 :el:defvar:`skk-annotation-dict-program` に指定された外部
   コマンドからアノテーションを指定します。

.. el:defvar:: skk-annotation-dict-program

   アノテーションを取得するための外部コマンド名を指定します。

.. el:defvar:: skk-annotation-dict-program-arguments

   アノテーションを取得に使う外部コマンドに渡す引数を指定します。

各種アノテーション機能を SKK の枠をこえて活用する
=================================================

これまでに解説した各種の外部アノテーション

  - lookup.el + EPWING 辞書
  - Apple macOS 辞書
  - Wikipedia / Wiktionary

は、SKK の変換モードだけでなく Emacs のあらゆる状況で辞書引き機能として使うことが
できます。そのためには、関数 :el:defun:`skk-annotation-lookup-region-or-at-point`
を任意にキー定義します。

.. el:defun:: skk-annotation-lookup-region-or-at-point &optional PREFIX-ARG START END

   このコマンドは、領域が指定されていればその領域の文字列をキーワードとして Lookup.el,
   Apple macOS 辞書サービス、または Wikipedia/Wiktionary アノテーションを探し、表
   示します。領域が指定されていなければ、可能な範囲でその位置にある単語（始点と終
   点）を推測します。

一例として、以下のキー割当を紹介します。

.. code:: emacs-lisp

    (global-set-key "\M-i" 'skk-annotation-lookup-region-or-at-point)

このようにしておくと、何かの意味が調べたくなったとき、領域選択して
:kbd:`M-i` と打鍵すれば、その場で辞書を引くことができます。

さらに、変数 :el:defvar:`skk-annotation-other-sources` の３番目 (Apple macOS では
４番目) は標準で ``en.wiktionary`` になっています。例えば、英文を読んでいて buffer
という語の正確な意味を参照したくなったとします。そのときは単語 buffer にポイント
を合わせて :kbd:`M-3 M-i` (Apple macOS では :kbd:`M-4 M-i`) とプレフィックス付で
コマンドを実行してみてください [#]_ 。

.. code:: text

   ----- Buffer: *scratch* -----
   ;; This buffer* is for notes you don't want to save, and for ...
   ----- Buffer: *scratch* -----

   M-3 M-i (Apple macOS では M-4 M-i)

すると SKK モードでのアノテーションと同様、以下のような説明が表示されます。

.. code:: text

   -------------------- Echo Area --------------------
    English, Noun
   buffer (plural&#160;buffers)
    1: Someone or something that buffs.
    2: (chemistry) A solution used to stabilize the pH (acidity) of a
    liquid.
    3: (computing) A portion of memory set aside to store data, often
    before it is sent to an external device or as it is received from an
    external device.
   -------------------- Echo Area --------------------

**************
文字コード関連
**************

.. _char-code-input:

文字コードまたはメニューによる文字入力
======================================

かなモードで :kbd:`\\` キーを打鍵すると、ミニバッファに

.. code:: text

   ---------------------------- Minibuffer -----------------------------
   ○○の文字を指定します。7/8 ビット JIS コード (00nn), 区点コード (00-00),
   UNICODE (U+00nn), または [RET] (文字一覧): *
   ---------------------------- Minibuffer -----------------------------

というプロンプトが表示され、文字コード（JIS コード、EUC コードまたは区点番号）ま
たはメニューによる文字入力が促されます。

プロンプト中の○○部分は、変数 :el:defvar:`skk-kcode-charset` の値であり、その初
期値は"japanese-jisx0208" 又は "japanese-jisx0213-1" です。初期値は環境によって自
動的に設定されます。キー :kbd:`\\` の代わりに :kbd:`C-u \\` と入力すると、異なる
文字集合 (charset) を指定する事ができます。

ここで、文字コードがあらかじめ分かっている場合には、その文字コードを入力します。
例えば「 ℃ 」の文字コードは、JIS コードでは 216e 、EUCコードでは a1ee なので、い
ずれかの文字コードを入力すれば ℃ が現在のバッファに挿入されます。

区点番号で入力するには 01-78 のように区と点の間にハイフン "-" を入れる必要があり
ます。ハイフン "-" で区切った３組の数字は JIS X 0213 の２面を指定したとみなします。
例えば 2-93-44 で「𩸽」（ほっけ、U+29e3d）が入力できます。

メニューによる文字入力
======================

文字コードが不明の文字を入力するには、文字コードを入力せずにそのまま :kbd:`RET` キー
を入力します。するとミニバッファに以下のような表示が現れます。

.. code:: text

    ---------------------------- Minibuffer -----------------------------
    A:　  S:￣  D:〜  F:｝  G:＝  H:¢  Q:◆  W:　  E:∩  R:　  T:≡  Y:　
    ---------------------------- Minibuffer -----------------------------

これを **第１段階のメニュー** と呼びます。
第１段階のメニューでは、JIS 漢字をコードの順に 16 文字毎に１文字抽出し、ミニバッ
ファに一度に 12 文字ずつ表示しています。

上記の例では、JIS コード 2121（全角スペース）、2131、2141、 2151 … の文字がそれ
ぞれ表示されています。

ここで :kbd:`SPC` を打鍵すると、次の候補群を表示します（文字コードの値を 16 * 12 = 192 ず
つ増やします）。
キー :kbd:`x` を打鍵すると、ひとつ前の候補群に戻ります。

キー :kbd:`a`, :kbd:`s`, :kbd:`d`, :kbd:`f`, :kbd:`g`, :kbd:`h`, :kbd:`q`, :kbd:`w`,
:kbd:`e`, :kbd:`r`, :kbd:`t`, :kbd:`y` のいずれかを打鍵する [#]_ と、そのキーに対
応する文字から始まる 16 個の文字が文字コード順に表示されます。これを **第２段階のメニュー**
と呼びます。

例えば、第１段階のメニューが上記の状態のときに :kbd:`d` を打鍵すると、第２段階の
メニューは以下のようになります。

.. code:: text

    --------------------------------- Minibuffer -----------------------------
    A:〜 S:‖ D:｜ F:… G:‥ H:‘ J:’ K:“ L:” Q:（ W:） E:〔 R:〕 T:［ Y:］ U:｛
    --------------------------------- Minibuffer -----------------------------

ここで、キー :kbd:`a`, :kbd:`s`, :kbd:`d`, :kbd:`f`, :kbd:`g`, :kbd:`h`, :kbd:`j`,
:kbd:`k`, :kbd:`l`, :kbd:`q`, :kbd:`w`, :kbd:`e`, :kbd:`r`, :kbd:`t`, :kbd:`y`,
:kbd:`u` のいずれかを打鍵すると、対応する文字がカレントバッファに挿入されてメニュー
による入力が終了します。

第２段階のメニューが表示されているときも :kbd:`SPC` と :kbd:`x` キーにより第２段
階のメニューが前進、後退します。

また、キー :kbd:`<` 及び :kbd:`>` により、メニューを１文字分だけ移動します。
例えば、第２段階のメニューが上記の状態のときにキー :kbd:`<` を打鍵すると、メニュ
ーは以下のようになります。

.. code:: text

    --------------------------------- Minibuffer -----------------------------
    A:＼ S:〜 D:‖ F:｜ G:… H:‥ J:‘ K:’ L:“ Q:” W:（ E:） R:〔 T:〕 Y:［ U:］
    --------------------------------- Minibuffer -----------------------------

第１段階あるいは第２段階のメニューが表示されているときにキー :kbd:`?` を打鍵する
と、そのときのキー :kbd:`A` に対応する文字（上記の例では "＼"）の文字コードが表示
されます。

.. index::
   pair: Key; \\
   pair: Function; skk-input-by-code-or-menu

.. el:defvar:: skk-kcode-method

   キー :kbd:`\\` の打鍵で起動する関数 :el:defun:`skk-input-by-code-or-menu` の挙
   動を調節します。

   .. list-table::

      * - 設定値
        - 挙動
      * - シンボル 'char-list
        - キー :kbd:`\\` の打鍵で、文字コード一覧関数 :el:defun:`skk-list-chars` を起動します。
      * - シンボル 'code-or-char-list
        - | キー :kbd:`\\` の打鍵で、文字コード関数 :el:defun:`skk-input-by-code` を起動します。
          | JIS コード／区点コード入力プロンプトの表示に対して単に :kbd:`RET` を打鍵した場合、
          | 文字コード一覧関数 :el:defun:`skk-list-chars` を起動します。
      * - シンボル 'this-key
        - キー :kbd:`\\` の打鍵で \\ を挿入します。
      * - 上記シンボル以外
        - | キー :kbd:`\\` の打鍵で、文字コード関数 :el:defun:`skk-input-by-code` を起動します。
          | JIS コード／区点コード入力プロンプトの表示に対して単に :kbd:`RET` を打鍵した場合、
          | 「メニュー入力」を起動します。

文字コード一覧
==============

.. index::
   pair: Key; M-x skk-list-chars

:kbd:`M-x skk-list-chars` と実行すると、変数 :el:defvar:`skk-kcode-charset` が指す
文字集合に従ってバッファ ``*skk-list-chars*`` に文字の JIS コード一覧が表示されます。
プレフィックス付きで、つまり :kbd:`C-u M-x skk-list-chars` と実行すると、カーソル
位置の文字に照準をあわすようコード一覧を表示します。

.. code:: text

    -------------------- *skk-list-chars* --------------------
    variable skk-kcode-charset's value is `japanese-jisx0208'.

    01-#x--- 0-- 1-- 2-- 3-- 4-- 5-- 6-- 7-- 8-- 9-- A-- B-- C-- D-- E-- F
    　 2120 　　　　、　。　，　．　・　：　；　？　！　゛　゜　´　｀　¨
    　 2130 ＾　￣　＿　ヽ　ヾ　ゝ　ゞ　〃　仝　々　〆　〇　ー　—　‐　／
    　 2140 ＼　〜　‖　｜　…　‥　‘　’　“　”　（　）　〔　〕　［　］
    　 2150 ｛　｝　〈　〉　《　》　「　」　『　』　【　】　＋　−　±　×
    　 2160 ÷　＝　≠　＜　＞　≦　≧　∞　∴　♂　♀　°　′　″　℃　￥
    -------------------- *skk-list-chars* --------------------

.. list-table::

   * - Key
     - 挙動
   * - :kbd:`C-f`, :kbd:`f`, :kbd:`l`
     - カーソル移動
   * - :kbd:`C-b`, :kbd:`b`, :kbd:`h`
     - カーソル移動
   * - :kbd:`C-n`, :kbd:`n`, :kbd:`j`
     - カーソル移動
   * - :kbd:`C-p`, :kbd:`p`, :kbd:`k`
     - カーソル移動
   * - :kbd:`C-x C-x`
     - カーソル移動
   * - :kbd:`RET`, :kbd:`i`
     - 文書バッファへ文字を挿入
   * - :kbd:`g`
     - aa
   * - :kbd:`\\`, :kbd:`o`
     - 文字集合の切り替え
   * - :kbd:`c`
     - 文字コード入力
   * - :kbd:`$`
     - カーソル位置の文字の文字コードを表示
   * - :kbd:`w`
     - aa
   * - :kbd:`q`
     - skk-list-chars を抜ける

ほか、Emacs のコマンド :kbd:`M-x list-charset-chars` や :kbd:`C-x 8 RET` も有用で
しょう。

.. el:defface:: skk-list-chars-table-header-face

   コード一覧の枠線などに適用するフェイス

.. el:defface:: skk-list-chars-face

   プレフィックス付きで実行したときの照準のフェイス

.. index::
   pair: Key; $
   pair: Key; M-x describe-char

文字コードを知る方法
====================

かなモード／カナモードでキー :kbd:`$` を打鍵する [#]_ と、現在のポイント位置の直
後にある文字の文字コードをエコーエリアに表示 [#]_ します
。

例えば、カーソルを文字 ``А`` の上に置いて :kbd:`$` を打鍵すると、

.. code:: text

    -------------------- Echo Area --------------------
    `А',KUTEN:07-01, JIS:#x2721, EUC:#xa7a1, SJIS:#x8440, UNICODE:U+0410,
    キリール大文字A,CYRILLIC CAPITAL LETTER A
    -------------------- Echo Area --------------------

とエコーエリアに表示され、この文字がキリル文字であることが分かります。

ほか、 Emacs のコマンド :kbd:`M-x describe-char` も有用でしょう。

.. el:defface:: skk-display-code-prompt-face

   エコーエリアに表示されるメッセージ中 ``KUTEN:`` 、 ``JIS:`` 、
   ``EUC:`` 、 ``SJIS:`` 及 び ``UNICODE:`` に適用するフェイスです。

.. el:defface:: skk-display-code-char-face

   エコーエリアに表示されるメッセージ中の当該文字に適用するフェイスです。

.. el:defface:: skk-display-code-tankan-radical-face

   エコーエリアに表示されるメッセージ中の総画数表示に適用するフェイスです。

.. el:defface:: skk-display-code-tankan-annotation-face

   エコーエリアに表示されるメッセージ中の文字名表示に適用するフェイスです。

**********************************
DDSKK 以外のツールを用いた辞書変換
**********************************

skk-lookup
==========

ファイル :file:`skk-lookup.el` を使用すると、辞書検索ツールの
`Lookup <http://openlab.jp/edict/lookup/>`_ で検索できる辞書を用いて単語の候補を
出すことができるようになります。

DDSKK のインストール過程で :code:`(require 'lookup)` が成功する場合は
ファイル :file:`skk-lookup.el` も自動的にインストールされます。
まずは コマンド :command:`make` :samp:`what-where` を実行して ``SKK modules:`` 欄 に ``skk-lookup``
が含まれていることを確認してください。

Lookup がインストールされているにも関わらず、うまくファイル :file:`skk-lookup.el` が
インストールされない場合は、ファイル :file:`SKK-CFG` を編集して（変数 :el:defvar:`ADDITIONAL_LISPDIR` にファイル :file:`lookup.el` が置かれているパスを設定する）、再度 DDSKK をインストールして下さい [#]_ 。

ファイル :file:`~/.skk` に以下のように設定します。

.. code:: emacs-lisp

   (setq skk-search-prog-list
         (append skk-search-prog-list
                 (list
                  '(skk-lookup-search))))

関数 :el:defun:`skk-lookup-search` は、 DDSKK が用意している検索プログラムの中で
最も遅いものです。したがって、変数 :el:defvar:`skk-search-prog-list` の設定にあっ
ては辞書サーバの検索関数 :el:defun:`skk-search-server` よりも後方に置くよう設定し
ます。

Lookup の agent で利用するのは、変数 :el:defvar:`lookup-search-agents` から
``ndkks``, ``ndcookie`` 及び ``ndnmz`` を取り去ったものです [#]_ 。

skk-look
========

ファイル :file:`skk-look.el` は、 コマンド :command:`look` コマンドを使って３つの
機能を提供します。

英単語の補完
------------

.. el:defvar:: skk-use-look

   non-nil に設定すると、ファイル :file:`skk-look.el` が使用できるようになります。
   例えばファイル :file:`~/.skk` で以下のように設定します。

   .. code:: emacs-lisp

      (setq skk-use-look t)

SKK abbrev モードが拡張されて コマンド :command:`look` コマンドを使用した補完が有
効になります。

.. code:: text

   / a b s t r

     ------ Buffer: foo ------
     ▽abstr*
     ------ Buffer: foo ------

   TAB

     ------ Buffer: foo ------
     ▽abstract*
     ------ Buffer: foo ------

と補完してくれます。通常の補完と同様に :kbd:`.` （ピリオド）で次の補完候補に、
:kbd:`,` （コンマ）でひとつ前の補完候補に移動できます。

:ref:`SKK 形式の英和辞書 edict <getting-jisyo-files>` があれば、
ここから :kbd:`SPC` を押して英和変換ができます。

.. _english-word:

英単語をあいまいに変換して取り出す
----------------------------------

見出し語にアスタリスク ``*`` を入れて :kbd:`SPC` を押すと、英単語をあいまいにして
変換できます。

.. code:: text

     ------ Buffer: foo ------
     ▽abstr*
     ------ Buffer: foo ------

   SPC

     ------ Buffer: foo ------
     ▼abstract*
     ------ Buffer: foo ------

確定すると、 ``abstr*`` を見出し語と、 ``abstract`` を候補とするエントリが個人辞
書に追加されます。このようなエントリを追加したくない場合、ユーザ変数
:el:defvar:`skk-search-excluding-word-pattern-function` を適切に設定します。

例えば次のような設定です。

.. el:defvar:: skk-search-excluding-word-pattern-function

   .. code:: emacs-lisp

       (add-hook 'skk-search-excluding-word-pattern-function
                 ;; 返り値が non-nil の時、個人辞書に取り込まない。
                 ;; KAKUTEI-WORD を引数にしてコールされるので、不要でも引数を取る
                 ;; 必要あり
                 (lambda (kakutei-word)
                     (and skk-abbrev-mode
                          (save-match-data
                            ;; SKK-HENKAN-KEY が "*" で終わるとき
                            (string-match "\\*$" skk-henkan-key)))))

英単語をあいまいに変換して取り出した後、更に再帰的な英和変換を行う
------------------------------------------------------------------

SKK 辞書に

.. code:: text

   abstract /アブストラクト/抽象/
   abstraction /アブストラクション/

というエントリがあるとして解説します [#]_ 。

.. el:defvar:: skk-look-recursive-search

   non-nil であれば、英単語 ＋ その英単語を見出し語にした候補の「セット」を変換結
   果として出力することができます。

   .. code:: text

         ▽abstr*

       SPC

         ▼abstract

       SPC

         ▼アブストラクト

       SPC

         ▼抽象

       SPC

         ▼abstraction

       SPC

         ▼アブストラクション

.. el:defvar:: skk-look-expanded-word-only

   この変数の値が non-nil であれば、再帰検索に成功した英単語の「セット」だけを出
   力することができます。再帰検索で検出されなかった英単語は無視して出力しません。

Lisp シンボル名の補完検索変換
=============================

SKK abbrev モードにて、Lisp シンボル名を補完して検索し、検索結果を候補として返す
ことができます。英文字の後ろに ``~`` を付加してから変換を開始してください。

まずは動作例を示します。

.. code:: text

    / d e f i ~

      ----- Buffer: foo -----
      ▽defi~*
      ----- Buffer: foo -----

    SPC

      ----- Buffer: foo -----
      ▽defimage*
      ----- Buffer: foo -----

    SPC

      ----- Buffer: foo -----
      ▽define-abbrev*
      ----- Buffer: foo -----

    SPC

      ----- Buffer: foo -----
      ▽define-abbrev-table*
      ----- Buffer: foo -----

    SPC

      ----- Buffer: foo -----
      ▽define-abbrevs*
      ----- Buffer: foo -----

    SPC

      ----- Buffer: *候補* -----
      A:define-auto-insert
      S:define-category
      D:define-ccl-codepoint-translation-table
      F:define-ccl-constant-translation-table
      J:define-ccl-identity-translation-table
      K:define-ccl-program
      L:define-ccl-slide-translation-table
      ----- Buffer: *候補* -----

この機能を有効とするには、リスト :el:defvar:`skk-search-prog-list` の要素に
関数 :el:defun:`skk-search-lisp-symbol` を加えてください。

.. code:: emacs-lisp

    (add-to-list 'skk-search-prog-list
             '(skk-search-lisp-symbol) t)

なお、見出し語に ``~`` を含む辞書もあります。例えばファイル :file:`SKK-JISYO.JIS3_4`
には

.. code:: text

    A~ /チルド付きA(LATIN CAPITAL LETTER A WITH TILDE)/

と登録されています。したがって、

.. code:: text

    ▽A~ SPC

と変換したときに「チルド付きA」が表示されるか、Lisp シンボル名が補完されるかは、
リスト :el:defvar:`skk-search-prog-list` 内の要素の順によります。

.. el:defun:: skk-search-lisp-symbol &optional PREDICATE NOT-ABBREV-ONLY WITHOUT-CHAR-MAYBE

   引数 ``PREDICATE`` で補完検索する範囲（関数名、変数名、コマンド名）を限定する
   ことができます。詳細は docstring を参照してください。

.. el:defvar:: skk-completion-search-char

   関数 :el:defun:`skk-completion-search` による変換機能を指示するキーキャラクタ。
   標準設定は ``~`` です。

Google CGI API for Japanese Input を利用したかな漢字変換
========================================================

かな漢字変換に `Google CGI API for Japanese Input <http://www.google.co.jp/ime/cgiapi.html>`_
を利用することができます。 連文節変換も可能となります。

まず、ファイル :file:`~/.skk` にて、変数 :el:defvar:`skk-use-search-web` を non-nil に
設定します。これにより、skk-mode を起動した際にファイル :file:`skk-search-web.el` を :el:defun:`require` するようになります。

同じくファイル :file:`~/.skk` にて、リスト :el:defvar:`skk-search-prog-list` の一
番最後の要素として、関数 :el:defun:`skk-search-web` を追加します。

.. code:: emacs-lisp

   (add-to-list 'skk-search-prog-list
                '(skk-search-web 'skk-google-cgi-api-for-japanese-input)
                t)

以上の設定によって、通常のかな漢字変換の **候補が尽きたとき** に関数 :el:defun:`skk-search-web` が
実行され、Google CGI API for Japanese Input による変換結果が表示されます。

そのほか、変数 :el:defvar:`skk-read-from-minibuffer-function` を以下のように設定
することで、辞書登録モードへの突入時の初期値に Google サジェストを表示することも
できます。

.. code:: emacs-lisp

   (setq skk-read-from-minibuffer-function
         (lambda ()
            (car (skk-google-suggest skk-henkan-key))))

********
飾りつけ
********

仮名文字のローマ字プレフィックスのエコー
========================================

.. el:defvar:: skk-echo

   この変数の値は、仮名文字の :ref:`ローマ字プレフィックス <roma-prefix>` のエコー
   の有無を制御します。

変数 :el:defvar:`skk-echo` の値が non-nil であれば、仮名文字の :ref:`ローマ字プレフィックス <roma-prefix>`
が、入力時点でいったん現在のバッファに挿入され、続く母音の入力の際に、かな文字に
変換された時点で現在のバッファから消去されます。

.. code:: text

   t

      ------ Buffer: foo ------
      t*
      ------ Buffer: foo ------

   a

      ------ Buffer: foo ------
      た*
      ------ Buffer: foo ------

変数 :el:defvar:`skk-echo` の値が nil であれば、仮名文字の :ref:`ローマ字プレフィックス <roma-prefix>`
のエコーは行われません。これを上記の例で考えると、 t が現在のバッファに挿入されず、
続く母音 a が入力された瞬間に「た」の文字が挿入されます。

.. el:defface:: skk-prefix-hiragana-face

   かなモードにおける :ref:`ローマ字プレフィックス <roma-prefix>` のフェイスを指定します。

.. el:defface:: skk-prefix-katakana-face

   カナモードにおける :ref:`ローマ字プレフィックス <roma-prefix>` のフェイスを指定します。

.. el:defface:: skk-prefix-jisx0201-face

   JIS X 0201 モードにおける :ref:`ローマ字プレフィックス <roma-prefix>` のフェイスを指定します。

入力モードを示すモードラインの文字列の変更
==========================================

.. index::
   pair: Variable; skk-latin-mode-string
   pair: Variable; skk-hiragana-mode-string
   pair: Variable; skk-katakana-mode-string
   pair: Variable; skk-jisx0208-latin-mode-string
   pair: Variable; skk-abbrev-mode-string

下記の変数の値を変更することによって、モードライン上の「入力モードを示す文字列」
を変更することができます。 skk-show-mode の表示も連動します。

.. list-table::

   * - 変数
     - モードライン
   * - :el:defvar:`skk-latin-mode-string`
     - アスキーモードを示す文字列。標準は ``SKK``
   * - :el:defvar:`skk-hiragana-mode-string`
     - かなモードを示す文字列。標準は ``かな``
   * - :el:defvar:`skk-katakana-mode-string`
     - カナモードを示す文字列。標準は ``カナ``
   * - :el:defvar:`skk-jisx0208-latin-mode-string`
     - 全英モードを示す文字列。標準は ``全英``
   * - :el:defvar:`skk-abbrev-mode-string`
     - SKK abbrev モードを示す文字列。標準は ``aあ``

.. _cursor-color-input-mode:

入力モードを示すカーソル色に関する設定
======================================

.. el:defvar:: skk-use-color-cursor

   この変数が non-nil ならば、カーソルを色付けします。

   標準では、ウィンドウシステムを使用して、かつ、色表示が可能な場合に限ってこの機
   能が有効になります。

この機能が有効になっているとき、以下の変数の値を変更することで、各モードにおける
カーソルの色を変更できます。

.. index::
   pair: Variable; skk-cursor-default-color
   pair: Variable; skk-cursor-hiragana-color
   pair: Variable; skk-cursor-katakana-color
   pair: Variable; skk-cursor-jisx0201-color
   pair: Variable; skk-cursor-jisx0208-latin-color
   pair: Variable; skk-cursor-latin-color
   pair: Variable; skk-cursor-abbrev-color

.. list-table::

   * - 変数
     - カーソルの色
   * - skk-cursor-default-color
     - | SKK モードがオフであることを示すカーソル色。
       | 標準では、カーソルのある該当フレームにおける標準のカーソル色を使います。
   * - skk-cursor-hiragana-color
     - | かなモードであることを示すカーソル色。
       | 標準は、背景の明暗により coral4 または pink です。
   * - skk-cursor-katakana-color
     - | カナモードであることを示すカーソル色。
       | 標準は、背景の明暗により forestgreen または green です。
   * - skk-cursor-jisx0201-color
     - | JIS X 0201 モードであることを示すカーソル色。
       | 標準は、背景の明暗により blueviolet または thistle です。
   * - skk-cursor-jisx0208-latin-color
     - | 全英モードであることを示すカーソル色。
       | 標準は gold です。
   * -  skk-cursor-latin-color
     - | アスキーモードであることを示すカーソル色。
       | 標準は、背景の明暗により ivory4 ま たは gray です。
   * - skk-cursor-abbrev-color
     - | SKK abbrev モードであることを示すカーソル色。
       | 標準は royalblue です。

変換候補一覧の表示方法
======================

変換候補一覧の表示方法は、次の４つに大別されます。

  1. 現在のウィンドウにインライン表示する

  2. ツールティップで表示する

  3. 現在のウィンドウの隣に別なウィンドウを開いて表示する（ポップアップ）

  4. エコーエリアに表示する

現在のウィンドウにインライン表示する
------------------------------------

.. el:defvar:: skk-show-inline

   この変数の値が non-nil であれば、候補一覧を現在のポイント位置でインライン表示
   します。値がシンボル 'vertical であれば、各候補を縦方向にインライン表示します。

.. el:defface:: skk-inline-show-face

   インライン表示する変換候補を装飾するフェイスを指定します。
   標準設定は ``underline`` です。

   .. code:: emacs-lisp

       (setq skk-inline-show-face 'font-lock-doc-face)

   変数 :el:defvar:`skk-treat-candidate-appearance-function` による装飾を優先するには nil に設
   定して下さい。

.. el:defvar:: skk-inline-show-background-color

   インライン表示する変換候補の背景色を指定します。

   :el:defface:`skk-inline-show-face` または変数 :el:defvar:`skk-treat-candidate-appearance-function` に
   て、背景色が指定されていない文字に対してのみ作用します。

.. el:defvar:: skk-inline-show-background-color-odd

   インライン表示する変換候補の背景色（奇数ライン）を指定します。

ツールティップで表示する
------------------------

.. el:defvar:: skk-show-tooltip

   この変数の値が non-nil であれば、候補一覧をツールティップで表示します。同時に、

     - :ref:`注釈（アノテーション）の表示方法 <annotation>` と
     - :ref:`文字コードの表示方法 <char-code-input>`

   も制御します。

.. el:defface:: skk-tooltip-face

   ツールティップ表示する文字列に適用するフェイスのシンボルを指定する変数です。

   .. code:: emacs-lisp

      (setq skk-tooltip-face 'font-lock-doc-face)
      ;; (make-face 'skk-tooltip-face) ではないことに注意

   候補文字列のフェイス属性（ 変数 :el:defvar:`skk-treat-candidate-appearance-function` に
   よる加工など）をそのまま使いたい場合は nil に設定して下さい。

.. el:defvar:: skk-tooltip-mouse-behavior

   ツールティップを表示する位置及びマウスポインタの挙動を指定します。下記に掲げる
   シンボル以外のシンボルを指定した場合は nil となります。

   シンボル 'follow
      マウスポインタをカーソル位置へ移動させてツールティップを表示します。
      ツールティップの表示を終えるとマウスポインタは元の位置へ戻ります。

      ただし、元のマウスポインタが Emacs フレーム外であったならばツールティップの
      表示を終えてもマウスポインタはカーソル位置のままです。

   シンボル 'banish
      マウスポインタを Emacs フレーム右上隅へ移動させてツールティップを表示します。
      ツールティップの表示を終えもてマウスポインタは Emacs フレーム 右上隅のままです。

   シンボル 'avoid
      マウスポインタを Emacs フレーム右上隅へ移動させてツールティップを表示します。
      ツールティップの表示を終えるとマウスポインタは元の位置へ戻ります。

      ただし、元のマウスポインタが Emacs フレーム外であったならばツールティップの
      表示を終えてもマウスポインタは Emacs フレーム右上隅のまま です。

   シンボル 'avoid-maybe
      マウスポインタが Emacs フレーム内であれば 'avoid と同じ動作です。
      マウスポインタが Emacs フレーム外であればマウスポインタ位置を変更せず、
      その位置にツールティップを表示します。

   nil
      マウスポインタを一切移動せず、その位置にツールティップを表示します。
      ツールティップのテキストとマウスポインタが重なったり、うまくツールテ
      ィップが表示できなかったりする場合があります。

.. el:defvar:: skk-tooltip-hide-delay

   ツールティップを表示する秒数（標準設定は 1000 秒）。
   この時間が経過すると、ツールティップは自動的に消える。

.. el:defvar:: skk-tooltip-parameters

   SKK 独自のフレームパラメータを設定する。
   標準設定 nil の場合、変数 :el:defvar:`tooltip-frame-parameters` が適用される。

現在のウィンドウの隣に別なウィンドウを開いて表示する（ポップアップ）
--------------------------------------------------------------------

.. el:defvar:: skk-show-candidates-always-pop-to-buffer

   この値が non-nil であれば、画面を上下に分割したうえで、候補一覧を専用の候補バ
   ッファで表示します。

候補一覧表示中に、この値を動的に切り換える手段が用意されています。

.. el:defvar:: skk-show-candidates-toggle-display-place-char

   候補一覧表示中に、候補一覧の表示位置をエコーエリアとバッファとで動的に切り換え
   ることができます。標準設定は :kbd:`C-f` です。

.. el:defvar:: skk-candidate-buffer-background-color

   候補バッファの背景色を指定します。背景色を付けたくない場合は nil を指定するこ
   と（標準設定）。

.. el:defvar:: skk-candidate-buffer-background-color-odd

   候補バッファの背景色（奇数ライン）を指定します。

エコーエリアに表示する
----------------------

標準設定では３つの変数

  - :el:defvar:`skk-show-inline`
  - :el:defvar:`skk-show-tooltip`
  - :el:defvar:`skk-show-candidates-always-pop-to-buffer`

とも nil であり、この状態では候補一覧はエコーエリアに表示 [#]_ します。

もしも、これら変数のうち２つ以上が non-nil の場合、優先順位は上記の解説の順です。

▼モードにおける変換候補のハイライト表示
========================================

.. el:defvar:: skk-use-face

   non-nil であれば、Emacs のフェイス機能を使って変換候補をハイライト表示します。
   このハイライト表示には GNU Emacs のオーバーレイ (overlay) の機能を使います [#]_ 。

.. el:defface:: skk-henkan-face

   この変数の値はフェイスであり、このフェイスによって変換候補がハイライト表示され
   ます。標準では、背景の明暗により black/darkseagreen2 又は white/darkolivegreen
   を用います。

   なお、この変数よりも変数 :el:defvar:`skk-treat-candidate-appearance-function` の
   設定が優先されます。
   
変数 :el:defvar:`skk-henkan-face` には、既存のフェイス [#]_ を指定できますが、
新たにフェイスを作ることもできます。そのために次の関数が用意されています。

.. el:defun:: skk-make-face FACE

   この関数は、引数 ``FACE`` と同じ名前のフェイスを作成して、そのフェイスを返しま
   す。フェイスの前景色・背景色は、引数 ``FACE`` にスラッシュ ``/`` を含めること
   よって、例えば以下の例のように決定されます。

   .. code:: emacs-lisp

       (setq skk-henkan-face (skk-make-face 'DimGray/PeachPuff1))

   上記の場合、前景色は DimGray に、背景色は PeachPuff1 になります。もうひとつ例
   を挙げます。

   .. code:: emacs-lisp

       (setq skk-henkan-face (skk-make-face 'RosyBrown1))

   上記の場合、前景色は RosyBrown1 になります。背景色が無指定の場合はバッファの背
   景色がそのまま見えます。

変換候補の更なる装飾
====================

変換候補についてユーザの任意の加工を施すための変数を用意してあります。

.. el:defvar:: skk-treat-candidate-appearance-function

   この変数に適切な形式で関数を収めることによって、変換候補をユーザの任意に加工す
   ることができます。「適切な形式」とは、次のとおりです。

   - 引数を２つ取ること。

   - 第１引数は文字列として扱うこと。これは加工前の文字列に相当する。

   - 第２引数が nil の時は通常の変換時、 non-nil の時は候補一覧表示時を表すもの
     として扱うこと。

   - 返り値は次のいずれかとすること。

     .. list-table::

        * - 返り値
          - 説明
        * - 文字列
          - 文字列は、候補と注釈を両方含みうるものとして処理される。
        * - :code:`(候補 . 注釈)`
          - | 候補は、もう注釈を含まないものとして処理される。
            | 注釈は、先頭が ``;`` かどうかを調べた上で処理される。
        * - :code:`(候補 . (セパレータ . 注釈))`
          - | 候補は、もう注釈を含まないものとして処理される。
            | セパレータは、通常の ``;`` の代わりに利用される。
            | 注釈は、もうセパレータを含まないものとして処理される。

ファイル :file:`etc/dot.skk` に設定例があるほか、サンプルとして
関数 :el:defun:`skk-treat-candidate-sample1` と関数 :el:defun:`skk-treat-candidate-sample2`
を用意してあります。

ファイル :file:`~/.skk` に次のいずれかを書いてみて変換候補の装飾を試してください。

.. code:: emacs-lisp

    (setq skk-treat-candidate-appearance-function
          'skk-treat-candidate-sample1)

.. code:: emacs-lisp

    (setq skk-treat-candidate-appearance-function
          'skk-treat-candidate-sample2)

モードラインの装飾
==================

インジケータ
------------

.. el:defvar:: skk-indicator-use-cursor-color

   モードラインの左に DDSKK のインジケータを表示（標準設定）している場合、インジ
   ケータの色がカーソルの色と同期します。インジケータに色を付けたくない場合は、こ
   の変数を nil にします。

   :ref:`入力モードを示すカーソル色に関する設定 <cursor-color-input-mode>`

インジケータに独自色を使いたい場合は、以下のフェイス [#]_ を設定します。この場合
カーソルの色は参照されません。

- GNU Emacs 21 以上（変数 :el:defvar:`mule-version` の値が 5.0 以上の GNU Emacs）の場合

   - :el:defface:`skk-emacs-hiragana-face`
   - :el:defface:`skk-emacs-katakana-face`
   - :el:defface:`skk-emacs-jisx0208-latin-face`
   - :el:defface:`skk-emacs-jisx0201-face`
   - :el:defface:`skk-emacs-abbrev-face`

なお、インジケータを右クリックするとポップアップメニューが表示されます。

インジケータの装飾
------------------

インジケータを装飾することができます。

.. el:defvar:: skk-indicator-prefix

   インジケータの接頭辞とする文字列を指定します。

.. el:defvar:: skk-indicator-suffix-func

   インジケータの接尾語とする文字列を返す関数を指定します。

アイコン
--------

.. el:defvar:: skk-show-icon

   変数 :el:defvar:`skk-show-icon` の値を non-nil と設定することにより、モードラ
   インに SKK のアイコンが表示されます。
   なお、アイコン表示は関数 :el:defun:`(image-type-available-p 'xpm)` が t を返す
   必要があるため、Emacs の種類／実行環境に依存します。

.. el:defvar:: skk-icon

   アイコンの画像ファイル :file:`skk.xpm` へのパス。
   関数 :el:defun:`skk-emacs-prepare-modeline-properties` で定義しています。

********************
ユーザガイダンス関連
********************

.. _display-japanese-message:

エラーなどの日本語表示
======================

標準では、エラー、メッセージ及びミニバッファでのプロンプトは、英語で表示されます。

.. el:defvar:: skk-japanese-message-and-error

   non-nil に設定すると、エラー、メッセージ及びミニバッファでのプロンプトを日本語
   で表示します。標準では nil です。

.. el:defvar:: skk-show-japanese-menu

   non-nil に設定すると、メニューバーを日本語で表示します。

.. el:defvar:: skk-version-codename-ja

   non-nil に設定すると、関数 :el:defun:`skk-version` を評価したときのコードネー
   ムを日本語で表示します。

.. _display-verbose-message:

冗長な案内メッセージの表示
==========================

.. el:defvar:: skk-verbose

   non-nil に設定すると、入力中／変換中に冗長なメッセージを表示します。

   .. code:: emacs-lisp

      (setq skk-verbose t)

▽モード
   ファンクションキー :kbd:`F1` 〜 :kbd:`F10` に割り当てられている機能を表示しま
   す。変数 :el:defvar:`skk-verbose` の設定と同時に変数 :el:defvar:`skk-j-mode-function-key-usage` を
   以下のように設定してみてください。

   .. code:: emacs-lisp

      (setq skk-j-mode-function-key-usage 'conversion)

   ▽モードにおいてキー入力が一定時間（標準では 1.5 秒）なされなかったとき、
   エコーエリアに以下のようなメッセージが表示されます。

   .. code:: text

      -------------------- Echo Area --------------------
      [F5]単漢字 [F6]無変換 [F7]カタカナ [F8]半角カナ [F9]全角ローマ [F10]ローマ
      -------------------- Echo Area --------------------

   この案内に従ってファンクションキーを押すことで、一時的に単漢字変換やカタカナ変
   換を行うことができます。

▼モード
   Wikipedia アノテーション機能の使い方をメッセージで案内します。
   変数 :el:defvar:`skk-verbose` の設定と同時に変数 :el:defvar:`skk-show-annotation` を
   non-nil に設定してみてください。

   .. code:: emacs-lisp

      (setq skk-show-annotation t)

   ▼モードにおいてキー入力が一定時間 (標準では 1.5 秒) なされなかったとき、
   エコーエリアに以下のようなメッセージが表示されます。

   .. code:: text

      -------------------- Echo Area --------------------
      どれを参照?[C-1 C-i]ja.wikipedia [C-2 C-i]en.wiktionary
      [C-3 C-i]simple.wikipedia [C-4 C-i]en.wikipedia [C-5 C-i]ja.wiktionary
      -------------------- Echo Area --------------------

   この案内に従って、例えば :kbd:`C-1 C-i` を打鍵すると、日本語 Wikipedia
   の該当記事を調べて、あればその一部をアノテーションとして表示します。

   一方、現在の変換候補に対するアノテーションが既に表示されているときは、
   以下のメッセージが上記のものと交互に表示されます。

   .. code:: text

      -------------------- Echo Area --------------------
      {アノテーション}[C-w]コピー [C-o]URLブラウズ [C-i]標準設定のソースを参照
      -------------------- Echo Area --------------------

   この案内に従って :kbd:`C-w` を打鍵すれば、アノテーションの全文を kill
   ring に 保存して利用することができます。また、キー :kbd:`C-o`
   を押した場合には、もし 現在のアノテーションが Wikipedia
   アノテーションであればその出典となる Wikipedia/Wiktionary
   のページをウェブブラウザで表示します。

.. el:defvar:: skk-verbose-wait

   冗長なメッセージを表示するまでの待ち時間（秒）。標準は 1.5 秒です。

.. el:defvar:: skk-verbose-message-interval

   冗長なメッセージが複数ある場合の１メッセージあたり表示時間を秒で指定す
   る。標準は 5.0 秒です。この時間が経過したら表示を次の冗長なメッセージ
   に切り替えます。

.. el:defface:: skk-verbose-intention-face

   「どれを参照?」と「アノテーション」に適用するフェイスです。

.. el:defface:: skk-verbose-kbd-face

   ``[F5]`` や ``[C-1 C-i]`` に適用するフェイスです。

*************
I-search 関連
*************

起動時の入力モードの指定
========================

.. el:defvar:: skk-isearch-start-mode

   インクリメンタル・サーチを起動したときの入力モードをこの変数で指定できます。
   以下のいずれかのシンボルを指定できますが、変数 :el:defvar:`skk-isearch-use-previous-mode` の
   設定が優先されます。

   .. list-table::

      * - 指定できるシンボル
        - インクリメンタル・サーチを起動したときの入力モード
      * - nil
        - | カレントバッファで SKK モードが起動されていれば、そのモードを。
          | 起動されていなければアスキーモード。
      * - シンボル 'hiragana
        - かなモード
      * - シンボル 'jisx0208-latin
        - 全英モード
      * - シンボル 'latin
        - アスキーモード

.. el:defvar:: skk-isearch-use-previous-mode

   non-nil であれば、次のインクリメンタル・サーチ起動時の入力モードは、前回のイン
   クリメンタル・サーチでの入力モードになります。
   nil であれば、変数 :el:defvar:`skk-isearch-start-mode` の設定が優先されます。

間に空白等を含む文字列の検索
============================

「検索」という文字列をインクリメンタル・サーチにより検索する場合に、バッファが以
下のような状態になっていることがあります。

.. code:: text

   -------- Buffer: foo --------
   この行末から始まる文字列を検
   索して下さい。
   -------- Buffer: foo --------

このような場合のために、Emacs は正規表現によるインクリメンタル・サーチを提供して
います。DDSKK はこの正規表現によるインクリメンタル・サーチにも対応しているため、
空白や改行を含んだ検索も可能です。

.. el:define-key:: M-x isearch-forward-regexp

   前方への正規表現によるインクリメンタル・サーチ。:kbd:`C-u C-s` または
   :kbd:`M-C-s` で起動します。

.. el:define-key:: M-x isearch-backward-regexp

   後方への正規表現によるインクリメンタル・サーチ。 :kbd:`C-u C-r` または
   :kbd:`M-C-r` で起動します。

.. el:defvar:: skk-isearch-whitespace-regexp

   この変数の値は正規表現です。この正規表現にマッチする要素は「正規表現に
   よるインクリメンタル・サーチにおいては、単語を区切る要素ではない」と判
   断されます。この変数の標準設定は以下のようになっています。

   .. code:: text

      "\\(\\s \\|[ \t\n\r\f]\\)*"

   この変数の値を変更することで、正規表現によるインクリメンタル・サーチを
   拡張することができます。例えば、電子メールの引用部分を検索する場合を考
   えます。

   .. code:: text

      > 引用部分も検
      > 索できる。

   上記のうち、「検索」という語は 2
   行に渡っている上、引用マークが挿入され ています。ここで

   .. code:: emacs-lisp

      (setq skk-isearch-whitespace-regexp "\\(\\s \\|[ \t\n\r\f<>|]\\)*")

と設定することにより、「検索」を検索できるようになります。

*****************
VIP/VIPERとの併用
*****************

VIPER については Info を参照してください。

:infonode:`VIPER Manual <(viper)Top>`

また、VIPER の前身である VIP にも対応します。

ただし、正式に対応しているバージョンは 3.5 のみです。これは Mule 2.3 に標準添付し
ます [#]_ 。

.. el:defvar:: skk-use-viper

   non-nil に設定すると、VIPER に対応します。

********************
picture-modeとの併用
********************

SKK モードを ``picture-mode`` において使用した場合は、以下のような問題点がありま
す。ただし、これらは ``picture-mode`` の問題なので、現在のところ DDSKK 側では対処
していません。

- SKK モードで全角文字を入力した場合に、 :kbd:`BS` で全角文字を消すことができませ
  ん。現状では、後方にある文字を消したい場合は、その文字にポイントを合わせ、 :kbd:`C-c C-d`
  で一文字ずつ消す必要があります。

- 関数 :el:defun:`picture-movement-up` や関数 :el:defun:`picture-movement-down` に
  より上下に全角文字を挿入した場合に、桁がずれる場合があります。

関数 :el:defun:`move-to-column-force` の中で使用されている関数 :el:defun:`move-to-column` の
引数として、全角文字を無視した桁数が与えられることがあり、そのときカーソル移動が
できないため、これらの問題が生じます。

.. rubric:: 脚注

.. [#] いわゆる半角カナ。以下、このマニュアルでは「半角カナ」と記述します

.. [#] 正確には、▽マークの次の位置にある文字列によって文字種を判別しているので、
       途中で文字種類の違う文字が混在していても無視されます。

.. [#] 全英文字とアスキー文字のトグルでの変換を行うこともできます。ただし、全英モ
       ードやアスキーモードでは :kbd:`Q` やその他の大文字により▽モードに入ること
       ができないので、かな←→カナのときと同様にトグル変換できるわけではありませ
       ん。かなモード／カナモードにおいて、既に入力された全英文字、アスキー文字に
       対してトグル変換をするような設計になっています。

.. [#] SKK abbrev モードでは ``is`` ⇒ 「インクリメンタル・サーチ」のような変換を
       行うことができます。他の変換と同様に :kbd:`SPC` を押すと変換モードに入って
       しまいますので、 SKK abbrev モードからアスキー文字を入力するのは、一語のみ
       の場合以外は不便です。

.. [#] `:kbd:C-q` は変数 :el:defvar:`skk-abbrev-mode-map` にて特別な動作をするよ
       うに定義されています。

.. [#] メニューバーが使用できる環境では、メニューバーを使ってこれらの一括変換コマ
       ンドを呼び出すことができます。ただし コマンド :command:`kakasi` がインスト
       ールされていない場合は コマンド :command:`kakasi` を利用する機能が灰色にな
       り使用できません。

.. [#] KAKASI - 漢字→かな（ローマ字）変換プログラム http://kakasi.namazu.org/

.. [#] 昭和29年12月9日付内閣告示第一号によれば、原則的に訓令式（日本式）を用いる
       かのように記載されていますが、今日一般的な記載方法は、むしろヘボン式である
       ようです。

.. [#] 細かい説明です。 :kbd:`TAB` を押す直前に▽モードで入力された文字列を X と
       呼ぶことにします。このとき、個人辞書の「送りなしエントリ」の中から
       「先頭が X と一致し」かつ「長さが X よりも長い見出し語」を検索して、そのよ
       うな語が該当すれば X の代わりに表示します。

.. [#] 同じ列に既に入力している文字列があったときにそれを参照して補完しようとする
       機能

.. [#] 現在は候補群の右側１カラムのフェイスが標準設定に戻る、という制約があります。

.. [#] ``p125`` という見出し語であれば、その数値部分である ``125`` が再変換の見出
       し語となります。

.. [#] :kbd:`SHIFT` キーを伴って数字を入力し始めることはできないので :kbd:`Q` ま
       たは :kbd:`/` で▽モードに入る必要があります。

.. [#] ``m`` や ``d`` などアスキー文字を見出し語として入力する場合は :kbd:`/` キー
       を最初に入力して SKK abbrev モードに入ってから入力する必要があります。

       :ref:`アスキー文字を見出し語とした変換 <conv-ascii-midasi>`

.. [#] 通常の単語では、改行を含むことが可能です。それは、評価するとその位置に改行
       を挿入するような実行変換プログラムに変換して辞書に書き込んでいるからです。

       :ref:`辞書の種類 <jisyo-variant>`

       しかし、実行変換されるプログラムを辞書登録する際にはこの機能を利用できない
       ため、改行を含むことができません。

.. [#] 変数 :el:defvar:`skk-search-prog-list` の設定をユーザが変更している場合は
       期待どおりに動作しない場合があります。その場合は変数 :el:defvar:`skk-search-prog-list` の
       設定に関数 :el:defun:`skk-search-katakana` の呼び出しがあることを確認して
       ください。またこの機能の設定は DDSKK 14.1 以前では異なります。詳しくはソー
       スに付属のドキュメント、設定例をご覧ください。

.. [#] 変数 :el:defvar:`skk-search-prog-list` の設定をユーザが変更している場合は
       期待どおりに動作しない場合があります。その場合は変数 :el:defvar:`skk-search-prog-list` の
       設定に関数 :el:defun:`skk-search-sagyo-henkaku` の呼び出しがあることを確認
       してください。またこの機能の設定は DDSKK 14.1 以前では異なります。詳しくは
       ソースに付属のドキュメント、設定例をご覧ください。

.. [#] 辞書が充実していれば、かな漢字変換で見出し語「へん」から「邊」や「邉」を求
       めることができます。もちろん、文字コードを指定して「邊」や「邉」を直接挿入
       することもできます。

.. [#] 変数 :el:defvar:`skk-use-kana-keyboard` が non-nil ならば無効である。

.. [#] 辞書登録モードの標準の確定、登録の動作は :ref:`辞書登録モード <jisyo-register-mode>`

.. [#] あくまでも「任意のキーで変換開始位置を指定する」ものであり、sticky-shift
       そのものではありません。したがって、アスキーモードや SKK abbrev モード、
       また SKK 以外でも sticky-shift を使いたい場合は、前述のような設定を併用す
       る必要があります。

.. [#] ファイル :file:`skk-hint.el` を併用する場合は変数 :el:defvar:`skk-hint-start-char` の
       標準設定も :kbd:`;` であるため、どちらかを別のキーに割り当てる必要があります。

       :ref:`候補の絞り込み <skk-hint>`

.. [#] :ref:`Q3-4 左手の小指を SHIFT で酷使したくありません。 <Q3-4>`

.. [#] ``buffer-undo-list`` に Emacs が挿入したアンドゥの境目の目印を取り除く方法
       でエミュレートしています。

.. [#] SKK abbrev モードでは、アスキー文字入力が Emacs 本来の関数 :el:defun:`self-insert-command` に
       より行われているので、エミュレーションのための内部変数である
       変数 :el:defvar:`skk-self-insert-non-undo-count` をインクリメントすることができず、
       アンドゥをエミュレートできません。しかも、カンマやピリオドを挿入した時点で、
       関数 :el:defun:`skk-abbrev-comma` や関数 :el:defun:`skk-abbrev-period` を使うことに
       なるので、本来のアンドゥの機能も損なってしまいます。

       ただし、現実問題として、元来 SKK abbrev モードは省略形としての見出し語を挿
       入するためのモードですから、長い見出し語を挿入することはあまりないと考えら
       れます。

.. [#] 20 は Emacs のソースファイルの一部であるファイル :file:`keyboard.c` に定め
       られたマジックナンバーと一致します。

.. [#] かなモードでの入力中、アスキーモードに移行して入力した場合などがこれにあた
       ります。

.. [#] :kbd:`C-j` を打鍵して明示的に確定した場合は勿論、「暗黙の確定」を行った場
       合も同様です。

.. [#] ▽マークからポイントの直前の文字までを見出し語とします。打鍵に入力された文
       字（「を」や「。」）は見出し語には含まれません。

.. [#] 正確には、印字可能な文字または :kbd:`RET` が入力されたときです。

.. [#] 変数 :el:defvar:`skk-kakutei-early` の機能と変数 :el:defvar:`skk-process-okuri-early` の
       機能を同時に有効にすることはできません。
       変数 :el:defvar:`skk-kakutei-early` の値を non-nil にする場合は変数 :el:defvar:`skk-process-okuri-early` の値を nil にする必要があります。

.. [#] 確定変換用辞書の見出し語の配列については、サイズが大きい場合は、共有辞書と
       同様、ソートして二分検索（バイナリサーチ）を行い、サイズが小さければ適当な
       配置で直線的検索（リニアサーチ）を行うことをお勧めします。次も参照してくだ
       さい。

         - :ref:`辞書検索のための関数 <jisyo-search-functions>`
         - :ref:`エントリの配列 <entries>`

.. [#] この機能は、変数 :el:defvar:`skk-process-okuri-early` の値を non-nil に設定した
       状態と共存できません。

       :ref:`送りあり変換の変換開始のタイミング <okuri-conv-start>`

.. [#] 実は変数 :el:defvar:`skk-henkan-okuri-strictly` の値は辞書バッファで参照さ
       れるので、ミニバッファのバッファローカル値を変更してもうまくいきません。将
       来のバージョンでは、これを改良し、辞書バッファでの動作に影響するユーザ変数
       をバッファローカル化できるようにする予定です。

.. [#] 「大く」などの候補は鬱陶しいが、すぐに単語登録に入ってしまうのも嫌な人にお
       すすめです。

.. [#] :ref:`送りあり変換の変換開始のタイミング <okuri-conv-start>`

.. [#] 実際には、普通の送りなし変換として最初は検索されます。個人辞書まで調べて候
       補が見つからないときは、その後、 :ref:`送り仮名の自動処理 <okurigana>` の
       検索に移ります。

.. [#] :ref:`送りありエントリのブロック形式 <okuri-block-format>`

.. [#] 長さ順にソートするのは、変換された部分がより長い候補を先順位として出力する
       ためです。

.. [#] 「該当の見出し語から切り捨てられた文字列」を送り仮名とみなして処理していま
       す。

.. [#] 専ら補完的に自動送り処理を利用するのであれば関数 :el:defun:`skk-okuri-search`
       を変数 :el:defvar:`skk-search-prog-list` の最後に設定するという方法もあります。

   :ref:`辞書の検索方法の設定 <search-jisyo>`

.. [#] 変数 :el:defvar:`skk-auto-okuri-process` の値を non-nil に設定している。

.. [#] :ref:`辞書の書式 <jisyo-format>`

.. [#] 「ふr」に対して「大量」（たいりょう）が関連語として保存されます。
       勿論、「ふr」に対する「雨」（あめ）の学習もまだ生きています。

.. [#] http://www.ruby-lang.org

.. [#] Ruby 2.4 以降を使用する場合は、DDSKK 16.2 以降に付属するファイル :file:`bayesian/bskk` を
       使用してください。

.. [#] 変数 :el:defvar:`skk-search-prog-list` に登録されている関数 :el:defun:`skk-search-extra-jisyo-files` が、
       変数 :el:defvar:`skk-extra-jisyo-file-list` の各要素を逐次処理します。

.. [#] 辞書バッファの文字コードの設定

.. [#] 検索領域の先頭ポイントと末尾ポイントの差

.. [#] あるかな文字をローマ字表現したときの１文字目を「ローマ字プレフィックス」と
       呼びます。

.. [#] - :ref:`送り仮名の自動処理 <okurigana>`
       - :ref:`送り仮名の厳密なマッチ <okuri-strictly>`
       - :ref:`送り仮名の優先的なマッチ <okuri-precedence>`

.. [#] ただし、変数 :el:defvar:`skk-process-okuri-early` の値が non-nil であれば、
       送り仮名を決定する前に変換を開始することになるので、送り仮名を明示的に入力
       していても個人辞書にはブロック形式は作られません。

.. [#] ソートする際には、見出し語を unsigned-char と見なします。この順序は Emacs
       が関数 :el:defun:`string<` で文字列を比較するときの順序であり、UNIX のコマ
       ンド :command:`sort` での標準の順序とは異なります。Emacs の関数 :el:defun:`sort-lines` を用
       いればファイルをこの順序でソートすることができます。
       Emacs の関数 :el:defun:`sort-columns` は 内部的に UNIX のコマンド :command:`sort`
       を使っているので、辞書のソートには使えません。

.. [#] 正確に言えば、送りあり変換では ``skk-okuri-ari-min + 1`` の位置、送りなし
       変換では ``skk-okuri-nasi-min + 1`` の位置

.. [#] 前置引数 :kbd:`C-u` を伴って実行する :kbd:`C-u M-x skk-edit-private-jisyo`
       ことで、コーディングシステムを指定して個人辞書を開くことができます。

.. [#] 通常の使用の範囲では :kbd:`M-x skk-purge-from-jisyo` した場合、あるいは個
       人辞書をユーザが意図的に編集した場合、複数の Emacs で DDSKK を 使用した場
       合などに、個人辞書が小さくなることがあります。他の場合はバグの可能性があり
       ます。

.. [#] :el:defvar:`skk-jisyo` が既に壊れていても、変数 :el:defvar:`skk-backup-jisyo` が指し示すファ
       イルにそれ以前の個人辞書が残っている可能性があります。

.. [#] これは、個人辞書の最小ポイントに、常に最後に変換を行ったエントリを移動させ
       るために、エントリ数、候補数が全く増えていなくとも、確定により個人辞書が更
       新されているからです。

.. [#] これは、Emacs のファイル :file:`dabbrev.el` の機能との調和を考えての措置です。
       Dabbrev においては、現在のバッファと同じモードの他のバッファを検索して
       abbreviation の展開を行うように設定することができるのですが、仮に辞書バッ
       ファにおける変数 ``major-mode`` の値が ``fundamental-mode`` のままだとする
       と、 Dabbrev が辞書バッファを検索してしまう可能性があります。この措置によ
       って、そのような事態を回避しています。

.. [#] coding system は、 'euc-jp, 'shift_jis, 'junet な
       どのシンボルで表され、 :kbd:`M-x describe-coding-system` や
       :kbd:`M-x list-coding-systems` で 調べることができます。

.. [#] ``*`` の文字は変換時には表示されません。

.. [#] 変数 :el:defvar:`skk-lookup-search-agents` にセットして検索するようにしています。
       Lookup とは異なる設定をする場合、この変数の設定を変更すれば可能です。

.. [#] この設定は、変数 :el:defvar:`skk-treat-candidate-appearance-function` の値
       を上書きします。変数 :el:defvar:`skk-treat-candidate-appearance-function` を
       自分で設定する場合は変数 :el:defvar:`skk-annotation-lookup-lookup` には t ま
       たは nil を必要に応じて設定します。

.. [#] この設定は、変数 :el:defvar:`skk-treat-candidate-appearance-function` の値
       を上書きします。変数 :el:defvar:`skk-treat-candidate-appearance-function` を
       自分で設定したい場合は変数 :el:defvar:`skk-annotation-lookup-DictionaryServices` に
       は t または nil を必要に応じて設定します。

.. [#] 変数 :el:defvar:`skk-show-tooltip` が non-nil の場合、ツールティップで表示
       します。

.. [#] 変数 :el:defvar:`skk-annotation-other-sources` の標準の値は環境によって異
       なります。
       ``lookup.el`` と ``skk-lookup.el`` の設定が有効になっている場合は
       ``en.wiktionary`` は ４番目 (Apple macOS では５番目) になります。

.. [#] 大文字でも小文字でも構いません。なお、第１段階・第２段階ともに、メニューの
       キーを変更することができます。

       :ref:`候補の選択に用いるキー <cand-select-key>`

.. [#] リードオンリーなバッファでは :kbd:`M-x skk-display-code-for-char-at-point` を
       実行してください。

.. [#] 変数 :el:defvar:`skk-show-tooltip` が non-nil であればツールティップで表示します。
       変数 :el:defvar:`skk-show-candidates-always-pop-to-buffer` が non-nil で あれば
       other-window に表示します。変数 :el:defvar:`skk-show-tooltip` が優先します。

.. [#] 関数 :el:defun:`skk-lookup-search` がファイル :file:`skk-autoloads.el` に
       追加されます。

.. [#] 変数 :el:defvar:`skk-lookup-search-agents` にセットして検索するようにして
       います。Lookup とは異なる設定をする場合、この変数の設定を変更すれば可能です。

.. [#] edict 辞書ファイル :file:`SKK-JISYO.edict` があれば、例えば、

   .. code:: emacs-lisp

       (setq skk-search-prog-list
             (append skk-search-prog-list
                     (list
                      '(skk-search-jisyo-file "/your-path/SKK-JISYO.edict" 0 t))))

   のように設定することにより、 edict 辞書を使用できます。

.. [#] ただし、変数 :el:defvar:`frame-width` が不足する場合は、候補バッファに表示
       します。

.. [#] 以前のバージョンではテキスト属性 (text property) を使用していました。
       オーバーレイ属性はテキスト属性と異なり、テキストの一部とは見なされません。
       そのため、テキストのコピーの際にオーバーレイ属性は保持されません。
       その他にも、オーバーレイの移動やその属性の変更はバッファの変更とは見なされ
       ないこと、オーバーレイの変更はバッファのアンドゥリストに記録されないこと、
       などが特徴として挙げられます。

.. [#] Emacs 標準 では ``default``, ``modeline``, ``region``, ``secondary-selection``,
       ``highlight``, ``underline``, ``bold``, ``italic``, ``bold-italic`` があります。

.. [#] 変数 :el:defvar:`window-system` が nil の場合は、これらフェイスは未定義と
       なります。

.. [#] ちなみに、VIP 3.5 の作者は、SKK の原作者でもある佐藤雅彦氏（京都大学名誉教
       授）です。VIP 3.5 の発展版である VIPER は現在もメンテナンスされています。
       GNU Emacs 19, 20 には、VIP 、VIPER とも標準添付します。
