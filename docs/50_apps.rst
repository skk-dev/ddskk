==============
便利な応用機能
==============

ファイル構成
============

SKK の基本的な機能は :file:`skk.el` に収められています。一方、DDSKK で応用機能を
提供するプログラムのほとんどは :file:`skk.el` とは別のファイルに収めています。
これらは、必要に応じてオートロードするように設計されています。各応用機能の概略と
該当のファイル名について説明します。

また、DDSKK の変数は :file:`skk-vars.el` に集約されていますので、カスタマイズした
い場合などには、このファイルを見ると参考になるかもしれません。

.. list-table::
   
   * - FILE
     - 説明
   * - ccc.el
     - buffer local cursor color control library
   * - cdb.el
     - constant database (cdb) reader for Emacs Lisp
   * - context-skk.el
     - | 編集の文脈に応じて自動的に skk のモードを切り替えたり、
       | SKK の各種設定を変更する機能を提供します。
   * - ddskk-pkg.el
     - Multi-file Packages in GNU Emacs Lisp Reference Manual
   * - skk-abbrev.el
     - SKK abbrev モードの機能を提供するプログラムを集めたファイル
   * - skk-act.el
     - dvorak 配列での拡張ローマ字入力 ACT を SKK で使うための設定
   * - skk-annotation.el
     - 個人辞書に付けたアノテーション（注釈）を活用するプログラムを集めたファイル
   * - skk-auto.el
     - 送り仮名の自動処理を行うプログラムを集めたファイル
   * - skk-autoloads.el
     - | :command:`make` 時に自動生成されるファイル。
       | オートロードの設定のほか ``register-input-method`` も行う。
       | XEmacs で DDSKK をパッケージとしてインストールした場合は
       | :file:`auto-autoloads.el` というファイルがこれに相当します。
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
     - プログラム実行変換を行うプログラムを集めたファイル
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
     - 文字コードまたはメニューによる文字入力を行うプログラムを集めたファイル
   * - skk-leim.el
     - | LEIM 関連プログラムファイル
       | DDSKK を Emacs の input method として利用できるようにします
   * - skk-look.el
     - :command:`look` コマンドとのインターフェイスプログラムを集めたファイル
   * - skk-lookup.el
     - Lookup で検索できる辞書を使って単語の候補を出力するプログラム
   * - skk-macs.el
     - 他のファイルで共通して使用するマクロなどを中心にまとめたファイル
   * - skk-num.el
     - 数値変換を行うプログラムを集めたファイル
   * - skk-search-web.el
     - | Google CGI API for Japanese Input を利用したかな漢字変換
       | 辞書登録モードに Google サジェストを初期表示する
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
     - SKK を使って単漢字変換を行うプログラム
   * - skk-tut.el
     - SKK チュートリアルプログラム
   * - skk-tutcode.el
     - SKK で TUT-code 入力を実現します
   * - skk-vars.el
     - DDSKK で使われる変数を集約したファイル
   * - skk-version.el
     - DDSKK のバージョン情報を提供するプログラムファイル
   * - skk-viper.el
     - VIPER インターフェイスプログラムを集めたファイル
   * - skk-xemacs.el
     - | XEmacs の拡張機能を利用するプログラムを集めたファイル
       | インジケータのカラー化や画像表示、ツールティップ利用など
   * - tar-util.el
     - utility for tar archive

ユーザオプションの設定方法
==========================

DDSKK のカスタマイズは、 :file:`~/.emacs.d/init.el` あるいは :file:`~/.skk` に記述します。
また、各ファイルの提供するフックも利用します。上記のファイルやフックを利用した設
定がいつ有効になるのか、という点についてここで説明します。

設定ファイル
------------

.. index::
   pair: File; ~/.emacs.d/init.el
   pair: File; ~/.xemacs/init.el

~/.emacs.d/init.el, ~/.xemacs/init.el
  Emacs を起動したときに一度だけ読み込まれます。

  [[info:emacs#Init File][The Emacs Initialization File in GNU Emacs Manual]]

  このマニュアルでは :file:`~/.emacs.d/init.el` という記述で統一しています。

.. index::
   pair: File; ~/.skk
   pair: Function; convert-standard-filename

~/.skk
  DDSKK を起動した最初の一度だけ読み込まれます。ファイル名の標準設定は OS の種類
  により異なりますが、実際は Emacs の関数 ``convert-standard-filename`` により加
  工されます。

  :file:`~/.skk` のファイル名は変数 ``skk-init-file`` で変更することができます。
  また、DDSKK にはこのファイルを自動的にバイトコンパイルする機能があります。

.. index::
   pair: Variable; skk-user-directory

skk-user-directory
  DDSKK は :file:`~/.skk` や :file:`~/.skk-jisyo` といった複数のファイルを使用し
  ます。これらのファイルをひとつのディレクトリにまとめて置きたい場合は、
  変数 ``skk-user-directory`` にそのディレクトリ名を設定します。
  標準設定は ``nil`` です。

  この変数は :file:`~/.emacs.d/init.el` で設定してください。
  DDSKK 起動時に ``skk-user-directory`` が指すディレクトリが存在しない場合は、自
  動的に作られます。

  .. code:: emacs-lisp

    (setq skk-user-directory "~/.ddskk")  

  この変数を設定した場合（例えば上記 :file:`~/.ddskk` ）、以下に挙げる各変数の標準設
  定値が変更されます。

  .. list-table::

     * - 影響を受ける変数
       - 標準の値
       - 変数 skk-user-directory を設定した場合の値  
     * - skk-init-file
       - ~/.skk
       - ~/.ddskk/init
     * - skk-jisyo
       - ~/.skk-jisyo
       - ~/.ddskk/jisyo
     * - skk-backup-jisyo
       - ~/.skk-jisyo.BAK
       - ~/.ddskk/jisyo.bak
     * - skk-emacs-id-file
       - ~/.skk-emacs-id
       - ~/.ddskk/emacs-id
     * - skk-record-file
       - ~/.skk-record
       - ~/.ddskk/record
     * - skk-study-file
       - ~/.skk-study
       - ~/.ddskk/study
     * - skk-study-backup-file
       - ~/.skk-study.BAK
       - ~/.ddskk/study.bak
     * - skk-bayesian-history-file
       - ~/.skk-bayesian
       - ~/.ddskk/bayesian
     * - skk-bayesian-corpus-file
       - ~/.skk-corpus
       - ~/.ddskk/corpus

  なお、 変数 ``skk-user-directory`` を設定した場合でも、上記「影響を受ける変数」
  を個別に設定している場合は、その個別の設定が優先されます。

skk-init-file の自動コンパイル
------------------------------

ここでは、「DDSKK の設定ファイル」を ``el`` と、「DDSKK の設定ファイルをバイトコ
ンパイルしたファイル」を ``elc`` とそれぞれ呼びます。

変数 ``skk-byte-compile-init-file`` を適切に設定することによって、DDSKK の起動時
に自動的に ``el`` をバイトコンパイルすることができます。

.. list-table::

   * - skk-byte-compile-init-file の値
     - DDSKK の起動時
   * - non-nil
     - | 「 ``elc`` が存在しない」又は「 ``elc`` よりも ``el`` が新しい」ときは、
       | ``el`` をバイトコンパイルした ``elc`` を生成します。
   * - nil
     - ``elc`` よりも ``el`` が新しいときは、 ``elc`` を消去します。

.. index::
   pair: Variable; skk-byte-compile-init-file

skk-byte-compile-init-file
  設定ファイルの自動バイトコンパイル機能を有効にしたい場合は、
  :file:`~/.emacs.d/init.el` に

  .. code:: emacs-lisp

     (setq skk-byte-compile-init-file t)

  と記述します。この変数は :file:`~/.skk` が読み込まれる前に調べられるた
  め、 :file:`~/.skk` に上記の設定を記述しても無効です。

フック
------

.. index::
   pair: Variable; skk-mode-hook

skk-mode-hook
  :kbd:`C-x C-j` と入力して SKK モードに入る度に呼ばれます。主にバッファローカルの設
  定などを行います。

.. index::
   pair: Variable; skk-auto-fill-mode-hook

skk-auto-fill-mode-hook
  :kbd:`C-x j` と入力してオートフィルモード付きで SKK モードに入る度に呼ばれます。
  主にバッファローカルの設定などを行います。

.. index::
   pair: Variable; skk-load-hook

skk-load-hook
  :file:`skk.el` の読み込みを完了した時点で呼ばれます。 :file:`~/.skk` は SKK モ
  ードを起動しなければ読み込まれないのに対し、このフックは :file:`skk.el` を読み
  込んだら SKK モードを起動しなくとも呼ばれます。

各ファイルの読み込みが完了した直後に呼ばれるフックは以下のとおり。

.. list-table::

   * - ファイル
     - フック
   * - skk-act.el
     - skk-act-load-hook
   * - skk-auto.el
     - skk-auto-load-hook
   * - skk-azik.el
     - skk-azik-load-hook
   * - skk-comp.el
     - skk-comp-load-hook
   * - skk-gadget.el
     - skk-gadget-load-hook
   * - skk-kakasi.el
     - skk-kakasi-load-hook
   * - skk-kcode.el
     - skk-kcode-load-hook
   * - skk-num.el
     - skk-num-load-hook
   * - skk-server.el
     - skk-server-load-hook

.. index::
   pair: Function; eval-after-load

``load-hook`` が提供されていないプログラムであっても、ロード完了後に何らか
の設定を行いたい場合は、関数 ``eval-after-load`` を使用します。

.. code:: emacs-lisp

   (eval-after-load "skk-look"
     '(...)
   )

Customize による設定変更
------------------------

Emacs 標準の Customize 機能を使って SKK を設定することもできます。
ただし、Customize での設定は :file:`~/.emacs.d/init.el` での設定と同様に、
:file:`/.skk` による設定で上書きされてしまいますので注意してください。

.. index::
   pair: Key; M-x customize-group
   pair: Key; M-x skk-emacs-customize

:kbd:`M-x customize-group` を実行すると skk の設定を対話的に変更することができます。
ミニバッファに ``Customize group:`` とプロンプトが表示されます。

.. code:: text

   ------ Minibuffer -------
   Customize group: (default emacs) *
   ------ Minibuffer -------

ここで ``skk`` と答えると、SKK グループの画面へ展開します。
:kbd:`M-x skk-emacs-customize` と実行するのも同様です。

あるいは、モードラインの SKK インジケータをマウスの右ボタン（第３ボタン）でクリッ
クすると表示されるメニューから「SKK をカスタマイズ」を選んでも同じ画面となります。

カスタマイズの使い方は Info ([[info:emacs#Easy Customization][Easy Customization in GNU Emacs Manual]].) を参照してください。

skk で設定できる変数の中には、まだこのマニュアルで解説されていないものもあります。
Customize を使うと、それらについても知ることができます。

skk-customize による設定変更
----------------------------

.. index::
   pair: Key; M-x skk-customize

:kbd:`M-x skk-customize`
  前述の「Emacs 標準の Customize 機能 M-x customize-group 」による設定が複雑
  すぎると感じるユーザのために、簡易版として :kbd:`M-x skk-customize` を用意していま
  す。これは SKK グループのユーザオプションのうち、よく使うものだけ抜粋して設定で
  きるようにしたものです。

  これは、モードラインの SKK インジケータをマウスの右ボタン（第３ボタン）でクリッ
  クして表示されるメニューから「SKK をカスタマイズ（簡易版）」を選んで呼び出すこ
  ともできます。

カタカナ、英字入力の便法
========================

この節では、カタカナや全英文字を入力するための、便利な方法を説明します。

.. _input-katakana:

かなモードからカタカナを入力
----------------------------

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

領域を対象としたコマンド ([[領域の操作][領域の操作]].) でも「かな←→カナ」のトグ
ル変換を行うことができます。

.. _input-zenei:

全英文字の入力
--------------

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

のような辞書エントリを持つ必要はありません。なぜなら、辞書を参照せずにアスキー文
字を１文字ずつ全英文字に変換しているからです。

領域の操作
----------

以下のコマンドを :kbd:`M-x` により呼ぶことで [#]_ 、領域内の文字列を一括変換する
ことができます。

.. index::
   pair: Key; M-x skk-hiragana-region

:kbd:`M-x skk-hiragana-region`
  カタカナ を ひらがな へ変換

.. index::
   pair: Key; M-x skk-katakana-region

:kbd:`M-x skk-katakana-region`
  ひらがな を カタカナ へ変換

.. index::
   pair: Key; M-x skk-latin-region

:kbd:`M-x skk-latin-region`
  全英文字 を アスキー文字 へ変換

.. index::
   pair: Key; M-x skk-jisx0208-latin-region

:kbd:`M-x skk-jisx0208-latin-region`
  アスキー文字 を 全英文字 へ変換

.. index::
   keyword: 逆引き

以下に紹介する「漢字から読みを求めるコマンド」は、外部プログラム :command:`KAKASI` [#]_ が
必要です。 :command:`KAKASI` がインストールされていなければ使用することができません。

.. index::
   pair: Key; M-x skk-gyakubiki-region

:kbd:`M-x skk-gyakubiki-region`
  漢字をひらがなへ変換。具体的な変換例をあげると、

  .. code:: text

     漢字をひらがなへ変換。 → かんじをひらがなへへんかん。

  のようになります。引数を渡して :kbd:`C-u M-x skk-gyakubiki-region` のようにする
  と、複数の候補がある場合に ``{ }`` で囲って表示します。例えば

  .. code:: text

     中島 → {なかしま|なかじま}

  のようになります。

  送り仮名がある語は、送り仮名まで含めて領域に指定します（さもないと誤変換の原因
  となります）。 例えば「五月蝿い」について、送り仮名「い」を含めずにこのコマンド
  を実行すると「ごがつはえ」に変換されてしまいます。

.. index::
   pair: Key; M-x skk-gyakubiki-and-henkan

:kbd:`M-x skk-gyakubiki-and-henkan`
  領域の漢字をひらがなへ変換し、これで得たひらがなを見出し語として漢字変換を実行
  します。

.. index::
   pair: Key; M-x skk-gyakubiki-katakana-region

:kbd:`M-x skk-gyakubiki-katakana-region`
  漢字をカタカナへ変換。

  引数を渡して ``C-u M-x skk-gyakubiki-katakana-region`` のようにすると、複数の候
  補がある場合に ``{ }`` で囲って表示します。

.. index::
   pair: Key; M-x skk-hurigana-region

:kbd:`M-x skk-hurigana-region`
  漢字にふりがなを付ける。例えば、

  .. code:: text

     漢字の脇に → 漢字[かんじ]の脇[わき]に

  のようになります。引数を渡して :kbd:`C-u M-x skk-hurigana-region` のようにする
  と、複数の候補がある場合に ``{ }`` で囲って表示します。

.. index::
   pair: Key; M-x skk-hurigana-katakana-region

:kbd:`M-x skk-hurigana-katakana-region`
  漢字にカタカナのふりがなを付ける。

  引数を渡して :kbd:`C-u M-x skk-hurigana-katakana-region` のようにすると、複数の
  候補がある場合に ``{ }`` で囲って表示します。

.. index::
   pair: Key; M-x skk-romaji-region

:kbd:`M-x skk-romaji-region`
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
   pair: Variable; skk-gyakubiki-jisyo-list
   pair: 環境変数; KANWADICTPATH

skk-gyakubiki-jisyo-list
  関数 ``skk-gyakubiki-region`` は、コマンド :command:`kakasi` を呼び出しています。
  :command:`kakasi` には漢字をひらがなへ変換する機能があり、この変換には環境変
  数 ``KANWADICTPATH`` で指定されている辞書を利用しています。

  変数 ``skk-gyakubiki-jisyo-list`` を設定することによって :command:`kakasi` へ与
  える辞書を任意に追加することができます。以下のように設定して :command:`kakasi` へ
  個人辞書 ``skk-jisyo`` を与えることによって辞書登録モードで登録したばかりの単語
  も :command:`kakasi` による逆引き変換の対象とすることができます。

  .. code:: emacs-lisp

     (setq skk-gyakubiki-jisyo-list (list skk-jisyo))

.. `-*-' は sphinx で WARNING 出る

.. index::
   pair: Variable; skk-romaji-*-by-hepburn

skk-romaji-＊-by-hepburn
  この変数の値を ``nil`` に設定すると、コマンド ``skk-romaji-{region|message}`` に
  よるローマ字への変換様式に訓令式 [#]_ を用います。標準設定は ``t`` です。

  .. code:: text

     し → si

カタカナの見出し語
------------------

:kbd:`q` の打鍵でかなモード、カナモードを度々切り替えて入力を続けていると、カナモー
ドで誤って▼モードに入ってしまうことがあります。そのため、カナモードで▼モードに
入った場合は、まず見出し語をひらがなに変換してから辞書の検索に入るよう設計されて
います。なお、この場合の「送りあり変換」での送り仮名は、カタカナになります。

文脈に応じた自動モード切り替え
------------------------------

.. index::
   pair: File; context-skk.el
   pair: Key; M-x context-skk-mode

:file:`context-skk.el` は、編集中の文脈に応じて SKK の入力モードを自動的にアスキーモ
ードに切り替える等の機能を提供します。

:file:`context-skk.el` をロードするには :file:`~/.emacs.d/init.el` に

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

.. index::
   pair: Variable; context-skk-programming-mode

context-skk-programming-mode
  context-skk が「プログラミングモード」と見做すメジャーモード。

.. index::
   pair: Variable; context-skk-mode-off-message

context-skk-mode-off-message
  アスキーモードに切り替わった瞬間にエコーエリアに表示するメッセージ。

.. _completion:

補完
====

読みの前半だけを入力して :kbd:`TAB` を押せば残りを自動的に補ってくれる、これが補
完です。 Emacs ユーザにはおなじみの機能が DDSKK でも使えます。

よく使う長い語を効率良く入力するには、アルファベットの略語を登録する方法もありま
す。

:ref:`アスキー文字を見出し語とした変換 <conv-ascii-midasi>`

読みの補完
----------

.. index::
   pair: Key; TAB

▽モードで :kbd:`TAB` を押すと、見出し語（▽マークからポイントまでの文字列）に対
する補完 [#]_ が行われます。見出し語補完は、個人辞書のうち送りなしエントリに対し
て行われます。個人辞書に限っているのは、共有辞書では先頭の文字を共通にする見出し
語が多すぎて、望みの補完が行える確率が低いためです。

.. index::
   pair: Key; .
   pair: Key; ,

次の読みの候補を表示するには :kbd:`.` （ピリオド）を、戻る時には :kbd:`,` （コンマ）を
押します。その読みで別の語を出すには、いつものように :kbd:`SPC` を押します。

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

[辞書の書式]

いったん :kbd:`SPC` を入力して▼モードに入ると、以後は見出し語補完は行われません。

.. index::
   pair: Key; C-u TAB

また、 :kbd:`.` の代わりに :kbd:`C-u TAB` を入力すると、現在の候補に対して補完を
します。上の例では「さ」に対し「さとう」が補完された時に :kbd:`C-u TAB` を押すと、
以後の補完は「さとう」を含む語（例えば「さとうせんせい」など）について行われます。

.. index::
   pair: Variable; skk-completion-prog-list

skk-completion-prog-list
  補完関数、補完対象の辞書を決定するためのリスト。標準設定は以下のとおり。

  .. code:: emacs-lisp

     '((skk-comp-by-history)
       (skk-comp-from-jisyo skk-jisyo)
       (skk-look-completion))

.. index::
   pair: Variable; skk-comp-circulate

skk-comp-circulate
  :kbd:`.` （ピリオド）で次の見出し語候補を、 :kbd:`,` （コンマ）で前の見出し語候
  補を表示するところ、候補が尽きていれば標準設定 ``nil`` では「○○で補完すべき見
  出し語は他にありません」とエコーエリアに表示して動作が止まります。
  この変数が ``non-nil`` であれば当初の見出し語を再び表示して見出し語補完を再開し
  ます。

.. index::
   pair: Variable; skk-try-completion-char

skk-try-completion-char
  見出し語補完を開始するキーキャラクタです。標準設定は :kbd:`TAB` です。

.. index::
   pair: Variable; skk-next-completion-char

skk-next-completion-char
  次の見出し語候補へ移るキーキャラクタです。標準設定はピリオド :kbd:`.` です。

.. index::
   pair: Variable; skk-previous-completion-char

skk-previous-completion-char
  前の見出し語候補へ戻るキーキャラクタです。標準設定はコンマ :kbd:`,` です。

.. index::
   pair: Key; backtab
   pair: Key; SHIFT TAB

skk-previous-completion-use-backtab
  ``Non-nil`` であれば、前の見出し語候補へ戻る動作を :kbd:`SHIFT + TAB` でも可能
  とします。標準設定は ``t`` です。この機能の有効化／無効化の切り替えは、
  ファイル :file:`~/.skk` を書き換えて Emacs を再起動してください。

.. index::
   pair: Variable; skk-previous-completion-backtab-key

skk-previous-completion-backtab-key
  :kbd:`SHIFT + TAB` が発行する key event です。Emacs の種類／実行環境によって異
  なります。

.. index::
   pair: Function; skk-comp-lisp-symbol

skk-comp-lisp-symbol &optional PREDICATE
  この関数をリスト ``skk-completion-prog-list`` へ追加すると、Lisp symbol 名の補
  完を行います。

  .. code:: emacs-lisp

     (add-to-list 'skk-completion-prog-list
                  '(skk-comp-lisp-symbol) t)

補完しながら変換
----------------

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

.. index::
   pair: Variable; skk-start-henkan-with-completion-char

skk-start-henkan-with-completion-char
  標準設定は :kbd:`M-SPC` です。

動的補完
--------

▽モードでは :kbd:`TAB` を押さなくとも、文字を入力する都度、自動的に見出し語補完
の読みを表示させる事ができます。この機能を以下「動的補完」と呼びます。
類似の機能としては、ウェブブラウザの URL の入力や、Microsoft Excel のセル入力の自
動補完 [#]_ をイメージすると分かりやすいかも知れません。動的補完も、個人辞書の送
りなしエントリに対してのみ行なわれます。

動的補完を利用するには :file:`~/.skk` に次の式を書きましょう。

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

.. index::
   pair: Variable; skk-dcomp-activate

skk-dcomp-activate
  この変数の値が ``Non-nil`` であれば、カーソル位置に関わらず常に動的補完が有効と
  なります。値がシンボル ``eolp`` であれば、カーソルが行末にあるときに限って動的
  補完が有効となります。値が ``nil`` であれば、動的補完機能は無効となります。

.. index::
   pair: Variable; skk-dcomp-face

skk-dcomp-face
  この変数の値はフェイスであり、このフェイスによって動的に補完された部分が装飾さ
  れます。標準は DarkKhaki です。

.. index::
   pair: Variable; skk-dcomp-multiple-activate

skk-dcomp-multiple-activate
  *XEmacs では動作しません。*

  ``Non-nil`` であれば、動的補完の候補をインラインに複数表示 [#]_ します。

  .. code:: text

     ---------------- Buffer: foo ------------------
     ▽ほ*んとう
       ほんとう
       ほかん
       ほっかいどう
       ほうほう
       :
     ---------------- Buffer: foo ------------------

  候補の選択には :kbd:`TAB` 又は :kbd:`SHIFT + TAB` を押します。
  また、普通の補完 ([[読みの補完][読みの補完]].) と同様に :kbd:`.` （ピリオド）
  と :kbd:`,` （コンマ）も利用できます。

.. index::
   pair: Variable; skk-dcomp-multiple-rows

skk-dcomp-multiple-rows
  動的補完の候補を複数表示する場合の表示行数。標準は 7。

.. index::
   pair: Variable; skk-dcomp-multiple-face

skk-dcomp-multiple-face
  動的補完の複数表示群のフェイス。上記例では「ほ」のフェイス。

.. index::
   pair: Variable; skk-dcomp-multiple-trailing-face

skk-dcomp-multiple-trailing-face
  動的補完の複数表示群の補完部分のフェイス。上記例では「んとう」、「かん」
  「っかいどう」、「うほう」のフェイス。

.. index::
   pair: Variable; skk-dcomp-multiple-selected-face

skk-dcomp-multiple-selected-face
  動的補完の複数表示群の選択対象のフェイス。上記例では :kbd:`TAB` を押すたびに
  「ほんとう」、「ほかん」、「ほっかいどう」と選択位置が移ります。その現在選択位
  置に適用するフェイスです。

便利な変換、その他の変換
========================

単漢字変換
----------

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
^^^^^^^^^^^^^^

標準設定の検索キーは :kbd:`@` です。DDSKK の標準設定ではキー :kbd:`@` は
関数 ``skk-today`` の実行に割り当てられていますが、DDSKK 14.2 からは特段の
設定なしに▽モードで :kbd:`@` の打鍵が可能となりました。

.. index::
   pair: Variable; skk-tankan-search-key

skk-tankan-search-key
  単漢字変換の検索キー。以下は、検索キーを :kbd:`!` へと変更する例です。

  .. code:: emacs-lisp

     (setq skk-tankan-search-key ?!)

辞書の設定
^^^^^^^^^^

DDSKK 14.2 からは標準で変数 ``skk-search-prog-list`` に ``skk-tankan-search`` が
含まれています。DDSKK 14.1 を利用の方、ご自身で ``skk-search-prog-list`` を設定す
る方は以下の解説を参考にしてください。

:file:`skk-tankan.el` には、漢字の部首とその中での画数のデータのみが入っています。
読みのデータは、普通の辞書ファイルを使います。

単漢字変換の辞書の設定は、変数 ``skk-search-prog-list`` に以下の形式で要素を追加
します。

.. code:: emacs-lisp

   (skk-tankan-search 'function . args)

*確定変換* を併用する場合は、 ``skk-search-prog-list`` の先頭の要素は
``skk-search-kakutei-jisyo-file`` でなければいけませんので、
``skk-search-prog-list`` の２番目の要素に ``skk-tankan-search`` を追加します。

.. code:: emacs-lisp

   ;; skk-search-prog-list の２番目の要素に skk-tankan-search を追加する
   (setq skk-search-prog-list
         (cons (car skk-search-prog-list)
               (cons '(skk-tankan-search 'skk-search-jisyo-file
                                         skk-large-jisyo 10000)
                     (cdr skk-search-prog-list))))

なお、確定変換を使用しない場合は、 ``skk-search-prog-list`` の要素の先頭
が ``skk-tankan-search`` でも大丈夫です。

.. code:: emacs-lisp

   (add-to-list 'skk-search-prog-list
                '(skk-tankan-search 'skk-search-jisyo-file
                                    skk-large-jisyo 10000))

:ref:`辞書の検索方法の設定 <search-jisyo>`

総画数による単漢字変換
^^^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^

▽モードで :kbd:`@` を２つ重ねて変換を開始すると、部首による単漢字変換が
できます。 :kbd:`M-x skk-tankan` でも可能です。

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

.. index::
   pair: Variable; skk-tankan-face

skk-tankan-face
   :kbd:`M-x skk-tankan` を実行したときに表示される「単漢字バッファ」で使用するフ
   ェイスです。

.. index::
   pair: Variable; skk-tankan-radical-name-face

skk-tankan-radical-name-face
   部首の読みに適用するフェイスです。

部首の読みによる単漢字変換
^^^^^^^^^^^^^^^^^^^^^^^^^^

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

候補の絞り込み
--------------

``skk-hint.el`` は、２つの読みの積集合みたいなものを取ることによって候補の絞り込
みを行うプログラムです。インストールは :file:`~/.skk` に以下を記入します。

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

``skk-hint.el`` は、２つの読みの厳密な積集合を取っているわけではなく、通常の変換
候補のなかでヒントとして与えられた読みを含んだ漢字を持つものに候補を絞ります。
この実例として「感動」と「感圧」を挙げます。

.. code:: text

    K a n d o u ; k a n n a t u

      ----- Buffer: foo -----
      ▽かんどうかんあつ
      ----- Buffer: foo -----

    SPC

      ----- Buffer: foo -----
      ▼感動
      ----- Buffer: foo -----

``skk-hint.el`` は単漢字の候補がたくさんある場合に、そこから候補を絞りこむ手段と
しても非常に有効です。例えば

.. code:: text

    ▽わ*

を変換すると、輪、環、話、和、羽、… と大量に候補が出てきます。この中から「和」を
選びたいとします。普通に変換していてもそのうち「和」が表示されますが、
これを ``W a ; h e i w a`` と入力し変換すると、「▼へいわ」の候補で ある「平和」
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

.. index::
   pair: Variable; skk-hint-start-char

skk-hint-start-char
   ヒント変換を開始するキーを character で指定します。

接頭辞・接尾辞
--------------

接頭辞 (prefix)、接尾辞 (suffix) の入力のために特別な方法が用意されています。
たとえば、「し」の候補は沢山あり、「し」から「氏」を変換するのは、そのままでは効
率が悪いです。接尾辞の「し」ならば、「氏」や「市」が優先されるでしょう。

接頭辞・接尾辞は、辞書の中では ``>`` などで示されます。

.. code:: text

    >し /氏/

というエントリがあるとき、「小林氏」を接尾辞入力を用いて、以下のように入力するこ
とができます。

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

というエントリがあるとき、「超大型」を接頭辞入力を用いて、以下のように入力するこ
とができます。

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

.. index::
   pair: Variable; skk-special-midashi-char-list

skk-special-midashi-char-list
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
--------

DDSKK は **数字を含む見出し語** を様々な候補に変換することができます。例えば、見
出し語「だい12かい」を変換すると「第１２回」、「第一二回」、「第十二回」といった
候補を挙げます。

この節では、このような候補を辞書に登録する方法を説明します。基本は、数字の部分を
``#`` で置き替えることです。辞書 :file:`SKK-JISYO.L` のエントリーから具体例を見て
みましょう。

.. code:: text

    だい#かい /第#1回/第#0回/第#2回/第#3回/第 #0 回/

「だい12かい」のような数字を含む見出し語を変換した場合、見出し語の中の数字の部分
は自動的に ``#`` に置き換えられますので、辞書エントリーの左辺（つまり見出し語）で
ある ``だい#かい`` にマッチします。

辞書エントリーの右辺の ``#1`` 、 ``#2`` などは「どのように数字を加工するか」のタ
イプを表します。以下、各タイプについて説明します。

-  ``#0``

   無変換。入力されたアスキー文字をそのまま出力します。例えば、「第12回」のような
   変換を得るために使います。

-  ``#1``

   全角文字の数字。 ``12`` を「１２」に変換します。

-  ``#2``

   漢数字で位取りあり。 ``1024`` を「一〇二四」に変換します。

-  ``#3``

   漢数字で位取りなし。 ``1024`` を「千二十四」に変換します。

-  ``#4``

   数値再変換。見出し語中の数字そのもの [#]_ をキーとして辞書を再検索し、 ``#4`` の
   部分を再検索の結果の文字列で入れ替えます。これについては後で例を挙げて説明しま
   す。

-  ``#5``

   小切手や手形の金額記入の際用いられる表記で変換します。例えば、 ``1995`` を
   「壱阡九百九拾伍」に変換します。これを大字と言います。

-  ``#8``

   桁区切り。 ``1234567`` を ``1,234,567`` に変換します。

-  ``#9``

   将棋の棋譜の入力用。「全角数字＋漢数字」に変換します。これについては後で例を挙
   げて説明します。

以下にいくつか例を示します。辞書に

.. code:: text

    # /#3/

というエントリがあるときに、 ``Q 1 0 0 2 0 0 3 0 0 4 0 0 5 0 0 SPC`` または
``/ 1 0 0 2 0 0 3 0 0 4 0 0 5 0 0 SPC`` とキー入力 [#]_ すれば「百兆二千三億四十
万五百」と変換されます。

辞書に

.. code:: text

    #m#d /#0月#0日/

というエントリがあるときに ``/ 2 m 2 5 d SPC`` と入力  [#]_ すれば「2月25日」と変
換されます。

辞書に

.. code:: text

    #kin /#9金/

というエントリがあるときに ``/ 3 4 k i n SPC`` と入力すれば「３四金」と変換されま
す。

辞書に

.. code:: text

    p# /#4/
    125 /東京都葛飾区/

というエントリがあるときに ``/ p 1 2 5 SPC`` と入力すれば、見出し語 ``p125`` の候
補が ``#4`` なので、見出し語の数字部分の ``125`` に対し辞書が再検索され「東京都葛
飾区」と変換されます。

最後に、実際に登録する例をひとつ挙げます。「２月２５日」を得るために、
``Q 2 g a t u 2 5 n i t i SPC`` とキー入力したときに、辞書に見出し語

.. code:: text

    #がつ#にち /#1月#1日/

がないときは、辞書登録モードのプロンプトは ``「#がつ#にち」`` となります。 全角数
字のタイプは ``#1`` なので ``「#1月#1日」`` をミニバッファで作り登録し ます。

タイプを覚えている必要はありません。ちゃんと、ウィンドウが開かれて説明が表示され
ます。

.. index::
   pair: Variable; skk-num-convert-float

skk-num-convert-float
   この変数の値を ``non-nil`` に設定すると、浮動小数点数を使った見出し語に対応し
   て数値変換を行います。ただし、辞書において

   .. code:: text

       #.# /#1．#1/#0月#0日/

   などの見出し語が使用できなくなります。

.. index::
   pair: Variable; skk-show-num-type-info

skk-show-num-type-info
   ``Non-nil`` であれば、辞書登録モードに入るのと同時に変換タイプの案内を表示しま
   す。標準設定は ``t`` です。

.. index::
   pair: Variable; skk-num-grouping-separator

skk-num-grouping-separator
   タイプ ``#8`` で使用する記号。標準設定は ``,`` 。

.. index::
   pair: Variable; skk-num-grouping-places

skk-num-grouping-places
   タイプ ``#8`` について、何桁毎に区切るのかを数値で指定する。標準設定は 3。

.. index::
   pair: Variable; skk-use-numeric-conversion

skk-use-numeric-conversion
   この変数を ``nil`` に設定すると、本節で説明した数値変換の機能を全て無効にしま
   す。

.. _conv-ascii-midasi:

アスキー文字を見出し語とした変換
--------------------------------

かなモードで :kbd:`/` を打鍵すると **SKK abbrev モード** に入り、以後の入力はアス
キー文字になります。普通に :kbd:`SPC` を押すと、その見出し語に係る変換が得られま
す。

仮に、辞書に

.. code:: text

    is /インクリメンタル・サーチ/

というエントリがあるとして、以下に例を示します。

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
----------------

かなモード／カナモードで :kbd:`@` を入力すれば、今日の日付が入力されます。

日付の形式は以下の変数により決定されます。

.. index::
   pair: Variable; skk-date-ad

skk-date-ad
   この変数の値が ``non-nil`` であれば西暦で、 ``nil`` であれば元号で表示します。
   標準設定は ``nil`` です。

.. index::
   pair: Variable; skk-number-style

skk-number-style
   この変数の値は以下のように解釈されます。標準設定は ``1`` です。

   .. list-table::

      * - ``0`` or ``nil``
        - ASCII 数字。「1996年7月21日(日)」のようになります。
      * - ``1`` or ``t``
        - 全角数字。「１９９６年７月２１日(日)」のようになります。
      * - ``2``
        - 漢数字(位取)。「一九九六年七月二一日(日)」のようになります。
      * - ``3``
        - 漢数字。「千九百九十六年七月二十一日(日)」のようになります。

上記の「1996年」、「１９９６年」、「一九九六年」の部分は、変数 ``skk-date-ad`` の
値が ``nil`` であれば「平成8年」のように元号で表示されます。

辞書 :file:`SKK-JISYO.lisp` には、見出し語 ``today`` の候補として ``skk-date-ad`` と
``skk-number-style`` の全ての組み合わせがプログラム実行変換機能を用いて登録されて
います。従って、 ``/ t o d a y SPC`` と入力すると、今日の日付が上記の形式で順次候
補として表 示されます。

関数 ``skk-relative-date`` を利用すると、昨日、一昨日、明後日など任意の日付を求め
ることができます。詳細は :file:`skk-gadget.el` のコメントを参照してください。

なお、 :kbd:`@` の打鍵で日付を挿入するのではなく、文字どおり ``@`` を挿入したい場
合は次のとおり。

.. code:: emacs-lisp

    (setq skk-rom-kana-rule-list
          (append skk-rom-kana-rule-list
                  '(("@" nil "@"))))

.. _program-conversion:

プログラム実行変換
------------------

辞書の候補に Emacs Lisp のプログラムが書いてあれば、そのプログラムを Emacs に実行
させ、返り値をカレントバッファに挿入します。これを **プログラム実行変換** と呼ん
でいます。例えば、辞書に

.. code:: text

    now /(current-time-string)/

というエントリがあるとします。このとき ``/ n o w SPC`` とキー入力すれば、現在のバ
ッファに関数 ``current-time-string`` の返り値である

.. code:: text

    Sun Jul 21 06:40:34 1996

のような文字列が挿入されます。

ここで、プログラムの返り値は文字列である必要があります。また、プログラム実行変換
の辞書登録は通常の単語と同様に行うことができますが、その中に改行を含まないように
書く必要  [#]_ があります。

:ref:`今日の日付の入力 <input-today>` で説明した ``today`` の辞書エントリは、実際
は下記のようなプログラムを候補にもっています。

.. code:: emacs-lisp

    today /(let ((skk-date-ad) (skk-number-style t)) (skk-today))/.../

:file:`skk-gadget.el` には、西暦／元号変換や簡単な計算などプログラム実行変換用の
関数が集められています。

.. index::
   pair: Function; skk-calc

skk-calc operator
   ``skk-calc`` は、引数をひとつ取り、見出し語の数字に対しその演算を行う簡単な計
   算プログラムです。

   .. code:: emacs-lisp

       (defun skk-calc (operator)
         ;; 2つの引数を取って operator の計算をする。
         ;; 注意: '/ は引数として渡せないので (defalias 'div '/) などとし、別の形で
         ;; skk-calc に渡す。
         ;; 辞書エントリの例 -> #*# /(skk-calc '*)/
         (number-to-string (apply operator
                                  (mapcar 'string-to-number
                                          skk-num-list))))

   この関数を実際にプログラム実行変換で利用するには、辞書に以下のようなエ
   ントリを追加します。 :ref:`数値変換 <number-conv>`

   .. code:: text

       #*# /(skk-calc '*)/

   ``Q 1 1 1 * 4 5 SPC`` とキー入力します。ここで ``111`` と ``45`` の２つの数字
   は、変換時に ``("111" "45")`` のような文字列のリストにまとめられ、変数 ``skk-num-list``
   の値として保存されます。次に関数 ``skk-calc`` が呼ばれます。この中で
   ``skk-num-list`` の各要素に対し演算を行うため、各要素は数に変換されます。
   その上で ``skk-calc`` に与えられた引数（この場合は ``*`` ）を演算子として演算
   を行います。

.. index::
   pair: Function; skk-gadget-units-conversion

skk-gadget-units-conversion 基準単位 数値 変換単位
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

   単位変換の情報は、変数 ``skk-units-alist`` で定義されています。

.. index::
   pair: Variable; skk-units-alist

skk-units-alist
   この変数は以下の形式の連想リストです。

   .. code:: text

       (基準となる単位 (変換する単位 . 変換時の倍率)
                       (… . …))

   関数 ``skk-gadget-units-conversion`` で利用されています。標準設定では、以下の
   単位変換の情報を定義しています。

   .. code:: emacs-lisp

       ("mile" ("km" . 1.6093)
               ("yard" . 1760))

       ("yard" ("feet" . 3)
               ("cm" . 91.44))

       ("feet" ("inch" . 12)
               ("cm" . 30.48))

       ("inch" ("feet" . 0.5)
               ("cm" . 2.54))

.. index::
   pair: Function; skk-relative-date pp-function

skk-relative-date pp-function format and-time &key (yy 0) (mm 0) (dd 0)
   ``skk-current-date`` の拡張版。 ``PP-FUNCTION`` , ``FORMAT`` , ``AND-TIME`` の
   意味は ``skk-current-date`` の docstring を参照のこと。
   キーワード変数 ``:yy`` , ``:mm`` , ``:dd`` に正または負の数値を指定することで
   明日、明後日、一昨日などの日付を求めることができる。詳細は :file:`skk-gadget.el` の
   コメントを参照のこと。

空白・改行・タブを含んだ見出し語の変換
--------------------------------------

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

.. index::
   pair: Variable; skk-allow-spaces-newlines-and-tabs

skk-allow-spaces-newlines-and-tabs
   この変数を ``nil`` に設定すると、本節で説明したような２行以上にまたがる文字列
   に対する変換を禁止します。

.. _katakana-conv:

カタカナ変換
------------

通常、SKK でカタカナ語を入力するには、

-  :kbd:`q` でカナモードに移ってからカタカナを入力する
-  ▽モードで :kbd:`q` によりカタカナへ変換する :ref:`かなモードからカタカナを入力 <input-katakana>`

のどちらかです。これらの方法は手軽ですが、個人辞書に登録されないため見出し語の補
完候補にも現れず、何度でも入力しなければなりません。

.. index::
   pair: Variable; skk-search-katakana

変数 ``skk-search-katakana`` を設定することで、カタカナ語が普通の変換候補として現
れ、個人辞書にも登録されます。設定するには以下を :file:`~/.skk` に記述します [#]_ 。

.. code:: emacs-lisp

    (setq skk-search-katakana t)

また、値をシンボル ``jisx0201-kana`` とすると、カタカナ候補に加え半角カタカナ候補
も変換候補に現れます。

.. code:: emacs-lisp

    (setq skk-search-katakana 'jisx0201-kana)

.. _sahen-dousi:

サ変動詞変換
------------

通常、SKK では諸般の事情によりサ行変格活用の動詞は送りなし変換をする前提になって
います。このことは共有辞書のメンテナンスにおける便宜上やむをえないのですが、個人
辞書が育たない（サ変動詞と名詞の区別ができない）という弱点もあります。

[サ変動詞の辞書登録に関する注意]

.. index::
   pair: Variable; skk-search-sagyo-henkaku

変数 ``skk-search-sagyo-henkaku`` を設定することで、任意の送りなし候補を利用して
サ行の送りプレフィックスに限定して送りあり変換が可能になり、個人辞書を育てること
が可能になります。設定するには以下を :file:`~/.skk` に記述します [#]_ 。

.. code:: emacs-lisp

    (setq skk-search-sagyo-henkaku t)

例えば「お茶する」の変換は以下のように変化します。

.. list-table::

   * - 従来
     - ``O c h a SPC s u r u``
   * - サ変
     - ``O c h a S u r u``

変数の値をシンボル ``anything`` に設定すると、サ行に限らず任意の送り仮名を許可し、
送りあり変換をします。これにより、送りあり変換の利用範囲を形容詞・動詞の変換のみ
ならず、あらゆるひらがな開始点の指定に拡張することができます。

このサ変動詞送りあり変換機能は、 :ref:`カタカナ変換機能 <katakana-conv>` と組み合
わせるとさらに有効です。

異体字へ変換する
----------------

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
   pair: Variable; skk-itaiji-jisyo

skk-itaiji-jisyo
   辞書ファイル :file:`SKK-JISYO.itaiji` 又は :file:`SKK-JISYO.itaiji.JIS3_4` へ
   のパスを指定する。他の辞書ファイルと異なり、この２つの辞書ファイルは見出し語が
   漢字です。

.. index::
   pair: Function; skk-search-itaiji

skk-search-itaiji
   not documented. http://mail.ring.gr.jp/skk/200303/msg00071.html

ファンクションキーの使い方
--------------------------

.. index::
   pair: Variable; skk-j-mode-function-key-usage

skk-j-mode-function-key-usage
   シンボル ``conversion`` ならば、 ``skk-search-prog-list-1`` 〜 ``skk-search-prog-list-9``
   および ``skk-search-prog-list-0`` を実行するよう自動設定します。
   これらのプログラムは▽モード限定でファンクションキー ( :kbd:`[F1]` 〜 :kbd:`[F10]` )
   に割り当てられます。

   :kbd:`[F5]` 〜 :kbd:`[F10]` については本オプションの設定により自動的に割り当て
   られます。これらの割り当てはユーザオプション ``skk-verbose`` を設定するとエコー
   エリアに表示されるようになります。 [冗長な案内メッセージの表示]

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

   シンボル ``kanagaki`` ならば、かなキーボード入力用に自動設定します。
   ``nil`` ならば、自動設定しません。

キー設定
========

かなモード／カナモードのキー設定
--------------------------------

ローマ字のルールの設定
^^^^^^^^^^^^^^^^^^^^^^

DDSKK の■モードにおける文字変換は、２つの変数

-  ``skk-rom-kana-base-rule-list``
-  ``skk-rom-kana-rule-list``

を用いて行われます。

``skk-rom-kana-base-rule-list`` には、基本的なローマ字かな変換のルールが定められ
ています。一方 ``skk-rom-kana-rule-list`` は、ユーザが独自のルールを定めるために
用意されており、 ``skk-rom-kana-base-rule-list`` よりも優先されます。

これらは「入出力の状態がいかに移り変わるべきか」を決定します。その内容は、

.. code:: emacs-lisp

    (入力される文字列 出力後に自動的に入力に追加される文字列 出力)

という形のリストを列挙したものです。

  - 入力される文字列…変換される前のアスキー文字の文字列をいいます。

  - 出力…次の入力状態に移るときにバッファに挿入される文字列の組み合わせであり、
    ``("ア" . "あ")`` のようなコンスセルです。

``skk-rom-kana-base-rule-list`` の一部を見てみましょう。

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

``skk-rom-kana-base-rule-list`` には、次のような便利な変換ルールも定められていま
す。

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
^^^^^^^^^^^^^^^^^^^^^^

``skk-rom-kana-base-rule-list`` の規則に従うと

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

更に ``skk-rom-kana-rule-list`` を用いれば TUT-code による日本語入力を実現するこ
ともできます。TUT-code による入力についてはソースアーカイブの :file:`tut-code` デ
ィレクトリに収録されている各ファイルを参照してください。

[ローマ字入力以外の入力方式]

■モードに関連するその他の変数
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index::
   pair: Variable; skk-kana-input-search-function

skk-kana-input-search-function
   ルールリストの中に記せない変換ルールを処理する関数。
   これは ``skk-rom-kana-base-rule-list`` と ``skk-rom-kana-rule-list`` の要素を
   全て検索した後にコールされます。引数はありません。バッファの文字を、
   直接 ``preceding-char`` などで調べて下さい。

   初期設定では ``h`` で、長音を表すために使われています。次の例を見て下さい。

   - :kbd:`ohsaka` → おおさか
   - :kbd:`ohta` → おおた

   一方で、 ``hh`` は「っ」になります。

   - :kbd:`ohhonn` → おっほん
   - :kbd:`ohhira` → おっひら

   これは ``skk-rom-kana-rule-list`` の標準設定に

   .. code:: emacs-lisp

       ("hh" "h" ("ッ" . "っ"))

   が入っているためです。これを削除すれば

   - :kbd:`ohhonn` → おおほん
   - :kbd:`ohhira` → おおひら

   となります。

.. index::
   pair: Variable; skk-kutouten-type

skk-kutouten-type
   ■モードの標準では、キーボードの :kbd:`.` を打鍵すると「。」が、 :kbd`,` を打
   鍵すると「、」がバッファに挿入されます。変数 ``skk-kutouten-type`` に適切なシ
   ンボルを設定することにより、この組み合せを変更 [#]_ することができます。
   そのシンボルとは、次の４つです。

   - シンボル ``jp`` → 「。」「、」 （標準設定）
   - シンボル ``en`` → 「．」「，」
   - シンボル ``jp-en`` → 「。」「，」
   - シンボル ``en-jp`` → 「．」「、」

   または、変数 ``skk-kutouten-type`` にはコンスセルを指定することも可能です。
   その場合は、

   .. code:: emacs-lisp

       (句点を示す文字列 . 読点を示す文字列)

   のように指定します。例として、次のように設定するとキーボードの :kbd:`.`
   で ``abc`` が、 :kbd:`,` で ``def`` がバッファに入力されます。

   .. code:: emacs-lisp

       (setq skk-kutouten-type '("abc" . "def"))

   なお、変数 ``skk-kutouten-type`` はバッファローカル変数です。すべてのバッファ
   で統一した設定としたい場合は、

   .. code:: emacs-lisp

       (setq-default skk-kutouten-type 'en)

   のように関数 ``setq-default`` を用いてください。

.. index::
   pair: Variable; skk-use-auto-kutouten

skk-use-auto-kutouten
   標準設定は ``nil`` 。 ``Non-nil`` であれば、カーソル直前の文字種に応じて句読点
   を動的に変更します。

数字や記号文字の入力
^^^^^^^^^^^^^^^^^^^^

かなモード／カナモードにおける次のキーは、関数 ``skk-insert`` にバインドされてい
ます。

.. code:: text

    !  #  %  &  '  *  +

    -  0  1  2  3  4  5

    6  7  8  9  :  ;  <

    =  >  ?  "  (  )  [

    ]  {  }  ^  _  `  |

    ~

これらの数字や記号文字のキーに対応し挿入される文字をカスタマイズするためには、変
数 ``skk-rom-kana-rule-list`` を利用します。

.. code:: emacs-lisp

    (setq skk-rom-kana-rule-list
          (append skk-rom-kana-rule-list
                  '(("!" nil "!")
                    ("," nil ",")
                    ("." nil ".")
                    (":" nil ":")
                    (";" nil ";")
                    ("?" nil "?"))))

関数 ``skk-insert`` は、Emacs のオリジナル関数 ``self-insert-command`` をエミュレー
トしています。具体的には、引数を渡すことによって同じ文字を複数、一度に挿入するこ
とが可能です。

.. code:: text

    C-u 2 !

      ------ Buffer: foo ------
      ！！
      ------ Buffer: foo ------

全英モードのキー設定
^^^^^^^^^^^^^^^^^^^^

全英モードにおける印字可能な全てのキーはコマンド ``skk-jisx0208-latin-insert`` に
割り付けられています。また、変数 ``skk-jisx0208-latin-vector`` の値により挿入され
る文字が決定され、その標準設定は以下のようになっています。

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

挿入される文字を変更したい場合: [数字や記号文字の入力]

関数 ``skk-jisx0208-latin-insert`` も Emacs オリジナルの関数
``self-insert-command`` を エミュレートしています。つまり、関数
``skk-insert`` における動作と同じく、引数を渡すことにより同じ文字を複数、一度に挿
入することができます。

[数字や記号文字の入力]

閉じ括弧の自動入力
------------------

リージョンを括弧で囲む
----------------------

確定するキー
------------

.. _cand-select-key:

候補の選択に用いるキー
----------------------

▼モードでの RET
----------------

▼モードでの BS
---------------

送りあり変換中の C-g
--------------------

変換位置の指定方法
------------------

１回の取り消し操作 (undo) の対象
--------------------------------

変換、確定の前後
================

ポイントを戻して▽モードへ
--------------------------

直前の確定を再変換
------------------

自動変換開始
------------

.. _ammoku-kakutei:

暗黙の確定のタイミング
----------------------

積極的な確定
------------

確定辞書
--------

送り仮名関連
============

SKK の送り仮名の処理は、好みが分かれるところです。色々な対策が用意されていますの
で、試してみて下さい。

送り仮名の厳密なマッチ
----------------------

送り仮名の優先的なマッチ
------------------------

.. _okurigana:

送り仮名の自動処理
------------------

どのように変換されるか
^^^^^^^^^^^^^^^^^^^^^^

辞書登録の際に注意すべきこと
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

候補の順序
==========

skk の初期設定では、変換で確定された単語は、次の変換時では最初に表示されます。
この動作を変更して、効率良く変換する方法があります。

ここで解説するほか、確定辞書を用いた変換も、候補の順序に影響を与えます。

変換の学習
----------

候補の順序の固定
----------------

ベイズ統計を用いた学習
----------------------

辞書関連
========

本節では、辞書の種別と形式、設定方法、その他辞書にまつわる動作や設定を説明します。

.. _jisyo-variant:

辞書の種類
----------

辞書ファイルの指定
------------------

.. _search-jisyo:

辞書の検索方法の設定
--------------------

.. _setting-search-jisyo:

辞書検索の設定の具体例
^^^^^^^^^^^^^^^^^^^^^^

辞書検索のための関数
^^^^^^^^^^^^^^^^^^^^

Emacs 付属の辞書
----------------

.. _server-relative:

サーバ関連
----------

サーバコンプリージョン
----------------------

辞書の書式
----------

送りありエントリと送りなしエントリ
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

送りありエントリのブロック形式
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

エントリの配列
^^^^^^^^^^^^^^

強制的に辞書登録モードへ入る
----------------------------

.. _delete-wrong-register:

誤った登録の削除
----------------

.. _edit-jisyo:

個人辞書ファイルの編集
----------------------

.. _saving-jisyo:

個人辞書の保存動作
------------------

変換及び個人辞書に関する統計
----------------------------

辞書バッファ
------------

辞書バッファの文字コードの設定
------------------------------

辞書バッファの buffer-file-name
-------------------------------

注釈（アノテーション）
======================

かな漢字変換の際に、候補に注釈（アノテーション）が登録されていれば、それを表示す
ることができます。

アノテーションの基礎
--------------------

アノテーションの使用
--------------------

アノテーションの登録
--------------------

アノテーションとして EPWING 辞書を表示する
------------------------------------------

Apple macOS 「辞書」サービスからアノテーションを取得する
--------------------------------------------------------

Wikipedia/Wiktionary からアノテーションを取得する
-------------------------------------------------

外部コマンドからアノテーションを取得する
----------------------------------------

各種アノテーション機能を SKK の枠をこえて活用する
-------------------------------------------------

文字コード関連
==============

文字コードまたはメニューによる文字入力
--------------------------------------

メニューによる文字入力
----------------------

文字コード一覧
--------------

文字コードを知る方法
--------------------


DDSKK 以外のツールを用いた辞書変換
==================================

skk-lookup
----------

skk-look
--------

Lisp シンボル名の補完検索変換
-----------------------------

Google CGI API for Japanese Input を利用したかな漢字変換
--------------------------------------------------------


飾りつけ
========

仮名文字のローマ字プレフィックスのエコー
----------------------------------------

入力モードを示すモードラインの文字列の変更
------------------------------------------

.. _cursor-color-input-mode:

入力モードを示すカーソル色に関する設定
--------------------------------------

変換候補一覧の表示方法
----------------------

▼モードにおける変換候補のハイライト表示
----------------------------------------

変換候補の更なる装飾
--------------------

モードラインの装飾
------------------


ユーザガイダンス関連
====================

.. _display-japanese-message:

エラーなどの日本語表示
----------------------

冗長な案内メッセージの表示
--------------------------

I-search 関連
=============

起動時の入力モードの指定
------------------------

間に空白等を含む文字列の検索
----------------------------


VIP/VIPERとの併用
=================

VIPER については Info を参照してください。


picture-modeとの併用
====================



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

.. [#] `:kbd:C-q` は ``skk-abbrev-mode-map`` にて特別な動作をするように定義されて
       います。

.. [#] メニューバーが使用できる環境では、メニューバーを使ってこれらの一括変換コマ
       ンドを呼び出すことができます。ただし :command:`kakasi` がインストールされ
       ていない場合は :command:`kakasi` を利用する機能が灰色になり使用できません。

.. [#] `KAKASI - 漢字→かな（ローマ字）変換プログラム <http://kakasi.namazu.org/>`_

.. [#] 昭和29年12月9日付内閣告示第一号によれば、原則的に訓令式（日本式）を用いる
       かのように記載されていますが、今日一般的な記載方法は、むしろヘボン式である
       ようです。

.. [#] 細かい説明です。 :kbd:`TAB` を押す直前に▽モードで入力された文字列を X と
       呼ぶことにします。このとき、個人辞書の送りなしエントリの中から「先頭が X と
       一致し」かつ「長さが X よりも長い見出し語」を検索して、そのような語が該当
       すれば X の代わりに表示します。

.. [#] 同じ列に既に入力している文字列があったときにそれを参照して補完しようとする
       機能

.. [#] 現在は候補群の右側１カラムのフェイスが標準設定に戻る、という制約があります。

.. [#] ``p125`` という見出し語であれば、その数値部分である ``125`` が再変換の見出
       し語となります。

.. [#] SHIFT キーを伴って数字を入力し始めることはできないので :kbd:`Q` または :kbd:`/` で
       ▽モードに入る必要があります。

.. [#] ``m`` や ``d`` などアスキー文字を見出し語として入力する場合は :kbd:`/` キー
       を最初に入力して SKK abbrev モードに入ってから入力する必要があります。

       :ref:`アスキー文字を見出し語とした変換 <conv-ascii-midasi>`

.. [#] 通常の単語では、改行を含むことが可能です。それは、評価するとその位置に改行
       を挿入するような実行変換プログラムに変換して辞書に書き込んでいるからです。

       [辞書の種類]

       しかし、実行変換されるプログラムを辞書登録する際にはこの機能を利用できない
       ため、改行を含むことができません。

.. [#] ``skk-search-prog-list`` の設定をユーザが変更している場合は期待どおりに動
       作しない場合があります。その場合は ``skk-search-prog-list`` の設定に
       関数 ``skk-search-katakana`` の呼び出しがあることを確認してください。
       またこの機能の設定は DDSKK 14.1 以前では異なります。詳しくはソースに付属の
       ドキュメント、設定例をご覧ください。

.. [#] ``skk-search-prog-list`` の設定をユーザが変更している場合は期待どおりに動
       作しない場合があります。その場合は ``skk-search-prog-list`` の設定に
       関数 ``skk-search-sagyo-henkaku`` の呼び出しがあることを確認してください。
       またこの機能の設定は DDSKK 14.1 以前では異なります。詳しくはソースに付属の
       ドキュメント、設定例をご覧ください。

.. [#] 辞書が充実していれば、かな漢字変換で見出し語「へん」から「邊」や「邉」を求
       めることができます。もちろん、文字コードを指定して「邊」や「邉」を直接挿入
       することもできます。

.. [#] 変数 ``skk-use-kana-keyboard`` が ``non-nil`` ならば無効である。
