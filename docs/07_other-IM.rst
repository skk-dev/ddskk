##########################
ローマ字入力以外の入力方式
##########################

DDSKK は、SKK 旧来のローマ字式かな入力（訓令式、ヘボン式）方式のほか、各種キー配
列と入力方式に対応しています。

****
AZIK
****

`AZIK （エイズィック） <http://hp.vector.co.jp/authors/VA002116/azik/azikindx.htm>`_
は、QWERTY 配列をベースとした拡張ローマ字入力です。一般のローマ字入力がそのまま使
える上での拡張であることが特徴です。

.. el:defvar:: skk-use-azik

   non-nil であれば AZIK 拡張が有効となります。
   ファイル :file:`~/.skk` に :code:`(setq skk-use-azik t)` と書きます。

.. el:defvar:: skk-azik-keyboard-type

   AZIK で使うときのキーボードのタイプをシンボルで指定する。

   .. list-table::

      * - シンボル
        - キーボードのタイプ
      * - シンボル 'jp106
        - 日本語 106 キーボード（標準設定）
      * - シンボル 'jp-pc98
        - NEC PC-98 キーボード
      * - シンボル 'us101
        - 英語キーボード
      * - nil
        - キーボード依存処理を無効にする

azik と skk で仕様が重なる部分があるため、ファイル :file:`skk-azik.el` では以下のとおり対
応しています。

.. el:define-key:: q

   AZIK では、撥音「ん」を入力するには :kbd:`q` を使うこととされていますが、
   skk では既に :kbd:`q` に関数 :el:defun:`skk-toggle-kana` を割り当てています。

   そのため、ファイル :file:`skk-azik.el` では関数 :el:defun:`skk-toggle-kana` の実行を

   -  日本語キーボードであれば :kbd:`@` を

   -  英語キーボードであれば :kbd:`[` を

   それぞれ使用します。

.. el:define-key:: @

   上記のとおり、関数 :el:defun:`skk-toggle-kana` の実行には :kbd:`@` （日本語キーボード）
   や :kbd:`[` （英語キーボード）を使用しますが、skk では既に :kbd:`@` には「今日
   の日付の入力」（ :ref:`プログラム実行変換 <program-conversion>` ）を割り当てて
   います。

   そのため、skk 本来の動作には :kbd:`x` を付けて、それぞれ :kbd:`x@` と :kbd:`x[`
   で代用できるようにしてあります。

.. el:define-key:: l
                   xx

   AZIK では、単独の拗音「ゃゅょぁぃぅぇぉゎ」を入力するには :kbd:`l` を前置する
   こととされていますが、skk では既に :kbd:`l` に「アスキーモードへの切り替え」
   を割り当てています。

   そのため、ファイル :file:`skk-azik.el` では、拗音のうち「ぁぃぅぇぉ」 の入力については
   :kbd:`xx` を前置することとしています。

   - :kbd:`xxa` → ぁ

   - :kbd:`xxi` → ぃ

   - :kbd:`xxu` → ぅ

   - :kbd:`xxe` → ぇ

   - :kbd:`xxo` → ぉ

   なお、拗音のうち「ゃゅょゎ」の単独入力は、AZIK 拡張ファイル :file:`skk-azik.el`
   では なく、標準ファイル :file:`skk-vars.el` です。

   - :kbd:`xya` → ゃ

   - :kbd:`xyu` → ゅ

   - :kbd:`xyo` → ょ

   - :kbd:`xwa` → ゎ

.. el:define-key:: X

   skk では、▼モードでの :kbd:`X` は関数 :el:defun:`skk-purge-from-jisyo` を実行し
   ますが、AZIK では :kbd:`X` は「シャ行」の入力に使われます。

   そのため、ファイル :file:`skk-azik.el` での :ref:`誤った登録の削除 <delete-wrong-register>` は、
   ▼モードで :kbd:`M-x skk-purge-from-jisyo` を実行してください。

****
ACT
****

`ACT (AZIK on Dvorak) <http://www1.vecceed.ne.jp/~bemu/act/act_index.html>`_ は
AZIK の考え方を Dvorak 配列に適用し、Dvorak 配列でかなを快適にタイプできるように
考案された方式です。

.. el:defvar:: skk-use-act

   non-nil であれば、 ACT 拡張が有効となります。
   ファイル :file:`~/.skk` に :code:`(setq skk-use-act t)` と書きます。

********
TUT-code
********

`TUT-code <http://plone.crew.sfc.keio.ac.jp/groups/tut-code>`_ は、２ストローク系
の日本語直接入力方式の一つです。

使用するには、SKK のインストール時にいくつかのファイルをインストールする必要があ
ります。

SKK ソースの `tut-code` ディレクトリにあるファイル :file:`skk-tutcdef.el` と
ファイル :file:`skk-tutcode.el` を SKK ソースのトップディレクトリにコピーしてから、
あらためて SKK をインストールします。

その後、ファイル :file:`~/.skk` に :code:`(require 'skk-tutcdef)` と書きます。

********************
かな入力と親指シフト
********************

DDSKK はローマ字式ではない、いわゆるかな入力方式をサポートします。具体的には

  - 旧 JIS 配列でのかな入力
  - 親指シフト方式でのかな入力

に対応しています。これを使うにはまず、nicola-ddskk 拡張パッケージをインストールす
る必要があります。SKK ソースのディレクトリ :file:`nicola` に移動し、ドキュメント
に従ってインストールしてください。

https://github.com/skk-dev/ddskk/blob/master/nicola/README.ja

.. el:defvar:: skk-use-kana-keyboard

   non-nil に設定すると、かな入力サポートが SKK 起動時に有効になります。

   .. code:: emacs-lisp

       (setq skk-use-kana-keyboard t)

.. el:defvar:: skk-kanagaki-keyboard-type

   適切なシンボルを設定することで、かな入力サポートの種類を切り換えます。

   .. list-table::

      * - シンボル '106-jis
        - | 日本語 106 キーボード (旧 JIS 配列) でのかな入力に対応します。
          | :code:`(setq skk-kanagaki-keyboard-type '106-jis)`
      * - シンボル 'nicola-jis
        - | 日本語 106 キーボード (旧 JIS 配列) での親指シフトエミュレーションに対応します。
          | :code:`(setq skk-kanagaki-keyboard-type 'nicola-jis)`
      * - シンボル 'nicola-us
        - 
      * - シンボル 'nicola-dvorak
        - 
      * - シンボル 'nicola-colemak
        - 
      * - シンボル 'omelet-jis
        - | 'nicola-jis と同様ですが、より入力しやすい配列が考慮されています。
          | :code:`(setq skk-kanagaki-keyboard-type 'omelet-jis)`
      * - シンボル 'omelet-us
        - 
      * - シンボル 'omelet-dvorak
        - 
      * - シンボル 'omelet-colemak
        - 
      * - シンボル 'oasys
        - 

かな入力方式使用時の■モードでは、以下のコマンドなどが役に立ちます。

.. el:define-key:: F1 1

   かな入力方式での特殊キー定義の一覧を表示します。

.. el:define-key:: F1 2

   かな入力方式でのかなキー配列を表示します。

.. el:define-key::  F12

   かな入力方式とローマ字入力方式とを切り換えます。

なお、親指シフト方式については `NICOLA 日本語入力コンソーシアム <http://nicola.sunicom.co.jp/>`_
を参照してください。
