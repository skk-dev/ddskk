############
インストール
############

********************
DDSKK のインストール
********************

ここでは、UNIX 上で :command:`make` が利用できる環境 [#]_ を想定します。

まず、DDSKK のアーカイブ :file:`ddskk-VERSION.tar.gz` を :command:`tar` と
:command:`gzip` を使用して展開します。

.. code:: console

   % gzip -cd ddskk-VERSION.tar.gz | tar xvf -

次に、DDSKK のトップディレクトリ [#]_ をカレントディレクトリにします。

.. code:: console

   % cd ddskk-VERSION

GNU Emacs へのインストール
--------------------------

まずは、DDSKK がどのディレクトリにインストールされるのか確認するために
``what-where`` を引数に :command:`make` を実行しましょう。

.. code:: console

  % make what-where
  -| emacs -batch -q -no-site-file -l SKK-MK -f SKK-MK-what-where
  -| Loading /home/USER/temp/ddskk-VERSION/SKK-CFG...

  -| Running in:
  -|   GNU Emacs 26.0.50 (build1, x86_64-pc-linux-gnu, GTK+ Version ...

  -| SKK modules:
  -|   skk-cursor, skk-viper, ...
  -|   -> /path/to/emacs/site-lisp/skk

  -| SKK infos:
  -|   skk.info
  -|   -> /path/to/share/info

  -| SKK tutorials:
  -|   SKK.tut, SKK.tut.E, NICOLA-SKK.tut, skk.xpm
  -|   -> /path/to/share/skk

emacs の実体ファイルを特定することもできます。

.. code:: console

   $ make what-where EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

.. index::
   pair: File; SKK-CFG

また、DDSKK のインストール先ディレクトリを変更したい場合は、ファイル :file:`SKK-CFG` を
編集してください。編集後は必ず :command:`make` ``what-where`` を実行して表示内容
を確認してください。

次にスーパーユーザになって、

.. code:: console

   $ su
   % make install

と実行すると、実際に DDSKK がインストールされます。

あるいは、一般ユーザが自分の home directory を root directory として DDSKK をイン
ストールするには、

.. code:: console

   % make install PREFIX=~/

と、 ``PREFIX`` を指定して :command:`make` を実行します。

特定の Emacs を指定する場合は、

.. code:: console

   % make install EMACS=mule

と指定します。

対話的なインストール
--------------------

DDSKK 14.3 では「対話的インストーラ」が追加されました。

.. index::
   keyword: dired

まず :kbd:`M-x dired` とキー入力して dired を起動してください。このとき、ディレク
トリを問われますので、先に述べた「DDSKK のアーカイブを展開したディレクトリ」を指
定してください。

.. code:: text

   ------ Minibuffer -------
   Dired (directory): ~/temp/ddskk-VERSION RET
   ------ Minibuffer -------

次に、表示されたディレクトリ一覧の ``SKK-MK`` にカーソルをあわせて :kbd:`L`
（アルファベットのエルの大文字）を打鍵してください。

.. code:: text

   ------ Dired -------
   -rw-r--r-- 1 user user  99999 2011-00-00 00:00 SKK-CFG
   -rw-r--r-- 1 user user  99999 2011-00-00 00:00*SKK-MK    "L"
   drwxr-xr-x 1 user user  99999 2011-00-00 00:00 bayesian
   ------ Dired -------

プロンプト ``Load SKK-MK?`` には :kbd:`y` を打鍵してください。

以降、インストーラが表示する質問に答えながら DDSKK のインストールを進めてください。
なお、パーミッションは一切考慮していませんので、インストール先は書き込み権限を有
するディレクトリを指定してください。

MELPA によるインストール
------------------------

.. index::
   keyword: MELPA
   keyword: package.el
   pair: Variable; package-archives
   pair: Function; package-initialize

2014年12月、 MELPA [#]_ に DDSKK が登録されたことにより、 GNU Emacs で
も package.el [#]_ によるインストールが可能となりました。

詳細については、次のドキュメントを参照してください。

https://github.com/skk-dev/ddskk/blob/master/READMEs/INSTALL.MELPA.md

************
辞書について
************

DDSKK を使用するには、いわゆる辞書（主にかなと漢字の対応を記述したデータ）が必要
です。

.. index::
   keyword: ja-dic
   keyword: LEIM

DDSKK 14.2 からは、 GNU Emacs 同梱の辞書データ ja-dic を利用したかな漢字変換に対
応しましたので、SKK 辞書ファイルを別途インストールしなくても最低限の使用ができま
す。

しかし、 ja-dic は、 GNU Emacs の入力メソッド LEIM のためにファイル :file:`SKK-JISYO.L` か
ら変換して生成されたものであり、英数変換や数値変換などのエントリ、および「大丈夫」
など複合語とみなし得る語が大幅に削除されています。
そのため、ファイル :file:`SKK-JISYO.L` を利用したかな漢字変換と同等の結果は得られません。

有志の知恵を結集して作られている各種 SKK 辞書は便利ですから、是非入手してインスト
ールしましょう。

.. _getting-jisyo-files:

**********
辞書の入手
**********

次のサイトには、様々な辞書が用意されています。

`SKK dictionary files gh-pages <https://skk-dev.github.io/dict/>`_

以下は、その一例です。

.. list-table::

   * - SKK-JISYO.S
     - S 辞書（主に単漢字が登録。最小限必要な語を収録）
   * - SKK-JISYO.M
     - M 辞書（普通に使う分には足りる程度）
   * - SKK-JISYO.ML
     - | M 辞書と L 辞書の中間のサイズの辞書。
       | L 辞書収録語の内、EPWING 辞書やオンライン辞書で正しいと判別された語をベースにして加除。
   * - SKK-JISYO.L
     - L 辞書（あらゆる単語を収録）
   * - zipcode
     - 郵便番号辞書
   * - SKK-JISYO.JIS2
     - JIS X 0208 で定められている第２水準の文字を、部首の読みを見出し語として単漢字を収録した辞書
   * - SKK-JISYO.JIS3_4
     - JIS 第３水準、第４水準の文字に代表される、JIS X 0208 には含まれないが JIS X 0213 には含まれる文字及びそれらを含む語録を収録した辞書
   * - SKK-JISYO.public+
     - public+ 辞書
   * - SKK-JISYO.edict
     - edict 辞書（英和辞書）
   * - SKK-JISYO.lisp
     - | 候補に Emacs Lisp 関数を含むエントリーを集めた辞書。
       | 見出し語を変換する過程で Emacs Lisp 関数を評価し、その値を候補として表示します。
       | :ref:`プログラム実行変換 <program-conversion>`
   * - SKK-JISYO.wrong
     - S, M, L 辞書に既に登録されていたが、間違いであったことが判明したために削除された単語を収録

.. note::

   一部の辞書は、著作権が GNU GPL v2 ではありませんのでご注意下さい。詳細は、次の
   資料を参照して下さい。

   https://github.com/skk-dev/dict/blob/master/committers.txt

.. el:define-key:: M-x skk-get

   Emacs の使用中に :kbd:`M-x skk-get` と実行すると、辞書ファイルを一括ダウンロー
   ドすることができます。プロンプトが表示されるので、ダウンロード先のディレクトリを入力
   してください。

.. el:defun:: skk-get &optional DIRECTORY

   :el:defun:`skk-get` を関数として使用することで、ユーザプログラムの中からで
   も辞書ファイルを一括ダウンロードすることができます。

.. code:: elisp

  (skk-get "~/jisyofiles")

*************************************
辞書を DDSKK と同時にインストールする
*************************************

DDSKK のソースを展開すると、中に :file:`dic` というディレクトリが存在します。
ファイル :file:`SKK-JISYO.L` などをこのディレクトリにコピーしてから :command:`make` ``install`` を
実行すると、辞書ファイルがチュートリアル (:file:`SKK.tut`) と同じディレクトリ [#]_
にインストールされます。

具体的なインストール先は :command:`make` ``what-where`` を実行すると表示されます。

.. code:: console

   -| SKK dictionaries:
   -|   SKK-JISYO.lisp, SKK-JISYO.zipcode, SKK-JISYO.office.zipcode, ...
   -|   -> c:/emacs-24.5/share/emacs/24.5/etc/skk

``dic`` ディレクトリに辞書ファイルを置くためには :command:`make` ``get`` と実行す
る [#]_ のが簡単です。

.. _get-jisyo-server:

****************
辞書サーバの入手
****************

辞書サーバはオプションです。辞書サーバが無くても DDSKK は動作しますが、特に辞書の
サイズが大きい場合は辞書サーバを利用することで省メモリ効果を得られます。また、辞
書サーバによっては複数辞書の検索、EPWING 辞書の検索ができたりするものもあります。

DDSKK は特定の辞書サーバの実装に依存していませんので、下記の辞書サーバのいずれで
も動作可能です。ソースやバイナリの入手、インストールについてはそれぞれのウェブサ
イトをご参照下さい。

`辞書サーバの説明とリンク <http://openlab.jp/skk/skkserv-ja.html>`_

.. rubric:: 脚注

.. [#] Microsoft Windows 環境では :command:`makeit.bat` を使用することで、UNIX と
       同様の操作でインストールできます。ファイル :file:`READMEs/README.w32.ja` を
       参照してください。cygwin 環境をインストールされている方は :command:`make` 
       が使用できるので、本文の解説がそのまま当てはまります。Apple macOS 環境の
       方はファイル :file:`READMEs/README.MacOSX.ja` を参照してください。

.. [#] ファイル :file:`ChangeLog` やファイル :file:`Makefile` が置かれているディ
       レクトリです。

.. [#] `Milkypostman's Emacs Lisp Package Archive. <http://melpa.org/>`_

.. [#] GNU Emacs 24 以降で標準で搭載されています。GNU Emacs 23 以前では手動でイ ン
       ストールする必要があります。 http://wikemacs.org/wiki/Package.el

.. [#] :file:`/usr/share/skk` や :file:`c:/emacs-24.5/etc/skk` など

.. [#] Microsoft Windows 環境では :command:`makeit.bat` ``get`` と実行します。
