############################
よくある質問とその回答 (FAQ)
############################

これは SKK に対するよくある質問と、それに対する回答集です。

************
Introduction
************

Q1-1 Daredevil SKK って SKK とは違うのですか?
=============================================

SKK Openlab で開発、リリースされる SKK は、京大の佐藤先生が中心になって開発してい
た SKK と区別するために、Daredevil SKK と呼ぶことにしました。
その略称は DDSKK で、SKK Openlab で最初に Daredevil SKK としてリリースされた version
は 11.1 です（オリジナルの version を継承しました）。

なお、 Daredevil の名前の採択は、開発陣の一人が講読している某ラジオ英会話講座の、
ある日のスキット名が「Daredevil なんとか」で、その内容は「とにかくやってみよう。
うぎゃぁぁぁ、やられたぁ」というものでした。これがあまりに自分の開発ポリシーに合
致していた、ということに由来します。

Q1-2 SKK はシンプルなのが長所だったのでは?
==========================================

かような議論は 10 年来行われてきており、結論は出ていませんが、事実として現在まで
開発が続けられています。「シンプルな操作性の維持と多機能化・高機能化は両立できる」
というのが現在の開発陣の考えであるようです。

SKK が Simple Kana to Kanji conversion program の略であるとおり、かなを漢字に変換
するルーチンの簡単さが SKK を定義付けています。その周辺の拡張に関する制約は基本的
にはありません。

多機能化と言っても多くはユーザオプションによって無効にすることができますし、
ファイル :file:`skk.el` 本体が複雑化しないようにモジュール化されています。

Q1-3 DDSKK はどの Emacs で使えますか?
=====================================

対応する Emacs のバージョンについては :doc:`このバージョンの SKK について <02_intro>` をご覧ください。

Q1-4 DDSKK はどんなオペレーティングシステムで使えますか?
========================================================

SKK がサポートしている Emacs がその OS で動いているなら、SKK の基本的な機能は動く
はずです。 Microsoft Windows でも Apple macOS でも使えます。

拡張機能については、UNIX の各種コマンド（ :command:`look` や :command:`ispell` など）
を前提としているものがいくつかあります。これらのコマンドがお使いの OS にも存在す
れば該当の拡張機能も基本的には使えるでしょう。

Apple macOS 版 Emacs に特化した情報については、以下のファイルを参照してください。

  - https://github.com/skk-dev/ddskk/blob/master/READMEs/README.MacOSX.ja

Q1-5 APEL って何? 必要ですか?
=============================

APEL は A Portable Emacs Library の略です。APEL の主な機能は、異なる Emacs 間の非
互換性を吸収することです。

GNU Emacs 22 以上では APEL は不要となりました。この変更は 2010 年 9 月 に CVS に commit され、2011 年 1 月に DDSKK 14.2 としてリリースされました。

************
Installation
************

Q2-1 SKK を使うのに何が必要ですか?
==================================

SKK 本体と SKK 辞書が必要です。オプションで辞書サーバを用意することができます。

Q2-2 SKK 辞書はどこにありますか?
================================

:ref:`SKK 辞書について <skk-jisyo>`

Q2-3 SKK サーバはどこにありますか?
==================================

DDSKK は辞書サーバの種類、バージョンには依存していません。

  - :ref:`辞書サーバの入手 <get-jisyo-server>`

  - http://openlab.jp/skk/skkserv-ja.html

*************
Customization
*************

Q3-1 「．」、「，」 が入力できるようにカスタマイズしたいのですが。
==================================================================

３通りの方法を紹介します。

通常 :kbd:`.` で「．」を、 :kbd:`,` で「，」を入力したい場合
------------------------------------------------------------

:ref:`■モードに関連するその他の変数 <var-skk-kutouten-type>` をご覧ください。

一時的に :kbd:`.` で「．」を、 :kbd:`,` で「，」を入力したい場合
----------------------------------------------------------------

:kbd:`M-x skk-toggle-kutouten` を実行すると、その場で「，」「．」に切り替え
ることができます。「、」「。」に戻すには、もう一度 :kbd:`M-x skk-toggle-kutouten` を
実行します。

特定のバッファ（例えば tex モード）でのみ「，」「．」に切り替えたい場合は、次の設
定を tex 文書ファイルの最後に追加します。

.. code:: text

   % Local Variables:
   %   skk-kutouten-type: en
   % end:

常に :kbd:`.` で「．」を、 :kbd:`,` で「，」を入力したい場合
------------------------------------------------------------

変数 :el:defvar:`skk-rom-kana-rule-list` を直接変更します。

.. warning::

   この設定をすると :kbd:`M-x skk-toggle-kutouten` での切り替えが効かなくなるので、
   注意して下さい。

ファイル :file:`~/.skk` に以下を追加します。

.. code:: emacs-lisp

   (setq skk-rom-kana-rule-list
         (append '(("." nil "．") ("," nil "，"))
                 skk-rom-kana-rule-list))

この設定方法は応用が効き、細かく制御することが可能です。
「．」と「，」のところをそれぞれ ``.`` と ``,`` とすることで、
「かなモード」「カナモード」でも ``.`` と ``,`` を直接入力することができます。

Q3-2 「ゐ」や「ヰ」 が入力できるようにカスタマイズしたいのですが。
==================================================================

一つ前の Q の変形問題ですね。かなモード／カナモードでそれぞれ出力する文字を変える
やり方です。ファイル :file:`~/.skk` に

.. code:: emacs-lisp

    (setq skk-rom-kana-rule-list
          (append '(("wi" nil ("ヰ" . "ゐ")))
                  skk-rom-kana-rule-list))

と書いてみましょう。

一番内側の cons cell は

  - 関数 :el:defun:`car` の評価、つまり「ヰ」が、カナモード
  - 関数 :el:defun:`cdr` の評価、つまり「ゐ」が、かなモードで

の入力文字を表しています。

一つ前の Q に対する答えのように、カナモード、かなモードともに入力する文字が変わら
なければ、cons cell の代りに文字列を書くことができます。

Q3-3 検索する辞書を増やしたいのですが。
=======================================

変数 :el:defvar:`skk-search-prog-list` で設定をしましょう。

まず、現在の設定を確認しましょうね。 scratch バッファに ``skk-search-prog-list`` と
書いてそのシンボルの末尾にポイントを置いて :kbd:`C-j` してみましょう。
例えば次のように出力されます。

.. code:: emacs-lisp

    ((skk-search-jisyo-file skk-jisyo 0 t)
     (skk-search-server skk-aux-large-jisyo 10000))

上記の例は２つの要素を持ったリストになっています。設定によりもっと多くの要素があ
るかもしれません。

各要素は検索する関数と辞書を指定したリストです。要素の順番に検索がなされます。
上記の例だと、

- まず最初に :el:defvar:`skk-jisyo` （個人辞書）を関数 :el:defun:`skk-search-jisyo` を使って
  リニアサーチし、
- 次に関数 :el:defun:`skk-search-server` を使って :el:defvar:`skk-aux-large-jisyo` をサーチ
  します。

変換の際、 :kbd:`SPC` を押しますよね？　１回 :kbd:`SPC` を押すと、SKK は候補が見
つかるまでの間、 :el:defvar:`skk-search-prog-list` の要素を前から読んでいって検索を行い、
見つかればそこでいったん検索を止めてユーザに候補を提示します。

ユーザが :kbd:`SPC` を更に押してゆき最初の要素のプログラムが見つけた候補が尽きると、
SKK は中断していた個所から再び :el:defvar:`skk-search-prog-list` の次の要素を見つけ、ここ
で指定されている関数を使って検索する、で新しい候補が見つかればまた提示する、とい
うシステムになっています。

では、辞書サーバを使って検索した後に、JIS 第２水準の単漢字辞書ファイル :file:`SKK-JISYO.JIS2` を
検索したい場合はどうすれば良いでしょう？　もう分かりますよね？
辞書サーバを使った検索式の次に第２水準辞書の検索式を書いたリストを :el:defvar:`skk-search-prog-list` に
指定すれば良いのです。ファイル :file:`~/.skk` に次のように書きましょう。

.. code:: emacs-lisp

    (setq skk-search-prog-list
          '((skk-search-jisyo-file skk-jisyo 0 t)
            (skk-search-server skk-aux-large-jisyo 10000)
            (skk-search-jisyo-file "~/dic/SKK-JISYO.JIS2" 0)))

:el:defun:`skk-search-jisyo-file` の第２引数である 0 の数字でリニアサーチにて検索するよう
指定しています。第２水準辞書はあまり大きくないので、リニアサーチで十分でしょう。
大きな辞書を検索する場合などは、

.. code:: emacs-lisp

    (skk-search-jisyo-file "~/dic/SKK-JISYO.L" 10000)

のようにすると良いでしょう。SKK は Emacs のバッファに読み込まれた辞書の検索リージ
ョンのポイント差が 10,000 未満になるまではバイナリサーチを行い、その後リニアサー
チを行います。大きな辞書ではバイナリサーチを行う方がはるかに効率が良いです。

ちなみに、ファイル :file:`SKK-JISYO.JIS2` は、最大でもリージョン間のポイント差が 8,500 程度です。

.. _Q3-4:

Q3-4 左手の小指を SHIFT で酷使したくありません。
================================================

SKK を標準の状態で使っている場合、変換のためにシフトキーを多用しますので小指への
負担が大きくなります。この苦しみを回避するためにここでは４つの方法を紹介します。

親指の近くにあるキーを利用してシフトキーの代用とする。
------------------------------------------------------

日本語 106 キーボードのように :kbd:`無変換` 、:kbd:`変換` などのキーがある場合は、
これらをシフトキーの代用とすることが可能です。こうすると、例えば

- :kbd:`SHIFT` を押しながら :kbd:`a` を押す

というキー操作は

- :kbd:`無変換` を押して、その後で :kbd:`a` を押す

という操作で置き換えることができるようになります。

それでは具体的なやり方を説明しましょう。まず、使用中の Emacs が :kbd:`無変換` を
何という名前で認識しているか調べます。それには

:kbd:`M-x describe-key`

というコマンドを実行し、続いて :kbd:`無変換` を押してみます。
X Window System 上 であれば、おそらく

.. code:: text

   muhenkan is undefined

という答えが返ってくるでしょう。

次に、この名前を使ってファイル :file:`~/.emacs.d/init.el` に設定を書きこみます。
以下は :kbd:`無変換` = ``muhenkan`` の場合の例です。

.. code:: emacs-lisp

    (unless (keymapp key-translation-map)
      (setq key-translation-map (make-sparse-keymap)))
    (let ((i ?a))
      (while (<= i ?z)
        (define-key key-translation-map
                    (vector 'muhenkan i) (vector (- i 32)))
        (setq i (1+ i))))

この設定を終えると、 ``muhenkan-a`` で ``A`` が入力できるようになります。

続いて SKK を起動してみましょう。 ``muhenkan-a`` で

.. code:: text

   ▽あ*

となります。送りの開始点も、もちろん同様の操作で指定できます。

xmodmap を使う。
----------------

X Window System 上では、 :command:`xmodmap` を使ってキー配列を変更できます。

例えば、「無変換キー」をシフトキーとして使いたければ

.. code:: text

   % xmodmap -e 'add Shift = Muhenkan'

とします。これで「無変換キー」は通常のシフトキーと同じような感じで使えるよ
うになります。

``skk-sticky.el`` を使う。
--------------------------

:ref:`変換位置の指定方法 <sticky>`

親指シフト入力のエミュレーション機能を利用する。
------------------------------------------------

これは前述した方法とはかなり違ったアプローチです。SKK 本来のローマ字的入力を捨
てて、富士通のワープロ OASYS のような親指シフト入力 [#]_ を修得します 。

DDSKK には NICOLA-DDSKK というプログラムが付属しており、これをインストー
ルすると親指シフト入力が可能になります。インストール自体は簡単で、

.. code:: console

    % cd nicola
    % make install

とした後に、ファイル :file:`~/.skk` に

.. code:: emacs-lisp

    (setq skk-use-kana-keyboard t)
    (setq skk-kanagaki-keyboard-type 'omelet-jis)

と書くだけです。詳しいことは、NICOLA-DDSKK 付属のドキュメントを参照してください。

NICOLA 配列は、特別に日本語入力のために考えられた配列なので、慣れれば非常に効率的
な日本語入力ができるようになると期待されます。

一方で、ローマ字的入力方式に慣れてしまっている人にとっては、NICOLA 配列に慣れるま
でか なり練習を要することは確かです。

Q3-5 全く漢字が出てきません。
=============================

恐らく辞書の設定ができていないのでしょう。

ファイル :file:`SKK-JISYO.L` というファイルがインストールされている場所を確認してください。
普通は

  - `/usr/local/share/skk`
  - `/usr/share/skk`

といった場所にインストールされています。

その後でファイル :file:`~/.skk` に

.. code:: emacs-lisp

    (setq skk-large-jisyo "/usr/local/share/skk/SKK-JISYO.L")

のように設定します。

なお、辞書サーバを使っている場合はこの設定は必要ありません。その場合は、辞書サー
バの設定や、それがちゃんと起動しているかどうかを確認してください。

どこにも辞書がインストールされていない場合は

  - https://skk-dev.github.io/dict/

から取得します。

Q3-6 チュートリアルが起動できません。
=====================================

ファイル :file:`SKK.tut` というファイルがインストールされている場所を確認してください。
普通は

  - `/usr/local/share/skk`
  - `/usr/share/skk`

といった場所にインストールされています。

その後でファイル :file:`~/.emacs.d/init.el` に

.. code:: emacs-lisp

    (setq skk-tut-file "/usr/local/share/skk/SKK.tut")

のように設定します。

Q3-7 C-x C-j で dired が起動してしまいます。
============================================

``dired-x`` を読み込むと :kbd:`C-x C-j` が関数 :el:defun:`dired-jump` にバインドされます。
この状態でも SKK を :kbd:`C-x C-j` で起動したいときは、変数 :el:defvar:`dired-bind-jump` に
nil を設定します。

.. code:: emacs-lisp

    (setq dired-bind-jump nil)

なお、この設定は ``dired-x`` を読み込む前である必要があります。

************
Dictionaries
************

Q4-1 SKK には郵便番号辞書がありますか?
======================================

`zipcode` というディレクトリに入っています。

  - https://skk-dev.github.io/dict/

使用方法は

  - https://github.com/skk-dev/dict/blob/master/zipcode/README.md

を御覧下さい。

Q4-2 SKK の辞書には、品詞情報がないんですね。
=============================================

SKK は漢字とかなとの区切りをユーザが指定する方式により、品詞情報を使った解析を用
いることなく効率的入力ができます。

TODO としては、辞書に品詞情報を持たせることで更なる入力の効率化ができるという提案
がなされており、そのような辞書の作成が既に試みられています。

興味のある方は次の url をご覧ください。

  - `SKK-JISYO.notes <http://openlab.jp/skk/wiki/wiki.cgi?page=SKK%BC%AD%BD%F1>`_

Q4-3 複数の SKK 辞書を結合できますか?
=====================================

SKK 本体のパッケージには同封されていませんが、 ``skk-tools`` という別パッケージが
あります。

:ref:`辞書ツール <jisyo-tools>`

Q4-4 SKK 形式の英和辞書があると聞いたのですが。
===============================================

edict は和英辞書ですが、これを SKK 辞書形式の英和辞書に変換したものを

  - https://skk-dev.github.io/dict/SKK-JISYO.edict.tar.gz

として置いています。これは edict を単純に機械的に変換した後、バグの修正や、エント
リ・候補の追加が SKK Openlab で独自に行われているものです。

edict を自分で加工して上記と同等のものを作成することもできます。edict は

  - ftp://ftp.u-aizu.ac.jp:/pub/SciEng/nihongo/ftp.cc.monash.edu.au/

などから入手できます。
加工には日本語の通る :command:`gawk` と ``skk-tools`` の中のプログラムを使い、下
記のように行います。

.. code:: console

    % jgawk -f edict2skk.awk edict > temp
    % skkdic-expr temp | skkdic-sort > SKK-JISYO.E2J
    % rm temp

できたファイル :file:`SKK-JISYO.E2J` の利用方法は色々ありますが、

.. code:: console

    % skkdic-expr SKK-JISYO.E2J + /usr/local/share/skk/SKK-JISYO.L | \
      skkdic-sort > SKK-JISYO.L

などとして、ファイル :file:`SKK-JISYO.L` とマージして使うのが手軽です。

なお、edict の配布条件は GNU GPL (General Public License) ではありません。

http://www.csse.monash.edu.au/groups/edrdg/newlic.html

をご覧下さい。ファイル :file:`SKK-JISYO.edict` のヘッダー部分にもそのダイジェスト
が記載されています。

*************
Miscellaneous
*************

Q5-1 SKK abbrev モードでもっと英単語を利用した変換ができませんか?
=================================================================

UNIX :command:`look` とファイル :file:`skk-look.el` を利用すると、色々できますよ。

まず、ファイル :file:`~/.skk` で変数 :el:defvar:`skk-use-look` を t にセットして Emacs/SKK を立ち上げ
直して下さい。

.. note::

   ``skk-look.el`` を使った補完・変換が期待するスピードよりも遅い、補完・変換で余
   分な候補が出る、とお感じの貴方は、変数 :el:defvar:`skk-look-use-ispell` の値を nil にして
   :command:`ispell` によるスペルチェック・修正をオフにしてお試し下さい。

さぁ、下記のような芸当が可能になりました。

英単語の補完ができます。
------------------------

.. code:: text

      ▽abstr*

    TAB

      ▽abstract*

通常の補完機能と同様に :kbd:`.` で次の補完候補に、 :kbd:`,` でひとつ前の補完候補
に移動できます。SKK 形式の英和辞書があれば、ここから :kbd:`SPC` を押して英和変換
ができますね。

また、変数 :el:defvar:`skk-look-use-ispell` の値が non-nil であれば、 :command:`look` で検索す
る前に :command:`ispell` でスペルチェック・修正をします。

英単語をあいまいに変換して取り出す
----------------------------------

上記同様、変数 :el:defvar:`skk-look-use-ispell` の値が non-nil であれば、
:command:`look` で検索する前に :command:`ispell` でスペルチェック・修正をします。

.. code:: text

      ▽abstr*

    SPC

      ▼abstract*

見出し語に ``*`` を入れるのをお忘れなく。

あいまいに変換した後、更に再帰的な英和変換を行う
------------------------------------------------

まず、変数 :el:defvar:`skk-look-recursive-search` の値を non-nil にセットして下さい。
Emacs / SKK を再起動する必要はありません。すると、例えば、

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

このように英単語＋その英単語を見出し語にした候補の「セット」を変換結果として出力
することができます。

この際、変数 :el:defvar:`skk-look-expanded-word-only` の値が non-nil であれば、再帰検索に成功
した英単語の「セット」だけを出力することができます（再帰検索で検出されなかった英
単語は無視して出力しません）。

もちろん、SKK 辞書に

.. code:: text

    abstract /アブストラクト/抽象/
    abstraction /アブストラクション/

というエントリがあることを前提としています。edict を SKK 辞書形式に変換すると良い
ですね。

Q5-2 市販の CD-ROM 辞書やネットワークの辞書サーバが利用できますか?
==================================================================

Lookup が扱える辞書はほとんど使えます。Lookup がインストールされている状態で
SKK をインストールすると、SKK と Lookup のゲートウェイプログラム
ファイル :file:`skk-lookup.el` が インストールされます。

インストールで注意すべきは、 :command:`make` で呼び出される Emacs は
``-q -no-site-file`` フラグ付きで呼ばれるので、ファイル :file:`~/.emacs.d/init.el`
やファイル :file:`site-start.el` などは読み込まれないことです。
標準設定で変数 :el:defvar:`load-path` の通っているディレクトリに Lookup をインストールするか、
ファイル :file:`SKK-CFG` の中で ``VERSION_SPECIFIC_LISPDIR`` などにディレクトリを明示する
ことで解決できます。

さぁ、ファイル :file:`~/.skk` で変数 :el:defvar:`skk-search-prog-list` の要素に :code:`(skk-lookup-search)` を
追加しましょう。他の検索エンジンよりも検索は比較的遅いので、最後の方が良いと思い
ます。

こんな感じです。

.. code:: emacs-lisp

    (setq skk-search-prog-list
          '((skk-search-jisyo-file skk-jisyo 0 t)
            (skk-search-server skk-aux-large-jisyo 10000)
            (skk-lookup-search)))

Lookup については、http://openlab.jp/edict/lookup/ をご参照下さい。

Q5-3 他の FEP を使用中にも SHIFT を押してしまいます。
=====================================================

治すには SKK をやめるしかありません :-)

Emacs 上以外でも SKK みたいな操作性を実現するソフトウェアがあります。

:ref:`SKK 関連ソフトウェア <other-software>`

.. rubric:: 脚注

.. [#] 親指シフト入力の詳細については、ここでは述べません。
       興味がある場合は、Web サイトを訪れてください。

       `日本語入力コンソーシアム <http://nicola.sunicom.co.jp/>`_
