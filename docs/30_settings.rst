============
はじめの設定
============

.. index::
   pair: File; skk-setup.el
   pair: File; leim-list.el
   pair: File; skk-autoloads.el
   pair: Function; normal-top-level
   pair: Function; register-input-method
   pair: Key; C-x C-j

標準的にインストールした場合は、特段の設定なしに Emacs を起動するだけで DDSKK が
使える状態になります。自動的に :file:`skk-setup.el` というファイルが読み込まれ、
設定されます [#]_ 。この自動設定によらずに手動で設定したい場合は、以下の説明を参
照してください。

最も基本的な設定
================

.. index::
   pair: File; init.el
   pair: Key; C-x C-j
   pair: Key; C-x j
   pair: Key; C-x t

自動設定によらず手動で設定する場合は、次の内容を :file:`~/.emacs.d/init.el` に書
きます [#]_ 。

.. code:: emacs-lisp

  (require 'skk-autoloads) ; XEmacs でパッケージとしてインストールした場合は不要
  (global-set-key "\C-x\C-j" 'skk-mode)
  (global-set-key "\C-xj" 'skk-auto-fill-mode)
  (global-set-key "\C-xt" 'skk-tutorial)

辞書サーバを使わない場合は、辞書ファイルを指定する必要があります。

.. code:: emacs-lisp

  (setq skk-large-jisyo "/your/path/to/SKK-JISYO.L")

辞書サーバを使わない場合は Emacs のバッファに ``skk-large-jisyo`` が指すファイル
を取り込んで使用するためメモリ使用量が増加します。これが支障となる場合は、上記の
:file:`SKK-JISYO.L` を :file:`SKK-JISYO.M` 、 :file:`SKK-JISYO.ML` 又は
:file:`SKK-JISYO.S` に変更してください。

.. _cdb-format:

.. index::
   keyword: CDB 形式辞書ファイル

DDSKK 14.1 以降は辞書サーバを経由せずとも CDB 形式 [#]_ の辞書ファイルを直接利用
できるようになりました。CDB 形式辞書ファイル [#]_ を利用する場合は、以下のように
指定してください。

.. code:: emacs-lisp

  (setq skk-cdb-large-jisyo "/your/path/to/SKK-JISYO.L.cdb")

変数 ``skk-large-jisyo`` と変数 ``skk-cdb-large-jisyo`` を同時に指定した場合は、
標準では CDB 形式辞書ファイルの方が先に検索 [#]_ されます。

インクリメント検索の設定
========================

基本的な設定は :file:`skk-setup.el` が読み込まれた時点で完了しています [#]_ 。

.. index::
   pair: Option; skk-isearch-mode-enable

skk-isearch-mode-enable
  この変数は :file:`~/.emacs.d/init.el` か :kbd:`M-x customize-variable` で設定し
  てください。 ``Non-nil`` であれば、SKK が ON になっているバッファで skk-isearch を
  有効にします。標準設定は ``t`` です。 ``nil`` に設定すると skk-isearch を無効に
  することができます。シンボル ``always`` に設定すると、SKK が ON になっていない
  バッファでも skk-isearch を有効にします。

.. _setting-jisyo-server:

辞書サーバを使いたいときの設定
==============================

辞書サーバを使いたいときは、 :file:`~/.skk` で以下のように設定します。

.. code:: emacs-lisp

  (setq skk-server-host "example.org")
  (setq skk-server-portnum 1178)

.. index::
   pair: Variable; skk-server-host

skk-server-host
  辞書サーバが起動しているホスト名又は IP アドレス。

.. index::
   pair: Variable; skk-server-portnum

skk-server-portnum
  辞書サーバが使うポート番号。 :file:`/etc/services` に ``skkserv`` のエントリが記述
  されていれば、この変数を指定する必要は無い。

.. index::
   pair: Option; skk-server-inhibit-startup-server

skk-server-inhibit-startup-server
  この変数が ``nil`` であれば、辞書サーバが起動していなかったときに Emacs か
  ら ``skkserv`` プロセスを起動することができます。

  Emacs から立ち上げて利用する事ができる辞書サーバは、

  .. code:: console

    skkserv [-p port] [jisyo]

  のようなオプションを受け付け、 ``inetd`` などを経由せず直接起動するものに限られ
  ます。辞書サーバプログラムと辞書ファイルは、次のように設定します。

  .. code:: console

    (setq skk-server-prog "/your/path/to/skkserv")
    (setq skk-server-jisyo "/your/path/to/SKK-JISYO.L")

.. index::
   pair: Variable; skk-server-prog

skk-server-prog
  辞書サーバプログラムをフルパスで指定する。

.. index::
   pair: Variable; skk-server-jisyo

skk-server-jisyo
  辞書サーバに渡す辞書をフルパスで指定する。辞書サーバによっては独自の方法で辞書
  ファイルを指定して emacs からの指定を無視するものもあります。詳しくは各辞書サー
  バの説明書を読んで下さい。

.. index::
   pair: 環境変数; SKKSERVER
   pair: 環境変数; SKKSERV
   pair: 環境変数; SKK_JISYO

これらの設定は、環境変数を利用して下記のようにすることもできます。

- B シェルの場合（sh, bash, ksh, zsh など）

  .. code:: sh

    export SKKSERVER=example.org
    export SKKSERV=/your/path/to/skkserv
    export SKK_JISYO=/your/path/to/SKK-JISYO.L


- C シェルの場合（csh, tcsh など）

  .. code:: csh

    setenv SKKSERVER example.org
    setenv SKKSERV /your/path/to/skkserv
    setenv SKK_JISYO /your/path/to/SKK-JISYO.L

関連項目

  - :ref:`辞書サーバの入手 <get-jisyo-server>`

  - :ref:`サーバ関連 <server-relative>`

DDSKK を Emacs の Input Method とする
=====================================

.. index::
   pair: File; skk-leim.el
   pair: Key; C-\
   pair: Key; M-x toggle-input-method

Emacs の標準キーバインドでは :kbd:`C-\\` を打鍵すると、関数 :func:`toggle-input-method` を
実行します。この関数は、変数 ``default-input-method`` が指す input method を
トグル切り替えします。

.. index::
   keyword: default-input-method
   keyword: LEIM

変数 ``default-input-method`` の値はおそらく "Japanese" であり、結果として
:kbd:`C-\\` の打鍵で LEIM (Library of Emacs Input Method) を on / off します。

.. index::
   pair: Key; M-x list-input-methods
   pair: Key; M-x set-input-method
   pair: Key; C-x RET C-\

使用可能な input method は :kbd:`M-x list-input-methods` で確認することができ、コ
マンド :kbd:`M-x set-input-method` 又は :kbd:`C-x RET C-\\` を実行することで
input method を切り替えることができます。

ファイル :file:`skk-leim.el` から生成されるファイル :file:`skk-autoloads.el` で
input method をふたつ追加しています。

.. list-table::

   * - input method
     - 内容
   * - "japanese-skk"
     - :code:`(skk-mode 1)`
   * - "japanese-skk-auto-fill"
     - :code:`(skk-auto-fill-mode 1)`

.. index::
   pair: Option; default-input-method

default-input-method
  Emacs 起動時の input method を DDSKK とするには、 :file:`~/.emacs.d/init.el` に

  .. code:: emacs-lisp

    (setq default-input-method "japanese-skk")

  と記述してください。

.. rubric:: 脚注

.. [#] Emacs が起動する過程の関数 :func:`normal-top-level` で :file:`SKK_LISPDIR/leim-list.el` が
       読み込まれます。
       :file:`leim-list.el` は :file:`skk-autoloads.el` と :file:`skk-setup.el` を require します。
       :file:`skk-autoloads.el` は DDSKK の :command:`make` 時に自動的に生成され
       るファイルであり、各関数を autoload するよう定義するほか :func:`register-input-method` も
       行います。
       :file:`skk-setup.el` はキーバインド（ :kbd:`C-x C-j` → :func:`skk-mode` ）の定義、
       変数 ``skk-tut-file`` の定義及びインクリメンタル・サーチの定義を行っています。

.. [#] 配布物にサンプルファイル :file:`etc/dot.emacs` と :file:`etc/dot.skk` があります。
       参考にして下さい。

.. [#] constant database のこと。
       詳しくは http://cr.yp.to/cdb.html 又は http://ja.wikipedia.org/wiki/Cdb を
       参照のこと。

.. [#] SKK 辞書 の :file:`Makefile` 中の ``cdb`` ターゲットを実行することで
       :file:`SKK-JISYO.L` に基づく :file:`SKK-JISYO.L.cdb` を生成することができます。

.. [#] :ref:`辞書検索の設定の具体例 <setting-search-jisyo>`

.. [#] :file:`skk-setup.el` では、 ``isearch-mode-hook`` に ``skk-isearch-setup-maybe`` を、
       ``isearch-mode-end-hook`` に ``skk-isearch-cleanup-maybe`` をそれぞれ追加
       しています。
       ``skk-isearch-{setup|cleanup}-maybe`` も :file:`skk-setup.el` で定義されて
       おり、その実態は、関数 :func:`skk-isearch-mode-{setup|cleanup}` です。
