============
はじめの設定
============

.. index::
   pair: File; skk-setup.el
   pair: File; leim-list.el
   pair: File; skk-autoloads.el
   keyword: normal-top-level
   keyword: register-input-method
   pair: Key; C-x C-j

標準的にインストールした場合は、特段の設定なしに Emacs を起動するだけで DDSKK が
使える状態になります。自動的に ``skk-setup.el`` というファイルが読み込まれ、設定
されます [#]_ 。この自動設定によらずに手動で設定したい場合は、以下の説明を参照し
てください。

最も基本的な設定
================

.. index::
   pair: File; init.el
   pair: Key; C-x C-j
   pair: Key; C-x j
   pair: Key; C-x t

自動設定によらず手動で設定する場合は、次の内容を ``~/.emacs.d/init.el`` に書きま
す [#]_ 。

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
``SKK-JISYO.L`` を ``SKK-JISYO.M`` 、 ``SKK-JISYO.ML`` 又は ``SKK-JISYO.S`` に変
更してください。

.. index::
   keyword: CDB 形式辞書ファイル

DDSKK 14.1 以降は辞書サーバを経由せずとも CDB 形式 [#]_ の辞書ファイルを直接利用
できるようになりました。CDB 形式辞書ファイル [#]_ を利用する場合は、以下のように
指定してください。

.. code:: emacs-lisp

  (setq skk-cdb-large-jisyo "/your/path/to/SKK-JISYO.L.cdb")

変数 ``skk-large-jisyo`` と 変数 ``skk-cdb-large-jisyo`` を同時に指定した場合は、
標準では CDB 形式辞書ファイルの方が先に検索 [#]_ されます。

インクリメント検索の設定
========================

基本的な設定は ``skk-setup.el`` が読み込まれた時点で完了しています [#]_ 。

.. index::
   pair: Option; skk-isearch-mode-enable

skk-isearch-mode-enable
  この変数は ``~/.emacs.d/init.el`` か ``M-x customize-variable`` で設定してくだ
  さい。 ``Non-nil`` であれば、SKK が ON になっているバッファで skk-isearch を有
  効にします。標準設定は ``t`` です。 ``nil`` に設定すると skk-isearch を無効にす
  ることができます。シンボル ``always`` に設定すると、SKK が ON になっていないバ
  ッファでも skk-isearch を有効にします。

辞書サーバを使いたいときの設定
==============================

辞書サーバを使いたいときは、 ``~/.skk`` で以下のように設定します。

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
  辞書サーバが使うポート番号。 ``/etc/services`` に ``skkserv`` のエントリが記述
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
  辞書サーバに渡す辞書をフルパスで指定する。辞書サーバによっては独自の方
  法で辞書ファイルを指定して emacs からの指定を無視するものもあります。
  詳しくは各辞書サーバの説明書を読んで下さい。

.. index::
   keyword: SKKSERVER
   keyword: SKKSERV
   keyword: SKK_JISYO

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

  - [辞書サーバの入手]

  - [サーバ関連]

DDSKK を Emacs の Input Method とする
=====================================

.. index::
   pair: File; skk-leim.el
   pair: Key; C-\
   pair: Key; M-x toggle-input-method

Emacs の標準キーバインドでは ``C-\`` を打鍵すると、関数 ``toggle-input-method`` を
実行します。この関数は、変数 ``default-input-method`` が指す input method を
トグル切り替えします。

.. index::
   keyword: default-input-method
   keyword: LEIM

変数 ``default-input-method`` の値はおそらく "Japanese" であり、結果として
``C-\`` の打鍵で LEIM (Library of Emacs Input Method) を on / off します。

.. index::
   pair: Key; M-x list-input-methods
   pair: Key; M-x set-input-method
   pair: Key; C-x RET C-\

使用可能な input method は ``M-x list-input-methods`` で確認することができ、コマ
ンド ``M-x set-input-method`` 又は ``C-x RET C-\`` を実行することで input method を
切り替えることができます。

ファイル ``skk-leim.el`` から生成されるファイル ``skk-autoloads.el`` で input method を
ふたつ追加しています。

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
  Emacs 起動時の input method を DDSKK とするには、 ``~/.emacs.d/init.el`` に

  .. code:: emacs-lisp

    (setq default-input-method "japanese-skk")

  と記述してください。

.. rubric:: 脚注

.. [#] Emacs が起動する過程の関数 ``normal-top-level`` で ``SKK_LISPDIR/leim-list.el`` が
       読み込まれます。 ``leim-list.el`` は ``skk-autoloads.el`` と ``skk-setup.el`` を
       ``require`` します。 ``skk-autoloads.el`` は DDSKK の ``make`` 時に自動的
       に生成されるファイルであり、各関数を自動ロード (autoload) するよう定義する
       ほか ``register-input-method`` も行います。 ``skk-setup.el`` はキーバイン
       ド（ ``C-x C-j`` → ``skk-mode`` ）の定義、変数 ``skk-tut-file`` の定義及
       びインクリメンタル・サーチの定義を行っています。

.. [#] 配布物にサンプルファイル ``etc/dot.emacs`` と ``etc/dot.skk`` があります。
       参考にして下さい。

.. [#] constant database のこと。
       詳しくは http://cr.yp.to/cdb.html 又は http://ja.wikipedia.org/wiki/Cdb を
       参照のこと。

.. [#] SKK 辞書 の ``Makefile`` 中の ``cdb`` ターゲットを実行することで
       ``SKK-JISYO.L`` に基づく ``SKK-JISYO.L.cdb`` を生成することができます。

.. [#] [辞書検索の設定の具体例]

.. [#] ``skk-setup.el`` では、 ``isearch-mode-hook`` に ``skk-isearch-setup-maybe`` を、
       ``isearch-mode-end-hook`` に ``skk-isearch-cleanup-maybe`` をそれぞれ追加
       しています。 ``skk-isearch-{setup|cleanup}-maybe`` も ``skk-setup.el`` で定
       義されており、その実態は、関数 ``skk-isearch-mode-{setup|cleanup}`` です。
