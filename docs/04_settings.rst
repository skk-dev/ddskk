############
はじめの設定
############

.. index::
   pair: File; skk-setup.el
   pair: File; leim-list.el
   pair: File; skk-autoloads.el
   pair: Function; normal-top-level
   pair: Function; register-input-method
   pair: Key; C-x C-j

標準的にインストールした場合は、特段の設定なしに Emacs を起動するだけで DDSKK が
使える状態になります。自動的にファイル :file:`skk-setup.el` が読み込まれ、
設定されます [#]_ 。この自動設定によらずに手動で設定したい場合は、以下の説明を参
照してください。

****************
最も基本的な設定
****************

.. index::
   pair: File; init.el
   pair: Key; C-x C-j
   pair: Key; C-x j
   pair: Key; C-x t

自動設定によらず手動で設定する場合は、次の内容をファイル :file:`~/.emacs.d/init.el` に
書きます [#]_ 。

.. code:: elisp

  (require 'skk-autoloads)
  (global-set-key "\C-x\C-j" 'skk-mode)
  (global-set-key "\C-xj" 'skk-auto-fill-mode)
  (global-set-key "\C-xt" 'skk-tutorial)

辞書サーバを使わない場合は、辞書ファイルを指定する必要があります。

.. code:: elisp

  (setq skk-large-jisyo "/your/path/to/SKK-JISYO.L")

辞書サーバを使わない場合は Emacs のバッファに変数 :el:defvar:`skk-large-jisyo` が指すファイル
を取り込んで使用するためメモリ使用量が増加します。これが支障となる場合は、上記の
ファイル :file:`SKK-JISYO.L` を :file:`SKK-JISYO.M` 、 :file:`SKK-JISYO.ML` 又は
:file:`SKK-JISYO.S` に変更してください。

.. _cdb-format:

.. index::
   keyword: CDB 形式辞書ファイル

DDSKK 14.1 以降は辞書サーバを経由せずとも CDB 形式 [#]_ の辞書ファイルを直接利用
できるようになりました。CDB 形式辞書ファイル [#]_ を利用する場合は、以下のように
指定してください。

.. code:: elisp

  (setq skk-cdb-large-jisyo "/your/path/to/SKK-JISYO.L.cdb")

変数 :el:defvar:`skk-large-jisyo` と変数 :el:defvar:`skk-cdb-large-jisyo` を同時
に指定した場合は、標準では CDB 形式辞書ファイルの方が先に検索 [#]_ されます。

************************
インクリメント検索の設定
************************

基本的な設定はファイル :file:`skk-setup.el` が読み込まれた時点で完了しています [#]_ 。

.. index::
   pair: Option; skk-isearch-mode-enable

.. el:defvar:: skk-isearch-mode-enable

  この変数はファイル :file:`~/.emacs.d/init.el` か :kbd:`M-x customize-variable` で設定し
  てください。

  .. list-table::

     * - Non-nil (標準設定は t )
       - SKK が ON になっているバッファで skk-isearch を有効にします。
     * - nil
       - skk-isearch を無効にすることができます。
     * - シンボル 'always
       - SKK が ON になっていないバッファでも skk-isearch を有効にします。

.. _setting-jisyo-server:

******************************
辞書サーバを使いたいときの設定
******************************

辞書サーバを使いたいときは、ファイル :file:`~/.skk` で以下のように設定します。

.. code:: elisp

  (setq skk-server-host "example.org")
  (setq skk-server-portnum 1178)

.. el:defvar:: skk-server-host

  辞書サーバが起動しているホスト名又は IP アドレス。

.. el:defvar:: skk-server-portnum

  辞書サーバが使うポート番号。ファイル :file:`/etc/services` に ``skkserv`` のエ
  ントリが記述されていれば、この変数を指定する必要は無い。

.. el:defvar:: skk-server-prog

  辞書サーバプログラムをフルパスで指定する。

.. el:defvar:: skk-server-jisyo

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

*************************************
DDSKK を Emacs の Input Method とする
*************************************

.. index::
   pair: File; skk-leim.el
   pair: Key; C-\
   pair: Key; M-x toggle-input-method

Emacs の標準キーバインドでは :kbd:`C-\\` を打鍵すると、関数 :el:defun:`toggle-input-method` を
実行します。この関数は、変数 :el:defvar:`default-input-method` が指す input method を
トグル切り替えします。

.. index::
   keyword: default-input-method
   keyword: LEIM

変数 :el:defvar:`default-input-method` の値はおそらく ``Japanese`` であり、結果として
:kbd:`C-\\` の打鍵で LEIM (Library of Emacs Input Method) を on / off します。

.. index::
   pair: Key; M-x list-input-methods
   pair: Key; M-x set-input-method
   pair: Key; C-x RET C-\

使用可能な input method は :kbd:`M-x list-input-methods` で確認することができ、コ
マンド :kbd:`M-x set-input-method` 又は :kbd:`C-x RET C-\\` を実行することで
input method を切り替えることができます。

ファイル:file:`skk-leim.el` から生成されるファイル :file:`skk-autoloads.el` で
input method をふたつ追加しています。

.. list-table::

   * - input method
     - 内容
   * - "japanese-skk"
     - 関数 :el:defun:`(skk-mode 1)`
   * - "japanese-skk-auto-fill"
     - :関数 el:defun:`(skk-auto-fill-mode 1)`

.. el:defvar:: default-input-method

  Emacs 起動時の input method を DDSKK とするには、ファイル :file:`~/.emacs.d/init.el` に

  .. code:: elisp

    (setq default-input-method "japanese-skk")

  と記述してください。

.. rubric:: 脚注

.. [#] Emacs が起動する過程の関数 :el:defun:`normal-top-level` でファイル :file:`SKK_LISPDIR/leim-list.el` が
       読み込まれます。
       ファイル :file:`leim-list.el` はファイル :file:`skk-autoloads.el` とファイル :file:`skk-setup.el` を require します。
       ファイル :file:`skk-autoloads.el` は DDSKK の :command:`make` 時に自動的に生成され
       るファイルであり、各関数を autoload するよう定義するほか
       関数 :el:defun:`register-input-method` も行います。
       ファイル :file:`skk-setup.el` はキーバインド（ :kbd:`C-x C-j` → ``skk-mode`` ）の定義、
       変数 :el:defvar:`skk-tut-file` の定義及びインクリメンタル・サーチの定義を行っています。

.. [#] 配布物にサンプルとしてファイル :file:`etc/dot.emacs` とファイル :file:`etc/dot.skk` が
       あります。参考にして下さい。

.. [#] constant database のこと。
       詳しくは http://cr.yp.to/cdb.html 又は http://ja.wikipedia.org/wiki/Cdb を
       参照のこと。

.. [#] SKK 辞書のファイル :file:`Makefile` 中の ``cdb`` ターゲットを実行することで
       ファイル :file:`SKK-JISYO.L` に基づくファイル :file:`SKK-JISYO.L.cdb` を生
       成することができます。

.. [#] :ref:`辞書検索の設定の具体例 <setting-search-jisyo>`

.. [#] ファイル :file:`skk-setup.el` では、 ``isearch-mode-hook`` に関数 :el:defun:`skk-isearch-setup-maybe` を、
       ``isearch-mode-end-hook`` に関数 :el:defun:`skk-isearch-cleanup-maybe` をそれぞれ追加
       しています。
       関数 :el:defun:`skk-isearch-{setup|cleanup}-maybe` もファイル :file:`skk-setup.el` で
       定義されており、その実体は、関数 :el:defun:`skk-isearch-mode-{setup|cleanup}` です。
