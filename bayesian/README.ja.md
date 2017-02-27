skk-bayesian.el
===============

`skk-bayesian.el` は、直前の履歴のみ使用する `skk-study.el` に比べて、より拡張さ
れた学習機能です。文脈から変換候補が選択される確率を計算して候補順をソートします。

## 1. skk-bayesian.el のインストール

以下のいずれかの方法で `skk-bayesian.el` をインストールしてください。

(a) SKK ソースのトップディレクトリ（このディレクトリのひとつ上）
に `skk-bayesian.el` をコピーしてから SKK をインストールしてください。

    % pwd
    /home/USER/ddskk-20170218/bayesian
    % mv skk-bayesian.el ..
    % cd ..
    % make install

(b) `skk-bayesian.el` のみを手動で変数 `load-path` の通った場所にコピーして利用
することもできます。

    % cp skk-bayesian.el ~/site-lisp

## 2. bskk のインストール

bskk は ruby スクリプトです。事前に ruby (http://www.ruby-lang.org/ja/) をインス
トールしてください。 ruby 2.4 以降を使用する場合は、DDSKK 16.2 以降を使用してく
ださい。

bskk は Emacs のサブプロセスとして、または単独のサーバとして使用します。

### (a) Emacs のサブプロセスとして使用する場合

bskk を 環境変数 `PATH` の通った場所に置いてください。
Emacs が `skk-bayesian.el` を require 処理する過程で bskk が起動します。

### (b) サーバとして使用する場合

Emacs が `skk-bayesian.el` を require する前に、以下のように bskk をあらかじめ起
動しておいてください。

    % bskk -f ~/.skk-bayesian -s

`f` オプションで history ファイルのパスを指定します。
`s` オプションはサーバ起動を指示するものです。

サーバを終了させるときは以下のようにします。

    % kill -TERM {bskk の PID}
       
bskk サーバが起動したのち `skk-bayesian.el` を require します。
このとき、変数 `skk-bayesian-prefer-server` を `Non-nil` と指定します。
`~/.skk` への記述方法は後述します。

## 3. `~/.skk` の設定

以下を自分の `~/.skk` に追加してください。

    (require 'skk-bayesian)

なお、 bskk をサーバとして使う場合は以下も追加してください。

    (setq skk-bayesian-prefer-server t)

## 4. その他

`skk-bayesian.el` の動作の仕組み、仕様については `skk-bayesian.el` 内のコメント
を参照してください。

## 5. ruby のリビジョンによる不具合について

ruby は様々なリビジョンのものが利用されていますが、場合によっては出力が正常でな
く、skk-bayesian で文字化けとして現れることがあります。
例えば ruby 1.8.7 (2009-12-24 patchlevel 248) では問題があることが分かっています。
これについては ruby 1.8.8 のリビジョン 26103 で修正されています。または最後のパ
ッチを当てることで修正されます。参考になる URL は以下です。

   http://redmine.ruby-lang.org/issues/show/2569
   (Ruby1.8.7p248 String#inspect broken on multibyte string.)

   http://redmine.ruby-lang.org/repositories/revision/ruby-18?rev=26103
   (Ruby 1.8 - リビジョン 26103)

    *** string.c.org	2010-01-22 01:54:05.000000000 +0900
    --- string.c	2010-01-22 01:54:23.000000000 +0900
    ***************
    *** 2642,2648 ****
          while (p < pend) {
            char c = *p++;
            int len;
    !       if (ismbchar(c) && p + (len = mbclen(c)) <= pend) {
              rb_str_buf_cat(result, p - 1, len);
              p += len - 1;
            }
    --- 2642,2648 ----
          while (p < pend) {
            char c = *p++;
            int len;
    !       if (ismbchar(c) && p - 1 + (len = mbclen(c)) <= pend) {
              rb_str_buf_cat(result, p - 1, len);
              p += len - 1;
            }

[EOF]
