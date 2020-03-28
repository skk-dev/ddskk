
このディレクトリには SKK の仮名入力対応コードを置いています。


# 1. 概要

SKK をローマ字的入力以外の入力方式に対応させることを目的とします。現在
は以下の入力方式について、サポートするべく開発しています。

  - 日本語 106 キーボードでの仮名入力

  - 親指シフトキーボードのエミュレーション

後者については、 箕浦逸史さん作の NICOLA-SKK 0.39 を基にして開発を進め
ています。というよりこちらが主力です。

なお、 TUT-code 入力のサポートは別に開発されており、そのソースコードは
SKK の experimental repository にあります。


# 2. ステータス

Beta.


# 3. ファイル

  - skk-kanagaki.el … 仮名入力のための基本的な枠組み

  - skk-106-jis.el  … 日本語 106 キーボードで仮名入力するためのルール

  - skk-nicola.el   … 親指シフト入力をエミュレートする関数  (オリジナルの skk-nicola.el に書かれていた関数)

  - skk-nicola-*.el … NICOLA 配列のルール (JIS, US, Dvorak, Colemak)

  - skk-oasys.el    … OASYS 風配列のルール

  - skk-omelet-*.el … omelet 独自配列のルール (JIS, US, Dvorak, Colemak)


skk-nicola-*.el, skk-oasys.el, skk-omelet-*.el の配列はすべて

http://www.eva.hi-ho.ne.jp/%7Eminoura/kbd/keymap.html

のものを利用させていただきました。


# 4. インストール

## 4.1. SKK をインストール

     https://github.com/skk-dev/ddskk

## 4.2. NICOLA-DDSKK のインストール

   必ず、上記でインストールした SKK に同梱されている NICOLA-DDSKK をイン
   ストールしてください。つまり、SKK をバージョンアップしたら必ず
   NICOLA-DDSKK をインストールし直してください。

     % cd nicola
     % make install


# 5. 設定

## 5.1. SKK の設定

   ~/.emacs.d/init.el に

     (require 'skk-setup)

   と記入。

## 5.2. NICOLA-DDSKK の設定

   親指シフト入力する場合は ~/.skk に

     (setq skk-use-kana-keyboard t)
     (setq skk-kanagaki-keyboard-type 'omelet-jis)

   と記入。旧 JIS 配列の仮名入力を行う場合は

     (setq skk-use-kana-keyboard t)
     (setq skk-kanagaki-keyboard-type '106-jis)

   と記入。

## 5.3. カスタマイズ

   skk-kanagaki-keyboard-type に対応したファイルに書かれている変数によっ
   て、キー入力と挿入される文字のルールを変更します。

   例えば skk-kanagaki-keyboard-type が omelet-jis であれば、
   skk-omelet-jis.el に書かれている以下の変数を設定します。

   `skk-kanagaki-omelet-jis-base-rule-list'

        この変数は、各文字キーの入力を SKK にどう解釈させるかを決定し
        ます。標準設定では、多くの文字キーは関数 `skk-nicola-insert'
        を呼び出します。

   `skk-omelet-jis-plain-rule-list'

   `skk-omelet-jis-rshift-rule-list'

   `skk-omelet-jis-lshift-rule-list'

        これらの変数は、関数 `skk-nicola-insert' が文字を挿入する際の
        ルールを決定します。順に単独打鍵、右シフト入力、左シフト入力の
        ルールです。

   これらの設定をファイル ~/.skk に記述してから SKK を起動すれば、設定
   が反映されます。

# 6. 使用法

親指シフト入力についての詳しいことは、[README.NICOLA.md](README.NICOLA.md) を
ご一読ください。

それ以外の仮名入力について詳しいことは skk-kanagaki.el の冒頭のコメン
トをご一読ください。

以下で、<kbd>[fj]</kbd> などというのは <kbd>f</kbd> と <kbd>j</kbd> とを同時に打鍵することを意味します。

## 6.1. 一般的キー定義

  - <kbd>[fj]</kbd> … 変換開始点をセット
  - <kbd>[gh]</kbd> … 接頭辞 or 接尾辞変換 (▽モード or ▼モード)
  - <kbd>[gh]</kbd> … abbrev モード
  - <kbd>[dk]</kbd> … カナモード or カナ変換
  - <kbd>space</kbd> … 送りなし変換開始、変換・次候補表示
  - <kbd>x</kbd> … 前候補表示
  - <kbd>S</kbd> … <kbd>SHIFT</kbd> + <kbd>s</kbd>。送りあり変換開始
  - <kbd>C-h</kbd> <kbd>1</kbd> … ヘルプを表示

## 6.2. 親指シフト入力モードの独自キー定義

  - <kbd>muhenkan</kbd> … 左親指シフトキー
  - <kbd>henkan</kbd> … 右親指シフトキー
  - <kbd>space</kbd> … 右親指シフトキー
  - <kbd>[fj]</kbd> … 送り開始点指定
  - <kbd>[muhenkan + henkan]</kbd> … latin モード ⇔ かなモードの切り替え
  - <kbd>C-h</kbd> <kbd>2</kbd> … 現在の入力方式のキー配列を表示
  - <kbd>C-h</kbd> <kbd>3</kbd> … ヘルプを表示

## 6.3. 旧 JIS 仮名入力モードの独自キー定義

  - <kbd>[fj]</kbd> … 直前の 1 文字を送り仮名として送りあり変換を開始。
  - <kbd>[muhenkan + henkan]</kbd> … latin モード ⇔ かなモード の切り替え
  - <kbd>C-h</kbd> <kbd>2</kbd> … 現在の入力方式のキー配列を表示
  - <kbd>C-h</kbd> <kbd>3</kbd> … ヘルプを表示

## 6.4. 特殊機能の退避キー

  - <kbd>f2</kbd> … 変換開始点をセット
  - <kbd>f3</kbd> … 接頭辞 or 接尾辞変換
  - <kbd>f5</kbd> … コード入力
  - <kbd>f6</kbd> … abbrev モード
  - <kbd>f7</kbd> … カナモード or カナ変換
  - <kbd>f8</kbd> … 全英モード
  - <kbd>f9</kbd> … 半角カナモード or 半角カナ変換
  - <kbd>f10</kbd> … latin モード
  - <kbd>f12</kbd> … ローマ字入力 ⇔ 仮名入力

# 7. 問題点

親指シフト入力は、 まだ NICOLA-SKK と挙動の異なる部分があります。 問題
は多くありますが、徐々に改良する予定です。また、omelet 及び NICOLA-SKK
との互換性を十分考慮していない部分があります。

日本語 106 キーボードで仮名入力する場合、 キー配列の関係で「ー」か「ろ」
のいずれかが犠牲にならなければなりません。デフォルトでは、「ー」を犠牲
にしてあります (SHIFT と共に押せば入力できるようにしてあります)。これ
を刻印通りの入力にするには、 xmodmap を使うなどしてキー配列を変更する
必要があります。

# 8. 謝辞

NICOLA-SKK 原作者の箕浦さんに敬意を表し，また感謝いたします。

# 9. 後日談 (2004-03-10)

実は、SKK のモデルとなったかな漢字変換システムである Kanzen は標準でロー
マ字式入力と新 JIS 配列かな入力に対応していたようです。(旧ではなくて新
なところがあれですが...)

   http://www.nue.org/people/amagai/kanzen/index.html

