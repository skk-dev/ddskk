DDSKK リリース手順書
====================

DDSKK 17.1（熱郛 Neppu）のリリース作業を例として、リリース手順書としてまとめ
ておきます。

**この文書は DDSKK の開発者・リリース担当者に向けたものです。**

## 1. リリースのタイミング

GNU Emacs の新メジャーバージョンがリリースされたときには、さほど遅れないタイミン
グで DDSKK も新版をリリースするように心掛けています。このリリースにおけるバージ
ョン番号は DDSKK `xx.y` の `xx` 部分をインクリメントします。

上記のタイミング以外でも DDSKK の新版をリリースすることがありますが、それは不定
期です。このリリースにおけるバージョン番号は DDSKK `xx.y` の `y` 部分をインクリ
メントします

ただし、過去のリリースは必ずしも上記ルールに沿ったものではありません。これまでの
リリース経過は次のドキュメントを参照してください。

[SKK の歴史 http://openlab.jp/skk/history-ja.html](http://openlab.jp/skk/history-ja.html)

## 2. リリース前に向けた準備

リリースする DDSKK がサポートする emacsen を使用して、少なくとも make インストー
ルを試して、正常にインストールできることを確認してください。

  * GNU Emacs HEAD

  * GNU Emacs 24.3

なお、XEmacs のサポートは DDSKK 16.3 で終了しました。

## 3. ドキュメントの整理

すべてのドキュメントを査読して内容を整理しましょう。特に、次のドキュメントは重点
的に見直してください。

  * READMEs/INSTALL

  * READMEs/NEWS.ja

  * doc/skk.texi

  * docs/*.rst

いったん commit しておきます。

    $ git add -u
    $ git commit -v
    $ git push

## 4. 各ファイルのバージョン情報を修正

以下のファイルについて、リリース版の番号となるようバージョン情報を修正します。

  * doc/skk.texi: 36 行目の `SKK-VERSION`

  * Makefile: 6 行目の `VERSION`

  * skk-version.el: 34 行目の `ver`

  * ddskk-pkg.el: 8 行目

  * docs/conf.py: 30 行目

  * READMEs/CODENAME.ja

ChangeLog を修正します。

```
2017-03-04  Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>
	* Version 16.2 Warabitai Released.
	* Makefile, ddskk-pkg.el, skk-version.el: Bump SKK version to 16.2.
```

commit します。

    $ git add -u
    $ git commit -v
    $ git push

## 5. タグを打つ

まずは、タグの現状を確認しましょう。

    $ git tag
    ddskk-16.2_Warabitai
    ddskk-16.3_Kutomatsunai

新たなタグを打ち、github へ反映します。

    $ git tag ddskk-17.1_Neppu
    $ git tag
    ddskk-16.2_Warabitai
    ddskk-16.3_Kutomatsunai
    ddskk-17.1_Neppu

    $ git push --tags
    Total 0 (delta 0), reused 0 (delta 0)
    To github.com:skk-dev/ddskk.git
    * [new tag]         ddskk-xx -> ddskk-xx

## 6. github でのリリース作業

https://github.com/skk-dev/ddskk を開き、タブ "n releases" を開きます。

先ほど打ったタグ `ddskk-17.1_Neppu` が確認できたら、ボタン "Draft a new release" を
クリックします。

必要事項を入力して、画面下部のボタン "Publish release" を押す。

なお、あとから自由に編集することもできます。

#### 歴史表


## 9. 各ファイルのバージョン情報を修正

前章 (アナウンス) までで、いったんはリリース作業は完了です。

さて、リリース作業の後のソースコード変更は、次版リリース向け作業に当たります。
そのため、開発版の番号を付しておきます。

以上です。おつかれさまでした。
