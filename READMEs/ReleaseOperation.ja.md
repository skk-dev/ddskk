DDSKK リリース手順書
====================

DDSKK 16.2（蕨岱 Warabitai）のリリース作業を例として、リリース手順書としてまとめ
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
ルを試してください。

* GNU Emacs HEAD (26.0.50)
* GNU Emacs 23.1 (DDSKK 16.2 は GNU Emacs 22 はサポート対象外)
* XEmacs 21.5 (beta34) "kale"

また、次版リリースに向けた準備中であることをメーリングリストでアナウンスします。

## 3. ドキュメントの整理

すべてのドキュメントを査読して内容を整理しましょう。特に、次のドキュメントは重点
的に見直してください。

* READMEs/INSTALL
* READMEs/NEWS.ja
* doc/skk.texi

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


    diff --git a/Makefile b/Makefile
    index 4836286..7beec60 100644
    --- a/Makefile
    +++ b/Makefile
    @@ -6 +6 @@
    -VERSION = 16.1.50
    +VERSION = 16.2

    diff --git a/ddskk-pkg.el b/ddskk-pkg.el
    index 402cfdc..4a82ab2 100644
    --- a/ddskk-pkg.el
    +++ b/ddskk-pkg.el
    @@ -8 +8 @@
    -(define-package "ddskk" "16.1.50"
    +(define-package "ddskk" "16.2"

    diff --git a/doc/skk.texi b/doc/skk.texi
    index d96fe6c..3262467 100644
    --- a/doc/skk.texi
    +++ b/doc/skk.texi
    @@ -36,2 +36,2 @@
    -@set SKK-VERSION 16.1.50
    -@set UPDATED Date: 2017/02/18 13:12:05
    +@set SKK-VERSION 16.2
    +@set UPDATED Date: 2017/03/04

    diff --git a/skk-version.el b/skk-version.el
    index b0e4041..d3f3bf6 100644
    --- a/skk-version.el
    +++ b/skk-version.el
    @@ -34 +34 @@
    -     (let ((ver "16.1.50")
    +     (let ((ver "16.2")

ChangeLog を修正します。

```
2017-03-04  Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>
	* Version 16.2 Warabitai Released.
	* Makefile, ddskk-pkg.el, skk-version.el: Bump SKK version to 16.2.
```

```
2017-03-04  Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>
	* skk.texi: Bump SKK version to 16.2.
```

commit します。

    $ git add -u
    $ git commit -v
    $ git push

## 5. タグを打つ

まずは、タグの現状を確認しましょう。

    $ git tag
    ddskk-16.1.50
    ddskk-16.1_Futamata

新たなタグを打ち、github へ反映します。

    $ git tag ddskk-16.2_Warabitai
    $ git tag
    ddskk-16.1.50
    ddskk-16.1_Futamata
    ddskk-16.2_Warabitai

    $ git push --tags
    Total 0 (delta 0), reused 0 (delta 0)
    To github.com:skk-dev/ddskk.git
    * [new tag]         ddskk-16.2_Warabitai -> ddskk-16.2_Warabitai

## 6. github でのリリース作業

https://github.com/skk-dev/ddskk を開き、タブ "n releases" を開きます。

先ほど打ったタグ `ddskk-16.2_Warabitai` が確認できたら、ボタン "Draft a new release" を
クリックします。

必要事項を入力して、画面下部のボタン "Publish release" を押す。

なお、あとから自由に編集することもできます。

## 7. openlab でのリリース作業

### (1) ファイルを配置する

#### ア. 公開用アーカイブを配置する

手元でアーカイブを作ります。

    $ make release
    /bin/rm -f leim-list.el skk-autoloads.el skk-setup.el *.elc experimental/*.elc \
    auto-autoloads.el custom-load.el ert.el \
    ./doc/skk.info* `find . -name '*~'` `find . -name '.*~'` `find . -name '.#*'`
    /bin/rm -f ../ddskk-16.2.tar.gz ../ddskk-16.2.tar.bz2 ;\
    git archive --format=tar.gz --prefix=ddskk-16.2/ HEAD > ../ddskk-16.2.tar.gz ;\
    git archive --format=tar --prefix=ddskk-16.2/ HEAD | bzip2 -9 -c > ../ddskk-16.2.tar.bz2 ;\
    md5 ../ddskk-16.2.tar.bz2 > ../ddskk-16.2.tar.bz2.md5 ;\
    md5 ../ddskk-16.2.tar.gz > ../ddskk-16.2.tar.gz.md5
    $ cd ..
	$ ls
    ddskk-16.2.tar.bz2
    ddskk-16.2.tar.bz2.md5
    ddskk-16.2.tar.gz
    ddskk-16.2.tar.gz.md5

アーカイブを openlab へ転送します

    $ scp ddskk-16.2.tar.gz* USERNAME@openlab.jp:
    ddskk-16.2.tar.gz               100%  866KB   1.2MB/s   00:00
    ddskk-16.2.tar.gz.md5           100%   55     1.4KB/s   00:00

openlab へログインして、アーカイブを所定のディレクトリへ移します。

    $ ssh USERNAME@openlab.jp
    [USERNAME@openlab ~]$ mv ddskk-16.2.tar.gz* /circus/openlab/skk/maintrunk/

#### イ. PDF マニュアルを配置する

手元でマニュアルの PDF 版を生成します。

    $ cd doc
	$ ./makepdf.sh
	$ mv skk.pdf skk-16.2.pdf

skk.pdf を openlab へ転送し、所定のディレクトリへ移します。

    $ scp skk-16.2.pdf USERNAME@openlab.jp:
    $ ssh USERNAME@openlab.jp
    [USERNAME@openlab ~]$ mv skk-16.2.pdf /circus/openlab/skk/skk-manual
    [USERNAME@openlab ~]$ cd /circus/openlab/skk/maintrunk/
    [USERNAME@openlab /circus/openlab/skk/maintrunk]$ ln -s ../skk-manual/skk-16.2.pdf skk-16.2.pdf

### (2) WEB ページを更新する

openlab 向け cvs 作業です。

#### トップページ

ファイル `skk/web/index-j.html.in` と `skk/web/index.html.in` に、リリースアナウ
ンスを記載します。

    Index: web/index-j.html.in
    ===================================================================
    RCS file: /circus/cvsroot/skk/web/index-j.html.in,v
    retrieving revision 1.117
    diff -u -U 1 -r1.117 index-j.html.in
    --- web/index-j.html.in 1 Oct 2016 08:56:05 -0000       1.117
    +++ web/index-j.html.in 4 Mar 2017 08:09:53 -0000
    @@ -34,2 +34,3 @@
     <ul>
    +  <li>elisp: <a href="./maintrunk">Daredevil SKK 16.2 (蕨岱 Warabitai)</a> をリリースしました。(2017-03-04)
       <li>elisp: <a href="./maintrunk">Daredevil SKK 16.1 (Futamata)</a> をリリースしました。(2016-10-01)

    Index: web/index.html.in
    ===================================================================
    RCS file: /circus/cvsroot/skk/web/index.html.in,v
    retrieving revision 1.116
    diff -u -U 1 -r1.116 index.html.in
    --- web/index.html.in   1 Oct 2016 08:56:05 -0000       1.116
    +++ web/index.html.in   4 Mar 2017 08:09:53 -0000
    @@ -37,4 +37,5 @@
     <ul>
    +  <li>elisp: <a href="./maintrunk">Daredevil SKK 16.2</a> released. (2017-03-04)
       <li>elisp: <a href="./maintrunk">Daredevil SKK 16.1</a> released. (2016-10-01)

#### 歴史表

ファイル `skk/web/history-ja.html.in` と `skk/web/history.html.in` に、リリース
情報を記載します。

    Index: web/history-ja.html.in
    ===================================================================
    RCS file: /circus/cvsroot/skk/web/history-ja.html.in,v
    retrieving revision 1.72
    diff -u -U 1 -r1.72 history-ja.html.in
    --- web/history-ja.html.in      4 Mar 2017 03:15:41 -0000       1.72
    +++ web/history-ja.html.in      4 Mar 2017 08:36:29 -0000
    @@ -291,5 +291,13 @@
           <td></td>
    +      <td></td>
    +    </tr>
    +
    +    <tr>
    +      <td>2017</td>
    +      <td>Daredevil SKK 16.2 (03/04) -- Ruby 2.4 対応など</td>
    +      <td></td>
    +      <td></td>
           <td><a href="./countdic.cgi">只今の L 辞書の候補数</a></td>
         </tr>
    -
    +
       </tbody>

    Index: web/history.html.in
    ===================================================================
    RCS file: /circus/cvsroot/skk/web/history.html.in,v
    retrieving revision 1.48
    diff -u -U 1 -r1.48 history.html.in
    --- web/history.html.in 4 Mar 2017 03:15:41 -0000       1.48
    +++ web/history.html.in 4 Mar 2017 08:36:29 -0000
    @@ -291,2 +291,10 @@
           <td></td>
    +      <td></td>
    +    </tr>
    +
    +    <tr>
    +      <td>2017</td>
    +      <td>Daredevil SKK 16.2 (03/04)</td>
    +      <td></td>
    +      <td></td>
           <td><a href="./countdic.cgi">Number of candidates today</a></td>

#### マニュアル

ファイル `skk/web/doc-ja.html.in` と `skk/web/doc.html.in` に、7-(2) で公開し
た PDF へのリンクを記載します。

    Index: web/doc-ja.html.in
    ===================================================================
    RCS file: /circus/cvsroot/skk/web/doc-ja.html.in,v
    retrieving revision 1.15
    diff -u -U 1 -r1.15 doc-ja.html.in
    --- web/doc-ja.html.in  1 Oct 2016 08:56:05 -0000       1.15
    +++ web/doc-ja.html.in  4 Mar 2017 08:00:26 -0000
    @@ -40,2 +40,3 @@
       <li><a href="./skk-manual/skk-manual-ja.html">CVS trunk html</a> (<a href="./skk-manual/skk-manual-ja_frame.html">フレーム版</a>)
    +  <li><a href="./skk-manual/skk-16.2.pdf">DDSKK 16.2 pdf</a>
       <li><a href="./skk-manual/skk-16.1.pdf">DDSKK 16.1 pdf</a>

    Index: web/doc.html.in
    ===================================================================
    RCS file: /circus/cvsroot/skk/web/doc.html.in,v
    retrieving revision 1.11
    diff -u -U 1 -r1.11 doc.html.in
    --- web/doc.html.in     1 Oct 2016 08:56:05 -0000       1.11
    +++ web/doc.html.in     4 Mar 2017 08:00:26 -0000
    @@ -39,2 +39,3 @@
       <li><a href="./skk-manual/skk-manual-ja.html">CVS trunk html</a> (<a     href="./skk-manual/skk-manual-ja_frame.html">with frames</a>)
    +  <li><a href="./skk-manual/skk-16.2.pdf">DDSKK 16.2 pdf</a>
       <li><a href="./skk-manual/skk-16.1.pdf">DDSKK 16.1 pdf</a>

#### cvs commit

ChangeLog を付して cvs commit します。

    $ cvs commit -m "Update for release 16.2"

## 8. アナウンス

メーリングリストで新版をリリースした旨をアナウンスしましょう。

## 9. 各ファイルのバージョン情報を修正

前章 (アナウンス) までで、いったんはリリース作業は完了です。

さて、リリース作業の後のソースコード変更は、次版リリース向け作業に当たります。
そのため、開発版の番号を付しておきます。

* Makefile
* READMEs/CODENAME.ja
* READMEs/NEWS.ja
* ddskk-pkg.el
* doc/skk.texi
* skk-version.el


    diff --git a/Makefile b/Makefile
    index 7beec60..1eed6e7 100644
    --- a/Makefile
    +++ b/Makefile
    @@ -6 +6 @@
    -VERSION = 16.2
    +VERSION = 16.2.50

    diff --git a/READMEs/CODENAME.ja b/READMEs/CODENAME.ja
    index 599f1c0..4433740 100644
    --- a/READMEs/CODENAME.ja
    +++ b/READMEs/CODENAME.ja
    @@ -96,3 +96,3 @@ o 異なる路線で同じ駅を二度通過しても同じ Codename は 2 度
    -蕨岱      Warabitai       16.2
    -黒松内    Kuromatsunai     :
    -熱郛      Neppu
    +蕨岱      Warabitai       16.2 (2017-03-04)
    +黒松内    Kuromatsunai    16.3
    +熱郛      Neppu            :

    diff --git a/READMEs/NEWS.ja b/READMEs/NEWS.ja
    index f03283a..42b3cda 100644
    --- a/READMEs/NEWS.ja
    +++ b/READMEs/NEWS.ja
    @@ -1 +1,3 @@
    -* 16.2
    +* 16.3
    +
    +* 16.2 (2017-03-04)

    diff --git a/ddskk-pkg.el b/ddskk-pkg.el
    index 4a82ab2..cea056b 100644
    --- a/ddskk-pkg.el
    +++ b/ddskk-pkg.el
    @@ -8 +8 @@
    -(define-package "ddskk" "16.2"
    +(define-package "ddskk" "16.2.50"

    diff --git a/doc/skk.texi b/doc/skk.texi
    index 3262467..09bbd8d 100644
    --- a/doc/skk.texi
    +++ b/doc/skk.texi
    @@ -36 +36 @@
    -@set SKK-VERSION 16.2
    +@set SKK-VERSION 16.3

    diff --git a/skk-version.el b/skk-version.el
    index d3f3bf6..c0d0f20 100644
    --- a/skk-version.el
    +++ b/skk-version.el
    @@ -34 +34 @@
    -     (let ((ver "16.2")
    +     (let ((ver "16.2.50")
    @@ -45,2 +45,2 @@
    -(put 'skk-version 'codename "Warabitai") ; See also `READMEs/CODENAME.ja'
    -(put 'skk-version 'codename-ja "蕨岱")
    +(put 'skk-version 'codename "Kuromatsunai") ; See also `READMEs/CODENAME.ja'
    +(put 'skk-version 'codename-ja "黒松内")

ChangeLog も修正して commit します。

```
2017-03-04  Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>
        * Makefile, ddskk-pkg.el, skk-version.el: Bump SKK version to 16.2.50.
```

    $ git add -u
    $ git commit -v
    $ git push

以上です。おつかれさまでした。
