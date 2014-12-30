# How to install DDSKK using MELPA

2014 年 12 月、MELPA に DDSKK が登録されたことにより GNU Emacs でも `package.el` によるインストールが可能となりました。

On Decenmber 2014, DDSKK joined to MELPA repository and DDSKK is possible to be installed
on GNU Emacs with `package.el`.

 * MELPA: Milkypostman's Emacs Lisp Package Archive (http://melpa.org/)

 * package.el: GNU Emacs 24 からは標準で搭載されています。GNU Emacs 23 以前では手動でインストール必要があります。
http://wikemacs.org/wiki/Package.el

 * package.el is bundled with Emacs 24, but it’s not bound to Emacs 23. Before it became
part of Emacs it was an external package
http://wikemacs.org/wiki/Package.el


## 1. Setting for package.el

まず、`package.el` が MELPA を参照するよう、`~/.emacs.d/init.el` に次のとおり設定してください。

To use MELPA add following lines to your `~/.emacs.d/init.el`

```
(when (require 'package nil t)
  (add-to-list 'package-archives
    '("melpa" . "http://melpa.org/packages/") t))
(package-initialize)
```

## 2. Installing DDSKK

それでは MELPA を利用して DDSKK をインストールします。
`M-x list-packages` を実行すると、Package Menu モードが表示されます。

Then, submit `M-x list-packages` to install DDSKK using MELPA, and Package Menu is indicated. 


```
-------------------- Package Menu mode --------------------
  Package   Version      Status Archive Description
  darkroom  0.1          new    gnu     Remove visual ...
* ddskk     20141227.828 new    melpa   Simple Kana to ...
  form-feed 20141228.... new    melpa   Display ^L glyphs as ...
-------------------- Package Menu mode --------------------
```

ddskk の行にカーソルを置いて `i` `x` と順にタイプすると、ミニバッファにてパッケージをイン
ストールするか問われます。

Place the cursor on ddskk and type `i` `x`, then you asked in the mini buffer if
this packege will be installed.

```
------ Minibuffer -------
Install package `ddskk-20141227.828'? (yes or no)
------ Minibuffer -------
```

`y e s` とタイプすると、インストールが始まります。

To build and install the package, type `y e s`.

** A limit to install the package **

MELPA によるインストールでは、DDSKK の配布物に含まれている `doc/` や `etc/` といったディレクトリは全てインストールされません。インストールされるファイルは MELPA の recipe に記述されます。
詳しい recipe は https://github.com/milkypostman/melpa/blob/master/recipes/ddskk
を参照してください。

The directories such like `doc/` or `etc/` included in distribution of DDSKK will not be installed by MELPA.
The files contained install package are described on recipe of MELPA.
Refer the detailed recipe on https://github.com/milkypostman/melpa/blob/master/recipes/ddskk

## 3. Setting of DDSKK

MELPA を利用してインストールした場合、make による通常のインストールと比べて DDSKK の実行時に `leim-list.el` と `skk-setup.el` が存在しません。
そのため、`~/.emacs.d/init.el` にてキーバインドを定義する必要があります。

`leim-list.el` and `skk-setup.el` are absent, different from Makefile to install, therefor
you should set a key bind on `~/.emacs.d/init.el`.

```
(global-set-key (kbd "C-x C-j") 'skk-mode)
```

skk-mode を起動すると設定ファイル `~/.skk` (skk-init-file) が読み込まれますので、
辞書などの諸設定はこちらで定義してください。

After running skk-mode, Emacs read `~/.skk` (skk-init-file), so
you use this to set such like dictionary place.

## 4. Upgrading DDSKK

MELPA は、わりと頻繁に github のリポジトリ(skk-dev/ddskk)の更新を確認しているようです。

MELPA seems to check the updating of github repository of skk-dev/ddskk.

Package Menu モードで `U` とタイプすると、package.el は MELPA 上の DDSKK のアップ
デートをチェックします。

You can check updating of DDSKK on MELPA by typing `U` on Package Menu mode.

アップデートがあれば、アップデート対象のパッケージ数が表示されます。

If there exist the updates, the target packages are indicated as follows.

```
-------------------- Echo Area --------------------
N package marked for upgrading.
-------------------- Echo Area --------------------
```

同時に `I` マークが付されますので、続けて `x` とタイプすることでアップデートが開始されます。

`I` is marked and then updating will start after type `x`.

最終段階で、古いパッケージを消去するか問われますので、答えてください。

Finaly, you are asked if delete old package, answer it.