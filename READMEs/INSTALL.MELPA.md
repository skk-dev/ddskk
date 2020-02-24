# How to install DDSKK using MELPA

2014 年 12 月、MELPA に DDSKK が登録されたことにより、 GNU Emacs で `package.el` を利用して DDSKK をインストールすることができるようになりました。

 * MELPA: Milkypostman's Emacs Lisp Package Archive (https://melpa.org/)

## 1. package.el の設定

まず、`package.el` が、パッケージを MELPA から取得できるよう、
変数 package-archives を設定します。`~/.emacs.d/init.el` に
次のとおり記載してください。

```
(when (require 'package nil t)
  (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") t))
```

## 2. DDSKK のインストール

それでは MELPA を利用して DDSKK をインストールします。

`M-x list-packages` を実行すると、Package Menu モードが表示されます。

```
-------------------- Package Menu mode --------------------
  Package   Version      Status Archive Description
  darkroom  0.1          new    gnu     Remove visual ...
* ddskk     20141227.828 new    melpa   Simple Kana to ...
  form-feed 20141228.... new    melpa   Display ^L glyphs as ...
-------------------- Package Menu mode --------------------
```

ddskk の行にカーソルを置いて `i` をタイプすると、行頭に `I` マークが付きます。
続けて `x` をタイプすると、パッケージをインストールするか問われます。

```
------ Minibuffer -------
Install package `ddskk-20141227.828'? (yes or no)
------ Minibuffer -------
```

`y e s` とタイプすると、依存関係にあるパッケージ群 (ccc, cdb, ddskk) がダウンロードされ、インストールが始まります。

`skk.info` も同時にインストールされますので、`M-x info` で閲覧することができます。

** インストール時の制限 **

MELPA によるインストールでは、DDSKK の配布物に含まれている `doc/` や `etc/` といったディレクトリは全てインストールされません(`skk.info` と `skk.xpm` はインストールされます)。
インストールされるファイルは MELPA の recipe に記述されます。
詳しい recipe は https://github.com/milkypostman/melpa/blob/master/recipes/ddskk
を参照してください。

## 3. DDSKK の設定

MELPA を利用してインストールした場合、make による通常のインストールと比べて DDSKK の実行時に `leim-list.el` と `skk-setup.el` が存在しません。
そのため、`~/.emacs.d/init.el` にてキーバインドを定義する必要があります。

```
(global-set-key (kbd "C-x C-j") 'skk-mode)
```

必要に応じて、`skk-setup.el` を参考にしてインクリメンタル・サーチに関する設定も
記載してください。

skk-mode を起動すると設定ファイル `~/.skk` (skk-init-file) が読み込まれますので、
辞書などの諸設定は skk-init-file で定義してください。

DDSKK をインストールした後に `M-x skk-get` を実行することで、辞書ファイルを自動にダウンロードすることができます。
辞書ファイルは `skk-get-jisyo-directory` で指定されたディレクトリに保存されます。デフォルトでは `~/.emacs.d/skk-get-jisyo` となっています。

## 4. DDSKK のアップグレード

MELPA は、わりと頻繁に github のリポジトリ (skk-dev/ddskk) の更新を確認しているようです。
Package Menu モードで `U` とタイプすると、package.el は MELPA 上の DDSKK のアップ
デートをチェックします。

アップデートがあれば、アップデート対象のパッケージ数が表示されます。

```
-------------------- Echo Area --------------------
N package marked for upgrading.
-------------------- Echo Area --------------------
```

同時に `I` マークが付されますので、続けて `x` とタイプすることでアップデートが開始されます。

最終段階で、古いパッケージを消去するか問われますので、答えてください。
