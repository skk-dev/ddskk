########
はじめに
########

*****************************
このバージョンの SKK について
*****************************

Daredevil SKK （以下、このマニュアルにおいて DDSKK と呼びます。）は、動作が早くて
効率的な日本語入力環境を提供するソフトウェアです。

GNU General Public License に基づいて配布されているフリー・ソフトウェアです。
DDSKK |release| が動作すると思われる Emacsen のバージョンは、次のとおりです。

- GNU Emacs 24.3 以降
- GNU Emacs 25.1 以降
- GNU Emacs 26.1 以降

現時点で Emacs のバージョンごとに少なくとも以下の制限があります。

GNU Emacs 20
   DDSKK 14.2 以降は GNU Emacs 20 はサポート対象外です。GNU Emacs 20 のユ
   ーザは DDSKK 14.1 をお使いください。

GNU Emacs 21
   DDSKK 15.1 以降は GNU Emacs 21 はサポート対象外です。GNU Emacs 21 のユ
   ーザは DDSKK 14.4 をお使いください。

GNU Emacs 22
   DDSKK 16.2 以降は GNU Emacs 22 はサポート対象外です。GNU Emacs 22 のユ
   ーザは DDSKK 16.1 をお使いください。

GNU Emacs 23
   DDSKK 17.1 以降は GNU Emacs 23 はサポート対象外です。GNU Emacs 23 のユ
   ーザは DDSKK 16.3 をお使いください。

GNU Emacs 24.1, 24.2

   DDSKK 17.1 以降は GNU Emacs 24.1 と 24.2 はサポート対象外です。
   これら GNU Emacs ユーザは DDSKK 16.3 をお使いください。

GNU Emacs 24.3
   GNU Emacs 24.3 と DDSKK 14 の組み合わせで isearch 使用時の不具合が発見されてい
   ます。GNU Emacs 24.3 のユーザは DDSKK 15 以降をお使いください。

   - http://mail.ring.gr.jp/skk/201211/msg00000.html
   - http://mail.ring.gr.jp/skk/201212/msg00000.html

GNU Emacs 24.4
   - coding tag を明示していないファイルは utf-8 と取り扱われます [#]_ 。DDSKK 15.2 で対策済みです。
   - NTEmacs は 24.3 と比べてディレクトリ構成 が異なります [#]_ 。DDSKK 15.2 で対策済みです。

GNU Emacs 25.1
   DDSKK 15.2 以降をお使いください（DDSKK 16 を推奨します）。

GNU Emacs 27.1

   cl が正式に廃止され、cl-lib が採用されました。
   DDSKK 17 で対応しています。

XEmacs
   DDSKK 17.1 以降は XEmacs はサポート対象外です。
   XEmacs のユーザは DDSKK 16.3 をお使いください。

**************
SKK とはなにか
**************

SKK は、かな漢字変換プログラムです。

Simple Kana to Kanji conversion program にちなんで名付けられ、その名は
Combinatory Logic での有名な等式 :math:`SKK = I` にも由来 [#]_ しています。

Daredevil SKK は、SKK の更なる拡張版です [#]_ 。

ただし、SKK モード、SKK 辞書、SKK サーバ といった歴史的な用語は引き続き使用してお
り、DDSKK と呼ばない場合もあります。また、SKK 方式の入力方法を採用したプログラム
など、広く SKK family を意味する場合も同様です。

DDSKK の主な特徴は、次のとおりです。

- | :doc:`多彩な入力方式 <07_other-IM>` をサポート。
  | ローマ／かな 両対応のかな入力のほか、AZIK、ACT、TUT-code の各方式による入力も可能。
- 文法的知識を用いない高速な「かな→漢字」変換。
- シームレスかつ再帰的な :ref:`辞書登録モード <jisyo-register-mode>` 。
- 確定語を個人辞書へ自動登録することによって、変換候補を効率的に表示する。
- | マイナーモードとして実装されているので、メジャーモードにほとんど影響を与えない。
  | つまり、Emacs との親和性が高い。
- | DDSKK 本体 (Emacs Lisp) と辞書ファイルのみで動作可能。
  | つまり、辞書サーバは必須ではなく、辞書サーバがダウンしていても使用できる。
- 辞書サーバを使うことで、使用メモリの削減が可能。
- ディスク容量に応じて選べる辞書ファイル。
- 辞書ファイルの一括ダウンロード機能。
- Emacs のオリジナル操作と同様に行える :ref:`日本語インクリメンタル・サーチ <isearch>` 。
- Emacs Lisp で書かれた :ref:`プログラムが返す値を変換候補に挙げる <program-conversion>` ことができる。
- :ref:`入力モードの自動切り替え <context-skk>` :file:`context-skk.el`
- 多彩な :ref:`アノテーション表示 <annotation>`

  - ユーザ・アノテーション
  - EPWING 辞書
  - Apple macOS 辞書
  - Wikipedia/Wiktionary

- 「見出し語」の :ref:`動的補完 <dcomp>`
- :ref:`単漢字変換 <tankan>` （総画数、部首、部首の読み）
- 文字コード入力

.. rubric:: 脚注

.. [#] 2013-06-11 international/mule-conf.el (file-coding-system-alist).

.. [#] Emacs News: Changes in Emacs 24.4 on Non-Free Operating Systems.

.. [#] :math:`SKK = I` について詳しくは
       https://github.com/skk-dev/ddskk/blob/master/READMEs/history.md
       をご参照下さい。

.. [#] Daredevil の名の由来は [Q1-1 Daredevil SKK って SKK とは違うのですか?].
